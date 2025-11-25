#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE init_rivers_props_mod

USE model_interface_mod, ONLY: identifier_len

USE jules_rivers_mod, ONLY: inland_drainage, river_mouth, sea

USE logging_mod, ONLY: log_fatal, log_info, log_warn

USE missing_data_mod, ONLY: imdi, rmdi

USE string_utils_mod, ONLY: to_string

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!------------------------------------------------------------------------------
! Terminology and grids used:
! Land grid:
!   the 2-D grid used by JULES (not rivers) and on which land points sit.
!   If JULES is using a 1-D grid, the model grid is the notional 2D grid
!   across which the points can be scattered.
! River input grid:
!   the 2-D river input grid (as held in ancillary files).
! River grid:
!   the 2-D river grid. In many configurations this is identical to the river
!   input grid, but in others the input grid is transformed (e.g. the order
!   of the rows is reversed and/or the columns of a global grid have undergone
!   a cyclic shift).
! River domain:
!   that part of the river grid that is scanned for river points.
!   The size and location of this depends on the configuration, but ideally it
!   should be just large enough to cover all land points.
! River points:
!    the points within the river domain at which rivers are modelled.
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Scalar parameters.
!------------------------------------------------------------------------------
! The values of dir_mouth and dir_inland_drainage should differ and should not
! be in the range 1:SIZE(flow_dir_delta,1), i.e. not in the range of values
! used to encode the flow directions.
INTEGER, PARAMETER :: dir_mouth = 9
  ! The value of the flow direction field that indicates a river mouth.
INTEGER, PARAMETER :: dir_inland_drainage = 10
  ! The value of the flow direction field that indicates an inland drainage
  ! point, i.e. an endorheic catchment.

INTEGER, PARAMETER :: land_off_domain = sea + 1
    ! The value of river_mask that is used to indicate a land point (according
    ! to the river ancillary) that is outside of the valid river domain, i.e.
    ! it is not in the area covered by land and hence cannot become a river
    ! point. This value must not be either of sea or land_in_domain.

INTEGER, PARAMETER :: land_in_domain = land_off_domain + 1
    ! The value of river_mask that is used to indicate a land point (according
    ! to the river ancillary) that is inside the valid river domain, i.e. it is
    ! in the area covered by land and hence can become a river point.
    ! This value must not be either sea or land_off_domain.

INTEGER, PARAMETER :: nvars_max = 6
  ! The maximum possible number of ancillary variables that are read in.

REAL(KIND=real_jlslsm), PARAMETER :: frac_toler = 1.0e-3
  ! Tolerance (fraction of a gridbox) used when testing whether model points
  ! lie on a regular grid, if grids have the same resolution, and when setting
  ! some bounding values.

LOGICAL, PARAMETER :: l_use_direction = .TRUE.
  ! Parameter indicating that we are provided with a flow direction ancillary.
  ! At present this is always the case, but anticipated developments will
  ! create a need to start from nextx & nexty fields.

LOGICAL ::                                                                     &
  l_riv_number = .FALSE.
    ! Flag indicating if river number ancillary has been specified.

!------------------------------------------------------------------------------
! Array parameters.
!------------------------------------------------------------------------------
INTEGER, PARAMETER :: flow_dir_river(10) =                                     &
  [ 1, 2, 3, 4, 5, 6, 7, 8, dir_mouth, dir_inland_drainage ]
    ! The values of flow direction that are acceptable for a river point.
    ! Effectively these also the only values that indicate land points in the
    ! flow direction field. The first 8 places represent "compass directions"
    ! for the 8 points immediately neighbouring a given point and these should
    ! be >0. All values should be unique.

INTEGER, PARAMETER :: flow_dir_river_edge(8) = -flow_dir_river(1:8)
  ! Values of nextx_grid that are used to indicate flow across the edge of the
  ! river domain or grid. These are -1 * values of flow_dir_river that indicate
  ! a "physical" flow direction and as such will have values <0.

!------------------------------------------------------------------------------
! Scalar variables.
!------------------------------------------------------------------------------
INTEGER :: nvars_required
  ! The number of routing variables that are required in this configuration.

!------------------------------------------------------------------------------
! Array variables.
!------------------------------------------------------------------------------
INTEGER, ALLOCATABLE :: direction_grid(:,:)
  ! Integer version of rivers_dir (the river routing direction index).
  ! We use the REAL version for i.o (JULES cannot read an integer ancillary
  ! field) but thereafter the integer version is easier to use.

INTEGER, ALLOCATABLE :: grid_riv_pt_number(:,:)
  ! Map full river grid to river points. For each location on the grid, this
  ! is the river point number.

INTEGER, ALLOCATABLE :: nextx_grid(:,:)
  ! x index of the next downstream point.

INTEGER, ALLOCATABLE :: nexty_grid(:,:)
  ! y index of the next downstream point.

INTEGER, ALLOCATABLE :: river_mask(:,:)
  ! Indicates type of each point.
  ! Values are sea, land_off_domain or land_in_domain.

CHARACTER(LEN=identifier_len) :: required_vars(nvars_max)
  ! The variable identifiers of the required variables

PRIVATE  !  private scope by default
PUBLIC init_rivers_props

CONTAINS

!##############################################################################

SUBROUTINE init_rivers_props(rivers, rivers_data)

USE ancil_namelist_mod, ONLY: check_namelist_values, initialise_namelist_vars, &
                              set_constant_vars, set_file_vars

USE fill_variables_from_file_mod, ONLY: fill_variables_from_file

USE grid_utils_mod, ONLY: grid_create, grid_info

USE init_ancillaries_coupling_mod, ONLY: read_ancillaries_coupling

USE input_mod, ONLY:                                                           &
    od_grid => grid,                                                           &
    dummy_grid => grid,                                                        &
    use_subgrid

USE init_overbank_props_mod, ONLY: init_overbank_props

USE io_constants, ONLY: max_sdf_name_len, max_file_name_len, namelist_unit

USE jules_rivers_mod, ONLY:                                                    &
  i_river_vn, land_dx, land_dy, l_rivers, nx_land_grid,                        &
  nx_rivers_in=>nx_rivers, ny_land_grid, ny_rivers_in=>ny_rivers,              &
  rivers_length, rivers_regrid, rivers_rfm, rivers_trip, x1_land_grid,         &
  y1_land_grid, l_riv_overbank,                                                &
  ! types
  rivers_type, rivers_data_type

USE model_grid_mod, ONLY: global_land_pts, l_coord_latlon

USE overbank_inundation_mod, ONLY:                                             &
  init_rosgen_vars, overbank_model, overbank_simple_rosgen

USE parallel_mod, ONLY: is_master_task

USE templating_mod, ONLY: tpl_has_var_name

IMPLICIT NONE

!------------------------------------------------------------------------------
! Description:
!    Top-level routine that controls reading of river routing grid and river
!    and properties, and allocation and setting of river point values.
!
!    When running the river executable in coupled mode, no regridding between
!    the land and river grids is necessary, so we can skip the calculation of
!    some variables that will not be used.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Subroutine arguments.
!------------------------------------------------------------------------------
! Types
TYPE(rivers_type), INTENT(IN OUT) :: rivers
TYPE(rivers_data_type), INTENT(IN OUT), TARGET :: rivers_data

!------------------------------------------------------------------------------
! Local parameters.
!------------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'INIT_RIVERS_PROPS'

!------------------------------------------------------------------------------
! Local variables appearing in the namelist.
!------------------------------------------------------------------------------
! Scalars in namelist.
INTEGER :: nvars
    ! The number of variables in this section

INTEGER :: nx_rivers, ny_rivers
    ! The sizes of the x and y dimensions of the river routing input grid.

LOGICAL ::                                                                     &
  l_find_grid = .FALSE.,                                                       &
    ! Switch controlling how characteristics of the land and rivers grids are
    ! determined.
    ! F = use namelist values for the variables nx_land_grid, ny_land_grid,
    !     x1_land_grid and y1_land_grid, which describe the land grid. Note
    !     that in this case rivers_regrid=F requires nx_land_grid and
    !     ny_land_grid only so as to ensure historical results are maintained.
    ! T = calculate details of the land grid from the known coordinates of
    !     land points. This also triggers differences in how the river domain
    !     is set up, including better treatment of the cases in which the
    !     resolutions of the land and river grids differ and/or land points
    !     (e.g. from a regional domain) straddle the longitudinal edges of a
    !     global river input grid.
  l_use_area = .FALSE.
    ! Switch (used with RFM) to use a drainage area ancillary field to identify
    ! river points.
    ! T means use ancillary data for area.
    ! F means no ancillary data provided. Area will be set above threshold so
    ! that all points will be identified as rivers.

LOGICAL ::                                                                     &
  l_ignore_ancil_rivers_check = .FALSE.
    ! Switch to check river routing & river number ancillary for compatibility.
    ! The current Rose stem test ancils are not compatible as the land sea
    ! mask is different. Setting this to true maintains bit comparability &
    ! protects the calculation of the coupling field needed for the LFRic
    ! coupled miniapp.

CHARACTER(LEN=max_file_name_len) :: FILE
    ! The name of the file (or variable name template) to
    ! use for variables that need to be filled from file.
    ! Co-ordinate variables are also read from this file,
    ! assuming file does not include variable-name
    ! templating (i.e. assuming all ancillary variables will
    ! come from the same file).

CHARACTER(LEN=max_file_name_len) :: coordinate_file
    ! The name of the file given for reading coordinate
    ! values from. This is only used if the ancillary file
    ! name includes templating (i.e. ancillary variables will
    ! come from multiple files), in which case this variable
    ! is used to provide clarity as to where the coordinates
    ! are read from.

CHARACTER(LEN=max_file_name_len) :: riv_number_file = ''
    ! The name of the file given for reading river number
    ! at each grid point

CHARACTER(LEN=max_sdf_name_len) :: x_dim_name, y_dim_name
    ! The names of the x and y dimensions.

! Arrays in namelist.
REAL(KIND=real_jlslsm) :: const_val(nvars_max)
    ! The constant value to use for each variable if
    ! use_file = F for that variable

LOGICAL :: use_file(nvars_max)
    !   T - the variable uses the file
    !   F - the variable is set using a constant value
LOGICAL ::                                                                     &
  use_var(nvars_max)
    ! Flag indicating if a variable is required.

CHARACTER(LEN=max_sdf_name_len) :: tpl_name(nvars_max)
    ! The name to substitute in a template for each variable
CHARACTER(LEN=identifier_len) :: var(nvars_max)
    ! The variable identifiers of the variables
CHARACTER(LEN=max_sdf_name_len) :: var_name(nvars_max)
    ! The name of each variable in the file

!------------------------------------------------------------------------------
! Definition of the namelist. This contains local variables and some from
! jules_rivers_mod.
!------------------------------------------------------------------------------
NAMELIST  /jules_rivers_props/                                                 &
   x_dim_name, y_dim_name, nx_rivers, ny_rivers, FILE, coordinate_file,        &
   nvars, var, use_file, var_name, tpl_name, const_val,                        &
   land_dx, land_dy, l_find_grid, l_use_area, nx_land_grid, ny_land_grid,      &
   rivers_length, rivers_regrid, x1_land_grid, y1_land_grid, riv_number_file,  &
   l_ignore_ancil_rivers_check

!------------------------------------------------------------------------------
! Local scalar variables (not in namelist).
!------------------------------------------------------------------------------
INTEGER :: ERROR, error_sum    ! Error indicators

INTEGER :: nvars_const         ! The number of variables to be set using
                               ! constant values.
INTEGER :: nvars_file          ! The number of variables that will be set
                               ! from the given file (template?)

LOGICAL ::                                                                     &
  l_calc_rivers_length,                                                        &
    ! Flag indicating if rivers_length will be calculated.
  l_have_template,                                                             &
    ! Flag indicating if variable-name templating is being used.
  use_sub_local
    ! A local copy of use_subgrid.


!------------------------------------------------------------------------------
! Local array variables (not in namelist).
!------------------------------------------------------------------------------
CHARACTER(LEN=max_file_name_len) :: file_name(nvars_max)
    ! The name of the file to use for each variable.
CHARACTER(LEN=max_file_name_len) :: file_name_coords
    ! The name of the file to be used to read coordinate values from.

!------------------------------------------------------------------------------
! Types
!------------------------------------------------------------------------------
TYPE(grid_info) :: local_grid

!end of header
!------------------------------------------------------------------------------

! If rivers are not on, there is nothing to do.
IF ( .NOT. l_rivers ) RETURN

! If this run does not have any land points we cannot do river routing.
IF ( global_land_pts == 0 ) THEN
  CALL log_fatal( RoutineName,                                                 &
                 "River routing not appropriate for runs with no land "     // &
                 "points." )
END IF

! Check for a consistency between the river scheme and the coordinate system.
! This does not involve river ancillaries, but at present there is no obviously
! better location at which to do this.
! TRIP needs rivers_dx and rivers_dy to be defined in terms of latitude and
! longitude for use when calculating distances in SUBROUTINE get_rivers_len_rp.
! At present that is only the case when the grid is defined in terms of
! latitude and longitude.
IF ( i_river_vn == rivers_trip .AND. .NOT. l_coord_latlon ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 'TRIP can only be used with coordinates of latitude and '  // &
                 'longitude.')
END IF

!------------------------------------------------------------------------------
! Most tasks need only be done on the master task, which is where the river
! routing code will run. If the JULES_RIVERS_PROPS namelist were to contain
! values that are needed by other tasks, all tasks would have to read it.
!------------------------------------------------------------------------------
IF ( is_master_task() ) THEN

  !----------------------------------------------------------------------------
  ! Get ready for reading the river routing properties namelist.
  !----------------------------------------------------------------------------
  ! Initialisation of some common variables related to namelists.
  CALL initialise_namelist_vars( nvars, nvars_const, nvars_file,               &
                                 nvars_required, l_have_template,              &
                                 FILE, const_val, use_file, use_var,           &
                                 file_name, tpl_name, var, var_name )

  ! Initialisation of further variables.
  coordinate_file = '' ! Empty file name.
  l_calc_rivers_length = .FALSE.  !  value will not be calculated

  !----------------------------------------------------------------------------
  ! Read river routing properties namelist
  !----------------------------------------------------------------------------
  CALL log_info( RoutineName, "Reading JULES_RIVERS_PROPS namelist..." )

  READ(namelist_unit, NML = jules_rivers_props, IOSTAT = ERROR)
  IF ( ERROR /= 0 ) THEN
    CALL log_fatal( RoutineName,                                               &
                    "Error reading namelist JULES_RIVERS_PROPS "            // &
                    "(IOSTAT=" // TRIM(to_string(ERROR)) // ")" )
  END IF

  !----------------------------------------------------------------------------
  ! Pass the values of nx_rivers and ny_rivers that were read from
  ! jules_rivers_props to the variables in jules_rivers_mod.
  ! Note that these are only needed by the master task.
  !----------------------------------------------------------------------------
  nx_rivers_in = nx_rivers
  ny_rivers_in = ny_rivers

  !----------------------------------------------------------------------------
  ! Check values from the namelist.
  !----------------------------------------------------------------------------
  CALL check_jules_rivers_props( nx_rivers, ny_rivers, l_find_grid,            &
                                 coordinate_file, FILE, riv_number_file,       &
                                 l_calc_rivers_length )

  !----------------------------------------------------------------------------
  ! Set up the list of required ancillary variables.
  !----------------------------------------------------------------------------
  ! Add variables that depend only on the chosen routing model.
  SELECT CASE ( i_river_vn )

  CASE ( rivers_trip )

    nvars_required = 2
    required_vars(1:nvars_required) = [ 'direction', 'sequence ' ]

  CASE ( rivers_rfm )

    nvars_required = 1
    required_vars(1:nvars_required) = [ 'direction' ]

  CASE DEFAULT
    CALL log_fatal( RoutineName,                                               &
                    "Do not recognise i_river_vn = '"                       // &
                    TRIM(to_string(i_river_vn)) // "'" )
  END SELECT

  ! Add required variables that depend on the coordinates used.
  ! If the river grid is not a lat-lon grid, we need lat and lon variables.
  ! These are needed for output files, and for calculating gridbox areas
  ! under some configurations. The river grid is defined using the same
  ! coordinate system (type) as the main model grid, hence we test using
  ! l_coord_latlon here.
  IF ( .NOT. l_coord_latlon ) THEN
    nvars_required = nvars_required + 2
    required_vars(nvars_required-1:nvars_required) =                           &
      [ 'latitude_2d ', 'longitude_2d' ]
  END IF

  ! Add variables that are required by some configurations.
  IF ( i_river_vn == rivers_rfm .AND. l_use_area ) THEN
    ! Add upstream drainage area.
    nvars_required = nvars_required + 1
    required_vars(nvars_required) = 'area'
  END IF

  !----------------------------------------------------------------------------
  ! Check that variables in the namelist have been provided and set up other
  ! variables.
  !----------------------------------------------------------------------------
  CALL check_namelist_values( nvars, nvars_max, nvars_required,                &
                              RoutineName, FILE, const_val,                    &
                              use_file, required_vars, tpl_name, var,          &
                              nvars_const, nvars_file, l_have_template,        &
                              use_var, file_name, var_name )

  !----------------------------------------------------------------------------
  ! Set up the regular routing input grid.
  !----------------------------------------------------------------------------
  use_sub_local = use_subgrid
  use_subgrid   = .FALSE.

  ! Temporarily copy saved grid (set for full model grid) to a local variable
  ! before overwriting to define the river routing grid
  local_grid = grid_create( dummy_grid%is_1d, dummy_grid%dim_name,             &
                            dummy_grid%nx, dummy_grid%x_name, dummy_grid%nx,   &
                            dummy_grid%y_name, dummy_grid%ny )

  ! Get the name of the file to be used to read coordinates. This depends on
  ! whether variable-name templating is used. We have already checked that a
  ! file name was provided.

  ! Establish if templating is specified.
  IF ( tpl_has_var_name(FILE) ) THEN
    ! We use a separate variable to get the file name.
    ! We have previously checked that a file name was provided.
    file_name_coords = coordinate_file
  ELSE
    ! Use the only ancillary file.
    file_name_coords = FILE
    ! If another file was indicated, clarify that it will not be used.
    IF ( LEN_TRIM(coordinate_file) > 0 ) THEN
      CALL log_info( RoutineName,                                              &
                     "No templating; coordinate_file will be ignored.")
    END IF
  END IF

  !----------------------------------------------------------------------------
  ! Read y dimension from file.
  od_grid = grid_create( grid_is_1d = .TRUE.,                                  &
                         dim_name = TRIM(y_dim_name), npoints = ny_rivers )
  ALLOCATE(rivers_data%rivers_ygrid(ny_rivers), STAT = ERROR)
  rivers%rivers_ygrid => rivers_data%rivers_ygrid
  CALL fill_variables_from_file(file_name_coords,                              &
                                [ 'rivers_ygrid' ],[ TRIM(y_dim_name) ])

  ! Read x dimension from file.
  od_grid = grid_create( grid_is_1d = .TRUE.,                                  &
                         dim_name = TRIM(x_dim_name), npoints = nx_rivers)
  ALLOCATE(rivers_data%rivers_xgrid(nx_rivers), STAT = ERROR)
  rivers%rivers_xgrid => rivers_data%rivers_xgrid
  CALL fill_variables_from_file(file_name_coords,                              &
                                [ 'rivers_xgrid' ],[ TRIM(x_dim_name) ])

  !----------------------------------------------------------------------------
  ! Build the routing grid object from the namelist values
  !----------------------------------------------------------------------------
  dummy_grid = grid_create( .FALSE., "", 0, x_dim_name, nx_rivers, y_dim_name, &
                            ny_rivers)

  !----------------------------------------------------------------------------
  ! Allocate gridded river ancillary variables.
  !----------------------------------------------------------------------------
  CALL allocate_river_vars_grid( global_land_pts, nx_rivers, ny_rivers,        &
                                 rivers, rivers_data )

  !----------------------------------------------------------------------------
  ! Set variables that are using a constant value.
  !----------------------------------------------------------------------------
  CALL set_constant_vars( nvars, nvars_const, nvars_max, const_val,            &
                          use_file, use_var, var )

  !----------------------------------------------------------------------------
  ! Set variables that are to be read from a file.
  !----------------------------------------------------------------------------
  CALL set_file_vars( nvars, nvars_file, nvars_max, l_have_template,           &
                      RoutineName, use_file, use_var, file_name, var, var_name)

  !----------------------------------------------------------------------------
  ! If required, read river number ancillary information from file.
  !----------------------------------------------------------------------------
  IF ( l_riv_number ) THEN
    CALL read_ancillaries_coupling( riv_number_file, rivers, rivers_data )
  END IF

END IF  !  is_master_task

!-----------------------------------------------------------------------------
! Read overbank properties namelists if required - so that any required
! ancillary fields are read from file before translating to the river routing
! points.
!-----------------------------------------------------------------------------
IF ( l_riv_overbank ) THEN
  CALL init_overbank_props()
END IF

!------------------------------------------------------------------------------
! Process the ancillary fields to identify river points and populate fields
! on river points.
!------------------------------------------------------------------------------
CALL process_rivers_data( l_calc_rivers_length, l_find_grid,                   &
                          l_ignore_ancil_rivers_check, l_riv_number,           &
                          l_use_area, rivers, rivers_data )

IF ( is_master_task() ) THEN

  !----------------------------------------------------------------------------
  ! Calculate derived variables for inundation.
  !----------------------------------------------------------------------------
  IF ( l_riv_overbank .AND. overbank_model == overbank_simple_rosgen ) THEN
    CALL init_rosgen_vars( rivers )
  END IF

  !----------------------------------------------------------------------------
  ! Reset saved JULES grid from river to full land model
  !----------------------------------------------------------------------------
  dummy_grid = grid_create(local_grid%is_1d,local_grid%dim_name,               &
                           local_grid%nx,local_grid%x_name,local_grid%nx,      &
                           local_grid%y_name,local_grid%ny)
  use_subgrid = use_sub_local

END IF  !  is_master_task

RETURN
END SUBROUTINE init_rivers_props

!##############################################################################

SUBROUTINE check_jules_rivers_props( nx_rivers, ny_rivers, l_find_grid,        &
                                     coordinate_file, FILE, riv_number_file,   &
                                     l_calc_rivers_length )

USE jules_rivers_mod, ONLY: i_river_vn, land_dx, land_dy, l_riv_overbank,      &
                            nx_land_grid, ny_land_grid, x1_land_grid,          &
                            y1_land_grid, rivers_length, l_outflow_per_river,  &
                            rivers_regrid, rivers_rfm

USE model_grid_mod, ONLY: l_coord_latlon

USE overbank_inundation_mod, ONLY: overbank_simple, overbank_simple_rosgen,    &
                                   overbank_model

USE templating_mod, ONLY: tpl_has_var_name

USE jules_print_mgr, ONLY: jules_message

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  nx_rivers, ny_rivers
    ! The sizes of the x and y dimensions of the river routing input grid.

LOGICAL, INTENT(IN) ::                                                         &
  l_find_grid
    ! Switch controlling how characteristics of the land and rivers grids are
    ! determined.

CHARACTER(LEN=*), INTENT(IN) ::                                                &
  coordinate_file,                                                             &
    ! The name of the file given for reading coordinate values from.
  FILE,                                                                        &
    ! The name of the file to be used to read data from.
  riv_number_file
    ! The name of the file given for reading river number
    ! at each grid point

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN OUT)
!------------------------------------------------------------------------------
LOGICAL, INTENT(IN OUT) ::                                                     &
  l_calc_rivers_length
    ! Flag indicating if rivers_length will be calculated.

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'CHECK_JULES_RIVERS_PROPS'

!end of header
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Check that a file name was provided.
!------------------------------------------------------------------------------
IF ( LEN_TRIM(FILE) == 0 ) THEN
  CALL log_fatal( RoutineName, "No name given for file." )
END IF

! If templating is specified, check we have a coordinate_file.
IF ( tpl_has_var_name(FILE) .AND. LEN_TRIM(coordinate_file) == 0 ) THEN
  CALL log_fatal( RoutineName, "No name given for coordinate_file." )
END IF

!------------------------------------------------------------------------------
! Regridding is only supported for (regular) lat-lon grids.
! The regularity is checked later.
!------------------------------------------------------------------------------
IF ( rivers_regrid .AND. .NOT. l_coord_latlon ) THEN
  CALL log_fatal( RoutineName,                                                 &
                 "Regridding can only be used with lat-lon grids." )
END IF

!------------------------------------------------------------------------------
! Insist on a river grid that is at least 2 x 2. This is required for the
! calculations of rivers_dx and rivers_dy.
!------------------------------------------------------------------------------
IF ( nx_rivers < 2  .OR. ny_rivers < 2 ) THEN
  CALL log_fatal( RoutineName,                                                 &
                  "Routing grid must be at least 2x2. Dimensions provided: " //&
                  "nx_rivers = " // TRIM(to_string(nx_rivers))               //&
                  "ny_rivers = " // TRIM(to_string(ny_rivers)) )
END IF

!------------------------------------------------------------------------------
! land_dy and land_dx are required, and should be >0.
!------------------------------------------------------------------------------
IF ( ABS( land_dy - rmdi ) < EPSILON(1.0) .OR.                                 &
     ABS( land_dx - rmdi ) < EPSILON(1.0) ) THEN
  CALL log_fatal( RoutineName,                                                 &
                  "land_dy and land_dx must be provided." )
ELSE IF ( land_dx <= 0.0 .OR. land_dy <= 0.0 ) THEN
  CALL log_fatal( RoutineName,                                                 &
                  "land_dy and land_dx must be >0." )
END IF

!------------------------------------------------------------------------------
! Conditions for nx_land_grid, ny_land_grid, x1_land_grid and y1_land_grid.
!------------------------------------------------------------------------------
IF ( .NOT. l_find_grid ) THEN
  ! Values shoud be supplied. Note that we later test that nx_land_grid and
  ! ny_land_grid are large enough.
  IF ( nx_land_grid == imdi .OR. ny_land_grid == imdi .OR.                     &
       ABS( y1_land_grid - rmdi ) < EPSILON(1.0)  .OR.                         &
       ABS( x1_land_grid - rmdi ) < EPSILON(1.0) ) THEN
    CALL log_fatal( RoutineName,                                               &
                    "nx_land_grid, ny_land_grid, x1_land_grid and "         // &
                    "y1_land_grid must be provided for this configuration." )
  ELSE IF ( nx_land_grid <= 0 .OR. ny_land_grid <= 0 ) THEN
    CALL log_fatal( RoutineName,                                               &
                    "nx_land_grid and ny_land_grid must be >0.")
  END IF
ELSE
  ! l_find_grid = .TRUE.
  ! Values are not required as they will be calculated.
  IF ( nx_land_grid /= imdi .OR. ny_land_grid /= imdi .OR.                     &
       ABS( y1_land_grid - rmdi ) > EPSILON(1.0)  .OR.                         &
       ABS( x1_land_grid - rmdi ) > EPSILON(1.0) ) THEN
    CALL log_fatal( RoutineName,                                               &
                    "nx_land_grid, ny_land_grid, x1_land_grid and "         // &
                    "y1_land_grid will be calculated and must not be "      // &
                    "provided for this configuration." )
  END IF
END IF  !  l_find_grid

!------------------------------------------------------------------------------
! Conditions for rivers_length.
!------------------------------------------------------------------------------
IF ( .NOT. l_coord_latlon ) THEN
  ! We don't have a latitude-longitude grid, and so must have rivers_length>0
  ! because that is used to calculate box areas.
  IF ( ABS( rivers_length - rmdi ) < EPSILON(1.0) .OR.                         &
       rivers_length <= 0.0 ) THEN
    CALL log_fatal( RoutineName,                                               &
                    "rivers_length > 0 must be provided for this "          // &
                    "configuration." )
  END IF
END IF

! Note that this check requires that we have read the jules_overbank namelist.
IF ( i_river_vn == rivers_rfm .OR.                                             &
     ( l_riv_overbank .AND. ( overbank_model == overbank_simple .OR.           &
                              overbank_model == overbank_simple_rosgen ) )     &
   ) THEN
  ! These configurations require rivers_length. A value <=0 will later trigger
  ! calculation of rivers_length.
  IF ( ABS( rivers_length - rmdi ) < EPSILON(1.0) ) THEN
    CALL log_fatal( RoutineName,                                               &
                    "rivers_length must be provided for this configuration." )
  ELSE IF ( rivers_length <= 0.0 ) THEN
    ! A non-missing value has been provided but it is <= 0, hence a new value
    ! will be calculated - this is so as to mimic the behaviour of earlier
    ! versions of the code. Note that we will only get here for grids that are
    ! using lat and lon coords, because non-latlon grids were tested earlier
    ! to ensure rivers_length > 0.
    ! Set a switch to trigger later calculation of rivers_length.
    l_calc_rivers_length = .TRUE.
    CALL log_info( RoutineName,                                                &
                   "rivers_length <= 0: value will be calculated from grid." )
  END IF
END IF

!-----------------------------------------------------------------------------
! Checks for outflow_per_river diagnostic, which requires the river number
! ancillary
!-----------------------------------------------------------------------------
! OASIS-Rivers:
!   l_outflow_per_river is set before now by check_oasis_rivers and is
!   true when outflow_per_river is present in send_fields; requires the
!   riv_number ancillary.
IF ( l_outflow_per_river ) THEN
  WRITE(jules_message,*)                                                       &
     "OASIS send field outflow_per_river has been requested."
  CALL log_info(RoutineName, jules_message)
  IF ( LEN_TRIM(riv_number_file) == 0 ) THEN
    CALL log_fatal(RoutineName, "No name given for riv_number_file.")
  END IF
END IF

IF ( LEN_TRIM(riv_number_file) > 0 ) THEN
  l_riv_number = .TRUE.
  WRITE(jules_message,*) "riv_number_file = ", TRIM(riv_number_file)
  CALL log_info(RoutineName,jules_message)
  ! Diagnostic output is requested later.
  IF ( .NOT. l_outflow_per_river ) THEN
    WRITE(jules_message,*)                                                     &
       "riv_number_file detected; " //                                         &
       "assume that outflow_per_river diagnostic will be requested."
    CALL log_info(RoutineName, jules_message)
    l_outflow_per_river = .TRUE.
  END IF
END IF

RETURN
END SUBROUTINE check_jules_rivers_props

!##############################################################################

SUBROUTINE allocate_river_vars_grid( global_land_pts, nx_rivers, ny_rivers,    &
                                     rivers, rivers_data )

!------------------------------------------------------------------------------
! Description:
!   Allocate gridded river ancillary variables, initialise, and associate
!   pointers.
!------------------------------------------------------------------------------

USE jules_rivers_mod, ONLY: i_river_vn, rivers_rfm, rivers_trip,               &
                            rivers_data_type, rivers_type

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  global_land_pts,                                                             &
    ! Number of land points.
  nx_rivers, ny_rivers
    ! Grid sizes.

!------------------------------------------------------------------------------
! Arguments with INTENT(IN OUT)
!------------------------------------------------------------------------------
TYPE(rivers_type), INTENT(IN OUT) :: rivers
TYPE(rivers_data_type), INTENT(IN OUT), TARGET :: rivers_data

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'ALLOCATE_RIVER_VARS_GRID'

INTEGER ::                                                                     &
   ERROR, error_sum,                                                           &
     ! Error values.
   nx_size, ny_size
     ! Sizes used for allocations.

!end of header
!------------------------------------------------------------------------------

! This routine is only called by the master task. All variables are allocated,
! but they are only allocated at full size when required by the run
! configuration.

! Variables that are always required.
ALLOCATE( grid_riv_pt_number(nx_rivers, ny_rivers), STAT = ERROR )
error_sum = ERROR
ALLOCATE( nextx_grid(nx_rivers, ny_rivers), STAT = ERROR )
error_sum = error_sum + ERROR
ALLOCATE( nexty_grid(nx_rivers, ny_rivers), STAT = ERROR )
error_sum = error_sum + ERROR
ALLOCATE( direction_grid(nx_rivers, ny_rivers), STAT = ERROR )
error_sum = error_sum + ERROR
ALLOCATE( river_mask(nx_rivers,ny_rivers), STAT = ERROR )
error_sum = error_sum + ERROR
ALLOCATE(rivers_data%rivers_dir(nx_rivers, ny_rivers), STAT = ERROR )
error_sum = error_sum + ERROR
ALLOCATE(rivers_data%rivers_lat2d(nx_rivers, ny_rivers), STAT = ERROR )
error_sum = error_sum + ERROR
ALLOCATE(rivers_data%rivers_lon2d(nx_rivers, ny_rivers), STAT = ERROR )
error_sum = error_sum + ERROR

! A remapping variable.
ALLOCATE(rivers_data%global_land_index(global_land_pts), STAT = ERROR )
error_sum = error_sum + ERROR

! RFM variables.
IF ( i_river_vn == rivers_rfm ) THEN
  nx_size = nx_rivers
  ny_size = ny_rivers
ELSE
  nx_size = 1
  ny_size = 1
END IF
ALLOCATE(rivers_data%rivers_dra(nx_size,ny_size), STAT = ERROR )
error_sum = error_sum + ERROR

! TRIP variables.
IF ( i_river_vn == rivers_trip ) THEN
  nx_size = nx_rivers
  ny_size = ny_rivers
ELSE
  nx_size = 1
  ny_size = 1
END IF
ALLOCATE(rivers_data%rivers_seq(nx_size,ny_size), STAT = ERROR )
error_sum = error_sum + ERROR

! River coupling variables.
IF ( l_riv_number ) THEN
  nx_size = nx_rivers
  ny_size = ny_rivers
ELSE
  nx_size = 1
  ny_size = 1
END IF
ALLOCATE( rivers_data%rivers_outflow_number(nx_size,ny_size), STAT = ERROR )
error_sum = error_sum + ERROR

IF ( error_sum /= 0 ) THEN
  CALL log_fatal( RoutineName, "Error allocating for rivers arrays." )
END IF

! Initialise to missing data.
grid_riv_pt_number(:,:)       = imdi
nextx_grid(:,:)               = imdi
nexty_grid(:,:)               = imdi
direction_grid(:,:)           = imdi
river_mask(:,:)               = imdi
rivers_data%rivers_dir(:,:)   = rmdi
rivers_data%rivers_lat2d(:,:) = rmdi
rivers_data%rivers_lon2d(:,:) = rmdi
rivers_data%global_land_index(:) = imdi
rivers_data%rivers_dra(:,:)   = rmdi
rivers_data%rivers_seq(:,:)   = rmdi
rivers_data%rivers_outflow_number(:,:) = rmdi

! Associate pointers
rivers%rivers_dir   => rivers_data%rivers_dir
rivers%rivers_lat2d => rivers_data%rivers_lat2d
rivers%rivers_lon2d => rivers_data%rivers_lon2d
rivers%global_land_index => rivers_data%global_land_index
rivers%rivers_dra => rivers_data%rivers_dra
rivers%rivers_seq => rivers_data%rivers_seq
rivers%rivers_outflow_number => rivers_data%rivers_outflow_number

RETURN
END SUBROUTINE allocate_river_vars_grid

!##############################################################################

SUBROUTINE process_rivers_data( l_calc_rivers_length, l_find_grid,             &
                                l_ignore_ancil_rivers_check, l_riv_number,     &
                                l_use_area, rivers, rivers_data )

!------------------------------------------------------------------------------
! Description:
!   Process river ancillary data to produce variables on river points.
!------------------------------------------------------------------------------

USE ancil_info, ONLY: land_pts

USE conversions_mod, ONLY: pi_over_180

USE init_ancillaries_coupling_mod, ONLY: check_ancil_rivers

USE init_overbank_props_mod, ONLY: allocate_overbank_vars_rp

USE jules_model_environment_mod, ONLY: l_oasis_rivers

USE jules_rivers_mod, ONLY:                                                    &
  l_riv_overbank, l_trivial_mapping, np_rivers, nx_rivers, ny_rivers,          &
  rivers_dx, rivers_dy, rivers_length, rivers_reglatlon, rivers_regrid,        &
  rivers_x1, rivers_y1,                                                        &
  ! types
  rivers_data_type, rivers_type

USE model_grid_mod, ONLY: global_land_pts, projection_x_coord_land,            &
                          projection_y_coord_land

USE parallel_mod, ONLY: gather_land_field, is_master_task

USE planet_constants_mod, ONLY: planet_radius

USE rivers_regrid_mod, ONLY: calc_map_river_to_land_points

USE rivers_utils, ONLY: rivers_earth_area

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN)
!------------------------------------------------------------------------------
LOGICAL, INTENT(IN) ::                                                         &
  l_calc_rivers_length,                                                        &
    ! Flag indicating if rivers_length will be calculated.
  l_find_grid,                                                                 &
    ! Switch controlling how characteristics of the land and rivers grids are
    ! determined.
  l_ignore_ancil_rivers_check,                                                 &
    ! Switch to check river routing & river number ancillary for compatibility.
  l_riv_number,                                                                &
    ! Flag indicating if river number ancillary has been specified.
  l_use_area
    ! Switch (used with RFM) to use a drainage area ancillary field to identify
    ! river points.

!------------------------------------------------------------------------------
! Arguments with INTENT(IN OUT)
!------------------------------------------------------------------------------
TYPE(rivers_type), INTENT(IN OUT) :: rivers
TYPE(rivers_data_type), INTENT(IN OUT), TARGET :: rivers_data

!------------------------------------------------------------------------------
! Local parameters.
!------------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'PROCESS_RIVERS_DATA'

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  ERROR,                                                                       &
    ! An error value
  error_sum,                                                                   &
    ! Accumulated error values.
  ip, ix, iy, l
    ! Index variables

REAL(KIND=real_jlslsm) ::                                                      &
  half_dlat, half_dlon,                                                        &
    ! Sizes of half a gridbox (degrees)
  rivers_x1_input
    ! The first value of the x coordinate of the river input grid (i.e.
    ! rivers_xgrid(1), after any conversion of longitude values).

LOGICAL ::                                                                     &
  l_shift_x,                                                                   &
    ! Flag indicating that a cyclic shift is to be applied in the x direction
    ! to river ancillaries. This can be TRUE only if the coordinate is
    ! longitude.
  l_cyclic_x,                                                                  &
    ! Flag indicating a river grid that is cyclic (global) in the x direction.
  l_reverse_y
    ! Flag indicating if the order in the y direction of ancillary fields is
    ! to be reversed so that the coordinate is monotonically increasing.

!------------------------------------------------------------------------------
! Local array variables.
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
  global_proj_x_land(:),                                                       &
    ! x-coordinates of land points.
  global_proj_y_land(:)
    ! y-coordinates of land points.

REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
  global_proj_x_land_work(:)
    ! x coordinates of land points. In many circumstances this is simply a copy
    ! of global_proj_x_land, but in others the values are transformed to allow
    ! smaller grids to be used.

REAL(KIND=real_jlslsm) ::                                                      &
  river_xbound(2), river_ybound(2)
    ! Coordinates of the corners of the river domain - i.e. that part of the
    ! river input grid that will be searched for river points. Units as given
    ! by rivers_xgrid and rivers_ygrid.

!end of header
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Overview of this subroutine:
! 1. Call process_rivers_coords to calculate values related to grid coordinates
!    and the area that will be searched for river points (the river domain).
! 2. Call remap_ancil_control if necessary to remap from river input grid to
!    river grid
! 3. Call process_direction_fields to convert flow direction to nextx and nexty
!    indices.
! 4. Call find_river_points to search the river domain and establish the number
!    of river points.
! 5. Call check_ancil_rivers if required, to check for consistency between
!    river direction and river number fields.
! 6. Allocate river point arrays.
! 7. Call set_river_point_values to populate the river point arrays using
!    values from the river input grid.
! 8. Call find_downstream_points to identify points downstream of others.
! 9. Calculate river grid mappings.
!
! Step 6 is done on all tasks (though only the master task allocates at full
! size), all other steps are only necessary on the master task (because rivers
! are run on that).
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Gather global land coordinate values from all tasks onto the master task.
!------------------------------------------------------------------------------
ALLOCATE(global_proj_x_land(global_land_pts), STAT = ERROR )
error_sum = ERROR
ALLOCATE(global_proj_x_land_work(global_land_pts), STAT = ERROR )
error_sum = error_sum + ERROR
ALLOCATE(global_proj_y_land(global_land_pts), STAT = ERROR )
error_sum = error_sum + ERROR
IF ( error_sum /= 0 ) THEN
  CALL log_fatal( RoutineName, "Error allocating global lat/lon arrays." )
END IF
! Initialise.
global_proj_x_land(:) = 0.0
global_proj_y_land(:) = 0.0
! Gather values onto master task.
CALL gather_land_field( projection_x_coord_land, global_proj_x_land )
CALL gather_land_field( projection_y_coord_land, global_proj_y_land )

IF ( is_master_task() ) THEN

  !----------------------------------------------------------------------------
  ! Process coordinates and establish the extent of the river domain.
  !----------------------------------------------------------------------------
  CALL process_rivers_coords( l_find_grid, global_proj_x_land,                 &
             global_proj_y_land, rivers%rivers_ygrid, rivers%rivers_xgrid,     &
             rivers_dx,                                                        &
             rivers_dy, rivers_x1, rivers_x1_input, rivers_y1, l_shift_x,      &
             l_cyclic_x, l_reverse_y, rivers_reglatlon,                        &
             rivers%global_land_index,                                         &
             global_proj_x_land_work, river_xbound, river_ybound )

  !----------------------------------------------------------------------------
  ! If necessary remap from river input grid to river grid. This handles river
  ! routing and overbank inundation variables.
  !----------------------------------------------------------------------------
  IF ( l_shift_x .OR. l_reverse_y ) THEN
    CALL remap_ancil_control( nx_rivers, ny_rivers, rivers_dx, l_shift_x,      &
                              l_reverse_y, rivers )
  END IF

  !----------------------------------------------------------------------------
  ! Convert the flow direction field into nextx and nexty fields.
  !----------------------------------------------------------------------------
  CALL process_direction_fields( l_cyclic_x, rivers%rivers_dir,                &
                                 direction_grid,                               &
                                 nextx_grid, nexty_grid, river_mask )

  ! Comments below discriminate between river locations and river points.
  ! River locations are any land point on the river grid.
  ! River points are the subset of river locations that are modelled as rivers,
  ! i.e. they are river locations within the valid river domain.
  !
  ! At this point all locations on the river grid fall into exactly one of the
  ! following categories:
  !
  !  river locations that have a physical flow direction (that doesn't involve
  !     flow across the edge of the grid):
  !     nextx_grid is in [1:nx_rivers]
  !     river_mask = land_off_domain
  !  river locations that flow across the edge of the grid:
  !     nextx_grid is in [-8:-1]
  !     river_mask = land_off_domain
  !  river locations that are mouths and inland drainage points:
  !     nextx_grid is in [river_mouth, inland_drainage]
  !     river_mask = land_off_domain
  !  sea points:
  !     nextx_grid = sea
  !     river_mask = sea

  !----------------------------------------------------------------------------
  ! Establish the number and locations of river points.
  !----------------------------------------------------------------------------
  CALL find_river_points( rivers%rivers_xgrid, rivers%rivers_ygrid,            &
                          river_xbound, river_ybound,                          &
                          direction_grid, nextx_grid, nexty_grid,              &
                          river_mask, np_rivers )

  ! direction_grid, nextx_grid and nexty_grid are not changed after this point.
  !
  ! At this point all locations on the river grid fall into exactly one of the
  ! following categories:
  !
  !  river points that have a physical flow direction (that doesn't involve
  !     flow across the edge of the grid):
  !     nextx_grid is in [1:nx_rivers]
  !     river_mask = land_in_domain
  !  river points that flow across the edge of the grid):
  !     nextx_grid is in [-8:-1]
  !     river_mask = land_in_domain
  !  river points that are mouths or inland drainage points:
  !     nextx_grid = river_mouth or inland_drainage
  !     river_mask = river_mouth or inland_drainage
  !  river locations that are not in the domain (hence are not river points):
  !     nextx_grid = land_off_domain
  !     river_mask = land_off_domain
  !  sea points:
  !     nextx_grid = sea
  !     river_mask = sea

  CALL log_info( RoutineName,                                                  &
                 "River routing points = " // TRIM(to_string(np_rivers)) )
  CALL log_info( RoutineName,                                                  &
                 "Global land pts = " // TRIM(to_string(global_land_pts)) )

  IF ( np_rivers == 0 ) THEN
    CALL log_fatal( RoutineName,                                               &
                    "No river points found. Check inputs." )
  END IF

  !----------------------------------------------------------------------------
  ! If required, check river number ancillary field.
  ! We have waited until after find_river_points is called as that subroutine
  ! can introduce additional river mouths.
  !----------------------------------------------------------------------------
  IF ( l_riv_number ) THEN
    CALL check_ancil_rivers( dir_mouth, l_ignore_ancil_rivers_check,           &
                             direction_grid, rivers, rivers_data )
  END IF

END IF  ! is_master

!------------------------------------------------------------------------------
! Allocate routing point arrays.
!------------------------------------------------------------------------------
CALL allocate_rivers_vars_rp( np_rivers, rivers, rivers_data )

!------------------------------------------------------------------------------
! Allocate overbank inundation variables.
!------------------------------------------------------------------------------
CALL allocate_overbank_vars_rp( land_pts, np_rivers )

IF ( is_master_task() ) THEN

  !----------------------------------------------------------------------------
  ! Set values at river points.
  !----------------------------------------------------------------------------
  CALL set_river_point_values( rivers_x1_input, l_use_area, direction_grid,    &
                               river_mask, rivers, grid_riv_pt_number )

  !----------------------------------------------------------------------------
  ! Identify downstream points.
  !----------------------------------------------------------------------------
  CALL find_downstream_points( direction_grid, nextx_grid, nexty_grid,         &
                               rivers%rivers_index_rp, river_mask,             &
                               rivers%rivers_next_rp)

  ! At this point all values of rivers%rivers_next_rp are one of the following:
  !   >0              : there is a downstream river point (in the river points
  !                     vector)
  !   river_mouth     : a river mouth
  !   inland_drainage : an inland drainage point
  !   -8:-1           : the downstream location is not a river point - because
  !                     it is outside of the valid river domain (and possibly
  !                     off the river grid).

  ! Note that the science code could use rivers%rivers_next_rp values in -8:-1
  ! to sum flow across the edge of the domain, e.g. to check conservation of
  ! mass. That is not currently coded.

  !----------------------------------------------------------------------------
  ! Calculate river length scale and gridbox areas.
  !----------------------------------------------------------------------------
  IF ( l_calc_rivers_length ) THEN
    ! Calculate length scale from grid parameters. Here rivers_dy will be the
    ! gridbox size (degrees), because l_calc_rivers_length can be TRUE only
    ! for lat-lon grids.
    rivers_length = planet_radius * ( ABS( rivers_dy ) * pi_over_180 )
  END IF

  IF ( rivers_reglatlon ) THEN
    half_dlat = 0.5 * rivers_dy
    half_dlon = 0.5 * rivers_dx
    DO ip = 1,np_rivers
      rivers%rivers_boxareas_rp(ip) =                                          &
        ABS( rivers_earth_area( rivers%rivers_lat_rp(ip) - half_dlat,          &
                                rivers%rivers_lat_rp(ip) + half_dlat,          &
                                rivers%rivers_lon_rp(ip) - half_dlon,          &
                                rivers%rivers_lon_rp(ip) + half_dlon ) )
    END DO
  ELSE
    ! Note that this assumes that grids that are not using lat-lon coordinates
    ! are equal area grids - i.e. all gridboxes have the same area.
    rivers%rivers_boxareas_rp(:) = rivers_length * rivers_length
  END IF

  IF ( .NOT. l_oasis_rivers ) THEN
    !--------------------------------------------------------------------------
    ! Calculate mapping ahead of main run.
    !--------------------------------------------------------------------------
    IF ( .NOT. rivers_regrid ) THEN
      ! Find land points that are coincident with river points.
      ! For large grids, this can be slow, hence we issue messages to keep the
      ! user informed.
      CALL log_info( RoutineName,                                              &
                     "Searching for matching river and land points." )
      CALL calc_map_river_to_land_points( global_land_pts,                     &
                                 global_proj_x_land_work, global_proj_y_land,  &
                                 rivers%rivers_x_coord_rp,                     &
                                 rivers%rivers_y_coord_rp,                     &
                                 rivers%map_river_to_land_points )
      CALL log_info( RoutineName, "Search complete." )

      ! Note that if the mapping between land and river points is trivial
      ! (indicated by l_trivial_mapping=T) we could deallocate
      ! map_river_to_land_points at this point - but that has not beed coded.

    END IF  !  rivers_regrid

    !--------------------------------------------------------------------------
    ! Calculate diagnostics of grid coverage.
    !--------------------------------------------------------------------------
    CALL check_river_coverage( rivers%map_river_to_land_points,                &
                               global_proj_x_land_work, global_proj_y_land,    &
                               rivers%rivers_x_coord_rp,                       &
                               rivers%rivers_y_coord_rp )

  END IF  !  .NOT. l_oasis_rivers

END IF !  is_master_task

!------------------------------------------------------------------------------
! Dellocate local arrays.
!------------------------------------------------------------------------------
! Variables that are allocated on all tasks.
DEALLOCATE( global_proj_x_land )
DEALLOCATE( global_proj_x_land_work )
DEALLOCATE( global_proj_y_land )

! Variables that are only allocated on master task.
IF ( ALLOCATED(direction_grid) ) THEN
  DEALLOCATE( direction_grid )
END IF
IF ( ALLOCATED(grid_riv_pt_number) ) THEN
  DEALLOCATE( grid_riv_pt_number )
END IF
IF ( ALLOCATED(nextx_grid) ) THEN
  DEALLOCATE( nextx_grid )
END IF
IF ( ALLOCATED(nexty_grid) ) THEN
  DEALLOCATE( nexty_grid )
END IF
IF ( ALLOCATED(river_mask) ) THEN
  DEALLOCATE( river_mask )
END IF

RETURN
END SUBROUTINE process_rivers_data

!##############################################################################

SUBROUTINE process_rivers_coords( l_find_grid, global_proj_x_land,             &
             global_proj_y_land, rivers_ygrid, rivers_xgrid, rivers_dx,        &
             rivers_dy, rivers_x1, rivers_x1_input, rivers_y1, l_shift_x,      &
             l_cyclic_x, l_reverse_y, rivers_reglatlon, global_land_index,     &
             global_proj_x_land_work, river_xbound, river_ybound )

!------------------------------------------------------------------------------
! Description:
!   Process river coordinates, calculating characteristics and checking that
!   grids make sense.
!------------------------------------------------------------------------------

USE jules_rivers_mod, ONLY:                                                    &
  nx_land_grid, nx_rivers, ny_land_grid, ny_rivers, land_dx, land_dy,          &
  rivers_regrid, rivers_type, x1_land_grid, y1_land_grid

USE model_grid_mod, ONLY:                                                      &
  global_land_pts, l_coord_latlon

USE rivers_regrid_mod, ONLY: calc_grid_index

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN)
!------------------------------------------------------------------------------
LOGICAL, INTENT(IN) ::                                                         &
  l_find_grid
    ! Switch controlling how characteristics of the land and rivers grids are
    ! determined.

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  global_proj_x_land(global_land_pts), global_proj_y_land(global_land_pts),    &
    ! Coordinates of land points.
  rivers_ygrid(ny_rivers)
   ! Coordinate values for y-dimension of rivers grid

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN OUT)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  rivers_xgrid(nx_rivers)
    ! Coordinate values for x-dimension of rivers grid.

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(OUT)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  rivers_dx,                                                                   &
    ! Size of gridbox of rivers grid in x direction.
    ! If l_coord_latlon=T, the units are degrees of longitude, otherwise units
    ! are unknown.
  rivers_dy,                                                                   &
    ! Size of gridbox of rivers grid in y direction.
    ! If l_coord_latlon=T, the units are degrees of latitude, otherwise units
    ! are unknown.
  rivers_x1,                                                                   &
    ! x coordinate of of westernmost (first) column of gridpoints on the
    ! rivers grid.
  rivers_x1_input,                                                             &
    ! The first value of the x coordinate of the river input grid.
  rivers_y1
    ! y coordinate of "southernmost" (first) row of gridpoints on the rivers
    ! grid.

LOGICAL, INTENT(OUT) ::                                                        &
  l_shift_x,                                                                   &
    ! Flag indicating that a cyclic shift is to be applied in the x direction
    ! to river ancillaries. This can be TRUE only if the coordinate is
    ! longitude.
  l_cyclic_x,                                                                  &
    ! Flag indicating a river input grid that is cyclic (global) in the x
    ! direction.
  l_reverse_y,                                                                 &
    ! Flag indicating if the order in the y direction of ancillary fields is to
    ! be reversed so that the coordinate is monotonically increasing.
  rivers_reglatlon
    ! Flag indicating if rivers grid is regular in latitude and longitude.

!------------------------------------------------------------------------------
! Array arguments with INTENT(OUT)
!------------------------------------------------------------------------------
INTEGER, INTENT(OUT) ::                                                        &
  global_land_index(global_land_pts)
    ! List of indices for the land grid. For every land point, this gives the
    ! location in a 2-D grid.

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  global_proj_x_land_work(global_land_pts),                                    &
    ! x coordinates of land points, including any transformation.
  river_xbound(2), river_ybound(2)
    ! Coordinates of the corners of the river domain - i.e. that part of the
    ! river input grid that will be searched for river points. Units as given
    ! by rivers_xgrid and rivers_ygrid.

!------------------------------------------------------------------------------
! Local parameters.
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), PARAMETER :: global_lon_threshold = 359.9
  ! Threshold value used when determining if a grid is cyclic (global) in the
  ! longitudinal direction (degrees). This should be close to 360 degrees.

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'PROCESS_RIVERS_COORDS'

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  i, ix, iy, l,                                                                &
    ! Index variables
  n_change
    ! Counter of number of longitudes that have been changed.

LOGICAL ::                                                                     &
  l_land_180,                                                                  &
    ! Flag indicating the range of land point longitudes.
  l_river_180
    ! Flag indicating the range of river longitudes.

!------------------------------------------------------------------------------
! Local array variables.
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  land_index_tmp(global_land_pts)
    ! Local version of global land index.

!-----------------------------------------------------------------------------
! Local array variables.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) :: land_x(2), land_y(2)
  ! Min and max values of the projection coordinates, at land points.

REAL(KIND=real_jlslsm) :: land_xbound(2), land_ybound(2)
  ! Coordinates of the "extreme" land gridpoints (e.g. gridpoints at the
  ! corners of the grid - though exact definition differs between
  ! configurations). Units are those of the land grid projection coords.

!end of header
!------------------------------------------------------------------------------

! Initialise flag to show that we will not apply a cyclic shift to the input
! data (as part of the process of matching the land and river coordinates).
l_shift_x = .FALSE.

!------------------------------------------------------------------------------
! Get river gridbox size.
!------------------------------------------------------------------------------
rivers_dy = ABS( rivers_ygrid(2) - rivers_ygrid(1) )
rivers_dx = ABS( rivers_xgrid(2) - rivers_xgrid(1) )
CALL log_info( RoutineName,                                                    &
               'River gridbox size='                                        // &
               TRIM(to_string(rivers_dx)) // ' x '                          // &
               TRIM(to_string(rivers_dy)) )

!------------------------------------------------------------------------------
! Check if the river grid is regular.
!------------------------------------------------------------------------------
! We assume that we always need a regular river grid, because:
! * if we are regridding, we need regular lat-lon
! * if not regridding (i.e. 1:1 mapping) it is easier if we can use a single
!   length scale to characterise distances and areas.
!
! For each coordinate value we compare with the previous value and flag if
! the separation does not match the expected uniform grid -
! if the difference is a non-negligible fraction of a gridbox.
! Essentially we are checking that this location appears to be on or
! "sufficiently close" to the grid (where "sufficiently close" allows for
! differences from the finite precision of the calculation).
! Note that comparing adjacent values in this way, rather than comparing
! each with an origin, seems to suffer less from issues related to
! precision.

! Check spacings in x are regular.
! We can start at i=3 because 1 and 2 were used to get rivers_dx.
DO i = 3, nx_rivers
  IF ( ABS( ABS( rivers_xgrid(i) -  rivers_xgrid(i-1) )                        &
            - rivers_dx ) / rivers_dx  > frac_toler ) THEN
    CALL log_fatal( RoutineName, "River input grid must be regular (x)." )
  END IF
END DO

! Check spacings in y are regular.
DO i = 3, ny_rivers
  IF ( ABS( ABS( rivers_ygrid(i) -  rivers_ygrid(i-1) )                        &
            - rivers_dy ) / rivers_dy  > frac_toler ) THEN
    CALL log_fatal( RoutineName, "River input grid must be regular (y)." )
  END IF
END DO

IF ( l_coord_latlon ) THEN
  ! This is a regular lat-lon river grid.
  rivers_reglatlon = .TRUE.
  CALL log_info( RoutineName, "River grid is a regular lat-lon grid." )
ELSE
  rivers_reglatlon = .FALSE.
END IF

IF ( .NOT. rivers_reglatlon .AND. rivers_regrid ) THEN
  ! Regridding requires a regular lat-lon grid.
  CALL log_fatal( RoutineName,                                                 &
                  "River grid is not a regular lat-lon grid but regridding" // &
                  "is requested. This is not supported." )
ELSE IF ( .NOT. rivers_regrid ) THEN
  ! Remapping requires grids of the same resolution. Here we test that the
  ! difference does not exceed a given fraction of the land resolution.
  IF ( ABS(rivers_dx - land_dx) > frac_toler * land_dx .OR.                    &
       ABS(rivers_dy - land_dy) > frac_toler * land_dy ) THEN
    CALL log_fatal( RoutineName,                                               &
                    "Land and river grids are not of the same resolution " //  &
                    "but regridding is not requested. This is not supported." )
  END IF
END IF

!------------------------------------------------------------------------------
! Detect if y coordinates are presented in increasing or decreasing order.
! We simply compare the extreme points.
!------------------------------------------------------------------------------
l_reverse_y = .FALSE.
IF ( rivers_ygrid(1) > rivers_ygrid(ny_rivers) ) THEN

  l_reverse_y = .TRUE.

  !----------------------------------------------------------------------------
  ! If l_coord_latlon=T (coordinates are latitude and longitude) this code
  ! includes provision for reading input files that are presented in N to S
  ! order before recasting the data in S to N order. Although we could allow
  ! the same for l_coord_latlon=F, it is simpler to insist that in that case
  ! the coordinates are presented in strictly increasing order.
  !---------------------------------------------------------------------------
  IF ( .NOT. l_coord_latlon ) THEN
    CALL log_fatal( RoutineName,                                               &
                    "l_coord_latlon=F requires that river coordinates "     // &
                    "are presented in strictly increasing order." )
  END IF

END IF  !  rivers_ygrid

!------------------------------------------------------------------------------
! Identify a grid for land points. This also sets other values that are always
! needed, even if details of the grid are not required.
!------------------------------------------------------------------------------
CALL find_land_grid( l_find_grid, global_proj_x_land, global_proj_y_land,      &
                     nx_land_grid, ny_land_grid, x1_land_grid, y1_land_grid,   &
                     l_land_180, global_proj_x_land_work, land_x, land_y )

!------------------------------------------------------------------------------
! Check that longitudes for land and river points are in the same range.
! This is only required if coordinates are latitude and longitude, because of
! the different ranges that are in common use. For other coordinate systems
! it is assumed that land and river grids use the same general range. If land
! and river ranges differ, the river longitudes are changed.
!------------------------------------------------------------------------------
IF ( l_coord_latlon ) THEN
  ! Establish if longitude values of land and river grids are in the same
  ! range. In particular, we want both to either use values in the range
  ! [-180,180] or [0,360]. We assume that the presence of any value < 0
  ! means [-180,180] is used, otherwise it is [0,360]. Note that the longitudes
  ! of land points might have been changed to a new range so as to allow a
  ! smaller land grid.
  IF ( MINVAL( rivers_xgrid ) < 0.0 ) THEN
    l_river_180 = .TRUE.
  ELSE
    l_river_180 = .FALSE.
  END IF

  ! If ranges are different, change river values.
  IF ( l_land_180 .NEQV. l_river_180 ) THEN

    IF ( l_land_180 ) THEN
      ! Land uses [-180,180]. Change river to [-180,180].
      CALL change_longitude_from_360( rivers_xgrid, n_change )
    ELSE
      ! l_river_180=T.
      ! Land uses [0,360]. Change river to [0,360].
      CALL change_longitude_from_180( rivers_xgrid, n_change )
    END IF

    ! Set a flag if some values were changed but others were not - because
    ! this suggests that a cyclic shift in the x direction is required.
    IF ( n_change /= nx_rivers ) THEN
      l_shift_x = .TRUE.
    END IF

  END IF  !  l_land_180 .NEQV. l_river_180

END IF  !  l_coord_latlon

!------------------------------------------------------------------------------
! Now that we have finished manipulating rivers_xgrid, we can save minima.
! Note that rivers_xgrid might no longer be strictly increasing - there could
! be a discontinuity at 180 or 360degE, which is catered for in the code. (It
! is assumed that input coordinates were strictly increasing.)
!------------------------------------------------------------------------------
rivers_x1 = MINVAL( rivers_xgrid )
rivers_y1 = MINVAL( rivers_ygrid )

! Save the first x-coord value of the river input grid.
rivers_x1_input = rivers_xgrid(1)

CALL log_info( RoutineName,"River routing grid: x " //                         &
               TRIM(to_string(MINVAL(rivers_xgrid)))  //  ' to ' //            &
               TRIM(to_string(MAXVAL(rivers_xgrid))) //   ' y '  //            &
               TRIM(to_string(MINVAL(rivers_ygrid))) //   ' to ' //            &
               TRIM(to_string(MAXVAL(rivers_ygrid))) )

!------------------------------------------------------------------------------
! Decide if the river grid is cyclic (global) in the x direction. We can
! only do this reliably for a regular lat-lon grid - otherwise we have to
! assume that it is not cyclic.
!------------------------------------------------------------------------------
l_cyclic_x = .FALSE.
IF ( rivers_reglatlon ) THEN
  IF ( REAL(nx_rivers) * rivers_dx > global_lon_threshold ) THEN
    l_cyclic_x = .TRUE.
    CALL log_info( RoutineName, "River grid is cyclic in longitude." )
  END IF
END IF

!------------------------------------------------------------------------------
! Raise an error if a cyclic shift in x is indicated but the grid is not
! global in longitude.
!------------------------------------------------------------------------------
IF ( l_shift_x .AND. .NOT. l_cyclic_x ) THEN
  CALL log_fatal( RoutineName,                                                 &
                  "A cyclic shift can only be applied to a grid that is "   // &
                  "cyclic (global) in longitude - see code for details. A " // &
                  "possible solution is to use a cyclic (global) river "    // &
                  "input grid.")
END IF

!------------------------------------------------------------------------------
! Identify the coordinate values that delimit the river domain - i.e. the part
! of the river grid that is to be searched for river points.
!
! In general the most satisfactory arrangement is if each land gridbox is
! completely covered by river points, and vice versa. In that case all runoff
! (from the land boxes) will be captured by the rivers, and all river boxes
! have well-defined runoff inputs. However:
! (1) Historically JULES could run with river boxes that did not include land
!     (e.g. a large river domain in which a small land grid is embedded) and
!     we need to be able to reproduce those configurations. The simulated
!     river flow in these river boxes will not be realistic but this
!     configuration could still be considered useful if, e.g., only river
!     points within the land grid are considered and the river network is not
!     missing upstream areas that contribute a significant amount of runoff.
! (2) In general it is non-trivial to reliably establish the overlap between
!     grids which can be of different resolutions.
!------------------------------------------------------------------------------
IF ( l_find_grid ) THEN

  ! In both cases here, the river bounds are related to the extent of the land
  ! grid. This means that if the river input grid is much larger than the land
  ! grid, river points will not be added beyond the land grid. Although this
  ! is reasonable (with no land we can't define inputs to rivers), it is a
  ! restriction that is not present with l_find_grid=F.
  IF ( rivers_regrid ) THEN
    ! We know we are dealing with regular lat-lon grids in this case.
    ! Ideally only want to include river boxes that are completely covered by
    ! land (so that the input runoff can be calculated). However, given that
    ! is relatively difficult to determine in the general case of grids of
    ! different resolution and potentially a domain that includes land and sea
    ! (i.e. we do not have a rectangle that is all land), we instead use a
    ! necessary but not sufficient condition, that the river box falls within
    ! the extremes of the land grid.
    ! Calculate the coordinates (lat-lon) of the outer edges of the land grid
    ! - i.e. outside edges of gridboxes (not centres).
    land_xbound(1) = land_x(1) - 0.5 * land_dx
    land_xbound(2) = land_x(2) + 0.5 * land_dx
    land_ybound(1) = land_y(1) - 0.5 * land_dy
    land_ybound(2) = land_y(2) + 0.5 * land_dy

    ! Calculate the coordinates of the extreme river gridpoints allowed within
    ! the river domain. These have to be at least half a river gridbox
    ! away from the outer edge of the land domain, so that the edges of the
    ! the gridbox still lie within the land domain. We then allow a small
    ! tolerance (to allow for imprecise arithmetic) by moving the bounds out
    ! by a fraction of a gridbox (increasing the size of the rectangle).
    river_xbound(1) = land_xbound(1) + ( 0.5 - frac_toler ) * rivers_dx
    river_xbound(2) = land_xbound(2) - ( 0.5 - frac_toler ) * rivers_dx
    river_ybound(1) = land_ybound(1) + ( 0.5 - frac_toler ) * rivers_dy
    river_ybound(2) = land_ybound(2) - ( 0.5 - frac_toler ) * rivers_dy
  ELSE
    ! .NOT. rivers_regrid
    ! In this case we only need to set the bounding coordinates so that they
    ! do not exclude any land points. We have land and river boxes of the
    ! same resolution and we allow a small tolerance when comparing
    ! coordinates.
    river_xbound(1) = land_x(1) - frac_toler * land_dx
    river_xbound(2) = land_x(2) + frac_toler * land_dx
    river_ybound(1) = land_y(1) - frac_toler * land_dy
    river_ybound(2) = land_y(2) + frac_toler * land_dy
  END IF  !  rivers_regrid

ELSE
  !  .NOT. l_find_grid

  ! Use values from the namelist. The coordinates used to outline the valid
  ! domain are calculated as in older versions - essentially assuming that
  ! input parameters (e.g. y1_land_grid, ny_land_grid) are reasonable.
  ! If these inputs are not specified correctly, the model might still run but
  ! the results can be dubious - e.g. river points can be set up outside the
  ! area of the land grid but these will not have any runoff inputs, meaning
  ! the modelled riverflows are compromised. In other instances an error will
  ! be raised at a later stage.

  ! In the older code, river gridpoints were simply compared with these
  ! notional land extremes - and we do that here.
  ! If we assume that x1_land_grid and y1_land_grid have been set to indicate
  ! the gridpoint in the SW corner of the land grid, and nx_land_grid and
  ! ny_land_grid are the smallest values that include all land points, then by
  ! using nx_land_grid (rather than nx_land_grid-1) this calculation
  ! effectively allows an extra column of points at the E edge (and similarly
  ! in the y direction), i.e. the domain would be larger than required.
  river_xbound(1) = x1_land_grid
  river_xbound(2) = x1_land_grid + nx_land_grid * land_dx
  river_ybound(1) = y1_land_grid
  river_ybound(2) = y1_land_grid + ny_land_grid * land_dy

END IF  !  l_find_grid

CALL log_info( RoutineName,                                                    &
               "River points will be searched for in x "                    // &
               TRIM(to_string(river_xbound(1))) // ' to '                   // &
               TRIM(to_string(river_xbound(2))) // ' y '                    // &
               TRIM(to_string(river_ybound(1))) // ' to '                   // &
               TRIM(to_string(river_ybound(2))) )

!------------------------------------------------------------------------------
! If we have a regular lat-lon river grid (which requires that the land grid
! must also be regular lat-lon, for regridding or mapping) check that land
! points fall on the regular grid (which was precribed or calculated) and
! return the mapping index.
! Although remapping does not strictly require a grid, CALC_GRID_INDEX includes
! some useful checks that test whether the declared grid is consistent with the
! data values. Hence we call it for l_find_grid=F because in that case the
! declared grid is used to set the bounds of the river domain.
! Note that here we use the longitudes that might have been changed to a new
! range so as to allow a smaller land grid.
!------------------------------------------------------------------------------
IF ( rivers_reglatlon ) THEN
  IF ( rivers_regrid .OR. .NOT. l_find_grid ) THEN
    CALL calc_grid_index( global_land_pts, nx_land_grid, ny_land_grid,         &
                          land_dx, land_dy, frac_toler, x1_land_grid,          &
                          y1_land_grid, global_proj_x_land_work,               &
                          global_proj_y_land, land_index_tmp )
  END IF
  IF ( rivers_regrid ) THEN
    ! Save the land index.
    global_land_index(:) = land_index_tmp(:)
  END IF
END IF  !  rivers_reglatlon

RETURN
END SUBROUTINE process_rivers_coords

!##############################################################################

SUBROUTINE remap_ancil_control( nx_rivers, ny_rivers, rivers_dx, l_shift_x,    &
                                l_reverse_y, rivers )

!------------------------------------------------------------------------------
! Description:
!   Controls the transfer of ancillary fields from the river input grid to the
!   river grid.
!------------------------------------------------------------------------------

USE jules_rivers_mod, ONLY:                                                    &
  l_riv_overbank, rivers_type

USE init_overbank_props_mod, ONLY:                                             &
  nvars_overbank=>nvars_required, required_vars_overbank=>required_vars

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  nx_rivers, ny_rivers  ! Sizes of field.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  rivers_dx
    !  Size of rivers gridbox in the x direction.

LOGICAL, INTENT(IN) ::                                                         &
  l_shift_x,                                                                   &
    ! Flag indicating that a cyclic shift is to be applied in the x direction
    ! to river ancillaries. This can be TRUE only if the coordinate is
    ! longitude.
  l_reverse_y
    ! Flag indicating if the order in the y direction of ancillary fields is
    ! to be reversed so that the coordinate is monotonically increasing.

!------------------------------------------------------------------------------
! Type arguments with INTENT(IN OUT)
!------------------------------------------------------------------------------
TYPE(rivers_type), INTENT(IN OUT) :: rivers

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER :: ivar  !  Loop counter.

!end of header
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Deal with river fields.
!------------------------------------------------------------------------------
! Loop through required fields.
DO ivar=1, nvars_required
  CALL remap_ancil( nx_rivers, ny_rivers, rivers_dx, l_shift_x, l_reverse_y,   &
                    required_vars(ivar), rivers )
END DO

! Deal with the river outflow number field.
IF ( l_riv_number ) THEN
  CALL remap_ancil( nx_rivers, ny_rivers, rivers_dx, l_shift_x, l_reverse_y,   &
                    'rivers_outflow_number', rivers )
END IF

!------------------------------------------------------------------------------
! Deal with overbank inundation fields.
!------------------------------------------------------------------------------
IF ( l_riv_overbank ) THEN
  DO ivar=1, nvars_overbank
    CALL remap_ancil( nx_rivers, ny_rivers, rivers_dx, l_shift_x, l_reverse_y, &
                      required_vars_overbank(ivar), rivers )
  END DO
END IF

!------------------------------------------------------------------------------
! Having used the river coordinates to remap all the required fields, we now
! change the coordinates themselves. This stage must be done after all other
! fields have been remapped.
!------------------------------------------------------------------------------
IF ( l_shift_x ) THEN
  CALL remap_ancil( nx_rivers, ny_rivers, rivers_dx, l_shift_x, l_reverse_y,   &
                    'rivers_xgrid', rivers )
END IF

IF ( l_reverse_y ) THEN
  CALL remap_ancil( nx_rivers, ny_rivers, rivers_dx, l_shift_x, l_reverse_y,   &
                    'rivers_ygrid', rivers )
END IF

RETURN
END SUBROUTINE remap_ancil_control

!##############################################################################

SUBROUTINE remap_ancil( nx_rivers, ny_rivers, rivers_dx,                       &
                        l_shift_x, l_reverse_y, var, rivers )

!------------------------------------------------------------------------------
! Description:
!   Provides functionality to transfer each river ancillary field from the
!   river input grid to the river grid.
!------------------------------------------------------------------------------

USE jules_rivers_mod, ONLY:                                                    &
  rivers_type

USE overbank_inundation_mod, ONLY:                                             &
  logn_mean, logn_stdev

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  nx_rivers, ny_rivers  ! Sizes of field.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  rivers_dx
    !  Size of rivers gridbox in the x direction.

LOGICAL, INTENT(IN) ::                                                         &
  l_shift_x,                                                                   &
    ! Flag indicating that a cyclic shift is to be applied in the x direction
    ! to river ancillaries. This can be TRUE only if the coordinate is
    ! longitude.
  l_reverse_y
    ! Flag indicating if the order in the y direction of ancillary fields is
    ! to be reversed so that the coordinate is monotonically increasing.

CHARACTER(LEN=*), INTENT(IN) :: var
    ! The name of a variable.

!------------------------------------------------------------------------------
! Type arguments with INTENT(IN OUT)
!------------------------------------------------------------------------------
TYPE(rivers_type), INTENT(IN OUT) :: rivers

!------------------------------------------------------------------------------
! Local parameters.
!------------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'REMAP_ANCIL'

!------------------------------------------------------------------------------
! Local variables.
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  tmp_2d_coord(nx_rivers,ny_rivers)
    ! A 2-D version of a 1-D coordinate variable.

!end of header
!------------------------------------------------------------------------------

SELECT CASE ( var )

  !----------------------------------------------------------------------------
  ! Cases for river (not overbank) variables.
  !----------------------------------------------------------------------------
CASE ( 'area' )
  CALL remap_field( nx_rivers, ny_rivers, rivers_dx, l_shift_x, l_reverse_y,   &
                    rivers%rivers_xgrid, rivers%rivers_dra )

CASE ( 'direction' )
  CALL remap_field( nx_rivers, ny_rivers, rivers_dx, l_shift_x, l_reverse_y,   &
                    rivers%rivers_xgrid, rivers%rivers_dir )

CASE ( 'latitude_2d' )
  CALL remap_field( nx_rivers, ny_rivers, rivers_dx, l_shift_x, l_reverse_y,   &
                    rivers%rivers_xgrid, rivers%rivers_lat2d )

CASE ( 'longitude_2d' )
  CALL remap_field( nx_rivers, ny_rivers, rivers_dx, l_shift_x, l_reverse_y,   &
                    rivers%rivers_xgrid, rivers%rivers_lon2d )

CASE ( 'rivers_outflow_number' )
  CALL remap_field( nx_rivers, ny_rivers, rivers_dx, l_shift_x, l_reverse_y,   &
                    rivers%rivers_xgrid, rivers%rivers_outflow_number )

CASE ( 'sequence' )
  CALL remap_field( nx_rivers, ny_rivers, rivers_dx, l_shift_x, l_reverse_y,   &
                    rivers%rivers_xgrid, rivers%rivers_seq )

  !----------------------------------------------------------------------------
  ! Cases for river overbank inundation variables.
  !----------------------------------------------------------------------------
CASE ( 'logn_mean' )
  CALL remap_field( nx_rivers, ny_rivers, rivers_dx, l_shift_x, l_reverse_y,   &
                    rivers%rivers_xgrid, logn_mean )

CASE ( 'logn_stdev' )
  CALL remap_field( nx_rivers, ny_rivers, rivers_dx, l_shift_x, l_reverse_y,   &
                    rivers%rivers_xgrid, logn_stdev )

  !----------------------------------------------------------------------------
  ! Cases for the 1-D coordinate variables.
  ! Rather than requiring separate 1-D remapping routines, we instead create
  ! temporary 2D coordinate variables that can be processed by remap_field.
  !----------------------------------------------------------------------------
CASE ( 'rivers_xgrid' )
  ! Create a 2D field by copying the 1D xgrid.
  tmp_2d_coord = SPREAD( rivers%rivers_xgrid, 2, ny_rivers )
  CALL remap_field( nx_rivers, ny_rivers, rivers_dx, l_shift_x, l_reverse_y,   &
                    rivers%rivers_xgrid, tmp_2d_coord )
  ! Extract the first row from the 2D field.
  rivers%rivers_xgrid(:) = tmp_2d_coord(:,1)

CASE ( 'rivers_ygrid' )
  ! Create a 2D field by copying the 1D ygrid.
  tmp_2d_coord = SPREAD( rivers%rivers_ygrid, 1, nx_rivers )
  CALL remap_field( nx_rivers, ny_rivers, rivers_dx, l_shift_x, l_reverse_y,   &
                    rivers%rivers_xgrid, tmp_2d_coord )
  ! Extract the first column from the 2D field.
  rivers%rivers_ygrid(:) = tmp_2d_coord(1,:)

CASE DEFAULT
  CALL log_fatal( RoutineName,                                                 &
                  "Do not recognise var: " // TRIM(var) )
END SELECT

RETURN
END SUBROUTINE remap_ancil

!##############################################################################
SUBROUTINE remap_field( nx_rivers, ny_rivers, rivers_dx, l_shift_x,            &
                        l_reverse_y, rivers_xgrid, field )

!------------------------------------------------------------------------------
! Description:
!   Transfer a river ancillary field from the river input grid to the river
!   grid.
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  nx_rivers, ny_rivers  ! Sizes of field.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  rivers_dx
    !  Size of rivers gridbox in the x direction.

LOGICAL, INTENT(IN) ::                                                         &
  l_shift_x,                                                                   &
    ! Flag indicating that a cyclic shift is to be applied in the x direction
    ! to river ancillaries. This can be TRUE only if the coordinate is
    ! longitude.
  l_reverse_y
    ! Flag indicating if the order in the y direction of ancillary fields is
    ! to be reversed so that the coordinate is monotonically increasing.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  rivers_xgrid(nx_rivers)
    ! Coordinate values for x-dimension of rivers grid.

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN OUT)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  field(nx_rivers,ny_rivers)
    ! The field to be transformed. On input this is on the river input grid,
    ! on output this is on the river grid.

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  dy,                                                                          &
    ! Loop increment.
  ix, iy,                                                                      &
    ! Positions in input field.
  iy1, iy2,                                                                    &
    ! Loop bounds.
  jx, jy,                                                                      &
    ! Positions in output field.
  xshift
    ! Number of grid boxes by which fields are moved in the positive x
    ! direction.

!------------------------------------------------------------------------------
! Local array variables.
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  field_tmp(nx_rivers,ny_rivers)
    ! Work space.

!end of header
!-----------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Set bounds and step for loop over rows.
!------------------------------------------------------------------------------
IF ( l_reverse_y ) THEN
  iy1 = ny_rivers
  iy2 = 1
  dy  = -1
ELSE
  iy1 = 1
  iy2 = ny_rivers
  dy  = 1
END IF

!------------------------------------------------------------------------------
! Calculate the size of any shift in the x direction. The remapping involves
! what is effectively a cyclic shift in x (performed element-by-element as the
! variable is loaded).
!------------------------------------------------------------------------------
IF ( l_shift_x ) THEN
  xshift = NINT( (rivers_xgrid(1) - MINVAL(rivers_xgrid)) / rivers_dx )
ELSE
  xshift = 0
END IF

!------------------------------------------------------------------------------
! Loop over elements of the input grid, mapping them onto the output grid.
!------------------------------------------------------------------------------

DO ix = 1,nx_rivers

  ! Work out where in x to move this column to.
  jx = ix + xshift
  IF ( jx > nx_rivers ) THEN
    ! Note that this can only occur if l_shift_x = TRUE.
    jx = jx - nx_rivers
  END IF

  ! Process rows in the order to match the required output order.
  jy = 0
  DO iy = iy1,iy2,dy
    jy  = jy + 1
    field_tmp(jx,jy) = field(ix,iy)
  END DO

END DO  !  ix

! Copy the transformed field into the output variable.
field(:,:) = field_tmp(:,:)

END SUBROUTINE remap_field


!##############################################################################

SUBROUTINE process_direction_fields( l_cyclic_x, rivers_dir, direction_grid,   &
                                     nextx_grid, nexty_grid, river_mask )

!------------------------------------------------------------------------------
! Description:
!   Convert a flow direction field to nextx and nexty fields, so that all
!   configurations use those fields. Also check values are reasonable and set
!   a mask that identifies different types of point.
!------------------------------------------------------------------------------

USE jules_rivers_mod, ONLY:                                                    &
  flow_dir_delta, nx_rivers, ny_rivers

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN)
!------------------------------------------------------------------------------
LOGICAL, INTENT(IN) ::                                                         &
  l_cyclic_x
    ! Flag indicating a river input grid that is cyclic (global) in the x
    ! direction.

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  rivers_dir(nx_rivers,ny_rivers)
    ! River routing direction index (REAL version).

!------------------------------------------------------------------------------
! Array arguments with INTENT(OUT)
!------------------------------------------------------------------------------
INTEGER, INTENT(OUT) ::                                                        &
  direction_grid(nx_rivers,ny_rivers),                                         &
    ! Integer version of river routing direction index.
  nextx_grid(nx_rivers,ny_rivers),                                             &
    ! x index of the next downstream point.
  nexty_grid(nx_rivers,ny_rivers),                                             &
    ! y index of the next downstream point.
  river_mask(nx_rivers,ny_rivers)
  ! Indicates type of each point.

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  ix, iy
    ! Loop counters.

!end of header
!------------------------------------------------------------------------------

IF ( l_use_direction ) THEN

  !----------------------------------------------------------------------------
  ! Flow direction is provided. Create an integer version (which is required
  ! later in some configurations) and convert to nextx and nexty fields.
  !----------------------------------------------------------------------------

  DO iy = 1,ny_rivers
    DO ix = 1,nx_rivers

      !------------------------------------------------------------------------
      ! Get an integer version of rivers_dir. The REAL version is not used
      ! after this.
      !------------------------------------------------------------------------
      direction_grid(ix,iy) = NINT( rivers_dir(ix,iy) )

      !------------------------------------------------------------------------
      ! Convert the flow direction field to nextx and nexty pairs.
      !------------------------------------------------------------------------
      IF ( ANY( direction_grid(ix,iy) == flow_dir_river(1:8) ) ) THEN
        ! These are the 8 "physical" flow directions, i.e. there is a flow
        ! direction. Set indices to show the next point on the grid.
        ! Set river mask to show land (for now assumed to be outside the river
        ! domain).
        nextx_grid(ix,iy) = ix + flow_dir_delta( direction_grid(ix,iy),1 )
        nexty_grid(ix,iy) = iy + flow_dir_delta( direction_grid(ix,iy),2 )
        river_mask(ix,iy) = land_off_domain

        ! Deal with cyclic boundary conditions in x and flow across the edges
        ! of the grid in the x direction.
        IF ( l_cyclic_x ) THEN
          IF ( nextx_grid(ix,iy) == 0 ) THEN
            nextx_grid(ix,iy) = nx_rivers
          ELSE IF ( nextx_grid(ix,iy) == nx_rivers + 1 ) THEN
            nextx_grid(ix,iy) = 1
          END IF
        ELSE
          ! Flow across the edge of the grid. Save the direction * -1.
          IF ( nextx_grid(ix,iy) == 0 .OR.                                     &
               nextx_grid(ix,iy) == nx_rivers + 1 ) THEN
            ! Note that these values of -direction_grid should be appear in the
            ! array parameter flow_dir_river_edge (see elsewhere).
            nextx_grid(ix,iy) = -direction_grid(ix,iy)
            nexty_grid(ix,iy) = -direction_grid(ix,iy)
          END IF
        END IF  !  l_cyclic_x

        ! In the y direction we do not allow a cyclic boundary condition but
        ! we do still have to deal with flow across the edge of the grid.
        IF ( nexty_grid(ix,iy) == 0 .OR.                                       &
             nexty_grid(ix,iy) == ny_rivers + 1 ) THEN
          ! Flow across the edge of the grid. Save the direction * -1.
          nextx_grid(ix,iy) = -direction_grid(ix,iy)
          nexty_grid(ix,iy) = -direction_grid(ix,iy)
        END IF

      ELSE IF ( direction_grid(ix,iy) == dir_mouth ) THEN
        ! A river mouth. Identify this with a special value < 0.
        nextx_grid(ix,iy) = river_mouth
        nexty_grid(ix,iy) = river_mouth
        river_mask(ix,iy) = land_off_domain

      ELSE IF ( direction_grid(ix,iy) == dir_inland_drainage ) THEN
        ! An inland drainage point. Identify this with a special value < 0.
        nextx_grid(ix,iy) = inland_drainage
        nexty_grid(ix,iy) = inland_drainage
        river_mask(ix,iy) = land_off_domain

      ELSE
        ! Set all other values to sea. This deals with files that have missing
        ! data values at sea points, but could potentially obscure other
        ! (hopefully obvious) issues such as a different encoding scheme having
        ! been used.
        nextx_grid(ix,iy) = sea
        nexty_grid(ix,iy) = sea
        river_mask(ix,iy) = sea

      END IF  !  direction_grid

    END DO  !  ix
  END DO  !  iy

END IF  !  l_use_direction

RETURN
END SUBROUTINE process_direction_fields

!##############################################################################

SUBROUTINE find_land_grid( l_find_grid, global_proj_x_land, global_proj_y_land,&
                          nx_land_grid, ny_land_grid, x1_land_grid,            &
                          y1_land_grid, l_land_180, global_proj_x_land_work,   &
                          land_x, land_y )

!------------------------------------------------------------------------------
! Description:
!   Calculate various characteristics of the land points and the land grid,
!   including whether shifting in longitude allows a smaller grid.
!------------------------------------------------------------------------------

USE jules_rivers_mod, ONLY: land_dx, land_dy, rivers_regrid

USE logging_mod, ONLY: log_info

USE model_grid_mod, ONLY: global_land_pts, l_coord_latlon

IMPLICIT NONE

!------------------------------------------------------------------------------
! Arguments with INTENT(IN)
!------------------------------------------------------------------------------
LOGICAL, INTENT(IN) :: l_find_grid
    ! Switch controlling how characteristics of the land and rivers grids are
    ! determined.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  global_proj_x_land(global_land_pts),                                         &
    ! x-coordinates of land points.
  global_proj_y_land(global_land_pts)
    ! y-coordinates of land points.

!------------------------------------------------------------------------------
! Arguments with INTENT(IN OUT)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN OUT) :: nx_land_grid, ny_land_grid
  ! x and y sizes of the land grid. These are INTENT(IN) in some configurations
  ! and INTENT(IN OUT) in others.

REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  ! These are INTENT(IN) in some configurations and INTENT(OUT) in others.
  x1_land_grid,                                                                &
    ! x coordinate of "western-most" (i.e. first) column of gridpoints on a
    ! regular land grid.
  y1_land_grid
    ! y coordinate of "southern-most" (i.e. first) row of gridpoints on a
    ! regular land grid.

!------------------------------------------------------------------------------
! Arguments with INTENT(OUT)
!------------------------------------------------------------------------------
LOGICAL, INTENT(OUT) ::                                                        &
  l_land_180
    ! Flag indicating the range of land point longitudes.

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  global_proj_x_land_work(global_land_pts),                                    &
    ! x coordinates of land points, including any transformation.
  land_x(2), land_y(2)
    ! Min and max values of the projection coordinates, at land points.

!------------------------------------------------------------------------------
! Local parameters.
!------------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'FIND_LAND_GRID'

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  nx_land_grid_work
    ! An alternative value of nx_land_grid (the grid size).

!------------------------------------------------------------------------------
! Local array variables.
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  land_x_work(2)
    ! Min and max values of the x projection coordinates, at land points.

!end of header
!------------------------------------------------------------------------------

! Get the min and max values of the projection coordinates - to describe the
! area in which land points occur.
land_x(1) = MINVAL( global_proj_x_land )
land_x(2) = MAXVAL( global_proj_x_land )
land_y(1) = MINVAL( global_proj_y_land )
land_y(2) = MAXVAL( global_proj_y_land )

! Report range of coordinate values for global land points.
CALL log_info( RoutineName,                                                    &
               'Land points lie in x='                                      // &
               TRIM(to_string( land_x(1) )) // ' to '                       // &
               TRIM(to_string( land_x(2) )) // ' y='                        // &
               TRIM(to_string( land_y(1) )) // ' to '                       // &
               TRIM(to_string( land_y(2) )) )

! Copy land x coordinate values into a new variable. If coordinates are
! longitude, these might later be altered.
global_proj_x_land_work(:) = global_proj_x_land(:)

IF ( l_coord_latlon ) THEN
  ! Establish if longitude values of land points use the range [-180,180] or
  ! [0,360]. We assume that the presence of any value < 0 means [-180,180] is
  ! used, otherwise it is [0,360].
  IF ( land_x(1) < 0.0 ) THEN
    l_land_180 = .TRUE.
  ELSE
    l_land_180 = .FALSE.
  END IF
END IF

IF ( l_find_grid .AND. l_coord_latlon ) THEN

  ! Get characteristics of the land grid.
  ! Although we only need to identify a regular land grid (described by
  ! variables such as nx_land_grid) if rivers_regrid=T, we use this section
  ! more generally because it can also be used to give better values of land_x
  ! and land_y that later are used in setting the part of the river grid that
  ! is scanned for points.
  ! Note that if l_coord_latlon=F (in which case this code is not executed) we
  ! will have rivers_regrid=T and none of the values set here are needed
  ! elsewhere.

  ! Get first estimate of grid location and size.
  x1_land_grid = land_x(1)
  y1_land_grid = land_y(1)
  nx_land_grid = NINT( ( land_x(2) - land_x(1) ) / land_dx ) + 1
  ny_land_grid = NINT( ( land_y(2) - land_y(1) ) / land_dy ) + 1

  ! Here we deal with a special case that follows from the cyclic nature of
  ! longitude. In general we want to identify the smallest land grid
  ! possible, which for many cases follows from simple arithmetic based on
  ! the coordinate values (as is done if the x-coordinate is not longitude).
  ! However, consider the case in which we have a global model input grid for
  ! which longitudes are given in the range [0,360] and we are considering
  ! a domain over part of Africa in the range of longitude [-10,10]. Points
  ! with longitudes in [350,360] or [0,10] fall within this domain, and a
  ! naive approach that simply looks at the min and max longitudes might
  ! conclude that a land grid that covers [0,360] (i.e. global) is required.
  ! If instead the longitudes are recast to be in the range [-180,180] it is
  ! easy to identify that a grid covering [-10,10] can be used.
  ! Below we compare the grid sizes that result from using longitudes in the
  ! ranges [0,360] and [-180,180], and use whichever allows a smaller grid.
  IF ( l_land_180 ) THEN
    ! Change land longitudes to be in [0,360].
    CALL change_longitude_from_180( global_proj_x_land_work )
  ELSE
    ! Change land longitudes to be in [-180,180].
    CALL change_longitude_from_360( global_proj_x_land_work )
  END IF  !  l_land_180
  ! Recalculate grid size. In many cases this will not have changed.
  land_x_work(1) = MINVAL( global_proj_x_land_work )
  land_x_work(2) = MAXVAL( global_proj_x_land_work )
  nx_land_grid_work = NINT( ( land_x_work(2) - land_x_work(1) )                &
                      / land_dx ) + 1

  ! Identify if either arrangement gives a smaller grid.
  IF ( nx_land_grid_work < nx_land_grid ) THEN
    ! Changing the longitude range gives a smaller land grid.
    ! Save the new values.
    nx_land_grid = nx_land_grid_work
    land_x(:)    = land_x_work(:)
    l_land_180   = .NOT. l_land_180
    CALL log_info( RoutineName,                                                &
                   'Range of land longitudes altered to allow a smaller ' //   &
                   'grid.')
    CALL log_info( RoutineName,                                                &
                   'Land points lie in x='                                //   &
                   TRIM(to_string( land_x(1) )) // ' to '                 //   &
                   TRIM(to_string( land_x(2) )) )
  ELSE
    ! Retain the original longitude values.
    global_proj_x_land_work(:) = global_proj_x_land(:)
  END IF

  ! Reload the minimum value, as that might have changed.
  x1_land_grid = land_x(1)

END IF  !  l_find_grid .AND. l_coord_latlon

END SUBROUTINE find_land_grid

!##############################################################################

SUBROUTINE change_longitude_from_180( longitude, n_change )

!------------------------------------------------------------------------------
! Description:
!   Change the range of a list of longitudes from [-180,180] to [0,360].
!   The current range has already been established.
!------------------------------------------------------------------------------

IMPLICIT NONE

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN OUT):
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  longitude(:)
    ! Longitude (degrees).

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(OUT):
!------------------------------------------------------------------------------
INTEGER, INTENT(OUT), OPTIONAL ::                                              &
  n_change
    ! The number of longitude values that were changed.

!------------------------------------------------------------------------------
! Local scalars.
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  i,                                                                           &
    ! Loop index.
  n_change_local
    ! Local version of n_change.

!end of header
!------------------------------------------------------------------------------

! Initialise counter.
n_change_local = 0

DO i = 1, SIZE(longitude)
  IF ( longitude(i) < 0.0 ) THEN
    longitude(i) = longitude(i) + 360.0
    n_change_local = n_change_local + 1
  END IF
END DO

! Return the counter if argument provided.
IF ( PRESENT( n_change ) ) THEN
  n_change = n_change_local
END IF

RETURN
END SUBROUTINE change_longitude_from_180

!##############################################################################

SUBROUTINE change_longitude_from_360( longitude, n_change )

!------------------------------------------------------------------------------
! Description:
!   Change the range of a list of longitudes from [0,360] to [-180,180].
!   The current range has already been established.
!------------------------------------------------------------------------------

IMPLICIT NONE

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN OUT):
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  longitude(:)
    ! Longitude (degrees).

!------------------------------------------------------------------------------
! Optional scalar arguments with INTENT(OUT):
!------------------------------------------------------------------------------
INTEGER, INTENT(OUT), OPTIONAL ::                                              &
  n_change
    ! The number of longitude values that were changed.

!------------------------------------------------------------------------------
! Local scalars.
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  i,                                                                           &
    ! Loop index.
  n_change_local
    ! Local version of n_change.

!end of header
!------------------------------------------------------------------------------

! Initialise counter.
n_change_local = 0

DO i = 1, SIZE(longitude)
  IF ( longitude(i) > 180.0 ) THEN
    longitude(i) = longitude(i) - 360.0
    n_change_local = n_change_local + 1
  END IF
END DO

! Return the counter if argument provided.
IF ( PRESENT( n_change ) ) THEN
  n_change = n_change_local
END IF

RETURN
END SUBROUTINE change_longitude_from_360

!##############################################################################

SUBROUTINE find_river_points( rivers_xgrid, rivers_ygrid,                      &
                              river_xbound, river_ybound,                      &
                              direction_grid, nextx_grid, nexty_grid,          &
                              river_mask, np_rivers )

!------------------------------------------------------------------------------
! Description:
!   Identify points that are rivers.
!------------------------------------------------------------------------------

USE jules_rivers_mod, ONLY:                                                    &
  nx_rivers, ny_rivers, rivers_regrid

IMPLICIT NONE

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  river_xbound(2), river_ybound(2),                                            &
    ! Coordinates of the corners of the river domain - i.e. that part of the
    ! river input grid that will be searched for river points. Units as given
    ! by rivers_xgrid and rivers_ygrid.
  rivers_xgrid(nx_rivers), rivers_ygrid(ny_rivers)
    ! Coordinates for rivers grid.

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN OUT)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN OUT) ::                                                     &
  direction_grid(nx_rivers,ny_rivers),                                         &
    ! Integer version of river routing direction index.
  nextx_grid(nx_rivers, ny_rivers),                                            &
    ! x index of the next downstream point.
  nexty_grid(nx_rivers, ny_rivers),                                            &
    ! y index of the next downstream point.
  river_mask(nx_rivers,ny_rivers)
    ! Indicates type of each point.

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(OUT)
!------------------------------------------------------------------------------
INTEGER, INTENT(OUT) ::                                                        &
  np_rivers
    ! Number of river points.

!------------------------------------------------------------------------------
! Local parameters.
!------------------------------------------------------------------------------
LOGICAL, PARAMETER :: l_add_mouths = .TRUE.
    ! Switch to allow insertion of missing river mouths. This is provided in
    ! case we later want to make it optional or switch it off.

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'FIND_RIVER_POINTS'

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  ix, iy,                                                                      &
    ! Loop counters.
  n_mouths_added
    ! Counter of missing river mouths that were added.

!end of header
!------------------------------------------------------------------------------

! Initialise counters.
n_mouths_added = 0
np_rivers      = 0

!------------------------------------------------------------------------------
! Scan the grid looking for river points. Sea points are not altered.
!------------------------------------------------------------------------------
DO iy = 1,ny_rivers
  DO ix = 1,nx_rivers

    !-------------------------------------------------------------------------
    ! Consider points on the valid domain only (i.e. the part of the river
    ! grid that is to be searched for river points).
    !-------------------------------------------------------------------------
    IF ( ( rivers_xgrid(ix) < river_xbound(1) ) .OR.                           &
         ( rivers_ygrid(iy) < river_ybound(1) ) .OR.                           &
         ( rivers_xgrid(ix) > river_xbound(2) ) .OR.                           &
         ( rivers_ygrid(iy) > river_ybound(2) ) ) THEN

      !-----------------------------------------------------------------------
      ! This point is not in the valid river domain.
      ! If this is a land point (as indicated by river_mask) set nextx to
      ! land_off_domain.
      !-----------------------------------------------------------------------
      IF ( river_mask(ix,iy) == land_off_domain ) THEN
        ! For clarity reset nextx_grid and nexty_grid - though these variables
        ! are not used again at such points.
        nextx_grid(ix,iy) = land_off_domain
        nexty_grid(ix,iy) = land_off_domain
      END IF

    ELSE IF ( river_mask(ix,iy) == land_off_domain ) THEN

      !------------------------------------------------------------------------
      ! This point is in the valid river domain and is land (though currently
      ! flagged as land_off_domain). It is a river point.
      !------------------------------------------------------------------------
      np_rivers   = np_rivers + 1

      ! Set river_mask to differentiate between different types of river point.
      SELECT CASE ( nextx_grid(ix,iy) )
      CASE ( river_mouth )
        river_mask(ix,iy) = river_mouth
      CASE ( inland_drainage )
        river_mask(ix,iy) = inland_drainage
      CASE DEFAULT
        ! These will all have nextx_grid in the range 1:nx_rivers.
        river_mask(ix,iy) = land_in_domain
      END SELECT

      !------------------------------------------------------------------------
      ! Insert any missing river mouth.
      !------------------------------------------------------------------------
      IF ( l_add_mouths .AND. nextx_grid(ix,iy) > 0 ) THEN
        ! This point is indicated as having a physical flow direction.
        ! Check if the next downstream point is sea.
        IF ( river_mask(nextx_grid(ix,iy),nexty_grid(ix,iy)) == sea ) THEN
          ! The next point is sea. In many ancillary files this point would
          ! have been flagged as a river mouth. Convert the current point to a
          ! river mouth.
          n_mouths_added    = n_mouths_added + 1
          nextx_grid(ix,iy) = river_mouth
          nexty_grid(ix,iy) = river_mouth
          river_mask(ix,iy) = river_mouth
          IF ( l_use_direction ) THEN
            ! If l_riv_number=T we will later check that the flow direction
            ! field is consistent with the river number field, for which we
            ! need to update direction_grid here. (Although nextx_grid contains
            ! much the same information as direction_grid, we also need the
            ! separate direction_grid variable to deal with flows across the
            ! edge of the river domain.)
            direction_grid(ix,iy) = dir_mouth
          END IF
        END IF
      END IF  !  l_add_mouths

    END IF

  END DO  !  ix
END DO  !  iy

! Report if mouths were added.
IF ( n_mouths_added > 0 ) THEN
  CALL log_warn( RoutineName,                                                  &
                 "Number of river mouths inserted = " //                       &
                 TRIM(to_string(n_mouths_added)) )
END IF

RETURN
END SUBROUTINE find_river_points

!##############################################################################

SUBROUTINE allocate_rivers_vars_rp( np_rivers, rivers, rivers_data )

!------------------------------------------------------------------------------
! Description:
!   Allocate river point variables, initialise, and associate pointers.
!------------------------------------------------------------------------------

USE jules_model_environment_mod, ONLY: l_oasis_rivers

USE jules_rivers_mod, ONLY: i_river_vn, rivers_data_type, rivers_rfm,          &
                            rivers_trip, rivers_type

USE parallel_mod, ONLY: is_master_task

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  np_rivers
    ! Number of river points.

!------------------------------------------------------------------------------
! Arguments with INTENT(IN OUT)
!------------------------------------------------------------------------------
TYPE(rivers_type), INTENT(IN OUT) :: rivers
TYPE(rivers_data_type), INTENT(IN OUT), TARGET :: rivers_data

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'ALLOCATE_RIVERS_VARS_RP'

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  ERROR, error_sum,                                                            &
    ! Error flags.
  nland_tmp,                                                                   &
    ! Number of land points to allocate for.
  np_rivers_tmp
    ! Number of river points to allocate for.

!end of header
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! As river routing is performed on the master task, we only allocate arrays at
! full size on that task, and only if the run configuration requires it.
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Allocate variables that are required by all configurations.
!------------------------------------------------------------------------------
IF ( is_master_task() ) THEN
  ! Full size.
  np_rivers_tmp = np_rivers
ELSE
  ! Minimum size.
  np_rivers_tmp = 1
END IF

! Ancillaries and related fields.
ALLOCATE(rivers_data%rivers_boxareas_rp(np_rivers_tmp),  STAT = ERROR)
error_sum = ERROR
ALLOCATE(rivers_data%rivers_index_rp(np_rivers_tmp),     STAT = ERROR)
error_sum = error_sum + ERROR
ALLOCATE(rivers_data%rivers_lat_rp(np_rivers_tmp),       STAT = ERROR)
error_sum = error_sum + ERROR
ALLOCATE(rivers_data%rivers_lon_rp(np_rivers_tmp),       STAT = ERROR)
error_sum = error_sum + ERROR
ALLOCATE(rivers_data%rivers_next_rp(np_rivers_tmp),      STAT = ERROR)
error_sum = error_sum + ERROR
ALLOCATE(rivers_data%rivers_x_coord_rp(np_rivers_tmp),   STAT = ERROR)
error_sum = error_sum + ERROR
ALLOCATE(rivers_data%rivers_y_coord_rp(np_rivers_tmp),   STAT = ERROR)
error_sum = error_sum + ERROR
! Fluxes.
ALLOCATE(rivers_data%rflow_rp(np_rivers_tmp),            STAT = ERROR)
error_sum = error_sum + ERROR
ALLOCATE(rivers_data%rivers_outflow_rp(np_rivers),       STAT = ERROR)
error_sum = error_sum + ERROR
ALLOCATE(rivers_data%rrun_rp(np_rivers_tmp),             STAT = ERROR)
error_sum = error_sum + ERROR
ALLOCATE(rivers_data%rrun_sub_surf_rp(np_rivers_tmp),    STAT = ERROR)
error_sum = error_sum + ERROR
ALLOCATE(rivers_data%rrun_surf_rp(np_rivers_tmp),        STAT = ERROR)
error_sum = error_sum + ERROR

!------------------------------------------------------------------------------
! Allocate variable specific to remapping.
!------------------------------------------------------------------------------
ALLOCATE(rivers_data%map_river_to_land_points(np_rivers_tmp), STAT = ERROR)
error_sum = error_sum + ERROR

!------------------------------------------------------------------------------
! Allocate RFM variables
!------------------------------------------------------------------------------
IF ( i_river_vn == rivers_rfm .AND. is_master_task() ) THEN
  ! Full size.
  np_rivers_tmp = np_rivers
ELSE
  ! Minimum size.
  np_rivers_tmp = 1
END IF

! Ancillaries and related fields.
! Note: rfm_flowobs1_rp is not used by standalone JULES - but is left here
!       for now.
ALLOCATE(rivers_data%rfm_flowobs1_rp(np_rivers_tmp),  STAT = ERROR )
error_sum = error_sum + ERROR
ALLOCATE(rivers_data%rfm_iarea_rp(np_rivers_tmp),     STAT = ERROR )
error_sum = error_sum + ERROR
ALLOCATE(rivers_data%rfm_land_rp(np_rivers_tmp),      STAT = ERROR )
error_sum = error_sum + ERROR
! Fluxes and stores.
ALLOCATE(rivers_data%rfm_baseflow_rp(np_rivers_tmp),  STAT = ERROR )
error_sum = error_sum + ERROR
ALLOCATE(rivers_data%rfm_bflowin_rp(np_rivers_tmp),   STAT = ERROR )
error_sum = error_sum + ERROR
ALLOCATE(rivers_data%rfm_flowin_rp(np_rivers_tmp),    STAT = ERROR )
error_sum = error_sum + ERROR
ALLOCATE(rivers_data%rfm_rivflow_rp(np_rivers_tmp),   STAT = ERROR)
error_sum = error_sum + ERROR
ALLOCATE(rivers_data%rfm_substore_rp(np_rivers_tmp),  STAT = ERROR )
error_sum = error_sum + ERROR
ALLOCATE(rivers_data%rfm_surfstore_rp(np_rivers_tmp), STAT = ERROR )
error_sum = error_sum + ERROR

!------------------------------------------------------------------------------
! Allocate TRIP variables
!------------------------------------------------------------------------------
IF ( i_river_vn == rivers_trip .AND. is_master_task() ) THEN
  ! Full size.
  np_rivers_tmp = np_rivers
ELSE
  ! Minimum size.
  np_rivers_tmp = 1
END IF
! Ancillaries and related fields.
ALLOCATE(rivers_data%rivers_seq_rp(np_rivers_tmp), STAT = ERROR)
error_sum = error_sum + ERROR
! Stores.
ALLOCATE(rivers_data%rivers_sto_rp(np_rivers_tmp), STAT = ERROR)
error_sum = error_sum + ERROR

!------------------------------------------------------------------------------
! Allocate River coupling variables.
!------------------------------------------------------------------------------
IF ( l_oasis_rivers .AND. is_master_task() ) THEN
  ! Full size.
  np_rivers_tmp = np_rivers
ELSE
  ! Minimum size.
  np_rivers_tmp = 1
END IF
ALLOCATE(rivers_data%sub_surf_roff_rp(np_rivers_tmp), STAT = ERROR)
error_sum = error_sum + ERROR
ALLOCATE(rivers_data%surf_roff_rp(np_rivers_tmp),     STAT = ERROR)
error_sum = error_sum + ERROR
ALLOCATE(rivers_data%rivers_ilat_rp(np_rivers_tmp),   STAT = ERROR)
error_sum = error_sum + ERROR
ALLOCATE(rivers_data%rivers_ilon_rp(np_rivers_tmp),   STAT = ERROR)
error_sum = error_sum + ERROR

IF ( l_riv_number .AND. is_master_task() ) THEN
  ! Full size.
  np_rivers_tmp = np_rivers
ELSE
  ! Minimum size.
  np_rivers_tmp = 1
END IF
ALLOCATE(rivers_data%rivers_outflow_number_rp(np_rivers_tmp), STAT = ERROR)
error_sum = error_sum + ERROR

IF ( error_sum /= 0 ) THEN
  CALL log_fatal( RoutineName, "Error allocating for routing point arrays." )
END IF

!------------------------------------------------------------------------------
! Initialise array values to missing values, except fluxes can be zero to
! preserve existing results.
!------------------------------------------------------------------------------
! Ancillaries etc.
rivers_data%rivers_boxareas_rp(:) = rmdi
rivers_data%rivers_index_rp(:)   = imdi
rivers_data%rivers_lat_rp(:)     = rmdi
rivers_data%rivers_lon_rp(:)     = rmdi
rivers_data%rivers_next_rp(:)    = imdi
rivers_data%rivers_x_coord_rp(:) = rmdi
rivers_data%rivers_y_coord_rp(:) = rmdi
! Fluxes.
rivers_data%rflow_rp(:)          = 0.0
rivers_data%rivers_outflow_rp(:) = rmdi
rivers_data%rrun_rp(:)           = 0.0
rivers_data%rrun_sub_surf_rp(:)  = 0.0
rivers_data%rrun_surf_rp(:)      = 0.0

! Initialise regridding variable.
rivers_data%map_river_to_land_points(:) = imdi

! Initialise RFM array values
rivers_data%rfm_flowobs1_rp(:)  = rmdi
rivers_data%rfm_iarea_rp(:)     = imdi
rivers_data%rfm_land_rp(:)      = imdi
rivers_data%rfm_baseflow_rp(:)  = 0.0
! rfm_bflowin_rp, rfm_flowin_rp, rfm_substore_rp and rfm_surfstore_rp should
! be initiailsed to zero to preserve existing results and until better
! initialisation is possible.
rivers_data%rfm_bflowin_rp(:)   = 0.0
rivers_data%rfm_flowin_rp(:)    = 0.0
rivers_data%rfm_rivflow_rp(:)   = 0.0
rivers_data%rfm_substore_rp(:)  = 0.0
rivers_data%rfm_surfstore_rp(:) = 0.0

! Initialise TRIP array values
rivers_data%rivers_seq_rp(:) = imdi
! rivers_sto_rp should be initialised to zero to preserve existing results and
! until better initialisation is possible.
rivers_data%rivers_sto_rp(:) = 0.0

! Initialise coupling variables.
rivers_data%rivers_outflow_number_rp(:) = imdi
rivers_data%sub_surf_roff_rp(:) = 0.0
rivers_data%surf_roff_rp(:)     = 0.0

!------------------------------------------------------------------------------
! Associate pointers
!------------------------------------------------------------------------------
! Ancillaries etc.
rivers%rivers_boxareas_rp => rivers_data%rivers_boxareas_rp
rivers%rivers_index_rp => rivers_data%rivers_index_rp
rivers%rivers_lat_rp => rivers_data%rivers_lat_rp
rivers%rivers_lon_rp => rivers_data%rivers_lon_rp
rivers%rivers_ilat_rp => rivers_data%rivers_ilat_rp
rivers%rivers_ilon_rp => rivers_data%rivers_ilon_rp
rivers%rivers_next_rp => rivers_data%rivers_next_rp
rivers%rivers_x_coord_rp => rivers_data%rivers_x_coord_rp
rivers%rivers_y_coord_rp => rivers_data%rivers_y_coord_rp
! Fluxes.
rivers%rflow_rp => rivers_data%rflow_rp
rivers%rivers_outflow_rp => rivers_data%rivers_outflow_rp
rivers%rrun_rp => rivers_data%rrun_rp
rivers%rrun_sub_surf_rp => rivers_data%rrun_sub_surf_rp
rivers%rrun_surf_rp => rivers_data%rrun_surf_rp

! Associate pointers for regridding variable.
rivers%map_river_to_land_points => rivers_data%map_river_to_land_points

! Associate pointers for RFM variables.
rivers%rfm_flowobs1_rp => rivers_data%rfm_flowobs1_rp
rivers%rfm_iarea_rp => rivers_data%rfm_iarea_rp
rivers%rfm_land_rp => rivers_data%rfm_land_rp
rivers%rfm_baseflow_rp => rivers_data%rfm_baseflow_rp
rivers%rfm_bflowin_rp => rivers_data%rfm_bflowin_rp
rivers%rfm_flowin_rp => rivers_data%rfm_flowin_rp
rivers%rfm_rivflow_rp => rivers_data%rfm_rivflow_rp
rivers%rfm_substore_rp => rivers_data%rfm_substore_rp
rivers%rfm_surfstore_rp => rivers_data%rfm_surfstore_rp

! Associate pointers for TRIP variables.
rivers%rivers_seq_rp => rivers_data%rivers_seq_rp
rivers%rivers_sto_rp => rivers_data%rivers_sto_rp

! Associate pointers for coupling variables.
rivers%rivers_outflow_number_rp => rivers_data%rivers_outflow_number_rp
rivers%sub_surf_roff_rp => rivers_data%sub_surf_roff_rp
rivers%surf_roff_rp => rivers_data%surf_roff_rp

RETURN
END SUBROUTINE allocate_rivers_vars_rp

!##############################################################################

SUBROUTINE set_river_point_values( rivers_x1_input, l_use_area, direction_grid,&
                                   river_mask, rivers, grid_riv_pt_number )

!------------------------------------------------------------------------------
! Description:
!   Set values at river points - generally by extracting from 2-D ancillary
!   fields.
!------------------------------------------------------------------------------

USE jules_rivers_mod, ONLY:                                                    &
  a_thresh, i_river_vn, l_riv_overbank, np_rivers, nseqmax, nx_rivers,         &
  ny_rivers, rfm_land, rfm_river, rivers_dx, rivers_rfm, rivers_trip,          &
  rivers_x1,                                                                   &
  ! types
  rivers_type

USE model_grid_mod, ONLY: l_coord_latlon
USE jules_model_environment_mod, ONLY: l_oasis_rivers

USE overbank_inundation_mod, ONLY:                                             &
  logn_mean, logn_stdev, logn_mean_rp, logn_stdev_rp, overbank_hypsometric,    &
  overbank_model

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  rivers_x1_input
    ! The first value of the x coordinate of the river input grid.

LOGICAL, INTENT(IN) ::                                                         &
  l_use_area
    ! Switch (used with RFM) to use a drainage area ancillary field to identify
    ! river points.

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  direction_grid(nx_rivers, ny_rivers),                                        &
    ! River direction index.
  river_mask(nx_rivers, ny_rivers)

!------------------------------------------------------------------------------
! Arguments with INTENT(IN OUT)
!------------------------------------------------------------------------------
TYPE(rivers_type), INTENT(IN OUT) :: rivers

!------------------------------------------------------------------------------
! Array arguments with INTENT(OUT)
!------------------------------------------------------------------------------
INTEGER, INTENT(OUT) ::                                                        &
  grid_riv_pt_number(nx_rivers, ny_rivers)
    ! Map full river grid to river points. For each location on the grid, this
    ! is the river point number.

!------------------------------------------------------------------------------
! Local parameters.
!------------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'SET_RIVER_POINT_VALUES'

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  ERROR, error_sum,                                                            &
    ! Error variables.
  ip, irx, iry, ix, iy
    ! Counters and index variables.

!end of header
!------------------------------------------------------------------------------

! Initialise mapping to show no points on grid.
grid_riv_pt_number(:,:) = 0

! Initialise max sequence and point number to zero.
nseqmax = 0
ip      = 0

!------------------------------------------------------------------------------
! Scan the river grid for river points and add these to the 1-D river point
! fields. The scanning is done so that the order in which points are found is
! the same as in previous versions of the code - scanning is columnwise (bottom
! to top, i.e. "south to north"), starting with the first column of the river
! input grid (NOT the river grid). The intention is to simplify this in future
! so that we will simply scan the river grid in a more straightforward
! manner, but that will change the order of points in the 1D fields, which
! cannot be allowed while standalone JULES does not check that restart files
! contain the required locations in the required order. (Without such a check
! we would be able to run from an old restart file that had the required points
! but in a different order, which would generate incorrect results.)
!
! Here we use the loops over ix and iy to scan the grid. Within this, irx and
! iry are calculated so as to preserve the order of points used in previous
! versions - in particular irx can differ from ix (if a cyclic shift in the x
! direction has been used to map the river input grid to the river grid).
!------------------------------------------------------------------------------

! Initialise irx so that the first increment will take us to the first column
! in the river input grid - essentially we are locating rivers_x1_input in the
! river grid.
irx = NINT( (rivers_x1_input - rivers%rivers_xgrid(1)) / rivers_dx )

! For now we use ix and iy to scan across a full-sized grid. We calculate irx
! and iry to match the old scan order, and load grid value at (irx,iry) into
! river vector point ip.

DO ix = 1,nx_rivers

  ! Move to next column of river grid.
  irx = irx + 1
  ! Deal with cyclic grid.
  IF ( irx == nx_rivers + 1 ) THEN
    irx = 1
  END IF

  DO iy = 1,ny_rivers

    ! Trivially iry = iy, but we use this separate iry variable for similarity
    ! with the (less trivial) irx variable.
    iry = iy

    ! Populate 2-D latitude and longitude coordinates.
    ! Note that if l_coord_latlon=F, the lat-lon coordinates were read from
    ! a file (and the river input and river grids are identical) and there is
    ! nothing to do here.
    IF ( l_coord_latlon ) THEN
      rivers%rivers_lat2d(irx,iry) = rivers%rivers_ygrid(iry)
      rivers%rivers_lon2d(irx,iry) = rivers%rivers_xgrid(irx)
    END IF

    !--------------------------------------------------------------------------
    ! Set values at river points.
    !--------------------------------------------------------------------------
    SELECT CASE ( river_mask(irx,iry) )

    CASE ( land_in_domain, river_mouth, inland_drainage )
      ! This should catch all types of river point.

      ! Increment counter of river points.
      ip = ip + 1

      !------------------------------------------------------------------------
      ! Define mapping between river vector and routing grid.
      !------------------------------------------------------------------------
      grid_riv_pt_number(irx,iry) = ip

      !------------------------------------------------------------------------
      ! Calculate index of position on river grid.
      ! Note that this index scans row-wise from bottom left, bottom to top.
      ! The loops (inside which this comment sits) that control the order in
      ! which points are found do not necessarily scan the same way - which is
      ! confusing but acceptable.
      !------------------------------------------------------------------------
      rivers%rivers_index_rp(ip) = (iry-1) * nx_rivers + irx

      !----------------------------------------------------------------------
      ! Store the latitude and longitude indices for coupling from
      ! and to the atmospheric model (via OASIS)
      !----------------------------------------------------------------------
      IF ( l_oasis_rivers ) THEN
        rivers%rivers_ilat_rp(ip) = iry
        rivers%rivers_ilon_rp(ip) = irx
      END IF

      !----------------------------------------------------------------------
      ! Set river vector information based on 2D input ancillaries.
      ! First, coordinates and flow direction.
      !------------------------------------------------------------------------
      rivers%rivers_lon_rp(ip)     = rivers%rivers_lon2d(irx,iry)
      rivers%rivers_lat_rp(ip)     = rivers%rivers_lat2d(irx,iry)
      rivers%rivers_x_coord_rp(ip) = rivers%rivers_xgrid(irx)
      rivers%rivers_y_coord_rp(ip) = rivers%rivers_ygrid(iry)

      !------------------------------------------------------------------------
      ! Set values that depend on the chosen river model.
      !------------------------------------------------------------------------
      IF ( i_river_vn == rivers_rfm ) THEN
        !----------------------------------------------------------------------
        ! RFM variables.
        !----------------------------------------------------------------------
        IF ( l_use_area ) THEN
          ! Set drainage area from ancillary.
          rivers%rfm_iarea_rp(ip) = NINT( rivers%rivers_dra(irx,iry) )
        ELSE
          ! Ancillary not present. Set area above threshold, so that all
          ! points will be identified as rivers.
          rivers%rfm_iarea_rp(ip) = a_thresh + 1
        END IF
        ! Set points to be either land or river.
        IF (rivers%rfm_iarea_rp(ip) > a_thresh) THEN
          rivers%rfm_land_rp(ip) = rfm_river  ! river
        ELSE
          rivers%rfm_land_rp(ip) = rfm_land   ! land
        END IF

      ELSE IF ( i_river_vn == rivers_trip ) THEN
        !----------------------------------------------------------------------
        ! TRIP variables.
        !----------------------------------------------------------------------
        rivers%rivers_seq_rp(ip) = NINT(rivers%rivers_seq(irx,iry))
        IF ( rivers%rivers_seq_rp(ip) > nseqmax ) THEN
          nseqmax = rivers%rivers_seq_rp(ip)
        END IF

      END IF  !  i_river_vn

      !------------------------------------------------------------------------
      ! Set values of coupling variables.
      !------------------------------------------------------------------------
      IF ( l_riv_number ) THEN
        rivers%rivers_outflow_number_rp(ip) =                                  &
                                    NINT(rivers%rivers_outflow_number(irx,iry))
      END IF

      !------------------------------------------------------------------------
      ! Set overbank inundation ancillary variables, if required.
      !------------------------------------------------------------------------
      IF ( l_riv_overbank .AND. overbank_model == overbank_hypsometric ) THEN
        logn_mean_rp(ip)  = logn_mean(irx,iry)
        logn_stdev_rp(ip) = logn_stdev(irx,iry)
      END IF

    END SELECT  !  river_mask

  END DO  !  iy
END DO  !  ix

! Double check that we have found all the river points.
IF ( ip /= np_rivers ) THEN
  CALL log_fatal( RoutineName,                                                 &
                  "Failure of logic; did not find all river points." )
END IF

RETURN
END SUBROUTINE set_river_point_values

!##############################################################################

SUBROUTINE find_downstream_points( direction_grid, nextx_grid, nexty_grid,     &
                                   rivers_index_rp, river_mask,                &
                                   rivers_next_rp )

!------------------------------------------------------------------------------
! Description:
!   Identify the next downstream point in the river vector.
!------------------------------------------------------------------------------

USE jules_rivers_mod, ONLY:                                                    &
  flow_dir_delta, np_rivers, nx_rivers, ny_rivers

USE rivers_regrid_mod, ONLY: get_xy_pos

IMPLICIT NONE

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  direction_grid(nx_rivers,ny_rivers),                                         &
    ! River routing direction index.
  nextx_grid(nx_rivers,ny_rivers),                                             &
    ! x index of the next downstream point.
  nexty_grid(nx_rivers,ny_rivers),                                             &
    ! y index of the next downstream point.
  rivers_index_rp(np_rivers),                                                  &
    ! Index of points where routing is calculated.
    ! This index refers to position in the river grid.
  river_mask(nx_rivers,ny_rivers)
    ! Indicates type of each point.

!------------------------------------------------------------------------------
! Array arguments with INTENT(OUT)
!------------------------------------------------------------------------------
INTEGER, INTENT(OUT) ::                                                        &
  rivers_next_rp(np_rivers)
    ! Index (river point number) of the next downstream point.

!------------------------------------------------------------------------------
! Local parameters.
!------------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'FIND_DOWNSTREAM_POINTS'

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  inext, ip, ix, iy, jnext
    ! Counters and index variables.

!end of header
!------------------------------------------------------------------------------

! Note that there is no need to initialise rivers_next_rp as we set a value at
! every point.

DO ip = 1,np_rivers

  ! Get location on the river grid.
  CALL get_xy_pos(rivers_index_rp(ip),nx_rivers,ny_rivers,ix,iy)

  SELECT CASE ( river_mask(ix,iy) )

  CASE ( land_in_domain )

    ! This is a "regular" river point with a defined flow direction (not a
    ! special case such as a mouth). The next point can be either another river
    ! point or flow across the edge of the river domain or river grid.

    IF ( ANY( nextx_grid(ix,iy) == flow_dir_river_edge(:) ) ) THEN

      ! Flow across the edge of the river domain or grid.
      IF ( l_use_direction ) THEN
        rivers_next_rp(ip) = -direction_grid(ix,iy)
      END IF

    ELSE

      ! Flow to another river location in the grid.
      inext = nextx_grid(ix,iy)
      jnext = nexty_grid(ix,iy)

      !  Check if the next location is also in the river point vector.
      IF ( grid_riv_pt_number(inext, jnext) > 0 ) THEN
        ! The next location is a river (vector) point.
        rivers_next_rp(ip) = grid_riv_pt_number(inext,jnext)
      ELSE
        ! The next point is a river but it is not in the valid river domain
        ! (not in the river point vector). For some configurations we will save
        ! the direction in which this next point lies.
        ! It is relatively straightforward to get the direction if that was a
        ! direct input (l_use_direction=T), but more involved if we start with
        ! nextx and nexty fields - and for now we do not attempt to deal with
        ! that possibility (which is not yet allowed).
        ! Note that because the next point is on the grid we currently know its
        ! location (i.e. we have lat-lon values) but this information will not
        ! be available to the science code (which only gets the coordinates
        ! of river points).
        IF ( l_use_direction ) THEN
          rivers_next_rp(ip) = -direction_grid(ix,iy)
        END IF
      END IF  !  grid_riv_pt_number

    END IF  !  nextx_grid

  CASE ( river_mouth )

    ! A river mouth. Identify this with a special value < 0.
    rivers_next_rp(ip) = river_mouth

  CASE ( inland_drainage )
    ! An inand drainage point. Identify this with a special value < 0.
    rivers_next_rp(ip) = inland_drainage

  CASE DEFAULT
    CALL log_fatal( RoutineName,                                               &
                    "Unexpected flow direction (nextx_grid): "              // &
                    TRIM(to_string(nextx_grid(ix,iy))) )

  END SELECT  !  river_mask

END DO  !  ip

END SUBROUTINE find_downstream_points

!##############################################################################

SUBROUTINE check_river_coverage( map_river_to_land_points, global_proj_x_land, &
                 global_proj_y_land, rivers_x_coord_rp, rivers_y_coord_rp )

!------------------------------------------------------------------------------
! Description:
!   Assesses the extent to which the land and river layers overlap, so as to
!   produce diagnostic messages.
!------------------------------------------------------------------------------

USE jules_rivers_mod, ONLY: land_dx, land_dy, l_trivial_mapping, np_rivers,    &
                            rivers_dx, rivers_dy, rivers_regrid

USE model_grid_mod, ONLY: global_land_pts

IMPLICIT NONE

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  map_river_to_land_points(np_rivers)
    ! List of coincident land point numbers, on river points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  global_proj_x_land(global_land_pts),                                         &
    ! x coordinates of land points.
  global_proj_y_land(global_land_pts),                                         &
    ! y coordinates of land points.
  rivers_x_coord_rp(np_rivers),                                                &
    ! River routing point projection x coordinates.
  rivers_y_coord_rp(np_rivers)
    ! River routing point projection y coordinates.

!------------------------------------------------------------------------------
! Local scalar parameters.
!------------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'CHECK_RIVER_COVERAGE'

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  num_match,                                                                   &
    ! Number of points for which a matching point is found.
  num_no_match
    ! Number of points for which no matching point is found.

REAL(KIND=real_jlslsm) ::                                                      &
  dx_toler, dy_toler
    ! Tolerances. Units depend on the coordinate system in use.

!------------------------------------------------------------------------------
! Local array variables.
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  land_x_bounds_edge(2),                                                       &
    ! x coordinates of westermost and easternmost edges of land gridboxes.
  land_y_bounds_edge(2),                                                       &
    ! y coordinates of southernmost and northernmost edges of land gridboxes.
  rivers_x_bounds_edge(2),                                                     &
    ! x coordinates of westermost and easternmost edges of river gridboxes.
  rivers_y_bounds_edge(2)
    ! y coordinates of southernmost and northernmost edges of river gridboxes.

!end of header
!------------------------------------------------------------------------------

IF ( rivers_regrid ) THEN

  !----------------------------------------------------------------------------
  ! Regridding can take place between grids of different resolutions and with
  ! grids that do not nest (i.e. a gridbox on one grid can straddle multiple
  ! gridboxes on the other grid). As such it is non-trivial to determine the
  ! extent to which each land gridbox is covered by river boxes, and vice
  ! versa. Instead we test and report on some necessary but not sufficient
  ! conditions.
  !----------------------------------------------------------------------------

  ! Get the coordinates at the outer edges of the extreme land gridboxes.
  ! (For some configurations these have already been calculated.)
  land_x_bounds_edge(1) = MINVAL( global_proj_x_land ) - 0.5 * land_dx
  land_x_bounds_edge(2) = MAXVAL( global_proj_x_land ) + 0.5 * land_dx
  land_y_bounds_edge(1) = MINVAL( global_proj_y_land ) - 0.5 * land_dy
  land_y_bounds_edge(2) = MAXVAL( global_proj_y_land ) + 0.5 * land_dy

  CALL log_info( RoutineName,                                                  &
                 "Outer edges of land gridboxes are at x="                 //  &
                  TRIM(to_string(land_x_bounds_edge(1))) // ' to '         //  &
                  TRIM(to_string(land_x_bounds_edge(2))) // ' y='          //  &
                  TRIM(to_string(land_y_bounds_edge(1))) // ' to '         //  &
                  TRIM(to_string(land_y_bounds_edge(2))) )

  ! Get the coordinates at the outer edges of the extreme river gridboxes.
  rivers_x_bounds_edge(1) = MINVAL( rivers_x_coord_rp(:) ) - 0.5 * rivers_dx
  rivers_x_bounds_edge(2) = MAXVAL( rivers_x_coord_rp(:) ) + 0.5 * rivers_dx
  rivers_y_bounds_edge(1) = MINVAL( rivers_y_coord_rp(:) ) - 0.5 * rivers_dy
  rivers_y_bounds_edge(2) = MAXVAL( rivers_y_coord_rp(:) ) + 0.5 * rivers_dy

  CALL log_info( RoutineName,                                                  &
                 "Outer edges of river gridboxes are at x="                 // &
                 TRIM(to_string(rivers_x_bounds_edge(1))) // ' to '         // &
                 TRIM(to_string(rivers_x_bounds_edge(2))) // ' y='          // &
                 TRIM(to_string(rivers_y_bounds_edge(1))) // ' to '         // &
                 TRIM(to_string(rivers_y_bounds_edge(2))) )

  ! Look for river gridboxes that have some area outside the land area, by
  ! comparing the coordinates of the edges of the extreme gridboxes, allowing
  ! a small tolerance based on river gridbox size.
  dx_toler = frac_toler * rivers_dx
  dy_toler = frac_toler * rivers_dy
  IF ( rivers_x_bounds_edge(1) < land_x_bounds_edge(1) - dx_toler  .OR.        &
       rivers_x_bounds_edge(2) > land_x_bounds_edge(2) + dx_toler  .OR.        &
       rivers_y_bounds_edge(1) < land_y_bounds_edge(1) - dy_toler  .OR.        &
       rivers_y_bounds_edge(2) > land_y_bounds_edge(2) + dy_toler ) THEN
    CALL log_info( RoutineName,                                                &
                   "There are river gridboxes outside the area of land. " //   &
                   " Runoff to these river sections will be zero." )
  END IF

  ! Look for land gridboxes that have some area outside the river area,
  ! allowing a small tolerance based on land gridbox size.
  dx_toler = frac_toler * land_dx
  dy_toler = frac_toler * land_dy
  IF ( land_x_bounds_edge(1) < rivers_x_bounds_edge(1) - dx_toler  .OR.        &
       land_x_bounds_edge(2) > rivers_x_bounds_edge(2) + dx_toler  .OR.        &
       land_y_bounds_edge(1) < rivers_y_bounds_edge(1) - dy_toler  .OR.        &
       land_y_bounds_edge(2) > rivers_y_bounds_edge(2) + dy_toler ) THEN
    CALL log_info( RoutineName,                                                &
                   "There are land gridboxes outside the area of rivers. " //  &
                   " Runoff from these will not enter the river system." )
  END IF

ELSE

  !----------------------------------------------------------------------------
  ! .NOT. river_regrid
  ! In this case grids are of the same resolution and we have 1:1 mappings.
  !----------------------------------------------------------------------------
  ! Report on how many land and river points match.
  IF ( l_trivial_mapping ) THEN

    CALL log_info( RoutineName,                                                &
                   "Land and river points are identical (trivial mapping)." )

  ELSE

    ! Report on river points for which no matching land point was found.
    num_no_match = COUNT( map_river_to_land_points(:) == 0 )
    IF ( num_no_match == 0 ) THEN
      CALL log_info( RoutineName,                                              &
                     "All river points have a matching land point." )
    ELSE
      CALL log_info( RoutineName,                                              &
                     "Number of river points without a matching land "      // &
                     "point = " // TRIM(to_string(num_no_match))            // &
                     ". Runoff to these river sections will be zero." )
    END IF

    ! Report on land points for which no matching river point was found.
    ! First count river points with matching land points.
    num_match = COUNT( map_river_to_land_points(:) > 0 )
    IF ( num_match == global_land_pts ) THEN
      CALL log_info( RoutineName,                                              &
                     "All land points have a matching river point." )
    ELSE
      CALL log_info( RoutineName,                                              &
                     "Number of land points without a matching river "      // &
                     "point = "                                             // &
                     TRIM(to_string(global_land_pts - num_match))           // &
                     ". Runoff from these will not enter the river system." )
    END IF

  END IF  !  l_trivial_match

END IF  !  rivers_regrid

RETURN
END SUBROUTINE check_river_coverage

!##############################################################################

END MODULE init_rivers_props_mod
#endif
