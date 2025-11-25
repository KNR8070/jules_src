! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE init_rivers_props_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE init_rivers_props(rivers, rivers_data)

USE ancil_namelist_mod, ONLY: check_namelist_values, initialise_namelist_vars, &
                              set_constant_vars, set_file_vars

USE mpi, ONLY: mpi_comm_world

USE io_constants, ONLY: max_sdf_name_len, max_file_name_len, namelist_unit

USE grid_utils_mod, ONLY: grid_create, grid_info

USE input_mod, ONLY:                                                           &
    od_grid => grid,                                                           &
    dummy_grid => grid,                                                        &
    use_subgrid

USE missing_data_mod, ONLY: imdi, rmdi

USE ancil_info, ONLY: land_pts

USE logging_mod, ONLY: log_info, log_warn, log_fatal

USE fill_variables_from_file_mod, ONLY: fill_variables_from_file

USE string_utils_mod, ONLY: to_string

USE templating_mod, ONLY: tpl_has_var_name

USE model_interface_mod, ONLY: identifier_len

USE model_grid_mod, ONLY: latitude, longitude, global_land_pts, model_grid

USE conversions_mod, ONLY: pi_over_180

USE planet_constants_mod, ONLY: planet_radius  ! the Earth's radius (m)

USE jules_rivers_mod, ONLY:                                                    &
     i_river_vn, inland_drainage, river_mouth, rivers_rfm, rivers_trip,        &
     flow_dir_delta, np_rivers, nx_rivers, ny_rivers, rfm_land, rfm_river,     &
     rivers_dlat, rivers_dlon, rivers_lat1, rivers_lon1, nx_grid, ny_grid,     &
     nseqmax, reg_lon1, reg_lat1, reg_dlon, reg_dlat, rivers_dx,               &
     rivers_reglatlon, rivers_regrid, l_rivers,                                &
     a_thresh,                                                                 &
     ! types
     rivers_type,rivers_data_type

USE rivers_utils, ONLY: rivers_earth_area

USE rivers_regrid_mod, ONLY: rivers_remap_match, rivers_remap_unmatch,         &
                             rivers_get_xy_pos

USE init_overbank_props_mod, ONLY: init_overbank_props

USE overbank_inundation_mod, ONLY:                                             &
     l_riv_overbank, l_riv_hypsometry, use_rosgen,                             &
     logn_mean, logn_stdev, logn_mean_rp, logn_stdev_rp,                       &
     qbf, wbf, dbf,                                                            &
     riv_a, riv_b, riv_c, riv_f, coef_b, exp_c

USE jules_irrig_mod, ONLY: l_irrig_limit

USE jules_print_mgr, ONLY:                                                     &
  jules_message,                                                               &
  jules_print

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!    Reads in details of the river routing grid and river properties,
!    and allocates various river variables.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Work variables

INTEGER :: ERROR, error_sum  ! Error indicators

INTEGER :: ntasks ! Parallel mode indicator

INTEGER, PARAMETER :: nvars_max = 6 ! The maximum possible number of
                                          ! routing variables

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'INIT_RIVERS_PROPS'

INTEGER :: nvars_const         ! The number of variables to be set using
                               ! constant values.
INTEGER :: nvars_required      ! The number of routing variables that are
                               ! required in this configuration
INTEGER :: nvars_optional      ! The number of optional routing variables
                               ! in this configuration
CHARACTER(LEN=identifier_len) :: required_vars(nvars_max)
                               ! The variable identifiers of the required
                               ! variables
CHARACTER(LEN=identifier_len) :: optional_vars(nvars_max)
                               ! The variable identifiers of any optional
                               ! variables

INTEGER :: nvars_file          ! The number of variables that will be set
                               ! from the given file (template?)

LOGICAL ::                                                                     &
  l_have_template
    ! Flag indicating if variable-name templating is being used.

LOGICAL ::                                                                     &
  use_var(nvars_max)
    ! Flag indicating if a variable is required.

CHARACTER(LEN=max_file_name_len) :: file_name(nvars_max)
    ! The name of the file to use for each variable.

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

CHARACTER(LEN=max_file_name_len) :: file_name_coords
                      ! The name of the file to be used to read coordinate
                      ! values from.

INTEGER :: nvars      ! The number of variables in this section
CHARACTER(LEN=identifier_len) :: var(nvars_max)
                      ! The variable identifiers of the variables
LOGICAL :: use_file(nvars_max)
                      !   T - the variable uses the file
                      !   F - the variable is set using a constant value
CHARACTER(LEN=max_sdf_name_len) :: var_name(nvars_max)
                      ! The name of each variable in the file
CHARACTER(LEN=max_sdf_name_len) :: tpl_name(nvars_max)
                      ! The name to substitute in a template for each
                      ! variable
REAL(KIND=real_jlslsm) :: const_val(nvars_max)
                      ! The constant value to use for each variable if
                      ! use_file = F for that variable

INTEGER, PARAMETER :: flow_dir_sea = 0
                      ! The value of flow direction that is used to indicate
                      ! a sea point. This value is used to set the flow
                      ! direction variable in this code, but it need not be
                      ! the value used to indicate sea in the input ancillary
                      ! file. This value must not be any of 1:8, river_mouth,
                      ! inland_drainage, or land_off_domain.
INTEGER, PARAMETER :: land_off_domain = -1
                      ! The value of flow direction that is used to indicate
                      ! a land point that is outside of the valid river domain,
                      ! i.e. it is on the river grid but not in the area
                      ! covered by land, hence it cannot become a river point.
                      ! This value must not be any of 1:8, river_mouth,
                      ! inland_drainage, or flow_dir_sea.
INTEGER :: i,ip,ix,iy,j  ! Index variables
INTEGER :: inext, jnext
INTEGER :: i1, i2, ilat, ilon, step

REAL(KIND=real_jlslsm) :: reg_lat2, reg_lon2, min_lon, dlat, dlon

REAL(KIND=real_jlslsm) :: contribarea
                      ! Upstream contributing drainage area (km^2)
REAL(KIND=real_jlslsm) :: rix, riy ! Locations on grid.

!Conversion from m2 to km2
REAL(KIND=real_jlslsm), PARAMETER :: m2tokm2 = 1.0e-6
REAL(KIND=real_jlslsm), PARAMETER :: global_lon_threshold = 359.9
  ! Threshold value used when determining if a grid is cyclic (global) in the
  ! longitudinal direction (degrees). This should be close to 360 degrees.
REAL(KIND=real_jlslsm), PARAMETER :: location_toler = 1.0e-3
  ! Tolerance (fraction of a gridbox)  used when testing whether model points
  ! lie on a regular grid.

LOGICAL :: use_sub_local
LOGICAL :: set_dra
LOGICAL :: l_cyclic_x
  ! Flag indicating a river grid that is cyclic (global) in the x direction.

INTEGER, PARAMETER :: flow_dir_river(10) =                                     &
  [ 1, 2, 3, 4, 5, 6, 7, 8, river_mouth, inland_drainage ]
  ! The values of flow direction that are acceptable for a river point.
  ! Effectively these also the only values that indicate land points in the
  ! flow direction field. The first 8 places represent "compass directions"
  ! for the 8 points immediately neighbouring a given point.

INTEGER, ALLOCATABLE :: rivers_dir_int(:,:)
  ! Integer version of rivers_dir.
  ! We use the REAL version for i.o (JULES cannot read an integer ancillary
  ! field) but thereafter the integer version is easier to use.

! Remapping full grid to vector points
INTEGER, ALLOCATABLE :: mapfr(:,:)                   ! map full to river

! The names and sizes of the x and y dimensions

CHARACTER(LEN=max_sdf_name_len) :: x_dim_name, y_dim_name
INTEGER :: nx, ny

TYPE(grid_info) :: local_grid, rivers_grid

! types
TYPE(rivers_type), INTENT(IN OUT) :: rivers
TYPE(rivers_data_type), INTENT(IN OUT), TARGET :: rivers_data

NAMELIST  / jules_rivers_props/                                                &
   x_dim_name, y_dim_name, nx, ny, FILE, coordinate_file,                      &
   nvars, var, use_file, var_name, tpl_name, const_val,                        &
   rivers_regrid, rivers_reglatlon,                                            &
   nx_grid, ny_grid, reg_lat1, reg_lon1, reg_dlon, reg_dlat, rivers_dx

! If rivers are not on, there is nothing to do
IF ( .NOT. l_rivers ) RETURN

!-----------------------------------------------------------------------------
! Initialise
!-----------------------------------------------------------------------------
! Initialisation of some common variables.
CALL initialise_namelist_vars( nvars, nvars_const, nvars_file,                 &
                               nvars_required, l_have_template,                &
                               FILE, const_val, use_file, use_var,             &
                               file_name, tpl_name, var, var_name )
! Initialisation of further variables.
coordinate_file = '' ! Empty file name.

IF ( global_land_pts <= 1 ) THEN
  l_rivers = .FALSE.
  CALL log_warn(RoutineName,                                                   &
                "River routing not appropriate for single point runs. " //     &
                "River routing disabled.")

ELSE

  !---------------------------------------------------------------------------
  ! Read river routing properties namelist
  !---------------------------------------------------------------------------
  CALL log_info(RoutineName, "Reading JULES_RIVERS_PROPS namelist...")

  READ(namelist_unit, NML = jules_rivers_props, IOSTAT = ERROR)
  IF ( ERROR /= 0 )                                                            &
    CALL log_fatal(RoutineName,                                                &
                   "Error reading namelist JULES_RIVERS_PROPS " //             &
                   "(IOSTAT=" // TRIM(to_string(ERROR)) // ")")

  !---------------------------------------------------------------------------
  ! Define regular routing grid lat/lon
  !---------------------------------------------------------------------------

  use_sub_local = use_subgrid
  use_subgrid   = .FALSE.

  ! Temporarily copy saved grid (set for full model grid) to a local variable
  ! before overwriting to define the river routing grid
  local_grid = grid_create(dummy_grid%is_1d,dummy_grid%dim_name,               &
                    dummy_grid%nx,dummy_grid%x_name,dummy_grid%nx,             &
                    dummy_grid%y_name,dummy_grid%ny)

  ! Get the name of the file to be used to read coordinates. This depends on
  ! whether variable-name templating is used.
  ! First we check that a file name was provided.
  IF ( LEN_TRIM(FILE) == 0 ) THEN
    CALL log_fatal("init_rivers_props", "No file name provided")
  END IF

  ! Establish if templating is specified.
  IF ( tpl_has_var_name(FILE) ) THEN
    ! We will use a separate variable to get the file name.
    ! Check that a file name was provided.
    IF ( LEN_TRIM(coordinate_file) == 0 ) THEN
      CALL log_fatal("init_rivers_props",                                      &
                     "No name given for coordinate_file.")
    END IF
    file_name_coords = coordinate_file
  ELSE
    ! Use the only ancillary file.
    file_name_coords = FILE
    ! If another file was indicated, clarify that it will not be used.
    IF ( LEN_TRIM(coordinate_file) > 0 ) THEN
      CALL log_info("init_rivers_props",                                       &
                    "No templating; coordinate_file will be ignored.")
    END IF
  END IF

  ! Read latitude dimension from file
  od_grid = grid_create( grid_is_1d = .TRUE.,                                  &
                         dim_name = TRIM(y_dim_name), npoints = ny )
  ALLOCATE(rivers_data%rivers_ygrid(ny), STAT = ERROR)
  rivers%rivers_ygrid => rivers_data%rivers_ygrid
  CALL fill_variables_from_file(file_name_coords,                              &
                                [ 'rivers_ygrid' ],[ TRIM(y_dim_name) ])

  ! Read longitude dimension from file
  od_grid = grid_create( grid_is_1d = .TRUE.,                                  &
                         dim_name = TRIM(x_dim_name), npoints = nx)
  ALLOCATE(rivers_data%rivers_xgrid(nx), STAT = ERROR)
  rivers%rivers_xgrid => rivers_data%rivers_xgrid
  CALL fill_variables_from_file(file_name_coords,                              &
                                [ 'rivers_xgrid' ],[ TRIM(x_dim_name) ])

    !---------------------------------------------------------------------------
    ! Build the routing grid object from the namelist values
    !---------------------------------------------------------------------------
  dummy_grid = grid_create( .FALSE., "", 0, x_dim_name, nx, y_dim_name, ny)
  rivers_grid = grid_create( .FALSE., "", 0, x_dim_name, nx, y_dim_name, ny)

  nx_rivers = rivers_grid%nx
  ny_rivers = rivers_grid%ny

  ! Set all river routing arrays to be S to N irrespective of how latitudes
  ! are stored in the ancillary files
  IF ( rivers%rivers_ygrid(1) > MINVAL(rivers%rivers_ygrid) ) THEN
    i1   = ny_rivers
    i2   = 1
    step = -1
  ELSE
    i1   = 1
    i2   = ny_rivers
    step = 1
  END IF

  !---------------------------------------------------------------------------
  ! Set up routing properties using namelist values
  !---------------------------------------------------------------------------

  ! Set up the required and optional routing variables

  SELECT CASE ( i_river_vn )

  CASE ( rivers_trip )

    nvars_required = 2
    required_vars(1:nvars_required) = [ 'direction', 'sequence ' ]
    nvars_optional = 2
    optional_vars(1:nvars_optional) = [ 'latitude_2d ', 'longitude_2d' ]

  CASE ( rivers_rfm )

    IF ( rivers_reglatlon ) THEN
      nvars_required = 1
      required_vars(1:nvars_required) = [ 'direction' ]
      nvars_optional = 4
      optional_vars(1:nvars_optional) =                                        &
        [ 'latitude_2d ', 'longitude_2d',                                      &
           'area        ', 'sequence    ' ]
    ELSE
      nvars_required = 3
      required_vars(1:nvars_required) = [ 'direction   ',                      &
                                  'latitude_2d ', 'longitude_2d' ]
      nvars_optional = 2
      optional_vars(1:nvars_optional) =                                        &
        [ 'area        ', 'sequence    ' ]
    END IF

  CASE DEFAULT
    CALL log_fatal(RoutineName,                                                &
                   "Do not recognise i_river_vn = '" //                        &
                   TRIM(to_string(i_river_vn)) // "'")
  END SELECT

  !----------------------------------------------------------------------------
  ! Check that variables in the namelist have been provided and set up other
  ! variables.
  !----------------------------------------------------------------------------
  CALL check_namelist_values( nvars, nvars_max, nvars_required,                &
                              RoutineName, FILE, const_val,                    &
                              use_file, required_vars, tpl_name, var,          &
                              nvars_const, nvars_file, l_have_template,        &
                              use_var, file_name, var_name,                    &
                              nvars_optional = nvars_optional,                 &
                              optional_vars = optional_vars )

  !---------------------------------------------------------------------------
  ! Allocate routing-specific arrays from ancillary information
  !---------------------------------------------------------------------------

  IF ( nx_rivers > 0 .AND. ny_rivers > 0 ) THEN

    ALLOCATE(rivers_data%rivers_dir(nx_rivers, ny_rivers),      STAT = ERROR )
    error_sum = ERROR
    ALLOCATE( rivers_dir_int(nx_rivers, ny_rivers),  STAT = ERROR )
    error_sum = error_sum + ERROR
    ALLOCATE(rivers_data%rivers_seq(nx_rivers, ny_rivers),      STAT = ERROR )
    error_sum = error_sum + ERROR
    ALLOCATE(rivers_data%rivers_dra(nx_rivers, ny_rivers),      STAT = ERROR )
    error_sum = error_sum + ERROR
    ALLOCATE(rivers_data%rivers_lat2d(nx_rivers, ny_rivers),    STAT = ERROR )
    error_sum = error_sum + ERROR
    ALLOCATE(rivers_data%rivers_lon2d(nx_rivers, ny_rivers),    STAT = ERROR )
    error_sum = error_sum + ERROR
    ALLOCATE( mapfr(nx_rivers, ny_rivers),           STAT = ERROR )
    error_sum = error_sum + ERROR
    IF ( error_sum /= 0 ) THEN
      CALL log_fatal(RoutineName,                                              &
                     "Error allocating for rivers arrays.")
    END IF

    ! Initialise to impossible values.

    rivers_data%rivers_dir(:,:)      = rmdi
    rivers_dir_int(:,:)  = imdi
    rivers_data%rivers_seq(:,:)      = rmdi
    rivers_data%rivers_dra(:,:)      = rmdi
    rivers_data%rivers_lat2d(:,:)    = rmdi
    rivers_data%rivers_lon2d(:,:)    = rmdi
    mapfr(:,:)           = 0

    ! Associate pointers
    rivers%rivers_dir => rivers_data%rivers_dir
    rivers%rivers_seq => rivers_data%rivers_seq
    rivers%rivers_dra => rivers_data%rivers_dra
    rivers%rivers_lat2d => rivers_data%rivers_lat2d
    rivers%rivers_lon2d => rivers_data%rivers_lon2d


  ELSE

    CALL log_fatal(RoutineName,                                                &
                   "Invalid routing grid dimensions provided (nx_rivers = "//  &
                    TRIM(to_string(nx_rivers)) // ", ny_rivers = " //          &
                    TRIM(to_string(ny_rivers)) // "). Check inputs. ")

  END IF

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

  !---------------------------------------------------------------------------
  ! Create INTEGER version of rivers%rivers_dir. The REAL version is not used
  ! after this.
  !---------------------------------------------------------------------------
  DO iy = 1, ny_rivers
    DO ix = 1, nx_rivers
      rivers_dir_int(ix,iy) = NINT( rivers%rivers_dir(ix,iy) )
    END DO
  END DO

  !---------------------------------------------------------------------------
  ! Setup regular river routing grid (assuming same as model grid if not set
  ! from namelist or model grid information)
  !---------------------------------------------------------------------------

  ! Wrap longitude inputs -180 to +180
  DO ix = 1,nx
    IF ( rivers%rivers_xgrid(ix) > 180.0 ) THEN
      rivers%rivers_xgrid(ix) = rivers%rivers_xgrid(ix) - 360.0
    END IF
  END DO

  ! Set regular Lat/Lon grid variables (S to N)
  rivers_lat1 = MINVAL( rivers%rivers_ygrid )
  rivers_lon1 = MINVAL( rivers%rivers_xgrid )
  rivers_dlat = ABS( rivers%rivers_ygrid(2) - rivers%rivers_ygrid(1) )
  rivers_dlon = ABS( rivers%rivers_xgrid(2) - rivers%rivers_xgrid(1) )

  CALL mpi_comm_size(mpi_comm_world,ntasks,ERROR)
  IF ( ntasks == 1 ) THEN
    IF ( reg_dlat > -900.0 .OR. reg_dlon > -900.0 .OR.                         &
        ny_grid > 0 .OR. nx_grid > 0 .OR.                                      &
        reg_lat1 > -900.0 .OR. reg_lon1  > -900.0 ) THEN

      CALL log_info(RoutineName,                                               &
                   "Running in serial mode and with grid_is_1d==.FALSE. " //   &
                   "means that the values; nx_grid, ny_grid, reg_dlat, "  //   &
                   "reg_dlon, reg_lat1 and reg_lon1 will be ignored "     //   &
                   "in the init_rivers_props namelist.")
    END IF
  END IF

  IF ( reg_lon1 <= -900.0 .OR. reg_lat1 <= -900.0 ) THEN

    ! Infer model grid settings for 1-dimensional grid input
    IF ( model_grid%nx == 1 .OR. .NOT. rivers_reglatlon ) THEN

      CALL log_info(RoutineName,                                               &
              "No regular model grid set - assuming same as routing grid")
      nx_grid  = nx_rivers
      ny_grid  = ny_rivers
      reg_lat1 = rivers_lat1
      reg_lon1 = rivers_lon1
      reg_dlon = rivers_dlon
      reg_dlat = rivers_dlat

    ELSE

      CALL log_info(RoutineName,                                               &
               "Using 2D input model grid to define grid parameters")
      nx_grid  = SIZE( latitude,1 )
      ny_grid  = SIZE( latitude,2 )
      reg_lat1 = MINVAL( latitude )
      reg_lon1 = MINVAL( longitude )
      reg_dlon = ABS( longitude(2,2) - longitude(1,1) )
      reg_dlat = ABS( latitude(2,2) - latitude(1,1) )

    END IF
  ELSE
    IF ( reg_dlat <= -900.0 .OR. reg_dlon <= -900.0 .OR.                       &
         ny_grid <= 0 .OR. nx_grid <= 0 ) THEN
      CALL log_info(RoutineName,                                               &
                    "If reg_lon1 and reg_lat1 are specified in namelist," //   &
          " then you must specify reg_dlat, reg_dlon, ny_grid, nx_grid too." )
    END IF
  END IF

  CALL log_info(RoutineName,"Setting regular routing grid " //                 &
            " Minimum " // TRIM(x_dim_name) // ": " //                         &
              TRIM(to_string(MINVAL(rivers%rivers_xgrid)))  //                 &
            " Minimum " // TRIM(y_dim_name) // ": " //                         &
              TRIM(to_string(MINVAL(rivers%rivers_ygrid))) //                  &
            " Maximum " // TRIM(x_dim_name) // ": " //                         &
              TRIM(to_string(MAXVAL(rivers%rivers_xgrid))) //                  &
            " Maximum " // TRIM(y_dim_name) // ": " //                         &
              TRIM(to_string(MAXVAL(rivers%rivers_ygrid))))

  !---------------------------------------------------------------------------
  ! Decide if the river grid is cyclic (global) in the x direction. We can
  ! only do this reliably for a regular lat-lon grid - otherwise we have to
  ! assume that it is not cyclic.
  !---------------------------------------------------------------------------
  l_cyclic_x = .FALSE.
  IF ( rivers_reglatlon ) THEN
    IF ( MAXVAL(rivers%rivers_xgrid) - rivers_lon1 + rivers_dlon               &
         > global_lon_threshold ) THEN
      l_cyclic_x = .TRUE.
      CALL log_info(RoutineName,"River grid is cyclic in longitude.")
    END IF
  END IF

  !---------------------------------------------------------------------------
  ! Process the namelist values and set derived variables
  !---------------------------------------------------------------------------

  ! Calculate maximum lat and lon of land grid.
  reg_lat2 = reg_lat1 + ny_grid * reg_dlat
  reg_lon2 = reg_lon1 + nx_grid * reg_dlon

  !---------------------------------------------------------------------------
  ! Get the number of river points.
  ! Consider points on the valid domain only (i.e. the part of the river grid
  ! that is covered by land points).
  !---------------------------------------------------------------------------
  np_rivers = 0
  DO iy = 1,ny_rivers
    DO ix = 1,nx_rivers
      IF ( (rivers%rivers_xgrid(ix) < reg_lon1) .OR.                           &
           (rivers%rivers_ygrid(iy) < reg_lat1) .OR.                           &
           (rivers%rivers_xgrid(ix) > reg_lon2) .OR.                           &
           (rivers%rivers_ygrid(iy) > reg_lat2) ) THEN
        ! This point is not in the valid river domain.
        ! If this is a land point (as indicated by the flow direction) set
        ! the direction to land_off_domain, or if this is sea (all other
        ! values) set to flow_dir_sea.
        IF ( ANY( rivers_dir_int(ix,iy) == flow_dir_river(:) ) ) THEN
          rivers_dir_int(ix,iy) = land_off_domain
        ELSE
          rivers_dir_int(ix,iy) = flow_dir_sea
        END IF
      ELSE IF ( ANY( rivers_dir_int(ix,iy) == flow_dir_river(:) ) ) THEN
        ! This point is in the valid river domain and is land.
        ! It is a river point.
        np_rivers = np_rivers + 1
      ELSE
        ! This point is in the valid river domain but is not land.
        ! Set flow direction to indicate sea. This is not currently necessary
        ! but doing so ensures that the same value is used for sea across
        ! the grid, in case any later code changes rely on that.
        rivers_dir_int(ix,iy) = flow_dir_sea
      END IF
    END DO
  END DO

  ! At this point all values of rivers_dir_int are one of the following:
  ! values in flow_dir_river: these are land values in the river domain
  ! land_off_domain: land but not in the valid river domain
  ! flow_dir_sea: sea.

  CALL log_info(RoutineName,                                                   &
                "River routing points = " // TRIM(to_string(np_rivers)))
  CALL log_info(RoutineName,                                                   &
                "Global land pts = " // TRIM(to_string(global_land_pts)))

  IF ( np_rivers <= 0 ) THEN
    CALL log_fatal(RoutineName,                                                &
                   "Invalid number of valid routing grid points " //           &
                   "(np_rivers = " // TRIM(to_string(np_rivers)) //            &
                   "). Check inputs. ")
  END IF

  ! Allocate land point arrays and initialise
  !---------------------------------------------------------------------------

  ALLOCATE(rivers_data%ir_land_grid(global_land_pts), STAT = ERROR )
  error_sum = ERROR
  IF ( error_sum /= 0 ) THEN
    CALL log_fatal(RoutineName,                                                &
                   "Error allocating overbank variables.")
  END IF


  ! If required, read overbank inundation ancillary information from file
  ! before translating to the river routing grid
  IF ( l_riv_overbank ) CALL init_overbank_props()

  !---------------------------------------------------------------------------
  ! Allocate routing point arrays
  !---------------------------------------------------------------------------

  ALLOCATE(rivers_data%rivers_index_rp(np_rivers),    STAT = ERROR)
  error_sum = ERROR
  ALLOCATE(rivers_data%rivers_next_rp(np_rivers),     STAT = ERROR)
  error_sum = error_sum + ERROR
  ALLOCATE(rivers_data%rivers_seq_rp(np_rivers),      STAT = ERROR)
  error_sum = error_sum + ERROR
  ALLOCATE(rivers_data%rivers_dir_rp(np_rivers),      STAT = ERROR)
  error_sum = error_sum + ERROR
  ALLOCATE(rivers_data%rivers_dra_rp(np_rivers),      STAT = ERROR)
  error_sum = error_sum + ERROR
  ALLOCATE(rivers_data%rivers_lat_rp(np_rivers),      STAT = ERROR)
  error_sum = error_sum + ERROR
  ALLOCATE(rivers_data%rivers_lon_rp(np_rivers),      STAT = ERROR)
  error_sum = error_sum + ERROR
  ALLOCATE(rivers_data%rivers_boxareas_rp(np_rivers), STAT = ERROR)
  error_sum = error_sum + ERROR
  ALLOCATE(rivers_data%il_river_grid(np_rivers),      STAT = ERROR)
  error_sum = error_sum + ERROR
  ALLOCATE(rivers_data%rflow_rp(np_rivers),           STAT = ERROR)
  error_sum = error_sum + ERROR
  ALLOCATE(rivers_data%rrun_rp(np_rivers),            STAT = ERROR)
  error_sum = error_sum + ERROR
  ALLOCATE(rivers_data%rrun_surf_rp(np_rivers),       STAT = ERROR)
  error_sum = error_sum + ERROR
  ALLOCATE(rivers_data%rrun_sub_surf_rp(np_rivers),   STAT = ERROR)
  error_sum = error_sum + ERROR

  ! Allocate TRIP variables
  IF ( i_river_vn == rivers_trip ) THEN
    ALLOCATE(rivers_data%rivers_sto_rp(np_rivers), STAT = ERROR)
    error_sum = error_sum + ERROR
  END IF

  ! Allocate RFM variables
  ALLOCATE(rivers_data%rfm_rivflow_rp(np_rivers),    STAT = ERROR)
  error_sum = error_sum + ERROR

  IF ( i_river_vn == rivers_rfm ) THEN
    ALLOCATE(rivers_data%rfm_flowobs1_rp(np_rivers),  STAT = ERROR )
    error_sum = error_sum + ERROR
    ALLOCATE(rivers_data%rfm_surfstore_rp(np_rivers), STAT = ERROR )
    error_sum = error_sum + ERROR
    ALLOCATE(rivers_data%rfm_substore_rp(np_rivers),  STAT = ERROR )
    error_sum = error_sum + ERROR
    ALLOCATE(rivers_data%rfm_flowin_rp(np_rivers),    STAT = ERROR )
    error_sum = error_sum + ERROR
    ALLOCATE(rivers_data%rfm_bflowin_rp(np_rivers),   STAT = ERROR )
    error_sum = error_sum + ERROR
    ALLOCATE(rivers_data%rfm_iarea_rp(np_rivers),     STAT = ERROR )
    error_sum = error_sum + ERROR
    ALLOCATE(rivers_data%rfm_land_rp(np_rivers),      STAT = ERROR )
    error_sum = error_sum + ERROR
    ALLOCATE(rivers_data%rfm_baseflow_rp(np_rivers),  STAT = ERROR )
    error_sum = error_sum + ERROR
  END IF

  IF ( error_sum /= 0 ) THEN
    CALL log_fatal(RoutineName,                                                &
                   "Error allocating for routing point arrays.")
  END IF

  ! Initialise array values
  rivers_data%rivers_index_rp(:)  = imdi
  rivers_data%rivers_next_rp(:)   = imdi
  rivers_data%rivers_seq_rp(:)    = imdi
  rivers_data%rivers_dir_rp(:)    = imdi
  rivers_data%il_river_grid(:)    = 0
  rivers_data%rflow_rp(:)         = 0.0
  rivers_data%rrun_rp(:)          = 0.0
  rivers_data%rrun_surf_rp(:)     = 0.0
  rivers_data%rrun_sub_surf_rp(:) = 0.0

  ! Initialise TRIP array values
  IF ( i_river_vn == rivers_trip ) THEN
    rivers_data%rivers_sto_rp(:)    = 0.0
  END IF

  ! Initialise RFM array values
  rivers_data%rfm_rivflow_rp(:)   = 0.0

  IF ( i_river_vn == rivers_rfm ) THEN
    rivers_data%rfm_land_rp(:)      = imdi
    rivers_data%rfm_iarea_rp(:)     = imdi
    rivers_data%rfm_flowobs1_rp(:)  = 0.0
    rivers_data%rfm_surfstore_rp(:) = 0.0
    rivers_data%rfm_substore_rp(:)  = 0.0
    rivers_data%rfm_flowin_rp(:)    = 0.0
    rivers_data%rfm_bflowin_rp(:)   = 0.0
    rivers_data%rfm_baseflow_rp(:)  = 0.0
  END IF  !  RFM

  ! Initialise max sequence and point number to zero.
  nseqmax = 0
  ip      = 0

  ! Initialise iarea from drainage?
  set_dra = .FALSE.
  IF ( MAXVAL(rivers_data%rivers_dra) > 0.0 ) set_dra = .TRUE.

  min_lon = MINVAL(rivers_data%rivers_xgrid)

  ! Associate pointers
  rivers%il_river_grid => rivers_data%il_river_grid
  rivers%ir_land_grid => rivers_data%ir_land_grid
  rivers%rivers_dir_rp => rivers_data%rivers_dir_rp
  rivers%rivers_seq_rp => rivers_data%rivers_seq_rp
  rivers%rivers_dra_rp => rivers_data%rivers_dra_rp
  rivers%rivers_index_rp => rivers_data%rivers_index_rp
  rivers%rivers_next_rp => rivers_data%rivers_next_rp
  rivers%rivers_sto_rp => rivers_data%rivers_sto_rp
  rivers%rivers_lat_rp => rivers_data%rivers_lat_rp
  rivers%rivers_lon_rp => rivers_data%rivers_lon_rp
  rivers%rflow_rp => rivers_data%rflow_rp
  rivers%rrun_rp => rivers_data%rrun_rp
  rivers%rrun_surf_rp => rivers_data%rrun_surf_rp
  rivers%rrun_sub_surf_rp => rivers_data%rrun_sub_surf_rp
  rivers%rivers_boxareas_rp => rivers_data%rivers_boxareas_rp
  rivers%rfm_rivflow_rp => rivers_data%rfm_rivflow_rp

  IF ( i_river_vn == rivers_rfm ) THEN
    rivers%rfm_iarea_rp => rivers_data%rfm_iarea_rp
    rivers%rfm_land_rp => rivers_data%rfm_land_rp
    rivers%rfm_flowobs1_rp => rivers_data%rfm_flowobs1_rp
    rivers%rfm_surfstore_rp => rivers_data%rfm_surfstore_rp
    rivers%rfm_substore_rp => rivers_data%rfm_substore_rp
    rivers%rfm_flowin_rp => rivers_data%rfm_flowin_rp
    rivers%rfm_bflowin_rp => rivers_data%rfm_bflowin_rp
    rivers%rfm_baseflow_rp => rivers_data%rfm_baseflow_rp
  END IF  !  RFM

  !---------------------------------------------------------------------------
  ! Set up routing point arrays, and correct 2d lat/lon grid (S to N)
  !---------------------------------------------------------------------------
  DO ix = 1,nx_rivers

    ilat = 0
    IF ( rivers_reglatlon ) THEN
      ilon = NINT( (rivers%rivers_xgrid(ix) - min_lon) / rivers_dlon ) + 1
      IF ( ilon > nx_rivers ) THEN
        ilon = ilon - nx_rivers
      END IF
    ELSE
      ilon = ix
    END IF

    DO iy = i1,i2,step

      ilat = ilat + 1

      IF ( rivers_reglatlon ) THEN
        ! Assume 1d x and y grid variables define lat / lon
        rivers%rivers_lat2d(ilon,ilat) = rivers%rivers_ygrid(iy)
        rivers%rivers_lon2d(ilon,ilat) = rivers%rivers_xgrid(ix)
      END IF

      ! Set values at river points.
      IF ( ANY( rivers_dir_int(ix,iy) == flow_dir_river(:) ) ) THEN

        ip = ip + 1
        rivers%rivers_index_rp(ip) = (ilat-1) * nx_rivers + ilon

        ! Update river vector information from 2D input ancillaries
        rivers%rivers_lon_rp(ip) = rivers%rivers_lon2d(ilon,ilat)
        rivers%rivers_lat_rp(ip) = rivers%rivers_lat2d(ilon,ilat)
        rivers%rivers_dir_rp(ip) = rivers_dir_int(ix,iy)
        rivers%rivers_dra_rp(ip) = rivers%rivers_dra(ix,iy)
        rivers%rivers_seq_rp(ip) = NINT(rivers%rivers_seq(ix,iy))
        IF ( rivers%rivers_seq_rp(ip) > nseqmax ) THEN
          nseqmax = rivers%rivers_seq_rp(ip)
        END IF

        IF ( i_river_vn == rivers_rfm ) THEN
          ! Initialise drainage area from iarea ancillary
          IF ( set_dra ) THEN
            rivers%rfm_iarea_rp(ip) = NINT( rivers%rivers_dra(ix,iy) )
          ELSE
            ! if not present, set area above threshold, so that all points
            ! will be identified as rivers
            rivers%rfm_iarea_rp(ip) = a_thresh + 1
          END IF
          ! Set points to be either land or river.
          IF (rivers%rfm_iarea_rp(ip) > a_thresh) THEN
            rivers%rfm_land_rp(ip) = rfm_river  ! river
          ELSE
            rivers%rfm_land_rp(ip) = rfm_land   ! land
          END IF
        END IF  !  RFM

        ! Define mapping between ip river vector and 2D x,y routing grid
        mapfr(ilon,ilat) = ip

        ! Define overbank inundation ancillary variables, if required
        IF ( l_riv_overbank .AND. l_riv_hypsometry ) THEN
          logn_mean_rp(ip) = logn_mean(ix,iy)
          logn_stdev_rp(ip) = logn_stdev(ix,iy)
        END IF

      END IF  !  rivers_dir_int

    END DO     ! iy river rows
  END DO    ! ix river columns

  ! Double check that we have found all the river points.
  IF ( ip /= np_rivers ) THEN
    CALL log_fatal(RoutineName,                                                &
                   "Failure of logic; did not find all river points.")
  END IF

  !---------------------------------------------------------------------------
  ! Identify the next downstream point from flow direction.
  !---------------------------------------------------------------------------

  DO ip = 1,np_rivers

    SELECT CASE ( rivers%rivers_dir_rp(ip) )

    CASE ( 1:8 )
      ! These are the 8 "physical" flow directions, i.e. there is a flow
      ! direction and a point downstream of this.
      CALL rivers_get_xy_pos(rivers%rivers_index_rp(ip),nx_rivers,ny_rivers,ix,iy)
      inext = ix + flow_dir_delta( rivers%rivers_dir_rp(ip),1 )
      jnext = iy + flow_dir_delta( rivers%rivers_dir_rp(ip),2 )

      ! Deal with cyclic boundaries.
      ! Note that these lines allow the possibility of flow to a point that
      ! is not an immediate neighbour.
      IF ( l_cyclic_x ) THEN
        IF ( inext > nx_rivers ) inext = inext - nx_rivers
        IF ( inext < 1 ) inext = inext + nx_rivers
      END IF

      ! Identify the next point downstream.
      IF ( inext >= 1 .AND. inext <= nx_rivers .AND. jnext >=1                 &
           .AND. jnext <= ny_rivers ) THEN
        ! The next point is on the river grid. Check that it is a river point.
        IF ( mapfr(inext, jnext) > 0 ) THEN
          ! The next point is a river point.
          rivers%rivers_next_rp(ip) = mapfr(inext,jnext)
        ELSE
          ! The next point is on the grid but is not a river point.
          IF ( rivers_dir_int(inext,jnext) == flow_dir_sea ) THEN
            ! The next point is sea. In many ancillary files this point would
            ! have been flagged as a river mouth. Convert the current  point
            ! to a river mouth (but there is no need to change
            ! rivers%rivers_dir_rpas it is not used again).
            rivers%rivers_next_rp(ip) = -river_mouth
          ELSE
            ! The next point is land (but it is not in the valid river
            ! domain). Save the direction in which it lies.
            ! Note that because it it on the grid we know its location (i.e.
            ! we have lat-lon values) but this information will not be
            ! available to the science code (which only gets the coordinates
            ! of river points).
            rivers%rivers_next_rp(ip) = -rivers%rivers_dir_rp(ip)
          END IF
        END IF  !  mapfr
      ELSE
        ! The next point is not on the grid, but we know which direction it
        ! lies in. Save that information for later use when getting a length
        ! for the flow. Note that flow across the E or W edge of a cyclic
        ! global grid is dealt with above.
        rivers%rivers_next_rp(ip) = -rivers%rivers_dir_rp(ip)
      END IF  !  inext

    CASE ( river_mouth )

      ! Identify this with a special value < 0.
      rivers%rivers_next_rp(ip) = -river_mouth

    CASE ( inland_drainage )
      ! Identify this with a special value < 0.
      rivers%rivers_next_rp(ip) = -inland_drainage

    CASE DEFAULT
      CALL log_fatal(RoutineName,                                              &
                     "Unexpected flow direction: " //                          &
                     TRIM(to_string(rivers%rivers_dir_rp(ip))) )

    END SELECT  !  rivers%rivers_dir_rp

  END DO  !  ip

  ! At this point all values of rivers%rivers_next_rp are one of the following:
  !   -10 : an inland drainage point (value = -1*inland_drainage)
  !    -9 : a river mouth (value = -1*river_mouth)
  ! -8:-1 : next point is not a river point - it is outside of the valid river
  !         domain, and possibly off the river grid
  !         (value = -1*rivers%rivers_dir_rp)
  !    >0 : next point is on the grid.

  ! Note that the science code could use rivers%rivers_next_rp values in -8:-1 to
  ! sum flow across the edge of the domain, e.g. to check conservation of
  ! mass. That is not currently coded.

  DEALLOCATE(mapfr)

  !---------------------------------------------------------------------------
  ! Check routing and model grid settings are appropriate
  !---------------------------------------------------------------------------

  ! Check that full routing latitude/longitude grid within range
  IF ( ANY(rivers%rivers_lat2d < -90.0) .OR. ANY(rivers%rivers_lat2d > 90.0) ) &
    CALL log_fatal(RoutineName,                                                &
                   "Latitude is out of range - allowed range is " //           &
                   "-90.0 to 90.0, given range is " //                         &
                   TRIM(to_string(MINVAL(rivers%rivers_lat2d))) // " to " //   &
                   TRIM(to_string(MAXVAL(rivers%rivers_lat2d))))

  IF (ANY(rivers%rivers_lon2d < -180.0) .OR. ANY(rivers%rivers_lon2d > 360.0)) &
     CALL log_fatal(RoutineName,                                               &
                   "Longitude is out of range - allowed range is " //          &
                   "-180.0 to 360.0, given range is " //                       &
                   TRIM(to_string(MINVAL(rivers%rivers_lon2d))) // " to " //   &
                   TRIM(to_string(MAXVAL(rivers%rivers_lon2d))))

  ! Checks for grids other than regular latitude/longitude.
  IF ( .NOT. rivers_reglatlon ) THEN

    ! Must be regular for any regridding methods
    IF ( rivers_regrid ) THEN
      WRITE(jules_message,*) "ERROR: init_rivers: routing codes with " //      &
                             "regridding only currently exist for " //         &
                             "regular lat/lon grids."
      CALL jules_print('init_rivers_props',jules_message)
      CALL log_fatal(RoutineName,                                              &
                     "Error in routing - non-regular lat/lon river grid " //   &
                     "defined and regridding requested. This is not a " //     &
                     "valid option currently.")

    END IF

    ! Check if rivers_dx set
    IF ( rivers_dx <= 0 ) THEN
      WRITE(jules_message,*) "ERROR: init_rivers: routing codes with " //      &
                           "non-regular lat/lon grid require setting " //      &
                           "rivers_dx > 0 in the namelist."
      CALL jules_print('init_rivers_props',jules_message)
      CALL log_fatal(RoutineName,                                              &
                   "Error in routing - non-regular lat/lon river grid " //     &
                   "defined but no grid dimension size provided.")
    END IF

  END IF  !  .NOT. rivers_reglatlon

  !---------------------------------------------------------------------------
  ! Check that land grid points fall on the calculated model grid.
  !---------------------------------------------------------------------------

  IF ( rivers_reglatlon ) THEN
    ! n.b. in parallel mode, this check is not comprehensive (there might be
    ! a different gap around the edges of the regions given to each task)
    DO i = 1,SIZE(latitude, DIM = 1)
      DO j = 1,SIZE(longitude, DIM = 2)
        ! Calculate number of gridboxes from edge.
        rix = (longitude(i,j) - reg_lon1) / reg_dlon
        riy = (latitude(i,j) - reg_lat1)  / reg_dlat
        ! Error if the location does not match the expected grid - if the
        ! difference is a non-negligible fraction of a gridbox.
        ! Essentially we are checking that this location appears to be
        ! "sufficiently close" to the model grid (where "sufficiently close"
        ! allows for differences from the finite precision of the calculation)
        IF ( ABS( rix - NINT(rix) ) > location_toler .OR.                      &
             ABS( riy - NINT(riy) ) > location_toler ) THEN
          CALL log_fatal(RoutineName,                                          &
                         "Land grid points do not coincide with " //           &
                         "calculated main model grid. "           //           &
                         "latitude=" // TRIM(to_string(latitude(i,j))) //      &
                         " longitude=" // TRIM(to_string(longitude(i,j))) )
        END IF
      END DO
    END DO
  END IF

  ! Checks for compatible dimension sizes if not regridding

  IF ( .NOT. rivers_regrid ) THEN

    IF ( nx_rivers /= nx_grid .OR. ny_rivers /= ny_grid ) THEN
      CALL jules_print(RoutineName,jules_message)
      CALL log_fatal(RoutineName,                                              &
         "Error in routing - model grid and river grid dimensions " //         &
         " must be equal size if no regridding attempted.")
    END IF
  END IF

  !---------------------------------------------------------------------------
  ! Initialise river gridbox areas (m2) and set grid spacing (m)
  !---------------------------------------------------------------------------

  IF ( rivers_dlat == 0 ) THEN
    rivers_dlat = rivers%rivers_lat2d(2,2) - rivers%rivers_lat2d(1,1)
  END IF
  IF ( rivers_dlon == 0 ) THEN
    rivers_dlon = rivers%rivers_lon2d(2,2) - rivers%rivers_lon2d(1,1)
  END IF

  IF ( rivers_reglatlon ) THEN

    IF ( rivers_dx < 0 ) THEN
      rivers_dx = planet_radius * (ABS(rivers_dlat) * pi_over_180)
    END IF

    dlat = 0.5 * rivers_dlat
    dlon = 0.5 * rivers_dlon

    DO ip = 1,np_rivers
      rivers%rivers_boxareas_rp(ip) = ABS( rivers_earth_area(                  &
                                             rivers%rivers_lat_rp(ip) - dlat,  &
                                             rivers%rivers_lat_rp(ip) + dlat,  &
                                             rivers%rivers_lon_rp(ip) - dlon,  &
                                             rivers%rivers_lon_rp(ip) + dlon ))
    END DO

  ELSE

    rivers%rivers_boxareas_rp(:) = rivers_dx * rivers_dx

  END IF

  !---------------------------------------------------------------------------
  ! Define overbank inundation initial variables for bankfull on river points
  ! if using Rosgen entrenchment option.
  ! Bankfull discharge (qbf) from power-law relationship from
  ! "Flood Modeling, Prediction and Mitigation" by Sen 2018.
  ! Bankfull width and depth (wbf, dbf) from Leopold & Maddock (1953).
  !---------------------------------------------------------------------------
  IF ( l_riv_overbank .AND. use_rosgen ) THEN
    DO ip = 1,np_rivers
      contribarea = (rivers%rivers_boxareas_rp(ip) * m2tokm2) *                &
                    ( 1.0 + MAX(0.0, REAL(rivers%rfm_iarea_rp(ip))) )
      qbf(ip) = coef_b * ( contribarea**exp_c )
      wbf(ip) = riv_a * ( qbf(ip)**riv_b )
      dbf(ip) = riv_c * ( qbf(ip)**riv_f )
    END DO
  END IF

  !---------------------------------------------------------------------------
  ! Calculate land_pts to river_pts remappings ahead of main run
  !---------------------------------------------------------------------------
  IF ( rivers_regrid ) THEN
    CALL rivers_remap_unmatch(rivers%ir_land_grid)
  ELSE IF ( global_land_pts /= np_rivers ) THEN
    CALL rivers_remap_match(rivers%ir_land_grid, rivers%il_river_grid,         &
                            rivers%rivers_lat_rp, rivers%rivers_lon_rp)
  END IF

  !---------------------------------------------------------------------------
  ! Reset saved JULES grid from river to full land model
  !---------------------------------------------------------------------------

  dummy_grid = grid_create(local_grid%is_1d,local_grid%dim_name,               &
                           local_grid%nx,local_grid%x_name,local_grid%nx,      &
                           local_grid%y_name,local_grid%ny)
  use_subgrid = use_sub_local

END IF ! (check global_landpoint > 1)

END SUBROUTINE init_rivers_props
END MODULE init_rivers_props_mod
