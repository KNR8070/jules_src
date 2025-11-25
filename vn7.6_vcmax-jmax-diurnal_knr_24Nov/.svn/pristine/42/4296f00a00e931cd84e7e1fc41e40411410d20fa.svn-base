#if !defined(UM_JULES)
!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************

MODULE init_overbank_props_mod

USE missing_data_mod, ONLY: rmdi

USE model_interface_mod, ONLY: identifier_len

IMPLICIT NONE

!------------------------------------------------------------------------------
! Variables
!------------------------------------------------------------------------------
INTEGER, PARAMETER :: nvars_max = 2
                               ! The maximum possible number of inundation
                               ! variables.

INTEGER :: nvars_required      ! The number of inundation variables that are
                               ! required in this configuration

CHARACTER(LEN=identifier_len) :: required_vars(nvars_max)
                               ! The variable identifiers of the required
                               ! variables

PRIVATE  !  private scope by default
PUBLIC allocate_overbank_vars_rp, init_overbank_props,                         &
       nvars_max, nvars_required, required_vars

CONTAINS

!#############################################################################

SUBROUTINE init_overbank_props()

USE ancil_namelist_mod, ONLY: check_namelist_values, initialise_namelist_vars, &
                              set_constant_vars, set_file_vars

USE io_constants, ONLY: max_sdf_name_len, max_file_name_len, namelist_unit

USE logging_mod, ONLY: log_info, log_warn, log_fatal

USE string_utils_mod, ONLY: to_string

USE jules_rivers_mod, ONLY:                                                    &
  np_rivers, nx_rivers, ny_rivers

USE overbank_inundation_mod, ONLY:                                             &
  overbank_hypsometric, overbank_model

USE parallel_mod, ONLY: is_master_task

USE logging_mod, ONLY: log_info

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!------------------------------------------------------------------------------
! Description:
!   Reads in river overbank inundation parameters and properties, and
!   allocates various overbank variables.
!
!  Code Owner: Please refer to ModuleLeaders.txt
!  This file belongs in section: Hydrology
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!------------------------------------------------------------------------------

CHARACTER(LEN=*), PARAMETER :: RoutineName='init_overbank_props'

INTEGER :: nvars_const
    ! The number of variables to be set using constant values.

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
CHARACTER(LEN=identifier_len) :: file_var(nvars_max)
                      ! The variable identifiers of the variables to set
                      ! from file
CHARACTER(LEN=max_sdf_name_len) :: file_var_name(nvars_max)
                      ! The name of each variable in the file
CHARACTER(LEN=max_sdf_name_len) :: file_tpl_name(nvars_max)
                      ! The name to substitute in a template for each
                      ! variable

CHARACTER(LEN=max_file_name_len) :: FILE
                      ! The name of the file (or variable name template) to
                      ! use for variables that need to be filled from file

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

INTEGER :: i  ! Index variables

INTEGER :: ERROR  !  Error value.

NAMELIST  / jules_overbank_props/                                              &
   FILE, nvars, var, use_file, var_name, tpl_name, const_val

!end of header
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! All tasks need only be done on the master task, which is where the river
! routing code will run.
!------------------------------------------------------------------------------

IF ( is_master_task() ) THEN

  !----------------------------------------------------------------------------
  ! Initialise
  !----------------------------------------------------------------------------
  ! Initialise some common variables.
  CALL initialise_namelist_vars( nvars, nvars_const, nvars_file,               &
                                 nvars_required, l_have_template,              &
                                 FILE, const_val, use_file, use_var,           &
                                 file_name, tpl_name, var, var_name )

  !----------------------------------------------------------------------------
  ! Read overbank inundation namelist
  !----------------------------------------------------------------------------
  CALL log_info(RoutineName,"Reading JULES_OVERBANK_PROPS namelist...")

  READ(namelist_unit, NML = jules_overbank_props, IOSTAT = ERROR)
  IF ( ERROR /= 0 ) THEN
    CALL log_fatal(RoutineName,                                                &
                   "Error reading namelist JULES_OVERBANK_PROPS " //           &
                   "(IOSTAT=" // TRIM(to_string(ERROR)) // ")")
  END IF

  !----------------------------------------------------------------------------
  ! Set up river overbank inundation properties
  !----------------------------------------------------------------------------
  IF ( overbank_model == overbank_hypsometric ) THEN
    nvars_required = 2
    required_vars(1:nvars_required) = [ 'logn_mean ', 'logn_stdev' ]
  END IF

  !----------------------------------------------------------------------------
  ! Check that variables in the namelist have been provided and set up
  ! other variables.
  !----------------------------------------------------------------------------
  CALL check_namelist_values( nvars, nvars_max, nvars_required,                &
                              RoutineName, FILE, const_val,                    &
                              use_file, required_vars, tpl_name, var,          &
                              nvars_const, nvars_file, l_have_template,        &
                              use_var, file_name, var_name )

  !----------------------------------------------------------------------------
  ! Allocate space for gridded ancillary variables.
  !----------------------------------------------------------------------------
  CALL allocate_overbank_vars_grid( nx_rivers, ny_rivers )

  !--------------------------------------------------------------------------
  ! Set variables that are using a constant value.
  !--------------------------------------------------------------------------
  CALL set_constant_vars( nvars, nvars_const, nvars_max, const_val,            &
                          use_file, use_var, var )

  !--------------------------------------------------------------------------
  ! Set variables that are to be read from a file.
  !--------------------------------------------------------------------------
  CALL set_file_vars( nvars, nvars_file, nvars_max, l_have_template,           &
                      RoutineName, use_file, use_var, file_name, var,          &
                      var_name)

END IF  !  is_master_task

RETURN
END SUBROUTINE init_overbank_props

!#############################################################################

SUBROUTINE allocate_overbank_vars_grid( nx_rivers, ny_rivers )

!------------------------------------------------------------------------------
! Description:
!   Allocates overbank inundation variables on the river input grid.
!------------------------------------------------------------------------------

USE logging_mod, ONLY: log_fatal

USE overbank_inundation_mod, ONLY:                                             &
  logn_mean, logn_stdev, overbank_hypsometric, overbank_model

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  nx_rivers, ny_rivers
    ! Size of the river input grid.

CHARACTER(LEN=*), PARAMETER :: RoutineName='allocate_overbank_vars_grid'

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  ERROR, error_sum,                                                            &
    ! Error values.
  nx, ny
    ! Sizes used in allocation.

!end of header
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! This routine is only called by the master task. All variables are allocated,
! but they are only allocated at full size when required by the run
! configuration.
!------------------------------------------------------------------------------
IF ( overbank_model == overbank_hypsometric ) THEN
  nx = nx_rivers
  ny = ny_rivers
ELSE
  nx = 1
  ny = 1
END IF

ALLOCATE( logn_mean(nx, ny), STAT = ERROR )
error_sum = ERROR
ALLOCATE( logn_stdev(nx, ny), STAT = ERROR )
error_sum = error_sum + ERROR
IF ( error_sum /= 0 ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 "Error allocating arrays from ancillaries")
END IF

! Initialise to missing data.
logn_mean(:,:)  = rmdi
logn_stdev(:,:) = rmdi

RETURN
END SUBROUTINE allocate_overbank_vars_grid

!#############################################################################
SUBROUTINE allocate_overbank_vars_rp( land_pts, np_rivers )

!------------------------------------------------------------------------------
! Description:
!   Allocates overbank inundation variables on river points. Also allocates
!   inundation variables on land points.
!------------------------------------------------------------------------------

USE jules_rivers_mod, ONLY: l_riv_overbank

USE logging_mod, ONLY: log_fatal

USE overbank_inundation_mod, ONLY:                                             &
  logn_mean_rp, logn_stdev_rp, overbank_hypsometric, overbank_model,           &
  overbank_simple_rosgen, frac_fplain_lp, frac_fplain_rp, qbf, dbf, wbf

USE parallel_mod, ONLY: is_master_task

IMPLICIT NONE

! Scalar arguments with INTENT(IN)
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! Number of land points.
  np_rivers
    ! Number of river points.

CHARACTER(LEN=*), PARAMETER :: RoutineName='allocate_overbank_vars_rp'

! Local scalar variables.
INTEGER ::                                                                     &
  ERROR, error_sum,                                                            &
    ! Error values.
  np_land_tmp,                                                                 &
    ! Number of land points to allocate for.
  np_rivers_tmp
    ! Number of river points to allocate for.

!end of header
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Allocate inundation variables defined on river routing points.
! All variables are allocated (on all tasks), but they are only allocated at
! full size on the master task and when required by the run configuration.
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Allocate river point variables that are used by all overbank models.
!------------------------------------------------------------------------------
IF ( l_riv_overbank .AND. is_master_task() ) THEN
  ! Full size.
  np_rivers_tmp = np_rivers
ELSE
  ! Minimum size.
  np_rivers_tmp = 1
END IF

ALLOCATE(frac_fplain_rp(np_rivers_tmp), STAT = ERROR)

IF ( ERROR /= 0 ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 "Error allocating river point arrays (1)")
END IF

! Initialise array values
frac_fplain_rp(:) = rmdi

!------------------------------------------------------------------------------
! Allocate river point variables that are only used with the option
! overbank_hypsometric.
!------------------------------------------------------------------------------
IF ( overbank_model == overbank_hypsometric .AND. is_master_task() ) THEN
  ! Full size.
  np_rivers_tmp = np_rivers
ELSE
  ! Minimum size.
  np_rivers_tmp = 1
END IF

ALLOCATE(logn_mean_rp(np_rivers_tmp), STAT = ERROR)
error_sum = ERROR
ALLOCATE(logn_stdev_rp(np_rivers_tmp), STAT = ERROR)
error_sum = error_sum + ERROR

IF ( error_sum /= 0 ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 "Error allocating river point arrays (2)")
END IF

! Initialise array values
logn_mean_rp(:)  = rmdi
logn_stdev_rp(:) = rmdi

!------------------------------------------------------------------------------
! Allocate river point variables that are only used with the option
! overbank_simple_rosgen.
!------------------------------------------------------------------------------
IF ( overbank_model == overbank_simple_rosgen .AND. is_master_task() ) THEN
  ! Full size.
  np_rivers_tmp = np_rivers
ELSE
  ! Minimum size.
  np_rivers_tmp = 1
END IF

! Note that these variables are part of the workspace required by the
! parameterisation.
ALLOCATE(qbf(np_rivers_tmp), STAT = ERROR)
error_sum = ERROR
ALLOCATE(dbf(np_rivers_tmp), STAT = ERROR)
error_sum = error_sum + ERROR
ALLOCATE(wbf(np_rivers_tmp), STAT = ERROR)
error_sum = error_sum + ERROR

IF ( error_sum /= 0 ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 "Error allocating river point arrays (3)")
END IF

! Initialise array values
qbf(:) = rmdi
dbf(:) = rmdi
wbf(:) = rmdi

!-------------------------------------------------------------------------
! Allocate inundation variables defined on land points.
! This is required on all tasks.
!-------------------------------------------------------------------------
IF ( l_riv_overbank ) THEN
  ! Full size.
  np_land_tmp = land_pts
ELSE
  ! Minimum size.
  np_land_tmp = 1
END IF

ALLOCATE(frac_fplain_lp(np_land_tmp), STAT = ERROR)

IF ( ERROR /= 0 ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 "Error allocating land grid arrays")
END IF

! Initialise array values
frac_fplain_lp(:) = 0.0

RETURN
END SUBROUTINE allocate_overbank_vars_rp

!#############################################################################

END MODULE init_overbank_props_mod
#endif
