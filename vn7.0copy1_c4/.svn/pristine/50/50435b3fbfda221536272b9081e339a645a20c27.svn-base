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

IMPLICIT NONE

CONTAINS

SUBROUTINE init_overbank_props()

USE ancil_namelist_mod, ONLY: check_namelist_values, initialise_namelist_vars, &
                              set_constant_vars, set_file_vars

USE io_constants, ONLY: max_sdf_name_len, max_file_name_len, namelist_unit

USE missing_data_mod, ONLY: rmdi

USE ancil_info, ONLY: land_pts

USE logging_mod, ONLY: log_info, log_warn, log_fatal

USE string_utils_mod, ONLY: to_string

USE model_interface_mod, ONLY: identifier_len

USE model_grid_mod, ONLY: global_land_pts

USE jules_rivers_mod, ONLY:                                                    &
  np_rivers, nx_rivers, ny_rivers, l_rivers

USE overbank_inundation_mod, ONLY:                                             &
  l_riv_overbank, l_riv_hypsometry,                                            &
  logn_mean, logn_stdev, logn_mean_rp, logn_stdev_rp,                          &
  frac_fplain_lp, frac_fplain_rp,                                              &
  qbf, dbf, wbf

USE logging_mod, ONLY: log_info

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
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
!-----------------------------------------------------------------------------

CHARACTER(LEN=*), PARAMETER :: RoutineName='init_overbank_props'

! Work variables

INTEGER ::                                                                     &
  ERROR        = 0,                                                            &
                     ! Variable for trapping the error from each
                     ! individual call to allocate
  error_sum    = 0
                     ! Variable to track the sum of all errors
                     ! resulting from calls to allocate. Hence we
                     ! know that everything was successful if and
                     ! only if this is zero at the end

INTEGER, PARAMETER :: nvars_max = 2 ! The maximum possible number
                                              ! of inundation variables

INTEGER :: nvars_required      ! The number of inundation variables that are
                               ! required in this configuration
CHARACTER(LEN=identifier_len) :: required_vars(nvars_max)
                               ! The variable identifiers of the required
                               ! variables
CHARACTER(LEN=identifier_len) :: optional_vars(nvars_max)
                               ! The variable identifiers of any optional
                               ! variables

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

NAMELIST  / jules_overbank_props/                                              &
   FILE, nvars, var, use_file, var_name, tpl_name, const_val

!------------------------------------------------------------------------------

! If rivers or overbank inundation are not on, there is nothing to do
IF (l_rivers .AND. l_riv_overbank) THEN

  !---------------------------------------------------------------------------
  ! Initialise
  !---------------------------------------------------------------------------
  ! Initialise some common variables.
  CALL initialise_namelist_vars( nvars, nvars_const, nvars_file,               &
                                 nvars_required, l_have_template,              &
                                 FILE, const_val, use_file, use_var,           &
                                 file_name, tpl_name, var, var_name )

  IF ( global_land_pts <= 1 ) THEN
    l_rivers = .FALSE.
    CALL log_warn(RoutineName,                                                 &
                  "River routing not appropriate for single point runs. " //   &
                  "Overbank inundation disabled.")

  ELSE

    !-------------------------------------------------------------------------
    ! Read overbank inundation namelist
    !--------------------------------------------------------------------------

    CALL log_info(RoutineName,"Reading JULES_OVERBANK_PROPS namelist...")

    READ(namelist_unit, NML = jules_overbank_props, IOSTAT = ERROR)
    IF ( ERROR /= 0 ) CALL log_fatal(RoutineName,                              &
                  "Error reading namelist JULES_OVERBANK_PROPS " //            &
                  "(IOSTAT=" // TRIM(to_string(ERROR)) // ")")

    !--------------------------------------------------------------------------
    ! Set up river overbank inundation properties
    !--------------------------------------------------------------------------

    IF ( l_riv_hypsometry ) THEN
      nvars_required = 2
      required_vars(1:nvars_required) = [ 'logn_mean ', 'logn_stdev' ]

      !------------------------------------------------------------------------
      ! Check that variables in the namelist have been provided and set up
      ! other variables.
      !------------------------------------------------------------------------
      CALL check_namelist_values( nvars, nvars_max, nvars_required,            &
                                  RoutineName, FILE, const_val,                &
                                  use_file, required_vars, tpl_name, var,      &
                                  nvars_const, nvars_file, l_have_template,    &
                                  use_var, file_name, var_name )

      !-----------------------------------------------------------------------
      ! Allocate inundation-specific arrays from ancillary information
      !-----------------------------------------------------------------------

      IF ( nx_rivers > 0 .AND. ny_rivers > 0) THEN

        ALLOCATE( logn_mean(nx_rivers, ny_rivers), STAT = ERROR )
        error_sum = ERROR
        ALLOCATE( logn_stdev(nx_rivers, ny_rivers), STAT = ERROR )
        error_sum = error_sum + ERROR
        IF ( error_sum == 0 ) THEN
          ! Initialise to impossible values.
          logn_mean(:,:)  = rmdi
          logn_stdev(:,:) = rmdi
        ELSE
          CALL log_fatal(RoutineName,                                          &
                         "Error allocating arrays from ancillaries")
        END IF

      ELSE

        CALL log_fatal(RoutineName,                                            &
                  "No overbank inundation with invalid routing dimensions" )
      END IF
    END IF

    !--------------------------------------------------------------------------
    ! Set variables that are using a constant value.
    !--------------------------------------------------------------------------
    CALL set_constant_vars( nvars, nvars_const, nvars_max, const_val,          &
                            use_file, use_var, var )

    !--------------------------------------------------------------------------
    ! Set variables that are to be read from a file.
    !--------------------------------------------------------------------------
    CALL set_file_vars( nvars, nvars_file, nvars_max, l_have_template,         &
                        RoutineName, use_file, use_var, file_name, var,        &
                        var_name)

    !-------------------------------------------------------------------------
    ! Allocate inundation variables defined on river routing points
    !-------------------------------------------------------------------------

    ALLOCATE(logn_mean_rp(np_rivers),       STAT = ERROR)
    error_sum = ERROR
    ALLOCATE(logn_stdev_rp(np_rivers),      STAT = ERROR)
    error_sum = error_sum + ERROR
    ALLOCATE(qbf(np_rivers),                STAT = ERROR)
    error_sum = error_sum + ERROR
    ALLOCATE(dbf(np_rivers),                STAT = ERROR)
    error_sum = error_sum + ERROR
    ALLOCATE(wbf(np_rivers),                STAT = ERROR)
    error_sum = error_sum + ERROR
    ALLOCATE(frac_fplain_rp(np_rivers), STAT = ERROR)
    error_sum = error_sum + ERROR
    IF ( error_sum == 0 ) THEN
      ! Initialise array values
      logn_mean_rp(:)       = rmdi
      logn_stdev_rp(:)      = rmdi
      qbf(:)                = rmdi
      dbf(:)                = rmdi
      wbf(:)                = rmdi
      frac_fplain_rp(:)     = rmdi
    ELSE
      CALL log_fatal(RoutineName,                                              &
                     "Error allocating river grid arrays")
    END IF

    !-------------------------------------------------------------------------
    ! Allocate inundation variables defined on land points
    !-------------------------------------------------------------------------

    ALLOCATE(frac_fplain_lp(land_pts), STAT = ERROR)
    error_sum = ERROR
    IF ( error_sum == 0 ) THEN
      ! Initialise array values
      frac_fplain_lp(:) = 0.0
    ELSE
      CALL log_fatal(RoutineName,                                              &
                     "Error allocating land grid arrays")
    END IF

  END IF ! (check global_landpoint > 1)

END IF

END SUBROUTINE init_overbank_props
END MODULE init_overbank_props_mod
#endif
