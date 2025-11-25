! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************
MODULE check_compatible_options_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE check_compatible_options(call_type)
!-----------------------------------------------------------------------------
! Description:
!   Checks that the enabled science schemes are compatible. Refer to the JULES
!   user manual for more information.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE jules_deposition_mod,          ONLY: l_deposition
USE jules_irrig_mod,               ONLY: l_irrig_dmd
USE jules_model_environment_mod,   ONLY: lsm_id, cable
USE jules_radiation_mod,           ONLY: l_albedo_obs, l_snow_albedo,          &
                                         l_albedo_obs
USE jules_soil_biogeochem_mod,     ONLY: l_layeredc, z_burn_max
USE jules_soil_mod,                ONLY: l_tile_soil, l_holdwater
USE jules_surface_mod,             ONLY: l_flake_model, l_aggregate
USE jules_surface_types_mod,       ONLY: urban_roof, npft, nnvg, ntype
USE jules_urban_mod,               ONLY: l_moruses_storage
USE jules_vegetation_mod,          ONLY: l_triffid, l_inferno, l_trif_fire
USE land_tile_ids_mod,             ONLY: surface_type_ids, ml_snow_type_ids
USE nvegparm,                      ONLY: vf_nvg, albsnc_nvg, albsnf_nvgl,      &
                                         albsnf_nvgu

USE missing_data_mod,              ONLY: imdi, rmdi

USE ereport_mod,                   ONLY: ereport
USE jules_print_mgr,               ONLY: jules_print, jules_message,           &
                                         jules_format, newline
USE errormessagelength_mod,        ONLY: errormessagelength

USE check_unavailable_options_mod, ONLY: check_unavailable_options
USE jules_water_tracers_mod,       ONLY: l_wtrac_jls
USE wtrac_check_options_mod,       ONLY: wtrac_check_options

IMPLICIT NONE

CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: call_type

! Work variables
INTEGER :: ERROR  ! Error indicator
INTEGER :: i      ! Loop counter

LOGICAL :: check_tiles_namelists

CHARACTER(LEN=*), PARAMETER :: routinename='CHECK_COMPATIBLE_OPTIONS'

! Not all JULES options are available to the parent model. Before the
! cross-namelist checking is done, first check that no unavailable options are
! being attempted to be used. A CASE statement using lsm_id can be added here
! and the routine split up if it transpires that some are required by child
! models.
CALL check_unavailable_options()

ERROR = 0

! Check for compatibility with soil tiling. At present it is not allowed for
! TRIFFID, INFERNO, and l_albedo_obs

IF ( l_triffid .AND. l_tile_soil ) THEN
  ERROR = 1
  CALL jules_print(routinename,                                                &
                 "TRIFFID not presently compatible with soil tiling")
END IF

IF ( l_inferno .AND. l_tile_soil ) THEN
  ERROR = 1
  CALL jules_print(routinename,                                                &
                 "INFERNO not presently compatible with soil tiling")
END IF

IF ( l_albedo_obs .AND. l_tile_soil ) THEN
  ERROR = 1
  CALL jules_print(routinename,                                                &
                 "l_albedo_obs not presently compatible with soil tiling")
END IF

IF (l_irrig_dmd .AND. l_holdwater) THEN
  ERROR = 1
  CALL jules_print(routinename,                                                &
                 "l_holdwater=T not presently compatible with l_irrig_dmd=T")
END IF

IF (l_irrig_dmd .AND. l_flake_model ) THEN
  ERROR = 1
  CALL jules_print(routinename,                                                &
                 "l_flake_model=T not presently compatible with l_irrig_dmd=T")
END IF

IF ( l_triffid .AND. l_flake_model ) THEN
  ERROR = 1
  CALL jules_print(routinename,                                                &
                 "TRIFFID not presently compatible with FLake model")
END IF


! Namelists based on tiles are not read by the UM RECON so any checks based on
! these namelists cannot be made.
! By default, we will perform these checks.
check_tiles_namelists = .TRUE.

! The UM RECON is an exception.
IF (PRESENT(call_type)) THEN
  IF ( call_type == "RECON" ) THEN
    check_tiles_namelists = .FALSE.
  END IF
END IF

IF ( lsm_id == cable ) check_tiles_namelists = .FALSE.

! These checks are not to be carried out when called from UM RECON or when the
! land surface model is CABLE, which has its own namelists.
IF (check_tiles_namelists) THEN
  IF ( l_moruses_storage ) THEN
    ! The roof surface needs to be either 0 (meaning "uncoupled"
    ! in this case)
    ! or 1 (radiatively coupled).
    IF ( vf_nvg(urban_roof - npft) /= 1.0 .AND.                                &
       vf_nvg(urban_roof - npft) /= 0.0 )                                      &
       THEN
      ERROR = 1
      WRITE(jules_message,'(A,F3.1)')                                          &
         "MORUSES roof coupling needs vf_nvg = 0 or 1: vf_nvg = ",             &
         vf_nvg(urban_roof - npft)
      CALL jules_print(routinename, jules_message)
    END IF
  END IF
  IF ( l_snow_albedo ) THEN
    IF ( ANY( albsnc_nvg(1:nnvg) > 0.0 ) ) THEN
      ERROR = 1
      WRITE(jules_format,'(a,i0,a)') '(a,',nnvg,'(1x,F4.2))'
      WRITE(jules_message,jules_format)                                        &
         "When l_snow_albedo, albsnc_nvg should be unset. This may "      //   &
         "indicate that something is wrong." // newline //                     &
         "       albsnc_nvg = ", albsnc_nvg(1:nnvg)
      CALL jules_print(routinename, jules_message)
    END IF
  ELSE
    IF ( ANY( albsnc_nvg(1:nnvg) < 0.0 ) ) THEN
      ERROR = 1
      WRITE(jules_message,jules_format)                                        &
         "When l_snow_albedo = F, albsnc_nvg should not be unset."
      CALL jules_print(routinename, jules_message)
    END IF
  END IF
  IF ( l_albedo_obs ) THEN
    IF ( ANY( albsnf_nvgl(1:nnvg) < 0.0 ) ) THEN
      ERROR = 1
      WRITE(jules_message,'(a)')                                               &
         "When l_albedo_obs, albsnf_nvgl should not be unset."
      CALL jules_print(routinename, jules_message)
    END IF
    IF ( ANY( albsnf_nvgu(1:nnvg) < 0.0 ) ) THEN
      ERROR = 1
      WRITE(jules_message,'(a)')                                               &
         "When l_albedo_obs, albsnf_nvgu should not be unset."
      CALL jules_print(routinename, jules_message)
    END IF
  END IF
END IF

! Aggregate tile: The only surface configuration allowed is the original
! surface configuration of [1,2,3,4,5,6,7,8,9].
IF ( l_aggregate ) THEN
  IF ( ntype == 9 ) THEN
    IF ( ALL ( surface_type_ids(1:ntype) - [ (i, i = 1, ntype) ] == 0 ) )      &
       THEN
      ! This is the only scenario allowed. The surface_type_ids and
      ! ml_snow_type_ids are not used when l_aggregate so reset to prevent
      ! errors going undetected.
      surface_type_ids(:) = imdi
      ml_snow_type_ids(:) = imdi
      WRITE(jules_message,'(a)')                                               &
         'Aggregate tile: Surface type and ML snow IDs unset'
      CALL jules_print(routinename, jules_message)
    ELSE
      ERROR = 2
    END IF
  ELSE
    ERROR = 2
  END IF
  IF ( ERROR == 2 ) THEN
    WRITE(jules_format,'(a,i0,a)') '(a,',ntype,'(1x,i0))'
    WRITE(jules_message,jules_format)                                          &
       'The original surface configuration of [1,2,3,4,5,6,7,8,9] is '      // &
       'the only allowed configuration with the aggregate tile.' // newline // &
       '       The current configuration is:', surface_type_ids(1:ntype)
    CALL jules_print(routinename, jules_message)
  END IF
END IF

! Deposition requires a tiled model.
IF ( l_aggregate .AND. l_deposition) THEN
  ERROR = 1
  WRITE(jules_message,jules_format) 'Deposition cannot use aggregate tiles.'
END IF

! l_layeredc with l_inferno or l_trif_fire require z_burn_max to be set
IF ( l_layeredc .AND. ( l_trif_fire .OR. l_inferno ) ) THEN
  IF ( ABS( z_burn_max - rmdi ) < EPSILON(1.0) ) THEN
    ERROR = 1
    CALL jules_print(routinename,                                              &
                   "z_burn_max must be set when using l_layeredc with " //     &
                   "l_trif_fire / l_inferno")
  END IF
END IF

! Check water tracer options
IF (l_wtrac_jls) THEN
  CALL wtrac_check_options(ERROR)
END IF

IF ( ERROR /= 0 )                                                              &
   CALL ereport(routinename, ERROR,                                            &
                  "Error(s) found in enabled science options and/or " //       &
                  "parameter choice(s) - see job.out for error message(s)")

RETURN
END SUBROUTINE check_compatible_options

END MODULE check_compatible_options_mod
