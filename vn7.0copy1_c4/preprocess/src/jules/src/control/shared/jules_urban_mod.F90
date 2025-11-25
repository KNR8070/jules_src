! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE jules_urban_mod

! -----------------------------------------------------------------------------
! Description:
!   Contains switches and other inputs for the urban scheme MORUSES.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
! -----------------------------------------------------------------------------

USE max_dimensions,    ONLY: nsurft_max
USE um_types,          ONLY: real_jlslsm

IMPLICIT NONE

REAL(KIND=real_jlslsm) ::                                                      &
   anthrop_heat_scale = 1.0 ! Scales anthropogenic heat source of roof
                            ! canyon from being equally distributed (= 1.0)
                            ! to all being released in the canyon (= 0.0).
                            ! Takes a value between 0.0 - 1.0

LOGICAL ::                                                                     &
     l_urban_empirical      = .FALSE.,  & ! Empirical relationships for urban
                                          ! geometry (WRR, HWR & HGT)
                                          ! (Standalone only)
     l_moruses_macdonald    = .FALSE.,  & ! MacDonald formulation for
                                          ! displacement height and effective
                                          ! roughness length for momentum
     l_moruses              = .FALSE.,  & ! MORUSES umbrella switch

! Independent parametristaion switches
     l_moruses_albedo       = .FALSE.,  & ! SW canyon albedo
     l_moruses_emissivity   = .FALSE.,  & ! LW canyon emissivity
     l_moruses_rough        = .FALSE.,  & ! Heat transfer
     l_moruses_storage      = .FALSE.,  & ! Storage
     l_moruses_storage_thin = .FALSE.,  & ! Storage thin roof

     l_moruses_rough_surft(nsurft_max)  = .FALSE.,                             &
     l_moruses_albedo_surft(nsurft_max) = .FALSE.

!-----------------------------------------------------------------------
! Set up a namelist to allow switches to be set. l_moruses is set by
! inspecting all other moruses parametrisations.
!-----------------------------------------------------------------------
NAMELIST  /jules_urban/                                                        &
   anthrop_heat_scale, l_urban_empirical,                                      &
   l_moruses_albedo,l_moruses_emissivity,l_moruses_rough,                      &
   l_moruses_storage,l_moruses_storage_thin,l_moruses_macdonald

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='JULES_URBAN_MOD'

CONTAINS

SUBROUTINE check_jules_urban()

!-----------------------------------------------------------------------------
! Description:
!   Checks JULES_URBAN namelist for consistency
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!-----------------------------------------------------------------------------

USE ereport_mod, ONLY: ereport
USE jules_print_mgr, ONLY: jules_print
USE jules_surface_mod, ONLY: l_urban2t, l_aggregate
USE jules_radiation_mod, ONLY: l_cosz
USE jules_surface_types_mod, ONLY: urban_canyon, urban_roof

IMPLICIT NONE

INTEGER :: errcode   ! error code to pass to ereport.

CHARACTER(LEN=*), PARAMETER :: RoutineName='CHECK_JULES_URBAN'

IF ( .NOT. l_urban2t ) THEN
  ! If the two-tile urban schemes are not used these switches should be false.
  IF ( ANY ( [ l_urban_empirical,                                              &
                l_moruses_albedo,                                              &
                l_moruses_emissivity,                                          &
                l_moruses_rough,                                               &
                l_moruses_storage,                                             &
                l_moruses_storage_thin,                                        &
                l_moruses_macdonald] ) ) THEN
    errcode = 10
    CALL ereport( RoutineName, errcode,                                        &
                 "l_urban2t=F. All of the urban switches should be .false. .")
  END IF
END IF

! If any of the MORUSES parametrisations are used turn on the umbrella switch,
! l_moruses, which copies the roughness length for momentum (ztm) to z0 in
! sparm.
IF ( ANY ( [ l_moruses_albedo,                                                 &
              l_moruses_emissivity,                                            &
              l_moruses_rough,                                                 &
              l_moruses_storage,                                               &
              l_moruses_macdonald] ) ) THEN
  l_moruses = .TRUE.
  CALL jules_print(RoutineName,                                                &
                   "MORUSES: At least one parametrisation is being used.")
END IF

! Check MORUSES switch logic
IF ( l_urban_empirical .AND. .NOT. l_moruses_macdonald ) THEN
  errcode = 20
  CALL ereport(RoutineName, errcode,                                           &
               "l_urban_empirical=T: Empirical relationships are " //          &
               "being used to provide the urban morphology data. " //          &
               "MacDonald (1998) formulation for roughness length for " //     &
               "momentum and displacement height (l_moruses_macdonald) " //    &
               "must be also be used for consistency.")
END IF

IF ( l_moruses_albedo .AND. .NOT. l_cosz ) THEN
  errcode = 30
  CALL ereport(RoutineName, errcode,                                           &
               "l_moruses_albedo=T: Must also use l_cosz.")
END IF

IF ( l_moruses_storage_thin .AND. .NOT. l_moruses_storage ) THEN
  errcode = 40
  CALL ereport(RoutineName, errcode,                                           &
               "l_moruses_storage_thin=T: MORUSES storage parametrisation " // &
               "should also be used. Currently l_moruses_storage=F.")
END IF

! Set internal switch to perform MORUSES roughness calculations
IF ( l_moruses_rough ) THEN
  IF ( l_aggregate ) THEN
    errcode = 50
    CALL ereport(RoutineName, errcode,                                         &
       "l_aggregate=T and l_moruses_rough=T: " //                              &
       "l_moruses_rough should be .false. .")
  ELSE
    l_moruses_rough_surft(urban_canyon) = .TRUE.
    l_moruses_rough_surft(urban_roof)   = .TRUE.
  END IF
END IF

! Set internal switch to perform MORUSES albedo calculations
! MORUSES only calculates the canyon albedo; roof albedo is set by namelist
IF ( l_moruses_albedo ) l_moruses_albedo_surft(urban_canyon) = .TRUE.

END SUBROUTINE check_jules_urban

SUBROUTINE print_nlist_jules_urban()
USE jules_print_mgr, ONLY: jules_print
IMPLICIT NONE
CHARACTER(LEN=50000) :: lineBuffer

CALL jules_print('jules_urban', 'Contents of namelist jules_urban')

WRITE(lineBuffer,*)' anthrop_heat_scale = ',anthrop_heat_scale
CALL jules_print('jules_urban',lineBuffer)
WRITE(lineBuffer,*)' l_moruses_albedo = ',l_moruses_albedo
CALL jules_print('jules_urban',lineBuffer)
WRITE(lineBuffer,*)' l_moruses_emissivity = ',l_moruses_emissivity
CALL jules_print('jules_urban',lineBuffer)
WRITE(lineBuffer,*)' l_moruses_rough = ',l_moruses_rough
CALL jules_print('jules_urban',lineBuffer)
WRITE(lineBuffer,*)' l_moruses_storage = ',l_moruses_storage
CALL jules_print('jules_urban',lineBuffer)
WRITE(lineBuffer,*)' l_moruses_storage_thin = ',l_moruses_storage_thin
CALL jules_print('jules_urban',lineBuffer)
WRITE(lineBuffer,*)' l_moruses_macdonald = ',l_moruses_macdonald
CALL jules_print('jules_urban',lineBuffer)

CALL jules_print('jules_urban',                                                &
    '- - - - - - end of namelist - - - - - -')

END SUBROUTINE print_nlist_jules_urban


SUBROUTINE read_nml_jules_urban(nml_dir)

! Description:
!  Read the JULES_URBAN namelist (standalone)

USE io_constants, ONLY: namelist_unit

USE string_utils_mod, ONLY: to_string

USE logging_mod, ONLY: log_info, log_fatal

USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the

INTEGER :: ERROR  ! Error indicator
CHARACTER(LEN=errormessagelength) :: iomessage

! Open the urban namelist file
OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'urban.nml'),                &
               STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR, &
               IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_urban",                                                 &
                 "Error opening namelist file urban.nml " //                   &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

! There are two namelists to read from this file
CALL log_info("init_urban", "Reading JULES_URBAN namelist...")
READ(namelist_unit, NML = jules_urban, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_urban",                                                 &
                 "Error reading namelist JULES_URBAN " //                      &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

! Close the namelist file
CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_urban",                                                 &
                 "Error closing namelist file urban.nml " //                   &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

END SUBROUTINE read_nml_jules_urban

END MODULE jules_urban_mod
