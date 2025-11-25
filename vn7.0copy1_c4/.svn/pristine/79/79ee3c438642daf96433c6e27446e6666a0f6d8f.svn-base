! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Module holding parameter arrays for non-vegetation surface types.


! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v8.2 programming standards.


MODULE nvegparm

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
 albsnc_nvg(:)                                                                 &
                  ! Snow-covered albedo.
,albsnf_nvgu(:)                                                                &
                  ! Max Snow-free albedo, when scaled to obs
,albsnf_nvg(:)                                                                 &
                  ! Snow-free albedo.
,albsnf_nvgl(:)                                                                &
                  ! Min Snow-free albedo, when scaled to obs
,catch_nvg(:)                                                                  &
                  ! Canopy capacity for water (kg/m2).
,gs_nvg(:)                                                                     &
                  ! Surface conductance (m/s).
,infil_nvg(:)                                                                  &
                  ! Infiltration enhancement factor.
,z0_nvg(:)                                                                     &
                  ! Roughness length (m).
,ch_nvg(:)                                                                     &
                  ! "Canopy" heat capacity (J/K/m2)
,vf_nvg(:)                                                                     &
                  ! Fractional "canopy" coverage
,emis_nvg(:)

LOGICAL, ALLOCATABLE :: l_z0_nvg(:)
                  ! Flag to initialise roughness length from namelist

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='NVEGPARM'

CONTAINS

SUBROUTINE nvegparm_alloc(nnvg)

USE missing_data_mod, ONLY: rmdi

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: nnvg

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='NVEGPARM_ALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

ALLOCATE( albsnc_nvg(nnvg))
ALLOCATE( albsnf_nvgu(nnvg))
ALLOCATE( albsnf_nvg(nnvg))
ALLOCATE( albsnf_nvgl(nnvg))
ALLOCATE( catch_nvg(nnvg))
ALLOCATE( emis_nvg(nnvg))
ALLOCATE( gs_nvg(nnvg))
ALLOCATE( infil_nvg(nnvg))
ALLOCATE( z0_nvg(nnvg))
ALLOCATE( ch_nvg(nnvg))
ALLOCATE( vf_nvg(nnvg))
ALLOCATE( l_z0_nvg(nnvg))

albsnc_nvg(:)  = rmdi
albsnf_nvgu(:) = rmdi
albsnf_nvg(:)  = rmdi
albsnf_nvgl(:) = rmdi
catch_nvg(:)   = rmdi
emis_nvg(:)    = rmdi
gs_nvg(:)      = rmdi
infil_nvg(:)   = rmdi
z0_nvg(:)      = rmdi
ch_nvg(:)      = rmdi
vf_nvg(:)      = rmdi
! Initialise to .true. i.e. z0_surft will be set to the namelist value z0_nvg
! When .false. z0_surft will be set according to other science options.
l_z0_nvg(:)    = .TRUE.

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE nvegparm_alloc


SUBROUTINE check_jules_nvegparm(nnvg,npft)

USE missing_data_mod, ONLY: rmdi
USE ereport_mod,      ONLY: ereport
USE jules_print_mgr,  ONLY: jules_print, jules_message, newline

! May want to move this to remove dependency
USE c_z0h_z0m, ONLY: z0h_z0m,  z0h_z0m_classic

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: nnvg, npft

! Work variables
INTEGER :: ERROR  ! Error indicator
CHARACTER(LEN=*), PARAMETER :: RoutineName='CHECK_JULES_NVEGPARM'

!-----------------------------------------------------------------------------
! Check that all required variables were present in the namelist.
! The namelist variables were initialised to rmdi.
! Some configurations don't need all parameters (e.g. albsnc_nvg) but for now
! we insist on getting all parameters (and that there are not rmdi!).
!-----------------------------------------------------------------------------
ERROR = 0
IF ( ANY( ABS( albsnf_nvg(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(RoutineName, "No value for albsnf_nvg")
END IF
IF ( ANY( ABS( catch_nvg(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(RoutineName, "No value for catch_nvg")
END IF
IF ( ANY( ABS( gs_nvg(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(RoutineName, "No value for gs_nvg")
END IF
IF ( ANY( ABS( infil_nvg(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(RoutineName, "No value for infil_nvg")
END IF
IF ( ANY( ABS( z0_nvg(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(RoutineName, "No value for z0_nvg")
END IF
IF ( ANY( ABS( ch_nvg(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(RoutineName, "No value for ch_nvg")
END IF
IF ( ANY( ABS( vf_nvg(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(RoutineName, "No value for vf_nvg")
END IF
IF ( ANY( ABS( emis_nvg(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(RoutineName, "No value for emis_nvg")
END IF
IF ( ANY( ABS( z0h_z0m(npft+1:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(RoutineName, "No value for z0hm_nvg")
END IF
IF ( ANY( ABS( z0h_z0m_classic(npft+1:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(RoutineName, "No value for z0hm_classic_nvg")
END IF

IF ( ERROR /= 0 )                                                              &
  CALL ereport(RoutineName, ERROR,                                             &
                 "Variable(s) missing from namelist - see job.out for " //     &
                 "error message(s)")

ERROR = 0
IF ( ANY( vf_nvg(:) > 1.0 ) ) THEN
  ERROR = 1
  CALL jules_print(RoutineName, "At least one value of vf_nvg > 1.0")
END IF
IF ( ANY( vf_nvg(:) < 0.0 ) ) THEN
  ERROR = 1
  CALL jules_print(RoutineName, "At least one value of vf_nvg < 0.0")
END IF

IF ( ERROR /= 0 )                                                              &
   CALL ereport(RoutineName, ERROR,                                            &
                  "Variable(s) outside of allowed range - see job.out for " // &
                  "error message(s)")

END SUBROUTINE check_jules_nvegparm

END MODULE nvegparm
