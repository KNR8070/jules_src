#if !defined(UM_JULES)
!******************************COPYRIGHT**************************************
! (c) UK Centre for Ecology & Hydrology.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************

MODULE water_resources_drive_mod

!-----------------------------------------------------------------------------
! Description:
!   Driver routine for water resource management.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in HYDROLOGY
!
! Code Description:
!   Language: Fortran 90.
!-----------------------------------------------------------------------------

IMPLICIT NONE

PRIVATE  !  private scope by default
PUBLIC water_resources_drive

! Module parameters.
CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
  ModuleName = 'WATER_RESOURCES_DRIVE_MOD'

CONTAINS

!#############################################################################

SUBROUTINE water_resources_drive( global_land_pts, demand_accum, irrig_eff,    &
                                  supply_irrig )

USE jules_water_resources_mod, ONLY: l_water_irrigation, nwater_use,           &
                                     use_irrigation

USE um_types, ONLY: real_jlslsm

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with INTENT(IN)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  global_land_pts
    ! Number of land points in the full model grid.

!-----------------------------------------------------------------------------
! Array arguments with INTENT(IN)
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  demand_accum(global_land_pts,nwater_use),                                    &
    ! Demands for water accumulated over the water resource timestep (kg).
  irrig_eff(global_land_pts)
    ! Irrigation efficiency i.e. the fraction of the water withdrawn for
    ! irrigation that is demanded by the crop scheme.

!-----------------------------------------------------------------------------
! Array arguments with INTENT(OUT)
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  supply_irrig(global_land_pts)
    ! Water supplied for irrigation (kg).

!-----------------------------------------------------------------------------
! Local parameters
!-----------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'WATER_RESOURCES_DRIVE'

!-----------------------------------------------------------------------------
! Local scalar variables
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  l
    ! Loop counter.

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

IF ( l_water_irrigation ) THEN
  !---------------------------------------------------------------------------
  ! To allow testing during development we will assume that all irrigation
  ! requirements can be met. Note that the extra water demanded to account for
  ! the limited efficiency of irrigation will be accounted for in the conveyance
  ! loss - but this is not yet coded.
  !---------------------------------------------------------------------------
  DO l = 1, global_land_pts
    supply_irrig(l) = demand_accum(l,use_irrigation) / ( 2.0 - irrig_eff(l) )
  END DO
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE water_resources_drive

END MODULE water_resources_drive_mod
#endif
