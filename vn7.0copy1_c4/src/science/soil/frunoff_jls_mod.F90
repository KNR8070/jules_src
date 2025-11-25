! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!  SUBROUTINE FRUNOFF---------------------------------------------------------

!  PURPOSE : TO CALCULATE SURFACE RUNOFF

!  SUITABLE FOR SINGLE COLUMN MODEL USE

!  DOCUMENTATION : UNIFIED MODEL DOCUMENTATION PAPER NO 25
!                  SECTION (3B(II)), EQN(P252.14)
!-----------------------------------------------------------------------------

MODULE frunoff_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='FRUNOFF_MOD'

CONTAINS

SUBROUTINE frunoff ( npnts, surft_pts, curr_surft, timestep, surft_index,      &
                     area, can_cpy, can_wcnt, infil, r, frac,                  &
                     surf_roff, surf_roff_surft)

USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim

USE jules_surface_mod, ONLY: l_flake_model
USE jules_surface_types_mod, ONLY: lake

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  npnts,                                                                       &
    ! Total number of land points.
  surft_pts,                                                                   &
    ! Number of tile points.
  curr_surft
    ! Current surface tile

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  timestep
    ! Timestep (s).

!-----------------------------------------------------------------------------
! Array arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  surft_index(npnts)
    ! Index of tile points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  area(npnts),                                                                 &
    ! Fractional area of gridbox over which water falls (%).
  can_cpy(npnts),                                                              &
    ! Canopy capacity (kg/m2).
  can_wcnt(npnts),                                                             &
    ! Canopy water content (kg/m2).
  infil(npnts),                                                                &
    ! Infiltration rate (kg/m2/s).
  r(npnts),                                                                    &
    ! Water fall rate (kg/m2/s).
  frac(npnts)
    ! Tile fraction.

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  surf_roff(npnts),                                                            &
    ! Cummulative surface runoff (kg/m2/s).
  surf_roff_surft(npnts)
    ! Surface runoff contributions for this tile (kg/m2/s).

!-----------------------------------------------------------------------------
!Local variables
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  i, j

REAL(KIND=real_jlslsm) ::                                                      &
  aexp,                                                                        &
    ! Used in the calculation of exponential.
  aexp1,                                                                       &
    ! terms in the surface runoff formula.
  aexp2,                                                                       &
  cm,                                                                          &
    ! (CAN_CPY - CAN_WCNT)/TIMESTEP
  can_ratio,                                                                   &
    ! CAN_WCNT / CAN_CPY
  runoff,                                                                      &
    ! Local runoff.
  smallestp
    ! Smallest +ve number that can be represented.

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='FRUNOFF'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)
!-----------------------------------------------------------------------------

smallestp = TINY(1.0)

!$OMP PARALLEL DO DEFAULT(NONE)                                                &
!$OMP PRIVATE(i, runoff, aexp, aexp1, aexp2, can_ratio, cm)                    &
!$OMP SHARED(surft_pts, surft_index, r, infil, timestep, can_wcnt, can_cpy,    &
!$OMP        area, smallestp, surf_roff, frac, surf_roff_surft, l_flake_model, &
!$OMP        curr_surft, lake )                                                &
!$OMP        SCHEDULE(STATIC)
!CDIR NODEP
DO j = 1,surft_pts
  i = surft_index(j)
  runoff = 0.0
  IF ( r(i) > EPSILON(r(i)) ) THEN
    IF ( infil(i) * timestep <= can_wcnt(i)                                    &
         .AND. can_cpy(i) >  0.0 ) THEN
      ! Infiltration in timestep < or = canopy water content
      aexp = area(i) * can_cpy(i) / r(i)
      IF ( can_wcnt(i) > EPSILON(can_wcnt(i)) ) THEN
        aexp1 = EXP( -aexp * infil(i) / can_wcnt(i))
      ELSE
        aexp1 = 0.0
      END IF
      aexp2     = EXP( -aexp / timestep)
      can_ratio = can_wcnt(i) / can_cpy(i)
      can_ratio = MIN(can_ratio,1.0)
      runoff    = r(i) * ( can_ratio * aexp1 + (1.0 - can_ratio) * aexp2 )
      !                                                        ... P252.14A
    ELSE
      ! Infiltration in timestep > canopy water content
      cm = (can_cpy(i) - can_wcnt(i)) / timestep
      cm = MAX(cm,0.0)
      ! Only compute AEXP if will not generate an underflow error
      IF ( area(i) * (infil(i) + cm) / r(i) < - LOG(smallestp) ) THEN
        aexp = EXP( -area(i) * (infil(i) + cm) / r(i))
      ELSE
        aexp = 0.0
      END IF
      runoff = r(i) * aexp                    !     ... P252.14B
    END IF
  END IF

  !Increment the tile runoff for this precip type
  surf_roff_surft(i) = surf_roff_surft(i) + runoff

  IF ( .NOT. ((l_flake_model) .AND. (curr_surft == lake)) ) THEN
    surf_roff(i)       = surf_roff(i)       + frac(i) * runoff
  END IF
END DO
!$OMP END PARALLEL DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE frunoff
END MODULE frunoff_mod
