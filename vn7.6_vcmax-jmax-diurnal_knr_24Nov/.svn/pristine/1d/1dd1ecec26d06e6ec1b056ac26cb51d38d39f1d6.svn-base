#if !defined(RECON)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Purpose:
! Calculates the anthropogenic contribution to surface heat fluxes for
! urban tiles by linear interpolation of monthly values. The value is
! then passed in anthrop_heat(n), that has a value of 0.0 except when
! n=6 (urban) and l_anthrop_heat=.true., and added to the surface
! heat fluxes in sf_expl and sf_impl2.

! Original code from Martin Best and Peter Clark (December 2005).
! Updated for UM7.1 by Jorge Bornemann (May 2008)

MODULE gen_anthrop_heat_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='GEN_ANTHROP_HEAT_MOD'
CONTAINS
SUBROUTINE generate_anthropogenic_heat(val_day_number, land_pts, frac,         &
              surft_pts, surft_index,                                          &
              !New arguments replacing USE statements
              !urban_param (IN)
              wrr_gb,                                                          &
              !Fluxes (IN OUT)
              anthrop_heat_surft)

!Use in scalar variables
USE jules_surface_types_mod, ONLY: urban, ntype, urban_canyon,                 &
                                    urban_roof

USE ancil_info, ONLY: nsurft

USE jules_urban_mod, ONLY: anthrop_heat_scale

USE switches, ONLY: l_360

USE jules_surface_mod, ONLY: l_urban2t, l_anthrop_heat_use_wrr

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

! IN time information for current timestep
INTEGER, INTENT(IN) ::                                                         &
  val_day_number

! IN Number of tiles
INTEGER, INTENT(IN) ::                                                         &
   land_pts           ! No.of land points being processed, can be 0.

REAL(KIND=real_jlslsm), INTENT(IN)    ::                                       &
   frac(land_pts,ntype)            ! IN Fractions of surface types.

! OUT

!New arguments replacing USE statements
!urban_param (IN)
REAL(KIND=real_jlslsm), INTENT(IN) :: wrr_gb(land_pts)

!Fluxes (IN OUT)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: anthrop_heat_surft(land_pts,nsurft)

!Ancil_info
INTEGER, INTENT(IN) ::                                                         &
   surft_index(land_pts,ntype),                                                &
                              ! Index of tile points  : Only used for
   surft_pts(ntype)           ! Number of tile points : urban_canyon

REAL(KIND=real_jlslsm) ::                                                      &
   urban_month(12),                                                            &
                     ! Monthly values of anthropogenic
                     ! contribution to surface heat fluxes W/m2
   furb,                                                                       &
               ! Total urban fraction
   anthrop_heat_urban,                                                         &
               ! Anthropogenic heat on the total urbanised surface
   mm, dpm

INTEGER :: im,im1,n,k,l

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='GENERATE_ANTHROPOGENIC_HEAT'


! urban anthropogenic heat source is taken from the digest of energy
! statistics (1995 - 2003) monthly averaged to 9 years, converted
! to w/m2 and adjusted to fraction dissipated in urban areas

DATA urban_month /                                                             &
  25.1447                                                                      &
, 23.4529                                                                      &
, 24.6175                                                                      &
, 20.7802                                                                      &
, 18.8677                                                                      &
, 18.1567                                                                      &
, 17.1881                                                                      &
, 16.6732                                                                      &
, 18.5490                                                                      &
, 20.7435                                                                      &
, 22.9870                                                                      &
, 26.1500 /

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Make sure that we either have an urban tile
IF ( urban > 0 .OR. urban_canyon > 0 ) THEN
  ! Make sure we have the correct average days per month depending on
  ! whether we are running with a 360 day year
  IF ( l_360 ) THEN
    dpm = 360.0 / 12.0
  ELSE
    dpm = 365.0 / 12.0
  END IF

  mm = val_day_number / dpm - 0.5

  im  = INT(mm)
  mm  = mm - im
  im  = im + 1
  im1 = im + 1

  IF (im == 0) THEN
    im  = 12
  ELSE IF (im1 == 13) THEN
    im1 = 1
  END IF

  anthrop_heat_urban = mm * urban_month(im1) + (1.0 - mm) * urban_month(im)

  ! For the two-tile urban schemes, distribute the anthropogenic heat between
  ! the canyon and roof tiles depending on anthrop_heat_scale otherwise copy it
  ! to the urban tile.
  IF ( l_urban2t ) THEN
    ! anthrop_heat_surft(urban_roof) is a fraction (anthrop_heat_scale)
    ! of anthrop_heat_surft(urban_canyon). The total from the urban tile is
    ! conserved.
    IF ( l_anthrop_heat_use_wrr ) THEN
      ! W/R supplied so use it (bit comparability plus less calculations)
      DO k = 1,surft_pts(urban_canyon)
        l = surft_index(k,urban_canyon)
        anthrop_heat_surft(l,urban_canyon) = anthrop_heat_urban /              &
           ( anthrop_heat_scale * ( 1.0 - wrr_gb(l) ) + wrr_gb (l) )
        anthrop_heat_surft(l,urban_roof) =                                     &
           anthrop_heat_scale * anthrop_heat_surft(l,urban_canyon)
      END DO
    ELSE
      DO k = 1,surft_pts(urban_canyon)
        l = surft_index(k,urban_canyon)
        furb = frac(l,urban_canyon) + frac(l,urban_roof)
        anthrop_heat_surft(l,urban_canyon) = anthrop_heat_urban * furb /       &
           ( frac(l,urban_canyon) + anthrop_heat_scale * frac(l,urban_roof) )
        anthrop_heat_surft(l,urban_roof) =                                     &
           anthrop_heat_scale * anthrop_heat_surft(l,urban_canyon)
      END DO
    END IF
  ELSE IF ( urban > 0 ) THEN
    anthrop_heat_surft(:,urban) = anthrop_heat_urban
  END IF
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE generate_anthropogenic_heat
END MODULE gen_anthrop_heat_mod
#endif
