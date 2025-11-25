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

SUBROUTINE gcm_anlg(                                                           &
  q,land_pts,n_olevs,dir_patt,f_ocean,kappa_o,                                 &
  lambda_l,lambda_o,mu,dtemp_o,longmin_am,latmin_am,longmax_am,                &
  latmax_am,mm,imgn_drive, ainfo)

USE io_constants, ONLY: imogen_unit, max_file_name_len

USE imogen_map, ONLY: sgindinv

USE imogen_constants, ONLY: drive_month,n_imogen_land

USE imgn_drive_mod, ONLY: imgn_drive_type
USE ancil_info, ONLY: ainfo_type
USE theta_field_sizes, ONLY: t_i_length

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   GCM Analouge Model. Uses pattern scaling to supply meteorological anomalies
!   based on the global temperature anomaly.
!
! Code Owner: Please refer to ModuleLeaders.txt
!             This file belongs in IMOGEN
!
! Written by: C.Huntingford & P.Cox, 1998
! Code Description:
!   Language: Fortran 90.
!
!-----------------------------------------------------------------------------

TYPE(imgn_drive_type), INTENT(IN OUT) :: imgn_drive
TYPE(ainfo_type), INTENT(IN) :: ainfo

INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
                 ! Number of land points.
  n_olevs,                                                                     &
                 ! Number of ocean thermal layers.
  mm             ! Number of months in a year

CHARACTER(LEN=max_file_name_len), INTENT(IN) ::                                &
  dir_patt
                 ! IN Directory containing anomaly patterns.


CHARACTER(LEN=max_file_name_len) ::                                            &
  driver_patt
                 ! File containing anomaly patterns from analogue model.

REAL, INTENT(IN) ::                                                            &
  f_ocean,                                                                     &
                 ! Fractional coverage of the ocean (.).
  kappa_o,                                                                     &
                 ! Ocean eddy diffusivity (W/m/K).
  lambda_l,                                                                    &
                 ! Inverse climate sensitivity over land (W/m2/K).
  lambda_o,                                                                    &
                 ! Inverse climate sensitivity over ocean (W/m2/K).
  mu,                                                                          &
                 ! Ratio of land to ocean temperature anomalies (.).
  q
                 ! Increase in radiative forcing (W/m2).

REAL, INTENT(IN OUT) ::                                                        &
  dtemp_o(n_olevs)
                 ! Ocean mean temperature anomaly (K).

REAL ::                                                                        &
  lat_am(land_pts),                                                            &
                  ! Latitude read from file.
  long_am(land_pts),                                                           &
                  !Longitude read from file.
  dtemp_l
                 ! Land mean temperature anomaly (K).

REAL, INTENT(OUT) ::                                                           &
  latmin_am,latmax_am,                                                         &
                 ! WORK Latitudinal limits of the area
                 !      (degrees).
  longmin_am,longmax_am
                 ! WORK Longitudinal limits of the area
                 !      (degrees).

!-----------------------------------------------------------------
! Anomaly patterns scaled to land mean temperature anomalies.
!-----------------------------------------------------------------
REAL ::                                                                        &
  drainfall_pat,                                                               &
                 ! WORK Rainfall rate (mm/day/K).
  dsnowfall_pat
                 ! WORK Snowfall rate (mm/day/K).

INTEGER ::                                                                     &
  l, im, i, j, id, ijk
                 ! Loop counters.

!-----------------------------------------------------------------
! Calculate new area mean temperature anomalies
!-----------------------------------------------------------------
CALL delta_temp(n_olevs,f_ocean,kappa_o,lambda_l,lambda_o,mu,q,                &
      dtemp_l,dtemp_o)

!-----------------------------------------------------------------
! Read in spatial patterns (/K) and convert to anomalies
! use to define the new climate data.
!-----------------------------------------------------------------
DO im = 1,mm
  !-----------------------------------------------------------------
  ! Define the anomaly patterns and read the header
  !-----------------------------------------------------------------
  driver_patt = TRIM(dir_patt) // drive_month(im)

  OPEN(imogen_unit, FILE=driver_patt,                                          &
                    STATUS='old', POSITION='rewind', ACTION='read')

  READ(imogen_unit,*) longmin_am,latmin_am,longmax_am,latmax_am

  !-----------------------------------------------------------------
  ! Read in initial climatology and then define the new climate data.
  !-----------------------------------------------------------------
  DO ijk = 1,n_imogen_land
    IF (sgindinv(ijk) > 0) THEN
      l = sgindinv(ijk)
      j = (ainfo%land_index(l) - 1) / t_i_length + 1
      i = ainfo%land_index(l) - (j-1) * t_i_length

      READ(imogen_unit,*)                                                      &
        long_am(l),lat_am(l),imgn_drive%t1p5m_ij_patt(i,j,im),                 &
        imgn_drive%rh1p5m_ij_patt(i,j,im),                                     &
        imgn_drive%uwind_ij_patt(i,j,im),                                      &
        imgn_drive%vwind_ij_patt(i,j,im),                                      &
        imgn_drive%lwdown_ij_patt(i,j,im),                                     &
        imgn_drive%swdown_ij_patt(i,j,im),                                     &
        imgn_drive%diurnal_t1p5m_ij_patt(i,j,im),                              &
        drainfall_pat,dsnowfall_pat,                                           &
        imgn_drive%pstar_ij_patt(i,j,im)

      imgn_drive%precip_ij_patt(i,j,im) = drainfall_pat + dsnowfall_pat

      imgn_drive%t1p5m_ij_anom(i,j,im) =                                       &
                     imgn_drive%t1p5m_ij_patt(i,j,im) * dtemp_l
      imgn_drive%rh1p5m_ij_anom(i,j,im) =                                      &
                     imgn_drive%rh1p5m_ij_patt(i,j,im) * dtemp_l
      imgn_drive%uwind_ij_anom(i,j,im) =                                       &
                     imgn_drive%uwind_ij_patt(i,j,im) * dtemp_l
      imgn_drive%vwind_ij_anom(i,j,im) =                                       &
                     imgn_drive%vwind_ij_patt(i,j,im) * dtemp_l
      imgn_drive%precip_ij_anom(i,j,im) =                                      &
                     imgn_drive%precip_ij_patt(i,j,im) * dtemp_l
      imgn_drive%diurnal_t1p5m_ij_anom(i,j,im) =                               &
                     imgn_drive%diurnal_t1p5m_ij_patt(i,j,im) * dtemp_l
      imgn_drive%lwdown_ij_anom(i,j,im) =                                      &
                     imgn_drive%lwdown_ij_patt(i,j,im) * dtemp_l
      imgn_drive%swdown_ij_anom(i,j,im) =                                      &
                     imgn_drive%swdown_ij_patt(i,j,im) * dtemp_l
      imgn_drive%pstar_ij_anom(i,j,im) =                                       &
                     imgn_drive%pstar_ij_patt(i,j,im) * dtemp_l

    ELSE
      READ(imogen_unit,*)
    END IF
  END DO     !End of loop over land points

  CLOSE(imogen_unit)
END DO     !End of loop over months

RETURN
END SUBROUTINE gcm_anlg
#endif
