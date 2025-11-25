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

SUBROUTINE drdat(                                                              &
  iyear,land_pts,dir_anom,                                                     &
  longmin_dat,latmin_dat,longmax_dat,latmax_dat,mm,drive_month,                &
  imgn_drive, ainfo)

USE io_constants, ONLY: imogen_unit, max_file_name_len
USE imogen_map, ONLY: sgindinv
USE imogen_constants, ONLY: n_imogen_land
USE imgn_drive_mod, ONLY: imgn_drive_type
USE ancil_info, ONLY: ainfo_type
USE theta_field_sizes, ONLY: t_i_length

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Reads anomalies of driving data for the IMOGEN simulations
!   If anom=.true. and anlg=.false.
!
! Code Owner: Please refer to ModuleLeaders.txt
!             This file belongs in IMOGEN
! Written by: P. Cox (1997) and C. Huntingford (2004)
! Code Description:
!   Language: Fortran 90.
!
!-----------------------------------------------------------------------------

TYPE(imgn_drive_type), INTENT(IN OUT) :: imgn_drive
TYPE(ainfo_type), INTENT(IN) :: ainfo

INTEGER, INTENT(IN) ::                                                         &
  iyear,                                                                       &
              ! Year of interest.
  land_pts,                                                                    &
              ! IN Number of land points.
  mm
              ! IN Number months in year.

REAL, INTENT(OUT) ::                                                           &
  latmin_dat,latmax_dat,                                                       &
             ! Latitudinal limits of the area
  longmin_dat,longmax_dat
             ! Longitudinal limits of the area

REAL ::                                                                        &
  lat(land_pts),                                                               &
             ! Latitude (degrees).
  lon(land_pts)
             ! Longitude (degrees).


CHARACTER(LEN=4) ::                                                            &
  drive_year,                                                                  &
              ! IN Label for year of driving data.
  drive_month(12)
              ! Labels for month of driving data.

CHARACTER(LEN=max_file_name_len), INTENT(IN OUT) ::                            &
  dir_anom
              ! Directory containing anomalies.
CHARACTER(LEN=180) ::                                                          &
  driver_anom
              ! Base filename containing anomalies.

!-----------------------------------------------------------------------
! Climatological forcing variables.
!-----------------------------------------------------------------------
REAL ::                                                                        &
  blank_imogen_anomaly_input,                                                  &
  rainfall_anom_dat(land_pts,mm),                                              &
             ! WORK Rainfall rate (mm/day).
  snowfall_anom_dat(land_pts,mm)
             ! WORK Snowfall rate (mm/day).

!-----------------------------------------------------------------------
! Loop counters.
!-----------------------------------------------------------------------
INTEGER :: l, i, j, im, ijk ! Loop counters

!-----------------------------------------------------------------------
! Convert year to string.
!-----------------------------------------------------------------------
WRITE(drive_year,'(I4)') iyear

DO im = 1,mm
  driver_anom = TRIM(dir_anom) // drive_year // drive_month(im)

  OPEN(imogen_unit, FILE=driver_anom,                                          &
                    STATUS='old', POSITION='rewind', ACTION='read')

  READ(imogen_unit,*) longmin_dat,latmin_dat,longmax_dat,latmax_dat
  DO ijk = 1,n_imogen_land
    ! index of grid locations with index from get_imogen_map (see also gcm_anlg)
    IF (sgindinv(ijk) > 0) THEN
      l = sgindinv(ijk)

      j = (ainfo%land_index(l) - 1) / t_i_length + 1
      i = ainfo%land_index(l) - (j-1) * t_i_length

      READ(imogen_unit,*) lon(l), lat(l),                                      &
                        imgn_drive%t1p5m_ij_anom(i,j,im),                      &
                        imgn_drive%rh1p5m_ij_anom(i,j,im),                     &
                        imgn_drive%uwind_ij_anom(i,j,im),                      &
                        imgn_drive%vwind_ij_anom(i,j,im),                      &
                        imgn_drive%lwdown_ij_anom(i,j,im),                     &
                        imgn_drive%swdown_ij_anom(i,j,im),                     &
                        imgn_drive%diurnal_t1p5m_ij_anom(i,j,im),              &
                        rainfall_anom_dat(l,im),                               &
                        snowfall_anom_dat(l,im),                               &
                        imgn_drive%pstar_ij_anom(i,j,im),                      &
                        blank_imogen_anomaly_input


      imgn_drive%precip_ij_anom(i,j,im) = rainfall_anom_dat(l,im) +            &
                                          snowfall_anom_dat(l,im)

    ELSE
      READ(imogen_unit,*)
    END IF
  END DO
END DO
CLOSE(imogen_unit)

RETURN

END SUBROUTINE drdat
#endif
