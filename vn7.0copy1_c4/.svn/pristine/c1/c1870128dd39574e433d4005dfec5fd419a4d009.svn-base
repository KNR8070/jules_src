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

SUBROUTINE clim_calc(                                                          &
  land_pts,wgen,mm,md,tmin_wg,tmax_wg,                                         &
  swdown_wg,rh1p5m_wg,precip_wg,nsdmax,                                        &
  seed_rain,lat,lon, imgn_drive, ainfo)

USE missing_data_mod, ONLY: rmdi
USE model_time_mod, ONLY: timesteps_in_day
USE datetime_mod, ONLY: secs_in_day
USE imgn_drive_mod, ONLY: imgn_drive_type
USE ancil_info, ONLY: ainfo_type
USE theta_field_sizes, ONLY: t_i_length

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Calculates the daily and sub daily meteorology based on the monthly
!      climatology and any anomalies / patterns
!
! Code Owner: Please refer to ModuleLeaders.txt
!             This file belongs in IMOGEN
!
! Code Description:
!   Language: Fortran 90.
!
!-----------------------------------------------------------------------------

INTEGER, INTENT(IN) ::                                                         &
  mm,                                                                          &
           ! Number of months
  md,                                                                          &
           ! Number of days in (GCM) month
  nsdmax,                                                                      &
           ! Maximum number of possible subdaily increments
  land_pts,                                                                    &
           ! Number of land points
  seed_rain(4)
           ! Seeding number for subdaily rainfall.

LOGICAL, INTENT(IN) ::                                                         &
  wgen     ! Is the weather generator switched on.

TYPE(imgn_drive_type), INTENT(IN OUT) :: imgn_drive

TYPE(ainfo_type), INTENT(IN OUT) :: ainfo

REAL, INTENT(IN) ::                                                            &
  lat(land_pts),                                                               &
           ! Latitudinal position of l
  lon(land_pts)
           ! Longitudinal position of

! Output from the weather generator when called
REAL, INTENT(IN OUT) ::                                                        &
  precip_wg(land_pts,mm,md),                                                   &
           ! Daily precipitation (mm/day)
  tmin_wg(land_pts,mm,md),                                                     &
           ! Daily minimum temperature (K)
  tmax_wg(land_pts,mm,md),                                                     &
           ! Daily maximum temperature (K)
  swdown_wg(land_pts,mm,md),                                                   &
           ! Daily shortwave radiation (W/m2)
  rh1p5m_wg(land_pts,mm,md)
           ! Daily relative humidity (%)

! Create "subdaily" values of the arrays below using day_calc
REAL ::                                                                        &
  t1p5m_subdaily(land_pts,nsdmax),                                             &
           ! Calculated temperature (K)
  conv_rain_subdaily(land_pts,nsdmax),                                         &
           ! Calculated convective rainfall (mm/day)
  ls_rain_subdaily(land_pts,nsdmax),                                           &
           ! Calculated large scale rainfall (mm/day)
  ls_snow_subdaily(land_pts,nsdmax),                                           &
           ! Calculated large scale snowfall (mm/day)
  q1p5m_subdaily(land_pts,nsdmax),                                             &
           ! Calculated humidity (kg/kg)
  wind_subdaily(land_pts,nsdmax),                                              &
           ! Calculated wind  (m/s)
  pstar_subdaily(land_pts,nsdmax),                                             &
           ! Calculated pressure (Pa)
  swdown_subdaily(land_pts,nsdmax),                                            &
           ! Calculated shortwave radiation (W/m2)
  lwdown_subdaily(land_pts,nsdmax)
           ! Calculated longwave radiation (W/m2)

! Variables for daily climatology
REAL ::                                                                        &
  t1p5m_daily(land_pts,mm,md),                                                 &
           ! Calculated temperature (K)
  precip_daily(land_pts,mm,md),                                                &
           ! Calculated precipitation (mm/day)
  uwind_daily(land_pts,mm,md),                                                 &
           ! Calculated "u"-wind  (m/s)
  vwind_daily(land_pts,mm,md),                                                 &
           ! Calculated "v"-wind  (m/s)
  wind_daily(land_pts,mm,md),                                                  &
           ! Calculated wind  (m/s)
  diurnal_t1p5m_daily(land_pts,mm,md),                                         &
           ! Calculated diurnal Temperature (K)
  pstar_daily(land_pts,mm,md),                                                 &
           ! Calculated pressure (Pa)
  swdown_daily(land_pts,mm,md),                                                &
           ! Calculated shortwave radiation (W/m2)
  lwdown_daily(land_pts,mm,md),                                                &
           ! Calculated longwave radiation (W/m2)                                                                       &
  rh1p5m_daily(land_pts,mm,md)
           ! Calculated relative humidity (%)

INTEGER ::                                                                     &
  l,i,j,id,im,                                                                 &
           ! Loop parameters
  istep
           ! Looping parameter over sub-daily periods

!---------------------------------------------------------------------
! Variables required to split rainfall up so that it rains roughly
! the correct no. of days/month when weather generator is switched off.
!    INTEGER ::                                                    &
!      NO_RAINDAY,                                                 &
               ! WORK No. of rainy days in the month.
!      INT_RAINDAY
               ! WORK Rain day interval.
!      IC_RAINDAY
               ! WORK No. of rain days counter.
!    REAL ::                                                       &
!      TOT_RAIN ! WORK Total rain counter.
!---------------------------------------------------------------------


! Calculate monthly means and add anomalies.
! Anomalies will be zero, if anom=.false. and anlg=.false.
DO im = 1,mm                ! Loop over months
  DO l = 1,land_pts         ! Loop over land points

    j = (ainfo%land_index(l) - 1) / t_i_length + 1
    i = ainfo%land_index(l) - (j-1) * t_i_length

    DO id = 1,md            ! Loop over the days

      IF (wgen) THEN
        ! Temperature (K)
        t1p5m_daily(l,im,id) = ( 0.5 * (tmin_wg(l,im,id) + tmax_wg(l,im,id)) ) &
                          + imgn_drive%t1p5m_ij_anom(i,j,im)
        diurnal_t1p5m_daily(l,im,id) = ( tmax_wg(l,im,id) - tmin_wg(l,im,id) ) &
                           + imgn_drive%diurnal_t1p5m_ij_anom(i,j,im)

        ! Shortwave radiation (W/m2)
        swdown_daily(l,im,id) = swdown_wg(l,im,id) +                           &
                               imgn_drive%swdown_ij_anom(i,j,im)

        ! Relative humidity (kg/kg)
        rh1p5m_daily(l,im,id) = rh1p5m_wg(l,im,id) +                           &
                               imgn_drive%rh1p5m_ij_anom(i,j,im)

        ! Precip
        precip_daily(l,im,id) = precip_wg(l,im,id) +                           &
                               imgn_drive%precip_ij_anom(i,j,im)
      ELSE
        t1p5m_daily(l,im,id) = imgn_drive%t1p5m_ij_clim(i,j,im) +              &
                               imgn_drive%t1p5m_ij_anom(i,j,im)
        swdown_daily(l,im,id) = imgn_drive%swdown_ij_clim(i,j,im) +            &
                               imgn_drive%swdown_ij_anom(i,j,im)
        rh1p5m_daily(l,im,id) = imgn_drive%rh1p5m_ij_clim(i,j,im) +            &
                               imgn_drive%rh1p5m_ij_anom(i,j,im)
        diurnal_t1p5m_daily(l,im,id) =                                         &
                               imgn_drive%diurnal_t1p5m_ij_clim(i,j,im) +      &
                               imgn_drive%diurnal_t1p5m_ij_anom(i,j,im)
        precip_daily(l,im,id) = imgn_drive%precip_ij_clim(i,j,im) +            &
                               imgn_drive%precip_ij_anom(i,j,im)

        !CH-EDITTED OUT LINES BELOW BUT WILL BE RE_IMPLEMENTED WITH DOCUMENTATIO
        ! To correct the no. of rain days per month if WGEN is off:
        !                 IF (K == 1)THEN
        !                    NO_RAINDAY=NINT(F_WET_CLIM(L,J)*MD)
        !                    IF (NO_RAINDAY <  1)NO_RAINDAY=1
        !                    IF (NO_RAINDAY >  MD)NO_RAINDAY=MD
        !                    INT_RAINDAY=INT(FLOAT(MD)/FLOAT(NO_RAINDAY))
        !                    IF (INT_RAINDAY >  MD)INT_RAINDAY=MD
        !                    IC_RAINDAY=0
        !                    TOT_RAIN=0.0
        !                 END IF
        !
        !                 IF (MOD(K,INT_RAINDAY) == 0
        !    &                 .AND.IC_RAINDAY <  NO_RAINDAY)THEN
        !                    PRECIP_DAILY(L,J,K) = (RAINFALL_CLIM(L,J)+
        !    &                 SNOWFALL_CLIM(L,J)+PRECIP_ANOM(L,J))
        !    &                    *FLOAT(MD)/FLOAT(NO_RAINDAY)
        !                    IC_RAINDAY=IC_RAINDAY+1
        !                 ELSE
        !                    PRECIP_DAILY(L,J,K) = 0.0
        !                 END IF
        !                 TOT_RAIN=TOT_RAIN+PRECIP_DAILY(L,J,K)
        !CH-END OF EDITTING OUT
      END IF

      ! Make sure precip anomalies do not produce negative rainfall
      ! Convert input files to kg/m2/s when read in from netcdf in next ticket
      precip_daily(l,im,id) = MAX(precip_daily(l,im,id), 0.0)

      ! Pressure (Pa)
      ! currently converts from hPa to Pa - will read in Pa in the future
      pstar_daily(l,im,id) =                                                   &
               100.0 * ( imgn_drive%pstar_ij_clim(i,j,im) +                    &
               imgn_drive%pstar_ij_anom(i,j,im) )

      ! Check on humidity bounds
      rh1p5m_daily(l,im,id) = MIN(rh1p5m_daily(l,im,id), 100.0)
      rh1p5m_daily(l,im,id) = MAX(rh1p5m_daily(l,im,id), 0.0)

      ! Check to make sure anomalies do not produce negative values
      diurnal_t1p5m_daily(l,im,id) = MAX(diurnal_t1p5m_daily(l,im,id), 0.0)

      ! Longwave radiation (W/m2)
      lwdown_daily(l,im,id) = imgn_drive%lwdown_ij_clim(i,j,im) +              &
                                       imgn_drive%lwdown_ij_anom(i,j,im)

      ! Wind
      uwind_daily(l,im,id) = imgn_drive%uwind_ij_clim(i,j,im) +                &
                                      imgn_drive%uwind_ij_anom(i,j,im)
      vwind_daily(l,im,id) = imgn_drive%vwind_ij_clim(i,j,im) +                &
                                      imgn_drive%vwind_ij_anom(i,j,im)
      wind_daily(l,im,id)  = SQRT(                                             &
                    (uwind_daily(l,im,id)**2) + (vwind_daily(l,im,id)**2) )
      ! Check to make sure anomalies do not produce zero windspeed
      wind_daily(l,im,id) = MAX(wind_daily(l,im,id), 0.01)
    END DO
  END DO
END DO

! Disaggregate daily down to sub-daily

! Variables going in (with units) are SWdown (W/m2), Precip (mm/day), t1p5m
! (K), diurnal_t1p5m (K), LWdown (W/m2), pstar(Pa), Wind (m/2) and rh1p5m (%)

! Variables coming out are subdaily estimates of above variables, except
! for diurnal_t1p5m (which no longer has meaning), and the temperature dependent
! splitting of precipitation back into ls and conv snow and rainfall
! There is assumed no convective snow, and so conv_snow_ij_drive is set to zero.

DO im = 1,mm                ! Loop over the months
  DO id = 1,md              ! Loop over the days

    CALL day_calc(                                                             &
      land_pts,swdown_daily(:,im,id),precip_daily(:,im,id),                    &
      t1p5m_daily(:,im,id),diurnal_t1p5m_daily(:,im,id),lwdown_daily(:,im,id), &
      pstar_daily(:,im,id),wind_daily(:,im,id),rh1p5m_daily(:,im,id),          &
      swdown_subdaily,t1p5m_subdaily,lwdown_subdaily,                          &
      conv_rain_subdaily,ls_rain_subdaily,                                     &
      ls_snow_subdaily,pstar_subdaily,wind_subdaily,                           &
      q1p5m_subdaily,im,id,lat,lon,nsdmax,seed_rain)

    ! Finalise value and set unused output values as rmdi as a precaution.
    DO l = 1,land_pts

      j = (ainfo%land_index(l) - 1) / t_i_length + 1
      i = ainfo%land_index(l) - (j-1) * t_i_length

      DO istep = 1,timesteps_in_day
        imgn_drive%swdown_ij_drive(i,j,im,id,istep) =                          &
                                 swdown_subdaily(l,istep)
        imgn_drive%t1p5m_ij_drive(i,j,im,id,istep) =                           &
                                 t1p5m_subdaily(l,istep)
        imgn_drive%lwdown_ij_drive(i,j,im,id,istep) =                          &
                                 lwdown_subdaily(l,istep)
        imgn_drive%conv_rain_ij_drive(i,j,im,id,istep) =                       &
                                 conv_rain_subdaily(l,istep)/REAL(secs_in_day)
        imgn_drive%conv_snow_ij_drive(i,j,im,id,istep) = 0.0
        imgn_drive%ls_rain_ij_drive(i,j,im,id,istep) =                         &
                                 ls_rain_subdaily(l,istep)/REAL(secs_in_day)
        imgn_drive%ls_snow_ij_drive(i,j,im,id,istep) =                         &
                                 ls_snow_subdaily(l,istep)/REAL(secs_in_day)
        imgn_drive%pstar_ij_drive(i,j,im,id,istep) =                           &
                                 pstar_subdaily(l,istep)
        imgn_drive%wind_ij_drive(i,j,im,id,istep) =                            &
                                 wind_subdaily(l,istep)
        imgn_drive%q1p5m_ij_drive(i,j,im,id,istep) =                           &
                                 q1p5m_subdaily(l,istep)
      END DO

      DO istep = timesteps_in_day+1,nsdmax
        imgn_drive%swdown_ij_drive(i,j,im,id,istep)    = rmdi
        imgn_drive%t1p5m_ij_drive(i,j,im,id,istep)     = rmdi
        imgn_drive%lwdown_ij_drive(i,j,im,id,istep)    = rmdi
        imgn_drive%conv_rain_ij_drive(i,j,im,id,istep) = rmdi
        imgn_drive%conv_snow_ij_drive(i,j,im,id,istep) = rmdi
        imgn_drive%ls_rain_ij_drive(i,j,im,id,istep)   = rmdi
        imgn_drive%ls_snow_ij_drive(i,j,im,id,istep)   = rmdi
        imgn_drive%pstar_ij_drive(i,j,im,id,istep)     = rmdi
        imgn_drive%wind_ij_drive(i,j,im,id,istep)      = rmdi
        imgn_drive%q1p5m_ij_drive(i,j,im,id,istep)     = rmdi
      END DO
    END DO
  END DO                  ! End of loop over days
END DO                    ! End of loop over months

RETURN

END SUBROUTINE clim_calc
#endif
