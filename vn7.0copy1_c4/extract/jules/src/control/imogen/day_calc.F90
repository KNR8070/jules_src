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
SUBROUTINE day_calc(                                                           &
  land_pts,swdown_daily,precip_daily,t1p5m_daily,diurnal_t1p5m_daily,          &
  lwdown_daily, pstar_daily,wind_daily,rh1p5m_daily,swdown_subdaily,           &
  t1p5m_subdaily, lwdown_subdaily,conv_rain_subdaily,ls_rain_subdaily,         &
  ls_snow_subdaily, pstar_subdaily,wind_subdaily,q1p5m_subdaily,               &
  imonth,iday, lat,lon,nsdmax,seed_rain                                        &
)

USE conversions_mod, ONLY: rhour_per_day, rsec_per_hour, pi
USE update_mod, ONLY: dur_ls_rain,dur_conv_rain,dur_ls_snow
USE datetime_mod, ONLY: secs_in_hour, secs_in_day, l_360, l_leap
USE model_time_mod, ONLY: current_time, timesteps_in_day
USE datetime_utils_mod, ONLY: day_of_year, days_in_year

USE qsat_mod, ONLY: qsat
USE sunny_mod, ONLY: sunny
USE redis_mod, ONLY: redis

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   This routine calculates sub-daily variability
!
! Code Owner: Please refer to ModuleLeaders.txt
!             This file belongs in IMOGEN
!
! Written by: C. Huntingford (April 2001) - based on earlier version by P. Cox
! Code Description:
!   Language: Fortran 90.
!
!-----------------------------------------------------------------------------

INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
          ! Maximum number of points in grid.
  nsdmax,                                                                      &
          ! Maximum possible number of sub-daily timesteps.
  imonth,                                                                      &
          ! Month of interest
  iday,                                                                        &
          ! Day number since beginning of month
  seed_rain(4)
          ! Seeding numbers required to disaggregate the rainfall

!-----------------------------------------------------------------------
! Single day arrays of incoming variables
!-----------------------------------------------------------------------
REAL, INTENT(IN) ::                                                            &
  swdown_daily(land_pts),                                                      &
          ! Daily values for downward shortwave radiation (W/m2)
  precip_daily(land_pts),                                                      &
          ! Daily values of rain+snow (mm/day)
  t1p5m_daily(land_pts),                                                       &
          ! Daily values of temperature (K)
  diurnal_t1p5m_daily(land_pts),                                               &
          ! Daily values of diurnal temperature range (K)
  lwdown_daily(land_pts),                                                      &
          ! Daily values of surface longwave radiation (W/m2).
  pstar_daily(land_pts),                                                       &
          ! Daily values of pressure at level 1 (Pa).
  wind_daily(land_pts),                                                        &
          ! Daily values of wind speed (m/s)
  rh1p5m_daily(land_pts)
          ! Relative humidity (%)

!-----------------------------------------------------------------------
! Single disaggregated day arrays of variables above, but for up to hourly
! periods. NOTE: diurnal_t1p5m_subdaily does not exist as t1p5m_subdaily
! combines t1p5m_daily and diurnal_t1p5m_daily
!-----------------------------------------------------------------------

REAL, INTENT(OUT) ::                                                           &
  swdown_subdaily(land_pts,nsdmax),                                            &
          ! Sub-daily values for downward shortwave radiation (W/m2)
  t1p5m_subdaily(land_pts,nsdmax),                                             &
          ! Sub-daily values of temperature (K)
  lwdown_subdaily(land_pts,nsdmax),                                            &
          ! Sub-daily values of surface longwave radiation (W/m2).
  pstar_subdaily(land_pts,nsdmax),                                             &
          ! Sub-daily values of pressure at level 1 (Pa).
  wind_subdaily(land_pts,nsdmax),                                              &
          ! Sub-daily values of wind speed (m/s)
  q1p5m_subdaily(land_pts,nsdmax),                                             &
          ! Sub_daily humidity calculated from rh1p5m (kg/kg)
  conv_rain_subdaily(land_pts,nsdmax),                                         &
          ! Sub-daily convective rain (mm/day or kg/m2/day)
  ls_rain_subdaily(land_pts,nsdmax),                                           &
          ! Sub-daily large scale rain (mm/day or kg/m2/day)
  ls_snow_subdaily(land_pts,nsdmax)
          ! Sub-daily large scale snow (mm/day or kg/m2/day)

REAL, INTENT(IN) ::                                                            &
  lat(land_pts),                                                               &
          ! Latitude (degrees)
  lon(land_pts)
          ! Longitude (degrees)

INTEGER ::                                                                     &
  n_tally,                                                                     &
          ! Counting up number of precipitation periods.
  n_event(land_pts,nsdmax),                                                    &
          ! 1: if rains/snows during timestep period
          ! 0: otherwise
  n_event_local(nsdmax)
          ! As n_event, but for each gridpoint

REAL ::                                                                        &
  prec_loc(nsdmax),                                                            &
          ! Temporary value of rainfall for each gridbox.
  qs_subdaily(land_pts)
          ! Saturated humidity deficit from t1p5m_subdaily and pstar_subdaily

INTEGER ::                                                                     &
  istep,                                                                       &
          ! Loop over sub-daily periods
  daynumber,                                                                   &
          ! Daynumber since beginning of year
  l,i,j

REAL ::                                                                        &
  sun(land_pts,nsdmax),                                                        &
          ! Normalised solar radiation
  time_max(land_pts),                                                          &
          ! Time (UTC) at which temperature is maximum (hours)
  time_day,                                                                    &
          ! Time of day (hours)
  timestep,                                                                    &
          ! Timestep (seconds)
  random_num_sd
          ! Random number associated with rain

REAL ::                                                                        &
  temp_conv,                                                                   &
          ! Temperature above which rainfall is convective (K)
  temp_snow
          ! Temperature below which snow occurs (K)
PARAMETER(temp_conv = 293.15) !ejb these need sorting in next ticket
PARAMETER(temp_snow = 275.15) !ejb these need sorting in next ticket

REAL ::                                                                        &
  init_hour_conv_rain,                                                         &
          ! Start of convective rain event (hour)
  init_hour_ls_rain,                                                           &
          ! Start of large scale rain event (hour)
  init_hour_ls_snow,                                                           &
          ! Start of large scale snow event (hour)
  dur_conv_rain_in_hours,                                                      &
          ! Start of convective rain event (hour)
  dur_ls_rain_in_hours,                                                        &
          ! Duration of large scale rain event (hour)
  dur_ls_snow_in_hours,                                                        &
          ! Start of large scale snow event (hour)
  end_hour_conv_rain,                                                          &
          ! End of convective rain event (hour)
  end_hour_ls_rain,                                                            &
          ! End of large scale rain event (hour)
  end_hour_ls_snow,                                                            &
          ! End of large scale snow event (hour)
  hourevent,                                                                   &
          ! Local variable giving hours during diurnal
          ! period for checking if precip. event occurs
  max_precip_rate,                                                             &
          ! Maximum precip. rate allowed within  each sub-daily timestep
          ! (mm/day). Only applies when timesteps_in_day >= 2
  period_len
          !WORK Length of period (hr)


!-----------------------------------------------------------------------
! Calculate the maximum precipitation rate. It is noted that 58 mm/day
! over 8 timesteps, and where all fell within a single 3 hour period
! caused numerical issues for MOSES. This corresponded to a rate of
! 464 mm/day during the 3-hour period. Hence, place a limit of 350
! mm/day.
!-----------------------------------------------------------------------
PARAMETER(max_precip_rate = 350.0)

dur_conv_rain_in_hours = dur_conv_rain / secs_in_hour
dur_ls_rain_in_hours   = dur_ls_rain   / secs_in_hour
dur_ls_snow_in_hours   = dur_ls_snow   / secs_in_hour

period_len = rhour_per_day / REAL(timesteps_in_day)

!-----------------------------------------------------------------------
! If sub-daily calculations are required.
!-----------------------------------------------------------------------
IF (timesteps_in_day >= 2) THEN

  !-----------------------------------------------------------------------
  ! Ensure that the durations are at least as long as a time period for
  ! the model to prevent solution "falling through gaps"
  !-----------------------------------------------------------------------
  IF (dur_conv_rain_in_hours <= period_len)                                    &
    dur_conv_rain_in_hours = period_len + 1.0e-6
  IF (dur_ls_rain_in_hours <= period_len)                                      &
    dur_ls_rain_in_hours = period_len + 1.0e-6
  IF (dur_ls_snow_in_hours <= period_len)                                      &
    dur_ls_snow_in_hours = period_len + 1.0e-6

  timestep = REAL(secs_in_day) / REAL(timesteps_in_day)

  !-----------------------------------------------------------------------
  ! Calculate the diurnal cycle in the SW radiation
  !-----------------------------------------------------------------------
  daynumber = day_of_year(current_time%year, imonth, iday, l_360, l_leap)
  daynumber = NINT( REAL(daynumber) * 360.0                                    &
                / REAL(days_in_year(current_time%year, l_360, l_leap)))

  CALL sunny(                                                                  &
    daynumber,timesteps_in_day,land_pts,current_time%year,lat,lon,             &
    sun,time_max)

  !-----------------------------------------------------------------------
  ! Loop over timesteps
  !-----------------------------------------------------------------------
  DO istep = 1,timesteps_in_day

    !-----------------------------------------------------------------------
    ! Calculate timestep values of the driving data.
    !-----------------------------------------------------------------------
    time_day = (REAL(istep) - 0.5) * timestep

    DO l = 1,land_pts
      t1p5m_subdaily(l,istep) = t1p5m_daily(l) + 0.5 * diurnal_t1p5m_daily(l) *&
                    COS(2.0 * pi * (time_day - rsec_per_hour * time_max(l))    &
                      / REAL(secs_in_day))
      lwdown_subdaily(l,istep) = lwdown_daily(l)                               &
                     * (4.0 * t1p5m_subdaily(l,istep) / t1p5m_daily(l) - 3.0)
      swdown_subdaily(l,istep) = swdown_daily(l) * sun(l,istep)
    END DO

    !-----------------------------------------------------------------------
    ! Calculate timestep values of the driving data that is not split up
    ! into diurnal behaviour.
    !-----------------------------------------------------------------------
    DO l = 1,land_pts
      pstar_subdaily(l,istep) = pstar_daily(l)
      wind_subdaily(l,istep)  = wind_daily(l)
    END DO

    !-----------------------------------------------------------------------
    ! Check that humidity value is not greater than qsat (but otherwise, q1p5m
    ! is not split up into diurnal behaviour).
    !-----------------------------------------------------------------------
    CALL qsat(qs_subdaily, t1p5m_subdaily(:,istep),                            &
                                            pstar_subdaily(:,istep), land_pts)

    DO l = 1,land_pts
      q1p5m_subdaily(l,istep) = 0.01 * rh1p5m_daily(l) * qs_subdaily(l)
    END DO

  END DO   ! End of timestep loop within the individual days.

  !-----------------------------------------------------------------------
  ! Calculate daily rainfall disaggregation
  !-----------------------------------------------------------------------
  DO l = 1,land_pts

    !-----------------------------------------------------------------------
    ! Precipitation is split into four components,
    ! these being large scale rain, convective rain, large scale snow,
    ! convective snow. Call random number generator for different durations.
    !-----------------------------------------------------------------------
    CALL rndm(random_num_sd,seed_rain)

    !-----------------------------------------------------------------------
    ! Calculate type of precipitation. The decision is based purely up
    ! mean daily temperature, t1p5m_daily. The cutoffs are:
    !
    ! Convective scale rain (duration conv_rain_dur): t1p5m_daily > 20.0oC
    ! Large scale rain (duration ls_rain_dur) : 20.0oC > t1p5m_daily > 2oC
    ! Large scale snow (duration ls_snow_dur) : t1p5m_daily < 2oC
    ! Convective snow - IGNORED
    !-----------------------------------------------------------------------
    ! Initialise arrays
    DO istep = 1,timesteps_in_day
      conv_rain_subdaily(l,istep) = 0.0
      ls_rain_subdaily(l,istep)   = 0.0
      ls_snow_subdaily(l,istep)   = 0.0
      n_event(l,istep)      = 0
      n_event_local(istep)  = 0
      prec_loc(istep)       = 0.0
    END DO

    ! Calculate rainfall disaggregation.

    ! Start with convective rain
    ! (temperatures based upon mean daily temperature)

    ! First check if warm enough for convective rain
    IF (t1p5m_daily(l) >= temp_conv) THEN

      init_hour_conv_rain = random_num_sd                                      &
                            * (rhour_per_day - dur_conv_rain_in_hours)
      end_hour_conv_rain = init_hour_conv_rain + dur_conv_rain_in_hours

      n_tally = 0

      DO istep = 1,timesteps_in_day
        hourevent = (REAL(istep) - 0.5) * period_len
        IF (hourevent >= init_hour_conv_rain .AND.                             &
           hourevent < end_hour_conv_rain) THEN
          n_event(l,istep) = 1
          n_tally = n_tally + 1
        END IF
      END DO

      DO istep = 1,timesteps_in_day
        IF (n_event(l,istep) == 1) THEN      !Rains on this day
          conv_rain_subdaily(l,istep) =                                        &
                    (REAL(timesteps_in_day) / REAL(n_tally)) * precip_daily(l)
          prec_loc(istep) = conv_rain_subdaily(l,istep)
          n_event_local(istep) = n_event(l,istep)
        END IF
      END DO

      ! Check that no convective rain periods
      ! exceed max_precip_rate, or if so,
      ! then redistribute. The variable that is redistributed is local
      ! variable prec_loc - conv_rain_subdaily is then set to this after the
      ! call to redis.

      CALL redis(                                                              &
        nsdmax,timesteps_in_day,max_precip_rate,prec_loc,                      &
        n_event_local,n_tally                                                  &
      )

      DO istep = 1,timesteps_in_day
        conv_rain_subdaily(l,istep) = prec_loc(istep)
      END DO

      ! Now look at large scale rainfall components
    ELSE IF (t1p5m_daily(l) < temp_conv .AND. t1p5m_daily(l) >= temp_snow) THEN

      init_hour_ls_rain = random_num_sd                                        &
                          * (rhour_per_day - dur_ls_rain_in_hours)
      end_hour_ls_rain = init_hour_ls_rain + dur_ls_rain_in_hours

      n_tally = 0

      DO istep = 1,timesteps_in_day
        hourevent = (REAL(istep) - 0.5) * period_len
        IF (hourevent >= init_hour_ls_rain .AND.                               &
           hourevent < end_hour_ls_rain) THEN
          n_event(l,istep) = 1
          n_tally = n_tally + 1
        END IF
      END DO

      DO istep = 1,timesteps_in_day
        IF (n_event(l,istep) == 1) THEN      !Rains on this day
          ls_rain_subdaily(l,istep) =                                          &
                    (REAL(timesteps_in_day) / REAL(n_tally)) * precip_daily(l)
          prec_loc(istep) = ls_rain_subdaily(l,istep)
          n_event_local(istep) = n_event(l,istep)
        END IF
      END DO

      ! Check that no large scale rain periods
      ! exceed MAX_PRECIP_RATE, or if so,
      ! then redistribute.

      CALL redis(                                                              &
        nsdmax,timesteps_in_day,max_precip_rate,prec_loc,                      &
        n_event_local,n_tally                                                  &
      )

      DO istep = 1,timesteps_in_day
        ls_rain_subdaily(l,istep) = prec_loc(istep)
      END DO

      ! Now look at large scale snow components
    ELSE

      init_hour_ls_snow = random_num_sd                                        &
                          * (rhour_per_day - dur_ls_snow_in_hours)
      end_hour_ls_snow = init_hour_ls_snow + dur_ls_snow_in_hours

      n_tally = 0

      DO istep = 1,timesteps_in_day
        hourevent = (REAL(istep) - 0.5) * period_len
        IF (hourevent >= init_hour_ls_snow .AND.                               &
           hourevent < end_hour_ls_snow) THEN
          n_event(l,istep) = 1
          n_tally = n_tally + 1
        END IF
      END DO

      DO istep = 1,timesteps_in_day
        IF (n_event(l,istep) == 1) THEN       ! Rains on this day
          ls_snow_subdaily(l,istep) =                                          &
                    (REAL(timesteps_in_day) / REAL(n_tally)) * precip_daily(l)
          prec_loc(istep) = ls_snow_subdaily(l,istep)
          n_event_local(istep) = n_event(l,istep)
        END IF
      END DO

      ! Check that no large scale snow periods exceed max_precip_rate,
      ! or if so, then redistribute.
      CALL redis(                                                              &
        nsdmax,timesteps_in_day,max_precip_rate,prec_loc,                      &
        n_event_local,n_tally                                                  &
      )

      DO istep = 1,timesteps_in_day
        ls_snow_subdaily(l,istep) = prec_loc(istep)
      END DO

    END IF

  END DO        ! End of large loop over different land points.
                ! in calculation of different rainfall behaviours

ELSE           ! Now case where no subdaily variation (timesteps_in_day=1)

  CALL qsat(qs_subdaily, t1p5m_subdaily(:,istep),                              &
                                        pstar_subdaily(:,istep), land_pts)

  DO l = 1,land_pts
    swdown_subdaily(l,1) = swdown_daily(l)
    t1p5m_subdaily(l,1) = t1p5m_daily(l)
    lwdown_subdaily(l,1) = lwdown_daily(l)
    pstar_subdaily(l,1) = pstar_daily(l)
    wind_subdaily(l,1) = wind_daily(l)
    q1p5m_subdaily(l,1) = 0.01 * rh1p5m_daily(l) * qs_subdaily(l)

    IF (t1p5m_daily(l) >= temp_conv) THEN
      conv_rain_subdaily(l,1) = precip_daily(l)
    ELSE IF (t1p5m_daily(l) < temp_conv .AND. t1p5m_daily(l) >= temp_snow) THEN
      ls_rain_subdaily(l,1) = precip_daily(l)
    ELSE
      ls_snow_subdaily(l,1) = precip_daily(l)
    END IF

  END DO

END IF ! End of loop to chose whether sub-daily is required

RETURN

END SUBROUTINE day_calc
#endif
