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
  dqradf, land_pts, n_olevs, f_ocean, kappa_o,                                 &
  lambda_l, lambda_o, mu, dtemp_o, dtemp_g, mm, imgn_drive, ainfo)

USE imogen_run, ONLY: l_drive_with_global_temps

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
  dqradf
                 ! Increase in radiative forcing (W/m2).

REAL, INTENT(IN OUT) ::                                                        &
  dtemp_o(n_olevs)
                 ! Ocean mean temperature anomaly (K).

REAL, INTENT(OUT) ::                                                           &
  dtemp_g
                 ! Global mean temperature anomaly (K).

REAL ::                                                                        &
  dtemp_l,                                                                     &
                 ! Land mean temperature anomaly (K).
  dtemp
                 ! Mean temperature change either land or global
                 ! depends on switch (K).
INTEGER ::                                                                     &
  l, im, i, j
                 ! Loop counters.

!-----------------------------------------------------------------
! Calculate area mean temperature changes on both land & globally
!-----------------------------------------------------------------
IF ( .NOT. l_drive_with_global_temps ) THEN
  CALL delta_temp(n_olevs, f_ocean, kappa_o, lambda_l, lambda_o, mu, dqradf,   &
      dtemp_l, dtemp_o, dtemp_g)
  dtemp = dtemp_l
  ! ejb land temperature not global temperature - should be an option
END IF

!-----------------------------------------------------------------
! Loop over months and calculate climate anomalies from patterns (/K)
! and global or land temperature change
!-----------------------------------------------------------------
DO im = 1,mm
  DO l = 1,land_pts

    j = (ainfo%land_index(l) - 1) / t_i_length + 1
    i = ainfo%land_index(l) - (j-1) * t_i_length

    imgn_drive%tl1_ij_anom(i,j,im) =                                           &
                      imgn_drive%tl1_ij_patt(i,j,im) * dtemp
    imgn_drive%ql1_ij_anom(i,j,im) =                                           &
                     imgn_drive%ql1_ij_patt(i,j,im) * dtemp
    imgn_drive%wind_ij_anom(i,j,im) =                                          &
                     imgn_drive%wind_ij_patt(i,j,im) * dtemp
    imgn_drive%precip_ij_anom(i,j,im) =                                        &
                     imgn_drive%precip_ij_patt(i,j,im) * dtemp
    imgn_drive%diurnal_tl1_ij_anom(i,j,im) =                                   &
                     imgn_drive%diurnal_tl1_ij_patt(i,j,im) * dtemp
    imgn_drive%lwdown_ij_anom(i,j,im) =                                        &
                     imgn_drive%lwdown_ij_patt(i,j,im) * dtemp
    imgn_drive%swdown_ij_anom(i,j,im) =                                        &
                     imgn_drive%swdown_ij_patt(i,j,im) * dtemp
    imgn_drive%pstar_ij_anom(i,j,im) =                                         &
                     imgn_drive%pstar_ij_patt(i,j,im) * dtemp
  END DO     !End of loop over land points
END DO     !End of loop over months

RETURN
END SUBROUTINE gcm_anlg
#endif
