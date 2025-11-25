! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! Purpose:
! Subroutine to calculate leaf-level values of net photosynthesis,
! leaf conductance and ozone flux.
! *********************************************************************
MODULE leaf_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='LEAF_MOD'

CONTAINS
SUBROUTINE leaf (clos_pts, ft, land_field, open_pts, pft_photo_model, veg_pts  &
,                clos_index ,open_index, veg_index, icr, fpar, pstar           &
,                t1,t_growth_gb,vpd,vpd_growth_gb,l_growth_gb, co2_growth_gb   &
,                pstar_growth_gb,xipam_opt,vcmax_opt,jmax_opt                  &
,                ca,cim,ci, fsmc, o3mol, ra, tl, wcarb, wexpt, wlite, rd          &
,                al, flux_o3, fo3, gl)

USE pftparm, ONLY: beta_c3c4, dfp_dcuo, eagamma, eakc, eako, fd, fl_o3_ct      &
,                 gamma25, glmin, kc25, ko25,c3
USE c_rmol, ONLY: rmol
USE jules_surface_mod, ONLY: beta1, beta2, ratio, ratio_o3
USE conversions_mod, ONLY:zerodegc
USE jules_vegetation_mod, ONLY:                                                &
! imported parameters
    photo_collatz, photo_farquhar, photo_pmodel,photo_acclim_model,            &
    photo_adapt, photo_acclim, photo_adapt_acclim, photo_pmodel_acclim,        &
! imported scalars that are not changed
     l_o3_damage
USE model_time_mod, ONLY: timestep_len, current_time
USE ereport_mod, ONLY: ereport
USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with INTENT(in).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
 clos_pts                                                                      &
                            ! Number of land points with closed stomata.
,ft                                                                            &
                            ! Plant functional type.
,land_field                                                                    &
                            ! Total number of land points.
,open_pts                                                                      &
                            ! Number of land points with open stomata.
,pft_photo_model                                                               &
                            ! Indicates which photosynthesis model to use for
                            ! the current PFT.
,veg_pts
                            ! Number of vegetated points.

!-----------------------------------------------------------------------------
! Array arguments with INTENT(in).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
 clos_index(land_field)                                                        &
                            ! Index of land points with closed stomata.
,open_index(land_field)                                                        &
                            ! Index of land points with open stomata.
,veg_index(land_field)
                            ! Index of vegetated points on the land grid.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 ca(land_field)                                                                &
                            ! Canopy CO2 pressure (Pa).
 ,ci(land_field)                                                               &
                            ! Leaf-internal CO2 partial pressure (Pa).
,fsmc(land_field)                                                              &
                            ! Soil water factor.
,o3mol(land_field)                                                             &
                            ! Molar concentration of ozone
                            ! at reference level (nmol/m3).
,ra(land_field)                                                                &
                            ! Total aerodynamic+boundary layer resistance
                            ! between leaf surface and reference level (s/m).
,tl(land_field)                                                                &
                            ! Leaf temperature (K).
,wexpt(land_field)                                                             &
                            ! Export-limited gross photosynthetic rate
                            ! (mol CO2/m2/s).
,icr(land_field)                                                               &
                            ! Incident PAR (mol photons m-2 s-1).
,fpar(land_field)                                                              &
			    ! PAR absorption factor.
,pstar(land_field)                                                             &
                            ! Surface pressure (Pa).
,t1(land_field) 
                            ! Atmospheric temperature (K)

                                                                 

!-----------------------------------------------------------------------------
! Array arguments with INTENT(inout).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
 rd(land_field)                                                                &
                             ! Dark respiration (mol CO2/m2/s).
,wcarb(land_field)                                                             &
                            ! Carboxylation-limited gross photosynthetic
                            ! rate (mol CO2/m2/s).    
,wlite(land_field)                                                             &
                            ! Light-limited gross photosynthetic rate
                            ! (mol CO2/m2/s).                               
,co2_growth_gb(land_field)                                                     &
                            ! Running mean (growth) CO2 for
                            ! acclimation of photosynthesis  
,l_growth_gb(land_field)                                                       &
                            ! exponantial mean (growth) light for
                            ! acclimation of photosynthesis                                                      
,pstar_growth_gb(land_field)                                                  &
                            ! Running mean (growth) surface pressure for
                            ! acclimation of photosynthesis
,t_growth_gb(land_field)                                                       &
                            ! Running mean (growth) temperature for
                            ! acclimation of photosynthesis (K).
                            ! Atmospheric temperature (K).
,jmax_opt(land_field)                                                          &
			    ! Maximum rate of electron transport at optimal
                            ! condition (mol m-2 s-1)
,vpd_growth_gb(land_field)                                                     &
                            ! optimal vapour pressure          
                            ! deficit for acclimation of photosynthesis
,vcmax_opt(land_field)                                                         &
			    ! Maximum rate of carboxylation at optimal
                            ! condition(mol CO2/m2s)  
,xipam_opt(land_field)                                                         &
                            ! Sensitivity of ci/ca ratio to VPD at 
                            ! optimal condition (Pa^1/2)
,cim(land_field)                                                               &
! Leaf-internal CO2 partial pressure (Pa). pmodel
,vpd(land_field)
                            ! optimal vapour pressure          
                            ! deficit for acclimation of photosynthesis                            


!-----------------------------------------------------------------------------
! Array arguments with INTENT(out).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 al(land_field)                                                                &
                            ! Net Leaf photosynthesis (mol CO2/m2/s).
,flux_o3(land_field)                                                           &
                            ! Flux of O3 to stomata (nmol O3/m2/s).
,fo3(land_field)                                                               &
                            ! Ozone exposure factor.
,gl(land_field)
                            ! Leaf conductance for H2O (m/s).

!-----------------------------------------------------------------------------
! Local variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
 errcode                                                                       &
                            ! Error code to pass to ereport.
,j,m,l                                                                          &                       
                              ! Loop counters.
,current_time_pmodel        ! save currentime for comparison                              

!-----------------------------------------------------------------------------
! Local parameters.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), PARAMETER ::                                           &
  ha = 65330.0,                                                                &
    ! Activation energy
  rgas = 8.314,                                                                &
    ! Universal gas constant 
  haj = 43900.0
    ! Deactivation energy

REAL(KIND=real_jlslsm) ::                                                      &
  b,c,                                                                         &
                            ! Work variables for ozone calculations.
  beta1p2m4, beta2p2m4
                            ! beta[12] ** 2 * 4.

REAL(KIND=real_jlslsm) ::                                                      &
 b1(land_field)                                                                &
,b2(land_field)                                                                &
,b3(land_field)                                                                &
                            ! Coefficients of the quadratic.
,conv(land_field)                                                              &
                            ! Factor for converting mol/m3 into Pa (J/mol).
,glco2(land_field)                                                             &
                            ! Leaf conductance for CO2 (m/s).
,wp(land_field)                                                                &
                            ! Smoothed minimum of carboxylation- and light-
                            ! limited gross photosynthesis (mol CO2/m2/s).
,gammastar25(land_field)                                                  &
			    ! CO2 compensation point at 25 degree C (Pa)
,gammastarm(land_field)                                                   &
			    ! Pressure-dependent photorespiratory          
		            ! compensation point  (Pa)  
,iabs_light(land_field)                                                         &
		            ! Amount of absorbed light (Photon mol m-2 s-1).              
,gammastarm_opt(land_field)                                               &
          ! Pressure-dependent photorespiratory at optimal
,jmax25_opt(land_field)                                                   &
			    ! Maximum rate of electron transport at optimal
                            ! condition normalirsed at 25 degree (mol m-2 s-1) 
,jmax_pmodel(land_field)                                                  &
			    ! Maximum rate of electron transport (mol m-2 s-1)
,jmax_adjusted(land_field)                                                &
			    ! Adjusted maximum rate of electron transport 
                            ! (mol m-2 s-1)
,jp1(land_field)                                                           &
			  ! The electron transport rate mol electrons (m-2 s-1)                           
,kcpa(land_field)                                                         &
                          ! Michaelis-Menten constant for CO2 (Pa) for a point. 
,kopa(land_field)                                                         &
                          ! Michaelis-Menten constant for O2 (Pa) for a point.
,kmpa(land_field)                                                          &
	   	    	  ! A combination of Michaelis-Menten and other terms.
,ta(land_field)                                                            &
		            ! Atmospheric temperature (degree C). 
,O2_partial_pres(land_field)                                               &
                            ! Atmospheric O2 partial pressure (K). 
,phi0(land_field)                                                          &
			    ! Intrinsic quantum yield parameter. 
,pratio(land_field)                                                        &
                            ! Atmospheric pressure/atmospheric pressure at 0m
,tgdegc(land_field)                                                        &
                            ! Temperatures t_growth_gb in degrees Celsius.                            
,vcmax25_opt(land_field)                                                   &
			    ! Maximum rate of carboxylation (mol CO2/m2s) 
                            ! normalise at 25 degrees
,vcmax_pmodel(land_field)                                                  &
			    ! Maximum rate of carboxylation (mol CO2/m2s)
,vcmax_adjusted(land_field)                                                &
			    ! Adjusted maximum rate of carboxylation 
                            ! (mol CO2/m2s)                 
,viscosityh2ostar(land_field)                                              &

		            ! viscosity of water unit less
,wl(land_field)                                                                &
                            ! Gross leaf phtosynthesis (mol CO2/m2/s).
,xipam(land_field)
		            ! Sensitivity of ci/ca ratio to VPD (Pa^1/2)                               

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='LEAF'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(j,l,b,c,beta1p2m4,beta2p2m4)              &
!$OMP IF(open_pts > 1)                                                         &
!$OMP SHARED(open_pts, pft_photo_model, veg_index, open_index, beta1,          &
!$OMP        errcode, wcarb, wlite,                                            &
!$OMP        beta2, wp, wexpt, b1, b2, b3, wl, al, rd, fsmc,                   &
!$OMP        conv, tl, glco2, ca, ci, gl, glmin, l_o3_damage, ft,              &
!$OMP        o3mol, ra, fo3, dfp_dcuo, fl_o3_ct, flux_o3)

SELECT CASE ( pft_photo_model )

CASE ( photo_collatz )
  !---------------------------------------------------------------------------
  ! Use the Collatz model.
  ! Calculate the co-limited rate of gross photosynthesis.
  !---------------------------------------------------------------------------
  beta1p2m4 = 4 * beta1 * beta1
  beta2p2m4 = 4 * beta2 * beta2

!DIR$ IVDEP
!$OMP DO SCHEDULE(STATIC)
  DO j = 1,open_pts
    l = veg_index(open_index(j))

    b1(l) = beta1
    b2(l) = - (wcarb(l) + wlite(l))
    b3(l) = wcarb(l) * wlite(l)

    wp(l) = -b2(l) / (2.0 * b1(l))                                             &
            - SQRT(b2(l) * b2(l) / beta1p2m4      - b3(l) / b1(l))

    b1(l) = beta2
    b2(l) = - (wp(l) + wexpt(l))
    b3(l) = wp(l) * wexpt(l)

    wl(l) = -b2(l) / (2.0 * b1(l))                                             &
            - SQRT(b2(l) * b2(l) / beta2p2m4       - b3(l) / b1(l))
    print*,'wl',wl
  END DO
!$OMP END DO

CASE ( photo_farquhar )
  !---------------------------------------------------------------------------
  ! Use the Farquhar model.
  !---------------------------------------------------------------------------
!DIR$ IVDEP
!$OMP DO SCHEDULE(STATIC)
  DO j = 1,open_pts
    l = veg_index(open_index(j))
    wl(l) = MIN( wcarb(l), wlite(l) )
  END DO

!$OMP END DO
CASE ( photo_pmodel )
 SELECT CASE ( photo_acclim_model )
 CASE ( 0 )
!$OMP END PARALLEL DO
  CASE ( photo_adapt, photo_acclim, photo_adapt_acclim )
      ! DO nothing
CASE( photo_pmodel_acclim )
  DO m = 1,veg_pts
    l = veg_index(m)
    ta(l) = tl(l) - zerodegc
   ! print*,'t',tl(l)
   ! print*,'ta',ta(l)
    !--------------------------------------------------------------------------
    !Calculate adjusted Vcmax, Jmax and xim at current time step
    !Calculate acclimated GPP
    !(Mengoli et al., 2022)
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    ! Compute K at give condition(effective Michaelis-Menten coefficient) current
    !--------------------------------------------------------------------------
    O2_partial_pres(l) = 2.09476E5 * 1.0E-6 * pstar(l)
  !print*,'pstar',pstar(l)
    !--------------------------------------------------------------------------
    ! Effective Michaelis-Menten coefficient Kc and ko in pascal
    !--------------------------------------------------------------------------
    kcpa(l) = kc25(ft) * exp(eakc(ft)*(ta(l) - 25.0)                           &
                                            /(298.15*8.3145* tl(l)))
    kopa(l) = ko25(ft) * exp(eako(ft)*(ta(l) - 25.0)                           &
                                            /(298.15*8.3145* tl(l))) 
    ! Compute Kmpa in Pascal 
    kmpa(l) = kcpa(l) * (1.0 + O2_partial_pres(l)/kopa(l))
   !print*, 'cpa',kmpa(l)
   !---------------------------------------------------------------------------
   !calculate the dark respiration mol co2 m-2 s-1 (Wang et al., 2020)
   !---------------------------------------------------------------------------
    rd(l) = 0.03 * vcmax_opt(l) / EXP((ha/rgas) *                                &
    ((t_growth_gb(l) -273.15 - 25) / (298.15 * t_growth_gb(l))))*           &
    EXP(0.1012*(t_growth_gb(l) - 273.15-25) - 0.0005*(t_growth_gb(l)**2-25**2))*1E-6
  !print*,'rd',rd(l)
    !--------------------------------------------------------------------------
    ! Adjust gammaStar25 for the pressure at current surface pressure
    !--------------------------------------------------------------------------
    pratio(l) = pstar(l)/  101325.0
    !--------------------------------------------------------------------------
    ! Compute CO2 compensation point at current surface pressure
    !--------------------------------------------------------------------------
    ! Adjust gammaStar25 for the pressure at given surface pressure
    gammastar25(l) = gamma25(ft) * pratio(l)
    !--------------------------------------------------------------------------
    ! Pressure-dependent photorespiratory compensation point
    !--------------------------------------------------------------------------
    gammastarm(l) = gammastar25(l) *                                           &
                   EXP(eagamma(ft) *(ta(l) - 25.0)                             &
                                     /(8.3145 * 298.15 * tl(l)))
  !print*, 'cgamma',gammastarm(l)
  !--------------------------------------------------------------------------
  ! Compute water viscosity optimal
  !--------------------------------------------------------------------------
  viscosityh2ostar(l) = (exp(-3.719 + 580 /((ta(l) + 273) - 138.0)))/    &
                               0.9102827622
  !print *, '3', viscosityh2ostar(l)
  cim(l) = (xipam_opt(l)*ca(l) + gammastarm(l)                          &
       *sqrt(vpd(l))) /(xipam_opt(l) + sqrt(vpd(l)))
 ! print*,'ci',cim(l)
  !--------------------------------------------------------------------------
  ! Compute sensitivity of ci/ca ratio to VPD (xi)
  !--------------------------------------------------------------------------
   xipam(l) = sqrt((beta_c3c4(ft) * (kmpa(l)+gammastarm(l)))               &
                    / (1.6*viscosityh2ostar(l)))
   !align with 
   !print*,'vl',vpd(l)
   !--------------------------------------------------------------------------
   ! Calculate absorbed light
   !--------------------------------------------------------------------------   
    iabs_light(l) = icr(l)*fpar(l)
    !print *, 'iabs_light',iabs_light(l)
    !--------------------------------------------------------------------------
    ! calculate vcmax at optimal condition
    ! [molco2 m-2 s-1] optimal kmpa,gammastarm
    !--------------------------------------------------------------------------
   ! IF ((1-(0.41*(cim(l)+2*gammastarm(l))/                           &
   !                 (cim(l)-gammastarm(l)))**(2.0/3))<0) THEN
   !   vcmax_pmodel(l) = phi0(l) * iabs_light(l) * sqrt(1 - 0.41**(2.0/3))
   !  ELSE
   !  vcmax_pmodel(l) = phi0(l) * iabs_light(l) * (cim(l) + kmpa(l))/     &
   !                             (cim(l) + 2.0 * gammastarm(l)) *      &
   !           sqrt(1.0 - (0.41 * (cim(l)+ 2.0 * gammastarm(l))        & 
   !                         / (cim(l) - gammastarm(l))) ** (2.0/3.0))
   ! END IF 
    !vcmax_pmodel(l) = max(0.0,vcmax_pmodel(l))
   !---------------------------------------------------------------------------
   !calculate adjusted vcmax (mol CO2 m2-1 s-1) 
   !(Mengoli et al., 2022)
   !---------------------------------------------------------------------------
   vcmax_adjusted(l) = vcmax_opt(l)*exp((ha/rgas)*(1/298.15-1/tl(l)))
   !vcmax_adjusted(l) = vcmax_pmodel(l) 
!   print *, 'b',vcmax_adjusted(l)
   END DO
   DO j = 1,open_pts
    l = veg_index(open_index(j))
   !---------------------------------------------------------------------------
   !calculate gross primary productivity umol CO2 m-2 s-1 light
   !---------------------------------------------------------------------------   
    !--------------------------------------------------------------------------
    ! compute maximum rate of electron transport (ref: Wang et al., 2017)
    ! [µmol electrons m2-1 s-1]
    !--------------------------------------------------------------------------
    !IF ((1-(0.41*(cim(l)+2*gammastarm(l))/                           &
    !               (cim(l)-gammastarm(l)))**(2.0/3))<0)THEN
    !jmax_pmodel(l)=(4.0*phi0(l)*iabs_light(l))/SQRT(1/(1-0.41**(2.0/3.0))-1.0)
    !print*,'JR'
    !ELSE
    !jmax_pmodel(l) =( 4 * phi0(l) * iabs_light(l) ) / sqrt (              &
    !1/(1-(0.41 * ( cim(l) + 2*gammastarm(l) ) /                      &
    !                ( cim(l) - gammastarm(l) ) ) ** (2.0/3.0) ) - 1 )
    !print*,'jn'
    ! END IF  
    !jmax_pmodel(l)=MAX(0.0, jmax_pmodel(l))
   !---------------------------------------------------------------------------
   !calculate adjusted jmax (umol CO2/m2s)
   !---------------------------------------------------------------------------
   jmax_adjusted(l) = jmax_opt(l) * EXP((haj/rgas)* (1/298.15 - 1/tl(l)))
   !jmax_adjusted(l) = jmax_pmodel(l)
   !PRINT *, 'c', jmax_adjusted(l)

   IF (c3(ft) ==1 ) THEN
   !---------------------------------------------------------------------------
   ! calcaulate intrinsic quantum efficiency of photosynthesis 
   ! phi0 is temperature dependent (phi0, mol Co2/mol photons)
   ! (ref: Bernacchi et al., 2003)
   !---------------------------------------------------------------------------
    phi0(l) = (1.0/8.0) * (0.352 + 0.022 * ta(l) - 0.00034 * ta(l) ** 2)
    !print*,'phi',phi0(l)
   !---------------------------------------------------------------------------
   ! Compute the electron transport rate (Smith et al, 1937)
   ! [µmol CO2 m2-1 s-1]
   !---------------------------------------------------------------------------
   jp1(l) = (4.0* phi0(l) * iabs_light(l))/                                     &
                sqrt(1+((4*phi0(l)*iabs_light(l)/jmax_adjusted(l)) ** 2))
    !print*,'jp1',jp1(l)
   !---------------------------------------------------------------------------
   !aj with the acclimated cim and current gammastarm
   !umol to mol
   !calculate acclimated Rubisco-limited assimilation rate (ac1)
   !this gammastarm and kmpa are current 
   !---------------------------------------------------------------------------
    wlite(l) = ((jp1(l)/4.0) * (cim(l) - gammastarm(l)) /                       &
                                 (cim(l) + 2.0 * gammastarm(l)))*1E-6
    wcarb(l) =(vcmax_adjusted(l) *(cim(l) - gammastarm(l))                      &
                                 /(cim(l)+kmpa(l)) )*1E-6
    !print*,'c3'
    ELSE !c4 phi0
    !---------------------------------------------------------------------------
    ! calcaulate intrinsic quantum efficiency of photosynthesis 
    ! phi0 is temperature dependent (phi0, mol Co2/mol photons)
    ! (ref: Bernacchi et al., 2003)
    !---------------------------------------------------------------------------
    phi0(l) = (-0.008 + 0.00375 * ta(l) - 0.58 * 1e-4 * ta(l) ** 2)
   !---------------------------------------------------------------------------
   ! Compute the electron transport rate (Smith et al, 1937)
   ! [µmol CO2 m2-1 s-1]
   !---------------------------------------------------------------------------
    jp1(l) = (4.0* phi0(l) * iabs_light(l))/                                     &
    sqrt(1+((4*phi0(l)*iabs_light(l)/jmax_adjusted(l)) ** 2))
   !---------------------------------------------------------------------------
   !aj with the acclimated cim and current gammastarm
   !umol to mol
   !calculate acclimated Rubisco-limited assimilation rate (ac1)
   !this gammastarm and kmpa are current 
   !---------------------------------------------------------------------------
    wlite(l) = (jp1(l)/4.0) *1E-6
    wcarb(l) = vcmax_adjusted(l)*1E-6 !c4 carbon limited assmilation rate
   !print*,'c4'
   END IF
  !print*,'d', wlite(l)
  !print*,'e',wcarb(l)
   !compute gpp
   wl(l) = MIN(wcarb(l), wlite(l))
 !print*,'gpp_leaf',wl(l)
  END DO
  END SELECT !  photo_acclim_model 

CASE DEFAULT
  errcode = 101  !  a hard error
  CALL ereport(RoutineName, errcode,                                           &
               'pft_photo_model should be photo_collatz or photo_farquhar or P')

END SELECT  !  pft_photo_model


!-----------------------------------------------------------------------------
! Carry out calculations for points with open stomata
!-----------------------------------------------------------------------------
!DIR$ IVDEP
!$OMP DO  SCHEDULE(STATIC)
DO j = 1,open_pts
  l = veg_index(open_index(j))
  !---------------------------------------------------------------------------
  ! Calculate the net rate of photosynthesis
  !---------------------------------------------------------------------------
  al(l) = (wl(l) - rd(l)) * fsmc(l)
 ! print*,'npp_l',al(l)
  !---------------------------------------------------------------------------
  ! Calculate the factor for converting mol/m3 into Pa (J/m3).
  !---------------------------------------------------------------------------
  conv(l) = rmol * tl(l)
  !---------------------------------------------------------------------------
  ! Diagnose the leaf conductance
  !---------------------------------------------------------------------------
  IF (ABS(ca(l) - cim(l)) < 1.0e-6) THEN
  gl(l) = glmin(ft)
  ELSE
  glco2(l) = (al(l) * conv(l)) / (ca(l) - cim(l))
  gl(l)    = ratio * glco2(l)
  END IF
  !print*,'gl',gl(l)
END DO
!$OMP END DO

!-----------------------------------------------------------------------------
! Close stomata at points with negative or zero net photosynthesis
! or where the leaf resistance exceeds its maximum value.
!-----------------------------------------------------------------------------
!DIR$ IVDEP
!$OMP DO SCHEDULE(STATIC)
DO j = 1,open_pts
  l = veg_index(open_index(j))

  IF (gl(l) <= glmin(ft) .OR. al(l) <= 0.0) THEN
    gl(l) = glmin(ft)
    al(l)    = -rd(l) * fsmc(l)
  END IF

END DO
!$OMP END DO

IF ( l_o3_damage ) THEN
  !---------------------------------------------------------------------------
  ! Modify the stomatal conductance and photosynthesis for ozone effects
  ! (Peter Cox, 12/11/04)
  !---------------------------------------------------------------------------
!$OMP DO SCHEDULE(STATIC)
  DO j = 1,open_pts
    l = veg_index(open_index(j))

    !-------------------------------------------------------------------------
    ! Flux of O3 without ozone effects (for use in analytical eqn)
    !-------------------------------------------------------------------------
    flux_o3(l) = o3mol(l) / (ra(l) + (ratio_o3 / gl(l)))

    !-------------------------------------------------------------------------
    ! Analytic solution for the ozone exposure factor
    !-------------------------------------------------------------------------
    ! Use EPSILON to avoid overflow on division
    IF (ABS(ra(l)) < EPSILON(1.0)) THEN
      fo3(l) = (1.0 + dfp_dcuo(ft) * fl_o3_ct(ft))                             &
             / (1.0 + dfp_dcuo(ft) * flux_o3(l))
    ELSE
      b      = ratio_o3 / (gl(l) * ra(l))                                      &
               + dfp_dcuo(ft) * o3mol(l) / ra(l)                               &
               - (1.0 + dfp_dcuo(ft) * fl_o3_ct(ft))
      c      = -ratio_o3 / (gl(l) * ra(l))                                     &
               * (1.0 + dfp_dcuo(ft) * fl_o3_ct(ft))
      fo3(l) = -0.5 * b + 0.5 * SQRT(b * b - 4.0 * c)
    END IF

    fo3(l) = MIN(MAX(fo3(l),0.0),1.0)

    !-------------------------------------------------------------------------
    ! Update the leaf conductance and photosynthesis
    !-------------------------------------------------------------------------
    gl(l) = gl(l) * fo3(l)
    al(l) = al(l) * fo3(l)
  END DO
!$OMP END DO

  !---------------------------------------------------------------------------
  ! Close stomata at points with negative or zero net photosynthesis
  ! or where the leaf resistance exceeds its maximum value.
  !---------------------------------------------------------------------------
!DIR$ IVDEP
!$OMP DO SCHEDULE(STATIC)
  DO j = 1,open_pts
    l = veg_index(open_index(j))

    IF (gl(l) <= glmin(ft) .OR. al(l) <= 0.0) THEN
      gl(l) = glmin(ft)
      al(l) = -rd(l) * fsmc(l) * fo3(l)
    END IF
  END DO
!$OMP END DO

END IF ! o3 damage

!$OMP END PARALLEL

!-----------------------------------------------------------------------------
! Define fluxes and conductances for points with closed stomata
!-----------------------------------------------------------------------------
!$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(j,l) IF(clos_pts > 1) SCHEDULE(STATIC) &
!$OMP  SHARED(clos_pts,veg_index,clos_index,gl,glmin,ft,al,rd,fsmc,l_o3_damage,fo3)
!DIR$ IVDEP
DO j = 1,clos_pts
  l = veg_index(clos_index(j))
  gl(l)    = glmin(ft)
  al(l)    = -rd(l) * fsmc(l)
  ! Indicate no ozone damage at closed points.
  IF (l_o3_damage) fo3(l) = 1.0

END DO
!$OMP END PARALLEL DO

IF ( l_o3_damage ) THEN
  !---------------------------------------------------------------------------
  ! Diagnose the ozone deposition flux on all points
  !---------------------------------------------------------------------------
!$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(j,l) IF(veg_pts > 1) SCHEDULE(STATIC)  &
!$OMP  SHARED(veg_pts,veg_index,flux_o3,o3mol,ra,gl,rd,fo3)
  DO j = 1,veg_pts
    l = veg_index(j)
    flux_o3(l) = o3mol(l) / (ra(l) + (ratio_o3 / gl(l)))
    rd(l)      = rd(l) * fo3(l)
  END DO
!$OMP END PARALLEL DO
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE leaf
END MODULE leaf_mod
