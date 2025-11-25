! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


MODULE init_params_mod

USE logging_mod, ONLY: log_info, log_warn, log_error, log_fatal

IMPLICIT NONE

PRIVATE
PUBLIC init_params

CONTAINS

! Fortran INCLUDE statements would be preferred, but (at least) the pgf90
! compiler objects to their use when the included file contains pre-processor
! directives. At present, such directives are used to exclude files from
! the UM build, so are required. This may change in the future.
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE init_params(nml_dir,progs)

!Module imports
USE jules_model_environment_mod,           ONLY: lsm_id, jules, cable

!TYPE definitions
USE prognostics, ONLY: progs_type

!Common modules
USE ereport_mod,              ONLY: ereport
USE jules_print_mgr,          ONLY: jules_message

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Initialises various model parameters
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists
CHARACTER(LEN=*), PARAMETER :: RoutineName='INIT_PARAMS'

!TYPES containing the data
TYPE(progs_type), INTENT(IN OUT) :: progs

!-----------------------------------------------------------------------------
! Variables

INTEGER :: errorstatus

!-----------------------------------------------------------------------------

! Process the PFT and non-veg parameters

SELECT CASE(lsm_id)

CASE (jules)
  CALL init_pftparm_jules(nml_dir,progs)
  CALL init_nvegparm_jules(nml_dir)

CASE (cable)
  CALL init_vegin_cbl(nml_dir,progs)
  CALL init_soilin_cbl(nml_dir)

CASE DEFAULT
  errorstatus = 101
  WRITE(jules_message,'(A,I0)') 'Unrecognised surface scheme. lsm_id = ',      &
     lsm_id
  CALL ereport(RoutineName, errorstatus, jules_message)

END SELECT

! Process the crop parameters
CALL init_cropparm(nml_dir)

! Process TRIFFID parameters
CALL init_triffid(nml_dir)

! Process atmospheric deposition species.
CALL init_deposition_species(nml_dir)

RETURN

END SUBROUTINE init_params
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE init_pftparm_jules(nml_dir,progs)

USE missing_data_mod, ONLY:                                                    &
!  imported scalar parameters
     rmdi

USE io_constants, ONLY: namelist_unit

USE string_utils_mod, ONLY: to_string

USE jules_soil_biogeochem_mod, ONLY: l_layeredC, soil_bgc_model,               &
                                      soil_model_rothc

USE jules_surface_types_mod, ONLY: npft, nnpft

USE ancil_info, ONLY: land_pts

USE pftparm_io, ONLY: jules_pftparm, init_pftparm_allocated,                   &
                      canht_ft_io, lai_io

USE pftparm, ONLY:                                                             &
  fsmc_mod,     psi_close,     psi_open,                                       &
  a_wl,         a_ws,          aef,                                            &
  act_jmax,     act_vcmax,     albsnc_max,                                     &
  albsnc_min,   albsnf_max,    albsnf_maxl,                                    &
  albsnf_maxu,  alpha,         alpha_elec,                                     &
  alnir,        alnirl,        alniru,                                         &
  alpar,        alparl,        alparu,                                         &
  avg_ba,       b_wl,          c3,                                             &
  can_struct_a, catch0,        ccleaf_max,                                     &
  ccleaf_min,   ccwood_max,    ccwood_min,                                     &
  ci_st,        dcatch_dlai,   deact_jmax,                                     &
  deact_vcmax,  dfp_dcuo,      dgl_dm,                                         &
  dgl_dt,       dqcrit,        ds_jmax,                                        &
  ds_vcmax,     dust_veg_scj,  dz0v_dh,                                        &
  emis_pft,     eta_sl,        f0,                                             &
  fd,           fef_bc,        fef_ch4,                                        &
  fef_co,       fef_co2,       fef_nox,                                        &
  fef_oc,       fef_so2,       fire_mort,                                      &
  fl_o3_ct,     fsmc_of,       fsmc_p0,                                        &
  g1_stomata,   g_leaf_0,      glmin,                                          &
  gpp_st,       gsoil_f,       hw_sw,                                          &
  ief,          infil_f,       jv25_ratio,                                     &
  kext,         kn,            knl,                                            &
  kpar,         lai_alb_lim,   lma,                                            &
  mef,          neff,          nl0,                                            &
  nmass,        nr,            nr_nl,                                          &
  ns_nl,        nsw,           omega,                                          &
  omegal,       omegau,        omnir,                                          &
  omnirl,       omniru,        orient,                                         &
  q10_leaf,     r_grow,        rootd_ft,                                       &
  sigl,         tef,           tleaf_of,                                       &
  tlow,         tupp,          vint,                                           &
  vsl,          z0v

USE c_z0h_z0m, ONLY: z0h_z0m, z0h_z0m_classic

USE jules_vegetation_mod, ONLY: can_rad_mod, l_crop, l_trait_phys,             &
                                 l_use_pft_psi, l_bvoc_emis, l_inferno,        &
                                 l_o3_damage, l_trif_fire, photo_acclim_model, &
                                 photo_act_model, photo_act_pft,               &
                                 photo_farquhar, photo_model,                  &
                                 stomata_jacobs, stomata_medlyn,               &
                                 stomata_model, l_spec_veg_z0

USE jules_radiation_mod, ONLY: l_spec_albedo, l_albedo_obs, l_snow_albedo

USE errormessagelength_mod, ONLY: errormessagelength

!TYPE definitions
USE prognostics, ONLY: progs_type

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Initialises the PFT parameters
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists

!TYPES containing the data
TYPE(progs_type), INTENT(IN OUT) :: progs

! Work variables
INTEGER :: ERROR  ! Error indicator
INTEGER :: i      ! Loop counter.
CHARACTER(LEN=errormessagelength) :: iomessage

CHARACTER(LEN=*), PARAMETER :: routinename='INIT_PFTPARM_JULES'

!-----------------------------------------------------------------------------
! Read namelist
!-----------------------------------------------------------------------------
CALL log_info(routinename, "Reading JULES_PFTPARM namelist...")

! Open the pft parameters namelist file
OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'pft_params.nml'),           &
               STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR, &
               IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(routinename,                                                  &
                 "Error opening namelist file pft_params.nml " //              &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

READ(namelist_unit, NML = jules_pftparm, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(routinename,                                                  &
                 "Error reading namelist JULES_PFTPARM " //                    &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

! Close the namelist file
CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(routinename,                                                  &
                 "Error closing namelist file pft_params.nml " //              &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

!-----------------------------------------------------------------------------
! Process the namelist values
!-----------------------------------------------------------------------------
CALL init_pftparm_allocated()
progs%canht_pft(:,:)   = SPREAD(canht_ft_io(1:npft), 1, land_pts)
progs%lai_pft(:,:)     = SPREAD(lai_io(1:npft), 1, land_pts)

!-----------------------------------------------------------------------------
! Check that all required variables were present in the namelist.
! The namelist variables were initialised to rmdi.
! Some configurations don't need all parameters but in some cases these are
! still tested below.
!-----------------------------------------------------------------------------
ERROR = 0
IF ( ANY( orient(:) < 0 ) ) THEN  ! orient was initialised to < 0
  ERROR = 1
  CALL log_error(routinename, "No value for orient")
END IF
IF ( ANY( fsmc_mod(:) < 0 ) ) THEN  ! fsmc_mod was initialised to < 0
  ERROR = 1
  CALL log_error(routinename, "No value for fsmc_mod")
END IF
IF ( ANY( ABS( kext(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for kext")
END IF
IF ( ANY( ABS( kpar(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for kpar")
END IF
IF ( ANY( ABS( lai_alb_lim(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for lai_alb_lim")
END IF
IF ( ANY( ABS( can_struct_a(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for can_struct_a")
END IF
IF ( ANY( ABS( gsoil_f(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for gsoil_f")
END IF
IF ( ANY( c3(:) < 0 ) ) THEN  ! c3 was initialised to < 0
  ERROR = 1
  CALL log_error(routinename, "No value for c3")
END IF
IF ( ANY( ABS( alpha(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for alpha")
END IF
IF ( ANY( ABS( fd(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for fd")
END IF
IF ( ANY( ABS( neff(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for neff")
END IF
IF ( ANY( ABS( nl0(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for nl0")
END IF
IF ( ANY( ABS( nr_nl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for nr_nl")
END IF
IF ( ANY( ABS( ns_nl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for ns_nl")
END IF
IF ( ANY( ABS( r_grow(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for r_grow")
END IF
! Note that tlow and tupp are always required, for some PFTs at least.
! If using the Farquhar model for C3 plants, we still need tlow and
! tupp for C4 plants.
IF ( ANY( ABS( tlow(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for tlow")
END IF
IF ( ANY( ABS( tupp(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for tupp")
END IF

SELECT CASE ( photo_model )
CASE ( photo_farquhar )
  !---------------------------------------------------------------------------
  ! First check parameters that are always required with this model.
  !---------------------------------------------------------------------------
  ! Note that these parameter values are not used for C4 plants, but
  ! here we're still checking that they have been provided.
  IF ( ANY( ABS( alpha_elec(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error("init_pftparm", "No value for alpha_elec")
  END IF
  IF ( ANY( ABS( deact_jmax(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error("init_pftparm", "No value for deact_jmax")
  END IF
  IF ( ANY( ABS( deact_vcmax(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error("init_pftparm", "No value for deact_vcmax")
  END IF
  !---------------------------------------------------------------------------
  ! Check parameters that depend on any chosen acclimation model.
  !---------------------------------------------------------------------------
  IF ( photo_acclim_model == 0 ) THEN
    ! No acclimation.
    IF ( ANY( ABS( ds_jmax(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      ERROR = 1
      CALL log_error("init_pftparm", "No value for ds_jmax")
    END IF
    IF ( ANY( ABS( ds_vcmax(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      ERROR = 1
      CALL log_error("init_pftparm", "No value for ds_vcmax")
    END IF
    IF ( ANY( ABS( jv25_ratio(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      ERROR = 1
      CALL log_error("init_pftparm", "No value for jv25_ratio")
    END IF
  END IF  !  photo_acclim_model == 0

  IF ( photo_acclim_model == 0 .OR.                                            &
      (photo_acclim_model /= 0 .AND. photo_act_model == photo_act_pft) ) THEN
    IF ( ANY( ABS( act_jmax(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      ERROR = 1
      CALL log_error("init_pftparm", "No value for act_jmax")
    END IF
    IF ( ANY( ABS( act_vcmax(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      ERROR = 1
      CALL log_error("init_pftparm", "No value for act_vcmax")
    END IF
  END IF  !  photo_acclim_model

END SELECT  !  photo_model

SELECT CASE ( stomata_model )
CASE ( stomata_jacobs )
  IF ( ANY( ABS( dqcrit(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error("init_pftparm", "No value for dqcrit")
  END IF
  IF ( ANY( ABS( f0(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error("init_pftparm", "No value for f0")
  END IF
CASE ( stomata_medlyn )
  IF ( ANY( ABS( g1_stomata(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error("init_pftparm", "No value for g1_stomata")
  END IF
END SELECT

IF ( .NOT. l_spec_albedo .AND. can_rad_mod == 1 ) THEN
  ! These don't need to be set
ELSE
  IF ( ANY( ABS( alnir(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for alnir")
  END IF
  IF ( ANY( ABS( alpar(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for alpar")
  END IF
  IF ( ANY( ABS( omega(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for omega")
  END IF
  IF ( ANY( ABS( omnir(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for omnir")
  END IF
END IF

IF ( l_albedo_obs ) THEN
  IF ( ANY( ABS( alnirl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for alnirl")
  END IF
  IF ( ANY( ABS( alniru(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for alniru")
  END IF
  IF ( ANY( ABS( alparl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for alparl")
  END IF
  IF ( ANY( ABS( alparu(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for alparu")
  END IF
  IF ( ANY( ABS( omegal(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for omegal")
  END IF
  IF ( ANY( ABS( omegau(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for omegau")
  END IF
  IF ( ANY( ABS( omnirl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for omnirl")
  END IF
  IF ( ANY( ABS( omniru(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for omniru")
  END IF
END IF

IF ( .NOT. l_spec_albedo ) THEN
  IF ( ANY( ABS( albsnf_max(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for albsnf_max")
  END IF
  IF ( l_albedo_obs ) THEN
    IF ( ANY( ABS( albsnf_maxl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      ERROR = 1
      CALL log_error(routinename, "No value for albsnf_maxl")
    END IF
    IF ( ANY( ABS( albsnf_maxu(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      ERROR = 1
      CALL log_error(routinename, "No value for albsnf_maxu")
    END IF
  END IF
END IF

IF ( .NOT. l_snow_albedo ) THEN
  IF ( ANY( ABS( albsnc_max(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for albsnc_max")
  END IF
  IF ( ANY( ABS( albsnc_min(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for albsnc_min")
  END IF
END IF

IF (l_trait_phys) THEN
  IF ( ANY( ABS( lma(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for lma")
  END IF
  IF ( ANY( ABS( nmass(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for nmass")
  END IF
  IF ( ANY( ABS( vsl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for vsl")
  END IF
  IF ( ANY( ABS( vint(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for vint")
  END IF
  IF ( ANY( ABS( nr(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for nr")
  END IF
  IF ( ANY( ABS( nsw(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for nsw")
  END IF
  IF ( ANY( ABS( hw_sw(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for hw_sw")
  END IF

END IF !l_trait_phys

IF ( ANY( ABS( kn(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for kn")
END IF
IF ( can_rad_mod == 6 .AND. ANY( ABS( knl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for knl")
END IF
IF ( ANY( ABS( q10_leaf(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for q10_leaf")
END IF
IF ( ANY( ABS( a_wl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for a_wl")
END IF
IF ( ANY( ABS( a_ws(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for a_ws")
END IF
IF ( ANY( ABS( b_wl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for b_wl")
END IF
IF ( ANY( ABS( eta_sl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for eta_sl")
END IF
IF ( ANY( ABS( sigl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for sigl")
END IF
IF ( ANY( ABS( g_leaf_0(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for g_leaf_0")
END IF
IF ( ANY( ABS( dgl_dm(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for dgl_dm")
END IF
IF ( ANY( ABS( fsmc_of(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for fsmc_of")
END IF
IF ( ANY( ABS( dgl_dt(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for dgl_dt")
END IF
IF ( ANY( ABS( tleaf_of(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for tleaf_of")
END IF
IF ( ANY( ABS( catch0(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for catch0")
END IF
IF ( ANY( ABS( dcatch_dlai(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for dcatch_dlai")
END IF
IF ( ANY( ABS( infil_f(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for infil_f")
END IF
IF ( ANY( ABS( glmin(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for glmin")
END IF
IF ( .NOT. l_spec_veg_z0) THEN
  IF ( ANY( ABS( dz0v_dh(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for dz0v_dh")
  END IF
ELSE
  IF ( ANY( ABS( z0v(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error("init_pftparm", "No value for z0v")
  END IF
END IF
IF ( ANY( ABS( rootd_ft(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for rootd_ft")
END IF

IF ( l_use_pft_psi ) THEN
  IF ( ANY( ABS( psi_close(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for psi_close")
  END IF
  IF ( ANY( ABS( psi_open(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for psi_open")
  END IF
END IF !l_use_pft_psi

IF ( l_bvoc_emis ) THEN
  IF ( ANY( ABS( ci_st(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for ci_st")
  END IF
  IF ( ANY( ABS( gpp_st(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for gpp_st")
  END IF
  IF ( ANY( ABS( ief(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for ief")
  END IF
  IF ( ANY( ABS( tef(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for tef")
  END IF
  IF ( ANY( ABS( mef(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for mef")
  END IF
  IF ( ANY( ABS( aef(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for aef")
  END IF
END IF

IF ( l_inferno ) THEN
  IF ( ANY( ABS( fef_co2(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for fef_co2")
  END IF
  IF ( ANY( ABS( fef_co(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for fef_co")
  END IF
  IF ( ANY( ABS( fef_ch4(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for fef_ch4")
  END IF
  IF ( ANY( ABS( fef_nox(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for fef_nox")
  END IF
  IF ( ANY( ABS( fef_so2(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for fef_so2")
  END IF
  IF ( ANY( ABS( ccleaf_min(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for ccleaf_min")
  END IF
  IF ( ANY( ABS( ccleaf_max(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for ccleaf_max")
  END IF
  IF ( ANY( ABS( ccwood_min(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for ccwood_min")
  END IF
  IF ( ANY( ABS( ccwood_max(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for ccwood_max")
  END IF
  IF ( ANY( ABS( avg_ba(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for avg_ba")
  END IF
END IF

IF ( l_trif_fire ) THEN
  IF ( ANY( ABS( fire_mort(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for fire_mort")
  END IF
END IF

IF ( l_o3_damage ) THEN
  IF ( ANY( ABS( fl_o3_ct(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for fl_o3_ct")
  END IF
  IF ( ANY( ABS( dfp_dcuo(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for dfp_dcuo")
  END IF
END IF

IF ( ANY( ABS( fsmc_p0(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for fsmc_p0")
END IF
IF ( ANY( ABS( emis_pft(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for emis_pft")
END IF
IF ( ANY( ABS( z0h_z0m(1:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for z0hm_pft")
END IF
IF ( ANY( ABS( z0h_z0m_classic(1:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for z0hm_classic_pft")
END IF
IF ( ANY( ABS( progs%canht_pft(:,:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for canht_ft")
END IF
IF ( ANY( ABS( progs%lai_pft(:,:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for lai")
END IF

IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(routinename,                                                  &
                 "Variable(s) missing from namelist - see earlier " //         &
                 "error message(s)")

!-----------------------------------------------------------------------------
! Check that glmin is >0.
! This ensures that wt_ext in subroutine soil_evap cannot become a NaN (which
! it would if gs=glmin and gsoil=0), or blow up, and might well be required
! elsewhere too.
!-----------------------------------------------------------------------------
IF ( ANY(glmin < 1.0e-10) )                                                    &
  CALL log_warn(routinename,                                                   &
                "Increasing one or more values of glmin - very small " //      &
                "values can cause model to blow up or NaNs")

WHERE ( glmin < 1.0e-10 )
  glmin = 1.0e-10
END WHERE

IF ( l_crop ) THEN
  IF ( ANY( ABS( a_ws(nnpft+1: npft) - 1.0 ) > EPSILON(1.0) ) ) THEN
    CALL log_fatal(routinename, "crop tiles should have a_ws=1.0")
  END IF
END IF

IF ( l_use_pft_psi ) THEN
  IF ( ANY( psi_close(1: npft) > EPSILON(1.0) ) ) THEN
    CALL log_fatal(routinename, "psi_close should be negative")
  END IF
  IF ( ANY( psi_open(1: npft) > EPSILON(1.0) ) ) THEN
    CALL log_fatal(routinename, "psi_open should be negative")
  END IF
END IF

!-----------------------------------------------------------------------------
! fsmc_mod=1 should not be allowed with a layered RothC  model until this has
! been properly evaluated. (With fsmc_mod=1, subroutine root_frac does not
! return the exponential root profile that users might expect.)
! Note that l_layeredC=T is not currently allowed with the UM.
!-----------------------------------------------------------------------------
IF ( l_layeredC .AND. ( soil_bgc_model == soil_model_rothc ) .AND.             &
     ANY( fsmc_mod(:) == 1 ) ) THEN
  CALL log_error(routinename,                                                  &
                 "fsmc_mod=1 is not allowed with l_layeredC and RothC")
END IF

RETURN

END SUBROUTINE init_pftparm_jules


SUBROUTINE init_vegin_cbl(nml_dir,progs)

USE missing_data_mod, ONLY:                                                    &
!  imported scalar parameters
     rmdi
USE io_constants, ONLY: namelist_unit

USE string_utils_mod, ONLY: to_string

USE errormessagelength_mod, ONLY: errormessagelength

USE logging_mod, ONLY: log_info, log_fatal

USE jules_surface_types_mod, ONLY: ntype, npft

USE ancil_info, ONLY: land_pts

USE grid_constants_mod_cbl, ONLY: nsl, nscs, nvcs, nrb

USE max_dimensions, ONLY: ntype_max

!TYPE definitions
USE prognostics,      ONLY: progs_type
USE cable_fields_mod, ONLY: pars_io_data_cbl

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!  Reads the JULES_PFT_PARAMS_CABLE namelist for standalone runs
!-----------------------------------------------------------------------------
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists
!TYPES containing the data
TYPE(progs_type), INTENT(IN OUT) :: progs

! Work variables
INTEGER :: ERROR  ! Error indicator
CHARACTER(LEN=errormessagelength) :: iomessage

CHARACTER(LEN=*), PARAMETER :: routinename='INIT_VEGIN_CABLE'

! With some compilers, namelists cannot contain multidimensional arrays.
! Therefore, an input type without multidimensional arrays is used to read
! in the the values from the namelist, and these values will then be
! transferred to the desired data type which does contain multidimensional
! arrays

! Whereas in JULES PFT parameters are only used for veg types, in CABLE they
! are used for both. Therefore CABLE PFT arrays need to be allocated ntype_max,
! which equals (npft_max + nvg_max).

REAL ::                                                                        &
      canst1_io(ntype_max),                                                    &
      length_io(ntype_max),                                                    &
      width_io(ntype_max),                                                     &
      vcmax_io(ntype_max),                                                     &
      ejmax_io(ntype_max),                                                     &
      hc_io(ntype_max),                                                        &
      xfang_io(ntype_max),                                                     &
      rp20_io(ntype_max),                                                      &
      rpcoef_io(ntype_max),                                                    &
      rs20_io(ntype_max),                                                      &
      wai_io(ntype_max),                                                       &
      rootbeta_io(ntype_max),                                                  &
      shelrb_io(ntype_max),                                                    &
      vegcf_io(ntype_max),                                                     &
      frac4_io(ntype_max),                                                     &
      xalbnir_io(ntype_max),                                                   &
      extkn_io(ntype_max),                                                     &
      tminvj_io(ntype_max),                                                    &
      tmaxvj_io(ntype_max),                                                    &
      vbeta_io(ntype_max),                                                     &
      a1gs_io(ntype_max),                                                      &
      d0gs_io(ntype_max),                                                      &
      alpha_io(ntype_max),                                                     &
      convex_io(ntype_max),                                                    &
      cfrd_io(ntype_max),                                                      &
      gswmin_io(ntype_max),                                                    &
      conkc0_io(ntype_max),                                                    &
      conko0_io(ntype_max),                                                    &
      ekc_io(ntype_max),                                                       &
      eko_io(ntype_max),                                                       &
      g0_io(ntype_max),                                                        &
      g1_io(ntype_max),                                                        &
      zr_io(ntype_max),                                                        &
      clitt_io(ntype_max),                                                     &
      froot1_io(ntype_max),                                                    &
      froot2_io(ntype_max),                                                    &
      froot3_io(ntype_max),                                                    &
      froot4_io(ntype_max),                                                    &
      froot5_io(ntype_max),                                                    &
      froot6_io(ntype_max),                                                    &
      csoil1_io(ntype_max),                                                    &
      csoil2_io(ntype_max),                                                    &
      ratecs1_io(ntype_max),                                                   &
      ratecs2_io(ntype_max),                                                   &
      cplant1_io(ntype_max),                                                   &
      cplant2_io(ntype_max),                                                   &
      cplant3_io(ntype_max),                                                   &
      ratecp1_io(ntype_max),                                                   &
      ratecp2_io(ntype_max),                                                   &
      ratecp3_io(ntype_max),                                                   &
      refl1_io(ntype_max),                                                     &
      refl2_io(ntype_max),                                                     &
      refl3_io(ntype_max),                                                     &
      taul1_io(ntype_max),                                                     &
      taul2_io(ntype_max),                                                     &
      taul3_io(ntype_max),                                                     &
      lai_io(ntype_max)

!-----------------------------------------------------------------------------
! Namelist definition
!-----------------------------------------------------------------------------
NAMELIST / cable_pftparm/ canst1_io, length_io, width_io, vcmax_io,            &
          ejmax_io, hc_io, xfang_io, rp20_io, rpcoef_io, rs20_io, wai_io,      &
          rootbeta_io, shelrb_io, vegcf_io, frac4_io, xalbnir_io, extkn_io,    &
          tminvj_io, tmaxvj_io, vbeta_io, a1gs_io, d0gs_io, alpha_io,          &
          convex_io, cfrd_io, gswmin_io, conkc0_io, conko0_io, ekc_io,         &
          eko_io, g0_io, g1_io, zr_io, clitt_io, froot1_io, froot2_io,         &
          froot3_io, froot4_io, froot5_io, froot6_io, cplant1_io,              &
          cplant2_io, cplant3_io, csoil1_io, csoil2_io, ratecp1_io,            &
          ratecp2_io, ratecp3_io, ratecs1_io, ratecs2_io, refl1_io,            &
          refl2_io, refl3_io, taul1_io, taul2_io, taul3_io, lai_io

!-----------------------------------------------------------------------------
! Initialise namelist values before reading them
!-----------------------------------------------------------------------------
canst1_io(:ntype)     = rmdi
length_io(:ntype)     = rmdi
width_io(:ntype)      = rmdi
vcmax_io(:ntype)      = rmdi
ejmax_io(:ntype)      = rmdi
hc_io(:ntype)         = rmdi
xfang_io(:ntype)      = rmdi
rp20_io(:ntype)       = rmdi
rpcoef_io(:ntype)     = rmdi
rs20_io(:ntype)       = rmdi
wai_io(:ntype)        = rmdi
rootbeta_io(:ntype)   = rmdi
shelrb_io(:ntype)     = rmdi
vegcf_io(:ntype)      = rmdi
frac4_io(:ntype)      = rmdi
xalbnir_io(:ntype)    = rmdi
extkn_io(:ntype)      = rmdi
tminvj_io(:ntype)     = rmdi
tmaxvj_io(:ntype)     = rmdi
vbeta_io(:ntype)      = rmdi
a1gs_io(:ntype)       = rmdi
d0gs_io(:ntype)       = rmdi
alpha_io(:ntype)      = rmdi
convex_io(:ntype)     = rmdi
cfrd_io(:ntype)       = rmdi
gswmin_io(:ntype)     = rmdi
conkc0_io(:ntype)     = rmdi
conko0_io(:ntype)     = rmdi
ekc_io(:ntype)        = rmdi
eko_io(:ntype)        = rmdi
g0_io(:ntype)         = rmdi
g1_io(:ntype)         = rmdi
zr_io(:ntype)         = rmdi
clitt_io(:ntype)      = rmdi
froot1_io(:ntype)     = rmdi
froot2_io(:ntype)     = rmdi
froot3_io(:ntype)     = rmdi
froot4_io(:ntype)     = rmdi
froot5_io(:ntype)     = rmdi
froot6_io(:ntype)     = rmdi
cplant1_io(:ntype)    = rmdi
cplant2_io(:ntype)    = rmdi
cplant3_io(:ntype)    = rmdi
csoil1_io(:ntype)     = rmdi
csoil2_io(:ntype)     = rmdi
ratecp1_io(:ntype)    = rmdi
ratecp2_io(:ntype)    = rmdi
ratecp3_io(:ntype)    = rmdi
ratecs1_io(:ntype)    = rmdi
ratecs2_io(:ntype)    = rmdi
refl1_io(:ntype)      = rmdi
refl2_io(:ntype)      = rmdi
refl3_io(:ntype)      = rmdi
taul1_io(:ntype)      = rmdi
taul2_io(:ntype)      = rmdi
taul3_io(:ntype)      = rmdi
lai_io(:ntype)        = rmdi

!-----------------------------------------------------------------------------
! Read namelist
!-----------------------------------------------------------------------------
CALL log_info(routinename, "Reading CABLE_PFTPARM namelist...")

OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'pft_params.nml'),           &
               STATUS='old', POSITION='rewind', ACTION='read', IOSTAT  = ERROR,&
               IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(routinename,                                                  &
                 "Error opening namelist file pft_params.nml " //              &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

READ(namelist_unit, NML = cable_pftparm, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(routinename,                                                  &
                 "Error reading namelist CABLE_PFTPARM " //                    &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(routinename,                                                  &
                 "Error closing namelist file pft_params.nml " //              &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

!-----------------------------------------------------------------------------
! Transfer values from io values to vegin
!-----------------------------------------------------------------------------

pars_io_data_cbl%vegin_canst1(:ntype)     = canst1_io(:ntype)
pars_io_data_cbl%vegin_length(:ntype)     = length_io(:ntype)
pars_io_data_cbl%vegin_width(:ntype)      = width_io(:ntype)
pars_io_data_cbl%vegin_vcmax(:ntype)      = vcmax_io(:ntype)
pars_io_data_cbl%vegin_ejmax(:ntype)      = ejmax_io(:ntype)
pars_io_data_cbl%vegin_hc(:ntype)         = hc_io(:ntype)
pars_io_data_cbl%vegin_xfang(:ntype)      = xfang_io(:ntype)
pars_io_data_cbl%vegin_rp20(:ntype)       = rp20_io(:ntype)
pars_io_data_cbl%vegin_rpcoef(:ntype)     = rpcoef_io(:ntype)
pars_io_data_cbl%vegin_rs20(:ntype)       = rs20_io(:ntype)
pars_io_data_cbl%vegin_wai(:ntype)        = wai_io(:ntype)
pars_io_data_cbl%vegin_rootbeta(:ntype)   = rootbeta_io(:ntype)
pars_io_data_cbl%vegin_shelrb(:ntype)     = shelrb_io(:ntype)
pars_io_data_cbl%vegin_vegcf(:ntype)      = vegcf_io(:ntype)
pars_io_data_cbl%vegin_frac4(:ntype)      = frac4_io(:ntype)
pars_io_data_cbl%vegin_xalbnir(:ntype)    = xalbnir_io(:ntype)
pars_io_data_cbl%vegin_extkn(:ntype)      = extkn_io(:ntype)
pars_io_data_cbl%vegin_tminvj(:ntype)     = tminvj_io(:ntype)
pars_io_data_cbl%vegin_tmaxvj(:ntype)     = tmaxvj_io(:ntype)
pars_io_data_cbl%vegin_vbeta(:ntype)      = vbeta_io(:ntype)
pars_io_data_cbl%vegin_a1gs(:ntype)       = a1gs_io(:ntype)
pars_io_data_cbl%vegin_d0gs(:ntype)       = d0gs_io(:ntype)
pars_io_data_cbl%vegin_alpha(:ntype)      = alpha_io(:ntype)
pars_io_data_cbl%vegin_convex(:ntype)     = convex_io(:ntype)
pars_io_data_cbl%vegin_cfrd(:ntype)       = cfrd_io(:ntype)
pars_io_data_cbl%vegin_gswmin(:ntype)     = gswmin_io(:ntype)
pars_io_data_cbl%vegin_conkc0(:ntype)     = conkc0_io(:ntype)
pars_io_data_cbl%vegin_conko0(:ntype)     = conko0_io(:ntype)
pars_io_data_cbl%vegin_ekc(:ntype)        = ekc_io(:ntype)
pars_io_data_cbl%vegin_eko(:ntype)        = eko_io(:ntype)
pars_io_data_cbl%vegin_g0(:ntype)         = g0_io(:ntype)
pars_io_data_cbl%vegin_g1(:ntype)         = g1_io(:ntype)
pars_io_data_cbl%vegin_zr(:ntype)         = zr_io(:ntype)
pars_io_data_cbl%vegin_clitt(:ntype)      = clitt_io(:ntype)
pars_io_data_cbl%vegin_froot(1,:ntype)    = froot1_io(:ntype)
pars_io_data_cbl%vegin_froot(2,:ntype)    = froot2_io(:ntype)
pars_io_data_cbl%vegin_froot(3,:ntype)    = froot3_io(:ntype)
pars_io_data_cbl%vegin_froot(4,:ntype)    = froot4_io(:ntype)
pars_io_data_cbl%vegin_froot(5,:ntype)    = froot5_io(:ntype)
pars_io_data_cbl%vegin_froot(6,:ntype)    = froot6_io(:ntype)
pars_io_data_cbl%vegin_cplant(1,:ntype)   = cplant1_io(:ntype)
pars_io_data_cbl%vegin_cplant(2,:ntype)   = cplant2_io(:ntype)
pars_io_data_cbl%vegin_cplant(3,:ntype)   = cplant3_io(:ntype)
pars_io_data_cbl%vegin_csoil(1,:ntype)    = csoil1_io(:ntype)
pars_io_data_cbl%vegin_csoil(2,:ntype)    = csoil2_io(:ntype)
pars_io_data_cbl%vegin_ratecp(1,:ntype)   = ratecp1_io(:ntype)
pars_io_data_cbl%vegin_ratecp(2,:ntype)   = ratecp2_io(:ntype)
pars_io_data_cbl%vegin_ratecp(3,:ntype)   = ratecp3_io(:ntype)
pars_io_data_cbl%vegin_ratecs(1,:ntype)   = ratecs1_io(:ntype)
pars_io_data_cbl%vegin_ratecs(2,:ntype)   = ratecs2_io(:ntype)
pars_io_data_cbl%vegin_refl(1,:ntype)     = refl1_io(:ntype)
pars_io_data_cbl%vegin_refl(2,:ntype)     = refl2_io(:ntype)
pars_io_data_cbl%vegin_refl(3,:ntype)     = refl3_io(:ntype)
pars_io_data_cbl%vegin_taul(1,:ntype)     = taul1_io(:ntype)
pars_io_data_cbl%vegin_taul(2,:ntype)     = taul2_io(:ntype)
pars_io_data_cbl%vegin_taul(3,:ntype)     = taul3_io(:ntype)
progs%canht_pft(:,:npft)       = SPREAD(hc_io(1:npft), 1, land_pts)
progs%lai_pft(:,:npft)         = SPREAD(lai_io(1:npft), 1, land_pts)

!-----------------------------------------------------------------------------
! Check that all required variables were present in the namelist.
! The namelist variables were initialised to rmdi.
! Some configurations don't need all parameters but for now we insist on
! getting all parameters (and that there are not rmdi!).
!-----------------------------------------------------------------------------
ERROR = 0
IF ( ANY( ABS( pars_io_data_cbl%vegin_canst1(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for canst1")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_length(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for length")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_width(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for width")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_vcmax(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for vcmax")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_ejmax(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for ejmax")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_hc(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for hc")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_xfang(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for xfang")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_rp20(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for rp20")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_rpcoef(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for rpcoef")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_rs20(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for rs20")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_wai(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for wai")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_rootbeta(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for rootbeta")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_shelrb(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for shelrb")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_vegcf(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for vegcf")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_frac4(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for frac4")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_xalbnir(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for xalbnir")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_extkn(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for extkni")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_tminvj(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for tminvj")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_tmaxvj(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for tmaxvj")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_vbeta(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for vbeta")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_a1gs(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for a1hs")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_d0gs(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for d0gs")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_alpha(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for alpha")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_convex(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for convex")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_cfrd(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for cfrd")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_gswmin(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for gswmin")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_conkc0(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for conkc0")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_conko0(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for conko0")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_ekc(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for ekc")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_eko(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for eko")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_g0(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for g0")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_g1(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for g1")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_zr(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for zr")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_clitt(:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for clitt")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_froot(:,:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for froot")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_cplant(:,:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for cplant")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_csoil(:,:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for csoil")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_ratecp(:,:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for ratecp")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_ratecs(:,:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for ratecs")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_refl(:,:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for refl")
END IF
IF ( ANY( ABS( pars_io_data_cbl%vegin_taul(:,:ntype) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for taul")
END IF
IF ( ANY( ABS( progs%canht_pft(:,:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for hc")
END IF
IF ( ANY( ABS( progs%lai_pft(:,:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for lai")
END IF

IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(routinename,                                                  &
                 "Variable(s) missing from namelist - see earlier " //         &
                 "error message(s)")

pars_io_data_cbl%vegin_dleaf(:) = SQRT(pars_io_data_cbl%vegin_width(:) * pars_io_data_cbl%vegin_length(:))

END SUBROUTINE init_vegin_cbl

! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE init_nvegparm_jules(nml_dir)


USE io_constants, ONLY: namelist_unit

USE string_utils_mod, ONLY: to_string

USE jules_surface_types_mod, ONLY: nnvg, npft

USE nvegparm_io, ONLY: jules_nvegparm, init_nvegparm_allocated

USE nvegparm, ONLY: check_jules_nvegparm

USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Initialises the non-vegetation parameters
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists
! Work variables
INTEGER :: ERROR  ! Error indicator
CHARACTER(LEN=errormessagelength) :: iomessage

CHARACTER(LEN=*), PARAMETER :: routinename='INIT_NVEGPARM_JULES'

!-----------------------------------------------------------------------------
! Read namelist
!-----------------------------------------------------------------------------
CALL log_info(routinename, "Reading JULES_NVEGPARM namelist...")

! Open the nveg parameters namelist file
OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'nveg_params.nml'),          &
               STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR, &
               IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(routinename,                                                  &
                 "Error opening namelist file nveg_params.nml " //             &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

READ(namelist_unit, NML = jules_nvegparm, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(routinename,                                                  &
                 "Error reading namelist JULES_NVEGPARM " //                   &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

! Close the namelist file
CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(routinename,                                                  &
                 "Error closing namelist file nveg_params.nml " //             &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")


!-----------------------------------------------------------------------------
! Process the namelist values
!-----------------------------------------------------------------------------
! Copy values from fixed length arrays used in namelist into allocated arrays
CALL init_nvegparm_allocated()
CALL check_jules_nvegparm(nnvg,npft)

RETURN

END SUBROUTINE init_nvegparm_jules
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

SUBROUTINE init_cropparm(nml_dir)

USE missing_data_mod, ONLY: rmdi

USE io_constants, ONLY: namelist_unit

USE string_utils_mod, ONLY: to_string

USE jules_surface_types_mod, ONLY: ncpft

USE cropparm, ONLY: t_bse, t_opt, t_max, tt_emr, crit_pp, pp_sens, rt_dir,     &
  alpha1, alpha2, alpha3, beta1, beta2, beta3, r_gamma, delta, remob,          &
  cfrac_s, cfrac_r, cfrac_l, allo1, allo2, mu, nu, yield_frac, initial_carbon, &
  initial_c_dvi, sen_dvi, t_mort

USE cropparm_io, ONLY: jules_cropparm,                                         &
   t_bse_io, t_opt_io, t_max_io, tt_emr_io, crit_pp_io, pp_sens_io,            &
   rt_dir_io, alpha1_io, alpha2_io, alpha3_io, beta1_io, beta2_io, beta3_io,   &
   gamma_io, delta_io, remob_io, cfrac_s_io, cfrac_r_io, cfrac_l_io,           &
   allo1_io, allo2_io, mu_io, nu_io, yield_frac_io, initial_carbon_io,         &
   initial_c_dvi_io, sen_dvi_io, t_mort_io


USE jules_vegetation_mod, ONLY: l_crop

USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Initialises the crop PFT parameters
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!
!-----------------------------------------------------------------------------
! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists

! Work variables
INTEGER :: ERROR  ! Error indicator
CHARACTER(LEN=errormessagelength) :: iomessage

!----------------------------------------------------------------------------

! Nothing to do if crop model is not selected
IF ( .NOT. l_crop ) RETURN

!-----------------------------------------------------------------------------
! Initialise namelist values before reading them
!-----------------------------------------------------------------------------

T_BSE_io(:)     = rmdi
T_OPT_io(:)     = rmdi
T_MAX_io(:)     = rmdi
TT_EMR_io(:)    = rmdi

CRIT_PP_io(:)   = rmdi
PP_SENS_io(:)   = rmdi
RT_DIR_io(:)    = rmdi
ALPHA1_io(:)    = rmdi

ALPHA2_io(:)    = rmdi
ALPHA3_io(:)    = rmdi
BETA1_io(:)     = rmdi
BETA2_io(:)     = rmdi
BETA3_io(:)     = rmdi

gamma_io(:)     = rmdi
DELTA_io(:)     = rmdi
REMOB_io(:)     = rmdi
CFRAC_S_io(:)   = rmdi
CFRAC_R_io(:)   = rmdi

CFRAC_L_io(:)   = rmdi
ALLO1_io(:)     = rmdi
ALLO2_io(:)     = rmdi

mu_io(:)             = rmdi
nu_io(:)             = rmdi
yield_frac_io(:)     = rmdi
initial_carbon_io(:) = rmdi
initial_c_dvi_io(:)  = rmdi
sen_dvi_io(:)        = rmdi
t_mort_io(:)         = rmdi

!-----------------------------------------------------------------------------
! Read namelist
!-----------------------------------------------------------------------------
CALL log_info("init_cropparm", "Reading JULES_CROPPARM namelist...")

! Open the crop pft parameters namelist file
OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'crop_params.nml'),          &
               STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR, &
               IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_cropparm",                                              &
                 "Error opening namelist file crop_params.nml " //             &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

READ(namelist_unit, NML = jules_cropparm, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_cropparm",                                              &
                 "Error reading namelist JULES_CROPPARM " //                   &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

! Close the namelist file
CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_cropparm",                                              &
                 "Error closing namelist file crop_params.nml " //             &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")


!-----------------------------------------------------------------------------
! Process the namelist values
!-----------------------------------------------------------------------------
! Copy values from fixed length arrays used in namelist into allocated arrays

t_bse(:)   = T_BSE_io(1:ncpft)
t_opt(:)   = T_OPT_io(1:ncpft)
t_max(:)   = T_MAX_io(1:ncpft)
tt_emr(:)  = TT_EMR_io(1:ncpft)
crit_pp(:) = CRIT_PP_io(1:ncpft)
pp_sens(:) = PP_SENS_io(1:ncpft)
rt_dir(:)  = RT_DIR_io(1:ncpft)
alpha1(:)  = ALPHA1_io(1:ncpft)
alpha2(:)  = ALPHA2_io(1:ncpft)
alpha3(:)  = ALPHA3_io(1:ncpft)
beta1(:)   = BETA1_io(1:ncpft)
beta2(:)   = BETA2_io(1:ncpft)
beta3(:)   = BETA3_io(1:ncpft)
r_gamma(:) = gamma_io(1:ncpft)
delta(:)   = DELTA_io(1:ncpft)
remob(:)   = REMOB_io(1:ncpft)
cfrac_s(:) = CFRAC_S_io(1:ncpft)
cfrac_r(:) = CFRAC_R_io(1:ncpft)
cfrac_l(:) = CFRAC_L_io(1:ncpft)
allo1(:)   = ALLO1_io(1:ncpft)
allo2(:)   = ALLO2_io(1:ncpft)
mu(:)             = mu_io(1:ncpft)
nu(:)             = nu_io(1:ncpft)
yield_frac(:)     = yield_frac_io(1:ncpft)
initial_carbon(:) = initial_carbon_io(1:ncpft)
initial_c_dvi(:)  = initial_c_dvi_io(1:ncpft)
sen_dvi(:)        = sen_dvi_io(1:ncpft)
t_mort(:)         = t_mort_io(1:ncpft)

!-----------------------------------------------------------------------------
! Check that all required variables were present in the namelist.
! The namelist variables were initialised to rmdi.
!-----------------------------------------------------------------------------
ERROR = 0

IF ( ANY( ABS( t_bse(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for T_BSE")
END IF
IF ( ANY( ABS( t_opt(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for T_OPT")
END IF
IF ( ANY( ABS( t_max(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for T_MAX")
END IF
IF ( ANY( ABS( tt_emr(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for TT_EMR")
END IF


IF ( ANY( ABS( crit_pp(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for CRIT_PP")
END IF
IF ( ANY( ABS( pp_sens(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for PP_SENS")
END IF
IF ( ANY( ABS( rt_dir(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for RT_DIR")
END IF
IF ( ANY( ABS( alpha1(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for ALPHA1")
END IF


IF ( ANY( ABS( alpha2(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for ALPHA2")
END IF
IF ( ANY( ABS( alpha3(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for ALPHA3")
END IF
IF ( ANY( ABS( beta1(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for BETA1")
END IF
IF ( ANY( ABS( beta2(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for BETA2")
END IF
IF ( ANY( ABS( beta3(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for BETA3")
END IF


IF ( ANY( ABS( r_gamma(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for gamma")
END IF
IF ( ANY( ABS( delta(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for DELTA")
END IF
IF ( ANY( ABS( remob(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for REMOB")
END IF
IF ( ANY( ABS( cfrac_s(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for CFRAC_S")
END IF
IF ( ANY( ABS( cfrac_r(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for CFRAC_R")
END IF


IF ( ANY( ABS( cfrac_l(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for CFRAC_L")
END IF
IF ( ANY( ABS( allo1(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for ALLO1")
END IF
IF ( ANY( ABS( allo2(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for ALLO2")
END IF


IF ( ANY( ABS( mu(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for mu")
END IF
IF ( ANY( ABS( nu(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for nu")
END IF

IF ( ANY( ABS( yield_frac(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for yield_frac")
END IF
IF ( ANY( ABS( initial_carbon(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for initial_carbon")
END IF
IF ( ANY( ABS( initial_c_dvi(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for initial_c_dvi")
END IF
IF ( ANY( ABS( sen_dvi(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for sen_dvi")
END IF
IF ( ANY( ABS( t_mort(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_cropparm", "No value for t_mort")
END IF


IF ( ERROR /= 0 ) THEN
  CALL log_fatal("init_cropparm",                                              &
                 "Variable(s) missing from namelist - see earlier " //         &
                 "error message(s)")
END IF

RETURN

END SUBROUTINE init_cropparm

! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE init_triffid(nml_dir)

USE missing_data_mod, ONLY:                                                    &
!  imported scalar parameters
     rmdi

USE io_constants, ONLY: namelist_unit

USE string_utils_mod, ONLY: to_string

USE jules_surface_types_mod, ONLY: npft, nnpft

USE jules_vegetation_mod, ONLY: l_triffid, l_phenol, l_trif_crop, l_ag_expand, &
                                l_trif_biocrop

USE trif, ONLY: crop, harvest_type, harvest_freq, ag_expand,                   &
                g_area, g_grow, g_root, g_wood, lai_max, lai_min,              &
                alloc_fast, alloc_med, alloc_slow, dpm_rpm_ratio,              &
                retran_r, retran_l, harvest_ht

USE trif_io, ONLY: jules_triffid,                                              &
                         crop_io, harvest_type_io, harvest_freq_io,            &
                         ag_expand_io, g_area_io,g_grow_io,g_root_io,          &
                         g_wood_io,lai_max_io,lai_min_io,                      &
                         alloc_fast_io,alloc_med_io,alloc_slow_io,             &
                         dpm_rpm_ratio_io,retran_l_io,retran_r_io,             &
                         harvest_ht_io

USE errormessagelength_mod, ONLY: errormessagelength

USE pftparm, ONLY: a_wl, a_ws, eta_sl, b_wl

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Initialises the TRIFFID parameters
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists

! Work variables
INTEGER :: ERROR  ! Error indicator
INTEGER :: n      ! Loop counter
CHARACTER(LEN=errormessagelength) :: iomessage
REAL    :: tmp_minlai          ! Used for trif_biocrop check
INTEGER :: days_in_yr          ! Used for trif_biocrop check

!-----------------------------------------------------------------------------


! Nothing to do if neither triffid or phenology are selected
IF ( .NOT. l_triffid .AND. .NOT. l_phenol ) RETURN

!-----------------------------------------------------------------------------
! Read namelist
!-----------------------------------------------------------------------------
CALL log_info("init_triffid", "Reading JULES_TRIFFID namelist...")

! Open the pft parameters namelist file
OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'triffid_params.nml'),       &
               STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR, &
               IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_triffid",                                               &
                 "Error opening namelist file triffid_params.nml " //          &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

READ(namelist_unit, NML = jules_triffid, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_triffid",                                               &
                 "Error reading namelist JULES_TRIFFID " //                    &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

! Close the namelist file
CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_triffid",                                               &
                 "Error closing namelist file triffid_params.nml " //          &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")


!-----------------------------------------------------------------------------
! Copy values from fixed length arrays used in namelist into allocated arrays
!-----------------------------------------------------------------------------
crop(:)    = crop_io(1:npft)
harvest_freq(:)    = harvest_freq_io(1:npft)
harvest_type(:)    = harvest_type_io(1:npft)
ag_expand(:)       = ag_expand_io(1:npft)
g_area(:)  = g_area_io(1:npft)
g_grow(:)  = g_grow_io(1:npft)
g_root(:)  = g_root_io(1:npft)
g_wood(:)  = g_wood_io(1:npft)
lai_max(:) = lai_max_io(1:npft)
lai_min(:) = lai_min_io(1:npft)
alloc_fast(:) = alloc_fast_io(1:npft)
alloc_med(:)  = alloc_med_io(1:npft)
alloc_slow(:) = alloc_slow_io(1:npft)
dpm_rpm_ratio(:) = dpm_rpm_ratio_io(1:npft)
retran_r(:)   = retran_r_io(1:npft)
retran_l(:)   = retran_l_io(1:npft)
harvest_ht(:) = harvest_ht_io(1:npft)

!-----------------------------------------------------------------------------
! Check that all required variables were present in the namelist.
! The namelist variables were initialised to rmdi.
!-----------------------------------------------------------------------------
ERROR = 0
IF ( ANY( crop(1:nnpft) < 0 ) ) THEN  ! crop was initialised to < 0
  ERROR = 1
  CALL log_error("init_triffid", "No value for crop")
END IF
IF ( ANY( ABS( g_area(1:nnpft) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_triffid", "No value for g_area")
END IF
IF ( ANY( ABS( g_grow(1:nnpft) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_triffid", "No value for g_grow")
END IF
IF ( ANY( ABS( g_root(1:nnpft) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_triffid", "No value for g_root")
END IF
IF ( ANY( ABS( g_wood(1:nnpft) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_triffid", "No value for g_wood")
END IF
IF ( ANY( ABS( lai_max(1:nnpft) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_triffid", "No value for lai_max")
END IF
IF ( ANY( ABS( lai_min(1:nnpft) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_triffid", "No value for lai_min")
END IF
IF ( ANY( ABS( alloc_fast(1:nnpft) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_triffid", "No value for alloc_fast")
END IF
IF ( ANY( ABS( alloc_med(1:nnpft) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_triffid", "No value for alloc_med")
END IF
IF ( ANY( ABS( alloc_slow(1:nnpft) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_triffid", "No value for alloc_slow")
END IF
IF ( ANY( ABS( dpm_rpm_ratio(1:nnpft) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_triffid", "No value for dpm_rpm_ratio")
END IF
IF ( ANY( ABS( retran_r(1:nnpft) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_triffid", "No value for retran_r")
END IF
IF ( ANY( ABS( retran_l(1:nnpft) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error("init_triffid", "No value for retran_l")
END IF

! Harvest variables are only required if l_trif_biocrop is true
IF ( l_trif_biocrop ) THEN

  IF ( ANY( harvest_type(1:nnpft) < 0 ) ) THEN  ! harvest_type was initialised to < 0
    ERROR = 1
    CALL log_error("init_triffid", "No value for harvest_type")
  END IF

  ! harvest_ht, harvest_freq only required if ANY(harvest_type(1:nnpft) = 2
  IF ( ANY( harvest_type(1:nnpft) == 2 ) ) THEN ! require harvest_ht and harvest_freq

    IF ( ANY( harvest_freq(1:nnpft) < 0 ) ) THEN  ! harvest_freq was initialised to < 0
      ERROR = 1
      CALL log_error("init_triffid", "No value for harvest_freq")
    END IF

    IF ( ANY( ABS( harvest_ht(1:nnpft) - rmdi ) < EPSILON(1.0) ) ) THEN
      ERROR = 1
      CALL log_error("init_triffid", "No value for harvest_ht")
    END IF
  END IF ! any (harvest_type ==2)

  ! only warn about ag_expand if l_ag_expand is true
  IF ( l_ag_expand ) THEN
    IF ( ANY( ag_expand(1:nnpft) < 0 ) ) THEN  ! ag_expand was initialised to < 0
      ERROR = 1
      CALL log_error("init_triffid", "No value for ag_expand")
    END IF
    IF ( SUM(ag_expand) < 1 ) THEN
      CALL log_warn("init_triffid", "ag_expand = 0 for all PFTs: Assisted" //  &
                    " expansion will never occur")
    END IF
  END IF ! l_ag_expand

  ! If using periodic harvest (harvest_type = 2),
  ! Check lai_min is appropriate to harvest_ht
  DO n = 1,nnpft
    IF (harvest_type(n) == 2) THEN

      ! Calculate value of lai_bal at harvest_ht
      tmp_minlai = ( harvest_ht(n) * a_ws(n) * eta_sl(n) / a_wl(n) )           &
                   ** ( 1 / ( b_wl(n) - 1 ) )

      IF (lai_min(n) > tmp_minlai) THEN
        ERROR = 1
        CALL log_error("init_triffid",                                         &
                       "lai_min is too large for harvest height for PFT " //   &
                       to_string(n) // ": Reset to < " // to_string(tmp_minlai) )

      END IF ! lai_min > tmp_minlai

    END IF ! harvest_type =2

    ! Check natural PFTs (crop=0) aren't harvested or expanded
    IF (crop(n) == 0) THEN
      ! warn if harvest is enabled (harvest_type>0)
      IF (harvest_type(n) > 0) THEN
        CALL log_warn("init_triffid", "harvest_type > 0 specified for PFT" //  &
                      to_string(n) // ": Harvesting is disabled for " //       &
                      " natural PFTs" )
      END IF

      ! warn if assisted expansion is enabled (ag_expand=1)
      IF (ag_expand(n) > 0) THEN
        CALL log_warn("init_triffid", "ag_expand = 1 specified for PFT" //     &
                      to_string(n) // ": Assisted expansion is disabled" //    &
                      " for natural PFTs" )
      END IF

    END IF ! crop(n) = 0
  END DO ! n=1,nnpft


END IF ! l_trif_biocrop



IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_triffid",                                               &
                 "Variable(s) missing from namelist - see earlier " //         &
                 "error message(s)")

RETURN

END SUBROUTINE init_triffid

! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

SUBROUTINE init_soilin_cbl(nml_dir)

USE missing_data_mod, ONLY:                                                    &
!  imported scalar parameters
     rmdi

USE io_constants, ONLY: namelist_unit

USE string_utils_mod, ONLY: to_string
USE errormessagelength_mod, ONLY: errormessagelength

USE grid_constants_mod_cbl, ONLY: nsoil_max   ! # of soil types [9]
USE cable_fields_mod,       ONLY: pars_io_data_cbl

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Initialises the non-vegetation parameters
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists
! Work variables
INTEGER :: ERROR  ! Error indicator
CHARACTER(LEN=errormessagelength) :: iomessage

CHARACTER(LEN=*), PARAMETER :: routinename='INIT_SOILIN_CABLE'

!-----------------------------------------------------------------------------

REAL ::                                                                        &
      silt_io(nsoil_max),                                                      &
      clay_io(nsoil_max),                                                      &
      sand_io(nsoil_max),                                                      &
      swilt_io(nsoil_max),                                                     &
      sfc_io(nsoil_max),                                                       &
      ssat_io(nsoil_max),                                                      &
      bch_io(nsoil_max),                                                       &
      hyds_io(nsoil_max),                                                      &
      sucs_io(nsoil_max),                                                      &
      rhosoil_io(nsoil_max),                                                   &
      css_io(nsoil_max)

!-----------------------------------------------------------------------------
! Namelist definition
!-----------------------------------------------------------------------------
NAMELIST / cable_soilparm/ silt_io, clay_io, sand_io, swilt_io,                &
          sfc_io, ssat_io, bch_io, hyds_io, sucs_io, rhosoil_io, css_io

!-----------------------------------------------------------------------------
! Initialise namelist values before reading them
!-----------------------------------------------------------------------------
silt_io(:nsoil_max)       = rmdi
clay_io(:nsoil_max)    = rmdi
sand_io(:nsoil_max)    = rmdi
swilt_io(:nsoil_max)   = rmdi
sfc_io(:nsoil_max)     = rmdi
ssat_io(:nsoil_max)    = rmdi
bch_io(:nsoil_max)     = rmdi
hyds_io(:nsoil_max)    = rmdi
sucs_io(:nsoil_max)    = rmdi
rhosoil_io(:nsoil_max) = rmdi
css_io(:nsoil_max)     = rmdi

!-----------------------------------------------------------------------------
! Read namelist
!-----------------------------------------------------------------------------
CALL log_info(routinename, "Reading CABLE_SOILPARM namelist...")

! Open the CABLE soil parameters namelist file
OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' //                              &
               'cable_soilparm.nml'),                                          &
               STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR, &
               IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(routinename,                                                  &
                 "Error opening namelist file cable_soilparm.nml " //          &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

READ(namelist_unit, NML = cable_soilparm, IOSTAT = ERROR,                      &
                 IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(routinename,                                                  &
                 "Error reading namelist CABLE_SOILPARM " //                   &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

! Close the namelist file
CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(routinename,                                                  &
                 "Error closing namelist file cable_soilparm.nml " //          &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")


!-----------------------------------------------------------------------------
! Process the namelist values
!-----------------------------------------------------------------------------
! Copy values from dedicated I/O arrays into the soil parameter data type
pars_io_data_cbl%soilin_silt(1:nsoil_max)    = silt_io(1:nsoil_max)
pars_io_data_cbl%soilin_clay(1:nsoil_max)    = clay_io(1:nsoil_max)
pars_io_data_cbl%soilin_sand(1:nsoil_max)    = sand_io(1:nsoil_max)
pars_io_data_cbl%soilin_swilt(1:nsoil_max)   = swilt_io(1:nsoil_max)
pars_io_data_cbl%soilin_sfc(1:nsoil_max)     = sfc_io(1:nsoil_max)
pars_io_data_cbl%soilin_ssat(1:nsoil_max)    = ssat_io(1:nsoil_max)
pars_io_data_cbl%soilin_bch(1:nsoil_max)     = bch_io(1:nsoil_max)
pars_io_data_cbl%soilin_hyds(1:nsoil_max)    = hyds_io(1:nsoil_max)
pars_io_data_cbl%soilin_sucs(1:nsoil_max)    = sucs_io(1:nsoil_max)
pars_io_data_cbl%soilin_rhosoil(1:nsoil_max) = rhosoil_io(1:nsoil_max)
pars_io_data_cbl%soilin_css(1:nsoil_max)     = css_io(1:nsoil_max)

!-----------------------------------------------------------------------------
! Check that all variables were present in the namelist.
! The namelist variables were initialised to rmdi.
!-----------------------------------------------------------------------------
ERROR = 0
IF ( ANY( ABS( pars_io_data_cbl%soilin_silt(1:nsoil_max) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for silt")
END IF
IF ( ANY( ABS( pars_io_data_cbl%soilin_clay(1:nsoil_max) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for clay")
END IF
IF ( ANY( ABS( pars_io_data_cbl%soilin_sand(1:nsoil_max) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for sand")
END IF
IF ( ANY( ABS( pars_io_data_cbl%soilin_swilt(1:nsoil_max) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for swilt")
END IF
IF ( ANY( ABS( pars_io_data_cbl%soilin_sfc(1:nsoil_max) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for sfc")
END IF
IF ( ANY( ABS( pars_io_data_cbl%soilin_ssat(1:nsoil_max) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for ssat")
END IF
IF ( ANY( ABS( pars_io_data_cbl%soilin_bch(1:nsoil_max) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for bch")
END IF
IF ( ANY( ABS( pars_io_data_cbl%soilin_hyds(1:nsoil_max) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for hyds")
END IF
IF ( ANY( ABS( pars_io_data_cbl%soilin_sucs(1:nsoil_max) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for sucs")
END IF
IF ( ANY( ABS( pars_io_data_cbl%soilin_rhosoil(1:nsoil_max) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for rhosoil")
END IF
IF ( ANY( ABS( pars_io_data_cbl%soilin_css(1:nsoil_max) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for css")
END IF

IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(routinename,                                                  &
                 "Variable(s) missing from namelist - see earlier " //         &
                 "error message(s)")

RETURN

END SUBROUTINE init_soilin_cbl

!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************

SUBROUTINE init_deposition_species(nml_dir)

USE deposition_species_io_mod, ONLY:                                           &
! imported scalars
  ch4_scaling_io, cuticle_o3_io, dd_ice_coeff_io,                              &
  dep_species_name_io, diffusion_coeff_io, diffusion_corr_io,                  &
  r_tundra_io, r_wet_soil_o3_io,                                               &
! imported arrays
  ch4dd_tundra_io, h2dd_c_io, h2dd_m_io, h2dd_q_io, rsurf_std_io,              &
! imported namelist
  jules_deposition_species

USE deposition_species_mod, ONLY:                                              &
! imported procedures
  check_jules_deposition_species,                                              &
! imported scalars
  ch4_scaling, cuticle_o3, r_wet_soil_o3,                                      &
! imported arrays
  ch4dd_tundra, dd_ice_coeff, dep_species_name, diffusion_coeff,               &
  diffusion_corr, h2dd_c, h2dd_m, h2dd_q, r_tundra, rsurf_std

USE jules_deposition_mod, ONLY:                                                &
! imported scalars
  l_deposition, ndry_dep_species

USE jules_surface_types_mod, ONLY:                                             &
! imported scalars
  ntype

USE io_constants, ONLY: namelist_unit
USE missing_data_mod, ONLY:                                                    &
! imported scalar parameters
  rmdi
USE string_utils_mod, ONLY: to_string
USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Initialises parameters related to atmospheric deposition.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Scalar arguments with INTENT(IN).
!-----------------------------------------------------------------------------
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists

!-----------------------------------------------------------------------------
! Parameters.
!-----------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName='INIT_DEPOSITION_SPECIES'

!-----------------------------------------------------------------------------
! Scalar variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  ERROR,                                                                       &
    ! Error indicator
  i,                                                                           &
    ! Loop counter.
  j
    ! Loop counter.

CHARACTER(LEN=errormessagelength) :: iomessage

!-----------------------------------------------------------------------------
! There's nothing to do if deposition is not selected.
!-----------------------------------------------------------------------------
IF ( .NOT. l_deposition ) RETURN

!-----------------------------------------------------------------------------
! Read deposition species namelists, one per species.
!-----------------------------------------------------------------------------
CALL log_info(RoutineName, "Reading JULES_DEPOSITION namelist...")

! Open the namelist file.
OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'jules_deposition.nml'),     &
     STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR,           &
     IOMSG = iomessage)

IF ( ERROR /= 0 ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 "Error opening namelist file jules_deposition.nml " //        &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")
END IF

!-----------------------------------------------------------------------------
! Loop over the species.
!-----------------------------------------------------------------------------
DO i = 1,ndry_dep_species

  ! Initialise namelist values before reading the next species.
  dep_species_name_io = 'unset'
  ch4_scaling_io      = rmdi
  ch4dd_tundra_io(:)  = rmdi
  cuticle_o3_io       = rmdi
  dd_ice_coeff_io(:)  = rmdi
  diffusion_coeff_io  = rmdi
  diffusion_corr_io   = rmdi
  h2dd_c_io(:)        = rmdi
  h2dd_m_io(:)        = rmdi
  h2dd_q_io(:)        = rmdi
  r_tundra_io         = rmdi
  r_wet_soil_o3_io    = rmdi
  rsurf_std_io(:)     = rmdi

  ! Read namelist for this species.
  READ( namelist_unit, NML = jules_deposition_species, IOSTAT = ERROR,         &
       IOMSG = iomessage)

  IF ( ERROR /= 0 ) THEN
    CALL log_fatal(RoutineName,                                                &
                   "Error reading namelist JULES_DEPOSITION_SPECIES " //       &
                   "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //        &
                   TRIM(iomessage) // ")")
  END IF

  !---------------------------------------------------------------------------
  ! Load values into the final variables.
  !---------------------------------------------------------------------------
  ! Variables that have a value for every species (even if not necessarily
  ! used for all species).
  dep_species_name(i) = dep_species_name_io
  dd_ice_coeff(i,:)   = dd_ice_coeff_io(:)
  diffusion_coeff(i)  = diffusion_coeff_io
  diffusion_corr(i)   = diffusion_corr_io
  r_tundra(i)         = r_tundra_io
  rsurf_std(:,i)      = rsurf_std_io(1:ntype)

  ! Deal with variables that only apply to one species.
  ! If values are given for other species they will be ignored.
  SELECT CASE ( dep_species_name(i) )

  CASE ( 'CH4' )

    ch4_scaling     = ch4_scaling_io
    ch4dd_tundra(:) = ch4dd_tundra_io(:)

  CASE ( 'H2' )
    h2dd_c(:) = h2dd_c_io(1:ntype)
    h2dd_m(:) = h2dd_m_io(1:ntype)
    h2dd_q(:) = h2dd_q_io(1:ntype)

  CASE ( 'O3' )
    cuticle_o3    = cuticle_o3_io
    r_wet_soil_o3 = r_wet_soil_o3_io

  END SELECT

END DO  !  species

!-----------------------------------------------------------------------------
! Close the namelist file
!-----------------------------------------------------------------------------
CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 "Error closing namelist file jules_deposition.nml " //        &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")
END IF

!-----------------------------------------------------------------------------
! Check the values.
!-----------------------------------------------------------------------------
CALL check_jules_deposition_species()

!-----------------------------------------------------------------------------
! Print information about the parameters.
!-----------------------------------------------------------------------------
DO i = 1,ndry_dep_species
  CALL log_info( RoutineName, "Species: " // TRIM(dep_species_name(i)) )
  CALL log_info( RoutineName, "  diffusion_coeff = " //                        &
                              to_string(diffusion_coeff(i)) )
  CALL log_info( RoutineName, "  diffusion_corr = " //                         &
                              to_string(diffusion_corr(i)) )
  CALL log_info( RoutineName, "  r_tundra = " //                               &
                              to_string(r_tundra(i)) )

  SELECT CASE ( dep_species_name(i) )
  CASE ( 'CH4' )
    CALL log_info( RoutineName, "  ch4_scaling = " //                          &
                                to_string(ch4_scaling) )
  CASE ( 'O3' )
    CALL log_info( RoutineName, "  r_wet_soil_o3 = " //                        &
                                to_string(r_wet_soil_o3) )
    CALL log_info( RoutineName, "  cuticle_o3 = " //                           &
                                to_string(cuticle_o3) )
  END SELECT

END DO

RETURN

END SUBROUTINE init_deposition_species


END MODULE init_params_mod

