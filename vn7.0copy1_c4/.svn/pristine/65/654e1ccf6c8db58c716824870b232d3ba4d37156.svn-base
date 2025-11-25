#if !defined(UM_JULES)
MODULE allocate_river_arrays_mod

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
   ModuleName='ALLOCATE_RIVER_ARRAYS_MOD'

CONTAINS
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! Subroutine ALLOCATE_RIVER_ARRAYS
!
! Description: Routine that allocates memory used by standalone Rivers
!
! This routine may be better written using completely separate routines as it
! is allocating a small amount of memory that is not required and increasing
! dependencies. Please see individual comments.
!
! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v8.2 programming standards.
!
!   Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt
!   This file belongs in section: Technical

SUBROUTINE allocate_river_arrays(psparms_data,ainfo_data, progs_data,          &
                                 coastal_data,                                 &
                                 fluxes_data,                                  &
                                 rivers_data)

USE jules_vegetation_mod,      ONLY: l_triffid, l_phenol, l_use_pft_psi,       &
                                     l_acclim
USE jules_soil_mod,            ONLY: l_bedrock, ns_deep
USE jules_soil_biogeochem_mod, ONLY: soil_model_ecosse, soil_bgc_model,        &
                                     l_layeredc, dim_ch4layer
USE veg3_parm_mod,             ONLY: l_veg3

!Variables- dimensions
USE jules_snow_mod,            ONLY: nsmax
USE jules_surface_types_mod,   ONLY: npft, ntype, nnpft
USE theta_field_sizes,         ONLY: t_i_length, t_j_length,                   &
                                     u_i_length, u_j_length,                   &
                                     v_i_length, v_j_length
USE ancil_info,                ONLY: nsoilt, dim_cs1, dim_cslayer, land_pts,   &
                                     nsurft, nmasst
USE jules_sea_seaice_mod,      ONLY: nice, nice_use
USE jules_soil_mod,            ONLY: sm_levels

!Subroutines
USE fluxes_mod,                ONLY: fluxes_alloc
USE prognostics,               ONLY: prognostics_alloc
USE p_s_parms,                 ONLY: psparms_alloc
USE ancil_info,                ONLY: ancil_info_alloc
USE jules_rivers_mod,          ONLY: jules_rivers_alloc
USE coastal,                   ONLY: coastal_alloc

!TYPE definitions
USE p_s_parms,         ONLY: psparms_data_type
USE ancil_info,        ONLY: ainfo_data_type
USE prognostics,       ONLY: progs_data_type
USE coastal,           ONLY: coastal_data_type
USE fluxes_mod,        ONLY: fluxes_data_type
USE jules_rivers_mod,  ONLY: rivers_data_type

USE parkind1,      ONLY: jprb, jpim
USE yomhook,       ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments

!TYPES containing field data (IN OUT)
TYPE(psparms_data_type), INTENT(IN OUT) :: psparms_data
TYPE(ainfo_data_type),   INTENT(IN OUT) :: ainfo_data
TYPE(progs_data_type),   INTENT(IN OUT) :: progs_data
TYPE(coastal_data_type), INTENT(IN OUT) :: coastal_data
TYPE(fluxes_data_type),  INTENT(IN OUT) :: fluxes_data
TYPE(rivers_data_type),  INTENT(IN OUT) :: rivers_data

!Local variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='ALLOCATE_RIVER_ARRAYS'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!Any dimension sizes should be set before we get here. Some special cases for
!UM mode can be found in surf_couple_allocate.

! prognostics_alloc is only needed for progs%smcl_soilt as far as currently
! aware. This is not actually needed as only used by rivers_um_trip, but as it
! is part of the soil moisture correction for water conservation it may be
! needed in future. Could possibly allocate this separately though?
CALL prognostics_alloc(land_pts, t_i_length, t_j_length,                       &
                      nsurft, npft, nsoilt, sm_levels, ns_deep, nsmax,         &
                      dim_cslayer, dim_cs1, dim_ch4layer,                      &
                      nice, nice_use, soil_bgc_model, soil_model_ecosse,       &
                      l_layeredc, l_triffid, l_phenol, l_bedrock, l_veg3,      &
                      nmasst, nnpft, l_acclim, progs_data)

! This is where sub_surf_roff_gb and surf_roff_gb are allocated, but could
! allocate this separately too.
CALL fluxes_alloc(land_pts, t_i_length, t_j_length,                            &
                  nsurft, npft, nsoilt, sm_levels,                             &
                  nice, nice_use,                                              &
                  fluxes_data)

! Similarly psparms%smvcst_soilt, psparms%smvcwt_soilt, psparms%sthu_soilt
! used for soil moisture correction for water conservation
CALL psparms_alloc(land_pts, t_i_length, t_j_length,                           &
                   nsoilt, sm_levels, dim_cslayer, nsurft, npft,               &
                   soil_bgc_model, soil_model_ecosse,  l_use_pft_psi,          &
                   psparms_data)

CALL ancil_info_alloc(land_pts, t_i_length, t_j_length,                        &
                      nice, nsoilt, ntype,                                     &
                      ainfo_data)

CALL jules_rivers_alloc(land_pts, rivers_data)

CALL coastal_alloc(land_pts, t_i_length, t_j_length,                           &
                   u_i_length, u_j_length,                                     &
                   v_i_length, v_j_length,                                     &
                   nice_use, nice, coastal_data)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName, zhook_out, zhook_handle)
RETURN
END SUBROUTINE allocate_river_arrays

END MODULE allocate_river_arrays_mod
#endif
