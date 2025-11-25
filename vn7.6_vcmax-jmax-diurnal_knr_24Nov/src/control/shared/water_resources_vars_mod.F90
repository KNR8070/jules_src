!******************************COPYRIGHT**************************************
! (c) UK Centre for Ecology & Hydrology.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************

!-----------------------------------------------------------------------------
! Description:
!   Contains variables and field types for water resource modelling.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!-----------------------------------------------------------------------------

MODULE water_resources_vars_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!------------------------------------------------------------------------------
! Implementation for field variables
! Each variable is declared in both the 'data' TYPE and the 'pointer' type.
! Instances of these types are declared at at high level as required
! This is to facilitate advanced memory management features, which are
! generally not visible in the science code.
! Checklist for adding a new variable:
! -add to data_type
! -add to pointer_type
! -add to the allocate routine, passing in any new dimension sizes required
!  by argument (not via USE statement)
! -add to the deallocate routine
! -add to the assoc and nullify routines
!------------------------------------------------------------------------------

TYPE :: water_resources_data_type

  INTEGER, ALLOCATABLE ::                                                      &
    priority_order(:,:)
      ! Priorities of water demands at each gridpoint, in order of decreasing
      ! priority. Values are the index in multi-sector arrays.
      ! e.g. priority_order(l,1) = 3 indicates that the first priority use is
      !      in slice 3 of multi-sector arrays.

  REAL(KIND=real_jlslsm), ALLOCATABLE ::                                       &
    !---------------------------------------------------------------------------
    ! Ancillary fields.
    !---------------------------------------------------------------------------
    conveyance_loss(:),                                                        &
      ! Fraction of water that is lost during conveyance from source to user.
    irrig_eff(:),                                                              &
      ! Irrigation efficiency i.e. the fraction of the water withdrawn for
      ! irrigation that is demanded by the crop scheme.
    sfc_water_frac(:),                                                         &
      ! Target for the fraction of demand that will be met from surface water
      ! (as opposed to groundwater).
    !---------------------------------------------------------------------------
    ! Demands that can be prescribed.
    !---------------------------------------------------------------------------
    demand_rate_domestic(:),                                                   &
      ! Demand for water for domestic use (kg s-1).
    demand_rate_industry(:),                                                   &
      ! Demand for water for industrial use (kg s-1).
    demand_rate_livestock(:),                                                  &
      ! Demand for water for livestock (kg s-1).
    demand_rate_transfers(:),                                                  &
      ! Demand for water for (explicit) transfers (kg s-1).
    !---------------------------------------------------------------------------
    ! Other variables.
    !---------------------------------------------------------------------------
    demand_accum(:,:)
      ! Demands for water accumulated over the water resource timestep (kg).
      ! Note that in general this should be written to restart files (dumps)
      ! but this is not done yet.

END TYPE

!##############################################################################

TYPE :: water_resources_type

  INTEGER, POINTER :: priority_order(:,:)
  REAL(KIND=real_jlslsm), POINTER :: conveyance_loss(:)
  REAL(KIND=real_jlslsm), POINTER :: irrig_eff(:)
  REAL(KIND=real_jlslsm), POINTER :: sfc_water_frac(:)
  REAL(KIND=real_jlslsm), POINTER :: demand_rate_domestic(:)
  REAL(KIND=real_jlslsm), POINTER :: demand_rate_industry(:)
  REAL(KIND=real_jlslsm), POINTER :: demand_rate_livestock(:)
  REAL(KIND=real_jlslsm), POINTER :: demand_rate_transfers(:)
  REAL(KIND=real_jlslsm), POINTER :: demand_accum(:,:)

END TYPE

!##############################################################################

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='WATER_RESOURCES_VARS_MOD'

CONTAINS

!##############################################################################

SUBROUTINE water_resources_alloc( land_pts, nwater_use, l_water_domestic,      &
             l_water_industry, l_water_irrigation, l_water_livestock,          &
             l_water_resources, l_water_transfers, water_resources_data )

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! Number of land points (current task).
  nwater_use
    ! Number of water resource sectors that are considered.

LOGICAL, INTENT(IN) ::                                                         &
  l_water_domestic,                                                            &
    ! Switch to consider demand for water for domestic use.
  l_water_industry,                                                            &
    ! Switch to consider demand for water for industrial use.
  l_water_irrigation,                                                          &
    ! Switch to consider demand for water for irrigation.
  l_water_livestock,                                                           &
    ! Switch to consider demand for water for livestock.
  l_water_resources,                                                           &
    ! Switch to select water resource management modelling.
  l_water_transfers
    ! Switch to consider (explicit) water transfers.

!------------------------------------------------------------------------------
! Arguments with INTENT(IN OUT)
!------------------------------------------------------------------------------
TYPE(water_resources_data_type), INTENT(IN OUT) :: water_resources_data

!------------------------------------------------------------------------------
! Local variables
!------------------------------------------------------------------------------
INTEGER :: land_pts_dim, nwater_use_dim
  ! Sizes used when allocating arrays.

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='WATER_RESOURCES_ALLOC'

!End of header
!------------------------------------------------------------------------------

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Arrays are always allocated, but with minimal size if the science is not
! selected. Decide on sizes.
!-----------------------------------------------------------------------------
IF ( l_water_resources ) THEN
  land_pts_dim   = land_pts
  nwater_use_dim = nwater_use
ELSE
  land_pts_dim   = 1
  nwater_use_dim = 1
END IF

!-----------------------------------------------------------------------------
! Priority order.
!-----------------------------------------------------------------------------
ALLOCATE( water_resources_data%priority_order(land_pts_dim,nwater_use_dim) )

!-----------------------------------------------------------------------------
! Ancillary fields.
!-----------------------------------------------------------------------------
ALLOCATE( water_resources_data%conveyance_loss(land_pts_dim) )
IF ( l_water_irrigation ) THEN
  ALLOCATE( water_resources_data%irrig_eff(land_pts_dim) )
ELSE
  ALLOCATE( water_resources_data%irrig_eff(1) )
END IF
ALLOCATE( water_resources_data%sfc_water_frac(land_pts_dim) )

!-----------------------------------------------------------------------------
! Individual demands (which can be prescibed).
! We allocate a minimum size if a sector is not being used.
!-----------------------------------------------------------------------------
IF ( l_water_domestic ) THEN
  ALLOCATE( water_resources_data%demand_rate_domestic(land_pts_dim) )
ELSE
  ALLOCATE( water_resources_data%demand_rate_domestic(1) )
END IF

IF ( l_water_industry ) THEN
  ALLOCATE( water_resources_data%demand_rate_industry(land_pts_dim) )
ELSE
  ALLOCATE( water_resources_data%demand_rate_industry(1) )
END IF

IF ( l_water_livestock ) THEN
  ALLOCATE( water_resources_data%demand_rate_livestock(land_pts_dim) )
ELSE
  ALLOCATE( water_resources_data%demand_rate_livestock(1) )
END IF

IF ( l_water_transfers ) THEN
  ALLOCATE( water_resources_data%demand_rate_transfers(land_pts_dim) )
ELSE
  ALLOCATE( water_resources_data%demand_rate_transfers(1) )
END IF

!-----------------------------------------------------------------------------
! Accumulated demands.
!-----------------------------------------------------------------------------
ALLOCATE( water_resources_data%demand_accum(land_pts_dim,nwater_use_dim) )

!-----------------------------------------------------------------------------
! Initialise arrays.
!-----------------------------------------------------------------------------
water_resources_data%priority_order(:,:)      = 0
water_resources_data%conveyance_loss(:)       = 0.0
water_resources_data%irrig_eff(:)             = 0.0
water_resources_data%sfc_water_frac(:)        = 0.0
water_resources_data%demand_rate_domestic(:)  = 0.0
water_resources_data%demand_rate_industry(:)  = 0.0
water_resources_data%demand_rate_livestock(:) = 0.0
water_resources_data%demand_rate_transfers(:) = 0.0
water_resources_data%demand_accum(:,:)        = 0.0

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE water_resources_alloc

!##############################################################################

SUBROUTINE water_resources_dealloc(water_resources_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!------------------------------------------------------------------------------
! Arguments with INTENT(IN OUT)
!------------------------------------------------------------------------------
TYPE(water_resources_data_type), INTENT(IN OUT) :: water_resources_data

!------------------------------------------------------------------------------
! Local variables
!------------------------------------------------------------------------------

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='WATER_RESOURCES_DEALLOC'

!End of header
!------------------------------------------------------------------------------

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

DEALLOCATE( water_resources_data%priority_order )
DEALLOCATE( water_resources_data%conveyance_loss )
DEALLOCATE( water_resources_data%irrig_eff )
DEALLOCATE( water_resources_data%sfc_water_frac )
DEALLOCATE( water_resources_data%demand_rate_domestic )
DEALLOCATE( water_resources_data%demand_rate_industry )
DEALLOCATE( water_resources_data%demand_rate_livestock )
DEALLOCATE( water_resources_data%demand_rate_transfers )
DEALLOCATE( water_resources_data%demand_accum )

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE water_resources_dealloc

!##############################################################################

SUBROUTINE water_resources_assoc(water_resources,water_resources_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!------------------------------------------------------------------------------
! Arguments with INTENT(IN)
!------------------------------------------------------------------------------
TYPE(water_resources_data_type), TARGET, INTENT(IN) :: water_resources_data
  ! Instance of the data type we are associating to.

!------------------------------------------------------------------------------
! Arguments with INTENT(IN OUT)
!------------------------------------------------------------------------------
TYPE(water_resources_type), INTENT(IN OUT) :: water_resources
  ! Instance of the pointer type we are associating.

!------------------------------------------------------------------------------
! Local variables
!------------------------------------------------------------------------------

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='WATER_RESOURCES_ASSOC'

!End of header
!------------------------------------------------------------------------------

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL water_resources_nullify(water_resources)

water_resources%priority_order  => water_resources_data%priority_order
water_resources%conveyance_loss => water_resources_data%conveyance_loss
water_resources%irrig_eff       => water_resources_data%irrig_eff
water_resources%sfc_water_frac  => water_resources_data%sfc_water_frac
water_resources%demand_rate_domestic                                           &
                => water_resources_data%demand_rate_domestic
water_resources%demand_rate_industry                                           &
                => water_resources_data%demand_rate_industry
water_resources%demand_rate_livestock                                          &
                => water_resources_data%demand_rate_livestock
water_resources%demand_rate_transfers                                          &
                => water_resources_data%demand_rate_transfers
water_resources%demand_accum    => water_resources_data%demand_accum

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE water_resources_assoc

!##############################################################################

SUBROUTINE water_resources_nullify(water_resources)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments

TYPE(water_resources_type), INTENT(IN OUT) :: water_resources
  ! Instance of the pointer type we are associating.

!Local variables

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='WATER_RESOURCES_NULLIFY'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

NULLIFY( water_resources%priority_order )
NULLIFY( water_resources%conveyance_loss )
NULLIFY( water_resources%irrig_eff )
NULLIFY( water_resources%sfc_water_frac )
NULLIFY( water_resources%demand_rate_domestic )
NULLIFY( water_resources%demand_rate_industry )
NULLIFY( water_resources%demand_rate_livestock )
NULLIFY( water_resources%demand_rate_transfers )
NULLIFY( water_resources%demand_accum )

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE water_resources_nullify

END MODULE water_resources_vars_mod
