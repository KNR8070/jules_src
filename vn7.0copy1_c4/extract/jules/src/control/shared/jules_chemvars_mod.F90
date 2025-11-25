! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!

! Module containing jules_chem diagnostics

MODULE jules_chemvars_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

TYPE :: chemvars_data_type
  ! Contains LOGICALs, INTEGERs and REALs
  ! BVOC diagnostics
  REAL(KIND=real_jlslsm), ALLOCATABLE :: isoprene_gb(:)
        ! Gridbox mean isoprene emission flux (kgC/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: isoprene_pft(:,:)
        ! Isoprene emission flux on PFTs (kgC/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: terpene_gb(:)
        ! Gridbox mean (mono-)terpene emission flux (kgC/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: terpene_pft(:,:)
        ! (Mono-)Terpene emission flux on PFTs (kgC/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: methanol_gb(:)
        ! Gridbox mean methanol emission flux (kgC/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: methanol_pft(:,:)
        ! Methanol emission flux on PFTs (kgC/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: acetone_gb(:)
        ! Gridbox mean acetone emission flux (kgC/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: acetone_pft(:,:)
        ! Acetone emission flux on PFTs (kgC/m2/s)
  ! Ozone forcing
  REAL(KIND=real_jlslsm), ALLOCATABLE :: o3_gb(:)
        ! Surface ozone concentration (ppb).
  ! Ozone diagnostics
  REAL(KIND=real_jlslsm), ALLOCATABLE :: flux_o3_pft(:,:)
        ! Flux of O3 to stomata (nmol O3/m2/s).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fo3_pft(:,:)
        ! Ozone exposure factor.


END TYPE

!===============================================================================
TYPE :: chemvars_type
  ! Contains LOGICALs, INTEGERs and REALs
  REAL(KIND=real_jlslsm), POINTER :: isoprene_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: isoprene_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: terpene_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: terpene_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: methanol_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: methanol_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: acetone_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: acetone_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: o3_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: flux_o3_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: fo3_pft(:,:)
END TYPE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='JULES_CHEMVARS_MOD'

CONTAINS

SUBROUTINE chemvars_alloc(land_pts,npft, chemvars_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: land_pts,npft
TYPE(chemvars_data_type), INTENT(IN OUT) :: chemvars_data

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='CHEMVARS_ALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)


! ==== from bvoc_vars module ====
ALLOCATE(chemvars_data%isoprene_gb(land_pts))
ALLOCATE(chemvars_data%isoprene_pft(land_pts,npft))
ALLOCATE(chemvars_data%terpene_gb(land_pts))
ALLOCATE(chemvars_data%terpene_pft(land_pts,npft))
ALLOCATE(chemvars_data%methanol_gb(land_pts))
ALLOCATE(chemvars_data%methanol_pft(land_pts,npft))
ALLOCATE(chemvars_data%acetone_gb(land_pts))
ALLOCATE(chemvars_data%acetone_pft(land_pts,npft))
! ==== from ozone_vars module ====
ALLOCATE(chemvars_data%o3_gb(land_pts))
ALLOCATE(chemvars_data%flux_o3_pft(land_pts,npft))
ALLOCATE(chemvars_data%fo3_pft(land_pts,npft))

!  ==== from bvoc_vars module ====
chemvars_data%isoprene_gb(:)    = 0.0
chemvars_data%isoprene_pft(:,:) = 0.0
chemvars_data%terpene_gb(:)     = 0.0
chemvars_data%terpene_pft(:,:)  = 0.0
chemvars_data%methanol_gb(:)    = 0.0
chemvars_data%methanol_pft(:,:) = 0.0
chemvars_data%acetone_gb(:)     = 0.0
chemvars_data%acetone_pft(:,:)  = 0.0
!  ==== from ozone_vars module ====
chemvars_data%o3_gb(:)          = 0.0
chemvars_data%flux_o3_pft(:,:)  = 0.0
chemvars_data%fo3_pft(:,:)      = 0.0

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE chemvars_alloc


!===============================================================================
SUBROUTINE chemvars_dealloc(chemvars_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments

TYPE(chemvars_data_type), INTENT(IN OUT) :: chemvars_data

!Local variables

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='CHEMVARS_DEALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

DEALLOCATE(chemvars_data%isoprene_gb)
DEALLOCATE(chemvars_data%isoprene_pft)
DEALLOCATE(chemvars_data%terpene_gb)
DEALLOCATE(chemvars_data%terpene_pft)
DEALLOCATE(chemvars_data%methanol_gb)
DEALLOCATE(chemvars_data%methanol_pft)
DEALLOCATE(chemvars_data%acetone_gb)
DEALLOCATE(chemvars_data%acetone_pft)
DEALLOCATE(chemvars_data%o3_gb)
DEALLOCATE(chemvars_data%flux_o3_pft)
DEALLOCATE(chemvars_data%fo3_pft)


IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE chemvars_dealloc

!===============================================================================
SUBROUTINE chemvars_assoc(chemvars,chemvars_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments

TYPE(chemvars_data_type), TARGET, INTENT(IN OUT) :: chemvars_data
! Instance of the data type we are associtating to
TYPE(chemvars_type), INTENT(IN OUT) :: chemvars
  ! Instance of the pointer type we are associating

!Local variables

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='CHEMVARS_ASSOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL chemvars_nullify(chemvars)

chemvars%isoprene_gb  => chemvars_data%isoprene_gb
chemvars%isoprene_pft => chemvars_data%isoprene_pft
chemvars%terpene_gb   => chemvars_data%terpene_gb
chemvars%terpene_pft  => chemvars_data%terpene_pft
chemvars%methanol_gb  => chemvars_data%methanol_gb
chemvars%methanol_pft => chemvars_data%methanol_pft
chemvars%acetone_gb   => chemvars_data%acetone_gb
chemvars%acetone_pft  => chemvars_data%acetone_pft
chemvars%o3_gb        => chemvars_data%o3_gb
chemvars%flux_o3_pft  => chemvars_data%flux_o3_pft
chemvars%fo3_pft      => chemvars_data%fo3_pft

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE chemvars_assoc

!===============================================================================
SUBROUTINE chemvars_nullify(chemvars)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments

TYPE(chemvars_type), INTENT(IN OUT) :: chemvars
  ! Instance of the pointer type we are associating

!Local variables

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='CHEMVARS_NULLIFY'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

NULLIFY(chemvars%isoprene_gb)
NULLIFY(chemvars%isoprene_pft)
NULLIFY(chemvars%terpene_gb)
NULLIFY(chemvars%terpene_pft)
NULLIFY(chemvars%methanol_gb)
NULLIFY(chemvars%methanol_pft)
NULLIFY(chemvars%acetone_gb)
NULLIFY(chemvars%acetone_pft)
NULLIFY(chemvars%o3_gb)
NULLIFY(chemvars%flux_o3_pft)
NULLIFY(chemvars%fo3_pft)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

RETURN
END SUBROUTINE chemvars_nullify

END MODULE jules_chemvars_mod
