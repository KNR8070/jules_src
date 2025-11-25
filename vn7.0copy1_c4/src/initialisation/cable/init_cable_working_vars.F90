#if !defined(UM_JULES)

MODULE init_cable_work_mod

!------------------------------------------------------------------------------
! Description:
!
!   Main driver to dec/alloc/initialize CABLE prognostic variables
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in CABLE SCIENCE
!------------------------------------------------------------------------------

IMPLICIT NONE

PUBLIC :: init_cable_work
PRIVATE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='INIT_CABLE_WORK_MOD'

CONTAINS

SUBROUTINE init_cable_work( land_pts, nsurft, sm_levels, lsm_id, cable,        &
                            work_cbl, work_cbl_data )

USE work_vars_mod_cbl,           ONLY: work_vars_type,                         &
                                       work_vars_data_type,                    &
                                       alloc_work_vars_cbl,                    &
                                       assoc_work_vars_cbl

IMPLICIT NONE

INTEGER, INTENT(IN) :: land_pts, nsurft,sm_levels
INTEGER, INTENT(IN) :: lsm_id, cable
TYPE(work_vars_data_type), INTENT(IN OUT) :: work_cbl_data
TYPE(work_vars_type),      INTENT(IN OUT) :: work_cbl

CALL alloc_work_vars_cbl(land_pts, nsurft, sm_levels, lsm_id, cable,           &
                         work_cbl_data )
CALL assoc_work_vars_cbl(work_cbl, work_cbl_data )

RETURN

END SUBROUTINE init_cable_work

END MODULE init_cable_work_mod
#endif
