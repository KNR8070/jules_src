! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Module contains variables used for reading in nvegparm data
! and initialisations

! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v8.2 programming standards.


MODULE nvegparm_io

USE max_dimensions, ONLY:                                                      &
  nnvg_max
USE missing_data_mod, ONLY: rmdi
USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------
! Set up variables to use in IO (a fixed size version of each array
! in nvegparm that we want to initialise).
!-----------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  albsnc_nvg_io(nnvg_max) = rmdi,                                              &
  albsnf_nvgu_io(nnvg_max) = rmdi,                                             &
  albsnf_nvg_io(nnvg_max) = rmdi,                                              &
  albsnf_nvgl_io(nnvg_max) = rmdi,                                             &
  catch_nvg_io(nnvg_max) = rmdi,                                               &
  gs_nvg_io(nnvg_max) = rmdi,                                                  &
  infil_nvg_io(nnvg_max) = rmdi,                                               &
  z0_nvg_io(nnvg_max) = rmdi,                                                  &
  ch_nvg_io(nnvg_max) = rmdi,                                                  &
  vf_nvg_io(nnvg_max) = rmdi,                                                  &
  emis_nvg_io(nnvg_max) = rmdi,                                                &
  z0hm_nvg_io(nnvg_max) = rmdi,                                                &
  z0hm_classic_nvg_io(nnvg_max) = rmdi


!-----------------------------------------------------------------------
! Set up a namelist for reading and writing these arrays
!-----------------------------------------------------------------------
NAMELIST  / jules_nvegparm/                                                    &
                          albsnc_nvg_io,albsnf_nvgu_io,                        &
                          albsnf_nvg_io, albsnf_nvgl_io,                       &
                          catch_nvg_io,gs_nvg_io,infil_nvg_io,                 &
                          z0_nvg_io,ch_nvg_io,vf_nvg_io,                       &
                          emis_nvg_io,z0hm_nvg_io,                             &
                          z0hm_classic_nvg_io

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='NVEGPARM_IO'

CONTAINS

SUBROUTINE print_nlist_jules_nvegparm()
USE jules_print_mgr, ONLY: jules_print
IMPLICIT NONE
CHARACTER(LEN=50000) :: lineBuffer

CALL jules_print('nvegparm_io',                                                &
    'Contents of namelist jules_nvegparm')

WRITE(lineBuffer,*)' albsnc_nvg_io = ',albsnc_nvg_io
CALL jules_print('nvegparm_io',lineBuffer)
WRITE(lineBuffer,*)' albsnf_nvg_io = ',albsnf_nvg_io
CALL jules_print('nvegparm_io',lineBuffer)
WRITE(lineBuffer,*)' catch_nvg_io = ',catch_nvg_io
CALL jules_print('nvegparm_io',lineBuffer)
WRITE(lineBuffer,*)' gs_nvg_io = ',gs_nvg_io
CALL jules_print('nvegparm_io',lineBuffer)
WRITE(lineBuffer,*)' infil_nvg_io = ',infil_nvg_io
CALL jules_print('nvegparm_io',lineBuffer)
WRITE(lineBuffer,*)' z0_nvg_io = ',z0_nvg_io
CALL jules_print('nvegparm_io',lineBuffer)
WRITE(lineBuffer,*)' ch_nvg_io = ',ch_nvg_io
CALL jules_print('nvegparm_io',lineBuffer)
WRITE(lineBuffer,*)' vf_nvg_io = ',vf_nvg_io
CALL jules_print('nvegparm_io',lineBuffer)
WRITE(lineBuffer,*)' emis_nvg_io = ',emis_nvg_io
CALL jules_print('nvegparm_io',lineBuffer)
WRITE(lineBuffer,*)' z0hm_nvg_io = ',z0hm_nvg_io
CALL jules_print('nvegparm_io',lineBuffer)

CALL jules_print('nvegparm_io',                                                &
    '- - - - - - end of namelist - - - - - -')

END SUBROUTINE print_nlist_jules_nvegparm


SUBROUTINE init_nvegparm_allocated()

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

USE nvegparm, ONLY:                                                            &
! namelist variables:
  albsnc_nvg,   albsnf_nvgu,  albsnf_nvg,                                      &
  albsnf_nvgl,  catch_nvg,    gs_nvg,                                          &
  infil_nvg,    z0_nvg,       ch_nvg,                                          &
  vf_nvg,       emis_nvg,     l_z0_nvg

USE c_z0h_z0m, ONLY: z0h_z0m,  z0h_z0m_classic

USE jules_surface_types_mod, ONLY: npft, nnvg, urban_canyon, urban_roof, soil
USE jules_surface_mod,       ONLY: l_vary_z0m_soil
USE jules_urban_mod,         ONLY: l_moruses

IMPLICIT NONE

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='INIT_NVEGPARM_ALLOCATED'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

albsnc_nvg(:)                       = albsnc_nvg_io(1:nnvg)
albsnf_nvgu(:)                      = albsnf_nvgu_io(1:nnvg)
albsnf_nvg(:)                       = albsnf_nvg_io(1:nnvg)
albsnf_nvgl(:)                      = albsnf_nvgl_io(1:nnvg)
catch_nvg(:)                        = catch_nvg_io(1:nnvg)
gs_nvg(:)                           = gs_nvg_io(1:nnvg)
infil_nvg(:)                        = infil_nvg_io(1:nnvg)
z0_nvg(:)                           = z0_nvg_io(1:nnvg)
ch_nvg(:)                           = ch_nvg_io(1:nnvg)
vf_nvg(:)                           = vf_nvg_io(1:nnvg)
emis_nvg(:)                         = emis_nvg_io(1:nnvg)
z0h_z0m(npft+1:npft + nnvg)         = z0hm_nvg_io(1:nnvg)
z0h_z0m_classic(npft+1:npft + nnvg) = z0hm_classic_nvg_io(1:nnvg)

IF ( l_vary_z0m_soil ) l_z0_nvg( soil - npft ) = .FALSE.
IF ( l_moruses ) THEN
  l_z0_nvg( urban_canyon - npft ) = .FALSE.
  l_z0_nvg( urban_roof - npft )   = .FALSE.
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE init_nvegparm_allocated

END MODULE nvegparm_io
