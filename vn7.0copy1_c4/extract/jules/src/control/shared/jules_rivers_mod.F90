! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE jules_rivers_mod

!-----------------------------------------------------------------------------
! Description:
!   Contains river routing options and a namelist for setting them
!   This currently holds both "physics" and control variables
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE missing_data_mod, ONLY: imdi, rmdi

USE um_types, ONLY: real_jlslsm

USE jules_irrig_mod, ONLY: l_irrig_limit

USE ereport_mod, ONLY: ereport

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Module constants
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Scalar parameters.
INTEGER, PARAMETER ::                                                          &
   rivers_um_trip = 1
                            ! value of i_river_vn indicating that the UM
                            ! TRIP linear model is to be used.
INTEGER, PARAMETER ::                                                          &
   rivers_rfm = 2
                            ! value of i_river_vn indicating that the RFM
                            ! Kinematic Wave model is to be used.
INTEGER, PARAMETER ::                                                          &
   rivers_trip = 3
                            ! value of i_river_vn indicating that the TRIP
                            ! linear model is to be used.

! The values of river_mouth and inland_drainage should differ and should not
! be in the range 1:SIZE(flow_dir_delta,1), i.e. not in the range of values
! used to encode the flow directions.
INTEGER, PARAMETER :: river_mouth = 9
  ! The value of the flow direction field that indicates a river mouth.
INTEGER, PARAMETER :: inland_drainage = 10
  ! The value of the flow direction field that indicates an inland drainage
  ! point, i.e. an endorheic catchment.
INTEGER, PARAMETER :: sea = 0
    ! Value used to indicate that a river point is sea, for the UM.
    ! This should be <1, should not equal -1*river_mouth nor
    ! -1*inland_drainage, nor should it be in the range
    ! -1:-SIZE(flow_dir_delta,1).

INTEGER, PARAMETER :: rfm_land = 2
  ! The value of rfm_land_rp indicating a land point.
INTEGER, PARAMETER :: rfm_river = 1
  ! The value of rfm_land_rp indicating a river point.
INTEGER, PARAMETER :: rfm_sea = 0
  ! The value of rfm_land_rp indicating a sea point.
  ! This is only used in the UM.

!-----------------------------------------------------------------------------
! Array parameters.

! The values in the flow direction ancillary layer must be consistent with the
! size and values of flow_dir_delta, e.g. a flow direction of 1 is used with
! flow_dir_delta(1,:) to indicate flow to the north.

INTEGER,PARAMETER :: flow_dir_delta(8,2) = RESHAPE( [                          &
!       1    2     3    4     5    6      7   8
!       N    NE    E    SE    S    SW     W   NW
!  x offsets:
        0,   1,    1,    1,   0,   -1,   -1,  -1,                              &
!  y offsets:
        1,   1,    0,   -1,  -1,   -1,    0,   1                               &
         ], [ 8,2 ] )

!      Components of displacement (number of gridboxes) in x and
!      y directions to the immediately downstream gridbox.
!      The elements in the 2nd dimension are x and y respectively.
!             e.g. flow_dir_delta(1,1:2)=(/0,1/)
!             flow_dir_delta(1,2)=1 means 1 gridbox in the N (y) direction
!             flow_dir_delta(1,1)=0 means no displacement in the W-E (x)
!                                 direction

!-----------------------------------------------------------------------------
! Items set in namelist
!-----------------------------------------------------------------------------

LOGICAL ::                                                                     &
   l_rivers = .FALSE.                                                          &
                            ! Switch for runoff routing
   ,l_inland = .FALSE.
                            ! Control rerouting of inland basin water

INTEGER ::                                                                     &
   nstep_rivers = imdi                                                         &
                            ! Timestep for runoff routing
                            ! (number of model timesteps)
   ,i_river_vn  = imdi                                                         &
                            ! integer representation of river routing type
                            !   1 == 'um_trip'
                            !   2 == 'rfm'
                            !   3 == 'trip'
   ,a_thresh = 1
                            ! threshold area (TRIP: pixels; RFM: km)

REAL(KIND=real_jlslsm) ::                                                      &
   rivers_meander = 1.4                                                        &
                            ! meander ratio for rivers - the ratio of
                            ! actual river length to the calculated length
   ,rivers_speed = 0.4                                                         &
                            ! flow speed for rivers (m s-1)
   ,runoff_factor = 1.0                                                        &
                            ! runoff volume factor
   ,cland  = 0.2                                                               &
                            ! land wave speed (m/s)
   ,criver = 0.62                                                              &
                            ! subsurf river wave speed (m/s)
   ,cbland  = 0.1                                                              &
                            ! subsurf land wave speed (m/s)
   ,cbriver = 0.15                                                             &
                            ! subsurf river wave speed (m/s)
   ,retl = 0.0                                                                 &
                            ! return flow (land squares) (<1)
   ,retr = 0.005
                            ! return flow (river squares) (<1)

!-----------------------------------------------------------------------------
! Rivers parameters
!-----------------------------------------------------------------------------

! Scalar variables (general)
INTEGER ::                                                                     &
   np_rivers = 0                                                               &
                            ! number of points in the rivers grid at which
                            ! routing is calculated
   ,nx_rivers = 0                                                              &
                            ! row length for rivers grid
   ,ny_rivers = 0
                            ! column length for rivers grid

!-----------------------------------------------------------------------------
! Definition of the river routing grid
!-----------------------------------------------------------------------------

REAL(KIND=real_jlslsm) ::                                                      &
   rivers_dlat  = rmdi                                                         &
                            ! size of gridbox of (regular) rivers grid
                            ! in latitude (degrees)
   ,rivers_dlon = rmdi                                                         &
                            ! size of gridbox of (regular) rivers grid
                            ! in longitude (degrees)
   ,rivers_lat1 = rmdi                                                         &
                            ! latitude of southernmost row of gridpoints
                            ! on a regular rivers grid (degrees)
   ,rivers_lon1 = rmdi                                                         &
                            !  longitude of westernmost (first) column of
                            ! gridpoints on a regular rivers grid (degrees)
   ,rivers_dx   = rmdi
                            ! size of gridbox of rivers grid in m (for
                            ! non-regular lat/lon grids)

INTEGER ::                                                                     &
   nx_grid      = imdi                                                         &
                            ! row length of full land model grid
                            ! (only needed for river routing)
   ,ny_grid     = imdi                                                         &
                            ! column length of model grid
                            ! (only needed for river routing)
   ,nseqmax     = imdi
                            ! maximum value of routing grid sequence

REAL(KIND=real_jlslsm) ::                                                      &
   reg_lon1     = rmdi                                                         &
                            ! longitude of westernnmost row of gridpoints
                            ! on a regular full model grid (degrees)
   ,reg_lat1    = rmdi                                                         &
                            ! latitude of southernnmost row of gridpoints
                            ! on a regular full model grid (degrees)
   ,reg_dlon    = rmdi                                                         &
                            ! size of gridbox of (regular) full model grid
                            ! in longitude (degrees)
   ,reg_dlat    = rmdi
                            ! size of gridbox of (regular) full model grid
                            ! in latitude (degrees)

LOGICAL ::                                                                     &
   rivers_reglatlon = .TRUE.                                                   &
                            ! flag indicating if rivers grid is regular in
                            ! latitude and longitude See above for
                            ! definition of a regular grid.
   ,rivers_regrid   = .TRUE.                                                   &
                            ! flag indicating if model and rivers grids
                            ! are identical
                            !     FALSE grids are identical
                            !     TRUE grids differ and regridding required
   ,rivers_first    = .TRUE.                                                   &
                            ! TRUE indicates first river rivers timestep
   ,rivers_call     = .FALSE.


!-----------------------------------------------------------------------------
! Single namelist definition for UM and standalone
!-----------------------------------------------------------------------------

NAMELIST  / jules_rivers/                                                      &
  l_rivers, l_inland, i_river_vn, nstep_rivers,                                &
  cland, criver, cbland, cbriver, runoff_factor, retl, retr,                   &
  a_thresh, rivers_meander, rivers_speed

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='JULES_RIVERS_MOD'

!-----------------------------------------------------------------------------
TYPE :: rivers_data_type
  REAL, ALLOCATABLE :: tot_surf_runoff_gb(:)
                           !  accumulated surface runoff (production)
                           !  between calls to rivers (kg m-2 s-1)
  REAL, ALLOCATABLE :: tot_sub_runoff_gb(:)
                           !  accumulatd sub-surface runoff (production)
                           !  rate between calls to rivers (kg m-2 s-1)
  REAL, ALLOCATABLE :: acc_lake_evap_gb(:)
                           !  accumulated lake evap over river routing
                           !  timestep (Kg/m2) - on land points
  REAL, ALLOCATABLE :: rivers_sto_per_m2_on_landpts(:)
                           ! Water storage (kg m-2) on land points
  REAL, ALLOCATABLE :: rivers_adj_on_landpts(:)
                           ! adjustment factor for water storage on landpts
  INTEGER, ALLOCATABLE :: il_river_grid(:)
                           ! map of land point index on river routing grid
  INTEGER, ALLOCATABLE :: ir_land_grid(:)
                           ! map of river point index on land model grid
  ! Arrays defined on rivers points only
  INTEGER, ALLOCATABLE :: rfm_iarea_rp(:)
                            ! Number of pixels draining to a pixel
  INTEGER, ALLOCATABLE :: rfm_land_rp(:)
                            ! Flag to indicate river grid pixel type
                            !    0 = sea, 1 = river, 2 = land
  INTEGER, ALLOCATABLE :: rivers_dir_rp(:)
                            ! River routing direction index
  INTEGER, ALLOCATABLE :: rivers_index_rp(:)
                            ! Index of points where routing is calculated
  INTEGER, ALLOCATABLE :: rivers_next_rp(:)
                            ! Index of the next downstream point.
  INTEGER, ALLOCATABLE :: rivers_seq_rp(:)
                            ! River routing pathway sequence
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rivers_sto_rp(:)
                            ! Water storage (kg)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rivers_dra_rp(:)
                            ! Catchment area draining to a grid cell
                            !    (no. of grid cells)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rivers_lat_rp(:)
                            ! River routing point latitude
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rivers_lon_rp(:)
                            ! River routing point longitude
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rflow_rp(:)
                            ! River outflow on river vector (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rrun_rp(:)
                            ! Runoff after river routing on river
                            ! vector (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rrun_surf_rp(:)
                            ! Surface runoff after river routing on river
                            ! vector (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rrun_sub_surf_rp(:)
                            ! Sub-surface runoff after river routing on river
                            ! vector (kg/m2/s)
  ! Ancillary arrays defined on full 2D rivers grid
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rivers_seq(:,:)
                            ! River routing pathway sequence
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rivers_dir(:,:)
                            ! River routing direction index
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rivers_dra(:,:)
                            ! Catchment area draining to a grid cell
                            !    (no. of grid cells)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rivers_lat2d(:,:)
                            ! Full 2D latitude field
                            ! (enables non-regular lat-lon river grids)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rivers_lon2d(:,:)
                            ! Full 2D longitude field
                            ! (enables non-regular lat-lon river grids)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rivers_xgrid(:)
                            ! 1D x-dimension of rivers grid
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rivers_ygrid(:)
                            ! 1D y-dimension of rivers grid
  ! Arrays defined on 1d river points vectors
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rfm_flowobs1_rp(:)
                            ! Initial (observed) river flow (kg m-2 s-1)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rfm_surfstore_rp(:)
                            ! Surface storage (m3)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rfm_substore_rp(:)
                            ! Sub-surface storage (m3)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rfm_flowin_rp(:)
                            ! Surface lateral inflow (m3)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rfm_bflowin_rp(:)
                            ! Sub-surface lateral inflow (m3)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rfm_rivflow_rp(:)
                            ! Surface river flow (m3 s-1)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rfm_baseflow_rp(:)
                            ! Sub-surface flow (m3 s-1)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rivers_boxareas_rp(:)
                            ! Gridbox area of each river grid pixel (m2)
END TYPE rivers_data_type

TYPE :: rivers_type
  REAL, POINTER :: tot_surf_runoff_gb(:)
  REAL, POINTER :: tot_sub_runoff_gb(:)
  REAL, POINTER :: acc_lake_evap_gb(:)
  REAL, POINTER :: rivers_sto_per_m2_on_landpts(:)
  REAL, POINTER :: rivers_adj_on_landpts(:)
  INTEGER, POINTER :: il_river_grid(:)
  INTEGER, POINTER :: ir_land_grid(:)
  INTEGER, POINTER :: rfm_iarea_rp(:)
  INTEGER, POINTER :: rfm_land_rp(:)
  INTEGER, POINTER :: rivers_dir_rp(:)
  INTEGER, POINTER :: rivers_index_rp(:)
  INTEGER, POINTER :: rivers_next_rp(:)
  INTEGER, POINTER :: rivers_seq_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: rivers_sto_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: rivers_dra_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: rivers_lat_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: rivers_lon_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: rflow_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: rrun_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: rrun_surf_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: rrun_sub_surf_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: rivers_seq(:,:)
  REAL(KIND=real_jlslsm), POINTER :: rivers_dir(:,:)
  REAL(KIND=real_jlslsm), POINTER :: rivers_dra(:,:)
  REAL(KIND=real_jlslsm), POINTER :: rivers_lat2d(:,:)
  REAL(KIND=real_jlslsm), POINTER :: rivers_lon2d(:,:)
  REAL(KIND=real_jlslsm), POINTER :: rivers_xgrid(:)
  REAL(KIND=real_jlslsm), POINTER :: rivers_ygrid(:)
  REAL(KIND=real_jlslsm), POINTER :: rfm_flowobs1_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: rfm_surfstore_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: rfm_substore_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: rfm_flowin_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: rfm_bflowin_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: rfm_rivflow_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: rfm_baseflow_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: rivers_boxareas_rp(:)
END TYPE rivers_type

CONTAINS

SUBROUTINE jules_rivers_alloc(land_pts, rivers_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: land_pts
TYPE(rivers_data_type), INTENT(IN OUT) :: rivers_data

!Local variables
INTEGER :: temp_size
INTEGER :: ERROR, error_sum  ! Error indicators

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='JULES_RIVERS_ALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

ALLOCATE(rivers_data%tot_surf_runoff_gb(land_pts))
ALLOCATE(rivers_data%tot_sub_runoff_gb(land_pts))
ALLOCATE(rivers_data%acc_lake_evap_gb(land_pts))
ALLOCATE(rivers_data%rivers_sto_per_m2_on_landpts(land_pts))
ALLOCATE(rivers_data%rivers_adj_on_landpts(land_pts))
! Arrays defined on rivers points are currently allocated in
! src/initialisation/standalone/ancillaries/init_rivers_props_mod.F90
! for JULES standaline OR
! src/science/river_routing/um/um_riv_to_jules_mod.F90 for the UM
! This will be changed in #1258 so the setup has been left in.
!!!ALLOCATE(rivers_data%il_river_grid(np_rivers))
!!!ALLOCATE(rivers_data%ir_land_grid(np_rivers))
!!!ALLOCATE(rivers_data%rfm_iarea_rp(np_rivers))
!!!ALLOCATE(rivers_data%rfm_land_rp(np_rivers))
!!!ALLOCATE(rivers_data%rivers_dir_rp(np_rivers))
!!!ALLOCATE(rivers_data%rivers_index_rp(np_rivers))
!!!ALLOCATE(rivers_data%rivers_next_rp(np_rivers))
!!!ALLOCATE(rivers_data%rivers_seq_rp(np_rivers))
!!!ALLOCATE(rivers_data%rivers_sto_rp(np_rivers))
!!!ALLOCATE(rivers_data%rivers_dra_rp(np_rivers))
!!!ALLOCATE(rivers_data%rivers_lat_rp(np_rivers))
!!!ALLOCATE(rivers_data%rivers_lon_rp(np_rivers))
!!!ALLOCATE(rivers_data%rflow_rp(np_rivers))
!!!ALLOCATE(rivers_data%rrun_rp(np_rivers))
!!!ALLOCATE(rivers_data%rrun_surf_rp(np_rivers))
!!!ALLOCATE(rivers_data%rrun_sub_surf_rp(np_rivers))
!!!ALLOCATE(rivers_data%rivers_seq(nx_rivers, ny_rivers))
!!!ALLOCATE(rivers_data%rivers_dir(nx_rivers, ny_rivers))
!!!ALLOCATE(rivers_data%rivers_dra(nx_rivers, ny_rivers))
!!!ALLOCATE(rivers_data%rivers_lat2d(nx_rivers, ny_rivers))
!!!ALLOCATE(rivers_data%rivers_lon2d(nx_rivers, ny_rivers))
!!!ALLOCATE(rivers_data%rivers_xgrid(np_rivers))
!!!ALLOCATE(rivers_data%rivers_ygrid(np_rivers))
!!!ALLOCATE(rivers_data%rfm_flowobs1_rp(np_rivers))
!!!ALLOCATE(rivers_data%rfm_surfstore_rp(np_rivers))
!!!ALLOCATE(rivers_data%rfm_substore_rp(np_rivers))
!!!ALLOCATE(rivers_data%rfm_flowin_rp(np_rivers))
!!!ALLOCATE(rivers_data%rfm_bflowin_rp(np_rivers))
!!!ALLOCATE(rivers_data%rfm_rivflow_rp(np_rivers))
!!!ALLOCATE(rivers_data%rfm_baseflow_rp(np_rivers))
!!!ALLOCATE(rivers_data%rivers_boxareas_rp(np_rivers))

rivers_data%tot_surf_runoff_gb(:) = 0.0
rivers_data%tot_sub_runoff_gb(:)  = 0.0
rivers_data%acc_lake_evap_gb(:)   = 0.0
rivers_data%rivers_sto_per_m2_on_landpts(:) = 0.0
rivers_data%rivers_adj_on_landpts(:) = 0.0
!!!rivers_data%il_river_grid(:) = 0
!!!rivers_data%ir_land_grid(:) = 0
!!!rivers_data%rfm_iarea_rp(:) = 0
!!!rivers_data%rfm_land_rp(:) = 0
!!!rivers_data%rivers_dir_rp(:) = 0
!!!rivers_data%rivers_index_rp(:) = 0
!!!rivers_data%rivers_next_rp(:) = 0
!!!rivers_data%rivers_seq_rp(:) = 0
!!!rivers_data%rivers_sto_rp(:) = 0.0
!!!rivers_data%rivers_dra_rp(:) = 0.0
!!!rivers_data%rivers_lat_rp(:) = 0.0
!!!rivers_data%rivers_lon_rp(:) = 0.0
!!!rivers_data%rflow_rp(:) = 0.0
!!!rivers_data%rrun_rp(:) = 0.0
!!!rivers_data%rrun_surf_rp(:) = 0.0
!!!rivers_data%rrun_sub_surf_rp(:) = 0.0
!!!rivers_data%rivers_seq(:,:) = 0.0
!!!rivers_data%rivers_dir(:,:) = 0.0
!!!rivers_data%rivers_dra(:,:) = 0.0
!!!rivers_data%rivers_lat2d(:,:) = 0.0
!!!rivers_data%rivers_lon2d(:,:) = 0.0
!!!rivers_data%rivers_xgrid(:) = 0.0
!!!rivers_data%rivers_ygrid(:) = 0.0
!!!rivers_data%rfm_flowobs1_rp(:) = 0.0
!!!rivers_data%rfm_surfstore_rp(:) = 0.0
!!!rivers_data%rfm_substore_rp(:) = 0.0
!!!rivers_data%rfm_flowin_rp(:) = 0.0
!!!rivers_data%rfm_bflowin_rp(:) = 0.0
!!!rivers_data%rfm_rivflow_rp(:) = 0.0
!!!rivers_data%rfm_baseflow_rp(:) = 0.0
!!!rivers_data%rivers_boxareas_rp(:) = 0.0

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE jules_rivers_alloc

#if !defined(LFRIC)
SUBROUTINE check_jules_rivers()

USE ereport_mod, ONLY: ereport

USE jules_irrig_mod, ONLY: l_irrig_limit

!-----------------------------------------------------------------------------
! Description:
!   Checks JULES_RIVERS namelist for consistency
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IMPLICIT NONE

INTEGER :: errcode

IF ( .NOT. l_rivers ) THEN
  IF (l_irrig_limit) THEN
    errcode = 101
    CALL ereport("check_jules_rivers", errcode,                                &
                 'l_irrig_limit=T requires l_rivers=T ')
  END IF

  ! If rivers are not enabled, there is nothing further to check
  RETURN
END IF

! Check that a valid integer timestep was given
IF ( nstep_rivers <= 0 ) THEN
  errcode = 101
  CALL ereport("check_jules_rivers", errcode, 'nstep_rivers must be > 0')
END IF

! Check that parameter values are appropriate for the selected algorithm
! This also serves as a check that we have a recognised rivers type
SELECT CASE ( i_river_vn )
CASE ( rivers_rfm )
  IF ( cland <= 0.0 .OR. criver <= 0.0 ) THEN
    errcode = 101
    CALL ereport("check_jules_rivers", errcode,                                &
                 "Surface wave speeds must be > 0")
  END IF
  IF ( cbland <= 0.0 .OR. cbriver <= 0.0) THEN
    errcode = 102
    CALL ereport("check_jules_rivers", errcode,                                &
                 "Sub surface wave speeds must be > 0")
  END IF
  IF ( runoff_factor <= 0.0 ) THEN
    errcode = 103
    CALL ereport("check_jules_rivers", errcode,                                &
                 "Runoff factor must be > 0")
  END IF

CASE ( rivers_trip )
  IF ( rivers_speed <= 0.0 .OR. rivers_meander <= 0.0 ) THEN
    errcode = 104
    CALL ereport("check_jules_rivers", errcode,                                &
                 "River speed and meander ratio must be > 0")
  END IF
CASE ( rivers_um_trip )
  IF ( rivers_speed <= 0.0 .OR. rivers_meander <= 0.0 ) THEN
    errcode = 104
    CALL ereport("check_jules_rivers", errcode,                                &
                 "River speed and meander ratio must be > 0")
  END IF
CASE DEFAULT
  errcode = 101
  CALL ereport("check_jules_rivers", errcode,                                  &
               'Unrecognised river routing algorithm (i_river_vn)')
END SELECT

IF ( l_irrig_limit .AND. ( i_river_vn /= rivers_trip ) ) THEN
  errcode = 101
  CALL ereport("check_jules_rivers", errcode,                                  &
               'l_irrig_limit=T requires i_river_vn=3')
END IF

END SUBROUTINE check_jules_rivers
#endif

SUBROUTINE print_nlist_jules_rivers()

USE jules_print_mgr, ONLY: jules_print

IMPLICIT NONE
CHARACTER(LEN=50000) :: lineBuffer

CHARACTER(LEN=*), PARAMETER :: RoutineName='PRINT_NLIST_JULES_RIVERS'

CALL jules_print('jules_rivers_inputs_mod',                                    &
   'Contents of namelist jules_rivers')

WRITE(lineBuffer,*)' l_rivers = ',l_rivers
CALL jules_print('jules_rivers',lineBuffer)
WRITE(lineBuffer,*)' l_inland = ',l_inland
CALL jules_print('jules_rivers',lineBuffer)
WRITE(lineBuffer,*)' i_river_vn = ',i_river_vn
CALL jules_print('jules_rivers',lineBuffer)
WRITE(lineBuffer,*)' nstep_rivers = ',nstep_rivers
CALL jules_print('jules_rivers',lineBuffer)

SELECT CASE ( i_river_vn )

CASE ( rivers_trip, rivers_um_trip )
  WRITE(lineBuffer,*)' RIVERS_SPEED = ',rivers_speed
  CALL jules_print('jules_rivers',lineBuffer)
  WRITE(lineBuffer,*)' RIVERS_MEANDER = ',rivers_meander
  CALL jules_print('jules_rivers',lineBuffer)

CASE ( rivers_rfm )
  WRITE(lineBuffer,*)' CLAND = ',cland
  CALL jules_print('jules_rivers',lineBuffer)
  WRITE(lineBuffer,*)' CRIVER = ',criver
  CALL jules_print('jules_rivers',lineBuffer)
  WRITE(lineBuffer,*)' CBLAND = ',cbland
  CALL jules_print('jules_rivers',lineBuffer)
  WRITE(lineBuffer,*)' CBRIVER = ',cbriver
  CALL jules_print('jules_rivers',lineBuffer)
  WRITE(lineBuffer,*)' RETL = ',retl
  CALL jules_print('jules_rivers',lineBuffer)
  WRITE(lineBuffer,*)' RETR = ',retr
  CALL jules_print('jules_rivers',lineBuffer)
  WRITE(lineBuffer,*)' A_THRESH = ',a_thresh
  CALL jules_print('jules_rivers',lineBuffer)
  WRITE(lineBuffer,*)' RUNOFF_FACTOR = ',runoff_factor
  CALL jules_print('jules_rivers',lineBuffer)

END SELECT

CALL jules_print('jules_rivers_mod',                                           &
   '- - - - - - end of namelist - - - - - -')

END SUBROUTINE print_nlist_jules_rivers


#if defined(UM_JULES) && !defined(LFRIC)
SUBROUTINE read_nml_jules_rivers(unit_in)

USE setup_namelist,   ONLY: setup_nml_type
USE check_iostat_mod, ONLY: check_iostat
USE UM_parcore,       ONLY: mype
USE parkind1,         ONLY: jprb, jpim
USE yomhook,          ONLY: lhook, dr_hook
USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

INTEGER,INTENT(IN) :: unit_in
INTEGER :: my_comm
INTEGER :: mpl_nml_type
INTEGER :: ErrorStatus
INTEGER :: icode
CHARACTER(LEN=errormessagelength) :: iomessage
REAL(KIND=jprb) :: zhook_handle
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1

CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_NML_JULES_RIVERS'

! set number of each type of variable in my_namelist type
INTEGER, PARAMETER :: no_of_types = 3
INTEGER, PARAMETER :: n_int = 3
INTEGER, PARAMETER :: n_real = 9
INTEGER, PARAMETER :: n_log = 2

TYPE :: my_namelist
  SEQUENCE
  INTEGER :: nstep_rivers
  INTEGER :: a_thresh
  INTEGER :: i_river_vn
  REAL(KIND=real_jlslsm) :: cland
  REAL(KIND=real_jlslsm) :: criver
  REAL(KIND=real_jlslsm) :: cbland
  REAL(KIND=real_jlslsm) :: cbriver
  REAL(KIND=real_jlslsm) :: runoff_factor
  REAL(KIND=real_jlslsm) :: retl
  REAL(KIND=real_jlslsm) :: retr
  REAL(KIND=real_jlslsm) :: rivers_meander
  REAL(KIND=real_jlslsm) :: rivers_speed
  LOGICAL :: l_rivers
  LOGICAL :: l_inland
END TYPE my_namelist

TYPE (my_namelist) :: my_nml

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL gc_get_communicator(my_comm, icode)

CALL setup_nml_type(no_of_types, mpl_nml_type, n_int_in = n_int,               &
                    n_real_in = n_real, n_log_in = n_log)

IF (mype == 0) THEN
  READ(UNIT = unit_in, NML = jules_rivers, IOSTAT = ErrorStatus,               &
                                           IOMSG = iomessage)
  CALL check_iostat(errorstatus, "namelist JULES_RIVERS", iomessage)

  my_nml % nstep_rivers = nstep_rivers
  my_nml % a_thresh = a_thresh
  my_nml % i_river_vn = i_river_vn
  my_nml % cland = cland
  my_nml % criver = criver
  my_nml % cbland = cbland
  my_nml % cbriver = cbriver
  my_nml % runoff_factor = runoff_factor
  my_nml % retl = retl
  my_nml % retr = retr
  my_nml % rivers_meander = rivers_meander
  my_nml % rivers_speed = rivers_speed
  my_nml % l_rivers = l_rivers
  my_nml % l_inland = l_inland
END IF

CALL mpl_bcast(my_nml,1,mpl_nml_type,0,my_comm,icode)

IF (mype /= 0) THEN
  nstep_rivers = my_nml % nstep_rivers
  a_thresh = my_nml % a_thresh
  i_river_vn = my_nml % i_river_vn
  cland = my_nml % cland
  criver = my_nml % criver
  cbland = my_nml % cbland
  cbriver = my_nml % cbriver
  runoff_factor = my_nml % runoff_factor
  retl = my_nml % retl
  retr = my_nml % retr
  a_thresh = my_nml % a_thresh
  rivers_meander = my_nml % rivers_meander
  rivers_speed = my_nml % rivers_speed
  l_rivers = my_nml % l_rivers
  l_inland = my_nml % l_inland
END IF

CALL mpl_type_free(mpl_nml_type,icode)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

END SUBROUTINE read_nml_jules_rivers
#endif

!===============================================================================

SUBROUTINE rivers_dealloc(rivers_data)


!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
TYPE(rivers_data_type), INTENT(IN OUT) :: rivers_data

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='RIVERS_DEALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

DEALLOCATE(rivers_data%tot_surf_runoff_gb)
DEALLOCATE(rivers_data%tot_sub_runoff_gb)
DEALLOCATE(rivers_data%acc_lake_evap_gb)
DEALLOCATE(rivers_data%rivers_sto_per_m2_on_landpts)
DEALLOCATE(rivers_data%rivers_adj_on_landpts)
!!!DEALLOCATE(rivers_data%il_river_grid)
!!!DEALLOCATE(rivers_data%ir_land_grid)
!!!DEALLOCATE(rivers_data%rfm_iarea_rp)
!!!DEALLOCATE(rivers_data%rfm_land_rp)
!!!DEALLOCATE(rivers_data%rivers_dir_rp)
!!!DEALLOCATE(rivers_data%rivers_index_rp)
!!!DEALLOCATE(rivers_data%rivers_next_rp)
!!!DEALLOCATE(rivers_data%rivers_seq_rp)
!!!DEALLOCATE(rivers_data%rivers_sto_rp)
!!!DEALLOCATE(rivers_data%rivers_dra_rp)
!!!DEALLOCATE(rivers_data%rivers_lat_rp)
!!!DEALLOCATE(rivers_data%rivers_lon_rp)
!!!DEALLOCATE(rivers_data%rflow_rp)
!!!DEALLOCATE(rivers_data%rrun_rp)
!!!DEALLOCATE(rivers_data%rrun_surf_rp)
!!!DEALLOCATE(rivers_data%rrun_sub_surf_rp)
!!!DEALLOCATE(rivers_data%rivers_seq)
!!!DEALLOCATE(rivers_data%rivers_dir)
!!!DEALLOCATE(rivers_data%rivers_dra)
!!!DEALLOCATE(rivers_data%rivers_lat2d)
!!!DEALLOCATE(rivers_data%rivers_lon2d)
!!!DEALLOCATE(rivers_data%rivers_xgrid)
!!!DEALLOCATE(rivers_data%rivers_ygrid)
!!!DEALLOCATE(rivers_data%rfm_flowobs1_rp)
!!!DEALLOCATE(rivers_data%rfm_surfstore_rp)
!!!DEALLOCATE(rivers_data%rfm_substore_rp)
!!!DEALLOCATE(rivers_data%rfm_flowin_rp)
!!!DEALLOCATE(rivers_data%rfm_bflowin_rp)
!!!DEALLOCATE(rivers_data%rfm_rivflow_rp)
!!!DEALLOCATE(rivers_data%rfm_baseflow_rp)
!!!DEALLOCATE(rivers_data%rivers_boxareas_rp)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE rivers_dealloc

!===============================================================================
SUBROUTINE rivers_assoc(rivers, rivers_data)


!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
TYPE(rivers_type), INTENT(IN OUT) :: rivers
TYPE(rivers_data_type), INTENT(IN OUT), TARGET :: rivers_data

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='RIVERS_ASSOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL rivers_nullify(rivers)

rivers%tot_surf_runoff_gb => rivers_data%tot_surf_runoff_gb
rivers%tot_sub_runoff_gb => rivers_data%tot_sub_runoff_gb
rivers%acc_lake_evap_gb => rivers_data%acc_lake_evap_gb
rivers%rivers_sto_per_m2_on_landpts => rivers_data%rivers_sto_per_m2_on_landpts
rivers%rivers_adj_on_landpts => rivers_data%rivers_adj_on_landpts
!!!rivers%il_river_grid => rivers_data%il_river_grid
!!!rivers%ir_land_grid => rivers_data%ir_land_grid
!!!rivers%rfm_iarea_rp => rivers_data%rfm_iarea_rp
!!!rivers%rfm_land_rp => rivers_data%rfm_land_rp
!!!rivers%rivers_dir_rp => rivers_data%rivers_dir_rp
!!!rivers%rivers_index_rp => rivers_data%rivers_index_rp
!!!rivers%rivers_next_rp => rivers_data%rivers_next_rp
!!!rivers%rivers_seq_rp => rivers_data%rivers_seq_rp
!!!rivers%rivers_sto_rp => rivers_data%rivers_sto_rp
!!!rivers%rivers_dra_rp => rivers_data%rivers_dra_rp
!!!rivers%rivers_lat_rp => rivers_data%rivers_lat_rp
!!!rivers%rivers_lon_rp => rivers_data%rivers_lon_rp
!!!rivers%rflow_rp => rivers_data%rflow_rp
!!!rivers%rrun_rp => rivers_data%rrun_rp
!!!rivers%rrun_surf_rp => rivers_data%rrun_surf_rp
!!!rivers%rrun_sub_surf_rp => rivers_data%rrun_sub_surf_rp
!!!rivers%rivers_seq => rivers_data%rivers_seq
!!!rivers%rivers_dir => rivers_data%rivers_dir
!!!rivers%rivers_dra => rivers_data%rivers_dra
!!!rivers%rivers_lat2d => rivers_data%rivers_lat2d
!!!rivers%rivers_lon2d => rivers_data%rivers_lon2d
!!!rivers%rivers_xgrid => rivers_data%rivers_xgrid
!!!rivers%rivers_ygrid => rivers_data%rivers_ygrid
!!!rivers%rfm_flowobs1_rp => rivers_data%rfm_flowobs1_rp
!!!rivers%rfm_surfstore_rp => rivers_data%rfm_surfstore_rp
!!!rivers%rfm_substore_rp => rivers_data%rfm_substore_rp
!!!rivers%rfm_flowin_rp => rivers_data%rfm_flowin_rp
!!!rivers%rfm_bflowin_rp => rivers_data%rfm_bflowin_rp
!!!rivers%rfm_rivflow_rp => rivers_data%rfm_rivflow_rp
!!!rivers%rfm_baseflow_rp => rivers_data%rfm_baseflow_rp
!!!rivers%rivers_boxareas_rp => rivers_data%rivers_boxareas_rp


IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE rivers_assoc


!===============================================================================
SUBROUTINE rivers_nullify(rivers)


!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
TYPE(rivers_type), INTENT(IN OUT) :: rivers

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='RIVERS_NULLIFY'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

NULLIFY(rivers%tot_surf_runoff_gb)
NULLIFY(rivers%tot_sub_runoff_gb)
NULLIFY(rivers%acc_lake_evap_gb)
NULLIFY(rivers%rivers_sto_per_m2_on_landpts)
NULLIFY(rivers%rivers_adj_on_landpts)
!!!NULLIFY(rivers%il_river_grid)
!!!NULLIFY(rivers%ir_land_grid)
!!!NULLIFY(rfm_iarea_rp)
!!!NULLIFY(rfm_land_rp)
!!!NULLIFY(rivers_dir_rp)
!!!NULLIFY(rivers_index_rp)
!!!NULLIFY(rivers_next_rp)
!!!NULLIFY(rivers_seq_rp)
!!!NULLIFY(rivers_sto_rp)
!!!NULLIFY(rivers_dra_rp)
!!!NULLIFY(rivers_lat_rp)
!!!NULLIFY(rivers_lon_rp)
!!!NULLIFY(rflow_rp)
!!!NULLIFY(rrun_rp)
!!!NULLIFY(rrun_surf_rp)
!!!NULLIFY(rrun_sub_surf_rp)
!!!NULLIFY(rivers_seq)
!!!NULLIFY(rivers_dir)
!!!NULLIFY(rivers_dra)
!!!NULLIFY(rivers_lat2d)
!!!NULLIFY(rivers_lon2d)
!!!NULLIFY(rivers_xgrid)
!!!NULLIFY(rivers_ygrid)
!!!NULLIFY(rfm_flowobs1_rp)
!!!NULLIFY(rfm_surfstore_rp)
!!!NULLIFY(rfm_substore_rp)
!!!NULLIFY(rfm_flowin_rp)
!!!NULLIFY(rfm_bflowin_rp)
!!!NULLIFY(rfm_rivflow_rp)
!!!NULLIFY(rfm_baseflow_rp)
!!!NULLIFY(rivers_boxareas_rp)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE rivers_nullify

END MODULE jules_rivers_mod

!-----------------------------------------------------------------------------
