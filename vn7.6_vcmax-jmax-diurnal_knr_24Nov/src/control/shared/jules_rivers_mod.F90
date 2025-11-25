! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE jules_rivers_mod

!------------------------------------------------------------------------------
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
!------------------------------------------------------------------------------

USE missing_data_mod, ONLY: imdi, rmdi

USE um_types, ONLY: real_jlslsm

USE ereport_mod, ONLY: ereport

IMPLICIT NONE

!------------------------------------------------------------------------------
! Module constants
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
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

INTEGER, PARAMETER :: inland_drainage = -10
    ! The value of nextx_grid that indicates an inland drainage point, i.e. an
    ! endorheic catchment. This should be <0 and should not be either of sea or
    ! river_mouth, nor should it appear in flow_dir_river_edge.

INTEGER, PARAMETER :: river_mouth = -9
  ! The value of nextx_grid that indicates a river mouth. This should be <0 and
  ! should not be either of sea or inland_drainage, nor should it appear in
  ! flow_dir_river_edge.

INTEGER, PARAMETER :: sea = 0
    ! The value of flow direction that is used to indicate a sea point.
    ! This value is used to set the flow direction variable in this code, but
    ! it need not be the value used to indicate sea in the input ancillary
    ! file. This value should be <1 and must not be any of flow_dir_river(1:8),
    ! dir_mouth or dir_inland_drainage.
    ! This value is also used to indicate sea in river_mask.

INTEGER, PARAMETER :: rfm_land = 2
  ! The value of rfm_land_rp indicating a land point.
INTEGER, PARAMETER :: rfm_river = 1
  ! The value of rfm_land_rp indicating a river point.
INTEGER, PARAMETER :: rfm_sea = 0
  ! The value of rfm_land_rp indicating a sea point.
  ! This is only used in the UM.

!------------------------------------------------------------------------------
! Array parameters.

! The values in any flow direction ancillary layer must be consistent with the
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

!------------------------------------------------------------------------------
! Items set in namelist
!------------------------------------------------------------------------------

LOGICAL ::                                                                     &
   l_rivers = .FALSE.                                                          &
                            ! Switch for runoff routing
   ,l_inland = .FALSE.                                                         &
                            ! Control rerouting of inland basin water
   ,l_riv_overbank = .FALSE.                                                   &
                            ! Logical to control overbank inundation
   ,l_outflow_per_river = .FALSE.
                 ! Internal flag where exact usage depends on the parent model
                 ! as a result of the order of namelist reading:
                 !   oasis_rivers, jules_rivers_props, jules_output.
                 ! When TRUE:
                 ! - OASIS-Rivers: OASIS send field contains
                 !   outflow_per_river so river number ancillary should be
                 !   specified in jules_rivers_props.
                 ! - Standalone: The river number ancillary has been specified
                 !   so outflow_per_river should be present in jules_output.
                 ! Both cases are checked once the latter namelist is read.
                 ! Additionally, rivers_outflow_rp is added to the dump.

INTEGER ::                                                                     &
   nstep_rivers = imdi                                                         &
                            ! Timestep for runoff routing
                            ! (number of model timesteps)
   ,i_river_vn  = imdi                                                         &
                            ! integer representation of river routing type
                            !   1 == 'um_trip'
                            !   2 == 'rfm'
                            !   3 == 'trip'
   ,a_thresh = imdi
                            ! threshold area (TRIP: pixels; RFM: km)

! Select shape of Earth used in TRIP river routing scheme
INTEGER :: trip_globe_shape = imdi
INTEGER, PARAMETER :: globe_spherical = 1
INTEGER, PARAMETER :: globe_ellipsoidal = 2

REAL(KIND=real_jlslsm) ::                                                      &
   rivers_meander = rmdi                                                       &
                            ! meander ratio for rivers - the ratio of
                            ! actual river length to the calculated length
   ,rivers_speed = rmdi                                                        &
                            ! flow speed for rivers (m s-1)
   ,runoff_factor = rmdi                                                       &
                            ! runoff volume factor
   ,cland  = rmdi                                                              &
                            ! land wave speed (m/s)
   ,criver = rmdi                                                              &
                            ! subsurface river wave speed (m/s)
   ,cbland  = rmdi                                                             &
                            ! subsurface land wave speed (m/s)
   ,cbriver = rmdi                                                             &
                            ! subsurface river wave speed (m/s)
   ,retl = rmdi                                                                &
                            ! return flow (land squares) (<1)
   ,retr = rmdi
                            ! return flow (river squares) (<1)

! Selects different fields for use in water conservation of lake evaporation
! JULES ticket #1360
INTEGER :: lake_water_conserve_method = imdi
INTEGER, PARAMETER :: use_fqw_surft  = 1
INTEGER, PARAMETER :: use_elake_surft = 2

!------------------------------------------------------------------------------
! Single namelist definition for UM and standalone
!------------------------------------------------------------------------------
NAMELIST  /jules_rivers/                                                       &
  l_rivers, l_inland, l_riv_overbank, i_river_vn, nstep_rivers,                &
  trip_globe_shape,                                                            &
  cland, criver, cbland, cbriver, runoff_factor, retl, retr,                   &
  a_thresh, rivers_meander, rivers_speed, lake_water_conserve_method

!------------------------------------------------------------------------------
! Note that items declared below here are not in the jules_rivers namelist.
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Definitions and characteristics of the river routing grid
!------------------------------------------------------------------------------
! Scalar variables
INTEGER ::                                                                     &
    nx_rivers = 0                                                              &
                            ! row length for rivers grid
   ,ny_rivers = 0                                                              &
                            ! column length for rivers grid
   ,n_rivers = 0
                            ! number of rivers in the River number ancillary
                            ! - identifies the River an outflow gridbox belongs
                            !   to by assigning a river number [1:n_rivers]


REAL(KIND=real_jlslsm) ::                                                      &
   rivers_dx = rmdi                                                            &
                            ! Size of gridbox of regular rivers grid
                            ! in x direction. If l_coord_latlon=T, the units
                            ! are degrees of longitude, otherwise units are
                            ! unknown.
   ,rivers_dy = rmdi                                                           &
                            ! Size of gridbox of regular rivers grid
                            ! in y direction. If l_coord_latlon=T, the units
                            ! are degrees of latitude, otherwise units are
                            ! unknown.
   ,rivers_x1 = rmdi                                                           &
                            ! x coordinate of of westernmost (first) column of
                            ! gridpoints on the regular rivers grid.
   ,rivers_y1 = rmdi                                                           &
                            ! y coordinate of "southernmost" (first) row of
                            ! gridpoints on the regular rivers grid.
   ,rivers_length = rmdi
                            ! Size of gridbox of rivers grid (m).
      ! This is used:
      !   to calculate gridbox areas (if l_coord_latlon=F);
      !   as a length scale for RFM;
      !   as a length scale for overbank inundation with l_riv_hypsometry=F.

!------------------------------------------------------------------------------
! Variables that describe the land grid (not the river grid).
! If the model grid is 1-D, this is the notional 2-D grid across which the
! land points can be scattered. In many cases we expect this grid to
! be identical to the full JULES model grid (where that is 2D), but as these
! grid descriptors are input via a namelist this grid can in principle be
! larger than the model grid.
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
   nx_land_grid  = imdi                                                        &
                            ! Row length of land grid.
   ,ny_land_grid = imdi
                            ! Column length of land grid.

REAL(KIND=real_jlslsm) ::                                                      &
   land_dx = rmdi                                                              &
                            ! Size in x direction of gridbox of (regular) full
                            ! land grid. If l_coord_latlon=T, this is
                            ! longitude in degrees, otherwise units are
                            ! unknown.
   ,land_dy = rmdi                                                             &
                            ! Size in y direction of gridbox of (regular) full
                            ! land grid. If l_coord_latlon=T, this is
                            ! latitude in degrees, otherwise units are
                            ! unknown.
   ,x1_land_grid  = rmdi                                                       &
                            ! x coordinate of "western-most" (i.e. first)
                            ! column of gridpoints on a regular full land
                            ! grid. If l_coord_latlon=T, this is longitude in
                            ! degrees, otherwise units are unknown.
   ,y1_land_grid  = rmdi
                            ! y coordinate of "southern-most" (i.e. first)
                            ! row of gridpoints on a regular full land
                            ! grid. If l_coord_latlon=T, this is latitude in
                            ! degrees, otherwise units are unknown.

!------------------------------------------------------------------------------
! Further river variables.
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
    np_rivers = 0                                                              &
                            ! Number of river points, i.e. the number of points
                            ! in the rivers grid at which routing is calculated
   ,nseqmax   = imdi
                            ! Maximum value of routing grid sequence.

LOGICAL ::                                                                     &
   l_trivial_mapping = .FALSE.                                                 &
                            ! Flag indicating if the land and river points
                            ! are identical (and in the same order) and hence
                            ! fields can be moved between grids simply by
                            ! copying.
   ,rivers_reglatlon = .TRUE.                                                  &
                            ! Flag indicating if rivers grid is regular in
                            ! latitude and longitude. See above for
                            ! definition of a regular grid.
   ,rivers_regrid   = .FALSE.                                                  &
                            ! Flag indicating if variables on the land grid
                            ! need to be regridded (interpolated) to the river
                            ! routing grid.
                            ! Grids are considered consistent (and therefore
                            ! regridding is not required) if they are of the
                            ! same resolution and points on one coincide with
                            ! points on the other. These grids can be handled
                            ! by a 1:1 mapping. We do not require that all
                            ! locations have to be in both grids (though that
                            ! is desirable), nor do the points need to be
                            ! presented in the same order in both grids.
                            !   FALSE grids are consistent and fields can be
                            !     transferred between grids through a simple
                            !     mapping
                            !   TRUE grids differ and regridding is required.
   ,rivers_first    = .TRUE.                                                   &
                            ! TRUE indicates first river rivers timestep
   ,rivers_call     = .FALSE.
                            ! Flag that is TRUE on timesteps when rivers are
                            ! called, otherwise FALSE.

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='JULES_RIVERS_MOD'

!------------------------------------------------------------------------------
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
  INTEGER, ALLOCATABLE :: map_river_to_land_points(:)
                           ! List of coincident land point numbers, on
                           ! river points.
                           ! For every river point, this is the matching land
                           ! point. Only used when rivers_regrid=F.
  INTEGER, ALLOCATABLE :: global_land_index(:)
                           ! List of indices for the land grid.
                           ! For every land point, this gives the location in a
                           ! 2-D grid. Only used after initialisation when
                           ! rivers_regrid=T.

  ! Arrays defined on rivers points only
  INTEGER, ALLOCATABLE :: rfm_iarea_rp(:)
                            ! Number of pixels draining to a pixel
  INTEGER, ALLOCATABLE :: rfm_land_rp(:)
                            ! Flag to indicate river grid pixel type
                            !    0 = sea, 1 = river, 2 = land
  INTEGER, ALLOCATABLE :: rivers_index_rp(:)
                            ! Index of points where routing is calculated.
                            ! This index refers to position in the river grid.
  INTEGER, ALLOCATABLE :: rivers_next_rp(:)
                            ! Index (river point number) of the next downstream
                            ! point.
  INTEGER, ALLOCATABLE :: rivers_seq_rp(:)
                            ! River routing pathway sequence
  INTEGER, ALLOCATABLE :: rivers_outflow_number_rp(:)
                            ! Maps ocean outflow gridboxes to the river they
                            !    belong to (on river points)
                            ! Used for coupling to ocean model
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rivers_sto_rp(:)
                            ! Water storage (kg)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rivers_lat_rp(:)
                            ! River routing point latitude
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rivers_lon_rp(:)
                            ! River routing point longitude
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rivers_ilat_rp(:)
                            ! River routing index in latitude direction
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rivers_ilon_rp(:)
                            ! River routing index in longitude direction
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rivers_x_coord_rp(:)
                            ! River routing point projection x coordinate.
                            ! Units can differ between input datasets.
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rivers_y_coord_rp(:)
                            ! River routing point projection y coordinate.
                            ! Units can differ between input datasets.
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
  REAL(KIND=real_jlslsm), ALLOCATABLE :: sub_surf_roff_rp(:)
    ! OASIS-Rivers: Sub-surface runoff on river vector (kg m-2 s-1)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: surf_roff_rp(:)
    ! OASIS-Rivers: Surface runoff on river vector (kg m-2 s-1)

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
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rivers_lon2d(:,:)
                            ! Full 2D longitude field
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rivers_outflow_number(:,:)
                            ! Maps ocean outflow gridboxes to the river they
                            !    belong to (on rivers grid)
                            ! Used for coupling to ocean model
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rivers_xgrid(:)
                            ! Coordinate values for x-dimension of rivers grid
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rivers_ygrid(:)
                            ! Coordinate values for y-dimension of rivers grid

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
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rivers_outflow_rp(:)
                            ! River outflow into the ocean (kg s-1)
END TYPE rivers_data_type

TYPE :: rivers_type
  REAL, POINTER :: tot_surf_runoff_gb(:)
  REAL, POINTER :: tot_sub_runoff_gb(:)
  REAL, POINTER :: acc_lake_evap_gb(:)
  REAL, POINTER :: rivers_sto_per_m2_on_landpts(:)
  REAL, POINTER :: rivers_adj_on_landpts(:)
  INTEGER, POINTER :: map_river_to_land_points(:)
  INTEGER, POINTER :: global_land_index(:)
  INTEGER, POINTER :: rfm_iarea_rp(:)
  INTEGER, POINTER :: rfm_land_rp(:)
  INTEGER, POINTER :: rivers_index_rp(:)
  INTEGER, POINTER :: rivers_next_rp(:)
  INTEGER, POINTER :: rivers_seq_rp(:)
  INTEGER, POINTER :: rivers_outflow_number_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: rivers_sto_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: rivers_lat_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: rivers_lon_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: rivers_ilat_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: rivers_ilon_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: rivers_x_coord_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: rivers_y_coord_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: rflow_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: rrun_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: rrun_surf_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: rrun_sub_surf_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: sub_surf_roff_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: surf_roff_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: rivers_seq(:,:)
  REAL(KIND=real_jlslsm), POINTER :: rivers_dir(:,:)
  REAL(KIND=real_jlslsm), POINTER :: rivers_dra(:,:)
  REAL(KIND=real_jlslsm), POINTER :: rivers_lat2d(:,:)
  REAL(KIND=real_jlslsm), POINTER :: rivers_lon2d(:,:)
  REAL(KIND=real_jlslsm), POINTER :: rivers_xgrid(:)
  REAL(KIND=real_jlslsm), POINTER :: rivers_ygrid(:)
  REAL(KIND=real_jlslsm), POINTER :: rivers_outflow_number(:,:)
  REAL(KIND=real_jlslsm), POINTER :: rfm_flowobs1_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: rfm_surfstore_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: rfm_substore_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: rfm_flowin_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: rfm_bflowin_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: rfm_rivflow_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: rfm_baseflow_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: rivers_boxareas_rp(:)
  REAL(KIND=real_jlslsm), POINTER :: rivers_outflow_rp(:)
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
!!!ALLOCATE(rivers_data%map_river_to_land_points(np_rivers))
!!!ALLOCATE(rivers_data%global_land_index(np_rivers))
!!!ALLOCATE(rivers_data%rfm_iarea_rp(np_rivers))
!!!ALLOCATE(rivers_data%rfm_land_rp(np_rivers))
!!!ALLOCATE(rivers_data%rivers_index_rp(np_rivers))
!!!ALLOCATE(rivers_data%rivers_next_rp(np_rivers))
!!!ALLOCATE(rivers_data%rivers_seq_rp(np_rivers))
!!!ALLOCATE(rivers_data%rivers_sto_rp(np_rivers))
!!!ALLOCATE(rivers_data%rivers_lat_rp(np_rivers))
!!!ALLOCATE(rivers_data%rivers_lon_rp(np_rivers))
!!!ALLOCATE(rivers_data%rivers_ilat_rp(np_rivers))
!!!ALLOCATE(rivers_data%rivers_ilon_rp(np_rivers))
!!!ALLOCATE(rivers_data%rivers_x_coord_rp(np_rivers))
!!!ALLOCATE(rivers_data%rivers_y_coord_rp(np_rivers))
!!!ALLOCATE(rivers_data%rflow_rp(np_rivers))
!!!ALLOCATE(rivers_data%rrun_rp(np_rivers))
!!!ALLOCATE(rivers_data%rrun_surf_rp(np_rivers))
!!!ALLOCATE(rivers_data%rrun_sub_surf_rp(np_rivers))
!!!ALLOCATE(rivers_data%rivers_seq(nx_rivers, ny_rivers))
!!!ALLOCATE(rivers_data%rivers_dir(nx_rivers, ny_rivers))
!!!ALLOCATE(rivers_data%rivers_dra(nx_rivers, ny_rivers))
!!!ALLOCATE(rivers_data%rivers_lat2d(nx_rivers, ny_rivers))
!!!ALLOCATE(rivers_data%rivers_lon2d(nx_rivers, ny_rivers))
!!!ALLOCATE(rivers_data%rivers_xgrid(nx_rivers))
!!!ALLOCATE(rivers_data%rivers_ygrid(ny_rivers))
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
!!!rivers_data%map_river_to_land_points(:) = 0
!!!rivers_data%global_land_index(:) = 0
!!!rivers_data%rfm_iarea_rp(:) = 0
!!!rivers_data%rfm_land_rp(:) = 0
!!!rivers_data%rivers_index_rp(:) = 0
!!!rivers_data%rivers_next_rp(:) = 0
!!!rivers_data%rivers_seq_rp(:) = 0
!!!rivers_data%rivers_sto_rp(:) = 0.0
!!!rivers_data%rivers_lat_rp(:) = 0.0
!!!rivers_data%rivers_lon_rp(:) = 0.0
!!!rivers_data%rivers_ilat_rp(:) = 0.0
!!!rivers_data%rivers_ilon_rp(:) = 0.0
!!!rivers_data%rivers_x_coord_rp(:) = 0.0
!!!rivers_data%rivers_y_coord_rp(:) = 0.0
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

!------------------------------------------------------------------------------
! Description:
!   Checks JULES_RIVERS namelist for consistency
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!------------------------------------------------------------------------------

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'CHECK_JULES_RIVERS'

INTEGER :: errcode

IF ( .NOT. l_rivers ) THEN
  IF (l_irrig_limit) THEN
    errcode = 101
    CALL ereport(RoutineName, errcode,                                         &
                 'l_irrig_limit=T requires l_rivers=T ')
  END IF

  ! If rivers are not enabled, there is nothing further to check
  RETURN
END IF

! Check that a valid integer timestep was given
IF ( nstep_rivers <= 0 ) THEN
  errcode = 101
  CALL ereport(RoutineName, errcode, 'nstep_rivers must be > 0')
END IF

! Check that the required parameter values have been provided and are
! appropriate for the selected algorithm.
! This also serves as a check that we have a recognised rivers type.
SELECT CASE ( i_river_vn )

CASE ( rivers_rfm )
  !---------------------------------------------------------------------------
  ! RFM parameters.
  !---------------------------------------------------------------------------

  IF ( a_thresh == imdi ) THEN
    errcode = 101  !  a hard error
    CALL ereport(RoutineName, errcode,                                         &
                 "No value for a_thresh")
  END IF

  IF ( ABS( cbland - rmdi ) < EPSILON(1.0) ) THEN
    errcode = 101  !  a hard error
    CALL ereport(RoutineName, errcode,                                         &
                 "No value for cbland")
  ELSE IF ( cbland <= 0.0 ) THEN
    errcode = 101
    CALL ereport(RoutineName, errcode,                                         &
                 "Sub surface land wave speed must be > 0")
  END IF

  IF ( ABS( cbriver - rmdi ) < EPSILON(1.0) ) THEN
    errcode = 101  !  a hard error
    CALL ereport(RoutineName, errcode,                                         &
                 "No value for cbriver")
  ELSE IF ( cbriver <= 0.0 ) THEN
    errcode = 101
    CALL ereport(RoutineName, errcode,                                         &
                 "Sub surface river wave speed must be > 0")
  END IF

  IF ( ABS( cland - rmdi ) < EPSILON(1.0) ) THEN
    errcode = 101  !  a hard error
    CALL ereport(RoutineName, errcode,                                         &
                 "No value for cland")
  ELSE IF ( cland <= 0.0 ) THEN
    errcode = 101
    CALL ereport(RoutineName, errcode,                                         &
                 "Surface wave land speed must be > 0")
  END IF

  IF ( ABS( criver - rmdi ) < EPSILON(1.0) ) THEN
    errcode = 101  !  a hard error
    CALL ereport(RoutineName, errcode,                                         &
                 "No value for criver")
  ELSE IF ( criver <= 0.0 ) THEN
    errcode = 101
    CALL ereport(RoutineName, errcode,                                         &
                 "Surface wave river speed must be > 0")
  END IF

  IF ( ABS( retl - rmdi ) < EPSILON(1.0) ) THEN
    errcode = 101  !  a hard error
    CALL ereport(RoutineName, errcode,                                         &
                 "No value for retl")
  ELSE IF ( retl < -1.0 .OR. retl > 1.0 ) THEN
    errcode = 101
    CALL ereport(RoutineName, errcode,                                         &
                 "retl must be in the range -1 to 1")
  END IF

  IF ( ABS( retr - rmdi ) < EPSILON(1.0) ) THEN
    errcode = 101  !  a hard error
    CALL ereport(RoutineName, errcode,                                         &
                 "No value for retr")
  ELSE IF ( retr < -1.0 .OR. retr > 1.0 ) THEN
    errcode = 101
    CALL ereport(RoutineName, errcode,                                         &
                 "retr must be in the range -1 to 1")
  END IF

  IF ( ABS( runoff_factor - rmdi ) < EPSILON(1.0) ) THEN
    errcode = 101  !  a hard error
    CALL ereport(RoutineName, errcode,                                         &
                 "No value for runoff_factor")
  ELSE IF ( runoff_factor <= 0.0 ) THEN
    errcode = 101
    CALL ereport(RoutineName, errcode,                                         &
                 "Runoff factor must be > 0")
  END IF

CASE ( rivers_trip, rivers_um_trip )
  !---------------------------------------------------------------------------
  ! TRIP and UM TRIP parameters.
  !---------------------------------------------------------------------------

  IF ( ABS( rivers_meander - rmdi ) < EPSILON(1.0) ) THEN
    errcode = 101  !  a hard error
    CALL ereport(RoutineName, errcode,                                         &
                 "No value for rivers_meander")
  ELSE IF ( rivers_meander <= 0.0 ) THEN
    errcode = 101
    CALL ereport(RoutineName, errcode,                                         &
                 "Meander ratio must be > 0")
  END IF

  IF ( ABS( rivers_speed - rmdi ) < EPSILON(1.0) ) THEN
    errcode = 101  !  a hard error
    CALL ereport(RoutineName, errcode,                                         &
                 "No value for rivers_speed")
  ELSE IF ( rivers_speed <= 0.0 ) THEN
    errcode = 101
    CALL ereport(RoutineName, errcode,                                         &
                 "River speed must be > 0")
  END IF

  SELECT CASE ( i_river_vn )

  CASE ( rivers_um_trip )
    !---------------------------------------------------------------------------
    ! UM TRIP only parameters.
    !---------------------------------------------------------------------------
    IF ( lake_water_conserve_method < 1 .OR.                                   &
         lake_water_conserve_method > 2 ) THEN
      errcode = 101
      CALL ereport(RoutineName, errcode,                                       &
                   "lake_water_conserve_method must be 1 or 2")
    END IF

    IF ( trip_globe_shape < 1 .OR.                                             &
         trip_globe_shape > 2 ) THEN
      errcode = 101
      CALL ereport(RoutineName, errcode,                                       &
                   "trip_globe_shape must be 1 or 2")
    END IF

  CASE ( rivers_trip )
    !---------------------------------------------------------------------------
    ! Standalone TRIP only parameters.
    !---------------------------------------------------------------------------
    IF ( lake_water_conserve_method /= imdi ) THEN
      errcode = 101
      CALL ereport(RoutineName, errcode,                                       &
                   "lake_water_conserve_method is not used in standalone")
    END IF

    IF ( trip_globe_shape /= imdi ) THEN
      errcode = 101
      CALL ereport(RoutineName, errcode,                                       &
                   "trip_globe_shape is not used in standalone")
    END IF
  END SELECT

CASE DEFAULT
  errcode = 101
  CALL ereport(RoutineName, errcode,                                           &
               'Unrecognised river routing algorithm (i_river_vn)')
END SELECT

IF ( l_irrig_limit .AND. ( i_river_vn /= rivers_trip ) ) THEN
  errcode = 101
  CALL ereport(RoutineName, errcode,                                           &
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
WRITE(lineBuffer,*)' l_riv_overbank = ',l_riv_overbank
CALL jules_print('jules_rivers',lineBuffer)
WRITE(lineBuffer,*)' trip_globe_shape = ',trip_globe_shape
CALL jules_print('jules_rivers',lineBuffer)
WRITE(lineBuffer,*)' i_river_vn = ',i_river_vn
CALL jules_print('jules_rivers',lineBuffer)
WRITE(lineBuffer,*)' nstep_rivers = ',nstep_rivers
CALL jules_print('jules_rivers',lineBuffer)
WRITE(lineBuffer,'(A,I0)') ' lake_water_conserve_method = ',                   &
                           lake_water_conserve_method
CALL jules_print(ModuleName,lineBuffer)

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

#if !defined(UM_JULES) && !defined(LFRIC)
SUBROUTINE read_nml_jules_rivers(nml_dir)

USE io_constants, ONLY: namelist_unit

USE string_utils_mod, ONLY: to_string

USE logging_mod, ONLY: log_info, log_fatal

IMPLICIT NONE

! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists
! Work variables
INTEGER :: ERROR  ! Error indicator

CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_NML_JULES_RIVERS'

!-----------------------------------------------------------------------------
! Read river routing namelist
!----------------------------------------------------------------------------
CALL log_info(RoutineName, "Reading JULES_RIVERS namelist...")

! Open the river routing parameters namelist file
OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'jules_rivers.nml'),         &
               STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR)
IF ( ERROR /= 0 ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 "Error opening namelist file jules_rivers.nml " //            &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // ")")
END IF

READ(namelist_unit, NML = jules_rivers, IOSTAT = ERROR)
IF ( ERROR /= 0 ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 "Error reading namelist JULES_RIVERS " //                     &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // ")")
END IF

CLOSE(namelist_unit, IOSTAT = ERROR)
IF ( ERROR /= 0 ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 "Error closing namelist file jules_rivers.nml " //            &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // ")")
END IF

END SUBROUTINE read_nml_jules_rivers
#endif

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
INTEGER, PARAMETER :: n_int = 5
INTEGER, PARAMETER :: n_real = 9
INTEGER, PARAMETER :: n_log = 3

TYPE :: my_namelist
  SEQUENCE
  INTEGER :: nstep_rivers
  INTEGER :: trip_globe_shape
  INTEGER :: a_thresh
  INTEGER :: i_river_vn
  INTEGER :: lake_water_conserve_method
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
  LOGICAL :: l_riv_overbank
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
  my_nml % trip_globe_shape = trip_globe_shape
  my_nml % retl = retl
  my_nml % retr = retr
  my_nml % rivers_meander = rivers_meander
  my_nml % rivers_speed = rivers_speed
  my_nml % l_rivers = l_rivers
  my_nml % l_inland = l_inland
  my_nml % l_riv_overbank = l_riv_overbank
  my_nml % lake_water_conserve_method = lake_water_conserve_method
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
  trip_globe_shape = my_nml % trip_globe_shape
  retl = my_nml % retl
  retr = my_nml % retr
  a_thresh = my_nml % a_thresh
  rivers_meander = my_nml % rivers_meander
  rivers_speed = my_nml % rivers_speed
  l_rivers = my_nml % l_rivers
  l_inland = my_nml % l_inland
  l_riv_overbank = my_nml % l_riv_overbank
  lake_water_conserve_method = my_nml % lake_water_conserve_method
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
!!!DEALLOCATE(rivers_data%map_river_to_land_points)
!!!DEALLOCATE(rivers_data%global_land_index)
!!!DEALLOCATE(rivers_data%rfm_iarea_rp)
!!!DEALLOCATE(rivers_data%rfm_land_rp)
!!!DEALLOCATE(rivers_data%rivers_index_rp)
!!!DEALLOCATE(rivers_data%rivers_next_rp)
!!!DEALLOCATE(rivers_data%rivers_seq_rp)
!!!DEALLOCATE(rivers_data%rivers_sto_rp)
!!!DEALLOCATE(rivers_data%rivers_lat_rp)
!!!DEALLOCATE(rivers_data%rivers_lon_rp)
!!!DEALLOCATE(rivers_data%rivers_ilat_rp)
!!!DEALLOCATE(rivers_data%rivers_ilon_rp)
!!!DEALLOCATE(rivers_data%rivers_x_coord_rp)
!!!DEALLOCATE(rivers_data%rivers_y_coord_rp)
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
!!!rivers%map_river_to_land_points => rivers_data%map_river_to_land_points
!!!rivers%global_land_index => rivers_data%global_land_index
!!!rivers%rfm_iarea_rp => rivers_data%rfm_iarea_rp
!!!rivers%rfm_land_rp => rivers_data%rfm_land_rp
!!!rivers%rivers_index_rp => rivers_data%rivers_index_rp
!!!rivers%rivers_next_rp => rivers_data%rivers_next_rp
!!!rivers%rivers_seq_rp => rivers_data%rivers_seq_rp
!!!rivers%rivers_sto_rp => rivers_data%rivers_sto_rp
!!!rivers%rivers_lat_rp => rivers_data%rivers_lat_rp
!!!rivers%rivers_lon_rp => rivers_data%rivers_lon_rp
!!!rivers%rivers_ilat_rp => rivers_data%rivers_ilat_rp
!!!rivers%rivers_ilon_rp => rivers_data%rivers_ilon_rp
!!!rivers%rivers_x_coord_rp => rivers_data%rivers_x_coord_rp
!!!rivers%rivers_y_coord_rp => rivers_data%rivers_y_coord_rp
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
!!!NULLIFY(rivers%map_river_to_land_points)
!!!NULLIFY(rivers%global_land_index)
!!!NULLIFY(rfm_iarea_rp)
!!!NULLIFY(rfm_land_rp)
!!!NULLIFY(rivers_index_rp)
!!!NULLIFY(rivers_next_rp)
!!!NULLIFY(rivers_seq_rp)
!!!NULLIFY(rivers_sto_rp)
!!!NULLIFY(rivers_lat_rp)
!!!NULLIFY(rivers_lon_rp)
!!!NULLIFY(rivers_ilat_rp)
!!!NULLIFY(rivers_ilon_rp)
!!!NULLIFY(rivers_x_coord_rp)
!!!NULLIFY(rivers_y_coord_rp)
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

FUNCTION calc_outflow_per_river(rivers) RESULT(outflow_per_river)

USE jules_print_mgr, ONLY: jules_print, jules_message

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Calculates river outflow for each river.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Arguments with intent(in)
TYPE(rivers_type), INTENT(IN) :: rivers

! Return type
REAL :: outflow_per_river(n_rivers)
  ! River outflow into the ocean for each river (kg s-1)
  ! Used in coupling to ocean model

! Local variables
INTEGER :: ip

CHARACTER(LEN=*), PARAMETER :: RoutineName='CALC_OUTFLOW_PER_RIVER'

outflow_per_river(:) = 0.0
DO ip = 1, np_rivers
  IF (rivers%rivers_outflow_number_rp(ip) > 0) THEN
    outflow_per_river(rivers%rivers_outflow_number_rp(ip)) =                   &
       outflow_per_river(rivers%rivers_outflow_number_rp(ip)) +                &
       rivers%rivers_outflow_rp(ip)
  END IF
END DO

END FUNCTION calc_outflow_per_river

END MODULE jules_rivers_mod

!------------------------------------------------------------------------------
