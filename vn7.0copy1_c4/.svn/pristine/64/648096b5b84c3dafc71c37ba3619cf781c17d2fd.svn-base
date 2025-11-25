#if defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Description:
!     Driver and science routines for calculating river flow routing
!     using a kinematic wave model
!     RFM: see Bell et al. 2007 Hydrol. Earth Sys. Sci. 11. 532-549
!     TRIP: see Oki et al 1999 J.Met.Soc.Japan, 77, 235-255.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

MODULE um_riv_to_jules_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

PRIVATE  !  private scope by default
PUBLIC um_riv_to_jules, jules_riv_to_um

CONTAINS

!#############################################################################
! subroutine um_riv_to_jules
! Conversion routine for UM variables to JULES runoff routing

SUBROUTINE um_riv_to_jules(g_p_field, global_river_row_length,                 &
                           global_river_rows, land_points, land_index,         &
                           delta_lambda, delta_phi, a_boxareas,                &
                           r_area, flowobs1, r_inext,r_jnext,                  &
                           substore,surfstore,flowin,bflowin)

USE UM_ParVars,        ONLY: glsize
USE Field_Types,       ONLY: fld_type_p

USE theta_field_sizes, ONLY: t_i_length, t_j_length

USE atm_land_sea_mask, ONLY: global_land_pts => atmos_number_of_landpts
USE um_parallel_mod,   ONLY: is_master_task, um_gather_field
USE um_latlon_mod,     ONLY: global_land_index

USE jules_rivers_mod,  ONLY:                                                   &
  !  imported scalar parameters
  a_thresh, np_rivers, river_mouth, rivers_dlat, rivers_dlon, rivers_rfm       &
  ,rivers_first, rivers_dx, i_river_vn                                         &
  ,rivers_regrid, sea, rfm_land, rfm_river, rfm_sea                            &
! types
  , rivers_type, rivers_data_type

USE conversions_mod,          ONLY: recip_pi_over_180
USE planet_constants_mod,     ONLY: planet_radius

USE ereport_mod,              ONLY:                                            &
  ereport

IMPLICIT NONE

INTEGER, INTENT(IN) ::                                                         &
  land_points                                                                  &
                ! number of landpoints
  ,land_index (land_points)                                                    &
                ! index of land to global points
  ,g_p_field                                                                   &
                ! size of global ATMOS field
  ,global_river_row_length                                                     &
                ! size of global RIV field
  ,global_river_rows
                ! size of global RIV field

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  delta_lambda, delta_phi
                ! RCM gridsize (radians)

! ancillary variables for river routing model
! should be defined on river_row_length, river_rows
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  r_area(t_i_length,t_j_length)                                                &
                ! accumulated areas file
  ,r_inext(t_i_length,t_j_length)                                              &
                ! x-coordinate offset to downstream grid pt
  ,r_jnext(t_i_length,t_j_length)                                              &
                ! y-coordinate offset to downstream grid pt
  ,flowobs1(t_i_length,t_j_length)                                             &
                ! optional initialisation for flows
  ,a_boxareas(t_i_length,t_j_length)
                ! ATMOS gridbox areas

! prognostic variables for river routing model
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  substore(t_i_length,t_j_length)                                              &
                ! routing sub_surface store (mm)
  ,surfstore(t_i_length,t_j_length)                                            &
                ! routing surface store (mm)
  ,flowin(t_i_length,t_j_length)                                               &
                ! surface lateral inflow (mm)
  ,bflowin(t_i_length,t_j_length)
                ! sub-surface lateral inflow (mm)

! Local parameters.
REAL(KIND=real_jlslsm), PARAMETER :: global_lon_threshold = 359.9
  ! Threshold value used when determining if a grid is cyclic (global) in the
  ! longitudinal direction (degrees). This should be close to 360 degrees.

! local variables
INTEGER :: i,j,k,l,gf,ip
INTEGER :: inext, jnext, global_row_length
INTEGER :: ERROR, error_sum, errcode

LOGICAL :: l_cyclic_x
  ! Flag indicating a river grid that is cyclic (global) in the x direction.

INTEGER, ALLOCATABLE :: mapfr(:,:)                   ! map full to river

! Gathering and Scattering variables:
REAL(KIND=real_jlslsm) :: gather_r_area(g_p_field)
                ! global field of accumulated area
REAL(KIND=real_jlslsm) :: gather_r_inext(g_p_field)
                ! global field of x-flow directions
REAL(KIND=real_jlslsm) :: gather_r_jnext(g_p_field)
                ! global field of y-flow directions
REAL(KIND=real_jlslsm) :: gather_flowobs1(g_p_field)
                ! global field initial flow values
REAL(KIND=real_jlslsm) :: gather_substore(g_p_field)
                ! global field of surface storage
REAL(KIND=real_jlslsm) :: gather_surfstore(g_p_field)
                ! global field sub-surface storage
REAL(KIND=real_jlslsm) :: gather_flowin(g_p_field)
                ! global field of flowin
REAL(KIND=real_jlslsm) :: gather_bflowin(g_p_field)
                ! global field of bflowin
REAL(KIND=real_jlslsm) :: gather_boxareas(g_p_field)
                ! field for gather to grid box areas
! types
TYPE(rivers_type) :: rivers
TYPE(rivers_data_type), TARGET :: rivers_data

! 2.  Initialise river routing parameters
!-----------------------------------------------------------------------------
! River routing algorithm
! Lat/lon grid spacing (degrees)
rivers_dlat = delta_phi * recip_pi_over_180
rivers_dlon = delta_lambda * recip_pi_over_180

! Regular grid spacing (m)
! rivers_dx is read from namelist jules_rivers_props in standalone mode;
! in UM it will have to be properly calculated for RFM
! The value of 1500 is what is needed for UKV
rivers_dx = planet_radius * delta_phi

! Flag to specify if regridding required from land to river grids
rivers_regrid = .FALSE.

! 3.   Compute number of valid river points
!-----------------------------------------------------------------------------
np_rivers = global_land_pts     ! assume 1:1 if not regrid

! 4.   Allocate river routing variables
!-----------------------------------------------------------------------------
IF ( rivers_first ) THEN
  IF ( is_master_task() ) THEN

    ALLOCATE( mapfr(global_river_row_length,global_river_rows), STAT = ERROR )
    error_sum = ERROR
    ALLOCATE(rivers_data%rfm_flowobs1_rp(np_rivers),    STAT = ERROR )
    error_sum = ERROR
    ALLOCATE(rivers_data%rivers_next_rp(np_rivers),     STAT = ERROR )
    error_sum = error_sum + ERROR
    ALLOCATE(rivers_data%rfm_iarea_rp(np_rivers),       STAT = ERROR )
    error_sum = error_sum +ERROR
    ALLOCATE(rivers_data%rfm_land_rp(np_rivers),        STAT = ERROR )
    error_sum = error_sum + ERROR
    ALLOCATE(rivers_data%rivers_sto_rp(np_rivers),      STAT = ERROR )
    error_sum = error_sum + ERROR
    ALLOCATE(rivers_data%rivers_boxareas_rp(np_rivers), STAT = ERROR )
    error_sum = error_sum + ERROR
    ALLOCATE(rivers_data%rfm_rivflow_rp(np_rivers),     STAT = ERROR )
    error_sum = error_sum + ERROR
    ALLOCATE(rivers_data%il_river_grid(np_rivers),      STAT = ERROR )
    error_sum = error_sum + ERROR
    ALLOCATE(rivers_data%rfm_surfstore_rp(np_rivers),   STAT = ERROR )
    error_sum = error_sum + ERROR
    ALLOCATE(rivers_data%rfm_substore_rp(np_rivers),    STAT = ERROR )
    error_sum = error_sum + ERROR
    ALLOCATE(rivers_data%rfm_flowin_rp(np_rivers),      STAT = ERROR )
    error_sum = error_sum + ERROR
    ALLOCATE(rivers_data%rfm_bflowin_rp(np_rivers),     STAT = ERROR )
    error_sum = error_sum + ERROR
    ALLOCATE(rivers_data%rfm_baseflow_rp(np_rivers),    STAT = ERROR )
    error_sum = error_sum + ERROR

    IF ( error_sum /= 0 ) THEN
      errcode = 150
      CALL ereport("um_riv_to_jules", errcode,                                 &
                   "Error allocating JULES model arrays")
    END IF

    DO ip = 1, np_rivers
      rivers_data%rfm_land_rp(ip)      = 0
      rivers_data%rfm_iarea_rp(ip)     = 0

      rivers_data%rfm_flowobs1_rp(ip)  = 0.0
      rivers_data%rfm_surfstore_rp(ip) = 0.0
      rivers_data%rfm_substore_rp(ip)  = 0.0
      rivers_data%rfm_flowin_rp(ip)    = 0.0
      rivers_data%rfm_bflowin_rp(ip)   = 0.0
      rivers_data%rfm_rivflow_rp(ip)   = 0.0
      rivers_data%rfm_baseflow_rp(ip)  = 0.0
      ! Initialise rivers_data%rivers_next_rp to indicate that the point is sea.
      rivers_data%rivers_next_rp(ip)   = sea
      rivers_data%il_river_grid(ip)    = 0
      rivers_data%rivers_sto_rp(ip)    = 0.0
    END DO

    ! Associate pointers in one block here to make them easier to unpick and move
    ! back into jules_rivers_mod later
    rivers%il_river_grid => rivers_data%il_river_grid
    rivers%ir_land_grid => rivers_data%ir_land_grid
    rivers%rfm_iarea_rp => rivers_data%rfm_iarea_rp
    rivers%rfm_land_rp => rivers_data%rfm_land_rp
    rivers%rivers_index_rp => rivers_data%rivers_index_rp
    rivers%rivers_next_rp => rivers_data%rivers_next_rp
    rivers%rivers_sto_rp => rivers_data%rivers_sto_rp
    rivers%rfm_flowobs1_rp => rivers_data%rfm_flowobs1_rp
    rivers%rfm_surfstore_rp => rivers_data%rfm_surfstore_rp
    rivers%rfm_substore_rp => rivers_data%rfm_substore_rp
    rivers%rfm_flowin_rp => rivers_data%rfm_flowin_rp
    rivers%rfm_bflowin_rp => rivers_data%rfm_bflowin_rp
    rivers%rfm_rivflow_rp => rivers_data%rfm_rivflow_rp
    rivers%rfm_baseflow_rp => rivers_data%rfm_baseflow_rp
    rivers%rivers_boxareas_rp => rivers_data%rivers_boxareas_rp

    DO j = 1, global_river_rows
      DO i = 1, global_river_row_length
        mapfr(i,j) = 0
      END DO
    END DO

  END IF       !! end master_task
END IF    !! end rivers_first

! 5.   Gather river routing ancillary and prognostic variables
!-----------------------------------------------------------------------------
CALL um_gather_field(r_area,     gather_r_area)
CALL um_gather_field(flowobs1,   gather_flowobs1)
CALL um_gather_field(r_inext,    gather_r_inext)
CALL um_gather_field(r_jnext,    gather_r_jnext)
CALL um_gather_field(a_boxareas, gather_boxareas)
CALL um_gather_field(substore,   gather_substore)
CALL um_gather_field(surfstore,  gather_surfstore)
CALL um_gather_field(flowin,     gather_flowin)
CALL um_gather_field(bflowin,    gather_bflowin)

! 6.   Translate gathered variables to river points vector variables
!-----------------------------------------------------------------------------
IF ( rivers_first ) THEN
  IF ( is_master_task() ) THEN

    ! Get row length.
    global_row_length = glsize(1,fld_type_p)

    DO l = 1,global_land_pts
      gf = global_land_index(l)
      CALL index_to_coords( gf, global_row_length, i, j )

      rivers%rfm_iarea_rp(l)       = NINT(gather_r_area(gf))
      rivers%rfm_substore_rp(l)    = gather_substore(gf)
      rivers%rivers_boxareas_rp(l) = gather_boxareas(gf)
      rivers%rfm_surfstore_rp(l)   = gather_surfstore(gf)
      rivers%rfm_flowin_rp(l)      = gather_flowin(gf)
      rivers%rfm_bflowin_rp(l)     = gather_bflowin(gf)
      rivers%rfm_flowobs1_rp(l)    = gather_flowobs1(gf)

      mapfr(i,j) = l      ! full 2D rivgrid to rivpts

      ! Note that some of these river points (which are currently the
      ! global_land_pts) could be marked as sea in the river ancillary.

    END DO

    !-------------------------------------------------------------------------
    ! Ensure that sea points are represented consistently in both the "next"
    ! fields and the drainage area. A well-formed ancillary file will be
    ! consistent, but while this code is still being developed we ensure
    ! consistency here - for clarity if nothing else.
    ! If either field indicates sea, ensure both are sea.
    !-------------------------------------------------------------------------
    DO l = 1,global_land_pts
      gf = global_land_index(l)
      ! If either inext or jnext < -1 consider this as sea.
      IF ( gather_r_inext(gf) < -1.0 .OR. gather_r_jnext(gf) < -1.0 .OR.       &
           rivers%rfm_iarea_rp(l) < 0 ) THEN
        ! Set inext and jnext to <-1, and iarea to <0.
        gather_r_inext(gf) = -9.0
        gather_r_jnext(gf) = -9.0
        rivers%rfm_iarea_rp(l)    = -9
      END IF
    END DO

    !-------------------------------------------------------------------------
    ! Decide if the river grid is cyclic (global) in the x direction. This
    ! assumes that we have a grid spacing that is uniform in longitude.
    !-------------------------------------------------------------------------
    l_cyclic_x = .FALSE.
    IF ( REAL(global_river_row_length) * rivers_dlon                           &
         > global_lon_threshold ) THEN
      l_cyclic_x = .TRUE.
    END IF

    !-------------------------------------------------------------------------
    ! Set downstream points in river points vector and identify river mouths.
    ! River mouths are river points (not sea) for which the next point
    ! downstream is not a river point or is a river point that is sea.
    ! Points (not sea) at which zero flow direction is indicated
    ! (inext=0, jnext=0) are also treated as river mouths (which means
    ! conflating river mouths with inland drainage locations if both have
    ! zero flow direction).
    !-------------------------------------------------------------------------
    DO l = 1,global_land_pts
      gf = global_land_index(l)
      CALL index_to_coords( gf, global_row_length, i, j )

      ! We assume that the ancillary file has inext and jnext values >=-1 for
      ! all river points, and <-1 at all other points, including sea.
      ! All values <-1 are considered to indicate a sea point (for which
      ! rivers%rivers_next_rp retains the value with which it was initialised above).
      IF ( gather_r_inext(gf) >= -1.0 .AND. gather_r_jnext(gf) >= -1.0 ) THEN

        ! Get coordinates of the point downstream.
        inext =  i + NINT(gather_r_inext(gf))
        jnext =  j + NINT(gather_r_jnext(gf))

        ! Use a cyclic boundary condition in the x direction (assumed to be
        ! longitude).
        IF ( l_cyclic_x ) THEN
          IF ( inext > global_river_row_length ) THEN
            inext = inext - global_river_row_length
          END IF
          IF ( inext < 1 ) THEN
            inext = inext + global_river_row_length
          END IF
        ELSE IF ( inext < 1 .OR. inext > global_river_row_length ) THEN
          ! Flow is across edge of grid.
          inext = -1
        END IF

        ! Assume grid is not cyclic in the y direction.
        IF ( jnext < 1 .OR. jnext > global_river_rows ) THEN
          ! Flow is across edge of grid.
          jnext = -1
        END IF

        IF ( inext == i .AND. jnext == j ) THEN
          ! There is zero flow direction.
          ! Mark the current point as a river mouth.
          rivers%rivers_next_rp(l) = -river_mouth
        ELSE IF ( inext > 0 .AND. jnext > 0 ) THEN
          ! Next point is on the river grid.
          IF ( mapfr(inext, jnext) > 0 ) THEN
            ! The next point is a river point.
            ! Check if the next point is sea.
            k = inext + (j-1) * global_row_length
            IF ( gather_r_inext(k) >= -1.0 ) THEN
              ! Next point is not sea.
              rivers%rivers_next_rp(l) = mapfr(inext,jnext)
            ELSE
              ! Next point is sea; this is a river mouth.
              rivers%rivers_next_rp(l) = -river_mouth
            END IF
          ELSE
            ! The next point is not a river point.
            ! Mark the current point as a river mouth.
            rivers%rivers_next_rp(l) = -river_mouth
          END IF
        ELSE
          ! This is flow across the edge of the grid.
          ! Set rivers%rivers_next_rp to a value in the range -8 to -1 (as used in
          ! standalone JULES for flow across edge of grid). Note that we could
          ! try harder to mimic the standalone code here and set a value that
          ! codes for the direction of flow (e.g. -1 to -8) - but that is left
          ! for a future development.
          rivers%rivers_next_rp(l) = -1
        END IF

      END IF  !  gather_r_inext

      ! For RFM, use the drained area to identify the type of gridbox.
      ! Later, routing is only done at points that are marked as land or
      ! river, not at sea points.
      IF ( i_river_vn == rivers_rfm ) THEN
        IF (rivers%rfm_iarea_rp(ip) < 0) THEN
          rivers%rfm_land_rp(ip) = rfm_sea    ! Gridcell is sea
        ELSE IF (rivers%rfm_iarea_rp(ip) > a_thresh) THEN
          rivers%rfm_land_rp(ip) = rfm_river  ! Gridcell is river
        ELSE
          rivers%rfm_land_rp(ip) = rfm_land   ! Gridcell is land
        END IF
      END IF

    END DO

    ! At this point all values of rivers%rivers_next_rp are one of the following:
    !    -9 : a river mouth (value = -1*river_mouth)
    !    -1 : flow across the edge of the grid
    !     0 : sea (value = sea)
    !    >0 : next point is a river point.
    ! Nothing more is done at points at which rivers%rivers_next_rp=0.
    ! Note that the standalone JULES code (subroutine init_rivers_props) does
    ! not have rivers%rivers_next_rp=0 because in that code a sea point cannot be a
    ! river point.

    DEALLOCATE(mapfr)

  END IF    !! master task
END IF    !! rivers_first





END SUBROUTINE um_riv_to_jules

!#############################################################################
! subroutine jules_riv_to_um
! Conversion routine for JULES runoff routing variables to UM variables

SUBROUTINE jules_riv_to_um(global_rflow, riverout_atmos, substore, surfstore,  &
                           flowin, bflowin, rivers )

USE rivers_regrid_mod, ONLY: rivpts_to_landpts
USE atm_land_sea_mask, ONLY: global_land_pts => atmos_number_of_landpts
USE theta_field_sizes, ONLY: t_i_length, t_j_length
USE um_parallel_mod,   ONLY: is_master_task, scatter_land2d_field

USE jules_rivers_mod,  ONLY:                                                   &
  !  imported scalar parameters
  np_rivers, rivers_type

IMPLICIT NONE

REAL(KIND=real_jlslsm), INTENT(IN) :: global_rflow(:)
                                           ! River flow diagnostic

! prognostic variables for river routing model
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  riverout_atmos(t_i_length, t_j_length)                                       &
                ! ROUTING RIVER FLOW on atmosphere grid (kg/m2/s)
  ,substore(t_i_length,t_j_length)                                             &
                ! ROUTING SUB_SURFACE STORE (MM)
  ,surfstore(t_i_length,t_j_length)                                            &
                ! ROUTING SURFACE STORE (MM)
  ,flowin(t_i_length,t_j_length)                                               &
                !SURFACE LATERAL INFLOW (MM)
  ,bflowin(t_i_length,t_j_length)
                ! SUB-SURFACE LATERAL INFLOW (MM)

REAL(KIND=real_jlslsm) ::                                                      &
  substore_lp(global_land_pts)                                                 &
                ! ROUTING SUB_SURFACE STORE (MM)
  ,surfstore_lp(global_land_pts)                                               &
                ! ROUTING SURFACE STORE (MM)
  ,flowin_lp(global_land_pts)                                                  &
                !SURFACE LATERAL INFLOW (MM)
  ,bflowin_lp(global_land_pts)
                ! SUB-SURFACE LATERAL INFLOW (MM)

INTEGER :: l    ! Loop counter

! types
TYPE(rivers_type), INTENT(IN OUT) :: rivers

! Initialization
DO l = 1, global_land_pts
  flowin_lp(l)    = 0.0
  bflowin_lp(l)   = 0.0
  surfstore_lp(l) = 0.0
  substore_lp(l)  = 0.0
END DO

IF ( is_master_task() ) THEN
  CALL rivpts_to_landpts( np_rivers, rivers%rfm_flowin_rp, global_land_pts,    &
                          flowin_lp, rivers%il_river_grid, rivers%ir_land_grid,&
                          rivers%rivers_index_rp)
  CALL rivpts_to_landpts( np_rivers, rivers%rfm_bflowin_rp, global_land_pts,   &
                          bflowin_lp, rivers%il_river_grid,                    &
                          rivers%ir_land_grid, rivers%rivers_index_rp)
  CALL rivpts_to_landpts( np_rivers, rivers%rfm_surfstore_rp, global_land_pts, &
                          surfstore_lp, rivers%il_river_grid,                  &
                          rivers%ir_land_grid, rivers%rivers_index_rp)
  CALL rivpts_to_landpts( np_rivers, rivers%rfm_substore_rp, global_land_pts,  &
                          substore_lp, rivers%il_river_grid,                   &
                          rivers%ir_land_grid, rivers%rivers_index_rp)
END IF

! Update river flow diagnostics for UM dump
CALL scatter_land2d_field(global_rflow, riverout_atmos)

! Update prognostics for UM dump
CALL scatter_land2d_field(flowin_lp, flowin)
CALL scatter_land2d_field(bflowin_lp, bflowin)
CALL scatter_land2d_field(surfstore_lp, surfstore)
CALL scatter_land2d_field(substore_lp, substore)

END SUBROUTINE jules_riv_to_um

!#############################################################################

SUBROUTINE index_to_coords( index_value, row_length, i, j )

! Given an index (point number) for a location in a 2-D grid, returns the
! column and row number.

INTEGER, INTENT(IN) ::                                                         &
  index_value,                                                                 &
    ! The index indicating the point number in a 2-D grid.
  row_length
    ! The number of points in each row of the 2-D grid.

INTEGER, INTENT(OUT) ::                                                        &
  i,                                                                           &
    ! Column number (x coordinate) of point in 2-D grid.
  j
    ! Row number (y coordinate) of point in 2-D grid.

j = (index_value-1) / row_length + 1
i = index_value - (j-1) * row_length

END SUBROUTINE index_to_coords

END MODULE um_riv_to_jules_mod
#endif
