! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE init_ancillaries_coupling_mod

USE logging_mod, ONLY: log_fatal, log_error, log_warn

IMPLICIT NONE

CONTAINS

!##############################################################################

SUBROUTINE read_ancillaries_coupling( riv_number_file, rivers, rivers_data)

USE jules_rivers_mod,         ONLY: nx_rivers, ny_rivers,                      &
                                    rivers_type, rivers_data_type
USE grid_utils_mod,           ONLY: grid_create, grid_info
USE input_mod,                ONLY: dummy_grid => grid
USE io_constants,             ONLY: max_file_name_len
USE fill_variables_from_file_mod, ONLY: fill_variables_from_file

!-------------------------------------------------------------------------------
IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Fills river number ancillary.
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN)
!------------------------------------------------------------------------------
CHARACTER(LEN=max_file_name_len), INTENT(IN) :: riv_number_file
                      ! The name of the file given for reading river number
                      ! at each grid point

!------------------------------------------------------------------------------
! Arguments with INTENT(IN OUT)
!------------------------------------------------------------------------------
TYPE(rivers_type), INTENT(IN OUT) :: rivers
TYPE(rivers_data_type), INTENT(IN OUT), TARGET :: rivers_data

!------------------------------------------------------------------------------
! Local variables
!------------------------------------------------------------------------------
TYPE(grid_info) :: local_grid

CHARACTER(LEN=*), PARAMETER  :: RoutineName = 'READ_ANCILLARIES_COUPLING'

!end of header
!------------------------------------------------------------------------------

! Read river number ancillary.

! Temporarily copy saved grid (set for full model grid) to a local variable
! before overwriting to define the river number grid
local_grid = grid_create(dummy_grid%is_1d,dummy_grid%dim_name,                 &
   dummy_grid%nx,dummy_grid%x_name,dummy_grid%nx,                              &
   dummy_grid%y_name,dummy_grid%ny)
dummy_grid = grid_create( .FALSE., "", 0, 'longitude', nx_rivers,              &
   'latitude', ny_rivers)

CALL fill_variables_from_file(TRIM(riv_number_file),                           &
                            [ 'rivers_outflow_number' ],[ 'river_number' ])

! Reset saved JULES grid from river to full land model
dummy_grid = grid_create(local_grid%is_1d,local_grid%dim_name,                 &
                         local_grid%nx,local_grid%x_name,local_grid%nx,        &
                         local_grid%y_name,local_grid%ny)


RETURN

END SUBROUTINE read_ancillaries_coupling

!##############################################################################

SUBROUTINE check_ancil_rivers( dir_mouth, l_ignore_ancil_rivers_check,         &
                               direction_grid, rivers, rivers_data )

USE jules_rivers_mod, ONLY: nx_rivers, ny_rivers, n_rivers,                    &
                            rivers_type, rivers_data_type

USE missing_data_mod, ONLY: imdi
USE string_utils_mod, ONLY: to_string
USE jules_print_mgr,  ONLY: jules_message

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  dir_mouth
    ! The value of the flow direction field that indicates a river mouth.

LOGICAL, INTENT(IN) :: l_ignore_ancil_rivers_check
    ! Switch to check river routing & river number ancillary for compatibility.

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  direction_grid(nx_rivers,ny_rivers)
    ! Flow direction.

!------------------------------------------------------------------------------
! Arguments with INTENT(IN OUT)
!------------------------------------------------------------------------------
TYPE(rivers_type), INTENT(IN OUT) :: rivers
TYPE(rivers_data_type), INTENT(IN OUT), TARGET :: rivers_data

!------------------------------------------------------------------------------
! Local variables
!------------------------------------------------------------------------------
INTEGER :: i, ERROR

INTEGER, ALLOCATABLE :: check_river_number(:,:), check_river_mouth(:,:),       &
                        match(:,:)

CHARACTER(LEN=*), PARAMETER  :: RoutineName = 'CHECK_ANCIL_RIVERS'

!end of header
!------------------------------------------------------------------------------

n_rivers = NINT( MAXVAL( rivers%rivers_outflow_number(:,:) ) )

IF ( n_rivers == imdi ) THEN
  jules_message = "Error in River number ancillary. " //                       &
                  "Maximum River number is missing data."
  CALL log_fatal( RoutineName, jules_message )
END IF

DO i = 1, n_rivers
  IF ( COUNT ( rivers%rivers_outflow_number(:,:) == i ) == 0 ) THEN
    jules_message = "River numbers are non-consecutive. " //                   &
                    "Missing at least River " // TRIM( to_string(i) )
    CALL log_fatal( RoutineName, jules_message )
  END IF
END DO

! Each grid point with a river outflow number > 0 should be defined as a
! river mouth in the River routing ancillary so check this
ALLOCATE ( check_river_number(nx_rivers,ny_rivers) )
ALLOCATE ( check_river_mouth(nx_rivers,ny_rivers) )
ALLOCATE ( match(nx_rivers,ny_rivers) )
check_river_number(:,:) = 0
check_river_mouth(:,:)  = 0

WHERE ( rivers_data%rivers_outflow_number(:,:) > 0 )
  check_river_number(:,:) = 1
END WHERE

WHERE ( direction_grid(:,:) == dir_mouth  )
  check_river_mouth(:,:)  = 1
END WHERE

match(:,:) = check_river_number(:,:) - check_river_mouth(:,:)

ERROR = COUNT ( match(:,:) /= 0 )

IF ( ERROR /= 0 ) THEN

  WRITE(jules_message, *)                                                      &
     'Gridpoints with a river outflow number' //                               &
     ', but not classified as a river mouth      = ',                          &
     COUNT ( match(:,:) > 0 )
  CALL log_error(RoutineName, jules_message)

  WRITE(jules_message, *)                                                      &
     'Gridpoints classified as a river mouth' //                               &
     ', but does not have a river outflow number = ',                          &
     ABS ( COUNT ( match(:,:) < 0 ) )
  CALL log_error(RoutineName, jules_message)

  WRITE(jules_message,*)                                                       &
     'River routing & coupling ancillaries are not consistent'

  IF ( l_ignore_ancil_rivers_check ) THEN
    CALL log_warn(RoutineName, jules_message)
    CALL log_warn(RoutineName,                                                 &
       'Setting river number to zero where river mouths are not classified.')
    ! Ensures missing data numbers in gridboxes, which are not classified as
    ! river mouths, are not used in outflow_per_river calculation.
    WHERE ( match(:,:) > 0 )
      rivers_data%rivers_outflow_number(:,:) = 0.0
    END WHERE
  ELSE
    CALL log_fatal(RoutineName, jules_message)
  END IF

END IF

DEALLOCATE( check_river_number, check_river_mouth, match )

END SUBROUTINE check_ancil_rivers

END MODULE init_ancillaries_coupling_mod
