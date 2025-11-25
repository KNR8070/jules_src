#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE fill_model_grid_arrays_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE fill_model_grid_arrays( ainfo_data, coastal_data )

USE model_grid_mod, ONLY: latitude, longitude,                                 &
                           latitude_of_land_pts, longitude_of_land_pts

USE coastal, ONLY: flandg

USE theta_field_sizes, ONLY: t_i_length, t_j_length

USE ancil_info, ONLY: land_pts

!TYPE definitions
USE ancil_info,    ONLY: ainfo_data_type
USE coastal,       ONLY: coastal_data_type

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Fills previously allocated model grid arrays with data
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
!Arguments
!TYPES containing field data (IN OUT)
TYPE(ainfo_data_type),   INTENT(IN OUT) :: ainfo_data
TYPE(coastal_data_type), INTENT(IN OUT) :: coastal_data

INTEGER :: i, j, l  ! Index variables

ainfo_data%land_mask(:,:) = ( flandg(:,:) > EPSILON(1.0) )

ALLOCATE(latitude_of_land_pts(land_pts))
ALLOCATE(longitude_of_land_pts(land_pts))

l = 0
DO j = 1,t_j_length
  DO i = 1,t_i_length
    IF ( ainfo_data%land_mask(i,j) ) THEN
      l = l + 1
      ainfo_data%land_index(l) = (j-1) * t_i_length + i
      coastal_data%fland(l) = flandg(i,j)
      latitude_of_land_pts(l)  = latitude(i,j)
      longitude_of_land_pts(l) = longitude(i,j)
    END IF
  END DO
END DO

! Initialise ocn_cpl_point as false for standalone JULES
! (i.e. not coupled to ocean)
ainfo_data%ocn_cpl_point(:,:) = .FALSE.

RETURN

END SUBROUTINE fill_model_grid_arrays

END MODULE fill_model_grid_arrays_mod
#endif
