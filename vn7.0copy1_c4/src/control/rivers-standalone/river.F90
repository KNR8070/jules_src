#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


PROGRAM river

!$ USE omp_lib

USE init_mod, ONLY: init

USE next_time_mod, ONLY: next_time

USE io_constants, ONLY: max_file_name_len

USE time_varying_input_mod, ONLY:                                              &
  update_prescribed_variables => update_model_variables,                       &
  input_close_all => close_all

USE output_mod, ONLY: output_initial_data, sample_data, output_data,           &
                       output_close_all => close_all

USE model_time_mod, ONLY: timestep, end_of_run

USE jules_print_mgr, ONLY: jules_message, jules_print

USE control_mod, ONLY: control

!TYPE definitions
USE jules_fields_mod, ONLY: psparms_data, psparms,                             &
                            ainfo_data, ainfo,                                 &
                            progs_data, progs,                                 &
                            trifctl_data, trifctltype,                         &
                            coastal_data, coast,                               &
                            fluxes_data, fluxes,                               &
                            rivers_data, rivers

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   This is the main program routine for standalone Rivers
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Work variables
CHARACTER(LEN=max_file_name_len) :: nml_dir  ! Directory containing namelists

INTEGER :: ERROR  ! Error indicator


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! Initialise the MPI environment
!-----------------------------------------------------------------------------
! We don't check the error since most (all?) MPI implementations will just
! fail if a call is unsuccessful
CALL mpi_init(ERROR)

!-----------------------------------------------------------------------------
! If OpenMP is in use provide an information message to make sure the
! user is aware.
!-----------------------------------------------------------------------------
!$ WRITE(jules_message, '(A, I3, A)') 'Using OpenMP with up to ',              &
!$                                        OMP_get_max_threads(), ' thread(s)'
!$ CALL jules_print('jules', jules_message)

!-----------------------------------------------------------------------------
! Try to read a single argument from the command line
!
! If present, that single argument will be the directory we try to read
! namelists from
! If not present, we use current working directory instead
!-----------------------------------------------------------------------------
CALL GET_COMMAND_ARGUMENT(1, nml_dir)
! If no argument is given, GET_COMMAND_ARGUMENT returns a blank string
IF ( LEN_TRIM(nml_dir) == 0 ) nml_dir = "."

!-----------------------------------------------------------------------------
! Initialise the model
!-----------------------------------------------------------------------------
CALL init(nml_dir,                                                             &
          psparms_data, psparms,                                               &
          ainfo, ainfo_data,                                                   &
          progs, progs_data,                                                   &
          coastal_data, coast,                                                 &
          fluxes_data, fluxes,                                                 &
          rivers_data, rivers                                                  &
          )

!-----------------------------------------------------------------------------
! Loop over timesteps.
! Note that the number of timesteps is of unknown length at the start of run,
! if the model is to determine when it has spun up.
!-----------------------------------------------------------------------------
DO    !  timestep

  !-----------------------------------------------------------------------------
  ! The update of prescribed data is done in two phases
  !  - Update variables provided by files
  !-----------------------------------------------------------------------------
  CALL update_prescribed_variables()

  !-----------------------------------------------------------------------------
  ! Check if this is a timestep that we need to output initial data for (i.e.
  ! start of spinup cycle or start of main run), and output that data if
  ! required
  !-----------------------------------------------------------------------------
  CALL output_initial_data()

  !-----------------------------------------------------------------------------
  ! Call the main model science routine
  !-----------------------------------------------------------------------------
  CALL control(                                                                &
  !   Scalar arguments (INTENT IN)
     timestep,                                                                 &
     !TYPES containing field data (IN OUT)
     psparms, ainfo, progs, fluxes, rivers )

  !-----------------------------------------------------------------------------
  ! Sample variables for output
  !-----------------------------------------------------------------------------
  CALL sample_data()

  !-----------------------------------------------------------------------------
  ! Output collected data if required
  !-----------------------------------------------------------------------------
  CALL output_data()

  !-----------------------------------------------------------------------------
  ! Move the model on to the next timestep
  !-----------------------------------------------------------------------------
  CALL next_time(progs,trifctltype)

  IF ( end_of_run ) EXIT

END DO  !  timestep loop

!-----------------------------------------------------------------------------
! Clean up by closing all open files
!-----------------------------------------------------------------------------
CALL input_close_all()
CALL output_close_all()

!-----------------------------------------------------------------------------
! Clean up the MPI environment
!-----------------------------------------------------------------------------
CALL mpi_finalize(ERROR)


END PROGRAM river
#endif
