!****************************************************************************
!  NORTRIP_fortran_control_v2.f90 
!****************************************************************************
!
!   PROGRAMME:      NORTRIP_fortran_control_v2
!   PURPOSE:        Main programme for running multiroad NORTRIP
!   CALLED FROM:    
!   CALLS TO:       set_constant_string_values
!                   read_NORTRIP_commandline
!                   read_NORTRIP_pathnames
!                   read_NORTRIP_parameters
!                   read_NORTRIP_flags
!                   read_NORTRIP_activities
!                   read_NORTRIP_inputdata
!                   check_NORTRIP_inputdata
!                   allocate_NORTRIP_arrays
!                   NORTRIP_initialise_time
!                   NORTRIP_main_run
!                   NORTRIP_save_init_data
!                   NORTRIP_save_episode_emissions
!                   NORTRIP_save_road_meteo_data
!                   NORTRIP_save_road_emission_and_mass_data
!                   NORTRIP_save_road_summary_data
!                   NORTRIP_save_road_emission_and_mass_data_stat
!                   call NORTRIP_save_data
!                   deallocate_NORTRIP_arrays           
!   SUBROUTINES:
!   FUNCTIONS:                      
!   VERSION:        14.10.2017
!   AUTHOR:         Bruce Rolstad Denby 
!                   Norwegian Meteorological Institute (www.met.no)
!
!****************************************************************************

    subroutine NORTRIP_fortran_control_v2

    use NORTRIP_definitions

    implicit none

    logical :: init_exists 

    write(*,'(A)') ''
    write(*,'(A)') '################################################################'
    write(*,'(A)') 'Starting program NORTRIP_fortran_v3.6 (64 bit)'
    write(*,'(A)') '################################################################'
    

    !NOTE: When use_single_road_loop_flag=.true. then cannot use the following routine:
    !NORTRIP_save_all_data
    
    !Initialise and set constants
    call set_constant_string_values

    if (.not.NORTRIP_fortran_combined_flag) then
        !Read in commandline, input paths and input parameters
        if (unit_logfile.gt.0) write(*,'(A)') 'Reading parameters' 
        call read_NORTRIP_commandline
        call read_NORTRIP_pathnames !Opens logfile here
    endif
    
    call read_NORTRIP_parameters
    call read_NORTRIP_flags
    call read_NORTRIP_activities
    
    !Read in and check the input data
    if (unit_logfile.gt.0) write(*,'(A)') 'Reading inputdata'      
    if (.not.NORTRIP_fortran_combined_flag) then
        call read_NORTRIP_inputdata        
    endif
    call check_NORTRIP_inputdata
    call override_NORTRIP_inputdata
    
    !Set which file types to save based on the calculation_type
    call set_NORTRIP_save_file_flags

    if ( calculation_type=="Avinor" ) then
        call read_NORTRIP_runway_info
    end if
 
    !Reassign input arrays to save memory by running NORTRIP one road at a time
    if (use_single_road_loop_flag) then
        n_roads_total=n_roads
        n_roads_end=0
        n_roads_start=0
        n_roads=0
    else
        !Normal calculation without single road loop
        n_roads_total=1
        n_roads_end=n_roads
        n_roads_start=1   
    endif

    !Allocate arrays based on input data
    call allocate_NORTRIP_arrays

    !Open netCDF file for reading init data. The file will be open 
    !during the whole simulation.
    if ( save_init_data_as_netcdf_flag.eq.1 .and. use_single_road_loop_flag) then
        call open_NETCDF_init_file(ncid_init,init_exists)
    end if
    
    !Call main run time and road loop
    if (unit_logfile.gt.0) write(*,'(A)') 'Starting calculations'
    
    do ro_tot=1,n_roads_total

        !Reassign input arrays to save memory
        call NORTRIP_reassign_input_arrays

        !Set start and stop times
        call NORTRIP_initialise_time
    
        call NORTRIP_main_run
        
        if (unit_logfile.gt.0.and.ro_tot.eq.1) write(*,'(A)') 'Saving data'   
        if (NORTRIP_save_init_data_flag.and..not.use_single_road_loop_flag .and. save_init_data_as_netcdf_flag.eq.0) call NORTRIP_save_init_data
        if (NORTRIP_save_episode_emissions_flag) call NORTRIP_save_episode_emissions
        if (NORTRIP_save_episode_grid_emissions_flag) call NORTRIP_save_episode_grid_emissions
        if (NORTRIP_save_road_meteo_data_flag) call NORTRIP_save_road_meteo_data
        if (NORTRIP_save_road_emission_and_mass_data_flag) call NORTRIP_save_road_emission_and_mass_data
        if (NORTRIP_save_road_summary_data_flag) then
            if ( save_road_summary_data_as_netcdf_flag.eq.0) then
                call NORTRIP_save_road_summary_data
            else if (save_road_summary_data_as_netcdf_flag.eq.1) then
                call NORTRIP_save_road_summary_data_netcdf
            else if (save_road_summary_data_as_netcdf_flag.eq.2) then
                call NORTRIP_save_road_summary_data_netcdf
                call NORTRIP_save_road_summary_data
            else
                write(*,*) "Warning: Do not write summary files, bc. save_road_summary_data_as_netcdf_flag = ", save_road_summary_data_as_netcdf_flag
            end if 
        end if          
        if (NORTRIP_save_road_emission_activity_data_flag) call NORTRIP_save_road_emission_activity_data
        if (NORTRIP_save_road_emission_and_mass_data_stats_flag) call NORTRIP_save_road_emission_and_mass_data_stats
        if (NORTRIP_save_all_data_flag) call NORTRIP_save_all_data
        if (NORTRIP_save_uEMEP_emissions_flag) call NORTRIP_save_uEMEP_emissions_all
        if (NORTRIP_save_uEMEP_grid_emissions_flag) call NORTRIP_save_uEMEP_grid_emissions
    
    enddo

    if (NORTRIP_save_init_data_flag.eq.1 .and. init_exists ) then
        call close_NETCDF_file(ncid_init)
    end if
        
    call deallocate_NORTRIP_arrays
    
    write(*,'(A)') ''
    write(*,'(A)') '################################################################'
    write(*,'(A)') 'Finished program NORTRIP_fortran'
    write(*,'(A)') '################################################################'
    write(*,'(A)') ''
    write(*,'(A)') ''

    !Close log file that was opened in read_NORTRIP_pathnames
    call close_logfile    

    !Save finished file
    if (trim(finished_filename).ne.'') then
        write(*,'(A)') '################################################################'
        write(*,'(A)') 'Writing finished file to uEMEP emission output'
        open(unit_save_emissions,file=finished_filename,status='replace')
        close(unit_save_emissions)
        write(*,'(A)') '################################################################'
    endif

    end subroutine NORTRIP_fortran_control_v2
    
    

