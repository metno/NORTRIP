!****************************************************************************
!  NORTRIP_fortran_control_v1.f90 
!****************************************************************************
!
!   PROGRAMME:      NORTRIP_fortran_control_v1
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
!                   NORTRIP_save_road_emission_and_mass_data_stat
!                   call NORTRIP_save_data
!                   deallocate_NORTRIP_arrays           
!   SUBROUTINES:
!   FUNCTIONS:                      
!   VERSION:        14.10.2015
!   AUTHOR:         Bruce Rolstad Denby 
!                   Norwegian Meteorological Institute (www.met.no)
!
!****************************************************************************

    program NORTRIP_fortran_control_v1

    use NORTRIP_definitions
    
    implicit none
 
 	write(*,'(A)') ''
 	write(*,'(A)') '################################################################'
	write(*,'(A)') 'Starting program NORTRIP_fortran_v3.4 (64 bit)'
	write(*,'(A)') '################################################################'
    
    !Initialise and set constants
    call set_constant_string_values

	!Read in commandline, input paths and input parameters
    if (unit_logfile.gt.0) write(*,'(A)') 'Reading parameters'
    call read_NORTRIP_commandline
    call read_NORTRIP_pathnames
    call read_NORTRIP_parameters
    call read_NORTRIP_flags
    call read_NORTRIP_activities
    
    !Read in and check the input data
    if (unit_logfile.gt.0) write(*,'(A)') 'Reading inputdata'        
    call read_NORTRIP_inputdata
    call check_NORTRIP_inputdata
    call set_NORTRIP_save_file_flags
 
    !Allocate arrays based on input data
    call allocate_NORTRIP_arrays
    
    !Set start and stop times
    call NORTRIP_initialise_time
    
	!Call main run time and road loop
    if (unit_logfile.gt.0) write(*,'(A)') 'Starting calculations'
    
    if (forecast_hour.eq.0) call NORTRIP_main_run
    if (forecast_hour.gt.0) call NORTRIP_main_run_forecast
        
    if (unit_logfile.gt.0) write(*,'(A)') 'Saving data'
    
    if (NORTRIP_save_init_data_flag) call NORTRIP_save_init_data
    if (NORTRIP_save_episode_emissions_flag) call NORTRIP_save_episode_emissions
    if (NORTRIP_save_episode_grid_emissions_flag) call NORTRIP_save_episode_grid_emissions
    if (NORTRIP_save_road_meteo_data_flag) call NORTRIP_save_road_meteo_data
    if (NORTRIP_save_road_emission_and_mass_data_flag) call NORTRIP_save_road_emission_and_mass_data
    if (NORTRIP_save_road_emission_and_mass_data_stats_flag) call NORTRIP_save_road_emission_and_mass_data_stats
    if (NORTRIP_save_all_data_flag) call NORTRIP_save_all_data
    
    !Save emissions, initi data, summary road meteo and summary emission and mass data for Bedre Byluft calculation type
    if (trim(calculation_type).eq.'Bedre byluft') then
        if (unit_logfile.gt.0) write(*,'(A)') 'Saving Bedre byluft emission and initial files'
        call NORTRIP_save_init_data
        call NORTRIP_save_episode_emissions
        call NORTRIP_save_episode_grid_emissions
        call NORTRIP_save_road_meteo_data
        call NORTRIP_save_road_emission_and_mass_data
        !call NORTRIP_save_road_emission_and_mass_data_stats
    endif

    !Save summary road meteo and summary emission and mass data for Bedre Byluft calculation type
    if (trim(calculation_type).eq.'SMHI') then
        if (unit_logfile.gt.0) write(*,'(A)') 'Saving SMHI data'
        call NORTRIP_save_init_data
        call NORTRIP_save_road_meteo_data
        call NORTRIP_save_road_emission_and_mass_data
        call NORTRIP_save_all_data
    endif

    !Save summary road meteo and summary emission and mass data for Bedre Byluft calculation type
    if (trim(calculation_type).eq.'Road weather') then
        if (unit_logfile.gt.0) write(*,'(A)') 'Saving road weather data'
        call NORTRIP_save_init_data
        call NORTRIP_save_road_meteo_data
        call NORTRIP_save_road_emission_and_mass_data
    endif

    if (trim(calculation_type).eq.'SLB') then
        if (unit_logfile.gt.0) write(*,'(A)') 'Saving SLB data'
        call NORTRIP_save_init_data
        call NORTRIP_save_road_emission_and_mass_data_stats
    endif

    !Save complete data in ascii for 'normal' calculation type
    if (trim(calculation_type).eq.'Normal') then
        call NORTRIP_save_all_data
    endif
    
    !Close log file
    
    call deallocate_NORTRIP_arrays
    
 	write(*,'(A)') ''
 	write(*,'(A)') '################################################################'
	write(*,'(A)') 'Finished program NORTRIP_fortran'
	write(*,'(A)') '################################################################'
	write(*,'(A)') ''
	write(*,'(A)') ''

    end program NORTRIP_fortran_control_v1
    
    

