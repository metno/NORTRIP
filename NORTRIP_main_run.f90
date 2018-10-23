!****************************************************************************
!  NORTRIP_main_run.f90 
!****************************************************************************
!
!   SUBROUTINE:     NORTRIP_main_run
!   PURPOSE:        Controls the main road and time loop of NORTRIP
!   CALLED FROM:    NORTRIP_fortran_control
!   CALLS TO:       NORTRIP_calc_radiation
!                   NORTRIP_initialise_data
!                   NORTRIP_read_init_data
!                   NORTRIP_running_mean_temperature
!                   NORTRIP_set_activity_data
!                   NORTRIP_surface_moisture_submodel
!                   NORTRIP_dust_emission_submodel
!                   NORTRIP_unbin_variables
!                   NORTRIP_ospm (not implemented)
!                   NORTRIP_dispersion
!                   NORTRIP_concentrations
!                   NORTRIP_display_results
!   SUBROUTINES:
!   FUNCTIONS:                      
!   VERSION:        14.10.2015
!   AUTHOR:         Bruce Rolstad Denby 
!                   Norwegian Meteorological Institute (www.met.no)
!
!****************************************************************************

    subroutine NORTRIP_main_run
  
    use NORTRIP_definitions
    
    implicit none
    
    !Declare internal logical variables for showing results
    logical :: show_time_moisture=.false.
    logical :: show_time_dust=.false.

    !Open log file for main run. Already established in NORTRIP_read_pathnames
    !if (unit_logfile.gt.0) then
    !    open(unit_logfile,file=filename_log,status='old',position='append')
    !endif

    if (ro_tot.eq.1) then
    write(unit_logfile,'(A)') ''
    write(unit_logfile,'(A)') '================================================================'
    write(unit_logfile,'(A)') 'Starting calculations (NORTRIP_main_run)' 
  	write(unit_logfile,'(A)') '================================================================'
    endif
    
    !Precalculate radiation for all roads
    call NORTRIP_calc_radiation
        
    !Initialising some data for all times and roads
    call NORTRIP_initialise_data

    !Read in init file and reinitialise. If not available then nothing happens
    !Does not work for single road application, version 'single' is used for this which is called later
    if (.not.use_single_road_loop_flag) then
        call NORTRIP_read_init_data
    endif
    
    
    !Main road loop
    !----------------------------------------------------------------------
    do ro=n_roads_start,n_roads_end
    
        !Read in init file and reinitialise. If not available then nothing happens
        if (use_single_road_loop_flag) then
            call NORTRIP_read_init_data_single
        endif
        
        !Calculate running mean (sub_surf_average_time) temperature if T_sub is not already available
        call NORTRIP_running_mean_temperature(sub_surf_average_time)
        
        !Print road to screen to see progress
        if ((mod(ro,10000).eq.0.and..not.use_single_road_loop_flag).or.(mod(ro_tot,10000).eq.0.and.use_single_road_loop_flag)) then
        if (unit_logfile.gt.0) then
            write(*,'(A6,2I9)') 'ROAD: ',ro,ro_tot
        else
            write(unit_logfile,'(A6,2I9)') 'ROAD: ',ro,ro_tot
        endif
        endif
        
        !Main time loop
        !----------------------------------------------------------------------
        if (((ro.eq.1.or.ro.eq.n_roads).and..not.use_single_road_loop_flag).or.((ro_tot.eq.1.or.ro_tot.eq.n_roads_total).and.use_single_road_loop_flag)) then
            write(unit_logfile,'(A)')'Starting time loop (NORTRIP_main_run)'
        endif
        
        do ti=min_time,max_time
      
            !Print the day date. Not active
            if (date_data(hour_index,ti).eq.1) then
                !write(unit_logfile,'(I5,I3,I3)') date_data(year_index,ti),date_data(month_index,ti),date_data(day_index,ti)
            endif
              
            !Use activity rules to determine salting, sanding and cleaning activities
            call NORTRIP_set_activity_data
        
            !Main track loop
            !----------------------------------------------------------------------
            do tr=1,num_track
         
                !Calculate road surface conditions
                call NORTRIP_surface_moisture_submodel
                
                !For debugging purposes only
                if (show_time_moisture) then
                    write(unit_logfile,'(a24,a2,2i8,f8.2,f8.1,f8.2,f8.2,f8.2,f8.2,f8.2,f8.2)') trim(date_str(3,ti)),': ',ro,ti &
                    ,meteo_data(T_a_index,ti,ro),meteo_data(RH_index,ti,ro),road_meteo_data(T_s_index,ti,tr,ro),road_meteo_data(T_sub_index,ti,tr,ro),road_meteo_data(T_s_dewpoint_index,ti,tr,ro) &
                    ,g_road_data(water_index,ti,tr,ro),g_road_data(snow_index,ti,tr,ro),g_road_data(ice_index,ti,tr,ro)
                endif
                
                !Calculate road emissions and dust loading
                call NORTRIP_dust_emission_submodel
                if (show_time_dust) then
                    write(unit_logfile,'(a24,a2,2i8,f8.2,f8.1,f8.2,f8.2,f8.2,f8.2,f8.2,f8.2)') trim(date_str(3,ti)),': ',ro,ti &
                    ,road_meteo_data(H_index,ti,tr,ro),road_meteo_data(L_index,ti,tr,ro),road_meteo_data(G_index,ti,tr,ro),road_meteo_data(G_sub_index,ti,tr,ro),road_meteo_data(evap_index,ti,tr,ro) &
                    ,road_meteo_data(RH_s_index,ti,tr,ro),M_road_bin_data(road_index,pm_200,ti_bin,tr,ro_bin),M_road_bin_data(salt_index(1),pm_all,ti_bin,tr,ro_bin)
                endif
            end do
            !End main track loop
            !----------------------------------------------------------------------

            !Redistribute mass and moisture between tracks. Not yet implemented
      
            !Put the binned variables in the unbinned ones 
            call NORTRIP_unbin_variables
            
            !If the single road loop is used then save the init files here
            if (use_single_road_loop_flag) then
                call NORTRIP_save_init_data_single
            endif
            
        end do
        !End main time loop
        !----------------------------------------------------------------------
    
        !Only calculate concentrations for the special road links if required. NOT IMPLEMENTED YET
        if (save_road_data_flag(ro).ne.0) then
        endif
        
        if (use_ospm_flag.eq.1) then
            if (((ro.eq.1.or.ro.eq.n_roads).and..not.use_single_road_loop_flag).or.((ro_tot.eq.1.or.ro_tot.eq.n_roads_total).and.use_single_road_loop_flag)) then
                write(unit_logfile,'(A)') 'Calculating dispersion using OSPM'
            endif
            call ospm_nortrip_control
        elseif (available_airquality_data(f_conc_index)) then
            if (((ro.eq.1.or.ro.eq.n_roads).and..not.use_single_road_loop_flag).or.((ro_tot.eq.1.or.ro_tot.eq.n_roads_total).and.use_single_road_loop_flag)) then
                write(unit_logfile,'(A)') 'Calculating dispersion using input dispersion factor'
            endif
            !call NORTRIP_dispersion
        else
            if (((ro.eq.1.or.ro.eq.n_roads).and..not.use_single_road_loop_flag).or.((ro_tot.eq.1.or.ro_tot.eq.n_roads_total).and.use_single_road_loop_flag)) then
                write(unit_logfile,'(A)') 'Calculating dispersion using NOX'
            endif
            call NORTRIP_dispersion
        endif

        !Calculate concentrations
        if (((ro.eq.1.or.ro.eq.n_roads).and..not.use_single_road_loop_flag).or.((ro_tot.eq.1.or.ro_tot.eq.n_roads_total).and.use_single_road_loop_flag)) then
            write(unit_logfile,'(A)')'Calculating concentrations'
        endif
        call NORTRIP_concentrations
        
        
    enddo
    !End road loop
    !--------------------------------------------------------------------------
            
    !Write summary results to the log file for track=1 and the first and last road
    do ro=n_roads_start,n_roads_end
        if (((ro.eq.1.or.ro.eq.n_roads).and..not.use_single_road_loop_flag).or.((ro_tot.eq.1.or.ro_tot.eq.n_roads_total).and.use_single_road_loop_flag)) then
            write(unit_logfile,'(A)') '================================================================'
            write(unit_logfile,'(A,8I)') 'Displaying summary results for road = ',ro 
            write(unit_logfile,'(A)') '================================================================'           
            call NORTRIP_display_results
        endif
    enddo
            
    if (ro_tot.eq.n_roads_total) then
    write(unit_logfile,'(A)') ''
    write(unit_logfile,'(A)') '================================================================'
    write(unit_logfile,'(A)') 'Finished calculations (NORTRIP_main_run)' 
  	write(unit_logfile,'(A)') '================================================================'
    endif
    
    !Close the log file
    !if (unit_logfile.gt.0) then
    !    close(unit_logfile,status='keep')
    !endif

    end subroutine NORTRIP_main_run