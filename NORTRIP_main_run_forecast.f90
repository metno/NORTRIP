!****************************************************************************
!  NORTRIP_main_run_forecast.f90 
!****************************************************************************
!
!   SUBROUTINE:     NORTRIP_main_run_forecast
!   PURPOSE:        Controls the main road and time loop of NORTRIP but with a surface temperature forecast
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
!   VERSION:        14.10.2016
!   AUTHOR:         Bruce Rolstad Denby 
!                   Norwegian Meteorological Institute (www.met.no)
!
!****************************************************************************

    subroutine NORTRIP_main_run_forecast
  
    use NORTRIP_definitions
    
    implicit none
    
    !Declare internal logical variables for showing results
    logical :: show_time_moisture=.false.
    logical :: show_time_dust=.true. !Not implemented
    
    !Local forecast variables
    real, allocatable :: forecast_T_s(:,:)
    integer tf,forecast_index
    real :: flux_correction=0.
    real :: bias_correction=0.

    !Open log file for main run. Already established in NORTRIP_read_pathnames
    !if (unit_logfile.gt.0) then
    !    open(unit_logfile,file=filename_log,status='old',position='append')
    !endif

    write(unit_logfile,'(A)') ''
    write(unit_logfile,'(A)') '================================================================'
    write(unit_logfile,'(A)') 'Starting calculations (NORTRIP_main_run_forecast)' 
  	write(unit_logfile,'(A)') '================================================================'
    
    if (.not.allocated(forecast_T_s)) allocate(forecast_T_s(n_time,num_track))   
    
    !Precalculate radiation for all roads
    call NORTRIP_calc_radiation
        
    !Initialising some data for all times and roads
    call NORTRIP_initialise_data

    !Read in init file and reinitialise. If not available then nothing happens
    if (.not.use_single_road_loop_flag) then
        call NORTRIP_read_init_data
    endif
    
    !Calculate running mean (sub_surf_average_time) temperature if T_sub is not already available
    !call NORTRIP_running_mean_temperature(sub_surf_average_time)
    
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
        if (mod(ro,10000).eq.0) then
        if (unit_logfile.gt.0) then
            write(*,'(A6,I5)') 'ROAD: ',ro
        else
            write(unit_logfile,'(A6,I5)') 'ROAD: ',ro
        endif
        endif
        
        !Main time loop
        !----------------------------------------------------------------------
        if (ro.eq.1.or.ro.eq.n_roads) then
            write(unit_logfile,'(A)')'Starting time loop (NORTRIP_main_run_forecast)'
        endif

       forecast_T_s=nodata
       
       do tf=min_time,max_time 

            !Set the previous (initial) model surface temperature to the observed surface temperature in forecast mode
            !Do not do this if bias correction is used for the forecast
            forecast_index=max(0,forecast_hour-1);
            if (forecast_hour.gt.0.and.forecast_type.ne.4.and.forecast_type.ne.5) then
                tr=1       
                if (road_meteo_data(road_temperature_obs_index,max(min_time,tf-1),tr,ro).ne.nodata) then
                   road_meteo_data(T_s_index,max(min_time,tf-1),:,ro)=road_meteo_data(road_temperature_obs_index,max(min_time,tf-1),:,ro)
                   !write(*,*) tf,max(min_time,tf-1),road_meteo_data(road_temperature_obs_index,max(min_time,tf-1),tr,ro)
                endif
            endif
            !Bias correction
            bias_correction=0.
            if (forecast_hour.gt.0.and.forecast_type.eq.4) then
                tr=1       
                if (road_meteo_data(road_temperature_obs_index,max(min_time,tf-1),tr,ro).ne.nodata) then
                   bias_correction=-(road_meteo_data(T_s_index,max(min_time,tf-1),tr,ro)-road_meteo_data(road_temperature_obs_index,max(min_time,tf-1),tr,ro))
                   !write(*,*) ro,tf,bias_correction
                endif
            endif
            !Flux correction. Estimate for now
            if (forecast_hour.gt.0.and.forecast_type.eq.5) then
                tr=1       
                if (road_meteo_data(road_temperature_obs_index,max(min_time,tf-1),tr,ro).ne.nodata) then
                   bias_correction=(road_meteo_data(T_s_index,max(min_time,tf-1),tr,ro)-road_meteo_data(road_temperature_obs_index,max(min_time,tf-1),tr,ro))
                   if (bias_correction.gt.0.) flux_correction=-bias_correction*60./dt !76.9
                   if (bias_correction.lt.0.) flux_correction=-bias_correction*60./dt !14.9
                   !write(*,*) tf,max(min_time,tf-1),road_meteo_data(road_temperature_obs_index,max(min_time,tf-1),tr,ro)
                   meteo_data(long_rad_in_index,tf:tf+forecast_index,ro)=meteo_data(long_rad_in_index,tf:tf+forecast_index,ro)+flux_correction !min(1000.,max(-1000.,flux_correction))
                   !write(*,*) ro,tf,min(500.,max(-500.,flux_correction)),bias_correction
                endif
            endif


            do ti=tf,tf+forecast_index
      
                if (ti.le.max_time) then
                
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
                            write(unit_logfile,'(a24,a2,f8.2,f8.1,f8.2,f8.2,f8.2,f8.2,f8.2)') trim(date_str(3,ti)),': ' &
                            ,meteo_data(T_a_index,ti,ro),meteo_data(RH_index,ti,ro),road_meteo_data(T_s_index,ti,tr,ro),road_meteo_data(T_s_dewpoint_index,ti,tr,ro) &
                            ,g_road_data(water_index,ti,tr,ro),g_road_data(snow_index,ti,tr,ro),g_road_data(ice_index,ti,tr,ro)
                        endif

                        !Calculate road emissions and dust loading
                        call NORTRIP_dust_emission_submodel
                
                    end do
                    !End main track loop
                    !----------------------------------------------------------------------
      
                    !Redistribute mass and moisture between tracks. Not yet implemented
      
                    !Put the binned variables in the unbinned ones 
                    call NORTRIP_unbin_variables
            
                endif
            
            enddo

            !Save the forecast surface temperature into the +forecast index if the starting surface temperature was valid
            tr=1
            if (forecast_hour.gt.0.and.tf+forecast_index.le.max_time.and.road_meteo_data(road_temperature_obs_index,max(min_time,tf-1),tr,ro).ne.nodata) then
                !modelled
                if (forecast_type.eq.1) then
                    forecast_T_s(min(max_time,tf+forecast_index),:)=road_meteo_data(T_s_index,min(max_time,tf+forecast_index),:,ro)
                    !write(*,*) max_time,tf+forecast_index,min(max_time,tf+forecast_index),forecast_T_s(min(max_time,tf+forecast_index),:)
                endif
                !persistence
                if (forecast_type.eq.2) then
                    forecast_T_s(min(max_time,tf+forecast_index),:)=road_meteo_data(T_s_index,max(tf-1,min_time),:,ro);
                endif
                !linear extrapolation
                if (forecast_type.eq.3.and.tf.ge.min_time+2.and.road_meteo_data(T_s_index,tf-2,tr,ro).ne.nodata) then
                    forecast_T_s(min(max_time,tf+forecast_index),:)=road_meteo_data(T_s_index,tf-1,:,ro) &
                        +(road_meteo_data(T_s_index,tf-1,:,ro)-road_meteo_data(T_s_index,tf-2,:,ro)) &
                        /(date_data(datenum_index,tf-1)-date_data(datenum_index,tf-2)) &
                        *(date_data(datenum_index,tf+forecast_index)-date_data(datenum_index,tf-1))
                elseif (forecast_type.eq.3) then
                    forecast_T_s(min(max_time,tf+forecast_index),:)=nodata
                endif
                !bias correction
                if (forecast_type.eq.4) then
                    forecast_T_s(min(max_time,tf+forecast_index),:)=road_meteo_data(T_s_index,min(max_time,tf+forecast_index),:,ro)+bias_correction
                endif
                !flux correction, just transfer results
                if (forecast_type.eq.5) then
                    !Remove correction
                    meteo_data(long_rad_in_index,tf:tf+forecast_index,ro)=meteo_data(long_rad_in_index,tf:tf+forecast_index,ro)-flux_correction
                    forecast_T_s(min(max_time,tf+forecast_index),:)=road_meteo_data(T_s_index,min(max_time,tf+forecast_index),:,ro)
                    !write(*,*) forecast_T_s(min(max_time,tf+forecast_index),tr)-road_meteo_data(road_temperature_obs_index,min(max_time,tf+forecast_index),tr,ro),forecast_T_s(min(max_time,tf+forecast_index),tr),road_meteo_data(road_temperature_obs_index,min(max_time,tf+forecast_index),tr,ro),bias_correction
                endif
            else 
                forecast_T_s(min(max_time,tf+forecast_index),:)=nodata
            endif

            !write(*,*) tf+forecast_index,forecast_T_s(min(max_time,tf+forecast_index),:)
        enddo
        !End main time loop
        !----------------------------------------------------------------------
    
        !Put forecast surface temperature into the normal road temperature
        if (forecast_hour.gt.0.and.forecast_type.ne.4.and.forecast_type.ne.5) then
            road_meteo_data(T_s_index,min_time:max_time,:,ro)=forecast_T_s(min_time:max_time,:)
        endif

        if (forecast_hour.gt.0.and.(forecast_type.eq.4.or.forecast_type.eq.5)) then
            tr=1
            road_meteo_data(T_s_index,min_time:min_time+forecast_index,:,ro)=nodata
            do tf=min_time+forecast_index,max_time
                if (forecast_T_s(tf,tr).ne.nodata) then
                    road_meteo_data(T_s_index,tf,:,ro)=forecast_T_s(tf,:)
                endif
            enddo
        endif


        if (use_ospm_flag.eq.1) then
            if (ro.eq.1.or.ro.eq.n_roads) then
                write(unit_logfile,'(A)') 'Calculating dispersion using OSPM'
            endif
            !call ospm_nortrip_control
        elseif (available_airquality_data(f_conc_index)) then
            if (ro.eq.1.or.ro.eq.n_roads) then
                write(unit_logfile,'(A)') 'Calculating dispersion using input dispersion factor'
            endif
            !call NORTRIP_dispersion
        else
            if (ro.eq.1.or.ro.eq.n_roads) then
                write(unit_logfile,'(A)') 'Calculating dispersion using NOX'
            endif
            call NORTRIP_dispersion
        endif

        !Calculate concentrations
        if (ro.eq.1.or.ro.eq.n_roads) then
            write(unit_logfile,'(A)')'Calculating concentrations'
        endif
        call NORTRIP_concentrations

    end do
    !End road loop
    !--------------------------------------------------------------------------
            
    !Write summary results to the log file for track=1 and the first and last road
    do ro=n_roads_start,n_roads_end
        if (ro.eq.1.or.ro.eq.n_roads.or.ro_tot.eq.1.or.ro_tot.eq.n_roads_total) then
            write(unit_logfile,'(A)') '================================================================'
            write(unit_logfile,'(A,8I)') 'Displaying summary results for road = ',ro 
            write(unit_logfile,'(A)') '================================================================'           
            call NORTRIP_display_results
        endif
    enddo
            
    write(unit_logfile,'(A)') ''
    write(unit_logfile,'(A)') '================================================================'
    write(unit_logfile,'(A)') 'Finished calculations (NORTRIP_main_run)' 
  	write(unit_logfile,'(A)') '================================================================'

    !Close the log file
    !if (unit_logfile.gt.0) then
    !    close(unit_logfile,status='keep')
    !endif

    deallocate (forecast_T_s)
    
    end subroutine NORTRIP_main_run_forecast