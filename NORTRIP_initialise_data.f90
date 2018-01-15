
    subroutine NORTRIP_initialise_data
    !Transfers and initialises some data before the model run
    
    use NORTRIP_definitions
    
    implicit none
    
    if (ro_tot.eq.1) then
  	write(unit_logfile,'(A)') ''
    write(unit_logfile,'(A)')'Initialising variables (NORTRIP_main_run/NORTRIP_initialise_data)'
  	write(unit_logfile,'(A)') '================================================================'
    endif
    
    !Start the road loop
    do ro=n_roads_start,n_roads_end
        
        !Transfer observed meteo data to road_meteo_data
        !----------------------------------------------------------------------
        do tr=1,num_track
            road_meteo_data(road_temperature_obs_index,:,tr,ro) = meteo_data(road_temperature_obs_input_index,:,ro)
            road_meteo_data(road_wetness_obs_index,:,tr,ro) = meteo_data(road_wetness_obs_input_index,:,ro)
        enddo

        !Initialise T_sub if input available, otherwise set it to the atmospheric temperature
        !Will be updated again in NORTRIP_running_mean_temperature
        !----------------------------------------------------------------------
        do tr=1,num_track
            if (available_meteo_data(T_sub_input_index)) then
                road_meteo_data(T_sub_index,:,tr,ro)=meteo_data(T_sub_input_index,:,ro)
            else
                road_meteo_data(T_sub_index,:,tr,ro)=meteo_data(T_a_index,:,ro)
            endif
        enddo

        !Initialise T_s and RH_s with the atmospheric temperature and humidity
        !Will be overwritten in NORTRIP_read_init_data if data is available
        !----------------------------------------------------------------------
        do tr=1,num_track
            road_meteo_data(T_s_index,min_time,tr,ro)=meteo_data(T_a_index,min_time,ro)
            road_meteo_data(RH_s_index,min_time,tr,ro)=meteo_data(RH_index,min_time,ro)
        enddo

        !Set or transfer exhaust emission into all sizes and all tracks.
        !Only pm2.5 is used when placed in the model in NORTRIP_dust_emission_submodel
        !The values set here will be overwritten to be correct for multitrack cases as well
        !----------------------------------------------------------------------
        if (exhaust_EF_available.eq.1.and.exhaust_flag.gt.0) then
            E_road_data(exhaust_index,:,E_total_index,:,:,ro)=0.
            do x=1,num_size
            do tr=1,num_track
            do v=1,num_veh
                !E_road_bin_data(exhaust_index,x,E_total_index,:,tr,ro)= &
                !E_road_bin_data(exhaust_index,x,E_total_index,:,tr,ro) &
                !+traffic_data(N_v_index(v),:,ro)*exhaust_EF(v,ro)
                E_road_data(exhaust_index,x,E_total_index,:,tr,ro)= &
                E_road_data(exhaust_index,x,E_total_index,:,tr,ro) &
                +traffic_data(N_v_index(v),:,ro)*exhaust_EF(v,ro)
            enddo
            enddo
            enddo
        elseif (available_airquality_data(EP_emis_index).and.exhaust_flag.gt.0) then
            do x=1,num_size
            do tr=1,num_track
    	        E_road_data(exhaust_index,x,E_total_index,:,tr,ro)=airquality_data(EP_emis_index,:,ro)
            enddo
            enddo
        endif
    
        !Initialise the mass balance specified through M_road_init
        !----------------------------------------------------------------------
        M_road_data(:,:,min_time,:,ro)=M_road_init(:,:,:,ro)
        g_road_data(:,min_time,:,ro)=g_road_init(:,:,ro)
    
        !Set pressure to default if not available
        !----------------------------------------------------------------------
        if (.not.available_meteo_data(pressure_index)) then
            meteo_data(pressure_index,:,ro)=Pressure(ro)
        endif

        !Implement wind speed correction
        !----------------------------------------------------------------------
        meteo_data(FF_index,:,ro)=meteo_data(FF_index,:,ro)*wind_speed_correction(ro)

        !Add LW rad, RH and temperature offsets
        !----------------------------------------------------------------------
        meteo_data(RH_index,:,ro)=max(0.,min(100.,meteo_data(RH_index,:,ro)+RH_offset))
        meteo_data(T_a_index,:,ro)=meteo_data(T_a_index,:,ro)+T_a_offset
        meteo_data(long_rad_in_index,:,ro)= meteo_data(long_rad_in_index,:,ro)+long_rad_in_offset
 
        !Initialise the activity last time variables
        !Will be overwritten by init files if they are read in
        !----------------------------------------------------------------------
        time_since_last_salting(ro)=0.
        time_since_last_binding(ro)=0.
        time_since_last_sanding(ro)=0.
        time_since_last_cleaning(ro)=0.
        time_since_last_ploughing(ro)=0.
    

    enddo !End the road loop
    
    if (ro_tot.eq.1) then
    write(unit_logfile,'(A32,f12.2)')'Wind speed correction factor ', sum(wind_speed_correction)/size(wind_speed_correction,1)
    write(unit_logfile,'(A32,f12.2)')'RH offset ', RH_offset
    write(unit_logfile,'(A32,f12.2)')'Temperature offset ', T_a_offset
    write(unit_logfile,'(A32,f12.2)')'Longwave radiation offset ', long_rad_in_offset
    write(unit_logfile,'(A)') '----------------------------------------------------------------'
    endif
    
    end subroutine NORTRIP_initialise_data
    