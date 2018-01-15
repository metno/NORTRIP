!NORTRIP_reassign_input_arrays.f90
    
!This routine is used to reassign input arrays so that the rest of the model thinks there is only one road
    subroutine NORTRIP_reassign_input_arrays
    
    use NORTRIP_definitions
    
    implicit none
    
    if (.not.use_single_road_loop_flag) return
    
  	!write(unit_logfile,'(A)') ''
    !write(unit_logfile,'(A)') 'Reassigning input arrays for single road run (NORTRIP_reassign_input_arrays)'
  	!write(unit_logfile,'(A)') '================================================================'
   
        traffic_data(:,:,0)=traffic_data(:,:,ro_tot)
        meteo_data(:,:,0)=meteo_data(:,:,ro_tot)
        airquality_data(:,:,0)=airquality_data(:,:,ro_tot)
        activity_data(:,:,0)=activity_data(:,:,ro_tot)
        
        d_index(0)=d_index(ro_tot)
        p_index(0)=p_index(ro_tot)
        b_road(0)=b_road(ro_tot)
        n_lanes(0)=n_lanes(ro_tot)
        b_road_lanes(0)=b_road_lanes(ro_tot)
        b_lane(0)=b_lane(ro_tot)
        b_canyon(0)=b_canyon(ro_tot)
        h_canyon(:,0)=h_canyon(:,ro_tot)
        ang_road(0)=ang_road(ro_tot)
        slope_road(0)=slope_road(ro_tot)
        roadtype_index(0)=roadtype_index(ro_tot)

        LAT(0)=LAT(ro_tot)
        LON(0)=LON(ro_tot)
        Z_SURF(0)=Z_SURF(ro_tot)
        z_FF(0)=z_FF(ro_tot)
        z_T(0)=z_T(ro_tot)
        z2_T(0)=z2_T(ro_tot)
        albedo_road(0)=albedo_road(ro_tot)
        DIFUTC_H(0)=DIFUTC_H(ro_tot)
        Pressure(0)=Pressure(ro_tot)

        !Correction factors
        wind_speed_correction(0)=wind_speed_correction(ro_tot)
        h_sus(0)=h_sus(ro_tot)
        h_texture(0)=h_texture(ro_tot)
    
        !OSPM factors
        choose_receptor_ospm(0)=choose_receptor_ospm(ro_tot)
        SL1_ospm(0)=SL1_ospm(ro_tot)
        SL2_ospm(0)=SL2_ospm(ro_tot)
        f_roof_ospm(0)=f_roof_ospm(ro_tot)
        RecHeight_ospm(0)=RecHeight_ospm(ro_tot)
        f_turb_ospm(0)=f_turb_ospm(ro_tot)
   
        !Single factors
        observed_moisture_cutoff_value(0)=observed_moisture_cutoff_value(ro_tot)
        road_ID(0)=road_ID(ro_tot)
        save_road_data_flag(0)=save_road_data_flag(ro_tot)

        !Emisison factors (num_veh,n_road)
        exhaust_EF(:,0)=exhaust_EF(:,ro_tot)
        NOX_EF(:,0)=NOX_EF(:,ro_tot)

        !Grid data
        x_road(:,0)=x_road(:,ro_tot)
        y_road(:,0)=y_road(:,ro_tot)
        length_road(0)=length_road(ro_tot)
        line_or_grid_data_flag(0)=line_or_grid_data_flag(ro_tot)
        !adt_road(0)=

        M_road_init(:,:,:,0)=M_road_init(:,:,:,ro_tot)
        g_road_init(:,:,0)=g_road_init(:,:,ro_tot)

        road_type_activity_flag(:,0)=road_type_activity_flag(:,ro_tot)

        az_skyview(:,0)=az_skyview(:,ro_tot)
        zen_skyview(:,0)=zen_skyview(:,ro_tot)
               
    !Initialise all arrays to 0
    !M_road_data=0.0
    !M_road_bin_data=0.0
    !M_road_bin_balance_data=0.0
    !M_road_balance_data=0.0
    !C_bin_data=0.0
    !C_data=0.0
    !E_road_data=0.0
    !E_road_bin_data=0.0
    !WR_time_data=0.0
    !road_salt_data=0.0
    !road_meteo_data=0.0
    !g_road_balance_data=0.0
    !g_road_data=0.0
    !f_q=0.0
    !f_q_obs=0.0
    
    !azimuth_ang=0.0
    !zenith_ang=0.0
    
    end subroutine NORTRIP_reassign_input_arrays
    