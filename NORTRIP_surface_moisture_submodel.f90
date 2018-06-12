!==========================================================================
!NORTRIP model
!SUBROUTINE: NORTRIP_surface_moisture_submodel
!VERSION: 2, 27.06.2012
!AUTHOR: Bruce Rolstad Denby and Ingrid Sundvor (bde@nilu.no)
!DESCRIPTION: Subroutine for calculating surface moisture and retention
!==========================================================================

    subroutine NORTRIP_surface_moisture_submodel
    
    use NORTRIP_definitions
    
    implicit none
    
    !Internal variables that need to be defined
    real :: dz_snow_albedo      !Depth of snow required before implementing snow albedo mm.w.e.
    real :: Z_CLOUD             !Only used when no global radiation is available
    real :: z0t                 !Defines the rougness length for temperature relative to that for momentum
    real length_veh(num_veh)    !Vehicle lengths used to calculate the vehicle heat flux    
    integer :: retain_water_by_snow=1 !Decides if water is allowed to drain off normally when snow is present
    real :: surface_moisture_min=1e-12 !Mimimum allowable total surface wetness 

    !Local arrays and variables
    real :: g_road_0_data(num_moisture)
    real :: S_melt_temp
    real :: g_road_fraction(num_moisture)
    real :: M2_road_salt_0(num_salt)
    real :: R_evaporation(num_moisture)
    real :: R_ploughing(num_moisture)
    real :: R_road_drainage(num_moisture)
    real :: R_spray(num_moisture)
    real :: R_drainage(num_moisture)
    real :: R_total(num_moisture)
    !real :: g_road_temp
    !real :: melt_temperature_salt_temp
    !real :: RH_salt_temp
    !real :: M_road_dissolved_ratio_temp
    real :: T_s_0
    real :: RH_s_0,RH_s_final
    real :: short_rad_net_temp
    real :: g_road_water_drainable
    real :: g_road_drainable_withrain
    real :: g_road_drainable_min_temp
    real :: g_road_total
    real :: g_ratio_road
    real :: g_ratio_brake
    real :: g_ratio_binder
    real :: g_ratio_obs
    real :: g_road_sprayable
    real b_factor
    integer mm
    real middle_max_road_wetness_obs
    real observed_moisture_cutoff_value_temp
    
    !Functions
    real r_aero_func
    real f_spray_func
    real mass_balance_func
    real dewpoint_from_RH_func
    
    !Initialise
    g_road_0_data=nodata
    S_melt_temp=nodata
    g_road_fraction=nodata
    M2_road_salt_0=nodata
    R_evaporation=0
    R_ploughing=0
    R_road_drainage=0
    R_spray=0
    R_drainage=0
    R_total=0

    !Set some parameters that should actually be set outside of the programme
    !--------------------------------------------------------------------------
    retain_water_by_snow=1      !Decides if water is allowed to drain off normally when snow is present
    dz_snow_albedo=3            !Depth of snow required before implementing snow albedo mm.w.e.
    Z_CLOUD=100                 !Only used when no global radiation is available
    z0t=z0/10                   !Sets temperature and humidity roughness length
    length_veh(li)=5            !Vehicle lengths used to calculate the vehicle heat flux
    length_veh(he)=15
    
    b_factor=1./(1000*b_road_lanes(ro)*f_track(tr)) !Coverts g/km to g/m^2

    !Sub surface temperature given as weighted sum of surface temperatures
    !when use_subsurface_flag=2
    !More realistic than using the air temperature
    !--------------------------------------------------------------------------
    if (ti.gt.min_time.and.use_subsurface_flag.eq.2) then
        road_meteo_data(T_sub_index,ti,tr,ro)= &
        road_meteo_data(T_sub_index,max(1,ti-1),tr,ro)*(1.-dt/sub_surf_average_time) &
        +road_meteo_data(T_s_index,max(1,ti-1),tr,ro)*dt/sub_surf_average_time
    endif

    !Set initial values for the time step
    !--------------------------------------------------------------------------
    g_road_0_data(1:num_moisture)=g_road_data(1:num_moisture,max(min_time,ti-1),tr,ro)+surface_moisture_min*0.5
    T_s_0=road_meteo_data(T_s_index,max(min_time,ti-1),tr,ro)
    RH_s_0=road_meteo_data(RH_s_index,max(min_time,ti-1),tr,ro)
    M2_road_salt_0(1:num_salt)=M_road_data(salt_index,pm_all,max(min_time,ti-1),tr,ro)*b_factor
    
    !Set precipitation production term
    !This assumes that the rain is in mm for the given dt period
    !--------------------------------------------------------------------------
    g_road_balance_data(water_index,P_precip_index,ti,tr,ro)=meteo_data(Rain_precip_index,ti,ro)/dt
    g_road_balance_data(snow_index,P_precip_index,ti,tr,ro)=meteo_data(Snow_precip_index,ti,ro)/dt
    g_road_balance_data(ice_index,P_precip_index,ti,tr,ro)=0

    !Ploughing road sinks rate
    !--------------------------------------------------------------------------
    R_ploughing(1:num_moisture)=-log(1-h_ploughing_moisture(1:num_moisture)+.0001)/dt*activity_data(t_ploughing_index,ti,ro) &
        *use_ploughing_data_flag*road_type_activity_flag(road_type_ploughing_index,ro)
    !--------------------------------------------------------------------------

    !Wetting production
    !--------------------------------------------------------------------------
    g_road_balance_data(water_index,P_roadwetting_index,ti,tr,ro)=activity_data(g_road_wetting_index,ti,ro)/dt*use_wetting_data_flag
    !--------------------------------------------------------------------------

    !Evaporation
    !--------------------------------------------------------------------------
    !Calculate aerodynamic resistance
    road_meteo_data(r_aero_index,ti,tr,ro) &
        =r_aero_func(meteo_data(FF_index,ti,ro),z_FF(ro),z_T(ro),z0,z0t,traffic_data(V_veh_index,ti,ro),traffic_data(N_v_index,ti,ro)/n_lanes(ro),num_veh,a_traffic)
    road_meteo_data(r_aero_notraffic_index,ti,tr,ro) &
        =r_aero_func(meteo_data(FF_index,ti,ro),z_FF(ro),z_T(ro),z0,z0t,traffic_data(V_veh_index,ti,ro)*0.,traffic_data(N_v_index,ti,ro)/n_lanes(ro)*0,num_veh,a_traffic)
    if (use_traffic_turb_flag.eq.0) then
        road_meteo_data(r_aero_index,ti,tr,ro)=road_meteo_data(r_aero_notraffic_index,ti,tr,ro)
    endif

    !Calculate the traffic induced heat flux (W/m2).
    road_meteo_data(H_traffic_index,ti,tr,ro)=0
    if (use_traffic_turb_flag.eq.1) then
        do v=1,num_veh
            road_meteo_data(H_traffic_index,ti,tr,ro)=road_meteo_data(H_traffic_index,ti,tr,ro) &
                +H_veh(v)*min(1.,length_veh(v)/traffic_data(V_veh_index(v),ti,ro)*traffic_data(N_v_index(v),ti,ro)/(n_lanes(ro)*1000))
        enddo
    endif

    !Set surface relative humidity
    if (surface_humidity_flag.eq.1) then
        road_meteo_data(RH_s_index,ti,tr,ro)=(min(1.,sum(g_road_0_data(1:num_moisture))/g_road_evaporation_thresh))*100
    elseif (surface_humidity_flag.eq.2) then
        road_meteo_data(RH_s_index,ti,tr,ro)=(1-exp(-sum(g_road_0_data(1:num_moisture))/g_road_evaporation_thresh*4.))*100
    else
        road_meteo_data(RH_s_index,ti,tr,ro)=100
    endif

    !Calculate evaporation energy balance including melt of snow and ice
    if (evaporation_flag.gt.0) then
        short_rad_net_temp=road_meteo_data(short_rad_net_index,ti,tr,ro)
        if (g_road_0_data(snow_index).gt.dz_snow_albedo) then
            short_rad_net_temp=road_meteo_data(short_rad_net_index,ti,tr,ro)*(1.-albedo_snow)/(1.-albedo_road(ro))
        endif

        !if (ro.eq.18163) then
        !   write(*,*) ti,short_rad_net_temp,road_meteo_data(short_rad_net_index,ti,tr,ro),g_road_0_data(snow_index),dz_snow_albedo,(1.-albedo_snow)/(1.-albedo_road(ro))
        !    
        !    endif
        
        call surface_energy_submodel_4 &
            (short_rad_net_temp &
            ,meteo_data(long_rad_in_index,ti,ro) &
	        ,road_meteo_data(H_traffic_index,ti,tr,ro) &
	        ,road_meteo_data(r_aero_index,ti,tr,ro) &
	        ,meteo_data(T_a_index,ti,ro) &
	        ,T_s_0 &
	        ,road_meteo_data(T_sub_index,ti,tr,ro) &
	        ,meteo_data(RH_index,ti,ro) &
	        ,road_meteo_data(RH_s_index,ti,tr,ro) &
	        ,RH_s_0 &
	        ,meteo_data(pressure_index,ti,ro) &
	        ,dzs &
	        ,dt &
	        ,g_road_0_data(water_index) &
	        ,g_road_0_data(ice_index)+g_road_0_data(snow_index) &
	        ,g_road_evaporation_thresh &
            ,M2_road_salt_0 &
            ,salt_type &
	        ,sub_surf_param &
	        ,surface_humidity_flag &
	        ,use_subsurface_flag &
            ,use_salt_humidity_flag &
            !Outputs start here
            ,road_meteo_data(T_s_index,ti,tr,ro) &  
            ,road_meteo_data(T_melt_index,ti,tr,ro) &
            ,road_meteo_data(RH_salt_final_index,ti,tr,ro) &
            ,RH_s_final &
            ,road_salt_data(dissolved_ratio_index,:,ti,tr,ro) &
            ,road_meteo_data(evap_index,ti,tr,ro) &
            ,road_meteo_data(evap_pot_index,ti,tr,ro) &
            ,S_melt_temp &
            ,g_road_balance_data(ice_index,P_freeze_index,ti,tr,ro) &
            ,road_meteo_data(H_index,ti,tr,ro) &
            ,road_meteo_data(L_index,ti,tr,ro) &
            ,road_meteo_data(G_index,ti,tr,ro) &
            ,road_meteo_data(long_rad_out_index,ti,tr,ro) &
            ,road_meteo_data(long_rad_net_index,ti,tr,ro) &
            ,road_meteo_data(rad_net_index,ti,tr,ro) &
            ,road_meteo_data(G_sub_index,ti,tr,ro))
    
         !Taken out of the call to avoid overlapping in and outputs to the subroutine
        road_meteo_data(RH_s_index,ti,tr,ro)=RH_s_final
                
        !Because does not differentiate between snow and ice resdistribute the
        !melting between snow and ice    
        do mm=1,2
            m=snow_ice_index(mm)
            g_road_balance_data(m,S_melt_index,ti,tr,ro)=S_melt_temp*g_road_0_data(m)/(g_road_0_data(snow_index)+g_road_0_data(ice_index))
        enddo
        
    endif

    !Calculate surface dewpoint temperature based on atmospheric humidity
    road_meteo_data(T_s_dewpoint_index,ti,tr,ro) &
        =dewpoint_from_RH_func(road_meteo_data(T_s_index,ti,tr,ro) &
        ,road_meteo_data(RH_s_index,ti,tr,ro))

    meteo_data(T_dewpoint_index,ti,ro) &
        =dewpoint_from_RH_func(meteo_data(T_a_index,ti,ro) &
        ,meteo_data(RH_index,ti,ro))

    !Set the evaporation/condensation rates
    !Distribute evaporation between water and ice according to the share of water and ice
    !Distribute the condensation between water and ice according to temperature
    g_road_fraction(1:num_moisture)=g_road_0_data(1:num_moisture)/sum(g_road_0_data(1:num_moisture))
    if (road_meteo_data(evap_index,ti,tr,ro).gt.0) then !Evaporation
        R_evaporation(1:num_moisture)=road_meteo_data(evap_index,ti,tr,ro)/g_road_0_data(1:num_moisture)*g_road_fraction(1:num_moisture)
        g_road_balance_data(1:num_moisture,P_evap_index,ti,tr,ro)=0
    else !Condensation
        if (road_meteo_data(T_s_index,ti,tr,ro).ge.road_meteo_data(T_melt_index,ti,tr,ro)) then
            !Condensation to water
            g_road_balance_data(water_index,P_evap_index,ti,tr,ro)=-road_meteo_data(evap_index,ti,tr,ro)
            g_road_balance_data(snow_index,P_evap_index,ti,tr,ro)=0
            g_road_balance_data(ice_index,P_evap_index,ti,tr,ro)=0
        else
            !Condensation only to ice (not snow)
            g_road_balance_data(snow_index,P_evap_index,ti,tr,ro)=0
            g_road_balance_data(water_index,P_evap_index,ti,tr,ro)=0
            g_road_balance_data(ice_index,P_evap_index,ti,tr,ro)=-road_meteo_data(evap_index,ti,tr,ro)
        endif
        R_evaporation(1:num_moisture)=0
    endif
    g_road_balance_data(1:num_moisture,S_evap_index,ti,tr,ro)=R_evaporation(1:num_moisture)*g_road_0_data(1:num_moisture)
    !--------------------------------------------------------------------------
    
    !Set drainage rates
    !--------------------------------------------------------------------------
    !This drainage type reduces exponentially according to a time scale
    !Should only be used when the model is run at much shorter time scales than
    !1 hour, e.g. 5 - 10 minutes
    !Needs to be reviewed and updated
    if (drainage_type_flag.eq.1) then 
        g_road_water_drainable=max(0.,g_road_0_data(water_index)-g_road_drainable_min)
        g_road_drainable_withrain=max(0.,meteo_data(Rain_precip_index,ti,ro)+g_road_0_data(water_index)-g_road_drainable_min)

        if (g_road_drainable_withrain.gt.0) then
            R_drainage(water_index)=1/tau_road_drainage
        else
            R_drainage(water_index)=0
        endif
    
        !Diagnostic only. Not correct mathematically
        g_road_balance_data(water_index,S_drainage_tau_index,ti,tr,ro)= g_road_drainable_withrain*R_drainage(water_index)
    endif

    if (drainage_type_flag.eq.2) then
        R_drainage(water_index)=0
        g_road_balance_data(water_index,S_drainage_tau_index,ti,tr,ro)= 0.
    endif   
 
    if (drainage_type_flag.eq.3) then 
        g_road_water_drainable=max(0.,g_road_0_data(water_index)-g_road_drainable_thresh)
        g_road_drainable_withrain=max(0.,meteo_data(Rain_precip_index,ti,ro)+g_road_0_data(water_index)-g_road_drainable_min)

        if (g_road_drainable_withrain.eq.0.and.g_road_water_drainable.gt.0) then
            R_drainage(water_index)=1/tau_road_drainage
        else
            R_drainage(water_index)=0
        endif
    
        !Diagnostic only. Not correct mathematically
        g_road_balance_data(water_index,S_drainage_tau_index,ti,tr,ro)= g_road_water_drainable*R_drainage(water_index)
    endif
    
    g_road_balance_data(water_index,R_drainage_index,ti,tr,ro)= R_drainage(water_index)

    !--------------------------------------------------------------------------

    !Splash and spray sinks and production. Also for snow and ice
    !--------------------------------------------------------------------------
    do m=1,num_moisture
        g_road_sprayable=max(0.,g_road_0_data(m)-g_road_sprayable_min(m))
        if (g_road_sprayable.gt.0.and.water_spray_flag.gt.0) then
            do v=1,num_veh
                R_spray(m)= R_spray(m)+traffic_data(N_v_index(v),ti,ro)/n_lanes(ro)*veh_track(tr) &
                    *f_spray_func(R_0_spray(v,m),traffic_data(V_veh_index(v),ti,ro) &
                    ,V_ref_spray(m),V_thresh_spray(m),a_spray(m),water_spray_flag)
            enddo
            !Adjust according to minimum
            R_spray(m)=R_spray(m)*g_road_sprayable/(g_road_0_data(m)+surface_moisture_min)
        endif
        g_road_balance_data(m,S_spray_index,ti,tr,ro)=R_spray(m)*g_road_0_data(m)
        g_road_balance_data(m,R_spray_index,ti,tr,ro)=R_spray(m)
    enddo
    !--------------------------------------------------------------------------

    !Add production terms
    !--------------------------------------------------------------------------
    g_road_balance_data(1:num_moisture,P_total_index,ti,tr,ro) &
        =g_road_balance_data(1:num_moisture,P_precip_index,ti,tr,ro) &
        +g_road_balance_data(1:num_moisture,P_evap_index,ti,tr,ro) &
        +g_road_balance_data(1:num_moisture,P_roadwetting_index,ti,tr,ro)
    !--------------------------------------------------------------------------

    !Add sink rate terms
    !--------------------------------------------------------------------------
    R_total(1:num_moisture) &
        =R_evaporation(1:num_moisture) &
        +R_drainage(1:num_moisture) &
        +R_spray(1:num_moisture) &
        +R_ploughing(1:num_moisture)
    !--------------------------------------------------------------------------

    !Calculate change in water, ice and snow
    !--------------------------------------------------------------------------
    do m=1,num_moisture
        g_road_data(m,ti,tr,ro)=mass_balance_func(g_road_0_data(m),g_road_balance_data(m,P_total_index,ti,tr,ro),R_total(m),dt)
    enddo
    !--------------------------------------------------------------------------

    !Recalculate spray and evaporation diagnostics based on average moisture
    !--------------------------------------------------------------------------
    do m=1,num_moisture
        g_road_balance_data(m,S_spray_index,ti,tr,ro)=R_spray(m)*(g_road_data(m,ti,tr,ro)+g_road_0_data(m))/2
        g_road_balance_data(m,S_evap_index,ti,tr,ro)=R_evaporation(m)*(g_road_data(m,ti,tr,ro)+g_road_0_data(m))/2
    enddo

    !Remove and add snow melt after the rest of the calculations
    !--------------------------------------------------------------------------
    !Can't melt more ice or snow than there is
    do mm=1,2
        m=snow_ice_index(mm)
        g_road_balance_data(m,S_melt_index,ti,tr,ro)=min(g_road_data(m,ti,tr,ro)/dt,g_road_balance_data(m,S_melt_index,ti,tr,ro))
    enddo
    !Sink of melt is the same as production of water
    g_road_balance_data(water_index,P_melt_index,ti,tr,ro)=sum(g_road_balance_data(snow_ice_index,S_melt_index,ti,tr,ro),1)
    do m=1,num_moisture
        g_road_data(m,ti,tr,ro)=max(0.,g_road_data(m,ti,tr,ro)-g_road_balance_data(m,S_melt_index,ti,tr,ro)*dt)
        g_road_data(m,ti,tr,ro)=g_road_data(m,ti,tr,ro)+g_road_balance_data(m,P_melt_index,ti,tr,ro)*dt
    enddo

    !Remove water through drainage for drainage_type_flag=2 nad 3
    !--------------------------------------------------------------------------
    g_road_water_drainable=0
    if (drainage_type_flag.eq.2.or.drainage_type_flag.eq.3) then
        if (retain_water_by_snow.eq.1) then
            g_road_drainable_min_temp=max(g_road_drainable_min,g_road_data(snow_index,ti,tr,ro))
        else
            g_road_drainable_min_temp=g_road_drainable_min
        endif
        g_road_water_drainable=max(0.,g_road_data(water_index,ti,tr,ro)-g_road_drainable_min_temp)
        g_road_data(water_index,ti,tr,ro)=min(g_road_data(water_index,ti,tr,ro),g_road_drainable_min_temp)
        g_road_balance_data(water_index,S_drainage_index,ti,tr,ro)= g_road_water_drainable/dt
    endif

    !Freeze after the rest of the calculations
    !--------------------------------------------------------------------------
    !Limit the amount of freezing to the amount of available water
    g_road_balance_data(ice_index,P_freeze_index,ti,tr,ro)=min(g_road_data(water_index,ti,tr,ro),g_road_balance_data(ice_index,P_freeze_index,ti,tr,ro)*dt)/dt
    g_road_balance_data(water_index,S_freeze_index,ti,tr,ro)=g_road_balance_data(ice_index,P_freeze_index,ti,tr,ro)
    g_road_data(water_index,ti,tr,ro)=g_road_data(water_index,ti,tr,ro)-g_road_balance_data(water_index,S_freeze_index,ti,tr,ro)
    g_road_data(ice_index,ti,tr,ro)=g_road_data(ice_index,ti,tr,ro)+g_road_balance_data(ice_index,P_freeze_index,ti,tr,ro)

    !Set moisture content to be always>=0. Avoiding round off errors
    !--------------------------------------------------------------------------
    do m=1,num_moisture
        g_road_data(m,ti,tr,ro)=max(0.,g_road_data(m,ti,tr,ro)) 
    enddo

    !Calculate inhibition/retention factors
    !--------------------------------------------------------------------------
    g_road_total=sum(g_road_data(1:num_moisture,ti,tr,ro))
    g_ratio_road=(g_road_total-g_retention_min(road_index)) &
        /(g_retention_thresh(road_index)-g_retention_min(road_index))
    g_ratio_brake=(g_road_data(water_index,ti,tr,ro)-g_retention_min(brake_index)) &
        /(g_retention_thresh(brake_index)-g_retention_min(brake_index))
    g_ratio_binder=(M2_road_salt_0(2)-g_retention_min(salt_index(2))) &
        /(g_retention_thresh(salt_index(2))-g_retention_min(salt_index(2)))
    if (retention_flag.eq.1) then
        f_q(1:num_source,ti,tr,ro)=max(0.,min(1.,1-g_ratio_road))
        f_q(1:num_source,ti,tr,ro)=max(0.,min(1.,1-g_ratio_binder))*f_q(1:num_source,ti,tr,ro)
        f_q(brake_index,ti,tr,ro)=max(0.,min(1.,1-g_ratio_brake))
    elseif (retention_flag.eq.2) then
        f_q(1:num_source,ti,tr,ro)=exp(-2*max(0.,g_ratio_road))
        f_q(1:num_source,ti,tr,ro)=exp(-2*max(0.,g_ratio_binder))*f_q(1:num_source,ti,tr,ro)
        f_q(brake_index,ti,tr,ro)=exp(-2*max(0.,g_ratio_brake))
    elseif (retention_flag.eq.3) then        
        f_q(1:num_source,ti,tr,ro)=0.
    else        
        f_q(1:num_source,ti,tr,ro)=1.
    endif

    !Set observed retention parameter if available
    if (available_meteo_data(road_wetness_obs_input_index).and.road_wetness_obs_in_mm.eq.1) then
        g_ratio_obs=(road_meteo_data(road_wetness_obs_index,ti,tr,ro)-g_retention_min(road_index)) &
            /(g_retention_thresh(road_index)-g_retention_min(road_index))
        if (road_meteo_data(road_wetness_obs_index,ti,tr,ro).eq.nodata_input) then
            f_q_obs(ti,tr,ro)=1!No data then the road is dry
        elseif (retention_flag.eq.1) then
            f_q_obs(ti,tr,ro)=max(0.,min(1.,1-g_ratio_obs))
        elseif (retention_flag.eq.2) then
            f_q_obs(ti,tr,ro)=exp(-2*max(0.,g_ratio_obs))
        else
            f_q_obs(ti,tr,ro)=1
        endif
    endif
    if (available_meteo_data(road_wetness_obs_input_index).and.road_wetness_obs_in_mm.eq.0) then
        !f_q_obs=1-(road_wetness_obs-min(road_wetness_obs))./(max(road_wetness_obs)-min(road_wetness_obs))
        middle_max_road_wetness_obs=(max_road_wetness_obs-min_road_wetness_obs)/2.
        if (observed_moisture_cutoff_value(ro).eq.0.) then
            observed_moisture_cutoff_value_temp=middle_max_road_wetness_obs
        else
            observed_moisture_cutoff_value_temp=observed_moisture_cutoff_value(ro)
        endif
        if (road_meteo_data(road_wetness_obs_index,ti,tr,ro).eq.nodata_input) then
            f_q_obs(ti,tr,ro)=1!No data then dry road
        elseif (road_meteo_data(road_wetness_obs_index,ti,tr,ro).lt.observed_moisture_cutoff_value_temp) then
            f_q_obs(ti,tr,ro)=1.
        else
            f_q_obs(ti,tr,ro)=0.
        endif
    endif
    
    !Set retention based on observed wetness if required
    if (use_obs_retention_flag.eq.1.and.available_meteo_data(road_wetness_obs_input_index).and.retention_flag.gt.0) then
        f_q(1:num_source,ti,tr,ro)=f_q_obs(ti,tr,ro)
        f_q(brake_index,ti,tr,ro)=1.!No retention for brakes when using observed moisture
        f_q(exhaust_index,ti,tr,ro)=1.!No retention for exhaust when using observed moisture        
    endif

    !If the road is a tunnel roadtype then no retention occurs and is never wet
    if (roadtype_index(ro).eq.tunnel_roadtype.or.roadtype_index(ro).eq.tunnelportal_roadtype) then
        f_q(1:num_source,ti,tr,ro)=1.
        f_q(brake_index,ti,tr,ro)=1.
        f_q(exhaust_index,ti,tr,ro)=1.
        do m=1,num_moisture
            g_road_data(m,ti,tr,ro)=0.
        enddo
    endif

    !--------------------------------------------------------------------------

    end subroutine NORTRIP_surface_moisture_submodel

