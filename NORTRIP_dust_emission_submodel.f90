!****************************************************************************
!NORTRIP_read_commandline.f90
!****************************************************************************
!
!   SUBROUTINE:     NORTRIP_dust_emission_submodel
!   PURPOSE:        Main routine for calculating wear, mass loading and emissions
!                   from inside the time (ti), track (tr) and road (ro) loops
!                   All calculations carried out in size segregated binned data
!                   with no time (ti_bin=1) or road (ro_bin=1) dependence
!                   Loading initialised from M_road_data
!                   Binned data unbinned later in NORTRIP_unbin_variables 
!   CALLED FROM:    NORTRIP_main_run
!   CALLS TO:       
!   FUNCTIONS:      W_func
!                   f_abrasion_func
!                   f_crushing_func
!                   f_susroad_func
!                   R_0_wind_func
!                   mass_balance_func
!   VERSION:        25.11.2015
!   AUTHOR:         Bruce Rolstad Denby 
!                   Norwegian Meteorological Institute (www.met.no)
!
!****************************************************************************

    subroutine NORTRIP_dust_emission_submodel
    
    use NORTRIP_definitions
    
    implicit none

    !Local loop variables
    integer tr2,x2
    
    !Dimmension internal arrays without time dependence
    real :: M_road_bin_0_data(num_source,num_size)
    real :: P_wear(num_size)
    real :: E_wear(num_size)
    real :: V_temp
    real :: P_abrasion(num_size)
    real :: E_abrasion(num_size)
    real :: f_abrasion_temp(num_size)
    real :: abrasion_temp(num_size)
    real :: P_crushing(num_size)
    real :: E_crushing(num_size)
    real :: f_crushing_temp(num_size)
    real :: crushing_temp(num_size)
    real :: WR_array(num_wear,num_tyre,num_veh)
    real :: wear_temp
    real :: WR_temp
    real :: f_PM_adjust(num_source,num_size,num_tyre,num_veh)
    real :: R_crushing(num_source,num_size)=0
    real :: R_suspension(num_source,num_size)
    real :: R_suspension_array(num_size)
    real :: f_0_suspension_temp(num_size)
    real :: R_windblown(num_source,num_size)
    real :: R_spray(num_source,num_size)
    real :: dissolved_ratio_temp
    real :: not_dissolved_ratio_temp
    real :: h_eff_temp(num_size)
    real :: R_drainage(num_source,num_size)
    real :: R_cleaning(num_source,num_size)
    real :: R_ploughing(num_source,num_size)
    real :: M_road_bin_balance_data_temp(num_source,num_size,num_dustbalance)
    real :: R_total(num_source,num_size)
    real :: drain_factor
    integer :: loop_index(3)

    !Allows salt that is dissolved to be suspended and sprayed
    integer :: use_dissolved_ratio

    !Functions
    real W_func
    real f_abrasion_func
    real f_crushing_func
    real f_susroad_func
    real R_0_wind_func
    real mass_balance_func
    
    !If use_dissolved_ratio=1 then only allows disolved salt to be sprayed and non dissolved salt to be suspended
    !If use_dissolved_ratio=0 then all salt will be suspended and/or sprayed
    use_dissolved_ratio=1

    !Initialise the local arrays
    M_road_bin_0_data=nodata
    P_wear=0
    E_wear=0
    P_abrasion=0
    E_abrasion=0
    abrasion_temp=0
    f_abrasion_temp=0
    P_crushing=0
    E_crushing=0
    crushing_temp=0
    f_crushing_temp=0
    WR_array=0
    f_PM_adjust=1.
    R_crushing=0
    R_suspension=0
    R_suspension_array=0
    f_0_suspension_temp=0
    R_windblown=0
    R_spray=0
    h_eff_temp=0
    R_drainage=0
    R_cleaning=0
    R_ploughing=0
    M_road_bin_balance_data_temp=0
    R_total=0

    !Set the binned balance and emission data to 0
    M_road_bin_data=0.
    M_road_bin_balance_data=0.
    E_road_bin_data=0.
    
    !--------------------------------------------------------------------------
    !Set the 0 binned mass loading prior to the time step from unbinned mass
    !--------------------------------------------------------------------------
    x=num_size
    M_road_bin_0_data(1:num_source,x)=M_road_data(1:num_source,x,max(min_time,ti-1),tr,ro)
    do x=1,num_size-1
        M_road_bin_0_data(1:num_source,x)=M_road_data(1:num_source,x,max(min_time,ti-1),tr,ro) &
            -M_road_data(1:num_source,x+1,max(min_time,ti-1),tr,ro)
    enddo
    !--------------------------------------------------------------------------

    !==========================================================================
    !Calculate road production of dust and salt for each track and each road
    !==========================================================================

    !--------------------------------------------------------------------------
    !Calculate the direct source wear rates for each s, t and v (WR_array, WR_time_data)
    !--------------------------------------------------------------------------
    do s=1,num_wear
        WR_temp=0
        do t=1,num_tyre
        do v=1,num_veh
            wear_temp=W_func(W_0(s,t,v),h_pave(p_index(ro)),h_drivingcycle(d_index(ro)) &
                ,traffic_data(V_veh_index(v),ti,ro),a_wear(s,:),sum(g_road_data(snow_ice_index,ti,tr,ro)) &
                ,s_roadwear_thresh,s,road_index,tyre_index,brake_index)
            WR_array(s,t,v)=traffic_data(N_t_v_index(t,v),ti,ro)*veh_track(tr)*wear_temp*wear_flag(s)
            WR_temp=WR_temp+WR_array(s,t,v)
        enddo
        enddo
        WR_time_data(s,ti,tr,ro)=WR_temp
    enddo
    !--------------------------------------------------------------------------

    !--------------------------------------------------------------------------
    !Calculate PM fraction speed dependence correction for road wear and PM only
    !--------------------------------------------------------------------------
    s=road_index
    do x=pm_10,pm_25
        do v=1,num_veh
            !Only allow the paramterisation between 20 and 60 km/hr
            V_temp=min(60.,max(20.,traffic_data(V_veh_index(v),ti,ro)))
            f_PM_adjust(s,x,1:num_tyre,v)=(1+c_pm_fraction*V_temp)/(1+c_pm_fraction*V_ref_pm_fraction)
        enddo
    enddo
    !--------------------------------------------------------------------------

    !--------------------------------------------------------------------------
    !Calculate surface mass production due to retention of wear (P_wear)
    !--------------------------------------------------------------------------
    do s=1,num_wear
        P_wear(1:num_size)=0
        E_wear(1:num_size)=0
        do t=1,num_tyre
        do v=1,num_veh
            P_wear(1:num_size)=P_wear(1:num_size) &
                +WR_array(s,t,v)*(1-f_0_dir(s)*f_q(s,ti,tr,ro))*f_PM_bin(s,1:num_size,t)*f_PM_adjust(s,1:num_size,t,v)
            E_wear(1:num_size)=E_wear(1:num_size) &
                +WR_array(s,t,v)*f_0_dir(s)*f_PM_bin(s,1:num_size,t)*f_PM_adjust(s,1:num_size,t,v)*f_q(s,ti,tr,ro)
        enddo
        enddo
        E_road_bin_data(s,1:num_size,E_direct_index,ti_bin,tr,ro_bin)=E_wear(1:num_size)
        !Distribute over all the tracks according to area of tracks (f_track)
        do tr2=1,num_track
            M_road_bin_balance_data(s,1:num_size,P_wear_index,ti_bin,tr2,ro_bin)=P_wear(1:num_size)*f_track(tr2)
        enddo
    enddo
    !write(*,*) sum(M_road_bin_balance_data(road_index,:,P_wear_index,ti_bin,tr,ro_bin))/b_road_lanes(ro)/1000
    !--------------------------------------------------------------------------

    !--------------------------------------------------------------------------
    !Calculate road production and emission rate due to abrasion  (P_abrasion)
    !Not thoroughly tested
    !--------------------------------------------------------------------------
    if (abrasion_flag.gt.0) then
        P_abrasion=0
        E_abrasion=0
        f_abrasion_temp=0
        WR_temp=0
        !Calculate the abrasion caused by each of the size bins
        do s=1,num_source
            if (p_0_abrasion(s).gt.0) then
                do t=1,num_tyre
                do v=1,num_veh
                    f_abrasion_temp=f_abrasion_func(f_0_abrasion(t,v),h_pave(p_index(ro)),traffic_data(V_veh_index(v),ti,ro) &
                        ,sum(g_road_data(snow_ice_index,ti,tr,ro),1),V_ref_abrasion,s_roadwear_thresh)*h_0_abrasion(1:num_size)
                    abrasion_temp(1:num_size)=traffic_data(N_t_v_index(t,v),ti,ro)/n_lanes(ro)*veh_track(tr) &
                        *f_abrasion_temp(1:num_size)*M_road_bin_0_data(s,1:num_size)
                    P_abrasion(1:num_size)=P_abrasion(1:num_size)+abrasion_temp(1:num_size) &
                        *(1-f_0_dir(abrasion_index)*f_q(road_index,ti,tr,ro))
                    E_abrasion(1:num_size)=E_abrasion(1:num_size)+abrasion_temp(1:num_size) &
                        *f_0_dir(abrasion_index)*f_q(road_index,ti,tr,ro)            
                    WR_temp=WR_temp+sum(abrasion_temp(1:num_size))
                enddo
                enddo  
            endif
        enddo
        !Distribute the abrasion to the size bins and tracks
        s=road_index
        do x=1,num_size
            do tr2=1,num_track
                M_road_bin_balance_data(s,1:num_size,P_abrasion_index,ti_bin,tr2,ro_bin) &
                    =P_abrasion(x)*f_PM_bin(abrasion_index,1:num_size,1)*f_track(tr2)
            enddo
            E_road_bin_data(s,1:num_size,E_direct_index,ti_bin,tr,ro_bin) &
                =E_road_bin_data(s,1:num_size,E_direct_index,ti_bin,tr,ro_bin) &
                +E_abrasion(x)*f_PM_bin(abrasion_index,1:num_size,1)
        enddo
        WR_time_data(s,ti,tr,ro)=WR_time_data(s,ti,tr,ro)+WR_temp
    endif
    !--------------------------------------------------------------------------

    !--------------------------------------------------------------------------
    !Calculate production and emission due to crushing (P_crushing)
    !Not thoroughly tested
    !--------------------------------------------------------------------------
    if (crushing_flag.gt.0) then
        do s=1,num_source
            if (p_0_crushing(s).gt.0) then
                do t=1,num_tyre
                do v=1,num_veh
                    f_crushing_temp=f_crushing_func(f_0_crushing(t,v),traffic_data(V_veh_index(v),ti,ro) &
                        ,sum(g_road_data(snow_ice_index,ti,tr,ro)),V_ref_crushing,s_roadwear_thresh)*h_0_crushing(1:num_size)
                    R_crushing(s,1:num_size)=R_crushing(s,1:num_size)+traffic_data(N_t_v_index(t,v),ti,ro)/n_lanes(ro)*veh_track(tr)*f_crushing_temp
                    M_road_bin_balance_data(s,1:num_size,S_crushing_index,ti_bin,tr,ro_bin)=R_crushing(s,1:num_size)*M_road_bin_0_data(s,1:num_size)
                enddo
                enddo
                do x=1,num_size-1
                do x2=x+1,num_size
                    do tr2=1,num_track
                        M_road_bin_balance_data(s,x2,P_crushing_index,ti_bin,tr2,ro_bin) &
                            =M_road_bin_balance_data(s,x,S_crushing_index,ti_bin,tr,ro_bin) &
                            *(1-f_0_dir(crushing_index)*f_q(s,ti,tr,ro))*f_PM_bin(crushing_index,x2,1)
                    enddo
                    E_road_bin_data(s,x2,E_direct_index,ti_bin,tr,ro_bin) &
                        =E_road_bin_data(s,x2,E_direct_index,ti_bin,tr,ro_bin) &
                        +M_road_bin_balance_data(s,x,S_crushing_index,ti_bin,tr,ro_bin) &
                        *f_0_dir(crushing_index)*f_q(s,ti,tr,ro)*f_PM_bin(crushing_index,x2,1)
                enddo
                enddo
            endif  
        enddo
    endif
    !--------------------------------------------------------------------------

    !--------------------------------------------------------------------------
    !Calculate road production flux due to deposition (F_deposition g/(km.m)/hr)
    !Based on PM10 background concentrations and inferred size distribution f_PM_bin(depo_index,1:num_size,1)
    !--------------------------------------------------------------------------
    if (airquality_data(PM_bg_index(pm_10),ti,ro).ne.nodata_input.and.dust_deposition_flag.gt.0) then
        M_road_bin_balance_data(depo_index,1:num_size,P_depo_index,ti_bin,tr,ro_bin)=w_dep(1:num_size) &
            *f_PM_bin(depo_index,1:num_size,1)/f_PM_bin(depo_index,pm_10,1)*max(0.,airquality_data(PM_bg_index(pm_10),ti,ro)) &
            *3.6*b_road_lanes(ro)*f_track(tr)
    else
        M_road_bin_balance_data(depo_index,1:num_size,P_depo_index,ti_bin,tr,ro_bin)=0.
    endif
    !--------------------------------------------------------------------------

    !--------------------------------------------------------------------------
    !Calculate road production due to sanding (P_sanding)
    !--------------------------------------------------------------------------
    M_road_bin_balance_data(sand_index,1:num_size,P_depo_index,ti_bin,tr,ro_bin)= &
        activity_data(M_sanding_index,ti,ro)/dt*f_PM_bin(sand_index,1:num_size,1) &
        *1000*b_road_lanes(ro)*f_track(tr)*use_sanding_data_flag &
        *road_type_activity_flag(road_type_sanding_index,ro)
    !--------------------------------------------------------------------------

    !--------------------------------------------------------------------------
    !Calculate road production due to exhaust deposition (P_exhaust)
    !Initial exhaust emissions are in E_road_data(exhaust_index,pm_25,E_total_index,ti,tr,ro)
    !These are initially set in NORTRIP_initialise_data based on emission factors or air quality input data
    !This is overwritten at the end of the calculation as the sum of direct and suspended
    !--------------------------------------------------------------------------
    if ((available_airquality_data(EP_emis_index).or.exhaust_EF_available.eq.1).and.exhaust_flag.gt.0) then
	    M_road_bin_balance_data(exhaust_index,pm_25,P_depo_index,ti_bin,tr,ro_bin) &
            =E_road_data(exhaust_index,pm_25,E_total_index,ti,tr,ro) &
            *f_PM_bin(exhaust_index,pm_25,1)*f_track(tr)*(1-f_0_dir(exhaust_index))        
        E_road_bin_data(exhaust_index,pm_25,E_direct_index,ti_bin,tr,ro_bin) &
            =E_road_data(exhaust_index,pm_25,E_total_index,ti,tr,ro) &
            *f_PM_bin(exhaust_index,pm_25,1)*f_track(tr)*f_0_dir(exhaust_index)        
    else
        M_road_bin_balance_data(exhaust_index,1:num_size,P_depo_index,ti_bin,tr,ro_bin)=0
        E_road_bin_data(exhaust_index,1:num_size,E_direct_index,ti_bin,tr,ro_bin)=0
    endif
    !--------------------------------------------------------------------------

    !--------------------------------------------------------------------------
    !Calculate road production due to fugitive deposition (P_fugitive g/km)
    !Adds the constant value given in metadata to the time seires activity data
    !--------------------------------------------------------------------------
    if (available_activity_data(M_fugitive_index)) then
    M_road_bin_balance_data(fugitive_index,1:num_size,P_depo_index,ti_bin,tr,ro_bin)= &
        (P_fugitive+activity_data(M_fugitive_index,ti,ro))/dt*f_PM_bin(fugitive_index,1:num_size,1)*f_track(tr)
    else
    M_road_bin_balance_data(fugitive_index,1:num_size,P_depo_index,ti_bin,tr,ro_bin)= &
        P_fugitive/dt*f_PM_bin(fugitive_index,1:num_size,1)*f_track(tr)      
    endif
    !--------------------------------------------------------------------------

    !--------------------------------------------------------------------------
    !Calculate production of salt (P_salt)
    !Converts activity data from g/m^2 to g/km
    !--------------------------------------------------------------------------
    do i=1,num_salt
    if (activity_data(M_salting_index(i),ti,ro).gt.0) then
       M_road_bin_balance_data(salt_index(i),1:num_size,P_depo_index,ti_bin,tr,ro_bin)= &
            activity_data(M_salting_index(i),ti,ro)/dt*f_PM_bin(salt_index(i),1:num_size,1) &
            *1000.*b_road_lanes(ro)*f_track(tr)*use_salting_data_flag(i) &
            *road_type_activity_flag(road_type_salt_index(i),ro)
    endif
    enddo
    !--------------------------------------------------------------------------

    !==========================================================================
    !Calculate road sinks
    !==========================================================================

    !--------------------------------------------------------------------------
    !Calculate the suspension emission sink rate from the road (R_suspension)
    !Does size differentiate the suspension using h_0_sus on f_0_suspension
    !--------------------------------------------------------------------------
    do s=1,num_source
        R_suspension(s,1:num_size)=0
        if (s.eq.salt_index(1).and.use_dissolved_ratio.gt.0) then
            not_dissolved_ratio_temp=(1.-road_salt_data(dissolved_ratio_index,1,ti,tr,ro))
        elseif (s.eq.salt_index(2).and.use_dissolved_ratio.gt.0) then
            not_dissolved_ratio_temp=(1.-road_salt_data(dissolved_ratio_index,2,ti,tr,ro))
        else
            not_dissolved_ratio_temp=1.
        endif
        do t=1,num_tyre
        do v=1,num_veh
            f_0_suspension_temp(1:num_size)=h_sus(ro)*f_0_suspension(s,1:num_size,t,v)*f_susroad_func(traffic_data(V_veh_index(v),ti,ro),a_sus)
            R_suspension_array(1:num_size)=traffic_data(N_t_v_index(t,v),ti,ro)/n_lanes(ro)*veh_track(tr)*f_0_suspension_temp(1:num_size) &
                *(f_q(s,ti,tr,ro)*h_0_q_road(1:num_size)+(1.-h_0_q_road(1:num_size))) &
                *not_dissolved_ratio_temp*road_suspension_flag        
            R_suspension(s,1:num_size)=R_suspension(s,1:num_size)+R_suspension_array(1:num_size)
        enddo
        enddo
        !Diagnose the suspension sink 
	    M_road_bin_balance_data(s,1:num_size,S_suspension_index,ti_bin,tr,ro_bin) &
        =R_suspension(s,1:num_size)*M_road_bin_0_data(s,1:num_size)
        !Calculate the emissions. The same as the suspension sink
        E_road_bin_data(s,1:num_size,E_suspension_index,ti_bin,tr,ro_bin) &
            =M_road_bin_balance_data(s,1:num_size,S_suspension_index,ti_bin,tr,ro_bin)
    enddo

    
    !--------------------------------------------------------------------------
    !Wind blown dust road sink and emission rate (R_windblown)
    !Only suspendable particles included, < 200 um.
    !Does not size differentiate but could use suspension size differentiation h_0_sus
    !--------------------------------------------------------------------------
    do s=1,num_source
        R_windblown(s,pm_sus)=R_0_wind_func(meteo_data(FF_index,ti,ro),tau_wind,FF_thresh)*f_q(s,ti,tr,ro)*wind_suspension_flag
	    M_road_bin_balance_data(s,pm_sus,S_windblown_index,ti_bin,tr,ro_bin)= &
            R_windblown(s,pm_sus)*M_road_bin_0_data(s,pm_sus)
        E_road_bin_data(s,1:num_size,E_windblown_index,ti_bin,tr,ro_bin)= &
            M_road_bin_balance_data(s,1:num_size,S_windblown_index,ti_bin,tr,ro_bin)    
    enddo
    !--------------------------------------------------------------------------

    !--------------------------------------------------------------------------
    !Spray and splash road sink (R_spray)
    !Size differentiates using h_eff
    !--------------------------------------------------------------------------
    if (sum(g_road_data(1:num_moisture,ti,tr,ro),1).gt.0.and.dust_spray_flag.gt.0) then
        do s=1,num_source
            if (s.eq.salt_index(1).and.use_dissolved_ratio.gt.0) then
                dissolved_ratio_temp=road_salt_data(dissolved_ratio_index,1,ti,tr,ro)
            elseif (s.eq.salt_index(2).and.use_dissolved_ratio.gt.0) then
                dissolved_ratio_temp=road_salt_data(dissolved_ratio_index,2,ti,tr,ro)
            else
                dissolved_ratio_temp=1.
            endif
            h_eff_temp(1:num_size)=h_eff(spraying_eff_index,s,1:num_size)
            R_spray(s,1:num_size)=sum(g_road_balance_data(1:num_moisture,R_spray_index,ti,tr,ro),1) &
                *h_eff_temp(1:num_size)*dissolved_ratio_temp
	        M_road_bin_balance_data(s,1:num_size,S_dustspray_index,ti_bin,tr,ro_bin)= &
                R_spray(s,1:num_size)*M_road_bin_0_data(s,1:num_size)
	        !Production due to spray for multitracks. See the surface wetness routine
            M_road_bin_balance_data(s,1:num_size,P_dustspray_index,ti_bin,tr,ro_bin)= &
                sum(g_road_balance_data(1:num_moisture,P_spray_index,ti,tr,ro),1) &
                *h_eff_temp(1:num_size)*dissolved_ratio_temp &
                *M_road_bin_0_data(s,1:num_size)
        enddo
    else
        M_road_bin_balance_data(1:num_source,1:num_size,S_dustspray_index,ti_bin,tr,ro_bin)=0.
    endif
    !--------------------------------------------------------------------------

    !--------------------------------------------------------------------------
    !Drainage road sink rate (R_drainage)
    !Size differentiates using h_eff
    !--------------------------------------------------------------------------
    if (drainage_type_flag.eq.1.or.drainage_type_flag.eq.3) then
        if (g_road_data(snow_index,ti,tr,ro).lt.snow_dust_drainage_retainment_limit.and.dust_drainage_flag.gt.0) then
            do s=1,num_source
                if (s.eq.salt_index(1).and.use_dissolved_ratio.gt.0) then
                    dissolved_ratio_temp=road_salt_data(dissolved_ratio_index,1,ti,tr,ro)
                elseif (s.eq.salt_index(2).and.use_dissolved_ratio.gt.0) then
                    dissolved_ratio_temp=road_salt_data(dissolved_ratio_index,2,ti,tr,ro)
                else
                    dissolved_ratio_temp=1.
                endif
                R_drainage(s,1:num_size)=g_road_balance_data(water_index,R_drainage_index,ti,tr,ro) &
                    *h_eff(drainage_eff_index,s,1:num_size)*dissolved_ratio_temp
	            M_road_bin_balance_data(s,1:num_size,S_dustdrainage_index,ti_bin,tr,ro_bin)= &
                    R_drainage(s,1:num_size)*M_road_bin_0_data(s,1:num_size)      
            enddo
        else
            M_road_bin_balance_data(1:num_source,1:num_size,S_dustdrainage_index,ti_bin,tr,ro_bin)=0
        endif
    endif
    !--------------------------------------------------------------------------
    
    !--------------------------------------------------------------------------
    !Cleaning road sink rate (R_cleaning)
    !Size differentiates using h_eff
    !--------------------------------------------------------------------------
    if (activity_data(t_cleaning_index,ti,ro).gt.0) then
    do s=1,num_source
        R_cleaning(s,1:num_size)=-log(1-min(0.99999,h_eff(cleaning_eff_index,s,1:num_size) &
            *activity_data(t_cleaning_index,ti,ro)))/dt*use_cleaning_data_flag &
            *road_type_activity_flag(road_type_cleaning_index,ro)
	    M_road_bin_balance_data(s,1:num_size,S_cleaning_index,ti_bin,tr,ro_bin)= &
            R_cleaning(s,1:num_size)*M_road_bin_0_data(s,1:num_size)      
    enddo
    endif
    !--------------------------------------------------------------------------

    !--------------------------------------------------------------------------
    !Ploughing road sink (R_ploughing)
    !Size differentiates using h_eff
    !--------------------------------------------------------------------------
    if (activity_data(t_ploughing_index,ti,ro).gt.0) then
        do s=1,num_source
            R_ploughing(s,1:num_size)=-log(1-min(0.99999,h_eff(ploughing_eff_index,s,1:num_size) &
                *activity_data(t_ploughing_index,ti,ro)))/dt*use_ploughing_data_flag*dust_ploughing_flag &
                *road_type_activity_flag(road_type_ploughing_index,ro)

	        M_road_bin_balance_data(s,1:num_size,S_dustploughing_index,ti_bin,tr,ro_bin)= &
                R_ploughing(s,1:num_size)*M_road_bin_0_data(s,1:num_size)
        enddo
    endif
    !--------------------------------------------------------------------------

    !--------------------------------------------------------------------------
    !Distribute wear, abrasion and crushing production terms between tracks
    !This routine not tested for the multitrack application yet
    !--------------------------------------------------------------------------
    if (num_track.gt.1) then
        loop_index(1)=P_wear_index
        loop_index(2)=P_abrasion_index 
        loop_index(3)=P_crushing_index
        do i=1,3
            !Save the current production in all tracks
            M_road_bin_balance_data_temp(1:num_source,1:num_size,loop_index(i)) &
                =M_road_bin_balance_data(1:num_source,1:num_size,loop_index(i),ti,tr,ro)
            !Delete the current balance in the current track
            M_road_bin_balance_data(1:num_source,1:num_size,loop_index(i),ti_bin,tr,ro_bin)=0
            !Redistribute according to track fraction (f_track)
            do tr2=1,num_track
                M_road_bin_balance_data(1:num_source,1:num_size,loop_index(i),ti_bin,tr2,ro_bin) &
                    =M_road_bin_balance_data(1:num_source,1:num_size,loop_index(i),ti_bin,tr2,ro_bin) &
                    +M_road_bin_balance_data_temp(1:num_source,1:num_size,loop_index(i))*f_track(tr2)
            enddo
        enddo
    endif
    !--------------------------------------------------------------------------

    !--------------------------------------------------------------------------
    !Add up the contributions for the road mass and salt production (P_road)
    !--------------------------------------------------------------------------
    M_road_bin_balance_data(1:num_source,1:num_size,P_dusttotal_index,ti_bin,tr,ro_bin) &
        =M_road_bin_balance_data(1:num_source,1:num_size,P_wear_index,ti_bin,tr,ro_bin) &
        +M_road_bin_balance_data(1:num_source,1:num_size,P_abrasion_index,ti_bin,tr,ro_bin) &
        +M_road_bin_balance_data(1:num_source,1:num_size,P_crushing_index,ti_bin,tr,ro_bin) &
        +M_road_bin_balance_data(1:num_source,1:num_size,P_depo_index,ti_bin,tr,ro_bin)
    !write(*,*) sum(M_road_bin_balance_data(road_index,1:num_size,P_dusttotal_index,ti,tr,ro))/1000./b_road_lanes(ro)
    !--------------------------------------------------------------------------

    !--------------------------------------------------------------------------
    !Add up all the road sink rates (R_total)
    !--------------------------------------------------------------------------
    R_total(1:num_source,1:num_size) &
        =R_drainage(1:num_source,1:num_size) &
        +R_cleaning(1:num_source,1:num_size) &
        +R_ploughing(1:num_source,1:num_size) &
        +R_spray(1:num_source,1:num_size) &
        +R_crushing(1:num_source,1:num_size) &
        +R_suspension(1:num_source,1:num_size) &
        +R_windblown(1:num_source,1:num_size)  
     !write(*,*) R_spray(road_index,pm_10),R_spray(salt_index(1),pm_all),R_spray(salt_index(2),pm_all)
    !--------------------------------------------------------------------------
    
    !--------------------------------------------------------------------------
    !Calculate mass balance for the road
    !--------------------------------------------------------------------------
    do s=1,num_source
    do x=1,num_size
        M_road_bin_data(s,x,ti_bin,tr,ro_bin) &
            =mass_balance_func(M_road_bin_0_data(s,x),M_road_bin_balance_data(s,x,P_dusttotal_index,ti_bin,tr,ro_bin),R_total(s,x),dt)
    enddo
    enddo

    !--------------------------------------------------------------------------
    !Diagnose sinks
    !--------------------------------------------------------------------------
    M_road_bin_balance_data(1:num_source,1:num_size,S_dusttotal_index,ti_bin,tr,ro_bin) &
        =R_total(1:num_source,1:num_size)*M_road_bin_0_data(1:num_source,1:num_size)
    !--------------------------------------------------------------------------

    !--------------------------------------------------------------------------
    !Remove mass through drainage after the mass balance when drainage type = 2,3
    !--------------------------------------------------------------------------
    if (drainage_type_flag.eq.2.or.drainage_type_flag.eq.3) then
        drain_factor=g_road_balance_data(water_index,S_drainage_index,ti,tr,ro)*dt &
            /(g_road_drainable_min+g_road_balance_data(water_index,S_drainage_index,ti,tr,ro)*dt)
        do s=1,num_source
            if (s.eq.salt_index(1).and.use_dissolved_ratio.gt.0) then
                dissolved_ratio_temp=road_salt_data(dissolved_ratio_index,1,ti,tr,ro)
            elseif (s.eq.salt_index(2).and.use_dissolved_ratio.gt.0) then
                dissolved_ratio_temp=road_salt_data(dissolved_ratio_index,2,ti,tr,ro)
            else
                dissolved_ratio_temp=1.
            endif
            M_road_bin_balance_data(s,1:num_size,S_dustdrainage_index,ti_bin,tr,ro_bin)=0
            h_eff_temp(1:num_size)=h_eff(drainage_eff_index,s,1:num_size)
            if (dust_drainage_flag.eq.1) then
                M_road_bin_balance_data(s,1:num_size,S_dustdrainage_index,ti_bin,tr,ro_bin) &
                    =M_road_bin_data(s,1:num_size,ti_bin,tr,ro_bin) &
                    *dissolved_ratio_temp*h_eff_temp*drain_factor/dt
            elseif (dust_drainage_flag.eq.2) then
                M_road_bin_balance_data(s,1:num_size,S_dustdrainage_index,ti_bin,tr,ro_bin) &
                    =M_road_bin_data(s,1:num_size,ti_bin,tr,ro_bin) &
                    *dissolved_ratio_temp*(1-exp(-h_eff_temp*drain_factor))/dt
            endif
            M_road_bin_data(s,1:num_size,ti_bin,tr,ro_bin) &
                =M_road_bin_data(s,1:num_size,ti_bin,tr,ro_bin) &
                -M_road_bin_balance_data(s,1:num_size,S_dustdrainage_index,ti_bin,tr,ro_bin)*dt
            M_road_bin_balance_data(s,1:num_size,S_dusttotal_index,ti_bin,tr,ro_bin) &
                =M_road_bin_balance_data(s,1:num_size,S_dusttotal_index,ti_bin,tr,ro_bin) &
                +M_road_bin_balance_data(s,1:num_size,S_dustdrainage_index,ti_bin,tr,ro_bin)
        enddo
    endif
    !write(*,*) sum(M_road_bin_balance_data(road_index,:,P_dusttotal_index,ti_bin,tr,ro_bin))/b_road_lanes(ro)/1000, &
    !    sum(M_road_bin_balance_data(road_index,:,S_dusttotal_index,ti_bin,tr,ro_bin))/b_road_lanes(ro)/1000, &
    !    sum(M_road_bin_balance_data(road_index,:,S_suspension_index,ti_bin,tr,ro_bin))/b_road_lanes(ro)/1000
    
    !--------------------------------------------------------------------------
    !Remove any negative values in mass (round off errors)
    !--------------------------------------------------------------------------
    do s=1,num_source
    do x=1,num_size
        M_road_bin_data(s,x,ti_bin,tr,ro_bin)=max(0.,M_road_bin_data(s,x,ti_bin,tr,ro_bin))
    enddo
    enddo

    !--------------------------------------------------------------------------
    !Calculate the final total road dust loadings
    !--------------------------------------------------------------------------
    M_road_bin_data(total_dust_index,1:num_size,ti_bin,tr,ro_bin) &
        =sum(M_road_bin_data(dust_index,1:num_size,ti_bin,tr,ro_bin),1)
    M_road_bin_balance_data(total_dust_index,1:num_size,1:num_dustbalance,ti_bin,tr,ro_bin) &
        =sum(M_road_bin_balance_data(dust_index,1:num_size,1:num_dustbalance,ti_bin,tr,ro_bin),1)
    !--------------------------------------------------------------------------

    !==========================================================================
    !Calculate binned emissions
    !==========================================================================

    !--------------------------------------------------------------------------
    !Total emissions for each source
    !--------------------------------------------------------------------------
    E_road_bin_data(1:num_source,1:num_size,E_total_index,ti_bin,tr,ro_bin) &
        =E_road_bin_data(1:num_source,1:num_size,E_direct_index,ti_bin,tr,ro_bin) &
        +E_road_bin_data(1:num_source,1:num_size,E_suspension_index,ti_bin,tr,ro_bin) &
        +E_road_bin_data(1:num_source,1:num_size,E_windblown_index,ti_bin,tr,ro_bin)
    !--------------------------------------------------------------------------
    !Total emissions including salt. Not entirely consistent with the M_road_data(total_dust_index,.. since that is only for dust
    !--------------------------------------------------------------------------
    E_road_bin_data(total_dust_index,1:num_size,1:num_process,ti_bin,tr,ro_bin) &
        =sum(E_road_bin_data(1:num_source,1:num_size,1:num_process,ti_bin,tr,ro_bin),1)
    !--------------------------------------------------------------------------

    !==========================================================================

end subroutine NORTRIP_dust_emission_submodel