!NORTRIP_set_activity_data

!==========================================================================
!NORTRIP model
!SUBROUTINE: set_activity_data
!VERSION: 1, 12.04.2015
!AUTHOR: Bruce Rolstad Denby (bruce.denby@met.no)
!DESCRIPTION: Determines if road maintenance activities are undertaken
!==========================================================================

    subroutine NORTRIP_set_activity_data
    
    use NORTRIP_definitions
    
    implicit none
    
    !Internal variables
    real :: M_salting_0(num_salt)
    real :: g_road_wetting_0
    real :: g_road_0_data(num_moisture)
    real :: plough_temp(num_moisture)
    real :: M_sanding_0
    real :: t_ploughing_0
    real :: t_cleaning_0
    
    integer check_day,check_day_min,check_day_max
    integer salt_temperature_flag,salt_precip_flag,salt_RH_flag
    integer sand_temperature_flag,sand_precip_flag,sand_RH_flag
    integer plough_moisture_flag
    integer binding_RH_flag
    integer cleaning_allowed,binding_allowed
    real g_road_wetting_sand,g_road_wetting_salt
    logical :: show_events=.false.

    M_salting_0=0.0
    g_road_wetting_0=0.0
    g_road_0_data=0.0
    plough_temp=0.0
    M_sanding_0=0.0
    t_ploughing_0=0.0
    t_cleaning_0=0.0

    do tr=1,num_track
        g_road_0_data(1:num_moisture)=g_road_0_data(1:num_moisture)+g_road_data(1:num_moisture,max(min_time,ti-1),tr,ro)/num_track
    enddo

    !--------------------------------------------------------------------------
!Automatically carry out ploughing based on previous hours
!--------------------------------------------------------------------------
    plough_temp(1:num_moisture)=0.0
    if (auto_ploughing_flag.ne.0.and.auto_ploughing_flag.ne.3.and.use_ploughing_data_flag.gt.0.and.road_type_activity_flag(road_type_ploughing_index,ro).gt.0) then
        if (auto_ploughing_flag.eq.1.or.auto_ploughing_flag.ge.4) then
            t_ploughing_0=0
        elseif (auto_ploughing_flag.eq.2) then
            t_ploughing_0=activity_data(t_ploughing_index,ti,ro)
        endif
    
        do tr=1,num_track
            plough_temp(1:num_moisture)=plough_temp(1:num_moisture)+g_road_data(1:num_moisture,max(min_time,ti-1),tr,ro)/num_track
        enddo
        plough_moisture_flag=0
        do m=1,num_moisture
            if (plough_temp(m).gt.ploughing_thresh(m)) then
                plough_moisture_flag=1
            endif
        enddo
    
        if (plough_moisture_flag.gt.0.and.(time_since_last_ploughing(ro).ge.delay_ploughing_hour(ro))) then
            activity_data(t_ploughing_index,ti,ro)=t_ploughing_0+1.
            time_since_last_ploughing(ro)=0.
            if (show_events) then
                write(unit_logfile,'(a24,a24,f8.4,a12)') 'Auto ploughing:',trim(date_str(3,ti)),1.,' efficiency'
            endif
        else
            activity_data(t_ploughing_index,ti,ro)=t_ploughing_0
            time_since_last_ploughing(ro)=time_since_last_ploughing(ro)+dt !In hours
        endif

    endif
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
!Automatically add salt
!--------------------------------------------------------------------------
    if (auto_salting_flag.ne.0.and.auto_salting_flag.ne.3.and.road_type_activity_flag(road_type_salt_index(1),ro).gt.0) then
        if (auto_salting_flag.eq.1.or.auto_salting_flag.ge.4) then
            M_salting_0(1:num_salt)=0.0
            g_road_wetting_0=0.0
        elseif (auto_salting_flag.eq.2) then
            M_salting_0(1:num_salt)=activity_data(M_salting_index(1:num_salt),ti,ro)
            g_road_wetting_0=activity_data(g_road_wetting_index,ti,ro)        
        endif

        !Check temperature within range within the given delay time
        check_day=min(max_time,int(ti+dt*check_salting_day(ro)*24+.5))
        salt_temperature_flag=0
        do i=ti,check_day
            if (meteo_data(T_a_index,i,ro).gt.min_temp_salt(ro).and.meteo_data(T_a_index,i,ro).lt.max_temp_salt(ro)) then
                salt_temperature_flag=1
            endif
        enddo
        
        !Check precipitation within range within +/- 1/2 the given delay time
        check_day_min=max(min_time,int(ti-dt*check_salting_day(ro)*24+.5))
        check_day_max=min(max_time,int(ti+dt*check_salting_day(ro)*24+.5))
        salt_precip_flag=0
        do i=check_day_min,check_day_max
            if (meteo_data(Rain_precip_index,i,ro)+meteo_data(Snow_precip_index,i,ro).ge.precip_rule_salt(ro)) then
                salt_precip_flag=1
            endif
        enddo

        check_day_min=ti
        check_day_max=min(max_time,int(ti+dt*check_salting_day(ro)*24+.5))
        salt_RH_flag=0
        do i=check_day_min,check_day_max
            if (meteo_data(RH_index,i,ro).ge.RH_rule_salt(ro)) then
                salt_RH_flag=1
            endif
        enddo

        if ((date_data(hour_index,ti).eq.salting_hour(1,ro).or.date_data(hour_index,ti).eq.salting_hour(2,ro)) &
            .and.salt_temperature_flag.eq.1.and.(salt_precip_flag.eq.1.or.salt_RH_flag.eq.1) &
            .and.time_since_last_salting(ro).ge.delay_salting_day(ro) &
            .or.(salt_after_ploughing_flag.and.activity_data(t_ploughing_index,ti,ro).gt.0)) then
            
            activity_data(M_salting_index(1),ti,ro)=M_salting_0(1)+salt_mass(ro)*salt_type_distribution(ro)
            activity_data(M_salting_index(2),ti,ro)=M_salting_0(2)+salt_mass(ro)*(1.-salt_type_distribution(ro))       
            time_since_last_salting(ro)=0.0
            if (sum(g_road_0_data(1:num_moisture)).lt.g_salting_rule(ro).and.salt_dilution(ro).gt.0) then
                g_road_wetting_salt=salt_mass(ro)*(1-salt_dilution(ro))/salt_dilution(ro)*1e-3
            else
                g_road_wetting_salt=0
            endif
            activity_data(g_road_wetting_index,ti,ro)=g_road_wetting_0+g_road_wetting_salt
            if (show_events) then
                write(unit_logfile,'(a24,a24,f8.2,a13,f8.3,a5)') 'Auto salting(1):' &
                ,trim(date_str(3,ti)),salt_mass(ro)*salt_type_distribution(ro),' (g/m^2) and' &
                ,g_road_wetting_salt,' (mm)'
                if (salt_type_distribution(ro).lt.1.) then
                    write(unit_logfile,'(a24,a24,f8.2,a13,f8.3,a5)') 'Auto salting(2):' &
                    ,trim(date_str(3,ti)),salt_mass(ro)*(1-salt_type_distribution(ro)),' (g/m^2) and ' &
                    ,g_road_wetting_salt,' (mm)'
                endif
            endif
        else
            activity_data(M_salting_index(1),ti,ro)=M_salting_0(1)       
            activity_data(M_salting_index(2),ti,ro)=M_salting_0(2)       
            activity_data(g_road_wetting_index,ti,ro)=g_road_wetting_0
            time_since_last_salting(ro)=time_since_last_salting(ro)+dt/24. !In days
        endif

    endif
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
!Automatically add sand
!--------------------------------------------------------------------------
    if (auto_sanding_flag.ne.0.and.auto_sanding_flag.ne.3.and.road_type_activity_flag(road_type_sanding_index,ro).gt.0) then
        if (auto_sanding_flag.eq.1.or.auto_sanding_flag.ge.4) then
            M_sanding_0=0
            g_road_wetting_0=activity_data(g_road_wetting_index,ti,ro)
        elseif (auto_sanding_flag.eq.2) then
            M_sanding_0=activity_data(M_sanding_index,ti,ro)
            g_road_wetting_0=activity_data(g_road_wetting_index,ti,ro)        
        endif
        
        !Check temperature within range within the given delay time
        check_day=min(max_time,int(ti+dt*check_sanding_day(ro)*24+.5))
        sand_temperature_flag=0
        do i=ti,check_day
            if (meteo_data(T_a_index,i,ro).gt.min_temp_sand(ro).and.meteo_data(T_a_index,i,ro).lt.max_temp_sand(ro)) then
                sand_temperature_flag=1
            endif
        enddo
    
        !Check precipitation within range within +/- the given delay time
        check_day_min=max(min_time,int(ti-dt*check_sanding_day(ro)*24+.5))
        check_day_max=min(max_time,int(ti+dt*check_sanding_day(ro)*24+.5))
        sand_precip_flag=0
        do i=check_day_min,check_day_max
            if (meteo_data(Rain_precip_index,i,ro)+meteo_data(Snow_precip_index,i,ro).ge.precip_rule_sand(ro)) then
                sand_precip_flag=1
            endif
        enddo

        check_day_min=ti
        check_day_max=min(max_time,int(ti+dt*check_sanding_day(ro)*24+.5))
        sand_RH_flag=0
        do i=check_day_min,check_day_max
            if (meteo_data(RH_index,i,ro).ge.RH_rule_sand(ro)) then
                sand_RH_flag=1
            endif
        enddo

        !write(*,*) sand_temperature_flag,sand_precip_flag,sand_RH_flag,time_since_last_sanding(ro)
        
        if ((date_data(hour_index,ti).eq.sanding_hour(1,ro).or.date_data(hour_index,ti).eq.sanding_hour(2,ro)) &
            .and.sand_temperature_flag.eq.1.and.(sand_precip_flag.eq.1.or.sand_RH_flag.eq.1) &
            .and.time_since_last_sanding(ro).ge.delay_sanding_day(ro).and.sum(g_road_0_data(snow_ice_index)).ge.g_sanding_rule(ro)) then
            activity_data(M_sanding_index,ti,ro)=M_sanding_0+sand_mass(ro)
            time_since_last_sanding(ro)=0.0
            if (sum(g_road_0_data(snow_ice_index)).ge.g_sanding_rule(ro).and.sand_dilution(ro).gt.0) then
                g_road_wetting_sand=sand_mass(ro)/sand_dilution(ro)*1e-3
            else
                g_road_wetting_sand=0
            endif
            activity_data(g_road_wetting_index,ti,ro)=g_road_wetting_0+g_road_wetting_sand
            if (show_events) then
                write(unit_logfile,'(a24,a24,f8.2,a13,f8.3,a5)') 'Auto sanding:',trim(date_str(3,ti)),sand_mass(ro),' (g/m^2) and' &
                    ,g_road_wetting_sand,' (mm)'
            endif
        else
            activity_data(M_sanding_index,ti,ro)=0       
            activity_data(g_road_wetting_index,ti,ro)=g_road_wetting_0
            time_since_last_sanding(ro)=time_since_last_sanding(ro)+dt/24. !In days
        endif

    endif
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
!Automatically carry out cleaning
!--------------------------------------------------------------------------
    if (auto_cleaning_flag.ne.0.and.auto_cleaning_flag.ne.3.and.use_cleaning_data_flag.gt.0.and.road_type_activity_flag(road_type_cleaning_index,ro).gt.0) then
    
        if (auto_cleaning_flag.eq.1.or.auto_cleaning_flag.ge.4) then
            t_cleaning_0=0
            g_road_wetting_0=activity_data(g_road_wetting_index,ti,ro)
        elseif (auto_cleaning_flag.eq.2) then
            t_cleaning_0=activity_data(t_cleaning_index,ti,ro)
            g_road_wetting_0=activity_data(g_road_wetting_index,ti,ro)        
        endif

        !if (efficiency_of_cleaning(ro).gt.0) then
        !    write(*,'(3i,4f12.2,2f12.2)') ti,ro_tot,road_ID(ro),efficiency_of_cleaning(ro),time_since_last_cleaning(ro),delay_cleaning_day(ro),traffic_data(N_total_index,ti,ro),start_month_cleaning(ro),end_month_cleaning(ro)
        !    time_since_last_cleaning(ro)=100
            !delay_cleaning_day(ro)=2.5
        !endif

        if (clean_with_salting(ro).gt.0) then
            if (sum(activity_data(M_salting_index(1:num_salt),ti,ro)).gt.0) then
                cleaning_allowed=1
            else
                cleaning_allowed=0
            endif
        else
            cleaning_allowed=1
        endif
    
        !Check month
        if (start_month_cleaning(ro).le.end_month_cleaning(ro)) then
            if (date_data(month_index,ti).ge.start_month_cleaning(ro).and.date_data(month_index,ti).le.end_month_cleaning(ro)) then
                cleaning_allowed=cleaning_allowed*1
            else
                cleaning_allowed=0
            endif
        else
            if (date_data(month_index,ti).ge.start_month_cleaning(ro).or.date_data(month_index,ti).le.end_month_cleaning(ro)) then
                cleaning_allowed=cleaning_allowed*1
            else
                cleaning_allowed=0
            endif
        endif
    
        if ((date_data(hour_index,ti).eq.cleaning_hour(1,ro).or.date_data(hour_index,ti).eq.cleaning_hour(2,ro)) &
            .and.time_since_last_cleaning(ro).ge.delay_cleaning_day(ro).and.meteo_data(T_a_index,ti,ro).gt.min_temp_cleaning(ro) &
            .and.cleaning_allowed.gt.0) then
            activity_data(t_cleaning_index,ti,ro)=t_cleaning_0+efficiency_of_cleaning(ro)
            time_since_last_cleaning(ro)=0
            activity_data(g_road_wetting_index,ti,ro)=g_road_wetting_0+wetting_with_cleaning(ro)
            if (show_events) then
                write(unit_logfile,'(a24,a24,f8.4,a12)') 'Auto cleaning:',trim(date_str(3,ti)),efficiency_of_cleaning(ro),' efficiency'
            endif
            
            !write(*,'(a,i,2f12.2)') 'Cleaned: ',date_data(hour_index,ti),cleaning_hour(1,ro),cleaning_hour(2,ro)
        
        else
            activity_data(t_cleaning_index,ti,ro)=t_cleaning_0
            time_since_last_cleaning(ro)=time_since_last_cleaning(ro)+dt/24. !In days
            activity_data(g_road_wetting_index,ti,ro)=g_road_wetting_0
        endif

    endif

!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
!Automatically add second salt for binding
!--------------------------------------------------------------------------
    if (auto_binding_flag.ne.0.and.auto_binding_flag.ne.3.and.road_type_activity_flag(road_type_salt_index(2),ro).gt.0) then
        if (auto_binding_flag.eq.1.or.auto_binding_flag.ge.4) then
            M_salting_0(2)=0
            g_road_wetting_0=0
        elseif (auto_binding_flag.eq.2) then
            M_salting_0(2)=activity_data(M_salting_index(2),ti,ro)
            g_road_wetting_0=activity_data(g_road_wetting_index,ti,ro)
        endif
    
        !if (binding_mass(ro).gt.0) then
        !    write(*,'(3i,4f12.2,2f12.2)') ti,ro_tot,road_ID(ro),binding_mass(ro),time_since_last_binding(ro),delay_binding_day(ro),traffic_data(N_total_index,ti,ro),start_month_binding(ro),end_month_binding(ro)
            !time_since_last_binding(ro)=2.5
        !    delay_binding_day(ro)=2.5
        !endif
        
            
        !Start with no binding allowed
        binding_allowed=0
    
        !Check temperature within range within the given delay time
        check_day=min(max_time,int(ti+dt*check_binding_day(ro)*24+.5))
        do i=ti,check_day
            if (meteo_data(T_a_index,i,ro).gt.min_temp_binding(ro).and.meteo_data(T_a_index,i,ro).lt.max_temp_binding(ro)) then
                binding_allowed=1
            endif
        enddo

        !Check precipitation within range within +/- the given delay time
        check_day_min=max(min_time,int(ti-dt*check_binding_day(ro)*24+.5))
        check_day_max=min(max_time,int(ti+dt*check_binding_day(ro)*24+.5))
        do i=check_day_min,check_day_max
            if (meteo_data(Rain_precip_index,i,ro)+meteo_data(Snow_precip_index,i,ro).gt.precip_rule_binding(ro)) then
        	    binding_allowed=0
            endif
        enddo

        !Check month
        if (start_month_binding(ro).le.end_month_binding(ro)) then
            if (date_data(month_index,ti).ge.start_month_binding(ro).and.date_data(month_index,ti).le.end_month_binding(ro)) then
                binding_allowed=binding_allowed
            else
                binding_allowed=0
            endif
        else
            if (date_data(month_index,ti).ge.start_month_binding(ro).or.date_data(month_index,ti).le.end_month_binding(ro)) then
                binding_allowed=binding_allowed
            else
                binding_allowed=0
            endif
        endif
   
        !Check current surface conditions
        if (sum(g_road_0_data(1:num_moisture)).gt.g_binding_rule(ro)) then
            binding_allowed=0
        endif
   
        check_day_min=ti
        check_day_max=min(max_time,int(ti+dt*check_binding_day(ro)*24+.5))
        binding_RH_flag=0
        do i=check_day_min,check_day_max
            if (meteo_data(RH_index,i,ro).gt.RH_rule_binding(ro)) then
                binding_RH_flag=1
            endif
        enddo
        !write(*,*) 'Allowed(5)',ro,binding_allowed,binding_RH_flag,delay_binding_day,time_since_last_binding(ro)

        if ((date_data(hour_index,ti).eq.binding_hour(1,ro).or.date_data(hour_index,ti).eq.binding_hour(2,ro)) &
            .and.time_since_last_binding(ro).ge.delay_binding_day(ro) &
            .and.binding_RH_flag.and.binding_allowed) then
       
            activity_data(M_salting_index(2),ti,ro)=M_salting_0(2)+binding_mass(ro)
            time_since_last_binding(ro)=0.0
            
            !write(*,'(a,i,6f12.2)') 'Bound: ',date_data(hour_index,ti),binding_hour(1,ro),binding_hour(2,ro),meteo_data(RH_index,i,ro),RH_rule_binding(ro),sum(g_road_0_data(1:num_moisture)),g_binding_rule(ro)
      
            if (binding_dilution(ro).ne.0) then
                activity_data(g_road_wetting_index,ti,ro)=g_road_wetting_0+binding_mass(ro)*(1-binding_dilution(ro))/binding_dilution(ro)*1e-3
            else
                activity_data(g_road_wetting_index,ti,ro)=g_road_wetting_0
            endif
       
            if (show_events) then
                write(unit_logfile,'(a24,a24,f8.2,a13,f8.3,a5)') 'Auto binding:' &
                ,trim(date_str(3,ti)),binding_mass(ro),' (g/m^2) and' &
                ,activity_data(g_road_wetting_index,ti,ro),' (mm)'
            endif
            

        else
            activity_data(M_salting_index(2),ti,ro)=M_salting_0(2)   
            activity_data(g_road_wetting_index,ti,ro)=g_road_wetting_0
            time_since_last_binding(ro)=time_since_last_binding(ro)+dt/24. !In days
        endif

    endif
 
!--------------------------------------------------------------------------
!Use input activity data
!--------------------------------------------------------------------------
    !Over ride the existing activity data with the input activity values if they are not no data values
    !If flag is 4 then override auto calculations if activity data exists, if 3 then auto calculations have not been done
    !If flag is 5 then will add input data to rule data
        if (auto_salting_flag.ge.3.and.activity_input_data(M_salting_index(1),ti,ro).ne.nodata_activity.and.available_activity_data(M_salting_index(1))) then
            !if (ti.eq.1) write(*,*) 'First hour salting activity data: ',activity_input_data(M_salting_index(1),ti,ro)
            if (auto_salting_flag.eq.5) then
                activity_data(M_salting_index(1),ti,ro)=activity_data(M_salting_index(1),ti,ro)+activity_input_data(M_salting_index(1),ti,ro)
            else
                activity_data(M_salting_index(1),ti,ro)=activity_input_data(M_salting_index(1),ti,ro)
            endif          
            if (activity_input_data(g_road_wetting_index,ti,ro).ne.nodata_activity) then
                if (auto_salting_flag.eq.5) then
                    activity_data(g_road_wetting_index,ti,ro)=activity_data(g_road_wetting_index,ti,ro)+activity_input_data(g_road_wetting_index,ti,ro)
                else
                    activity_data(g_road_wetting_index,ti,ro)=activity_input_data(g_road_wetting_index,ti,ro)
                endif              
            endif
            !Reset last time to avoid additional automatic activities
            if (activity_data(M_salting_index(1),ti,ro).gt.0) then
                time_since_last_salting(ro)=0
            endif            
            !write(*,*) 'Salting'
        endif
        if (auto_binding_flag.ge.3.and.activity_input_data(M_salting_index(2),ti,ro).ne.nodata_activity.and.available_activity_data(M_salting_index(2))) then
            
            if (auto_binding_flag.eq.5) then
                activity_data(M_salting_index(2),ti,ro)=activity_data(M_salting_index(2),ti,ro)+activity_input_data(M_salting_index(2),ti,ro)
            else
                activity_data(M_salting_index(2),ti,ro)=activity_input_data(M_salting_index(2),ti,ro)
            endif          
            if (activity_input_data(g_road_wetting_index,ti,ro).ne.nodata_activity) then
                if (auto_binding_flag.eq.5) then
                    activity_data(g_road_wetting_index,ti,ro)=activity_data(g_road_wetting_index,ti,ro)+activity_input_data(g_road_wetting_index,ti,ro)
                else
                    activity_data(g_road_wetting_index,ti,ro)=activity_input_data(g_road_wetting_index,ti,ro)
                endif              
            endif
            
            if (activity_data(M_salting_index(2),ti,ro).gt.0) then
                time_since_last_binding(ro)=0
            endif            
            !write(*,*) 'Binding'
        endif
        if (auto_sanding_flag.ge.3.and.activity_input_data(M_sanding_index,ti,ro).ne.nodata_activity.and.available_activity_data(M_sanding_index)) then

            if (auto_sanding_flag.eq.5) then
                activity_data(M_sanding_index,ti,ro)=activity_data(M_sanding_index,ti,ro)+activity_input_data(M_sanding_index,ti,ro)
            else
                activity_data(M_sanding_index,ti,ro)=activity_input_data(M_sanding_index,ti,ro)
            endif          
            if (activity_input_data(g_road_wetting_index,ti,ro).ne.nodata_activity) then
                if (auto_sanding_flag.eq.5) then
                    activity_data(g_road_wetting_index,ti,ro)=activity_data(g_road_wetting_index,ti,ro)+activity_input_data(g_road_wetting_index,ti,ro)
                else
                    activity_data(g_road_wetting_index,ti,ro)=activity_input_data(g_road_wetting_index,ti,ro)
                endif              
            endif

            if (activity_data(M_sanding_index,ti,ro).gt.0) then
                time_since_last_sanding(ro)=0
            endif            
            !write(*,*) 'Sanding'
        endif
        if (auto_cleaning_flag.ge.3.and.activity_input_data(t_cleaning_index,ti,ro).ne.nodata_activity.and.available_activity_data(t_cleaning_index)) then

            if (auto_cleaning_flag.eq.5) then
                activity_data(t_cleaning_index,ti,ro)=min(1.,activity_data(t_cleaning_index,ti,ro)+activity_input_data(t_cleaning_index,ti,ro))
            else
                activity_data(t_cleaning_index,ti,ro)=activity_input_data(t_cleaning_index,ti,ro)
            endif          
            if (activity_input_data(g_road_wetting_index,ti,ro).ne.nodata_activity) then
                if (auto_cleaning_flag.eq.5) then
                    activity_data(g_road_wetting_index,ti,ro)=activity_data(g_road_wetting_index,ti,ro)+activity_input_data(g_road_wetting_index,ti,ro)
                else
                    activity_data(g_road_wetting_index,ti,ro)=activity_input_data(g_road_wetting_index,ti,ro)
                endif              
            endif

            if (activity_data(t_cleaning_index,ti,ro).gt.0) then
                time_since_last_cleaning(ro)=0
            endif            
            !write(*,*) 'Cleaning'
        endif
            !write(*,*) ti,ro_tot,auto_ploughing_flag,activity_input_data(t_ploughing_index,ti,ro)

        if (auto_ploughing_flag.ge.3.and.activity_input_data(t_ploughing_index,ti,ro).ne.nodata_activity.and.available_activity_data(t_ploughing_index)) then

            if (auto_ploughing_flag.eq.5) then
                activity_data(t_ploughing_index,ti,ro)=min(1.,activity_data(t_ploughing_index,ti,ro)+activity_input_data(t_ploughing_index,ti,ro))
            else
                activity_data(t_ploughing_index,ti,ro)=activity_input_data(t_ploughing_index,ti,ro)
            endif      
            !No wetting in ploughing
            !if (activity_input_data(g_road_wetting_index,ti,ro).ne.nodata_activity) then
            !    if (auto_ploughing_flag.eq.5) then
            !        activity_data(g_road_wetting_index,ti,ro)=activity_data(g_road_wetting_index,ti,ro)+activity_input_data(g_road_wetting_index,ti,ro)
            !    else
            !        activity_data(g_road_wetting_index,ti,ro)=activity_input_data(g_road_wetting_index,ti,ro)
            !    endif              
            !endif
            
            if (activity_data(t_ploughing_index,ti,ro).gt.0) then
                time_since_last_ploughing(ro)=0
            endif            
            !write(*,*) 'Ploughing'
        endif
        
    
    if (roadtype_index(ro).eq.tunnel_roadtype.or.roadtype_index(ro).eq.tunnelportal_roadtype) then
        !Turn off all activities in tunnels
        if (activity_in_tunnels_flag.eq.0) then
            !write(*,*) 'Not allowing activities in tunnels 0:',t,ro_tot
            activity_data(M_salting_index(1),ti,ro)=0.
            activity_data(M_salting_index(2),ti,ro)=0.
            activity_data(t_cleaning_index,ti,ro)=0.
            activity_data(M_sanding_index,ti,ro)=0.
            activity_data(t_ploughing_index,ti,ro)=0.
        endif
        !Turn of all activities except cleaning in tunnels
        if (activity_in_tunnels_flag.eq.1) then
            !write(*,*) 'Not allowing activities in tunnels 1:',t,ro_tot
            activity_data(M_salting_index(1),ti,ro)=0.
            activity_data(M_salting_index(2),ti,ro)=0.
            !activity_data(t_cleaning_index,ti,ro)=0.
            activity_data(M_sanding_index,ti,ro)=0.
            activity_data(t_ploughing_index,ti,ro)=0.
        endif
        
    endif
    
!--------------------------------------------------------------------------

    end subroutine NORTRIP_set_activity_data
!--------------------------------------------------------------------------
