!----------------------------------------------------------------------
! Various routines and functions
!----------------------------------------------------------------------

! Calculates running mean temperature
!----------------------------------------------------------------------
    subroutine NORTRIP_running_mean_temperature(num_running_hours)

    use NORTRIP_definitions

    implicit none
    
    !Avergaing time is 1.5 - 3 days
    !road_meteo_data(T_sub_index,:,tr,ro) has already been initialised as meteo_data(T_a_index,:,ro) in NORTRIP_initialise_data
    
    real num_running_hours
    integer min_running_index
    logical :: use_running_mean=.false.

    !do ro=n_roads_start,n_roads_end
        do tr=1,num_track
            if (available_meteo_data(T_sub_input_index)) then
               road_meteo_data(T_sub_index,:,tr,ro)=meteo_data(T_sub_input_index,:,ro)
            else
                do ti=min_time,max_time 
                    !specify the minimum index
                    min_running_index=max(1,ti-int(num_running_hours))
                    !write(*,*)min_running_index,tr,ro
                
                    if (use_running_mean) then
                        !Calculate running mean
                        road_meteo_data(T_sub_index,ti,tr,ro)=sum(meteo_data(T_a_index,min_running_index:ti,ro))/size(meteo_data(T_a_index,min_running_index:ti,ro),1)
                    else
                        !Alternative formulation so as to preserve the initial value in min_time index. Assumes a number in min_time already
                        if (ti.gt.min_time) then
                            road_meteo_data(T_sub_index,ti,tr,ro)= &
                                 road_meteo_data(T_sub_index,max(1,ti-1),tr,ro)*(1.-dt/num_running_hours) &
                                 +meteo_data(T_a_index,ti,ro)*dt/num_running_hours
                            
                            !write(*,*) ti,road_meteo_data(T_sub_index,ti,tr,ro),road_meteo_data(T_sub_index,max(1,ti-1),tr,ro),meteo_data(T_a_index,ti,ro)
                        endif
                    endif
                    !write(*,*) ti,road_meteo_data(T_sub_index,ti,tr,ro)
                end do    
            endif

            !Treat this differently in a tunnel
            if (roadtype_index(ro).eq.tunnel_roadtype.or.roadtype_index(ro).eq.tunnelportal_roadtype) then
                !Not certain what to do here as not enough information is available about a tunnels temperature. Set at ambient temperature
                road_meteo_data(T_sub_index,:,tr,ro)=meteo_data(T_a_index,:,ro)
            endif
         
            !Treat this differently on a bridge. Set sublayer temperature to atmospheric layer
            if (roadtype_index(ro).eq.bridge_roadtype) then
                road_meteo_data(T_sub_index,:,tr,ro)=meteo_data(T_a_index,:,ro)
            endif
        
        end do

        if (((ro.eq.1.or.ro.eq.n_roads).and..not.use_single_road_loop_flag).or.((ro_tot.eq.1.or.ro_tot.eq.n_roads_total).and.use_single_road_loop_flag)) then
            write(unit_logfile,'(A)') ''
            write(unit_logfile,'(A)') 'Calculating subsurface temperature (NORTRIP_running_mean_temperature)' 
  	        write(unit_logfile,'(A)') '================================================================'

            write(unit_logfile,'(A40,A3,L)') trim(meteo_match_str(T_sub_input_index))//' available',' = ',available_meteo_data(T_sub_input_index)
	        write(unit_logfile,'(A)') '----------------------------------------------------------------'
            write(unit_logfile,'(a32,a14,a14,a14)') 'Temperature parameter','Min value','Max value','Mean value'
	        write(unit_logfile,'(A)') '----------------------------------------------------------------'
            write(unit_logfile,'(a32,f14.2,f14.2,f14.2)') 'T_a',minval(meteo_data(T_a_index,min_time:max_time,ro)),maxval(meteo_data(T_a_index,min_time:max_time,ro)),sum(meteo_data(T_a_index,min_time:max_time,ro)/(max_time-min_time+1))
            write(unit_logfile,'(a32,f14.2,f14.2,f14.2)') 'T_sub',minval(road_meteo_data(T_sub_index,min_time:max_time,1,ro)),maxval(road_meteo_data(T_sub_index,min_time:max_time,1,ro)),sum(road_meteo_data(T_sub_index,min_time:max_time,1,ro)/(max_time-min_time+1))
	        write(unit_logfile,'(A)') '----------------------------------------------------------------'
        endif
    
        !(*,*) road_meteo_data(T_sub_index,min_time:max_time,1,ro)
        !write(*,*) meteo_data(T_a_index,min_time:max_time,ro)
        !write(*,*) num_running_hours
        !stop
    !enddo
    
    end subroutine NORTRIP_running_mean_temperature
!----------------------------------------------------------------------
   
!----------------------------------------------------------------------
    function e_sat_func(TC,P)

    implicit none
    !TC: Degrees C
    !P: mbar or hPa
    !esat: hPa
    !qsat: kg/kg
    real TC,P
    real e_sat_func

    real a,b,c
    parameter (a=6.1121,b=17.67,c=243.5)

    e_sat_func=a*exp(b*TC/(c+TC))
    !qsat=0.622*esat./(P-0.378*esat)

    !d_esat_dT=esat*b*c./(TC+c).^2
    !d_qsat_dT=0.622.*P./(P-0.378*esat).^2.*d_esat_dT

    end function e_sat_func
!----------------------------------------------------------------------

!----------------------------------------------------------------------
    function q_sat_func(TC,P)

    implicit none
    !TC: Degrees C
    !P: mbar or hPa
    !esat: hPa
    !qsat: kg/kg
    real TC,P
    real esat
    real q_sat_func

    real a,b,c
    parameter (a=6.1121,b=17.67,c=243.5)

    esat=a*exp(b*TC/(c+TC))
    q_sat_func=0.622*esat/(P-0.378*esat)

    !d_esat_dT=esat*b*c./(TC+c).^2
    !d_qsat_dT=0.622.*P./(P-0.378*esat).^2.*d_esat_dT

    end function q_sat_func
!----------------------------------------------------------------------

!----------------------------------------------------------------------
    function q_sat_ice_func(TC,P)

    implicit none
    !TC: Degrees C
    !P: mbar or hPa
    !esat: hPa
    !qsat: kg/kg
    real TC,P
    real esat
    real q_sat_ice_func

    real a,b,c
    parameter (a=6.1121,b=22.46,c=272.62)

    esat=a*exp(b*TC/(c+TC))
    q_sat_ice_func=0.622*esat/(P-0.378*esat)

    !d_esat_dT=esat*b*c./(TC+c).^2
    !d_qsat_dT=0.622.*P./(P-0.378*esat).^2.*d_esat_dT

    end function q_sat_ice_func
!----------------------------------------------------------------------

!----------------------------------------------------------------------
    function dewpoint_from_RH_func(TC,RH)

    implicit none
    !TC: Degrees C
    !esat: hPa
    !RH: !
    real dewpoint_from_RH_func
    real TC,RH
    real esat,eair

    real a,b,c
    parameter (a=6.1121,b=17.67,c=243.5)

    esat=a*exp(b*TC/(c+TC))
    eair=RH/100*esat
    dewpoint_from_RH_func=c*log(eair/a)/(b-log(eair/a))

    end function dewpoint_from_RH_func
!----------------------------------------------------------------------

    !----------------------------------------------------------------------
    function r_aero_func(FF,z_FF,z_T,z0,z0t,V_veh,N_v,num_veh,a_traffic)

    implicit none
    real r_aero_func
    real FF,z_FF,z_T,z0,z0t
    real V_veh(num_veh),N_v(num_veh),a_traffic(num_veh)
    integer num_veh
    real inv_r_wind,inv_r_traffic,inv_r_aero
    integer v
    real kappa
    parameter (kappa=0.4)
    
    inv_r_wind=max(FF,0.2)*kappa**2/(log(z_FF/z0)*log(z_T/z0t))
    
    inv_r_traffic=0
    do v=1,num_veh
        inv_r_traffic=inv_r_traffic+N_v(v)*V_veh(v)*a_traffic(v)
    enddo
    
    inv_r_traffic=max(1e-6,inv_r_traffic/3600./3.6)
    inv_r_aero=inv_r_traffic+inv_r_wind
    r_aero_func=1./inv_r_aero

    end function r_aero_func
 !----------------------------------------------------------------------

!----------------------------------------------------------------------
    function f_spray_func(R_0_spray,V_veh,V_ref_spray,V_thresh_spray,a_spray,water_spray_flag)
    
    implicit none

    real f_spray_func
    real R_0_spray,V_veh,V_ref_spray,V_thresh_spray,a_spray
    integer water_spray_flag
    
    f_spray_func=0
    if (water_spray_flag.gt.0.and.V_ref_spray.gt.V_thresh_spray) then
        f_spray_func=R_0_spray*(max(0.,(V_veh-V_thresh_spray))/(V_ref_spray-V_thresh_spray))**a_spray    
    endif
    
    end function f_spray_func
!----------------------------------------------------------------------

!----------------------------------------------------------------------
    function mass_balance_func(M_0,P,R,dt)
    !mass_balance_func: Caclulates temporal mass balance changes
    
    implicit none
    real mass_balance_func
    real M_0,P,R,dt
    
    if (P.lt.R*1.0E8) then
        mass_balance_func=P/R*(1-exp(-R*dt))+M_0*exp(-R*dt)
    else
        mass_balance_func=M_0+P*dt
    endif

    end function mass_balance_func
!----------------------------------------------------------------------

!----------------------------------------------------------------------
    function W_func(W_0,h_pave_in,h_dc_in,V_veh,a_wear,s_road,s_roadwear_thresh,s,road_index,tyre_index,brake_index)
    ! W_func: Wear function
    ! Depends on:

    implicit none
    
    !Output variable
    real W_func
    !Input variables
    real h_pave_in,h_dc_in
    real W_0,V_veh,a_wear(5),s_road,s_roadwear_thresh
    integer s,road_index,tyre_index,brake_index
    !Internal variables
    real f_V,f_snow
    real h_pave,h_dc
    
    h_pave=h_pave_in
    h_dc=h_dc_in
    
    !No wear production due to snow on the surface
    f_snow=1.
    if (s_road.gt.s_roadwear_thresh) then
        f_snow=0.
    endif

    f_V=max(0.,a_wear(1)+a_wear(2)*(max(V_veh,a_wear(5))/a_wear(4))**a_wear(3))

    if (s.eq.road_index) then
        h_dc=1.
    endif
    if (s.eq.tyre_index) then
        h_dc=1.
        h_pave=1.
    endif
    if (s.eq.brake_index) then
        h_pave=1.
        f_snow=1.
    endif

    W_func=W_0*h_pave*h_dc*f_V*f_snow

    end function W_func
!----------------------------------------------------------------------

!----------------------------------------------------------------------
    function f_abrasion_func(f_sandpaper_0,h_pave,V_veh,s_road,V_ref,s_roadwear_thresh)
    !W_func: Abrasion function
    
    implicit none
    real f_abrasion_func
    real f_sandpaper_0,h_pave,V_veh,s_road,V_ref,s_roadwear_thresh
    real f_V,f_snow

    f_V=V_veh/V_ref

    !No wear production due to snow on the surface
    f_snow=1.
    if (s_road.gt.s_roadwear_thresh) then
        f_snow=0.
    endif

    f_abrasion_func=f_sandpaper_0*h_pave*f_V*f_snow

    end function f_abrasion_func
!----------------------------------------------------------------------

!----------------------------------------------------------------------
    function f_crushing_func(f_crushing_0,V_veh,s_road,V_ref,s_roadwear_thresh)
    !W_func: Sandpaper function

    implicit none
    real f_crushing_func
    real f_crushing_0,V_veh,s_road,V_ref,s_roadwear_thresh
    real f_V,f_snow

    f_V=(V_veh/V_ref)

    !No wear production due to snow on the surface
    f_snow=1.
    if (s_road.gt.s_roadwear_thresh) then
        f_snow=0.
    endif

    f_crushing_func=f_crushing_0*f_V*f_snow

    end function f_crushing_func
!----------------------------------------------------------------------

!----------------------------------------------------------------------
    function f_susroad_func(V_veh,a_sus)
    !f_sus_func: Vehicle speed dependence function for suspension
    implicit none
    real f_susroad_func
    integer num_size
    real V_veh
    real a_sus(5)
    real h_V

    h_V=max(0.,a_sus(1)+a_sus(2)*(max(V_veh,a_sus(5))/a_sus(4))**a_sus(3))
    f_susroad_func=h_V

    end function f_susroad_func
!----------------------------------------------------------------------

!----------------------------------------------------------------------
    function R_0_wind_func(FF,tau_wind,FF_thresh)
    !R_0_wind_func: Wind blown dust wind speed depedency
    
    implicit none
    real R_0_wind_func
    real FF,tau_wind,FF_thresh
    real h_FF
    
    if (FF.gt.FF_thresh) then
        h_FF=(FF/FF_thresh-1.)**3
    else
        h_FF=0.
    endif

    R_0_wind_func=1./tau_wind*h_FF

    end function R_0_wind_func
!----------------------------------------------------------------------

    
