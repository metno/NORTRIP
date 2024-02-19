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
                !Do nothing for the time being
                !road_meteo_data(T_sub_index,:,tr,ro)=meteo_data(T_a_index,:,ro)
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
    !+.01 to avoid a NaN error in the log when RH=0
    eair=(RH+.01)/100.*esat
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
    function r_aero_func_with_stability(FF,Tc,Ts,z_FF,z_T,z0,z0t,V_veh,N_v,num_veh,a_traffic)

    implicit none
    real r_aero_func_with_stability
    real FF,Tc,Ts,z_FF,z_T,z0,z0t
    real V_veh(num_veh),N_v(num_veh),a_traffic(num_veh)
    integer num_veh
    real inv_r_wind,inv_r_traffic,inv_r_aero
    real phim,phih,phi_m,phi_h,FF_temp,Rib,eps
    integer v,iterations,i
    real kappa,g,T0K,a,b,p,q,pi
    parameter (kappa=0.4,g=9.8,T0K=273.15,a=16.,b=5.,p=-0.25,q=-0.5,pi=3.1415926)
    
    iterations=2
    phi_m=0.0
    phi_h=0.0
     
    do i=1,iterations
    
        !Calculate wind at temperature height
        FF_temp=max(0.2,FF*(log(z_T/z0)-phi_m)/(log(z_FF/z0)-phi_m))
        !Set bilk Richardsons number
        Rib=g/(Tc+T0K)*z_T*(Tc-Ts)/FF_temp/FF_temp
        !Calculate z/L
        eps=Rib*(log(z_T/z0)-phi_m)*(log(z_T/z0)-phi_m)/((log(z_T/z0t)-phi_h))
 
        if (eps.ge.0) then
            phim=1+b*eps
            phih=1+b*eps
            phi_m=-b*eps
            phi_h=-b*eps
        else
            phim=exp(p*log((1.-a*eps)))
            phih=exp(q*log((1.-a*eps)))
            phi_m=2.*log((1.+1./phim)/2.)+log((1.+1./(phim*phim))/2.)-2.*atan(1./phim)+pi/2.
            phi_h=2.*log((1.+1./phih)/2.)
        endif

    enddo

    !Calculate bulk exchange coefficient including wind (inverse of aerodynamic resistance)
    inv_r_wind=FF_temp*kappa*kappa/((log(z_T/z0)-phi_m)*(log(z_T/z0t)-phi_h));
    
    inv_r_traffic=0
    do v=1,num_veh
        inv_r_traffic=inv_r_traffic+N_v(v)*V_veh(v)*a_traffic(v)
    enddo
    
    inv_r_traffic=max(1e-6,inv_r_traffic/3600./3.6)
    inv_r_aero=inv_r_traffic+inv_r_wind
    r_aero_func_with_stability=1./inv_r_aero

    end function r_aero_func_with_stability
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

    if (V_ref.eq.0) then
        f_V=1.
    else        
        f_V=(V_veh/V_ref)
    endif

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

    if (V_ref.eq.0) then
        f_V=1.
    else        
        f_V=(V_veh/V_ref)
    endif
    
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

!----------------------------------------------------------------------
    function Energy_correction_func(dE1,dE2)
        !Energy_correction_func: Used for correcting energy balance based on observed road temperature
        implicit none

        !Input
        real, intent(in) :: dE1 !Energy difference in last time step
        real, intent(in) :: dE2 !Energy difference in second to last timestep
        !Output
        real :: Energy_correction_func
        !Local
        real f !Determines weighting between dE1 and dE2

        Energy_correction_func = f*dE1 + (1-f)*dE2
    
    end function Energy_correction_func
!----------------------------------------------------------------------    

!----------------------------------------------------------------------    
    function relaxation_func(forecast_step)
        !Used to relax the energy correction during the course of the forecast
        !TODO: Consider making the lin_array a function of forecast steps, so that there will be a decrease at every timestep for 10 min runs.

        !Input
        integer, intent(in) :: forecast_step

        !Local
        real, dimension(3),parameter :: lin_array=(/3.0, 1.5, 0.0/)

        real :: relaxation_func

        if (forecast_step > 3) then
            relaxation_func = 0.0
        else
            relaxation_func = lin_array(forecast_step)

        end if
    end function relaxation_func