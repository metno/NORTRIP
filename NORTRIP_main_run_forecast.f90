!****************************************************************************
!  NORTRIP_main_run_forecast.f90 !NOTE: Should be renamed, if so remember to update makefile
!****************************************************************************
!
!   MODULE :        NORTRIP_main_run_forecast
!   PURPOSE:        Contains subroutines related to the calculation and storing of forecasted data
!   CALLS TO:       
!   SUBROUTINES:    NORTRIP_main_run_forecast_prepare
!                   NORTRIP_main_run_forecast_calculate
!                   NORTRIP_main_run_forecast_save
!   FUNCTIONS:                      
!   VERSION:        15.02.2024
!   AUTHOR:         Bruce Rolstad Denby, Elin Aas 
!                   Norwegian Meteorological Institute (www.met.no)
!
!****************************************************************************
 module NORTRIP_main_run_forecast
     implicit none
 contains

    subroutine NORTRIP_main_run_forecast_prepare(bias_correction,forecast_index) !Prepare for forecast; Set previous temperature (in tf-1) to Tobs, or calculate bias_correction. forecast_T_s is allocated here

        use NORTRIP_definitions
        
        implicit none
        
        !Input
        !integer, intent(in) :: tf

        !Output:
        real,intent(out) :: bias_correction
        integer,intent(out) :: forecast_index

        forecast_index=max(0,forecast_hour-1);

        !Set the previous (initial) model surface temperature to the observed surface temperature in forecast mode
        !Do not do this if bias correction is used for the forecast
        if (forecast_hour.gt.0.and.forecast_type.ne.4.and.forecast_type.ne.5) then
            tr=1       
            if (road_meteo_data(road_temperature_obs_index,max(min_time,tf-1),tr,ro).ne.nodata) then
                road_meteo_data(T_s_index,max(min_time,tf-1),:,ro)=road_meteo_data(road_temperature_obs_index,max(min_time,tf-1),:,ro)
                
            endif
        endif
        
        !Bias correction
        bias_correction=0.
        if (forecast_hour.gt.0.and.forecast_type.eq.4) then
            tr=1       
            if (road_meteo_data(road_temperature_obs_index,max(min_time,tf-1),tr,ro).ne.nodata) then
                bias_correction=-(road_meteo_data(T_s_index,max(min_time,tf-1),tr,ro)-road_meteo_data(road_temperature_obs_index,max(min_time,tf-1),tr,ro))
                
            endif
        endif
        
        !Energy correction
        if ( forecast_hour .gt. 0 .and. forecast_type .eq. 5 ) then
            tr=1
            if (road_meteo_data(road_temperature_obs_index,max(min_time,tf-1),tr,ro).ne.nodata) then
                call E_diff_func &
                    (road_meteo_data(road_temperature_obs_index,max(min_time,tf-1),tr,ro) &
                    ,road_meteo_data(T_s_index,max(min_time,tf-2),tr,ro) &
                    ,meteo_data(T_a_index,max(min_time,tf-1),ro) &
                    ,road_meteo_data(T_sub_index,max(min_time,tf-1),tr,ro) &
                    ,road_meteo_data(E_corr_index,max(min_time,tf-1),tr,ro) &
                    ,meteo_data(pressure_index,max(min_time,tf-1),ro) &
                    ,dzs &
                    ,dt &
                    ,road_meteo_data(r_aero_t_index,max(min_time,tf-1),tr,ro) &
                    ,road_meteo_data(r_aero_q_index,max(min_time,tf-1),tr,ro) &
                    ,road_meteo_data(rad_net_index,max(min_time,tf-1),tr,ro) &
                    ,meteo_data(long_rad_in_index,max(min_time,tf-1),ro) &
                    ,road_meteo_data(H_traffic_index,max(min_time,tf-1),tr,ro) &
                    ,road_meteo_data(L_index,max(min_time,tf-1),tr,ro) &
                    !,G_freeze & !find this
                    !,G_melt & !find this
                    ,sub_surf_param &
                    ,use_subsurface_flag &
                    !Output starts here
                    ,road_meteo_data(E_diff_index,tf,tr,ro) &
                    ,road_meteo_data(E_corr_index,tf,tr,ro) &
                    ,road_meteo_data(T_s_index,max(min_time,tf-1),tr,ro))
            end if
        end if
    end subroutine NORTRIP_main_run_forecast_prepare

    subroutine NORTRIP_main_run_forecast_calculate(bias_correction,forecast_index, forecast_T_s) !Fill forecast_T_s using a method determined by forecast_type

        use NORTRIP_definitions
        
        implicit none
        
        !Input
        !integer,intent(in) :: tf
        real,   intent(in) :: bias_correction
        integer,intent(in) :: forecast_index
        
        !Output:
        real,allocatable,intent(inout) :: forecast_T_s(:,:)

        !Save the forecast surface temperature into the +forecast index if the starting surface temperature was valid
        tr=1
        if (forecast_hour.gt.0.and.tf+forecast_index.le.max_time.and.road_meteo_data(road_temperature_obs_index,max(min_time,tf-1),tr,ro).ne.nodata) then
            !modelled
            if (forecast_type.eq.1) then
                forecast_T_s(min(max_time,tf+forecast_index),:)=road_meteo_data(T_s_index,min(max_time,tf+forecast_index),:,ro)
            endif
            !persistence
            if (forecast_type.eq.2) then
                forecast_T_s(min(max_time,tf+forecast_index),:)=road_meteo_data(T_s_index,max(tf-1,min_time),:,ro);
            endif
            !linear extrapolation
            if (forecast_type.eq.3.and.tf.ge.min_time+2.and.road_meteo_data(T_s_index,max(min_time,tf-2),tr,ro).ne.nodata) then
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
            else 
                forecast_T_s(min(max_time,tf+forecast_index),:)=nodata
        endif

    end subroutine NORTRIP_main_run_forecast_calculate

    subroutine NORTRIP_main_run_forecast_save(forecast_T_s,forecast_index)
        use NORTRIP_definitions
        
        implicit none
        
        !Input
        real,allocatable,intent(in)    :: forecast_T_s(:,:)
        integer,intent(in) :: forecast_index


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
    end subroutine NORTRIP_main_run_forecast_save

    subroutine E_diff_func(T_obs, TCs_0, TC, TCsub, E_correction_old,P,dzs_in,dt_h_in,r_aero_t,r_aero_q,short_net,long_in,H_traffic,L,sub_surf_param,use_subsurface_flag, E_diff, E_correction, T_new)
            !G_freeze, G_melt
        use NORTRIP_index_definitions

        implicit none

        !Input:
        real, intent(in) :: T_obs
        real, intent(in) :: TCs_0
        real, intent(in) :: TC
        real, intent(in) :: TCsub
        real, intent(in) :: E_correction_old
        real, intent(in) :: P
        real, intent(in) :: dzs_in
        real, intent(in) :: dt_h_in
        real, intent(in) :: r_aero_t
        real, intent(in) :: r_aero_q
        real, intent(in) :: short_net
        real, intent(in) :: long_in
        real, intent(in) :: H_traffic
        real, intent(in) :: L
        real, intent(in) :: sub_surf_param(3)
        integer, intent(in) :: use_subsurface_flag
        
        
        !Output: TODO: Review these names
        real, intent(out) :: E_diff
        real, intent(out) :: E_correction
        real, intent(out) :: T_new
        
        !Local:
        real Cp,lambda,lambda_ice,lambda_melt
        parameter (Cp=1006,lambda=2.50E6,lambda_ice=2.83E6,lambda_melt=3.33E5) !(J/kg)
        real RD,T0C,sigma,eps_s,omega
        parameter (RD=287.0,T0C=273.15,sigma=5.67E-8,eps_s=0.95,omega=7.3e-5)
        real :: G_freeze !TODO: Find out if these should come from *_submodel_4
        real :: G_melt
        
        real :: dt_sec
        real :: dt_h
        real :: rho_s
        real :: c_s
        real :: k_s_road
        real :: c_rho_s
        real :: dzs,mu
        real :: TK_a
        real :: rho
        real :: TCs
        real :: a_G
        real :: a_rad
        real :: a_RL
        real :: b_RL
        real :: a_H

        !Function
        real :: Energy_correction_func
        
        !Set time step in seconds
        dt_sec=dt_h_in*3600

        G_freeze = 0
        G_melt = 0

        !Set subsurface parameters
        rho_s=sub_surf_param(1)
        c_s=sub_surf_param(2)
        k_s_road=sub_surf_param(3)
        c_rho_s=rho_s*c_s

        !Automatically set dzs if it is 0.
        !This calculated value of dzs is optimal for a sinusoidal varying flux
        if (dzs_in.eq.0.) then
            dzs=(k_s_road/c_rho_s/2/omega)**.5
        else
            dzs=dzs_in
        endif
        mu=omega*c_rho_s*dzs
        
        !If subsurface flux is turned of
        if (use_subsurface_flag.eq.0) then
            mu=0
        endif

        !Set atmospheric temperature in Kelvin
        TK_a=T0C+TC

        !Set air density
        rho=P*100./(RD*TK_a)

        !Preset values of constants for implicit solution
        a_G=1.0/(c_rho_s*dzs)
        a_rad=short_net+long_in*eps_s+H_traffic
        a_RL=(1-4*TC/TK_a)*eps_s*sigma*TK_a**4
        b_RL=4.0*eps_s*sigma*TK_a**3
        a_H=rho*Cp/r_aero_t

        !TODO: these might need to change if we make changes in the double loop in
        !surface_energy_model_4_submodel
        !G_freeze = 0;
        !G_melt = 0;

        !calculate the energy difference needed to make T_mod = T_obs
        E_diff = (T_obs*(1+dt_sec*a_G*(a_H+b_RL+mu))-TCs_0)/(dt_sec*a_G)-a_rad+a_RL+L-a_H*TC-mu*TCsub + G_melt-G_freeze; !=E_diff + E_correction_old

        E_correction = Energy_correction_func(E_diff,E_correction_old);

        !Use the new E_correction to determine surface temperature. If f = 1 in
        !Energy_correction_func, T_new = T_obs
        T_new = (TCs_0+dt_sec*a_G*(a_rad-a_RL-L+a_H*TC+mu*TCsub-G_melt+G_freeze+E_correction))/(1+dt_sec*a_G*(a_H+b_RL+mu));

    end subroutine E_diff_func
end module 