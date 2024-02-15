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
subroutine NORTRIP_main_run_forecast_prepare(tf,bias_correction,forecast_index) !Prepare for forecast; Set previous temperature (in tf-1) to Tobs, or calculate bias_correction. forecast_T_s is allocated here

    use NORTRIP_definitions
    
    implicit none
    
    !Input
    integer, intent(in) :: tf

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
end subroutine NORTRIP_main_run_forecast_prepare

subroutine NORTRIP_main_run_forecast_calculate(tf,bias_correction,forecast_index, forecast_T_s) !Fill forecast_T_s using a method determined by forecast_type

    use NORTRIP_definitions
    
    implicit none
    
    !Input
    integer, intent(in) :: tf
    real, intent(in)    :: bias_correction
    integer, intent(in) :: forecast_index
    
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

        !Local
        integer :: tf

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


end module 