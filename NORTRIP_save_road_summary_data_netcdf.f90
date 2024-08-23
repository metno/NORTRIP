!NORTRIP_save_road_summary_data_netcdf.f90
        
!==========================================================================
!   NORTRIP model NORTRIP_save_road_summary_data_netcdf
!==========================================================================
subroutine check(status)
    use netcdf

    !implicit none
    integer, intent(in) :: status

    if ( status /= nf90_noerr ) then
        print*, nf90_strerror(status)

        stop "Stopped"
    end if
end subroutine check

!===========Create and write summary files==================================

subroutine NORTRIP_create_summary_netcdf(filename,ncid)
    use netcdf
    use NORTRIP_definitions

    implicit none

    !INPUT
    character(256), intent(in)      :: filename

    !OUTPUT
    integer, intent(out) :: ncid 

    !LOCAL
    integer :: varid
    integer :: t_dimid
    integer :: f_dimid
    integer :: date_dimid
    integer :: exists
    character(len=4)    :: time_string
    character(len=12)   :: datetime_string
    integer             :: datetime_int
    integer :: a(num_date_index)
    integer,dimension(8) :: datetime_now
    character(len=256) :: history_string

    character(8)  :: date
    character(10) :: time
    character(5)  :: zone
    call date_and_time(date = date,time=time,zone=zone)    

    history_string = "Created at: "//date//" "//time(1:2)//":"//time(3:4)//zone//"UTC"
    call check(nf90_create(trim(filename),nf90_clobber,ncid))
    
    !Add global attributes: 
    call check(nf90_put_att(ncid,nf90_global,"title","NORTRIP output"))
    call check(nf90_put_att(ncid,nf90_global,"history",trim(history_string)))
    call check(nf90_put_att(ncid,nf90_global,"institution","Norwegian Meteorological Institute, MET Norway"))
    
    !call check(nf90_put_att(ncid,nf90_global,"Conventions","CF 1.10")) !TODO: Check which convention should be followed.
    
    call check(nf90_def_dim(ncid,"time", nf90_unlimited, t_dimid))
    
    call check(nf90_def_dim(ncid,"road_id",n_roads_total, f_dimid))
    
    call check(nf90_def_dim(ncid,"maxdatelength", int(24/dt) , date_dimid)) !NOTE: This might not be needed if the writing to variable "datetime" (string) is handled better..
    
    call check(nf90_def_var(ncid, "time", nf90_float, t_dimid,varid))
    call check(nf90_put_att(ncid,varid, "units", "seconds since "//trim(date_str(4,min_time)))) !Time dimension as seconds since start of simulation.
    call check(nf90_put_att(ncid,varid, "calendar", "standard"))
    call check(nf90_put_att(ncid,varid, "long_name", "time"))
    
    call check(nf90_def_var(ncid, "datetime", nf90_char, (/date_dimid,t_dimid/),varid))
    call check(nf90_put_att(ncid,varid, "description", "date and time in format yyyy.mm.dd HH:MM:SS"))
    call check(nf90_put_att(ncid,varid, "long_name", "yyyy.mm.dd_HH:MM:SS"))
    
    call check(nf90_def_var(ncid, "road_id", nf90_int, f_dimid,varid))
    !call check(nf90_put_att(ncid,varid, "units", ""))
    call check(nf90_put_att(ncid,varid, "description", "ID number for road link"))
    call check(nf90_put_att(ncid,varid, "long_name", "road_link_id"))
    
    call check(nf90_def_var(ncid, "T_surf_mod", nf90_float, (/f_dimid,t_dimid/),varid))
    call check(nf90_put_att(ncid,varid, "units", "Celsius"))
    call check(nf90_put_att(ncid,varid, "long_name", "surface_temperature"))
    call check(nf90_put_att(ncid,varid,"description","Modeled surface temperature"))
    
    call check(nf90_def_var(ncid, "T_surf_meteo", nf90_float, (/f_dimid,t_dimid/),varid))
    call check(nf90_put_att(ncid,varid, "units", "Celsius"))
    call check(nf90_put_att(ncid,varid,"description","Observed surface temperature, otherwise nodata value")) 
    call check(nf90_put_att(ncid,varid,"long_name","surface_temperature")) 
    
    call check(nf90_def_var(ncid, "T_air", nf90_float, (/f_dimid,t_dimid/),varid))
    call check(nf90_put_att(ncid,varid, "units", "Celsius"))
    call check(nf90_put_att(ncid,varid,"description","Air temperature at 2 m above ground")) 
    call check(nf90_put_att(ncid,varid,"long_name","air_temperature")) 
    
    call check(nf90_def_var(ncid, "Td_air", nf90_float, (/f_dimid,t_dimid/),varid))
    call check(nf90_put_att(ncid,varid, "units", "Celsius"))
    call check(nf90_put_att(ncid,varid,"description","Dew point temperature at 2 m above ground, calc. from relative humidity (RH).")) 
    call check(nf90_put_att(ncid,varid,"long_name","dew_point_temperature")) 
    
    call check(nf90_def_var(ncid, "T_freeze_mod", nf90_float, (/f_dimid,t_dimid/),varid))
    call check(nf90_put_att(ncid,varid, "units", "Celsius"))
    call check(nf90_put_att(ncid,varid,"description","Freezing point temperature"))
    call check(nf90_put_att(ncid,varid,"long_name","freezing_point_temperature"))
    
    call check(nf90_def_var(ncid, "T_sub_mod", nf90_float, (/f_dimid,t_dimid/),varid))
    call check(nf90_put_att(ncid,varid, "units", "Celsius"))
    call check(nf90_put_att(ncid,varid,"description","Modeled subsurface temperature"))
    call check(nf90_put_att(ncid,varid,"long_name","sub_surface_temperature_temperature"))
    
    call check(nf90_def_var(ncid, "RH_air", nf90_float, (/f_dimid,t_dimid/),varid))
    call check(nf90_put_att(ncid,varid, "units", "percent"))
    call check(nf90_put_att(ncid,varid,"description","Relative humidity at 2 m above ground"))
    call check(nf90_put_att(ncid,varid,"long_name","relative_humidity")) 
    
    call check(nf90_def_var(ncid, "Rain", nf90_float, (/f_dimid,t_dimid/),varid))
    call check(nf90_put_att(ncid,varid, "units", "mm"))
    call check(nf90_put_att(ncid,varid,"description","Amount of liquid precipitation within the model time step"))
    call check(nf90_put_att(ncid,varid,"long_name","rainfall_amount")) 
    
    call check(nf90_def_var(ncid, "Snow", nf90_float, (/f_dimid,t_dimid/),varid))
    call check(nf90_put_att(ncid,varid, "units", "mm"))
    call check(nf90_put_att(ncid,varid,"description","Amount of solid precipitation within the model time step (water equivalent)"))
    call check(nf90_put_att(ncid,varid,"long_name","snowfall_amount")) 
    
    call check(nf90_def_var(ncid, "Wind_FF", nf90_float, (/f_dimid,t_dimid/),varid)) !TODO: Rename to Wind_speed?
    call check(nf90_put_att(ncid,varid, "units", "m/s"))
    call check(nf90_put_att(ncid,varid,"description","Wind speed at 10 m above ground"))
    call check(nf90_put_att(ncid,varid,"long_name","wind_speed")) 
    
    call check(nf90_def_var(ncid, "Wind_DD", nf90_float, (/f_dimid,t_dimid/),varid)) !TODO: Rename to Wind_direction?
    call check(nf90_put_att(ncid,varid, "units", "degree"))
    call check(nf90_put_att(ncid,varid,"description","Wind from direction at 10 m above ground"))
    call check(nf90_put_att(ncid,varid,"long_name","wind_from_direction")) 
    
    call check(nf90_def_var(ncid, "SW_rad_cls", nf90_float, (/f_dimid,t_dimid/),varid)) 
    call check(nf90_put_att(ncid,varid, "units", "W/m2"))
    call check(nf90_put_att(ncid,varid,"description","Clear sky incoming short wave radiation"))
    call check(nf90_put_att(ncid,varid,"long_name","downwelling_shortwave_flux_in_air_assuming_clear_sky")) 
    
    call check(nf90_def_var(ncid, "SW_rad_in", nf90_float, (/f_dimid,t_dimid/),varid)) 
    call check(nf90_put_att(ncid,varid, "units", "W/m2"))
    call check(nf90_put_att(ncid,varid,"description","Incoming short wave radiation"))
    call check(nf90_put_att(ncid,varid,"long_name","downwelling_shortwave_flux_in_air")) 
    
    call check(nf90_def_var(ncid, "SW_rad_net", nf90_float, (/f_dimid,t_dimid/),varid)) 
    call check(nf90_put_att(ncid,varid, "units", "W/m2"))
    call check(nf90_put_att(ncid,varid,"description","Net short wave radiation"))
    call check(nf90_put_att(ncid,varid,"long_name","net_downward_shortwave_flux_in_air")) 
    
    
    call check(nf90_def_var(ncid, "LW_rad_net", nf90_float, (/f_dimid,t_dimid/),varid)) 
    call check(nf90_put_att(ncid,varid, "units", "W/m2"))
    call check(nf90_put_att(ncid,varid,"description","Net long wave radiation"))
    call check(nf90_put_att(ncid,varid,"long_name","net_downward_longwave_flux_in_air")) 
    
    call check(nf90_def_var(ncid, "LW_rad_in", nf90_float, (/f_dimid,t_dimid/),varid)) 
    call check(nf90_put_att(ncid,varid, "units", "W/m2"))
    call check(nf90_put_att(ncid,varid,"description","Incoming long wave radiation"))
    call check(nf90_put_att(ncid,varid,"long_name","downwelling_longwave_flux_in_air")) 
    
    call check(nf90_def_var(ncid, "H_in", nf90_float, (/f_dimid,t_dimid/),varid)) 
    call check(nf90_put_att(ncid,varid, "units", "W/m2"))
    call check(nf90_put_att(ncid,varid,"description","Surface sensible heat flux, positive downwards"))
    call check(nf90_put_att(ncid,varid,"long_name","surface_downward_sensible_heat_flux")) 
    
    call check(nf90_def_var(ncid, "L_in", nf90_float, (/f_dimid,t_dimid/),varid)) 
    call check(nf90_put_att(ncid,varid, "units", "W/m2"))
    call check(nf90_put_att(ncid,varid,"description","Surface latent heat flux, positive downwards"))
    call check(nf90_put_att(ncid,varid,"long_name","surface_downward_latent_heat_flux")) 
    
    call check(nf90_def_var(ncid, "G_sub", nf90_float, (/f_dimid,t_dimid/),varid)) 
    call check(nf90_put_att(ncid,varid, "units", "W/m2"))
    call check(nf90_put_att(ncid,varid,"description","Sub-surface energy flux"))
    call check(nf90_put_att(ncid,varid,"long_name","sub_surface_energy_flux")) !Need better long_name/long_name 
    
    call check(nf90_def_var(ncid, "G_net", nf90_float, (/f_dimid,t_dimid/),varid)) 
    call check(nf90_put_att(ncid,varid, "units", "W/m2"))
    call check(nf90_put_att(ncid,varid,"description","Surface energy flux"))
    call check(nf90_put_att(ncid,varid,"long_name","surface_energy_flux")) !Need better long_name/long_name 
    
    call check(nf90_def_var(ncid, "Energy_correction", nf90_float, (/f_dimid,t_dimid/),varid)) 
    call check(nf90_put_att(ncid,varid, "units", "W/m2"))
    call check(nf90_put_att(ncid,varid,"description","Energy correction term used in surface energy balance"))
    call check(nf90_put_att(ncid,varid,"long_name","energy_correction_term")) 
    
    call check(nf90_def_var(ncid, "Energy_difference", nf90_float, (/f_dimid,t_dimid/),varid)) 
    call check(nf90_put_att(ncid,varid, "units", "W/m2"))
    call check(nf90_put_att(ncid,varid,"description","Energy difference needed to match observed surface temperature"))
    call check(nf90_put_att(ncid,varid,"long_name","energy_difference_to_match_observed_surface_temperature")) 
    
    call check(nf90_def_var(ncid, "W_surf_mod", nf90_float, (/f_dimid,t_dimid/),varid))
    call check(nf90_put_att(ncid,varid, "units", "mm"))
    call check(nf90_put_att(ncid,varid,"description","Water mass on the road surface (water equivalent)")) 
    call check(nf90_put_att(ncid,varid,"long_name","liquid_water_on_surface")) 
    
    call check(nf90_def_var(ncid, "I_surf_mod", nf90_float, (/f_dimid,t_dimid/),varid))
    call check(nf90_put_att(ncid,varid, "units", "mm"))
    call check(nf90_put_att(ncid,varid,"description","Ice mass on the road surface (water equivalent)")) 
    call check(nf90_put_att(ncid,varid,"long_name","ice_on_surface_as_water_equivalent")) 
    
    call check(nf90_def_var(ncid, "S_surf_mod", nf90_float, (/f_dimid,t_dimid/),varid))
    call check(nf90_put_att(ncid,varid, "units", "mm"))
    call check(nf90_put_att(ncid,varid,"description","Snow mass on the road surface (water equivalent)")) 
    call check(nf90_put_att(ncid,varid,"long_name","snow_on_surface_as_water_equivalent")) 
    
    call check(nf90_def_var(ncid, "f_q", nf90_float, (/f_dimid,t_dimid/),varid)) !NOTE: Give this a more descriptive name?
    call check(nf90_put_att(ncid,varid, "units", "1"))
    call check(nf90_put_att(ncid,varid,"description","Surface retainment factor (0-1) based on the surface moisture. All is retained when value is zero.")) 
    call check(nf90_put_att(ncid,varid,"long_name","surface_retainment_factor")) 
    
    call check(nf90_def_var(ncid, "Salt1_a", nf90_float, (/f_dimid,t_dimid/),varid)) !NOTE: Give this a more descriptive name? Salting instead of salt?
    call check(nf90_put_att(ncid,varid, "units", "g/m2"))
    call check(nf90_put_att(ncid,varid,"description","Total mass of NaCl applied in the time step")) !TODO: Need to rethink this in the context of 10 min resolution. 
    call check(nf90_put_att(ncid,varid,"long_name","mass_of_applied_NaCl")) 
    
    call check(nf90_def_var(ncid, "Salt2_a", nf90_float, (/f_dimid,t_dimid/),varid)) !NOTE: Give this a more descriptive name?
    call check(nf90_put_att(ncid,varid, "units", "g/m2"))
    call check(nf90_put_att(ncid,varid,"description","Total mass of alternative salt applied in the time step")) !TODO: Replace "alternative" with a string read from parameters?
    call check(nf90_put_att(ncid,varid,"long_name","mass_of_applied_alternative_salt")) 
    
    call check(nf90_def_var(ncid, "Sand_a", nf90_float, (/f_dimid,t_dimid/),varid)) 
    call check(nf90_put_att(ncid,varid, "units", "g/m2"))
    call check(nf90_put_att(ncid,varid,"description","Total mass of sand applied in the time step"))
    call check(nf90_put_att(ncid,varid,"long_name","mass_of_applied_sand")) 
    
    call check(nf90_def_var(ncid, "Wetting_a", nf90_float, (/f_dimid,t_dimid/),varid)) 
    call check(nf90_put_att(ncid,varid, "units", "mm")) 
    call check(nf90_put_att(ncid,varid,"description","Water added to the road during cleaning or salting "))
    call check(nf90_put_att(ncid,varid,"long_name","water_added_during_activity")) 

    call check(nf90_def_var(ncid, "Ploughing_a", nf90_float, (/f_dimid,t_dimid/),varid)) 
    call check(nf90_put_att(ncid,varid, "units", "1"))
    call check(nf90_put_att(ncid,varid,"description","Snow ploughing event in time step (0 to 1)"))
    call check(nf90_put_att(ncid,varid,"long_name","plowing_event")) 
    
    call check(nf90_def_var(ncid, "Cleaning_a", nf90_float, (/f_dimid,t_dimid/),varid)) 
    call check(nf90_put_att(ncid,varid, "units", "1")) 
    call check(nf90_put_att(ncid,varid,"description","Road cleaning event in time step (0 to 1). Value denote max. cleaning efficiency")) 
    call check(nf90_put_att(ncid,varid,"long_name","cleaning_event")) 
    
    call check(nf90_def_var(ncid, "Mass_salt1", nf90_float, (/f_dimid,t_dimid/),varid)) 
    call check(nf90_put_att(ncid,varid, "units", "g/m2")) 
    call check(nf90_put_att(ncid,varid,"description","Mass of NaCl on road"))
    call check(nf90_put_att(ncid,varid,"long_name","Mass_of_NaCl_on_road"))
    
    call check(nf90_def_var(ncid, "Mass_salt2", nf90_float, (/f_dimid,t_dimid/),varid)) 
    call check(nf90_put_att(ncid,varid, "units", "g/m2")) 
    call check(nf90_put_att(ncid,varid,"description","Mass of alternative salt on road"))
    call check(nf90_put_att(ncid,varid,"long_name","mass_of_alternative_salt_on_road"))

    if ( .not. index(calculation_type,'Avinor').gt.0 ) then        
        call check(nf90_def_var(ncid, "PM10_Emissions_tot", nf90_float, (/f_dimid,t_dimid/),varid))     
        call check(nf90_put_att(ncid,varid, "units", "g/km/h")) !NOTE: doublecheck units
        call check(nf90_put_att(ncid,varid,"description","Total non-exhaust emissions of PM10")) 

        call check(nf90_def_var(ncid, "PM25_Emissions_tot", nf90_float, (/f_dimid,t_dimid/),varid)) 
        call check(nf90_put_att(ncid,varid, "units", "g/km/h")) !NOTE: doublecheck units
        call check(nf90_put_att(ncid,varid,"description","Total non-exhaust emissions of PM2.5")) 
        
        call check(nf90_def_var(ncid, "PM10_Emissions_roadwear", nf90_float, (/f_dimid,t_dimid/),varid)) 
        call check(nf90_put_att(ncid,varid, "units", "g/km/h")) !NOTE: doublecheck units
        call check(nf90_put_att(ncid,varid,"description","Emissions of PM10 from roadwear")) 
        
        call check(nf90_def_var(ncid, "PM10_Emissions_tyrewear", nf90_float, (/f_dimid,t_dimid/),varid)) 
        call check(nf90_put_att(ncid,varid, "units", "g/km/h")) !NOTE: doublecheck units
        call check(nf90_put_att(ncid,varid,"description","Emissions of PM10 from tyrewear")) 
        
        call check(nf90_def_var(ncid, "PM10_Emissions_breakwear", nf90_float, (/f_dimid,t_dimid/),varid)) 
        call check(nf90_put_att(ncid,varid, "units", "g/km/h")) !NOTE: doublecheck units
        call check(nf90_put_att(ncid,varid,"description","Emissions of PM10 from breakwear")) 
        
        call check(nf90_def_var(ncid, "PM10_Emissions_sand", nf90_float, (/f_dimid,t_dimid/),varid)) 
        call check(nf90_put_att(ncid,varid, "units", "g/km/h")) 
        call check(nf90_put_att(ncid,varid,"description","Emissions of PM10 from sanding"))
        
        call check(nf90_def_var(ncid, "PM10_Emissions_fugitive", nf90_float, (/f_dimid,t_dimid/),varid)) 
        call check(nf90_put_att(ncid,varid, "units", "g/km/h")) 
        call check(nf90_put_att(ncid,varid,"description","Fugitive emissions of PM10"))
        
        call check(nf90_def_var(ncid, "PM10_Emissions_exhaust", nf90_float, (/f_dimid,t_dimid/),varid)) 
        call check(nf90_put_att(ncid,varid, "units", "g/km/h")) 
        call check(nf90_put_att(ncid,varid,"description","Emissions of PM10 from exhaust"))

        call check(nf90_def_var(ncid, "PM10_Emissions_salt1", nf90_float, (/f_dimid,t_dimid/),varid)) 
        call check(nf90_put_att(ncid,varid, "units", "g/km/h")) 
        call check(nf90_put_att(ncid,varid,"description","Emissions of PM10 from NaCl"))

        call check(nf90_def_var(ncid, "PM10_Emissions_salt2", nf90_float, (/f_dimid,t_dimid/),varid)) 
        call check(nf90_put_att(ncid,varid, "units", "g/km/h")) 
        call check(nf90_put_att(ncid,varid,"description","Emissions of PM10 from alternative salt"))
        
        call check(nf90_def_var(ncid, "PM10_Emissions_direct", nf90_float, (/f_dimid,t_dimid/),varid)) 
        call check(nf90_put_att(ncid,varid, "units", "g/km/h")) 
        call check(nf90_put_att(ncid,varid,"description","Direct emissions of PM10"))
        
        call check(nf90_def_var(ncid, "PM10_Emissions_suspension", nf90_float, (/f_dimid,t_dimid/),varid)) 
        call check(nf90_put_att(ncid,varid, "units", "g/km/h")) 
        call check(nf90_put_att(ncid,varid,"description","suspension emissions of PM10"))
        
        call check(nf90_def_var(ncid, "PM10_Emissions_windblown", nf90_float, (/f_dimid,t_dimid/),varid)) 
        call check(nf90_put_att(ncid,varid, "units", "g/km/h")) 
        call check(nf90_put_att(ncid,varid,"description","windblown emissions of PM10"))

        call check(nf90_def_var(ncid, "Traffic", nf90_float, (/f_dimid,t_dimid/),varid)) 
        call check(nf90_put_att(ncid,varid, "units", "veh")) 
        call check(nf90_put_att(ncid,varid,"description","Annual average daily traffic (AADT)")) 
        
        call check(nf90_def_var(ncid, "HDV", nf90_float, (/f_dimid,t_dimid/),varid)) 
        call check(nf90_put_att(ncid,varid, "units", "%")) 
        call check(nf90_put_att(ncid,varid,"description","Percentage of heavy duty vehicles")) 
        
        call check(nf90_def_var(ncid, "Studs_li", nf90_float, (/f_dimid,t_dimid/),varid)) 
        call check(nf90_put_att(ncid,varid, "units", "%")) 
        call check(nf90_put_att(ncid,varid,"description","Percentage of light vehicles with studded tyres")) 
        
        call check(nf90_def_var(ncid, "Studs_he", nf90_float, (/f_dimid,t_dimid/),varid)) 
        call check(nf90_put_att(ncid,varid, "units", "%")) 
        call check(nf90_put_att(ncid,varid,"description","Percentage of heavy vehicles with studded tyres"))
        
        call check(nf90_def_var(ncid, "Speed_li", nf90_float, (/f_dimid,t_dimid/),varid)) 
        call check(nf90_put_att(ncid,varid, "units", "km/h")) 
        call check(nf90_put_att(ncid,varid,"description","Speed of light vehicles")) 
        !TODO: Why is only the speed of light vehicles included in the summary file?
        
        call check(nf90_def_var(ncid, "NOX_Emissions_tot", nf90_float, (/f_dimid,t_dimid/),varid)) 
        call check(nf90_put_att(ncid,varid, "units", "g/km/h")) 
        call check(nf90_put_att(ncid,varid,"description","Total emissions of NOX")) 

        call check(nf90_def_var(ncid, "Mass_dust_PM200", nf90_float, (/f_dimid,t_dimid/),varid)) 
        call check(nf90_put_att(ncid,varid, "units", "g/m2")) !TODO: Check units and description
        call check(nf90_put_att(ncid,varid,"description","Mass of non-suspendable dust (>200 micrometer) on road"))
        
        call check(nf90_def_var(ncid, "Mass_sand_PM200", nf90_float, (/f_dimid,t_dimid/),varid)) 
        call check(nf90_put_att(ncid,varid, "units", "g/m2")) !TODO: Check units and description
        call check(nf90_put_att(ncid,varid,"description","Mass of non-suspendable sand (>200 micrometer) on road"))
    end if

    



    call check(nf90_enddef(ncid))
    
end subroutine NORTRIP_create_summary_netcdf

subroutine NORTRIP_save_road_summary_data_netcdf
    
    use netcdf
    use NORTRIP_definitions
    
    implicit none
    
    !LOCAL
    integer :: varid
    integer :: ncid 
    integer :: t_dimid
    integer :: f_dimid
    integer :: exists
    real :: timestamp
    character(len=4)    :: time_string
    character(len=12)   :: datetime_string
    integer             :: datetime_int
    character(len=256)      :: filename
    character(len=19) :: datetime
    integer ro_num
    real ,dimension(max_time_save) :: st_li
    real ,dimension(max_time_save) :: st_he
    real ,dimension(max_time_save) :: fr_hdv
    integer :: a(num_date_index)
    real    :: conversion
    real, dimension(max_time_save) :: water,snow,ice
    real, parameter :: surface_moisture_cutoff = 0.01

    
    !Check that path exists after filling in date stamp
    a=date_data(:,min_time_save)
    
    filename =trim(path_outputdata)//trim(filename_outputdata)//'_summary.nc'
    !Check that path exists after filling in date stamp
    a=date_data(:,min_time_save)
    
    !Put in date if required
    call date_to_datestr_bracket(a,filename,filename)
    call date_to_datestr_bracket(a,filename,filename)
    call date_to_datestr_bracket(a,filename,filename)
    
    !If file do not exist,  create and open, otherwise just open. 
    inquire(file=trim(filename),exist=exists)
    if (.not.exists) then
        write(unit_logfile,'(A)') '================================================================'
        write(unit_logfile,'(A)') 'Create netcdf summary file'
        write(unit_logfile,'(A)') '================================================================'
        call NORTRIP_create_summary_netcdf(filename,ncid)
        call check(nf90_open(filename,nf90_write,ncid))
    else
        call check(nf90_open(filename,nf90_write,ncid))
    end if

    !NOTE: Track is always = 1. If the model code is extended to include more than one track, this must be changed.
    !NOTE: ro = 0 assumes that the single road flag is true
    tr=1
    ro=0

    !if (save_road_data_flag(ro).ne.0 .and. use_only_special_links_flag ) then !TODO: Unsure how it is best to do it with this if-test in the general case (not just for smartkjemi)
        conversion=1./1000./b_road_lanes(ro)
        save_road_counter = ro_tot 
        timestamp=0

        !Calculate time as seconds since base date
        do ti=min_time_save,max_time_save   

            !Write time: 
            timestamp = (ti-1)*dt*60*60
            call check(nf90_inq_varid(ncid, "time",varid))
            call check(nf90_put_var(ncid, varid, timestamp, start = (/ti/)))

            call check(nf90_inq_varid(ncid, "datetime",varid))
            call check(nf90_put_var(ncid, varid, trim(date_str(4,ti)),start = (/1,ti/)))
        enddo

        !Calculate percentages of studded tyres and heavy duty vehicles
        st_li=traffic_data(N_st_li_index,:,ro)/traffic_data(N_li_index,:,ro)*100.
        if (isnan(st_li(i))) st_li(i)=0.
        st_he=traffic_data(N_st_he_index,:,ro)/traffic_data(N_he_index,:,ro)*100.
        if (isnan(st_he(i))) st_he(i)=0.
        fr_hdv=traffic_data(N_he_index,:,ro)/traffic_data(N_total_index,:,ro)*100.
        if (isnan(fr_hdv(i))) fr_hdv(i)=0.

        !Fill netcdf file with variables. NOTE: This is assuming that single road flag is used (It is a bit confusing that the iterator is called ro_tot...)
        call check(nf90_inq_varid(ncid, "road_id",varid))
        call check(nf90_put_var(ncid, varid, road_ID(ro_tot), start = (/save_road_counter/)))

        call check  (nf90_inq_varid(ncid, "T_surf_mod",varid))
        call check(nf90_put_var(ncid, varid, road_meteo_data(T_s_index,:,tr,0), start = (/save_road_counter,1/), count = (/1,max_time_save/)))
        
        call check(nf90_inq_varid(ncid, "Td_air",varid))
        call check(nf90_put_var(ncid, varid, meteo_data(T_dewpoint_index,:,0), start = (/save_road_counter,1/), count = (/1,max_time_save/)))

        call check(nf90_inq_varid(ncid, "T_air",varid))
        call check(nf90_put_var(ncid, varid, meteo_data(T_a_index,:,0), start = (/save_road_counter,1/), count = (/1,max_time_save/)))

        call check(nf90_inq_varid(ncid, "T_freeze_mod",varid))
        call check(nf90_put_var(ncid, varid, road_meteo_data(T_melt_index,:,tr,0), start = (/save_road_counter,1/), count = (/1,max_time_save/)))

        call check(nf90_inq_varid(ncid, "T_sub_mod",varid))
        call check(nf90_put_var(ncid, varid, road_meteo_data(T_sub_index,:,tr,0), start = (/save_road_counter,1/), count = (/1,max_time_save/)))

        call check(nf90_inq_varid(ncid, "T_surf_meteo",varid))
        call check(nf90_put_var(ncid, varid, road_meteo_data(road_temperature_obs_index,:,tr,0), start = (/save_road_counter,1/), count = (/1,max_time_save/)))
        
        call check(nf90_inq_varid(ncid, "RH_air",varid))
        call check(nf90_put_var(ncid, varid, meteo_data(RH_index,:,0), start = (/save_road_counter,1/), count = (/1,max_time_save/)))

        call check(nf90_inq_varid(ncid, "Rain",varid))
        call check(nf90_put_var(ncid, varid, meteo_data(Rain_precip_index,:,0), start = (/save_road_counter,1/), count = (/1,max_time_save/)))

        call check(nf90_inq_varid(ncid, "Snow",varid))
        call check(nf90_put_var(ncid, varid, meteo_data(Snow_precip_index,:,0), start = (/save_road_counter,1/), count = (/1,max_time_save/)))

        call check(nf90_inq_varid(ncid, "Wind_FF",varid))
        call check(nf90_put_var(ncid, varid, meteo_data(FF_index,:,0), start = (/save_road_counter,1/), count = (/1,max_time_save/)))

        call check(nf90_inq_varid(ncid, "Wind_DD",varid))
        call check(nf90_put_var(ncid, varid, meteo_data(DD_index,:,0), start = (/save_road_counter,1/), count = (/1,max_time_save/)))

        call check(nf90_inq_varid(ncid, "SW_rad_cls",varid))
        call check(nf90_put_var(ncid, varid, meteo_data(short_rad_in_clearsky_index,:,0), start = (/save_road_counter,1/), count = (/1,max_time_save/)))

        call check(nf90_inq_varid(ncid, "SW_rad_in",varid))
        call check(nf90_put_var(ncid, varid, meteo_data(short_rad_in_index,:,0), start = (/save_road_counter,1/), count = (/1,max_time_save/)))

        call check(nf90_inq_varid(ncid, "LW_rad_in",varid))
        call check(nf90_put_var(ncid, varid, meteo_data(long_rad_in_index,:,0), start = (/save_road_counter,1/), count = (/1,max_time_save/)))

        call check(nf90_inq_varid(ncid, "SW_rad_net",varid))
        call check(nf90_put_var(ncid, varid, road_meteo_data(short_rad_net_index,:,tr,0), start = (/save_road_counter,1/), count = (/1,max_time_save/)))

        call check(nf90_inq_varid(ncid, "LW_rad_net",varid))
        call check(nf90_put_var(ncid, varid, road_meteo_data(long_rad_net_index,:,tr,0), start = (/save_road_counter,1/), count = (/1,max_time_save/)))

        call check(nf90_inq_varid(ncid, "H_in",varid))
        call check(nf90_put_var(ncid, varid, -road_meteo_data(H_index,:,tr,0), start = (/save_road_counter,1/), count = (/1,max_time_save/)))

        call check(nf90_inq_varid(ncid, "L_in",varid))
        call check(nf90_put_var(ncid, varid, -road_meteo_data(L_index,:,tr,0), start = (/save_road_counter,1/), count = (/1,max_time_save/)))

        call check(nf90_inq_varid(ncid, "G_sub",varid))
        call check(nf90_put_var(ncid, varid, road_meteo_data(G_sub_index,:,tr,0), start = (/save_road_counter,1/), count = (/1,max_time_save/)))

        call check(nf90_inq_varid(ncid, "G_net",varid))
        call check(nf90_put_var(ncid, varid, road_meteo_data(G_index,:,tr,0), start = (/save_road_counter,1/), count = (/1,max_time_save/)))

        call check(nf90_inq_varid(ncid, "Energy_correction",varid))
        call check(nf90_put_var(ncid, varid, road_meteo_data(E_corr_index,:,tr,0), start = (/save_road_counter,1/), count = (/1,max_time_save/)))

        call check(nf90_inq_varid(ncid, "Energy_difference",varid))
        call check(nf90_put_var(ncid, varid, road_meteo_data(E_diff_index,:,tr,0), start = (/save_road_counter,1/), count = (/1,max_time_save/)))

        call check(nf90_inq_varid(ncid, "W_surf_mod",varid))
        where(g_road_data(water_index,:,tr,0) .ge. surface_moisture_cutoff)
            water = g_road_data(water_index,:,tr,0)
        elsewhere
            water = 0.
        endwhere
        call check(nf90_put_var(ncid, varid, water, start = (/save_road_counter,1/), count = (/1,max_time_save/)))

        call check(nf90_inq_varid(ncid, "I_surf_mod",varid))
        where(g_road_data(ice_index,:,tr,0) .ge. surface_moisture_cutoff)
            ice = g_road_data(ice_index,:,tr,0)
        elsewhere
            ice = 0.
        endwhere
        call check(nf90_put_var(ncid, varid, ice, start = (/save_road_counter,1/), count = (/1,max_time_save/)))

        call check(nf90_inq_varid(ncid, "S_surf_mod",varid))
        where(g_road_data(snow_index,:,tr,0) .ge. surface_moisture_cutoff)
            snow = g_road_data(snow_index,:,tr,0)
        elsewhere
            snow = 0.
        endwhere
        call check(nf90_put_var(ncid, varid, snow, start = (/save_road_counter,1/), count = (/1,max_time_save/)))

        call check(nf90_inq_varid(ncid, "f_q",varid))
        call check(nf90_put_var(ncid, varid, f_q(road_index,:,tr,0), start = (/save_road_counter,1/), count = (/1,max_time_save/)))

        call check(nf90_inq_varid(ncid, "Salt1_a",varid))
        call check(nf90_put_var(ncid, varid, activity_data(M_salting_index(1),:,0), start = (/save_road_counter,1/), count = (/1,max_time_save/)))

        call check(nf90_inq_varid(ncid, "Salt2_a",varid))
        call check(nf90_put_var(ncid, varid, activity_data(M_salting_index(2),:,0), start = (/save_road_counter,1/), count = (/1,max_time_save/)))

        call check(nf90_inq_varid(ncid, "Sand_a",varid))
        call check(nf90_put_var(ncid, varid, activity_data(M_sanding_index,:,0)*f_PM_bin(sand_index,pm_all,1), start = (/save_road_counter,1/), count = (/1,max_time_save/)))

        call check(nf90_inq_varid(ncid, "Wetting_a",varid))
        call check(nf90_put_var(ncid, varid, activity_data(g_road_wetting_index,:,0), start = (/save_road_counter,1/), count = (/1,max_time_save/)))

        call check(nf90_inq_varid(ncid, "Ploughing_a",varid))
        call check(nf90_put_var(ncid, varid, activity_data(t_ploughing_index,:,0)*h_ploughing_moisture(snow_index), start = (/save_road_counter,1/), count = (/1,max_time_save/)))

        call check(nf90_inq_varid(ncid, "Cleaning_a",varid))
        call check(nf90_put_var(ncid, varid, activity_data(t_cleaning_index,:,0)*efficiency_of_cleaning(ro_tot), start = (/save_road_counter,1/), count = (/1,max_time_save/)))
                
        call check(nf90_inq_varid(ncid, "Mass_salt1",varid))
        call check(nf90_put_var(ncid, varid, sum(M_road_data(salt_index(1),pm_all,:,:,ro),dim=2)*conversion, start = (/save_road_counter,1/), count = (/1,max_time_save/)))

        call check(nf90_inq_varid(ncid, "Mass_salt2",varid))
        call check(nf90_put_var(ncid, varid, sum(M_road_data(salt_index(2),pm_all,:,:,ro),dim=2)*conversion, start = (/save_road_counter,1/), count = (/1,max_time_save/)))

        if ( .not. index(calculation_type,'Avinor').gt.0 ) then
            call check(nf90_inq_varid(ncid, "Traffic",varid))
            call check(nf90_put_var(ncid, varid, traffic_data(N_total_index,:,ro), start = (/save_road_counter,1/), count = (/1,max_time_save/)))

            call check(nf90_inq_varid(ncid, "HDV",varid))
            call check(nf90_put_var(ncid, varid, fr_hdv , start = (/save_road_counter,1/), count = (/1,max_time_save/)))

            call check(nf90_inq_varid(ncid, "Studs_li",varid))
            call check(nf90_put_var(ncid, varid, st_li,  start = (/save_road_counter,1/), count = (/1,max_time_save/)))

            call check(nf90_inq_varid(ncid, "Studs_he",varid))
            call check(nf90_put_var(ncid, varid, st_he,  start = (/save_road_counter,1/), count = (/1,max_time_save/)))

            call check(nf90_inq_varid(ncid, "Speed_li",varid))
            call check(nf90_put_var(ncid, varid, traffic_data(V_li_index,:,ro), start = (/save_road_counter,1/), count = (/1,max_time_save/)))
            
            call check(nf90_inq_varid(ncid, "NOX_Emissions_tot",varid))
            call check(nf90_put_var(ncid, varid, airquality_data(NOX_emis_index,:,ro), start = (/save_road_counter,1/), count = (/1,max_time_save/)))
                    
            call check(nf90_inq_varid(ncid, "PM10_Emissions_tot",varid))
            call check(nf90_put_var(ncid, varid, sum(E_road_data(total_dust_index,pm_10,E_total_index,:,:,ro),dim=2), start = (/save_road_counter,1/), count = (/1,max_time_save/)))
            
            call check(nf90_inq_varid(ncid, "PM25_Emissions_tot",varid))
            call check(nf90_put_var(ncid, varid, sum(E_road_data(total_dust_index,pm_25,E_total_index,:,:,ro),dim=2), start = (/save_road_counter,1/), count = (/1,max_time_save/)))
            
            call check(nf90_inq_varid(ncid, "PM10_Emissions_roadwear",varid))
            call check(nf90_put_var(ncid, varid, sum(E_road_data(road_index,pm_10,E_total_index,:,:,ro),dim=2), start = (/save_road_counter,1/), count = (/1,max_time_save/)))
            
            call check(nf90_inq_varid(ncid, "PM10_Emissions_tyrewear",varid))
            call check(nf90_put_var(ncid, varid, sum(E_road_data(tyre_index,pm_10,E_total_index,:,:,ro),dim=2), start = (/save_road_counter,1/), count = (/1,max_time_save/)))

            call check(nf90_inq_varid(ncid, "PM10_Emissions_breakwear",varid))
            call check(nf90_put_var(ncid, varid, sum(E_road_data(brake_index,pm_10,E_total_index,:,:,ro),dim=2), start = (/save_road_counter,1/), count = (/1,max_time_save/)))

            call check(nf90_inq_varid(ncid, "PM10_Emissions_sand",varid))
            call check(nf90_put_var(ncid, varid, sum(E_road_data(sand_index,pm_10,E_total_index,:,:,ro),dim=2), start = (/save_road_counter,1/), count = (/1,max_time_save/)))

            call check(nf90_inq_varid(ncid, "PM10_Emissions_fugitive",varid))
            call check(nf90_put_var(ncid, varid, sum(E_road_data(fugitive_index,pm_10,E_total_index,:,:,ro),dim=2), start = (/save_road_counter,1/), count = (/1,max_time_save/)))

            call check(nf90_inq_varid(ncid, "PM10_Emissions_exhaust",varid))
            call check(nf90_put_var(ncid, varid, sum(E_road_data(exhaust_index,pm_10,E_total_index,:,:,ro),dim=2), start = (/save_road_counter,1/), count = (/1,max_time_save/)))

            call check(nf90_inq_varid(ncid, "PM10_Emissions_salt1",varid))
            call check(nf90_put_var(ncid, varid, sum(E_road_data(salt_index(1),pm_10,E_total_index,:,:,ro),dim=2), start = (/save_road_counter,1/), count = (/1,max_time_save/)))

            call check(nf90_inq_varid(ncid, "PM10_Emissions_salt2",varid))
            call check(nf90_put_var(ncid, varid, sum(E_road_data(salt_index(2),pm_10,E_total_index,:,:,ro),dim=2), start = (/save_road_counter,1/), count = (/1,max_time_save/)))

            call check(nf90_inq_varid(ncid, "PM10_Emissions_direct",varid))
            call check(nf90_put_var(ncid, varid, sum(E_road_data(total_dust_index,pm_10,E_direct_index,:,:,ro),dim=2), start = (/save_road_counter,1/), count = (/1,max_time_save/)))

            call check(nf90_inq_varid(ncid, "PM10_Emissions_suspension",varid))
            call check(nf90_put_var(ncid, varid, sum(E_road_data(total_dust_index,pm_10,E_suspension_index,:,:,ro),dim=2), start = (/save_road_counter,1/), count = (/1,max_time_save/)))

            call check(nf90_inq_varid(ncid, "PM10_Emissions_windblown",varid))
            call check(nf90_put_var(ncid, varid, sum(E_road_data(total_dust_index,pm_10,E_windblown_index,:,:,ro),dim=2), start = (/save_road_counter,1/), count = (/1,max_time_save/)))

            call check(nf90_inq_varid(ncid, "Mass_dust_PM200",varid))
            call check(nf90_put_var(ncid, varid, sum(M_road_data(total_dust_index,pm_200,:,:,ro),dim=2)*conversion, start = (/save_road_counter,1/), count = (/1,max_time_save/)))
    
            call check(nf90_inq_varid(ncid, "Mass_sand_PM200",varid))
            call check(nf90_put_var(ncid, varid, sum(M_road_data(sand_index,pm_200,:,:,ro),dim=2)*conversion, start = (/save_road_counter,1/), count = (/1,max_time_save/)))
        end if

!    endif

    call check(nf90_close(ncid))
end subroutine NORTRIP_save_road_summary_data_netcdf

!===========================================================================
