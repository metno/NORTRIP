!NORTRIP_save_road_emissions_data_netcdf.f90
        
!==========================================================================
!   NORTRIP model NORTRIP_save_road_emissions_data_netcdf
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

!===========Create and write emissions files==================================

subroutine NORTRIP_create_emissions_netcdf(filename,ncid)
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
    integer :: char_dimid
    integer :: exists
    integer :: a(num_date_index)
    integer,dimension(8) :: datetime_now
    character(len=256) :: history_string

    character(8)  :: date
    character(10) :: time
    character(5)  :: zone
    call date_and_time(date = date,time=time,zone=zone)    

    history_string = "Created at: "//date//" "//time(1:2)//":"//time(3:4)//zone//"UTC"
    call check(nf90_create(trim(filename),IOR(NF90_NETCDF4, NF90_CLOBBER),ncid))
    !Add global attributes: 
    call check(nf90_put_att(ncid,nf90_global,"title","NORTRIP output"))
    call check(nf90_put_att(ncid,nf90_global,"history",trim(history_string)))
    call check(nf90_put_att(ncid,nf90_global,"institution","Norwegian Meteorological Institute, MET Norway"))
    
    !call check(nf90_put_att(ncid,nf90_global,"Conventions","CF 1.10")) !TODO: Check which convention should be followed.
    
    call check(nf90_def_dim(ncid,"time", nf90_unlimited, t_dimid))
    call check(nf90_def_dim(ncid,"road_id",n_save_links_netcdf, f_dimid))
    
    call check(nf90_def_dim(ncid,"maxcharlength", int(24/dt) , char_dimid)) !NOTE: This might not be needed if the writing to variable "datetime" (string) is handled better..

    call check(nf90_def_var(ncid, "time", nf90_float, t_dimid,varid))
    call check(nf90_put_att(ncid,varid, "units", "seconds since "//trim(date_str(4,min_time)))) !Time dimension as seconds since start of simulation.
    call check(nf90_put_att(ncid,varid, "calendar", "standard"))
    call check(nf90_put_att(ncid,varid, "long_name", "time"))
    
    call check(nf90_def_var(ncid, "datetime", nf90_char, (/char_dimid,t_dimid/),varid))
    call check(nf90_put_att(ncid,varid, "description", "date and time in format yyyy.mm.dd HH:MM:SS"))
    call check(nf90_put_att(ncid,varid, "long_name", "yyyy.mm.dd_HH:MM:SS"))
    
    call check(nf90_def_var(ncid, "road_id", nf90_int, f_dimid,varid))
    !call check(nf90_put_att(ncid,varid, "units", ""))
    call check(nf90_put_att(ncid,varid, "description", "ID number for road link"))
    call check(nf90_put_att(ncid,varid, "long_name", "road_link_id"))

    call check(nf90_def_var(ncid, "lat", nf90_float, f_dimid,varid))
    call check(nf90_put_att(ncid,varid, "description", "latitude for road link"))
    call check(nf90_put_att(ncid,varid, "long_name", "latitude"))

    call check(nf90_def_var(ncid, "lon", nf90_float, f_dimid,varid))
    call check(nf90_put_att(ncid,varid, "description", "longitude for road link"))
    call check(nf90_put_att(ncid,varid, "long_name", "longitude"))


    if ( ANY(roadtype_index==normal_roadtype) ) then  !TODO: This if-test could be further refined to include variables only applicable for e.g tunnels
        
        call check(nf90_def_var(ncid, "PM10_Emissions_tot", nf90_float, (/f_dimid,t_dimid/),varid))     
        call check(nf90_put_att(ncid,varid, "units", "g/km/h")) 
        call check(nf90_put_att(ncid,varid,"description","Total non-exhaust emissions of PM10")) 

        call check(nf90_def_var(ncid, "PM25_Emissions_tot", nf90_float, (/f_dimid,t_dimid/),varid)) 
        call check(nf90_put_att(ncid,varid, "units", "g/km/h")) 
        call check(nf90_put_att(ncid,varid,"description","Total non-exhaust emissions of PM2.5")) 
        
        call check(nf90_def_var(ncid, "PM10_Emissions_roadwear", nf90_float, (/f_dimid,t_dimid/),varid)) 
        call check(nf90_put_att(ncid,varid, "units", "g/km/h")) 
        call check(nf90_put_att(ncid,varid,"description","Emissions of PM10 from roadwear")) 
        
        call check(nf90_def_var(ncid, "PM10_Emissions_tyrewear", nf90_float, (/f_dimid,t_dimid/),varid)) 
        call check(nf90_put_att(ncid,varid, "units", "g/km/h")) 
        call check(nf90_put_att(ncid,varid,"description","Emissions of PM10 from tyrewear")) 
        
        call check(nf90_def_var(ncid, "PM10_Emissions_brakewear", nf90_float, (/f_dimid,t_dimid/),varid)) 
        call check(nf90_put_att(ncid,varid, "units", "g/km/h")) 
        call check(nf90_put_att(ncid,varid,"description","Emissions of PM10 from brakewear")) 
        
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

        call check(nf90_def_var(ncid, "NOX_Emissions_tot", nf90_float, (/f_dimid,t_dimid/),varid)) 
        call check(nf90_put_att(ncid,varid, "units", "g/km/h")) 
        call check(nf90_put_att(ncid,varid,"description","Total emissions of NOX")) 

    endif

    call check(nf90_enddef(ncid))
end subroutine NORTRIP_create_emissions_netcdf

subroutine NORTRIP_save_road_emissions_data_netcdf
    
    use netcdf
    use NORTRIP_definitions
    
    implicit none
    
    !LOCAL
    integer :: varid, t_varid
    integer :: t_dimid
    integer :: f_dimid
    integer :: exists
    real :: timestamp
    character(len=256)      :: filename
    character(len=19) :: datetime
    integer ro_num
    real ,dimension(max_time_save) :: st_li
    real ,dimension(max_time_save) :: st_he
    real ,dimension(max_time_save) :: fr_hdv
    integer :: a(num_date_index)
    real    :: conversion
    real, dimension(max_time_save) :: water,snow,ice
    real, parameter :: surface_moisture_cutoff = -5 !0.01
    integer, dimension(2) :: runway_match
    integer :: runway_index

    !Check that path exists after filling in date stamp
    a=date_data(:,min_time_save)
    
    filename =trim(path_outputdata)//trim(filename_outputdata)//'_emissions.nc'
    !Check that path exists after filling in date stamp
    a=date_data(:,min_time_save)
    
    !Put in date if required
    call date_to_datestr_bracket(a,filename,filename)
    call date_to_datestr_bracket(a,filename,filename)
    call date_to_datestr_bracket(a,filename,filename)
    
    write(unit_logfile,'(A)') '================================================================'
    write(unit_logfile,'(A)') 'Create netcdf emissions file'
    write(unit_logfile,'(A)') '================================================================'
    write(*,*) "Filename: ", filename
    call NORTRIP_create_emissions_netcdf(filename,ncid_emissions)

    ! !Calculate time as seconds since base date
    timestamp=0
    do ti=min_time_save,max_time_save   
        !Write time: 
        timestamp = (ti-1)*dt*60*60
        call check(nf90_inq_varid(ncid_emissions, "time",t_varid))
        call check(nf90_put_var(ncid_emissions, t_varid, timestamp, start = (/ti/)))

        call check(nf90_inq_varid(ncid_emissions, "datetime",varid))
        call check(nf90_put_var(ncid_emissions, varid, trim(date_str(4,ti)),start = (/1,ti/)))
        
    enddo

    call check(nf90_inq_varid(ncid_emissions, "road_id",varid))
    call check(nf90_put_var(ncid_emissions, varid, save_emissions_1d_vars(emissions_road_id_index,:), start = (/1/)))

    call check(nf90_inq_varid(ncid_emissions, "lat",varid))
    call check(nf90_put_var(ncid_emissions, varid, save_emissions_1d_vars(emissions_lat_index,:), start = (/1/)))

    call check(nf90_inq_varid(ncid_emissions, "lon",varid))
    call check(nf90_put_var(ncid_emissions, varid, save_emissions_1d_vars(emissions_lon_index,:), start = (/1/)))

    call check  (nf90_inq_varid(ncid_emissions, "PM10_Emissions_tot",varid))
    call check(nf90_put_var(ncid_emissions, varid,  save_emissions_2d_vars(emissions_PM10_Emissions_tot_index,:,:), start = (/1,1/), count = (/n_save_links_netcdf,max_time_save/)))
    
    call check(nf90_inq_varid(ncid_emissions, "PM25_Emissions_tot",varid))
    call check(nf90_put_var(ncid_emissions, varid, save_emissions_2d_vars(emissions_PM25_Emissions_tot_index,:,:), start = (/1,1/), count = (/n_save_links_netcdf,max_time_save/)))

    call check(nf90_inq_varid(ncid_emissions, "PM10_Emissions_roadwear",varid))
    call check(nf90_put_var(ncid_emissions, varid,  save_emissions_2d_vars(emissions_PM10_Emissions_roadwear_index,:,:), start = (/1,1/), count = (/n_save_links_netcdf,max_time_save/)))

    call check(nf90_inq_varid(ncid_emissions, "PM10_Emissions_tyrewear",varid))
    call check(nf90_put_var(ncid_emissions, varid, save_emissions_2d_vars(emissions_PM10_Emissions_tyrewear_index,:,:), start = (/1,1/), count = (/n_save_links_netcdf,max_time_save/)))

    call check(nf90_inq_varid(ncid_emissions, "PM10_Emissions_brakewear",varid))
    call check(nf90_put_var(ncid_emissions, varid,  save_emissions_2d_vars(emissions_PM10_Emissions_brakewear_index,:,:), start = (/1,1/), count = (/n_save_links_netcdf,max_time_save/)))

    call check(nf90_inq_varid(ncid_emissions, "PM10_Emissions_sand",varid))
    call check(nf90_put_var(ncid_emissions, varid,  save_emissions_2d_vars(emissions_PM10_Emissions_sand_index,:,:), start = (/1,1/), count = (/n_save_links_netcdf,max_time_save/)))
    
    call check(nf90_inq_varid(ncid_emissions, "PM10_Emissions_fugitive",varid))
    call check(nf90_put_var(ncid_emissions, varid,  save_emissions_2d_vars(emissions_PM10_Emissions_fugitive_index,:,:), start = (/1,1/), count = (/n_save_links_netcdf,max_time_save/)))
    
    call check(nf90_inq_varid(ncid_emissions, "PM10_Emissions_exhaust",varid))
    call check(nf90_put_var(ncid_emissions, varid,  save_emissions_2d_vars(emissions_PM10_Emissions_exhaust_index,:,:), start = (/1,1/), count = (/n_save_links_netcdf,max_time_save/)))
    
    call check(nf90_inq_varid(ncid_emissions, "PM10_Emissions_salt1",varid))
    call check(nf90_put_var(ncid_emissions, varid,  save_emissions_2d_vars(emissions_PM10_Emissions_salt1_index,:,:), start = (/1,1/), count = (/n_save_links_netcdf,max_time_save/)))
    
    call check(nf90_inq_varid(ncid_emissions, "PM10_Emissions_salt2",varid))
    call check(nf90_put_var(ncid_emissions, varid,  save_emissions_2d_vars(emissions_PM10_Emissions_salt2_index,:,:), start = (/1,1/), count = (/n_save_links_netcdf,max_time_save/)))
    
    call check(nf90_inq_varid(ncid_emissions, "PM10_Emissions_direct",varid))
    call check(nf90_put_var(ncid_emissions, varid,  save_emissions_2d_vars(emissions_PM10_Emissions_direct_index,:,:), start = (/1,1/), count = (/n_save_links_netcdf,max_time_save/)))
    
    call check(nf90_inq_varid(ncid_emissions, "PM10_Emissions_suspension",varid))
    call check(nf90_put_var(ncid_emissions, varid,  save_emissions_2d_vars(emissions_PM10_Emissions_suspension_index,:,:), start = (/1,1/), count = (/n_save_links_netcdf,max_time_save/)))
    
    call check(nf90_inq_varid(ncid_emissions, "PM10_Emissions_windblown",varid))
    call check(nf90_put_var(ncid_emissions, varid,  save_emissions_2d_vars(emissions_PM10_Emissions_windblown_index,:,:), start = (/1,1/), count = (/n_save_links_netcdf,max_time_save/)))
    
    call check(nf90_inq_varid(ncid_emissions, "NOX_Emissions_tot",varid))
    call check(nf90_put_var(ncid_emissions, varid,  save_emissions_2d_vars(emissions_NOX_Emissions_tot_index,:,:), start = (/1,1/), count = (/n_save_links_netcdf,max_time_save/)))
    

        write(*,*) "Closing netcdf emissions file: ", trim(filename)
        call check(nf90_close(ncid_emissions))
end subroutine NORTRIP_save_road_emissions_data_netcdf

!===========================================================================
subroutine NORTRIP_fill_emissions_array(save_road_counter)
    use NORTRIP_definitions

    implicit none

    integer, intent(in) :: save_road_counter
    
    tr = 1 !NOTE: Assume there is only one track! Needs to be modified if the option for more then one track is implemented. 
    ro = 0 !NOTE: Assume that use_single_road_loop_flag is True!

    save_emissions_1d_vars(emissions_road_id_index,save_road_counter) = road_ID(ro) 
    save_emissions_1d_vars(emissions_lat_index,save_road_counter) = LAT(ro) 
    save_emissions_1d_vars(emissions_lon_index,save_road_counter) = LON(ro)

    save_emissions_2d_vars(emissions_PM10_Emissions_tot_index,save_road_counter,:) = sum(E_road_data(total_dust_index,pm_10,E_total_index,:,:,ro),dim=2)
    save_emissions_2d_vars(emissions_PM25_Emissions_tot_index,save_road_counter,:) = sum(E_road_data(total_dust_index,pm_25,E_total_index,:,:,ro),dim=2)
    save_emissions_2d_vars(emissions_PM10_Emissions_roadwear_index,save_road_counter,:) = sum(E_road_data(road_index,pm_10,E_total_index,:,:,ro),dim=2)
    save_emissions_2d_vars(emissions_PM10_Emissions_tyrewear_index,save_road_counter,:) = sum(E_road_data(tyre_index,pm_10,E_total_index,:,:,ro),dim=2)
    save_emissions_2d_vars(emissions_PM10_Emissions_brakewear_index,save_road_counter,:) = sum(E_road_data(brake_index,pm_10,E_total_index,:,:,ro),dim=2)
    save_emissions_2d_vars(emissions_PM10_Emissions_sand_index,save_road_counter,:) = sum(E_road_data(sand_index,pm_10,E_total_index,:,:,ro),dim=2)
    save_emissions_2d_vars(emissions_PM10_Emissions_fugitive_index,save_road_counter,:) = sum(E_road_data(fugitive_index,pm_10,E_total_index,:,:,ro),dim=2)
    save_emissions_2d_vars(emissions_PM10_Emissions_exhaust_index,save_road_counter,:) =  sum(E_road_data(exhaust_index,pm_10,E_total_index,:,:,ro),dim=2)  
    save_emissions_2d_vars(emissions_PM10_Emissions_salt1_index,save_road_counter,:) = sum(E_road_data(salt_index(1),pm_10,E_total_index,:,:,ro),dim=2)
    save_emissions_2d_vars(emissions_PM10_Emissions_salt2_index,save_road_counter,:) = sum(E_road_data(salt_index(2),pm_10,E_total_index,:,:,ro),dim=2)
    save_emissions_2d_vars(emissions_PM10_Emissions_direct_index,save_road_counter,:)        = sum(E_road_data(total_dust_index,pm_10,E_direct_index,:,:,ro),dim=2)
    save_emissions_2d_vars(emissions_PM10_Emissions_suspension_index,save_road_counter,:)       = sum(E_road_data(total_dust_index,pm_10,E_suspension_index,:,:,ro),dim=2)
    save_emissions_2d_vars(emissions_PM10_Emissions_windblown_index,save_road_counter,:)       = sum(E_road_data(total_dust_index,pm_10,E_windblown_index,:,:,ro),dim=2)
    save_emissions_2d_vars(emissions_NOX_Emissions_tot_index,save_road_counter,:)         = airquality_data(NOX_emis_index,:,ro)

end subroutine NORTRIP_fill_emissions_array