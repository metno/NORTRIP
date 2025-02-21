!NORTRIP_save_output_data_netcdf.f90
        
!==========================================================================
!   NORTRIP model NORTRIP_save_output_data_netcdf
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


subroutine NORTRIP_save_output_data_netcdf
    
    use netcdf
    use NORTRIP_definitions
    
    implicit none
    
    !LOCAL
    integer :: ncid_summary 
    integer :: ncid_emissions 
    integer :: ncid_activity
    integer :: ncid_meteo 

    integer :: varid
    integer :: t_dimid
    integer :: f_dimid
    integer :: char_dimid
    
    character(256)      :: filename_summary
    character(256)      :: filename_activity
    character(256)      :: filename_meteo 
    character(256)      :: filename_emissions
    
    real                :: timestamp
    integer             :: a(num_date_index)
    character(len=256)  :: history_string
    character(8)        :: date
    character(10)       :: time
    character(5)        :: zone
    
    integer, dimension(4) :: ncid_array = -99
    integer :: ncid_iterator
    !TODO: The files all appear in /summary directory now. Should modify the paths here. 
    filename_summary    = trim(path_outputdata)//trim(filename_outputdata)//'_summary.nc'
    filename_activity   = trim(path_outputdata)//trim(filename_outputdata)//'_activities.nc'
    filename_meteo      = trim(path_outputdata)//trim(filename_outputdata)//'_road_meteo.nc'
    filename_emissions  = trim(path_outputdata)//trim(filename_outputdata)//'_emissions.nc'

    !Check that path exists after filling in date stamp
    a=date_data(:,min_time_save)

    !Put in date if required
    call date_to_datestr_bracket(a,filename_summary,filename_summary)
    call date_to_datestr_bracket(a,filename_summary,filename_summary)
    call date_to_datestr_bracket(a,filename_summary,filename_summary)

    call date_to_datestr_bracket(a,filename_activity,filename_activity)
    call date_to_datestr_bracket(a,filename_activity,filename_activity)
    call date_to_datestr_bracket(a,filename_activity,filename_activity)

    call date_to_datestr_bracket(a,filename_meteo,filename_meteo)
    call date_to_datestr_bracket(a,filename_meteo,filename_meteo)
    call date_to_datestr_bracket(a,filename_meteo,filename_meteo)

    call date_to_datestr_bracket(a,filename_emissions,filename_emissions)
    call date_to_datestr_bracket(a,filename_emissions,filename_emissions)
    call date_to_datestr_bracket(a,filename_emissions,filename_emissions)
    
    write(unit_logfile,'(A)') '================================================================'
    write(unit_logfile,'(A)') 'Create netcdf output file(s)'
    write(unit_logfile,'(A)') '================================================================'

    call date_and_time(date = date,time=time,zone=zone)    

    history_string = "Created at: "//date//" "//time(1:2)//":"//time(3:4)//zone//"UTC"
    if (NORTRIP_save_road_summary_data_flag) then 
         call check(nf90_create(trim(filename_summary),IOR(NF90_NETCDF4, NF90_CLOBBER),ncid_summary))
         ncid_array(1) = ncid_summary 
    end if
    if (NORTRIP_save_road_emission_and_mass_data_flag) then 
         call check(nf90_create(trim(filename_emissions),IOR(NF90_NETCDF4, NF90_CLOBBER),ncid_emissions))
        ncid_array(2) = ncid_emissions 
    end if

    if (NORTRIP_save_road_emission_activity_data_flag) then 
        call check(nf90_create(trim(filename_activity),IOR(NF90_NETCDF4, NF90_CLOBBER),ncid_activity))
        ncid_array(3) = ncid_activity
    end if    
    
    if (NORTRIP_save_road_meteo_data_flag) then 
        call check(nf90_create(trim(filename_meteo),IOR(NF90_NETCDF4, NF90_CLOBBER),ncid_meteo))
        ncid_array(4) = ncid_meteo 
    end if

    !Add global attributes to output files: 
    do ncid_iterator = 1,size(ncid_array)
        if (ncid_array(ncid_iterator) .ne. -99) then
            call check(nf90_put_att(ncid_array(ncid_iterator),nf90_global,"title","NORTRIP output"))
            call check(nf90_put_att(ncid_array(ncid_iterator),nf90_global,"history",trim(history_string)))
            call check(nf90_put_att(ncid_array(ncid_iterator),nf90_global,"institution","Norwegian Meteorological Institute, MET Norway"))
            
            !call check(nf90_put_att(ncid_array(ncid_iterator),nf90_global,"Conventions","CF 1.10")) !TODO: Check which convention should be followed.
            
            call check(nf90_def_dim(ncid_array(ncid_iterator),"time", nf90_unlimited, t_dimid))
            call check(nf90_def_dim(ncid_array(ncid_iterator),"road_id",n_save_links_netcdf, f_dimid))
            
            call check(nf90_def_dim(ncid_array(ncid_iterator),"maxcharlength", int(24/dt) , char_dimid)) !NOTE: This might not be needed if the writing to variable "datetime" (string) is handled better..

            call check(nf90_def_var(ncid_array(ncid_iterator), "time", nf90_float, t_dimid,varid))
            call check(nf90_put_att(ncid_array(ncid_iterator),varid, "units", "seconds since "//trim(date_str(4,min_time)))) !Time dimension as seconds since start of simulation.
            call check(nf90_put_att(ncid_array(ncid_iterator),varid, "calendar", "standard"))
            call check(nf90_put_att(ncid_array(ncid_iterator),varid, "long_name", "time"))
            
            call check(nf90_def_var(ncid_array(ncid_iterator), "datetime", nf90_char, (/char_dimid,t_dimid/),varid))
            call check(nf90_put_att(ncid_array(ncid_iterator),varid, "description", "date and time in format yyyy.mm.dd HH:MM:SS"))
            call check(nf90_put_att(ncid_array(ncid_iterator),varid, "long_name", "yyyy.mm.dd_HH:MM:SS"))
            
            call check(nf90_def_var(ncid_array(ncid_iterator), "road_id", nf90_int, f_dimid,varid))
            call check(nf90_put_att(ncid_array(ncid_iterator),varid, "description", "ID number for road link"))
            call check(nf90_put_att(ncid_array(ncid_iterator),varid, "long_name", "road_link_id"))

            call check(nf90_def_var(ncid_array(ncid_iterator), "lat", nf90_float, f_dimid,varid))
            call check(nf90_put_att(ncid_array(ncid_iterator),varid, "description", "latitude for road link"))
            call check(nf90_put_att(ncid_array(ncid_iterator),varid, "long_name", "latitude"))

            call check(nf90_def_var(ncid_array(ncid_iterator), "lon", nf90_float, f_dimid,varid))
            call check(nf90_put_att(ncid_array(ncid_iterator),varid, "description", "longitude for road link"))
            call check(nf90_put_att(ncid_array(ncid_iterator),varid, "long_name", "longitude"))
            call check(nf90_enddef(ncid_array(ncid_iterator)))
        end if
    end do

    !Define and give attributes to the variables going into the output files
    do v = 1, size(save_vars)
        if (save_vars(v)%save_in_summary .and. NORTRIP_save_road_summary_data_flag) then
            if (allocated(save_vars(v)%data_1d)) call check(nf90_def_var(ncid_summary, trim(save_vars(v)%varname), nf90_float, (/f_dimid/),varid))
            if (allocated(save_vars(v)%data_2d)) call check(nf90_def_var(ncid_summary, trim(save_vars(v)%varname), nf90_float, (/f_dimid,t_dimid/),varid))
            call check(nf90_put_att(ncid_summary,varid, "description", trim(save_vars(v)%description)))
            call check(nf90_put_att(ncid_summary,varid, "long_name", trim(save_vars(v)%long_name)))
            call check(nf90_put_att(ncid_summary,varid, "units", trim(save_vars(v)%units)))
        end if 
        
        if (save_vars(v)%save_in_emissions .and. NORTRIP_save_road_emission_and_mass_data_flag) then
            if (allocated(save_vars(v)%data_1d)) call check(nf90_def_var(ncid_emissions, trim(save_vars(v)%varname), nf90_float, (/f_dimid/),varid))
            if (allocated(save_vars(v)%data_2d)) call check(nf90_def_var(ncid_emissions, trim(save_vars(v)%varname), nf90_float, (/f_dimid,t_dimid/),varid))
            call check(nf90_put_att(ncid_emissions,varid, "description", trim(save_vars(v)%description)))
            call check(nf90_put_att(ncid_emissions,varid, "long_name", trim(save_vars(v)%long_name)))
            call check(nf90_put_att(ncid_emissions,varid, "units", trim(save_vars(v)%units)))
        end if 
        
        if (save_vars(v)%save_in_activity .and. NORTRIP_save_road_emission_activity_data_flag) then
            if (allocated(save_vars(v)%data_1d)) call check(nf90_def_var(ncid_activity, trim(save_vars(v)%varname), nf90_float, (/f_dimid/),varid))
            if (allocated(save_vars(v)%data_2d)) call check(nf90_def_var(ncid_activity, trim(save_vars(v)%varname), nf90_float, (/f_dimid,t_dimid/),varid))
            call check(nf90_put_att(ncid_activity,varid, "description", trim(save_vars(v)%description)))
            call check(nf90_put_att(ncid_activity,varid, "long_name", trim(save_vars(v)%long_name)))
            call check(nf90_put_att(ncid_activity,varid, "units", trim(save_vars(v)%units)))
        end if 

        if (save_vars(v)%save_in_meteo .and. NORTRIP_save_road_meteo_data_flag) then
            if (allocated(save_vars(v)%data_1d)) call check(nf90_def_var(ncid_meteo, trim(save_vars(v)%varname), nf90_float, (/f_dimid/),varid))
            if (allocated(save_vars(v)%data_2d)) call check(nf90_def_var(ncid_meteo, trim(save_vars(v)%varname), nf90_float, (/f_dimid,t_dimid/),varid))
            call check(nf90_put_att(ncid_meteo,varid, "description", trim(save_vars(v)%description)))
            call check(nf90_put_att(ncid_meteo,varid, "long_name", trim(save_vars(v)%long_name)))
            call check(nf90_put_att(ncid_meteo,varid, "units", trim(save_vars(v)%units)))
        end if 
    end do
        
    !Put values into the variables that will be equal for all output files.
    do ncid_iterator = 1,size(ncid_array) 
        if (ncid_array(ncid_iterator) .ne. -99) then
            ! !Calculate time as seconds since base date
            timestamp=0
            do ti=min_time_save,max_time_save   
                timestamp = (ti-1)*dt*60*60
                call check(nf90_inq_varid(ncid_array(ncid_iterator), "time",varid))
                call check(nf90_put_var(ncid_array(ncid_iterator), varid, timestamp, start = (/ti/)))

                call check(nf90_inq_varid(ncid_array(ncid_iterator), "datetime",varid))
                call check(nf90_put_var(ncid_array(ncid_iterator), varid, trim(date_str(4,ti)),start = (/1,ti/)))        
            enddo

            call check(nf90_inq_varid(ncid_array(ncid_iterator), "road_id",varid))
            call check(nf90_put_var(ncid_array(ncid_iterator), varid, save_1d_vars(save_road_id_index,:), start = (/1/)))

            call check(nf90_inq_varid(ncid_array(ncid_iterator), "lat",varid))
            call check(nf90_put_var(ncid_array(ncid_iterator), varid, save_1d_vars(save_lat_index,:), start = (/1/)))

            call check(nf90_inq_varid(ncid_array(ncid_iterator), "lon",varid))
            call check(nf90_put_var(ncid_array(ncid_iterator), varid, save_1d_vars(save_lon_index,:), start = (/1/)))
        end if
    end do

    !Put values into the output files. 
    do v = 1,size(save_vars)
        if (save_vars(v)%save_in_summary .and. save_road_summary_data_as_netcdf_flag) then
            if (allocated(save_vars(v)%data_2d)) then
                call check(nf90_inq_varid(ncid_summary,save_vars(v)%varname,varid))
                call check(nf90_put_var(ncid_summary, varid, save_vars(v)%data_2d, start = (/1,1/), count = (/n_save_links_netcdf,max_time_save/)))
            else if (allocated(save_vars(v)%data_1d)) then
                call check(nf90_inq_varid(ncid_summary,trim(save_vars(v)%varname),varid))
                call check(nf90_put_var(ncid_summary, varid, save_vars(v)%data_1d, start = (/1/), count = (/n_save_links_netcdf/))) 
            else 
                write(*,*) "Warning: Do not write variable ", trim(save_vars(v)%varname) , " to summary output file."
            end if
        end if
        
        if (save_vars(v)%save_in_emissions .and. NORTRIP_save_road_emission_and_mass_data_flag) then
            if (allocated(save_vars(v)%data_2d)) then
                call check(nf90_inq_varid(ncid_emissions,save_vars(v)%varname,varid))
                call check(nf90_put_var(ncid_emissions, varid, save_vars(v)%data_2d, start = (/1,1/), count = (/n_save_links_netcdf,max_time_save/)))
            else if (allocated(save_vars(v)%data_1d)) then
                call check(nf90_inq_varid(ncid_emissions,trim(save_vars(v)%varname),varid))
                call check(nf90_put_var(ncid_emissions, varid, save_vars(v)%data_1d, start = (/1/), count = (/n_save_links_netcdf/))) 
            else 
                write(*,*) "Warning: Do not write variable ", trim(save_vars(v)%varname) , " to emissions output file."
            end if
        end if

        if (save_vars(v)%save_in_activity .and. NORTRIP_save_road_emission_activity_data_flag) then
            if (allocated(save_vars(v)%data_2d)) then
                call check(nf90_inq_varid(ncid_activity,trim(save_vars(v)%varname),varid))
                call check(nf90_put_var(ncid_activity, varid, save_vars(v)%data_2d, start = (/1,1/), count = (/n_save_links_netcdf,max_time_save/)))
            else if (allocated(save_vars(v)%data_1d)) then
                    call check(nf90_inq_varid(ncid_activity,trim(save_vars(v)%varname),varid))
                    call check(nf90_put_var(ncid_activity, varid, save_vars(v)%data_1d, start = (/1/), count = (/n_save_links_netcdf/))) 
            else 
                write(*,*) "Warning: Do not write variable ", trim(save_vars(v)%varname) , " to activity output file."
            end if
        end if

        if (save_vars(v)%save_in_meteo .and. NORTRIP_save_road_meteo_data_flag) then
            if (allocated(save_vars(v)%data_2d)) then
                call check(nf90_inq_varid(ncid_meteo,save_vars(v)%varname,varid))
                call check(nf90_put_var(ncid_meteo, varid, save_vars(v)%data_2d, start = (/1,1/), count = (/n_save_links_netcdf,max_time_save/)))
            else if (allocated(save_vars(v)%data_1d)) then
                call check(nf90_inq_varid(ncid_meteo,trim(save_vars(v)%varname),varid))
                call check(nf90_put_var(ncid_meteo, varid, save_vars(v)%data_1d, start = (/1/), count = (/n_save_links_netcdf/))) 
            else 
                write(*,*) "Warning: Do not write variable ", trim(save_vars(v)%varname) , " to meteo output file."
            end if
        end if
    end do

        write(*,*) "Closing netcdf output file(s)."
    do ncid_iterator = 1, size(ncid_array)
        if (ncid_array(ncid_iterator) .ne. -99) call check(nf90_close(ncid_array(ncid_iterator))) 
    end do   
end subroutine NORTRIP_save_output_data_netcdf

!===========================================================================

subroutine NORTRIP_fill_save_array(save_road_counter)
    use NORTRIP_definitions

    implicit none

    !IN: 
    integer, intent(in) :: save_road_counter

    !LOCAL:
    real ,dimension(max_time_save) :: st_li
    real ,dimension(max_time_save) :: st_he
    real ,dimension(max_time_save) :: fr_hdv
    real  :: st_li_sum
    real  :: st_he_sum
    real  :: fr_hdv_sum
    real    :: conversion
    real    :: conversion_sum
    real length_road_km(0:n_roads)
    
    tr = 1 !NOTE: Assume there is only one track! Needs to be modified if the option for more then one track is implemented. 
    ro = 0 !NOTE: Assume that use_single_road_loop_flag is True!

    !Conversion used for for 2d variables: 
    conversion=1./1000./b_road_lanes(ro)

    !Conversion to produce activity data that is in g/m2 to g/km using road length in km
    conversion_sum=b_road_lanes(ro)*1000.

    !Calculate percentages of studded tyres and heavy duty vehicles
    st_li=traffic_data(N_st_li_index,:,ro)/traffic_data(N_li_index,:,ro)*100.
    st_he=traffic_data(N_st_he_index,:,ro)/traffic_data(N_he_index,:,ro)*100.
    fr_hdv=traffic_data(N_he_index,:,ro)/traffic_data(N_total_index,:,ro)*100.
    where ((isnan(st_li)))
        st_li=0.
    endwhere
    where ((isnan(st_he)))
        st_he=0.
    endwhere
    where ((isnan(fr_hdv)))
        fr_hdv=0.
    endwhere
    
    !Calculate percentages of studded tyres and heavy duty vehicles, sum
    st_li_sum=sum(traffic_data(N_st_li_index,min_time_save:max_time_save,ro))/sum(traffic_data(N_li_index,min_time_save:max_time_save,ro))*100.
    if (isnan(st_li_sum)) st_li_sum=0.
    st_he_sum=sum(traffic_data(N_st_he_index,min_time_save:max_time_save,ro))/sum(traffic_data(N_he_index,min_time_save:max_time_save,ro))*100.
    if (isnan(st_he_sum)) st_he_sum=0.
    fr_hdv_sum=sum(traffic_data(N_he_index,min_time_save:max_time_save,ro))/sum(traffic_data(N_total_index,min_time_save:max_time_save,ro))*100.
    if (isnan(fr_hdv_sum)) fr_hdv_sum=0.
    
    !Length of road in km for calculating sum of emissions and activities
    if (length_road(ro).eq.0) then
        length_road_km(ro)=sqrt((x_road(1,ro)-x_road(2,ro))**2+(y_road(1,ro)-y_road(2,ro))**2)/1000.
    else
        length_road_km(ro)=length_road(ro)/1000.
    endif

    !TODO: these could be put into the save_vars array
    save_1d_vars(save_road_id_index,save_road_counter) = road_ID(ro) 
    save_1d_vars(save_lat_index,save_road_counter) = LAT(ro) 
    save_1d_vars(save_lon_index,save_road_counter) = LON(ro)

    !If the "data" arrays are allocated, it means they will be saved in at least one of the output files, and should therefore be filled.
    if (allocated(save_vars(save_T_surf_mod_index)%data_2d))   save_vars(save_T_surf_mod_index)%data_2d(save_road_counter,:)      = road_meteo_data(T_s_index,:,tr,ro)
    if (allocated(save_vars(save_T_sub_mod_index)%data_2d))    save_vars(save_T_sub_mod_index)%data_2d(save_road_counter,:)       = road_meteo_data(T_sub_index,:,tr,ro)
    if (allocated(save_vars(save_T_freeze_mod_index)%data_2d)) save_vars(save_T_freeze_mod_index)%data_2d(save_road_counter,:)    = road_meteo_data(T_melt_index,:,tr,ro)
    if (allocated(save_vars(save_T_surf_meteo_index)%data_2d)) save_vars(save_T_surf_meteo_index)%data_2d(save_road_counter,:)    = road_meteo_data(road_temperature_obs_index,:,tr,ro)
    if (allocated(save_vars(save_T_air_index)%data_2d))        save_vars(save_T_air_index)%data_2d(save_road_counter,:)           = meteo_data(T_a_index,:,ro)
    if (allocated(save_vars(save_Td_air_index)%data_2d))       save_vars(save_Td_air_index)%data_2d(save_road_counter,:)          = meteo_data(T_dewpoint_index,:,ro)
    if (allocated(save_vars(save_RH_air_index)%data_2d))       save_vars(save_RH_air_index)%data_2d(save_road_counter,:)          = meteo_data(RH_index,:,ro)
    if (allocated(save_vars(save_Rain_index)%data_2d))         save_vars(save_Rain_index)%data_2d(save_road_counter,:)            = meteo_data(Rain_precip_index,:,ro)
    if (allocated(save_vars(save_Snow_index)%data_2d))         save_vars(save_Snow_index)%data_2d(save_road_counter,:)            = meteo_data(Snow_precip_index,:,ro)
    if (allocated(save_vars(save_Wind_FF_index)%data_2d))      save_vars(save_Wind_FF_index)%data_2d(save_road_counter,:)         = meteo_data(FF_index,:,ro)
    if (allocated(save_vars(save_Wind_DD_index)%data_2d))      save_vars(save_Wind_DD_index)%data_2d(save_road_counter,:)         = meteo_data(DD_index,:,ro)
    if (allocated(save_vars(save_SW_rad_cls_index)%data_2d))   save_vars(save_SW_rad_cls_index)%data_2d(save_road_counter,:)      = meteo_data(short_rad_in_clearsky_index,:,ro)
    if (allocated(save_vars(save_SW_rad_net_index)%data_2d))   save_vars(save_SW_rad_net_index)%data_2d(save_road_counter,:)      = road_meteo_data(short_rad_net_index,:,tr,ro)
    if (allocated(save_vars(save_SW_rad_in_index)%data_2d))    save_vars(save_SW_rad_in_index)%data_2d(save_road_counter,:)       = road_meteo_data(short_rad_in_index,:,tr,ro)
    if (allocated(save_vars(save_LW_rad_in_index)%data_2d))    save_vars(save_LW_rad_in_index)%data_2d(save_road_counter,:)       = road_meteo_data(long_rad_in_index,:,tr,ro)
    if (allocated(save_vars(save_LW_rad_net_index)%data_2d))   save_vars(save_LW_rad_net_index)%data_2d(save_road_counter,:)      = road_meteo_data(long_rad_net_index,:,tr,ro)
    if (allocated(save_vars(save_G_sub_index)%data_2d))        save_vars(save_G_sub_index)%data_2d(save_road_counter,:)           = road_meteo_data(G_sub_index,:,tr,ro)
    if (allocated(save_vars(save_G_net_index)%data_2d))        save_vars(save_G_net_index)%data_2d(save_road_counter,:)           = road_meteo_data(G_index,:,tr,ro)
    if (allocated(save_vars(save_H_in_index)%data_2d))         save_vars(save_H_in_index)%data_2d(save_road_counter,:)            = road_meteo_data(H_index,:,tr,ro)
    if (allocated(save_vars(save_L_in_index)%data_2d))         save_vars(save_L_in_index)%data_2d(save_road_counter,:)            = road_meteo_data(L_index,:,tr,ro)
    if (allocated(save_vars(save_Traffic_index)%data_2d))      save_vars(save_Traffic_index)%data_2d(save_road_counter,:)         = traffic_data(N_total_index,:,ro)
    if (allocated(save_vars(save_HDV_index)%data_2d))          save_vars(save_HDV_index)%data_2d(save_road_counter,:)             = fr_hdv
    if (allocated(save_vars(save_Studs_li_index)%data_2d))     save_vars(save_Studs_li_index)%data_2d(save_road_counter,:)        = st_li
    if (allocated(save_vars(save_Speed_li_index)%data_2d))     save_vars(save_Speed_li_index)%data_2d(save_road_counter,:)        = traffic_data(V_li_index,:,ro)
    if (allocated(save_vars(save_Studs_he_index)%data_2d))     save_vars(save_Studs_he_index)%data_2d(save_road_counter,:)        = st_he
    if (allocated(save_vars(save_Salt1_a_index)%data_2d))      save_vars(save_Salt1_a_index)%data_2d(save_road_counter,:)         = activity_data(M_salting_index(1),:,ro)
    if (allocated(save_vars(save_Salt2_a_index)%data_2d))      save_vars(save_Salt2_a_index)%data_2d(save_road_counter,:)         = activity_data(M_salting_index(2),:,ro)
    if (allocated(save_vars(save_Sand_a_index)%data_2d))       save_vars(save_Sand_a_index)%data_2d(save_road_counter,:)          = activity_data(M_sanding_index,:,ro)*f_PM_bin(sand_index,pm_all,1)
    if (allocated(save_vars(save_Wetting_a_index)%data_2d))    save_vars(save_Wetting_a_index)%data_2d(save_road_counter,:)       = activity_data(g_road_wetting_index,:,ro)
    if (allocated(save_vars(save_Ploughing_a_index)%data_2d))  save_vars(save_Ploughing_a_index)%data_2d(save_road_counter,:)     = activity_data(t_ploughing_index,:,ro)*h_ploughing_moisture(snow_index)
    if (allocated(save_vars(save_Cleaning_a_index)%data_2d))   save_vars(save_Cleaning_a_index)%data_2d(save_road_counter,:)      = activity_data(t_cleaning_index,:,ro)*efficiency_of_cleaning(ro_tot)
    if (allocated(save_vars(save_Mass_salt1_index)%data_2d))   save_vars(save_Mass_salt1_index)%data_2d(save_road_counter,:)      = sum(M_road_data(salt_index(1),pm_all,:,:,ro),dim=2)*conversion
    if (allocated(save_vars(save_Mass_salt2_index)%data_2d))   save_vars(save_Mass_salt2_index)%data_2d(save_road_counter,:)      = sum(M_road_data(salt_index(2),pm_all,:,:,ro),dim=2)*conversion
    if (allocated(save_vars(save_f_q_index)%data_2d))          save_vars(save_f_q_index)%data_2d(save_road_counter, :)            = f_q(road_index,:,tr,ro)
    if (allocated(save_vars(save_W_surf_mod_index)%data_2d))   save_vars(save_W_surf_mod_index)%data_2d(save_road_counter, :)            = g_road_data(water_index,ti,tr,ro)
    if (allocated(save_vars(save_I_surf_mod_index)%data_2d))   save_vars(save_I_surf_mod_index)%data_2d(save_road_counter, :)            = g_road_data(ice_index,ti,tr,ro)
    if (allocated(save_vars(save_S_surf_mod_index)%data_2d))   save_vars(save_S_surf_mod_index)%data_2d(save_road_counter, :)            = g_road_data(snow_index,ti,tr,ro)
    if (allocated(save_vars(save_Energy_difference_index)%data_2d))        save_vars(save_Energy_difference_index)%data_2d(save_road_counter,:)           = road_meteo_data(E_diff_index,:,tr,ro)
    if (allocated(save_vars(save_Energy_correction_index)%data_2d))        save_vars(save_Energy_correction_index)%data_2d(save_road_counter,:)           = road_meteo_data(E_corr_index,:,tr,ro)
    if (allocated(save_vars(save_NOX_emissions_tot_index)%data_2d))        save_vars(save_NOX_emissions_tot_index)%data_2d(save_road_counter,:)           = airquality_data(NOX_emis_index,:,ro)
    if (allocated(save_vars(save_Mass_dust_PM200_index)%data_2d))          save_vars(save_Mass_dust_PM200_index)%data_2d(save_road_counter,:)             = sum(M_road_data(total_dust_index,pm_200,:,:,ro),dim=2)*conversion
    if (allocated(save_vars(save_Mass_dust_PM10_index)%data_2d))           save_vars(save_Mass_dust_PM10_index)%data_2d(save_road_counter,:)              = sum(M_road_data(total_dust_index,pm_10,:,:,ro),dim=2)*conversion
    if (allocated(save_vars(save_Mass_dust_PM25_index)%data_2d))           save_vars(save_Mass_dust_PM25_index)%data_2d(save_road_counter,:)              = sum(M_road_data(total_dust_index,pm_25,:,:,ro),dim=2)*conversion
    if (allocated(save_vars(save_Mass_sand_PM200_index)%data_2d))          save_vars(save_Mass_sand_PM200_index)%data_2d(save_road_counter,:)             = sum(M_road_data(sand_index,pm_200,:,:,ro),dim=2)*conversion
    if (allocated(save_vars(save_PM10_Emissions_tot_index)%data_2d))       save_vars(save_PM10_Emissions_tot_index)%data_2d(save_road_counter,:)          = sum(E_road_data(total_dust_index,pm_10,E_total_index,:,:,ro),dim=2)
    if (allocated(save_vars(save_PM25_Emissions_tot_index)%data_2d))       save_vars(save_PM25_Emissions_tot_index)%data_2d(save_road_counter,:)          = sum(E_road_data(total_dust_index,pm_25,E_total_index,:,:,ro),dim=2)
    if (allocated(save_vars(save_PM10_Emissions_roadwear_index)%data_2d))  save_vars(save_PM10_Emissions_roadwear_index)%data_2d(save_road_counter,:)     = sum(E_road_data(road_index,pm_10,E_total_index,:,:,ro),dim=2)
    if (allocated(save_vars(save_PM10_Emissions_tyrewear_index)%data_2d))  save_vars(save_PM10_Emissions_tyrewear_index)%data_2d(save_road_counter,:)     = sum(E_road_data(tyre_index,pm_10,E_total_index,:,:,ro),dim=2)
    if (allocated(save_vars(save_PM10_Emissions_brakewear_index)%data_2d)) save_vars(save_PM10_Emissions_brakewear_index)%data_2d(save_road_counter,:)    = sum(E_road_data(brake_index,pm_10,E_total_index,:,:,ro),dim=2)
    if (allocated(save_vars(save_PM10_Emissions_sand_index)%data_2d))      save_vars(save_PM10_Emissions_sand_index)%data_2d(save_road_counter,:)         = sum(E_road_data(sand_index,pm_10,E_total_index,:,:,ro),dim=2)
    if (allocated(save_vars(save_PM10_Emissions_exhaust_index)%data_2d))   save_vars(save_PM10_Emissions_fugitive_index)%data_2d(save_road_counter,:)     = sum(E_road_data(fugitive_index,pm_10,E_total_index,:,:,ro),dim=2)
    if (allocated(save_vars(save_PM10_Emissions_exhaust_index)%data_2d))   save_vars(save_PM10_Emissions_exhaust_index)%data_2d(save_road_counter,:)      = sum(E_road_data(exhaust_index,pm_10,E_total_index,:,:,ro),dim=2)
    if (allocated(save_vars(save_PM10_Emissions_salt1_index)%data_2d))     save_vars(save_PM10_Emissions_salt1_index)%data_2d(save_road_counter,:)        = sum(E_road_data(salt_index(1),pm_10,E_total_index,:,:,ro),dim=2)
    if (allocated(save_vars(save_PM10_Emissions_salt2_index)%data_2d))     save_vars(save_PM10_Emissions_salt2_index)%data_2d(save_road_counter,:)        = sum(E_road_data(salt_index(2),pm_10,E_total_index,:,:,ro),dim=2)
    if (allocated(save_vars(save_PM10_Emissions_direct_index)%data_2d))    save_vars(save_PM10_Emissions_direct_index)%data_2d(save_road_counter,:)       = sum(E_road_data(total_dust_index,pm_10,E_direct_index,:,:,ro),dim=2)
    if (allocated(save_vars(save_PM10_Emissions_suspension_index)%data_2d))  save_vars(save_PM10_Emissions_suspension_index)%data_2d(save_road_counter,:) = sum(E_road_data(total_dust_index,pm_10,E_suspension_index,:,:,ro),dim=2)
    if (allocated(save_vars(save_PM10_Emissions_windblown_index)%data_2d))   save_vars(save_PM10_Emissions_windblown_index)%data_2d(save_road_counter,:)  = sum(E_road_data(total_dust_index,pm_10,E_windblown_index,:,:,ro),dim=2)

    !Summarizing activity and emissions over the simulation period: 
    if (allocated(save_vars(save_Traffic_sum_index)%data_1d))                   save_vars(save_Traffic_sum_index)%data_1d(save_road_counter)                    = sum(traffic_data(N_total_index,min_time_save:max_time_save,ro))
    if (allocated(save_vars(save_fr_hdv_sum_index)%data_1d))                    save_vars(save_fr_hdv_sum_index)%data_1d(save_road_counter)                     = fr_hdv_sum
    if (allocated(save_vars(save_st_li_sum_index)%data_1d))                     save_vars(save_st_li_sum_index)%data_1d(save_road_counter)                      = st_li_sum
    if (allocated(save_vars(save_st_he_sum_index)%data_1d))                     save_vars(save_st_he_sum_index)%data_1d(save_road_counter)                      = st_he_sum
    if (allocated(save_vars(save_Speed_li_avg_index)%data_1d))                  save_vars(save_Speed_li_avg_index)%data_1d(save_road_counter)                   = sum(traffic_data(V_li_index,min_time_save:max_time_save,ro))/(max_time_save-min_time_save+1)
    if (allocated(save_vars(save_NOX_Emissions_sum_index)%data_1d))             save_vars(save_NOX_Emissions_sum_index)%data_1d(save_road_counter)              = sum(airquality_data(NOX_emis_index,min_time_save:max_time_save,ro))*length_road_km(ro)
    if (allocated(save_vars(save_PM10_Emissions_sum_index)%data_1d))            save_vars(save_PM10_Emissions_sum_index)%data_1d(save_road_counter)             = sum(E_road_data(total_dust_index,pm_10,E_total_index,min_time_save:max_time_save,:,ro))*length_road_km(ro)
    if (allocated(save_vars(save_PM25_Emissions_sum_index)%data_1d))            save_vars(save_PM25_Emissions_sum_index)%data_1d(save_road_counter)             = sum(E_road_data(total_dust_index,pm_25,E_total_index,min_time_save:max_time_save,:,ro))*length_road_km(ro)
    if (allocated(save_vars(save_PM10_Emissions_road_sum_index)%data_1d))       save_vars(save_PM10_Emissions_road_sum_index)%data_1d(save_road_counter)        = sum(E_road_data(road_index,pm_10,E_total_index,min_time_save:max_time_save,:,ro))*length_road_km(ro)
    if (allocated(save_vars(save_PM10_Emissions_tyre_sum_index)%data_1d))       save_vars(save_PM10_Emissions_tyre_sum_index)%data_1d(save_road_counter)        = sum(E_road_data(tyre_index,pm_10,E_total_index,min_time_save:max_time_save,:,ro))*length_road_km(ro)
    if (allocated(save_vars(save_PM10_Emissions_brake_sum_index)%data_1d))      save_vars(save_PM10_Emissions_brake_sum_index)%data_1d(save_road_counter)       = sum(E_road_data(brake_index,pm_10,E_total_index,min_time_save:max_time_save,:,ro))*length_road_km(ro)
    if (allocated(save_vars(save_PM10_Emissions_sand_sum_index)%data_1d))       save_vars(save_PM10_Emissions_sand_sum_index)%data_1d(save_road_counter)        = sum(E_road_data(sand_index,pm_10,E_total_index,min_time_save:max_time_save,:,ro))*length_road_km(ro)
    if (allocated(save_vars(save_PM10_Emissions_fugitive_sum_index)%data_1d))   save_vars(save_PM10_Emissions_fugitive_sum_index)%data_1d(save_road_counter)    = sum(E_road_data(fugitive_index,pm_10,E_total_index,min_time_save:max_time_save,:,ro))*length_road_km(ro)
    if (allocated(save_vars(save_PM10_Emissions_exhaust_sum_index)%data_1d))    save_vars(save_PM10_Emissions_exhaust_sum_index)%data_1d(save_road_counter)     = sum(E_road_data(exhaust_index,pm_10,E_total_index,min_time_save:max_time_save,:,ro))*length_road_km(ro)
    if (allocated(save_vars(save_PM10_Emissions_salt1_sum_index)%data_1d))      save_vars(save_PM10_Emissions_salt1_sum_index)%data_1d(save_road_counter)       = sum(E_road_data(salt_index(1),pm_10,E_total_index,min_time_save:max_time_save,:,ro))*length_road_km(ro)
    if (allocated(save_vars(save_PM10_Emissions_salt2_sum_index)%data_1d))      save_vars(save_PM10_Emissions_salt2_sum_index)%data_1d(save_road_counter)       = sum(E_road_data(salt_index(2),pm_10,E_total_index,min_time_save:max_time_save,:,ro))*length_road_km(ro)
    if (allocated(save_vars(save_PM10_Emissions_direct_sum_index)%data_1d))     save_vars(save_PM10_Emissions_direct_sum_index)%data_1d(save_road_counter)      = sum(E_road_data(total_dust_index,pm_10,E_direct_index,min_time_save:max_time_save,:,ro))*length_road_km(ro)
    if (allocated(save_vars(save_PM10_Emissions_suspension_sum_index)%data_1d)) save_vars(save_PM10_Emissions_suspension_sum_index)%data_1d(save_road_counter)  = sum(E_road_data(total_dust_index,pm_10,E_suspension_index,min_time_save:max_time_save,:,ro))*length_road_km(ro)
    if (allocated(save_vars(save_PM10_Emissions_windblown_sum_index)%data_1d))  save_vars(save_PM10_Emissions_windblown_sum_index)%data_1d(save_road_counter)   = sum(E_road_data(total_dust_index,pm_10,E_windblown_index,min_time_save:max_time_save,:,ro))*length_road_km(ro)
    if (allocated(save_vars(save_Mass_salt1_sum_index)%data_1d))                save_vars(save_Mass_salt1_sum_index)%data_1d(save_road_counter)                 = sum(activity_data(M_salting_index(1),min_time_save:max_time_save,ro))*conversion_sum*length_road_km(ro)
    if (allocated(save_vars(save_Mass_salt2_sum_index)%data_1d))                save_vars(save_Mass_salt2_sum_index)%data_1d(save_road_counter)                 = sum(activity_data(M_salting_index(2),min_time_save:max_time_save,ro))*conversion_sum*length_road_km(ro)
    if (allocated(save_vars(save_Mass_sand_PM200_sum_index)%data_1d))           save_vars(save_Mass_sand_PM200_sum_index)%data_1d(save_road_counter)            = sum(activity_data(M_sanding_index,min_time_save:max_time_save,ro))*f_PM_bin(sand_index,pm_all,1)*conversion_sum*length_road_km(ro)
    if (allocated(save_vars(save_Mass_sand_PMall_sum_index)%data_1d))           save_vars(save_Mass_sand_PMall_sum_index)%data_1d(save_road_counter)            = sum(activity_data(M_sanding_index,min_time_save:max_time_save,ro))*f_PM_bin(sand_index,pm_200,1)*conversion_sum*length_road_km(ro)
    if (allocated(save_vars(save_Mass_sand_PM200_sum_index)%data_1d))           save_vars(save_Mass_sand_PM200_sum_index)%data_1d(save_road_counter)            = sum(activity_data(M_sanding_index,min_time_save:max_time_save,ro))*f_PM_bin(sand_index,pm_10,1)*conversion_sum*length_road_km(ro)
    if (allocated(save_vars(save_Mass_cleaning_PMall_sum_index)%data_1d))       save_vars(save_Mass_cleaning_PMall_sum_index)%data_1d(save_road_counter)        = sum(M_road_balance_data(total_dust_index,pm_all,S_cleaning_index,min_time_save:max_time_save,:,ro))*length_road_km(ro)
    if (allocated(save_vars(save_Mass_cleaning_PM200_sum_index)%data_1d))       save_vars(save_Mass_cleaning_PM200_sum_index)%data_1d(save_road_counter)        = sum(M_road_balance_data(total_dust_index,pm_200,S_cleaning_index,min_time_save:max_time_save,:,ro))*length_road_km(ro)
    if (allocated(save_vars(save_Mass_cleaning_PM10_sum_index)%data_1d))        save_vars(save_Mass_cleaning_PM10_sum_index)%data_1d(save_road_counter)         = sum(M_road_balance_data(total_dust_index,pm_10,S_cleaning_index,min_time_save:max_time_save,:,ro))*length_road_km(ro)
    if (allocated(save_vars(save_Mass_drain_PM10_sum_index)%data_1d))           save_vars(save_Mass_drain_PM10_sum_index)%data_1d(save_road_counter)            = sum(M_road_balance_data(total_dust_index,pm_10,S_dustdrainage_index,min_time_save:max_time_save,:,ro))*length_road_km(ro)
    if (allocated(save_vars(save_Mass_spray_PM10_sum_index)%data_1d))           save_vars(save_Mass_spray_PM10_sum_index)%data_1d(save_road_counter)            = sum(M_road_balance_data(total_dust_index,pm_10,S_dustspray_index,min_time_save:max_time_save,:,ro))*length_road_km(ro)
    if (allocated(save_vars(save_Mass_ploughing_PM10_sum_index)%data_1d))       save_vars(save_Mass_ploughing_PM10_sum_index)%data_1d(save_road_counter)        = sum(M_road_balance_data(total_dust_index,pm_10,S_dustploughing_index,min_time_save:max_time_save,:,ro))*length_road_km(ro)
    if (allocated(save_vars(save_PM10_Wear_road_sum_index)%data_1d))            save_vars(save_PM10_Wear_road_sum_index)%data_1d(save_road_counter)             = sum(M_road_balance_data(road_index,pm_10,P_wear_index,min_time_save:max_time_save,:,ro))*length_road_km(ro)
    if (allocated(save_vars(save_PM10_Wear_tyre_sum_index)%data_1d))            save_vars(save_PM10_Wear_tyre_sum_index)%data_1d(save_road_counter)             = sum(M_road_balance_data(tyre_index,pm_10,P_wear_index,min_time_save:max_time_save,:,ro))*length_road_km(ro)
    if (allocated(save_vars(save_PM10_Wear_brake_sum_index)%data_1d))           save_vars(save_PM10_Wear_brake_sum_index)%data_1d(save_road_counter)            = sum(M_road_balance_data(brake_index,pm_10,P_wear_index,min_time_save:max_time_save,:,ro))*length_road_km(ro)
    if (allocated(save_vars(save_Mass_dust_road_PM200_sum_index)%data_1d))      save_vars(save_Mass_dust_road_PM200_sum_index)%data_1d(save_road_counter)       = sum(M_road_data(total_dust_index,pm_200,min_time_save:max_time_save,:,ro))/(max_time_save-min_time_save+1)*length_road_km(ro)
    if (allocated(save_vars(save_Mass_dust_road_PM10_sum_index)%data_1d))       save_vars(save_Mass_dust_road_PM10_sum_index)%data_1d(save_road_counter)        = sum(M_road_data(total_dust_index,pm_10,min_time_save:max_time_save,:,ro))/(max_time_save-min_time_save+1)*length_road_km(ro)

end subroutine NORTRIP_fill_save_array