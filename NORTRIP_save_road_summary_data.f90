!NORTRIP_save_road_summary_data.f90
        
!==========================================================================
!   NORTRIP model NORTRIP_save_road_summary_data
!==========================================================================
    subroutine NORTRIP_save_road_summary_data
    
    use NORTRIP_definitions
    
    implicit none
    
    integer :: unit_out
    character(256) temp_name,filename_temp
    logical exists
    real conversion
    integer a(num_date_index)
    integer ro_num
    real st_li,st_he,fr_hdv

    !Declare functions
    
    unit_out=unit_save_road_summary_data
    
    if (ro_tot.eq.1) then

	    write(unit_logfile,'(A)') '----------------------------------------------------------------'
	    write(unit_logfile,'(A)') 'Saving selected road summary data to file'
	    write(unit_logfile,'(A)') '----------------------------------------------------------------'

        !Check that path exists after filling in date stamp
        a=date_data(:,min_time_save)
    
        !call date_to_datestr(a,path_fortran_output,temp_name)
    
        inquire(directory=trim(path_outputdata),exist=exists)
        if (.not.exists) then
            write(unit_logfile,'(A)')'ERROR: Path '//trim(path_outputdata)//' does not exist. No file saved'
            return
        endif
        if (sum(save_road_data_flag).eq.0) then
            write(unit_logfile,'(A)')'WARNING: No receptor link sites selected. Will not save to '//trim(path_outputdata)
            return
        endif
 
        !Open the outputfile for date
        temp_name=trim(path_outputdata)//trim(filename_outputdata)//'_summary.txt'

        !Put in date if required
        a=date_data(:,min_time_save)
        
        call date_to_datestr_bracket(a,temp_name,temp_name)
        call date_to_datestr_bracket(a,temp_name,temp_name)
        call date_to_datestr_bracket(a,temp_name,temp_name)
        
        write(unit_logfile,'(a)') ' Filename= '//trim(temp_name)

        open(unit_out,file=temp_name,status='replace')
    
        !35 fields
        !write(unit_out,'(67A6)') &
        write(unit_out,'(a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14)') &
            'Road_num',achar(9), &
            'Road_ID',achar(9),&
            'Year',achar(9), &
            'Month',achar(9), &
            'Day',achar(9), &
            'Hour',achar(9), &
            'Minute',achar(9), &
            'Traffic',achar(9), &
            'HDV(%)',achar(9), &
            'Studs(li%)',achar(9), &
            'Studs(he%)',achar(9), &
            'Speed(li)',achar(9), &
            'NOX_E_tot',achar(9), &
            'PM10_E_tot',achar(9), &
            'PM25_E_tot',achar(9), &
            'PM10_E_road',achar(9), &
            'PM10_E_tyre',achar(9), &
            'PM10_E_brake',achar(9), &
            'PM10_E_sand',achar(9), &
            'PM10_E_fug',achar(9), &
            'PM10_E_exh',achar(9), &
            'PM10_E_salt1',achar(9), &
            'PM10_E_salt2',achar(9), &
            'PM10_E_dir',achar(9), &
            'PM10_E_sus',achar(9), &
            'PM10_E_wind',achar(9), &
            'M_dust_PM200',achar(9), &
            'M_salt1',achar(9), &
            'M_salt2',achar(9), &
            'M_sand_PM200',achar(9), &
            'f_q',achar(9), &
            'T_air',achar(9), &
            'Td_air',achar(9), &
            'RH_air',achar(9), &
            'Wind_FF',achar(9), &
            'Wind_DD',achar(9), &
            'Rain',achar(9), &
            'Snow',achar(9), &
            'T_surf_mod',achar(9), &
            'T_sub_mod',achar(9), &
            'T_freeze_mod',achar(9), &
            'T_surf_meteo',achar(9), &
            'SW_rad_cls',achar(9), &
            'SW_rad_in',achar(9), &
            'SW_rad_net',achar(9), &
            'LW_rad_in',achar(9), &
            'LW_rad_net',achar(9), &
            'H_in',achar(9), &
            'L_in',achar(9), &
            'G_sub',achar(9), &
            'G_net',achar(9), &
            'W_surf_mod',achar(9), &
            'I_surf_mod',achar(9), &
            'S_surf_mod',achar(9), &
            'Salt1_a(g/m^2)',achar(9), &
            'Salt2_a(g/m^2)',achar(9), &
            'Sand_a(g/m^2)',achar(9), &
            'Wetting_a(mm)',achar(9), &
            'Plough_a(eff)',achar(9), &
            'Clean_a(eff)'
        
    endif
    
        tr=1
        !Sum over the tracks        
        do ro=n_roads_start,n_roads_end
            
            if (use_single_road_loop_flag) then
                ro_num=ro_tot
            else
                ro_num=ro
            endif

            if (save_road_data_flag(ro).ne.0) then          
                
                conversion=1./1000./b_road_lanes(ro)
                do ti=min_time_save,max_time_save
                
                    st_li=traffic_data(N_st_li_index,ti,ro)/traffic_data(N_li_index,ti,ro)*100.
                    if (isnan(st_li)) st_li=0.
                    st_he=traffic_data(N_st_he_index,ti,ro)/traffic_data(N_he_index,ti,ro)*100.
                    if (isnan(st_he)) st_he=0.
                    fr_hdv=traffic_data(N_he_index,ti,ro)/traffic_data(N_total_index,ti,ro)*100.
                    if (isnan(fr_hdv)) fr_hdv=0.
                    
                   ! write(*,*) 'saving',ro_num,unit_out
                    
                    write(unit_out,'(i14,a,i14,a,i14,a,i14,a,i14,a,i14,a,i14,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3)') &
                        ro_num,achar(9), &
                        road_ID(ro),achar(9),&
                        int(date_data(year_index,ti)),achar(9), &
                        int(date_data(month_index,ti)),achar(9), &
                        int(date_data(day_index,ti)),achar(9), &
                        int(date_data(hour_index,ti)),achar(9), &
                        int(date_data(minute_index,ti)),achar(9), &
                        traffic_data(N_total_index,ti,ro),achar(9), &
                        fr_hdv,achar(9), &
                        st_li,achar(9), &
                        st_he,achar(9), &
                        traffic_data(V_li_index,ti,ro),achar(9), &
                        airquality_data(NOX_emis_index,ti,ro),achar(9), &
                        sum(E_road_data(total_dust_index,pm_10,E_total_index,ti,:,ro)),achar(9), &
                        sum(E_road_data(total_dust_index,pm_25,E_total_index,ti,:,ro)),achar(9), &
                        sum(E_road_data(road_index,pm_10,E_total_index,ti,:,ro)),achar(9), &
                        sum(E_road_data(tyre_index,pm_10,E_total_index,ti,:,ro)),achar(9), &
                        sum(E_road_data(brake_index,pm_10,E_total_index,ti,:,ro)),achar(9), &
                        sum(E_road_data(sand_index,pm_10,E_total_index,ti,:,ro)),achar(9), &
                        sum(E_road_data(fugitive_index,pm_10,E_total_index,ti,:,ro)),achar(9), &
                        sum(E_road_data(exhaust_index,pm_10,E_total_index,ti,:,ro)),achar(9), &
                        sum(E_road_data(salt_index(1),pm_10,E_total_index,ti,:,ro)),achar(9), &
                        sum(E_road_data(salt_index(2),pm_10,E_total_index,ti,:,ro)),achar(9), &
                        sum(E_road_data(total_dust_index,pm_10,E_direct_index,ti,:,ro)),achar(9), &
                        sum(E_road_data(total_dust_index,pm_10,E_suspension_index,ti,:,ro)),achar(9), &
                        sum(E_road_data(total_dust_index,pm_10,E_windblown_index,ti,:,ro)),achar(9), &
                        sum(M_road_data(total_dust_index,pm_200,ti,:,ro))*conversion,achar(9), &
                        sum(M_road_data(salt_index(1),pm_all,ti,:,ro))*conversion,achar(9), &
                        sum(M_road_data(salt_index(2),pm_all,ti,:,ro))*conversion,achar(9), &
                        sum(M_road_data(sand_index,pm_200,ti,:,ro))*conversion,achar(9), &
                        f_q(road_index,ti,tr,ro),achar(9), &
                        meteo_data(T_a_index,ti,ro),achar(9), &
                        meteo_data(T_dewpoint_index,ti,ro),achar(9), &
                        meteo_data(RH_index,ti,ro),achar(9), &
                        meteo_data(FF_index,ti,ro),achar(9), &
                        meteo_data(DD_index,ti,ro),achar(9), &
                        meteo_data(Rain_precip_index,ti,ro),achar(9), &
                        meteo_data(Snow_precip_index,ti,ro),achar(9), &
                        road_meteo_data(T_s_index,ti,tr,ro),achar(9), &
                        road_meteo_data(T_sub_index,ti,tr,ro),achar(9), &
                        road_meteo_data(T_melt_index,ti,tr,ro),achar(9), &
                        road_meteo_data(road_temperature_obs_index,ti,tr,ro),achar(9), &
                        meteo_data(short_rad_in_clearsky_index,ti,ro),achar(9), &
                        meteo_data(short_rad_in_index,ti,ro),achar(9), &
                        road_meteo_data(short_rad_net_index,ti,tr,ro),achar(9), &
                        meteo_data(long_rad_in_index,ti,ro),achar(9), &
                        road_meteo_data(long_rad_net_index,ti,tr,ro),achar(9), &
                        -road_meteo_data(H_index,ti,tr,ro),achar(9), &
                        -road_meteo_data(L_index,ti,tr,ro),achar(9), &
                        road_meteo_data(G_sub_index,ti,tr,ro),achar(9), &
                        road_meteo_data(G_index,ti,tr,ro),achar(9), &
                        g_road_data(water_index,ti,tr,ro),achar(9), &
                        g_road_data(ice_index,ti,tr,ro),achar(9), &
                        g_road_data(snow_index,ti,tr,ro),achar(9), &
                        activity_data(M_salting_index(1),ti,ro),achar(9), &
                        activity_data(M_salting_index(2),ti,ro),achar(9), &
                        activity_data(M_sanding_index,ti,ro)*f_PM_bin(sand_index,pm_all,1),achar(9), &
                        activity_data(g_road_wetting_index,ti,ro),achar(9), &
                        activity_data(t_ploughing_index,ti,ro)*h_ploughing_moisture(snow_index),achar(9), &
                        activity_data(t_cleaning_index,ti,ro)*efficiency_of_cleaning(ro)

                enddo
            endif
        enddo

                            
        !Note that road_meteo_data(road_temperature_obs_index,ti,tr,ro) is actually not obs but the meterological input 0 m temperature. It was used as a spare variable.
    if (ro_tot.eq.n_roads_total) close (unit_out)
    
    end subroutine NORTRIP_save_road_summary_data

    
!==========================================================================
!   NORTRIP model NORTRIP_save_road_emission_activity_data
!==========================================================================
    subroutine NORTRIP_save_road_emission_activity_data
    
    use NORTRIP_definitions
    
    implicit none
    
    integer :: unit_out
    character(256) temp_name,filename_temp
    logical exists
    real conversion
    integer a(num_date_index)
    integer ro_num
    real st_li,st_he,fr_hdv
    integer n_t
    real length_road_km(0:n_roads)

    !Declare functions
    
    !Time dimesnion length for when averaging is required
    n_t=max_time_save-min_time_save+1
    
    unit_out=unit_save_road_activity_data
    
    if (ro_tot.eq.1) then

	    write(unit_logfile,'(A)') '----------------------------------------------------------------'
	    write(unit_logfile,'(A)') 'Saving summed emission and activity data to file'
	    write(unit_logfile,'(A)') '----------------------------------------------------------------'

        !Check that path exists after filling in date stamp
        a=date_data(:,min_time_save)
    
        !call date_to_datestr(a,path_fortran_output,temp_name)
    
        inquire(directory=trim(path_outputdata),exist=exists)
        if (.not.exists) then
            write(unit_logfile,'(A)')'ERROR: Path '//trim(path_outputdata)//' does not exist. No file saved'
            return
        endif
        if (sum(save_road_data_flag).eq.0) then
            write(unit_logfile,'(A)')'WARNING: No receptor link sites selected. Will not save to '//trim(path_outputdata)
            return
        endif
 
        !Open the outputfile for date
        temp_name=trim(path_outputdata)//trim(filename_outputdata)//'_activities.txt'

        !Put in date if required
        a=date_data(:,min_time_save)
        
        call date_to_datestr_bracket(a,temp_name,temp_name)
        call date_to_datestr_bracket(a,temp_name,temp_name)
        call date_to_datestr_bracket(a,temp_name,temp_name)
        
        write(unit_logfile,'(a)') ' Filename= '//trim(temp_name)

        open(unit_out,file=temp_name,status='replace')
    
        !35 fields
        !write(unit_out,'(67A6)') &
        write(unit_out,'(a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14,a,a14)') &
            'Road_num',achar(9), &
            'Road_ID',achar(9),&
            'Year',achar(9), &
            'Month',achar(9), &
            'Day',achar(9), &
            'Hour',achar(9), &
            'Minute',achar(9), &
            'Traffic',achar(9), &
            'HDV(%)',achar(9), &
            'Studs(li%)',achar(9), &
            'Studs(he%)',achar(9), &
            'Speed(li)',achar(9), &
            'NOX_E_tot',achar(9), &
            'PM10_E_tot',achar(9), &
            'PM25_E_tot',achar(9), &
            'PM10_E_road',achar(9), &
            'PM10_E_tyre',achar(9), &
            'PM10_E_brake',achar(9), &
            'PM10_E_sand',achar(9), &
            'PM10_E_fug',achar(9), &
            'PM10_E_exh',achar(9), &
            'PM10_E_salt1',achar(9), &
            'PM10_E_salt2',achar(9), &
            'PM10_E_dir',achar(9), &
            'PM10_E_sus',achar(9), &
            'PM10_E_wind',achar(9), &
            'M_salt1',achar(9), &
            'M_salt2',achar(9), &
            'M_sand_PMall',achar(9), &
            'M_sand_PM200',achar(9), &
            'M_sand_PM10',achar(9), &
            'M_clean_PMall',achar(9), &
            'M_clean_PM200',achar(9), &
            'M_clean_PM10'
        
    endif
    
        tr=1
        !Sum over the tracks        
        do ro=n_roads_start,n_roads_end
            
            if (use_single_road_loop_flag) then
                ro_num=ro_tot
            else
                ro_num=ro
            endif

            !Length of road in km for calculating sum of emissions
            if (length_road(ro).eq.0) then
                length_road_km(ro)=sqrt((x_road(1,ro)-x_road(2,ro))**2+(y_road(1,ro)-y_road(2,ro))**2)/1000.
            else
                length_road_km(ro)=length_road(ro)/1000.
            endif
            
                !Conversion to produce activity data that is in g/m2 to g/km using road length in km
                conversion=b_road_lanes(ro)*1000.
                !do ti=min_time_save,max_time_save
                
                    st_li=sum(traffic_data(N_st_li_index,min_time_save:max_time_save,ro))/sum(traffic_data(N_li_index,min_time_save:max_time_save,ro))*100.
                    if (isnan(st_li)) st_li=0.
                    st_he=sum(traffic_data(N_st_he_index,min_time_save:max_time_save,ro))/sum(traffic_data(N_he_index,min_time_save:max_time_save,ro))*100.
                    if (isnan(st_he)) st_he=0.
                    fr_hdv=sum(traffic_data(N_he_index,min_time_save:max_time_save,ro))/sum(traffic_data(N_total_index,min_time_save:max_time_save,ro))*100.
                    if (isnan(fr_hdv)) fr_hdv=0.
                    
                    write(unit_out,'(i14,a,i14,a,i14,a,i14,a,i14,a,i14,a,i14,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3,a,es14.3)') &
                        ro_num,achar(9), &
                        road_ID(ro),achar(9),&
                        int(date_data(year_index,min_time_save)),achar(9), &
                        int(date_data(month_index,min_time_save)),achar(9), &
                        int(date_data(day_index,min_time_save)),achar(9), &
                        int(date_data(hour_index,min_time_save)),achar(9), &
                        int(date_data(minute_index,min_time_save)),achar(9), &
                        sum(traffic_data(N_total_index,min_time_save:max_time_save,ro)),achar(9), &
                        fr_hdv,achar(9), &
                        st_li,achar(9), &
                        st_he,achar(9), &
                        sum(traffic_data(V_li_index,min_time_save:max_time_save,ro))/n_t,achar(9), &
                        sum(airquality_data(NOX_emis_index,min_time_save:max_time_save,ro))*length_road_km(ro),achar(9), &
                        sum(E_road_data(total_dust_index,pm_10,E_total_index,min_time_save:max_time_save,:,ro))*length_road_km(ro),achar(9), &
                        sum(E_road_data(total_dust_index,pm_25,E_total_index,min_time_save:max_time_save,:,ro))*length_road_km(ro),achar(9), &
                        sum(E_road_data(road_index,pm_10,E_total_index,min_time_save:max_time_save,:,ro))*length_road_km(ro),achar(9), &
                        sum(E_road_data(tyre_index,pm_10,E_total_index,min_time_save:max_time_save,:,ro))*length_road_km(ro),achar(9), &
                        sum(E_road_data(brake_index,pm_10,E_total_index,min_time_save:max_time_save,:,ro))*length_road_km(ro),achar(9), &
                        sum(E_road_data(sand_index,pm_10,E_total_index,min_time_save:max_time_save,:,ro))*length_road_km(ro),achar(9), &
                        sum(E_road_data(fugitive_index,pm_10,E_total_index,min_time_save:max_time_save,:,ro))*length_road_km(ro),achar(9), &
                        sum(E_road_data(exhaust_index,pm_10,E_total_index,min_time_save:max_time_save,:,ro))*length_road_km(ro),achar(9), &
                        sum(E_road_data(salt_index(1),pm_10,E_total_index,min_time_save:max_time_save,:,ro))*length_road_km(ro),achar(9), &
                        sum(E_road_data(salt_index(2),pm_10,E_total_index,min_time_save:max_time_save,:,ro))*length_road_km(ro),achar(9), &
                        sum(E_road_data(total_dust_index,pm_10,E_direct_index,min_time_save:max_time_save,:,ro))*length_road_km(ro),achar(9), &
                        sum(E_road_data(total_dust_index,pm_10,E_suspension_index,min_time_save:max_time_save,:,ro))*length_road_km(ro),achar(9), &
                        sum(E_road_data(total_dust_index,pm_10,E_windblown_index,min_time_save:max_time_save,:,ro))*length_road_km(ro),achar(9), &
                        sum(activity_data(M_salting_index(1),min_time_save:max_time_save,ro))*conversion*length_road_km(ro),achar(9), &
                        sum(activity_data(M_salting_index(2),min_time_save:max_time_save,ro))*conversion*length_road_km(ro),achar(9), &
                        sum(activity_data(M_sanding_index,min_time_save:max_time_save,ro))*f_PM_bin(sand_index,pm_all,1)*conversion*length_road_km(ro),achar(9), &
                        sum(activity_data(M_sanding_index,min_time_save:max_time_save,ro))*f_PM_bin(sand_index,pm_200,1)*conversion*length_road_km(ro),achar(9), &
                        sum(activity_data(M_sanding_index,min_time_save:max_time_save,ro))*f_PM_bin(sand_index,pm_10,1)*conversion*length_road_km(ro),achar(9), &
                        sum(M_road_balance_data(total_dust_index,pm_all,S_cleaning_index,min_time_save:max_time_save,:,ro))*length_road_km(ro),achar(9), &
                        sum(M_road_balance_data(total_dust_index,pm_200,S_cleaning_index,min_time_save:max_time_save,:,ro))*length_road_km(ro),achar(9), &
                        sum(M_road_balance_data(total_dust_index,pm_10,S_cleaning_index,min_time_save:max_time_save,:,ro))*length_road_km(ro)
                        
                !enddo

        enddo

    if (ro_tot.eq.n_roads_total) close (unit_out)
    
    end subroutine NORTRIP_save_road_emission_activity_data