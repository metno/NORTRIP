!NORTRIP_save_road_meteo_data.f90
        
    subroutine NORTRIP_save_road_meteo_data
    
    use NORTRIP_definitions
    
    implicit none
    
    integer :: unit_out
    character(256) temp_name,filename_temp
    logical exists
    real conversion
    integer a(num_date_index)
    real salt1_solution_by_mass
    integer ro_num

    !Declare functions
    
    unit_out=unit_save_road_meteo_data
    
    !Open log file
    !if (unit_logfile.gt.0) then
    !    open(unit_logfile,file=filename_log,status='old',position='append')
    !endif

    if (ro_tot.eq.1) then
        
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	write(unit_logfile,'(A)') 'Saving selected road meteo data to file'
	write(unit_logfile,'(A)') '----------------------------------------------------------------'

    !Check that path exists after filling in date stamp
    a=date_data(:,min_time_save)
    
    !call date_to_datestr(a,path_output_roadmeteo,temp_name)
    
    inquire(directory=trim(path_output_roadmeteo),exist=exists)
    if (.not.exists) then
        write(unit_logfile,'(A)')'ERROR: Path '//trim(path_output_roadmeteo)//' does not exist. No file saved'
        return
    endif
    if (sum(save_road_data_flag).eq.0) then
        write(unit_logfile,'(A)')'WARNING: No receptor link sites selected. Will not save to '//trim(path_output_roadmeteo)
        return
    endif
    if (filename_output_roadmeteo.eq.'') then
        write(unit_logfile,'(A)')'WARNING: No road meteo filename given. Will not save to '//trim(path_output_roadmeteo)
        return
    endif
 
        !Open the outputfile for date
        temp_name=trim(path_output_roadmeteo)//trim(filename_output_roadmeteo) !//'_road_meteo.txt'

        !Put in date if required
        a=date_data(:,min_time_save)
        
        call date_to_datestr_bracket(a,temp_name,temp_name)
        
        write(unit_logfile,'(a)') ' Filename= '//trim(temp_name)

        open(unit_out,file=temp_name,status='replace')
    
        !write(unit_out,'(67A6)') &
        write(unit_out,'(a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12)') &
            'Road_num',achar(9), &
            'Road_ID',achar(9),&
            'Year',achar(9), &
            'Month',achar(9), &
            'Day',achar(9), &
            'Hour',achar(9), &
            'Minute',achar(9), &
            'T_air',achar(9), &
            'RH_air',achar(9), &
            'Td_mod',achar(9), &
            'Wind_FF',achar(9), &
            'Wind_DD',achar(9), &
            'Rain',achar(9), &
            'Snow',achar(9), &
            'Cloud',achar(9), &
            'SW_rad_cls',achar(9), &
            'SW_rad_in',achar(9), &
            'SW_rad_net',achar(9), &
            'LW_rad_in',achar(9), &
            'LW_rad_net',achar(9), &
            'H_in',achar(9), &
            'L_in',achar(9), &
            'G_sub',achar(9), &
            'G_net',achar(9), &
            'T_surf_mod',achar(9), &
            'T_sub_mod',achar(9), &
            'T_freeze_mod',achar(9), &
            'W_surf_mod',achar(9), &
            'I_surf_mod',achar(9), &
            'S_surf_mod',achar(9), &
            'Salt1_surf',achar(9), &
            'Salt2_surf',achar(9), &
            'Salt1_solut',achar(9), &
            'Dust200_surf',achar(9), &
            'T_surf_obs',achar(9), &
            'W_surf_obs',achar(9), &
            'I_surf_obs',achar(9), &
            'S_surf_obs'
        
    endif
    
        tr=1
        
        do ro=n_roads_start,n_roads_end
            
            if (use_single_road_loop_flag) then
                ro_num=ro_tot
            else
                ro_num=ro
            endif
            
            if (save_road_data_flag(ro).ne.0) then
            do ti=min_time_save,max_time_save
                
                conversion=1./1000./b_road_lanes(ro)
                
                salt1_solution_by_mass=100.*(M_road_data(salt_index(1),pm_all,ti,tr,ro)*conversion+.00001)/(M_road_data(salt_index(1),pm_all,ti,tr,ro)*conversion+g_road_data(water_index,ti,tr,ro)*1000.+.00001)
                
                write(unit_out,'(i12,a,i12,a,i12,a,i12,a,i12,a,i12,a,i12,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3)') &
                    ro_num,achar(9), &
                    road_ID(ro),achar(9),&
                    int(date_data(year_index,ti)),achar(9), &
                    int(date_data(month_index,ti)),achar(9), &
                    int(date_data(day_index,ti)),achar(9), &
                    int(date_data(hour_index,ti)),achar(9), &
                    int(date_data(minute_index,ti)),achar(9), &
                    meteo_data(T_a_index,ti,ro),achar(9), &
                    meteo_data(RH_index,ti,ro),achar(9), &
                    meteo_data(T_dewpoint_index,ti,ro),achar(9), &
                    !meteo_data(FF_index,ti,ro)/wind_speed_correction(ro),achar(9), &
                    meteo_data(FF_index,ti,ro),achar(9), &
                    meteo_data(DD_index,ti,ro),achar(9), &
                    meteo_data(Rain_precip_index,ti,ro),achar(9), &
                    meteo_data(Snow_precip_index,ti,ro),achar(9), &
                    meteo_data(cloud_cover_index,ti,ro),achar(9), &
                    meteo_data(short_rad_in_clearsky_index,ti,ro),achar(9), &
                    meteo_data(short_rad_in_index,ti,ro),achar(9), &
                    road_meteo_data(short_rad_net_index,ti,tr,ro),achar(9), &
                    meteo_data(long_rad_in_index,ti,ro),achar(9), &
                    road_meteo_data(long_rad_net_index,ti,tr,ro),achar(9), &
                    -road_meteo_data(H_index,ti,tr,ro),achar(9), &
                    -road_meteo_data(L_index,ti,tr,ro),achar(9), &
                    road_meteo_data(G_sub_index,ti,tr,ro),achar(9), &
                    road_meteo_data(G_index,ti,tr,ro),achar(9), &
                    road_meteo_data(T_s_index,ti,tr,ro),achar(9), &
                    road_meteo_data(T_sub_index,ti,tr,ro),achar(9), &
                    road_meteo_data(T_melt_index,ti,tr,ro),achar(9), &
                    g_road_data(water_index,ti,tr,ro),achar(9), &
                    g_road_data(ice_index,ti,tr,ro),achar(9), &
                    g_road_data(snow_index,ti,tr,ro),achar(9), &
                    M_road_data(salt_index(1),pm_all,ti,tr,ro)*conversion,achar(9), &
                    M_road_data(salt_index(2),pm_all,ti,tr,ro)*conversion,achar(9), &
                    salt1_solution_by_mass,achar(9), &
                    M_road_data(total_dust_index,pm_200,ti,tr,ro)*conversion,achar(9), &
                    road_meteo_data(road_temperature_obs_index,ti,tr,ro),achar(9), &
                    road_meteo_data(road_wetness_obs_index,ti,tr,ro),achar(9), &
                    -99.,achar(9), &
                    -99.
            enddo
            endif
        enddo
    
    if (ro_tot.eq.n_roads_total) close (unit_out)
    
    end subroutine NORTRIP_save_road_meteo_data