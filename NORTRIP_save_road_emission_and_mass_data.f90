!NORTRIP_save_road_meteo_data.f90
        
!==========================================================================
!   NORTRIP model NORTRIP_save_road_emission_and_mass_data
!==========================================================================
    subroutine NORTRIP_save_road_emission_and_mass_data
    
    use NORTRIP_definitions
    
    implicit none
    
    integer :: unit_out
    character(256) temp_name,filename_temp
    logical exists
    real conversion
    integer a(num_date_index)
    integer ro_num

    !Declare functions
    
    unit_out=unit_save_road_emission_and_mass_data
    
    !Open log file
    !if (unit_logfile.gt.0) then
    !    open(unit_logfile,file=filename_log,status='old',position='append')
    !endif
    if (ro_tot.eq.1) then

	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	write(unit_logfile,'(A)') 'Saving selected road emission and mass data to file'
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
        temp_name=trim(path_outputdata)//trim(filename_outputdata)//'_emission_mass.txt'

        !Put in date if required
        a=date_data(:,min_time_save)
        
        call date_to_datestr(a,temp_name,temp_name)
        
        write(unit_logfile,'(a)') ' Filename= '//trim(temp_name)

        open(unit_out,file=temp_name,status='replace')
    
        !write(unit_out,'(67A6)') &
        write(unit_out,'(a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12)') &
            'Road_num',achar(9), &
            'Road_ID',achar(9),&
            'Year',achar(9), &
            'Month',achar(9), &
            'Day',achar(9), &
            'Hour',achar(9), &
            'Minute',achar(9), &
            'Traffic',achar(9), &
            'PM10_C_tot',achar(9), &
            'PM25_C_tot',achar(9), &
            'PM10_C_exh',achar(9), &
            'f_conc',achar(9), &
            'PM10_E_tot',achar(9), &
            'PM25_E_tot',achar(9), &
            'M_dust_PM200',achar(9), &
            'M_dust_PM10',achar(9), &
            'M_dust_PM25',achar(9), &
            'M_salt1',achar(9), &
            'M_salt2',achar(9), &
            'M_sand_PM200',achar(9), &
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
            'PM10_E_wind'
        
    endif
    
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
                
                
                write(unit_out,'(i12,a,i12,a,i12,a,i12,a,i12,a,i12,a,i12,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3)') &
                    ro_num,achar(9), &
                    road_ID(ro),achar(9),&
                    int(date_data(year_index,ti)),achar(9), &
                    int(date_data(month_index,ti)),achar(9), &
                    int(date_data(day_index,ti)),achar(9), &
                    int(date_data(hour_index,ti)),achar(9), &
                    int(date_data(minute_index,ti)),achar(9), &
                    traffic_data(N_total_index,ti,ro),achar(9), &
                    sum(C_data(total_dust_index,pm_10,C_total_index,ti,:,ro)),achar(9), &
                    sum(C_data(total_dust_index,pm_25,C_total_index,ti,:,ro)),achar(9), &
                    sum(C_data(exhaust_index,pm_10,C_total_index,ti,:,ro)),achar(9), &
                    airquality_data(f_conc_index,ti,ro),achar(9), &
                    sum(E_road_data(total_dust_index,pm_10,E_total_index,ti,:,ro)),achar(9), &
                    sum(E_road_data(total_dust_index,pm_25,E_total_index,ti,:,ro)),achar(9), &
                    sum(M_road_data(total_dust_index,pm_200,ti,:,ro))*conversion,achar(9), &
                    sum(M_road_data(total_dust_index,pm_10,ti,:,ro))*conversion,achar(9), &
                    sum(M_road_data(total_dust_index,pm_25,ti,:,ro))*conversion,achar(9), &
                    sum(M_road_data(salt_index(1),pm_all,ti,:,ro))*conversion,achar(9), &
                    sum(M_road_data(salt_index(2),pm_all,ti,:,ro))*conversion,achar(9), &
                    sum(M_road_data(sand_index,pm_200,ti,:,ro))*conversion,achar(9), &
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
                    sum(E_road_data(total_dust_index,pm_10,E_windblown_index,ti,:,ro)) 
                    
            enddo
            endif
        enddo
    
    if (ro_tot.eq.n_roads_total) close (unit_out)
    
    end subroutine NORTRIP_save_road_emission_and_mass_data
    
!==========================================================================
!   NORTRIP model NORTRIP_save_road_emission_and_mass_data_stats
!==========================================================================
    subroutine NORTRIP_save_road_emission_and_mass_data_stats
    
    use NORTRIP_definitions
    !use NORTRIP_index_definitions
    
    implicit none
    
    integer :: unit_out=40
    character(256) temp_name,filename_temp
    logical exists
    real conversion
    integer a(num_date_index)
    integer n_time_save
    integer ro_num
    
    !Order is (date_type)
    real, save, allocatable :: date_data_av(:)
    !Order is (variable_type,road)
    real, save, allocatable :: traffic_data_av(:,:)
    real, save, allocatable :: airquality_data_av(:,:)
    real, save, allocatable :: activity_data_av(:,:)
    !Order is (source_type,size,track,road)
    real, save, allocatable :: M_road_data_av(:,:,:,:)  
    !Order is (source_type,size,process_type,track,road)
    real, save, allocatable :: C_data_av(:,:,:,:,:)
    real, save, allocatable :: E_road_data_av(:,:,:,:,:)

    !Declare functions
    
    unit_out=unit_save_road_emission_and_mass_data_stats
    
    !Allocate average dimension
    if (ro_tot.eq.1) then
    if (.not.allocated(date_data_av)) allocate (date_data_av(num_date_index))
    if (.not.allocated(traffic_data_av)) allocate (traffic_data_av(num_traffic_index,0:n_roads))
    if (.not.allocated(airquality_data_av)) allocate (airquality_data_av(num_airquality_index,0:n_roads))
    if (.not.allocated(activity_data_av)) allocate (activity_data_av(num_activity_index,0:n_roads))
    if (.not.allocated(M_road_data_av)) allocate (M_road_data_av(num_source_all,num_size,num_track,0:n_roads))
    if (.not.allocated(C_data_av)) allocate (C_data_av(num_source_all,num_size,num_process,num_track,0:n_roads))
    if (.not.allocated(E_road_data_av)) allocate (E_road_data_av(num_source_all,num_size,num_process,num_track,0:n_roads))
    endif
    
    !Calculate average
    n_time_save=max_time_save-min_time_save+1
    
    if (use_single_road_loop_flag) then
    date_data_av=sum(date_data(:,min_time_save:max_time_save),2)/n_time_save
    traffic_data_av=sum(traffic_data(:,min_time_save:max_time_save,0:n_roads),2)/n_time_save
    airquality_data_av=sum(airquality_data(:,min_time_save:max_time_save,0:n_roads),2)/n_time_save
    activity_data_av=sum(activity_data(:,min_time_save:max_time_save,0:n_roads),2)/n_time_save
    M_road_data_av=sum(M_road_data(:,:,min_time_save:max_time_save,:,0:n_roads),3)/n_time_save
    C_data_av=sum(C_data(:,:,:,min_time_save:max_time_save,:,0:n_roads),4)/n_time_save
    E_road_data_av=sum(E_road_data(:,:,:,min_time_save:max_time_save,:,0:n_roads),4)/n_time_save
    else
    date_data_av=sum(date_data(:,min_time_save:max_time_save),2)/n_time_save
    traffic_data_av=sum(traffic_data(:,min_time_save:max_time_save,:),2)/n_time_save
    airquality_data_av=sum(airquality_data(:,min_time_save:max_time_save,:),2)/n_time_save
    activity_data_av=sum(activity_data(:,min_time_save:max_time_save,:),2)/n_time_save
    M_road_data_av=sum(M_road_data(:,:,min_time_save:max_time_save,:,:),3)/n_time_save
    C_data_av=sum(C_data(:,:,:,min_time_save:max_time_save,:,:),4)/n_time_save
    E_road_data_av=sum(E_road_data(:,:,:,min_time_save:max_time_save,:,:),4)/n_time_save
    endif
    
    !Open log file
    !if (unit_logfile.gt.0) then
    !    open(unit_logfile,file=filename_log,status='old',position='append')
    !endif

    if (ro_tot.eq.1) then
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	write(unit_logfile,'(A)') 'Saving selected road emission and mass statistical data to file'
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
        temp_name=trim(path_outputdata)//trim(filename_outputdata)//'_emission_mass_stats.txt'

        !Put in date if required
        a=date_data(:,min_time_save)
        
        call date_to_datestr(a,temp_name,temp_name)
        
        write(unit_logfile,'(a)') ' Filename= '//trim(temp_name)

        open(unit_out,file=temp_name,status='replace')
    
        !write(unit_out,'(67A6)') &
        write(unit_out,'(a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12,a,a12)') &
            'Road_num',achar(9), &
            'Road_ID',achar(9),&
            'Year',achar(9), &
            'Month',achar(9), &
            'Day',achar(9), &
            'Hour',achar(9), &
            'n_hours',achar(9), &
            'Traffic',achar(9), &
            'PM10_C_tot',achar(9), &
            'PM25_C_tot',achar(9), &
            'PM10_C_exh',achar(9), &
            'f_conc',achar(9), &
            'PM10_E_tot',achar(9), &
            'PM25_E_tot',achar(9), &
            'M_dust_PM200',achar(9), &
            'M_dust_PM10',achar(9), &
            'M_dust_PM25',achar(9), &
            'M_salt1',achar(9), &
            'M_salt2',achar(9), &
            'M_sand_PM200',achar(9), &
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
            'PM10_E_wind'
        
    endif

    !Sum averages over the tracks 
         
        do ro=n_roads_start,n_roads_end
            if (save_road_data_flag(ro).ne.0) then
                conversion=1./1000./b_road_lanes(ro)
                
            !do ti=min_time_save,max_time_save
                if (use_single_road_loop_flag) then
                    ro_num=ro_tot
                else
                    ro_num=ro
                endif
               
                
                write(unit_out,'(i12,a,i12,a,i12,a,i12,a,i12,a,i12,a,i12,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3,a,es12.3)') &
                    ro_num,achar(9), &
                    road_ID(ro),achar(9),&
                    int(date_data_av(year_index)),achar(9), &
                    int(date_data_av(month_index)),achar(9), &
                    int(date_data_av(day_index)),achar(9), &
                    int(date_data_av(hour_index)),achar(9), &
                    n_time_save,achar(9), &
                    traffic_data_av(N_total_index,ro),achar(9), &
                    sum(C_data_av(total_dust_index,pm_10,C_total_index,:,ro)),achar(9), &
                    sum(C_data_av(total_dust_index,pm_25,C_total_index,:,ro)),achar(9), &
                    sum(C_data_av(exhaust_index,pm_10,C_total_index,:,ro)),achar(9), &
                    airquality_data_av(f_conc_index,ro),achar(9), &
                    sum(E_road_data_av(total_dust_index,pm_10,E_total_index,:,ro)),achar(9), &
                    sum(E_road_data_av(total_dust_index,pm_25,E_total_index,:,ro)),achar(9), &
                    sum(M_road_data_av(total_dust_index,pm_200,:,ro))*conversion,achar(9), &
                    sum(M_road_data_av(total_dust_index,pm_10,:,ro))*conversion,achar(9), &
                    sum(M_road_data_av(total_dust_index,pm_25,:,ro))*conversion,achar(9), &
                    sum(M_road_data_av(salt_index(1),pm_all,:,ro))*conversion,achar(9), &
                    sum(M_road_data_av(salt_index(2),pm_all,:,ro))*conversion,achar(9), &
                    sum(M_road_data_av(sand_index,pm_200,:,ro))*conversion,achar(9), &
                    sum(E_road_data_av(road_index,pm_10,E_total_index,:,ro)),achar(9), &
                    sum(E_road_data_av(tyre_index,pm_10,E_total_index,:,ro)),achar(9), &
                    sum(E_road_data_av(brake_index,pm_10,E_total_index,:,ro)),achar(9), &
                    sum(E_road_data_av(sand_index,pm_10,E_total_index,:,ro)),achar(9), &
                    sum(E_road_data_av(fugitive_index,pm_10,E_total_index,:,ro)),achar(9), &
                    sum(E_road_data_av(exhaust_index,pm_10,E_total_index,:,ro)),achar(9), &
                    sum(E_road_data_av(salt_index(1),pm_10,E_total_index,:,ro)),achar(9), &
                    sum(E_road_data_av(salt_index(2),pm_10,E_total_index,:,ro)),achar(9), &
                    sum(E_road_data_av(total_dust_index,pm_10,E_direct_index,:,ro)),achar(9), &
                    sum(E_road_data_av(total_dust_index,pm_10,E_suspension_index,:,ro)),achar(9), &
                    sum(E_road_data_av(total_dust_index,pm_10,E_windblown_index,:,ro))
                    
            !enddo
            endif
        enddo
    
    if (ro_tot.eq.n_roads_total) close (unit_out)
 
    if (ro_tot.eq.n_roads_total) then
    deallocate (date_data_av)
    deallocate (traffic_data_av)
    deallocate (airquality_data_av)
    deallocate (activity_data_av)
    deallocate (M_road_data_av)
    deallocate (C_data_av)
    deallocate (E_road_data_av)
    endif
    
    end subroutine NORTRIP_save_road_emission_and_mass_data_stats