!Saves all the prognostic variables into an ascii file that can be read by MAtlab
!This is chiefly to enable putting the results back into Matlab for comparison
!This routine should only be called when a single road is used
    
    subroutine NORTRIP_save_all_data
    
    use NORTRIP_definitions
    
    implicit none
    
    integer :: unit_out
    !character(64) :: default_file='output\fortran_output'
    character(64) :: default_file='NORTRIP_fortran_output'
    character (256) filename_bin
    
    unit_out=unit_save_all_data
    
    !Open log file
    !if (unit_logfile.gt.0) then
    !    open(unit_logfile,file=filename_log,status='old',position='append')
    !endif

    if (use_single_road_loop_flag) then
	    write(unit_logfile,'(A)') 'Cannot save data when use_single_road_loop_flag=.true.'
        return
    endif
        
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	write(unit_logfile,'(A)') 'Saving data to file'
	write(unit_logfile,'(A)') '----------------------------------------------------------------'

    !default_file=filename_outputdata
    !Save for track and road 1
    tr=1
    ro=1
    
    !Open the outputfile for date
    write(unit_logfile,'(A12,A)') 'Saving to: ',trim(path_fortran_output)//trim(default_file)//'_date_data.txt'
    open(unit_out,file=trim(path_fortran_output)//trim(default_file)//'_date_data.txt',status='replace')
    write(unit_out,'(4i8)') min_time,n_time,min_time_save,max_time_save
    do ti=min_time_save,max_time_save
        write(unit_out,'(5i8,e16.8)') (int(date_data(i,ti)),i=1,5),date_data(6,ti)
    enddo
    close(unit_out,status='keep')
    
    !write the variables as 4 decimal place ascii data with no headers. Only the saved data
    write(unit_logfile,'(A12,A)') 'Saving to: ',trim(path_fortran_output)//trim(default_file)//'_M_road_data.txt'
    open(unit_out,file=trim(path_fortran_output)//trim(default_file)//'_M_road_data.txt',status='replace')
    do ro=1,n_roads
    do tr=1,num_track
    do ti=min_time_save,max_time_save
        write(unit_out,'(<num_source_all*num_size>e12.4)') ((M_road_data(s,x,ti,tr,ro),s=1,num_source_all),x=1,num_size)
    enddo
    enddo
    enddo
    close(unit_out,status='keep')

    !Save the variable in binary format
    !filename_bin=trim(path_fortran_output)//trim(default_file)//'_M_road_data.dat'
    !write(unit_logfile,'(A12,A)') 'Saving to: ',trim(filename_bin)
    !open(unit_out,file=trim(filename_bin),form='unformatted',status='replace')
    !    write(unit_out) M_road_data
    !close(unit_out,status='keep')

    write(unit_logfile,'(A12,A)') 'Saving to: ',trim(path_fortran_output)//trim(default_file)//'_M_road_balance_data.txt'
    open(unit_out,file=trim(path_fortran_output)//trim(default_file)//'_M_road_balance_data.txt',status='replace')
    do ro=1,n_roads
    do tr=1,num_track
    do ti=min_time_save,max_time_save
        write(unit_out,'(<num_source_all*num_size*num_dustbalance>e12.4)') (((M_road_balance_data(s,x,i,ti,tr,ro),s=1,num_source_all),x=1,num_size),i=1,num_dustbalance)
    enddo
    enddo
    enddo
    close(unit_out,status='keep')

    write(unit_logfile,'(A12,A)') 'Saving to: ',trim(path_fortran_output)//trim(default_file)//'_C_data.txt'
    open(unit_out,file=trim(path_fortran_output)//trim(default_file)//'_C_data.txt',status='replace')
    do ro=1,n_roads
    do tr=1,num_track
    do ti=min_time_save,max_time_save
        write(unit_out,'(<num_source_all*num_size*num_process>e12.4)') (((C_data(s,x,i,ti,tr,ro),s=1,num_source_all),x=1,num_size),i=1,num_process)
    enddo
    enddo
    enddo
    close(unit_out,status='keep')

    write(unit_logfile,'(A12,A)') 'Saving to: ',trim(path_fortran_output)//trim(default_file)//'_E_road_data.txt'
    open(unit_out,file=trim(path_fortran_output)//trim(default_file)//'_E_road_data.txt',status='replace')
    do ro=1,n_roads
    do tr=1,num_track
    do ti=min_time_save,max_time_save
        write(unit_out,'(<num_source_all*num_size*num_process>e12.4)') (((E_road_data(s,x,i,ti,tr,ro),s=1,num_source_all),x=1,num_size),i=1,num_process)
    enddo
    enddo
    enddo
    close(unit_out,status='keep')

    write(unit_logfile,'(A12,A)') 'Saving to: ',trim(path_fortran_output)//trim(default_file)//'_WR_time_data.txt'
    open(unit_out,file=trim(path_fortran_output)//trim(default_file)//'_WR_time_data.txt',status='replace')
    do ro=1,n_roads
    do tr=1,num_track
    do ti=min_time_save,max_time_save
        write(unit_out,'(<num_wear>e12.4)') (WR_time_data(s,ti,tr,ro),s=1,num_wear)
    enddo
    enddo
    enddo
    close(unit_out,status='keep')

    write(unit_logfile,'(A12,A)') 'Saving to: ',trim(path_fortran_output)//trim(default_file)//'_road_salt_data.txt'
    open(unit_out,file=trim(path_fortran_output)//trim(default_file)//'_road_salt_data.txt',status='replace')
    do ro=1,n_roads
    do tr=1,num_track
    do ti=min_time_save,max_time_save
        write(unit_out,'(<num_saltdata*num_salt>e12.4)') ((road_salt_data(s,i,ti,tr,ro),s=1,num_saltdata),i=1,num_salt)
    enddo
    enddo
    enddo
    close(unit_out,status='keep')

    write(unit_logfile,'(A12,A)') 'Saving to: ',trim(path_fortran_output)//trim(default_file)//'_road_meteo_data.txt'
    open(unit_out,file=trim(path_fortran_output)//trim(default_file)//'_road_meteo_data.txt',status='replace')
    do ro=1,n_roads
    do tr=1,num_track
    do ti=min_time_save,max_time_save
        write(unit_out,'(<num_road_meteo>e12.4)') (road_meteo_data(i,ti,tr,ro),i=1,num_road_meteo)
    enddo
    enddo
    enddo
    close(unit_out,status='keep')

    write(unit_logfile,'(A12,A)') 'Saving to: ',trim(path_fortran_output)//trim(default_file)//'_g_road_balance_data.txt'
    open(unit_out,file=trim(path_fortran_output)//trim(default_file)//'_g_road_balance_data.txt',status='replace')
    do ro=1,n_roads
    do tr=1,num_track
    do ti=min_time_save,max_time_save
        write(unit_out,'(<num_moisture*num_moistbalance>e12.4)') ((g_road_balance_data(m,i,ti,tr,ro),m=1,num_moisture),i=1,num_moistbalance)
    enddo
    enddo
    enddo
    close(unit_out,status='keep')

    write(unit_logfile,'(A12,A)') 'Saving to: ',trim(path_fortran_output)//trim(default_file)//'_g_road_data.txt'
    open(unit_out,file=trim(path_fortran_output)//trim(default_file)//'_g_road_data.txt',status='replace')
    do ro=1,n_roads
    do tr=1,num_track
    do ti=min_time_save,max_time_save
        write(unit_out,'(<num_moisture>e12.4)') (g_road_data(m,ti,tr,ro),m=1,num_moisture)
    enddo
    enddo
    enddo
    close(unit_out,status='keep')

    write(unit_logfile,'(A12,A)') 'Saving to: ',trim(path_fortran_output)//trim(default_file)//'_f_q.txt'
    open(unit_out,file=trim(path_fortran_output)//trim(default_file)//'_f_q.txt',status='replace')
    do ro=1,n_roads
    do tr=1,num_track
    do ti=min_time_save,max_time_save
        write(unit_out,'(<num_source_all>e12.4)') (f_q(s,ti,tr,ro),s=1,num_source_all)
    enddo
    enddo
    enddo
   close(unit_out,status='keep')

    write(unit_logfile,'(A12,A)') 'Saving to: ',trim(path_fortran_output)//trim(default_file)//'_f_q_obs.txt'
    open(unit_out,file=trim(path_fortran_output)//trim(default_file)//'_f_q_obs.txt',status='replace')
    do ro=1,n_roads
    do tr=1,num_track
    do ti=min_time_save,max_time_save
        write(unit_out,'(e12.4)') f_q_obs(ti,tr,ro)
    enddo
    enddo
    enddo
    close(unit_out,status='keep')

    write(unit_logfile,'(A12,A)') 'Saving to: ',trim(path_fortran_output)//trim(default_file)//'_traffic_data.txt'
    open(unit_out,file=trim(path_fortran_output)//trim(default_file)//'_traffic_data.txt',status='replace')
    do ro=1,n_roads
    do tr=1,num_track
    do ti=min_time_save,max_time_save
        write(unit_out,'(<num_traffic_index>e12.4)') (traffic_data(i,ti,ro),i=1,num_traffic_index)
    enddo
    enddo
    enddo
    close(unit_out,status='keep')

    !Remove wind speed correction, LW rad, RH and temperature offsets before saving meteo_data
    !----------------------------------------------------------------------
    do ro=1,n_roads
        meteo_data(FF_index,:,ro)=meteo_data(FF_index,:,ro)/wind_speed_correction(ro)
        meteo_data(RH_index,:,ro)=max(0.,min(100.,meteo_data(RH_index,:,ro)-RH_offset))
        meteo_data(T_a_index,:,ro)=meteo_data(T_a_index,:,ro)-T_a_offset
        meteo_data(long_rad_in_index,:,ro)= meteo_data(long_rad_in_index,:,ro)-long_rad_in_offset
    enddo
    
    write(unit_logfile,'(A12,A)') 'Saving to: ',trim(path_fortran_output)//trim(default_file)//'_meteo_data.txt'
    open(unit_out,file=trim(path_fortran_output)//trim(default_file)//'_meteo_data.txt',status='replace')
    do ro=1,n_roads
    do ti=min_time_save,max_time_save
        write(unit_out,'(<num_meteo_index>e12.4)') (meteo_data(i,ti,ro),i=1,num_meteo_index)
    enddo
    enddo
    close(unit_out,status='keep')

    write(unit_logfile,'(A12,A)') 'Saving to: ',trim(path_fortran_output)//trim(default_file)//'_airquality_data.txt'
    open(unit_out,file=trim(path_fortran_output)//trim(default_file)//'_airquality_data.txt',status='replace')
    do ro=1,n_roads
    do ti=min_time_save,max_time_save
        write(unit_out,'(<num_airquality_index>e12.4)') (airquality_data(i,ti,ro),i=1,num_airquality_index)
    enddo
    enddo
    close(unit_out,status='keep')

    write(unit_logfile,'(A12,A)') 'Saving to: ',trim(path_fortran_output)//trim(default_file)//'_activity_data.txt'
    open(unit_out,file=trim(path_fortran_output)//trim(default_file)//'_activity_data.txt',status='replace')
    do ro=1,n_roads
    do ti=min_time_save,max_time_save
        write(unit_out,'(<num_activity_index>e12.4)') (activity_data(i,ti,ro),i=1,num_activity_index)
    enddo
    enddo

    close(unit_out,status='keep')
  
     
    !if (unit_logfile.gt.0) then
    !    close(unit_logfile,status='keep')
    !endif
   
    end subroutine NORTRIP_save_all_data