!NORTRIP_save_init_data.f90
    
!Saves initialisation data
    
    subroutine NORTRIP_read_init_data_single
    
    use NORTRIP_definitions
    
    implicit none
    
    integer :: unit_out
    character (256) filename_bin
    character (256) filename_asc
    character (256) filename_temp
    character(256) text_temp
    integer current_date(num_date_index)
    integer tt
    logical :: save_bin=.false.
    integer n_roads_init,num_track_init,num_source_all_init,num_road_meteo_init,num_moisture_init
    logical exists
    
    !Do not read init. Hardcode for testing
    !hours_between_init=0
    
    unit_out=unit_read_init_data
    
    !Leave this if it is not relevant
    !if (hours_between_init.lt.0) then
	!    write(unit_logfile,'(A)') ' WARNING: Not reading data from init file'
    !    return
    !endif
    
    !Open log file
    !if (unit_logfile.gt.0) then
    !    open(unit_logfile,file=filename_log,status='old',position='append')
    !endif
    
    !if (mod(ti,hours_between_init).eq.0) then
            !Set the path and file name
            filename_temp=filename_init
    
            !Open the outputfile for date
            filename_asc=trim(path_init)//trim(filename_temp)
            !filename_bin=trim(path_init)//trim(filename_temp)
              
            !Check file for reading
            inquire(file=trim(filename_asc),exist=exists)
            if (.not.exists) then
                if (ro_tot.eq.1) write(unit_logfile,'(A)')' WARNING: Initial input file does not exist: '//trim(filename_asc)
                return 
            endif

        !If it is the first loop then open the file
        if (ro_tot.eq.1) then


	        write(unit_logfile,'(A)') ''
	        write(unit_logfile,'(A)') 'Reading data from init file (NORTRIP_read_init_data_single)'
  	        write(unit_logfile,'(A)') '================================================================'
    

            !current_date=date_data(:,min_time)
            
            !Extract the initdata from zip files. Not used, too time consuming to save and extract again
            !if (save_initdata_in_zip_format) then
            !    temp_name_zip=trim(temp_path)//trim(temp_file)//'.zip'
            !    inquire(file=trim(temp_name_zip),exist=exists)
            !    if (.not.exists.and.input_file_type.ne.activity_file_type) then
            !        write(unit_logfile,'(a)')'ERROR: File '//trim(temp_name_zip)//' does not exist.'
            !        write(unit_logfile,'(a)')'STOPPING'
            !        stop
            !    endif
                !Unzip
            !    write(unit_logfile,'(a,a)') 'Extracting from zip format: ',trim(temp_name_zip)       
            !    command_line_zip='7za e -tzip '//trim(temp_name_zip)//' -o'//trim(temp_path)
            !    write(unit_logfile,'(a,a)') 'Command line zip: ',trim(command_line_zip)      
            !    CALL EXECUTE_COMMAND_LINE (trim(command_line_zip),wait=.true.)
            !endif
            

            write(unit_logfile,'(A,A)') ' Reading from: ',trim(filename_asc)
            open(unit_out,file=trim(filename_asc),access='sequential',status='old',readonly)
       
            read(unit_out,'(5A16)') text_temp,text_temp,text_temp,text_temp,text_temp
            read(unit_out,'(5i16)') n_roads_init,num_track_init,num_source_all_init,num_road_meteo_init,num_moisture_init
            
            if (n_roads_init.ne.n_roads_total.or.num_track_init.ne.num_track.or.num_source_all_init.ne.num_source_all &
                .or.num_road_meteo_init.ne.num_road_meteo.or.num_moisture_init.ne.num_moisture) then
                write(unit_logfile,'(A,5i4,A,5i4)')' ERROR: Initial input file arrays do not match current. Read: ', &
                    n_roads_init,num_track_init,num_source_all_init,num_road_meteo_init,num_moisture_init &
                    ,' Existing: ',n_roads,num_track,num_source_all,num_road_meteo,num_moisture
                stop         
            endif     
            
        endif
        
            ti=min_time
        
            read(unit_out,'(i16)') ro  
            if (ro_tot.ne.ro) then
                write(unit_logfile,'(A,2i6)')' ERROR: Mismatch in road index during single road loop init file reading (ro_tot,ro): ', ro_tot,ro
                stop
            endif
                
            !write(*,*) n_roads_init,num_track_init,num_source_all_init,num_road_meteo_init,num_moisture_init
            ro=n_roads !Which is 0 for the single road
            do tr=1,num_track
                read(unit_out,'(<num_source_all*num_size>e12.4)') ((M_road_data(s,x,ti,tr,ro),s=1,num_source_all),x=1,num_size)
            enddo
            !write(*,*) M_road_data(1,1,ti,1,ro),M_road_data(1,2,ti,1,ro),M_road_data(2,1,ti,1,ro)
            
            do tr=1,num_track
                 read(unit_out,'(<num_road_meteo>e12.4)') (road_meteo_data(i,ti,tr,ro),i=1,num_road_meteo)
            enddo

            do tr=1,num_track
                read(unit_out,'(<num_moisture>e12.4)') (g_road_data(m,ti,tr,ro),m=1,num_moisture)
            enddo
            !write(*,*) g_road_data(1,ti,1,ro),g_road_data(2,ti,1,ro)
            
            read(unit_out,'(5e12.4)') time_since_last_salting(ro),time_since_last_binding(ro), &
                    time_since_last_sanding(ro),time_since_last_cleaning(ro),time_since_last_ploughing(ro)
                !May be round off errors due to real precision of data and saving to init files so set mimimum of 0 hours
                

        if (ro_tot.eq.n_roads_total) then
            close(unit_out,status='keep')
        endif
            
        
            !if (save_bin) then
            !    write(unit_logfile,'(A,A)') ' Reading from: ',filename_bin
            !    open(unit_out,file=trim(filename_bin),form='unformatted',status='replace')
        
             !   read(unit_out) M_road_data
             !   read(unit_out) road_meteo_data
             !   read(unit_out) g_road_data 
        
             !   close(unit_out,status='keep')
            !endif
            
     
    !if (unit_logfile.gt.0) then
    !    close(unit_logfile,status='keep')
    !endif

    !Set the bin data for the given input
    !do ro=1,n_roads
    !do x=1,num_size-1
    !    M_road_bin_data(:,x,ti_bin,:,ro_bin)=M_road_data(:,x,ti,:,:)-M_road_data(:,x+1,ti,:,:)
    !enddo      
    !enddo
    
    !endif
    

    end subroutine NORTRIP_read_init_data_single
    
