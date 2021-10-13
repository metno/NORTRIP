!NORTRIP_save_init_data.f90
    
!Saves initialisation data
    
    subroutine NORTRIP_read_init_data
    
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
    integer ro_init
    logical :: input_is_nan=.false.
    character(256) temp_name
    integer a(num_date_index)    
    
    !Do not read init. Hardcode for testing
    !hours_between_init=0
    
    unit_out=unit_read_init_data
    
    !Leave this if it is not relevant
    if (hours_between_init.le.0) then
	    write(unit_logfile,'(A)') ' WARNING: Not reading data from init file'
        return
    endif
    
    !Open log file
    !if (unit_logfile.gt.0) then
    !    open(unit_logfile,file=filename_log,status='old',position='append')
    !endif

	write(unit_logfile,'(A)') ''
	write(unit_logfile,'(A)') 'Reading data from init file (NORTRIP_read_init_data)'
  	write(unit_logfile,'(A)') '================================================================'
    

        !current_date=date_data(:,min_time)
            
        !Set the path and file name
        filename_temp=filename_init
    
        a=date_data(:,min_time)
        call date_to_datestr_bracket(a,path_init,temp_name)
        call date_to_datestr_bracket(a,temp_name,temp_name)
        call date_to_datestr_bracket(a,temp_name,temp_name)

        !Open the outputfile for date
        filename_asc=trim(temp_name)//trim(filename_temp)
        !filename_bin=trim(path_init)//trim(filename_temp)
        
        !Check file for reading
        inquire(file=trim(filename_asc),exist=exists)
        if (.not.exists) then
            write(unit_logfile,'(A)')' WARNING: Initial input file does not exist: '//trim(filename_asc)
            return 
        endif

        write(unit_logfile,'(A,A)') ' Reading from: ',trim(filename_asc)
        open(unit_out,file=trim(filename_asc),access='sequential',status='old',readonly,ERR=10)
       
        read(unit_out,'(5A16)',ERR=10) text_temp,text_temp,text_temp,text_temp,text_temp
        read(unit_out,'(5i16)',ERR=10) n_roads_init,num_track_init,num_source_all_init,num_road_meteo_init,num_moisture_init
            
        if (n_roads_init.ne.n_roads.or.num_track_init.ne.num_track.or.num_source_all_init.ne.num_source_all &
            .or.num_road_meteo_init.ne.num_road_meteo.or.num_moisture_init.ne.num_moisture) then
            write(unit_logfile,'(A,i8,4i4,A,i8,4i4)')' WARNING: Initial input file arrays do not match current. Read: ', &
                n_roads_init,num_track_init,num_source_all_init,num_road_meteo_init,num_moisture_init &
                ,' Existing: ',n_roads,num_track,num_source_all,num_road_meteo,num_moisture
            return         
        endif     
            
        ti=min_time
        
            !read(unit_out,'(A)') text_temp  
            do ro=1,n_roads
            
            read(unit_out,'(i16)') ro_init 
            if (ro_init.ne.ro) then
                write(unit_logfile,'(A,2i6)')' ERROR: Mismatch in road index during init file reading (ro_init,ro): ', ro_init,ro
                stop 1
            endif
                
            do tr=1,num_track
                read(unit_out,'(<num_source_all*num_size>e12.4)',ERR=10) ((M_road_data(s,x,ti,tr,ro),s=1,num_source_all),x=1,num_size)
            enddo
            !enddo
        
            !read(unit_out,'(A)') text_temp
            !do ro=1,n_roads
            do tr=1,num_track
                 read(unit_out,'(<num_road_meteo>e12.4)',ERR=10) (road_meteo_data(i,ti,tr,ro),i=1,num_road_meteo)
            enddo
            !enddo

            !read(unit_out,'(A)') text_temp
            !do ro=1,n_roads
            do tr=1,num_track
                read(unit_out,'(<num_moisture>e12.4)',ERR=10) (g_road_data(m,ti,tr,ro),m=1,num_moisture)
            enddo
            !enddo

            !read(unit_out,'(A)') text_temp
            !do ro=1,n_roads
                read(unit_out,'(5e12.4)',ERR=10) time_since_last_salting(ro),time_since_last_binding(ro), &
                    time_since_last_sanding(ro),time_since_last_cleaning(ro),time_since_last_ploughing(ro)
                !May be round off errors due to real precision of data and saving to init files so set mimimum of 0 hours
                
            enddo

        close(unit_out,status='keep')
        
            !Check for NaNs in the input data
            do ro=1,n_roads
            do tr=1,num_track
                do s=1,num_source_all
                do x=1,num_size
                    if (isnan(M_road_data(s,x,ti,tr,ro))) input_is_nan=.true.
                enddo
                enddo
            
                do i=1,num_road_meteo
                    if (isnan(road_meteo_data(i,ti,tr,ro))) input_is_nan=.true.
                enddo

                do m=1,num_moisture
                    if (isnan(g_road_data(m,ti,tr,ro))) input_is_nan=.true.
                enddo
                    
            enddo
            enddo

            !if (save_bin) then
            !    write(unit_logfile,'(A,A)') ' Reading from: ',filename_bin
            !    open(unit_out,file=trim(filename_bin),form='unformatted',status='replace')
        
            !    read(unit_out) M_road_data
            !    read(unit_out) road_meteo_data
            !    read(unit_out) g_road_data 
        
            !    close(unit_out,status='keep')
            !endif
            
        if (input_is_nan) then
            write(unit_logfile,'(A)') 'ERROR reading init file. NaN values in data. Stopping'
            stop 3
        endif
        
        return
        
10      write(unit_logfile,'(A)') 'ERROR reading init file. Stopping'
        stop 2
     
    !if (unit_logfile.gt.0) then
    !    close(unit_logfile,status='keep')
    !endif

    !Set the bin data for the given input
    !do ro=1,n_roads
    !do x=1,num_size-1
    !    M_road_bin_data(:,x,ti_bin,:,ro_bin)=M_road_data(:,x,ti,:,:)-M_road_data(:,x+1,ti,:,:)
    !enddo      
    !enddo

    end subroutine NORTRIP_read_init_data
    
