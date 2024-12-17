!NORTRIP_initialise_time
    
!-----------------------------------------------------------------------
subroutine NORTRIP_initialise_time

    use NORTRIP_definitions    
    !use datetime_module!,ONLY:datetime,date2num

    implicit none
    
    integer a(6),b(6)
    double precision date_to_number
    character(24) a_str,format_str,b_str

    !Open log file
    !if (unit_logfile.gt.0) then
    !    open(unit_logfile,file=filename_log,status='old',position='append')
    !endif
    
    if (ro_tot.eq.1) then
        
    write(unit_logfile,'(A)') ''
	write(unit_logfile,'(A)') 'Setting time parameters (NORTRIP_initialise_time)'
	write(unit_logfile,'(A)') '================================================================'
    
    !Set time loop index
    min_time=1
    max_time=n_time
    max_time_inputdata=n_time

    a=0
    b=0
    !Set the date_num_data parameter and date strings
    !NOTE: THis, to be of any use, should be seconds, not days and should be fixed elsewhere.
    do ti=1,n_time
        a(1:5)=date_data(1:5,ti)
        date_data(datenum_index,ti)=date_to_number(a,2000)
        call date_to_datestr(a,trim('yyyy.mm.dd HH'),date_str(1,ti))
        call date_to_datestr(a,trim('HH:MM dd mm '),date_str(2,ti))
        call date_to_datestr(a,date_format_str,date_str(3,ti))
        call date_to_datestr(a,trim('yyyy-mm-dd HH:MM:SS'),date_str(4,ti))
    enddo
    
    a(1:5)=date_data(1:5,1)
    call date_to_datestr(a,date_format_str,min_date_str)
    write(unit_logfile,'(2A24,i6)') 'Min date: ',trim(min_date_str),1
    a(1:5)=date_data(1:5,n_time)
    call date_to_datestr(a,date_format_str,max_date_str)
    write(unit_logfile,'(2A24,i6)') 'Max date: ',trim(max_date_str),n_time
   
    !Set time step for iteration based on the first time step of the input data
    !dt=(date_data(datenum_index,min_time+1)-date_data(datenum_index,min_time))*24
    !Use the following versioon to preserve the double precision
    a(1:5)=date_data(1:5,1)
    b(1:5)=date_data(1:5,2)
    dt=(date_to_number(b,2000)-date_to_number(a,2000))*24.
    
    !Set start and end dates based on date string (if specified in 'set_road_dust_inputdata_files_v1')
    call find_time_index(start_date_str,min_time)
    call find_time_index(start_date_save_str,min_time_save)
    call find_time_index(end_date_str,max_time)
    call find_time_index(end_date_save_str,max_time_save)
    
    if (start_date_str.eq.'') then
        start_date_str=min_date_str
        min_time=1
    endif
    if (end_date_str.eq.'') then
        end_date_str=max_date_str
        max_time=n_time
    endif
    if (start_date_save_str.eq.'') then
        start_date_save_str=start_date_str
        min_time_save=min_time
    endif
    if (end_date_save_str.eq.'') then
        end_date_save_str=end_date_str
        max_time_save=max_time
    endif

    !Limit the saving to be within the time boundaries
    min_time_save=max(min_time_save,min_time)
    max_time_save=min(max_time_save,max_time)
    a(1:5)=date_data(1:5,min_time_save)
    call date_to_datestr(a,date_format_str,start_date_save_str)
    a(1:5)=date_data(1:5,max_time_save)
    call date_to_datestr(a,date_format_str,end_date_save_str)  
    
    write(unit_logfile,'(2A24,i6)') 'Start date: ',trim(start_date_str),min_time
    write(unit_logfile,'(2A24,i6)') 'End date: ',trim(end_date_str),max_time
    write(unit_logfile,'(2A24,i6)') 'Start save date: ',trim(start_date_save_str),min_time_save
    write(unit_logfile,'(2A24,i6)') 'End save date: ',trim(end_date_save_str),max_time_save
    write(unit_logfile,'(A24,f6.2)') 'Time step (hours): ',dt
  	write(unit_logfile,'(A)') '----------------------------------------------------------------'

    !if (unit_logfile.gt.0) then
    !    close(unit_logfile,status='keep')
    !endif

    endif

        
    end subroutine NORTRIP_initialise_time
!-----------------------------------------------------------------------
     
!-----------------------------------------------------------------------
    subroutine find_time_index(date_str_in,ti_index)
    
    use NORTRIP_definitions    

    implicit none
   
    character(24) date_str_in
    integer ti_index
    logical found
    integer a(6)
      
    if (date_str_in.ne.'') then
        found=.false.
        do ti=1,n_time
            call datestr_to_date(date_str_in,date_format_str,a)
            if ((a(1).eq.date_data(year_index,ti)).and.(a(2).eq.date_data(month_index,ti)).and.(a(3).eq.date_data(day_index,ti)).and.(a(4).eq.date_data(hour_index,ti)).and.(a(5).eq.date_data(minute_index,ti))) then
                ti_index=ti
                found=.true.
            endif
        enddo
        if (.not.found) then
            write(unit_logfile,*) 'WARNING: No match found for date: ',trim(date_str_in)
            return
        endif        
    endif
 
    end subroutine find_time_index
!-----------------------------------------------------------------------
