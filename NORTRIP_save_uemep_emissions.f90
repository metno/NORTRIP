!NORTRIP_save_uemep_emissions.f90
    
!----------------------------------------------------------------------
    subroutine NORTRIP_save_uemep_emissions
    
    use NORTRIP_definitions
    
    implicit none
    
    integer :: unit_out
    character(24) uemep_start_date_str
    character(24) uemep_end_date_str
    character(24) :: uemep_date_format_str='yyyymmddHH'
    character(256) temp_name,filename_temp
    logical exists
    integer a_start(num_date_index),a_end(num_date_index)
    real conversion
    character(12) :: pm_str=''
    real emis_road
    real length_road_km(0:n_roads)
    real, save, allocatable :: emis_sum(:)
    real emis_time(n_time)
    integer unit_count
    integer n_time_save
    integer x_loop,save_size(8)
    integer n_roads_total_save
    integer n_x_save
    
    unit_out=unit_save_emissions
    
    !Declare functions
    n_time_save=max_time_save-min_time_save+1
   
    if (ro_tot.eq.1) then
        
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	write(unit_logfile,'(A)') 'Saving uEMEP emissions to file'
	write(unit_logfile,'(A)') '----------------------------------------------------------------'

    !Set start and end date stamps
    n_time_save=max_time_save-min_time_save+1
    a_start=date_data(:,min_time_save)
    a_end=date_data(:,max_time_save)
    call date_to_datestr(a_start,uemep_date_format_str,uemep_start_date_str)
    call date_to_datestr(a_end,uemep_date_format_str,uemep_end_date_str)

    !Change the date stamp to follow NILU conventions
    !call incrtm(-1,a_start(1),a_start(2),a_start(3),a_start(4))
  
    if (.not.allocated(emis_sum)) then
        allocate (emis_sum(num_size+2))
    endif

    !Check that path exists after filling in date stamp
    call date_to_datestr_bracket(a_start,path_output_emis,temp_name)
    call date_to_datestr_bracket(a_start,temp_name,temp_name)
    call date_to_datestr_bracket(a_start,temp_name,temp_name)
    
    inquire(directory=trim(temp_name),exist=exists)
    if (.not.exists) then
        write(unit_logfile,'(A)')'ERROR: Path '//trim(temp_name)//' does not exist. No file saved'
        return
    endif

    
    endif
    
    !NORTRIP emissions are g/km/hour. 
    conversion=1.
    !conversion=1.e6/1000./3600 !Convert to ug/s/m for use in EMEP (ug/s)

    unit_count=0
    
    n_x_save=4
    save_size(1)=pm_10;save_size(2)=pm_25;save_size(3)=pm_exhaust;save_size(4)=nox_exhaust
    
    do x_loop=1,n_x_save
   
        x=save_size(x_loop)
        unit_count=x_loop
        unit_out=unit_save_emissions+unit_count

        if (x.eq.pm_25) pm_str='PM25'
        if (x.eq.pm_10) pm_str='PM10'
        if (x.eq.pm_exhaust) pm_str='EP'
        if (x.eq.nox_exhaust) pm_str='NOX'

        if (ro_tot.eq.1) then
            
            !Calculate number of roads that will be saved
            n_roads_total_save=0
            if (use_single_road_loop_flag) then
            do ro=1,n_roads_total
                if (line_or_grid_data_flag(ro).eq.1.or.line_or_grid_data_flag(ro).eq.3) then
                    n_roads_total_save=n_roads_total_save+1
                endif
            enddo  
            else
            do ro=1,n_roads
                if (line_or_grid_data_flag(ro).eq.1.or.line_or_grid_data_flag(ro).eq.3) then
                    n_roads_total_save=n_roads_total_save+1
                endif
            enddo
            endif
            
            
            !Open the outputfile for date
            temp_name=trim(path_output_emis)//trim(filename_output_emis)//'_'//trim(pm_str)//'_'//trim(uemep_start_date_str)//'-'//trim(uemep_end_date_str)//'.txt'
        
            !Put in date in path and filename if required
            call date_to_datestr_bracket(a_start,temp_name,temp_name)
            call date_to_datestr_bracket(a_start,temp_name,temp_name)
            call date_to_datestr_bracket(a_start,temp_name,temp_name)

            write(unit_logfile,'(a)') ' Filename= '//trim(temp_name)
            
            open(unit_out,file=temp_name,status='replace')

            
            write(unit_out,'(A)') '# NORTRIP emission output for uEMEP'
            if (x.eq.pm_25.or.x.eq.pm_10) then
            if (available_airquality_data(EP_emis_index).or.exhaust_EF_available.ne.0) then
                write(unit_out,'(A,A)') '# Hourly traffic non-exhaust and exhaust emission for compound: ',trim(pm_str)
            else
                write(unit_out,'(A,A)') '# Hourly traffic non-exhaust only emission for compound: ',trim(pm_str)            
            endif
            endif
            
            if (x.eq.pm_exhaust) then
            if (available_airquality_data(EP_emis_index).or.exhaust_EF_available.ne.0) then
                write(unit_out,'(A,A)') '# Hourly traffic exhaust emission for compound: ',trim(pm_str)
            else
                write(unit_out,'(A)') '# No exhaust emission data available'          
            endif
            endif
            
            if (x.eq.nox_exhaust) then
            if (available_airquality_data(NOX_emis_index).or.NOX_EF_available.ne.0) then
                write(unit_out,'(A,A)') '# Hourly traffic exhaust emission for compound: ',trim(pm_str)
            else
                write(unit_out,'(A)') '# No exhaust emission data available'          
            endif
            endif
            
            write(unit_out,'(A)')   '# Emission data unit'
            write(unit_out,'(A)')   'g/km/hr'
            write(unit_out,'(A)')   '# Emission data start and end date'
            write(unit_out,'(A)')    trim(uemep_start_date_str)//'   '//trim(uemep_end_date_str)
            write(unit_out,'(A)')   '# Number of roads     Number of hours'
            write(unit_out,'(2i)')   n_roads_total_save,n_time_save      
            write(unit_out,'(A)')   '# RoadLinkID    Emission'
        
            emis_sum(x)=0.
        
        endif
        
        !Note that if line_or_grid_data_flag(ro).eq.0 then this is a link without traffic data.
        !Must be saved but with no information
        do ro=n_roads_start,n_roads_end
            
            !Length of road in km
            if (length_road(ro).eq.0) then
                length_road_km(ro)=sqrt((x_road(1,ro)-x_road(2,ro))**2+(y_road(1,ro)-y_road(2,ro))**2)/1000.
            else
                length_road_km(ro)=length_road(ro)/1000.
            endif
            
            
            do ti=min_time_save,max_time_save
                
            if (line_or_grid_data_flag(ro).eq.1.or.line_or_grid_data_flag(ro).eq.-1.or.line_or_grid_data_flag(ro).eq.3) then
                
                if (line_or_grid_data_flag(ro).eq.1.or.line_or_grid_data_flag(ro).eq.3) then
                    if (x.eq.pm_exhaust) then
                        emis_road=sum(E_road_data(exhaust_index,pm_25,E_total_index,ti,:,ro))*conversion
                    elseif (x.eq.nox_exhaust) then
                        emis_road=0.
                        if (available_airquality_data(NOX_emis_index)) then
                            emis_road=emis_road+airquality_data(NOX_emis_index,ti,ro)*conversion
                        elseif (NOX_EF_available.ne.0) then
                            do v=1,num_veh
                                emis_road=emis_road+traffic_data(N_v_index(v),ti,ro)*NOX_EF(v,ro)*conversion
                            enddo
                        else
                            emis_road=0.
                        endif                              
                    else
                        emis_road=sum(E_road_data(total_dust_index,x,E_total_index,ti,:,ro))*conversion
                    endif
                    
                else
                    !These are roads without traffic data. Shouldn't go here but is possible if reading links to use instead of using ADT
                    emis_road=0.                   
                endif
            
                !Save emissions when using single roads to get it in the right order
                !Write them at end of calculation
                emis_time(ti)=emis_road            
                emis_sum(x)=emis_sum(x)+emis_road*length_road_km(ro)
                
            endif
            
            enddo

            !write(*,*) ro_tot,ro,road_ID(ro_tot),n_time_save
            if (line_or_grid_data_flag(ro).eq.1.or.line_or_grid_data_flag(ro).eq.-1.or.line_or_grid_data_flag(ro).eq.3) then
                if (use_single_road_loop_flag) then 
                    write(unit_out,'(i12,<n_time_save>es12.4)') road_ID(ro_tot),emis_time(min_time_save:max_time_save)
                else
                    write(unit_out,'(i12,<n_time_save>es12.4)') road_ID(ro),emis_time(min_time_save:max_time_save)
                endif
            endif
            
        enddo
    
        if (ro_tot.eq.n_roads_total) then           

            close (unit_out)
    
            !Conversion is for g to kg
            write(unit_logfile,'(A,a,a,es12.2)') ' Total emissions of ',trim(pm_str),' (kg) for all road links over this period = ',emis_sum(x)*1.e-3*dt

        endif
        
   enddo
    
   if (allocated(emis_sum).and.(ro_tot.eq.n_roads_total)) deallocate(emis_sum)
                
 
    end subroutine NORTRIP_save_uemep_emissions
!----------------------------------------------------------------------

!----------------------------------------------------------------------
    subroutine NORTRIP_save_uemep_emissions_all
    
    use NORTRIP_definitions
    
    implicit none
    
    integer :: unit_out
    character(24) uemep_start_date_str
    character(24) uemep_end_date_str
    character(24) :: uemep_date_format_str='yyyymmddHH'
    character(256) temp_name,filename_temp
    logical exists
    integer a_start(num_date_index),a_end(num_date_index)
    real conversion
    character(12) :: pm_str=''
    real emis_road
    real length_road_km(0:n_roads)
    real, save, allocatable :: emis_sum(:)
    real emis_time(n_time)
    integer unit_count
    integer n_time_save
    integer x_loop
    integer n_roads_total_save
    integer n_x_save,n_x_save_max
    parameter (n_x_save_max=8)
    integer save_size(n_x_save_max)
    integer save_source(n_x_save_max)
    character(10) save_str(n_x_save_max)
    character(256) file_str
    
    !Standard without sand or salt
    n_x_save=4
    save_size(1)=pm_10;save_size(2)=pm_25;save_size(3)=pm_exhaust;save_size(4)=nox_exhaust
    save_str(1)='PM10';save_str(2)='PM25';save_str(3)='PMex';save_str(4)='NOX'
    save_source(1)=total_dust_index;save_source(2)=total_dust_index;save_source(3)=exhaust_index;save_source(4)=NOX_emis_index
    file_str='all'

    if (index(calculation_type,'uEMEP_sand_salt').gt.0) then
        !With sand and salt
        n_x_save=6
        save_size(5)=pm_10;save_size(6)=pm_10;save_size(7)=pm_25;save_size(8)=pm_25
        save_str(5)='PM10_sand';save_str(6)='PM10_salt';save_str(7)='PM25_sand';save_str(8)='PM25_salt'
        save_source(5)=sand_index;save_source(6)=salt_index(na);save_source(7)=sand_index;save_source(8)=salt_index(na)
        file_str='all_sand_salt'
    elseif (index(calculation_type,'uEMEP_sand').gt.0) then
        !With sand
        n_x_save=5
        save_size(5)=pm_10;save_size(6)=pm_25;
        save_str(5)='PM10_sand';save_str(6)='PM25_sand';
        save_source(5)=sand_index;save_source(6)=sand_index;
        file_str='all_sand'
    elseif (index(calculation_type,'uEMEP_salt').gt.0) then
        !With salt
        n_x_save=5
        save_size(5)=pm_10;save_size(6)=pm_25;
        save_str(5)='PM10_salt';save_str(6)='PM25_salt'
        save_source(5)=salt_index(na);save_source(6)=salt_index(na)
        file_str='all_salt'
    endif

    unit_out=unit_save_emissions
    
    !Declare functions
    n_time_save=max_time_save-min_time_save+1
   
    if (ro_tot.eq.1) then
        
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	write(unit_logfile,'(A)') 'Saving uEMEP emissions to file'
	write(unit_logfile,'(A)') '----------------------------------------------------------------'

    !Set start and end date stamps
    n_time_save=max_time_save-min_time_save+1
    a_start=date_data(:,min_time_save)
    a_end=date_data(:,max_time_save)
    call date_to_datestr(a_start,uemep_date_format_str,uemep_start_date_str)
    call date_to_datestr(a_end,uemep_date_format_str,uemep_end_date_str)

    !Change the date stamp to follow NILU conventions
    !call incrtm(-1,a_start(1),a_start(2),a_start(3),a_start(4))
  
    if (.not.allocated(emis_sum)) then
        allocate (emis_sum(n_x_save))
        emis_sum=0.
    endif

    !Check that path exists after filling in date stamp
    call date_to_datestr_bracket(a_start,path_output_emis,temp_name)
    call date_to_datestr_bracket(a_start,temp_name,temp_name)
    call date_to_datestr_bracket(a_start,temp_name,temp_name)
    
    inquire(directory=trim(temp_name),exist=exists)
    if (.not.exists) then
        write(unit_logfile,'(A)')'ERROR: Path '//trim(temp_name)//' does not exist. No file saved'
        return
    endif

    
    endif
    
    !NORTRIP emissions are g/km/hour. 
    conversion=1.
    !conversion=1.e6/1000./3600 !Convert to ug/s/m for use in EMEP (ug/s)

        if (ro_tot.eq.1) then
            
            !Calculate number of roads that will be saved
            n_roads_total_save=0
            if (use_single_road_loop_flag) then
            do ro=1,n_roads_total
                if (line_or_grid_data_flag(ro).eq.1.or.line_or_grid_data_flag(ro).eq.3) then
                    n_roads_total_save=n_roads_total_save+1
                endif
            enddo  
            else
            do ro=1,n_roads
                if (line_or_grid_data_flag(ro).eq.1.or.line_or_grid_data_flag(ro).eq.3) then
                    n_roads_total_save=n_roads_total_save+1
                endif
            enddo
            endif
            
            !Check the total emission data for PM10 and PM2.5 for NaNs and stops if it finds any before writing
            do ro=n_roads_start,n_roads_end
            if (line_or_grid_data_flag(ro).eq.1.or.line_or_grid_data_flag(ro).eq.3) then
                do x_loop=1,2
                    x=save_size(x_loop)
                    do ti=min_time_save,max_time_save
                        if (isnan(sum(E_road_data(total_dust_index,x,E_total_index,ti,:,ro)))) then
                            write(unit_logfile,'(a)') ' ERROR: NaNs in uEMEP emission output data. Stopping before writing. Check input meteo data.'
                            stop 39
                        endif
                    enddo
                enddo
            endif
            enddo
            
            !Open the outputfile for date
            !temp_name=trim(path_output_emis)//trim(filename_output_emis)//'_'//'all'//'_'//trim(uemep_start_date_str)//'-'//trim(uemep_end_date_str)//'.txt'
            temp_name=trim(path_output_emis)//trim(filename_output_emis)//'_'//trim(file_str)//'_'//trim(uemep_start_date_str)//'.txt'
            
            !Set the finished file name if it is to be used
            if (trim(finished_file_append).ne.'') then
            finished_filename=trim(path_output_emis)//trim(filename_output_emis)//'_'//trim(file_str)//'_'//trim(uemep_start_date_str)//'.'//trim(finished_file_append)
            endif
            
            !Put in date in path and filename if required
            call date_to_datestr_bracket(a_start,temp_name,temp_name)
            call date_to_datestr_bracket(a_start,temp_name,temp_name)
            call date_to_datestr_bracket(a_start,temp_name,temp_name)

            write(unit_logfile,'(a)') ' Filename= '//trim(temp_name)
            
            open(unit_out,file=temp_name,status='replace')

            
            write(unit_out,'(A)') '# NORTRIP emission output for uEMEP'
            write(unit_out,'(A)') '# Hourly traffic non-exhaust and exhaust emission '
            write(unit_out,'(A)') '# Number of compounds '
            write(unit_out,'(i4)') n_x_save
            write(unit_out,'(A)') '# Compound names '
            write(unit_out,'(A,<n_x_save>A)') save_str(1:n_x_save)            
            write(unit_out,'(A)')   '# Emission data unit'
            write(unit_out,'(A)')   '"g/km/hr"'
            write(unit_out,'(A)')   '# Emission data start and end date'
            write(unit_out,'(A)')    trim(uemep_start_date_str)//'   '//trim(uemep_end_date_str)
            write(unit_out,'(A)')   '# Number of roads     Number of hours'
            write(unit_out,'(2i)')   n_roads_total_save,n_time_save      
            write(unit_out,'(A)')   '# RoadLinkID    Emission'
        
            emis_sum=0.
        
        endif
        
        !Note that if line_or_grid_data_flag(ro).eq.0 then this is a link without traffic data.
        !Must be saved but with no information
        do ro=n_roads_start,n_roads_end
            
            !Length of road in km for calculating sum of emissions
            if (length_road(ro).eq.0) then
                length_road_km(ro)=sqrt((x_road(1,ro)-x_road(2,ro))**2+(y_road(1,ro)-y_road(2,ro))**2)/1000.
            else
                length_road_km(ro)=length_road(ro)/1000.
            endif
                            
            if (line_or_grid_data_flag(ro).eq.1.or.line_or_grid_data_flag(ro).eq.-1.or.line_or_grid_data_flag(ro).eq.3) then
                
                if (use_single_road_loop_flag) then 
                    write(unit_out,'(i12)') road_ID(ro_tot)
                else
                    write(unit_out,'(i12)') road_ID(ro)
                endif

                do x_loop=1,n_x_save
   
                    x=save_size(x_loop)
                    s=save_source(x_loop)

                    do ti=min_time_save,max_time_save

                        emis_road=0.
                        !If line_or_grid_data_flag(ro).eq.-1 then missing data and so set to 0
                        if (line_or_grid_data_flag(ro).eq.1.or.line_or_grid_data_flag(ro).eq.3) then
                            if (x.eq.pm_exhaust) then
                                emis_road=sum(E_road_data(exhaust_index,pm_25,E_total_index,ti,:,ro))*conversion
                            elseif (x.eq.nox_exhaust) then
                                emis_road=0.
                                if (available_airquality_data(NOX_emis_index)) then
                                    emis_road=emis_road+airquality_data(NOX_emis_index,ti,ro)*conversion
                                elseif (NOX_EF_available.ne.0) then
                                    do v=1,num_veh
                                        emis_road=emis_road+traffic_data(N_v_index(v),ti,ro)*NOX_EF(v,ro)*conversion
                                    enddo
                                endif                              
                            else
                                emis_road=sum(E_road_data(s,x,E_total_index,ti,:,ro))*conversion
                            endif
                        endif
            
                        !Save emissions when using single roads to get it in the right order
                        !Write them at end of calculation
                        emis_time(ti)=emis_road            
                        emis_sum(x_loop)=emis_sum(x_loop)+emis_road*length_road_km(ro)
                
                    enddo
                
                    !Write the time values
                    write(unit_out,'(<n_time_save>es12.4)') emis_time(min_time_save:max_time_save)
            
                enddo
            
            endif
            
        enddo
    
        if (ro_tot.eq.n_roads_total) then           

            close (unit_out)
    
            !Write the total emissions to the log file
            do x_loop=1,n_x_save
                x=save_size(x_loop)
                s=save_source(x_loop)
                !Conversion is for g to kg
                write(unit_logfile,'(A,a,a,es12.2)') ' Total emissions of ',save_str(x_loop),' (kg) for all road links over this period = ',emis_sum(x_loop)*1.e-3*dt
            enddo
        endif
        
   if (allocated(emis_sum).and.(ro_tot.eq.n_roads_total)) deallocate(emis_sum)
                
 
    end subroutine NORTRIP_save_uemep_emissions_all
!----------------------------------------------------------------------

!----------------------------------------------------------------------
    subroutine NORTRIP_save_uEMEP_grid_emissions
    
    use NORTRIP_definitions
    
    implicit none
    
    integer :: unit_out=40
    character(24) uemep_start_date_str
    character(24) uemep_end_date_str
    character(24) :: uemep_date_format_str='yyyymmddHH'
    character(256) temp_name,filename_temp
    logical exists
    integer a_start(num_date_index),a_end(num_date_index)
    real conversion
    character(8) :: pm_str=''
    
    real x_grid(2),y_grid(2)
    !real f_grid(grid_dim(1),grid_dim(2),n_roads)
    real f_grid(0:n_roads)
    real length_road_km(0:n_roads)
    integer unit_count
    
    real, save :: emis_sum(num_size+2)
    real emis_road
    integer save_size(4)
    integer x_loop,n_time_save
    
    !Declare functions
    real line_fraction_in_grid_func
    
    return
    
    unit_out=unit_save_grid_emissions
    save_size(1)=pm_10;save_size(2)=pm_25;save_size(3)=pm_exhaust;save_size(4)=nox_exhaust
    
    if (ro_tot.eq.1) then

        !write(*,*) grid_dim(1),grid_dim(2),n_roads
        !stop
        if (.not.allocated(emis_grid)) allocate (emis_grid(grid_dim(1),grid_dim(2),num_size+2,n_time))
        emis_grid=0.
    
        !Open log file
        !if (unit_logfile.gt.0) then
        !    open(unit_logfile,file=filename_log,status='old',position='append')
        !endif

	    write(unit_logfile,'(A)') '----------------------------------------------------------------'
	    write(unit_logfile,'(A)') 'Saving uEMEP gridded emissions to file'
	    write(unit_logfile,'(A)') '----------------------------------------------------------------'


        !Set start and end date stamps
        a_start=date_data(:,min_time_save)
        a_end=date_data(:,max_time_save)
        !Chnage the date stamp to follow NILU conventions
        !call incrtm(-1,a_start(1),a_start(2),a_start(3),a_start(4))

        !Check that path exists after filling in date stamp
        call date_to_datestr_bracket(a_start,path_output_emis,temp_name)
        call date_to_datestr_bracket(a_start,temp_name,temp_name)
        call date_to_datestr_bracket(a_start,temp_name,temp_name)
    
        inquire(directory=trim(temp_name),exist=exists)
        if (.not.exists) then
            write(unit_logfile,'(A)')'ERROR: Path '//trim(temp_name)//' does not exist. No file saved'
            return
        endif
    
    endif
    
    !Convert from g/km/hr g/m/s
    !conversion=1./1000./3600.
    conversion=1.
    
    !This error trap indicates if NaN values in the grid but will not fix the problem which will propogate through the init files
    do ro=n_roads_start,n_roads_end
        
        !Length of road in km
        if (length_road(ro).eq.0) then
            length_road_km(ro)=sqrt((x_road(1,ro)-x_road(2,ro))**2+(y_road(1,ro)-y_road(2,ro))**2)/1000.
        else
            length_road_km(ro)=length_road(ro)/1000.
        endif
        
        !Check data for NaN
        do ti=min_time_save,max_time_save
            do tr=1,num_track
                if (isnan(E_road_data(total_dust_index,pm_10,E_total_index,ti,tr,ro)).or.isnan(E_road_data(total_dust_index,pm_25,E_total_index,ti,tr,ro))) then
                    !E_road_data(total_dust_index,x,E_total_index,ti,tr,ro))=0.
                    write(unit_logfile,'(A,2i12,A,i12,A,A)')'WARNING: NaN found for road link index/ID ',ro,road_ID(ro),' at time index ',ti,' and time ',trim(date_str(3,ti))
                endif
                if (E_road_data(total_dust_index,pm_10,E_total_index,ti,tr,ro).lt.0.or.E_road_data(total_dust_index,pm_25,E_total_index,ti,tr,ro).lt.0) then
                    !E_road_data(total_dust_index,x,E_total_index,ti,tr,ro))=0.
                    write(unit_logfile,'(A,2i12,A,i12,A,A)')'WARNING: Negative emission found for road link index/ID ',ro,road_ID(ro),' at time index ',ti,' and time ',trim(date_str(3,ti))
                endif
            enddo 
        enddo
    
    enddo
    
    !Calculate the emissions in each grid
    !write(*,*) grid_dim,grid_0,grid_delta
   
            
    do j=1,grid_dim(2)
    do i=1,grid_dim(1)
                    
        x_grid(1)=grid_0(1)+grid_delta(1)*(i-1)
        x_grid(2)=grid_0(1)+grid_delta(1)*i
        y_grid(1)=grid_0(2)+grid_delta(2)*(j-1)
        y_grid(2)=grid_0(2)+grid_delta(2)*j
                                
        do ro=n_roads_start,n_roads_end                   
            !if (adt_road(ro).ge.grid_adt_cutoff(1).and.adt_road(ro).lt.grid_adt_cutoff(2)) then          
            if (line_or_grid_data_flag(ro).eq.2.or.line_or_grid_data_flag(ro).eq.3) then
                f_grid(ro)=line_fraction_in_grid_func(x_grid,y_grid,x_road(:,ro),y_road(:,ro))
                !if (f_grid(ro).ne.0) write(*,*) ro,f_grid(ro)
            else
                f_grid(ro)=0.
            endif 
            
        enddo
                   
        do x_loop=1,4   
            x=save_size(x_loop)
            do ti=min_time_save,max_time_save             
                do ro=n_roads_start,n_roads_end
                    if (x.eq.pm_exhaust) then
                        emis_road=sum(E_road_data(exhaust_index,pm_25,E_total_index,ti,:,ro))
                    elseif (x.eq.nox_exhaust) then
                        emis_road=0.
                        if (available_airquality_data(NOX_emis_index)) then
                            emis_road=emis_road+airquality_data(NOX_emis_index,ti,ro)*conversion
                        elseif (NOX_EF_available.ne.0) then
                            do v=1,num_veh
                                emis_road=emis_road+traffic_data(N_v_index(v),ti,ro)*NOX_EF(v,ro)*conversion
                            enddo
                        else
                            emis_road=0.
                        endif                              
                    else
                        emis_road=sum(E_road_data(total_dust_index,x,E_total_index,ti,:,ro))
                    endif                    
                    emis_grid(i,j,x,ti)=emis_grid(i,j,x,ti)+length_road_km(ro)*f_grid(ro)*emis_road*conversion
                enddo                                   
                !emis_grid(i,j,x,ti)=sum(length_road(:)*f_grid(:)*sum(E_road_data(total_dust_index,x,E_total_index,ti,:,:),1))                  
                !write(*,*) length_road(ro),f_grid(ro),E_road_data(total_dust_index,x,E_total_index,ti,tr,ro),conversion
            enddo
        enddo

    enddo
    enddo

    !Save if the last road
    if (ro_tot.eq.n_roads_total) then 
    
    unit_count=0
    
    !Set date text in file
    n_time_save=max_time_save-min_time_save+1
    call date_to_datestr(a_start,uemep_date_format_str,uemep_start_date_str)
    call date_to_datestr(a_end,uemep_date_format_str,uemep_end_date_str)

    
    do x_loop=1,4   
        
        x=save_size(x_loop)
        unit_count=x_loop
        unit_out=unit_save_grid_emissions+unit_count
      
        if (x.eq.pm_25) pm_str='PM25'
        if (x.eq.pm_10) pm_str='PM10'
        if (x.eq.pm_exhaust) pm_str='EP'
        if (x.eq.nox_exhaust) pm_str='NOX'

        
            !Open the outputfile for date
            temp_name=trim(path_output_emis)//trim(filename_output_grid_emis)//'_'//trim(pm_str)//'_'//trim(uemep_start_date_str)//'-'//trim(uemep_end_date_str)//'.txt'
        
            !Put in date in path and filename if required
            call date_to_datestr_bracket(a_start,temp_name,temp_name)
            call date_to_datestr_bracket(a_start,temp_name,temp_name)
            call date_to_datestr_bracket(a_start,temp_name,temp_name)

            write(unit_logfile,'(a)') ' Filename= '//trim(temp_name)

            open(unit_out,file=temp_name,status='replace')
    
            write(unit_out,'(A)') '# NORTRIP grid emission output for uEMEP. Ordered visually with max y position at top'
            if (x.eq.pm_25.or.x.eq.pm_10) then
            if (available_airquality_data(EP_emis_index).or.exhaust_EF_available.ne.0) then
                write(unit_out,'(A,A)') '# Hourly traffic non-exhaust and exhaust emission for compound: ',trim(pm_str)
            else
                write(unit_out,'(A,A)') '# Hourly traffic non-exhaust only emission for compound: ',trim(pm_str)            
            endif
            endif
            
            if (x.eq.pm_exhaust) then
            if (available_airquality_data(EP_emis_index).or.exhaust_EF_available.ne.0) then
                write(unit_out,'(A,A)') '# Hourly traffic exhaust emission for compound: ',trim(pm_str)
            else
                write(unit_out,'(A)') '# No exhaust emission data available'          
            endif
            endif
            
            if (x.eq.nox_exhaust) then
            if (available_airquality_data(NOX_emis_index).or.NOX_EF_available.ne.0) then
                write(unit_out,'(A,A)') '# Hourly traffic exhaust emission for compound: ',trim(pm_str)
            else
                write(unit_out,'(A)') '# No exhaust emission data available'          
            endif
            endif
            
            write(unit_out,'(A)')   '# Emission data unit'
            write(unit_out,'(A)')   'g/hr'
            write(unit_out,'(A)')   '# Emission data start and end date'
            write(unit_out,'(A)')    trim(uemep_start_date_str)//'   '//trim(uemep_end_date_str)
            write(unit_out,'(A)')   '# Number of grids (i,j)     Number of hours'
            write(unit_out,'(3i)')   grid_dim(1),grid_dim(2),n_time_save      
            write(unit_out,'(A)')   '# Lower left center (x,y) grid position (m)'
            write(unit_out,'(2f12.2)')   grid_0(1),grid_0(2)     
            write(unit_out,'(A)')   '# Grid spacing (x,y) (m)'
            write(unit_out,'(2f12.2)')   grid_delta(1),grid_delta(2)   
            
            emis_sum(x)=0.
                    
        
        !Save the emission data in time
        do ti=min_time_save,max_time_save
        
            write(unit_out,'(i8)') ti
            do j=grid_dim(2),1,-1
                write(unit_out,'(<grid_dim(1)>es12.3)') (emis_grid(i,j,x,ti),i=1,grid_dim(1))
                do i=1,grid_dim(1)
                    emis_sum(x)=emis_sum(x)+emis_grid(i,j,x,ti)
                enddo
            enddo

        enddo
        
        close (unit_out)
    
        write(unit_logfile,'(A,a,a,es12.2)') ' Total emissions of ',trim(pm_str),' (kg) for all grids over this period = ',emis_sum(x)*1.e-3*dt
 
        
    enddo
    
    endif
   
    end subroutine NORTRIP_save_uEMEP_grid_emissions
!----------------------------------------------------------------------
