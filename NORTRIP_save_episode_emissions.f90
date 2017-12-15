!NORTRIP_save_episode_emissions.f90
    
!----------------------------------------------------------------------
    subroutine NORTRIP_save_episode_emissions
    
    use NORTRIP_definitions
    
    implicit none
    
    integer :: unit_out
    character(24) epi_start_date_str
    character(24) epi_end_date_str
    character(24) :: epi_date_format_str='yyyy/mm/dd HH:MM'
    character(256) temp_name,filename_temp
    logical exists
    integer a_start(num_date_index),a_end(num_date_index)
    real conversion
    character(8) :: pm_str=''
    real emis_road
    real length_road(0:n_roads)
    real, save :: emis_sum(num_size)
    real, save, allocatable :: emis_epi(:,:,:)
    integer unit_count
    
    unit_out=unit_save_episode_emissions
    
    !Declare functions
    
    !Open log file
    !if (unit_logfile.gt.0) then
    !    open(unit_logfile,file=filename_log,status='old',position='append')
    !endif
    if (ro_tot.eq.1) then
        
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	write(unit_logfile,'(A)') 'Saving EPISODE emissions to file'
	write(unit_logfile,'(A)') '----------------------------------------------------------------'

    if (.not.allocated(emis_epi).and.use_single_road_loop_flag) then
        allocate (emis_epi(num_size,n_time,n_roads_total))
        emis_epi=0.
    endif
    

    !Set start and end date stamps
    a_start=date_data(:,min_time_save)
    a_end=date_data(:,max_time_save)
    !Chnage the date stamp to follow NILU conventions
    call incrtm(-1,a_start(1),a_start(2),a_start(3),a_start(4))

    !Check that path exists after filling in date stamp
    call date_to_datestr(a_start,path_output_emis,temp_name)
    
    inquire(directory=trim(temp_name),exist=exists)
    if (.not.exists) then
        write(unit_logfile,'(A)')'ERROR: Path '//trim(temp_name)//' does not exist. No file saved'
        return
    endif

    !Set date text in file
    call date_to_datestr(a_start,epi_date_format_str,epi_start_date_str)
    call date_to_datestr(a_end,epi_date_format_str,epi_end_date_str)
    
    endif
    
    conversion=1./1000./3600.

    !Remove this after reading in properly
    !filename_temp='LsrcEmissionVariableData'
    unit_count=0
    
    do x=pm_10,pm_25
   
        unit_count=unit_count+1
        unit_out=unit_save_episode_emissions+unit_count

        if (x.eq.pm_25) pm_str='PM2.5'
        if (x.eq.pm_10) pm_str='PM10'

        if (ro_tot.eq.1) then
            
        
            !Open the outputfile for date
            temp_name=trim(path_output_emis)//trim(filename_output_emis)//'_'//trim(pm_str)//'.txt'
        
            !Put in date in path and filename if required
            call date_to_datestr(a_start,temp_name,temp_name)

            write(unit_logfile,'(a)') ' Filename= '//trim(temp_name)

            open(unit_out,file=temp_name,status='replace')
    
            !Check ISLEN that it works
            write(unit_out,'(A)')   trim(BB_output_ID(x))
            if (available_airquality_data(EP_emis_index).or.exhaust_EF_available.ne.0) then
                write(unit_out,'(A,A)') '* Hourly traffic non-exhaust and exhaust emission for compound: ',pm_str
            else
                write(unit_out,'(A,A)') '* Hourly traffic non-exhaust only emission for compound: ',pm_str            
            endif       
            write(unit_out,'(A)')   '* Emission data given in unit         :       g/s.m'
            write(unit_out,'(A,A)') '* Emission data Start date and time: ',trim(epi_start_date_str)
            write(unit_out,'(A,A)') '* Emission data End date and time  : ',trim(epi_end_date_str)
            write(unit_out,'(A)')   '* '
            write(unit_out,'(A)')   '* Hour  RoadLinkID    Emission_dir1    Emission_dir2'
        
            emis_sum(x)=0.
        
        endif

    
        !Calculate road lengths for total emission calculations. If no gridding involved then these will be 0
        do ro=n_roads_start,n_roads_end
            length_road(ro)=sqrt((x_road(1,ro)-x_road(2,ro))**2+(y_road(1,ro)-y_road(2,ro))**2)
        enddo
        
        !if (sum(length_road).eq.0) then 
        !    length_road=1 !Unit value
        !endif
        
        !Note that if line_or_grid_data_flag(ro).eq.0 then this is a link without traffic data.
        !Must be saved but with no information
        do ti=min_time_save,max_time_save
            do ro=n_roads_start,n_roads_end
                
            if (line_or_grid_data_flag(ro).le.1) then
                
                if (line_or_grid_data_flag(ro).eq.1) then
                    emis_road=sum(E_road_data(total_dust_index,x,E_total_index,ti,:,ro))*conversion
                else
                    emis_road=0.                   
                endif
            
                !Save emissions when using single roads to get it in the right order
                !Write them at end of calculation
                if (use_single_road_loop_flag) then 
                    emis_epi(x,ti,ro_tot)=emis_road
                else
                    write(unit_out,'(i8,i10,2es16.6)') ti,road_ID(ro),emis_road/2.,emis_road/2.
                endif
                           
                emis_sum(x)=emis_sum(x)+emis_road*length_road(ro)
                
            endif
            
            enddo
        enddo
    
        if (ro_tot.eq.n_roads_total) then

            !Special case for writing data when running single road because of the order data is written for episode
            if (use_single_road_loop_flag) then
                do ti=min_time_save,max_time_save
                do ro=1,n_roads_total
                
                    if (line_or_grid_data_flag(ro).le.1) then
                
                        if (line_or_grid_data_flag(ro).eq.1) then
                            emis_road=emis_epi(x,ti,ro)
                        else
                            emis_road=0.                   
                        endif
                        
                        write(unit_out,'(i8,i10,2es16.6)') ti,road_ID(ro),emis_road/2.,emis_road/2.
                                           
                    endif
            
                enddo
                enddo
                
            endif
                
            

            close (unit_out)
    
            if (sum(length_road).eq.0) then 
                write(unit_logfile,'(A,a,a,es12.2)') ' Average emission rates ',trim(pm_str),' (g/m) for all road links = ',emis_sum(x)/((max_time_save-min_time_save+1)*dt*3600)
            else
                write(unit_logfile,'(A,a,a,es12.2)') ' Total emissions of ',trim(pm_str),' (g) for all road links over this period = ',emis_sum(x)*dt*3600
            endif
        endif
        
 	
   enddo
    
   if (allocated(emis_epi).and.(ro_tot.eq.n_roads_total)) deallocate(emis_epi)
                
 
    end subroutine NORTRIP_save_episode_emissions
!----------------------------------------------------------------------
    
    
!----------------------------------------------------------------------
    subroutine NORTRIP_save_episode_grid_emissions
    
    use NORTRIP_definitions
    
    implicit none
    
    integer :: unit_out=40
    character(24) epi_start_date_str
    character(24) epi_end_date_str
    character(24) :: epi_date_format_str='yyyy/mm/dd HH:MM'
    character(256) temp_name,filename_temp
    logical exists
    integer a_start(num_date_index),a_end(num_date_index)
    real conversion
    character(8) :: pm_str=''
    
    real x_grid(2),y_grid(2)
    !real f_grid(grid_dim(1),grid_dim(2),n_roads)
    real f_grid(0:n_roads)
    real length_road(0:n_roads)
    integer unit_count
    
    real, save :: emis_sum(num_size)
    real emis_road
    
    !Declare functions
    real line_fraction_in_grid_func
    
    unit_out=unit_save_episode_grid_emissions
    
    if (ro_tot.eq.1) then

    !write(*,*) grid_dim(1),grid_dim(2),n_roads
    !stop
    if (.not.allocated(emis_grid)) allocate (emis_grid(grid_dim(1),grid_dim(2),num_size,n_time))
    emis_grid=0.
    
    !Open log file
    !if (unit_logfile.gt.0) then
    !    open(unit_logfile,file=filename_log,status='old',position='append')
    !endif

	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	write(unit_logfile,'(A)') 'Saving EPISODE gridded emissions to file'
	write(unit_logfile,'(A)') '----------------------------------------------------------------'


    !Set start and end date stamps
    a_start=date_data(:,min_time_save)
    a_end=date_data(:,max_time_save)
    !Chnage the date stamp to follow NILU conventions
    call incrtm(-1,a_start(1),a_start(2),a_start(3),a_start(4))

    !Check that path exists after filling in date stamp
    call date_to_datestr(a_start,path_output_emis,temp_name)
    
    inquire(directory=trim(temp_name),exist=exists)
    if (.not.exists) then
        write(unit_logfile,'(A)')'ERROR: Path '//trim(temp_name)//' does not exist. No file saved'
        return
    endif

    !Set date text in file
    call date_to_datestr(a_start,epi_date_format_str,epi_start_date_str)
    call date_to_datestr(a_end,epi_date_format_str,epi_end_date_str)
    
    endif
    
    
    !This error trap indicates if NaN values in the grid but will not fix the problem which will propogate through the init files
    do ro=n_roads_start,n_roads_end
        
        length_road(ro)=sqrt((x_road(1,ro)-x_road(2,ro))**2+(y_road(1,ro)-y_road(2,ro))**2)
        
        !Check data
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
            if (line_or_grid_data_flag(ro).eq.2) then
                f_grid(ro)=line_fraction_in_grid_func(x_grid,y_grid,x_road(:,ro),y_road(:,ro))
                !if (f_grid(ro).ne.0) write(*,*) f_grid(ro)
            else
                f_grid(ro)=0.
            endif 
            
        enddo
                   
        do x=pm_10,pm_25
            do ti=min_time_save,max_time_save
                !emis_grid(i,j,x,ti)=0.
                do ro=n_roads_start,n_roads_end
                    emis_grid(i,j,x,ti)=emis_grid(i,j,x,ti)+length_road(ro)*f_grid(ro)*sum(E_road_data(total_dust_index,x,E_total_index,ti,:,ro)) 
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
    
    do x=pm_10,pm_25
   
        unit_count=unit_count+1
        unit_out=unit_save_episode_grid_emissions+unit_count
      
        if (x.eq.pm_25) pm_str='PM2.5'
        if (x.eq.pm_10) pm_str='PM10'

        !Open the outputfile for date
        temp_name=trim(path_output_emis)//trim(filename_output_grid_emis)//'_'//trim(pm_str)//'.txt'
        
        !Put in date in path and filename if required
        call date_to_datestr(a_start,temp_name,temp_name)

        write(unit_logfile,'(a)') ' Filename= '//trim(temp_name)

        open(unit_out,file=temp_name,status='replace')
    
        !Check ISLEN that it works
        write(unit_out,'(A)')  trim(BB_output_ID(x))
        if (available_airquality_data(EP_emis_index).or.exhaust_EF_available.ne.0) then
            write(unit_out,'(A,A)') '* Hourly traffic area non-exhaust and exhaust emission for compound: ',pm_str
        else
            write(unit_out,'(A,A)') '* Hourly traffic area non-exhaust only emission for compound: ',pm_str            
        endif       
        write(unit_out,'(A)')   '* Emission data given in unit         :       g/s'
        write(unit_out,'(A,A)') '* Emission data Start date and time: ',trim(epi_start_date_str)
        write(unit_out,'(A,A)') '* Emission data End date and time  : ',trim(epi_end_date_str)
        write(unit_out,'(A)')   '* '
        
        !Convert from g/km/hr g/m/s
        conversion=1./1000./3600.

        emis_sum=0.
        
        
        !Save the emission data in time
        do ti=min_time_save,max_time_save
            write(unit_out,'(A6,I4,A10,2I3)')   'Time: ',ti,' Layer: 01',grid_dim(1),grid_dim(2)
            
            do j=1,grid_dim(2)
            do i=1,grid_dim(1)
 
                emis_road=emis_grid(i,j,x,ti)*conversion 
                write(unit_out,'(2i6,es16.6)'),i,j,emis_road
                emis_sum(x)=emis_sum(x)+emis_road

            enddo
            enddo

            write(unit_out,'(A)') ''

        enddo
        
        close (unit_out)
    
        write(unit_logfile,'(A,a,a,es12.2)') ' Total emissions of ',trim(pm_str),' (g) for all grids over this period = ',emis_sum(x)*dt*3600
 
        
    enddo
    
    endif
   
    end subroutine NORTRIP_save_episode_grid_emissions
!----------------------------------------------------------------------
