!----------------------------------------------------------------------
subroutine read_NORTRIP_inputdata

    use NORTRIP_definitions
    
    implicit none
    
    character(256) temp_path
    character(256) temp_file
    character(256) temp_name,temp_name_zip
    character(256) search_str
    character(2048) temp_str,temp_str1,temp_str2
    character(64) header_str(30),match_str
    integer unit_in,unit_logfile_temp
    integer index_val
    integer i_head
    integer ii,jj
    integer index_match
    logical exists
    real, allocatable :: input_array(:,:,:)
    integer input_file_type
    integer meteo_file_type,traffic_file_type,airquality_file_type,activity_file_type,date_file_type
    integer n_file_type
    parameter (date_file_type=1,traffic_file_type=2,meteo_file_type=3,activity_file_type=4,airquality_file_type=5)
    parameter (n_file_type=5)
    character(64), allocatable :: file_match_str(:)
    integer n_index,i_road
    logical match_found
    real, allocatable :: M_road_init_temp(:,:)
    real temp_val
    integer n_date  !Number of rows for the date data index_val
    logical file_available(n_file_type)
    integer dummy_int !For reading in files to determine lengths

    !Set which road to be read.
    !Muliple road files will require multiple 'filename_inputdata' names
    i_road=1

    unit_logfile_temp=unit_logfile !use -1 not to write anything, 0 to go to screen
   
    !Open log file
    !if (unit_logfile.gt.0) then
    !    open(unit_logfile,file=filename_log,status='old',position='append')
    !endif

    !read_metadata_in_zip_format=.true.
    !read_timeseriesdata_in_zip_format=.true.
    !read_initialdata_in_zip_format=.true.
    !save_initdata_in_zip_format=.true.
    
    !Set the general path name
    temp_path=trim(path_inputdata)
    unit_in=unit_read_NORTRIP_inputdata

    write(unit_logfile_temp,'(A)') '================================================================'

    !Read from the metadata file
    if (read_metadata_in_zip_format) then
        temp_file=trim(filename_inputdata)//'_metadata.zip'
        temp_name=trim(temp_path)//trim(temp_file)
        inquire(file=trim(temp_name),exist=exists)
        if (.not.exists) then
            write(unit_logfile,'(a)')'ERROR: File '//trim(temp_name)//' does not exist.'
            write(unit_logfile,'(a)')'STOPPING'
            stop 31
        endif
        !Unzip
        write(unit_logfile,'(a,a)') 'Extracting from zip format: ',trim(temp_name)       
        command_line_zip='7za e -tzip '//trim(temp_name)//' -o'//trim(temp_path)
        write(unit_logfile,'(a,a)') 'Command line zip: ',trim(command_line_zip)      
        CALL EXECUTE_COMMAND_LINE (trim(command_line_zip),wait=.true.)
    endif

    !Read from the metadata file
    temp_file=trim(filename_inputdata)//'_metadata.txt'
    write(unit_logfile_temp,'(A)') 'Reading road metadata file (read_NORTRIP_inputdata)' 
 	write(unit_logfile_temp,'(A)') '================================================================'

    !Open pathname file for reading
    temp_name=trim(temp_path)//trim(temp_file)
    inquire(file=trim(temp_name),exist=exists)
    if (.not.exists) then
        write(unit_logfile,'(a)')'ERROR: File '//trim(temp_name)//' does not exist.'
        write(unit_logfile,'(a)')'STOPPING'
        stop 32
    endif
    
    open(unit_in,file=temp_name,access='sequential',status='old',readonly)  
    write(unit_logfile,'(a)') ' Filename= '//trim(temp_name)
   
    !Read number of roads
    call find_read_line_int1(unit_in,unit_logfile_temp,n_roads,'Number of roads',1)
    !Read nodata value
    call find_read_line_val1(unit_in,unit_logfile_temp,nodata_input,'Missing data',nodata)
    !nodata=nodata_input
    !Hours between saving init files
    call find_read_line_int1(unit_in,unit_logfile_temp,hours_between_init,'Hours between saving init files',0)
    !Calculation type
    call find_read_line_str1(unit_in,unit_logfile_temp,calculation_type,'Calculation type','Normal')

    if (trim(calculation_type).eq.'Bedre byluft') then
        call find_read_line_str1(unit_in,unit_logfile_temp,BB_output_ID(pm_10),'Model output ID PM10','{}')
        call find_read_line_str1(unit_in,unit_logfile_temp,BB_output_ID(pm_25),'Model output ID PM2.5','{}')
    endif
    
    !Gridding data
    call find_read_line_val1(unit_in,unit_logfile_temp,grid_0(1),'Grid x lower left corner',nodata)
    call find_read_line_val1(unit_in,unit_logfile_temp,grid_0(2),'Grid y lower left corner',nodata)
    call find_read_line_val1(unit_in,unit_logfile_temp,grid_delta(1),'Grid x spacing',nodata)
    call find_read_line_val1(unit_in,unit_logfile_temp,grid_delta(2),'Grid y spacing',nodata)
    !call find_read_line_val1(unit_in,unit_logfile_temp,grid_adt_cutoff(1),'Grid ADT lower cutoff',0.)
    !call find_read_line_val1(unit_in,unit_logfile_temp,grid_adt_cutoff(2),'Grid ADT upper cutoff',0.)
    call find_read_line_int1(unit_in,unit_logfile_temp,grid_dim(1),'Grid x dimensions',0)
    call find_read_line_int1(unit_in,unit_logfile_temp,grid_dim(2),'Grid y dimensions',0)
    
    !if (grid_adt_cutoff(2).gt.0.and.grid_dim(1).gt.0.and.grid_dim(2).gt.0) then
    !    grid_road_data_flag=.true.
    !else
    !    grid_road_data_flag=.false.
    !endif
    

    !Allocate and initialise arrays for road metadata
    if (.not.allocated(d_index)) allocate (d_index(0:n_roads))
    if (.not.allocated(p_index)) allocate (p_index(0:n_roads))
    if (.not.allocated(b_road)) allocate (b_road(0:n_roads))
    if (.not.allocated(n_lanes)) allocate (n_lanes(0:n_roads))
    if (.not.allocated(b_road_lanes)) allocate (b_road_lanes(0:n_roads))
    if (.not.allocated(b_lane)) allocate (b_lane(0:n_roads))
    if (.not.allocated(b_canyon)) allocate (b_canyon(0:n_roads))
    if (.not.allocated(h_canyon)) allocate (h_canyon(2,0:n_roads)) !Two sides, north and south
    if (.not.allocated(ang_road)) allocate (ang_road(0:n_roads))
    if (.not.allocated(slope_road)) allocate (slope_road(0:n_roads))
    if (.not.allocated(roadtype_index)) allocate (roadtype_index(0:n_roads))

    if (.not.allocated(LAT)) allocate (LAT(0:n_roads))
    if (.not.allocated(LON)) allocate (LON(0:n_roads))
    if (.not.allocated(Z_SURF)) allocate (Z_SURF(0:n_roads))
    if (.not.allocated(z_FF)) allocate (z_FF(0:n_roads))
    if (.not.allocated(z_T)) allocate (z_T(0:n_roads))
    if (.not.allocated(z2_T)) allocate (z2_T(0:n_roads))
    if (.not.allocated(albedo_road)) allocate (albedo_road(0:n_roads))
    if (.not.allocated(DIFUTC_H)) allocate (DIFUTC_H(0:n_roads))
    if (.not.allocated(Pressure)) allocate (Pressure(0:n_roads))

    !Correction factors
    if (.not.allocated(wind_speed_correction)) allocate (wind_speed_correction(0:n_roads))
    if (.not.allocated(h_sus)) allocate (h_sus(0:n_roads))
    if (.not.allocated(h_texture)) allocate (h_texture(0:n_roads))
    
    !OSPM factors
    if (.not.allocated(choose_receptor_ospm)) allocate (choose_receptor_ospm(0:n_roads))
    if (.not.allocated(SL1_ospm)) allocate (SL1_ospm(0:n_roads))
    if (.not.allocated(SL2_ospm)) allocate (SL2_ospm(0:n_roads))
    if (.not.allocated(f_roof_ospm)) allocate (f_roof_ospm(0:n_roads))
    if (.not.allocated(RecHeight_ospm)) allocate (RecHeight_ospm(0:n_roads))
    if (.not.allocated(f_turb_ospm)) allocate (f_turb_ospm(0:n_roads))
   
    !Single factors
    if (.not.allocated(observed_moisture_cutoff_value)) allocate (observed_moisture_cutoff_value(0:n_roads))
    if (.not.allocated(road_ID)) allocate (road_ID(0:n_roads))
    if (.not.allocated(save_road_data_flag)) allocate (save_road_data_flag(0:n_roads))

    !Emisison factors (num_veh,n_road)
    if (.not.allocated(exhaust_EF)) allocate (exhaust_EF(num_veh,0:n_roads))
    if (.not.allocated(NOX_EF)) allocate (NOX_EF(num_veh,0:n_roads))
    
    !Grid data
    if (.not.allocated(x_road)) allocate (x_road(2,0:n_roads))
    if (.not.allocated(y_road)) allocate (y_road(2,0:n_roads))
    if (.not.allocated(length_road)) allocate (length_road(0:n_roads))
    if (.not.allocated(line_or_grid_data_flag)) allocate (line_or_grid_data_flag(0:n_roads))
    !if (.not.allocated(adt_road)) allocate (adt_road(0:n_roads))
    
    !Road type actvity factors
    if (.not.allocated(road_type_activity_flag)) allocate (road_type_activity_flag(num_road_type_activity,0:n_roads))

    !Skyview
    call find_read_line_int1(unit_in,unit_logfile_temp,n_skyview,'Number of skyview angles',0)
    if (n_skyview.gt.0) then
        !write(*,*) 'allocating az_skyview'
        allocate (az_skyview(n_skyview,0:n_roads))
        allocate (zen_skyview(n_skyview,0:n_roads))
        do i=1,n_skyview
            az_skyview(i,:)=360/n_skyview*(i-1)
            !write(*,*) i,floor(az_skyview(i,1))
            write(temp_str,'(i3.3)') floor(az_skyview(i,1))
            !write(*,*) i,'az_skyview:'//trim(temp_str)
            call find_read_line_valn(unit_in,unit_logfile_temp,zen_skyview(i,1:n_roads),n_roads,'az_skyview:'//trim(temp_str),0.)
            !write(*,*) i,zen_skyview(i,:)
        enddo
    endif
      
    call find_read_line_intn(unit_in,unit_logfile_temp,Road_ID(1:n_roads),n_roads,'Road ID',0)
    call find_read_line_valn(unit_in,unit_logfile_temp,b_road(1:n_roads),n_roads,'Road width',8.0)
    call find_read_line_intn(unit_in,unit_logfile_temp,d_index(1:n_roads),n_roads,'Driving cycle',1)
    call find_read_line_intn(unit_in,unit_logfile_temp,p_index(1:n_roads),n_roads,'Pavement type',1)
    call find_read_line_intn(unit_in,unit_logfile_temp,roadtype_index(1:n_roads),n_roads,'Road type',normal_roadtype)

    call find_read_line_intn(unit_in,unit_logfile_temp,n_lanes(1:n_roads),n_roads,'Number of lanes',2)
    call find_read_line_valn(unit_in,unit_logfile_temp,b_lane(1:n_roads),n_roads,'Width of lane',3.0)
    call find_read_line_valn(unit_in,unit_logfile_temp,b_canyon(1:n_roads),n_roads,'Street canyon width',100.)
    call find_read_line_valn(unit_in,unit_logfile_temp,h_canyon(1,1:n_roads),n_roads,'Street canyon height',0.0)
    call find_read_line_valn(unit_in,unit_logfile_temp,h_canyon(1,1:n_roads),n_roads,'Street canyon height north',h_canyon(1,i_road))
    call find_read_line_valn(unit_in,unit_logfile_temp,h_canyon(2,1:n_roads),n_roads,'Street canyon height south',h_canyon(1,i_road))
    call find_read_line_valn(unit_in,unit_logfile_temp,ang_road(1:n_roads),n_roads,'Street orientation',0.0)
    call find_read_line_valn(unit_in,unit_logfile_temp,slope_road(1:n_roads),n_roads,'Street slope',0.0)
    call find_read_line_valn(unit_in,unit_logfile_temp,LAT(1:n_roads),n_roads,'Latitude',0.0)
    call find_read_line_valn(unit_in,unit_logfile_temp,LON(1:n_roads),n_roads,'Longitude',0.0)
    call find_read_line_valn(unit_in,unit_logfile_temp,Z_SURF(1:n_roads),n_roads,'Elevation',0.0)
    call find_read_line_valn(unit_in,unit_logfile_temp,z_FF(1:n_roads),n_roads,'Height obs wind',10.0)
    call find_read_line_valn(unit_in,unit_logfile_temp,z_T(1:n_roads),n_roads,'Height obs temperature',2.0)
    call find_read_line_valn(unit_in,unit_logfile_temp,z2_T(1:n_roads),n_roads,'Height obs other temperature',25.0)
    call find_read_line_valn(unit_in,unit_logfile_temp,albedo_road(1:n_roads),n_roads,'Surface albedo',0.2)
    call find_read_line_valn(unit_in,unit_logfile_temp,DIFUTC_H(1:n_roads),n_roads,'Time difference',-1.0)
    call find_read_line_valn(unit_in,unit_logfile_temp,Pressure(1:n_roads),n_roads,'Surface pressure',1000.0)
    call find_read_line_valn(unit_in,unit_logfile_temp,wind_speed_correction(1:n_roads),n_roads,'Wind speed correction',1.0)
    call find_read_line_valn(unit_in,unit_logfile_temp,observed_moisture_cutoff_value(1:n_roads),n_roads,'Observed moisture cut off',1.5)
    call find_read_line_valn(unit_in,unit_logfile_temp,h_sus(1:n_roads),n_roads,'Suspension rate scaling factor',1.0)
    call find_read_line_valn(unit_in,unit_logfile_temp,h_texture(1:n_roads),n_roads,'Surface texture scaling',1.0)

    call find_read_line_intn(unit_in,unit_logfile_temp,choose_receptor_ospm(1:n_roads),n_roads,'Choose receptor position for ospm',3)
    call find_read_line_valn(unit_in,unit_logfile_temp,SL1_ospm(1:n_roads),n_roads,'Street canyon length north for ospm',50.0)
    call find_read_line_valn(unit_in,unit_logfile_temp,SL2_ospm(1:n_roads),n_roads,'Street canyon length south for ospm',50.0)
    call find_read_line_valn(unit_in,unit_logfile_temp,f_roof_ospm(1:n_roads),n_roads,'f_roof factor for ospm',0.8)!0.4, 0.8
    call find_read_line_valn(unit_in,unit_logfile_temp,RecHeight_ospm(1:n_roads),n_roads,'Receptor height for ospm',3.0)!2
    call find_read_line_valn(unit_in,unit_logfile_temp,f_turb_ospm(1:n_roads),n_roads,'f_turb factor for ospm',1.0)!0.6, 1.0
    
    call find_read_line_valn(unit_in,unit_logfile_temp,exhaust_EF(he,1:n_roads),n_roads,'Exhaust EF (he)',0.0)
    call find_read_line_valn(unit_in,unit_logfile_temp,exhaust_EF(li,1:n_roads),n_roads,'Exhaust EF (li)',0.0)
    call find_read_line_valn(unit_in,unit_logfile_temp,NOX_EF(he,1:n_roads),n_roads,'NOX EF (he)',0.0)
    call find_read_line_valn(unit_in,unit_logfile_temp,NOX_EF(li,1:n_roads),n_roads,'NOX EF (li)',0.0)
    
    call find_read_line_intn(unit_in,unit_logfile_temp,save_road_data_flag(1:n_roads),n_roads,'Save road data',1)
    call find_read_line_intn(unit_in,unit_logfile_temp,line_or_grid_data_flag(1:n_roads),n_roads,'Save as line or grid',1)
    
    call find_read_line_valn(unit_in,unit_logfile_temp,x_road(1,1:n_roads),n_roads,'Road position x1',0.0)
    call find_read_line_valn(unit_in,unit_logfile_temp,y_road(1,1:n_roads),n_roads,'Road position y1',0.0)
    call find_read_line_valn(unit_in,unit_logfile_temp,x_road(2,1:n_roads),n_roads,'Road position x2',0.0)
    call find_read_line_valn(unit_in,unit_logfile_temp,y_road(2,1:n_roads),n_roads,'Road position y2',0.0)
    call find_read_line_valn(unit_in,unit_logfile_temp,length_road(1:n_roads),n_roads,'Road length',0.0)
    !call find_read_line_valn(unit_in,unit_logfile_temp,adt_road(1:n_roads),n_roads,'Road ADT',0.0) !Not used for anything in NORTRIP 
   
    call find_read_line_intn(unit_in,unit_logfile_temp,road_type_activity_flag(road_type_salt_index(1),1:n_roads),n_roads,'road_type_salting_flag',1)
    call find_read_line_intn(unit_in,unit_logfile_temp,road_type_activity_flag(road_type_salt_index(2),1:n_roads),n_roads,'road_type_binding_flag',1)
    call find_read_line_intn(unit_in,unit_logfile_temp,road_type_activity_flag(road_type_sanding_index,1:n_roads),n_roads,'road_type_sanding_flag',1)
    call find_read_line_intn(unit_in,unit_logfile_temp,road_type_activity_flag(road_type_cleaning_index,1:n_roads),n_roads,'road_type_cleaning_flag',1)
    call find_read_line_intn(unit_in,unit_logfile_temp,road_type_activity_flag(road_type_ploughing_index,1:n_roads),n_roads,'road_type_ploughing_flag',1)

    
    !Test to see if any gridding is necessary or not
    !Only if gridding is specified and a grid is defined
    if ((maxval(line_or_grid_data_flag).eq.2.or.maxval(line_or_grid_data_flag).eq.3).and.minval(grid_dim).gt.0) then
        grid_road_data_flag=.true.
    else
        grid_road_data_flag=.false.
    endif

    if (sum(exhaust_EF(:,i_road)).eq.0) then
        exhaust_EF_available=0
    else
        exhaust_EF_available=1
    endif

    if (sum(NOX_EF(:,i_road)).eq.0) then
        NOX_EF_available=0
    else
        NOX_EF_available=1
    endif

    call find_read_line_str1(unit_in,unit_logfile_temp,start_date_str,'Start date',start_date_str)
    call find_read_line_str1(unit_in,unit_logfile_temp,start_date_save_str,'Start save date',start_date_save_str)
    call find_read_line_str1(unit_in,unit_logfile_temp,end_date_str,'End date',end_date_str)
    call find_read_line_str1(unit_in,unit_logfile_temp,end_date_save_str,'End save date',end_date_save_str)

    close(unit_in,status='keep')

    !If zip file used then delete the text file that has been extracted
    if (read_metadata_in_zip_format) then
        command_line_zip=trim(delete_file_command)//' '//trim(temp_name)
        write(unit_logfile,'(a,a)') 'Command line zip: ',trim(command_line_zip)      
        CALL EXECUTE_COMMAND_LINE (trim(command_line_zip),wait=.true.)
    endif

    !Set road width based on lane width
    b_road_lanes=n_lanes*b_lane
   
   !Read from the initial conditions file
    write(unit_logfile_temp,'(A)') '================================================================'

    !Unzip if necessary
    if (read_initialdata_in_zip_format) then
        temp_file=trim(filename_inputdata)//'_initial.zip'
        temp_name=trim(temp_path)//trim(temp_file)
        inquire(file=trim(temp_name),exist=exists)
        if (.not.exists) then
            write(unit_logfile,'(a)')'ERROR: File '//trim(temp_name)//' does not exist.'
            write(unit_logfile,'(a)')'STOPPING'
            stop 33
        endif
        !Unzip
        write(unit_logfile,'(a,a)') 'Extracting from zip format: ',trim(temp_name)       
        command_line_zip='7za e -tzip '//trim(temp_name)//' -o'//trim(temp_path)
        write(unit_logfile,'(a,a)') 'Command line zip: ',trim(command_line_zip)      
        CALL EXECUTE_COMMAND_LINE (trim(command_line_zip),wait=.true.)
    endif

    temp_file=trim(filename_inputdata)//'_initial.txt'
    write(unit_logfile_temp,'(A)') 'Reading road initial conditions file (read_NORTRIP_inputdata)' 
  	write(unit_logfile_temp,'(A)') '================================================================'
 
    !Allocate and initialise arrays for the initial conditions
    if (.not.allocated(M_road_init)) then
        allocate (M_road_init(num_source_all,num_size,num_track,0:n_roads))
        M_road_init=0.0
    endif
    if (.not.allocated(M_road_init_temp)) then
        allocate (M_road_init_temp(num_source_all,0:n_roads))
        M_road_init_temp=0.0
    endif

    if (.not.allocated(g_road_init)) then
        allocate (g_road_init(num_moisture,num_track,0:n_roads))
        g_road_init=0.0
    endif
    
    !Open pathname file for reading
    temp_name=trim(temp_path)//trim(temp_file)
    inquire(file=trim(temp_name),exist=exists)
    if (.not.exists) then
        write(unit_logfile,'(A)')'WARNING: File '//trim(temp_name)//' does not exist. Setting initial values to 0 except water = 0.02'
        M_road_init_temp=0.
        g_road_init=0.
        g_road_init(water_index,:,:)=0.02
        P_fugitive=0.0;long_rad_in_offset=0.0;RH_offset=0.0;T_a_offset=0.0
    else
        open(unit_in,file=temp_name,access='sequential',status='old',readonly)  
        write(unit_logfile,'(a)') ' Filename= '//trim(temp_name)
   
        !Read in mass as g/m^2 only
        call find_read_line_valn(unit_in,unit_logfile_temp,M_road_init_temp(road_index,1:n_roads),n_roads,'M2_dust_road',0.0)
        call find_read_line_valn(unit_in,unit_logfile_temp,M_road_init_temp(sand_index,1:n_roads),n_roads,'M2_sand_road',0.0)
        call find_read_line_valn(unit_in,unit_logfile_temp,M_road_init_temp(salt_index(1),1:n_roads),n_roads,'M2_salt_road(na)',0.0)
        
        call find_read_line_valn(unit_in,unit_logfile_temp,M_road_init_temp(salt_index(2),1:n_roads),n_roads,'M2_salt_road(mg)',0.0)
        if (sum(M_road_init_temp(salt_index(2),:)).eq.0.) then 
            call find_read_line_valn(unit_in,unit_logfile_temp,M_road_init_temp(salt_index(2),1:n_roads),n_roads,'M2_salt_road(cma)',0.0)
            if (sum(M_road_init_temp(salt_index(2),:)).eq.0.) then 
                call find_read_line_valn(unit_in,unit_logfile_temp,M_road_init_temp(salt_index(2),1:n_roads),n_roads,'M2_salt_road(ca)',0.0)
            endif
        endif
        
        call find_read_line_valn(unit_in,unit_logfile_temp,g_road_init(water_index,1,1:n_roads),n_roads,'water_road',0.0)
        call find_read_line_valn(unit_in,unit_logfile_temp,g_road_init(snow_index,1,1:n_roads),n_roads,'snow_road',0.0)  
        call find_read_line_valn(unit_in,unit_logfile_temp,g_road_init(ice_index,1,1:n_roads),n_roads,'ice_road',0.0)

        !Single values only
        call find_read_line_val1(unit_in,unit_logfile_temp,long_rad_in_offset,'long_rad_in_offset',0.0)
        call find_read_line_val1(unit_in,unit_logfile_temp,RH_offset,'RH_offset',0.0)
        call find_read_line_val1(unit_in,unit_logfile_temp,T_a_offset,'T_2m_offset',0.0)

        close(unit_in,status='keep')

        !If zip file used then delete the text file that has been extracted
        if (read_metadata_in_zip_format) then
            command_line_zip=trim(delete_file_command)//' '//trim(temp_name)
            write(unit_logfile,'(a,a)') 'Command line zip: ',trim(command_line_zip)      
            CALL EXECUTE_COMMAND_LINE (trim(command_line_zip),wait=.true.)
        endif

    endif

    !Distribute the initial suspendable mass according to road wear over all tracks and convert from g/m^2 to g/km
    do s=1,num_source
    do tr=1,num_track
    !do ro=1,n_roads
        M_road_init(s,pm_all,tr,:)=M_road_init_temp(s,:)*b_road_lanes(:)*1000
        M_road_init(s,pm_200,tr,:)=M_road_init_temp(s,:)*f_PM(s,pm_200,st)*f_track(tr)*b_road_lanes(:)*1000
        M_road_init(s,pm_10,tr,:)=M_road_init_temp(s,:)*f_PM(s,pm_10,st)*f_track(tr)*b_road_lanes(:)*1000
        M_road_init(s,pm_25,tr,:)=M_road_init_temp(s,:)*f_PM(s,pm_25,st)*f_track(tr)*b_road_lanes(:)*1000
    !enddo
    enddo
    enddo
     
    do ro=1,n_roads
    do m=1,num_moisture
        g_road_init(m,1:num_track,ro)=g_road_init(m,1,ro)
    enddo
    enddo

    if (allocated(M_road_init_temp)) deallocate (M_road_init_temp)    
    
    !Loop through the different input files
    do input_file_type=1,n_file_type
 
 	write(unit_logfile_temp,'(A)') '================================================================'
    if (input_file_type.eq.date_file_type) then
        temp_file=trim(filename_inputdata)//'_traffic'
        write(unit_logfile_temp,'(A)') 'Reading model traffic date input data (read_NORTRIP_inputdata)' 
    endif
    if (input_file_type.eq.traffic_file_type) then
        temp_file=trim(filename_inputdata)//'_traffic'
        write(unit_logfile_temp,'(A)') 'Reading model traffic input data (read_NORTRIP_inputdata)' 
    endif
    if (input_file_type.eq.meteo_file_type) then
        temp_file=trim(filename_inputdata)//'_meteorology'
        write(unit_logfile_temp,'(A)') 'Reading model meteorological input data (read_NORTRIP_inputdata)'   
    endif
    if (input_file_type.eq.activity_file_type) then
        temp_file=trim(filename_inputdata)//'_activity'
        write(unit_logfile_temp,'(A)') 'Reading model activity input data (read_NORTRIP_inputdata)'   
    endif
    if (input_file_type.eq.airquality_file_type) then
        temp_file=trim(filename_inputdata)//'_airquality'
        write(unit_logfile_temp,'(A)') 'Reading model airquality input data (read_NORTRIP_inputdata)'   
    endif
	write(unit_logfile_temp,'(A)') '================================================================'
   
    !Extract the timeseries zip files
    if (read_timeseriesdata_in_zip_format) then
        temp_name_zip=trim(temp_path)//trim(temp_file)//'.zip'
        inquire(file=trim(temp_name_zip),exist=exists)
        if (.not.exists.and.input_file_type.ne.activity_file_type) then
            write(unit_logfile,'(a)')'ERROR: File '//trim(temp_name_zip)//' does not exist.'
            write(unit_logfile,'(a)')'STOPPING'
            stop 34
        endif
        !Unzip
        write(unit_logfile,'(a,a)') 'Extracting from zip format: ',trim(temp_name_zip)       
        command_line_zip='7za e -tzip '//trim(temp_name_zip)//' -o'//trim(temp_path)
        write(unit_logfile,'(a,a)') 'Command line zip: ',trim(command_line_zip)      
        CALL EXECUTE_COMMAND_LINE (trim(command_line_zip),wait=.true.)
    endif
    
    !Open pathname file for reading
    temp_name=trim(temp_path)//trim(temp_file)//'.txt'
    inquire(file=trim(temp_name),exist=exists)
    if (.not.exists) then
        write(unit_logfile,'(A)')'WARNING: File '//trim(temp_name)//' does not exist.'
        file_available(input_file_type)=.false.
    else
        file_available(input_file_type)=.true.
    endif
    
    if (file_available(input_file_type)) then
        
    open(unit_in,file=temp_name,access='sequential',status='old',readonly)  
    write(unit_logfile,'(a)') ' Filename= '//trim(temp_name)

    !Read header string and split at tabs
    temp_str1=''
    temp_str2='Not available'
    index_val=1
    i_head=0
    read(unit_in,'(a)',end=10) temp_str !Read the header string
    do while (index_val.ne.0)
        index_val=index(temp_str,achar(9))
        temp_str1=temp_str(1:index_val-1)
        i_head=i_head+1
        header_str(i_head)=temp_str1
        temp_str=temp_str(index_val+1:)
        if (index_val.eq.0) then !end of the header
            header_str(i_head)=temp_str
        endif           
        !write(*,*) i_head,index_val,trim(header_str(i_head))
    end do
    write(unit_logfile,*) 'Number of columns= ',i_head
  
    !Find out how long the file is, reading a dummy variable
    !Does this only for the date file and assumes the rest are the same, as they should be
    !Except for the activity data. This will not work with non-chronological activity data
    if (input_file_type.eq.date_file_type) then
        index_val=0
        do while(.not.eof(unit_in))
            index_val=index_val+1
            read(unit_in,*,ERR=5)
        enddo  
5       write(unit_logfile,*) 'Number of rows= ',index_val
    
        !Read data
        n_date=int(index_val/n_roads+.5)
        !write(unit_logfile,*) 'Number of roads= ',n_roads
        !write(unit_logfile,*) 'Number of dates= ',n_date
    endif
    
    !allocate (input_array(i_head,index_val))
    allocate (input_array(i_head,n_date,0:n_roads))
    input_array=nodata
    
    rewind(unit_in)
    read(unit_in,*,ERR=6) !Skip header
6   write(unit_logfile,*) 'Number of roads= ',n_roads
    write(unit_logfile,*) 'Number of dates= ',n_date

    !do jj=1,index_val
    !    read(unit_in,*) (input_array(ii,jj),ii=1,i_head)
    !    write(*,*) jj
    !enddo
    
    !write(*,*) input_array(:,1)
    !read(unit_in,*) ((input_array(ii,jj),ii=1,i_head),jj=1,index_val)
    read(unit_in,*) (((input_array(ii,jj,ro),ii=1,i_head),jj=1,n_date),ro=1,n_roads)
    !write(unit_logfile,'(<i_head>a14)') (trim(header_str(ii)), ii=1,i_head)
    !write(unit_logfile,'(<i_head>f14.2)') (input_array(ii,1), ii=1,i_head)
    !write(unit_logfile,'(<i_head>f14.2)') (input_array(ii,index_val), ii=1,i_head)
    write(unit_logfile,'(a32,a14,a14,a14)') 'Parameter','First value','Last value','Mean value'
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
    do i=1,i_head
        !write(unit_logfile,'(a32,f14.2,f14.2,f14.2)') trim(header_str(i)), input_array(i,1),input_array(i,index_val),sum(input_Array(i,1:index_val)/index_val)
        write(unit_logfile,'(a32,f14.2,f14.2,f14.2)') trim(header_str(i)), &
            input_array(i,1,1),input_array(i,n_date,n_roads), &
            sum(input_array(i,1:n_date,1:n_roads)/(n_date*n_roads))
    end do
 	write(unit_logfile,'(A)') '----------------------------------------------------------------'
   
    if (input_file_type.eq.date_file_type) then
        !Allocate the date array
        if (.not.allocated(date_data)) allocate(date_data(num_date_index,n_date))
        if (.not.allocated(date_str)) allocate(date_str(3,n_date))
        allocate(file_match_str(num_date_index))
        file_match_str=date_match_str
        n_index=num_date_index-1    !Does not include seconds
        !n_date=index_val
        date_data=0.
        !This is the first read. Set n_time according to this
        n_time=n_date
    endif
    if (input_file_type.eq.traffic_file_type) then
        !Allocate the traffic array        
        if (.not.allocated(traffic_data)) allocate(traffic_data(num_traffic_index,n_time,0:n_roads))
        allocate(file_match_str(num_traffic_index))
        file_match_str=traffic_match_str
        n_index=num_traffic_index
        traffic_data=nodata
    endif
    if (input_file_type.eq.meteo_file_type) then
        !Allocate the meteorology array
        if (n_time.ne.n_date) then
            write(unit_logfile,'(A,i,a,i,a)')'ERROR: Number of dates in meteo input file (',n_date,') not the same as in traffic input file (',n_time,'). Stopping'
            stop 35
        endif
        if (.not.allocated(meteo_data)) allocate(meteo_data(num_meteo_index,n_time,0:n_roads))
        allocate(file_match_str(num_meteo_index))
        file_match_str=meteo_match_str
        n_index=num_meteo_index
        meteo_data=nodata
    endif
    if (input_file_type.eq.activity_file_type) then
        !Allocate the activity array. different to the others because it is not chronological
        !This allocation assumes it is using the same dimensions as the other inputs
        if (.not.allocated(activity_input_data)) allocate(activity_input_data(num_activity_input_index,n_date,0:n_roads))
        activity_input_data=nodata
        !Note that this array size is set to n_time based on the size of the date_data array
        if (.not.allocated(activity_data)) allocate(activity_data(num_activity_index,n_time,0:n_roads))
        allocate(file_match_str(num_activity_input_index))
        file_match_str=activity_match_str
        n_index=num_activity_input_index
        activity_input_data=nodata
        activity_input_data(activity_hour_index:activity_minute_index,:,:)=0.
    endif

    if (input_file_type.eq.airquality_file_type) then
        !Allocate the airquality array using n_time
        if (n_time.ne.n_date) then
            write(unit_logfile,'(A,i,a,i,a)')'ERROR: Number of dates in airquality input file (',n_date,') not the same as in traffic input file (',n_time,'). Stopping'
            stop 36
        endif
        if (.not.allocated(airquality_data)) allocate(airquality_data(num_airquality_index,n_time,0:n_roads))
        allocate(file_match_str(num_airquality_index))
        file_match_str=airquality_match_str
        n_index=num_airquality_index
        airquality_data=nodata
    endif
    
    !Search for a string and retrieve index
    match_str=''
    do ii=1,i_head
        match_found=.false.
        do jj=1,n_index           
            index_match=0 
            match_str=trim(file_match_str(jj))
            
            !Special consideration for salt(1)
            if (input_file_type.eq.activity_file_type.and.jj.eq.M_salting_index(1)) then
                match_str=trim(file_match_str(jj))//trim(salt_match_str(na))
                !write(*,*) match_str
            endif
            
            !Special consideration for salt(2)
            if (input_file_type.eq.activity_file_type.and.jj.eq.M_salting_index(2)) then
                match_str='No match please'
                do i=2,num_salt_max
                    !write(*,'(I3,A,A,I)') i,trim(header_str(ii)),trim(salt_match_str(i)),index(header_str(ii),trim(salt_match_str(i)))
                    if (index(header_str(ii),trim(salt_match_str(i))).gt.0) then
                        salt_type(2)=i                       
                        match_str=trim(file_match_str(jj))//trim(salt_match_str(i))
                    endif
                end do                
            endif
            
            !Special consideration for observed moisture to retrieve the units
            if (input_file_type.eq.meteo_file_type.and.jj.eq.road_wetness_obs_input_index) then                
                if (index(header_str(ii),'(mm)').gt.0) then
                    road_wetness_obs_in_mm=1                      
                    match_str=trim(file_match_str(jj))//'(mm)'
                endif
                !write(*,*) match_str
            endif
            
            !Find the matching string in the header
            if (index(header_str(ii),trim(match_str)).ne.0) then
                index_match=ii
            else
                index_match=0
            endif
            
            !If a match then put into the apropriate arrays
            if (index_match.gt.0) then         
                match_found=.true.
                write(unit_logfile,'(A18,i4,i4,A32,A32)') 'Matching index: ',jj,index_match,trim(match_str),trim(header_str(ii))
                
                if (input_file_type.eq.date_file_type) then
                    date_data(jj,:)=input_array(index_match,:,1)
                    available_date_data(jj)=.true.
                endif
                if (input_file_type.eq.activity_file_type) then
                    activity_input_data(jj,:,:)=input_array(index_match,:,:)
                    available_activity_data(jj)=.true.
                endif
                if (input_file_type.eq.traffic_file_type) then
                    traffic_data(jj,:,:)=input_array(index_match,:,:)
                    available_traffic_data(jj)=.true.
                endif
                if (input_file_type.eq.meteo_file_type) then
                    meteo_data(jj,:,:)=input_array(index_match,:,:)
                    available_meteo_data(jj)=.true.
                endif
                if (input_file_type.eq.airquality_file_type) then
                    airquality_data(jj,:,:)=input_array(index_match,:,:)
                    available_airquality_data(jj)=.true.
                endif             

            endif
        end do

        if (.not.match_found) then
            write(unit_logfile,*) 'No match found for index: ',ii,trim(header_str(ii))
        endif
  
    enddo

    !Set road wetness max and min. Needs to be rethought. Not used
    if (available_meteo_data(road_wetness_obs_input_index)) then
        max_road_wetness_obs=maxval(meteo_data(road_wetness_obs_input_index,:,:))
        min_road_wetness_obs=minval(meteo_data(road_wetness_obs_input_index,:,:))
    else
        max_road_wetness_obs=nodata
        min_road_wetness_obs=nodata
    endif

    deallocate (input_array)
    deallocate (file_match_str)
    

10  close(unit_in,status='keep')

        !If zip file used then delete the text file that has been extracted
        if (read_metadata_in_zip_format) then
            command_line_zip=trim(delete_file_command)//' '//trim(temp_name)
            write(unit_logfile,'(a,a)') 'Command line zip: ',trim(command_line_zip)      
            CALL EXECUTE_COMMAND_LINE (trim(command_line_zip),wait=.true.)
        endif

    endif   !File available
    enddo
    
    !Set the total number of time indexes
    n_time=size(date_data,2)

    !Set some physical limits on the meteorological data. None of it can be nodata
    do ro=1,n_roads
    do ti=1,n_time
        meteo_data(RH_index,ti,ro)=min(max(meteo_data(RH_index,ti,ro),0.),100.)
        meteo_data(FF_index,ti,ro)=max(meteo_data(FF_index,ti,ro),0.)
        meteo_data(DD_index,ti,ro)=min(max(meteo_data(DD_index,ti,ro),0.),360.)
        meteo_data(Rain_precip_index,ti,ro)=max(meteo_data(Rain_precip_index,ti,ro),0.)
        meteo_data(Snow_precip_index,ti,ro)=max(meteo_data(Snow_precip_index,ti,ro),0.)
    enddo
    enddo
        
    

!    if (unit_logfile.gt.0) then
!        close(unit_logfile,status='keep')
!    endif

    !Allocate for the files that are not available
    if (.not.file_available(activity_file_type)) then
        write(unit_logfile,'(A)')'WARNING: Activity data not available. Allocating no activities to all dates'
        if (.not.allocated(activity_data)) allocate(activity_data(num_activity_index,n_time,0:n_roads))
        activity_data=0.
        available_activity_data=.true.
        !write(*,*) '####Setting activity data to 0'
    endif
    if (.not.file_available(airquality_file_type)) then
        write(unit_logfile,'(A)')'WARNING: Airquality data not available. Allocating nodata to all dates and 1 to concentration emission conversion factor'
        if (.not.allocated(airquality_data)) allocate(airquality_data(num_airquality_index,n_time,0:n_roads))
        airquality_data=nodata
        available_airquality_data=.false.
        airquality_data(f_conc_index,:,:)=1.0
        available_airquality_data(f_conc_index)=.true.
    endif

    !Redistribute the activity data
    !CHECK THIS WHEN NOT AVAILABLE
    if (file_available(activity_file_type)) then
        call process_NORTRIP_activity_inputdata
    endif



    
    end subroutine read_NORTRIP_inputdata
!----------------------------------------------------------------------

!----------------------------------------------------------------------
    subroutine override_NORTRIP_inputdata

    use NORTRIP_definitions
    
    implicit none
    
    !Override these values that come in the parameter flag files
    !Used to adjust all roads in NORTRIP
    if (override_wind_speed_correction.ne.nodata_orig) then
        wind_speed_correction=override_wind_speed_correction
    endif
    if (override_albedo_road_offset.ne.nodata_orig) then
        albedo_road=albedo_road+override_albedo_road_offset
    endif
    if (override_long_rad_in_offset.ne.nodata_orig) then
        long_rad_in_offset=override_long_rad_in_offset
    endif
    if (override_RH_offset.ne.nodata_orig) then
        RH_offset=override_RH_offset
    endif
    if (override_T_a_offset.ne.nodata_orig) then
        T_a_offset=override_T_a_offset
    endif

    end subroutine override_NORTRIP_inputdata
!----------------------------------------------------------------------

!----------------------------------------------------------------------
    subroutine set_NORTRIP_save_file_flags

    use NORTRIP_definitions
    
    implicit none
    
    !Save emissions, initi data, summary road meteo and summary emission and mass data for Bedre Byluft calculation type
    if (trim(calculation_type).eq.'Bedre byluft') then
        if (unit_logfile.gt.0) write(*,'(A)') 'Saving Bedre byluft emission and initial files'
        NORTRIP_save_init_data_flag=.true.
        NORTRIP_save_episode_emissions_flag=.true.
        NORTRIP_save_episode_grid_emissions_flag=.true.
        NORTRIP_save_road_meteo_data_flag=.true.
        NORTRIP_save_road_emission_and_mass_data_flag=.true.
        use_ospm_flag=1
    endif

    !Save summary road meteo and summary emission and mass data for Bedre Byluft calculation type
    if (trim(calculation_type).eq.'SMHI') then
        if (unit_logfile.gt.0) write(*,'(A)') 'Saving SMHI data'
        NORTRIP_save_init_data_flag=.true.
        NORTRIP_save_road_meteo_data_flag=.true.
        NORTRIP_save_road_emission_and_mass_data_flag=.true.
        if (n_roads_total.eq.1) NORTRIP_save_all_data_flag=.true.
    endif

    !Save summary road meteo and summary emission and mass data for Bedre Byluft calculation type
    if (trim(calculation_type).eq.'road weather') then
        if (unit_logfile.gt.0) write(*,'(A)') 'Saving road weather data'
        NORTRIP_save_init_data_flag=.true.
        NORTRIP_save_road_meteo_data_flag=.true.
        NORTRIP_save_road_emission_and_mass_data_flag=.true.
        !NORTRIP_save_episode_emissions_flag=.true.
        !NORTRIP_save_episode_grid_emissions_flag=.true.
        NORTRIP_save_road_emission_and_mass_data_stats_flag=.true.
        NORTRIP_save_road_summary_data_flag=.true.
    endif

    if (trim(calculation_type).eq.'SLB') then
        if (unit_logfile.gt.0) write(*,'(A)') 'Saving SLB data'
        NORTRIP_save_init_data_flag=.true.
        NORTRIP_save_road_emission_and_mass_data_stats_flag=.true.
    endif

    !Save complete data in ascii for 'normal' calculation type of one road only
    if (trim(calculation_type).eq.'Normal') then
        if (n_roads_total.eq.1) NORTRIP_save_all_data_flag=.true.
    endif

    !Save emissions, initi data, summary road meteo and summary emission and mass data for uEMEP calculation type
    !if (trim(calculation_type).eq.'uEMEP') then
    if (index(calculation_type,'uEMEP').gt.0) then
        if (unit_logfile.gt.0) write(*,'(A)') 'Saving uEMEP emission and initial files'
        NORTRIP_save_init_data_flag=.true.
        NORTRIP_save_uEMEP_emissions_flag=.true.
        NORTRIP_save_uEMEP_grid_emissions_flag=.false.
        NORTRIP_save_road_meteo_data_flag=.false.
        NORTRIP_save_road_emission_and_mass_data_flag=.false.
        NORTRIP_save_road_summary_data_flag=.true.
        NORTRIP_save_road_emission_activity_data_flag=.true.
        use_ospm_flag=0
    endif
    if (index(calculation_type,'Avinor').gt.0) then
        if (unit_logfile.gt.0) write(*,'(A)') 'Saving Avinor initial files'
        NORTRIP_save_init_data_flag=.true.
        NORTRIP_save_uEMEP_emissions_flag=.false.
        NORTRIP_save_uEMEP_grid_emissions_flag=.false.
        NORTRIP_save_road_meteo_data_flag=.true.
        NORTRIP_save_road_emission_and_mass_data_flag=.false.
        NORTRIP_save_road_summary_data_flag=.true.
        NORTRIP_save_road_emission_activity_data_flag=.false.
        use_ospm_flag=0
    endif

    end subroutine set_NORTRIP_save_file_flags
!----------------------------------------------------------------------

!----------------------------------------------------------------------
    subroutine check_NORTRIP_inputdata

    use NORTRIP_definitions
    
    implicit none

    real percent_available
    
    !Open log file
    !if (unit_logfile.gt.0) then
    !    open(unit_logfile,file=filename_log,status='old',position='append')
    !endif


 	write(unit_logfile,'(A)') '================================================================'
 	write(unit_logfile,'(A)') 'Checking input data for road 1 only'
   
    !Check traffic data for missing values and fill in with daily cycles. Not done
    
    !Only do these checks for road 1
    ro=1

    !Check traffic data. Fill nodata values with last valid value
 	write(unit_logfile,'(A)') '----------------------------------------------------------------'
 	write(unit_logfile,'(A)') 'Input traffic data available (%)'
    do i=1,num_traffic_index
        call check_data_sub(traffic_data(i,:,ro),available_traffic_data(i),nodata_input,percent_available)
 	    write(unit_logfile,'(a32,f6.1,f10.1,f10.1)') trim(traffic_match_str(i)),percent_available,minval(traffic_data(i,:,ro)),maxval(traffic_data(i,:,ro))     
    enddo

    !Check meteo data. Fill nodata values with last valid value
 	write(unit_logfile,'(A)') '----------------------------------------------------------------'
 	write(unit_logfile,'(A)') 'Input meteo data available (%) with min and max'
    do i=1,num_meteo_index
        call check_data_sub(meteo_data(i,:,ro),available_meteo_data(i),nodata_input,percent_available)
 	    write(unit_logfile,'(a32,f6.1,f10.1,f10.1)') trim(meteo_match_str(i)),percent_available,minval(meteo_data(i,:,ro)),maxval(meteo_data(i,:,ro))
    enddo    

   !Check activity data. Only check availability
 	write(unit_logfile,'(A)') '----------------------------------------------------------------'
 	write(unit_logfile,'(A)') 'Activity data available (%)'
    do i=1,num_activity_index
        call check_available_data_sub(activity_data(i,:,ro),available_activity_data(i),nodata_activity,percent_available)
 	    write(unit_logfile,'(a32,f6.1,f10.1,f10.1)') trim(activity_match_str(i)),percent_available,minval(activity_data(i,:,ro)),maxval(activity_data(i,:,ro))
        !Special case, must be set to 0 if not available
        if (.not.available_activity_data(i)) then
            activity_data(i,:,ro)=0.
        endif
        
    enddo

    !Check input activity data. Only check availability
    !Do not check as it sets the availability to false
 	write(unit_logfile,'(A)') '----------------------------------------------------------------'
 	write(unit_logfile,'(A)') 'Input activity data available (%)'
    do i=1,num_activity_index
        !call check_available_data_sub(activity_input_data(i,:,ro),available_activity_data(i),nodata_activity,percent_available)
 	    write(unit_logfile,'(a32,f6.1,f10.1,f10.1)') trim(activity_match_str(i)),percent_available,minval(activity_input_data(i,:,ro)),maxval(activity_input_data(i,:,ro))
        !Special case, must be set to 0 if not available
        if (.not.available_activity_data(i)) then
            activity_input_data(i,:,ro)=0.
        endif
        
    enddo
    
    !Check air quality data. Only checks availability
 	write(unit_logfile,'(A)') '----------------------------------------------------------------'
 	write(unit_logfile,'(A)') 'Input air quality data available (%)'
    do i=1,num_airquality_index
        call check_available_data_sub(airquality_data(i,:,ro),available_airquality_data(i),nodata_input,percent_available)
 	    write(unit_logfile,'(a32,f6.1,f10.1,f10.1)') trim(airquality_match_str(i)),percent_available,minval(airquality_data(i,:,ro)),maxval(airquality_data(i,:,ro))
    enddo
    
 	write(unit_logfile,'(A)') '================================================================'
   
    !Close log file
    !if (unit_logfile.gt.0) then
    !    close(unit_logfile,status='keep')
    !endif

    end subroutine check_NORTRIP_inputdata
!----------------------------------------------------------------------

!----------------------------------------------------------------------
    subroutine process_NORTRIP_activity_inputdata

    use NORTRIP_definitions
    
    implicit none

    integer i_road
    integer n_input_activity,n_output_activity
    integer match_count
    logical match_found
    
    !Open log file
    !if (unit_logfile.gt.0) then
    !    open(unit_logfile,file=filename_log,status='old',position='append')
    !endif

 	write(unit_logfile,'(A)') '================================================================'
 	write(unit_logfile,'(A)') 'Processing activity data'
   
    n_input_activity=size(activity_input_data,2)
    n_output_activity=size(activity_data,2)
    
 	write(unit_logfile,'(A)') '----------------------------------------------------------------'

    if (n_input_activity.eq.n_output_activity) then
        
        activity_data=activity_input_data
        write(unit_logfile,'(A,5i)') 'Number of activity dates same as input data dates: ',n_input_activity

    else
        
        activity_data=0.
        match_count=0
    
        do ro=1,n_roads
        do j=1,n_input_activity
            match_found=.false.
            do i=1,n_output_activity
                !write(*,*) int(date_data(year_index:minute_index,i))
                !write(*,*) '    ',int(activity_input_data(activity_year_index:activity_minute_index,j,i_road))
                i_road=ro
                !Matches date and road ID. If no road ID has been read (nodata) then apply to all roads for that date
                if (date_data(year_index,i).eq.int(activity_input_data(activity_year_index,j,i_road)).and. &
                    date_data(month_index,i).eq.int(activity_input_data(activity_month_index,j,i_road)).and. &
                    date_data(day_index,i).eq.int(activity_input_data(activity_day_index,j,i_road)).and. &
                    date_data(hour_index,i).eq.int(activity_input_data(activity_hour_index,j,i_road)).and. &
                    date_data(minute_index,i).eq.int(activity_input_data(activity_minute_index,j,i_road)).and. &
                    (road_ID(ro).eq.int(activity_input_data(activity_roadID_index,j,i_road)).or. &
                    activity_input_data(activity_roadID_index,j,i_road).eq.nodata)) then
                
                    activity_data(:,i,i_road)=activity_data(:,i,i_road)+activity_input_data(:,j,i_road)
                    match_count=match_count+1
                    match_found=.true.
                endif             
            
            enddo
            if (.not.match_found) then
                write(unit_logfile,'(A,5i6)') 'No match for : ',int(activity_input_data(activity_year_index:activity_minute_index,j,i_road))
            endif
        enddo
        enddo
    
        write(unit_logfile,'(A,5i)') 'Number of activity dates available : ',n_input_activity
        write(unit_logfile,'(A,5i)') 'Number of activity dates matched: ',match_count
    endif
    
    !Close log file
    !if (unit_logfile.gt.0) then
    !    close(unit_logfile,status='keep')
    !endif

    end subroutine process_NORTRIP_activity_inputdata
!----------------------------------------------------------------------

!----------------------------------------------------------------------
    subroutine check_data_sub(val,available_flag,nodata_in,percent_available)
    
    use NORTRIP_definitions
    
    implicit none
    
    !Input variables
    real val(n_time)
    logical available_flag
    real nodata_in

    !Output variables
    real percent_available

    !Internal
    real available(n_time)
    real val_in(n_time)

    val_in=val
    if (available_flag) then
        !Tag the nodata values
        do ti=1,n_time
            if (val_in(ti).eq.nodata_in) then
               available(ti)=0.0
            else
               available(ti)=1.0
            endif
        enddo

        percent_available=sum(available)/size(available,1)*100.
        !If there is no data available then set the availability to false
        if (sum(available).eq.0) then
           available_flag=.false.
           val=nodata
        else
           available_flag=.true.
        endif
       
        !Set the nodata values to the last valid data point forwards
        if (available_flag) then          
            do ti=2,n_time
                if (val_in(ti).eq.nodata_in) then
                    val(ti)=val_in(ti-1)
                else
                    val(ti)=val_in(ti)
                endif
            enddo
        endif
        !Set the nodata values to the last valid data point backwards. This is only when the first values are no data
        if (available_flag) then
            do ti=n_time-1,1,-1
                if (val(ti).eq.nodata_in.or.val(ti).eq.nodata) then
                    val(ti)=val(ti+1)
                endif
            enddo
        endif
        
    else
        val=nodata
        percent_available=0.0
    endif

end subroutine check_data_sub
!----------------------------------------------------------------------

!----------------------------------------------------------------------
    subroutine check_available_data_sub(val,available_flag,nodata_in,percent_available)
    
    use NORTRIP_definitions
    
    implicit none
    
    !Input variables
    real val(n_time)
    logical available_flag
    real nodata_in

    !Output variables
    real percent_available

    !Internal
    real available(n_time)
    real val_in(n_time)

    val_in=val
    if (available_flag) then
        !Tag the nodata values
        do ti=1,n_time
            if (val_in(ti).eq.nodata_in) then
               available(ti)=0.0
            else
               available(ti)=1.0
            endif
        enddo

        percent_available=sum(available)/size(available,1)*100.        
        
        !If there is no data available then set the availability to false
        if (sum(available).eq.0) then
           available_flag=.false.
           val=nodata
        else
           available_flag=.true.
        endif
    else
        val=nodata
        percent_available=0.0
    endif

end subroutine check_available_data_sub
!----------------------------------------------------------------------

!----------------------------------------------------------------------
subroutine find_read_line_val1(unit_in,unit_output,val1,search_str,val_default)
    !Finds a search string and reads a leading string and returns the real variable that follows it
    !Tab delimitted between.
    !use NORTRIP_definitions
    
    implicit none
    
    character(256) header_str,val_str,temp_str
    character(*) search_str
    integer unit_in,unit_output
    integer index_val
    real val1,val_default
    logical string_found
    
    val1=val_default
    
    rewind(unit_in)
    string_found=.false.
    do while ((.not.string_found).and.(.not.eof(unit_in)))
        read(unit_in,'(a)',ERR=10) temp_str
        if (index(temp_str,trim(search_str)).ne.0) then
            string_found=.true.
        endif
    end do

    if (string_found) then
        index_val=index(temp_str,achar(9))
        header_str=temp_str(1:index_val-1)
        val_str=temp_str(index_val+1:)
        !Only take the first value
        index_val=index(val_str,achar(9))
        if (index_val.gt.0) then
            val_str=val_str(1:index_val-1)
        endif
        read(val_str,*) val1
        !write(*,*) trim(temp_str)
        !write(*,*) trim(val_str),val1
        if (unit_output.ge.0) then
        write(unit_output,'(A40,A3,es12.3)') trim(header_str),' = ',val1
        endif
        return
    endif       
    
	write(unit_output,'(A40,A3,es12.3,A40)') trim(search_str),' = ',val1,'STRING NOT FOUND, SET TO DEFAULT'
    return
10	write(unit_output,'(A40,A3,es12.2,A40)') trim(search_str),' = ',val1,'ERROR'

    end subroutine find_read_line_val1
!----------------------------------------------------------------------

!----------------------------------------------------------------------
subroutine find_read_line_int1(unit_in,unit_output,val1,search_str,val_default)
    !Finds a search string and reads a leading string and returns the real variable that follows it
    !Tab delimitted between.

    !use NORTRIP_definitions
    
    implicit none
    
    character(256) header_str,val_str,temp_str
    character(*) search_str
    integer unit_in,unit_output
    integer index_val
    integer val1,val_default
    logical string_found
    
    val1=val_default
    
    rewind(unit_in)
    string_found=.false.
    do while ((.not.string_found).and.(.not.eof(unit_in)))
        read(unit_in,'(a)',ERR=10) temp_str 
        if (index(temp_str,trim(search_str)).ne.0) then
            string_found=.true.
        endif
    end do
    
    if (string_found) then
        index_val=index(temp_str,achar(9))
        header_str=temp_str(1:index_val-1)
        val_str=temp_str(index_val+1:)
        !Only take the first value
        index_val=index(val_str,achar(9))
        if (index_val.gt.0) then
            val_str=val_str(1:index_val-1)
        endif
        read(val_str,*) val1
        !write(*,*) trim(temp_str)
        !write(*,*) trim(val_str),val1
        if (unit_output.ge.0) then
        write(unit_output,'(A40,A3,i12)') trim(header_str),' = ',val1
        endif
        return
    endif       
    
	write(unit_output,'(A40,A3,i12,A40)') trim(search_str),' = ',val1,'STRING NOT FOUND, SET TO DEFAULT'
    return
10	write(unit_output,'(A40,A3,i12,A40)') trim(search_str),' = ',val1,'ERROR'

    end subroutine find_read_line_int1
!----------------------------------------------------------------------

!----------------------------------------------------------------------
subroutine find_read_line_valn(unit_in,unit_output,val,n_val,search_str,val_default)
    !Finds a search string and reads a leading string and returns the real variable that follows it
    !Tab delimitted between.
    implicit none
    
    integer n_val
    character(256) header_str
    character(*) search_str
    character(n_val*16+256) val_str,temp_str    !Assumes data is formatted with a width <16
    integer unit_in,unit_output
    integer index_val
    real val(n_val)
    real val_default
    logical string_found
    character(256) err_message
    integer :: n_show=4
    
    !write(*,*) len(val_str)
    
    val=val_default
    rewind(unit_in)
!    rewind(unit_in,ERR=5,IOMSG=err_message)
!5   write(*,'(2a)') 'ERROR: ',trim(err_message)
    
    string_found=.false.
    do while ((.not.string_found).and.(.not.eof(unit_in)))
        read(unit_in,'(a)',ERR=10) temp_str
        !write(*,*) trim(temp_str)
        if (index(temp_str,trim(search_str)).ne.0) then
            string_found=.true.
        endif
    end do
    if (string_found) then
        index_val=index(temp_str,achar(9))
        header_str=temp_str(1:index_val-1)
        val_str=temp_str(index_val+1:)
        !Only take the first value up to the end or the next tab
        index_val=index(val_str,achar(9))
        if (index_val.gt.0) then
            val_str=val_str(1:index_val-1)
        endif
        !write(*,'(A)') val_str
        if (LEN(trim(val_str)).gt.0) then
            read(val_str,*) val(1:n_val)
        else
            goto 15
        endif
    
        if (unit_output.ge.0) then
            n_show=min(n_show,n_val)
            write(unit_output,'(A40,A3,<n_show>es12.3)') trim(header_str),' = ',val(1:n_show)
        endif
        return
    endif
    
10	write(unit_output,'(A40,A3,e12.3,A40)') trim(search_str),' = ',val(1),'STRING NOT FOUND, SET TO DEFAULT'
    return
15	write(unit_output,'(A40,A3,e12.2,A40)') trim(search_str),' = ',val(1),'ERROR'

    end subroutine find_read_line_valn
!----------------------------------------------------------------------
!----------------------------------------------------------------------
subroutine find_read_line_intn(unit_in,unit_output,val,n_val,search_str,val_default)
    !Finds a search string and reads a leading string and returns the real variable that follows it
    !Tab delimitted between.
    implicit none
    
    integer n_val
    character(256) header_str
    character(*) search_str
    character(n_val*16+256) val_str,temp_str
    integer unit_in,unit_output
    integer index_val
    integer val(n_val)
    integer val_default
    logical string_found
    integer :: n_show=4
    
    val=val_default
    rewind(unit_in)
    string_found=.false.
    do while ((.not.string_found).and.(.not.eof(unit_in)))
        read(unit_in,'(a)',ERR=10) temp_str
        if (index(temp_str,trim(search_str)).ne.0) then
            string_found=.true.
        endif
    end do
    if (string_found) then
        index_val=index(temp_str,achar(9))
        header_str=temp_str(1:index_val-1)
        val_str=temp_str(index_val+1:)
        !Only take the first value up to the end or the next tab
        index_val=index(val_str,achar(9))
        if (index_val.gt.0) then
            val_str=val_str(1:index_val-1)
        endif
        !write(*,'(A)') val_str
        if (LEN(trim(val_str)).gt.0) then
            read(val_str,*) val(1:n_val)
        else
            goto 15
        endif
    
        if (unit_output.ge.0) then
            n_show=min(n_show,n_val)
            write(unit_output,'(A40,A3,<n_show>I12)') trim(header_str),' = ',val(1:n_show)
        endif
        return
    endif
    
10	write(unit_output,'(A40,A3,i12,A40)') trim(search_str),' = ',val(1),'STRING NOT FOUND, SET TO DEFAULT'
    return
15	write(unit_output,'(A40,A3,i12,A40)') trim(search_str),' = ',val(1),'ERROR'

    end subroutine find_read_line_intn
!----------------------------------------------------------------------


    subroutine find_read_line_str1(unit_in,unit_output,str1,search_str,str_default)
    !Finds a search string and reads a leading string and returns the string that follows it
    !Tab delimitted between.
    implicit none
    
    character(256) header_str,val_str,temp_str
    character(*) search_str
    integer unit_in,unit_output
    integer index_val
    real val_default
    logical string_found
    character(*) str1,str_default
    
    str1=str_default
    
    rewind(unit_in)
    string_found=.false.
    do while ((.not.string_found).and.(.not.eof(unit_in)))
        read(unit_in,'(a)',ERR=10) temp_str
        if (index(temp_str,trim(search_str)).ne.0) then
            string_found=.true.
        endif
    end do
    
    if (string_found) then
        index_val=index(temp_str,achar(9))
        header_str=temp_str(1:index_val-1)
        val_str=temp_str(index_val+1:)
        !Only take the first value
        index_val=index(val_str,achar(9))
        if (index_val.gt.0) then
            val_str=val_str(1:index_val-1)
        endif
        read(val_str,'(a)') str1
        !write(*,*) trim(temp_str)
        !write(*,*) trim(val_str),str1
        if (unit_output.ge.0) then
            write(unit_output,'(A40,A3,A)') trim(header_str),' = ',trim(str1)
        endif
        return
    endif       
    
	write(unit_output,'(A40,A3,A,A40)') trim(search_str),' = ',trim(str1),'STRING NOT FOUND, SET TO DEFAULT'
    return
10	write(unit_output,'(A40,A3,A,A40)') trim(search_str),' = ',trim(str1),'ERROR'

    end subroutine find_read_line_str1
!----------------------------------------------------------------------
