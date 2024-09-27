
subroutine open_NETCDF_init_file(ncid,exists)
    use NORTRIP_definitions
    use netcdf
    implicit none

    !Out
    integer, intent(out):: ncid
    logical, intent(out) :: exists
    
    !Local
    integer :: a(num_date_index)
    character(256) :: temp_name
    character(256) :: filename_temp
    character(256) :: filename
    
    !Set the path and file name
    filename_temp=filename_init_netcdf
    a=date_data(:,1)
    call date_to_datestr_bracket(a,path_init,temp_name)
    call date_to_datestr_bracket(a,temp_name,temp_name)
    call date_to_datestr_bracket(a,temp_name,temp_name)
    
    !Open the outputfile for date
    filename=trim(temp_name)//trim(filename_temp)
    !filename_bin=trim(path_init)//trim(filename_temp)
    !Check file for reading
    inquire(file=trim(filename), exist = exists)
    if (.not.exists) then
        if (ro_tot.eq.1) write(unit_logfile,'(A)')' WARNING: Initial netcdf input file does not exist: '//trim(filename)
        return 
    else 
        call check(NF90_OPEN(filename,nf90_nowrite, ncid))
    endif

end subroutine open_NETCDF_init_file

subroutine close_NETCDF_file(ncid)
    use netcdf
    implicit none

    integer, intent(in) :: ncid
    call check(NF90_close(ncid))

end subroutine close_NETCDF_file

!==================Read init files on netcdf format=========================
subroutine NORTRIP_read_init_data_netcdf(ncid)
    use NORTRIP_definitions
    use netcdf
    implicit none

    logical :: exists
    integer :: a(num_date_index)
    character(256) :: temp_name
    character(256) :: filename_temp
    character(256) :: filename
    logical :: input_is_nan=.false.
    integer :: dimid, varid
    integer, intent(in) :: ncid
    integer :: len_track, len_size, len_var, len_roads, len_moist, len_source

    real :: road_meteo_data_tmp(num_road_meteo)
    road_meteo_data_tmp = 0

    !Set the path and file name
    filename_temp=filename_init_netcdf
    a=date_data(:,min_time)
    call date_to_datestr_bracket(a,path_init,temp_name)
    call date_to_datestr_bracket(a,temp_name,temp_name)
    call date_to_datestr_bracket(a,temp_name,temp_name)
    
    !Open the outputfile for date
    filename=trim(temp_name)//trim(filename_temp)

    !Check file for reading
    inquire(file=trim(filename), exist = exists)
    if (.not.exists) then
        if (ro_tot.eq.1) write(unit_logfile,'(A)')' WARNING: Initial netcdf input file does not exist: '//trim(filename)
        return 
    endif
    !If it is the first loop then check if the dimensions match
    if (ro_tot.eq.1) then

        write(unit_logfile,'(A)') ''
        write(unit_logfile,'(A)') 'Reading data from init file (NORTRIP_read_init_data_single NETCDF)'
        write(unit_logfile,'(A)') '================================================================'

        ! ! Read the first lines --> determine if array lengths match
        call check(nf90_inq_dimid(ncid,"num_size",dimid))
        call check(nf90_inquire_dimension(ncid,dimid,len=len_size))

        call check(nf90_inq_dimid(ncid,"num_track",dimid))
        call check(nf90_inquire_dimension(ncid,dimid,len=len_track))

        call check(nf90_inq_dimid(ncid,"num_var",dimid))
        call check(nf90_inquire_dimension(ncid,dimid,len=len_var))

        call check(nf90_inq_dimid(ncid,"road_id",dimid))
        call check(nf90_inquire_dimension(ncid,dimid,len=len_roads))

        call check(nf90_inq_dimid(ncid,"num_moisture",dimid))
        call check(nf90_inquire_dimension(ncid,dimid,len=len_moist))

        call check(nf90_inq_dimid(ncid,"num_source_all",dimid))
        call check(nf90_inquire_dimension(ncid,dimid,len=len_source))

        if (len_roads.ne.n_roads_total.or.len_track.ne.num_track.or.len_source.ne.num_source_all &
        .or.len_var.ne.num_road_meteo.or.len_moist.ne.num_moisture) then
            write(unit_logfile,'(A,i8,4i4,A,i8,4i4)')' ERROR: Initial input netcdf file arrays do not match current. Read: ', &
            len_roads,len_track,num_source_all,len_var,len_moist &
            ,' Existing: ',n_roads_total,num_track,num_source_all,num_road_meteo,num_moisture
            stop 1    
        endif
    end if

    tr=1

    !read M_road_data
    call check(nf90_inq_varid(ncid,"M_road_data",varid))
    call check(nf90_get_var(ncid,varid, M_road_data(:,:,min_time,tr,0),start = (/1,1,1,ro_tot/)))

    call check(nf90_inq_varid(ncid,"road_meteo_data",varid))
    call check(nf90_get_var(ncid,varid, road_meteo_data_tmp(:),start = (/1,1,ro_tot/)))

    !read g_road_data
    call check(nf90_inq_varid(ncid,"g_road_data",varid))
    call check(nf90_get_var(ncid,varid, g_road_data(:,min_time,tr,0),start = (/1,1,ro_tot/)))

    ro = 0
    !read "time_since last" data
    call check(nf90_inq_varid(ncid,"time_since_last_salting",varid))
    call check(nf90_get_var(ncid,varid, time_since_last_salting(ro),start=(/ro_tot/)))

    call check(nf90_inq_varid(ncid,"time_since_last_sanding",varid))
    call check(nf90_get_var(ncid,varid, time_since_last_sanding(ro),start=(/ro_tot/)))

    call check(nf90_inq_varid(ncid,"time_since_last_cleaning",varid))
    call check(nf90_get_var(ncid,varid, time_since_last_cleaning(ro),start=(/ro_tot/)))

    call check(nf90_inq_varid(ncid,"time_since_last_ploughing",varid))
    call check(nf90_get_var(ncid,varid, time_since_last_ploughing(ro),start=(/ro_tot/)))

    !Do not overwrite the observed surface temperature with the init file, bc. it has already been set to observations in 
    !NORTRIP_multiroad_save_meteodata.f90
    do i = 1,num_road_meteo
        if (i .ne. road_temperature_obs_index) then
            road_meteo_data(i,1,1,0) = road_meteo_data_tmp(i) 
        endif
    end do

    !stop 
    !Make sure there is no energy correction when the flag is off.
    if ( .not. use_energy_correction_flag ) then
        road_meteo_data(E_corr_index,1,1,ro) = 0.
    end if

    !Check for NaNs in the input data. !TODO: It is probably unecessary to stop the whole simulation if any NaNs are found. Should have better checks and solutions for this. 
    do tr=1,num_track
        do s=1,num_source_all
            do x=1,num_size
                if (isnan(M_road_data(s,x,min_time,tr,ro))) input_is_nan=.true.
            enddo
        enddo
        
        do i=1,num_road_meteo
            if (isnan(road_meteo_data(i,min_time,tr,ro))) input_is_nan=.true.
        enddo
        
        do m=1,num_moisture
            if (isnan(g_road_data(m,min_time,tr,ro))) input_is_nan=.true.
        enddo
        
        if (input_is_nan) then
            write(unit_logfile,'(A)') 'ERROR reading init file (NORTRIP_read_init_data_netcdf). NaN values in data. Stopping'
            stop 3
        endif
        
    enddo

end subroutine NORTRIP_read_init_data_netcdf
!===========================================================================