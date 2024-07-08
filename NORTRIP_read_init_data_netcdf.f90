
subroutine open_NETCDF_init_file(ncid_init,exists)
    use NORTRIP_definitions
    use netcdf
    implicit none

    logical,intent(out) :: exists
    integer :: a(num_date_index)
    character(256) :: temp_name
    character(256) :: filename_temp
    character(256) :: filename
    integer :: dimid, varid
    integer, intent(out):: ncid_init

        !Set the path and file name
    filename_temp=filename_init_netcdf
    a=date_data(:,min_time)
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
        call check(NF90_OPEN(filename,nf90_nowrite, ncid_init))
    endif

end subroutine open_NETCDF_init_file

subroutine close_NETCDF_init_file(ncid)
    use NORTRIP_definitions
    use netcdf
    implicit none

    integer, intent(in) :: ncid

    call check(NF90_close(ncid))

end subroutine close_NETCDF_init_file

!==================Read init files on netcdf format=========================
subroutine NORTRIP_read_init_data_netcdf(ncid_init)
    use NORTRIP_definitions
    use netcdf
    implicit none

    integer :: current_data(num_date_index)
    logical :: exists
    integer :: a(num_date_index)
    character(256) :: temp_name
    character(256) :: filename_temp
    character(256) :: filename
    logical :: input_is_nan=.false.
    integer :: dimid, varid
    integer, intent(in) :: ncid_init
    integer :: len_track, len_size, len_var, len_roads, len_moist, len_source

    real :: road_meteo_data_tmp(num_road_meteo,n_time,num_track,n_roads)
    road_meteo_data_tmp = 0
    !allocate (road_meteo_data(num_road_meteo,n_time,num_track,0:n_roads))

    !Set the path and file name
    filename_temp=filename_init_netcdf
    a=date_data(:,min_time)
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
        write(unit_logfile, '(A)')'Opening netcdf init file for reading: '//trim(filename)
    endif
    !If it is the first loop then check if the dimensions match
    if (ro_tot.eq.1) then

        write(unit_logfile,'(A)') ''
        write(unit_logfile,'(A)') 'Reading data from init file (NORTRIP_read_init_data_single NETCDF)'
        write(unit_logfile,'(A)') '================================================================'

        ! ! Read the first lines --> determine if array lengths match
        call check(nf90_inq_dimid(ncid_init,"num_size",dimid))
        call check(nf90_inquire_dimension(ncid_init,dimid,len=len_size))

        call check(nf90_inq_dimid(ncid_init,"num_track",dimid))
        call check(nf90_inquire_dimension(ncid_init,dimid,len=len_track))

        call check(nf90_inq_dimid(ncid_init,"num_var",dimid))
        call check(nf90_inquire_dimension(ncid_init,dimid,len=len_var))

        call check(nf90_inq_dimid(ncid_init,"road_id",dimid))
        call check(nf90_inquire_dimension(ncid_init,dimid,len=len_roads))

        call check(nf90_inq_dimid(ncid_init,"num_moisture",dimid))
        call check(nf90_inquire_dimension(ncid_init,dimid,len=len_moist))

        call check(nf90_inq_dimid(ncid_init,"num_source_all",dimid))
        call check(nf90_inquire_dimension(ncid_init,dimid,len=len_source))

        if (len_roads.ne.n_roads_total.or.len_track.ne.num_track.or.len_source.ne.num_source_all &
        .or.len_var.ne.num_road_meteo.or.len_moist.ne.num_moisture) then
            write(unit_logfile,'(A,i8,4i4,A,i8,4i4)')' ERROR: Initial input netcdf file arrays do not match current. Read: ', &
            len_roads,len_track,num_source_all,len_var,len_moist &
            ,' Existing: ',n_roads_total,num_track,num_source_all,num_road_meteo,num_moisture
            stop 1    
        endif
    end if
    ti = min_time
    tr=1

    !read M_road_data
    call check(nf90_inq_varid(ncid_init,"M_road_data",varid))
    call check(nf90_get_var(ncid_init,varid, M_road_data(:,:,min_time,tr,0),start = (/1,1,1,ro_tot/)))

    call check(nf90_inq_varid(ncid_init,"road_meteo_data",varid))
    call check(nf90_get_var(ncid_init,varid, road_meteo_data_tmp(:,1,tr,0),start = (/1,1,ro_tot/)))

    !read g_road_data
    call check(nf90_inq_varid(ncid_init,"g_road_data",varid))
    call check(nf90_get_var(ncid_init,varid, g_road_data(:,min_time,tr,0),start = (/1,1,ro_tot/)))

    ro = 0
    !read "time_since last" data
    call check(nf90_inq_varid(ncid_init,"time_since_last_salting",varid))
    call check(nf90_get_var(ncid_init,varid, time_since_last_salting(ro),start=(/ro_tot/)))

    call check(nf90_inq_varid(ncid_init,"time_since_last_sanding",varid))
    call check(nf90_get_var(ncid_init,varid, time_since_last_sanding(ro),start=(/ro_tot/)))

    call check(nf90_inq_varid(ncid_init,"time_since_last_cleaning",varid))
    call check(nf90_get_var(ncid_init,varid, time_since_last_cleaning(ro),start=(/ro_tot/)))

    call check(nf90_inq_varid(ncid_init,"time_since_last_ploughing",varid))
    call check(nf90_get_var(ncid_init,varid, time_since_last_ploughing(ro),start=(/ro_tot/)))

    !Do not overwrite the observed surface temperature with the init file, bc. it has already been set to observations in 
    !NORTRIP_multiroad_save_meteodata.f90
    do i = 1,num_road_meteo
        if (i .ne. road_temperature_obs_index) then
            road_meteo_data(i,1,1,0) = road_meteo_data_tmp(i,1,1,0)
        endif
    end do

    !Check for NaNs in the input data
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
        
        if (input_is_nan) then
            write(unit_logfile,'(A)') 'ERROR reading init file (NORTRIP_read_init_data_netcdf). NaN values in data. Stopping'
            stop 3
        endif
        
    enddo

end subroutine NORTRIP_read_init_data_netcdf
!===========================================================================