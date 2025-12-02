
!===========Create and write init files=====================================

subroutine NORTRIP_create_init_netcdf(filename)

    use netcdf
    use NORTRIP_definitions

    implicit none

    !INPUT
    character(256), intent(in)      :: filename

    !OUTPUT
    integer :: ncid 

    !LOCAL
    integer :: varid
    integer :: sourceid, varnumid, sizeid,roadid, trackid,moistid
    integer :: a(num_date_index)


    call check(nf90_create(trim(filename),nf90_clobber,ncid))

    call check(nf90_def_dim(ncid,"road_id", n_roads_total , roadid))

    call check(nf90_def_dim(ncid,"num_track", num_track , trackid))
    call check(nf90_def_dim(ncid,"num_size", num_size , sizeid))
    call check(nf90_def_dim(ncid,"num_source_all", num_source_all , sourceid))
    call check(nf90_def_dim(ncid,"num_var", num_road_meteo , varnumid))
    call check(nf90_def_dim(ncid,"num_moisture", num_moisture , moistid))

    call check(nf90_def_var(ncid, "M_road_data", nf90_float, (/sourceid,sizeid,trackid,roadid/),varid))
    call check(nf90_put_att(ncid,varid,"description","Mass data array"))

    call check(nf90_def_var(ncid, "road_meteo_data", nf90_float, (/varnumid,trackid,roadid/),varid))
    call check(nf90_put_att(ncid,varid,"description","road meteo data array"))

    call check(nf90_def_var(ncid, "g_road_data", nf90_float, (/moistid,trackid,roadid/),varid))
    call check(nf90_put_att(ncid,varid,"description","g_road_data array"))

    call check(nf90_def_var(ncid = ncid, name = "time_since_last_salting", xtype=nf90_float,dimids=roadid, varid=varid))
    call check(nf90_def_var(ncid = ncid, name = "time_since_last_binding", xtype=nf90_float,dimids=roadid, varid=varid))
    call check(nf90_def_var(ncid = ncid, name = "time_since_last_sanding", xtype=nf90_float,dimids=roadid, varid=varid))
    call check(nf90_def_var(ncid = ncid, name = "time_since_last_cleaning", xtype=nf90_float,dimids=roadid, varid=varid))
    call check(nf90_def_var(ncid = ncid, name = "time_since_last_ploughing", xtype=nf90_float,dimids=roadid, varid=varid))
    call check(nf90_enddef(ncid))
end subroutine NORTRIP_create_init_netcdf

subroutine NORTRIP_fill_init_data_array
    use NORTRIP_definitions
    
    implicit none

    integer :: hour_test
    tr=1

    !Leave this if it is not relevant
    if (hours_between_init.lt.0) then
        if (ro_tot.eq.1) then
            write(unit_logfile,'(A)') ' WARNING: Not saving data to init netcdf file for single road loop'
        endif
        return
    endif

    hour_test=1
    if (hours_between_init.ne.0) hour_test=mod(tf,hours_between_init)

    if (index(calculation_type,'Avinor').gt.0 .and. tf.eq.ceiling(1/dt)  .or. &
        .not. index(calculation_type,'Avinor').gt.0 .and. hour_test.eq.0 .or. &
        .not. index(calculation_type,'Avinor').gt.0 .and. tf.eq.max_time ) then
            
        if (ro_tot ==1) then   
            init_file_counter = init_file_counter+1
            save_init_instances(init_file_counter)%date=date_data(:,tf)
            if(.not.allocated(save_init_instances(init_file_counter)%g_road_data))      allocate(save_init_instances(init_file_counter)%g_road_data(num_moisture,num_track,n_roads_total))
            if(.not.allocated(save_init_instances(init_file_counter)%M_road_data_))     allocate(save_init_instances(init_file_counter)%M_road_data_(num_source_all,num_size,num_track,n_roads_total))
            if(.not.allocated(save_init_instances(init_file_counter)%road_meteo_data))  allocate(save_init_instances(init_file_counter)%road_meteo_data(num_road_meteo,num_track,n_roads_total))
            if(.not.allocated(save_init_instances(init_file_counter)%time_since_binding)) allocate(save_init_instances(init_file_counter)%time_since_binding(n_roads_total))
            if(.not.allocated(save_init_instances(init_file_counter)%time_since_salting)) allocate(save_init_instances(init_file_counter)%time_since_salting(n_roads_total))
            if(.not.allocated(save_init_instances(init_file_counter)%time_since_ploughing)) allocate(save_init_instances(init_file_counter)%time_since_ploughing(n_roads_total))
            if(.not.allocated(save_init_instances(init_file_counter)%time_since_cleaning))  allocate(save_init_instances(init_file_counter)%time_since_cleaning(n_roads_total))
            if(.not.allocated(save_init_instances(init_file_counter)%time_since_sanding))   allocate(save_init_instances(init_file_counter)%time_since_sanding(n_roads_total))
        end if

        save_init_instances(init_file_counter)%M_road_data_(:,:,:,ro_tot)    = M_road_data(:,:,tf,:,ro)
        save_init_instances(init_file_counter)%g_road_data(:,:,ro_tot)      = g_road_data(:,tf,:,ro)
        save_init_instances(init_file_counter)%road_meteo_data(:,:,ro_tot)  = road_meteo_data(:,tf,:,ro)
        save_init_instances(init_file_counter)%time_since_binding(ro_tot)   = time_since_last_binding(ro)
        save_init_instances(init_file_counter)%time_since_salting(ro_tot)   = time_since_last_salting(ro)
        save_init_instances(init_file_counter)%time_since_ploughing(ro_tot) = time_since_last_ploughing(ro)
        save_init_instances(init_file_counter)%time_since_cleaning(ro_tot)  = time_since_last_cleaning(ro)
        save_init_instances(init_file_counter)%time_since_sanding(ro_tot)   = time_since_last_sanding(ro)
    end if
    
end subroutine NORTRIP_fill_init_data_array

subroutine NORTRIP_save_init_data_netcdf
    
    use netcdf
    use NORTRIP_definitions
    
    implicit none
    
    !LOCAL
    integer :: varid
    integer :: t_dimid
    integer :: f_dimid
    logical :: exists
    real :: timestamp
    character(len=256)      :: filename

    integer :: a(num_date_index)
    character (256) temp_name
    integer current_date(num_date_index)
    integer :: ncid 

    write(unit_logfile,'(A)') '================================================================'
    write(unit_logfile,'(A)') 'Saving init file as netcdf (NORTRIP_save_init_data_netcdf)'
    write(unit_logfile,'(A)') '================================================================'


    !Leave this if it is not relevant !NOTE Don't really understand this, copied from NORTRIP_save_init_single.f90
    if (hours_between_init.lt.0) then
        if (ro_tot.eq.1) then
            write(unit_logfile,'(A)') ' WARNING: Not saving data to init netcdf file for single road loop'
        endif
        return
    endif

    a=date_data(:,min_time_save)
    call date_to_datestr_bracket(a,path_init,temp_name)
    call date_to_datestr_bracket(a,temp_name,temp_name)
    call date_to_datestr_bracket(a,temp_name,temp_name) 

    inquire(directory=trim(temp_name),exist=exists)
    if (.not.exists) then
        write(unit_logfile,*)'ERROR: Path '//trim(temp_name)//' does not exist.'
        return
    endif

    do i=1,size(save_init_instances)

        !If not allocated that means that there is no more init data in the array, so returns without creating any file.
        if (.not.allocated(save_init_instances(i)%M_road_data_)) return 
        
        current_date = save_init_instances(i)%date 
        
        filename =trim(temp_name)//trim(filename_outputdata)//'_init.nc'
        call date_to_datestr_bracket(current_date,filename,filename)
        call date_to_datestr_bracket(current_date,filename,filename)
        call date_to_datestr_bracket(current_date,filename,filename)

        call NORTRIP_create_init_netcdf(filename)
        call check(nf90_open(filename,nf90_write,ncid))

        !NOTE: Track is always = 1. If the model code is extended to include more than one track, this must be changed.
        tr=1

        !NOTE: No formating is done except defining the netcdf variables as nf90_float
        call check(nf90_inq_varid(ncid, "M_road_data",varid))
        call check(nf90_put_var(ncid, varid, save_init_instances(i)%M_road_data_))
        
        call check(nf90_inq_varid(ncid, "g_road_data",varid))
        call check(nf90_put_var(ncid, varid, save_init_instances(i)%g_road_data,start=(/1,1,1/)))
        call check(nf90_inq_varid(ncid, "road_meteo_data",varid))
        call check(nf90_put_var(ncid, varid, save_init_instances(i)%road_meteo_data,start=(/1,1,1/)))
        call check(nf90_inq_varid(ncid, "time_since_last_salting",varid))
        call check(nf90_put_var(ncid, varid, save_init_instances(i)%time_since_salting,start=(/1/)))

        call check(nf90_inq_varid(ncid, "time_since_last_binding",varid))
        call check(nf90_put_var(ncid, varid, save_init_instances(i)%time_since_binding,start=(/1/)))
        
        call check(nf90_inq_varid(ncid, "time_since_last_sanding",varid))
        call check(nf90_put_var(ncid, varid, save_init_instances(i)%time_since_sanding,start=(/1/)))

        call check(nf90_inq_varid(ncid, "time_since_last_cleaning",varid))
        call check(nf90_put_var(ncid, varid, save_init_instances(i)%time_since_cleaning,start=(/1/)))

        call check(nf90_inq_varid(ncid, "time_since_last_ploughing",varid))
        call check(nf90_put_var(ncid, varid, save_init_instances(i)%time_since_ploughing,start=(/1/)))

        call check(nf90_close(ncid))
    end do
end subroutine NORTRIP_save_init_data_netcdf

!===========================================================================
