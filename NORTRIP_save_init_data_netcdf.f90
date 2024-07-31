
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
    integer :: t_dimid
    integer :: sourceid, varnumid, sizeid,roadid, trackid,moistid
    integer :: date_dimid
    integer :: exists
    character(len=4)    :: time_string
    character(len=12)   :: datetime_string
    integer             :: datetime_int
    integer :: a(num_date_index)


    call check(nf90_create(trim(filename),nf90_netcdf4,ncid))

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

subroutine NORTRIP_save_init_data_netcdf
    
    use netcdf
    use NORTRIP_definitions
    
    implicit none
    
    !LOCAL
    integer :: varid
    integer :: ncid 
    integer :: t_dimid
    integer :: f_dimid
    integer :: exists
    real :: timestamp
    character(len=4)    :: time_string
    character(len=12)   :: datetime_string
    integer             :: datetime_int
    character(len=256)      :: filename
    character(len=19) :: datetime

    integer :: a(num_date_index)
    integer :: hour_test
    character (256) temp_name
    integer current_date(num_date_index)

        !Leave this if it is not relevant !NOTE Don't really understand this, copied from NORTRIP_save_init_single.f90
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

        !TODO: THe first condition is to write an init file from the last timestep 
        !with observations in the avinor forecasts. Should maybe be turned into a flag?
        !Check that path exists after filling in date stamp
        a=date_data(:,min_time_save)
        call date_to_datestr_bracket(a,path_init,temp_name)
        call date_to_datestr_bracket(a,temp_name,temp_name)
        call date_to_datestr_bracket(a,temp_name,temp_name)

        inquire(directory=trim(temp_name),exist=exists)
        if (.not.exists) then
            write(unit_logfile,*)'ERROR: Path '//trim(temp_name)//' does not exist.'
            return
        endif

        current_date=date_data(:,tf)

        filename =trim(temp_name)//trim(filename_outputdata)//'_init.nc'
        call date_to_datestr_bracket(current_date,filename,filename)
        call date_to_datestr_bracket(current_date,filename,filename)
        call date_to_datestr_bracket(current_date,filename,filename)

        !If file do not exist,  create and open, otherwise just open. 
        inquire(file=trim(filename),exist=exists)
        if (.not.exists) then

            call NORTRIP_create_init_netcdf(filename)

            call check(nf90_open(filename,nf90_write,ncid))
            write(unit_logfile,*) "Creating and opening init file to save values: ", trim(filename)
            else
                call check(nf90_open(filename,nf90_write,ncid))
        end if        
        !NOTE: Track is always = 1. If the model code is extended to include more than one track, this must be changed.
        tr=1

        !Fill netcdf file with variables. NOTE: This is assuming that single road flag is used (It is a bit confusing that the iterator is called ro_tot...)
        !TODO: No formating is done except defining the netcdf variables as nf90_float. Should be put on approproate format when the file is read.

        call check(nf90_inq_varid(ncid, "M_road_data",varid))
        call check(nf90_put_var(ncid, varid, M_road_data(:,:,tf,tr,0),start = (/1,1,1,ro_tot/)))
        
        call check(nf90_inq_varid(ncid, "road_meteo_data",varid))
        call check(nf90_put_var(ncid, varid, road_meteo_data(:,tf,tr,0),start=(/1,1,ro_tot/)))

        call check(nf90_inq_varid(ncid, "g_road_data",varid))
        call check(nf90_put_var(ncid, varid, g_road_data(:,tf,tr,0),start=(/1,1,ro_tot/)))
        
        call check(nf90_inq_varid(ncid, "time_since_last_salting",varid))
        call check(nf90_put_var(ncid, varid, time_since_last_salting(ro),start=(/ro_tot/)))

        call check(nf90_inq_varid(ncid, "time_since_last_binding",varid))
        call check(nf90_put_var(ncid, varid, time_since_last_binding(ro),start=(/ro_tot/)))
        
        call check(nf90_inq_varid(ncid, "time_since_last_sanding",varid))
        call check(nf90_put_var(ncid, varid, time_since_last_sanding(ro),start=(/ro_tot/)))
        
        call check(nf90_inq_varid(ncid, "time_since_last_cleaning",varid))
        call check(nf90_put_var(ncid, varid, time_since_last_cleaning(ro),start=(/ro_tot/)))

        call check(nf90_inq_varid(ncid, "time_since_last_ploughing",varid))
        call check(nf90_put_var(ncid, varid, time_since_last_ploughing(ro),start=(/ro_tot/)))


        call check(nf90_close(ncid))
    end if
end subroutine NORTRIP_save_init_data_netcdf

!===========================================================================
