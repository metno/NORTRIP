subroutine NORTRIP_save_forecast(timestep,forecast_temperature)
    use netcdf
    use NORTRIP_definitions


    implicit none

    !input variables
    integer, intent(in) :: timestep
    real, intent(in),dimension(forecast_hour) :: forecast_temperature

    !local variables
    integer :: varid
    integer :: ncid 
    integer :: t_dimid
    integer :: f_dimid
    integer :: exists
    !character, parameter :: fmt = '(I5.5)' ! an integer of width 5 with zeros at the left
    character(len=8) :: date_string
    character(len=4) :: time_string
    character(len=12) :: datetime_string
    integer :: datetime_int
    character(256) :: filename

! FROM CTSM
!     call get_ref_date(yr, mon, day, nbsec)
!     nstep = get_nstep()
!     hours   = nbsec / 3600
!     minutes = (nbsec - hours*3600) / 60
!     secs    = (nbsec - hours*3600 - minutes*60)
!     write(basedate,80) yr,mon,day
! 80     format(i4.4,'-',i2.2,'-',i2.2)
!     write(basesec ,90) hours, minutes, secs
! 90     format(i2.2,':',i2.2,':',i2.2)

!     dim1id(1) = time_dimid
!     str = 'days since ' // basedate // " " // basesec
!     call ncd_defvar(nfid(t), 'time', tape(t)%ncprec, 1, dim1id, varid, &
!          long_name='time',units=str)

    ! time = mdcur + mscur/secspday
    ! call ncd_io('time'  , time  , 'write', nfid(t), nt=tape(t)%ntimes) 

    !mscur: current seconds of current day
   
    write (date_string,'(I4.4, I2.2, I2.2)') date_data(year_index,timestep), date_data(month_index,timestep), date_data(day_index,timestep)  ! converting integer to string using a 'internal file'

    filename = trim("/lustre/storeB/project/fou/kl/NORTRIP_Avinor/Runways_2/E_corr_tests/NORTRIP_test/NORTRIP_netcdf/"//date_string//".nc")


    write (datetime_string, '(I4.4, I2.2, I2.2, I2.2, I2.2)') date_data(year_index,timestep), date_data(month_index,timestep), date_data(day_index,timestep),date_data(hour_index,timestep), date_data(minute_index,timestep)


    read(datetime_string,*) datetime_int

    inquire(file=trim(filename),exist=exists)
    if (.not.exists) then
        call check(nf90_create(filename,nf90_netcdf4,ncid))
        call check(nf90_def_dim(ncid,"time", nf90_unlimited, t_dimid))
        call check(nf90_def_dim(ncid,"forecast_hour", forecast_hour, f_dimid))
        call check(nf90_def_var(ncid, "Ts_forecast", nf90_double, (/f_dimid,t_dimid/),varid))
        call check(nf90_def_var(ncid, "Ts_observed", nf90_double, (/t_dimid/),varid))
        call check(nf90_put_att(ncid,varid, "unit", "Celsius"))
        call check(nf90_def_var(ncid, "Year", nf90_double, (/t_dimid/),varid))
        call check(nf90_def_var(ncid, "Month", nf90_double, (/t_dimid/),varid))
        call check(nf90_def_var(ncid, "Day", nf90_double, (/t_dimid/),varid))
        call check(nf90_def_var(ncid, "Hour", nf90_double, (/t_dimid/),varid))
        call check(nf90_def_var(ncid, "time", nf90_int, (/t_dimid/),varid))

        call check(nf90_enddef(ncid))
    else 
        call check(nf90_open(filename,nf90_write,ncid))
    end if 

    call check(nf90_inq_varid(ncid, "Ts_forecast",varid))
    call check(nf90_put_var(ncid, varid, forecast_temperature, start = (/1,timestep/)))

    call check(nf90_inq_varid(ncid, "Ts_observed",varid))
    call check(nf90_put_var(ncid, varid, road_meteo_data(road_temperature_obs_index,tf,1,ro), start = (/timestep/))) !TODO: Since this subroutine is called outside of the tr loop, I've set tr=1 (since it always is). If the code is updated to more than one tracks, this needs to change.  

    call check(nf90_inq_varid(ncid, "Year",varid))
    call check(nf90_put_var(ncid, varid, date_data(year_index,timestep),start=(/timestep/)))
    call check(nf90_inq_varid(ncid, "Month",varid))
    call check(nf90_put_var(ncid, varid, date_data(month_index,timestep), start = (/timestep/)))
    call check(nf90_inq_varid(ncid, "Day",varid))
    call check(nf90_put_var(ncid, varid, date_data(day_index,timestep),start=(/timestep/)))
    
    call check(nf90_inq_varid(ncid, "time",varid))
    call check(nf90_put_var(ncid, varid, datetime_int,start=(/timestep/)))

    call check(nf90_inq_varid(ncid, "Hour",varid))
    call check(nf90_put_var(ncid, varid, timestep, start = (/timestep/) ))    
    call check(nf90_close(ncid))
    
end subroutine NORTRIP_save_forecast
