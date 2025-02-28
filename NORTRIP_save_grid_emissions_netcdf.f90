!NORTRIP_save_gridded_emissions_netcdf.f90
        
!==========================================================================
!   NORTRIP model NORTRIP_save_gridded_emissions_netcdf
!==========================================================================


subroutine NORTRIP_save_gridded_emissions_netcdf
    
    use netcdf
    use NORTRIP_definitions
    
    implicit none
    
    !LOCAL
    integer :: ncid 

    integer :: varid
    integer :: t_dimid
    integer :: x_dimid
    integer :: y_dimid
    integer :: char_dimid
    
    character(256)      :: filename_gridded

    real                :: timestamp
    integer             :: a(num_date_index)
    character(len=256)  :: history_string
    character(8)        :: date
    character(10)       :: time
    character(5)        :: zone

    real, dimension(grid_dim(2)) :: latitude_array
    real, dimension(grid_dim(1)) :: longitude_array
    
    !TODO: The files all appear in /summary directory now. Should modify the paths here. 
    filename_gridded    = trim(path_output_emis)//trim(filename_output_grid_emis)//'_gridded.nc'
    !Check that path exists after filling in date stamp
    a=date_data(:,min_time_save)

    !Put in date if required
    call date_to_datestr_bracket(a,filename_gridded,filename_gridded)
    call date_to_datestr_bracket(a,filename_gridded,filename_gridded)
    call date_to_datestr_bracket(a,filename_gridded,filename_gridded)

    write(unit_logfile,'(A)') '================================================================'
    write(unit_logfile,'(A)') 'Create netcdf output file(s)'
    write(unit_logfile,'(A)') '================================================================'

    
    call check(nf90_create(trim(filename_gridded),IOR(NF90_NETCDF4, NF90_CLOBBER),ncid))
    
    !Add global attributes to output files: 
    call date_and_time(date = date,time=time,zone=zone)    
    history_string = "Created at: "//date//" "//time(1:2)//":"//time(3:4)//zone//"UTC"
    
    call check(nf90_put_att(ncid,nf90_global,"title","NORTRIP output"))
    call check(nf90_put_att(ncid,nf90_global,"history",trim(history_string)))
    call check(nf90_put_att(ncid,nf90_global,"institution","Norwegian Meteorological Institute, MET Norway"))
    
    !call check(nf90_put_att(ncid,nf90_global,"Conventions","CF 1.10")) !TODO: Check which convention should be followed.
    
    !Add dimensions:
    call check(nf90_def_dim(ncid,"time", nf90_unlimited, t_dimid))
    call check(nf90_def_dim(ncid,"longitude",grid_dim(1), x_dimid))
    call check(nf90_def_dim(ncid,"latitude",grid_dim(2), y_dimid))
    call check(nf90_def_dim(ncid,"maxcharlength", 256 , char_dimid))

    !Define variables:
    call check(nf90_def_var(ncid, "time", nf90_float, t_dimid,varid))
    call check(nf90_put_att(ncid,varid, "units", "seconds since "//trim(date_str(4,min_time)))) !Time dimension as seconds since start of simulation.
    call check(nf90_put_att(ncid,varid, "calendar", "standard"))
    call check(nf90_put_att(ncid,varid, "long_name", "time"))
    
    call check(nf90_def_var(ncid, "datetime", nf90_char, (/char_dimid,t_dimid/),varid))
    call check(nf90_put_att(ncid,varid, "description", "date and time in format yyyy.mm.dd HH:MM:SS"))
    call check(nf90_put_att(ncid,varid, "long_name", "yyyy.mm.dd_HH:MM:SS"))

    call check(nf90_def_var(ncid, "longitude", nf90_float, x_dimid,varid))
    call check(nf90_put_att(ncid,varid, "units", "degree_east"))
    call check(nf90_put_att(ncid,varid, "long_name", "")) 
    call check(nf90_put_att(ncid,varid, "standard_name",""))

    call check(nf90_def_var(ncid, "latitude", nf90_float, y_dimid,varid))
    call check(nf90_put_att(ncid,varid, "units", "degree_north"))
    call check(nf90_put_att(ncid,varid, "long_name", ""))
    call check(nf90_put_att(ncid,varid, "standard_name",""))

    call check(nf90_def_var(ncid, "emissions_PMco_tot", nf90_float, (/x_dimid,y_dimid, t_dimid/),varid))
    call check(nf90_put_att(ncid, varid, "units", "g/h"))
    call check(nf90_put_att(ncid, varid, "long_name", "emissions"))
    call check(nf90_put_att(ncid, varid, "standard_name", "emissions"))

    call check(nf90_def_var(ncid, "emissions_PM25_tot", nf90_float, (/x_dimid,y_dimid, t_dimid/),varid))
    call check(nf90_put_att(ncid, varid, "units", "g/h"))

    call check(nf90_def_var(ncid, "emissions_PM10_tot", nf90_float, (/x_dimid,y_dimid, t_dimid/),varid))
    call check(nf90_put_att(ncid, varid, "units", "g/h"))
    call check(nf90_put_att(ncid, varid, "long_name", "emissions"))
    call check(nf90_put_att(ncid, varid, "standard_name", "emissions"))

    call check(nf90_def_var(ncid, "emissions_EP", nf90_float, (/x_dimid,y_dimid, t_dimid/),varid))
    call check(nf90_put_att(ncid, varid, "units", "g/h"))
    call check(nf90_put_att(ncid, varid, "long_name", "emissions exhaust"))
    call check(nf90_put_att(ncid, varid, "standard_name", "emissions"))

    call check(nf90_def_var(ncid, "emissions_NOX_exhaust", nf90_float, (/x_dimid,y_dimid, t_dimid/),varid))
    call check(nf90_put_att(ncid, varid, "units", "g/h"))
    call check(nf90_put_att(ncid, varid, "long_name", "emissions"))
    call check(nf90_put_att(ncid, varid, "standard_name", "emissions"))


    call check(nf90_def_var(ncid, "emissions_PMco_tyre", nf90_float, (/x_dimid,y_dimid, t_dimid/),varid))
    call check(nf90_put_att(ncid, varid, "units", "g/h"))
    call check(nf90_put_att(ncid, varid, "long_name", "emissions"))
    call check(nf90_put_att(ncid, varid, "standard_name", "emissions"))


    call check(nf90_def_var(ncid, "emissions_PMco_brake", nf90_float, (/x_dimid,y_dimid, t_dimid/),varid))
    call check(nf90_put_att(ncid, varid, "units", "g/h"))
    call check(nf90_put_att(ncid, varid, "long_name", "emissions"))
    call check(nf90_put_att(ncid, varid, "standard_name", "emissions"))


    call check(nf90_def_var(ncid, "emissions_PMco_sand", nf90_float, (/x_dimid,y_dimid, t_dimid/),varid))
    call check(nf90_put_att(ncid, varid, "units", "g/h"))
    call check(nf90_put_att(ncid, varid, "long_name", "emissions"))
    call check(nf90_put_att(ncid, varid, "standard_name", "emissions"))
    call check(nf90_put_att(ncid, varid, "coordinates", "longitude latitude"))

    call check(nf90_def_var(ncid, "emissions_PMco_salt", nf90_float, (/x_dimid,y_dimid, t_dimid/),varid))
    call check(nf90_put_att(ncid, varid, "units", "g/h"))
    call check(nf90_put_att(ncid, varid, "long_name", "emissions"))
    call check(nf90_put_att(ncid, varid, "standard_name", "emissions"))


    call check(nf90_def_var(ncid, "emissions_PMco_road", nf90_float, (/x_dimid,y_dimid, t_dimid/),varid))
    call check(nf90_put_att(ncid, varid, "units", "g/h"))
    call check(nf90_put_att(ncid, varid, "long_name", "emissions"))
    call check(nf90_put_att(ncid, varid, "standard_name", "emissions"))

    call check(nf90_def_var(ncid, "emissions_PM25_tyre", nf90_float, (/x_dimid,y_dimid, t_dimid/),varid))
    call check(nf90_put_att(ncid, varid, "units", "g/h"))
    call check(nf90_put_att(ncid, varid, "long_name", "emissions"))
    call check(nf90_put_att(ncid, varid, "standard_name", "emissions"))

    call check(nf90_def_var(ncid, "emissions_PM25_brake", nf90_float, (/x_dimid,y_dimid, t_dimid/),varid))
    call check(nf90_put_att(ncid, varid, "units", "g/h"))
    call check(nf90_put_att(ncid, varid, "long_name", "emissions"))
    call check(nf90_put_att(ncid, varid, "standard_name", "emissions"))

    call check(nf90_def_var(ncid, "emissions_PM25_sand", nf90_float, (/x_dimid,y_dimid, t_dimid/),varid))
    call check(nf90_put_att(ncid, varid, "units", "g/h"))
    call check(nf90_put_att(ncid, varid, "long_name", "emissions"))
    call check(nf90_put_att(ncid, varid, "standard_name", "emissions"))

    call check(nf90_def_var(ncid, "emissions_PM25_salt", nf90_float, (/x_dimid,y_dimid, t_dimid/),varid))
    call check(nf90_put_att(ncid, varid, "units", "g/h"))
    call check(nf90_put_att(ncid, varid, "long_name", "emissions"))
    call check(nf90_put_att(ncid, varid, "standard_name", "emissions"))


    call check(nf90_def_var(ncid, "emissions_PM25_road", nf90_float, (/x_dimid,y_dimid, t_dimid/),varid))
    call check(nf90_put_att(ncid, varid, "units", "g/h"))
    call check(nf90_put_att(ncid, varid, "long_name", "emissions"))
    call check(nf90_put_att(ncid, varid, "standard_name", "emissions"))

    
    
    do j=0,grid_dim(2)-1
        latitude_array(j+1)=grid_0(2)+grid_delta(2)*j
    end do 

    do i=0,grid_dim(1)-1

        longitude_array(i+1)=grid_0(1)+grid_delta(1)*i
    end do 
    
        call check(nf90_inq_varid(ncid,"longitude",varid))
        call check(nf90_put_var(ncid,varid,longitude_array,start = (/1/)))
    
        call check(nf90_inq_varid(ncid,"latitude",varid))
        call check(nf90_put_var(ncid,varid,latitude_array,start = (/1/)))

    !Put values into the variables that will be equal for all output files.

    ! !Calculate time as seconds since base date
    timestamp=0
    do ti=min_time_save,max_time_save   
        timestamp = (ti-1)*dt*60*60
        call check(nf90_inq_varid(ncid, "time",varid))
        call check(nf90_put_var(ncid, varid, timestamp, start = (/ti/)))

        call check(nf90_inq_varid(ncid, "datetime",varid))
        call check(nf90_put_var(ncid, varid, trim(date_str(4,ti)),start = (/1,ti/)))        
    enddo

    call check(nf90_inq_varid(ncid,"emissions_PM10_tot",varid))
    call check(nf90_put_var(ncid,varid,emis_grid(:,:,pm_10,:),start = (/1,1,1/)))

    call check(nf90_inq_varid(ncid,"emissions_PM25_tot",varid))
    call check(nf90_put_var(ncid,varid,emis_grid(:,:,pm_25,:),start = (/1,1,1/)))

    call check(nf90_inq_varid(ncid,"emissions_PMco_tot",varid))
    call check(nf90_put_var(ncid,varid,emis_grid(:,:,pm_co_tot,:),start = (/1,1,1/)))

    call check(nf90_inq_varid(ncid,"emissions_EP",varid))
    call check(nf90_put_var(ncid,varid,emis_grid(:,:,pm_exhaust,:),start = (/1,1,1/)))

    call check(nf90_inq_varid(ncid,"emissions_NOX_exhaust",varid))
    call check(nf90_put_var(ncid,varid,emis_grid(:,:,nox_exhaust,:),start = (/1,1,1/)))
    
    call check(nf90_inq_varid(ncid,"emissions_PMco_tyre",varid))
    call check(nf90_put_var(ncid,varid,emis_grid(:,:,pm_co_tyre,:),start = (/1,1,1/)))

    call check(nf90_inq_varid(ncid,"emissions_PMco_salt",varid))
    call check(nf90_put_var(ncid,varid,emis_grid(:,:,pm_co_salt1,:),start = (/1,1,1/)))


    call check(nf90_inq_varid(ncid,"emissions_PMco_brake",varid))
    call check(nf90_put_var(ncid,varid,emis_grid(:,:,pm_co_brake,:),start = (/1,1,1/)))


    call check(nf90_inq_varid(ncid,"emissions_PMco_road",varid))
    call check(nf90_put_var(ncid,varid,emis_grid(:,:,pm_co_road,:),start = (/1,1,1/)))


    call check(nf90_inq_varid(ncid,"emissions_PMco_sand",varid))
    call check(nf90_put_var(ncid,varid,emis_grid(:,:,pm_co_sand,:),start = (/1,1,1/)))

    call check(nf90_inq_varid(ncid,"emissions_PM25_tyre",varid))
    call check(nf90_put_var(ncid,varid,emis_grid(:,:,pm_25_tyre,:),start = (/1,1,1/)))

    call check(nf90_inq_varid(ncid,"emissions_PM25_salt",varid))
    call check(nf90_put_var(ncid,varid,emis_grid(:,:,pm_25_salt1,:),start = (/1,1,1/)))


    call check(nf90_inq_varid(ncid,"emissions_PM25_brake",varid))
    call check(nf90_put_var(ncid,varid,emis_grid(:,:,pm_25_brake,:),start = (/1,1,1/)))


    call check(nf90_inq_varid(ncid,"emissions_PM25_road",varid))
    call check(nf90_put_var(ncid,varid,emis_grid(:,:,pm_25_road,:),start = (/1,1,1/)))


    call check(nf90_inq_varid(ncid,"emissions_PM25_sand",varid))
    call check(nf90_put_var(ncid,varid,emis_grid(:,:,pm_25_sand,:),start = (/1,1,1/)))


    write(*,*) "Closing netcdf grid output file."
    call check(nf90_close(ncid)) 
end subroutine NORTRIP_save_gridded_emissions_netcdf

!===========================================================================
