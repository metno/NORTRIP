!NORTRIP_inputparams_subroutines.f90
!----------------------------------------------------------------------
    subroutine read_NORTRIP_parameters

    use NORTRIP_definitions
    
    implicit none
    
    character(256) temp_path
    character(256) temp_file
    character(256) temp_name
    character(256) temp_str,temp_str1,temp_str2,temp_str3
    character(256) read_str(2)
    integer unit_in,unit_logfile_temp
    integer index_val,index_temp
    logical exists
    real include_track_temp,f_track_temp(10)
   
    !Functions
    character(256) match_string_char
    real read_string_val
    real match_string_val
        
    unit_in=unit_read_NORTRIP_parameters
    unit_logfile_temp=unit_logfile !use -1 not to write anything, 0 to go to screen
    
    !Open pathname file for reading
    temp_name=trim(path_inputparam)//trim(filename_inputparam)//'_params.txt'
    inquire(file=temp_name,exist=exists)
    if (.not.exists) then
        write(unit_logfile_temp,*)'ERROR: '//trim(temp_name)//' does not exist.'
        return
    endif
    open(unit_in,file=temp_name,access='sequential',status='old',readonly)

    !Open log file
    !write(*,*) 'LOG NAME:',trim(filename_log)
    if (unit_logfile.gt.0) then
        open(unit_logfile,file=filename_log,status='old',position='append')
    endif
   
	write(unit_logfile_temp,'(A)') '================================================================'
	write(unit_logfile_temp,'(A)') 'Reading model parameters (read_NORTRIP_parameters)'
	write(unit_logfile_temp,'(A)') '================================================================'
    
    !Read wear parameters
    do s=1,num_wear
        temp_str=''
        temp_str1=''
        if (s.eq.road_index) then
            temp_str1='Road wear'
        elseif (s.eq.tyre_index) then
            temp_str1='Tyre wear'
        elseif (s.eq.brake_index) then
            temp_str1='Brake wear'
        endif
        rewind(unit_in)
        do while (index(temp_str,trim(temp_str1)).eq.0)
            read(unit_in,'(a)',ERR=10) temp_str
            !write(unit_logfile,*) trim(temp_str)
	    end do
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	write(unit_logfile,'(A)') trim(temp_str)
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	    read(unit_in,*,ERR=10) temp_str !Skip line
        call read_line_val3(unit_in,unit_logfile,W_0(s,st,he),W_0(s,wi,he),W_0(s,su,he))
        call read_line_val3(unit_in,unit_logfile,W_0(s,st,li),W_0(s,wi,li),W_0(s,su,li))
	    read(unit_in,*,ERR=10) temp_str !Skip line
        call read_line_val5(unit_in,unit_logfile,a_wear(s,1),a_wear(s,2),a_wear(s,3),a_wear(s,4),a_wear(s,5))
 	enddo

    !Snow depth wear threshold
    rewind(unit_in)
    temp_str=''
    temp_str1='Snow depth wear threshold'
    do while (index(temp_str,trim(temp_str1)).eq.0)
        read(unit_in,'(a)',ERR=10) temp_str
        !write(unit_logfile,*) trim(temp_str)
	end do
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	write(unit_logfile,'(A)') trim(temp_str)
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
    read(unit_in,*,ERR=10) temp_str !Skip line
    call read_line_val1(unit_in,unit_logfile,s_roadwear_thresh)

    !Pavement type scaling factor
    rewind(unit_in)
    temp_str=''
    temp_str1='Pavement type scaling factor'
    do while (index(temp_str,trim(temp_str1)).eq.0)
        read(unit_in,'(a)',ERR=10) temp_str
	end do
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	write(unit_logfile,'(A)') trim(temp_str)
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
    call read_line_int1(unit_in,unit_logfile,num_pave)
	read(unit_in,*,ERR=10) temp_str !Skip line
    do s=1,num_pave
        read(unit_in,'(a)',end=10) temp_str
        index_val=index(temp_str,achar(9))
        temp_str1=temp_str(1:index_val-1)
        read(temp_str1,*) index_temp
        temp_str=temp_str(index_val+1:)      
        index_val=index(temp_str,achar(9))
        temp_str2=temp_str(1:index_val-1)
        temp_str3=temp_str(index_val+1:)
        read(temp_str3,*) h_pave(s)
 	    write(unit_logfile,'(I2,A38,A3,e10.2)') index_temp,trim(temp_str2),' = ',h_pave(s)
    enddo
 
    
    !Driving cycle scaling factor
    rewind(unit_in)
    temp_str=''
    temp_str1='Driving cycle scaling factor'
    do while (index(temp_str,trim(temp_str1)).eq.0)
        read(unit_in,'(a)',ERR=10) temp_str
	end do
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	write(unit_logfile,'(A)') trim(temp_str)
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
    call read_line_int1(unit_in,unit_logfile,num_dc)
	read(unit_in,*,ERR=10) temp_str !Skip line
    do s=1,num_dc
        read(unit_in,'(a)',end=10) temp_str
        index_val=index(temp_str,achar(9))
        temp_str1=temp_str(1:index_val-1)
        read(temp_str1,*) index_temp
        temp_str=temp_str(index_val+1:)      
        index_val=index(temp_str,achar(9))
        temp_str2=temp_str(1:index_val-1)
        temp_str3=temp_str(index_val+1:)
        read(temp_str3,*) h_drivingcycle(s)
 	    write(unit_logfile,'(I2,A38,A3,e10.2)') index_temp,trim(temp_str2),' = ',h_drivingcycle(s)
	enddo

   !Suspension scaling factors
    rewind(unit_in)
    temp_str=''
    temp_str1='Suspension scaling factors'
    do while (index(temp_str,trim(temp_str1)).eq.0)
        read(unit_in,'(a)',ERR=10) temp_str
	end do
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	write(unit_logfile,'(A)') trim(temp_str)
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
    read(unit_in,*,ERR=10) temp_str !skip line
    do s=1,num_source
        call read_line_val4(unit_in,unit_logfile,h_0_sus(s,pm_all),h_0_sus(s,pm_200),h_0_sus(s,pm_10),h_0_sus(s,pm_25))
    enddo
        call read_line_val4(unit_in,unit_logfile,h_0_q_road(pm_all),h_0_q_road(pm_200),h_0_q_road(pm_10),h_0_q_road(pm_25))
 
    !Road suspension
    temp_str=''
    temp_str1='Road suspension'
    rewind(unit_in)
    do while (index(temp_str,trim(temp_str1)).eq.0)
        read(unit_in,'(a)',ERR=10) temp_str
	end do
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	write(unit_logfile,'(A)') trim(temp_str)
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	read(unit_in,*,ERR=10) temp_str !skip line
    call read_line_val3(unit_in,unit_logfile,f_0_suspension(1,1,st,he),f_0_suspension(1,1,wi,he),f_0_suspension(1,1,su,he))
    call read_line_val3(unit_in,unit_logfile,f_0_suspension(1,1,st,li),f_0_suspension(1,1,wi,li),f_0_suspension(1,1,su,li))
 	read(unit_in,*,ERR=10) temp_str !skip line
    call read_line_val5(unit_in,unit_logfile,a_sus(1),a_sus(2),a_sus(3),a_sus(4),a_sus(5))
	
    do s=1,num_source
    do x=1,num_size
    do t=1,num_tyre
    do v=1,num_veh
            f_0_suspension(s,x,t,v)=f_0_suspension(1,1,t,v)*h_0_sus(s,x)
    enddo
    enddo
    enddo
    enddo

    !Abrasion factor
    temp_str=''
    temp_str1='Abrasion factor'
    rewind(unit_in)
    do while (index(temp_str,trim(temp_str1)).eq.0)
        read(unit_in,'(a)',ERR=10) temp_str
	end do
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	write(unit_logfile,'(A)') trim(temp_str)
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	read(unit_in,*,ERR=10) temp_str !skip line
    call read_line_val3(unit_in,unit_logfile,f_0_abrasion(st,he),f_0_abrasion(wi,he),f_0_abrasion(su,he))
    call read_line_val3(unit_in,unit_logfile,f_0_abrasion(st,li),f_0_abrasion(wi,li),f_0_abrasion(su,li))
    call read_line_val1(unit_in,unit_logfile,V_ref_abrasion)
 	read(unit_in,*,ERR=10) temp_str !skip line
    call read_line_val4(unit_in,unit_logfile,h_0_abrasion(pm_all),h_0_abrasion(pm_200),h_0_abrasion(pm_10),h_0_abrasion(pm_25))
 
    !Crushing factor
    temp_str=''
    temp_str1='Crushing factor'
    rewind(unit_in)
    do while (index(temp_str,trim(temp_str1)).eq.0)
        read(unit_in,'(a)',ERR=10) temp_str
	end do
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	write(unit_logfile,'(A)') trim(temp_str)
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	read(unit_in,*,ERR=10) temp_str !skip line
    call read_line_val3(unit_in,unit_logfile,f_0_crushing(st,he),f_0_crushing(wi,he),f_0_crushing(su,he))
    call read_line_val3(unit_in,unit_logfile,f_0_crushing(st,li),f_0_crushing(wi,li),f_0_crushing(su,li))
    call read_line_val1(unit_in,unit_logfile,V_ref_crushing)
 	read(unit_in,*,ERR=10) temp_str !skip line
    call read_line_val4(unit_in,unit_logfile,h_0_crushing(pm_all),h_0_crushing(pm_200),h_0_crushing(pm_10),h_0_crushing(pm_25))


    !Sources participating in abrasion and crushing
    temp_str=''
    temp_str1='Sources participating in abrasion and crushing'
    rewind(unit_in)
    do while (index(temp_str,trim(temp_str1)).eq.0)
        read(unit_in,'(a)',ERR=10) temp_str
	end do
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	write(unit_logfile,'(A)') trim(temp_str)
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	read(unit_in,*,ERR=10) temp_str !skip line
	do s=1,num_source
        call read_line_val2(unit_in,unit_logfile,p_0_abrasion(s),p_0_crushing(s))
    enddo
    
    !Direct emission factor
    temp_str=''
    temp_str1='Direct emission factor'
    rewind(unit_in)
    do while (index(temp_str,trim(temp_str1)).eq.0)
        read(unit_in,'(a)',ERR=10) temp_str
	end do
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	write(unit_logfile,'(A)') trim(temp_str)
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	read(unit_in,*,ERR=10) temp_str !skip line
	do s=1,num_wear
        call read_line_val1(unit_in,unit_logfile,f_0_dir(s))
	enddo
    call read_line_val1(unit_in,unit_logfile,f_0_dir(crushing_index))
    call read_line_val1(unit_in,unit_logfile,f_0_dir(abrasion_index))
    call read_line_val1(unit_in,unit_logfile,f_0_dir(exhaust_index))


    !Fractional size distribution emissions
    temp_str=''
    temp_str1='Fractional size distribution'
    rewind(unit_in)
    do while (index(temp_str,trim(temp_str1)).eq.0)
        read(unit_in,'(a)',ERR=10) temp_str
	end do
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	write(unit_logfile,'(A)') trim(temp_str)
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	read(unit_in,*,ERR=10) temp_str !skip line
	do s=1,num_source
        call read_line_val4(unit_in,unit_logfile,f_PM(s,pm_all,1),f_PM(s,pm_200,1),f_PM(s,pm_10,1),f_PM(s,pm_25,1))
	    do t=1,num_tyre
	        f_PM(s,:,t)=f_PM(s,:,1)
	    enddo
	enddo
    s=crushing_index
    call read_line_val4(unit_in,unit_logfile,f_PM(s,pm_all,1),f_PM(s,pm_200,1),f_PM(s,pm_10,1),f_PM(s,pm_25,1))
    s=abrasion_index
    call read_line_val4(unit_in,unit_logfile,f_PM(s,pm_all,1),f_PM(s,pm_200,1),f_PM(s,pm_10,1),f_PM(s,pm_25,1))
	do t=1,num_tyre
	    f_PM(abrasion_index,:,t)=f_PM(abrasion_index,:,1)
	    f_PM(crushing_index,:,t)=f_PM(crushing_index,:,1)
    enddo
    !Create the binned size distribution
    f_PM_bin=f_PM
    do x=1,num_size-1
        f_PM_bin(1:num_source,x,1:num_tyre)=f_PM(1:num_source,x,1:num_tyre)-f_PM(1:num_source,x+1,1:num_tyre)
        f_PM_bin(crushing_index,x,1:num_tyre)=f_PM(crushing_index,x,1:num_tyre)-f_PM(crushing_index,x+1,1:num_tyre)
        f_PM_bin(abrasion_index,x,1:num_tyre)=f_PM(abrasion_index,x,1:num_tyre)-f_PM(abrasion_index,x+1,1:num_tyre)
    enddo

    call read_line_val1(unit_in,unit_logfile,V_ref_pm_fraction)
    call read_line_val1(unit_in,unit_logfile,c_pm_fraction)

    !Wind blown dust emission factors
    temp_str=''
    temp_str1='Wind blown dust emission factors'
    rewind(unit_in)
    do while (index(temp_str,trim(temp_str1)).eq.0)
        read(unit_in,'(a)',ERR=10) temp_str
	end do
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	write(unit_logfile,'(A)') trim(temp_str)
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	read(unit_in,*,ERR=10) temp_str !skip line
    call read_line_val1(unit_in,unit_logfile,tau_wind)
    call read_line_val1(unit_in,unit_logfile,FF_thresh)

    !Activity factors
    temp_str=''
    temp_str1='Activity efficiency factors'
    rewind(unit_in)
    do while (index(temp_str,trim(temp_str1)).eq.0)
        read(unit_in,'(a)',ERR=10) temp_str
	end do
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	write(unit_logfile,'(A)') trim(temp_str)
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	read(unit_in,*,ERR=10) temp_str !skip line
    i=ploughing_eff_index
    call read_line_val4(unit_in,unit_logfile,h_eff(i,1,pm_all),h_eff(i,1,pm_200),h_eff(i,1,pm_10),h_eff(i,1,pm_25))
    i=cleaning_eff_index
    call read_line_val4(unit_in,unit_logfile,h_eff(i,1,pm_all),h_eff(i,1,pm_200),h_eff(i,1,pm_10),h_eff(i,1,pm_25))
    i=drainage_eff_index
    call read_line_val4(unit_in,unit_logfile,h_eff(i,1,pm_all),h_eff(i,1,pm_200),h_eff(i,1,pm_10),h_eff(i,1,pm_25))
    i=spraying_eff_index
    call read_line_val4(unit_in,unit_logfile,h_eff(i,1,pm_all),h_eff(i,1,pm_200),h_eff(i,1,pm_10),h_eff(i,1,pm_25))

    do x=1,num_size
        i=ploughing_eff_index
        h_eff(i,dust_index,x)=h_eff(i,1,x)
        i=cleaning_eff_index
        h_eff(i,dust_index,x)=h_eff(i,1,x)
        i=drainage_eff_index
        h_eff(i,dust_index,x)=h_eff(i,1,x)
        i=spraying_eff_index
        h_eff(i,dust_index,x)=h_eff(i,1,x)
    enddo
    
	!Salt efficiency factors
	read(unit_in,*,ERR=10) temp_str !skip line
    i=ploughing_eff_index
    call read_line_val2(unit_in,unit_logfile,h_eff(i,salt_index(1),pm_all),h_eff(i,salt_index(2),pm_all))
    i=cleaning_eff_index
    call read_line_val2(unit_in,unit_logfile,h_eff(i,salt_index(1),pm_all),h_eff(i,salt_index(2),pm_all))
    i=drainage_eff_index
    call read_line_val2(unit_in,unit_logfile,h_eff(i,salt_index(1),pm_all),h_eff(i,salt_index(2),pm_all))
    i=spraying_eff_index
    call read_line_val2(unit_in,unit_logfile,h_eff(i,salt_index(1),pm_all),h_eff(i,salt_index(2),pm_all))

    do x=1,num_size
        i=ploughing_eff_index
        h_eff(i,salt_index,x)=h_eff(i,salt_index,pm_all)
        i=cleaning_eff_index
        h_eff(i,salt_index,x)=h_eff(i,salt_index,pm_all)
        i=drainage_eff_index
        h_eff(i,salt_index,x)=h_eff(i,salt_index,pm_all)
        i=spraying_eff_index
        h_eff(i,salt_index,x)=h_eff(i,salt_index,pm_all)
    enddo
 
    !Deposition velocity
    temp_str=''
    temp_str1='Deposition velocity'
    rewind(unit_in)
    do while (index(temp_str,trim(temp_str1)).eq.0)
        read(unit_in,'(a)',ERR=10) temp_str
	end do
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	write(unit_logfile,'(A)') trim(temp_str)
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	read(unit_in,*,ERR=10) temp_str !skip line
    call read_line_val3(unit_in,unit_logfile,w_dep(pm_200),w_dep(pm_10),w_dep(pm_25))

    !Concentration conversion limit values
    temp_str=''
    temp_str1='Concentration conversion limit values'
    rewind(unit_in)
    do while (index(temp_str,trim(temp_str1)).eq.0)
        read(unit_in,'(a)',ERR=10) temp_str
	end do
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	write(unit_logfile,'(A)') trim(temp_str)
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	read(unit_in,*,ERR=10) temp_str !skip line
    call read_line_val1(unit_in,unit_logfile,conc_min)
    call read_line_val1(unit_in,unit_logfile,emis_min)

    !Spray and splash factors
    temp_str=''
    temp_str1='Spray and splash factors'
    rewind(unit_in)
    do while (index(temp_str,trim(temp_str1)).eq.0)
        read(unit_in,'(a)',ERR=10) temp_str
	end do
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	write(unit_logfile,'(A)') trim(temp_str)
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	read(unit_in,*,ERR=10) temp_str !skip line

    call read_line_val3(unit_in,unit_logfile,R_0_spray(he,water_index),R_0_spray(he,snow_index),R_0_spray(he,ice_index))
    call read_line_val3(unit_in,unit_logfile,R_0_spray(li,water_index),R_0_spray(li,snow_index),R_0_spray(li,ice_index))
    call read_line_val3(unit_in,unit_logfile,V_ref_spray(water_index),V_ref_spray(snow_index),V_ref_spray(ice_index))
    call read_line_val3(unit_in,unit_logfile,g_road_sprayable_min(water_index),g_road_sprayable_min(snow_index),g_road_sprayable_min(ice_index))
    call read_line_val3(unit_in,unit_logfile,a_spray(water_index),a_spray(snow_index),a_spray(ice_index))
    call read_line_val3(unit_in,unit_logfile,V_thresh_spray(water_index),V_thresh_spray(snow_index),V_thresh_spray(ice_index))
   
    !Drainage parameters
    temp_str=''
    temp_str1='Drainage parameters'
    rewind(unit_in)
    do while (index(temp_str,trim(temp_str1)).eq.0)
        read(unit_in,'(a)',ERR=10) temp_str
	end do
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	write(unit_logfile,'(A)') trim(temp_str)
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	read(unit_in,*,ERR=10) temp_str !skip line
    call read_line_val2(unit_in,unit_logfile,g_road_drainable_min,g_road_drainable_thresh)
    call read_line_val1(unit_in,unit_logfile,snow_dust_drainage_retainment_limit)
    call read_line_val1(unit_in,unit_logfile,tau_road_drainage)

    !Ploughing parameters
    temp_str=''
    temp_str1='Ploughing parameters'
    rewind(unit_in)
    do while (index(temp_str,trim(temp_str1)).eq.0)
        read(unit_in,'(a)',ERR=10) temp_str
	end do
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	write(unit_logfile,'(A)') trim(temp_str)
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	read(unit_in,*,ERR=10) temp_str !skip line
    call read_line_val3(unit_in,unit_logfile,h_ploughing_moisture(water_index),h_ploughing_moisture(snow_index),h_ploughing_moisture(ice_index))
    call read_line_val3(unit_in,unit_logfile,ploughing_thresh(water_index),ploughing_thresh(snow_index),ploughing_thresh(ice_index))

    !Energy balance parameters
    temp_str=''
    temp_str1='Energy balance parameters'
    rewind(unit_in)
    do while (index(temp_str,trim(temp_str1)).eq.0)
        read(unit_in,'(a)',ERR=10) temp_str
	end do
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	write(unit_logfile,'(A)') trim(temp_str)
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	read(unit_in,*,ERR=10) temp_str !skip
    call read_line_val1(unit_in,unit_logfile,g_road_evaporation_thresh)
    call read_line_val1(unit_in,unit_logfile,z0)
    z0=z0/1000 !Convert from mm to m
    call read_line_val1(unit_in,unit_logfile,albedo_snow)
    call read_line_val1(unit_in,unit_logfile,dzs)
    call read_line_val1(unit_in,unit_logfile,sub_surf_average_time)
 	read(unit_in,*,ERR=10) temp_str !skip
    call read_line_val3(unit_in,unit_logfile,sub_surf_param(1),sub_surf_param(2),sub_surf_param(3))
	read(unit_in,*,ERR=10) temp_str !skip
    call read_line_val2(unit_in,unit_logfile,a_traffic(he),a_traffic(li))
    call read_line_val2(unit_in,unit_logfile,H_veh(he),H_veh(li))

    !Retention parameters
    temp_str=''
    temp_str1='Retention parameters'
    rewind(unit_in)
    do while (index(temp_str,trim(temp_str1)).eq.0)
        read(unit_in,'(a)',ERR=10) temp_str
	end do
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	write(unit_logfile,'(A)') trim(temp_str)
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	read(unit_in,*,ERR=10) temp_str !skip
    call read_line_val3(unit_in,unit_logfile,g_retention_thresh(road_index),g_retention_thresh(brake_index),g_retention_thresh(salt_index(2)))
    call read_line_val3(unit_in,unit_logfile,g_retention_min(road_index),g_retention_min(brake_index),g_retention_min(salt_index(2)))

    !Surface texture scaling parameters
    temp_str=''
    temp_str1='Surface texture parameters'
    rewind(unit_in)
    do while (index(temp_str,trim(temp_str1)).eq.0)
        read(unit_in,'(a)',ERR=10) temp_str
	end do
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	write(unit_logfile,'(A)') trim(temp_str)
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	read(unit_in,*,ERR=10) temp_str !skip
    do i=1,5
        call read_line_val1(unit_in,unit_logfile,texture_scaling(i))
    enddo
    !Rescale these parameters
    g_road_drainable_min=g_road_drainable_min*texture_scaling(1)
    f_0_suspension=f_0_suspension*texture_scaling(2)
    R_0_spray=R_0_spray*texture_scaling(3)
    h_eff(drainage_eff_index,dust_index,:)=h_eff(drainage_eff_index,dust_index,:)*texture_scaling(4)
    h_eff(spraying_eff_index,dust_index,:)=h_eff(spraying_eff_index,dust_index,:)*texture_scaling(5)

    !Road track parameters
    temp_str=''
    temp_str1='Road track parameters'
    rewind(unit_in)
    do while (index(temp_str,trim(temp_str1)).eq.0)
        read(unit_in,'(a)',ERR=10) temp_str
	end do
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	write(unit_logfile,'(A)') trim(temp_str)
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
	read(unit_in,*,ERR=10) temp_str !skip
    num_track=0
    do i=1,num_track_max
    !Must be in this order (alltrack_type,outtrack_type,intrack_type,shoulder_type,kerb_type)
        call read_line_val4(unit_in,unit_logfile,include_track_temp,f_track_temp(1),f_track_temp(2),f_track_temp(3))
        if (include_track_temp.gt.0) then
            num_track=num_track+1
            f_track(num_track)=f_track_temp(1)
            veh_track(num_track)=f_track_temp(2)
            mig_track(num_track)=f_track_temp(3)
            track_type(num_track)=i
        endif
    enddo
        
    return

    
10	close(unit_in,status='keep')
 
    end subroutine read_NORTRIP_parameters

!----------------------------------------------------------------------

!----------------------------------------------------------------------
    subroutine read_NORTRIP_flags

    use NORTRIP_definitions
    
    implicit none
    
    character(256) temp_name
    integer unit_in,unit_logfile_temp
    logical exists
    integer read_zip_file_temp,use_single_road_loop_temp
    
    !Functions
    integer match_string_int
    real match_string_val
        
    unit_in=unit_read_NORTRIP_flags
    unit_logfile_temp=unit_logfile !use -1 not to write anything, 0 to go to screen
    
    !Open pathname file for reading
    temp_name=trim(path_inputparam)//trim(filename_inputparam)//'_flags.txt'
    inquire(file=temp_name,exist=exists)
    if (.not.exists) then
        write(unit_logfile_temp,*)'ERROR: '//trim(temp_name)//' does not exist. Using default flags'
        return
    endif
    
    open(unit_in,file=temp_name,access='sequential',status='old',readonly)

    !Open log file
    !if (unit_logfile.gt.0) then
    !    open(unit_logfile,file=filename_log,status='old',position='append')
    !endif
   
	write(unit_logfile_temp,'(A)') '================================================================'
	write(unit_logfile_temp,'(A)') 'Reading model flags (read_NORTRIP_flags)'
	write(unit_logfile_temp,'(A)') '================================================================'
    !WEAR
    wear_flag(road_index)=match_string_int('road_wear_flag',unit_in,unit_logfile_temp,0)
    wear_flag(tyre_index)=match_string_int('tyre_wear_flag',unit_in,unit_logfile_temp,0)   
    wear_flag(brake_index)=match_string_int('brake_wear_flag',unit_in,unit_logfile_temp,0)   
    exhaust_flag=match_string_int('exhaust_flag',unit_in,unit_logfile_temp,0)   
    road_suspension_flag=match_string_int('road_suspension_flag',unit_in,unit_logfile_temp,0)  
    dust_deposition_flag=match_string_int('dust_deposition_flag',unit_in,unit_logfile_temp,0)
    abrasion_flag=match_string_int('abrasion_flag',unit_in,unit_logfile_temp,0)
    crushing_flag=match_string_int('crushing_flag',unit_in,unit_logfile_temp,0)
    dust_drainage_flag=match_string_int('dust_drainage_flag',unit_in,unit_logfile_temp,0)  
    dust_spray_flag=match_string_int('dust_spray_flag',unit_in,unit_logfile_temp,0)
    dust_ploughing_flag=match_string_int('dust_ploughing_flag',unit_in,unit_logfile_temp,0)
    wind_suspension_flag=match_string_int('wind_suspension_flag',unit_in,unit_logfile_temp,0)
    !MOISTURE                                                        
    retention_flag=match_string_int('retention_flag',unit_in,unit_logfile_temp,0)  
    use_obs_retention_flag=match_string_int('use_obs_retention_flag',unit_in,unit_logfile_temp,0)
    water_spray_flag=match_string_int('water_spray_flag',unit_in,unit_logfile_temp,0)
    drainage_type_flag=match_string_int('drainage_type_flag',unit_in,unit_logfile_temp,0)
    surface_humidity_flag=match_string_int('surface_humidity_flag',unit_in,unit_logfile_temp,0)
    use_salt_humidity_flag=match_string_int('use_salt_humidity_flag',unit_in,unit_logfile_temp,0)
    !ENERGY BALANCE                                                  
    evaporation_flag=match_string_int('evaporation_flag',unit_in,unit_logfile_temp,0)
    canyon_shadow_flag=match_string_int('canyon_shadow_flag',unit_in,unit_logfile_temp,0)
    canyon_long_rad_flag=match_string_int('canyon_long_rad_flag',unit_in,unit_logfile_temp,0)
    use_subsurface_flag=match_string_int('use_subsurface_flag',unit_in,unit_logfile_temp,0)
    use_traffic_turb_flag=match_string_int('use_traffic_turb_flag',unit_in,unit_logfile_temp,0)
    !ACTIVITY                                                        
    use_salting_data_flag(1)=match_string_val('use_salting_data_1_flag',unit_in,unit_logfile_temp,0.0)
    use_salting_data_flag(2)=match_string_val('use_salting_data_2_flag',unit_in,unit_logfile_temp,0.0)
    use_sanding_data_flag=match_string_int('use_sanding_data_flag',unit_in,unit_logfile_temp,0)
    use_ploughing_data_flag=match_string_int('use_ploughing_data_flag',unit_in,unit_logfile_temp,0)
    use_cleaning_data_flag=match_string_int('use_cleaning_data_flag',unit_in,unit_logfile_temp,0)
    use_wetting_data_flag=match_string_int('use_wetting_data_flag',unit_in,unit_logfile_temp,0)
    auto_salting_flag=match_string_int('auto_salting_flag',unit_in,unit_logfile_temp,0)
    auto_binding_flag=match_string_int('auto_binding_flag',unit_in,unit_logfile_temp,0)
    auto_sanding_flag=match_string_int('auto_sanding_flag',unit_in,unit_logfile_temp,0)
    auto_ploughing_flag=match_string_int('auto_ploughing_flag',unit_in,unit_logfile_temp,0)
    auto_cleaning_flag=match_string_int('auto_cleaning_flag',unit_in,unit_logfile_temp,0)
    !OUTPUT                                                          
    plot_type_flag=match_string_int('plot_type_flag',unit_in,unit_logfile_temp,0)
    save_type_flag=match_string_int('save_type_flag',unit_in,unit_logfile_temp,0)
    !DISPERSION                                                      
    use_ospm_flag=match_string_int('use_ospm_flag',unit_in,unit_logfile_temp,0)
    !FORECAST
    forecast_hour=match_string_int('forecast_hour',unit_in,unit_logfile_temp,0)
    forecast_type=match_string_int('forecast_type',unit_in,unit_logfile_temp,1)
    !ZIP INPUT FILES. Set to 3 to read all inputs as zip
    read_zip_file_temp=match_string_int('read_data_in_zip_format',unit_in,unit_logfile_temp,0)
    if (read_zip_file_temp.ge.1) read_timeseriesdata_in_zip_format=.true.
    if (read_zip_file_temp.ge.2) read_metadata_in_zip_format=.true.
    if (read_zip_file_temp.ge.3) read_initialdata_in_zip_format=.true.
    !Define if the routine uses the single road option for large datasets
    use_single_road_loop_temp=match_string_int('use_single_road_loop_flag',unit_in,unit_logfile_temp,0)
    use_single_road_loop_flag=.false.
    if (use_single_road_loop_temp.eq.1) use_single_road_loop_flag=.true.   
    !Operating system. Only used for deleting files that have been unzipped. 1 is windows, 2 is linux
    operating_system=match_string_int('operating_system',unit_in,unit_logfile_temp,operating_system)
    
  
    !Additional override parameters
    override_long_rad_in_offset=match_string_val('override_long_rad_in_offset',unit_in,unit_logfile_temp,nodata_orig)
    override_RH_offset=match_string_val('override_RH_offset',unit_in,unit_logfile_temp,nodata_orig)
    override_T_a_offset=match_string_val('override_T_2m_offset',unit_in,unit_logfile_temp,nodata_orig)
    override_wind_speed_correction=match_string_val('override_wind_speed_correction',unit_in,unit_logfile_temp,nodata_orig)
    override_albedo_road_offset=match_string_val('override_albedo_road_offset',unit_in,unit_logfile_temp,nodata_orig)

    !if (unit_logfile.gt.0) then
    !    close(unit_logfile,status='keep')
    !endif
    
10	close(unit_in,status='keep')
     
    end subroutine read_NORTRIP_flags
!----------------------------------------------------------------------

!----------------------------------------------------------------------
    subroutine read_NORTRIP_activities

    use NORTRIP_definitions
    
    implicit none
    
    character(256) temp_name
    integer unit_in,unit_logfile_temp
    logical exists
    
    !Functions
    integer match_string_int
    real match_string_val
        
    unit_in=unit_read_NORTRIP_activities
    unit_logfile_temp=unit_logfile !use -1 not to write anything, 0 to go to screen
    
    !Open pathname file for reading
    temp_name=trim(path_inputparam)//trim(filename_inputparam)//'_activities.txt'
    inquire(file=temp_name,exist=exists)
    if (.not.exists) then
        write(unit_logfile_temp,*)'ERROR: '//trim(temp_name)//' does not exist. Using default flags'
        return
    endif
    
    open(unit_in,file=temp_name,access='sequential',status='old',readonly)

    !Open log file
    !if (unit_logfile.gt.0) then
    !    open(unit_logfile,file=filename_log,status='old',position='append')
    !endif
   
 	write(unit_logfile_temp,'(A)') '================================================================'
	write(unit_logfile_temp,'(A)') 'Reading model auto activity parameters (read_NORTRIP_activities)'
	write(unit_logfile_temp,'(A)') '================================================================'
	write(unit_logfile_temp,'(A)') '----------------------------------------------------------------'
	write(unit_logfile_temp,'(A)') 'Salting (salt1+salt2)'
	write(unit_logfile_temp,'(A)') '----------------------------------------------------------------'
    salting_hour(1)=match_string_val('salting_hour(1)',unit_in,unit_logfile_temp,0.0)
    salting_hour(2)=match_string_val('salting_hour(2)',unit_in,unit_logfile_temp,0.0)                                                          
    delay_salting_day=match_string_val('delay_salting_day',unit_in,unit_logfile_temp,0.0)                                                           
    check_salting_day=match_string_val('check_salting_day',unit_in,unit_logfile_temp,0.0)                                                          
    min_temp_salt=match_string_val('min_temp_salt',unit_in,unit_logfile_temp,0.0)                                                            
    max_temp_salt=match_string_val('max_temp_salt',unit_in,unit_logfile_temp,0.0)                                                             
    precip_rule_salt=match_string_val('precip_rule_salt',unit_in,unit_logfile_temp,0.0)                                                         
    RH_rule_salt=match_string_val('RH_rule_salt',unit_in,unit_logfile_temp,0.0)                                                            
    g_salting_rule=match_string_val('g_salting_rule',unit_in,unit_logfile_temp,0.0)                                                        
    salt_mass=match_string_val('salt_mass',unit_in,unit_logfile_temp,0.0)                                                        
    salt_dilution=match_string_val('salt_dilution',unit_in,unit_logfile_temp,0.0)                                     
    salt_type_distribution=match_string_val('salt_type_distribution',unit_in,unit_logfile_temp,1.0)               
	write(unit_logfile_temp,'(A)') '----------------------------------------------------------------'
	write(unit_logfile_temp,'(A)') 'Sanding'
	write(unit_logfile_temp,'(A)') '----------------------------------------------------------------'
    sanding_hour(1)=match_string_val('sanding_hour(1)',unit_in,unit_logfile_temp,0.0)                                                           
    sanding_hour(2)=match_string_val('sanding_hour(2)',unit_in,unit_logfile_temp,0.0)                                                            
    delay_sanding_day=match_string_val('delay_sanding_day',unit_in,unit_logfile_temp,0.0)                                                          
    check_sanding_day=match_string_val('check_sanding_day',unit_in,unit_logfile_temp,0.0)                                                         
    min_temp_sand=match_string_val('min_temp_sand',unit_in,unit_logfile_temp,0.0)                                                            
    max_temp_sand=match_string_val('max_temp_sand',unit_in,unit_logfile_temp,0.0)                                                          
    precip_rule_sand=match_string_val('precip_rule_sand',unit_in,unit_logfile_temp,0.0)                                                        
    RH_rule_sand=match_string_val('RH_rule_sand',unit_in,unit_logfile_temp,0.0)                                                           
    g_sanding_rule=match_string_val('g_sanding_rule',unit_in,unit_logfile_temp,0.0)                                                           
    sand_mass=match_string_val('sand_mass',unit_in,unit_logfile_temp,0.0)                                                          
    sand_dilution=match_string_val('sand_dilution',unit_in,unit_logfile_temp,0.0)                                    
 	write(unit_logfile_temp,'(A)') '----------------------------------------------------------------'
	write(unit_logfile_temp,'(A)') 'Snow ploughing'
	write(unit_logfile_temp,'(A)') '----------------------------------------------------------------'
    delay_ploughing_hour=match_string_val('delay_ploughing_hour',unit_in,unit_logfile_temp,0.0)  
    ploughing_thresh_2=match_string_val('ploughing_thresh',unit_in,unit_logfile_temp,0.0)
    if (ploughing_thresh_2.gt.0) then
        ploughing_thresh(snow_index)=ploughing_thresh_2
        ploughing_thresh(ice_index)=ploughing_thresh_2
    endif
 	write(unit_logfile_temp,'(A)') '----------------------------------------------------------------'
	write(unit_logfile_temp,'(A)') 'Cleaning'
	write(unit_logfile_temp,'(A)') '----------------------------------------------------------------'
    delay_cleaning_hour=match_string_val('delay_cleaning_hour',unit_in,unit_logfile_temp,0.0)                                                            
    min_temp_cleaning=match_string_val('min_temp_cleaning',unit_in,unit_logfile_temp,0.0)                                                            
    clean_with_salting=match_string_int('clean_with_salting',unit_in,unit_logfile_temp,0)                                                                
    start_month_cleaning=match_string_val('start_month_cleaning',unit_in,unit_logfile_temp,0.0)                                                       
    end_month_cleaning=match_string_val('end_month_cleaning',unit_in,unit_logfile_temp,0.0)                                                        
    wetting_with_cleaning=match_string_val('wetting_with_cleaning',unit_in,unit_logfile_temp,0.0)                                                          
    efficiency_of_cleaning=match_string_val('efficiency_of_cleaning',unit_in,unit_logfile_temp,0.0)                                                          
	write(unit_logfile_temp,'(A)') '----------------------------------------------------------------'
	write(unit_logfile_temp,'(A)') 'Binding (salt2)'
	write(unit_logfile_temp,'(A)') '----------------------------------------------------------------'
    binding_hour(1)=match_string_val('binding_hour(1)',unit_in,unit_logfile_temp,0.0)
    binding_hour(2)=match_string_val('binding_hour(2)',unit_in,unit_logfile_temp,0.0)                                                          
    delay_binding_day=match_string_val('delay_binding_day',unit_in,unit_logfile_temp,0.0)                                                           
    check_binding_day=match_string_val('check_binding_day',unit_in,unit_logfile_temp,0.0)                                                          
    min_temp_binding=match_string_val('min_temp_binding',unit_in,unit_logfile_temp,0.0)                                                            
    max_temp_binding=match_string_val('max_temp_binding',unit_in,unit_logfile_temp,0.0)                                                             
    precip_rule_binding=match_string_val('precip_rule_binding',unit_in,unit_logfile_temp,0.0)                                                         
    RH_rule_binding=match_string_val('RH_rule_binding',unit_in,unit_logfile_temp,0.0)                                                            
    g_binding_rule=match_string_val('g_binding_rule',unit_in,unit_logfile_temp,0.0)                                                        
    binding_mass=match_string_val('binding_mass',unit_in,unit_logfile_temp,0.0)                                                        
    binding_dilution=match_string_val('binding_dilution',unit_in,unit_logfile_temp,0.0)                                     
    start_month_binding=match_string_val('start_month_binding',unit_in,unit_logfile_temp,0.0)                                                       
    end_month_binding=match_string_val('end_month_binding',unit_in,unit_logfile_temp,0.0)                                                        
  
    !if (unit_logfile.gt.0) then
    !    close(unit_logfile,status='keep')
    !endif
    
10	close(unit_in,status='keep')
     
end subroutine read_NORTRIP_activities
!----------------------------------------------------------------------

