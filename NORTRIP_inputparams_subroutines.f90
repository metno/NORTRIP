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
    do x=1,num_size
        if (x.eq.num_size) then
            f_PM_bin(1:num_source,x,1:num_tyre)=f_PM(1:num_source,x,1:num_tyre)
            f_PM_bin(crushing_index,x,1:num_tyre)=f_PM(crushing_index,x,1:num_tyre)
            f_PM_bin(abrasion_index,x,1:num_tyre)=f_PM(abrasion_index,x,1:num_tyre)
        else          
            f_PM_bin(1:num_source,x,1:num_tyre)=f_PM(1:num_source,x,1:num_tyre)-f_PM(1:num_source,x+1,1:num_tyre)
            f_PM_bin(crushing_index,x,1:num_tyre)=f_PM(crushing_index,x,1:num_tyre)-f_PM(crushing_index,x+1,1:num_tyre)
            f_PM_bin(abrasion_index,x,1:num_tyre)=f_PM(abrasion_index,x,1:num_tyre)-f_PM(abrasion_index,x+1,1:num_tyre)
        endif
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
    
    !temp_str=''
    !temp_str1='Ploughing minimum parameters'
    !rewind(unit_in)
    !do while (index(temp_str,trim(temp_str1)).eq.0)
    !    read(unit_in,'(a)',ERR=10) temp_str
	!end do
	!write(unit_logfile,'(A)') '----------------------------------------------------------------'
	!write(unit_logfile,'(A)') trim(temp_str)
	!write(unit_logfile,'(A)') '----------------------------------------------------------------'
    !call read_line_val3(unit_in,unit_logfile,ploughing_min_thresh(water_index),ploughing_min_thresh(snow_index),ploughing_min_thresh(ice_index))

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
    !call read_line_val1(unit_in,unit_logfile,z0)
    call read_line_val1or3(unit_in,unit_logfile,z0m_in,z0t_in,z0q_in)
    !write(*,*) z0m_in,z0t_in,z0q_in
    !stop
    z0=z0m_in/1000. !Convert from mm to m
    if (z0t_in.eq.0) then
        z0t=z0/10. !Default
    else
        z0t=z0t_in/1000.
    endif
    if (z0q_in.eq.0) then
        z0q=z0/10. !Default
    else
        z0q=z0q_in/1000.
    endif
    
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
        
    close(unit_in,status='keep')
    
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
    retain_water_by_snow_flag=match_string_val('retain_water_by_snow_flag',unit_in,unit_logfile_temp,retain_water_by_snow_flag)   
    !ENERGY BALANCE                                                  
    evaporation_flag=match_string_int('evaporation_flag',unit_in,unit_logfile_temp,0)
    canyon_shadow_flag=match_string_int('canyon_shadow_flag',unit_in,unit_logfile_temp,0)
    canyon_long_rad_flag=match_string_int('canyon_long_rad_flag',unit_in,unit_logfile_temp,0)
    use_subsurface_flag=match_string_int('use_subsurface_flag',unit_in,unit_logfile_temp,0)
    use_traffic_turb_flag=match_string_int('use_traffic_turb_flag',unit_in,unit_logfile_temp,0)
    use_melt_freeze_energy_flag=match_string_int('use_melt_freeze_energy_flag',unit_in,unit_logfile_temp,0)
    use_stability_flag=match_string_int('use_stability_flag',unit_in,unit_logfile_temp,use_stability_flag)
    !ACTIVITY                                                        
    use_salting_data_flag(1)=match_string_val('use_salting_data_1_flag',unit_in,unit_logfile_temp,0.0)
    use_salting_data_flag(2)=match_string_val('use_salting_data_2_flag',unit_in,unit_logfile_temp,0.0)
    use_sanding_data_flag=match_string_val('use_sanding_data_flag',unit_in,unit_logfile_temp,0)
    use_ploughing_data_flag=match_string_int('use_ploughing_data_flag',unit_in,unit_logfile_temp,0)
    use_cleaning_data_flag=match_string_int('use_cleaning_data_flag',unit_in,unit_logfile_temp,0)
    use_wetting_data_flag=match_string_int('use_wetting_data_flag',unit_in,unit_logfile_temp,0)
    auto_salting_flag=match_string_int('auto_salting_flag',unit_in,unit_logfile_temp,0)
    auto_binding_flag=match_string_int('auto_binding_flag',unit_in,unit_logfile_temp,0)
    auto_sanding_flag=match_string_int('auto_sanding_flag',unit_in,unit_logfile_temp,0)
    auto_ploughing_flag=match_string_int('auto_ploughing_flag',unit_in,unit_logfile_temp,0)
    auto_cleaning_flag=match_string_int('auto_cleaning_flag',unit_in,unit_logfile_temp,0)
    salt_type(2)=match_string_int('binding_salt_flag',unit_in,unit_logfile_temp,2)
    activity_in_tunnels_flag=match_string_int('activity_in_tunnels_flag',unit_in,unit_logfile_temp,0)
    salt_after_ploughing_flag=match_string_int('salt_after_ploughing_flag',unit_in,unit_logfile_temp,0)
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
    logical :: activity_data_already_allocated=.false.
    
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
    salting_hour_ref(1)=match_string_val('salting_hour(1)',unit_in,unit_logfile_temp,0.0)
    salting_hour_ref(2)=match_string_val('salting_hour(2)',unit_in,unit_logfile_temp,0.0)                                                          
    delay_salting_day_ref=match_string_val('delay_salting_day',unit_in,unit_logfile_temp,0.0)                                                           
    check_salting_day_ref=match_string_val('check_salting_day',unit_in,unit_logfile_temp,0.0)                                                          
    min_temp_salt_ref=match_string_val('min_temp_salt',unit_in,unit_logfile_temp,0.0)                                                            
    max_temp_salt_ref=match_string_val('max_temp_salt',unit_in,unit_logfile_temp,0.0)                                                             
    precip_rule_salt_ref=match_string_val('precip_rule_salt',unit_in,unit_logfile_temp,0.0)                                                         
    RH_rule_salt_ref=match_string_val('RH_rule_salt',unit_in,unit_logfile_temp,0.0)                                                            
    g_salting_rule_ref=match_string_val('g_salting_rule',unit_in,unit_logfile_temp,0.0)                                                        
    salt_mass_ref=match_string_val('salt_mass',unit_in,unit_logfile_temp,0.0)                                                        
    salt_dilution_ref=match_string_val('salt_dilution',unit_in,unit_logfile_temp,0.0)                                     
    salt_type_distribution_ref=match_string_val('salt_type_distribution',unit_in,unit_logfile_temp,1.0)               
	write(unit_logfile_temp,'(A)') '----------------------------------------------------------------'
	write(unit_logfile_temp,'(A)') 'Sanding'
	write(unit_logfile_temp,'(A)') '----------------------------------------------------------------'
    sanding_hour_ref(1)=match_string_val('sanding_hour(1)',unit_in,unit_logfile_temp,0.0)                                                           
    sanding_hour_ref(2)=match_string_val('sanding_hour(2)',unit_in,unit_logfile_temp,0.0)                                                            
    delay_sanding_day_ref=match_string_val('delay_sanding_day',unit_in,unit_logfile_temp,0.0)                                                          
    check_sanding_day_ref=match_string_val('check_sanding_day',unit_in,unit_logfile_temp,0.0)                                                         
    min_temp_sand_ref=match_string_val('min_temp_sand',unit_in,unit_logfile_temp,0.0)                                                            
    max_temp_sand_ref=match_string_val('max_temp_sand',unit_in,unit_logfile_temp,0.0)                                                          
    precip_rule_sand_ref=match_string_val('precip_rule_sand',unit_in,unit_logfile_temp,0.0)                                                        
    RH_rule_sand_ref=match_string_val('RH_rule_sand',unit_in,unit_logfile_temp,0.0)                                                           
    g_sanding_rule_ref=match_string_val('g_sanding_rule',unit_in,unit_logfile_temp,0.0)                                                           
    sand_mass_ref=match_string_val('sand_mass',unit_in,unit_logfile_temp,0.0)                                                          
    sand_dilution_ref=match_string_val('sand_dilution',unit_in,unit_logfile_temp,0.0)                                    
 	write(unit_logfile_temp,'(A)') '----------------------------------------------------------------'
	write(unit_logfile_temp,'(A)') 'Snow ploughing'
	write(unit_logfile_temp,'(A)') '----------------------------------------------------------------'
    delay_ploughing_hour_ref=match_string_val('delay_ploughing_hour',unit_in,unit_logfile_temp,0.0)  
    ploughing_thresh_2_ref=match_string_val('ploughing_thresh',unit_in,unit_logfile_temp,0.0)
    if (ploughing_thresh_2_ref.gt.0) then
        ploughing_thresh(snow_index)=ploughing_thresh_2_ref
        ploughing_thresh(ice_index)=ploughing_thresh_2_ref
    endif
    ploughing_min_thresh_2_ref=match_string_val('ploughing_min_thresh',unit_in,unit_logfile_temp,ploughing_min_thresh_2_ref)
    if (ploughing_min_thresh_2_ref.gt.0) then
        ploughing_min_thresh(snow_index)=ploughing_min_thresh_2_ref
        ploughing_min_thresh(ice_index)=ploughing_min_thresh_2_ref
        ploughing_min_thresh(water_index)=ploughing_min_thresh_2_ref
    endif
 	write(unit_logfile_temp,'(A)') '----------------------------------------------------------------'
	write(unit_logfile_temp,'(A)') 'Cleaning'
	write(unit_logfile_temp,'(A)') '----------------------------------------------------------------'
    cleaning_hour_ref(1)=match_string_val('cleaning_hour(1)',unit_in,unit_logfile_temp,cleaning_hour_ref(1))
    cleaning_hour_ref(2)=match_string_val('cleaning_hour(2)',unit_in,unit_logfile_temp,cleaning_hour_ref(2))                                                          
    delay_cleaning_hour_ref=match_string_val('delay_cleaning_hour',unit_in,unit_logfile_temp,0.0)                                                            
    delay_cleaning_day_ref=match_string_val('delay_cleaning_day',unit_in,unit_logfile_temp,0.0)
    !If no day value is read then use the hour value. Otherwise the rest is in days
    if (delay_cleaning_day_ref.eq.0.0) delay_cleaning_day_ref=delay_cleaning_hour_ref/24.
    min_temp_cleaning_ref=match_string_val('min_temp_cleaning',unit_in,unit_logfile_temp,0.0)
    clean_with_salting_ref=match_string_int('clean_with_salting',unit_in,unit_logfile_temp,0)                                                                
    start_month_cleaning_ref=match_string_val('start_month_cleaning',unit_in,unit_logfile_temp,0.0)                                                       
    end_month_cleaning_ref=match_string_val('end_month_cleaning',unit_in,unit_logfile_temp,0.0)                                                        
    wetting_with_cleaning_ref=match_string_val('wetting_with_cleaning',unit_in,unit_logfile_temp,0.0)                                                          
    efficiency_of_cleaning_ref=match_string_val('efficiency_of_cleaning',unit_in,unit_logfile_temp,0.0)                                                          
	write(unit_logfile_temp,'(A)') '----------------------------------------------------------------'
	write(unit_logfile_temp,'(A)') 'Binding (salt2)'
	write(unit_logfile_temp,'(A)') '----------------------------------------------------------------'
    binding_hour_ref(1)=match_string_val('binding_hour(1)',unit_in,unit_logfile_temp,0.0)
    binding_hour_ref(2)=match_string_val('binding_hour(2)',unit_in,unit_logfile_temp,0.0)                                                          
    delay_binding_day_ref=match_string_val('delay_binding_day',unit_in,unit_logfile_temp,0.0)                                                           
    check_binding_day_ref=match_string_val('check_binding_day',unit_in,unit_logfile_temp,0.0)                                                          
    min_temp_binding_ref=match_string_val('min_temp_binding',unit_in,unit_logfile_temp,0.0)                                                            
    max_temp_binding_ref=match_string_val('max_temp_binding',unit_in,unit_logfile_temp,0.0)                                                             
    precip_rule_binding_ref=match_string_val('precip_rule_binding',unit_in,unit_logfile_temp,0.0)                                                         
    RH_rule_binding_ref=match_string_val('RH_rule_binding',unit_in,unit_logfile_temp,0.0)                                                            
    g_binding_rule_ref=match_string_val('g_binding_rule',unit_in,unit_logfile_temp,0.0)                                                        
    binding_mass_ref=match_string_val('binding_mass',unit_in,unit_logfile_temp,0.0)                                                        
    binding_dilution_ref=match_string_val('binding_dilution',unit_in,unit_logfile_temp,0.0)                                     
    start_month_binding_ref=match_string_val('start_month_binding',unit_in,unit_logfile_temp,0.0)                                                       
    end_month_binding_ref=match_string_val('end_month_binding',unit_in,unit_logfile_temp,0.0)                                                        
  
    !if (unit_logfile.gt.0) then
    !    close(unit_logfile,status='keep')
    !endif
    
10  close(unit_in,status='keep')

    !Allocate the auto_activity parameters for all roads if they have not already been allocated in the transfer routine (coupling to multiroad code)
    !Set the flag if these have not been allocated. Onlt necessary fo rthe first case since they are either allocated for all or none in the multiroad code
    if (.not.allocated(salting_hour)) then
        allocate (salting_hour(2,0:n_roads))
        activity_data_already_allocated=.false.
    else
        activity_data_already_allocated=.true.
    endif    
    if (.not.allocated(delay_salting_day)) allocate (delay_salting_day(0:n_roads))
    if (.not.allocated(check_salting_day)) allocate (check_salting_day(0:n_roads))
    if (.not.allocated(min_temp_salt)) allocate (min_temp_salt(0:n_roads))
    if (.not.allocated(max_temp_salt)) allocate (max_temp_salt(0:n_roads))
    if (.not.allocated(precip_rule_salt)) allocate (precip_rule_salt(0:n_roads))
    if (.not.allocated(RH_rule_salt)) allocate (RH_rule_salt(0:n_roads))
    if (.not.allocated(g_salting_rule)) allocate (g_salting_rule(0:n_roads))
    if (.not.allocated(salt_mass)) allocate (salt_mass(0:n_roads)) 
    if (.not.allocated(salt_dilution)) allocate (salt_dilution(0:n_roads)) 
    if (.not.allocated(salt_type_distribution)) allocate (salt_type_distribution(0:n_roads)) 
    
    if (.not.allocated(sanding_hour)) allocate (sanding_hour(2,0:n_roads))
    if (.not.allocated(delay_sanding_day)) allocate (delay_sanding_day(0:n_roads)) 
    if (.not.allocated(check_sanding_day)) allocate (check_sanding_day(0:n_roads))
    if (.not.allocated(min_temp_sand)) allocate (min_temp_sand(0:n_roads)) 
    if (.not.allocated(max_temp_sand)) allocate (max_temp_sand(0:n_roads))
    if (.not.allocated(precip_rule_sand)) allocate (precip_rule_sand(0:n_roads))
    if (.not.allocated(RH_rule_sand)) allocate (RH_rule_sand(0:n_roads)) 
    if (.not.allocated(g_sanding_rule)) allocate (g_sanding_rule(0:n_roads)) 
    if (.not.allocated(sand_mass)) allocate (sand_mass(0:n_roads)) 
    if (.not.allocated(sand_dilution)) allocate (sand_dilution(0:n_roads))
    
    if (.not.allocated(delay_ploughing_hour)) allocate (delay_ploughing_hour(0:n_roads))
    if (.not.allocated(ploughing_thresh_2)) allocate (ploughing_thresh_2(0:n_roads)) 
    if (.not.allocated(cleaning_hour)) allocate (cleaning_hour(2,0:n_roads))
    if (.not.allocated(delay_cleaning_day)) allocate (delay_cleaning_day(0:n_roads))
    if (.not.allocated(min_temp_cleaning)) allocate (min_temp_cleaning(0:n_roads))
    if (.not.allocated(clean_with_salting)) allocate (clean_with_salting(0:n_roads))
    if (.not.allocated(start_month_cleaning)) allocate (start_month_cleaning(0:n_roads))
    if (.not.allocated(end_month_cleaning)) allocate (end_month_cleaning(0:n_roads))
    if (.not.allocated(wetting_with_cleaning)) allocate (wetting_with_cleaning(0:n_roads))
    if (.not.allocated(efficiency_of_cleaning)) allocate (efficiency_of_cleaning(0:n_roads))

    if (.not.allocated(binding_hour)) allocate (binding_hour(2,0:n_roads))
    if (.not.allocated(delay_binding_day)) allocate (delay_binding_day(0:n_roads))
    if (.not.allocated(check_binding_day)) allocate (check_binding_day(0:n_roads))
    if (.not.allocated(min_temp_binding)) allocate (min_temp_binding(0:n_roads))
    if (.not.allocated(max_temp_binding)) allocate (max_temp_binding(0:n_roads))
    if (.not.allocated(precip_rule_binding)) allocate (precip_rule_binding(0:n_roads))
    if (.not.allocated(RH_rule_binding)) allocate (RH_rule_binding(0:n_roads))
    if (.not.allocated(g_binding_rule)) allocate (g_binding_rule(0:n_roads))
    if (.not.allocated(binding_mass)) allocate (binding_mass(0:n_roads))
    if (.not.allocated(binding_dilution)) allocate (binding_dilution(0:n_roads))
    if (.not.allocated(start_month_binding)) allocate (start_month_binding(0:n_roads))
    if (.not.allocated(end_month_binding)) allocate (end_month_binding(0:n_roads))
    
    !If these were not previously allocated and given values then fill them all with no data
    if (.not.activity_data_already_allocated) then
        salting_hour=nodata_activity
        delay_salting_day=nodata_activity
        check_salting_day=nodata_activity
        min_temp_salt=nodata_activity
        max_temp_salt=nodata_activity
        precip_rule_salt=nodata_activity
        RH_rule_salt=nodata_activity
        g_salting_rule=nodata_activity
        salt_mass=nodata_activity 
        salt_dilution=nodata_activity 
        salt_type_distribution=nodata_activity 
    
        sanding_hour=nodata_activity
        delay_sanding_day=nodata_activity 
        check_sanding_day=nodata_activity
        min_temp_sand=nodata_activity 
        max_temp_sand=nodata_activity
        precip_rule_sand=nodata_activity
        RH_rule_sand=nodata_activity 
        g_sanding_rule=nodata_activity 
        sand_mass=nodata_activity 
        sand_dilution=nodata_activity
    
        delay_ploughing_hour=nodata_activity
        ploughing_thresh_2=nodata_activity 

        cleaning_hour=nodata_activity
        delay_cleaning_day=nodata_activity
        min_temp_cleaning=nodata_activity
        clean_with_salting=nodata_activity
        start_month_cleaning=nodata_activity
        end_month_cleaning=nodata_activity
        wetting_with_cleaning=nodata_activity
        efficiency_of_cleaning=nodata_activity

        binding_hour=nodata_activity
        delay_binding_day=nodata_activity
        check_binding_day=nodata_activity
        min_temp_binding=nodata_activity
        max_temp_binding=nodata_activity
        precip_rule_binding=nodata_activity
        RH_rule_binding=nodata_activity
        g_binding_rule=nodata_activity
        binding_mass=nodata_activity
        binding_dilution=nodata_activity
        start_month_binding=nodata_activity
        end_month_binding=nodata_activity        
    endif
    
    !Transfer the ref values to the read-in multiple road values if the multiple road values are 'nodata_activity'
    !Only checks the first road value for no data.
    if (salting_hour(1,1).eq.nodata_activity) salting_hour(1,:)=salting_hour_ref(1)
    if (salting_hour(2,1).eq.nodata_activity) salting_hour(2,:)=salting_hour_ref(2)
    if (delay_salting_day(1).eq.nodata_activity) delay_salting_day(:)=delay_salting_day_ref
    if (check_salting_day(1).eq.nodata_activity) check_salting_day(:)=check_salting_day_ref
    if (min_temp_salt(1).eq.nodata_activity) min_temp_salt(:)=min_temp_salt_ref 
    if (max_temp_salt(1).eq.nodata_activity) max_temp_salt(:)=max_temp_salt_ref
    if (precip_rule_salt(1).eq.nodata_activity) precip_rule_salt(:)=precip_rule_salt_ref
    if (RH_rule_salt(1).eq.nodata_activity) RH_rule_salt(:)=RH_rule_salt_ref 
    if (g_salting_rule(1).eq.nodata_activity) g_salting_rule(:)=g_salting_rule_ref
    if (salt_mass(1).eq.nodata_activity) salt_mass(:)=salt_mass_ref 
    if (salt_dilution(1).eq.nodata_activity) salt_dilution(:)=salt_dilution_ref 
    if (salt_type_distribution(1).eq.nodata_activity) salt_type_distribution(:)=salt_type_distribution_ref 
    
    if (sanding_hour(1,1).eq.nodata_activity) sanding_hour(1,:)=sanding_hour_ref(1)
    if (sanding_hour(2,1).eq.nodata_activity) sanding_hour(2,:)=sanding_hour_ref(2)
    if (delay_sanding_day(1).eq.nodata_activity) delay_sanding_day(:)=delay_sanding_day_ref 
    if (check_sanding_day(1).eq.nodata_activity) check_sanding_day(:)=check_sanding_day_ref
    if (min_temp_sand(1).eq.nodata_activity) min_temp_sand(:)=min_temp_sand_ref 
    if (max_temp_sand(1).eq.nodata_activity) max_temp_sand(:)=max_temp_sand_ref
    if (precip_rule_sand(1).eq.nodata_activity) precip_rule_sand(:)=precip_rule_sand_ref
    if (RH_rule_sand(1).eq.nodata_activity) RH_rule_sand(:)=RH_rule_sand_ref 
    if (g_sanding_rule(1).eq.nodata_activity) g_sanding_rule(:)=g_sanding_rule_ref 
    if (sand_mass(1).eq.nodata_activity) sand_mass(:)=sand_mass_ref 
    if (sand_dilution(1).eq.nodata_activity) sand_dilution(:)=sand_dilution_ref
    
    if (delay_ploughing_hour(1).eq.nodata_activity) delay_ploughing_hour(:)=delay_ploughing_hour_ref
    if (ploughing_thresh_2(1).eq.nodata_activity) ploughing_thresh_2(:)=ploughing_thresh_2_ref

    if (cleaning_hour(1,1).eq.nodata_activity) cleaning_hour(1,:)=cleaning_hour_ref(1)
    if (cleaning_hour(2,1).eq.nodata_activity) cleaning_hour(2,:)=cleaning_hour_ref(2)
    if (delay_cleaning_day(1).eq.nodata_activity) delay_cleaning_day(:)=delay_cleaning_day_ref
    if (min_temp_cleaning(1).eq.nodata_activity) min_temp_cleaning(:)=min_temp_cleaning_ref
    if (clean_with_salting(1).eq.nodata_activity) clean_with_salting(:)=clean_with_salting_ref
    if (start_month_cleaning(1).eq.nodata_activity) start_month_cleaning(:)=start_month_cleaning_ref
    if (end_month_cleaning(1).eq.nodata_activity) end_month_cleaning(:)=end_month_cleaning_ref
    if (wetting_with_cleaning(1).eq.nodata_activity) wetting_with_cleaning(:)=wetting_with_cleaning_ref
    if (efficiency_of_cleaning(1).eq.nodata_activity) efficiency_of_cleaning(:)=efficiency_of_cleaning_ref

    if (binding_hour(1,1).eq.nodata_activity) binding_hour(1,:)=binding_hour_ref(1)
    if (binding_hour(2,1).eq.nodata_activity) binding_hour(2,:)=binding_hour_ref(2)
    if (delay_binding_day(1).eq.nodata_activity) delay_binding_day(:)=delay_binding_day_ref
    if (check_binding_day(1).eq.nodata_activity) check_binding_day(:)=check_binding_day_ref
    if (min_temp_binding(1).eq.nodata_activity) min_temp_binding(:)=min_temp_binding_ref
    if (max_temp_binding(1).eq.nodata_activity) max_temp_binding(:)=max_temp_binding_ref
    if (precip_rule_binding(1).eq.nodata_activity) precip_rule_binding(:)=precip_rule_binding_ref
    if (RH_rule_binding(1).eq.nodata_activity) RH_rule_binding(:)=RH_rule_binding_ref
    if (g_binding_rule(1).eq.nodata_activity) g_binding_rule(:)=g_binding_rule_ref
    if (binding_mass(1).eq.nodata_activity) binding_mass(:)=binding_mass_ref
    if (binding_dilution(1).eq.nodata_activity) binding_dilution(:)=binding_dilution_ref
    if (start_month_binding(1).eq.nodata_activity) start_month_binding(:)=start_month_binding_ref
    if (end_month_binding(1).eq.nodata_activity) end_month_binding(:)=end_month_binding_ref
    


end subroutine read_NORTRIP_activities
!----------------------------------------------------------------------

