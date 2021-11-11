    subroutine NORTRIP_display_results
    !Routine for writing summary results to log or to file
    
    use NORTRIP_definitions
    
    implicit none
    
    !Functions
    real sd_func,mean_func,cov_func,rsquare_func,rmse_func,percentile_func,fb_func

    logical :: show_summary=.true.
    logical :: show_time=.false.
    logical :: write_to_output_file=.false.
    
    integer :: unit_output=20
    
    real b_factor,mean_factor
    integer x_load
    integer count
    real sum_P(20)
    real mean_energy(20)
    real mean_meteo(20)
    real mean_C_source(20)
    real mean_g_road(20)
    real mean_traffic(20)
    real stats_C(20)
    integer n_save_time
    
    character(10) sum_P_text(20)
    character(10) mean_energy_text(20)
    character(10) mean_meteo_text(20)
    character(10) mean_C_source_text(20)
    character(10) mean_g_road_text(20)
    character(10) mean_traffic_text(20)
    character(10) stats_C_text(20)
        
    if (.not.write_to_output_file) then
        unit_output=unit_logfile
    else
        !No option yet in the case where an output file is chosen. All output to log file
    endif
    
    n_save_time=max_time_save-min_time_save+1
    
    if(show_time) then
        tr=1
        do ti=min_time_save,max_time_save
            write(unit_output,'(a16,a2,f8.2,f8.2,f8.2,f8.2,f8.2,f8.1,f8.1,f8.1,f8.2,f8.2,f8.2,f8.2,f8.2)') trim(date_str(3,ti)), &
            ' =' &
            ,meteo_data(T_a_index,ti,ro) &
            ,road_meteo_data(T_s_index,ti,tr,ro) &
            ,g_road_data(water_index,ti,tr,ro) &
            ,g_road_data(snow_index,ti,tr,ro) &
            ,g_road_data(ice_index,ti,tr,ro) &
            ,sum(M_road_bin_data(road_index,:,ti,tr,ro))/b_road_lanes(ro)/1000 &
            ,sum(M_road_bin_data(salt_index(na),:,ti,tr,ro))/b_road_lanes(ro)/1000 &
            ,E_road_bin_data(total_dust_index,pm_10,E_total_index,ti,tr,ro) &
            ,E_road_bin_data(salt_index(na),pm_10,E_total_index,ti,tr,ro) &
            ,E_road_bin_data(exhaust_index,pm_25,E_total_index,ti,tr,ro) &
            ,f_q(road_index,ti,tr,ro) &
            ,road_salt_data(dissolved_ratio_index,1,ti,tr,ro) &
            ,C_data(total_dust_index,pm_10,C_total_index,ti,tr,ro)
        enddo
    endif
    
    if (show_summary) then
        !Calculate dust mass balance averages
        b_factor=1./1000./b_road_lanes(ro)
        x_load=pm_200
         
        do tr=1,num_track
            !Show the road dust balance parameters    
            sum_P(1)=sum(M_road_balance_data(road_index,x_load,P_wear_index,min_time_save:max_time_save,tr,ro))*b_factor;sum_P_text(1)='Roadwear'
            sum_P(2)=sum(M_road_balance_data(tyre_index,x_load,P_wear_index,min_time_save:max_time_save,tr,ro))*b_factor;sum_P_text(2)='Tyrewear'
            sum_P(3)=sum(M_road_balance_data(brake_index,x_load,P_wear_index,min_time_save:max_time_save,tr,ro))*b_factor;sum_P_text(3)='Brakewear'
            sum_P(4)=sum(M_road_balance_data(exhaust_index,x_load,P_depo_index,min_time_save:max_time_save,tr,ro))*b_factor;sum_P_text(4)='Exhaust'
            sum_P(5)=sum(M_road_balance_data(fugitive_index,x_load,P_depo_index,min_time_save:max_time_save,tr,ro))*b_factor;sum_P_text(5)='Fugitive'
            sum_P(6)=sum(M_road_balance_data(sand_index,x_load,P_depo_index,min_time_save:max_time_save,tr,ro))*b_factor;sum_P_text(6)='Sand'
            sum_P(7)=sum(M_road_balance_data(depo_index,x_load,P_depo_index,min_time_save:max_time_save,tr,ro))*b_factor;sum_P_text(7)='Depo'
            sum_P(8)=sum(M_road_balance_data(road_index,x_load,P_abrasion_index,min_time_save:max_time_save,tr,ro))*b_factor;sum_P_text(8)='Abrasion'
            sum_P(9)=sum(sum(M_road_balance_data(all_source_index,x_load,P_crushing_index,min_time_save:max_time_save,tr,ro),1))*b_factor;sum_P_text(9)='Crushing'
        
            sum_P(10)=sum(-M_road_balance_data(total_dust_index,x_load,S_suspension_index,min_time_save:max_time_save,tr,ro))*b_factor;sum_P_text(10)='Suspens.'
            sum_P(11)=sum(-M_road_balance_data(total_dust_index,x_load,S_dustdrainage_index,min_time_save:max_time_save,tr,ro))*b_factor;sum_P_text(11)='Drainage'
            sum_P(12)=sum(-M_road_balance_data(total_dust_index,x_load,S_dustspray_index,min_time_save:max_time_save,tr,ro))*b_factor;sum_P_text(12)='Spray'
            sum_P(13)=sum(-M_road_balance_data(total_dust_index,x_load,S_cleaning_index,min_time_save:max_time_save,tr,ro))*b_factor;sum_P_text(13)='Cleaning'
            sum_P(14)=sum(-M_road_balance_data(total_dust_index,x_load,S_windblown_index,min_time_save:max_time_save,tr,ro))*b_factor;sum_P_text(14)='Wind'
            sum_P(15)=sum(-M_road_balance_data(total_dust_index,x_load,S_dustploughing_index,min_time_save:max_time_save,tr,ro))*b_factor;sum_P_text(15)='Plough'
        
  	        write(unit_logfile,'(A)') '--------------------------------------------------------------------------------'
            write(unit_output,'(A,I6,A,I2)') 'Sum road dust PM200 mass balance production and sink (g/m^2): Road=',ro,' Track=',tr
  	        write(unit_logfile,'(A)') '--------------------------------------------------------------------------------'
            write(unit_output,'(15a10)') (ADJUSTR(trim(sum_P_text(i))),i=1,15)
            write(unit_output,'(15es10.2)') (sum_P(i),i=1,15) 
        
            !Show the energy budget
            mean_factor=1./(max_time_save-min_time_save+1)
            i=0;
            i=i+1;mean_energy(i)=sum(road_meteo_data(short_rad_net_index,min_time_save:max_time_save,tr,ro))*mean_factor;mean_energy_text(i)='SW_net'
            i=i+1;mean_energy(i)=sum(road_meteo_data(long_rad_net_index,min_time_save:max_time_save,tr,ro))*mean_factor;mean_energy_text(i)='LW_net'
            i=i+1;mean_energy(i)=sum(road_meteo_data(rad_net_index,min_time_save:max_time_save,tr,ro))*mean_factor;mean_energy_text(i)='Rad_net'
            i=i+1;mean_energy(i)=-sum(road_meteo_data(H_index,min_time_save:max_time_save,tr,ro))*mean_factor;mean_energy_text(i)='H_flux'
            i=i+1;mean_energy(i)=-sum(road_meteo_data(L_index,min_time_save:max_time_save,tr,ro))*mean_factor;mean_energy_text(i)='L_flux'
            i=i+1;mean_energy(i)=sum(road_meteo_data(H_traffic_index,min_time_save:max_time_save,tr,ro))*mean_factor;mean_energy_text(i)='Veh_flux'
            i=i+1;mean_energy(i)=sum(road_meteo_data(G_index,min_time_save:max_time_save,tr,ro))*mean_factor;mean_energy_text(i)='G_flux'
            i=i+1;mean_energy(i)=sum(road_meteo_data(G_sub_index,min_time_save:max_time_save,tr,ro))*mean_factor;mean_energy_text(i)='Gsub_flux'

  	        write(unit_logfile,'(A)') '--------------------------------------------------------------------------------'
            write(unit_output,'(A,I6,A,I2)') 'Mean energy budget (W/m^2): Road =',ro,' Track =',tr
  	        write(unit_logfile,'(A)') '--------------------------------------------------------------------------------'
            write(unit_output,'(14A10)') (ADJUSTR(trim(mean_energy_text(j))),j=1,i)
            write(unit_output,'(14F10.3)') (mean_energy(j),j=1,i) 

            !Show moisture budget
            !Rain              	Drainage          	Rain-drainage     	Evaporation       	Melt              	Freezing          	Spray             	Wetting           				
            mean_factor=1./(max_time_save-min_time_save+1)*24*dt
            i=0;
            m=water_index
            i=i+1;mean_g_road(i)=sum(g_road_balance_data(m,P_precip_index,min_time_save:max_time_save,tr,ro))*mean_factor;mean_g_road_text(i)='Rain'
            i=i+1;mean_g_road(i)=-sum(g_road_balance_data(m,S_drainage_index,min_time_save:max_time_save,tr,ro))*mean_factor;mean_g_road_text(i)='Drain'
            i=i+1;mean_g_road(i)=sum(g_road_balance_data(m,P_precip_index,min_time_save:max_time_save,tr,ro)-g_road_balance_data(m,S_drainage_index,min_time_save:max_time_save,tr,ro))*mean_factor;mean_g_road_text(i)='R-D'
            i=i+1;mean_g_road(i)=-sum(g_road_balance_data(m,S_evap_index,min_time_save:max_time_save,tr,ro))*mean_factor;mean_g_road_text(i)='Evap'
            i=i+1;mean_g_road(i)=sum(g_road_balance_data(m,P_evap_index,min_time_save:max_time_save,tr,ro))*mean_factor;mean_g_road_text(i)='Cond'
            i=i+1;mean_g_road(i)=sum(g_road_balance_data(m,P_melt_index,min_time_save:max_time_save,tr,ro))*mean_factor;mean_g_road_text(i)='Melt'
            i=i+1;mean_g_road(i)=-sum(g_road_balance_data(m,S_freeze_index,min_time_save:max_time_save,tr,ro))*mean_factor;mean_g_road_text(i)='Freeze'
            i=i+1;mean_g_road(i)=-sum(g_road_balance_data(m,S_spray_index,min_time_save:max_time_save,tr,ro))*mean_factor;mean_g_road_text(i)='Spray'
            i=i+1;mean_g_road(i)=sum(g_road_balance_data(m,P_roadwetting_index,min_time_save:max_time_save,tr,ro))*mean_factor;mean_g_road_text(i)='Wetting'
            i=i+1;mean_g_road(i)=-sum(g_road_balance_data(m,S_ploughing_index,min_time_save:max_time_save,tr,ro))*mean_factor;mean_g_road_text(i)='Plough_w'
            i=i+1;mean_g_road(i)=-sum(g_road_balance_data(snow_index,S_ploughing_index,min_time_save:max_time_save,tr,ro))*mean_factor;mean_g_road_text(i)='Plough_s'
 
            write(unit_logfile,'(A)') '--------------------------------------------------------------------------------'
            write(unit_output,'(A,I6,A,I2)') 'Road moisture balance (mm/day): Road =',ro,' Track =',tr
  	        write(unit_logfile,'(A)') '--------------------------------------------------------------------------------'
            write(unit_output,'(14A10)') (ADJUSTR(trim(mean_g_road_text(j))),j=1,i)
            write(unit_output,'(14F10.4)') (mean_g_road(j),j=1,i) 

            !Show traffic data
            !Number of days    	Mean ADT          	HDV (%)           	Mean speed (km/hr)	Mean studded (%LDV)	Max studded (%LDV)	Total salt (ton/km)	Salting(1) events 	Salting(2) events 	Sanding events    	Cleaning events   	Ploughing events  
            mean_factor=1./(max_time_save-min_time_save+1)
            mean_traffic=0.
            i=0;
            i=i+1;mean_traffic(i)=date_data(datenum_index,max_time_save)-date_data(datenum_index,min_time_save);mean_traffic_text(i)='Days'
            i=i+1;mean_traffic(i)=sum(traffic_data(N_total_index,min_time_save:max_time_save,ro))*mean_factor*24;mean_traffic_text(i)='ADT'
            i=i+1;
            if (sum(traffic_data(N_total_index,min_time_save:max_time_save,ro)).eq.0) then
                mean_traffic(i)=sum(traffic_data(N_v_index(he),min_time_save:max_time_save,ro))*mean_factor*100;mean_traffic_text(i)='%_HE'
            else
                mean_traffic(i)=sum(traffic_data(N_v_index(he),min_time_save:max_time_save,ro))/sum(traffic_data(N_total_index,min_time_save:max_time_save,ro))*100;mean_traffic_text(i)='%_HE'
            endif                
            i=i+1;
            if (sum(traffic_data(N_v_index(li),min_time_save:max_time_save,ro)).eq.0) then
                mean_traffic(i)=sum(traffic_data(V_veh_index(li),min_time_save:max_time_save,ro))*mean_factor
            else
                mean_traffic(i)=sum(traffic_data(V_veh_index(li),min_time_save:max_time_save,ro)*traffic_data(N_v_index(li),min_time_save:max_time_save,ro))/sum(traffic_data(N_v_index(li),min_time_save:max_time_save,ro));mean_traffic_text(i)='V_LI'
            endif                
            i=i+1;
            if (sum(traffic_data(N_v_index(he),min_time_save:max_time_save,ro)).eq.0) then 
                mean_traffic(i)=sum(traffic_data(V_veh_index(he),min_time_save:max_time_save,ro))*mean_factor
            else
                mean_traffic(i)=sum(traffic_data(V_veh_index(he),min_time_save:max_time_save,ro)*traffic_data(N_v_index(he),min_time_save:max_time_save,ro))/sum(traffic_data(N_v_index(he),min_time_save:max_time_save,ro));mean_traffic_text(i)='V_HE'
            endif           
            i=i+1;
            if (sum(traffic_data(N_v_index(li),min_time_save:max_time_save,ro)).eq.0) then
                mean_traffic(i)=sum(traffic_data(N_t_v_index(st,li),min_time_save:max_time_save,ro))*mean_factor*100;mean_traffic_text(i)='%ST'
            else
                mean_traffic(i)=sum(traffic_data(N_t_v_index(st,li),min_time_save:max_time_save,ro))/sum(traffic_data(N_v_index(li),min_time_save:max_time_save,ro))*100;mean_traffic_text(i)='%ST'
            endif
            i=i+1;
            if (sum(traffic_data(N_v_index(li),min_time_save:max_time_save,ro)).eq.0) then
                !This will not give the right answer but it should never call this either
                mean_traffic(i)=maxval(traffic_data(N_t_v_index(st,li),min_time_save:max_time_save,ro))*mean_factor*100;mean_traffic_text(i)='%ST_max'
            else
                mean_traffic(i)=maxval(traffic_data(N_t_v_index(st,li),min_time_save:max_time_save,ro)/traffic_data(N_v_index(li),min_time_save:max_time_save,ro))*100;mean_traffic_text(i)='%ST_max'
            endif           
            i=i+1;mean_traffic(i)=sum(M_road_balance_data(salt_index(1),pm_all,P_depo_index,min_time_save:max_time_save,tr,ro))*1e-3;mean_traffic_text(i)='S1(kg/km)'
            i=i+1;mean_traffic(i)=sum(M_road_balance_data(salt_index(2),pm_all,P_depo_index,min_time_save:max_time_save,tr,ro))*1e-3;mean_traffic_text(i)='S2(kg/km)'

            do ti=min_time_save,max_time_save
                if (activity_data(M_salting_index(1),ti,ro).gt.0) mean_traffic(i+1)=mean_traffic(i+1)+1
                if (activity_data(M_salting_index(2),ti,ro).gt.0) mean_traffic(i+2)=mean_traffic(i+2)+1
                if (activity_data(M_sanding_index,ti,ro).gt.0) mean_traffic(i+3)=mean_traffic(i+3)+1
                if (activity_data(t_cleaning_index,ti,ro).gt.0) mean_traffic(i+4)=mean_traffic(i+4)+1
                if (activity_data(t_ploughing_index,ti,ro).gt.0) mean_traffic(i+5)=mean_traffic(i+5)+1               
            enddo
            mean_traffic_text(i+1)='n_S1'
            mean_traffic_text(i+2)='n_S2'
            mean_traffic_text(i+3)='n_sand'
            mean_traffic_text(i+4)='n_clean'
            mean_traffic_text(i+5)='n_plough'
            i=i+5
            
            write(unit_logfile,'(A)') '--------------------------------------------------------------------------------'
            write(unit_output,'(A,I6,A,I2)') 'Traffic and activity data: Road =',ro,' Track =',tr
  	        write(unit_logfile,'(A)') '--------------------------------------------------------------------------------'
            write(unit_output,'(14A10)') (ADJUSTR(trim(mean_traffic_text(j))),j=1,i)
            write(unit_output,'(14F10.1)') (mean_traffic(j),j=1,i) 

            !Show meteorological data
            mean_factor=1./(max_time_save-min_time_save+1)
            i=0;
            i=i+1;mean_meteo(i)=sum(meteo_data(T_a_index,min_time_save:max_time_save,ro))*mean_factor;mean_meteo_text(i)='T_air'
            i=i+1;mean_meteo(i)=sum(road_meteo_data(T_s_index,min_time_save:max_time_save,tr,ro))*mean_factor;mean_meteo_text(i)='T_road'
            i=i+1;mean_meteo(i)=sum(meteo_data(RH_index,min_time_save:max_time_save,ro))*mean_factor;mean_meteo_text(i)='RH_air'
            i=i+1;mean_meteo(i)=sum(meteo_data(short_rad_in_index,min_time_save:max_time_save,ro))*mean_factor;mean_meteo_text(i)='SW_in'
            i=i+1;mean_meteo(i)=sum(meteo_data(cloud_cover_index,min_time_save:max_time_save,ro))*mean_factor*100.;mean_meteo_text(i)='Clouds'
            i=i+1;mean_meteo(i)=sum(meteo_data(Rain_precip_index,min_time_save:max_time_save,ro));mean_meteo_text(i)='Rain'
            i=i+1;mean_meteo(i)=sum(meteo_data(Snow_precip_index,min_time_save:max_time_save,ro));mean_meteo_text(i)='Snow'

            i=i+1;mean_meteo(i)=0;
            do ti=min_time_save,max_time_save
                if ((meteo_data(Snow_precip_index,ti,ro)+meteo_data(Rain_precip_index,ti,ro)).gt.0) mean_meteo(i)=mean_meteo(i)+1
            enddo
            mean_meteo(i)=mean_meteo(i)*mean_factor*100.;mean_meteo_text(i)='f_precip'
 
            i=i+1;mean_meteo(i)=0;
            do ti=min_time_save,max_time_save
                if (f_q(road_index,ti,tr,ro).lt.0.5) mean_meteo(i)=mean_meteo(i)+1
            enddo
            mean_meteo(i)=mean_meteo(i)*mean_factor*100.;mean_meteo_text(i)='f_wet'

            i=i+1;mean_meteo(i)=0;count=0
            do ti=min_time_save,max_time_save
                if (airquality_data(f_conc_index,ti,ro).ne.nodata) then
                    mean_meteo(i)=mean_meteo(i)+airquality_data(f_conc_index,ti,ro)
                    count=count+1
                endif
            enddo
            mean_meteo(i)=mean_meteo(i)/count;mean_meteo_text(i)='f_dis'
 
            write(unit_logfile,'(A)') '--------------------------------------------------------------------------------'
            write(unit_output,'(A,I6,A,I2)') 'Mean meteorological data: Road =',ro,' Track =',tr
  	        write(unit_logfile,'(A)') '--------------------------------------------------------------------------------'
            write(unit_output,'(14A10)') (ADJUSTR(trim(mean_meteo_text(j))),j=1,i)
            write(unit_output,'(14F10.3)') (mean_meteo(j),j=1,i) 
            
            !Show concentration data
            mean_factor=1./(max_time_save-min_time_save+1)

            mean_C_source_text(1)='Obs_total'
            mean_C_source_text(2)='Mod_total'
            mean_C_source_text(3)='Road'
            mean_C_source_text(4)='Tyre'
            mean_C_source_text(5)='Brake'
            mean_C_source_text(6)='Sand'
            mean_C_source_text(7)='Salt(1)'
            mean_C_source_text(8)='Salt(2)'
            mean_C_source_text(9)='Exhaust'
            mean_C_source_text(10)='Fugitive'
            mean_C_source_text(11)='Depo'
            mean_C_source_text(12)='Direct'
            mean_C_source_text(13)='Sus'
            mean_C_source_text(14)='Wind'
          
            count=0
            mean_C_source=0
            x=pm_10
            do ti=min_time_save,max_time_save
                if ((available_airquality_data(PM_net_index(x)).and.airquality_data(PM_net_index(x),ti,ro).ne.nodata.and.airquality_data(f_conc_index,ti,ro).ne.nodata) &
                    .or.(.not.available_airquality_data(PM_net_index(x)).and.airquality_data(f_conc_index,ti,ro).ne.nodata)) then
                !if (airquality_data(f_conc_index,ti,ro).ne.nodata) then
                    mean_C_source(1)=mean_C_source(1)+airquality_data(PM_net_index(x),ti,ro)
                    mean_C_source(2)=mean_C_source(2)+C_data(total_dust_index,x,C_total_index,ti,tr,ro)
                    mean_C_source(3)=mean_C_source(3)+C_data(road_index,x,C_total_index,ti,tr,ro)
                    mean_C_source(4)=mean_C_source(4)+C_data(tyre_index,x,C_total_index,ti,tr,ro)
                    mean_C_source(5)=mean_C_source(5)+C_data(brake_index,x,C_total_index,ti,tr,ro)
                    mean_C_source(6)=mean_C_source(6)+C_data(sand_index,x,C_total_index,ti,tr,ro)
                    mean_C_source(7)=mean_C_source(7)+C_data(salt_index(1),x,C_total_index,ti,tr,ro)
                    mean_C_source(8)=mean_C_source(8)+C_data(salt_index(2),x,C_total_index,ti,tr,ro)
                    mean_C_source(9)=mean_C_source(9)+C_data(exhaust_index,x,C_total_index,ti,tr,ro)
                    mean_C_source(10)=mean_C_source(10)+C_data(fugitive_index,x,C_total_index,ti,tr,ro)
                    mean_C_source(11)=mean_C_source(11)+C_data(depo_index,x,C_total_index,ti,tr,ro)
                    mean_C_source(12)=mean_C_source(12)+C_data(total_dust_index,x,C_direct_index,ti,tr,ro)
                    mean_C_source(13)=mean_C_source(13)+C_data(total_dust_index,x,C_suspension_index,ti,tr,ro)
                    mean_C_source(14)=mean_C_source(14)+C_data(total_dust_index,x,C_windblown_index,ti,tr,ro)
                    count=count+1

                endif
            enddo
            mean_C_source=mean_C_source/count
            i=14
            mean_C_source(1)=mean_func(airquality_data(PM_net_index(x),min_time_save:max_time_save,ro),n_save_time,nodata);

            write(unit_logfile,'(A)') '--------------------------------------------------------------------------------'
            write(unit_output,'(A,I6,A,I2)') 'Mean source contribution to PM10 concentrations or emissions (ug/m^3) or (g/km/hr) : Road =',ro,' Track =',tr
  	        write(unit_logfile,'(A)') '--------------------------------------------------------------------------------'
            write(unit_output,'(14A10)') (ADJUSTR(trim(mean_C_source_text(j))),j=1,i)
            write(unit_output,'(14F10.3)') (mean_C_source(j),j=1,i) 
           
            count=0
            mean_C_source=0
            x=pm_25
            do ti=min_time_save,max_time_save
                if ((available_airquality_data(PM_net_index(x)).and.airquality_data(PM_net_index(x),ti,ro).ne.nodata.and.airquality_data(f_conc_index,ti,ro).ne.nodata) &
                    .or.(.not.available_airquality_data(PM_net_index(x)).and.airquality_data(f_conc_index,ti,ro).ne.nodata)) then
                !if (airquality_data(f_conc_index,ti,ro).ne.nodata) then
                    mean_C_source(1)=mean_C_source(1)+airquality_data(PM_net_index(x),ti,ro)
                    mean_C_source(2)=mean_C_source(2)+C_data(total_dust_index,x,C_total_index,ti,tr,ro)
                    mean_C_source(3)=mean_C_source(3)+C_data(road_index,x,C_total_index,ti,tr,ro)
                    mean_C_source(4)=mean_C_source(4)+C_data(tyre_index,x,C_total_index,ti,tr,ro)
                    mean_C_source(5)=mean_C_source(5)+C_data(brake_index,x,C_total_index,ti,tr,ro)
                    mean_C_source(6)=mean_C_source(6)+C_data(sand_index,x,C_total_index,ti,tr,ro)
                    mean_C_source(7)=mean_C_source(7)+C_data(salt_index(1),x,C_total_index,ti,tr,ro)
                    mean_C_source(8)=mean_C_source(8)+C_data(salt_index(2),x,C_total_index,ti,tr,ro)
                    mean_C_source(9)=mean_C_source(9)+C_data(exhaust_index,x,C_total_index,ti,tr,ro)
                    mean_C_source(10)=mean_C_source(10)+C_data(fugitive_index,x,C_total_index,ti,tr,ro)
                    mean_C_source(11)=mean_C_source(11)+C_data(depo_index,x,C_total_index,ti,tr,ro)
                    mean_C_source(12)=mean_C_source(12)+C_data(total_dust_index,x,C_direct_index,ti,tr,ro)
                    mean_C_source(13)=mean_C_source(13)+C_data(total_dust_index,x,C_suspension_index,ti,tr,ro)
                    mean_C_source(14)=mean_C_source(14)+C_data(total_dust_index,x,C_windblown_index,ti,tr,ro)
                    count=count+1
                endif
            enddo
            mean_C_source=mean_C_source/count
            i=14
            mean_C_source(1)=mean_func(airquality_data(PM_net_index(x),min_time_save:max_time_save,ro),n_save_time,nodata);
            
            write(unit_logfile,'(A)') '--------------------------------------------------------------------------------'
            write(unit_output,'(A,I6,A,I2)') 'Mean source contribution to PM2.5 concentrations or emissions (ug/m^3) or (g/km/hr): Road =',ro,' Track =',tr
  	        write(unit_logfile,'(A)') '--------------------------------------------------------------------------------'
            write(unit_output,'(14A10)') (ADJUSTR(trim(mean_C_source_text(j))),j=1,i)
            write(unit_output,'(14F10.3)') (mean_C_source(j),j=1,i) 
        
            i=0
            x=pm_10
            i=i+1;stats_C(i)=mean_func(airquality_data(PM_net_index(x),min_time_save:max_time_save,ro),n_save_time,nodata);stats_C_text(i)='Mean_obs'
            i=i+1;stats_C(i)=mean_func(C_data(total_dust_index,x,C_total_index,min_time_save:max_time_save,tr,ro),n_save_time,nodata);stats_C_text(i)='Mean_mod'
            i=i+1;stats_C(i)=percentile_func(airquality_data(PM_net_index(x),min_time_save:max_time_save,ro),n_save_time,90.,nodata);stats_C_text(i)='90p_obs'
            i=i+1;stats_C(i)=percentile_func(C_data(total_dust_index,x,C_total_index,min_time_save:max_time_save,tr,ro),n_save_time,90.,nodata);stats_C_text(i)='90p_mod'
            i=i+1;stats_C(i)=sd_func(airquality_data(PM_net_index(x),min_time_save:max_time_save,ro),n_save_time,nodata);stats_C_text(i)='SD_obs'
            i=i+1;stats_C(i)=sd_func(C_data(total_dust_index,x,C_total_index,min_time_save:max_time_save,tr,ro),n_save_time,nodata);stats_C_text(i)='SD_mod'
            i=i+1;stats_C(i)=rsquare_func(C_data(total_dust_index,x,C_total_index,min_time_save:max_time_save,tr,ro),airquality_data(PM_net_index(x),min_time_save:max_time_save,ro),n_save_time,nodata);stats_C_text(i)='R2'
            i=i+1;stats_C(i)=rmse_func(C_data(total_dust_index,x,C_total_index,min_time_save:max_time_save,tr,ro),airquality_data(PM_net_index(x),min_time_save:max_time_save,ro),n_save_time,nodata);stats_C_text(i)='RMSE'
            i=i+1;stats_C(i)=stats_C(i-1)/stats_C(1)*100.;stats_C_text(i)='NRMSE%'
            i=i+1;stats_C(i)=fb_func(C_data(total_dust_index,x,C_total_index,min_time_save:max_time_save,tr,ro),airquality_data(PM_net_index(x),min_time_save:max_time_save,ro),n_save_time,nodata)*100.;stats_C_text(i)='FB%'        
       
            write(unit_logfile,'(A)') '--------------------------------------------------------------------------------'
            write(unit_output,'(A,I6,A,I2)') 'Statistics for PM10 concentrations (ug/m^3): Road =',ro,' Track =',tr
  	        write(unit_logfile,'(A)') '--------------------------------------------------------------------------------'
            write(unit_output,'(14A10)') (ADJUSTR(trim(stats_C_text(j))),j=1,i)
            write(unit_output,'(14F10.3)') (stats_C(j),j=1,i) 
       
            i=0
            x=pm_25
            i=i+1;stats_C(i)=mean_func(airquality_data(PM_net_index(x),min_time_save:max_time_save,ro),n_save_time,nodata);stats_C_text(i)='Mean_obs'
            i=i+1;stats_C(i)=mean_func(C_data(total_dust_index,x,C_total_index,min_time_save:max_time_save,tr,ro),n_save_time,nodata);stats_C_text(i)='Mean_mod'
            i=i+1;stats_C(i)=percentile_func(airquality_data(PM_net_index(x),min_time_save:max_time_save,ro),n_save_time,90.,nodata);stats_C_text(i)='90p_obs'
            i=i+1;stats_C(i)=percentile_func(C_data(total_dust_index,x,C_total_index,min_time_save:max_time_save,tr,ro),n_save_time,90.,nodata);stats_C_text(i)='90p_mod'
            i=i+1;stats_C(i)=sd_func(airquality_data(PM_net_index(x),min_time_save:max_time_save,ro),n_save_time,nodata);stats_C_text(i)='SD_obs'
            i=i+1;stats_C(i)=sd_func(C_data(total_dust_index,x,C_total_index,min_time_save:max_time_save,tr,ro),n_save_time,nodata);stats_C_text(i)='SD_mod'
            i=i+1;stats_C(i)=rsquare_func(C_data(total_dust_index,x,C_total_index,min_time_save:max_time_save,tr,ro),airquality_data(PM_net_index(x),min_time_save:max_time_save,ro),n_save_time,nodata);stats_C_text(i)='R2'
            i=i+1;stats_C(i)=rmse_func(C_data(total_dust_index,x,C_total_index,min_time_save:max_time_save,tr,ro),airquality_data(PM_net_index(x),min_time_save:max_time_save,ro),n_save_time,nodata);stats_C_text(i)='RMSE'
            i=i+1;stats_C(i)=stats_C(i-1)/stats_C(1)*100.;stats_C_text(i)='NRMSE%'
            i=i+1;stats_C(i)=fb_func(C_data(total_dust_index,x,C_total_index,min_time_save:max_time_save,tr,ro),airquality_data(PM_net_index(x),min_time_save:max_time_save,ro),n_save_time,nodata)*100.;stats_C_text(i)='FB%'        
       
            write(unit_logfile,'(A)') '--------------------------------------------------------------------------------'
            write(unit_output,'(A,I6,A,I2)') 'Statistics for PM2.5 concentrations (ug/m^3): Road =',ro,' Track =',tr
  	        write(unit_logfile,'(A)') '--------------------------------------------------------------------------------'
            write(unit_output,'(14A10)') (ADJUSTR(trim(stats_C_text(j))),j=1,i)
            write(unit_output,'(14F10.3)') (stats_C(j),j=1,i) 
        enddo

        !mean_factor=percentile_func(C_data(total_dust_index,pm_10,C_total_index,min_time_save:max_time_save,1,ro),90.0,nodata)
        
        !write(*,*) sd_func(C_data(total_dust_index,pm_10,C_total_index,min_time_save:max_time_save,tr,ro),nodata)
        !write(*,*) mean_func(airquality_data(PM_net_index(pm_10),min_time_save:max_time_save,ro),nodata)
        !write(*,*) 'R2',rsquare_func(C_data(total_dust_index,pm_10,C_total_index,min_time_save:max_time_save,1,ro),airquality_data(PM_net_index(pm_10),min_time_save:max_time_save,ro),nodata)
        !write(*,*) 'RMSE',rmse_func(C_data(total_dust_index,pm_10,C_total_index,min_time_save:max_time_save,1,ro),airquality_data(PM_net_index(pm_10),min_time_save:max_time_save,ro),nodata)
        !write(*,*) rsquare_func(airquality_data(PM_net_index(pm_10),min_time_save:max_time_save,ro),airquality_data(PM_net_index(pm_10),min_time_save:max_time_save,ro),nodata)
        !write(*,*) 'PER_90', percentile_func(airquality_data(PM_net_index(pm_10),min_time_save:max_time_save,ro),90.,nodata)
        !write(*,*) 'n_high', nhigh_func(airquality_data(PM_net_index(pm_10),min_time_save:max_time_save,ro),36,nodata)
    
    endif !Show summary
        
    end subroutine NORTRIP_display_results