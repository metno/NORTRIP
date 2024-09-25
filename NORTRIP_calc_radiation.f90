!NORTRIP_calc_radiation

!==========================================================================
!NORTRIP model
!SUBROUTINE:    calc_radiation
!VERSION:       12.03.2015
!AUTHOR:        Bruce Rolstad Denby (bruce.denby@met.no)
!DESCRIPTION:   Pre-calculates radiation paramters
!==========================================================================
    subroutine NORTRIP_calc_radiation
  
    use NORTRIP_definitions
    
    implicit none
    
    !Internal variables
    integer dti,ti1,ti2,ti_num,tt
    real short_rad_net_rmean,short_rad_net_clearsky_rmean,f_short_rad
    real cloud_cover_default
    real short_rad_net_temp
    integer a(6)
    real tau_cs_diffuse,tau_diffuse
    real h_canyon_temp(2)
    real h_canyon_temp2,theta
    real shadow_fraction(n_time)
    real short_rad_direct(num_track,n_time),short_rad_diffuse(num_track,n_time)
    real canyon_fraction,long_rad_canyon
    real :: sigma=5.67E-8
    real :: T0C=273.15
    real shadow_fraction1,shadow_fraction2
    
    !Functions
    real longwave_in_radiation_func,road_shading_func,road_shading_skyview_func

    !Start road loop
    do ro=n_roads_start,n_roads_end
    
        !Initialise arrays
        shadow_fraction=0.
        short_rad_direct=0.
        short_rad_diffuse=0.
        
        !Set the clear sky fraction of radiation that is diffuse
        tau_cs_diffuse=0.2
        
        !Set search window in hours for calculating cloud cover
        dti=11

        !Set initial cloud cover to default value if no data available
        cloud_cover_default=0.5
        do ti=min_time,max_time
            if (.not.available_meteo_data(cloud_cover_index).or.meteo_data(cloud_cover_index,ti,ro).eq.nodata) then
                meteo_data(cloud_cover_index,ti,ro)=cloud_cover_default
            endif
        end do

        !Calculate net shortwave radiation
        do ti=min_time,max_time
            a(1:5)=date_data(1:5,ti)
            a(6)=0
            if (available_meteo_data(short_rad_in_index).and.meteo_data(short_rad_in_index,ti,ro).ne.nodata) then
            !Calculate short wave net radiation when global radiation is available
                road_meteo_data(short_rad_net_index,ti,1:num_track,ro) = meteo_data(short_rad_in_index,ti,ro)*(1-albedo_road(ro))
                else   
                    !Calculate short wave net radiation when global radiation is not available
                    !+dt/2 is there so that it extracts between the solar time further west (ti-1 and ti), as in the solar angle 30 minutes ago sinc ehte radiation at time tt is based on accumulated between tt and tt-1
                    call global_radiation_sub(LAT(ro),LON(ro),a,DIFUTC_H(ro)-1./2,Z_SURF(ro),meteo_data(cloud_cover_index,ti,ro),albedo_road(ro),short_rad_net_temp,azimuth_ang(ti,ro),zenith_ang(ti,ro))
                    road_meteo_data(short_rad_net_index,ti,1:num_track,ro) =short_rad_net_temp
            endif
                
                    
            !Calculate clear sky short radiation
            call global_radiation_sub(LAT(ro),LON(ro),a,DIFUTC_H(ro)-1./2,Z_SURF(ro),0.,0.,meteo_data(short_rad_in_clearsky_index,ti,ro),azimuth_ang(ti,ro),zenith_ang(ti,ro))
            call global_radiation_sub(LAT(ro),LON(ro),a,DIFUTC_H(ro)-1./2,Z_SURF(ro),0.,albedo_road(ro),short_rad_net_temp,azimuth_ang(ti,ro),zenith_ang(ti,ro))
            road_meteo_data(short_rad_net_clearsky_index,ti,1:num_track,ro) = short_rad_net_temp
            !write(*,*) short_rad_net_temp,azimuth_ang(ti,ro),zenith_ang(ti,ro)
        enddo
    
    
        !Calculate cloud cover when cloud cover is not available and global is available
        !Calculate running means to calculate cloud cover per hour
        if ((.not.available_meteo_data(cloud_cover_index)).and.(available_meteo_data(short_rad_in_index))) then
            do ti=min_time,max_time
            if (meteo_data(short_rad_in_index,ti,ro).ne.nodata) then
                tr=1
                ti1=max(ti-dti,min_time)
                ti2=min(ti+dti,max_time)
                ti_num=ti2-ti1+1
                short_rad_net_rmean=0
                short_rad_net_clearsky_rmean=0
                do tt=ti1,ti2
                    short_rad_net_rmean=short_rad_net_rmean+road_meteo_data(short_rad_net_index,tt,tr,ro)/ti_num
                    short_rad_net_clearsky_rmean=short_rad_net_clearsky_rmean+road_meteo_data(short_rad_net_clearsky_index,tt,tr,ro)/ti_num
                enddo
                f_short_rad=short_rad_net_rmean/short_rad_net_clearsky_rmean
                f_short_rad=max(0.,f_short_rad)
                f_short_rad=min(1.,f_short_rad)
                meteo_data(cloud_cover_index,ti,ro)=min(1.,(1.-f_short_rad)/.9)
            else
                meteo_data(cloud_cover_index,ti,ro)=cloud_cover_default
            endif          
            enddo
        endif

        !Calculate incoming long wave radiation
        !available_meteo_data(long_rad_in_index)=.false.
        do ti=min_time,max_time
            if (.not.available_meteo_data(long_rad_in_index).or.meteo_data(long_rad_in_index,ti,ro).eq.nodata) then
                meteo_data(long_rad_in_index,ti,ro)=longwave_in_radiation_func(meteo_data(T_a_index,ti,ro),meteo_data(RH_index,ti,ro),meteo_data(cloud_cover_index,ti,ro),meteo_data(pressure_index,ti,ro))
            endif
        enddo

        !Calculate the shadow fraction
        if (canyon_shadow_flag.eq.1) then
            h_canyon_temp=h_canyon(:,ro)+.001!Avoids division by 0
            do ti=min_time,max_time
                shadow_fraction(ti) = road_shading_func(azimuth_ang(ti,ro),zenith_ang(ti,ro),ang_road(ro),b_road(ro),b_canyon(ro),h_canyon_temp)
                !shadow_fraction = 0
                tau_diffuse=tau_cs_diffuse+meteo_data(cloud_cover_index,ti,ro)*(1-tau_cs_diffuse)
                !write(*,*) shadow_fraction,tau_diffuse
                short_rad_direct(:,ti)=road_meteo_data(short_rad_net_index,ti,1:num_track,ro)*(1-tau_diffuse)*(1-shadow_fraction(ti))
                short_rad_diffuse(:,ti)=road_meteo_data(short_rad_net_index,ti,1:num_track,ro)*tau_diffuse    
                road_meteo_data(short_rad_net_index,ti,1:num_track,ro)=short_rad_direct(:,ti)+short_rad_diffuse(:,ti)
                !if (ro.eq.18163) then
                    !write(*,*) ti,shadow_fraction(ti),tau_diffuse,short_rad_direct(:,ti),short_rad_diffuse(:,ti),road_meteo_data(short_rad_net_index,ti,1:num_track,ro)    
                !    write(*,*) ti,shadow_fraction(ti),azimuth_ang(ti,ro),zenith_ang(ti,ro),ang_road(ro),b_road(ro),b_canyon(ro),h_canyon_temp,azimuth_ang(ti,ro)-ang_road(ro)
            !endif
            enddo
        endif

        !Calculate the shadow fraction using the skyview data
        !Can only return 0 or 1 for shadow fraction
        if (canyon_shadow_flag.eq.2) then
            do ti=min_time,max_time
                !write(*,*) 'n_skyview ',n_skyview
                shadow_fraction(ti) = road_shading_skyview_func(azimuth_ang(ti,ro),zenith_ang(ti,ro),ang_road(ro),az_skyview(:,ro),zen_skyview(:,ro),n_skyview)
                !shadow_fraction = 0
                tau_diffuse=tau_cs_diffuse+meteo_data(cloud_cover_index,ti,ro)*(1-tau_cs_diffuse)
                !write(*,*) shadow_fraction,tau_diffuse
                short_rad_direct(:,ti)=road_meteo_data(short_rad_net_index,ti,1:num_track,ro)*(1-tau_diffuse)*(1-shadow_fraction(ti))
                short_rad_diffuse(:,ti)=road_meteo_data(short_rad_net_index,ti,1:num_track,ro)*tau_diffuse    
                road_meteo_data(short_rad_net_index,ti,1:num_track,ro)=short_rad_direct(:,ti)+short_rad_diffuse(:,ti)
                !if (ro.eq.2) then
                    !write(*,*) ro,ti,shadow_fraction(ti),tau_diffuse,short_rad_direct(:,ti),short_rad_diffuse(:,ti),road_meteo_data(short_rad_net_index,ti,1:num_track,ro)    
                    !write(*,*) ro,ti,shadow_fraction(ti),azimuth_ang(ti,ro),zenith_ang(ti,ro),zen_skyview(:,ro)
                    !if (zenith_ang(ti,ro).lt.90) write(*,*) ro,ti,shadow_fraction(ti)
                !endif
            enddo
        endif

        !Calculate the shadow fraction using both the skyview and canyon data. if
        !Can only return 0 or 1 for shadow fraction
        if (canyon_shadow_flag.eq.3) then
            h_canyon_temp=h_canyon(:,ro)+.001!Avoids division by 0
            do ti=min_time,max_time
                shadow_fraction1 = road_shading_func(azimuth_ang(ti,ro),zenith_ang(ti,ro),ang_road(ro),b_road(ro),b_canyon(ro),h_canyon_temp)
                !write(*,*) 'n_skyview ',n_skyview
                shadow_fraction2 = road_shading_skyview_func(azimuth_ang(ti,ro),zenith_ang(ti,ro),ang_road(ro),az_skyview(:,ro),zen_skyview(:,ro),n_skyview)
                shadow_fraction(ti)=max(shadow_fraction1,shadow_fraction2)
                !shadow_fraction = 0
                tau_diffuse=tau_cs_diffuse+meteo_data(cloud_cover_index,ti,ro)*(1-tau_cs_diffuse)
                !write(*,*) shadow_fraction,tau_diffuse
                short_rad_direct(:,ti)=road_meteo_data(short_rad_net_index,ti,1:num_track,ro)*(1-tau_diffuse)*(1-shadow_fraction(ti))
                short_rad_diffuse(:,ti)=road_meteo_data(short_rad_net_index,ti,1:num_track,ro)*tau_diffuse    
                road_meteo_data(short_rad_net_index,ti,1:num_track,ro)=short_rad_direct(:,ti)+short_rad_diffuse(:,ti)
                !if (ro.eq.2) then
                    !write(*,*) ro,ti,shadow_fraction(ti),tau_diffuse,short_rad_direct(:,ti),short_rad_diffuse(:,ti),road_meteo_data(short_rad_net_index,ti,1:num_track,ro)    
                    !write(*,*) ro,ti,shadow_fraction(ti),azimuth_ang(ti,ro),zenith_ang(ti,ro),zen_skyview(:,ro)
                    !if (zenith_ang(ti,ro).lt.90) write(*,*) ro,ti,shadow_fraction(ti)
                !endif

            enddo
        endif

        !Canyon building fascade contribution to longwave radiation
        if (canyon_long_rad_flag.eq.1) then
            !This is based on the integral of a cylinder of height h_canyon. Could be done better
            !Uses the average of the two possible canyon heights
            h_canyon_temp2=(h_canyon(1,ro)+h_canyon(2,ro))/2.+0.001!Avoids division by 0
            !canyon_fraction=(1-sin(atan(b_canyon(ro)/2/h_canyon_temp)))!original
            theta=atan(h_canyon_temp2*2./b_canyon(ro))
            !canyon_fraction=(1-cos(2*theta))/2/3!factor 1/3 accounts for the non-cylyndrical nature
            canyon_fraction=(1-cos(2*theta/2))/2.!factor 2 for theta to get an average
            do ti=min_time,max_time
                long_rad_canyon=sigma*(T0C+meteo_data(T_a_index,ti,ro))**4.
                meteo_data(long_rad_in_index,ti,ro)=meteo_data(long_rad_in_index,ti,ro)*(1-canyon_fraction)+long_rad_canyon*canyon_fraction
            enddo
        endif

        !do ti=min_time,max_time
        !write(unit_logfile,'(a32,f14.2,f14.2,f14.2,f14.2,f14.2)') 'LONG SHORT SHORTNET SNET_CLEAR CC:', meteo_data(long_rad_in_index,ti,ro),meteo_data(short_rad_in_index,ti,ro),road_meteo_data(short_rad_net_index,ti,1,ro),road_meteo_data(short_rad_net_clearsky_index,ti,1,ro),meteo_data(cloud_cover_index,ti,ro)
        !enddo
        
        !Write for first and last road
        if (((ro.eq.1.or.ro.eq.n_roads).and..not.use_single_road_loop_flag).or.((ro_tot.eq.1.or.ro_tot.eq.n_roads_total).and.use_single_road_loop_flag)) then
        
            write(unit_logfile,'(A)') ''
            write(unit_logfile,'(A)') 'Calculating radiation (NORTRIP_calc_radiation)' 
            write(unit_logfile,'(A)') '================================================================'

            write(unit_logfile,'(A40,A3,L)') trim(meteo_match_str(short_rad_in_index))//' available',' = ',available_meteo_data(short_rad_in_index)
            write(unit_logfile,'(A40,A3,L)') trim(meteo_match_str(long_rad_in_index))//' available',' = ',available_meteo_data(long_rad_in_index)
            write(unit_logfile,'(A40,A3,L)') trim(meteo_match_str(cloud_cover_index))//' available',' = ',available_meteo_data(cloud_cover_index)

            write(unit_logfile,'(A)') '----------------------------------------------------------------'
            write(unit_logfile,'(a32,a14,a14,a14)') 'Radiation parameter','Min value','Max value','Mean value'
            write(unit_logfile,'(A)') '----------------------------------------------------------------'
            write(unit_logfile,'(a32,f14.2,f14.2,f14.2)') 'short_rad_in',minval(meteo_data(short_rad_in_index,min_time:max_time,ro)),maxval(meteo_data(short_rad_in_index,min_time:max_time,ro)),sum(meteo_data(short_rad_in_index,min_time:max_time,ro)/(max_time-min_time+1))
            write(unit_logfile,'(a32,f14.2,f14.2,f14.2)') 'short_rad_net',minval(road_meteo_data(short_rad_net_index,min_time:max_time,1,ro)),maxval(road_meteo_data(short_rad_net_index,min_time:max_time,1,ro)),sum(road_meteo_data(short_rad_net_index,min_time:max_time,1,ro)/(max_time-min_time+1))
            write(unit_logfile,'(a32,f14.2,f14.2,f14.2)') 'short_rad_net_clearsky',minval(road_meteo_data(short_rad_net_clearsky_index,min_time:max_time,1,ro)),maxval(road_meteo_data(short_rad_net_clearsky_index,min_time:max_time,1,ro)),sum(road_meteo_data(short_rad_net_clearsky_index,min_time:max_time,1,ro)/(max_time-min_time+1))
            write(unit_logfile,'(a32,f14.2,f14.2,f14.2)') 'shadow_fraction',minval(shadow_fraction(min_time:max_time)),maxval(shadow_fraction(min_time:max_time)),sum(shadow_fraction(min_time:max_time)/(max_time-min_time+1))
            write(unit_logfile,'(a32,f14.2,f14.2,f14.2)') 'short_rad_net_direct',minval(short_rad_direct(1,min_time:max_time)),maxval(short_rad_direct(1,min_time:max_time)),sum(short_rad_direct(1,min_time:max_time)/(max_time-min_time+1))
            write(unit_logfile,'(a32,f14.2,f14.2,f14.2)') 'short_rad_net_diffuse',minval(short_rad_diffuse(1,min_time:max_time)),maxval(short_rad_diffuse(1,min_time:max_time)),sum(short_rad_diffuse(1,min_time:max_time)/(max_time-min_time+1))
            write(unit_logfile,'(a32,f14.2,f14.2,f14.2)') 'long_rad_in',minval(meteo_data(long_rad_in_index,min_time:max_time,ro)),maxval(meteo_data(long_rad_in_index,min_time:max_time,ro)),sum(meteo_data(long_rad_in_index,min_time:max_time,ro)/(max_time-min_time+1))
            write(unit_logfile,'(a32,f14.2,f14.2,f14.2)') 'cloud_cover',minval(meteo_data(cloud_cover_index,min_time:max_time,ro)),maxval(meteo_data(cloud_cover_index,min_time:max_time,ro)),sum(meteo_data(cloud_cover_index,min_time:max_time,ro)/(max_time-min_time+1))
            write(unit_logfile,'(A)') '----------------------------------------------------------------'
            
        endif
    
    enddo !End road loop
    
    end subroutine NORTRIP_calc_radiation

!==========================================================================
    
    subroutine global_radiation_sub(LAT,LON,date_a,DIFUTC_H,Z_SURF,N_CLOUD,ALBEDO,SOLAR_NET,azimuth_ang,zenith_ang)
    !RETURNS THE NET SHORT WAVE RADIATION
    !DETERMINES SHORT WAVE FLUXES ON A HORIZONTAL SURFACE
    
    implicit none
    !INPUT
    real LAT,LON,DIFUTC_H,Z_SURF,N_CLOUD,ALBEDO
    integer date_a(6)
    !OUTPUT
    real SOLAR_NET,azimuth_ang,zenith_ang
    !INTERNAL
    real JULIAN_DAY,TIME_S,DAYANG,DEC,EQTIME,SOLARTIME,HOURANG,AZT,AZ
    real TAU_A,TAU_C,DAY_BIG,DAY_END,SOLAR_IN
    real SECPHOUR,SECPDAY,PI,S0
    parameter (SECPHOUR=3600.,SECPDAY=86400.,PI=3.14159/180.,S0=1367.)
    
    !FUNCTIONS
    !double precision date_to_number
    real date_to_julian
    
    JULIAN_DAY=date_to_julian(date_a)
    TIME_S=(JULIAN_DAY-1)*24.*3600.
    ![Y, M, D, H, MN, S] = datevec(date_num)
    !JULIAN_DAY=floor(date_num(i)-datenum(Y, 0, 0, 0, 0, 0)+1)
    !TIME_S=(date_num(i)-datenum(Y, M, D, 0, 0, 0))*24*3600

	DAYANG=360./365*(JULIAN_DAY-1.)
	DEC=0.396-22.91*cos(PI*DAYANG)+4.025*sin(PI*DAYANG)
	EQTIME=(1.03+25.7*cos(PI*DAYANG)-440.*sin(PI*DAYANG)-201.*cos(2.*PI*DAYANG)-562.*sin(2.*PI*DAYANG))/SECPHOUR
	SOLARTIME=mod(TIME_S+SECPDAY+SECPHOUR*(LON/15.+DIFUTC_H+EQTIME),SECPDAY)
	HOURANG=15.*(12.-SOLARTIME/SECPHOUR)
    
!	SET ZENITH ANGLE FOR ATMOSPHERIC CORRECTIONS
	AZT=sin(PI*DEC)*sin(PI*LAT)+cos(PI*DEC)*cos(PI*LAT)*cos(PI*HOURANG)
	if (abs(AZT).lt.1.) then
	  AZ=acos(AZT)/PI
    else
	  AZ=0.
    endif
    
    !write(*,*) AZT,AZ

!	CORRECTIONS FOR ATMOSPHERE AND CLOUD FROM OERLEMANS (GREENLAND)
    !These need to be updated
    !Have included a correction of 1.1 to match the Stockholm data
    !THe cloud cover transmission is still not assessed
	TAU_A=1.1*(0.75+6.8E-5*Z_SURF-7.1E-9*Z_SURF**2)*(1-.001*AZ)
    TAU_C=1-0.78*N_CLOUD**2*exp(-8.5E-4*Z_SURF)
    
    !New version Hottel (1976)
    !A simple model forestimating the transmittance of direct solar radiation
    !through clear atmosphere. Solar Energy 18,129, 1976.
    !This version is no better than the previous
    !a0=0.4237-0.00821*(6.0-min(2.5,Z_SURF/1000))^2
    !a1=0.5055+0.00595*(6.5-min(2.5,Z_SURF/1000))^2
    !k=0.2711+0.01858*(2.5-min(2.5,Z_SURF/1000))^2
    !a0=0.4237!-0.00821*(6.0-min(2.5,Z_SURF/1000))^2
    !a1=0.5055!+0.00595*(6.5-min(2.5,Z_SURF/1000))^2
    !k=0.2711!+0.01858*(2.5-min(2.5,Z_SURF/1000))^2
    !TAU_A=a0+a1*exp(-k./cos(AZ*PI))
    !Diffuse radition transmission
    !TAU_D=0.271-0.294*TAU_A
    !TAU_A=TAU_A+TAU_D

!	SET DAY BEGINNING AND END
	if (abs(tan(PI*DEC)*tan(PI*LAT)).lt.1.) then
	  DAY_BIG=(12.-acos(-tan(PI*DEC)*tan(PI*LAT))/PI/15.)*SECPHOUR
	  DAY_END=(12.+acos(-tan(PI*DEC)*tan(PI*LAT))/PI/15.)*SECPHOUR
    else
	  DAY_BIG=0.
	  DAY_END=24.*SECPHOUR
    endif
    
!	DETERMINE SOLAR RADIATION AT SURFACE DURING DAY
	if ((SOLARTIME.gt.DAY_BIG).and.(SOLARTIME.lt.DAY_END)) then
	  SOLAR_IN=S0*TAU_A*TAU_C*cos(AZ*PI)
    else
	  SOLAR_IN=0.
    endif
    
    SOLAR_NET=SOLAR_IN*(1-ALBEDO)
	!if (SOLARNEW.lt.0.) then
    !    SOLARNEW=0.
    !endif
    
    azimuth_ang=180-HOURANG
    zenith_ang=AZ
    
    end subroutine global_radiation_sub
    
 !==========================================================================
   
    function longwave_in_radiation_func(TC,RH,n_c,P)
    !Returns the incoming longwave radiation
    !based on Konzelman et al. (1994) and other related articles
    !See Sedlar and Hock (2008) On the use of incoming radiation
    !parameterisations in a glacier environment,

    implicit none
    
    real TC,P,RH,n_c
    real RL_in
    real longwave_in_radiation_func
    real esat,e_a,TK_a
    real e_sat_func
    real eps_cs,eps_cl,eps_eff
    
    !Set constants
    real T0C,sigma
    parameter(T0C=273.15,sigma=5.67E-8)

    esat = e_sat_func(TC,P)
    e_a=esat*RH/100
    TK_a=T0C+TC

    eps_cs=0.23+0.48*(e_a*100/TK_a)**(1./8.)
    eps_cl=0.97
    eps_eff=eps_cs*(1-n_c**2)+eps_cl*n_c**2
    RL_in=eps_eff*sigma*TK_a**4
    longwave_in_radiation_func=RL_in

    end function longwave_in_radiation_func
    
!==========================================================================
    
    function road_shading_func(azimuth,zenith,ang_road,b_road,b_canyon,h_canyon)
    !AZ is actually the zenith angle
    !HOURANG is actually the azimuth
    
    implicit none

    real azimuth,zenith,ang_road,b_road,b_canyon
    real h_canyon(2)
    real shadow_fraction
    real road_shading_func
    real ang_dif,h_canyon_temp
    real d_shadow,b_kerb,b1_kerb,b1_road
    
    
    if (ang_road.gt.180) then
        ang_road=ang_road-180.
    endif

    ang_dif=azimuth-ang_road
    
    !Avoid a roundoff problem
    if (abs(ang_dif).lt.1.0e-3) then
        ang_dif=0.
    endif
    

    if (ang_dif.eq.360) then
        ang_dif=0.
    endif

    if (ang_dif.le.-180) then
        h_canyon_temp=h_canyon(2)
        ang_dif=ang_dif+360.
    elseif (ang_dif.lt.0) then
        h_canyon_temp=h_canyon(1)
        ang_dif=ang_dif+180.
    elseif (ang_dif.ge.180) then
        h_canyon_temp=h_canyon(1)
        ang_dif=ang_dif-180.
    else
        h_canyon_temp=h_canyon(2)
    endif

    if (ang_dif.eq.0) then
        shadow_fraction=0.
    elseif (zenith.ge.90) then
        shadow_fraction=1.
    else
        d_shadow=h_canyon_temp*tand(zenith)
        b_kerb=max(0.,(b_canyon-b_road)/2.)
        b1_kerb=b_kerb/sind(ang_dif)
        b1_road=b_road/sind(ang_dif)
        shadow_fraction=max(0.,(d_shadow-b1_kerb)/b1_road)
        shadow_fraction=min(1.,shadow_fraction)
    endif

    road_shading_func=shadow_fraction
    
    end function road_shading_func
!==========================================================================
    
    function road_shading_skyview_func(azimuth,zenith,ang_road,az_skyview,zen_skyview,n_skyview)
    
    implicit none

    integer n_skyview
    real azimuth,zenith,ang_road
    real az_skyview(n_skyview),zen_skyview(n_skyview)
    real road_shading_skyview_func
    real zen_skyview_interp
    integer ang_index1,ang_index2
    real delta_az,az1,az2
    
    !Find the index to the nearest skyview azimuth
    delta_az=360./n_skyview
    ang_index1=floor(azimuth/delta_az+1)
    az1=az_skyview(ang_index1)
    az2=az1+delta_az
    ang_index2=ang_index1+1
    if (ang_index1.eq.n_skyview) then
        ang_index2=1
    endif
    
    !Linear interpolation to find the zenith angle from the skyview data
    zen_skyview_interp=zen_skyview(ang_index1)+(zen_skyview(ang_index2)-zen_skyview(ang_index1))*(azimuth-az1)/delta_az
    
    if (zenith.lt.zen_skyview_interp) then
        !Shaded
        road_shading_skyview_func=0.
    else
        !Not shaded
        road_shading_skyview_func=1.
    endif
    
    !write(*,*) road_shading_skyview_func,azimuth,zenith,zen_skyview_interp,zen_skyview
     
    end function road_shading_skyview_func
!==========================================================================