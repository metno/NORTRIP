!----------------------------------------------------------------------
      subroutine ospm_nortrip_control
      !Called within the road loop
      
      use NORTRIP_definitions
    
      implicit none

      !Static variables
      real H_ospm_in,L_ospm_in,SL1_ospm_in,SL2_ospm_in,P1_ospm_in,RecHeight_ospm_in,f_roof_ospm_in,f_turb_ospm_in
      integer isub_ospm_in,nexc_ospm_in
      real dd_l_ospm_in(12),dd_u_ospm_in(12),gg_ospm_in(12)
      
      !time dependent variables
      real U_mast_ospm(n_time),wind_dir_ospm(n_time),TK_ospm(n_time),GlobalRad_ospm(n_time),cNOx_b_ospm(n_time)
      real NNp_ospm(n_time),NNt_ospm(n_time),Vp_ospm(n_time),Vt_ospm(n_time),qNOX_ospm(n_time)

      !output
      real f_dis_ospm(n_time,2)
      
      !Feed the OSPM routine with data
      isub_ospm_in=2
      H_ospm_in=h_canyon(1,ro)    !defined in metadata
      L_ospm_in=b_canyon(ro)      !defined in metadata
      SL1_ospm_in=SL1_ospm(ro)    !defined in metadata
      SL2_ospm_in=SL2_ospm(ro)    !defined in metadata
      P1_ospm_in=ang_road(ro)
      nexc_ospm_in=1              
      dd_l_ospm_in(1)=359
      dd_u_ospm_in(1)=360
      gg_ospm_in(1)=H_ospm_in
      RecHeight_ospm_in=RecHeight_ospm(ro)  !defined in metadata
      f_roof_ospm_in=f_roof_ospm(ro)     !defined in metadata
      f_turb_ospm_in=f_turb_ospm(ro)     !defined in metadata

      !Alternative formulation if sides of canyon are different
      dd_l_ospm_in(1)=ang_road(ro)
      dd_u_ospm_in(1)=180+ang_road(ro)
      gg_ospm_in(1)=h_canyon(2,ro)
      
      U_mast_ospm(:)=meteo_data(FF_index,:,ro)/wind_speed_correction(ro)
      wind_dir_ospm(:)=meteo_data(DD_index,:,ro)
      TK_ospm(:)=meteo_data(T_a_index,:,ro)+273.15
      GlobalRad_ospm(:)=meteo_data(short_rad_in_index,:,ro)
      cNOx_b_ospm(:)=0.
      NNp_ospm(:)=traffic_data(N_v_index(li),:,ro)
      NNt_ospm(:)=traffic_data(N_v_index(he),:,ro)
      Vp_ospm(:)=traffic_data(V_veh_index(li),:,ro)
      Vt_ospm(:)=traffic_data(V_veh_index(he),:,ro)
      qNOX_ospm(:)=1./3.6;!unit emissions including conversion
      
      do ti=min_time,max_time
          !write(unit_logfile,'(i6,10e12.4)') ti,U_mast_ospm(ti),wind_dir_ospm(ti),TK_ospm(ti),GlobalRad_ospm(ti),cNOx_b_ospm(ti),NNp_ospm(ti),NNt_ospm(ti),Vp_ospm(ti),Vt_ospm(ti),qNOX_ospm(ti) 
          !write(unit_logfile,'(i6,10e12.4)') ti,U_mast_ospm(ti),meteo_data(FF_index,ti,ro),meteo_data(FF_index,ti,ro)/wind_speed_correction(ro)
          call ospm_nortrip_simair(isub_ospm_in,H_ospm_in,L_ospm_in,SL1_ospm_in,SL2_ospm_in,P1_ospm_in &
          ,nexc_ospm_in,dd_l_ospm_in,dd_u_ospm_in,gg_ospm_in,RecHeight_ospm_in,f_roof_ospm_in,f_turb_ospm_in &
          ,U_mast_ospm(ti),wind_dir_ospm(ti),TK_ospm(ti),GlobalRad_ospm(ti),cNOx_b_ospm(ti),NNp_ospm(ti),NNt_ospm(ti),Vp_ospm(ti),Vt_ospm(ti),qNOX_ospm(ti),1 &
          ,f_dis_ospm((ti),1),f_dis_ospm((ti),2))
      
          !Take the average if option is 3
          if (choose_receptor_ospm(ro).eq.3) then
              airquality_data(f_conc_index,ti,ro)=(f_dis_ospm(ti,1)+f_dis_ospm(ti,2))/2.
          else
              airquality_data(f_conc_index,ti,ro)=f_dis_ospm(ti,choose_receptor_ospm(ro))
          endif
      enddo
    
      end subroutine ospm_nortrip_control
!----------------------------------------------------------------------


!----------------------------------------------------------------------
      !This is a very old and messy routine but was the one used in NORTRIP matlab.
      !Should be shifted out at the earliest opportunity!
      !Rewritten to run as free form subroutine and to loop around n_time
      subroutine ospm_nortrip_simair(isub,H,L,SL1,SL2,P1,nexc,dd_l,dd_u,gg,RecHeight,f_roof,f_turb &
          ,U_mast,wind_dir,TK,GlobalRad,cNOx_b,NNp,NNt,Vp,Vt,qNOX,n_time &
          ,cNOX_mod_1b,cNOX_mod_2b)
      
      !Input
      integer isub,nexc
      real H,L,SL1,SL2,P1
      real dd_l(12),dd_u(12),gg(12)
      real RecHeight,f_roof,f_turb
      real U_mast,wind_dir,TK,GlobalRad
      real cNOx_b,NNp,NNt,Vp,Vt,qNOX
      integer n_time
      
      !Output
      real cNOX_mod_1b,cNOX_mod_2b

      integer time_counter
      
!  OSPM_SIMAIR.for
!
! OBS! 2012 10 04 för NO2 testerna skiver jag nu ut totala NO2 halter inte bara det lokala bidraget!!
!
!  The OSPM model for the SIMAIR system
! version:2004-04-27 G.Omstedt, SMHI
! change 2004-04-28 new input cNO_b and new output for NOx and NO2 only local contributions
! change 2004-05-17 input of temp on K
! change 2004-08-23 introduced ISUB which is an index for type of substance for the calculations
! change 2004-08-27 bugg for calculation of local contributions
! change 2004-12-13 changes so local calculated NO2 is >= 0.0
! change 2004-12-16 new formates for outputs (g12.4)
! change 2006-0105 Local contribution of NO2 must always be equal or less than local
!                  contribution of NOx
! change 2007-02-21 Reading additional emission NO2 (named qNO2). 
!                   If an old EDB (not ARTEMIS) is called, qNO2 will be set to 0.05*qNOX.
! ***********MAIN PROGRAM**********

      parameter(ppbNO2=0.51282)
! 
! old value ppbNO2=0.53
! *********Input Variables******************************
! Street data*********
! 
! ISUB: index for type of substance: ISUB=1 for NOx,NO2 and O3 which includes chemistry 
!       and ISUB=2 for  chemical passive substances such as PM10,CO and Benzene
! H: height of the street canyon (m)
! L: width of the street (m)
! P1: orientation of the street with respect to North (degree)
! SL1, SL2: the length of the street from the rec. point to the
!           nearest larger intersection in both directions (m)
! dd_l, dd_u, gg: lower and uper limiths of the wind sectors where
! the building heights (given by gg) are significantly different from H
! nexc: number of such wind sectors
! RecHeight: height of the calculation point (m)
! 
! Traffic data*********
! 
! NNp, NNt:  Number of light and heavy vehicles (veh/hour)
! Vp, Vt: traveling speed of light and heavy vehicles (km/hour)
! qNOX: emission of NOX or cSUB2 from the vehicles in the street (ug/m/s)
! qNO2: emission of NO2 from the vehicles in the street (ug/m/s)
! 
! Urban background**********
! ISUB=1
! cNO_b, cNO2_b, cO3_b: urban background concentrations of NO,
! NO2 and O3 (µg/m3)
! ISUB=2
! cSUB2_b: urban background consentration of cSUB2 (µg/m3) (=cPM10,cCO,cBensen)
!
! Meteorology*********
!
! u_mast: wind speed measured on a mast located on a roof (m/s)
! Wind_dir: wind direction (degree)
! TK: air temperature ( K ) OBS!!!!!!!!!!!!
! GlobalRad: Global radiation (W/m2)
!
! ********Output Variables*****************************
! ISUB=1---------------------------
! cNOX_mod_1, cNO2_mod_1, cO3_mod_1: modelled local concentrations
! for side 1 of the street (µg/m3)
! cNOX_mod_2, cNO2_mod_2, cO3_mod_2: modelled local concentrations
! for side 2 of the street (µg/m3))
! but not for O3 they are total
!    OBS! ändrat  till µg/m3 faktor 1.95 för nox och no2 och 2.03 för o3
! ISUB=2 --------------------------
! cSUB2_mod_1,cSUB2_mod_2: modelled local concentrations for side 1 and 2 of the street (µg/m3)
! ****************************************************
! Missing data data:-1. utom Temp=-99., GlobalRad=-99.
! ****************************************************
! 
! 
!      Real dd_l(12),dd_u(12),gg(12),lb(12),ub(12)
!      Real NNp,NNt,L
!	real cNOX_mod_1b,cNO2_mod_1b,cO3b_mod_1b
!	real cNOX_mod_2b,cNO2_mod_2b,cO3b_mod_2b
!      Integer  Year,day,month,hour,isub,ic
!      real f_roof   !Bruce: Introduced f_roof as an input variable
!      real f_turb  !Bruce: Introduced f_turb as an input variable

      Real lb(12),ub(12)
	real cNO2_mod_1b,cO3b_mod_1b
	real cNO2_mod_2b,cO3b_mod_2b
      Integer  Year,day,month,hour,ic
      !
!     Bruce: Set the file names and positions local to executable
!      open(9,file='D:\OSPMtest\input\Hornsgatan.txt')
      !open(9,file='.\input\nortrip_ospm_parameters.txt')
!      open(9,file='C:\NORTRIP SMHI\Gunnar Omstedt\OSPM model test\input
!     #\Hornsgatan.txt')
      
!
      
!     open(10,file='D:\OSPMtest\input\H_2007_NOx_1.txt')
!      open(20,file='D:\OSPMtest\output\Hut_2007_NOx_1.txt')
      !open(10,file='.\input\nortrip_ospm_input.txt')
      !open(20,file='.\output\nortrip_ospm_output.txt')
!      open(10,file='C:\NORTRIP SMHI\Gunnar Omstedt\OSPM model test\input
!     #\H_2007_NOx_1.txt')
!      open(20,file='C:\NORTRIP SMHI\Gunnar Omstedt\OSPM model test\
!     #output\Hut_2007_NOx_1.txt')
!
      !Bruce: Default values for input paramters if they do not exist
      !f_roof=0.82
      !f_turb=1.0
      
	!Read(9,*) isub
      !Read(9,*) H,L,SL1,SL2,P1
      !Read(9,*) nexc
      !Read(9,*) (dd_l(i),i=1,nexc)
      !Read(9,*) (dd_u(i),i=1,nexc)
      !Read(9,*) (gg(i),i=1,nexc)
      !Read(9,*) RecHeight
      !Read(9,*) f_roof  !Bruce: f_roof as input paramater
      !Read(9,*) f_turb  !Bruce: f_turb as input paramater
!
      irec1=1
      irec2=1
	cNOX_mod_1b=-1.
	cNO2_mod_1b=-1.
	cO3b_mod_1b=-1.
!
	cNOX_mod_2b=-1.
	cNO2_mod_2b=-1.
	cO3b_mod_2b=-1.
!
      Call ModPar(H, precis, h0, z0, Alfa, Beta, SSp2, SSt2)
!
      Call RotStreet(P1, dd_l, dd_u, lb, ub, nexc)
!
! Heading of output
!
!	if(isub.eq.1) write(20,*)'year mm dd hh u dd cNOX_mod_1 cNO2_mod_1 cO3_mod_1 cNOX_mod_2 cNO2_mod_2 cO3_mod_2 qNOX qNO2'
!	if(isub.eq.2) write(20,*)'year mm dd hh u dd csub2_mod_1 csub2_mod_2 qNOX' 
! **********Time Loop***********************************
!
      time_counter=0
   10 Continue
! ic=indikator för utskriften, om ical=0 är ic=-1
      time_counter=time_counter+1
      if (time_counter.eq.n_time+1) goto 2000
      
      ic=1
	if(isub.eq.1)then
        !Read(10,*,end=2000) year,month,day,hour,U_mast,wind_dir,TK,GlobalRad,cNO_b,cNO2_b,cO3_b, NNp,NNt,Vp,Vt,qNOX,qNO2
!
	   Temp=TK-273.15
! tillfälligt 2012 10 04
         if(cNO_b.lt.0.0)then
           cNOx_b=cNO2_b
         else
	      cNOx_b=cNO_b*1.5333+cNO2_b
	   endif
! change to ppb
	   cNOX_b=cNOX_b/1.95
	   cNO2_b=cNO2_b/1.95
	   cO3_b=cO3_b/2.03
!
! Measurements from Jagtvej*(cnox_s and cno2_s; cnox2 and cno22
! are model results with the extended version of OSPM
!
!   används ej!
!
!      Read(11,*,end=2000) day,month,hour,DayOfWeek,
!     #U_mast,wind_dir,
!     #cnox_s,cnox2,cno2_s,cno22
!
      icalc=1
      if(u_mast.lt.0.) icalc=0
      if(Wind_dir.lt.0.) icalc=0
	if(icalc.eq.0) then  !No calculations when Met. data missing
        go to 55
	endif
	if(qNOx.lt.0.)then ! Inga beräkningar då emission saknas 
	  qNOx=-1.
	  go to 55
	endif 
	
!

        iNOX=1
        iNO2=1
        if(cNOX_b.lt.0.) iNOX=0      !Conditions for calculation of NOX
        if(cNO2_b.lt.0.) iNO2=0      !Conditions for calculation of NO2
        if(cO3_b.lt.0.) iNO2=0       !Conditions for calculation of NO2
        if(GlobalRad.lt.-50.) iNO2=0 !Conditions for calculation of NO2
        if(Temp.lt.-50.) iNO2=0      !Conditions for calculation of NO2
        if(iNOX.eq.0) iNO2=0         !Conditions for calculation of NO2
      endif
! ISUB=2
	if(isub.eq.2)then
	        !Read(10,*,end=2000) year,month,day,hour,U_mast,wind_dir,TK,GlobalRad,cNOx_b,NNp,NNt,Vp,Vt,qNOX
!
	   Temp=TK-273.15
! change to ppb
	   cNOX_b=cNOX_b/1.95
         icalc=1
         if(u_mast.lt.0.) icalc=0
         if(Wind_dir.lt.0.) icalc=0
	   if(icalc.eq.0) then  !No calculations when Met. data missing
            go to 55
	    endif
	    if(qNOx.lt.0.)then ! Inga beräkningar då emission saknas
	      qNOx=-1.
	      go to 55
	    endif 
!
        iNOX=1
        iNO2=0
        if(cNOX_b.lt.0.) iNOX=0      !Conditions for calculation of NOX
	endif
!
! WD is wind direction with respect to the street axis
!
      WD = Wind_dir - P1
      If(WD.lt.0.) WD = WD + 360.
!
      Call Flow_par(u_mast, f_roof, u_roof, r_wind) !Bruce: f_roof as input variable
!
! Delta_Fi is the the wind direction averaging sector (meandering)
!
      Call Calc_Delta_Fi(u, Delta_Fi)
!
! s_w0 is the traffic produced turbulence (m/s)
!
      Call CarTurb(Vp, Vt, NNp, NNt, SSp2, SSt2, L, f_turb, s_w0)
      !Bruce: included scaling for traffic turbulence
!
! The subroutine Averaging calculates concentrations averaged over a
! wind sector given by +- Delta_Fi. The concentrations for the mean wind
! direction are calculated by the subroutine SC_st.
! The output is given by c C_st1 and C_st2, which are concentrations on
! the both sides of the c street but for an unit emission.
!
! The subroutine NO2NOX calculates transformation of NO to NO2, given
! background concentrations of NOX, NO2 and O3 and the modelled NOX
! concentration in the street. The reaction rates are calculated
! depending on the temperature and Global Radiation. The residence time
! of pollutant in the street is given by tau.
! The direct emission rate of NO2 is given by fno2; here we assume that
! it is constant (= 5%)
! s_wt is the turbulent exchange velocity at the top of
! the street canyon.

       Call Averaging(u_roof, WD, Delta_Fi, precis, nexc, lb, ub, gg,s_w0, s_wt, H, L, h0, SL1, SL2, r_wind, Alfa, Beta, irec1, irec2,C_st1, C_st2, RecHeight)

! *          fno2 = 0.05
          fno2 = qNO2/qNOX
          tau = H / s_wt
!
! ***** RECEPTOR 1 *********************
!
      cNOX_mod_1=-1.
      cNO2_mod_1=-1.
      cO3_mod_1 =-1.
!
      If(irec1.eq.1.and.iNOX.eq.1) Then
       cNOX_mod_1 = C_st1 * (qNOX*ppbNO2) + cNOX_b
       If(iNO2.eq.1) Then
        Call NO2NOX(cNOX_b, cO3_b, cNO2_b,cNOX_mod_1, cNO2_mod_1, cO3_mod_1,fno2, GlobalRad, Tk, tau)
       End If
      End If
!
! ***** RECEPTOR 2 *********************
!
      cNOX_mod_2=-1.
      cNO2_mod_2=-1.
      cO3_mod_2 =-1.
!
      If(irec2.eq.1.and.iNOX.eq.1) Then
       cNOX_mod_2 = C_st2 * (qNOX*ppbNO2) + cNOX_b
       If(iNO2.eq.1) Then
        Call NO2NOX(cNOX_b, cO3_b, cNO2_b,cNOX_mod_2, cNO2_mod_2, cO3_mod_2,fno2, GlobalRad, Tk, tau)
       End If
      End If
!
! ****************************************
!
!      write(20,100) day,month,hour,DayOfWeek,
!     #U_mast,wind_dir,
!     #cNOX_s,cNOX_mod_2,cNOX_s-cNOX_b,cNOX_mod_2-cNOX_b,
!     #cNO2_s,cNO2_mod_2,cNOX_b
!
!  change to  µg/m3
	if(isub.eq.1)then
	 cNOX_mod_1b=cNOX_mod_1*1.95
	 cNO2_mod_1b=cNO2_mod_1*1.95
	 cO3_mod_1b=cO3_mod_1*2.03
!
	 cNOX_mod_2b=cNOX_mod_2*1.95
	 cNO2_mod_2b=cNO2_mod_2*1.95
	 cO3_mod_2b=cO3_mod_2*2.03
!
!
! change to local conntributions but only for NOx and NO2/ Tar jag bort 2012 10 04!!!!!!!!!!!!!
!
!	 cNOX_mod_1b=cNOX_mod_1b-cNOx_b*1.95
!	if(qNOx.eq.0.0)then
!	 cNO2_mod_1b=0.0
!	else
!	 cNO2_mod_1b=amax1((cNO2_mod_1b-cNO2_b*1.95),fno2*cNOX_mod_1b,0.0)
!	endif
!	 cNOX_mod_2b=cNOX_mod_2b-cNOx_b*1.95
!	if(qNOx.eq.0.0)then
!	 cNO2_mod_2b=0.0
!	else
!	 cNO2_mod_2b=amax1((cNO2_mod_2b-cNO2_b*1.95),fno2*cNOX_mod_2b,0.0)
!	endif
        if(cNO2_mod_1b.gt.cNOx_mod_1b) cNO2_mod_1b = cNOx_mod_1b
        if(cNO2_mod_2b.gt.cNOx_mod_2b) cNO2_mod_2b = cNOx_mod_2b
!
	endif
	if(isub.eq.2)then
	 cNOX_mod_1b=cNOX_mod_1*1.95
!
	 cNOX_mod_2b=cNOX_mod_2*1.95
!
!
! change to local conntributions but only for NOx Tar jag bort 2013 01 17!!!!!!!!!!!!!
!
!	 cNOX_mod_1b=cNOX_mod_1b-cNOx_b*1.95
!	 cNOX_mod_2b=cNOX_mod_2b-cNOx_b*1.95
	endif
!
  55  continue

	if(cNOX_mod_1.lt.0.)cNOX_mod_1b=-1.
	if(cNO2_mod_1.lt.0.)cNO2_mod_1b=-1.
	if(cO3_mod_1.lt.0.)cO3_mod_1b=-1.
		
	if(cNOX_mod_2.lt.0.)cNOX_mod_2b=-1.
	if(cNO2_mod_2.lt.0.)cNO2_mod_2b=-1.
	if(cO3_mod_2.lt.0.)cO3_mod_2b=-1.
!
!	if(isub.eq.1)then
!	  if(icalc.eq.0)then
!	    ic=-1
!	    write(20,101)year,month,day,hour,U_mast,wind_dir,ic,ic,ic,ic,ic,ic,ic
!          go to 10
!        endif
!	 write(20,101)year,month,day,hour,U_mast,wind_dir,cNOX_mod_1b,cNO2_mod_1b,cO3_mod_1b,cNOX_mod_2b,cNO2_mod_2b,cO3_mod_2b,qNOx
!	endif
!	if(isub.eq.2)then
!	if(icalc.eq.0)then
!	  ic=-1
!	  write(20,101)year,month,day,hour,U_mast,wind_dir,ic,ic,ic
!        go to 10
!        endif
!	 write(20,102)year,month,day,hour,U_mast,wind_dir,cNOX_mod_1b,cNOX_mod_2b,qNOx
!	endif

!
!
      goto 10
!
 2000 continue
!
!  100 format(1x,4(i2,2x),8(f10.2,1x))
  101 format(1x,i5,1x,3(i2,2x),9(g12.4,1x))
  102 format(1x,i5,1x,3(i2,2x),5(g12.4,1x))
!
      !stop
      
      end subroutine ospm_nortrip_simair
!
! ****END OF MAIN***********************
!
!
! *****SUBROUTINES***********************
!
!
! ***************************************
!
      Subroutine CarTurb(Vp,Vt,NNp,NNt,Sp2,St2,L,f_turb,s_w0)
      !Bruce: included scaling for traffic turbulence
      Real L,NNp,NNt
      Real f_turb
!
      b=0.30
!
      s_w0 = b*Sqrt( (Vp*NNp*Sp2 + Vt*NNt*St2)/L )/113.842
      s_w0 = s_w0 * f_turb
      return
      end
!
! ****************************************************
!
      Subroutine ModPar(H,precis,h0,z0,Alfa,Beta,Sp2,St2)
! ***MODEL PARAMETERS*************
      precis=0.01       ! ACCURACY OF WIND DIRECTION INTEGRATION
      h0=2.             ! HEIGHT OF INITIAL DISPERSION (m)
      z0=0.60           ! ROUGHNESS HEIGHT (m)
      Alfa=0.1          ! TURBULENCE PARAMETER
      Sp2=2.            ! AREA OF CARS (m^2)
      St2=16.           ! AREA OF TRUCKS (m^2)
        z=max(H,h0)
      Beta =Log(h0/z0)/Log(z/z0)        ! U_street/U_roof
!
      return
      end
!
! ******************************************************************
!
!     Bruce: f_roof as input variable and included declarations
      Subroutine Flow_par(u_mast,f_roof,u,r_wind)
!
      real u_mast,f_roof,u,r_wind
!      
           u=f_roof*max(0.01,u_mast)            
!         u=0.4*max(0.01,u_mast) !               test     u = u_roof      
      If (u.gt.2.) then
        r_wind = 1.
      Else
        r_wind = sqrt(0.5*u)
      endif
!
      return
      end
!
! ******************************************
!
      function sigmaz(dist,windsp,s_w)
          sigmaz=(s_w/windsp)*dist
      return
      end
!
! ***************************************************
!
      Subroutine Lee_side(C_st,H,L,SLlis,u_b,u_d,x1,s_w,s_wt,r_wind,h0,Fi,H_u,H_d,u)
      common/Receptor/Zrec
!
      real L,L_rec,L_max
!
       ts       = 1.0e+6
       tl       = 1.0e-10
!
       PI=3.141592
       L_rec = 2.*H_u*r_wind
       Fi = MAX(Fi,tl)
       scos=abs(Cos(Fi))
       ssin=Sin(Fi)
       D_max = L/ssin
       L_max=L
       if(scos.ne.0.) L_max = MIN(D_max,SLlis/scos)
       R = MAX(cos(2.*r_wind*Fi),0.)
       d1 = MIN(D_max,L_rec)
       d2 = MIN(D_max,0.5*L_rec,ts)
!
       d5 = MIN(L_max,L_rec,x1)
       d6 = MAX(MIN(x1,L_max),L_rec)
       d7 = MAX(MIN(L_rec,L_max),x1)-x1
       d8 = MAX(L_max-MAX(L_rec,x1),0.)
       Z=max(H,h0)
!
       Ex=1.
       if(d7.gt.0.) Then
       Ep = -s_wt*d7/(u_b*Z)
!
       If (Ep.LT.-10.) then
          Ex = 0.
       Else
         Ex = Exp(Ep)
       end if
       endif
!
       Ex1=1.
       if(d8.gt.0.) Then
       Ep1= -s_wt*d8/(u_b*Z)
!
       If (Ep1.LT.-10.) then
          Ex1 = 0.
       Else
         Ex1 = Exp(Ep1)
       end if
       endif
!
       s_zd5=sigmaz(d5,u_b,s_w)
       s_zd6=sigmaz(d6,u_b,s_w)
       s_zL_Rec=sigmaz(L_Rec,u_b,s_w)
!
       Zcalc1=min(h0+s_zd5,max(h0,Zrec))
       Zcalc2=min(h0+s_zd6,max(h0+s_zL_Rec,Zrec))
       CBOX=SQRT(2./PI)/(L*s_w)
       C_dir = CBOX*( Log((s_zd5+h0)/Zcalc1) + (s_w/s_wt)*(1.- Ex) )
       if(R.gt.0..and.L_rec.lt.L_max) C_dir=C_dir+CBOX* R*(Log((s_zd6+h0)/Zcalc2) + (s_w/s_wt)*(Ex-Ex1))
!
        C_rec=0.
        if(L_rec.gt.0..and.d1.gt.0..and.Zrec.le.H_u) then
          d3_max = sqrt(H_u**2+0.25*L_rec**2)
          H_c = 2.*H_u*(L_rec-d1)/L_rec
          d3  = max(0., d3_max*(H_u-min(H_c,H_d))/H_u)
          u_s = (d3*u_d+d3_max*u)/(d3+d3_max)
          C_rec = (1./L)*d1*ssin/(s_wt*d2 + u_s*d3)
        endif
!
       C_st = C_dir + C_rec
!
      return
      end
!
! ***********************************************
!
      Subroutine Wind_Side(C_st,H,L,SLlis,u_b,u_d,x1,s_w,s_wt,r_wind,h0,Fi,H_u,H_d,u)
      common/Receptor/Zrec
!
      real L,L_rec,L_max
!
       ts       = 1.0e+6
       tl       = 1.0e-10
       PI=3.141592
!
       L_rec = 2.*H_u*r_wind
       Fi = Max(Fi,tl)
       scos=abs(Cos(Fi))
       ssin=Sin(Fi)
       D_max = L/ssin
       L_max=L
       if(scos.ne.0.) L_max = min(D_max,SLlis/scos)
       d1 = MIN(D_max,L_rec,ts)
       d2 = MIN(D_max,0.5*L_rec,ts)
!
!
       If (L_rec.ge.D_max) then
          C_dir = 0.
       Else
          d4 = MIN(L_max,(D_max-L_rec),X1)
          d5 = MAX(0.,MIN(L_max,(D_max-L_rec),ts),X1)-X1
          Z=max(H,h0)
          Ex=1.
          if(d5.gt.0.) then
          Ep = -s_wt*d5/(u_b*Z)
           If (Ep.LT.-10.) then
              Ex = 0.
           Else
              Ex = Exp(Ep)
           end if
           endif
       s_zd4=sigmaz(d4,u_b,s_w)
!
       Zcalc=min(h0+s_zd4,max(h0,Zrec))
       C_dir = SQRT(2./Pi)/(L*s_w)*( Log((h0+s_zd4)/Zcalc) + (s_w/s_wt)*(1.-Ex) )
       end if
!
        C_rec=0.
        if(L_rec.gt.0..and.d1.gt.0..and.Zrec.le.H_u) then
          d3_max = sqrt(H_u**2+0.25*L_rec**2)
          H_c = 2.*H_u*(L_rec-d1)/L_rec
          d3  = max(0., d3_max*(H_u-min(H_c,H_d))/H_u)
          u_s = (d3*u_d+d3_max*u)/(d3+d3_max)
          C_rec = (1./L)*d1*ssin/(s_wt*d2 + u_s*d3)
        endif
!
       C_st  =  C_dir + C_rec
!
      return
      end
!
! *******************************************************
!
      Function Isgn(x)
       if (x.eq.0.0)  then
         Isgn = 0
        else
         Isgn = nint(x/abs(x))
       endif
      return
      end
!
! ************************************************************
!
      Subroutine Calc_Delta_Fi(u,Delta_Fi)
       pi=3.141592
        Delta_Fi=180.
       If (u.gt.0..and.u.lt.1.) Then
         Delta_Fi = 0.5/u*180.0/pi
       Else
         Delta_Fi =0.5*180./pi
       endif
       If (Delta_Fi.gt.180.) Delta_Fi = 180.
!
      return
      end
!
! *******************************************************
!
      Subroutine Averaging(u,Wind_dir,Delta_Fi,precis,nexc,dd_l,dd_u,gg,s_w0,s_wt,H,L,h0,SL1,SL2,r_wind,Alfa,Beta,irec1,irec2,C_st1,C_st2,RecZ)
      real L,u,Wind_dir,Delta_Fi,precis,s_w0,s_wt,H,h0,SL1,SL2,r_wind,Alfa,Beta,C_st1,C_st2
      real dd_l(12),dd_u(12),gg(12),lbs(30),ubs(30),xb(30),sc(30)
!
      common/Receptor/Zrec
       Zrec=RecZ
!
       S_Calc = Wind_dir - Delta_Fi
       E_Calc = Wind_dir + Delta_Fi
!
         If(S_Calc.lt.0.) S_Calc=360.+S_Calc
         If(E_Calc.gt.360.) E_Calc=E_Calc-360.
!
!
         xmax=E_Calc-S_Calc
           if(xmax.le.0.) xmax=xmax+360.
       do jj=1,nexc
         lbs(jj)=dd_l(jj)-S_Calc
            if(lbs(jj).lt.0.) lbs(jj)=lbs(jj)+360.
         ubs(jj)=dd_u(jj)-S_Calc
            if(ubs(jj).le.0.) ubs(jj)=ubs(jj)+360.
       enddo
!
       Do jj=1,30
        xb(jj)=0.
       enddo
       xb(1) =xmax
       xb(2) =0.
       nmax=2
       nobs=2
            Do jj = 1,nexc
              if(lbs(jj).le.0..or.lbs(jj).ge.xmax) goto 50
                    nobs=nobs + 1
                    nmax=min(30,nobs)
              do kk=1,nmax
               sc(kk)=xb(kk)
              enddo
              call sortp(lbs(jj),sc,nmax)
              do kk=1,nmax
               xb(kk)=sc(kk)
              enddo
   50 continue
              if(ubs(jj).le.0..or.ubs(jj).ge.xmax) goto 60
                    nobs=nobs + 1
                    nmax=min(30,nobs)
              do kk=1,nmax
               sc(kk)=xb(kk)
              enddo
              call sortp(ubs(jj),sc,nmax)
              do kk=1,nmax
               xb(kk)=sc(kk)
              enddo
   60 continue
            Enddo
!
       do jj=1,nmax
          xb(jj)=xb(jj)+S_Calc
          if(xb(jj).gt.360.) xb(jj)=xb(jj)-360.
       enddo
!
       Sum1=0.
       Sum2=0.
       Do jj=1,nmax-1
           N_Calc = 1
           Sum_C1  = 0.
           Sum_C2  = 0.
           C_st1_o=0.
           C_st2_o=0.
           Delta1=0.
           Delta2=0.
!
           xstart=xb(nmax-jj+1)
           xend  =xb(nmax-jj)
         If(nmax.gt.2.and.xstart.eq.xend) goto 70
         If(nmax.eq.2) then
           x0=Wind_dir
           df=Delta_Fi
         else
            x0=0.5*(xstart+xend)
            df=0.5*(xend-xstart)
              if(xstart.gt.xend) then
                 x0=x0+180.
                 if(x0.gt.360.) x0=x0-360.
                 df=df+180.
              endif
         Endif
!
!
            DD=x0
         Call SC_st(u,DD,s_w0,s_wt,H,L,h0,SL1,SL2,r_wind, Alfa,Beta,irec1,irec2,nexc,dd_l,dd_u,gg,C_st1,C_st2)
        if(irec1.eq.1) Sum_C1= Sum_C1+ C_st1
        if(irec2.eq.1) Sum_C2= Sum_C2+ C_st2
!
            DD=xstart
         Call SC_st(u,DD,s_w0,s_wt,H,L,h0,SL1,SL2,r_wind, Alfa,Beta,irec1,irec2,nexc,dd_l,dd_u,gg,C_st1,C_st2)
        if(irec1.eq.1) Sum_C1= Sum_C1+ 0.5*C_st1
        if(irec2.eq.1) Sum_C2= Sum_C2+ 0.5*C_st2
!
            DD=xend
         Call SC_st(u,DD,s_w0,s_wt,H,L,h0,SL1,SL2,r_wind, Alfa,Beta,irec1,irec2,nexc,dd_l,dd_u,gg,C_st1,C_st2)
        if(irec1.eq.1) Sum_C1= Sum_C1+ 0.5*C_st1
        if(irec2.eq.1) Sum_C2= Sum_C2+ 0.5*C_st2
!
       if(irec1.eq.1) C_st1= Sum_C1*0.5
       if(irec2.eq.1) C_st2= Sum_C2*0.5
!
       if(irec1.eq.1) C_st1_o=C_st1
       if(irec2.eq.1) C_st2_o=C_st2
           N_Inter=4
!
!
   10 CONTINUE
          dfn=0.5*df
!
       do kk= 1,N_Calc
             DD=x0+dfn+float(kk-1)*df
        If (DD.gt.360.) DD = DD - 360.
         Call SC_st(u,DD,s_w0,s_wt,H,L,h0,SL1,SL2,r_wind, Alfa,Beta,irec1,irec2,nexc,dd_l,dd_u,gg,C_st1,C_st2)
        if(irec1.eq.1) Sum_C1= Sum_C1+ C_st1
        if(irec2.eq.1) Sum_C2= Sum_C2+ C_st2
!
             DD=x0-(dfn+float(kk-1)*df)
         If (DD.le.0.)   DD = DD + 360.
         Call SC_st(u,DD,s_w0,s_wt,H,L,h0,SL1,SL2,r_wind, Alfa,Beta,irec1,irec2,nexc,dd_l,dd_u,gg,C_st1,C_st2)
        if(irec1.eq.1) Sum_C1= Sum_C1+ C_st1
        if(irec2.eq.1) Sum_C2= Sum_C2+ C_st2
       enddo
!
       if(irec1.eq.1) C_st1= Sum_C1*dfn
       if(irec2.eq.1) C_st2= Sum_C2*dfn
           N_Calc=2*N_Calc
           N_Inter=2*N_Inter
           df=dfn
        if(irec1.eq.1) DELTA1=abs(C_st1 - C_st1_o)
        if(irec2.eq.1) DELTA2=abs(C_st2 - C_st2_o)
!
!
           if(irec1.eq.1) C_st1_o=C_st1
           if(irec2.eq.1) C_st2_o=C_st2
             if(dfn.gt.0.1) then
      if((delta1.gt.precis*C_st1_o).or.(delta2.gt.precis*C_st2_o)) goto 10
             endif
          if(irec1.eq.1) Sum1=Sum1+C_st1
          if(irec2.eq.1) Sum2=Sum2+C_st2
   70 Continue
      Enddo
!
          if(irec1.eq.1) C_st1=Sum1/(2.*Delta_Fi)
          if(irec2.eq.1) C_st2=Sum2/(2.*Delta_Fi)
!
      return
      end
!
! ******************************************************************
!
      Subroutine SC_st(u,DD,s_w0,s_wt,H,L,h0,SL1,SL2,r_wind,Alfa,Beta,irec1,irec2,nexc,dd_l,dd_u,gg,C_st1,C_st2)
      dimension dd_l(nexc),dd_u(nexc),gg(nexc)
      real L,lB,uB
      pi=3.141592
      rd=pi/180.
!
!
      H_u=H
      H_d=H
      DD180=DD+180.
      if(DD180.gt.360.) DD180=DD180-360.
      do n=1,nexc
        lB=dd_l(n)
        uB=dd_u(n)
          if(DD.ge.lB.and.DD.le.uB) then
            H_u=gg(n)
            goto 5
          endif
      enddo
    5 continue
      do n=1,nexc
        lB=dd_l(n)
        uB=dd_u(n)
          if(DD180.ge.lB.and.DD180.le.uB) then
            H_d=gg(n)
            goto 6
          endif
      enddo
    6 continue
!
      s_wt = Sqrt((Alfa*u)**2  + 0.4*s_w0**2)
!
      if(DD.gt.90..and.DD.lt.270.) then
                  SLlis  = SL2
                  Fi     = abs(180.-DD)*rd
      endif
!
      if(DD.gt.270..or.DD.lt.90.) then
                  SLlis  = SL1
                  if(DD.gt.270.) Fi = (360.-DD)*rd
                  if(DD.lt. 90.) Fi =  DD*rd
      endif
!
      if(DD.eq.90..or.DD.eq.270.) then
                  SLlis=L
                  Fi=0.5*pi
      endif
!
! ***************************************
      Z=max(H,h0)
      gamma=H_u/Z
      u_b = Beta*u*(1. - min(0.9,gamma*0.2)*Sin(Fi)**2)
      u_d = Sqrt(u_b**2 + s_w0**2)
      s_w = Sqrt((Alfa*u_b)**2 + s_w0**2)
!
      x1 = u_b*(Z-h0)/s_w
!
      If (DD.gt.0..and.DD.lt.180.) then
         If (irec1.eq.1) then
         call Wind_Side(C_st,H,L,SLlis,u_b,u_d,x1,s_w,s_wt,r_wind,h0,Fi,H_u,H_d,u)
         C_st1=C_st
         endif
!
         if (irec2.eq.1) then
         call Lee_Side(C_st,H,L,SLlis,u_b,u_d,x1,s_w,s_wt,r_wind,h0,Fi,H_u,H_d,u)
         C_st2=C_st
         endif
      endif
!
      If (DD.gt.180..and.DD.lt.360.) then
         If (irec1.eq.1) Then
         call Lee_Side(C_st,H,L,SLlis,u_b,u_d,x1,s_w,s_wt,r_wind,h0,Fi,H_u,H_d,u)
         C_st1=C_st
         endif
!
         if (irec2.eq.1) then
         call Wind_Side(C_st,H,L,SLlis,u_b,u_d,x1,s_w,s_wt,r_wind,h0,Fi,H_u,H_d,u)
         C_st2=C_st
         endif
      endif
!
      If ((DD.eq.0.).or.(DD.eq.180.).or.(DD.eq.360.)) then
         call Lee_Side(C_st,H,L,SLlis,u_b,u_d,x1,s_w,s_wt,r_wind,h0,Fi,H_u,H_d,u)
       if (irec1.eq.1) C_st1=C_st
       if (irec2.eq.1) C_st2=C_st
      endif
!
      return
      end
!
! ************************************************************
!
      Subroutine NO2NOX(cnox_b,co3_b,cno2_b,cnox,cno2,cO3,fno2,Qn,Tk,tau)
        real jno2,kno,kT
        cNOx_s = cnox - cnox_b
        cNO2_N = fNO2*cNOx_S + cNO2_B
       if(tau.eq.0.) then
        cO3=cO3_b
        cNO2=cNO2_N
        Return
       else
        jNO2 = 0.
        if(Qn.gt.1.) jNO2 = 0.8e-3*Exp(-10./Qn) + 7.4e-6*Qn
        kNO  = 5.38e-2*Exp(-1430./Tk)
        R_jk = jNO2/kNO
        kT   = 1./(tau*kNO)
        cNO2_O = cNO2_N + cO3_B
        Fak    = cNOx+R_jk+kT+cNO2_O
        cNO2 = 0.5*(Fak - Sqrt(Fak**2 - 4.*(cNOx*cNO2_O+cNO2_N*kT)))
        cO3  = cNO2_O - cNO2
       endif
      Return
      End
!
! ***************************************************
!
      subroutine sortp(c,sc,nmax)
      dimension sc(nmax)
        if(c.gt.sc(nmax)) then
         sc(nmax)=c
         if(nmax.eq.1) return
         do i=nmax-1,1,-1
            if(c.gt.sc(i)) then
              sc(i+1)=sc(i)
              sc(i)=c
             else
            return
            endif
         enddo
        endif
        return
        end
!
! ***************************************************
!
      Subroutine RotStreet(P1,dd_l,dd_u,lb,ub,nexc)
       real dd_l(nexc),dd_u(nexc),lb(nexc),ub(nexc)
       Do kk = 1,nexc
        lb(kk) = dd_l(kk) - P1
        If (lb(kk).lt.0.) lb(kk) = lb(kk) + 360.
        ub(kk) = dd_u(kk) - P1
        If (ub(kk).le.0.) ub(kk) = ub(kk) + 360.
       enddo
      return
      end
