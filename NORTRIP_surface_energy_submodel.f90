
    subroutine surface_energy_submodel_4(short_net,long_in,H_traffic,r_aero,TC,TCs_in,TCsub,RH,RHs_nosalt,RHs_0 &
        ,P,dzs_in,dt_h_in,g_surf_in,s_surf_in,g_min,M2_road_salt_0,salt_type,sub_surf_param &
        ,surface_humidity_flag,use_subsurface_flag,use_salt_humidity_flag &
        ,TCs_out,melt_temperature,RH_salt_final,RHs,M_road_dissolved_ratio_temp &
        ,evap,evap_pot,melt,freeze,H,L,G,long_out,long_net,rad_net,G_sub)

    !Includes melt of snow as output and includes snow surface and melt temperature as
    !additional inputs to the original Surface_energy_model_2_func version
    !Also includes a relationship for vapour pressure of ice
    use NORTRIP_index_definitions

    implicit none
    
    !Input variables
    real short_net,long_in,H_traffic,r_aero,TC,TCs_in,TCsub,RH,RHs_nosalt,RHs_0,P,dzs_in,dt_h_in
    real g_surf_in,s_surf_in,g_min,M2_road_salt_0(num_salt),sub_surf_param(3)
    integer surface_humidity_flag,use_subsurface_flag,use_salt_humidity_flag,salt_type(num_salt)
    !Output variables
    real TCs_out,melt_temperature,RH_salt_final,RHs,evap,evap_pot,melt,freeze,H,L,G,long_out,long_net,rad_net,G_sub
    real M_road_dissolved_ratio_temp(num_salt)
    
    !Parameters
    real Cp,lambda,lambda_ice,lambda_melt
    parameter (Cp=1006,lambda=2.50E6,lambda_ice=2.83E6,lambda_melt=3.33E5) !(J/kg)
    real RD,T0C,sigma,eps_s,omega
    parameter (RD=287.0,T0C=273.15,sigma=5.67E-8,eps_s=0.95,omega=7.3e-5)
    
    !Internal variables
    real dt_sec,dt_h
    real G_melt,G_freeze
    real lambda_mixed
    real rho_s,c_s,k_s_road,c_rho_s
    real dzs,mu
    real melt_temperature_salt_temp(num_salt),RH_salt_temp(num_salt),g_road_temp,s_road_temp
    real fac,TK_a,rho
    real TCs_0,TCs
    integer ti_sub
    real L_max,L_min
    integer ti,i

    real g_s_surf,g_surf_fraction,s_surf_fraction
    real a_G,a_rad,a_RL,b_RL,a_H
    real qsat,q
    real evap_0
    real qsats_water,qsats_ice,qsats,qs_water,qs_ice
    real L_water,L_ice
    real evap_water,evap_ice
    real g_equil,ratio_equil
    real L_pot_water,L_pot_ice,evap_pot_water,evap_pot_ice
    real g_surf,s_surf
    real g_road_equil_at_T_s,s_road_equil_at_T_s
    real no_salt_factor
            
    !Functions
    real q_sat_func,q_sat_ice_func
    
    !Controls
    integer :: set_limit_noevap=1   !Limit flag for setting evaporation limit
    integer :: limit_evap=1         !Implement limit for evaporation
    integer :: limit_condens=1      !Implement limit for condensation
    integer :: nsub=1               !Allows smaller time steps. Not used and not properly tested. Fails, so do not change from 1
    integer :: disolution_flag=1    !Allows melting using disolution of salt
    
    !Set time step in seconds
    dt_sec=dt_h_in*3600

    !Initialise the available melting energy (W/m2)
    G_melt=0
    G_freeze=0
    melt=0
    freeze=0
    
    !Initialise the internal water so it does not change surf_temp values
    g_surf=g_surf_in
    s_surf=s_surf_in

    !Set subsurface papameters
    rho_s=sub_surf_param(1)
    c_s=sub_surf_param(2)
    k_s_road=sub_surf_param(3)
    c_rho_s=rho_s*c_s

    !Automatically set dzs if it is 0.
    !This calculated value of dzs is optimal for a sinusoidal varying flux
    if (dzs_in.eq.0.) then
        dzs=(k_s_road/c_rho_s/2/omega)**.5
    else
        dzs=dzs_in
    endif
    mu=omega*c_rho_s*dzs
    
    !If subsurface flux is turned of
    if (use_subsurface_flag.eq.0) then
        mu=0
    endif

    !Set atmospheric temperature in Kelvin
    TK_a=T0C+TC

    !Set air density
    rho=P*100./(RD*TK_a)

    !Initialise surface temperature
    TCs=TCs_in
    TCs_0=TCs_in
    TCs_out=TCs_in

    !Sub time settings. Allows smaller time steps. Not used and not properly tested
    dt_sec=dt_sec/nsub
    dt_h=dt_h_in/nsub

    !Set internal limits of latent heat flux. Should not be used but
    L_max=500
    L_min=-200

    !Preset values of constants for implicit solution
    a_G=1.0/(c_rho_s*dzs)
    a_rad=short_net+long_in*eps_s+H_traffic
    a_RL=(1-4*TC/TK_a)*eps_s*sigma*TK_a**4
    b_RL=4.0*eps_s*sigma*TK_a**3
    a_H=rho*Cp/r_aero

    !Specific humidty of the air
    qsat = q_sat_func(TC,P)
    q=qsat*RH/100.

    !Initialise the evaporation
    evap=0
    
    !Start the sub time routine (not used)
    do ti_sub=1,nsub

        evap_0=evap

        !Loop twice to update the latent heat flux and salt induced melt with the new surface temperature
        do i=1,1
            
            !Calculate the salt solution, melt temperature and freezing and melting
            if (use_salt_humidity_flag.gt.0) then
                no_salt_factor=1.0
            else
                no_salt_factor=0.0
            endif   
                
                !Calculate the salt solution and change in water and ice/snow
                call salt_solution_sub(M2_road_salt_0*no_salt_factor,g_surf_in,s_surf_in,TCs_out,salt_type,dt_h,disolution_flag &
                    ,melt_temperature_salt_temp,RH_salt_temp,M_road_dissolved_ratio_temp &
                    ,g_road_temp,s_road_temp,g_road_equil_at_T_s,s_road_equil_at_T_s)
    
                !Determine the melt or freezing (mass) due to disollution of salt
                freeze=max(0.,s_road_temp-s_surf_in)
                melt=max(0.,g_road_temp-g_surf_in)
                
                !Use the salt with the lowest melt temperature to determine melt temperature
                melt_temperature=minval(melt_temperature_salt_temp)
             
                !Set the energy used for freezing or melting
                G_freeze=freeze*lambda_melt/dt_sec
                G_melt=melt*lambda_melt/dt_sec
    
                !Set the surface salt humidity to be the lowest for the two salts
                RH_salt_final=minval(RH_salt_temp)
                RH_salt_final=max(RH_salt_final,1.) !RH_salt_temp cannot be = 0
        
                !Set the final surface humidity based on surface and salt humidity
                RHs=RHs_nosalt*RH_salt_final/100.
    
                !Smooth the RH in time to avoid numerical oscillations
                !Check if this is still necessary
                fac=0.333
                RHs=RHs*(1-fac)+fac*RHs_0
    
                !Update g_surf to new values generated by salt_solution_sub
                g_surf=g_road_temp
                s_surf=s_road_temp
                
            !else
                !If salt not involved then set the following variables
            !    RHs=RHs_nosalt
	        !    G_melt=0.
            !    G_freeze=0.
            !    melt=0.
            !    freeze=0.
            !    melt_temperature=0
            !    RH_salt_final=100.
            !    M_road_dissolved_ratio_temp=1.
            !    if (TCs.lt.melt_temperature) then
            !        s_road_equil_at_T_s=g_surf_in+s_surf_in
            !        g_road_equil_at_T_s=0.
            !    else
            !        g_road_equil_at_T_s=g_surf_in+s_surf_in
            !        s_road_equil_at_T_s=0.
            !    endif
            !endif
    
	        !Do not allow the salt equilibrium to freeze water
            !Use the energy balance for that but keep the salt equilibrium limits
            if (freeze.gt.0.and.melt.eq.0) then
                G_melt=0.
                G_freeze=0.
                melt=0.
                freeze=0.
                g_surf=g_surf_in
                s_surf=s_surf_in
            endif            
            
            !Set sum of the water and ice and fraction
            g_s_surf=g_surf+s_surf
            if (g_s_surf.gt.0) then
                g_surf_fraction=g_surf/g_s_surf
                s_surf_fraction=s_surf/g_s_surf
            else
                g_surf_fraction=0.5
                s_surf_fraction=0.5
            endif

            !Weight lambda coefficient according to water and ice distribution
            lambda_mixed=g_surf_fraction*lambda+s_surf_fraction*lambda_ice
              
            !Specific surface humidity based on current surface temperature (TCs)
            qsats_water = q_sat_func(TCs,P)
            qsats_ice = q_sat_ice_func(TCs,P)
            qsats=g_surf_fraction*qsats_water+s_surf_fraction*qsats_ice
            qs_water=qsats_water*RHs/100.
            qs_ice=qsats_ice*RHs/100.

            !Latent heat flux
            L_water=-rho*lambda*(q-qs_water)/r_aero
            L_ice=-rho*lambda_ice*(q-qs_ice)/r_aero
            !Limit latent heat flux to reasonable values
            L_water=max(L_min,min(L_max,L_water))
            L_ice=max(L_min,min(L_max,L_ice))
            L=g_surf_fraction*L_water+s_surf_fraction*L_ice
    
            !Set evaporation 
            evap_water=L_water/lambda*dt_sec
            evap_ice=L_ice/lambda_ice*dt_sec
            evap=g_surf_fraction*evap_water+s_surf_fraction*evap_ice
        
            !Limit evaporation (L) to the amount of water (to be) available based on the surface humidity scheme used
            
            !Set the equilibrium surface moisture level
            ratio_equil=RH*qsat/RH_salt_final/qsats
            if (set_limit_noevap.eq.1) then
                if (surface_humidity_flag.eq.1) then
                    !g_equil=q/qsats*g_min
                    g_equil=ratio_equil*g_min !changed to include RH_salt_final
                elseif (surface_humidity_flag.eq.2) then
                    !g_equil=-log(1-q/qsats)*g_min/2
                    if (ratio_equil.lt.1) then
                        g_equil=-log(1-ratio_equil)*g_min/4. !changed to include RH_salt_final
                    else
                        g_equil=g_min*1000. !A large number as this is the asymptotic limit
                    endif                  
                else
                    g_equil=0.
                endif
            else
                g_equil=0
            endif
            
            !Limit the amount that can be evaporated
            if (limit_evap.eq.1) then
                if (evap.ge.(g_s_surf-g_equil)/dt_h.and.evap.ge.0) then
                    evap=max(0.,(g_s_surf-g_equil)/dt_h)
                    L=evap*lambda_mixed*dt_h/dt_sec
                    L=max(L_min,min(L_max,L))
                endif
            endif
            
            !Limit the amount that can be condensed
            if (limit_condens.eq.1) then
                if (g_equil.le.g_min.and.evap.lt.0) then !Allows unlimitted condensation if the equilibrium g is > g threshold
                if (evap.le.(g_s_surf-g_equil)/dt_h.and.evap.lt.0) then                   
                    evap=min(0.,(g_s_surf-g_equil)/dt_h)
                    L=evap*lambda_mixed*dt_h/dt_sec
                    L=max(L_min,min(L_max,L))
                endif
                endif
            endif
    
            !Calculate surface temperature implicitly
            TCs_out=(TCs_0+dt_sec*a_G*(a_rad-a_RL-L+a_H*TC+mu*TCsub-G_melt+G_freeze))/(1+dt_sec*a_G*(a_H+b_RL+mu))
            

            !Set the midpoint temperature for diagnostics
            TCs=(TCs_0+TCs_out)/2

            !Diagnose sensible heat flux based on average surface temperature
            H=-rho*Cp*(TC-TCs)/r_aero

            !Diagnose potential evaporation
            L_pot_water=-rho*lambda*(q-qsats_water)/r_aero
            L_pot_ice=-rho*lambda_ice*(q-qsats_ice)/r_aero
            evap_pot_water=L_pot_water/lambda*dt_sec
            evap_pot_ice=L_pot_ice/lambda_ice*dt_sec
            evap_pot=g_surf_fraction*evap_pot_water+s_surf_fraction*evap_pot_ice

            !Diagnose radiation based on current average temperature
            long_out=eps_s*sigma*(T0C+TCs)**4
            long_net=long_in*eps_s-long_out
            rad_net=short_net+long_net
            G_sub=-mu*(TCs-TCsub)

            !Diagnose surface flux without melt and freeze fluxes
            !G=rad_net-H-L+H_traffic
            G=rad_net-H-L+H_traffic-G_melt+G_freeze

            !Calculate additional melt, in addition to salting, due to energy
            !if (i.eq.1) then
                if (s_surf.gt.0.and.G.ge.0.and.TCs_out.ge.melt_temperature) then
	                melt=melt+G/lambda_melt*dt_sec
	                !Limit the melting so it does not melt more water than is available
                    melt=min(melt,s_surf)
                    !Limit the melting so it does not go past the equilibrium salt level
                    melt=min(melt,max(g_road_equil_at_T_s-g_surf,0.))
                    G_melt=melt*lambda_melt/dt_sec
                else
	                melt=melt+0
                    G_melt=melt*lambda_melt/dt_sec
                endif

                !Calculate additional freezing in first loop only
                if (g_surf.gt.0.and.G.lt.0.and.TCs_out.lt.melt_temperature) then
	                freeze=freeze-G/lambda_melt*dt_sec!/dt_h
	                !Limit the freezing so it does not freeze more water than is available
                    freeze=min(freeze,g_surf)
                    !Limit the freezing so it does not go past the equilibrium salt level
                    freeze=min(freeze,max(s_road_equil_at_T_s-s_surf,0.))
                    G_freeze=freeze*lambda_melt/dt_sec!/dt_h
                else
	                freeze=freeze+0
                    G_freeze=freeze*lambda_melt/dt_sec!/dt_h
                endif
            !endif
 
            !Diagnose surface flux with melt and freeze fluxes
            G=rad_net-H-L+H_traffic-G_melt+G_freeze

        enddo!Finish the double loop

        !Update the starting temperature for the case where sub time steps are used
        TCs_0=TCs_out

        !Add the evaporations when sub time steps are used
        evap=evap_0+evap
    
    enddo!sub
    
    !write(*,*),M2_road_salt_0(1),M_road_dissolved_ratio_temp(1),g_surf+s_surf,freeze,melt,melt_temperature,TCs_out

    end subroutine surface_energy_submodel_4

!--------------------------------------------------------------------------
!salt_solution_sub
!--------------------------------------------------------------------------
        
    subroutine salt_solution_sub(M2_road_salt,g_road,s_road,T_s,salt_type,dt_h,disolution_flag &
        ,melt_temperature_salt,RH_salt,M_road_dissolved_ratio,g_road_out,s_road_out,g_road_at_T_s_out,s_road_at_T_s_out)
!==========================================================================
!NORTRIP model
!SUBROUTINE: calc_salt_solution
!VERSION: 1, 27.08.2012
!AUTHOR: Ingrid Sundvor and Bruce Rolstad Denby(bde@nilu.no)
!DESCRIPTION: Calculates salt solution, melt temperature and RH salt
!==========================================================================

    use NORTRIP_index_definitions
    
    !Set the salt constants used.
    use NORTRIP_salt_constants

    implicit none
    
    !Input variables
    real M2_road_salt(num_salt) !In g/m^2
    real g_road,s_road,T_s
    integer salt_type(num_salt)
    real  dt_h
    integer disolution_flag
    !Output variables
    real melt_temperature_salt(num_salt)
    real RH_salt(num_salt)
    real M_road_dissolved_ratio(num_salt)
    real g_road_out,s_road_out
    real g_road_at_T_s_out,s_road_at_T_s_out
    
    !Use the oversaturated parameterisation for melt temperature and water vapour
    integer :: use_oversaturated=1

    !Internal
    real :: N_moles_salt(num_salt)
    real :: afactor(num_salt)
    real :: dissolved_salt(num_salt)
    real :: solution_salt_at_T_s(num_salt)
    real :: N_moles_water_at_T_s(num_salt)
    real :: g_road_at_T_s(num_salt)
    real :: s_road_at_T_s(num_salt)
    real :: solution_salt(num_salt)
    real :: N_moles_water=0
    real T_0
    parameter (T_0=273.13)
    real tau_dissolve
    real :: surface_moisture_min=1e-6 !Mimimum allowable total surface wetness 
    
    real vp_ice,vp_s,antoine_scaling
    real RH_salt_saturated
    
    integer i
    
    !Functions
    real antoine_func

    !Time scale for dissolving salt set to 0.5 hours
    tau_dissolve=1.0

    N_moles_salt=0.
    afactor=0.
    dissolved_salt=0.
    solution_salt_at_T_s=0.
    N_moles_water_at_T_s=0.
    g_road_at_T_s=0.
    s_road_at_T_s=0.

    call set_salt_parameters
    
    !Convert surface moisture to moles per m^2
    N_moles_water=1000*g_road/M_atomic_water

    if (disolution_flag.eq.1) then
        !Calculate the salt equilibrium water/ice dependent on temperature
        do i=1,num_salt
    
            !Determine moles of salt /m^2. M2 means salt is in g/m^2
            N_moles_salt(i)=max(0.,M2_road_salt(i)/M_atomic(salt_type(i)))

            !Determine the melt based on instantaneous dissolving of the salt
            !in the ice and snow surface to achieve a melt temperature the same as
            !the road surface temperature
            if (T_s.lt.0.and.T_s.ge.melt_temperature_saturated(salt_type(i))) then
                solution_salt_at_T_s(i)=saturated(salt_type(i))*(T_s/melt_temperature_saturated(salt_type(i)))**(1./salt_power)
                N_moles_water_at_T_s(i)=N_moles_salt(i)/solution_salt_at_T_s(i)-N_moles_salt(i)
                g_road_at_T_s(i)=min(g_road+s_road,N_moles_water_at_T_s(i)*M_atomic_water/1000)
                s_road_at_T_s(i)=max(0.,(g_road+s_road)-g_road_at_T_s(i))
                !if (i.eq.1) write(*,*) g_road,g_road_at_T_s,T_s
            elseif (T_s.ge.0) then
                !solution_salt_at_T_s(i)=0.
                !N_moles_water_at_T_s(i)=N_moles_water
                g_road_at_T_s(i)=g_road+s_road
                s_road_at_T_s(i)=0.               
            elseif (T_s.lt.melt_temperature_saturated(salt_type(i))) then
                g_road_at_T_s(i)=0.
                s_road_at_T_s(i)=s_road+g_road
            else
                !Should not be able to reach this if
                g_road_at_T_s(i)=s_road+g_road
                s_road_at_T_s(i)=s_road+g_road
            endif
        
        enddo

        !Set the equilibrium water amount at the current surface temperature
        g_road_at_T_s_out=maxval(g_road_at_T_s)
        s_road_at_T_s_out=g_road+s_road-g_road_at_T_s_out
        
        !Time scale for melting by salt. Greater than 0 delays the disolvment.
        !Only applicable for melting, not for freezing
        if (g_road_at_T_s_out.gt.g_road) then
            g_road_out=g_road*exp(-dt_h/tau_dissolve)+g_road_at_T_s_out*(1-exp(-dt_h/tau_dissolve))
        else
            g_road_out=g_road_at_T_s_out
        endif
        
        !g_road_out=maxval(g_road_at_T_s)
        s_road_out=max(0.,(g_road+s_road)-g_road_out)

        !g_road=g_road_out
        !Use surface_moisture_min to avoid divide by zero for calculating solution_salt
        N_moles_water=1000*(g_road_out+surface_moisture_min)/M_atomic_water
    else
        g_road_out=g_road
        s_road_out=s_road
        !Set equilibrium limit to total moisture
        g_road_at_T_s_out=g_road+s_road
        s_road_at_T_s_out=g_road+s_road
        N_moles_water=1000*(g_road_out+surface_moisture_min)/M_atomic_water    
        do i=1,num_salt
            N_moles_salt(i)=max(0.,M2_road_salt(i)/M_atomic(salt_type(i)))
        enddo
    endif

    !Calculate vapour pressure and melt temperature of the solution (vp).
    do i=1,num_salt
    
        solution_salt(i)=max(0.,N_moles_salt(i)/(N_moles_water+N_moles_salt(i)))
        vp_ice=antoine_func(a_antoine_ice,b_antoine_ice,c_antoine_ice,T_s)
        vp_s=max(0.,antoine_func(a_antoine(salt_type(i)),b_antoine(salt_type(i)),c_antoine(salt_type(i)),T_s)+vp_correction(salt_type(i)))
        antoine_scaling=vp_ice/vp_s
        if (solution_salt(i).gt.saturated(salt_type(i))) then
            afactor(i)=1.
        else
            afactor(i)=((1.-antoine_scaling)*(solution_salt(i)/saturated(salt_type(i)))**salt_power)+antoine_scaling
            !afactor(i)=(1.-antoine_scaling)*(((1-(1-T_0/(T_0+melt_temperature_saturated(salt_type(i))))*log(1-solution_salt(i))/log(1-saturated(salt_type(i))))**-1.)-1.)+antoine_scaling
        endif
        
        RH_salt(i)=min(100.,100.*afactor(i)*vp_s/vp_ice)
        RH_salt_saturated=min(100.,100.*vp_s/vp_ice)
        RH_salt(i)=max(RH_salt_saturated,RH_salt(i))
        melt_temperature_salt(i)=max(melt_temperature_saturated(salt_type(i)),((solution_salt(i)/saturated(salt_type(i)))**salt_power)*melt_temperature_saturated(salt_type(i)))
        !melt_temperature_salt(i)=max(melt_temperature_saturated(salt_type(i)), &
        !    T_0*((1-(1-T_0/(T_0+melt_temperature_saturated(salt_type(i))))*log(1-solution_salt(i))/log(1-saturated(salt_type(i))))**-1)-T_0)
    
        !Adjust for the oversaturated case. Otherwise sets RH and melt
        !temperature to the saturated value
        if (use_oversaturated.eq.1) then
            !Oversaturated
            if (solution_salt(i).gt.saturated(salt_type(i))) then
                solution_salt(i)=min(over_saturated(salt_type(i)),solution_salt(i))
                RH_over_saturated(i)=(100*(1-RH_over_saturated_fraction(salt_type(i))) &
                    +RH_salt_saturated*RH_over_saturated_fraction(salt_type(i)))!Large number chosen to make the impact clear. Not known
                RH_salt(i)=min(100.,RH_salt_saturated+(RH_over_saturated(i)-RH_salt_saturated)/ &
                     (f_salt_sat(salt_type(i))*saturated(salt_type(i))-saturated(salt_type(i)))*(solution_salt(i)-saturated(salt_type(i))))
                melt_temperature_salt(i)=min(0.,melt_temperature_saturated(salt_type(i)) &
                     +(melt_temperature_oversaturated(salt_type(i))-melt_temperature_saturated(salt_type(i))) &
                     /(f_salt_sat(salt_type(i))*saturated(salt_type(i))-saturated(salt_type(i)))*(solution_salt(i)-saturated(salt_type(i))))
            endif
        endif
    
        !Calculate dissolved mass of salt
        if (solution_salt(i).lt.saturated(salt_type(i))) then
            dissolved_salt(i)=N_moles_salt(i)*M_atomic(salt_type(i))
        else
            dissolved_salt(i)=saturated(salt_type(i))*N_moles_water/(1-saturated(salt_type(i)))*M_atomic(salt_type(i))
        endif
        if (M2_road_salt(i).gt.0) then
            M_road_dissolved_ratio(i)=dissolved_salt(i)/M2_road_salt(i)
        else
            M_road_dissolved_ratio(i)=1.0
        endif
    
        !Set exact limits
        M_road_dissolved_ratio(i)=max(0.,min(1.,M_road_dissolved_ratio(i)))
        
    enddo

    end subroutine salt_solution_sub

!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
    function antoine_func(a,b,c,TC)
 
    implicit none
    
    !TC: Degrees C
    real a,b,c
    real TC
    real antoine_func
    
    antoine_func=10**(a-(b/(c+TC)))

    end function antoine_func
!--------------------------------------------------------------------------
