!****************************************************************************
!  NORTRIP_dispersion.f90 
!****************************************************************************
!
!   SUBROUTINE:     NORTRIP_dispersion
!   PURPOSE:        Calculates the net PM and NOx concentrations from observations
!                   Calculates the dispersion factor using NOX as a tracer
!   CALLED FROM:    NORTRIP_main_run
!   CALLS TO:       
!   SUBROUTINES:
!   FUNCTIONS:                      
!   VERSION:        Bruce Rolstad Denby 14.10.2015
!
!****************************************************************************

    subroutine NORTRIP_dispersion

    use NORTRIP_definitions
    
    implicit none

    !Determine if enough data is available for using NOX as a tracer
    !--------------------------------------------------------------------------
    if (available_airquality_data(NOX_obs_index).and.available_airquality_data(NOX_bg_index)) then
        available_airquality_data(NOX_net_index)=.true.
    else
        available_airquality_data(NOX_net_index)=.false.
    endif

    !Determine if PM data is available for net concentrations
    !--------------------------------------------------------------------------
    do x=pm_10,pm_25
        if (available_airquality_data(PM_obs_index(x)).and.available_airquality_data(PM_bg_index(x))) then
            available_airquality_data(PM_net_index(x))=.true.
        else
            available_airquality_data(PM_net_index(x))=.false.
        endif
    enddo
    
    !Calculate net NOX concentrations   
    !--------------------------------------------------------------------------
    if (available_airquality_data(NOX_net_index).and.available_airquality_data(NOX_emis_index)) then
        do ti=min_time,max_time
           if (airquality_data(NOX_obs_index,ti,ro).ne.nodata_input.and.airquality_data(NOX_bg_index,ti,ro).ne.nodata_input) then
                airquality_data(NOX_net_index,ti,ro)=airquality_data(NOX_obs_index,ti,ro)-airquality_data(NOX_bg_index,ti,ro)
            else
                airquality_data(NOX_net_index,ti,ro)=nodata
            endif
            if (airquality_data(NOX_net_index,ti,ro).le.0) then
                airquality_data(NOX_net_index,ti,ro)=nodata
            endif
        enddo
    endif

    !Calculate net PM concentrations
    !--------------------------------------------------------------------------
    do x=pm_10,pm_25
        if (available_airquality_data(PM_net_index(x))) then  
            do ti=min_time,max_time
                if (airquality_data(PM_obs_index(x),ti,ro).ne.nodata_input.and.airquality_data(PM_bg_index(x),ti,ro).ne.nodata_input) then
                    airquality_data(PM_net_index(x),ti,ro)=airquality_data(PM_obs_index(x),ti,ro)-airquality_data(PM_bg_index(x),ti,ro)
                else
                    airquality_data(PM_net_index(x),ti,ro)=nodata
                endif
                if (airquality_data(PM_net_index(x),ti,ro).le.0) then
                   airquality_data(PM_net_index(x),ti,ro)=nodata
                endif
            enddo
        endif
    enddo     

    !Calculate dispersion factor    
    !--------------------------------------------------------------------------
    if (.not.available_airquality_data(f_conc_index).and.available_airquality_data(NOX_emis_index).and.available_airquality_data(NOX_net_index)) then
        do ti=min_time,max_time
            if (airquality_data(NOX_net_index,ti,ro).ne.nodata &
                .and.airquality_data(NOX_emis_index,ti,ro).ne.nodata &
                .and.airquality_data(NOX_net_index,ti,ro).gt.conc_min &
                .and.airquality_data(NOX_emis_index,ti,ro).gt.emis_min) then
                airquality_data(f_conc_index,ti,ro)=airquality_data(NOX_net_index,ti,ro)/airquality_data(NOX_emis_index,ti,ro)
            else
                airquality_data(f_conc_index,ti,ro)=nodata
            endif
        enddo
    endif 
    
    end subroutine NORTRIP_dispersion
