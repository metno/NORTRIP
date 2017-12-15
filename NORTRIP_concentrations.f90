!****************************************************************************
!  NORTRIP_concentrations.f90 
!****************************************************************************
!
!   SUBROUTINE:     NORTRIP_concentrations
!   PURPOSE:        Converts emissions to concentrations using dispersion factor
!   CALLED FROM:    NORTRIP_main_run
!   CALLS TO:       
!   SUBROUTINES:
!   FUNCTIONS:                      
!   VERSION:        Bruce Rolstad Denby 14.10.2015
!
!****************************************************************************

    subroutine NORTRIP_concentrations

    use NORTRIP_definitions
    
    implicit none

    !Convert emissions to concentrations within the road loop after the end of the time loop
    do ti=min_time,max_time 
        if (airquality_data(f_conc_index,ti,ro).ne.nodata) then
            C_data(1:num_source_all,1:num_size,1:num_process,ti,1:num_track,ro) &
                =E_road_data(1:num_source_all,1:num_size,1:num_process,ti,1:num_track,ro)*airquality_data(f_conc_index,ti,ro)
        else
            C_data(1:num_source_all,1:num_size,1:num_process,ti,1:num_track,ro)=nodata
        endif
    enddo
    
    end subroutine NORTRIP_concentrations
