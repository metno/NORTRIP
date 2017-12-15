    subroutine NORTRIP_unbin_variables
    !Unbins mass, emission and concentrations variables for output
    
    use NORTRIP_definitions
    
    implicit none

    !--------------------------------------------------------------------------
    !Put binned data in integrated data. Inside road loop
    !--------------------------------------------------------------------------
    do x=1,num_size
        M_road_data(:,x,ti,:,ro)=sum(M_road_bin_data(:,x:num_size,ti_bin,:,ro_bin),2)
        M_road_balance_data(:,x,:,ti,:,ro) =sum(M_road_bin_balance_data(:,x:num_size,:,ti_bin,:,ro_bin),2)
        E_road_data(:,x,:,ti,:,ro)=sum(E_road_bin_data(:,x:num_size,:,ti_bin,:,ro_bin),2)
        !C_data(:,x,:,ti,:,ro)=sum(C_bin_data(:,x:num_size,:,ti_bin,:,ro_bin),2)
    enddo

    !Set concentration data to nodata when f_conc not available as this is changed with the aggregation
    !--------------------------------------------------------------------------
    !do ro=1,n_roads
    !do ti=min_time,max_time
    !    if (airquality_data(f_conc_index,ti,ro).eq.nodata) then
    !        C_data(:,:,:,ti,:,ro)=nodata
    !    endif
    !enddo
    !enddo
    
    end subroutine NORTRIP_unbin_variables
    