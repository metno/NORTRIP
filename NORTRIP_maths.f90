!Maths functions used in NORTRIP
!==========================================================================
    
    function sd_func(val,n_val,nodata_val)
    
    implicit none
    
    !Functions
    real mean_func
    
    !Input
    integer n_val
    real :: val(n_val)
    real :: nodata_val
    
    !Output
    real sd_func
    
    !Internal
    real :: mean_val
    integer :: max_index
    integer :: i
    integer :: count
    real :: s
    
    max_index=size(val)
    
    mean_val=mean_func(val,n_val,nodata_val)
    
    s=0
    count=0
    do i=1,max_index
        if (val(i).ne.nodata_val) then
            s=s+(val(i)-mean_val)**2
            count=count+1
        endif
    enddo
    
    if (count.gt.1) then
        sd_func=sqrt(s/(count-1))
    else
        sd_func=nodata_val
    endif
    
    end function sd_func

!==========================================================================

    function mean_func(val,n_val,nodata_val)
    
    !implicit none
    
    !Input
    !real, allocatable :: val(:)
    integer n_val
    real :: val(n_val)
    real :: nodata_val
    
    !Output
    real mean_func
    
    !Internal
    real :: mean_val
    integer :: max_index
    integer :: i
    integer :: count
    
    max_index=size(val)
    
    mean_val=0
    count=0
    do i=1,max_index
        if (val(i).ne.nodata_val) then
            mean_val=mean_val+val(i)
            count=count+1
        endif
    enddo
    
    if (count.gt.0) then
        mean_func=mean_val/count
    else
        mean_func=nodata_val
    endif
        
    end function mean_func

    !==========================================================================

    function cov_func(val1,val2,n_val,nodata_val)
    
    implicit none
        
    !Input
    integer n_val
    real :: val1(n_val)
    real :: val2(n_val)
    real :: nodata_val
    
    !Output
    real cov_func
    
    !Internal
    real :: mean_val1
    real :: mean_val2
    integer :: max_index1,max_index2
    integer :: i
    integer :: count
    real :: s
        
    max_index1=size(val1)
    max_index2=size(val2)
        
    if (max_index1.ne.max_index2) then
        cov_func=nodata_val
        return
    endif
    
    count=0
    mean_val1=0
    mean_val2=0
    do i=1,max_index1
        if (val1(i).ne.nodata_val.and.val2(i).ne.nodata_val) then
            mean_val1=mean_val1+val1(i)
            mean_val2=mean_val2+val2(i)
            count=count+1
        endif
    enddo
    
    if (count.gt.0) then
        mean_val1=mean_val1/count
        mean_val2=mean_val2/count
    else
        mean_val1=nodata_val
        mean_val2=nodata_val
        cov_func=nodata_val
        return
    endif
    
    s=0
    do i=1,max_index1
        if (val1(i).ne.nodata_val.and.val2(i).ne.nodata_val) then
            s=s+(val1(i)-mean_val1)*(val2(i)-mean_val2)
            count=count+1
        endif
    enddo
    
    if (count.gt.1) then
        cov_func=s/(count-1)
    else
        cov_func=nodata_val
    endif
       
    end function cov_func

    !==========================================================================

    function correlation_func(val1,val2,n_val,nodata_val)
    
    implicit none
        
    !Input
    integer n_val
    real :: val1(n_val)
    real :: val2(n_val)
    real :: nodata_val
    
    !Output
    real correlation_func
    
    !Internal
    real :: s_val1
    real :: s_val2
    real :: cov_val
    real :: mean_val1
    real :: mean_val2
    integer :: count
    integer :: max_index1,max_index2
    integer i
    
    max_index1=size(val1)
    max_index2=size(val2)
        
    if (max_index1.ne.max_index2) then
        correlation_func=nodata_val
        return
    endif
    
    mean_val1=0
    mean_val2=0
    count=0
    do i=1,max_index1
        if (val1(i).ne.nodata_val.and.val2(i).ne.nodata_val) then
            mean_val1=mean_val1+val1(i)
            mean_val2=mean_val2+val2(i)
            count=count+1
        endif
    enddo
    
    if (count.gt.0) then
        mean_val1=mean_val1/count
        mean_val2=mean_val2/count
    else
        mean_val1=nodata_val
        mean_val2=nodata_val
        correlation_func=nodata_val
        return
    endif

    s_val1=0
    s_val2=0
    cov_val=0
    do i=1,max_index1
        if (val1(i).ne.nodata_val.and.val2(i).ne.nodata_val) then
            cov_val=cov_val+(val1(i)-mean_val1)*(val2(i)-mean_val2)
            s_val1=s_val1+(val1(i)-mean_val1)**2
            s_val2=s_val2+(val2(i)-mean_val2)**2
        endif
    enddo  
        
    if (count.gt.1.and.s_val1.gt.0.and.s_val2.gt.0) then
        correlation_func=cov_val/(sqrt(s_val1)*sqrt(s_val2))
    else
        correlation_func=nodata_val
    endif
            
    end function correlation_func

    !==========================================================================
   
    function rsquare_func(val1,val2,n_val,nodata_val)
    
    !This is just correlation**2, which it is if it is in regard to linear fitting
    
    implicit none
    
    !Functions
    real correlation_func
    
    !Input
    integer n_val
    real :: val1(n_val)
    real :: val2(n_val)
    real :: nodata_val
    
    !Output
    real rsquare_func
    
    rsquare_func=correlation_func(val1,val2,n_val,nodata_val)
    
    if (rsquare_func.ne.nodata_val) then
        rsquare_func=rsquare_func**2
    else
        rsquare_func=nodata_val
    endif
               
    end function rsquare_func

!==========================================================================

    function rmse_func(val1,val2,n_val,nodata_val)
    
    implicit none
        
    !Input
    integer n_val
    real :: val1(n_val)
    real :: val2(n_val)
    real :: nodata_val
    
    !Output
    real rmse_func
    
    !Internal
    integer :: max_index1,max_index2
    integer :: i
    integer :: count
    real :: s
        
    max_index1=size(val1)
    max_index2=size(val2)
        
    if (max_index1.ne.max_index2) then
        rmse_func=nodata_val
        return
    endif

    count=0
    s=0
    do i=1,max_index1
        if (val1(i).ne.nodata_val.and.val2(i).ne.nodata_val) then
            s=s+(val1(i)-val2(i))**2
            count=count+1
        endif
    enddo
    
    if (count.gt.0) then
        rmse_func=sqrt(s/count)
    else
        rmse_func=nodata_val
    endif
            
    end function rmse_func

!==========================================================================

    function percentile_func(val_in,n_val,percentile,nodata_val)
    !Uses a simple sorting routine
    !Percentile is a percentage
    
    implicit none
    
    !Input
    integer n_val
    real :: val_in(n_val)
    real :: nodata_val
    real :: percentile
    
    !Output
    real percentile_func
    
    !Internal
    integer :: max_index
    integer :: i,j
    integer :: count
    real :: val_i,val_j
    integer :: percentile_index
    real :: val(n_val)
    
    max_index=size(val_in,1)
    
    val=val_in
    
    !Bubble sort. Can be more effective if it doesn't bubble through the entire array
    do i=2,max_index
        val_i=val(i)
        do j=i,2,-1
            val_j=val(j-1)
            if (val_i.lt.val_j) then
                val(j-1)=val_i
                val(j)=val_j
            endif
        enddo
    enddo
    
    !Remove nodata values
    count=0
    do i=1,max_index
        if (val(i).ne.nodata_val) then
            count=count+1
            !write(*,*) count,i,max_index
            val(count)=val(i)
        endif
    enddo
        
    if (count.gt.0) then
        percentile_index=max(int(percentile/100.*count+.5),1)
        percentile_func=val(percentile_index)
    else
        percentile_func=nodata_val
    endif
            
    end function percentile_func

!==========================================================================

    !Do not use yet. Need to reorganise like percentile_func so that val does not change
    function nhigh_func(val,n_val,nhigh,nodata_val)
    !Uses a simple sorting routine
    !nhigh is a number
    
    implicit none
    
    !Input
    integer n_val
    real :: val(n_val)
    real :: nodata_val
    integer :: nhigh
    
    !Output
    real nhigh_func
    
    !Internal
    integer :: max_index
    integer :: i,j
    integer :: count
    real :: val_i,val_j
    integer :: nhigh_index
    
    max_index=size(val)
    
    !Bubble sort. Can be more effective if it doesn't bubble through the entire array
    do i=2,max_index
        val_i=val(i)
        do j=i,2,-1
            val_j=val(j-1)
            if (val_i.lt.val_j) then
                val(j-1)=val_i
                val(j)=val_j
            endif
        enddo
     enddo
    
    !Remove nodata values
    do i=1,max_index
        if (val(i).ne.nodata_val) then
            count=count+1
            val(count)=val(i)
        endif
    enddo
        
    if (count.gt.0) then
        nhigh_index=max(count-nhigh+1,1)
        nhigh_func=val(nhigh_index)
    else
        nhigh_func=nodata_val
    endif
        
    end function nhigh_func

    !==========================================================================

    function fb_func(val1,val2,n_val,nodata_val)
    !Fractional bias function
    
    implicit none
        
    !Input
    integer n_val
    real :: val1(n_val)
    real :: val2(n_val)
    real :: nodata_val
    
    !Output
    real fb_func
    
    !Internal
    real :: mean_val1
    real :: mean_val2
    integer :: max_index1,max_index2
    integer :: i
    integer :: count
    real :: s
        
    max_index1=size(val1)
    max_index2=size(val2)
        
    if (max_index1.ne.max_index2) then
        fb_func=nodata_val
        return
    endif
    
    count=0
    mean_val1=0
    mean_val2=0
    do i=1,max_index1
        if (val1(i).ne.nodata_val.and.val2(i).ne.nodata_val) then
            mean_val1=mean_val1+val1(i)
            mean_val2=mean_val2+val2(i)
            count=count+1
        endif
    enddo
    
    if (count.gt.0) then
        mean_val1=mean_val1/count
        mean_val2=mean_val2/count
        fb_func=(mean_val1-mean_val2)/(mean_val1+mean_val2)*2
    else
        mean_val1=nodata_val
        mean_val2=nodata_val
        fb_func=nodata_val
        return
    endif
       
    end function fb_func

!==========================================================================
!==========================================================================


    !subroutine daily_mean_sub()