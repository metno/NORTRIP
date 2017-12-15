    subroutine open_logfile
    
    use NORTRIP_definitions
    
    implicit none
    
    !Open log file
    if (unit_logfile.gt.0) then
        open(unit_logfile,file=filename_log,status='replace')!,position='append')
    endif    
    
    end subroutine open_logfile

    
    
    subroutine close_logfile

    use NORTRIP_definitions
    
    implicit none
    
    if (unit_logfile.gt.0) then
        close(unit_logfile,status='keep')
    endif
   
    end subroutine close_logfile
