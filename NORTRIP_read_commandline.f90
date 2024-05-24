!****************************************************************************
!NORTRIP_read_commandline.f90
!****************************************************************************
!
!   SUBROUTINE:     read_NORTRIP_commandline
!   PURPOSE:        Reads in pathname file name and start and end saving dates from command line
!                   If no filename exists then uses default test filename
!                   If no dates exist then saves for input data dates
!                   Checks to see if filename exists. If not then stops
!   CALLED FROM:    NORTRIP_fortran_control
!   CALLS TO:       date_to_datestr (NORTRIP_time_functions)
!   VERSION:        14.10.2015
!   AUTHOR:         Bruce Rolstad Denby 
!                   Norwegian Meteorological Institute (www.met.no)
!
!****************************************************************************
    
    subroutine read_NORTRIP_commandline
    
    use NORTRIP_definitions
    
    implicit none
    
    character(256) commandline_str
    character(256) start_date_input_str,end_date_input_str 
    integer start_date_input(num_date_index),end_date_input(num_date_index)
    logical exists
    integer status,length
    integer character_length
    integer :: unit_out=0
    
    !logfile name not defined at this point
    
    !Read in the command line if one is given and set the main info file
    commandline_filename=''
    call GET_COMMAND_ARGUMENT(1,commandline_str,length,status)
    
    if (commandline_str.eq.'') then
        !Hard coded filename used when testing without command line
        commandline_filename='C:\NORTRIP\Road dust model\model run info\text\modelrun_file.txt'
    else
        commandline_filename=trim(commandline_str)
    endif
    write(unit_out,'(A,A)') ' Commandline filename set to: ',commandline_filename
    
    !Check commandline_filename file for reading in paths
    inquire(file=trim(commandline_filename),exist=exists)
    if (.not.exists) then
        write(unit_out,'(A)')'ERROR: Main NORTRIP input file does not exist. Stopping: '//trim(commandline_filename)
        stop 37
    endif

    !Read in the date strings, check for length and then allocate to save date strings
    call GET_COMMAND_ARGUMENT(2,start_date_input_str,length,status)
    call GET_COMMAND_ARGUMENT(3,end_date_input_str,length,status)

    !Set saving start date string
    character_length = LEN_TRIM(start_date_input_str)
    if (character_length >= 10) then
        start_date_input=0
        read(start_date_input_str, *)  start_date_input(year_index),start_date_input(month_index),start_date_input(day_index),start_date_input(hour_index),start_date_input(minute_index)
        call date_to_datestr(start_date_input,date_format_str,start_date_save_str)
        write(unit_out,'(A,A)') ' Set start date to: ', start_date_save_str
    else
        write(unit_out,'(A)') ' Start date is too short. Setting to input data dates.'
        start_date_save_str=''
    endif
    
    !Set saving end date string
    character_length = LEN_TRIM(end_date_input_str)
    if (character_length >= 10) then
        end_date_input=0
        read(end_date_input_str, *)  end_date_input(year_index),end_date_input(month_index),end_date_input(day_index),end_date_input(hour_index), end_date_input(minute_index)
        call date_to_datestr(end_date_input,date_format_str,end_date_save_str)
        write(unit_out,'(A,A)') ' Set end date to:   ', end_date_save_str
    else
        write(unit_out,'(A)') ' End date is too short.  Setting to input data dates.'
        end_date_save_str=''
    endif
       
    end subroutine read_NORTRIP_commandline