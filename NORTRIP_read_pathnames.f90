!****************************************************************************
!NORTRIP_read_pathnames.f90
!****************************************************************************
!
!   SUBROUTINE:     NORTRIP_read_pathnames
!   PURPOSE:        Reads in the information/configuration file that sets path and file names
!                   Creates log file for the first time here
!   CALLED FROM:    NORTRIP_fortran_control
!   CALLS TO:       match_string_char (NORTRIP_reading_functions)
!   VERSION:        14.10.2015
!   AUTHOR:         Bruce Rolstad Denby 
!                   Norwegian Meteorological Institute (www.met.no)
!
!****************************************************************************

    subroutine read_NORTRIP_pathnames

    use NORTRIP_definitions
    
    implicit none
    
    character(256) temp_path
    character(256) temp_file
    character(256) temp_name
    character(256) temp_str,temp_str1,temp_str2
    integer unit_in
    integer index_val

    !Functions
    character(256) match_string_char
           
    unit_in=unit_read_NORTRIP_pathnames
    open(unit_in,file=commandline_filename,access='sequential',status='old',readonly)

    !Read log file name
    filename_log=match_string_char(trim('Log file name'),unit_in,0,'')
    
    !If no log file then write to screen
    if (filename_log.eq.'') then
        unit_logfile=0
    endif

    if (unit_logfile.gt.0) then
        write(*,'(A)') 'Writing to log file' 
  	    write(*,'(A)') '================================================================'
    endif
   
    !Open log file for the first time
    call open_logfile
   
	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Reading model path and file names (read_NORTRIP_pathnames)'
	write(unit_logfile,'(A)') '================================================================' 

    !Input path to model parameter files
    path_inputparam=match_string_char('Model input parameter path',unit_in,unit_logfile,'')
    filename_inputparam=match_string_char('Model parameter filename',unit_in,unit_logfile,'')

    !Input path to model meteo, traffic, activitty  and air quality data
    path_inputdata=match_string_char('Model input data path',unit_in,unit_logfile,'')
    filename_inputdata=match_string_char('Model input data filename',unit_in,unit_logfile,'')

    !General output path
    path_outputdata=match_string_char('Model output data path',unit_in,unit_logfile,'')
    filename_outputdata=match_string_char('Model output data filename',unit_in,unit_logfile,'')

    !Input path to complete initialisation path
    path_init=match_string_char('Model init data path',unit_in,unit_logfile,'')
    filename_init=match_string_char('Model init data filename',unit_in,unit_logfile,'')
   
    !Output path for emission files only
    path_output_emis=match_string_char('Model output emission path',unit_in,unit_logfile,'')
    filename_output_emis=match_string_char('Model output emission filename',unit_in,unit_logfile,'')
    filename_output_grid_emis=match_string_char('Model output gridded emission filename',unit_in,unit_logfile,'')

    !Output path for road meteo data only
    path_output_roadmeteo=match_string_char('Model output road meteo path',unit_in,unit_logfile,'')
    filename_output_roadmeteo=match_string_char('Model output road meteo filename',unit_in,unit_logfile,'')

    !Output path for complete data set
    path_fortran=match_string_char('Model fortran path',unit_in,unit_logfile,'')
    path_fortran_output=match_string_char('Model fortran output path',unit_in,unit_logfile,'')
    
    !Figure output path, not used in fortran routine
    path_outputfig=match_string_char('Model output figures path',unit_in,unit_logfile,'')
    
    !Path to OSPM model, not currently used in fortran routine
    path_ospm=match_string_char('Model ospm path',unit_in,unit_logfile,'')
        
    !Close log file
    !if (unit_logfile.gt.0) then
    !    close(unit_logfile,status='keep')
    !endif    

10	close(unit_in,status='keep')
     
    end subroutine read_NORTRIP_pathnames
!----------------------------------------------------------------------

