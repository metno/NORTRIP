!----------------------------------------------------------------------
! Various functions for manipulating and reading text in tab format
!----------------------------------------------------------------------

!----------------------------------------------------------------------
    function match_string_char_2048(match_str,unit_in,unit_output,default_char)
    !Finds a leading string and returns the string that follows it
    !Tab delimitted before and after
    implicit none
    
    character(2048) match_string_char_2048
    character (*) match_str,default_char
    character(2048) temp_str1,temp_str2,temp_str
    integer unit_in,unit_output
    integer index_val
    
    temp_str1=''
    temp_str2='Not available'
    temp_str2=trim(default_char)
    rewind(unit_in)
    do while (index(temp_str1,match_str).eq.0)
        read(unit_in,'(a)',end=10) temp_str
        if (temp_str(1:1).eq.'#'.or.temp_str(1:1).eq.'!') goto 5 !If first character is ! or # then ignore and go to next
        index_val=index(temp_str,achar(9))
        temp_str1=temp_str(1:index_val-1)
        temp_str=temp_str(index_val+1:)
        index_val=index(temp_str,achar(9))
        if (index_val.gt.0) then
            temp_str2=temp_str(1:index_val-1)
        else
            temp_str2=temp_str
        endif
        
5	end do

    match_string_char_2048=trim(temp_str2)
    
    if (unit_output.ge.0) then
        write(unit_output,'(A40,A3,A)') trim(match_str),' = ',adjustl(trim(match_string_char_2048))
    endif
    return
    
10	write(unit_output,'(A)') 'WARNING: No match found to "'//trim(match_str)//'" in input files. Returning: '//trim(default_char)
    match_string_char_2048=default_char
    
    end function match_string_char_2048
!----------------------------------------------------------------------
    
!----------------------------------------------------------------------
    function match_string_char(match_str,unit_in,unit_output,default_char)
    !Finds a leading string and returns the string that follows it
    !Tab delimitted before and after
    implicit none
    
    character(256) match_string_char
    character (*) match_str,default_char
    character(256) temp_str1,temp_str2,temp_str
    integer unit_in,unit_output
    integer index_val
    
    temp_str1=''
    temp_str2='Not available'
    temp_str2=trim(default_char)
    rewind(unit_in)
    do while (index(temp_str1,match_str).eq.0)
        read(unit_in,'(a)',end=10) temp_str
        if (temp_str(1:1).eq.'#'.or.temp_str(1:1).eq.'!') goto 5 !If first character is ! or # then ignore and go to next
        index_val=index(temp_str,achar(9))
        temp_str1=temp_str(1:index_val-1)
        temp_str=temp_str(index_val+1:)
        index_val=index(temp_str,achar(9))
        if (index_val.gt.0) then
            temp_str2=temp_str(1:index_val-1)
        else
            temp_str2=temp_str
        endif
5	end do

    match_string_char=trim(temp_str2)
    
    if (unit_output.ge.0) then
        write(unit_output,'(A40,A3,A)') trim(match_str),' = ',adjustl(trim(match_string_char))
    endif
    return
    
10	write(unit_output,'(A)') 'WARNING: No match found to "'//trim(match_str)//'" in input files. Returning: '//trim(default_char)
    match_string_char=default_char
    
    end function match_string_char
!----------------------------------------------------------------------

!----------------------------------------------------------------------
    function match_string_val(match_str,unit_in,unit_output,default_val)
    !Finds a leading string and returns the real variable that follows it
    !Tab delimitted before and after
    implicit none
    
    real match_string_val,default_val
    character (*) match_str
    character(256) temp_str1,temp_str2,temp_str
    integer unit_in,unit_output
    integer index_val
    
    match_string_val=default_val
    temp_str1=''
    temp_str2='Not available'
    rewind(unit_in)
    do while (index(temp_str1,match_str).eq.0)
        read(unit_in,'(a)',end=10) temp_str
        if (temp_str(1:1).eq.'#'.or.temp_str(1:1).eq.'!') goto 5 !If first character is ! or # then ignore and go to next
        index_val=index(temp_str,achar(9))
     !write(*,*) index_val,temp_str
        temp_str1=temp_str(1:index_val-1)
        temp_str=temp_str(index_val+1:)
        index_val=index(temp_str,achar(9))
     !write(*,*) index_val,temp_str
       if (index_val.gt.0) then
            temp_str2=temp_str(1:index_val-1)
        else
            temp_str2=temp_str
        endif
5    end do
    !write(*,*) index_val,temp_str2
    if (LEN(trim(temp_str2)).gt.0) then
        read(temp_str2,*,err=15) match_string_val
    else
        goto 15
    endif
    if (unit_output.ge.0) then
        write(unit_output,'(A40,A3,ES10.2)') trim(match_str),' = ',match_string_val
    endif
    return
    
10  write(unit_output,'(A,f12.3)') 'WARNING: No match found to "'//trim(match_str)//'" in input files. Returning default value: ',match_string_val
    return
15	write(unit_output,'(A,f12.3)') 'WARNING: No value found for "'//trim(match_str)//'" in input files. Setting to default: ',match_string_val

    end function match_string_val
!----------------------------------------------------------------------

!----------------------------------------------------------------------
    function match_string_int(match_str,unit_in,unit_output,default_int)
    !Finds a leading string and returns the integer variable that follows it
    !Tab delimitted before and after
    implicit none
    
    integer match_string_int,default_int
    character (*) match_str
    character(256) temp_str1,temp_str2,temp_str
    integer unit_in,unit_output
    integer index_val
    
    match_string_int=default_int
    temp_str1=''
    temp_str2='Not available'
    rewind(unit_in)
    do while (index(temp_str1,match_str).eq.0)
        read(unit_in,'(a)',end=10) temp_str
        if (temp_str(1:1).eq.'#'.or.temp_str(1:1).eq.'!') goto 5 !If first character is ! or # then ignore and go to next
        index_val=index(temp_str,achar(9))
        temp_str1=temp_str(1:index_val-1)
        temp_str=temp_str(index_val+1:)
        index_val=index(temp_str,achar(9))
        if (index_val.gt.0) then
            temp_str2=temp_str(1:index_val-1)
        else
            temp_str2=temp_str
        endif
5    end do
    
    if (LEN(trim(temp_str2)).gt.0) then
        read(temp_str2,*,err=15) match_string_int
    else
        goto 15
    endif
    
    if (unit_output.ge.0) then
        write(unit_output,'(A40,A3,I10)') trim(match_str),' = ',match_string_int
    endif
    return
    
10	write(unit_output,'(A,i)') 'WARNING: No match found to "'//trim(match_str)//'" in input files. Setting to ',match_string_int
    return
15	write(unit_output,'(A,i)') 'WARNING: No value found for "'//trim(match_str)//'" in input files. Setting to ',match_string_int

    end function match_string_int
!----------------------------------------------------------------------

!----------------------------------------------------------------------
    function read_string_val(unit_in,unit_output)
    !Reads a leading string and returns the real variable that follows it
    !Tab delimitted between.
    implicit none
    
    integer n_val
    real read_string_val
    character(256) temp_str1,temp_str2,temp_str
    integer unit_in,unit_output
    integer index_val
    
    read_string_val=-999
    temp_str1=''
    temp_str2='Not available'
    read(unit_in,'(a)',end=10) temp_str
    index_val=index(temp_str,achar(9))
    temp_str1=temp_str(1:index_val-1)
    temp_str=temp_str(index_val+1:)
    index_val=index(temp_str,achar(9))
    if (index_val.gt.0) then
        temp_str2=temp_str(1:index_val-1)
    else
        temp_str2=temp_str
    endif
    read(temp_str2,*) read_string_val
    if (unit_output.ge.0) then
        write(unit_output,'(A40,A3,es10.2)') trim(temp_str1),' = ',read_string_val
    endif
    return
    
10	write(unit_output,*) 'ERROR: End of file read during read_string_val'

    end function read_string_val
!----------------------------------------------------------------------

    
!----------------------------------------------------------------------
    subroutine string_split_tab(unit_in,unit_output,out_str)
    !Reads the next string in unit_in and returns the string that follows the first tab
    
    implicit none
    
    integer unit_in,unit_output
    integer index_val
    character(256) temp_str
    character(256) out_str(2) 
    
    read(unit_in,'(a)',ERR=10) temp_str
    index_val=index(temp_str,achar(9))
    out_str(1)=temp_str(1:index_val-1)
    out_str(2)=temp_str(index_val+1:)  
    return
10  write(unit_output,*) 'ERROR: End of file read during string_split_tab'
    
    end subroutine string_split_tab
    
!----------------------------------------------------------------------

!----------------------------------------------------------------------
    subroutine read_line_int1(unit_in,unit_output,val1)
    !Reads a leading string and returns the integer variable that follows it
    !Tab delimitted between.
    implicit none
    
    character(256) header_str,val_str,temp_str
    integer unit_in,unit_output
    integer index_val
    integer val1
    
    read(unit_in,'(a)',end=10) temp_str
    index_val=index(temp_str,achar(9))
    header_str=temp_str(1:index_val-1)
    val_str=temp_str(index_val+1:)
    !Only take the first value
    index_val=index(val_str,achar(9))
    if (index_val.gt.0) then
        val_str=val_str(1:index_val-1)
    endif
    read(val_str,*) val1
    if (unit_output.ge.0) then
       write(unit_output,'(A40,A3,i10)') trim(header_str),' = ',val1
    endif
    return
    
10	write(unit_output,*) 'ERROR: End of file read during read_line_int1'

    end subroutine read_line_int1
!----------------------------------------------------------------------

!----------------------------------------------------------------------
    subroutine read_line_int2(unit_in,unit_output,val1,val2)
    !Reads a leading string and returns the real variable that follows it
    !Tab delimitted between.
    implicit none
    
    character(256) header_str,val_str,temp_str
    integer unit_in,unit_output
    integer index_val
    integer val1,val2
 
    
    read(unit_in,'(a)',end=10) temp_str
    index_val=index(temp_str,achar(9))
    header_str=temp_str(1:index_val-1)
    val_str=temp_str(index_val+1:)
    read(val_str,*) val1,val2
    if (unit_output.ge.0) then
        write(unit_output,'(A40,A3,2i10)') trim(header_str),' = ',val1,val2
    endif
    return
    
10	write(unit_output,*) 'ERROR: End of file read during read_line_int2'

    end subroutine read_line_int2
!----------------------------------------------------------------------
!----------------------------------------------------------------------
    subroutine read_line_val1(unit_in,unit_output,val1)
    !Reads a leading string and returns the real variable that follows it
    !Tab delimitted between.
    implicit none
    
    character(256) header_str,val_str,temp_str
    integer unit_in,unit_output
    integer index_val
    real val1
    
    read(unit_in,'(a)',end=10) temp_str
    index_val=index(temp_str,achar(9))
    header_str=temp_str(1:index_val-1)
    val_str=temp_str(index_val+1:)
    !Only take the first value
    index_val=index(val_str,achar(9))
    if (index_val.gt.0) then
        val_str=val_str(1:index_val-1)
    endif
    read(val_str,*) val1
    if (unit_output.ge.0) then
       write(unit_output,'(A40,A3,es10.2)') trim(header_str),' = ',val1
    endif
    return
    
10	write(unit_output,*) 'ERROR: End of file read during read_line_val1'

    end subroutine read_line_val1
!----------------------------------------------------------------------

!----------------------------------------------------------------------
    subroutine read_line_val2(unit_in,unit_output,val1,val2)
    !Reads a leading string and returns the real variable that follows it
    !Tab delimitted between.
    implicit none
    
    character(256) header_str,val_str,temp_str
    integer unit_in,unit_output
    integer index_val
    real val1,val2
    
    read(unit_in,'(a)',end=10) temp_str
    index_val=index(temp_str,achar(9))
    header_str=temp_str(1:index_val-1)
    val_str=temp_str(index_val+1:)
    read(val_str,*) val1,val2
    if (unit_output.ge.0) then
        write(unit_output,'(A40,A3,2es10.2)') trim(header_str),' = ',val1,val2
    endif
    return
    
10	write(unit_output,*) 'ERROR: End of file read during read_line_val2'

    end subroutine read_line_val2
!----------------------------------------------------------------------

!----------------------------------------------------------------------
    subroutine read_line_val3(unit_in,unit_output,val1,val2,val3)
    !Reads a leading string and returns the real variable that follows it
    !Tab delimitted between.
    implicit none
    
    character(256) header_str,val_str,temp_str
    integer unit_in,unit_output
    integer index_val
    real val1,val2,val3
    
    read(unit_in,'(a)',end=10) temp_str
    index_val=index(temp_str,achar(9))
    header_str=temp_str(1:index_val-1)
    val_str=temp_str(index_val+1:)        
    read(val_str,*) val1,val2,val3
    if (unit_output.ge.0) then
        write(unit_output,'(A40,A3,3es10.2)') trim(header_str),' = ',val1,val2,val3
    endif
    return
    
10	write(unit_output,*) 'ERROR: End of file read during read_line_val3'

    end subroutine read_line_val3
!----------------------------------------------------------------------

    !----------------------------------------------------------------------
    subroutine read_line_val1or3(unit_in,unit_output,val1,val2,val3)
    !Reads a leading string and returns the real variable that follows it
    !Tab delimitted between.
    implicit none
    
    character(256) header_str,val_str,temp_str
    integer unit_in,unit_output
    integer index_val,verify_val,i
    real val1,val2,val3
    
    read(unit_in,'(a)',end=10) temp_str
    index_val=index(temp_str,achar(9))
    header_str=temp_str(1:index_val-1)
    val_str=temp_str(index_val+1:)
    read(val_str,*) val1
    !Go to next tab
    index_val=index(val_str,achar(9))
    if (index_val.eq.0) then
        val2=0;val3=0
    else
        val_str=val_str(index_val+1:)
        !Check for any more numbers. This is the most redickuless thing I have ever done but do not know what else to do
        index_val=0
        do i=48,57
            index_val=index_val+index(val_str,achar(i))
            !write(*,*) i,index_val,achar(i)
        enddo
        if (index_val.eq.0) then
            val2=0;val3=0
        else
            !Assume there are two more
            read(val_str,*) val2,val3
        endif
    endif

    if (unit_output.ge.0) then
        write(unit_output,'(A40,A3,3es10.2)') trim(header_str),' = ',val1,val2,val3
    endif
    return
    
10	write(unit_output,*) 'ERROR: End of file read during read_line_val3'

    end subroutine read_line_val1or3
!----------------------------------------------------------------------

!----------------------------------------------------------------------
    subroutine read_line_val4(unit_in,unit_output,val1,val2,val3,val4)
    !Reads a leading string and returns the real variable that follows it
    !Tab delimitted between.
    implicit none
    
    character(256) header_str,val_str,temp_str
    integer unit_in,unit_output
    integer index_val
    real val1,val2,val3,val4
    
    read(unit_in,'(a)',end=10) temp_str
    index_val=index(temp_str,achar(9))
    header_str=temp_str(1:index_val-1)
    val_str=temp_str(index_val+1:)
    read(val_str,*) val1,val2,val3,val4
    if (unit_output.ge.0) then
        write(unit_output,'(A40,A3,4es10.2)') trim(header_str),' = ',val1,val2,val3,val4
    endif
    return
    
10	write(unit_output,*) 'ERROR: End of file read during read_line_val4'

    end subroutine read_line_val4
!----------------------------------------------------------------------

!----------------------------------------------------------------------
    subroutine read_line_val5(unit_in,unit_output,val1,val2,val3,val4,val5)
    !Reads a leading string and returns the real variable that follows it
    !Tab delimitted between.
    implicit none
    
    character(256) header_str,val_str,temp_str
    integer unit_in,unit_output
    integer index_val
    real val1,val2,val3,val4,val5
    
    read(unit_in,'(a)',end=10) temp_str
    index_val=index(temp_str,achar(9))
    header_str=temp_str(1:index_val-1)
    val_str=temp_str(index_val+1:)
    read(val_str,*) val1,val2,val3,val4,val5
    if (unit_output.ge.0) then
        write(unit_output,'(A40,A3,5es10.2)') trim(header_str),' = ',val1,val2,val3,val4,val5
    endif
    return
    
10	write(unit_output,*) 'ERROR: End of file read during read_line_val5'

    end subroutine read_line_val5
!----------------------------------------------------------------------

!----------------------------------------------------------------------
    subroutine match_string_multi_int(match_str,unit_in,unit_output,val,n_val)
    !Finds a leading string and returns all the integer variables that follows it
    !Tab delimitted before and free format after
    implicit none
    
    integer n_val
    integer val(n_val)
    character (*) match_str
    character(256) temp_str1,temp_str2,temp_str
    integer unit_in,unit_output
    integer index_val
    
    val=-999
    temp_str1=''
    temp_str2='Not available'
    rewind(unit_in)
    do while (index(temp_str1,match_str).eq.0)
        read(unit_in,'(a)',end=10) temp_str
        if (temp_str(1:1).eq.'#'.or.temp_str(1:1).eq.'!') goto 5 !If first character is ! or # then ignore and go to next
        index_val=index(temp_str,achar(9))
        temp_str1=temp_str(1:index_val-1)
        temp_str=temp_str(index_val+1:)
        index_val=index(temp_str,achar(9))
        !if (index_val.gt.0) then
        !    temp_str2=temp_str(1:index_val-1)
        !else
        !    temp_str2=temp_str
        !endif
5    end do
    if (LEN(trim(temp_str)).gt.0) then
        read(temp_str,*) val(1:n_val)
    else
        goto 15
    endif
    
    if (unit_output.ge.0) then
        write(unit_output,'(A40,A3,<n_val>I10)') trim(match_str),' = ',val
    endif
    return
    
10  write(unit_output,*) 'WARNING: No match found to "'//trim(match_str)//'" in input files. Set to -999'
    return
15	write(unit_output,*) 'WARNING: No values for "'//trim(match_str)//'" in input files'

    end subroutine match_string_multi_int
!----------------------------------------------------------------------

    !----------------------------------------------------------------------
    subroutine match_string_multi_val(match_str,unit_in,unit_output,val,n_val)
    !Finds a leading string and returns all the integer variables that follows it
    !Tab delimitted before and free format after
    !Not used
    implicit none
    
    integer n_val
    real val(n_val)
    character (*) match_str
    character(256) temp_str1,temp_str2,temp_str
    integer unit_in,unit_output
    integer index_val
    
    val=-999.
    temp_str1=''
    temp_str2='Not available'
    rewind(unit_in)
    do while (index(temp_str1,match_str).eq.0)
        read(unit_in,'(a)',end=10) temp_str
        if (temp_str(1:1).eq.'#'.or.temp_str(1:1).eq.'!') goto 5 !If first character is ! or # then ignore and go to next
        index_val=index(temp_str,achar(9))
        temp_str1=temp_str(1:index_val-1)
        temp_str=temp_str(index_val+1:)
        index_val=index(temp_str,achar(9))
        !if (index_val.gt.0) then
        !    temp_str2=temp_str(1:index_val-1)
        !else
        !    temp_str2=temp_str
        !endif
5    end do
    if (LEN(trim(temp_str)).gt.0) then
        read(temp_str,*) val(1:n_val)
    else
        goto 15
    endif
    
    if (unit_output.ge.0) then
        write(unit_output,'(A40,A3,<n_val>es10.2)') trim(match_str),' = ',val
    endif
    return
    
10  write(unit_output,*) 'WARNING: No match found to "'//trim(match_str)//'" in input files. Set to -999'
    return
15	write(unit_output,*) 'WARNING: No values for "'//trim(match_str)//'" in input files'

    end subroutine match_string_multi_val
!----------------------------------------------------------------------

!----------------------------------------------------------------------
    function replace_string_char(replace_str,match_str,read_str)
    !Finds a leading string and returns the string that follows it
    !Tab delimitted before and after
    implicit none
    
    character(256) replace_string_char
    character (*) match_str,replace_str,read_str
    character(256) temp_str1,temp_str2
    integer index_start,index_stop
     
    replace_string_char=read_str
  
    index_start=index(read_str,trim(match_str))
    if (index_start.ne.0) then
        index_stop=index_start+len(match_str)
        temp_str1=read_str(1:index_start-1)
        temp_str2=read_str(index_stop:len(read_str))
        replace_string_char=trim(temp_str1)//trim(replace_str)//trim(temp_str2)
    endif
    !write(*,'(A)') trim(replace_string_char)

    end function replace_string_char
!----------------------------------------------------------------------
