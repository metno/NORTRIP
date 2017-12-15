      LOGICAL FUNCTION LEAP(YEAR)



! *** The function returns true if leap year, else false.



! *** Scalar arguments



      INTEGER YEAR



! *** YEAR - Year



      LEAP = .FALSE.

      IF (MOD(YEAR,  4) .EQ. 0 .AND. .NOT. (MOD(YEAR,100) .EQ. 0 .AND. MOD(YEAR,400) .NE. 0)) LEAP = .TRUE. 

      

      RETURN



! *** End of logical function LEAP



      END FUNCTION LEAP

