PROJECT NINE
*Program to display the Perfect Ns in a given search range
*(c)2001 by Marc Christian Saribay (I-1L)
*Compile Date: August 14, 2001

c     Declare variables
      INTEGER RANGE,FOUND,N,SUM,I,OK
      REAL TEST

c     Main Program
      PRINT*,'PROJECT 9 (Perfect N)'
      PRINT*,'(c)2001 by Marc Christian Saribay (I-1L)'
10    PRINT*,'Enter Search Range:'
      PRINT*,'(NOTE: Larger value will consume too much time)'
      READ*,RANGE
      IF(RANGE.LT.2)THEN
        PRINT*,'Enter a value greater than 1'
        GOTO 10
      ELSE
        FOUND=0
        DO 20,N=2,RANGE,1
          SUM=0
          DO 30,I=1,N-1,1
            TEST=MOD(N,I)
            IF(TEST.EQ.0)THEN
              SUM=SUM+I
            ENDIF
30        CONTINUE
          IF(SUM.EQ.N)THEN
            PRINT*,N
            FOUND=FOUND+1
          ENDIF
20      CONTINUE
        IF(FOUND.EQ.0)THEN
          PRINT*,'No perfect Ns found in the given range'
        ELSE
          PRINT*,'***',FOUND,' Perfect N(s) found!'
        ENDIF
      ENDIF
40    PRINT*,'Try Again? (1-Yes/2-No)'
      READ*,OK
      IF(OK.EQ.1)THEN
        GOTO 10
      ELSEIF(OK.EQ.2)THEN
        GOTO 50
      ELSE
        PRINT*,'Invalid Entry!'
        GOTO 40
      ENDIF
50    PRINT*,'Thanks for using my program!'
      END