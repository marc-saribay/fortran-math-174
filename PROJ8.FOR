PROJECT EIGHT
* Newton-Cotes Numerical Integration
* by Marc Christian Saribay and Jimson Serrano (S-3L)
* Date Compiled: September 23, 2003

      REAL X(50),Y(50),H
      INTEGER N,M,M2
      PRINT*,'*******************************************************'
      PRINT*,'* PROJECT 8: Newton-Cotes Numerical Integration       *'
      PRINT*,'* by Marc Christian Saribay and Jimson Serrano (S-3L) *'
      PRINT*,'*******************************************************'
1     PRINT*
      PRINT*,'Enter the number of data points'
      READ*,N
      PRINT*
      DO I=1,N
        PRINT*,'Enter x',I
        READ*,X(I)
      ENDDO
      H=(X(N)-X(1))/(N-1)
      DO I=1,N
        PRINT*,'Enter f(x)',I
        READ*,Y(I)
      ENDDO
2     PRINT*
      PRINT*,'What method will you use?'
      PRINT*,'1 - Trapezoidal Rule'
      PRINT*,'2 - Simpsons 1/3 Rule'
      PRINT*,'3 - Simpsons 3/8 Rule'
      READ*,M
      IF(M.EQ.1)THEN
        IF(N.GE.2)THEN
          CALL TR(Y,N,H)
        ELSE
          PRINT*,'Please enter at least 2 points'
          GOTO 1
        ENDIF
      ELSEIF(M.EQ.2)THEN
        IF(N.GE.3)THEN
          CALL S13R(Y,N,H)
        ELSE
          PRINT*,'Please enter at least 3 points'
          GOTO 1
        ENDIF

      ELSEIF(M.EQ.3)THEN
        IF(N.GE.4)THEN
          CALL S38R(Y,N,H)
        ELSE
          PRINT*,'Please enter at least 4 points'
          GOTO 1
        ENDIF
      ELSE
        PRINT*,'Choose from 1 to 3 only'
        GOTO 2
      ENDIF
3     PRINT*,'****************************************************'
      PRINT*,'* 1 - Use another method using the previous values *'
      PRINT*,'* 2 - Enter another set of data points             *'
      PRINT*,'* 3 - Quit Program                                 *'
      PRINT*,'****************************************************'
      READ*,M2
      IF(M2.EQ.1)THEN
        GOTO 2
      ELSEIF(M2.EQ.2)THEN
        GOTO 1
      ELSEIF(M2.EQ.3)THEN
        GOTO 4
      ELSE
        PRINT*,'Choose from 1 to 3 only'
        GOTO 3
      ENDIF
4     END

      SUBROUTINE TR(Y,N,H)
      REAL Y(50),H,P1,ANS
      INTEGER N
      P1=0
      DO I=2,N-1
        P1=P1+Y(I)
      ENDDO
      ANS=(H/2)*(Y(1)+(2*P1)+Y(N))
      PRINT*
      PRINT*,'The answer is'
      PRINT*,ANS
      PRINT*
      RETURN
      END

      SUBROUTINE S13R(Y,N,H)
      REAL Y(50),H,P1,P2,ANS
      INTEGER N,SW,OK
      DO I=3,50,2
        IF(N.EQ.I)THEN
          OK=1
          GOTO 10
        ELSE
          OK=0
        ENDIF
      ENDDO
10    IF(OK.EQ.1)THEN
        P1=0
        P2=0
        SW=0
        DO I=2,N-1
          IF(SW.EQ.0)THEN
            P1=P1+Y(I)
            SW=1
          ELSE
            P2=P2+Y(I)
            SW=0
          ENDIF
        ENDDO
        ANS=(H/3)*(Y(1)+(4*P1)+(2*P2)+Y(N))
        PRINT*
        PRINT*,'The answer is'
        PRINT*,ANS
        PRINT*
      ELSE
        PRINT*,'Method not applicable'
      ENDIF
      RETURN
      END

      SUBROUTINE S38R(Y,N,H)
      REAL Y(50),H,P1,P2,P3,ANS
      INTEGER N,SW,OK
      DO I=4,50,3
        IF(N.EQ.I)THEN
          OK=1
          GOTO 10
        ELSE
          OK=0
        ENDIF
      ENDDO
10    IF(OK.EQ.1)THEN
        P1=0
        P2=0
        P3=0
        SW=0
        DO I=2,N-1
          IF(SW.EQ.0)THEN
            P1=P1+Y(I)
            SW=1
          ELSEIF(SW.EQ.1)THEN
            P2=P2+Y(I)
            SW=2
          ELSE
            P3=P3+Y(I)
            SW=0
          ENDIF
        ENDDO
        ANS=((3*H)/8)*(Y(1)+(3*P1)+(3*P2)+(2*P3)+Y(N))
        PRINT*
        PRINT*,'The answer is'
        PRINT*,ANS
        PRINT*
      ELSE
        PRINT*,'Method not applicable'
      ENDIF
      RETURN
      END



