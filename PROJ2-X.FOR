PROJECT TWO
*Forward and Backward Difference Table
*(c)2003 by Marc Saribay and Jimson Serrano (S-3L)
*Compile Date: July 1, 2003

c     Declaration of variables
      INTEGER N,I,J,K,METHOD
      REAL X(50),Y(50),T(50),S0,X0,T2A(50),T2B(50),TEMPF,TEMPB,NF0,NB0

c     Main Program
      PRINT*,'PROJECT 2 (Forward and Backward Difference Table)'
      PRINT*,'(c)2003 by Marc Saribay and Jimson Serrano (S-3L)'
10    PRINT*,'Enter the number of data points'
      READ*,N
      IF(N.LT.2)THEN
        PRINT*,'Please enter a value greater than 1'
        GOTO 10
      ENDIF
      DO I=1,N
        PRINT*,'Enter point ',I
        READ*,X(I),Y(I)
c       PRINT*,X(I),Y(I)
        T(I)=Y(I)
      ENDDO
      K=1
      DO I=N-1,1,-1
        PRINT*,'LEVEL',K
        TEMPF=T(2)-T(1)
        TEMPB=-1*(T(I)-T(I+1))
        DO J=1,I
          PRINT*,T(J+1)-T(J)
          T(J)=T(J+1)-T(J)
        ENDDO
        T2A(K)=TEMPF
        T2B(K)=TEMPB
        K=K+1
        PRINT*
        PRINT*,'Press enter to continue'
        READ*
      ENDDO
      DO I=1,N-1
        PRINT*,T2A(I)
        PRINT*,T2B(I)
      ENDDO
      PRINT*,'Enter value to be interpolated'
      READ*,X0
20    PRINT*,'Select Method (Enter 1 for NFDF, 2 for NBDF)'
      READ*,METHOD
      IF(METHOD.EQ.1)THEN
        S0=(X0-X(1))/(X(2)-X(1))
        CALL NFDF(S0,X0,NF0,Y,T2A)
      ELSEIF(METHOD.EQ.2)THEN
        S0=(X0+X(N))/(X(2)-X(1))
        CALL NBDF(S0,X0,NB0,Y,T2B,N)
      ELSE
        PRINT*,'Invalid Input'
        GOTO 20
      ENDIF
      PRINT*,'Thank you for using our program!'
      END  

cc    Subroutines

      SUBROUTINE NFDF(S,X,NF,YX,T2AX)
      REAL S,X,NF,YX(50),T2AX(50)
      NF=YX(1)
      DO I=1,N-1
        NF=NF+T2AX(I)*(FACTORIAL(S)/(FACTORIAL(S-I)*FACTORIAL(I)))
      ENDDO
      PRINT*,NF
      RETURN
      END

      SUBROUTINE NBDF(S,X,NB,YX,T2BX,NX)
      REAL S,X,NB,YX(50),T2BX(50)
      INTEGER NX
      NB=YX(NX)
      DO I=1,N-1
        NB=NB+T2BX(I)*(FACTORIAL(S)/(FACTORIAL(S-I)*FACTORIAL(I)))
      ENDDO
      PRINT*,NB
      RETURN
      END

cc    Function Subprogram

      FUNCTION FACTORIAL(X)
      REAL X
      INTEGER L
      FACTORIAL=1
      IF(X.EQ.0)THEN
        FACTORIAL=1
      ELSEIF(X.LT.0)THEN
      DO L=2,X,-1
        FACTORIAL=FACTORIAL*L
      ENDDO
      ELSE
        DO L=2,X
          FACTORIAL=FACTORIAL*L
        ENDDO
      ENDIF
      RETURN
      END
