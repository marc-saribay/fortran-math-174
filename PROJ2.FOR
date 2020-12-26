PROJECT TWO
*Forward and Backward Difference Approximation
*(c)2003 by Marc Saribay and Jimson Serrano (S-3L)
*Compile Date: July 1, 2003

c     Declaration of variables
      INTEGER N,I,J,K,METHOD
      REAL X(50),Y(50),T(50),TF(50),TB(50)
      REAL X2,SF,SB

c     Main Program
      PRINT*,'PROJECT 2 (NFDF/NBDF Approximation)'
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
c        PRINT*,'LEVEL',K
        DO J=1,I
c          PRINT*,T(J+1)-T(J)
          IF(J.EQ.1)THEN
            TF(K)=T(J+1)-T(J)
            IF(I.EQ.1)THEN
              TB(K)=T(J+1)-T(J)
            ENDIF
          ELSEIF(J.EQ.I)THEN
            TB(K)=T(J+1)-T(J)
          ENDIF
          T(J)=T(J+1)-T(J)
        ENDDO
        K=K+1
c        PRINT*
c        PRINT*,'Press Enter to continue'
c        READ*
      ENDDO
      PRINT*,'Enter X for approximation'
      READ*,X2
      SF=(X2-X(1))/(X(2)-X(1))
      SB=(X2-X(N))/(X(2)-X(1))
20    PRINT*,'Select Method (1 for NFDF, 2 for NBDF)'
      READ*,METHOD
      IF(METHOD.EQ.1)THEN
        CALL NFDF(N,SF,Y,TF)
      ELSEIF(METHOD.EQ.2)THEN
        CALL NBDF(N,SB,Y,TB)
      ELSE
        PRINT*,'Invalid Entry'
        GOTO 20
      ENDIF
      END  

cc    Factorial Function
      FUNCTION FACTORIAL(Z)
      INTEGER Z,L
      FACTORIAL=1
      IF(Z.EQ.0)THEN
        FACTORIAL=1
      ELSEIF(Z.LT.0)THEN
      DO L=2,Z,-1
        FACTORIAL=FACTORIAL*L
      ENDDO
      ELSE
        DO L=2,Z
          FACTORIAL=FACTORIAL*L
        ENDDO
      ENDIF
      RETURN
      END

ccc   Subroutines
      SUBROUTINE NFDF(N0,S0,Y0,TF0)
      INTEGER N0,ORDER
      REAL S0,Y0(50),TF0(50)
      REAL SPART,ADTL,APPROX
30    PRINT*,'Enter order'
      READ*,ORDER
      APPROX=Y0(1)+S0*TF0(1)
      IF(ORDER.EQ.1)THEN
        PRINT*,'F(X) = ', APPROX
      ELSEIF((ORDER.GT.1).AND.(ORDER.LT.N0))THEN
        DO I=2,ORDER
          SPART=1
          DO J=1,I-1
            SPART=SPART*(S0-J)
          ENDDO
          ADTL=ADTL+TF0(I)*((S0*SPART)/FACTORIAL(I))
        ENDDO
        APPROX=APPROX+ADTL
        PRINT*,'F(X) = ', APPROX
      ELSE
        PRINT*,'Invalid Entry'
        GOTO 30
      ENDIF
      RETURN
      END

      SUBROUTINE NBDF(N0,S0,Y0,TB0)
      INTEGER N0,ORDER
      REAL S0,Y0(50),TB0(50)
      REAL SPART,ADTL,APPROX
40    PRINT*,'Enter order'
      READ*,ORDER
      APPROX=Y0(N0)+S0*TB0(1)
      IF(ORDER.EQ.1)THEN
        PRINT*,'F(X) = ', APPROX
      ELSEIF((ORDER.GT.1).AND.(ORDER.LT.N0))THEN
        DO I=2,N0-1
          SPART=1
          DO J=1,I-1
            SPART=SPART*(S0+J)
          ENDDO
          ADTL=ADTL+TB0(I)*((S0*SPART)/FACTORIAL(I))
        ENDDO
        APPROX=APPROX+ADTL
        PRINT*,'F(X) = ', APPROX
      ELSE
        PRINT*,'Invalid Entry'
        GOTO 40
      ENDIF
      RETURN
      END
