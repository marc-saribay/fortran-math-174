PROJECT THREE
*Divided Difference Table and Approximation
*(c)2003 by Marc Saribay and Jimson Serrano (S-3L)
*Compile Date: July 8, 2003

c     Declaration of variables
      INTEGER N,I,J,K,L,M,ULIT
      REAL X(50),Y(50),T(50),TD(50),X2

c     Main Program
      PRINT*,'PROJECT 3 (NDDF Table and Approximation)'
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
      L=1
      DO I=N-1,1,-1
        L=K+1
        M=1
c       PRINT*,'LEVEL',K
        DO J=1,I
c         PRINT*,(T(J+1)-T(J))/(X(L)-X(M))
          IF(J.EQ.1)THEN
            TD(K)=(T(J+1)-T(J))/(X(L)-X(M))
          ENDIF
          T(J)=(T(J+1)-T(J))/(X(L)-X(M))
          L=L+1
          M=M+1
        ENDDO
        K=K+1
c       PRINT*
c       PRINT*,'Press Enter to continue'
c       READ*
      ENDDO
20    PRINT*,'Enter X for approximation'
      READ*,X2
      CALL NDDF(N,X,Y,TD,X2)
      PRINT*
      PRINT*,'Do you want to approximate another value? (1-Yes, 2-No)'
      READ*,ULIT
      IF(ULIT.EQ.1)THEN
        GOTO 20
      ELSE
        PRINT*,'Thanks for using our program'
      ENDIF
      END  

ccc   Subroutine
      SUBROUTINE NDDF(N0,X0,Y0,TD0,X20)
      INTEGER N0,ORDER
      REAL Y0(50),TD0(50),X20,X0(50)
      REAL PART,ADTL,APPROX
30    PRINT*,'Enter order'
      READ*,ORDER
      APPROX=Y0(1)
      ADTL=0
      IF((ORDER.GE.1).AND.(ORDER.LT.N0))THEN
        DO I=1,ORDER
          PART=1
          DO J=1,I
            PART=PART*(X20-X0(J))
          ENDDO
          ADTL=ADTL+(TD0(I)*PART)
        ENDDO
        APPROX=APPROX+ADTL
        PRINT*,'The approximated value is ', APPROX
      ELSE
        PRINT*,'Invalid Entry'
        GOTO 30
      ENDIF
      RETURN
      END

