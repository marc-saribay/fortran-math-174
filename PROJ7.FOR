PROJECT SEVEN
*Numerical Differentiation
*(c)2003 by Marc Saribay and Jimson Serrano (S-3L)
*Compile Date: September 16, 2003

c     Declaration of variables
      INTEGER N,I,J,K,L,M,ULIT
      REAL X(50),Y(50),T(50),TD(50),X2,TX(50)

c     Main Program
      PRINT*,'PROJECT 7 (Numerical Differentiation)'
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
            TD(J)=(T(J+1)-T(J))/(X(L)-X(M))
            TX(K)=TD(J)
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
c      DO I=1,N-1
c        PRINT*,'T',TX(I)
c      ENDDO
20    PRINT*,'Enter X for approximation'
      READ*,X2
      CALL NDDF(N,X,TX,X2)
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
      SUBROUTINE NDDF(N0,X0,TD0,X20)
      INTEGER N0,ORDER
      REAL TD0(50),X20,X0(50)
      REAL PART,PART2,APPROX,ADTL
30    PRINT*,'Enter order'
      READ*,ORDER
      PART=1
      PART2=0
      PART3=1
      APPROX=0
      IF((ORDER.GE.1).AND.(ORDER.LT.N0))THEN
        DO K=2,ORDER+1
          DO I=1,K
c          ORDER+2
            DO J=1,K-1
c            ORDER+1
              IF(J.NE.I)THEN
                PART=PART*(X20-X0(J))
              ENDIF
            ENDDO
            PART2=PART2+PART
          ENDDO
          ADTL=ADTL+(TD0(K)*PART2)
        ENDDO
c        PRINT*,'T2',TD0(1)
        APPROX=ADTL+TD0(1)
        PRINT*,'The approximated value is ', APPROX
      ELSE
        PRINT*,'Invalid Entry'
        GOTO 30
      ENDIF
      RETURN
      END
