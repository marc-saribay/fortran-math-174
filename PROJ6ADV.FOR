PROJECT SIX
*Hermite Polynomial Interpolation
*(c)2003 by Marc Saribay (S-3L)
*Compile Date: August 9, 2003

c     Declaration of variables
      INTEGER N,I,J,K,L,M,ULIT,CTR
      REAL X(50),Y(50),T(50),TD(50),X2,FP(50)

c     Main Program
      PRINT*,'PROJECT 3 (Hermite Polynomial Interpolation)'
      PRINT*,'(c)2003 by Marc Saribay (S-3L)'
      PRINT*
10    PRINT*,'Enter the number of data points'
      READ*,N
      IF(N.LT.2)THEN
        PRINT*,'Please enter a value greater than 1'
        GOTO 10
      ENDIF
      DO I=1,N
        PRINT*
        PRINT*,'Point',I
        PRINT*,'Enter x'
        READ*,X((2*I)-1)
        X(2*I)=X((2*I)-1)
        PRINT*,'Enter f(x)'
        READ*,Y((2*I)-1)
        T((2*I)-1)=Y((2*I)-1)
        Y(2*I)=Y((2*I)-1)
        T(2*I)=Y((2*I)-1)
        PRINT*,'Enter f`(x)'
        READ*,FP(I)
      ENDDO
c      DO I=1,(2*N)
c        PRINT*,X(I),Y(I),FP(I)
c      ENDDO
      K=1
      L=1
      DO I=((2*N)-1),1,-1
        L=K+1
        M=1
        PRINT*,'LEVEL',K
        IF(K.EQ.1)THEN
          CTR=1
          DO J=1,I
            IF(RM(J,2).EQ.0)THEN
              PRINT*,(T(J+1)-T(J))/(X(L)-X(M))
              T(J)=(T(J+1)-T(J))/(X(L)-X(M))
            ELSE
              PRINT*,FP(CTR)
              IF(J.EQ.1)THEN
                TD(K)=FP(CTR)
                T(J)=FP(CTR)
                CTR=CTR+1
              ELSE
                T(J)=FP(CTR)
                CTR=CTR+1
              ENDIF
            ENDIF
            L=L+1
            M=M+1
          ENDDO
          K=K+1
          PRINT*
          PRINT*,'Press Enter to continue'
          READ*
        ELSE
          DO J=1,I
            PRINT*,(T(J+1)-T(J))/(X(L)-X(M))
            IF(J.EQ.1)THEN
              TD(K)=(T(J+1)-T(J))/(X(L)-X(M))
            ENDIF
            T(J)=(T(J+1)-T(J))/(X(L)-X(M))
            L=L+1
            M=M+1
          ENDDO
          K=K+1
          PRINT*
          PRINT*,'Press Enter to continue'
          READ*
        ENDIF
      ENDDO
20    PRINT*,'Enter X for approximation'
      READ*,X2
      IF((X2.LT.X(1)).OR.(X2.GT.X(2*N)))THEN
        PRINT*,'Out of Bounds'
        GOTO 20
      ELSE
        CALL NDDF(N,X,Y,TD,X2)
        PRINT*
        PRINT*,'Do you want to approximate another value? (1-Yes, 2-No)'
        READ*,ULIT
        IF(ULIT.EQ.1)THEN
          GOTO 20
        ELSE
          PRINT*,'Thanks for using my program'
        ENDIF
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
      IF((ORDER.GE.1).AND.(ORDER.LT.(2*N0)))THEN
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

c     Remainder Function
      FUNCTION RM(I,J)
        INTEGER I,J
        IF((I.EQ.1).AND.(J.EQ.2))THEN
          RM=1
        ELSE
          RM=i-(j*int(i/j))
        ENDIF
      RETURN
      END
