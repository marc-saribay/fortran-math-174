PROJECT FIVE
*Cubic Spline Interpolation
*(c)2003 by Marc Saribay and Jimson Serrano (S-3L)
*Compile Date: July 29, 2003

c     Declaration of variables
      INTEGER NU
      REAL X(50),Y(50),A(50),H(50),F(50),XA

c     Main Program
      PRINT*,'PROJECT 5 (Cubic Spline Interpolation)'
      PRINT*,'(c)2003 by Marc Saribay and Jimson Serrano (S-3L)'
 10    PRINT*,'Enter the number of data points'
      READ*,N
      IF(N.LT.2)THEN
        PRINT*,'Please enter a value greater than 1'
        GOTO 10
      ENDIF
      DO I=1,NU
        PRINT*,'Enter X'
        READ*,X(I)
        PRINT*,'Enter Y'
        READ*,Y(I)
      ENDDO
      DO I=1,NU-1
        A(I)=Y(I)
        H(I)=X(I+1)-X(I)
      ENDDO
      DO I=2,NU-1
        F(I-1)=3*(((Y(I+1)-Y(I))/H(I))-((Y(I)-Y(I-1))/H(I-1)))
      ENDDO
      PRINT*,'Enter value to be interpolated'
      READ*,XA
      CALL GAUSSJ(NU,F,H,A,XA,X,Y)
      END

ccc   Subroutines
      SUBROUTINE GAUSSJ(N0,E0,H0,A0,XA0,X0,Y0)

      INTEGER R,C,PLOC,OK,PR,PC,N0
      REAL M(50,50),TP,E0(50),C0(50),D(50),B(50),H0(50),A0(50),S(50)
      REAL X0(50),XA0,Y0(50)

      C=N0-1
      R=N0-2
      C0(1)=0
      C0(N0)=0
      DO I=2,N0-1
        M(I-1,I-1)=2*(H0(I-1)+H0(I))
        M(I,I-1)=H0(I)
        M(I-1,I)=H0(I-1)
      ENDDO
      DO I=1,R
        M(I,C)=E0(I)
      ENDDO
c      DO I=1,R
c        DO J=1,C
c          PRINT*,'Enter ',I,',',J
c          READ*,M(I,J)
c        ENDDO
c      ENDDO
c      DO I=1,R
c        DO J=1,C
c          PRINT*,M(I,J)
c        ENDDO
c      ENDDO
      DO J=1,C
        PLOC=0
        DO I=1,R
          IF(PLOC.EQ.0)THEN
            IF(M(I,J).NE.0)THEN
              IF(J.GT.1)THEN
                OK=0
                DO E=1,J-1
                  IF(M(I,E).EQ.0)THEN
                    OK=OK+0
                  ELSE
                    OK=OK+1
                  ENDIF
                ENDDO
              ENDIF
              IF((OK.EQ.0).OR.(J.EQ.1))THEN
                P=M(I,J)
                PR=I
                PC=J
                PLOC=1
              ENDIF
            ENDIF
            IF(PLOC.EQ.1)THEN
              DO K=PC,C
                M(PR,K)=M(PR,K)/P
              ENDDO
              DO L=1,R
                IF(L.NE.PR)THEN
                  IF(M(L,PC).NE.0)THEN
                    TP=-M(L,PC)
                    DO N=PC,C
                      M(L,N)=(TP*M(PR,N))+M(L,N)
                    ENDDO
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
          ENDIF
        ENDDO
      ENDDO
c      DO I=1,R
c        DO J=1,C
c          PRINT*,M(I,J)
c        ENDDO
c      ENDDO
      DO I=1,N0-1
        B(I)=((Y0(I+1)-Y0(I))/H0(I))-(H0(I)/3)*(2*C0(I)+C0(I+1))
        D(I)=(C0(I+1)-C0(I))/(3*H0(I))
      ENDDO
      DO I=1,N0-1
        S(I)=A0(I)+B(I)*(XA0-X0(I))+C0(I)
        S(I)=S(I)*((XA0-X0(I))**2)+D(I)*(XA0-X0(I))**3
      ENDDO
cc      PRINT*,S(I)
      RETURN
      END
