PROJECT FOUR
*Lagrange Interpolation
*(c)2003 by Marc Saribay (S-3L)
*Compile Date: July 12, 2003

c     Declaration of variables
      INTEGER N,I
      REAL X(50),Y(50),XA

c     Main Program
      PRINT*,'PROJECT 4 (Lagrange Interpolation)'
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
        PRINT*,'Enter point ',I
        READ*,X(I),Y(I)
c       PRINT*,X(I),Y(I)
      ENDDO
      PRINT*,'Enter X for approximation'
      READ*,XA
      CALL LAGRANGE(N,XA,X,Y)
      END  

ccc   Subroutines
      SUBROUTINE LAGRANGE(N0,XA0,X0,Y0)
      INTEGER N0
      REAL X0(50),Y0(50),L(50),PART,APPROX,XA0
      APPROX=0
      DO I=1,N0
        PART=1
        DO J=1,N0
          IF(J.NE.I)THEN
            PART=PART*((XA0-X0(J))/(X0(I)-X0(J)))
          ENDIF
        ENDDO
        L(I)=PART
      ENDDO
      DO I=1,N0
        APPROX=APPROX+(Y0(I)*L(I))
      ENDDO
      PRINT*,'F(X) = ', APPROX
      RETURN
      END
