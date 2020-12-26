PROJECT FOUR-B
*Lagrange Interpolation with Order
*(c)2003 by Marc Saribay and Jimson Serrano (S-3L)
*Compile Date: July 15, 2003

c     Declaration of variables
      INTEGER N,I
      REAL X(50),Y(50),XA,XT,YT

c     Main Program
      PRINT*,'PROJECT 4B (Lagrange Interpolation with Order)'
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
      ENDDO
c     Sorting
      DO I=1,N-1
        DO J=I,N-1
          IF(X(I).GT.X(J+1))THEN
            XT=X(I)
            YT=Y(I)
            X(I)=X(J+1)
            Y(I)=Y(J+1)
            X(J+1)=XT
            Y(J+1)=YT
          ENDIF
        ENDDO
      ENDDO
      PRINT*,'Enter X for approximation'
      READ*,XA
      CALL LAGRANGE(N,XA,X,Y)
      END  

ccc   Subroutines
      SUBROUTINE LAGRANGE(N0,XA0,X0,Y0)
      INTEGER N0,ORDER
      REAL X0(50),Y0(50),L(50),PART,APPROX,XA0
30    PRINT*,'Enter order'
      READ*,ORDER
      APPROX=0
      IF((ORDER.GE.1).AND.(ORDER.LT.N0))THEN
        DO I=1,N0
          PART=1
          DO J=1,N0
            IF(J.NE.I)THEN
              PART=PART*((XA0-X0(J))/(X0(I)-X0(J)))
            ENDIF
          ENDDO
          L(I)=PART
        ENDDO
        DO I=1,ORDER+1
          APPROX=APPROX+(Y0(I)*L(I))
        ENDDO
        PRINT*,'F(X) = ', APPROX
      ELSE
        PRINT*,'Invalid Entry'
        GOTO 30
      ENDIF
      RETURN
      END
