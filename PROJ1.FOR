PROJECT ONE
*Forward and Backward Difference Table
*(c)2003 by Marc Saribay and Jimson Serrano (S-3L)
*Compile Date: June 24, 2003

c     Declaration of variables
      INTEGER N,I,J,K
      REAL X(50),Y(50),T(50)

c     Main Program
      PRINT*,'PROJECT 1 (Forward and Backward Difference Table)'
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
        DO J=1,I
          PRINT*,T(J+1)-T(J)
          T(J)=T(J+1)-T(J)
        ENDDO
        K=K+1
        PRINT*
        PRINT*,'Press Enter to continue'
        READ*
      ENDDO
      PRINT*,'Thank you for using our program!'
      END  
