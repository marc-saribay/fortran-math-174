PROJECT GAUSSJ
*Gauss Jordan Reduction Method
*(c)2003 by Marc Saribay (S-3L)
*Compile Date: July 29, 2003

      INTEGER R,C,PLOC,OK,PR,PC
      REAL M(50,50),TP

      PRINT*,'PROJECT GAUSSJ (Gauss Jordan Reduction Method)'
      PRINT*,'(c)2003 by Marc Saribay (S-3L)'

      PRINT*,'Enter number of rows'
      READ*,R
      PRINT*,'Enter number of columns'
      READ*,C
      DO I=1,R
        DO J=1,C
          PRINT*,'Enter ',I,',',J
          READ*,M(I,J)
        ENDDO
      ENDDO
      DO I=1,R
        DO J=1,C
          PRINT*,M(I,J)
        ENDDO
      ENDDO
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
      DO I=1,R
        DO J=1,C
          PRINT*,M(I,J)
        ENDDO
      ENDDO
      END
