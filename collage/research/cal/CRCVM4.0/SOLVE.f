C==== SOLVE THE EIGENVALUE PROBLEM =====================================

      SUBROUTINE SOLVE_SYSTEM(IDET,DEG)

C-----------------------------------------------------------------------
      INCLUDE 'DIM_INC.f'
      INCLUDE 'GEM_INC.f'
      DIMENSION ZEIGE(NTOT)
      DIMENSION DD(NTOT)
      DO I=1,NOMAX_TMP
      DO J=1,NOMAX_TMP
        DN(I,J)=0.D0
        ZH(I,J)=(0.D0,0.D0)
      ENDDO
      ENDDO

      CALL SET_NORM
      CALL SET_KINE
      CALL SET_CENT
      CALL SET_CENP
      CALL SET_SPOR
      CALL SET_SPOP
      CALL SET_TENS
      CALL SET_COUL
      IF(IS_TB.EQ.1.OR.IS_TB.EQ.2) CALL SET_THRE
      IF(IS_PF.EQ.1) CALL SET_PSEU
      DO I=1,NOMAX_TMP
        IF(DABS(DN(I,I)).LT.1.D-14) GOTO 9999
        DD(I)=1.D0/DSQRT(DN(I,I))
      ENDDO
      DO I=1,NOMAX_TMP
      DO J=1,I
        DN(I,J)=DN(I,J)*DD(I)*DD(J)
        DN(J,I)=DN(I,J)
        ZH(I,J)=ZH(I,J)*DD(I)*DD(J)
        ZH(J,I)=ZH(I,J)
      ENDDO
      ENDDO

      CALL EIGENH(ZEIGE,DEG,NZ,NOMAX_TMP)
      RETURN
9999  WRITE(NOUT,*) 'NORM IS TOO SMALL!!'
      STOP
      END
C=======================================================================
