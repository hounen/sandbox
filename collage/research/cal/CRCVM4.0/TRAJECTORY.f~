C==== TRAJECTORY =======================================================

      SUBROUTINE TRAJECT

C-----------------------------------------------------------------------
      INCLUDE 'DIM_INC.f'
      INCLUDE 'GEM_INC.f'
C
C WRITE TRAJECTORY
C
      IF(DEGFI-DEGII.LT.0.D0) DDEGI=-DDEGI
      IF(BBFI-BBII.LT.0.D0)   DBBI=-DBBI
      NDEGI=IDINT((DEGFI-DEGII)/DDEGI+0.01D0)+1
      NBBI=IDINT((BBFI-BBII)/DBBI+0.01D0)+1
      IF(DEGFF-DEGIF.LT.0.D0) DDEGF=-DDEGF
      IF(BBFF-BBIF.LT.0.D0)   DBBF=-DBBF
      NDEGF=IDINT((DEGFF-DEGIF)/DDEGF+0.01D0)+1
      NBBF=IDINT((BBFF-BBIF)/DBBF+0.01D0)+1
      WRITE(NOUT,FMT=101)
      WRITE(NOUT,FMT=103) NDEGI,DEGII,DEGFI
      WRITE(NOUT,FMT=104) NBBI,BBII,BBFI
      IF(ITRANS.NE.0) THEN
        WRITE(NOUT,FMT=102)
        WRITE(NOUT,FMT=103) NDEGF,DEGIF,DEGFF
        WRITE(NOUT,FMT=104) NBBF,BBIF,BBFF
      ENDIF
      WRITE(NOUT,FMT=100)
C
C TRAJECTORY FOR INITIAL STATE
C
      NOTR_I=0
      DO IDEGI=1,NDEGI
        DEG_I=DEGII+DDEGI*(IDEGI-1)
      DO IBBI=1,NBBI
        BB_I=BBII+DBBI*(IBBI-1)
        NOTR_I=NOTR_I+1
        WRITE(NOUT,FMT=105) NOTR_I
        WRITE(NENE,FMT=105) NOTR_I
        WRITE(NWAV,FMT=105) NOTR_I
        WRITE(NSCT,FMT=105) NOTR_I
        WRITE(NOUT,FMT=107) DEG_I,BB_I
        WRITE(NENE,FMT=107) DEG_I,BB_I
        WRITE(NWAV,FMT=107) DEG_I,BB_I
        WRITE(NSCT,FMT=107) DEG_I,BB_I

        CALL SET_BASIS(1,DEG_I,BB_I)
        CALL REARRANGE(1)

        WRITE(NOUT,FMT=109)
        DO NO=1,NOCHI
          WRITE(NOUT,FMT=110) NO,IBI(NO),LSI(NO),SSI(NO),TSI(NO),
     &                        LLI(NO),LTI(NO),STI(NO),
     &                        N1I(NO),BB_I*B1I(NO),G1I(NO),
     &                        N2I(NO),BB_I*B2I(NO),G2I(NO)
        ENDDO

        CALL SOLVE_SYSTEM(1,DEG_I)
C
C TRAJECTORY FOR FINAL STATE
C
      IF(ITRANS.NE.0) THEN
      NOTR_F=0
      DO IDEGF=1,NDEGF
        DEG_F=DEGIF+DDEGF*(IDEGF-1)
      DO IBBF=1,NBBF
        BB_F=BBIF+DBBF*(IBBF-1)
        NOTR_F=NOTR_F+1
        WRITE(NOUT,FMT=106) NOTR_F
        WRITE(NENE,FMT=106) NOTR_F
        WRITE(NWAV,FMT=106) NOTR_F
        WRITE(NSCT,FMT=106) NOTR_F
        WRITE(NOUT,FMT=108) DEG_F,BB_F
        WRITE(NENE,FMT=108) DEG_F,BB_F
        WRITE(NWAV,FMT=108) DEG_F,BB_F
        WRITE(NSCT,FMT=108) DEG_F,BB_F

        CALL SET_BASIS(2,DEG_F,BB_F)
        CALL REARRANGE(2)
        
        WRITE(NOUT,FMT=109)
        DO NO=1,NOCHF
          WRITE(NOUT,FMT=110) NO,IBF(NO),LSF(NO),SSF(NO),TSF(NO),
     &                        LLF(NO),LTF(NO),STF(NO),
     &                        N1F(NO),BB_F*B1F(NO),G1F(NO),
     &                        N2F(NO),BB_F*B2F(NO),G2F(NO)
        ENDDO

        CALL SOLVE_SYSTEM(2,DEG_F)
      ENDDO
      ENDDO
      ENDIF
      ENDDO
      ENDDO
      RETURN
C
C FORMAT
C
 100  FORMAT(
     &  '#------------------------------------------------------------',
     &  '------------------')
 101  FORMAT('# FOR INITIAL STATE')
 102  FORMAT('#',/'# FOR FINAL STATE')
 103  FORMAT('# THETA-TRAJECTORY  : ',I3,' [ ',F7.4,' - ',F7.4,' ]')
 104  FORMAT('# B-TRAJECTORY      : ',I3,' [ ',F7.4,' - ',F7.4,' ]')
 105  FORMAT(/'# TRAJECTORY NO. (INI): ',I3)
 106  FORMAT(/'# TRAJECTORY NO. (FIN): ',I3)
 107  FORMAT('# FOR INITIAL STATE    ',
     &       ' [ THETA=',F7.3,'[deg.], BI/B0=',F7.3,'[fm] ]')
 108  FORMAT('# FOR FINAL STATE      ',
     &       ' [ THETA=',F7.3,'[deg.], BI/B0=',F7.3,'[fm] ]')
 109  FORMAT('# NO  YI  LAM  S12  T12   L  LTOT STOT ',
     &       '[BASE   B0   GAMMA] [BASE   B0   GAMMA]')
 110  FORMAT('#',I3,2X,'Y',I1,2X,I2,2X,F4.1,1X,F4.1,2X,I2,2X,I2,
     &       2X,F4.1,3X,I3,1X,F6.3,1X,F6.3,3X,I3,1X,F6.3,1X,F6.3)
      END
C=======================================================================
