C==== SET THREE-BODY FORCE MATRIX ======================================

      SUBROUTINE SET_THRE

C-----------------------------------------------------------------------
      INCLUDE 'DIM_INC.f'
      INCLUDE 'GEM_INC.f'
      DIMENSION PH(3)
      NO_EQ=0
      NO_ALPH=0
      DO I=1,2
      DO J=I+1,3
        IF(INF_NN(I).EQ.INF_NN(J)) NO_EQ=NO_EQ+1
      ENDDO
      ENDDO
      DO I=1,3
        IF(INF_NN(I).EQ.0) NO_ALPH=NO_ALPH+1
      ENDDO
      IF(NO_ALPH.EQ.3) THEN
        NANO=0
      DO 1000 IBOXA=1,NOCH_TMP
        NOA=IB_TMP(IBOXA)
        LL1=LS_TMP(IBOXA)
        LL2=LL_TMP(IBOXA)
        LT12=LT_TMP(IBOXA)
        ST12=ST_TMP(IBOXA)
        NFAMAX=N1_TMP(IBOXA)*N2_TMP(IBOXA)
        NBNO=0
      DO 1001 IBOXB=1,IBOXA
        NOB=IB_TMP(IBOXB)
        LL3=LS_TMP(IBOXB)
        LL4=LL_TMP(IBOXB)
        LT34=LT_TMP(IBOXB)
        ST34=ST_TMP(IBOXB)
        NFBMAX=N1_TMP(IBOXB)*N2_TMP(IBOXB)
        IF(NFAMAX*NFBMAX.EQ.0) THEN
          NBNO=NBNO+NFBMAX
          GOTO 1001
        ENDIF
        IF(LT12.NE.LT34.OR.ST12.NE.ST34) THEN
          NBNO=NBNO+NFBMAX
          GOTO 1001
        ENDIF
        LL34=LL3+LL4
        IP=LL1+LL2+LL3+LL4
        IF(IP/2*2.NE.IP) GOTO 9999

        NO=0
        DO NFA=1,NFAMAX
          NFBMX=NFBMAX
          IF(IBOXA.EQ.IBOXB) NFBMX=NFA
          DO NFB=1,NFBMX
            NO=NO+1
            ZNN(NO)=(0.D0,0.D0)
          ENDDO
        ENDDO
        NBMIN=1
        NBMAX=3
C
C SET POTENTIAL
C
        IF(IS_TB.EQ.1) THEN
          V_TB=31.7D0
          R_TB=0.15D0
          IF(JTOT_TMP.EQ.2.D0) V_TB=63.0D0
          IF(JTOT_TMP.EQ.4.D0) V_TB=151.1D0
        ENDIF
      DO 1002 NOB=NBMIN,NBMAX
        FACT3=SPFAC(IBOXA,IBOXB,NOA,NOB)*TPFAC(IBOXA,IBOXB,NOA,NOB)
        IF(FACT3.EQ.0.D0) GOTO 1002
        NO=0
        DO NFA=1,NFAMAX
          NFBMX=NFBMAX
          IF(IBOXA.EQ.IBOXB) NFBMX=NFA
          DO NFB=1,NFBMX
            NO=NO+1
            ZA1=ZA1_TMP(IBOXA,NFA)
            ZA2=ZA2_TMP(IBOXA,NFA)
            ZA3=ZA1_TMP(IBOXB,NFB)
            ZA4=ZA2_TMP(IBOXB,NFB)
            ZGSI1=ZA1
     &           +TCO_11(NOB,NOA)**2.D0*ZA3
     &           +TCO_21(NOB,NOA)**2.D0*ZA4
            ZGSI2=ZA2
     &           +TCO_12(NOB,NOA)**2.D0*ZA3
     &           +TCO_22(NOB,NOA)**2.D0*ZA4
            ZETA =TCO_11(NOB,NOA)*TCO_12(NOB,NOA)*ZA3
     &           +TCO_21(NOB,NOA)*TCO_22(NOB,NOA)*ZA4
            Z_GSI1(NO)=ZGSI1*0.5D0
            Z_GSI2(NO)=ZGSI2*0.5D0
            Z_ETA(NO) =ZETA*(-1.D0)
            DO NOV=1,3
              Z_GSI1(NO)=Z_GSI1(NO)
     &                  +TCO_11(NOV,NOA)**2.D0*R_TB
              Z_GSI2(NO)=Z_GSI2(NO)
     &                  +TCO_12(NOV,NOA)**2.D0*R_TB
              Z_ETA(NO) =Z_ETA(NO)
     &                  -TCO_11(NOV,NOA)*TCO_12(NOV,NOA)*2.D0*R_TB
            ENDDO
            Z_FACT(NO)=ZNF_TMP(IBOXA,NFA)*ZNF_TMP(IBOXB,NFB)*V_TB
          ENDDO
        ENDDO
        NMAX=NO
      DO 1003 ILQ3=0,LL34
      DO 1003 ILQ4=IABS(LT34-ILQ3),LT34+ILQ3
        IP=LL34-ILQ3-ILQ4
        IF(IP.LT.0)          GOTO 1003
        IF(IP/2*2.NE.IP)     GOTO 1003
        LAMMIN=MAX0(IABS(LL1-ILQ3),IABS(LL2-ILQ4))
        LAMMAX=MIN0(    (LL1+ILQ3),    (LL2+ILQ4))
        IF(LAMMIN.GT.LAMMAX) GOTO 1003
        DO 1004 IQ34=ILQ3,LL34-ILQ4,2
C
C REARRANGEMENT COEFFICIENT
C
          AMPD=AMP(IBOXB,NOB,NOA,0,ILQ3,ILQ4,IQ34)*FACT3
          IF(AMPD.EQ.0.D0) GOTO 1004
          DO 1005 LAM=LAMMIN,LAMMAX,2
            N2=LL1+IQ34-LAM
            M2=LL2+LL34-IQ34-LAM
            IF(N2/2*2.NE.N2.OR.N2.LT.0) GOTO 9998
            IF(M2/2*2.NE.M2.OR.M2.LT.0) GOTO 9998
C
C INTEGRATION OF ANGULAR PART
C
            FACT1=AMPD*CWFAC(LL1,LL2,ILQ3,ILQ4,LAM,LT12)
C
C INTEGRATION OF RADIAL PART
C
            INU1=LL1+IQ34
            INU2=LL2+LL34-IQ34
            DO NO=1,NMAX
              ZGSI1=Z_GSI1(NO)
              ZGSI2=Z_GSI2(NO)
              ZETA =Z_ETA(NO)
              ZNN(NO)=ZNN(NO)
     &               +Z_FACT(NO)*FACT1
     &               *ZGMAT(INU1,INU2,ZGSI1,ZGSI2,ZETA,LAM)
            ENDDO
1005      CONTINUE
1004    CONTINUE
1003  CONTINUE
1002  CONTINUE
        NO=0
      DO I=1,NFAMAX
        NFBMX=NFBMAX
        IF(IBOXA.EQ.IBOXB) NFBMX=I
        DO J=1,NFBMX
          NO=NO+1
          II=NANO+I
          JJ=NBNO+J
          ZH(II,JJ)=ZH(II,JJ)+ZNN(NO)
        ENDDO
      ENDDO
        NBNO=NBNO+NFBMAX
1001  CONTINUE
        NANO=NANO+NFAMAX
1000  CONTINUE
      ELSEIF(NO_ALPH.EQ.2) THEN
      ELSEIF(NO_ALPH.EQ.1) THEN
        NANO=0
      DO 3000 IBOXA=1,NOCH_TMP
        NOA=IB_TMP(IBOXA)
        LL1=LS_TMP(IBOXA)
        LL2=LL_TMP(IBOXA)
        LT12=LT_TMP(IBOXA)
        ST12=ST_TMP(IBOXA)
        NFAMAX=N1_TMP(IBOXA)*N2_TMP(IBOXA)
        NBNO=0
      DO 3001 IBOXB=1,IBOXA
        NOB=IB_TMP(IBOXB)
        LL3=LS_TMP(IBOXB)
        LL4=LL_TMP(IBOXB)
        LT34=LT_TMP(IBOXB)
        ST34=ST_TMP(IBOXB)
        SS=SS_TMP(IBOXB)
        TS=TS_TMP(IBOXB)
        NFBMAX=N1_TMP(IBOXB)*N2_TMP(IBOXB)
        IF(NFAMAX*NFBMAX.EQ.0) THEN
          NBNO=NBNO+NFBMAX
          GOTO 3001
        ENDIF
        IF(LT12.NE.LT34.OR.ST12.NE.ST34) THEN
          NBNO=NBNO+NFBMAX
          GOTO 3001
        ENDIF
        LL34=LL3+LL4
        IP=LL1+LL2+LL3+LL4
        IF(IP/2*2.NE.IP) GOTO 9999

        NO=0
        DO NFA=1,NFAMAX
          NFBMX=NFBMAX
          IF(IBOXA.EQ.IBOXB) NFBMX=NFA
          DO NFB=1,NFBMX
            NO=NO+1
            ZNN(NO)=(0.D0,0.D0)
          ENDDO
        ENDDO
        IF(NO_EQ.EQ.0) THEN
          NBMIN=NOB
          NBMAX=NOB
          DO I=1,3
            PH(I)=0.D0
            IF(I.EQ.NOB) PH(I)=1.D0
          ENDDO
        ELSE
          NBMIN=1
          NBMAX=3
          IF(NOB.EQ.1) THEN
            NOJ=2
            NOK=3
          ELSEIF(NOB.EQ.2) THEN
            NOJ=3
            NOK=1
          ELSE
            NOJ=1
            NOK=2
          ENDIF
          IF(INF_POT(NOB)/2*2.EQ.INF_POT(NOB)) THEN
            PH(NOJ)=0.D0
            PH(NOK)=0.D0
            IF(INF_POT(NOB).EQ.0) THEN
              IP=LL3
            ELSE
              IP=LL3+1
     &          +IDINT(2.D0*S_INF(NOJ)+2.D0*S_INF(NOK)-SS-TS+0.1D0)
            ENDIF
            PH(NOB)=1.D0+(-1.D0)**IP
          ELSEIF(INF_POT(NOJ)/2*2.EQ.INF_POT(NOJ)) THEN
            PH(NOB)=1.D0
            PH(NOJ)=0.D0
            IF(INF_POT(NOJ).EQ.0) THEN
              IP=LL3
            ELSE
              IP=LL3+1
     &          +IDINT(2.D0*S_INF(NOJ)+2.D0*S_INF(NOK)-SS-TS+0.1D0)
            ENDIF
            PH(NOK)=(-1.D0)**IP
          ELSE
            PH(NOB)=1.D0
            PH(NOK)=0.D0
            IF(INF_POT(NOK).EQ.0) THEN
              IP=LL3
            ELSE
              IP=LL3+1
     &          +IDINT(2.D0*S_INF(NOJ)+2.D0*S_INF(NOK)-SS-TS+0.1D0)
            ENDIF
            PH(NOJ)=(-1.D0)**IP
          ENDIF
        ENDIF
C
C SET POTENTIAL
C
        IF(IS_TB.EQ.1) THEN
          V_TB=-0.920D0
          R_TB=0.05D0/CORE_RAD**2.D0
        ENDIF
        DO I=1,3
          IF(INF_NN(I).EQ.0) THEN
            IF(I.EQ.1) THEN
              NOJ=2
              NOK=3
            ELSEIF(I.EQ.2) THEN
              NOJ=3
              NOK=1
            ELSE
              NOJ=1
              NOK=2
            ENDIF
          ENDIF
        ENDDO
      DO 3002 NOB=NBMIN,NBMAX
        FACT3=SPFAC(IBOXA,IBOXB,NOA,NOB)*TPFAC(IBOXA,IBOXB,NOA,NOB)
     &       *PH(NOB)
        IF(PH(NOB).EQ.0.D0) GOTO 3002
        IF(FACT3.EQ.0.D0)   GOTO 3002
        NO=0
        DO NFA=1,NFAMAX
          NFBMX=NFBMAX
          IF(IBOXA.EQ.IBOXB) NFBMX=NFA
          DO NFB=1,NFBMX
            NO=NO+1
            ZA1=ZA1_TMP(IBOXA,NFA)
            ZA2=ZA2_TMP(IBOXA,NFA)
            ZA3=ZA1_TMP(IBOXB,NFB)
            ZA4=ZA2_TMP(IBOXB,NFB)
            ZGSI1=ZA1
     &           +TCO_11(NOB,NOA)**2.D0*ZA3
     &           +TCO_21(NOB,NOA)**2.D0*ZA4
            ZGSI2=ZA2
     &           +TCO_12(NOB,NOA)**2.D0*ZA3
     &           +TCO_22(NOB,NOA)**2.D0*ZA4
            ZETA =TCO_11(NOB,NOA)*TCO_12(NOB,NOA)*ZA3
     &           +TCO_21(NOB,NOA)*TCO_22(NOB,NOA)*ZA4
            Z_GSI1(NO)=ZGSI1*0.5D0
     &                +TCO_11(NOJ,NOA)**2.D0*R_TB
     &                +TCO_11(NOK,NOA)**2.D0*R_TB
            Z_GSI2(NO)=ZGSI2*0.5D0
     &                +TCO_12(NOJ,NOA)**2.D0*R_TB
     &                +TCO_12(NOK,NOA)**2.D0*R_TB
            Z_ETA(NO) =ZETA*(-1.D0)
     &                -TCO_11(NOJ,NOA)*TCO_12(NOJ,NOA)*2.D0*R_TB
     &                -TCO_11(NOK,NOA)*TCO_12(NOK,NOA)*2.D0*R_TB
            Z_FACT(NO)=ZNF_TMP(IBOXA,NFA)*ZNF_TMP(IBOXB,NFB)*V_TB
          ENDDO
        ENDDO
        NMAX=NO

      DO 3003 ILQ3=0,LL34
      DO 3003 ILQ4=IABS(LT34-ILQ3),LT34+ILQ3
        IP=LL34-ILQ3-ILQ4
        IF(IP.LT.0)          GOTO 3003
        IF(IP/2*2.NE.IP)     GOTO 3003
        LAMMIN=MAX0(IABS(LL1-ILQ3),IABS(LL2-ILQ4))
        LAMMAX=MIN0(    (LL1+ILQ3),    (LL2+ILQ4))
        IF(LAMMIN.GT.LAMMAX) GOTO 3003
        DO 3004 IQ34=ILQ3,LL34-ILQ4,2
C
C REARRANGEMENT COEFFICIENT
C
          AMPD=AMP(IBOXB,NOB,NOA,0,ILQ3,ILQ4,IQ34)*FACT3
          IF(AMPD.EQ.0.D0) GOTO 3004
          DO 3005 LAM=LAMMIN,LAMMAX,2
            N2=LL1+IQ34-LAM
            M2=LL2+LL34-IQ34-LAM
            IF(N2/2*2.NE.N2.OR.N2.LT.0) GOTO 9998
            IF(M2/2*2.NE.M2.OR.M2.LT.0) GOTO 9998
C
C INTEGRATION OF ANGULAR PART
C
            FACT1=AMPD*CWFAC(LL1,LL2,ILQ3,ILQ4,LAM,LT12)
C
C INTEGRATION OF RADIAL PART
C
            INU1=LL1+IQ34
            INU2=LL2+LL34-IQ34
            DO NO=1,NMAX
              ZGSI1=Z_GSI1(NO)
              ZGSI2=Z_GSI2(NO)
              ZETA =Z_ETA(NO)
              ZNN(NO)=ZNN(NO)
     &               +Z_FACT(NO)*FACT1
     &               *ZGMAT(INU1,INU2,ZGSI1,ZGSI2,ZETA,LAM)
            ENDDO
3005      CONTINUE
3004    CONTINUE
3003  CONTINUE
3002  CONTINUE
        NO=0
        DO I=1,NFAMAX
          NFBMX=NFBMAX
          IF(IBOXA.EQ.IBOXB) NFBMX=I
          DO J=1,NFBMX
            NO=NO+1
            II=NANO+I
            JJ=NBNO+J
            ZH(II,JJ)=ZH(II,JJ)+ZNN(NO)
          ENDDO
        ENDDO
        NBNO=NBNO+NFBMAX
3001  CONTINUE
        NANO=NANO+NFAMAX
3000  CONTINUE
      ELSEIF(NO_ALPH.EQ.0) THEN
      ENDIF
      RETURN
C
C ERROR MESSAGE
C
9999  WRITE(NOUT,*) 'ERROR IN PARITY!!'
      STOP
9998  WRITE(NOUT,*) 'ERROR IN RADIAL INTEGRATION!!'
      STOP
      END
C=======================================================================
