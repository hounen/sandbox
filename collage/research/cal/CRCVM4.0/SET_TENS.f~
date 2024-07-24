C==== SET TENSOR FORCE MATRIX ==========================================

      SUBROUTINE SET_TENS

C-----------------------------------------------------------------------
      INCLUDE 'DIM_INC.f'
      INCLUDE 'GEM_INC.f'
      DIMENSION PH(3),C_TE(0:2)
      DIMENSION VC(8),RC(8)
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
        SS=SS_TMP(IBOXB)
        TS=TS_TMP(IBOXB)
        NFBMAX=N1_TMP(IBOXB)*N2_TMP(IBOXB)
        IF(NFAMAX*NFBMAX.EQ.0) THEN
          NBNO=NBNO+NFBMAX
          GOTO 1001
        ENDIF
        IF(IABS(LT12-LT34).GT.2.OR.LT12+LT34.LT.2) THEN
          NBNO=NBNO+NFBMAX
          GOTO 1001
        ENDIF
        IF(DABS(ST12-ST34).GT.2.D0.OR.ST12+ST34.LT.2.D0) THEN
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
        IF(NO_EQ.EQ.0) THEN
          NBMIN=NOB
          NBMAX=NOB
          DO I=1,3
            PH(I)=0.D0
            IF(I.EQ.NOB) PH(I)=1.D0
          ENDDO
        ELSEIF(NO_EQ.EQ.1) THEN
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
     &          +IDINT(2.D0*S_INF(NOJ)+2.D0*S_INF(NOK)-SS-TS+0.01D0)
            ENDIF
            PH(NOB)=1.D0+(-1.D0)**IP
          ELSEIF(INF_POT(NOJ)/2*2.EQ.INF_POT(NOJ)) THEN
            PH(NOB)=1.D0
            PH(NOJ)=0.D0
            IF(INF_POT(NOJ).EQ.0) THEN
              IP=LL3
            ELSE
              IP=LL3+1
     &          +IDINT(2.D0*S_INF(NOJ)+2.D0*S_INF(NOK)-SS-TS+0.01D0)
            ENDIF
            PH(NOK)=(-1.D0)**IP
          ELSE
            PH(NOB)=1.D0
            PH(NOK)=0.D0
            IF(INF_POT(NOK).EQ.0) THEN
              IP=LL3
            ELSE
              IP=LL3+1
     &          +IDINT(2.D0*S_INF(NOJ)+2.D0*S_INF(NOK)-SS-TS+0.01D0)
            ENDIF
            PH(NOJ)=(-1.D0)**IP
          ENDIF
        ELSEIF(NO_EQ.EQ.3) THEN
          NBMIN=1
          NBMAX=3
          DO I=1,3
            PH(I)=1.D0
          ENDDO
        ENDIF
      DO 1002 NOV=1,3
        C_TE(0)=TCO_12(NOV,NOA)**2.D0
        C_TE(1)=TCO_11(NOV,NOA)*TCO_12(NOV,NOA)
        C_TE(2)=TCO_11(NOV,NOA)**2.D0
      DO 1003 ITT=0,1
        IST=1
        FACT0=(-1.D0)**(LT34+ST12+JTOT_TMP)
     &       *(-1.D0)**(S_INF(NOV)+ST34+3)
     &       *4.D0*DSQRT(6.D0*PI)
     &       *DSQRT((2.D0*ST12+1.D0)*(2.D0*ST34+1.D0))
     &       *SIXJ_D(JTOT_TMP,ST12,DBLE(LT12),2.D0,DBLE(LT34),ST34)
     &       *SIXJ_D(1.D0,ST12,S_INF(NOV),ST34,1.D0,2.D0)
        IF(INF_POT(NOV).EQ.2) THEN
          NNC=NTN_NN
          DO I=1,NNC
            VC(I)=VTN_NN(I,IST,ITT)
            RC(I)=RTN_NN(I,IST,ITT)
          ENDDO
        ELSEIF(INF_POT(NOV).EQ.4) THEN
          NNC=NTN_NN
          DO I=1,NNC
            VC(I)=VTN_NN(I,IST,ITT)
            RC(I)=RTN_NN(I,IST,ITT)
          ENDDO
        ELSEIF(INF_POT(NOV).EQ.5) THEN
          NNC=NTN_NN
          DO I=1,NNC
            VC(I)=VTN_NN(I,IST,ITT)
            RC(I)=RTN_NN(I,IST,ITT)
          ENDDO
        ELSE
          GOTO 1002
        ENDIF
      DO 1003 IIV=1,NNC
        IF(VC(IIV).EQ.0.D0) GOTO 1003
        VVC=VC(IIV)
        RRC=RC(IIV)
      DO 1004 NOB=NBMIN,NBMAX
        FACT3=SVFAC(IBOXA,IST,NOA,NOV)*SVFAC(IBOXB,IST,NOB,NOV)
     &       *TVFAC(IBOXA,ITT,NOA,NOV)*TVFAC(IBOXB,ITT,NOB,NOV)
     &       *PH(NOB)
        IF(PH(NOB).EQ.0.D0) GOTO 1004
        IF(FACT3.EQ.0.D0)   GOTO 1004
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
     &                +TCO_11(NOV,NOA)**2.D0*RRC
            Z_GSI2(NO)=ZGSI2*0.5D0
     &                +TCO_12(NOV,NOA)**2.D0*RRC
            Z_ETA(NO) =ZETA*(-1.D0)
     &                -TCO_11(NOV,NOA)*TCO_12(NOV,NOA)*2.D0*RRC
            Z_FACT(NO)=ZNF_TMP(IBOXA,NFA)*ZNF_TMP(IBOXB,NFB)*VVC
          ENDDO
        ENDDO
        NMAX=NO

      DO 1005 ILQ3=0,LL34
      DO 1005 ILQ4=IABS(LT34-ILQ3),LT34+ILQ3
        IP=LL34-ILQ3-ILQ4
        IF(IP.LT.0)      GOTO 1005
        IF(IP/2*2.NE.IP) GOTO 1005
        DO 1006 IQ34=ILQ3,LL34-ILQ4,2
C
C REARRANGEMENT COEFFICIENT
C
          AMPD=AMP(IBOXB,NOB,NOA,0,ILQ3,ILQ4,IQ34)*FACT3
          IF(AMPD.EQ.0.D0) GOTO 1006
          DO 1007 II=0,2
            KT1=II
            KT2=2-II
          DO 1007 LA3=IABS(LL1-KT1),LL1+KT1,2
          DO 1007 LA4=IABS(LL2-KT2),LL2+KT2,2
C
C INTEGRATION OF ANGULAR PART
C
            FACT1=FACT0*AMPD*C_TE(II)
     &           *TSFAC(II,LL1,LL2,LA3,LA4,LT12,LT34)
            IF(FACT1.EQ.0.D0)    GOTO 1007
            LAMMIN=MAX0(IABS(LA3-ILQ3),IABS(LA4-ILQ4))
            LAMMAX=MIN0(    (LA3+ILQ3),    (LA4+ILQ4))
            IF(LAMMIN.GT.LAMMAX) GOTO 1007
            DO 1008 LAM=LAMMIN,LAMMAX,2
              FACT2=FACT1*CWFAC(LA3,LA4,ILQ3,ILQ4,LAM,LT34)
C
C INTEGRATION OF RADIAL PART
C
              INU1=LL1+IQ34+KT1
              INU2=LL2+LL34-IQ34+KT2
              N2=INU1-LAM
              M2=INU2-LAM
              IF(N2/2*2.NE.N2.OR.N2.LT.0) GOTO 1008
              IF(M2/2*2.NE.M2.OR.M2.LT.0) GOTO 1008
              DO NO=1,NMAX
                ZGSI1=Z_GSI1(NO)
                ZGSI2=Z_GSI2(NO)
                ZETA =Z_ETA(NO)
                ZNN(NO)=ZNN(NO)
     &                 +Z_FACT(NO)*FACT2
     &                 *ZGMAT(INU1,INU2,ZGSI1,ZGSI2,ZETA,LAM)
              ENDDO
1008        CONTINUE
1007      CONTINUE
1006    CONTINUE
1005  CONTINUE
1004  CONTINUE
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
      RETURN
C
C ERROR MESSAGE
C
9999  WRITE(NOUT,*) 'ERROR IN PARITY!!'
      STOP
      END
C=======================================================================
