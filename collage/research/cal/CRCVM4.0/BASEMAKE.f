C==== SET BASIS ========================================================

      SUBROUTINE SET_BASIS(IDET,DEG,BB)

C-----------------------------------------------------------------------
      INCLUDE 'DIM_INC.f'
      INCLUDE 'GEM_INC.f'
      ZTH2=CDEXP(2.D0*DEG/180.D0*PI*ZI)
      IF(IDET.EQ.1) THEN
        NOCH_TMP=NOCHI
        JTOT_TMP=JTOTI
        TTOT_TMP=TTOTI
        NO=0
        DO I=1,NOCHI
          IB_TMP(I)=IBI(I)
          LS_TMP(I)=LSI(I)
          LL_TMP(I)=LLI(I)
          LT_TMP(I)=LTI(I)
          SS_TMP(I)=SSI(I)
          ST_TMP(I)=STI(I)
          TS_TMP(I)=TSI(I)
          N1_TMP(I)=N1I(I)
          N2_TMP(I)=N2I(I)
          LL1=LSI(I)
          LL2=LLI(I)
          NN1=N1I(I)
          NN2=N2I(I)
          BB1=B1I(I)
          BB2=B2I(I)
          GG1=G1I(I)
          GG2=G2I(I)
          NF=0
          DO K1=1,NN1
          DO K2=1,NN2
            NO=NO+1
            NF=NF+1
            ZA1I(I,NF)=1.D0/(BB*BB1*GG1**(K1-1))**2.D0/ZTH2
            ZA2I(I,NF)=1.D0/(BB*BB2*GG2**(K2-1))**2.D0/ZTH2
            ZAN=2.D0**(LL1+LL2+4)/(PI*DFAC(LL1)*DFAC(LL2))
     &         *ZA1I(I,NF)**(LL1+1.5D0)*ZA2I(I,NF)**(LL2+1.5D0)
            ZNFI(I,NF)=CDSQRT(ZAN)
            ZA1_TMP(I,NF)=ZA1I(I,NF)
            ZA2_TMP(I,NF)=ZA2I(I,NF)
            ZNF_TMP(I,NF)=ZNFI(I,NF)
            IF(CDABS(ZAN).LT.1.D-40) GOTO 9999
          ENDDO
          ENDDO
        ENDDO
        NOMAXI=NO
        NOMAX_TMP=NO
      ELSEIF(IDET.EQ.2) THEN
        NOCH_TMP=NOCHF
        JTOT_TMP=JTOTF
        TTOT_TMP=TTOTF
        NO=0
        DO I=1,NOCHF
          IB_TMP(I)=IBF(I)
          LS_TMP(I)=LSF(I)
          LL_TMP(I)=LLF(I)
          LT_TMP(I)=LTF(I)
          SS_TMP(I)=SSF(I)
          ST_TMP(I)=STF(I)
          TS_TMP(I)=TSF(I)
          N1_TMP(I)=N1F(I)
          N2_TMP(I)=N2F(I)
          LL1=LSF(I)
          LL2=LLF(I)
          NN1=N1F(I)
          NN2=N2F(I)
          BB1=B1F(I)
          BB2=B2F(I)
          GG1=G1F(I)
          GG2=G2F(I)
          NF=0
          DO K1=1,NN1
          DO K2=1,NN2
            NO=NO+1
            NF=NF+1
            ZA1F(I,NF)=1.D0/(BB*BB1*GG1**(K1-1))**2.D0/ZTH2
            ZA2F(I,NF)=1.D0/(BB*BB2*GG2**(K2-1))**2.D0/ZTH2
            ZAN=2.D0**(LL1+LL2+4)/(PI*DFAC(LL1)*DFAC(LL2))
     &         *ZA1F(I,NF)**(LL1+1.5D0)*ZA2F(I,NF)**(LL2+1.5D0)
            ZNFF(I,NF)=CDSQRT(ZAN)
            ZA1_TMP(I,NF)=ZA1F(I,NF)
            ZA2_TMP(I,NF)=ZA2F(I,NF)
            ZNF_TMP(I,NF)=ZNFF(I,NF)
            IF(CDABS(ZAN).LT.1.D-40) GOTO 9999
          ENDDO
          ENDDO
        ENDDO
        NOMAXF=NO
        NOMAX_TMP=NO
      ENDIF
      RETURN
C
C ERROR MASSAGE
C
9999  WRITE(NOUT,*) 'NORMALIZATION FACTOR IS TOO SMALL!!'
      STOP
      END
C=======================================================================
