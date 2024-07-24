C==== REARRANGEMENT COEFFICIENT ========================================

      SUBROUTINE REARRANGE(IDET1)

C-----------------------------------------------------------------------
      INCLUDE 'DIM_INC.f'
      INCLUDE 'GEM_INC.f'
      IF(IDET1.EQ.1) THEN
C
C FOR INITIAL STATE
C
        DO 1000 NC=1,NOCHI
        DO 1000 NA=1,3
        DO 1000 NB=1,3
        DO 1000 NV=0,3
        DO 1000 ILQ1=0,12
        DO 1000 ILQ2=0,12
        DO 1000 ILAM1=0,12
          AMP(NC,NA,NB,NV,ILQ1,ILQ2,ILAM1)=0.D0
1000    CONTINUE

        DO 1001 NC=1,NOCHI
          NBS=IBI(NC)
          LL1=LSI(NC)
          LL2=LLI(NC)
          LTT=LTI(NC)
        DO 1001 NA=1,3
        DO 1001 NB=1,3
        DO 1001 NV=0,3
          IF(NV.EQ.0) THEN
            CO11=TCO_11(NA,NB)
            CO12=TCO_12(NA,NB)
            CO21=TCO_21(NA,NB)
            CO22=TCO_22(NA,NB)
          ELSE
            CO11=-TCO_11(NA,NV)*TCO_11(NV,NB)
     &           +TCO_12(NA,NV)*TCO_21(NV,NB)
            CO12=-TCO_11(NA,NV)*TCO_12(NV,NB)
     &           +TCO_12(NA,NV)*TCO_22(NV,NB)
            CO21=-TCO_21(NA,NV)*TCO_11(NV,NB)
     &           +TCO_22(NA,NV)*TCO_21(NV,NB)
            CO22=-TCO_21(NA,NV)*TCO_12(NV,NB)
     &           +TCO_22(NA,NV)*TCO_22(NV,NB)
          ENDIF
        DO 1001 IQ1=0,LL1
        DO 1001 IQ2=0,LL2
          ILAM1=IQ1+IQ2
          ILAM2=LL1+LL2-IQ1-IQ2
          FCO1=CO11**IQ1*CO12**(LL1-IQ1)*(2.D0*LL1+1.D0)
     &        *DSQRT(FAC(2*LL1)/FAC(2*IQ1)/FAC(2*LL1-2*IQ1))
          FCO2=CO21**IQ2*CO22**(LL2-IQ2)*(2.D0*LL2+1.D0)
     &        *DSQRT(FAC(2*LL2)/FAC(2*IQ2)/FAC(2*LL2-2*IQ2))
        DO 1002 ILQ1=0,12
          IF(IABS(IQ1-IQ2).GT.ILQ1) GOTO 1002
          IF(    (IQ1+IQ2).LT.ILQ1) GOTO 1002
          IP=ILAM1+ILQ1
          IF(IP/2*2.NE.IP)          GOTO 1002
          CG1=CGZ_I(IQ1,IQ2,ILQ1)
        DO 1003 ILQ2=0,12
          IF(IABS(LL1-IQ1-LL2+IQ2).GT.ILQ2) GOTO 1003
          IF(    (LL1-IQ1+LL2-IQ2).LT.ILQ2) GOTO 1003
          IP=ILAM2+ILQ2
          IF(IP/2*2.NE.IP)                  GOTO 1003
          CG2=CGZ_I(LL1-IQ1,LL2-IQ2,ILQ2)
          U12=DNINJ_I(IQ1,IQ2,ILQ1,LL1-IQ1,LL2-IQ2,ILQ2,LL1,LL2,LTT)
          AMPCO=FCO1*FCO2*CG1*CG2*U12
          AMP(NC,NA,NB,NV,ILQ1,ILQ2,ILAM1)
     &        =AMP(NC,NA,NB,NV,ILQ1,ILQ2,ILAM1)+AMPCO
1003    CONTINUE
1002    CONTINUE
1001    CONTINUE

        DO NC=1,NOCHI
        DO ND=1,NOCHI
        DO NA=1,3
        DO NB=1,3
          SPFAC(NC,ND,NA,NB)=0.D0
          TPFAC(NC,ND,NA,NB)=0.D0
        ENDDO
        ENDDO
        ENDDO
        ENDDO
        DO NA=1,3
          IF(NA.EQ.1) THEN
            I=1
            J=2
            K=3
          ELSEIF(NA.EQ.2) THEN
            I=2
            J=3
            K=1
          ELSE
            I=3
            J=1
            K=2
          ENDIF
          SI=S_INF(I)
          SJ=S_INF(J)
          SK=S_INF(K)
        DO NB=1,3
        DO NC=1,NOCHI
          SSS=SSI(NC)
          SST=STI(NC)
          TTS=TSI(NC)
          TTT=TTOTI
        DO ND=1,NOCHI
          SSSP=SSI(ND)
          SSTP=STI(ND)
          TTSP=TSI(ND)
          IF(MOD(NA-NB+3,3).EQ.1) THEN
            IF(SST.EQ.SSTP) THEN
              SPFAC(NC,ND,NA,NB)=(-1.D0)**(2.D0*SI+SJ+SK+SSS)
     &                          *DSQRT((2.D0*SSS+1.D0)*(2.D0*SSSP+1.D0))
     &                          *SIXJ_D(SK,SJ,SSS,SI,SST,SSSP)
            ENDIF
            TPFAC(NC,ND,NA,NB)=(-1.D0)**(2.D0*SI+SJ+SK+TTS)
     &                        *DSQRT((2.D0*TTS+1.D0)*(2.D0*TTSP+1.D0))
     &                        *SIXJ_D(SK,SJ,TTS,SI,TTT,TTSP)
          ELSEIF(MOD(NA-NB+3,3).EQ.2) THEN
            IF(SST.EQ.SSTP) THEN
              SPFAC(NC,ND,NA,NB)=(-1.D0)**(SI+2.D0*SJ+SK+SSSP)
     &                          *DSQRT((2.D0*SSS+1.D0)*(2.D0*SSSP+1.D0))
     &                          *SIXJ_D(SJ,SK,SSS,SI,SST,SSSP)
            ENDIF
            TPFAC(NC,ND,NA,NB)=(-1.D0)**(SI+2.D0*SJ+SK+TTSP)
     &                        *DSQRT((2.D0*TTS+1.D0)*(2.D0*TTSP+1.D0))
     &                        *SIXJ_D(SJ,SK,TTS,SI,TTT,TTSP)
          ELSE
            IF(SST.EQ.SSTP.AND.SSS.EQ.SSSP) THEN
              SPFAC(NC,ND,NA,NB)=1.D0
            ENDIF
            IF(TTS.EQ.TTSP) THEN
              TPFAC(NC,ND,NA,NB)=1.D0
            ENDIF
          ENDIF
        ENDDO
        ENDDO
        ENDDO
        ENDDO
        
        DO NC=1,NOCHI
        DO NS=0,1
        DO NA=1,3
        DO NB=1,3
          SVFAC(NC,NS,NA,NB)=0.D0
          TVFAC(NC,NS,NA,NB)=0.D0
        ENDDO
        ENDDO
        ENDDO
        ENDDO
        DO NA=1,3
          IF(NA.EQ.1) THEN
            I=1
            J=2
            K=3
          ELSEIF(NA.EQ.2) THEN
            I=2
            J=3
            K=1
          ELSE
            I=3
            J=1
            K=2
          ENDIF
          SI=S_INF(I)
          SJ=S_INF(J)
          SK=S_INF(K)
        DO NB=1,3
          IF(NB.EQ.1) THEN
            J=2
            K=3
          ELSEIF(NB.EQ.2) THEN
            J=3
            K=1
          ELSE
            J=1
            K=2
          ENDIF
        DO NC=1,NOCHI
          SSS=SSI(NC)
          SST=STI(NC)
          TTS=TSI(NC)
          TTT=TTOTI
        DO NS=0,1
          SSSP=S_INF(J)+S_INF(K)
          NNDET=0
          IF(SSSP.EQ.1.D0) THEN
            SSSP=DBLE(NS)
            IF(INF_NN(J).EQ.INF_NN(K)) NNDET=1
          ELSE
            IF(NS.EQ.1) GOTO 3000
          ENDIF
          TTSP=SSSP
          IF(MOD(NA-NB+3,3).EQ.1) THEN
            SVFAC(NC,NS,NA,NB)=(-1.D0)**(2.D0*SI+SJ+SK+SSS)
     &                        *DSQRT((2.D0*SSS+1.D0)*(2.D0*SSSP+1.D0))
     &                        *SIXJ_D(SK,SJ,SSS,SI,SST,SSSP)
            IF(TTSP.EQ.1.D0.OR.NNDET.NE.1) THEN
              TVFAC(NC,NS,NA,NB)=(-1.D0)**(2.D0*SI+SJ+SK+TTS)
     &                          *DSQRT((2.D0*TTS+1.D0)*(2.D0*TTSP+1.D0))
     &                          *SIXJ_D(SK,SJ,TTS,SI,TTT,TTSP)
            ENDIF
          ELSEIF(MOD(NA-NB+3,3).EQ.2) THEN
            SVFAC(NC,NS,NA,NB)=(-1.D0)**(SI+2.D0*SJ+SK+SSSP)
     &                        *DSQRT((2.D0*SSS+1.D0)*(2.D0*SSSP+1.D0))
     &                        *SIXJ_D(SJ,SK,SSS,SI,SST,SSSP)
            IF(TTSP.EQ.1.D0.OR.NNDET.NE.1) THEN
              TVFAC(NC,NS,NA,NB)=(-1.D0)**(SI+2.D0*SJ+SK+TTSP)
     &                          *DSQRT((2.D0*TTS+1.D0)*(2.D0*TTSP+1.D0))
     &                          *SIXJ_D(SJ,SK,TTS,SI,TTT,TTSP)
            ENDIF
          ELSE
            IF(SSS.EQ.SSSP) THEN
              SVFAC(NC,NS,NA,NB)=1.D0
            ENDIF
            IF(TTS.EQ.TTSP) THEN
              IF(TTSP.EQ.1.D0.OR.NNDET.NE.1) THEN
                TVFAC(NC,NS,NA,NB)=1.D0
              ENDIF
            ENDIF
          ENDIF
3000    ENDDO
        ENDDO
        ENDDO
        ENDDO
      ELSEIF(IDET2.EQ.2) THEN
C
C FOR FINAL STATE
C
        DO 2000 NC=1,NOCHF
        DO 2000 NA=1,3
        DO 2000 NB=1,3
        DO 2000 NV=0,3
        DO 2000 ILQ1=0,12
        DO 2000 ILQ2=0,12
        DO 2000 ILAM1=0,12
          AMP(NC,NA,NB,NV,ILQ1,ILQ2,ILAM1)=0.D0
2000    CONTINUE

        DO 2001 NC=1,NOCHF
          NBS=IBF(NC)
          LL1=LSF(NC)
          LL2=LLF(NC)
          LTT=LTF(NC)
        DO 2001 NA=1,3
        DO 2001 NB=1,3
        DO 2001 NV=0,3
          IF(NV.EQ.0) THEN
            CO11=TCO_11(NA,NB)
            CO12=TCO_12(NA,NB)
            CO21=TCO_21(NA,NB)
            CO22=TCO_22(NA,NB)
          ELSE
            CO11=-TCO_11(NA,NV)*TCO_11(NV,NB)
     &           +TCO_12(NA,NV)*TCO_21(NV,NB)
            CO12=-TCO_11(NA,NV)*TCO_12(NV,NB)
     &           +TCO_12(NA,NV)*TCO_22(NV,NB)
            CO21=-TCO_21(NA,NV)*TCO_11(NV,NB)
     &           +TCO_22(NA,NV)*TCO_21(NV,NB)
            CO22=-TCO_21(NA,NV)*TCO_12(NV,NB)
     &           +TCO_22(NA,NV)*TCO_22(NV,NB)
          ENDIF
        DO 2001 IQ1=0,LL1
        DO 2001 IQ2=0,LL2
          ILAM1=IQ1+IQ2
          ILAM2=LL1+LL2-IQ1-IQ2
          FCO1=CO11**IQ1*CO12**(LL1-IQ1)*(2.D0*LL1+1.D0)
     &        *DSQRT(FAC(2*LL1)/FAC(2*IQ1)/FAC(2*LL1-2*IQ1))
          FCO2=CO21**IQ2*CO22**(LL2-IQ2)*(2.D0*LL2+1.D0)
     &        *DSQRT(FAC(2*LL2)/FAC(2*IQ2)/FAC(2*LL2-2*IQ2))
        DO 2002 ILQ1=0,12
          IF(IABS(IQ1-IQ2).GT.ILQ1) GOTO 2002
          IF(    (IQ1+IQ2).LT.ILQ1) GOTO 2002
          IP=ILAM1+ILQ1
          IF(IP/2*2.NE.IP)          GOTO 2002
          CG1=CGZ_I(IQ1,IQ2,ILQ1)
        DO 2003 ILQ2=0,12
          IF(IABS(LL1-IQ1-LL2+IQ2).GT.ILQ2) GOTO 2003
          IF(    (LL1-IQ1+LL2-IQ2).LT.ILQ2) GOTO 2003
          IP=ILAM2+ILQ2
          IF(IP/2*2.NE.IP)                  GOTO 2003
          CG2=CGZ_I(LL1-IQ1,LL2-IQ2,ILQ2)
          U12=DNINJ_I(IQ1,IQ2,ILQ1,LL1-IQ1,LL2-IQ2,ILQ2,LL1,LL2,LTT)
          AMPCO=FCO1*FCO2*CG1*CG2*U12
          AMP(NC,NA,NB,NV,ILQ1,ILQ2,ILAM1)
     &        =AMP(NC,NA,NB,NV,ILQ1,ILQ2,ILAM1)+AMPCO
2003    CONTINUE
2002    CONTINUE
2001    CONTINUE

        DO NC=1,NOCHF
        DO ND=1,NOCHF
        DO NA=1,3
        DO NB=1,3
          SPFAC(NC,ND,NA,NB)=0.D0
          TPFAC(NC,ND,NA,NB)=0.D0
        ENDDO
        ENDDO
        ENDDO
        ENDDO
        DO NA=1,3
          IF(NA.EQ.1) THEN
            I=1
            J=2
            K=3
          ELSEIF(NA.EQ.2) THEN
            I=2
            J=3
            K=1
          ELSE
            I=3
            J=1
            K=2
          ENDIF
          SI=S_INF(I)
          SJ=S_INF(J)
          SK=S_INF(K)
        DO NB=1,3
        DO NC=1,NOCHF
          SSS=SSF(NC)
          SST=STF(NC)
        DO ND=1,NOCHF
          SSSP=SSF(ND)
          SSTP=STF(ND)
          IF(MOD(NA-NB+3,3).EQ.1) THEN
            IF(SST.EQ.SSTP) THEN
              SPFAC(NC,ND,NA,NB)=(-1.D0)**(2.D0*SI+SJ+SK+SSS)
     &                          *DSQRT((2.D0*SSS+1.D0)*(2.D0*SSSP+1.D0))
     &                          *SIXJ_D(SK,SJ,SSS,SI,SST,SSSP)
            ENDIF
            TPFAC(NC,ND,NA,NB)=(-1.D0)**(2.D0*SI+SJ+SK+TTS)
     &                        *DSQRT((2.D0*TTS+1.D0)*(2.D0*TTSP+1.D0))
     &                        *SIXJ_D(SK,SJ,TTS,SI,TTT,TTSP)
          ELSEIF(MOD(NA-NB+3,3).EQ.2) THEN
            IF(SST.EQ.SSTP) THEN
              SPFAC(NC,ND,NA,NB)=(-1.D0)**(SI+2.D0*SJ+SK+SSSP)
     &                          *DSQRT((2.D0*SSS+1.D0)*(2.D0*SSSP+1.D0))
     &                          *SIXJ_D(SJ,SK,SSS,SI,SST,SSSP)
            ENDIF
            TPFAC(NC,ND,NA,NB)=(-1.D0)**(SI+2.D0*SJ+SK+TTSP)
     &                        *DSQRT((2.D0*TTS+1.D0)*(2.D0*TTSP+1.D0))
     &                        *SIXJ_D(SJ,SK,TTS,SI,TTT,TTSP)
          ELSE
            IF(SST.EQ.SSTP.AND.SSS.EQ.SSSP) THEN
              SPFAC(NC,ND,NA,NB)=1.D0
            ENDIF
            IF(TTS.EQ.TTSP) THEN
              TPFAC(NC,ND,NA,NB)=1.D0
            ENDIF
          ENDIF
        ENDDO
        ENDDO
        ENDDO
        ENDDO

        DO NC=1,NOCHF
        DO NS=0,1
        DO NA=1,3
        DO NB=1,3
          SVFAC(NC,NS,NA,NB)=0.D0
          TVFAC(NC,NS,NA,NB)=0.D0
        ENDDO
        ENDDO
        ENDDO
        ENDDO
        DO NA=1,3
          IF(NA.EQ.1) THEN
            I=1
            J=2
            K=3
          ELSEIF(NA.EQ.2) THEN
            I=2
            J=3
            K=1
          ELSE
            I=3
            J=1
            K=2
          ENDIF
          SI=S_INF(I)
          SJ=S_INF(J)
          SK=S_INF(K)
        DO NB=1,3
          IF(NB.EQ.1) THEN
            J=2
            K=3
          ELSEIF(NB.EQ.2) THEN
            J=3
            K=1
          ELSE
            J=1
            K=2
          ENDIF
        DO NC=1,NOCHF
          SSS=SSF(NC)
          SST=STF(NC)
          TTS=TSF(NC)
          TTT=TTOTF
        DO NS=0,1
          SSSP=S_INF(J)+S_INF(K)
          IF(SSSP.EQ.1.D0) THEN
            SSSP=DBLE(NS)
          ELSE
            IF(NS.EQ.1) GOTO 4000
          ENDIF
          TTSP=SSSP
          IF(MOD(NA-NB+3,3).EQ.1) THEN
            SVFAC(NC,NS,NA,NB)=(-1.D0)**(2.D0*SI+SJ+SK+SSS)
     &                        *DSQRT((2.D0*SSS+1.D0)*(2.D0*SSSP+1.D0))
     &                        *SIXJ_D(SK,SJ,SSS,SI,SST,SSSP)
            TVFAC(NC,NS,NA,NB)=(-1.D0)**(2.D0*SI+SJ+SK+TTS)
     &                        *DSQRT((2.D0*TTS+1.D0)*(2.D0*TTSP+1.D0))
     &                        *SIXJ_D(SK,SJ,TTS,SI,TTT,TTSP)
          ELSEIF(MOD(NA-NB+3,3).EQ.2) THEN
            SVFAC(NC,NS,NA,NB)=(-1.D0)**(SI+2.D0*SJ+SK+SSSP)
     &                        *DSQRT((2.D0*SSS+1.D0)*(2.D0*SSSP+1.D0))
     &                        *SIXJ_D(SJ,SK,SSS,SI,SST,SSSP)
            TVFAC(NC,NS,NA,NB)=(-1.D0)**(SI+2.D0*SJ+SK+TTSP)
     &                        *DSQRT((2.D0*TTS+1.D0)*(2.D0*TTSP+1.D0))
     &                        *SIXJ_D(SJ,SK,TTS,SI,TTT,TTSP)
          ELSE
            IF(SSS.EQ.SSSP) THEN
              SVFAC(NC,NS,NA,NB)=1.D0
            ENDIF
            IF(TTS.EQ.TTSP) THEN
              TVFAC(NC,NS,NA,NB)=1.D0
            ENDIF
          ENDIF
4000    ENDDO
        ENDDO
        ENDDO
        ENDDO
      ENDIF
      RETURN
      END
C=======================================================================
