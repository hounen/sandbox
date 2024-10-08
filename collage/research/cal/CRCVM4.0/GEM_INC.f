C==== COMMON VARIABLES FOR GEM =========================================
      COMMON /PRTCL1/NO_EQ
      COMMON /PRTCL2/INF_NN(3),INF_POT(3)
      COMMON /PRTCL3/S_INF(3),T_INF(3)
      COMMON /INFPRM/ITRANS,IOUT

      COMMON /INICH1/NOCHI,NOMAXI
      COMMON /INICH2/JTOTI,TTOTI,DEGII,DEGFI,DDEGI,BBII,BBFI,DBBI
      COMMON /INICH3/IBI(NOC),LSI(NOC),LLI(NOC),LTI(NOC),
     &               N1I(NOC),N2I(NOC)
      COMMON /INICH4/SSI(NOC),STI(NOC),TSI(NOC),
     &               B1I(NOC),B2I(NOC),G1I(NOC),G2I(NOC)
      COMMON /INICH5/ZA1I(NOC,NOD),ZA2I(NOC,NOD),ZNFI(NOC,NOD)
      COMMON /INICH6/ZCI(NTOT,NTOT)
      COMMON /INICH7/ZEI(NTOT)
  
      COMMON /FINCH1/NOCHF,NOMAXF
      COMMON /FINCH2/JTOTF,TTOTF,DEGIF,DEGFF,DDEGF,BBIF,BBFF,DBBF
      COMMON /FINCH3/IBF(NOC),LSF(NOC),LLF(NOC),LTF(NOC),
     &               N1F(NOC),N2F(NOC)
      COMMON /FINCH4/SSF(NOC),STF(NOC),TSF(NOC),
     &               B1F(NOC),B2F(NOC),G1F(NOC),G2F(NOC)
      COMMON /FINCH5/ZA1F(NOC,NOD),ZA2F(NOC,NOD),ZNFF(NOC,NOD)
      COMMON /FINCH6/ZCF(NTOT,NTOT)
      COMMON /FINCH7/ZEF(NTOT)

      COMMON /TMPCH1/NOCH_TMP,NOMAX_TMP
      COMMON /TMPCH2/JTOT_TMP,TTOT_TMP
      COMMON /TMPCH3/IB_TMP(NOC),LS_TMP(NOC),LL_TMP(NOC),LT_TMP(NOC),
     &               N1_TMP(NOC),N2_TMP(NOC)
      COMMON /TMPCH4/SS_TMP(NOC),ST_TMP(NOC),TS_TMP(NOC)
      COMMON /TMPCH5/ZA1_TMP(NOC,NOD),ZA2_TMP(NOC,NOD),ZNF_TMP(NOC,NOD)

      COMMON /DOUMAT/DN(NTOT,NTOT)
      COMMON /COMMAT/ZH(NTOT,NTOT)
      COMMON /ARRAY1/ZNN(NOT2),Z_GSI1(NOT2),Z_GSI2(NOT2),Z_ETA(NOT2),
     &               Z_FACT(NOT2)

      COMMON /REDMAS/DMU1(3),DMU2(3)
      COMMON /TRANCO/TCO_11(3,3),TCO_12(3,3),TCO_21(3,3),TCO_22(3,3)
      COMMON /TRNAMP/AMP(NOC,1:3,1:3,0:3,0:12,0:12,0:12)
      COMMON /ANGAMP/CWFAC(0:12,0:12,0:12,0:12,0:12,0:6),
     &               SOFAC(8,0:12,0:12,0:12,0:12,0:6,0:6),
     &               TSFAC(0:2,0:12,0:12,0:12,0:12,0:6,0:6)
      COMMON /SPNAMP/SPFAC(NOC,NOC,1:3,1:3),TPFAC(NOC,NOC,1:3,1:3),
     &               SVFAC(NOC,0:1,1:3,1:3),TVFAC(NOC,0:1,1:3,1:3)

      COMMON /FACTRL/FAC(0:100),DFAC(0:100),FACLOG(0:100),DFACLOG(0:100)

      COMMON /NNPOT1/IS_NN,IS_LS,IS_TN,NC_NN,NLS_NN,NTN_NN
      COMMON /NNPOT2/U_PARA
      COMMON /NNPOT3/VC_NN(6,0:1,0:1),VLS_NN(6,0:1,0:1)
     &              ,RC_NN(6,0:1,0:1),RLS_NN(6,0:1,0:1)
      COMMON /NNPOT4/VTN_NN(8,0:1,0:1),RTN_NN(8,0:1,0:1)
      COMMON /CNPOT1/IS_CN,NC_CN,NCP_CN,NLS_CN,NLSP_CN
      COMMON /CNPOT2/VC_CN(2),RC_CN(2),VLSP_CN(2),RLSP_CN(2)
      COMMON /CNPOT3/VCP_CN(3),RCP_CN(3),VLS_CN(3),RLS_CN(3)
      COMMON /CCPOT1/VC_AA,RC_AA
      COMMON /TBPOT1/IS_TB
      COMMON /TBPOT2/V_TB,R_TB
      COMMON /PFPOT1/IS_PF
      COMMON /PFPOT2/V_PF,CORE_RAD
