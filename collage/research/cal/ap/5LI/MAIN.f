      PROGRAM MAIN
      INCLUDE 'LI_INC.f'      
      CHARACTER*80 MEMO 
      DIMENSION ZA(ND),DN(ND,ND),ZH(ND,ND),ZE(ND),ZR(ND,ND),ZC(ND,ND)
     &         ,ZH0(ND,ND),ZE0(ND),DN0(ND,ND),ZC0(ND,ND),ZMAT(ND)
     &         ,CLD(0:100000),RESOCLD(0:100000),CONTICLD(0:100000)
     &         ,PHS(0:100000),RESOPHS(0:100000),CONTIPHS(0:100000)
     &         ,APHS(0:100000),APHSRESO(0:100000),APHSCONTI(0:100000)
     &         ,ZSCAMP(100)
C SET FACTORIAL FUNCTION
      CALL SET_FAC()
      OPEN(UNIT=33,FILE='ENE.d',STATUS='UNKNOWN')!エネルギー準位
      OPEN(UNIT=55,FILE='CLD.d',STATUS='UNKNOWN')!連続状態密度
      OPEN(UNIT=10,FILE='PHS_ANA.d',STATUS='UNKNOWN')
      REWIND 5
      READ(5,'(A)') MEMO
      READ(5,'(A)') MEMO
      READ(5,*) DL_REL, DJ_TOT!角運動量,全角運動量
      READ(5,'(A)') MEMO
      READ(5,*) NMAX, A0, GAM!基底の数、初項、公比
      READ(5,'(A)') MEMO
      READ(5,*) DEGI,DEGF,DDEG!TRAGECT COMPLEX SCALLING
      READ(5,'(A)') MEMO
      READ(5,*) BBI,BBF,DBB!TRAGECT GAUSS

      NDEG=NINT((DEGF-DEGI)/DDEG)!刻む回数
      NBB =NINT((BBF-BBI)/DBB)
      
      DO IDEG=0,NDEG!TRAGECT COMPLEX SCALLING
        DEG=DEGI+DDEG*IDEG
      DO IBB=0,NBB!TRAGECT GAUSS
        BB=BBI+DBB*IBB
        
        TH=PI*DEG/180.D0 !RADIAN表示

        IF(DABS(DL_REL-DJ_TOT).GT.0.5D0.OR.DL_REL+DJ_TOT.LT.0.5D0)
     &   GOTO 9999!三角不等式の範囲内かどうか
        IF(NMAX.GT.ND) GOTO 9999!基底の数が配列NDの範囲内かどうか

        DO I=1,NMAX
          ZA(I)=1.D0/(BB*A0*GAM**(I-1))**2*EXP(-2.D0*TH*ZI)
        ENDDO 
        DO I=1,NMAX
          DO J=1,I

C NORM CAL.

            DN(I,J)=DBLE(ZNORM(DL_REL,DL_REL,ZA(I),ZA(J)))
            DN0(I,J)=DN(I,J)
            DN(J,I)=DN(I,J)
            DN0(J,I)=DN0(I,J)

C KKNN POTENTIAL

           CALL KKNN(ZH(I,J),DL_REL,DJ_TOT,ZA(I),ZA(J))
           CALL KKNN0(ZH0(I,J),DL_REL,ZA(I),ZA(J))

C SBB POTENTIAL

c            IMESH=10000!EVEN!
c            DIST=100D0
c            CALL SBB(ZH(I,J),DL_REL,DJ_TOT,ZA(I),ZA(J),IMESH,DIST)   
c            CALL SBB0(ZH0(I,J),DL_REL,ZA(I),ZA(J))       
c           
            ZH(J,I)=ZH(I,J)
            ZH0(J,I)=ZH0(I,J)

            ZR(I,J)=ZINTEG(DL_REL,DL_REL,ZA(I),ZA(J),2,0.D0)
            ZR(J,I)=ZR(I,J)

          ENDDO
        ENDDO

C DIAGONAZING

        CALL EIGENH(DEG,NMAX,NZ,ZH,ZE,DN,ZC)
        CALL EIGENH(DEG,NMAX,NZ0,ZH0,ZE0,DN0,ZC0)

C----CONTINUUM LEVEL DENSITY-------------------------------------------        
        DELE=0.001D0
        WRITE(55,*) 'ENERGY,CLD,RESOCLD,CONTICLD'
        DO I=0,10000
          EVAL=DELE*I
C----SCATTERING----
         DO J=1,NZ
           CLD(I)=CLD(I)-(1.D0/PI)*DIMAG(1.D0/(EVAL-ZH(J,J)))
         ENDDO
         DO J=1,NZ0
           CLD(I)=CLD(I)+(1.D0/PI)*DIMAG(1.D0/(EVAL-ZH0(J,J)))
         ENDDO
C----RESONANT---- 
        J=21  !STATE 
        RESOCLD(I)=-(1.D0/PI)*DIMAG(1.D0/(EVAL-ZH(J,J)))
C----CONTINNUM---- 
        CONTICLD(I)=CLD(I)-RESOCLD(I)
        
        WRITE(55,*) EVAL,CLD(I),RESOCLD(I),CONTICLD(I)
        ENDDO

C----PHASE SHIFT-------------------------------------------------------      
        WRITE(56,*) 'EVAL,PHS,RESOPHS,CONTIPHS' 
        WRITE(10,*) 'EVAL[MEV]','PHS[RAD.]' 
       PHS(0)=0.D0
       DO I=1,10000
          EVAL=DELE*I
C----SCATTERING----
          PHS(I)=PHS(I-1)+0.5D0*DELE*180.D0*(CLD(I)+CLD(I-1))
       WRITE(56,*) EVAL,PHS(I),RESOPHS(I),CONTIPHS(I) 
     
C LORENTZIAN ANALYSIS--------------------------------------------------
C----SCATTERING----       
        APHS(I)=0.D0
        DO K=1,NZ
          FINA=(DBLE(ZH(K,K)-EVAL)/DIMAG(ZH(K,K)))
          DINI=DBLE(ZH(K,K))/DIMAG(ZH(K,K))
          APHS(I)=APHS(I)+ATAN(FINA)-ATAN(DINI)    
        ENDDO
        DO K=1,NZ0
          FINA=(DBLE(ZH0(K,K)-EVAL)/DIMAG(ZH0(K,K)))
          DINI=DBLE(ZH0(K,K))/DIMAG(ZH0(K,K))
          APHS(I)=APHS(I)-ATAN(FINA)+ATAN(DINI)    
        ENDDO

C----RESONANCE CONTRIBUTION----        
        FINA=(DBLE(ZH(J,J)-EVAL)/DIMAG(ZH(J,J)))
        DINI=DBLE(ZH(J,J))/DIMAG(ZH(J,J))
        APHSRESO(I)=ATAN(FINA)-ATAN(DINI)
C---CONTINUUM CONTRIBUTION----
        APHSCONTI(I)=APHS(I)-APHSRESO(I)
         
        WRITE(10,*) EVAL
     &   ,APHS(I)*180.D0/PI,APHSRESO(I)*180.D0/PI,APHSCONTI(I)*180.D0/PI
       ENDDO
      WRITE(56,*) " "
      WRITE(55,*) 
      WRITE(10,*) 
      ENDDO!END TRAGECT GAUSS 
      ENDDO!END TRAGECT COMPLEX SCALLING
      CLOSE(10)
      CLOSE(33)
      CLOSE(55)
      STOP

9999  CONTINUE
      WRITE(6,*) 'INVALID INPUT PARAMETER'
      STOP
      END PROGRAM MAIN


