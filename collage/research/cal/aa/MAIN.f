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
      OPEN(UNIT=33,FILE='ENE.d',STATUS='UNKNOWN')
      OPEN(UNIT=55,FILE='CLD.d',STATUS='UNKNOWN')
      OPEN(UNIT=56,FILE='PHS.d',STATUS='UNKNOWN')
      OPEN(UNIT=10,FILE='PHS_ANA.d',STATUS='UNKNOWN')
      REWIND 5
      READ(5,'(A)') MEMO
      READ(5,'(A)') MEMO
      READ(5,*) DL_REL
      READ(5,'(A)') MEMO
      READ(5,*) NMAX, A0, GAM
      READ(5,'(A)') MEMO
      READ(5,*) DEGI,DEGF,DDEG!TRAGECT COMPLEX SCALLING
      READ(5,'(A)') MEMO
      READ(5,*) BBI,BBF,DBB!TRAGECT GAUSS

      NDEG=NINT((DEGF-DEGI)/DDEG)
      NBB =NINT((BBF-BBI)/DBB)

      DO IDEG=0,NDEG!TRAGECT COMPLEX SCALLING
        DEG=DEGI+DDEG*IDEG
      DO IBB=0,NBB!TRAGECT GAUSS
        BB=BBI+DBB*IBB

        TH=PI*DEG/180.D0

!        IF(DABS(DL_REL-DJ_TOT).GT.0.5D0.OR.DL_REL+DJ_TOT.LT.0.5D0)
!     &   GOTO 9999!
        IF(NMAX.GT.ND) GOTO 9999

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

C SCHMID-WILDERMUTH FORCE
            CALL SWFRC(ZH(I,J),DL_REL,ZA(I),ZA(J))
            CALL SWFRC0(ZH0(I,J),DL_REL,ZA(I),ZA(J))
C BUCK POTENTIAL

c            CALL BUCK(ZH(I,J),DL_REL,ZA(I),ZA(J))
c            CALL BUCK0(ZH0(I,J),DL_REL,ZA(I),ZA(J))

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
        DELE=0.002D0
        WRITE(55,*) '# ENERGY,CLD,RESOCLD,CONTICLD'
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
        J=12 !STATE 
        RESOCLD(I)=-(1.D0/PI)*DIMAG(1.D0/(EVAL-ZH(J,J)))
C----CONTINNUM---- 
        CONTICLD(I)=CLD(I)-RESOCLD(I)
        WRITE(55,*) EVAL,CLD(I),RESOCLD(I),CONTICLD(I)
        ENDDO
C----PHASE SHIFT---------------------------------------------------
        WRITE(56,*) '# EVAL,PHS,RESOPHS,CONTIPHS'
        WRITE(10,*) '# EVAL[MEV]','PHS[RAD.]'
       PHS(0)=0.D0
       DO I=1,10000
          EVAL=DELE*I
C----SCATTERING----
          PHS(I)=PHS(I-1)+0.5D0*DELE*180.D0*(CLD(I)+CLD(I-1))
CC----RESONANT CONTRIBUTION----
C          RESOPHS(I)
C     &     =RESOPHS(I-1)+0.5D0*DELE*180.D0*(RESOCLD(I)+RESOCLD(I-1))
CC----CONTINNUM CONTRIBUTION----
C          CONTIPHS(I)
C     &     =CONTIPHS(I-1)+0.5D0*DELE*180.D0*(CONTICLD(I)+CONTICLD(I-1))
C       
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
C----------------------------------------------------------------------
C       
C       ZRAD=(0.D0,0.D0)
C       DO I=1,NMAX
C         DO J=1,NMAX
C           ZRAD=ZRAD+ZC(I,19)*ZC(J,19)*ZR(I,J)
C         ENDDO
C       ENDDO
C       WRITE(*,*) 'R.M.S RADIUS = ',CDSQRT(ZRAD),'[FM]'
C        
CC t-MATRIX,SCATTERING AMPLITUDE
C        DMU=0.8D0*DMASS!REDUCED MASS
C        DO I=1,100
C          DDK=0.005D0
C          DK=DDK*I
CC          DE=0.1D0*I
CC          DK=DSQRT(2.D0*DMU*DE/HBC**2)
CC PART1
C          CALL CENT_F1(TVALC1,DK,DL_REL)
C          CALL CENL_F1(TVALCL1,DK,DL_REL)
C          CALL LSFRS_F1(TVALLS1,DK,DL_REL,DJ_TOT)
C          ZTVAL1=TVALC1+TVALCL1+TVALLS1
CC PART2-            
C          DO J=1,NMAX
C            CALL CENTMAT(ZMATC,DK,ZA(J),DL_REL)
C            CALL CENLMAT(ZMATCL,DK,ZA(J),DL_REL)
C            CALL LSMAT(ZMATLS,DK,ZA(J),DL_REL,DJ_TOT)
C            ZMAT(J)=ZMATC+ZMATCL+ZMATLS
C          ENDDO
C          ZTVAL2=0.D0
C          DEI=(HBC*DK)**2.D0/2.D0/DMU
CC         DO J=1,NZ
CC           ZSCAMP(J)=0.D0
CC         DO K=1,NMAX
CC           ZSCAMP(J)=ZSCAMP(J)+ZC(K,J)*ZMAT(K)
CC         ENDDO
CC         ENDDO
CC         DO J=1,NZ
CC           ZTVAL2=ZTVAL2+(2.D0*DL_REL+1.D0)/4.D0/PI
CC    &             *ZMAT(J)**2.D0/(DEI-ZE(J))
CC         ENDDO
C         DO J=1,NZ
C           DO K=1,NMAX
C             DO L=1,NMAX
C               ZTVAL2=ZTVAL2+1.D0/4.D0/PI
C     &                    *ZC(K,J)*ZC(L,J)*ZMAT(K)*ZMAT(L)/(DEI-ZE(J))
CC     &                    *DDK*DK**2.D0
C             ENDDO
C           ENDDO
C         ENDDO
C          
C          ZTVAL=ZTVAL1+ZTVAL2          
C          ZSCAMP(I)=(-2.D0*DMU/HBC**2.D0/DK**2.D0)*ZTVAL!*(HBC**2*DK/DMU)*TVAL
C          ZSVAL=1.D0-ZI*ZSCAMP(I)*DK*2.D0!DK*2.D0/ZI*ZSCAMP(I)
C          
C          WRITE(57,*) DK,DEI,ZTVAL,ZSVAL
C     &                 
C        ENDDO  
      WRITE(55,*) 
      WRITE(10,*) 
      ENDDO!END TRAGECT GAUSS 
      ENDDO!END TRAGECT COMPLEX SCALLING
      CLOSE(10)
      CLOSE(33)
      CLOSE(55)
      CLOSE(56)
      STOP

9999  CONTINUE
      WRITE(6,*) 'INVALID INPUT PARAMETER'
      STOP
      END PROGRAM MAIN
