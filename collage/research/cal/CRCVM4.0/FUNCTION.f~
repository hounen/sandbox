C==== RADIAL INTEGRATION ===============================================
      FUNCTION ZGMAT(NU1,NU2,ZBETA1,ZBETA2,ZGAM,LAM)
      INCLUDE 'DIM_INC.f'
      INCLUDE 'GEM_INC.f'
      ZGMAT=(0.D0,0.D0)
      IF(CDABS(ZGAM).LT.1.D-14) THEN
        IF(LAM.NE.0) RETURN
          ZGMAT=PI*DFAC(NU1/2)*DFAC(NU2/2)/2.D0**((NU1+NU2)/2+4)
     &         /ZBETA1**(NU1/2+1.5D0)/ZBETA2**(NU2/2+1.5D0)
        RETURN
      ENDIF
      NNMAX=(NU2-LAM)/2
      ZBETA3=ZGAM*ZGAM/4.D0/ZBETA2
      ZBETA4=ZBETA1-ZBETA3
      ZGMAT=ZGAM**LAM/2.D0*PI
     &     /(ZBETA2*2.D0)**((NU2+LAM)/2+1.5D0)
     &     /(ZBETA4*2.D0)**((NU1+LAM)/2+1.5D0)
      ZSUM=(0.D0,0.D0)
      DO NN=0,NNMAX
        FACL=DFACLOG((NU2+LAM)/2)+FACLOG((NU2-LAM)/2)
     &      -FACLOG((NU2-LAM)/2-NN)-DFACLOG(LAM+NN)-FACLOG(NN)
     &      +DFACLOG((NU1+LAM)/2+NN)
        ZSUM=ZSUM+DEXP(FACL)*(ZBETA3/ZBETA4)**NN
      ENDDO
      ZGMAT=ZGMAT*ZSUM
      RETURN
      END
C=======================================================================

C==== RADIAL INTEGRATION FOR COULOMB FORCE =============================
      FUNCTION ZGCOU(NU1,NU2,ZBETA1,ZBETA2,ZGAM,LAM,CLK)
      INCLUDE 'DIM_INC.f'
      INCLUDE 'GEM_INC.f'
      ZGCOU=(0.D0,0.D0)
      NNMAX=(NU2-LAM)/2
      ZBETA3=ZGAM*ZGAM/4.D0/ZBETA2
      ZBETA4=ZBETA1-ZBETA3
      ZKAP=1.D0+CLK*CLK*ZBETA4
      ZGCOU=ZGAM**LAM/2.D0*PI/DSQRT(2.D0)
     &     /(ZBETA2*2.D0)**((NU2+LAM)/2+1.5D0)
     &     /(ZBETA4*2.D0)**((NU1+LAM)/2+1.0D0)
      ZSUM=(0.D0,0.D0)
      DO NN=0,NNMAX
        FACL=DFACLOG((NU2+LAM)/2)+FACLOG((NU2-LAM)/2)
     &      -FACLOG((NU2-LAM)/2-NN)-DFACLOG(LAM+NN)-FACLOG(NN)
     &      +DFACLOG((NU1+LAM)/2+NN)
        ZSUM2=(0.D0,0.D0)
        DO MM=0,(NU1+LAM)/2+NN
          ZSUM2=ZSUM2
     &         +(-1.D0)**MM/(2*MM+1)/ZKAP**(MM+0.5D0)
     &         *FAC((NU1+LAM)/2+NN)/FAC((NU1+LAM)/2+NN-MM)/FAC(MM)
        ENDDO
        ZSUM=ZSUM+DEXP(FACL)*ZSUM2*(ZBETA3/ZBETA4)**NN
      ENDDO
      ZGCOU=ZGCOU*ZSUM
      RETURN
      END
C=======================================================================

C==== RADIAL INTEGRATION FOR PSEUDO POTENTIAL ==========================
      FUNCTION ZGPAU(NU1,NU2,NU3,ZBETA1,ZBETA2,ZBETA3,
     &               ZGAMA,ZGAMB,LAMA,LAMB)
      INCLUDE 'DIM_INC.f'
      INCLUDE 'GEM_INC.f'
      ZGPAU=(0.D0,0.D0)
      IF(CDABS(ZGAMA).LT.1.D-14.AND.LAMA.NE.0) RETURN
      IF(CDABS(ZGAMB).LT.1.D-14.AND.LAMB.NE.0) RETURN
      NNMAX=(NU1-LAMA)/2
      MMMAX=(NU3-LAMB)/2
      ZBETA4=ZGAMA*ZGAMA/4.D0/ZBETA1
      ZBETA5=ZGAMB*ZGAMB/4.D0/ZBETA3
      ZBETA6=ZBETA2-ZBETA4-ZBETA5
      ZGPAU=ZGAMA**LAMA*ZGAMB**LAMB*PI*DSQRT(PI)/2.D0**1.5D0
     &     /(ZBETA1*2.D0)**((NU1+LAMA)/2+1.5D0)
     &     /(ZBETA3*2.D0)**((NU3+LAMB)/2+1.5D0)
     &     /(ZBETA6*2.D0)**((NU2+LAMA+LAMB)/2+1.5D0)
      ZSUM=(0.D0,0.D0)
      DO NN=0,NNMAX
      DO MM=0,MMMAX
        FACL=DFACLOG((NU1+LAMA)/2)+DFACLOG((NU3+LAMB)/2)
     &      +FACLOG((NU1-LAMA)/2)+FACLOG((NU3-LAMB)/2)
     &      -FACLOG((NU1-LAMA)/2-NN)-FACLOG((NU3-LAMB)/2-MM)
     &      -DFACLOG(LAMA+NN)-DFACLOG(LAMB+MM)-FACLOG(NN)-FACLOG(MM)
     &      +DFACLOG((NU2+LAMA+LAMB)/2+NN+MM)
        ZSUM=ZSUM+DEXP(FACL)*(ZBETA4/ZBETA6)**NN*(ZBETA5/ZBETA6)**MM
      ENDDO
      ENDDO
      ZGPAU=ZGPAU*ZSUM
      RETURN
      END
C=======================================================================
