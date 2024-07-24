C KKNN POTENTIAL MATRIX ELEMENT 

      SUBROUTINE KINE(ZT,DL,ZA1,ZA2)!KINE(ZT,DL_REL,ZA(I),ZA(J))
      INCLUDE 'LI_INC.f'
      DMH=HBC**2.D0/4.0D0/DMASS
      ZAD=ZA1+ZA2
      ZPR=ZA1*ZA2
      ZT=DMH*(2.D0*DL+3.D0)*ZPR/ZAD*(2.D0*SQRT(ZPR)/ZAD)**(DL+1.5D0)
      RETURN
      END

      SUBROUTINE CENT(ZSW,DL,ZA1,ZA2)!CENT(ZCT,DL_REL,ZA(I),ZA(J))
      INCLUDE 'LI_INC.f'
      VC= -106.09D0
      RC=  0.2009D0
      ZSW=VC*ZINTEG(DL,DL,ZA1,ZA2,0,RC)
      RETURN
      END

      SUBROUTINE COULOMB(ZCLB,DL,ZA1,ZA2)
      INCLUDE 'LI_INC.f'
      DKAP=DSQRT(0.75D0/0.2675D0)
      ZAD=0.5D0*(ZA1+ZA2)
      ZPR=CDSQRT(ZA1*ZA2)
      IL=INT(DL+0.01D0)
      ZCO=1.D0/CDSQRT(1.D0+ZAD*DKAP*DKAP)
      ZSUM=(0.D0,0.D0)
      DO IR=0,IL
        FACL=FAC(IL)/FAC(IR)/FAC(IL-IR)
        ZSUM=ZSUM+(-1.D0)**IR/(2.D0*IR+1.D0)*FACL
     &      *ZCO**(2*IR+1)*ZPR**(IL+1.5D0)/ZAD**(IL+1)
      ENDDO
      ZCLB=ZSUM*4.D0*HBC*AL*2.D0/DSQRT(PI)
      RETURN
      END

      SUBROUTINE COULOMB0(ZCLB0,DL,ZA1,ZA2)
      INCLUDE 'LI_INC.f'
      ZAD=0.5D0*(ZA1+ZA2)
      ZPR=CDSQRT(ZA1*ZA2)
      IL=INT(DL+0.01D0)
      ZCO=1.D0
      ZSUM=(0.D0,0.D0)
      DO IR=0,IL
        FACL=FAC(IL)/FAC(IR)/FAC(IL-IR)
        ZSUM=ZSUM+(-1.D0)**IR/(2.D0*IR+1.D0)*FACL
     &      *ZCO**(2*IR+1)*ZPR**(IL+1.5D0)/ZAD**(IL+1)
      ENDDO
      ZCLB0=ZSUM*4.D0*HBC*AL*2.D0/DSQRT(PI)
      RETURN
      END

      SUBROUTINE ZSEUDO(ZPF,DL,ZA1,ZA2)
      INCLUDE 'LI_INC.f'
      DIMENSION DNF(0:3,0:7,0:3)
      V_PF=1.D+6
      R_PF=4.D0*0.2675D0
      L=IDINT(DL+0.01D0)
      DO NN=0,3
      DO LL=0,7
      DO KK=0,3
        IP=2*LL-NN+2*KK+3
        FCL=0.5D0*(FACLOG(NN)+DFACLOG(LL+NN)-DFACLOG(LL))
     &     -FACLOG(NN-KK)-FACLOG(KK)
        DNF(NN,LL,KK)
     &      =(-1.D0)**KK*DSQRT(2.D0**IP*R_PF**(LL+2*KK+1.5D0))
     &      *DEXP(FCL)
      ENDDO
      ENDDO
      ENDDO
      ZPF=(0.D0,0.D0)

      ITMP=3-L
      IF(ITMP.LE.0) RETURN
      IF(ITMP/2*2.EQ.ITMP) THEN
        NMAX=ITMP/2-1
      ELSE
        NMAX=ITMP/2
      ENDIF
      ZSUM=(0.D0,0.D0)
      DO NN=0,NMAX
        ZSUM1=(0.D0,0.D0)
        ZSUM2=(0.D0,0.D0)
        DO KK=0,NN
          ZSUM1=ZSUM1
     &       +DNF(NN,L,KK)*ZA1**(0.5D0*L+0.75D0)
     &       /(ZA1+R_PF)**(L+KK+1.5D0)
          ZSUM2=ZSUM2
     &       +DNF(NN,L,KK)*ZA2**(0.5D0*L+0.75D0)
     &       /(ZA2+R_PF)**(L+KK+1.5D0)
        ENDDO
        ZSUM=ZSUM+ZSUM1*ZSUM2
      ENDDO
      ZPF=V_PF*ZSUM
      RETURN
      END

C OVERLAP MATRIX ELEMENT BETWEEN H.O. WAVE FUNCTIONS

c      SUBROUTINE HOMAT(ZHO,N1,N2,DL)
c      INCLUDE 'LI_INC.F'
      
C BUCK POTENTIAL MATRIX ELEMENT 

      SUBROUTINE BCENT(ZBK,DL,ZA1,ZA2)!CENT(ZCT,DL_REL,ZA(I),ZA(J))
      INCLUDE 'LI_INC.f'
      VC= -122.6225D0
      RC=  0.22D0
      ZBK=VC*ZINTEG(DL,DL,ZA1,ZA2,0,RC)
      RETURN
      END

      SUBROUTINE BCOULOMB(ZBCLB,DL,ZA1,ZA2)
      INCLUDE 'LI_INC.f'
      DKAP=DSQRT(0.75D0/0.4219D0)
      ZAD=0.5D0*(ZA1+ZA2)
      ZPR=CDSQRT(ZA1*ZA2)
      IL=INT(DL+0.01D0)
      ZCO=1.D0/CDSQRT(1.D0+ZAD*DKAP*DKAP)
      ZSUM=(0.D0,0.D0)
      DO IR=0,IL
        FACL=FAC(IL)/FAC(IR)/FAC(IL-IR)
        ZSUM=ZSUM+(-1.D0)**IR/(2.D0*IR+1.D0)*FACL
     &      *ZCO**(2*IR+1)*ZPR**(IL+1.5D0)/ZAD**(IL+1)
      ENDDO
      ZBCLB=ZSUM*4.D0*HBC*AL*2.D0/DSQRT(PI)
      RETURN
      END

C T-MATRIX FIRST TERM      

      SUBROUTINE CENT_F1(TVAL,DK,DL)
      INCLUDE 'LI_INC.f'
      DIMENSION VC(2),RC(2)
      VC(1)=-96.3D0
      RC(1)= 0.36D0
      VC(2)= 77.0D0
      RC(2)=  0.9D0 
      TVAL=0.D0
      DO I=1,2
      X=DK**2/2.D0/RC(I)
C     TVAL=TVAL+VC(I)*PI**2.D0/2.D0/RC(I)/DK
C    &           *DEXP(-X)*BESSEL_FUC(X,DL+0.5D0)
      TVAL=TVAL
     &+VC(I)/8.D0/PI/RC(I)/DK
     &*DEXP(-X)*BESSEL_FUC(X,DL+0.5D0)
      ENDDO
      RETURN
      END
      
      SUBROUTINE CENL_F1(TVAL,DK,DL)
      INCLUDE 'LI_INC.f'
      DIMENSION VC(3),RC(3)
      VC(1)= 34.0D0
      RC(1)= 0.20D0
      VC(2)=-85.0D0
      RC(2)= 0.53D0
      VC(3)= 51.0D0
      RC(3)= 2.50D0
      TVAL=0.D0
      DO I=1,3
      X=DK**2/2.D0/RC(I)
      TVAL=TVAL
     &+(-1)**DL*VC(I)*PI**2.D0/2.D0/RC(I)/DK
     &*DEXP(-X)*BESSEL_FUC(X,DL+0.5D0)
      ENDDO
      RETURN
      END

      SUBROUTINE LSFRS_F1(TVAL,DK,DL,DJ)
      INCLUDE 'LI_INC.f'
      DIMENSION VC(2),RC(2)
      VA=    -8.4D0
      RA=    0.52D0
      VC(1)= -10.D0
      RC(1)=0.396D0
      VC(2)= 10.0D0
      RC(2)= 2.20D0
      TVAL=0.D0
      DO I=1,2
      X=DK**2/2.D0/RC(I)
      TVAL=TVAL+(1.D0+0.3D0*(-1.D0)**(DL-1.D0))
     &   *VC(I)*PI**2.D0/2.D0/RC(I)/DK*EXP(-X)*BESSEL_FUC(X,DL+0.5D0)
      ENDDO
      X=DK**2/2.D0/RA
      DLS=DJ*(DJ+1)-DL*(DL+1)-0.75D0
      TVAL=DLS*(VA*PI**2.D0/2.D0/RA/DK*EXP(-X)
     &         *BESSEL_FUC(X,DL+0.5D0)+TVAL)
      RETURN
      END
      
C T-MATRIX SECOND TERM

      SUBROUTINE CENTMAT(ZMATC,DK,ZA,DL)
      INCLUDE 'LI_INC.f'
      DIMENSION VC(2),RC(2)
      VC(1)=-96.3D0
      RC(1)= 0.36D0
      VC(2)= 77.0D0
      RC(2)=  0.9D0 
      ZMATC=0.D0
      DO I=1,2
      ZAD=ZA+2.D0*RC(I)
C      ZAPR=SQRT(2.D0/DFAC(DL))*(SQRT(ZA)/ZAD)**(DL+1.5D0)
      ZAPR=SQRT(2.D0**(DL+2)*SQRT(PI)/DFAC(DL))
     &    *(SQRT(ZA)/ZAD)**(DL+1.5D0)
      ZAEXP=EXP(-0.5D0*DK**2.D0/ZAD)
      ZMATC=ZMATC+VC(I)*DK**DL*ZAPR*ZAEXP
      ENDDO
      RETURN
      END

      SUBROUTINE CENLMAT(ZMATCL,DK,ZA,DL)
      INCLUDE 'LI_INC.f'
      DIMENSION VC(3),RC(3)
      VC(1)= 34.0D0
      RC(1)= 0.20D0
      VC(2)=-85.0D0
      RC(2)= 0.53D0
      VC(3)= 51.0D0
      RC(3)= 2.50D0
      ZMATCL=0.D0
      DO I=1,3
      ZAD=ZA+2.D0*RC(I)
      ZAPR=SQRT(2.D0/DFAC(DL))*(SQRT(ZA)/ZAD)**(DL+1.5D0)
      ZAEXP=EXP(-0.5D0*DK**2.D0/ZAD)
      ZMATCL=ZMATCL+(-1.D0)**DL*VC(I)*(2.D0*PI)**1.5D0*
     &               DK**DL*ZAPR*ZAEXP
      ENDDO
      RETURN
      END
      
      SUBROUTINE LSMAT(ZMATLS,DK,ZA,DL,DJ)
      INCLUDE 'LI_INC.f'
      DIMENSION VC(2),RC(2)
      VA=    -8.4D0
      RA=    0.52D0
      VC(1)= -10.D0
      RC(1)=0.396D0
      VC(2)= 10.0D0
      RC(2)= 2.20D0
      ZMATLS=0.D0
      DO I=1,2
      ZAD=ZA+2.D0*RC(I)
      ZAPR=SQRT(2.D0/DFAC(DL))*(SQRT(ZA)/ZAD)**(DL+1.5D0)
      ZAEXP=EXP(-0.5D0*DK**2.D0/ZAD)
      ZMATLS=ZMATLS+(1.D0+0.3D0*(-1.D0)**(DL-1.D0))
     &     *VC(I)*(2.D0*PI)**1.5D0*ZAPR*ZAEXP
      ENDDO
      ZAD=ZA+2.D0*RA
      ZAPR=SQRT(2.D0/DFAC(DL))*(SQRT(ZA)/ZAD)**(DL+1.5D0)
      ZAEXP=EXP(-0.5D0*DK**2.D0/ZAD)
      DLS=DJ*(DJ+1)-DL*(DL+1)-0.75D0
      ZMATLS=DLS*(VA*(2.D0*PI)**1.5D0*ZAPR*ZAEXP+ZMATLS)
      RETURN
      END
