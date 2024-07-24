C KKNN POTENTIAL MATRIX ELEMENT 

      SUBROUTINE KINE(ZT,DL,ZA1,ZA2)!KINE(ZT,DL_REL,ZA(I),ZA(J))
      INCLUDE 'LI_INC.f'
      DMH=HBC**2.D0/1.6D0/DMASS
      ZAD=ZA1+ZA2
      ZPR=ZA1*ZA2
      ZT=DMH*(2.D0*DL+3.D0)*ZPR/ZAD*(2.D0*SQRT(ZPR)/ZAD)**(DL+1.5D0)
      RETURN
      END

      SUBROUTINE CENT(ZC,DL,ZA1,ZA2)!CENT(ZCT,DL_REL,ZA(I),ZA(J))
      INCLUDE 'LI_INC.f'
      DIMENSION VC(2),RC(2)
      VC(1)=-96.3D0
      RC(1)= 0.36D0
      VC(2)= 77.0D0
      RC(2)=  0.9D0
      ZC=(0.D0,0.D0)
      DO I=1,2
      ZC=ZC+VC(I)*ZINTEG(DL,DL,ZA1,ZA2,0,RC(I))!0とは？
      ENDDO
      RETURN
      END

      SUBROUTINE CENL(ZCL,DL,ZA1,ZA2)
      INCLUDE 'LI_INC.f'
      DIMENSION VC(3),RC(3)
      VC(1)= 34.0D0
      RC(1)= 0.20D0
      VC(2)=-85.0D0
      RC(2)= 0.53D0
      VC(3)= 51.0D0
      RC(3)= 2.50D0
      ZCL=(0.D0,0.D0)
      DO I=1,3
      ZCL=ZCL
     &+(-1)**DL*VC(I)*ZINTEG(DL,DL,ZA1,ZA2,0,RC(I))!0とは？
      ENDDO
      RETURN
      END

      SUBROUTINE LSFRS(ZCLS,DL,DJ,ZA1,ZA2)
      INCLUDE 'LI_INC.f'
      DIMENSION VC(2),RC(2)
      VA=    -8.4D0
      RA=    0.52D0
      VC(1)= -10.D0
      RC(1)=0.396D0
      VC(2)= 10.0D0
      RC(2)= 2.20D0
      ZC_TOT=(0.D0,0.D0)
      DO I=1,2
      ZC_TOT=ZC_TOT+VC(I)*ZINTEG(DL,DL,ZA1,ZA2,0,RC(I))
      ENDDO
      DLS=DJ*(DJ+1)-DL*(DL+1)-0.75D0
      ZCLS=DLS*(VA*ZINTEG(DL,DL,ZA1,ZA2,0,RA)
     &    +(1.D0+0.3D0*(-1.D0)**(DL-1.D0))*ZC_TOT)
      RETURN
      END

      SUBROUTINE COULOMB(ZCLB,DL,ZA1,ZA2)
      INCLUDE 'LI_INC.f'
      
      DKAP=DSQRT(0.75D0)*1.4D0
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
      ZCLB=ZSUM*2.D0*HBC*AL*2.D0/DSQRT(PI)
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
      ZCLB0=ZSUM*2.D0*HBC*AL*2.D0/DSQRT(PI)
      RETURN
      END

C SBB POTENTIAL MATRIX ELEMENT

      SUBROUTINE WKINE(ZWT,DL,ZA1,ZA2)
      INCLUDE 'LI_INC.f'
      DMH=HBC**2.D0/1.6D0/DMASS
      ZAD=ZA1+ZA2
      ZPR=ZA1*ZA2
      ZWT=DMH*(2.D0*DL+3.D0)*ZPR/ZAD*(2.D0*SQRT(ZPR)/ZAD)**(DL+1.5D0)
      RETURN
      END

      SUBROUTINE WCENT(ZWCT,DL,ZA1,ZA2,IME,DIS)
      INCLUDE 'LI_INC.f'
      DIMENSION ZDATA(0:10000)
      ZAD=ZA1+ZA2
      ZFAC=ZNF(DL,ZA1)*ZNF(DL,ZA2)
      DME=DIS/IME
      DO IR=0,IME
      DR=DME*IR
      WS=-43.0D0/(1.D0+DEXP((DR-2.D0)/0.7D0))
      ZDATA(IR)=DR**(2.D0*DL+2.D0)*WS*CDEXP(-ZAD*DR**2.D0/2.D0)
      ENDDO
      ZWCT=ZFAC*ZSIMPSON(ZDATA,IME,DME)
      RETURN
      END

      SUBROUTINE WLSFRS1(ZWLS,DL,DJ,ZA1,ZA2,IME,DIS)
      !Used partial integrate method
      INCLUDE 'LI_INC.f'
      DIMENSION ZDATA(0:10000)
      ZAD=ZA1+ZA2
      ZFAC=ZNF(DL,ZA1)*ZNF(DL,ZA2)
      DLS=0.5D0*(DJ*(DJ+1.D0)-DL*(DL+1.D0)-0.75D0)
      DME=DIS/IME
      DO IR=0,IME
      DR=DME*IR
      WS=40.D0/(1.D0+DEXP((DR-1.5D0)/0.35D0))
      ZGWS=DR**(2.D0*DL)*WS*CDEXP(-ZAD*DR**2.D0/2.D0)
      ZDATA(IR)=(2.D0*DL+1.D0-ZAD*DR**2.D0)*ZGWS
      ENDDO
      ZGWINTEG=DIS**(2.D0*DL+1.D0)*WS*CDEXP(-ZAD*DIS**2.D0/2.D0)
      ZWLS=ZFAC*DLS*(ZGWINTEG-ZSIMPSON(ZDATA,IME,DME))
      RETURN
      END

      SUBROUTINE WLSFRS2(ZWLS,DL,DJ,ZA1,ZA2,IME,DIS)
      !Used difference method
      INCLUDE 'LI_INC.f'
      DIMENSION ZDATA(0:10000),R(0:10000)
      ZAD=ZA1+ZA2
      ZFAC=ZNF(DL,ZA1)*ZNF(DL,ZA2)
      DLS=0.5D0*(DJ*(DJ+1.D0)-DL*(DL+1.D0)-0.75D0)
      DME=DIS/IME
      DO IR=0,IME
      DR=DME*IR
      R(IR)=40.D0/(1.D0+DEXP((DR-1.5D0)/0.35D0))
      ENDDO
      DO IR=0,IME
      DR=DME*IR
      ZDATA(IR)=DR**(2.D0*DL+1.D0)*CDEXP(-ZAD/2.D0*DR**2.D0)
     &          *(R(IR+1)-R(IR))/DME 
      ENDDO
      ZWLS=ZFAC*DLS*ZSIMPSON(ZDATA,IME,DME)
      RETURN
      END

      SUBROUTINE WLSFRS3(ZWLS,DL,DJ,ZA1,ZA2,IME,DIS)
      !Used difference method
      !Wave function is differentiated with respect to [r]
      INCLUDE 'LI_INC.f'
      DIMENSION ZDATA(0:10000),ZR(0:10000)
      ZFAC=ZNF(DL,ZA1)*ZNF(DL,ZA2)
      DLS=0.5D0*(DJ*(DJ+1.D0)-DL*(DL+1.D0)-0.75D0)
      DME=DIS/IME
      DO IR=0,IME
      DR=DME*IR
      ZR(IR)=40.D0/(1.D0+DEXP((DR-1.5D0)/0.35D0))
     &           *DR**DL*CDEXP(-ZA2/2.D0*DR**2.D0)
      ENDDO
      DO IR=0,IME
      DR=DME*IR
      ZDATA(IR)=DR**(DL+1.D0)*CDEXP(-ZA1/2.D0*DR**2.D0)
     &          *(ZR(IR+1)-ZR(IR))/DME 
      ENDDO
      ZWLS=ZFAC*DLS*ZSIMPSON(ZDATA,IME,DME)
      RETURN
      END

      SUBROUTINE PSEUDO(ZPF,DL,ZA1,ZA2)
      INCLUDE 'LI_INC.f'
      ZAPF=0.8D0/1.4D0**2
      ZPF=1.D6*ZNORM(DL,0.D0,ZA1,ZAPF)*ZNORM(0.D0,DL,ZAPF,ZA2)
      RETURN
      END 

C T-MATRIX FIRST TERM      

      SUBROUTINE CENT_F1(TVAL,DK,DL)!中心力
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
      
      SUBROUTINE CENL_F1(TVAL,DK,DL)!角運動量依存の中心力
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

      SUBROUTINE LSFRS_F1(TVAL,DK,DL,DJ)!LS力
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
