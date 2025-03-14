      FUNCTION ZNF(DL,ZA) 
      INCLUDE 'LI_INC.f'
      IL=INT(DL+0.01D0)
      ZNF=ZA**(DL+1.5D0)*2.D0**(DL+2.D0)/SQRT(PI)/DFAC(IL)
      ZNF=CDSQRT(ZNF)
      RETURN
      END
      
C NORM
      FUNCTION ZNORM(DL1,DL2,ZA1,ZA2)
      INCLUDE 'LI_INC.f'
      ZAD=ZA1+ZA2
      ZPR=ZA1*ZA2
      ZNORM=(0.D0,0.D0)
      IF(DL1.NE.DL2) RETURN
      ZNORM=(2.D0*CDSQRT(ZPR)/ZAD)**(0.5D0*(DL1+DL2+3.D0))
      RETURN
      END

C EXP(-��R**2)
      FUNCTION ZINTEG(DL1,DL2,ZA1,ZA2,N,DMU)
      INCLUDE 'LI_INC.f'
      ZINTEG=(0.D0,0.D0)
      IF(DL1.NE.DL2) RETURN
      IF(N.NE.0) GOTO 1000
      ZAD=ZA1+ZA2+2.D0*DMU
      ZPR=ZA1*ZA2
      ZINTEG=(2.D0*CDSQRT(ZPR)/ZAD)**(0.5D0*(DL1+DL2+3.D0))
      RETURN
1000  CONTINUE
      NN=INT(DL1+DL2+0.01D0)+N+2
      NN=NN/2
      ZA=0.5D0*(ZA1+ZA2)+DMU
      ZFAC=ZNF(DL,ZA1)*ZNF(DL,ZA2)
      IF(N/2*2.NE.N) GOTO 1001
      ZINTEG=ZFAC*DFAC(NN-1)*SQRT(PI/ZA**(2*NN+1))/2.D0**(NN+1)
      RETURN
1001  CONTINUE ! N IS ODD
      ZINTEG=ZFAC*FAC(NN)/2.D0/ZA**(NN+1)
      RETURN
      END

C------WAVE FUNCTION---------------------------------------------------
      FUNCTION PHI(RAD,NZ,ZC,J,A0,GAM,DL)
      INCLUDE 'LI_INC.f'
      ZPHI=(0.D0,0.D0)
       DO K=1,NZ
         ZPHI=ZPHI+ZC(K,J)*RAD**DL*DEXP(-A0*GAM**(K-1)*RAD**2.D0/2.D0)
       ENDDO
         PHI=(CDABS(ZPHI))**2.D0
      END

C----------------------------------------------------------------------
      FUNCTION ZSIMPSON(ZDATA,IMESH,DMESH)
      INCLUDE 'LI_INC.f'
      DIMENSION ZDATA(0:10000)

      IF(IMESH/2*2.NE.IMESH) GOTO 9999
      ZSIMPSON=ZDATA(0)-ZDATA(IMESH)
      DO 5000 I=1,IMESH/2
      IO=2*I-1
      IE=2*I
 5000 ZSIMPSON=ZSIMPSON+4.D0*ZDATA(IO)+2.D0*ZDATA(IE)
      ZSIMPSON=ZSIMPSON/3.D0*DMESH
      RETURN 
C
C ERROR MESSAGE
C
 9999 WRITE(NOUT,*) '!!! NUMBER OF IMESH IS INVALID !!!'
      STOP
      END

      
C Gamma Function
      SUBROUTINE SET_FAC()
      INCLUDE 'LI_INC.f'
      FAC(0)=1.D0!L!
      DFAC(0)=1.D0!(2L+1)!!
c      DEFAC(0)=1.D0!(2L)!!
      FACLOG(0)=0.D0
      DFACLOG(0)=0.D0
      DO I=1,20
        FAC(I)=FAC(I-1)*I
        DFAC(I)=DFAC(I-1)*(2.D0*I+1.D0)
c        DEFAC(I)=2.D0**I*FAC(I)
        FACLOG(I)=FACLOG(I-1)+DLOG(DBLE(I))
        DFACLOG(I)=DFACLOG(I-1)+DLOG(2.D0*I+1.D0)
      ENDDO
      RETURN
      END
