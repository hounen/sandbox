C === DIAGO.f ======================================================= 

      SUBROUTINE EIGEN(N,ZG,ZX,IPRINT)

C -------------------------------------------------------------------
      INCLUDE 'LI_INC.f'
      REAL*8 TOLER,ANORM,EPS
      DIMENSION ZG(ND,ND),ZX(ND,ND),ZWK(ND+1,ND/10+5)
C ---<DIAGONALIZATION OF COMPLEX MATRIX>-----------------------------
C     DIAGONALIZATION OF SYMMETRIC COMPLEX N-TH MATRIX ZG(I,J)
C                                  VECTOR OF J-TH BASE ZX(I,J)
C                                  ZG(ND,ND),ZX(ND,ND) ND >= N
C                                  ZWK(NW,5) WORK NW >= ND+1
C     OUTPUT LEVEL
C     IPRINT = 0 NO PRINT OF EIGEN-VALUES & VECTORS
C              1    PRINT OF EIGEN-VALUES & VECTORS
C     NW=ND+1
C     TOLER IS THE RELATIVE PRECISION OF THE MACHINE

      TOLER=1.5D-60
      N1=N-1

      IF(N1) 21,24,23
   24 ZX(1,1)=(1.D0,0.D0)
      RETURN

   23 DO 1 I=1,N
      ZX(I,I)=(1.D0,0.D0)
      IF(I.EQ.N) GO TO 22
      I1=I+1
      DO 1  J=I1, N
      ZX(I,J)=(0.D0,0.D0)
    1 ZX(J,I)=(0.D0,0.D0)
   22 IF(N.EQ.2) GO TO 200

      ZABSB=(0.D0,0.D0)
      N2=N-2

      DO 2 K=1, N2
      ZWK(K,1)=ZG(K,K)
      K1=K+1
      ZSIGMA=(0.D0,0.D0)
      DO 3 I=K1, N
    3 ZSIGMA=ZSIGMA+ZG(I,K)**2

      ZABSB=SQRT(ZSIGMA)
      ZALPHA=ZG(K1,K)
      ZBETA=ZABSB
C      IF(REAL(ZALPHA).GE.0.D0)ZBETA=-ZBETA
      IF(DBLE(ZALPHA).GE.0.D0)ZBETA=-ZBETA
      ZWK(K1,2)=ZBETA
      IF(ABS(ZSIGMA).LE.ABS(TOLER*ZG(K,K))) GO TO 2
      ZGAMMA=1.D0/(ZSIGMA-ZALPHA*ZBETA)
      ZG(K1,K)=ZALPHA-ZBETA

      DO 6 I=K1, N
      ZSUM=(0.0D0,0.0D0)
      DO 7 J=K1,I
    7 ZSUM=ZSUM+ZG(I,J)*ZG(J,K)

      IF(I.EQ.N) GO TO 6
      I1=I+1

      DO 8 J=I1, N
    8 ZSUM=ZSUM+ZG(J,I)*ZG(J,K)

    6 ZWK(I,4)=ZSUM*ZGAMMA
      ZSUM=(0.0D0,0.0D0)

      DO 9 I=K1, N
    9 ZSUM=ZSUM+ZG(I,K)*ZWK(I,4)
      ZT=0.5D0*ZGAMMA*ZSUM

      DO 10 I=K1,N
   10 ZWK(I,4)=ZWK(I,4)-ZT*ZG(I,K)

      DO 11 I=K1, N
      DO 11 J=K1, I
   11 ZG(I,J)=ZG(I,J)-ZG(I,K)*ZWK(J,4)-ZWK(I,4)*ZG(J,K)

      DO 12 I=2,N
      ZSUM=(0.0D0,0.0D0)

      DO 13 J=K1,N
   13 ZSUM=ZSUM+ZX(I,J)*ZG(J,K)
   12 ZWK(I,4)=ZGAMMA*ZSUM

      DO 14 I=2,N
      DO 14 J=K1,N
   14 ZX(I,J)=ZX(I,J)-ZWK(I,4)*ZG(J,K)

    2 CONTINUE

  200 CONTINUE

      ZWK(N1,1)=ZG(N1,N1)
      ZWK(N,1)=ZG(N,N)
      ZWK(N,2)=ZG(N,N1)

      ANORM=0.D0
      ZTA = (0.D0,0.D0)
      ZWK(N+1,2) = (0.D0,0.D0)
      DO 110 K=1,N
      ZT=ABS(ZWK(K,1))+ZTA
      TA=ABS(ZWK(K+1,2))
      ZT=ZT+TA
      IF(DBLE(ZT).GT.ANORM) ANORM=DBLE(ZT)
  110 CONTINUE
      EPS = ANORM*TOLER

      ZAMU=(0.D0,0.D0)
      ZWK(1,2)=(0.D0,0.D0)
      M=N
   15 IF(M.EQ.0) GO TO 21
      I = M-1
      M1= I
      K = I
      IF(ABS(ZWK(K+1,2)).GT.EPS) GO TO 16
      ZAMU=(0.D0,0.D0)
      M = K
      GO TO 15
   16 I=I-1
      IF(ABS(ZWK(I+1,2)).LE.EPS.OR.I.EQ.0) GO TO 18
      K=I
      GO TO 16
   18 ZRAMDA=(0.0D0,0.0D0)
      IF(ABS(ZWK(M,1)-ZAMU).LT.0.5D0*ABS(ZWK(M,1)).OR.M1.EQ.K)
     &  ZRAMDA=ZWK(M,1)+0.5D0*ZWK(M1+1,2)
      ZAMU = ZWK(M,1)
      ZWK(K,1) = ZWK(K,1) - ZRAMDA
      ZBETA = ZWK(K+1,2)

      DO 19 J=K, M1
      ZA0=ZWK(J,1)
      ZA1=ZWK(J+1,1)-ZRAMDA
      ZB0=ZWK(J+1,2)
      ZT=SQRT(ZA0**2+ZBETA**2)
      ZCOSINE=ZA0/ZT
      ZWK(J,4)=ZCOSINE
      ZSINE=ZBETA/ZT
      ZWK(J,5)=ZSINE
      ZWK(J,1)=ZCOSINE*ZA0+ZSINE*ZBETA
      ZWK(J+1,1)=-ZSINE*ZB0+ZCOSINE*ZA1
      ZWK(J+1,2)=ZCOSINE*ZB0+ZSINE*ZA1
      ZBETA=ZWK(J+2,2)
      ZWK(J+2,2)=ZCOSINE*ZBETA
   19 ZWK(J+2,3)=ZSINE*ZBETA
      ZWK(K,2)  =(0.D0,0.D0)
      ZWK(K+1,3)=(0.D0,0.D0)

      DO 20 J=K,M1
      ZSINE=ZWK(J,5)
      ZCOSINE=ZWK(J,4)
      ZA0=ZWK(J,1)
      ZB0=ZWK(J+1,2)
      ZWK(J,2)=ZWK(J,2)*ZCOSINE+ZWK(J+1,3)*ZSINE
      ZWK(J,1)=ZA0*ZCOSINE+ZB0*ZSINE+ZRAMDA
      ZWK(J+1,2)=-ZA0*ZSINE+ZB0*ZCOSINE
      ZWK(J+1,1)=ZWK(J+1,1)*ZCOSINE
      DO 20 I=1,N
      ZX0=ZX(I,J)
      ZX1=ZX(I,J+1)
      ZX(I,J)=ZX0*ZCOSINE+ZX1*ZSINE
   20 ZX(I,J+1)=-ZX0*ZSINE+ZX1*ZCOSINE
      ZWK(M,1)=ZWK(M,1)+ZRAMDA
      GO TO 15
   21 DO 120 M=1,N
  120 ZG(M,M)=ZWK(M,1)
      IF(IPRINT.EQ.0) RETURN    !OUTPUT LEVEL

      DO 170 I=1,N,4
      I6=MIN0(I+3,N)
      WRITE(6,601) (ZG(J,J),J=I,I6)
      DO 160 J=1,N
  160 WRITE(6,602) J,(ZX(J,J9),J9=I,I6)
  170 CONTINUE

C --------------------------------------------------------------------
  601 FORMAT(1H0,4X,4(2X,'(',E13.6,E15.6,')'))
  602 FORMAT(1X,'(',I3,')',F13.6,7F16.6)
      RETURN
      END
C ===================================================================

      SUBROUTINE EIGNORM(N,SS,RMA,RMB)

C -------------------------------------------------------------------
      INCLUDE 'LI_INC.f'
      DIMENSION B(ND,2),RMA(ND,ND),RMB(ND,ND)
C DIAGONALIZATION OF REAL NORM MATRIX ---
C     N : DIMENSION   ,OBTAINED J-TH EIGENVALUE :RMA(I,I)
C     SS: ?           ,OBTAINED J-TH EIGENVECTOR:RMB(I,J)

      IF(N.EQ.1) GO TO 1034
      IND=0
      SUM=0.0D0
      DO 11 I=1,N
      DO 11 J=1,N
      IF(I-J) 10,11,10
   10 SUM=SUM+RMA(I,J)**2
   11 CONTINUE

      SUM=DSQRT(SUM)
      SIGMA=N
      FNORM=SUM/SIGMA
      E=FNORM*SS

      DO 22 I=1,N
      DO 22 J=1,N
      IF(I-J) 21,20,21
   21 RMB(I,J)=0.0D0
      GO TO 22
   20 RMB(I,J)=1.0D0
   22 CONTINUE

  107 J=2
  108 I=1
  109 IF(DABS(RMA(I,J))-FNORM) 122,122,110
  110 IND=1
      R=-RMA(I,J)
      Q=0.5D0*(RMA(I,I)-RMA(J,J))

      IF(Q) 331,330,330
  330 SIGNQ=1.0D0
      GO TO 332

  331 SIGNQ=-1.0D0
  332 W=SIGNQ*R/DSQRT(R**2+Q**2)

      SINE=W/DSQRT(2.0D0*(1.0D0+DSQRT(1.0D0-W**2)))
C     SINE=W/DSQRT(2.0D0*(1.0D0+DSQRT(Q**2/(R**2+Q**2))))
      COSE=DSQRT(1.0D0-SINE**2)

      DO 200 K=1,N
      IF(K-I) 201,200,201
  201 IF(K-J) 202,200,202
  202 B(K,1)=RMA(K,I)*COSE-RMA(K,J)*SINE
      B(K,2)=RMA(K,I)*SINE+RMA(K,J)*COSE
  200 CONTINUE

      DO 210 K=1,N
      IF(K-I) 211,210,211
  211 IF(K-J) 212,210,212
  212 RMA(K,I)=B(K,1)
      RMA(K,J)=B(K,2)
  210 CONTINUE

      DO 220 K=1,N
      B(K,1)=RMB(K,I)*COSE-RMB(K,J)*SINE
  220 B(K,2)=RMB(K,I)*SINE+RMB(K,J)*COSE

      DO 221 K=1,N
      RMB(K,I)=B(K,1)
  221 RMB(K,J)=B(K,2)

      B(1,1)=RMA(I,I)*COSE**2+RMA(J,J)*SINE**2-2.0D0*RMA(I,J)*SINE*COSE
      B(2,2)=RMA(I,I)*SINE**2+RMA(J,J)*COSE**2+2.0D0*RMA(I,J)*SINE*COSE
      B(2,1)=(RMA(I,I)-RMA(J,J))*SINE*COSE+RMA(I,J)*(COSE**2-SINE**2)

      RMA(I,I)=B(1,1)
      RMA(J,J)=B(2,2)
      RMA(J,I)=B(2,1)
      RMA(I,J)=B(2,1)

      DO 230 K=1,N
      IF(K-I) 231,230,231
  231 IF(K-J) 232,230,232
  232 RMA(I,K)=RMA(K,I)
      RMA(J,K)=RMA(K,J)
  230 CONTINUE

  122 IF(I-J+1) 123,124,123
  123 I=I+1
      GO TO 109

  124 IF(J-N) 125,126,125
  125 J=J+1
      GO TO 108

  126 IF(IND-1) 128,127,128
  127 IND=0
      GO TO 107

  128 IF(E-FNORM) 106,129,129
  106 FNORM=FNORM/SIGMA
      GO TO 107

  129 CONTINUE

      RETURN

 1034 RMB(1,1)=1.0D0
      RETURN

      END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE EIGJ(N,G,X,IPRINT)
      INCLUDE 'LI_INC.f'
      DIMENSION A(ND,5),G(ND,ND),X(ND,ND)

C     DIAGONALIZATION OF A REAL SYMMETRIC MATRIX BY HOUSEHOLDER=S METHOD
C     AND THE QR ALGOLTHM
      DOUBLE PRECISION SIGMA, SUM,  NORM,  T
      REAL*8 MU,LAMBDA
      IND=0
C     TOLER IS THE RELATIVE PRECISION OF THE MACHINE
      TOLER=1.5D-13
C     PRELIMINARY ACCOUNTING
      N1=N-1
      IF(N1) 21,24,23
   24 X(1,1)=1.D00
      A(1,1)=G(1,1)
      GO TO 21
   23 IF(IND.EQ.3) GO TO 100
C    SET X TO IDENTITY
      DO 1 I=1,N
      X(I,I)=1.D00
      IF(I.EQ.N) GO TO 22
      I1=I+1
      DO 1  J=I1, N
      X(I,J)=0.0D00
    1 X(J,I)=0.0D00
   22 IF(IND.EQ.2) GO TO 100
      IF(N.EQ.2) GO TO 200
C     REDUCE G TO TRIDIAGONAL FORM USING N-2 ORTHOGONAL TRANSFORMS
C     (I-2WW)=(I-GAMMA*UU)
C     G IS REPLACED BY U BELOW THE MAIN DIAGONAL
C     NORM IS THE INFINITY NORM OF THE REDUCED MATRIX.
      ABSB=0.0D00
      N2=N-2
      DO 2 K=1, N2
      A(K,1)=G(K,K)
      K1=K+1
      SIGMA=0.0D0
      DO 3 I=K1,N
    3 SIGMA=SIGMA+G(I,K)**2
      ABSB=DSQRT(SIGMA)
      ALPHA=G(K1,K)
      BETA=ABSB
      IF(ALPHA.GE.0.0D0)BETA=-BETA
      A(K1,2)=BETA
      IF(SIGMA.EQ.0.0D0) GO TO 2
      GAMMA=1.0D00/(SIGMA-ALPHA*BETA)
      G(K1,K)=ALPHA-BETA
      DO 6 I=K1,N
      SUM=0.0D0
      DO 7 J=K1,I
    7 SUM=SUM+G(I,J)*G(J,K)
      IF(I.EQ.N) GO TO 6
      I1=I+1
      DO 8 J=I1,N
    8 SUM=SUM+G(J,I)*G(J,K)
    6 A(I,4)=SUM*GAMMA
      SUM=0.0D00
      DO 9 I=K1,N
    9 SUM=SUM+G(I,K)*A(I,4)
      T=0.5D00*GAMMA*SUM
      DO 10 I=K1,N
   10 A(I,4)=A(I,4)-T*G(I,K)
      DO 11 I=K1,N
      DO 11 J=K1,I
   11 G(I,J)=G(I,J)-G(I,K)*A(J,4)-A(I,4)*G(J,K)
      DO 12 I=2,N
      SUM=0.0D0
      DO 13 J=K1,N
   13 SUM=SUM+X(I,J)*G(J,K)
   12 A(I,4)=GAMMA*SUM
      DO 14 I=2,N
      DO 14 J=K1,N
   14 X(I,J)=X(I,J)-A(I,4)*G(J,K)
    2 CONTINUE
  200 CONTINUE
      A(N1,1)=G(N1,N1)
      A(N,1)=G(N,N)
      A(N,2)=G(N,N1)
      IF(IND.EQ.1) RETURN
C     CALCULATION OF THE INFINITY NORM OF THE REDUCED MATRIX.
  100 NORM=0.0D00
      TA=0.0D00
      A(N+1,2)=0.0D00
      DO 110 K=1,N
      T=DABS(A(K,1))+TA
      TA=DABS(A(K+1,2))
      T=T+TA
  110 NORM=DMAX1(NORM,T)
      EPS=NORM*TOLER
C     INFINITY NORM TIMES RELATIVE MACHINE PRECISION
      MU=0.0D00
      A(1,2)=0.0D00
      M=N
   15 IF(M.EQ.0) GO TO 21
      I=M-1
      M1=I
      K=I
      IF(DABS(A(K+1,2)).GT.EPS) GO TO 16
      MU=0.0D00
      M=K
      GO TO 15
   16 I=I-1
      IF(DABS(A(I+1,2)).LE.EPS.OR.I.EQ.0) GO TO 18
      K=I
      GO TO 16
   18 LAMBDA=0.0D00
      IF(DABS(A(M,1)-MU).LT.0.5D0*DABS(A(M,1)).OR.M1.EQ.K) LAMBDA=A(M,1)
     1 +0.5D0*A(M1+1,2)
      MU=A(M,1)
      A(K,1)=A(K,1)-LAMBDA
      BETA=A(K+1,2)
C     TRANSFORMATION ON THE LEFT
      DO 19 J=K,M1
      A0=A(J,1)
      A1=A(J+1,1)-LAMBDA
      B0=A(J+1,2)
      T=DSQRT(A0**2+BETA**2)
      COSINE=A0/T
      A(J,4)=COSINE
      SINE=BETA/T
      A(J,5)= SINE
      A(J,1)=COSINE*A0+ SINE*BETA
      A(J+1,1)=- SINE*B0+COSINE*A1
      A(J+1,2)=COSINE*B0+ SINE*A1
      BETA=A(J+2,2)
      A(J+2,2)=COSINE*BETA
   19 A(J+2,3)= SINE*BETA
      A(K,2)=0.0D00
      A(K+1,3)=0.0D00
C     TRAN SFORMATION ON THE RIGHT
      DO 20 J=K,M1
      SINE=A(J,5)
      COSINE=A(J,4)
      A0=A(J,1)
      B0=A(J+1,2)
      A(J,2)=A(J,2)*COSINE+A(J+1,3)* SINE
      A(J,1)=A0*COSINE+B0* SINE+LAMBDA
      A(J+1,2)=-A0* SINE+B0*COSINE
      A(J+1,1)=A(J+1,1)*COSINE
      DO 20 I=1,N
      X0=X(I,J)
      X1=X(I,J+1)
      X(I,J)=X0*COSINE+X1*SINE
   20 X(I,J+1)=-X0*SINE+X1*COSINE
      A(M,1)=A(M,1)+LAMBDA
      GO TO 15
   21 IF(IND.NE.0) RETURN
      DO 120 M=1,N
  120 G(M,M)=A(M,1)
      IF(IPRINT.EQ.0) RETURN
      DO 170 I=1,N,7
      I6=MIN0(I+6,N)
      WRITE(6,601) (G(J,J),J=I,I6)
      DO 160 J=1,N
  160 WRITE(6,602) J,(X (J,J9),J9=I,I6)
  170 CONTINUE
  601 FORMAT(1H0,8X,  7E16.6)
  602 FORMAT(7X,1H(,I2,1H),F12.6,6F16.6)
      RETURN
      END

