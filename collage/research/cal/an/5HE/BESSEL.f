      FUNCTION BESSEL_FUC(X,XNU)
      DOUBLE PRECISION X,XNU,BESSEL_FUC
      DOUBLE PRECISION RI,RK,RIP,RKP
      CALL BESSIK(X,XNU,RI,RK,RIP,RKP)
      BESSEL_FUC=RI
      RETURN
      END
      
      SUBROUTINE BESSIK(X,XNU,RI,RK,RIP,RKP)
      INTEGER MAXIT
      DOUBLE PRECISION RI,RIP,RK,RKP,X,XNU,XMIN
      DOUBLE PRECISION EPS,FPMIN,PI
      PARAMETER (EPS=1.D-10,FPMIN=1.D-30,MAXIT=10000,XMIN=2.D0,
     & PI=3.141592653589793D0)
      INTEGER I,L,NL
      DOUBLE PRECISION A,A1,B,C,D,DEL,DEL1,DELH,DELS,E,F,FACT,
     & FACT2,FF,GAM1,GAM2,GAMMI,GAMPL,H,P,PIMU,Q,Q1,Q2,
     & QNEW,RIL,RIL1,RIMU,RIP1,RIPL,RITEMP,RK1,RKMU,RKMUP,
     & RKTEMP,S,SUM,SUM1,X2,XI,XI2,XMU,XMU2
      IF(X.LE.0.D0.OR.XNU.LT.0.D0) THEN
        WRITE(*,*) 'BAD ARGUMENTS IN BESSIK'
        STOP
      ENDIF
      NL=INT(XNU+0.5D0)
      XMU=XNU-NL
      XMU2=XMU*XMU
      XI=1.D0/X
      XI2=2.D0*XI
      H=XNU*XI
      IF(H.LT.FPMIN) H=FPMIN
      B=XI2*XNU
      D=0.D0
      C=H
      DO I=1,MAXIT
        B=B+XI2
        D=1.D0/(B+D)
        C=B+1.D0/C
        DEL=C*D
        H=DEL*H
        IF(ABS(DEL-1.D0).LT.EPS) GOTO 1
      ENDDO
      WRITE(*,*) 'X TOO LARGE IN BESSIK; TRY ASYMPTOTIC EXPANSION'
      STOP
    1 CONTINUE
      RIL=FPMIN
      RIPL=H*RIL
      RIL1=RIL
      RIP1=RIPL
      FACT=XNU*XI
      DO L=NL,1,-1
        RITEMP=FACT*RIL+RIPL
        FACT=FACT-XI
        RIPL=FACT*RITEMP+RIL
        RIL=RITEMP
      ENDDO
      F=RIPL/RIL
      IF(X.LT.XMIN) THEN
        X2=.5D0*X
        PIMU=PI*XMU
        IF(ABS(PIMU).LT.EPS)THEN
          FACT=1.D0
        ELSE
          FACT=PIMU/SIN(PIMU)
        ENDIF
        D=-LOG(X2)
        E=XMU*D
        IF(ABS(E).LT.EPS)THEN
          FACT2=1.D0
        ELSE
          FACT2=SINH(E)/E
        ENDIF
        CALL BESCHB(XMU,GAM1,GAM2,GAMPL,GAMMI)
        FF=FACT*(GAM1*COSH(E)+GAM2*FACT2*D) 
        SUM=FF
        E=EXP(E)
        P=0.5D0*E/GAMPL
        Q=0.5D0/(E*GAMMI)
        C=1.D0
        D=X2*X2
        SUM1=P
        DO I=1,MAXIT
          FF=(I*FF+P+Q)/(I*I-XMU2)
          C=C*D/I
          P=P/(I-XMU)
          Q=Q/(I+XMU)
          DEL=C*FF
          SUM=SUM+DEL
          DEL1=C*(P-I*FF)
          SUM1=SUM1+DEL1
          IF(ABS(DEL).LT.ABS(SUM)*EPS) GOTO 2
        ENDDO
        WRITE(*,*) 'BESSK SERIES FAILED TO CONVERGE'
        STOP
    2   CONTINUE
        RKMU=SUM
        RK1=SUM1*XI2
      ELSE
        B=2.D0*(1.D0+X)
        D=1.D0/B
        DELH=D
        H=DELH
        Q1=0.D0
        Q2=1.D0
        A1=.25D0-XMU2
        C=A1
        Q=C
        A=-A1
        S=1.D0+Q*DELH
        DO I=2,MAXIT
          A=A-2*(I-1)
          C=-A*C/I
          QNEW=(Q1-B*Q2)/A
          Q1=Q2
          Q2=QNEW
          Q=Q+C*QNEW
          B=B+2.D0
          D=1.D0/(B+A*D)
          DELH=(B*D-1.D0)*DELH
          H=H+DELH
          DELS=Q*DELH
          S=S+DELS
          IF(ABS(DELS/S).LT.EPS) GOTO 3
        ENDDO
        WRITE(*,*) 'BESSIK: FAILURE TO CONVERGE IN CF2'
        STOP
    3   CONTINUE
        H=A1*H
        RKMU=SQRT(PI/(2.D0*X))*EXP(-X)/S
        RK1=RKMU*(XMU+X+.5D0-H)*XI
      ENDIF
      RKMUP=XMU*XI*RKMU-RK1
      RIMU=XI/(F*RKMU-RKMUP)
      RI=(RIMU*RIL1)/RIL
      RIP=(RIMU*RIP1)/RIL
      DO I=1,NL
        RKTEMP=(XMU+I)*XI2*RK1+RKMU
        RKMU=RK1
        RK1=RKTEMP
      ENDDO
      RK=RKMU
      RKP=XNU*XI*RKMU-RK1
      RETURN
      END

      SUBROUTINE BESCHB(X,GAM1,GAM2,GAMPL,GAMMI)
      INTEGER NUSE1,NUSE2
      DOUBLE PRECISION GAM1,GAM2,GAMMI,GAMPL,X
      PARAMETER (NUSE1=5,NUSE2=5)
      DOUBLE PRECISION XX,C1(7),C2(8),CHEBEV
      SAVE C1,C2
      DATA C1/-1.142022680371168D0,6.5165112670737D-3,
     &     3.087090173086D-4,-3.4706269649D-6,6.9437664D-9,
     &     3.67795D-11,-1.356D-13/
      DATA C2/1.843740587300905D0,-7.68528408447867D-2,
     &     1.2719271366546D-3,-4.9717367042D-6,-3.31261198D-8,
     &     2.423096D-10,-1.702D-13,-1.49D-15/
      XX=8.D0*X*X-1.D0
      GAM1=CHEBEV(-1.D0,1.D0,C1,NUSE1,XX)
      GAM2=CHEBEV(-1.D0,1.D0,C2,NUSE2,XX)
      GAMPL=GAM2-X*GAM1
      GAMMI=GAM2+X*GAM1
      RETURN
      END

      FUNCTION CHEBEV(A,B,C,M,X)
      INTEGER M
      DOUBLE PRECISION CHEBEV,A,B,X,C(M)
      INTEGER J
      DOUBLE PRECISION D,DD,SV,Y,Y2
      IF((X-A)*(X-B).GT.0.D0) PAUSE 'X NOT IN RANGE IN CHEBEV'
      D=0.D0
      DD=0.D0
      Y=(2.D0*X-A-B)/(B-A)
      Y2=2.D0*Y
      DO J=M,2,-1
         SV=D
         D=Y2*D-DD+C(J)
         DD=SV
      ENDDO
      CHEBEV=Y*D-DD+0.5D0*C(1)
      RETURN
      END

      
      
      
      








      
