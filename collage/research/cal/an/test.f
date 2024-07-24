      PROGRAM SIMPSON RULE
      INCLUDE ''
      IMPLICIT REAL*8(A-H,O-Y)
      DIMENSION F(1)

      F(X)=4.0D0/(1.0D0+X**2.0D0)
      A=0.0D0
      B=1.0D0
      N=32
      H=(B-A)/FLOAT(N)
      S1=0.D0
      DO 10 I=2,N-2,2
      S1=S1+F(A+I*H)
10    CONTINUE
      S2=0.D0
      DO 20  I=1,N-1,2
      S2=S2+F(A+I*H)
20    CONTINUE
      S=(F(A)+2.D0*S1+4.D0*S2+F(B))*H/3.D0
      WRITE(*,*) 'S=',S
      CLOSE(22)
      STOP
      END PROGRAM

