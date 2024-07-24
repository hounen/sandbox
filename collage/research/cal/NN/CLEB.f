C==== CLEBSCH-GORDAN COEFFICIENT WITH ZERO MAGNETIC QUANTUM NUMBERS ====
      FUNCTION CGZ_I(J1,J2,J3)
      INTEGER I,J1,J2,J3,IG,IA,IB,IC,IFIRST
      REAL*8 CGZ_I,FACL,FLOG,XI
      DIMENSION FLOG(0:100)
      SAVE IFIRST,FLOG
      DATA IFIRST/0/
      CGZ_I=0.D0

      IF(IFIRST-9999.NE.0) THEN
        FLOG(0)=0.D0
        DO I=1,100
          XI=DBLE(I)
          FLOG(I)=FLOG(I-1)+DLOG(XI)
        ENDDO
      ENDIF
      IFIRST=9999

      IG=J1+J2+J3
      IF(IG/2*2.NE.IG)                       RETURN
      IF(IABS(J1-J2).GT.J3.OR.(J1+J2).LT.J3) RETURN
      IA=J1
      IB=J2
      IC=J3
      FACL=FLOG(IG/2)-FLOG(IG/2-IA)-FLOG(IG/2-IB)-FLOG(IG/2-IC)
     &    +0.5D0*(FLOG(IG-2*IA)+FLOG(IG-2*IB)+FLOG(IG-2*IC)-FLOG(IG+1))
      CGZ_I=(-1.D0)**(IG/2-IC)*DSQRT(2.D0*IC+1.D0)*DEXP(FACL)
      RETURN
      END
C=======================================================================
