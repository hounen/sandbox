!==== CLEBSCH-GORDAN COEFFICIENT ==============================================
  MODULE CLEB_DEF
    IMPLICIT NONE
    INTERFACE CGZ
      DOUBLE PRECISION FUNCTION CGZ_I(J1,J2,J3)
        INTEGER,INTENT(IN) :: J1,J2,J3
      END FUNCTION CGZ_I
      DOUBLE PRECISION FUNCTION CGZ_D(J1,J2,J3)
        DOUBLE PRECISION,INTENT(IN) :: J1,J2,J3
      END FUNCTION CGZ_D
    END INTERFACE CGZ
    INTERFACE CLEB
      DOUBLE PRECISION FUNCTION CLEB_I(J1,M1,J2,M2,J3,M3)
        INTEGER,INTENT(IN) :: J1,J2,J3,M1,M2,M3
      END FUNCTION CLEB_I
      DOUBLE PRECISION FUNCTION CLEB_D(J1,M1,J2,M2,J3,M3)
        DOUBLE PRECISION,INTENT(IN) :: J1,J2,J3,M1,M2,M3
      END FUNCTION CLEB_D
    END INTERFACE CLEB
  END MODULE CLEB_DEF
!==============================================================================

!==== CLEBSCH-GORDON COEFFICIENT WITH ZERO MAGNETIC QUANTUM NUMBERS ===========
  DOUBLE PRECISION FUNCTION CGZ_I(J1,J2,J3)
    USE FACTORIAL
    IMPLICIT NONE
    INTEGER,INTENT(IN) :: J1,J2,J3
    INTEGER :: IG,IA,IB,IC
    DOUBLE PRECISION :: FACL
    CGZ_I=0.D0

    CALL SET_FACT      
    IF(MIN0(J1,J2,J3).LT.0)              RETURN
    IF(IABS(J1-J2).GT.J3.OR.J1+J2.LT.J3) RETURN
    IG=J1+J2+J3
    IA=J1
    IB=J2
    IC=J3
    IF(IG/2*2.NE.IG) RETURN
    FACL=FACLOG(IG/2)-FACLOG(IG/2-IA)-FACLOG(IG/2-IB)-FACLOG(IG/2-IC)         &
    &   +0.5*(FACLOG(IG-2*IA)+FACLOG(IG-2*IB)+FACLOG(IG-2*IC)-FACLOG(IG+1))
    CGZ_I=(-1.D0)**(IG/2-IC)*DSQRT(2.D0*IC+1.D0)*DEXP(FACL)
    RETURN
  END FUNCTION CGZ_I
!==============================================================================

!==== CLEBSCH-GORDON COEFFICIENT WITH ZERO MAGNETIC QUANTUM NUMBERS ===========
  DOUBLE PRECISION FUNCTION CGZ_D(J1,J2,J3)
    USE FACTORIAL
    IMPLICIT NONE
    DOUBLE PRECISION,INTENT(IN) :: J1,J2,J3
    INTEGER :: IG,IA,IB,IC
    DOUBLE PRECISION :: FACL
    CGZ_D=0.D0

    CALL SET_FACT      
    IF(DMIN1(J1,J2,J3).LT.0.D0) RETURN
    IG=IDINT(J1+J2+J3+0.01D0)
    IA=IDINT(J1+0.01D0)
    IB=IDINT(J2+0.01D0)
    IC=IDINT(J3+0.01D0)
    IF(IG/2*2.NE.IG) RETURN
    FACL=FACLOG(IG/2)-FACLOG(IG/2-IA)-FACLOG(IG/2-IB)-FACLOG(IG/2-IC)         &
    &   +0.5*(FACLOG(IG-2*IA)+FACLOG(IG-2*IB)+FACLOG(IG-2*IC)-FACLOG(IG+1))
    CGZ_D=(-1.D0)**(IG/2-IC)*DSQRT(2.D0*IC+1.D0)*DEXP(FACL)
    RETURN
  END FUNCTION CGZ_D
!==============================================================================

!==== CLEBSCH-GORDAN COEFFICIENT FOR INTEGER ARGUMENTS ========================
  DOUBLE PRECISION FUNCTION CLEB_I(J1,M1,J2,M2,J3,M3)
    USE FACTORIAL
    IMPLICIT NONE
    INTEGER,INTENT(IN) :: J1,J2,J3,M1,M2,M3
    INTEGER :: IJ1,IJ2,IJ3,IM1,IM2,IM3
    INTEGER :: I,IIA,IIB,IIC,IID,II1,II2,II3,II4,II5,II6
    INTEGER :: KK1,KK2,KK3,KK4,KK5,KK6,KMAX,KMIN
    DOUBLE PRECISION :: SQFLG,CLG,CGZ_I
    CLEB_I=0.D0
    
    CALL SET_FACT
    IF(MIN0(J1,J2,J3).LT.0) RETURN
    IJ1=2*J1
    IJ2=2*J2
    IJ3=2*J3
    IM1=2*M1
    IM2=2*M2
    IM3=2*M3
    IF(IABS(IM1).GT.IJ1) RETURN
    IF(IABS(IM2).GT.IJ2) RETURN
    IF(IABS(IM3).GT.IJ3) RETURN
    IF((IJ1+IM1)/2*2.NE.IJ1+IM1) RETURN
    IF((IJ2+IM2)/2*2.NE.IJ2+IM2) RETURN
    IF((IJ3+IM3)/2*2.NE.IJ3+IM3) RETURN
    IF(IABS(IJ1-IJ2).GT.IJ3.OR.IJ1+IJ2.LT.IJ3) RETURN

    IF(IJ1.EQ.0.OR.IJ2.EQ.0) THEN
      CLEB_I=1.D0
      RETURN
    ELSEIF(IJ3.EQ.0) THEN
      CLEB_I=(-1.D0)**((IJ1-IM1)/2)/DSQRT(2.D0*J1+1.D0)
      RETURN
    ELSEIF(IABS(IM1)+IABS(IM2)+IABS(IM3).EQ.0) THEN
      CLEB_I=CGZ_I(J1,J2,J3)
    ENDIF

    IIA=(IJ1+IJ2+IJ3)/2
    IIB=IIA-IJ1
    IIC=IIA-IJ2
    IID=IIA-IJ3
    IIA=IIA+1

    II1=(IJ1+IM1)/2
    II2=II1-IM1
    II3=(IJ2+IM2)/2
    II4=II3-IM2
    II5=(IJ3+IM3)/2
    II6=II5-IM3

    SQFLG=0.5D0*(DLOG(IJ3+1.D0)                                               &
    &           -FACLOG(IIA)+FACLOG(IIB)+FACLOG(IIC)+FACLOG(IID)              &
    &           +FACLOG(II1)+FACLOG(II2)+FACLOG(II3)                          &
    &           +FACLOG(II4)+FACLOG(II5)+FACLOG(II6))

    KK2=(IJ1+IJ2-IJ3)/2
    KK3=(IJ1-IM1)/2
    KK4=(IJ2+IM2)/2
    KK5=(IJ2-IM1-IJ3)/2
    KK6=(IJ1+IM2-IJ3)/2
    KMAX=MIN0(KK2,KK3,KK4)
    KMIN=MAX0(0,KK5,KK6)
    IF(KMIN.GT.KMAX) RETURN
    DO KK1=KMIN,KMAX
      CLG=SQFLG-FACLOG(KK1)-FACLOG(KK2-KK1)-FACLOG(KK3-KK1)-FACLOG(KK4-KK1)   &
    &    -FACLOG(KK1-KK5)-FACLOG(KK1-KK6)
      CLEB_I=CLEB_I+(-1.D0)**KK1*DEXP(CLG)
    ENDDO
    RETURN
  END FUNCTION CLEB_I
!==============================================================================

!==== CLEBSCH-GORDAN COEFFICIENT FOR DOUBLE PRECISION ARGUMENTS ===============
  DOUBLE PRECISION FUNCTION CLEB_D(J1,M1,J2,M2,J3,M3)
    USE FACTORIAL
    IMPLICIT NONE
    DOUBLE PRECISION,INTENT(IN) :: J1,J2,J3,M1,M2,M3
    INTEGER :: IJ1,IJ2,IJ3,IM1,IM2,IM3
    INTEGER :: I,IIA,IIB,IIC,IID,II1,II2,II3,II4,II5,II6
    INTEGER :: KK1,KK2,KK3,KK4,KK5,KK6,KMAX,KMIN
    DOUBLE PRECISION :: SQFLG,CLG,CGZ_D
    CLEB_D=0.D0
    
    CALL SET_FACT
    IF(DMIN1(J1,J2,J3).LT.0.D0) RETURN
    IJ1=IDINT(2.D0*J1+0.01D0)
    IJ2=IDINT(2.D0*J2+0.01D0)
    IJ3=IDINT(2.D0*J3+0.01D0)
    IM1=IDINT(2.D0*M1+0.01D0)
    IM2=IDINT(2.D0*M2+0.01D0)
    IM3=IDINT(2.D0*M3+0.01D0)
    IF(IABS(IM1).GT.IJ1) RETURN
    IF(IABS(IM2).GT.IJ2) RETURN
    IF(IABS(IM3).GT.IJ3) RETURN
    IF((IJ1+IM1)/2*2.NE.IJ1+IM1) RETURN
    IF((IJ2+IM2)/2*2.NE.IJ2+IM2) RETURN
    IF((IJ3+IM3)/2*2.NE.IJ3+IM3) RETURN
    IF(IABS(IJ1-IJ2).GT.IJ3.OR.IJ1+IJ2.LT.IJ3) RETURN

    IF(IJ1.EQ.0.OR.IJ2.EQ.0) THEN
      CLEB_D=1.D0
      RETURN
    ELSEIF(IJ3.EQ.0) THEN
      CLEB_D=(-1.D0)**((IJ1-IM1)/2)/DSQRT(2.D0*J1+1.D0)
      RETURN
    ELSEIF(IABS(IM1)+IABS(IM2)+IABS(IM3).EQ.0) THEN
      CLEB_D=CGZ_D(J1,J2,J3)
    ENDIF

    IIA=(IJ1+IJ2+IJ3)/2
    IIB=IIA-IJ1
    IIC=IIA-IJ2
    IID=IIA-IJ3
    IIA=IIA+1

    II1=(IJ1+IM1)/2
    II2=II1-IM1
    II3=(IJ2+IM2)/2
    II4=II3-IM2
    II5=(IJ3+IM3)/2
    II6=II5-IM3

    SQFLG=0.5D0*(DLOG(IJ3+1.D0)                                               &
    &           -FACLOG(IIA)+FACLOG(IIB)+FACLOG(IIC)+FACLOG(IID)              &
    &           +FACLOG(II1)+FACLOG(II2)+FACLOG(II3)                          &
    &           +FACLOG(II4)+FACLOG(II5)+FACLOG(II6))

    KK2=(IJ1+IJ2-IJ3)/2
    KK3=(IJ1-IM1)/2
    KK4=(IJ2+IM2)/2
    KK5=(IJ2-IM1-IJ3)/2
    KK6=(IJ1+IM2-IJ3)/2
    KMAX=MIN0(KK2,KK3,KK4)
    KMIN=MAX0(0,KK5,KK6)
    IF(KMIN.GT.KMAX) RETURN
    DO KK1=KMIN,KMAX
      CLG=SQFLG-FACLOG(KK1)-FACLOG(KK2-KK1)-FACLOG(KK3-KK1)-FACLOG(KK4-KK1)   &
    &    -FACLOG(KK1-KK5)-FACLOG(KK1-KK6)
      CLEB_D=CLEB_D+(-1.D0)**KK1*DEXP(CLG)
    ENDDO
    RETURN
  END FUNCTION CLEB_D
!==============================================================================