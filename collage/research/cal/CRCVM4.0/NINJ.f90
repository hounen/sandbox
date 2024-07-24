!==== 9J-SYMBOL ===============================================================
  MODULE NINJ_DEF
    IMPLICIT NONE
    INTERFACE NINJ
      DOUBLE PRECISION FUNCTION NINJ_I(J1,J2,J3,J4,J5,J6,J7,J8,J9)
        INTEGER,INTENT(IN) :: J1,J2,J3,J4,J5,J6,J7,J8,J9
      END FUNCTION NINJ_I
      DOUBLE PRECISION FUNCTION NINJ_D(J1,J2,J3,J4,J5,J6,J7,J8,J9)
        DOUBLE PRECISION,INTENT(IN) :: J1,J2,J3,J4,J5,J6,J7,J8,J9
      END FUNCTION NINJ_D
    END INTERFACE NINJ
  END MODULE NINJ_DEF
!==============================================================================

!==== 9J-SYMBOL FOR INTEGER ARGUMENTS =========================================
  DOUBLE PRECISION FUNCTION NINJ_I(J1,J2,J3,J4,J5,J6,J7,J8,J9)
    USE FACTORIAL
    IMPLICIT NONE
    INTEGER,INTENT(IN) :: J1,J2,J3,J4,J5,J6,J7,J8,J9
    INTEGER :: I,II1,II2,II3,II4,II5,II6,JMAX,JMIN,JTMP
    DOUBLE PRECISION :: SIXJ_I
    NINJ_I=0.D0

    IF(MIN0(J1,J2,J3,J4,J5,J6,J7,J8,J9).LT.0) RETURN
    IF(IABS(J1-J2).GT.J3.OR.J1+J2.LT.J3) RETURN
    IF(IABS(J4-J5).GT.J6.OR.J4+J5.LT.J6) RETURN
    IF(IABS(J7-J8).GT.J9.OR.J7+J8.LT.J9) RETURN
    IF(IABS(J1-J4).GT.J7.OR.J1+J4.LT.J7) RETURN
    IF(IABS(J2-J5).GT.J8.OR.J2+J5.LT.J8) RETURN
    IF(IABS(J3-J6).GT.J9.OR.J3+J6.LT.J9) RETURN

    II1=2*(J1+J2+J3)
    II2=2*(J4+J5+J6)
    II3=2*(J7+J8+J9)
    II4=2*(J1+J4+J7)
    II5=2*(J2+J5+J8)
    II6=2*(J3+J6+J9)
    IF(II1/2*2.NE.II1) RETURN
    IF(II2/2*2.NE.II2) RETURN
    IF(II3/2*2.NE.II3) RETURN
    IF(II4/2*2.NE.II4) RETURN
    IF(II5/2*2.NE.II5) RETURN
    IF(II6/2*2.NE.II6) RETURN

    JMIN=MAX0(IABS(J1-J9),IABS(J4-J8),IABS(J2-J6))
    JMAX=MIN0(    (J1+J9),    (J4+J8),    (J2+J6))
    IF(JMIN.GT.JMAX) RETURN
    DO I=JMIN,JMAX
      JTMP=I
      NINJ_I=NINJ_I                                                           &
    &       +(-1.D0)**(2.D0*JTMP)*(2.D0*JTMP+1.D0)                            &
    &       *SIXJ_I(J1,J4,J7,J8,J9,JTMP)                                      &
    &       *SIXJ_I(J2,J5,J8,J4,JTMP,J6)                                      &
    &       *SIXJ_I(J3,J6,J9,JTMP,J1,J2)
    ENDDO
    RETURN
  END FUNCTION NINJ_I
!==============================================================================

!==== 9J-SYMBOL FOR DOUBLE PRECISION ARGUMENTS ================================
  DOUBLE PRECISION FUNCTION NINJ_D(J1,J2,J3,J4,J5,J6,J7,J8,J9)
    USE FACTORIAL
    IMPLICIT NONE
    DOUBLE PRECISION,INTENT(IN) :: J1,J2,J3,J4,J5,J6,J7,J8,J9
    INTEGER :: I,II1,II2,II3,II4,II5,II6,JMAX,JMIN
    DOUBLE PRECISION :: JTMP,SIXJ_D
    NINJ_D=0.D0

    IF(DMIN1(J1,J2,J3,J4,J5,J6,J7,J8,J9).LT.0.D0) RETURN
    IF(DABS(J1-J2).GT.J3.OR.J1+J2.LT.J3) RETURN
    IF(DABS(J4-J5).GT.J6.OR.J4+J5.LT.J6) RETURN
    IF(DABS(J7-J8).GT.J9.OR.J7+J8.LT.J9) RETURN
    IF(DABS(J1-J4).GT.J7.OR.J1+J4.LT.J7) RETURN
    IF(DABS(J2-J5).GT.J8.OR.J2+J5.LT.J8) RETURN
    IF(DABS(J3-J6).GT.J9.OR.J3+J6.LT.J9) RETURN

    II1=IDINT(2.D0*(J1+J2+J3)+0.01D0)
    II2=IDINT(2.D0*(J4+J5+J6)+0.01D0)
    II3=IDINT(2.D0*(J7+J8+J9)+0.01D0)
    II4=IDINT(2.D0*(J1+J4+J7)+0.01D0)
    II5=IDINT(2.D0*(J2+J5+J8)+0.01D0)
    II6=IDINT(2.D0*(J3+J6+J9)+0.01D0)
    IF(II1/2*2.NE.II1) RETURN
    IF(II2/2*2.NE.II2) RETURN
    IF(II3/2*2.NE.II3) RETURN
    IF(II4/2*2.NE.II4) RETURN
    IF(II5/2*2.NE.II5) RETURN
    IF(II6/2*2.NE.II6) RETURN

    JMIN=IDINT(2.D0*DMAX1(DABS(J1-J9),DABS(J4-J8),DABS(J2-J6))+0.01D0)
    JMAX=IDINT(2.D0*DMIN1(    (J1+J9),    (J4+J8),    (J2+J6))+0.01D0)
    IF(JMIN.GT.JMAX) RETURN
    DO I=JMIN,JMAX,2
      JTMP=0.5D0*DBLE(I)
      NINJ_D=NINJ_D                                                           &
    &       +(-1.D0)**(2.D0*JTMP)*(2.D0*JTMP+1.D0)                            &
    &       *SIXJ_D(J1,J4,J7,J8,J9,JTMP)                                      &
    &       *SIXJ_D(J2,J5,J8,J4,JTMP,J6)                                      &
    &       *SIXJ_D(J3,J6,J9,JTMP,J1,J2)
    ENDDO
    RETURN
  END FUNCTION NINJ_D
!==============================================================================
