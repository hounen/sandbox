!==== ANGULAR INTEGRATION =====================================================
  MODULE ANGLE
    DOUBLE PRECISION,ALLOCATABLE :: ANGFAC(:,:,:,:,:,:,:)
    CONTAINS
      SUBROUTINE SET_ANG(DET)
        USE CLEB_DEF
        USE SIXJ_DEF
        IMPLICIT NONE
        INTEGER,INTENT(IN) :: DET
        INTEGER :: LL1,LL2,LL3,LL4,LT12,LT34,LAM
        IF(DET.EQ.1) THEN
          ALLOCATE(ANGFAC(0:12,0:12,0:12,0:12,0:16,0:6,1:1))
          DO LL1=0,12
          DO LL2=0,12
          DO LL3=0,12
          DO LL4=0,12
          DO LAM=0,16
          DO LT12=0,6
            ANGFAC(LL1,LL2,LL3,LL4,LAM,LT12,1)=0.D0
          ENDDO
          ENDDO
          ENDDO
          ENDDO
          ENDDO
          ENDDO
          DO LT12=0,6
          DO LAM=0,16
          DO LL1=0,12
          DO LL2=0,12
            IF(IABS(LL1-LL2).GT.LT12.OR.LL1+LL2.LT.LT12) CYCLE
          DO LL3=0,12
          DO LL4=0,12
            IF(IABS(LL3-LL4).GT.LT12.OR.LL3+LL4.LT.LT12) CYCLE
            IF(IABS(LL1-LL3).GT.LAM .OR.LL1+LL3.LT.LAM)  CYCLE
            IF(IABS(LL2-LL4).GT.LAM .OR.LL2+LL4.LT.LAM)  CYCLE
            ANGFAC(LL1,LL2,LL3,LL4,LAM,LT12,1)                                &
    &         =(-1.D0)**(LL2+LL3+LT12)*(2.D0*LAM+1.D0)                        &
    &         *DSQRT((2.D0*LL3+1.D0)*(2.D0*LL4+1.D0))                         &
    &         *CGZ(LL3,LAM,LL1)*CGZ(LL4,LAM,LL2)                              &
    &         *SIXJ(LT12,LL2,LL1,LAM,LL3,LL4)
          ENDDO
          ENDDO
          ENDDO
          ENDDO
          ENDDO
          ENDDO
        ENDIF
        RETURN
      END SUBROUTINE SET_ANG
      SUBROUTINE DEL_ANG
        IMPLICIT NONE
        DEALLOCATE(ANGFAC)
        RETURN
      END SUBROUTINE DEL_ANG
  END MODULE ANGLE
!==============================================================================
