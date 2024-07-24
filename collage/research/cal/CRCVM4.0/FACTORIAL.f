C==== SET FACTORIAL FUNCTION ===========================================

      SUBROUTINE SET_FACT

C-----------------------------------------------------------------------
      INCLUDE 'DIM_INC.f'
      INCLUDE 'GEM_INC.f'
      FAC(0)=1.D0
      DFAC(0)=1.D0
      DO I=1,100
        FAC(I)=FAC(I-1)*DBLE(I)
        DFAC(I)=DFAC(I-1)*DBLE(2*I+1)
      ENDDO
      FACLOG(0)=0.D0
      DFACLOG(0)=0.D0
      DO I=1,100
        XI=DBLE(I)
        FACLOG(I)=FACLOG(I-1)+DLOG(XI)
        DFACLOG(I)=DFACLOG(I-1)+DLOG(2.D0*XI+1.D0)
      ENDDO
      RETURN
      END
C=======================================================================
