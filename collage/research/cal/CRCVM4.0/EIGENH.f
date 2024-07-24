C==== GENERALIZED EIGENVALUE PROBLEM ===================================

      SUBROUTINE EIGENH(ZEIGE,DEG,NZ,ND)

C-----------------------------------------------------------------------
      INCLUDE 'DIM_INC.f'
      INCLUDE 'GEM_INC.f'
      DIMENSION ZEIGE(NTOT)
      PARAMETER(ERROR=1.D-20)
      IF(ND.EQ.0) GOTO 9999
      IPRT=0
      CALL ZEIGEN(ZEIGE,NZ,IPRT,ND)
C
C SORT ENERGY EIGENVALUES AND EIGENVECTORS
C
      DO 1000 I=1,NZ
      DO 1000 J=I+1,NZ
        IF(DBLE(ZEIGE(I)).LT.DBLE(ZEIGE(J))) GOTO 1000
        ZMIN=ZEIGE(J)
        ZEIGE(J)=ZEIGE(I)
        ZEIGE(I)=ZMIN
        DO 1001 K=1,ND
          ZCOEF=ZH(K,J)
          ZH(K,J)=ZH(K,I)
          ZH(K,I)=ZCOEF
1001    CONTINUE
1000  CONTINUE
C
C WRITE EIGENVALUES OF HAMILTONIAN
C
      WRITE(NOUT,FMT=101) NZ,DEG,DEG/180.D0*PI
      WRITE(NENE,FMT=101) NZ,DEG,DEG/180.D0*PI
      DO I=1,NZ
        DREA=DBLE(ZEIGE(I))
        DIMA=DIMAG(ZEIGE(I))
        ANGLE=0.D0
        IF(DABS(DREA).LT.ERROR) THEN
          ANGLE=0.D0
        ELSEIF(DEG.GT.0.D0) THEN
          ANGLE=-DATAN(DIMA/DREA)/PI*180.D0
        ELSEIF(DEG.LT.0.D0) THEN
          ANGLE= DATAN(DIMA/DREA)/PI*180.D0
        ENDIF
        IF(DREA.LT.0.D0) THEN
          IF(DIMA.LT.0.D0) THEN
            ANGLE=ANGLE+180.D0
          ELSE
            ANGLE=ANGLE+180.D0
          ENDIF
        ELSEIF(DEG.GT.0.D0.AND.DIMA.GT.0.D0) THEN
          ANGLE=ANGLE+360.D0
        ELSEIF(DEG.LT.0.D0.AND.DIMA.LT.0.D0) THEN
          ANGLE=ANGLE+360.D0
        ENDIF

        IF(ANGLE.GT.350.D0.AND.ANGLE.LT.360.D0) THEN
          ANGLE=ANGLE-360.D0
        ENDIF
        IF(DBLE(ZEIGE(I)).LT.3.D+1.OR.DBLE(ZEIGE(I)).GT.0.9D0*V_PF) THEN
          WRITE(NOUT,FMT=102) I,ZEIGE(I),ANGLE,DEG
        ENDIF
        IF(IOUT.EQ.1) THEN
          WRITE(NENE,FMT=102) I,ZEIGE(I),ANGLE,DEG
        ENDIF
      ENDDO
      RETURN
 101  FORMAT(
     &   '# EIGENVALUES OF COMPLEX-SCALED HAMILTONIAN   ',
     &   'DIMESION :',I6,
     &  /'# SCALING ANGLE',F8.3,'[deg.] (',F7.3,'[rad.]) IN CSM',
     &  /'# STATE',4X,'ENERGY(Er-I/2*GAMMA[MeV])',5X,'ROT-ANGLE[deg.]'
     &      )
 102  FORMAT(I5,1X,2E16.7,2X,F10.4,1X,F10.4)
C
C ERROR MESSAGE
C
9999  WRITE(NOUT,*) 'DIMENSION IS ZERO!!'
      STOP
      END
C=======================================================================

C==== DOUBLE DIAGONALIZATION ===========================================

      SUBROUTINE ZEIGEN(ZEIGE,NC,IPRT,ND)

C-----------------------------------------------------------------------
      INCLUDE 'DIM_INC.f'
      INCLUDE 'GEM_INC.f'
      DIMENSION ZEIGE(NTOT)
      DIMENSION EIGN(NTOT),ZN(NTOT,NTOT)
      PARAMETER(ERROR=1.D-14,EPS=1.D-10)
C
C DIAGONALIZATION OF NORM
C
      I_DIAGO=0
      K1=0
      K2=0
      DO I=1,ND
        IF(DABS(DN(I,I)-1.D0).LT.ERROR) K1=K1+1
        DO J=1,I-1
          IF(DABS(DN(I,J)).GT.ERROR) K2=K2+1
        ENDDO
      ENDDO
      IF(K1.EQ.ND.AND.K2.EQ.0) THEN
        NC=ND
        I_DAIGO=1
        WRITE(NOUT,FMT=101)
        GOTO 5000
      ENDIF
      WRITE(NOUT,FMT=102) EPS
      CALL DIAG_DSYM(DN,EIGN,ND,NTOT)
      WRITE(NWAV,FMT=103) ND,(EIGN(I),I=1,ND)

      J=0
      DO I=1,ND
        IF(DABS(EIGN(I)-1.D0).LT.ERROR) J=J+1
      ENDDO
      IF(J.EQ.ND) THEN
        NC=ND
        I_DIAGO=1
        WRITE(NOUT,FMT=101)
        GOTO 5000
      ENDIF
C
C CHECK OVERCOMPLETENESS
C
      NC=0
      NEG=0
      DO I=1,ND
        IF(EIGN(I).GT.EPS) THEN
          NC=NC+1
          DO J=1,ND
            DN(J,NC)=DN(J,I)/DSQRT(EIGN(I))
          ENDDO
        ELSEIF(EIGN(I).LT.0.D0) THEN
          NEG=NEG+1
        ENDIF
      ENDDO
      WRITE(NOUT,FMT=104) NC,ND-NC,NEG
      WRITE(NWAV,FMT=104) NC,ND-NC,NEG
C
C CONSTRUCT NEW HAMILTONIAN
C
      DO I=1,ND
      DO J=1,I
        ZN(I,J)=(0.D0,0.D0)
        ZN(J,I)=(0.D0,0.D0)
      ENDDO
      ENDDO
      DO J=1,NC
      DO K=1,ND
        ZKJ=(0.D0,0.D0)
        DO L=1,ND
          ZKJ=ZKJ+ZH(K,L)*DN(L,J)
        ENDDO
        ZN(K,J)=ZKJ
      ENDDO
      ENDDO
      DO I=1,NC
      DO J=1,NC
        ZH(I,J)=(0.D0,0.D0)
        ZIJ=(0.D0,0.D0)
        DO K=1,ND
          ZIJ=ZIJ+DN(K,I)*ZN(K,J)
        ENDDO
        ZH(I,J)=ZIJ
      ENDDO
      ENDDO
5000  WRITE(NOUT,FMT=105)
      CALL DIAG_CSYM(ZH,ZEIGE,NC,NTOT)
C
C CONSTRUC VARIATIONAL COEFFICIENTS
C
      IF(I_DIAGO.EQ.1) THEN
        DO K=1,ND
        DO J=1,ND
          ZN(K,J)=ZH(K,J)
        ENDDO
        ENDDO
      ELSE
        DO K=1,ND
        DO J=1,NC
          ZN(K,J)=(0.D0,0.D0)
          DO I=1,NC
            ZN(K,J)=ZN(K,J)+DN(K,I)*ZH(I,J)
          ENDDO
        ENDDO
        ENDDO
      ENDIF
      DO I=1,ND
      DO J=1,NC
        ZN(I,J)=ZN(I,J)
      ENDDO
      ENDDO
      RETURN
C
C ERROR MESSAGE
C
 101  FORMAT('# ONE TIME DIAGONALIZATION')
 102  FORMAT('# LOWER LIMIT OF NORM EIGENVALUE:',E12.5
     &      /'# ... NOW DIAGONALIZING NORM MATRIX ...'/)
 103  FORMAT('# EIGENVALUES OF NORM ( DIMENSION :)',I7,' )',
     &      /('#',2X,5E13.5))
 104  FORMAT('# ALLOWED BASIS NUMBER    :',I6,
     &      /'# SUBTRACTED BASIS NUMBER :',I6,'( NEGATIVE :',I6,' )',
     &      /'# ... NOW CONSTRUCTING NEW HAMILTONIAN ...')
 105  FORMAT('# ... NOW DIAGONALIZING HAMILTONIAN MATRIX ...'/)
      END
