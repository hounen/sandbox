C==== MAIN PROGRAM =====================================================

      PROGRAM MAIN

C-----------------------------------------------------------------------
C
C THIS PROGRAM TO SOLVE THE THREE-BODY PROBLEM WITH GAUSSIAN EXPANSION
C METHOD. THE RESONANCES ARE OBTAINED AS COMPLEX-ENERGY EIGENSTATES IN
C THE COMPLEX SCALING METHOD.
C
C THIS PROGRAM IS WRITTEN BY YUMA KIKUCHI @ HOKKAIDO UNIVERSITY.
C
C-----------------------------------------------------------------------
      INCLUDE 'DIM_INC.f'
      INCLUDE 'GEM_INC.f'
C
C OPEN FILES
C
      OPEN(UNIT=NENE,FILE='O_ENE.d', STATUS='UNKNOWN')
      OPEN(UNIT=NWAV,FILE='O_WAVE.d',STATUS='UNKNOWN')
      OPEN(UNIT=NSCT,FILE='O_SCAT.d',STATUS='UNKNOWN')
      WRITE(NOUT,FMT=100)
      WRITE(NENE,FMT=100)
      WRITE(NWAV,FMT=100)
      WRITE(NSCT,FMT=100)
      WRITE(NOUT,FMT=101)
      WRITE(NENE,FMT=101)
      WRITE(NWAV,FMT=101)
      WRITE(NSCT,FMT=101)
      WRITE(NOUT,FMT=100)
      WRITE(NENE,FMT=100)
      WRITE(NWAV,FMT=100)
      WRITE(NSCT,FMT=100)
C
C SET FACTORIAL FUNCTION
C
      CALL SET_FACT
C
C READ INPUT FILE
C
      CALL READ_FILE
      CALL SET_SYSTEM
      CALL SET_ANGLE
C
C TRAJECTORY
C
      CALL TRAJECT
C
C CLOSE FILES
C
      CLOSE(NENE)
      CLOSE(NWAV)
      CLOSE(NSCT)
      STOP
C
C FORMAT
C
 100  FORMAT(
     &  '#------------------------------------------------------------',
     &  '------------------'
     &      )
 101  FORMAT(
     &  '# PROGRAM TO SOLVE THE THREE-BODY PROBLEM WITH GEM',
     & /'# METHOD      : ORTHOGONALITY CONDITION MODEL',
     & /'#               COMPLEX SCALING METHOD',
     & /'# THIS PROGRAM IS WRITTEN BY YUMA KIKUCHI @ HOKKAIDO UNIV.'
     &      )
      END
C=======================================================================
