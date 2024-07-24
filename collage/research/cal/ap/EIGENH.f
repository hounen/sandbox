C ==== EIGENH.f =========================================================

      SUBROUTINE EIGENH(DEG,NTOT,NZ,ZH,ZE,DN,ZC)

C -----------------------------------------------------------------------
      INCLUDE 'LI_INC.f'
      DATA ERROR/1.D-20/
      DIMENSION ZH(ND,ND), ZC(ND,ND), DN(ND,ND)
      DIMENSION ZE(ND)
C      OPEN(33,FILE="ENE.d")
C [SUBJECT] ---
C     EIGEN VALUE PROBLEM (DIAGONALIZATION) 
C [ARGUMENT] ---
C     (I) NTOT: PREPARED DIMENSION OF MATRIX
C     (I) H: HAMILTONIAN, N: NORM WITH "COMPLEX SYMMETRIC MATRIX"
C     (O) E: OIGENVALUE, C: NARIATIONAL NOEFFICIENTS
C     (O) NZ  : OUTPUT DIMENSION OF MATRIX

C DIAGONALIZATION -------------------------------------------------------*
C     <|H|>*C(J) = E(J)<|>*C(J):J-TH STATE ,|> = SUM(I){ |I>*C(I,J) }
C     NTOT: INPUT DIMENSION
C     NZ  : OUTPUT DIMENSION FOR POSITIVE DEFINITION OF NORM
C     IPRT: OUTPUT LEVEL (1: PRINT OF EIGEN-VALUES & VECTORS) 
      IPRT=0
      CALL ZEIGEN(NTOT,NZ,IPRT,ZH,DN,ZC)

C SORT EIGENSTATES 
C     IF ( SWAP I-TH AND J-TH)
C     EIGENVALUE :  ZH(I,I) <--->  ZH(J,J) [1:I,J:NZ]
C     EIGENVECTOR:  ZN(K,I) <--->  ZN(K,J),[1:K:NTOT]
C     ( = VARIATIONAL COEFFICIENTS, NOT NORM AND NOT ALWAYS ORTHOGONAL)
C     EIGENVECTOR:  ZC(K,I) <---> ZC(K,J),[1:K:NTOT]
C     ( = ORTHOGONAL VARIATIONAL COEFFICIENTS )
C     IF "ONE TIME" DIAGONALIZATAION : ZC = ZN

      DO I=1,NZ
         DO J=I+1,NZ
            IF ( DBLE(ZH(I,I)).LT.DBLE(ZH(J,J)) ) GO TO 35
            ZMIN    = ZH(J,J)
            ZH(J,J) = ZH(I,I)
            ZH(I,I) = ZMIN
            DO K=1,NTOT
               ZCOEF   = ZC(K,J) ! V.C. FOR GAUSSIAN BASIS
               ZC(K,J) = ZC(K,I)
               ZC(K,I) = ZCOEF
            ENDDO
 35      ENDDO
         ZE(I) = ZH(I,I)
      ENDDO
C WRITE DATA ----------------------------------------------------------*
      WRITE( 6,11) NZ,DEG,DEG/180.D0*PI
      WRITE(33,11) NZ,DEG,DEG/180.D0*PI
 11   FORMAT(
     &     '# EIGEN-VALUE OF COMPLEX SCALED HAMILTONIAN   ',
     &     'DIMENSION :',I6,/, 
     &     '# SCALING ANGLE',F8.3,'[DEG.] (',F7.3,'[RAD.] ) IN CSM',//,
     &     '# STATE  ENERGY(ER - I/2*GAMMA)[MEV]'
     &     ,2X,'ROT-ANGLE[DEG.] [DEG.] ',2X,'L',2X,'AM')

      DO 50 I=1,NZ              ! REVISE ANGLE
         DRE = DBLE(  ZE(I) )
         DIM = DIMAG( ZE(I) )

         ANGLE = 0.D0           ! INITIALIZE
         IF (ABS(DRE).LT.ERROR) THEN
            ANGLE = 0.D0
         ELSE IF (DEG.GT.0.D0) THEN   
            ANGLE = -DATAN( DIM/DRE )/PI*180.D0
         ELSE IF (DEG.LT.0.D0) THEN
            ANGLE =  DATAN( DIM/DRE )/PI*180.D0
         ENDIF

         IF ( DRE.LT.0.D0 ) THEN 
            IF (DIM.LT.0.D0 ) THEN
               ANGLE = ANGLE + 180.D0
            ELSE 
               ANGLE = ANGLE + 180.D0
            ENDIF
         ELSE IF ( DEG.GT.0.D0 .AND. DIM.GT.0.D0) THEN
            ANGLE = ANGLE + 360.D0
         ELSE IF ( DEG.LT.0.D0 .AND. DIM.LT.0.D0) THEN
            ANGLE = ANGLE + 360.D0
         ENDIF

         IF (ANGLE.GT.350.AND.ANGLE.LE.360.D0) 
     &        ANGLE = ANGLE - 360.D0

         WRITE(6,17) I,ZE(I),ANGLE,DEG,0,0!,0,0
         WRITE(33,17) I,ZE(I),ANGLE,DEG,0,0!,0,0

 50   CONTINUE
C      CLOSE(33)
      
CHECK HARMONIC OSCILLATOR
C      ZHW = ZE(1)/1.5D0
C      DO II=2,NZ
C         WRITE(6,*) DBLE(ZE(II)-ZE(II-1)),DBLE(ZE(II)/ZHW-1.5D0)
C      ENDDO

 17   FORMAT(I5,1X,2E16.7,2X,F10.4,1X,F10.4,4(2X,I2))
      RETURN
      END
C =================================================================

      SUBROUTINE ZEIGEN(NB,NC,IPRT,ZH,DN,ZC)

C -----------------------------------------------------------------
      INCLUDE 'LI_INC.f'
      DIMENSION ZH(ND,ND), DN(ND,ND), ZC(ND,ND)
      DIMENSION ZN(ND,ND), DC(ND,ND)
      COMMON /NORM/DN_LIMIT
      DATA ERROR/1.D-15/
C      DATA EPS,ERROR/2.D-4,1.D-12/ ! 2+
C      DATA EPS,ERROR/1.D-5,1.D-12/ ! 
C      DATA EPS,ERROR/1.D-6,1.D-12/ ! 
C      DATA EPS,ERROR/1.D-7,1.D-12/ ! 
C      DATA EPS,ERROR/1.D-8,1.D-12/ ! 0+,1-
C      DATA EPS,ERROR/1.D-10,1.D-12/
C [SUBJECT]
C     EIGENVALUE PROBLEM OF GENERALIZED COMPLEX MATRIX
C     <| H |>*A(J) = E(J)*<|>*A(J) :J-TH STATE
C     |> = |J> = SUM(I){ |I>*A(I,J) }
C     IT IS NECESSARY TO CARRY OUT TWO DIAGONALIZATION 
C     (1):<|>*N(J)= L(J)*N(J)
C          L(J):J-TH NORM EIGENVALUE  --- DDN(J,J)
C          N(J):J-TH NORM EIGENVECTOR --- DDC(I,J)
C     (2):<<| H |>>*C"(J) = E(J)*C"(J)
C          E(J) :J-TH EIGENVALUE   ------ ZZH(J,J)
C          C"(J):J-TH EIGENVECTOR  ------ ZZC(I,J)
C     |J>> = SUM(I){ |I">*C"(I,J) } 
C          = SUM(K){ |K>*SUM(I){ N(K,I)/SQRT(L(I))*C"(I,J) } } 
C          = SUM(K){ |K>*C(K,J) }
C     |I"> = SUM(K){ |K>*N(K,I)/SQRT(L(I)) }
C     <I"| = SUM(K){ N(K,I)*<I|/SQRT(L(I)) }
C     <I"|J"> = DELT(I,J), 1 = SUM(I){ C"(I,J)*C"(I,J) }
C       DNC(K,I) = N(K,I)/SQRT(L(I))  (REAL)
C      <I"|H|J"> = SUM(K,L){ DNC(K,I)*<K| H |L>*DNC(L,J) } 
C     IF NORM IS ALREADY DIAGONALIZE (E.G. HARMONIC OSCILLATOR BASE)
C     "ONE TIME" DIAGONALIZATION COULD BE EXCUTED
C [ARGUMENT]
C     (I) NB : INPUT DIMENSION 
C     (O) NC : OUTPUT DIMENSION (NUMBER OF ALLOWED STATE)

      EPS = 1.D-10
      I_DIAGO = 0
      K1=0
      K2=0
      DO 120 I=1,NB
         IF ( ABS(DN(I,I)-1.D0).LT.ERROR ) K1=K1+1
         DO 130 J=1,I-1
CC       WRITE(*,*)I,J,DN(I,J)
            IF ( ABS(DN(I,J)).GT.ERROR ) K2 = 1
 130     CONTINUE
 120  CONTINUE

      IF (K1.EQ.NB .AND. K2.EQ.0) THEN
         NC = NB                ! ALL THE NORM EIGENVALUES ARE "UNITY"
         I_DIAGO  = 1           ! ONE TIME DIAGONALIZATION
         WRITE(6,*) '# ONE TIME DIAGONALIZATION'
         GOTO 777               ! NO NECESSARY TO MAKE A NEW HAMILTONIAN
      ENDIF

      WRITE(6,14) EPS
 14   FORMAT(
     &     /,'# LOWER LIMIT OF NORM EIGENVALUE :',E12.5,
     &     /,'# ... NOW, DIAGONALIZING NORM MATRIX ...'/)
C [1]: NORM MATRIX -----------------------------------------------------*
C     DN: NORM MATRIX (IO)
C     DC: EIGEN-VECTOR (O)

      CALL EIGNORM(NB,ERROR,DN,DC)

      WRITE(6,11) NB,(DN(I,I),I=1,NB)
 11   FORMAT('# EIGENVALUE OF NORM (DIMENSION :',I7,')'
     &     /('#',2X,5E13.5))

      J=0
      DO 110 I=1,NB
         IF (ABS(DN(I,I)-1.D0).LT.EPS ) J=J+1
 110  CONTINUE

      IF (J.EQ.NB) THEN
         NC = NB                ! ALL THE NORM EIGENVALUES ARE "UNITY"
         I_DIAGO  = 1           ! FLAG FOR ONE TIME DIAGONALIZATION
         WRITE(6,*) '# ONE TIME DIAGONALIZATION'
         GOTO 777               ! NO NECESSARY TO MAKE A NEW HAMILTONIAN
      ENDIF

C POSITIVE DEFINITE AND OVERCOMPLETENESS ------------------------------*
      NC = 0
      NEG= 0                    ! NUMBER OF NEGATIVE EIGENVALUE
      DO 20 I=1,NB
         IF ( DN(I,I).GT.EPS ) THEN
            NC = NC + 1
            DO 30 J=1,NB
               DC(J,NC) = DC(J,I)/DSQRT(DN(I,I))
 30         CONTINUE
         ELSE IF (DN(I,I).LT.0.D0) THEN
            NEG = NEG + 1
         ENDIF
 20   CONTINUE

      WRITE( 6,16) NC,NB-NC,NEG
 16   FORMAT(
     &     '# ALLOWED BASIS NUMBER   :',I6,/,
     &     '# SUBTRACTED BASIS NUMBER:',I6,' (NEGATIVE: ',I6,')',/,
     &     '# ... NOW, CONSTRUCTING NEW HAMILTONIAN ...')

C RE-CONSTRUCTION OF HAMILTONIAN  ---
C     <<J|H|J'>> = SUM(I,I'){ <I"|H|I'"> } 
C     = SUM(I,I'){ N(J,I)*N(J',I')*<I|H|I'>/SQRT(L(J))*L(J') }
C     <I"|J"> = DELTA(I,J)
C      IWRITE = 1                ! FRAG TO WRITE A REPORT (1:ON, 0:OFF)
 888  FORMAT('# NEW H-1: (',I5,')-TH COLUMN IN 3-LOOP')
 555  FORMAT('# NEW H-2: (',I5,')-TH COLUMN IN 3-LOOP')

      DO I=1,NB
         DO J=1,I
            ZN(I,J) = (0.D0,0.D0)
            ZN(J,I) = (0.D0,0.D0)
         ENDDO
      ENDDO

      DO J=1,NC
C         IF (IWRITE.EQ.1.AND.NC.GT.100) THEN
C            REWIND 99
C            WRITE(99,888) J
C         ENDIF
         DO K=1,NB
            ZKJ = (0.D0,0.D0)
            DO L=1,NB
               ZKJ = ZKJ + ZH(K,L)*DC(L,J)
            ENDDO
            ZN(K,J) = ZKJ      ! ZZN IS NOT NORM 
         ENDDO
      ENDDO

      DO I=1,NC
C         IF (IWRITE.EQ.1.AND.NC.GT.100) THEN
C            REWIND 99
C            WRITE(99,555) I
C         ENDIF
         DO J=1,NC
            ZH(I,J) = (0.D0,0.D0)
            ZIJ = (0.D0,0.D0)
            DO K=1,NB
               ZIJ = ZIJ + DC(K,I)*ZN(K,J)
            ENDDO
            ZH(I,J) = ZIJ  
         ENDDO
      ENDDO

C [2]: NEW HAMILTONIAN ------------------------------------------------*
C     (I) NC : DIMENSION (<=NB)
C     (IO)ZH : HAMILTONIAN AND EIGENVALUE
C     (O) ZN : EIGEN-VECTOR
C     (I) IPRT: PRINT LEVEL

 777  WRITE(6,15)

      CALL EIGEN(NC,ZH,ZN,IPRT)
      
C MAKING HAMILTONIAN EIGEN-VECTOR ---
C     ZN(I,J) : VARIATIONAL COEFFICIENTS,"NOT NORM"
C     C(K,J)= SUM(I){ N(K,I)*C"(I,J)/SQRT(L(J)) }

CHECK ORTHONORMALITY 
C      DO I=1,NC
C         DO J=1,NC
C            ZZZ = (0.D0,0.D0)
C            DO K=1,NC
C               ZZZ = ZZZ + ZN(K,I)*ZN(K,J)
C            ENDDO
C            IF (ABS(ZZZ).GT.1.D-14) WRITE(6,*) I,J,I-J,ZZZ
C         ENDDO
C      ENDDO

      IF (I_DIAGO.EQ.1) THEN    ! ONE TIME DIAGONALIZATION
         DO 50 K=1,NB
         DO 50 J=1,NB
C         DO 50 J=1,K
            ZC(K,J) = ZN(K,J)
C            ZC(J,K) = ZN(J,K)
 50      CONTINUE
      ELSE
         DO 70 K=1,NB           ! TWO TIME DIAGONALIZATION
            DO 75 J=1,NC
               ZC(K,J) = (0.D0,0.D0)
               DO 80 I=1,NC
                  ZC(K,J) = ZC(K,J) + DC(K,I)*ZN(I,J)
 80            CONTINUE
 75         CONTINUE
 70      CONTINUE
      ENDIF

 15   FORMAT('# ... NOW, DIAGONALIZING NEW HAMILTONIAN ...'/)
      RETURN
      END
      

