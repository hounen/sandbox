C==== DIAGONALIZATION OF COMPLEX SYMMERTIC MATRIX ======================

      SUBROUTINE DIAG_CSYM(MAT,EIG,ND,NTOT)

C-----------------------------------------------------------------------
C THIS SUBROUTINE IS THE ONE FOR DIAGONALIZATION OF COMPLEX SYMMETRIC
C MATRIX IN DOUBLE PRECISION. THE HOUSEHOLDER TRANSFORMATION AND QL
C DECOMPOSITION ARE USED TO DIAGONALIZE.
C
C MAT  : INPUT  <- MATRIX TO BE DIAGONALIZE
C        OUTPUT -> EIGENVECTOR OF MATRIX
C EIG  : OUTPUT -> EIGENVALUE OF MATRIX
C ND   : INPUT  <- DIMENSION OF MATRIX
C NTOT : INPUT  <- PHYSICAL SIZE OF MATRIX AND VECTOR
C-----------------------------------------------------------------------
      INTEGER ND,NTOT
      COMPLEX*16 MAT(NTOT,NTOT),EIG(NTOT)
C
C CALL SUBROUTINES
C
      COMPLEX*16 W(NTOT)
      CALL CTRED2(MAT,EIG,W,ND,NTOT) ! HOUSEHOLDER
      CALL CTQLI(EIG,W,MAT,ND,NTOT)  ! QL METHOD
      RETURN
      END
C=======================================================================

C==== HOUSEHOLDER REDUCTION ============================================

      SUBROUTINE CTRED2(A,D,E,N,NP)

C-----------------------------------------------------------------------
      INTEGER N,NP
      COMPLEX*16 A(NP,NP),E(NP),D(NP)
C
C MAIN PART
C
      INTEGER I,J,K,L
      COMPLEX*16 F,G,H,HH,SCALE
      DO I=N,2,-1
        L=I-1
        H=0.D0
        SCALE=0.D0
        IF(L.GT.1) THEN
          DO K=1,L
            SCALE=SCALE+CDABS(A(I,K))
          ENDDO
          IF(CDABS(SCALE).EQ.0.D0) THEN
            E(I)=A(I,L)
          ELSE
            DO K=1,L
              A(I,K)=A(I,K)/SCALE
              H=H+A(I,K)**2
            ENDDO
            F=A(I,L)
            IF(DBLE(F).GE.0.D0) THEN
              G=-CDSQRT(H)
            ELSE
              G=CDSQRT(H)
            ENDIF
            E(I)=SCALE*G
            H=H-F*G
            A(I,L)=F-G
            F=0.D0
            DO J=1,L
              A(J,I)=A(I,J)/H
              G=0.D0
              DO K=1,J
                G=G+A(J,K)*A(I,K)
              ENDDO
              DO K=J+1,L
                G=G+A(K,J)*A(I,K)
              ENDDO
              E(J)=G/H
              F=F+E(J)*A(I,J)
            ENDDO
            HH=F/(H+H)
            DO J=1,L
              F=A(I,J)
              G=E(J)-HH*F
              E(J)=G
              DO K=1,J
                A(J,K)=A(J,K)-F*E(K)-G*A(I,K)
              ENDDO
            ENDDO
          ENDIF
        ELSE
          E(I)=A(I,L)
        ENDIF
        D(I)=H
      ENDDO
      D(1)=0.D0
      E(1)=0.D0
      DO I=1,N
        L=I-1
        IF(D(I).NE.0.D0) THEN
          DO J=1,L
            G=0.D0
            DO K=1,L
              G=G+A(I,K)*A(K,J)
            ENDDO
            DO K=1,L
              A(K,J)=A(K,J)-G*A(K,I)
            ENDDO
          ENDDO
        ENDIF
        D(I)=A(I,I)
        A(I,I)=1.D0
        DO J=1,L
          A(I,J)=0.D0
          A(J,I)=0.D0
        ENDDO
      ENDDO
      RETURN
      END
C=======================================================================

C==== QL ALGORITHM WITH IMPLICIT SHIFTS ================================

      SUBROUTINE CTQLI(D,E,Z,N,NP)

C-----------------------------------------------------------------------
      INTEGER N,NP
      COMPLEX*16 E(NP),D(NP),Z(NP,NP)
C
C MAIN PART
C
      INTEGER I,ITER,K,L,M
      REAL*8 DD
      COMPLEX*16 B,C,F,G,P,R,S,CPYTHAG,ONE
      PARAMETER(ONE=(1.D0,0.D0))
      DO I=2,N
        E(I-1)=E(I)
      ENDDO
      E(N)=0.D0
      DO L=1,N
        ITER=0
  1     DO M=L,N-1
          DD=CDABS(D(M))+CDABS(D(M+1))
          IF(CDABS(E(M))+DD.EQ.DD) GOTO 2
        ENDDO
        M=N
  2     IF(M.NE.L) THEN
          IF(ITER.EQ.1000) GOTO 9999
          ITER=ITER+1
          G=(D(L+1)-D(L))/(2.D0*E(L))
          R=CPYTHAG(G,ONE)
          IF(DBLE(R)*DBLE(G).GE.0.D0) THEN ! TO BE CHECKED
            G=D(M)-D(L)+E(L)/(G+R)
          ELSE
            G=D(M)-D(L)+E(L)/(G-R)
          ENDIF
          S=1.D0
          C=1.D0
          P=0.D0
          DO I=M-1,L,-1
            F=S*E(I)
            B=C*E(I)
            R=CPYTHAG(F,G)
            E(I+1)=R
            IF(CDABS(R).EQ.0.D0) THEN
              D(I+1)=D(I+1)-P
              E(M)=0.D0
              GOTO 1
            ENDIF
            S=F/R
            C=G/R
            G=D(I+1)-P
            R=(D(I)-G)*S+2.D0*C*B
            P=S*R
            D(I+1)=G+P
            G=C*R-B
            DO K=1,N
              F=Z(K,I+1)
              Z(K,I+1)=S*Z(K,I)+C*F
              Z(K,I)=C*Z(K,I)-S*F
            ENDDO
          ENDDO
          D(L)=D(L)-P
          E(L)=G
          E(M)=0.D0
          GOTO 1
        ENDIF
      ENDDO
      RETURN
9999  WRITE(6,*) 'TOO MANY ITERATIONS IN CTQLI!!'
      STOP
      END
C=======================================================================

C=======================================================================

      FUNCTION CPYTHAG(A,B)

C-----------------------------------------------------------------------
      COMPLEX*16 A,B,CPYTHAG
C
C MAIN PART
C
      REAL*8 ABSA,ABSB
      ABSA=CDABS(A)
      ABSB=CDABS(B)
      IF(ABSA.GT.ABSB) THEN
        CPYTHAG=A*CDSQRT(1.D0+(B/A)**2)
      ELSE
        IF(ABSB.EQ.0.D0) THEN
          CPYTHAG=(0.D0,0.D0)
        ELSE
          CPYTHAG=B*CDSQRT(1.D0+(A/B)**2)
        ENDIF
      ENDIF
      RETURN
      END
C=======================================================================
