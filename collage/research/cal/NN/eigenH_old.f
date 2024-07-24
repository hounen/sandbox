c ==== eigenH.f =========================================================

      subroutine eigenH(deg,Ntot,Nz,zH,zE,dN,zC)

c -----------------------------------------------------------------------
      include 'li_inc.f'
      data error/1.d-20/
      dimension zH(nd,nd), zC(nd,nd), dN(nd,nd)
      dimension zE(nd)
C      open(33,file="ene.d")
c [Subject] ---
c     Eigen value problem (Diagonalization) 
c [Argument] ---
c     (i) Ntot: Prepared dimension of matrix
c     (i) H: Hamiltonian, N: Norm with "complex symmetric matrix"
c     (o) E: Eigenvalue, C: Variational Coefficients
c     (o) Nz  : Output dimension of matrix

c Diagonalization -------------------------------------------------------*
c     <|H|>*C(j) = E(j)<|>*C(j):j-th state ,|> = Sum(i){ |i>*C(i,j) }
c     Ntot: Input Dimension
c     Nz  : Output Dimension for positive definition of norm
c     iprt: Output Level (1: print of eigen-values & vectors) 
      iprt=0
      call zeigen(Ntot,Nz,iprt,zH,dN,zC)

c Sort eigenstates 
c     if ( swap i-th and j-th)
c     Eigenvalue :  zH(i,i) <--->  zH(j,j) [1:i,j:Nz]
c     Eigenvector:  zN(k,i) <--->  zN(k,j),[1:k:Ntot]
c     ( = Variational Coefficients, not Norm and not always Orthogonal)
c     Eigenvector:  zC(k,i) <---> zC(k,j),[1:k:Ntot]
c     ( = Orthogonal Variational Coefficients )
c     if "One time" diagonalizataion : zC = zN

      do i=1,Nz
         do j=i+1,Nz
            if ( dble(zH(i,i)).lt.dble(zH(j,j)) ) go to 35
            zmin    = zH(j,j)
            zH(j,j) = zH(i,i)
            zH(i,i) = zmin
            do k=1,Ntot
               zcoef   = zC(k,j) ! V.C. for Gaussian Basis
               zC(k,j) = zC(k,i)
               zC(k,i) = zcoef
            enddo
 35      enddo
         zE(i) = zH(i,i)
      enddo
c write data ----------------------------------------------------------*
      write( 6,11) Nz,deg,deg/180.d0*pi
      write(33,11) Nz,deg,deg/180.d0*pi
 11   format(
     &     '# Eigen-value of Complex Scaled Hamiltonian   ',
     &     'Dimension :',i6,/, 
     &     '# scaling angle',f8.3,'[deg.] (',f7.3,'[rad.] ) in CSM',//,
     &     '# state  Energy(Er - i/2*Gamma)[MeV]'
     &     ,2x,'rot-angle[deg.] [deg.] ',2x,'L',2x,'Am')

      do 50 i=1,Nz              ! Revise angle
         dRe = dble(  zE(i) )
         dIm = dimag( zE(i) )

         angle = 0.d0           ! Initialize
         if (abs(dRe).lt.error) then
            angle = 0.d0
         else if (deg.gt.0.d0) then   
            angle = -datan( dIm/dRe )/pi*180.d0
         else if (deg.lt.0.d0) then
            angle =  datan( dIm/dRe )/pi*180.d0
         endif

         if ( dRe.lt.0.d0 ) then 
            if (dIm.lt.0.d0 ) then
               angle = angle + 180.d0
            else 
               angle = angle + 180.d0
            endif
         else if ( deg.gt.0.d0 .and. dIm.gt.0.d0) then
            angle = angle + 360.d0
         else if ( deg.lt.0.d0 .and. dIm.lt.0.d0) then
            angle = angle + 360.d0
         endif

         if (angle.gt.350.and.angle.le.360.d0) 
     &        angle = angle - 360.d0

         write(6,17) i,zE(i),angle,deg,0,0!,0,0
         write(33,17) i,zE(i),angle,deg,0,0!,0,0

 50   continue
C      close(33)
      
check harmonic oscillator
c      zhw = zE(1)/1.5d0
c      do ii=2,nz
c         write(6,*) dble(zE(ii)-zE(ii-1)),dble(zE(ii)/zhw-1.5d0)
c      enddo

 17   format(i5,1x,2e16.7,2x,f10.4,1x,f10.4,4(2x,i2))
      return
      end
c =================================================================

      subroutine zeigen(Nb,Nc,iprt,zH,dN,zC)

c -----------------------------------------------------------------
      include 'li_inc.f'
      dimension zH(nd,nd), dN(nd,nd), zC(nd,nd)
      dimension zN(nd,nd), dC(nd,nd)
      common /Norm/dn_limit
      data error/1.d-15/
c      data eps,error/2.d-4,1.d-12/ ! 2+
c      data eps,error/1.d-5,1.d-12/ ! 
c      data eps,error/1.d-6,1.d-12/ ! 
c      data eps,error/1.d-7,1.d-12/ ! 
c      data eps,error/1.d-8,1.d-12/ ! 0+,1-
c      data eps,error/1.d-10,1.d-12/
c [Subject]
c     eigenvalue problem of generalized complex matrix
c     <| H |>*A(j) = E(j)*<|>*A(j) :j-th state
c     |> = |j> = Sum(i){ |i>*a(i,j) }
c     it is necessary to carry out TWO diagonalization 
c     (1):<|>*N(j)= L(j)*N(j)
c          L(j):j-th norm eigenvalue  --- ddN(j,j)
c          N(j):j-th norm eigenvector --- ddC(i,j)
c     (2):<<| H |>>*C"(j) = E(j)*C"(j)
c          E(j) :j-th eigenvalue   ------ zzH(j,j)
c          C"(j):j-th eigenvector  ------ zzC(i,j)
c     |j>> = Sum(i){ |i">*C"(i,j) } 
c          = Sum(k){ |k>*Sum(i){ N(k,i)/sqrt(L(i))*C"(i,j) } } 
c          = Sum(k){ |k>*C(k,j) }
c     |i"> = Sum(k){ |k>*N(k,i)/sqrt(L(i)) }
c     <i"| = Sum(k){ N(k,i)*<i|/sqrt(L(i)) }
c     <i"|j"> = delt(i,j), 1 = Sum(i){ C"(i,j)*C"(i,j) }
c       dNC(k,i) = N(k,i)/sqrt(L(i))  (Real)
c      <i"|H|j"> = Sum(k,l){ dNC(k,i)*<k| H |l>*dNC(l,j) } 
c     If Norm is already diagonalize (e.g. harmonic oscillator base)
c     "One time" diagonalization could be excuted
c [Argument]
c     (i) Nb : input dimension 
c     (o) Nc : output dimension (number of allowed state)

      eps = 1.d-10
      i_diago = 0
      k1=0
      k2=0
      do 120 i=1,Nb
         if ( abs(dN(i,i)-1.d0).lt.error ) k1=k1+1
         do 130 j=1,i-1
cc       write(*,*)i,j,dN(i,j)
            if ( abs(dN(i,j)).gt.error ) k2 = 1
 130     continue
 120  continue

      if (k1.eq.Nb .and. k2.eq.0) then
         Nc = Nb                ! all the norm eigenvalues are "UNITY"
         i_diago  = 1           ! one time diagonalization
         write(6,*) '# one time diagonalization'
         goto 777               ! NO NECESSARY to make a new hamiltonian
      endif

      write(6,14) eps
 14   format(
     &     /,'# lower limit of norm eigenvalue :',e12.5,
     &     /,'# ... Now, diagonalizing Norm matrix ...'/)
c [1]: norm matrix -----------------------------------------------------*
c     dN: Norm Matrix (io)
c     dC: Eigen-Vector (o)

      call eignorm(Nb,error,dN,dC)

      write(6,11) Nb,(dN(i,i),i=1,Nb)
 11   format('# Eigenvalue of Norm (Dimension :',i7,')'
     &     /('#',2x,5e13.5))

      j=0
      do 110 i=1,Nb
         if (abs(dN(i,i)-1.d0).lt.eps ) j=j+1
 110  continue

      if (j.eq.Nb) then
         Nc = Nb                ! all the norm eigenvalues are "UNITY"
         i_diago  = 1           ! flag for one time diagonalization
         write(6,*) '# one time diagonalization'
         goto 777               ! NO NECESSARY to make a new hamiltonian
      endif

c positive definite and overcompleteness ------------------------------*
      Nc = 0
      Neg= 0                    ! Number of negative eigenvalue
      do 20 i=1,Nb
         if ( dN(i,i).gt.eps ) then
            Nc = Nc + 1
            do 30 j=1,Nb
               dC(j,Nc) = dC(j,i)/dsqrt(dN(i,i))
 30         continue
         else if (dN(i,i).lt.0.d0) then
            Neg = Neg + 1
         endif
 20   continue

      write( 6,16) Nc,Nb-Nc,Neg
 16   format(
     &     '# Allowed basis number   :',i6,/,
     &     '# Subtracted basis number:',i6,' (Negative: ',i6,')',/,
     &     '# ... Now, constructing NEW Hamiltonian ...')

c Re-construction of Hamiltonian  ---
c     <<j|H|j'>> = Sum(i,i'){ <i"|H|i'"> } 
c     = Sum(i,i'){ N(j,i)*N(j',i')*<i|H|i'>/sqrt(L(j))*L(j') }
c     <i"|j"> = delta(i,j)
c      iwrite = 1                ! Frag to write a report (1:ON, 0:OFF)
 888  format('# New H-1: (',i5,')-th column in 3-loop')
 555  format('# New H-2: (',i5,')-th column in 3-loop')

      do i=1,Nb
         do j=1,i
            zN(i,j) = (0.d0,0.d0)
            zN(j,i) = (0.d0,0.d0)
         enddo
      enddo

      do j=1,Nc
c         if (iwrite.eq.1.and.Nc.gt.100) then
c            rewind 99
c            write(99,888) j
c         endif
         do k=1,Nb
            zkj = (0.d0,0.d0)
            do l=1,Nb
               zkj = zkj + zH(k,l)*dC(l,j)
            enddo
            zN(k,j) = zkj      ! zzN is not Norm 
         enddo
      enddo

      do i=1,Nc
c         if (iwrite.eq.1.and.nc.gt.100) then
c            rewind 99
c            write(99,555) i
c         endif
         do j=1,Nc
            zH(i,j) = (0.d0,0.d0)
            zij = (0.d0,0.d0)
            do k=1,Nb
               zij = zij + dC(k,i)*zN(k,j)
            enddo
            zH(i,j) = zij  
         enddo
      enddo

c [2]: NEW Hamiltonian ------------------------------------------------*
c     (i) Nc : dimension (<=Nb)
c     (io)zH : hamiltonian and eigenvalue
c     (o) zN : eigen-vector
c     (i) iprt: print Level

 777  write(6,15)

      call eigen(Nc,zH,zN,iprt)
      
c Making Hamiltonian Eigen-Vector ---
c     zN(i,j) : variational coefficients,"NOT norm"
c     C(k,j)= Sum(i){ N(k,i)*C"(i,j)/sqrt(L(j)) }

check orthonormality 
c      do i=1,Nc
c         do j=1,Nc
c            zzz = (0.d0,0.d0)
c            do k=1,Nc
c               zzz = zzz + zN(k,i)*zN(k,j)
c            enddo
c            if (abs(zzz).gt.1.d-14) write(6,*) i,j,i-j,zzz
c         enddo
c      enddo

      if (i_diago.eq.1) then    ! One time diagonalization
         do 50 k=1,Nb
         do 50 j=1,Nb
c         do 50 j=1,k
            zC(k,j) = zN(k,j)
c            zC(j,k) = zN(j,k)
 50      continue
      else
         do 70 k=1,Nb           ! Two time diagonalization
            do 75 j=1,Nc
               zC(k,j) = (0.d0,0.d0)
               do 80 i=1,Nc
                  zC(k,j) = zC(k,j) + dC(k,i)*zN(i,j)
 80            continue
 75         continue
 70      continue
      endif

 15   format('# ... Now, diagonalizing NEW Hamiltonian ...'/)
      return
      end
      

