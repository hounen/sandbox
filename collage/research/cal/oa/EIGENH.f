c==== eigenvalue problem ===============================================

      subroutine eigenh(dn,zh,eige,deg,nz,nd,ntot)

c-----------------------------------------------------------------------
c solve the generalized eigenvalue problem for complex symmetric
c matrix.
c-----------------------------------------------------------------------
      implicit none
      integer nz,nd,ntot
      double precision deg,dn(ntot,ntot)
      complex(kind(0d0)) zh(ntot,ntot),eige(ntot)
c
c declaration of variables
c
      integer i,j,k
      double precision pi,drea,dima,angle,error
      complex(kind(0d0)) zmin,zcoef
      parameter (pi=3.141592653589793d0)
      data error/1.d-20/
c
c diagonalization
c
      call zeigen(dn,zh,eige,nz,nd,ntot)
c
c sort energy eigenvalues and eigenvectors
c
      do i=1,nz
        do j=i+1,nz
          if(dble(eige(i)).lt.dble(eige(j))) cycle
          zmin   =eige(j)
          eige(j)=eige(i)
          eige(i)=zmin
          do k=1,ntot
            zcoef  =zh(k,j)
            zh(k,j)=zh(k,i)
            zh(k,i)=zcoef
          enddo
        enddo
      enddo
c
c write eigenvalues of hamiltonian
c
      write(6,fmt=101) nz,deg,deg/180.d0*pi
      write(33,fmt=101) nz,deg,deg/180.d0*pi
      do i=1,nz
        drea=dble(eige(i))
        dima=dimag(eige(i))
        angle=0.d0
        if(dabs(drea).lt.error) then
          angle=0.d0
        elseif(deg.gt.0.d0) then
          angle=-datan(dima/drea)/pi*180.d0
        elseif(deg.lt.0.d0) then
          angle= datan(dima/drea)/pi*180.d0
        endif
        if(drea.lt.0.d0) then
          if(dima.lt.0.d0) then
            angle=angle+180.d0
          else
            angle=angle+180.d0
          endif
        elseif(deg.gt.0.d0.and.dima.gt.0.d0) then
          angle=angle+360.d0
        elseif(deg.lt.0.d0.and.dima.lt.0.d0) then
          angle=angle+360.d0
        endif

        if(angle.gt.350.d0.and.angle.lt.360.d0) then
          angle=angle-360.d0
        endif
        if(dble(eige(i)).lt.3.d+1) then
          write(6,fmt=102) i,eige(i),angle,deg
        endif
        write(33,fmt=102) i,eige(i),angle,deg
      enddo
      return
 101  format(
     & '# eigenvalues of complex-scaled hamiltonian  ',
     & 'dimension :',i6,
     &/'# scaling angle',f8.3,'[DEG.] (',f7.3,'[RAD.]) in csm',
     &/'# state',4x,'energy(eR-i/2*gamma)[mEv]',5x,'rot-angle[DEG.]'
     &      )
 102  format(i5,1x,2e16.7,2x,f10.4,1x,f10.4)
      end
c-----------------------------------------------------------------------

c==== generalized eigenvalue problem ===================================
      
      subroutine zeigen(dn,zh,eige,nc,nb,ntot)

c-----------------------------------------------------------------------
      implicit none
      integer nc,nb,ntot
      double precision dn(ntot,ntot)
      complex(kind(0d0)) zh(ntot,ntot),eige(ntot)
c
c declaration of variables
c
      integer neg,i_diago
      integer i,j,k,l,k1,k2
      double precision eps,error,eign(ntot)
      complex(kind(0d0)) zkj,zij
      complex(kind(0d0)) zn(ntot,ntot)
      data eps,error/1.d-10,1.d-14/
c
c diagonalization of norm matrix
c
      i_diago=0
      k1=0
      k2=0
      do i=1,nb
        if(dabs(dn(i,i)-1.d0).lt.error) k1=k1+1
        do j=1,i-1
          if(dabs(dn(i,j)).gt.error) k2=k2+1
        enddo
      enddo
      if(k1.eq.nb.and.k2.eq.0) then
        nc=nb
        i_diago=1
        write(6,fmt=101)
        goto 5000
      endif
      write(6,fmt=102) eps
      call diag_dsym(dn,eign,nb,ntot)

      j=0
      do i=1,nb
        if(dabs(eign(i)-1.d0).lt.eps) j=j+1
      enddo
      if(j.eq.nb) then
        nc=nb
        i_diago=1
        write(6,fmt=101)
        goto 5000
      endif
c
c check overcompleteness
c
      nc=0
      neg=0
      do i=1,nb
        if(eign(i).gt.eps) then
          nc=nc+1
          do j=1,nb
            dn(j,i)=dn(j,i)/dsqrt(eign(i))
          enddo
        elseif(eign(i).lt.0.d0) then
          neg=neg+1
        endif
      enddo
      write(6,fmt=104) nc,nb-nc,neg
c
c constructing new hamiltonian matrix
c
      do i=1,nb
      do j=1,i
        zn(i,j)=(0.d0,0.d0)
        zn(j,i)=zn(i,j)
      enddo
      enddo
      do j=1,nc
      do k=1,nb
        zkj=(0.d0,0.d0)
        do l=1,nb
          zkj=zkj+zh(k,l)*dn(l,j)
        enddo
        zn(k,j)=zkj
      enddo
      enddo
      do i=1,nc
      do j=1,nc
        zh(i,j)=(0.d0,0.d0)
        zij=(0.d0,0.d0)
        do k=1,nb
          zij=zij+dn(k,i)*zn(k,j)
        enddo
        zh(i,j)=zij
      enddo
      enddo
c
c diagonalization of hamiltonian matrix
c
5000  write(6,fmt=105)
      call diag_csym(zh,eige,nc,ntot)
c
c construct variational coefficients
c
      if(i_diago.eq.1) then
        do k=1,nb
        do j=1,nb
          zn(k,j)=zh(k,j)
        enddo
        enddo
      else
        do k=1,nb
        do j=1,nc
          zn(k,j)=(0.d0,0.d0)
          do i=1,nc
            zn(k,j)=zn(k,j)+dn(k,i)*zh(i,j)
          enddo
        enddo
        enddo
      endif
      do i=1,nb
      do j=1,nc
        zh(i,j)=zn(i,j)
      enddo
      enddo
      return
 101  format('# one time diagonalization')
 102  format('# lower limit of norm eigenvalue:',e12.5,
     &      /'# ... now diagonalizing norm matrix ...'/ )
 103  format('# eigenvalues of norm (dimension:',i7,')',
     &      /('#',2x,5e13.5))
 104  format('# allowed basis number   :',i6,
     &      /'# subtracted basis number:',i6,'(negative:',i6,')',
     &      /'# ... now constructing new hamiltonian ...' )
 105  format('# ... now diagonalizing hamiltonian matrix ...'/)
      end
c=======================================================================

