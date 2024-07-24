      program main
      include 'li_inc.f'      
      character*80 memo 
      dimension za(10,nd),dn(nd,nd),zh(nd,nd),
     &          ze(nd),zr(nd,nd),zc(nd,nd)
     &         ,zh0(nd,nd),ze0(nd),dn0(nd,nd),zc0(nd,nd),zmat(nd)
     &         ,cld(0:100000),resocld(0:100000),conticld(0:100000)
     &         ,phs(0:100000),resophs(0:100000),contiphs(0:100000)
     &         ,aphs(0:100000),aphsreso(0:100000),aphsconti(0:100000)
     &         ,zscamp(100),wfunc(100)
     &         ,il_rel(10),nmax(10),a0(10),gam(10) 
c set factorial function
      call set_fac()
      open(unit=33,file='ene.d',status='unknown')!エネルギー準位
      open(unit=55,file='cld.d',status='unknown')!連続状態密度
      open(unit=56,file='phs.d',status='unknown')
      open(unit=10,file='phs_ana.d',status='unknown')!位相差
      rewind 5
      read(5,'(a)') memo
      read(5,'(a)') memo
      read(5,*) in1,in2
      read(5,'(a)') memo
      read(5,*) ij_tot,is_tot,it_tot
      read(5,'(a)') memo
      read(5,*) degi,degf,ddeg
      read(5,'(a)') memo
      read(5,*) bbi,bbf,dbb
      read(5,'(a)') memo
      read(5,*) no_ch
      read(5,'(a)') memo
      ntot=0
      do i=1,no_ch
        read(5,*) il_rel(i),nmax(i),a0(i),gam(i)
        ntot=ntot+nmax(i)
      enddo

      ndeg=nint((degf-degi)/ddeg)!刻む回数
      nbb =nint((bbf-bbi)/dbb)
      
      do ideg=0,ndeg!tragect complex scalling
        deg=degi+ddeg*ideg
      do ibb=0,nbb!tragect gauss
        bb=bbi+dbb*ibb
        
        th=pi*deg/180.d0 !radian

        if(iabs(il_rel(i)-ij_tot).gt.is_tot
     &                        .or.il_rel(i)+ij_tot.lt.is_tot)
     &   goto 9999!sankaku futoushiki
        if(ntot.gt.nd) goto 9999

        do ich=1,no_ch
          do i=1,nmax(ich)
            za(ich,i)=1.d0/(bb*a0(ich)*gam(ich)**(i-1))**2
     &                     *exp(-2.d0*th*zi)
          enddo 
        enddo

        istart=0
        do ich=1,no_ch
        jstart=0
        do jch=1,ich
        do i=1,nmax(ich)
          jmax=nmax(jch)
          if(ich.eq.jch) jmax=i
          do j=1,jmax
          
            ii=istart+i
            jj=jstart+j

            dn(ii,jj)
     &        =dble(znorm(il_rel(ich),il_rel(jch),za(ich,i),za(jch,j)))
            dn0(ii,jj)
     &        =dble(znorm(il_rel(ich),il_rel(jch),za(ich,i),za(jch,j)))
            dn(jj,ii)=dn(ii,jj)
            dn0(jj,ii)=dn0(ii,jj)

            call kine(zt,il_rel(ich),il_rel(jch),za(ich,i),za(jch,j))

            call cent(zct,il_rel(ich),il_rel(jch)
     &                            ,is_tot,it_tot,za(ich,i),za(jch,j))
            call lsfrs(zcls,il_rel(ich),il_rel(jch)
     &                     ,ij_tot,is_tot,it_tot,za(ich,i),za(jch,j))
            call tnfrs(ztn,il_rel(ich),il_rel(jch)
     &                     ,ij_tot,is_tot,it_tot,za(ich,i),za(jch,j))

            if (in1.eq.1.and.in2.eq.1)
     &      call coulomb(zclb,il_rel(ich),il_rel(jch)
     6                                          ,za(ich,i),za(jch,j))
            
c           call pseudo(zpf,dl_rel,za(i),za(j))

            zh(ii,jj)=zt+zct+zcls+ztn+zclb
            zh0(ii,jj)=zt+zclb
            zh(jj,ii)=zh(ii,jj)
            zh0(jj,ii)=zh0(ii,jj)

          enddo
        enddo
        jstart=jstart+nmax(jch)
        enddo
        istart=istart+nmax(ich)
        enddo

        call eigenh(dn,zh,nd,ze,deg,nz,ntot,6,33,51)
        call eigenh(dn0,zh0,nd,ze0,deg,nz0,ntot,6,33,51)

c----continuum level density-------------------------------------------        
        dele=0.05d0
        write(55,*) 'energy,cld,resocld,conticld'
        do i=0,10000
          eval=dele*i
          cld(i)=0.d0
         do j=1,nz
           cld(i)=cld(i)-(1.d0/pi)*dimag(1.d0/(eval-ze(j)))
         enddo
         do j=1,nz0
           cld(i)=cld(i)+(1.d0/pi)*dimag(1.d0/(eval-ze0(j)))
         enddo

        j=22  !state 
        resocld(i)=-(1.d0/pi)*dimag(1.d0/(eval-ze(j)))
        conticld(i)=cld(i)-resocld(i)
        write(55,*) eval,cld(i),resocld(i),conticld(i)
        enddo
      
c----phase shift-------------------------------------------------------      
        write(56,*) 'eval,phs,resophs,contiphs' 
        write(10,*) 'eval[MeV]','phs[rad.]' 
       phs(0)=0.d0
       do i=1,10000
          eval=dele*i
          phs(i)=phs(i-1)+0.5d0*dele*180.d0*(cld(i)+cld(i-1))
c          resophs(i)
c     &     =resophs(i-1)+0.5d0*dele*180.d0*(resocld(i)+resocld(i-1))
cc----continnum contribution----
c          contiphs(i)
c     &     =contiphs(i-1)+0.5d0*dele*180.d0*(conticld(i)+conticld(i-1))
c       
       write(56,*) eval,phs(i),resophs(i),contiphs(i) 
     
c lorentzian analysis--------------------------------------------------
        aphs(i)=0.d0
        do k=1,nz
          fina=(dble(ze(k)-eval)/dimag(ze(k)))
          dini=dble(ze(k))/dimag(ze(k))
          aphs(i)=aphs(i)+atan(fina)-atan(dini)    
        enddo
        do k=1,nz0
          fina=(dble(ze0(k)-eval)/dimag(ze0(k)))
          dini=dble(ze0(k))/dimag(ze0(k))
          aphs(i)=aphs(i)-atan(fina)+atan(dini)    
        enddo

        fina=(dble(ze(j)-eval)/dimag(ze(j)))
        dini=dble(ze(j))/dimag(ze(j))
        aphsreso(i)=atan(fina)-atan(dini)
        aphsconti(i)=aphs(i)-aphsreso(i)
         
        write(10,*) eval
     &   ,aphs(i)*180.d0/pi
                         !,aphsreso(i)*180.d0/pi,aphsconti(i)*180.d0/pi
       enddo
       write(10,*) ""
c----------------------------------------------------------------------
c      zrad=(0.d0,0.d0)
c      do i=1,nmax
c        do j=1,nmax
c          zrad=zrad+zc(i,19)*zc(j,19)*zr(i,j)
c        enddo
c      enddo
c      write(*,*) 'r.m.s radius = ',cdsqrt(zrad),'[fm]'
c       
c T-matrix,scattering amplitude
c       dmu=0.8d0*dmass!reduced mass
c       do i=1,100
c         ddk=0.005d0
c         dk=ddk*i
c          de=0.1d0*i
c          dk=dsqrt(2.d0*dmu*de/hbc**2)
c part1
c         call cent_f1(tvalc1,dk,dl_rel)
c         call cenl_f1(tvalcl1,dk,dl_rel)
c         call lsfrs_f1(tvalls1,dk,dl_rel,dj_tot)
c         ztval1=tvalc1+tvalcl1+tvalls1
c part2-            
c         do j=1,nmax
c           call centmat(zmatc,dk,za(j),dl_rel)
c           call cenlmat(zmatcl,dk,za(j),dl_rel)
c           call lsmat(zmatls,dk,za(j),dl_rel,dj_tot)
c           zmat(j)=zmatc+zmatcl+zmatls
c         enddo
c         ztval2=0.d0
c         dei=(hbc*dk)**2.d0/2.d0/dmu
c         do j=1,nz
c           zscamp(j)=0.d0
c         do k=1,nmax
c           zscamp(j)=zscamp(j)+zc(k,j)*zmat(k)
c         enddo
c         enddo
c         do j=1,nz
c           ztval2=ztval2+(2.d0*dl_rel+1.d0)/4.d0/pi
c    &             *zmat(j)**2.d0/(dei-ze(j))
c         enddo
c        do j=1,nz
c          do k=1,nmax
c            do l=1,nmax
c              ztval2=ztval2+1.d0/4.d0/pi
c    &                    *zc(k,j)*zc(l,j)*zmat(k)*zmat(l)/(dei-ze(j))
c     &                    *ddk*dk**2.d0
c            enddo
c          enddo
c        enddo
c         
c         ztval=ztval1+ztval2          
c         zscamp(i)=(-2.d0*dmu/hbc**2.d0/dk**2.d0)*ztval!*(hbc**2*dk/dmu)*tval
c         zsval=1.d0-zi*zscamp(i)*dk*2.d0!dk*2.d0/zi*zscamp(i)
c         
c         write(57,*) dk,dei,ztval,zsval
c    &                 
c       enddo  
      enddo!end tragect gauss 
      enddo!end tragect complex scalling
      close(10)
      close(33)
      close(55)
      close(56)
      stop

9999  continue
      write(6,*) 'invalid input parameter'
      stop
      end program main


