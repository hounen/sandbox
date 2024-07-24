      program main
      include 'li_inc.f'      
      character*80 memo 
      dimension za(nd),dn(nd,nd),zh(nd,nd),ze(nd),zr(nd,nd),zc(nd,nd)
     &         ,zh0(nd,nd),ze0(nd),dn0(nd,nd),zc0(nd,nd),zmat(nd)
     &         ,cld(0:100000),resocld(0:100000),conticld(0:100000)
     &         ,phs(0:100000),resophs(0:100000),contiphs(0:100000)
     &         ,aphs(0:100000),aphsreso(0:100000),aphsconti(0:100000)
     &         ,zscamp(100) 
c set factorial function
      call set_fac()
      open(unit=33,file='ene.d',status='unknown')!エネルギー準位
      open(unit=55,file='cld.d',status='unknown')!連続状態密度
      open(unit=56,file='phs.d',status='unknown')!位相差
      open(unit=10,file='phs_ana.d',status='unknown')
      rewind 5
      read(5,'(a)') memo
      read(5,'(a)') memo
      read(5,*) dl_rel, dj_tot!角運動量,全角運動量
      read(5,'(a)') memo
      read(5,*) nmax, a0, gam!基底の数、初項、公比
      read(5,'(a)') memo
      read(5,*) degi,degf,ddeg!tragect complex scalling
      read(5,'(a)') memo
      read(5,*) bbi,bbf,dbb!tragect gauss
      read(5,'(a)') memo
      read(5,*) ggi,ggf,dgg!tragect gamma

      ndeg=nint((degf-degi)/ddeg)!刻む回数
      nbb =nint((bbf-bbi)/dbb)!刻む回数
      ngg=nint((ggf-ggi)/dgg)!刻む回数
      
      do ideg=0,ndeg!tragect complex scalling
        deg=degi+ddeg*ideg
      do ibb=0,nbb!tragect gauss
        bb=bbi+dbb*ibb
      do igg=0,ngg!tragect gamma
        gg=ggi+dgg*igg
        
        th=pi*deg/180.d0 !radian表示

        if(dabs(dl_rel-dj_tot).gt.0.5d0.or.dl_rel+dj_tot.lt.0.5d0)
     &   goto 9999!三角不等式の範囲内かどうか
        if(nmax.gt.nd) goto 9999!基底の数が配列ndの範囲内かどうか

        do i=1,nmax
          za(i)=1.d0/(bb*a0*gg*gam**(i-1))**2*exp(-2.d0*th*zi)
        enddo 

        do i=1,nmax
          do j=1,i
            dn(i,j)=dble(znorm(dl_rel,dl_rel,za(i),za(j)))
            dn(j,i)=dn(i,j)!対称行列
            dn0(i,j)=dble(znorm(dl_rel,dl_rel,za(i),za(j)))
            dn0(j,i)=dn0(i,j)!対称行列

            call kine(zt,dl_rel,za(i),za(j))
            call cent(zct,dl_rel,za(i),za(j))
            call cenl(zcl,dl_rel,za(i),za(j))
            call lsfrs(zcls,dl_rel,dj_tot,za(i),za(j))
            call coulomb(zclb,dl_rel,za(i),za(j))
            call coulomb0(zclb0,dl_rel,za(i),za(j))
            call pseudo(zpf,dl_rel,za(i),za(j))

            zh(i,j)=zt+zct+zcl+zcls+zpf+zclb !hamiltonianを配列
            zh(j,i)=zh(i,j)!対称行列
            zh0(i,j)=zt+zclb0
            zh0(j,i)=zh0(i,j)
            zr(i,j)=zinteg(dl_rel,dl_rel,za(i),za(j),2,0.d0)
            zr(j,i)=zr(i,j)
          enddo
        enddo

        call eigenh(deg,nmax,nz,zh,ze,dn,zc)
        call eigenh(deg,nmax,nz0,zh0,ze0,dn0,zc0)
        
c----continuum level density-------------------------------------------        
        dele=0.001d0
        write(55,*) 'energy,cld,resocld,conticld'
        do i=0,10000
          eval=dele*i
c----scattering----
          cld(i)=0.d0
         do j=1,nz
           cld(i)=cld(i)-(1.d0/pi)*dimag(1.d0/(eval-zh(j,j)))
         enddo
         do j=1,nz0
           cld(i)=cld(i)+(1.d0/pi)*dimag(1.d0/(eval-zh0(j,j)))
         enddo
c----resonant---- 
        j=21  !state 
        resocld(i)=-(1.d0/pi)*dimag(1.d0/(eval-zh(j,j)))
c----continnum---- 
        conticld(i)=cld(i)-resocld(i)
        
        write(55,*) eval,cld(i),resocld(i),conticld(i)
        enddo
c----------------------------------------------------------------------        
      
c----phase shift-------------------------------------------------------      
        write(56,*) 'eval,phs,resophs,contiphs' 
        write(10,*) 'eval[MeV]','phs[rad.]' 
       phs(0)=0.d0
       do i=1,10000
          eval=dele*i
c----scattering----
          phs(i)=phs(i-1)+0.5d0*dele*180.d0*(cld(i)+cld(i-1))
cc----resonant contribution----
c          resophs(i)
c     &     =resophs(i-1)+0.5d0*dele*180.d0*(resocld(i)+resocld(i-1))
cc----continnum contribution----
c          contiphs(i)
c     &     =contiphs(i-1)+0.5d0*dele*180.d0*(conticld(i)+conticld(i-1))
c       
       write(56,*) eval,phs(i),resophs(i),contiphs(i) 
     
c lorentzian analysis--------------------------------------------------
c----scattering----       
        aphs(i)=0.d0
        do k=1,nz
          fina=(dble(zh(k,k)-eval)/dimag(zh(k,k)))
          dini=dble(zh(k,k))/dimag(zh(k,k))
          aphs(i)=aphs(i)+atan(fina)-atan(dini)    
        enddo
        do k=1,nz0
          fina=(dble(zh0(k,k)-eval)/dimag(zh0(k,k)))
          dini=dble(zh0(k,k))/dimag(zh0(k,k))
          aphs(i)=aphs(i)-atan(fina)+atan(dini)    
        enddo

c----resonance contribution----        
        fina=(dble(zh(j,j)-eval)/dimag(zh(j,j)))
        dini=dble(zh(j,j))/dimag(zh(j,j))
        aphsreso(i)=atan(fina)-atan(dini)
c---continuum contribution----
        aphsconti(i)=aphs(i)-aphsreso(i)
         
        write(10,*) eval
     &   ,aphs(i)*180.d0/pi,aphsreso(i)*180.d0/pi,aphsconti(i)*180.d0/pi
c-----------------------------------------------------------------------        
       enddo
       
       zrad=(0.d0,0.d0)
       do i=1,nmax
         do j=1,nmax
           zrad=zrad+zc(i,19)*zc(j,19)*zr(i,j)
         enddo
       enddo
       write(*,*) 'r.m.s radius = ',cdsqrt(zrad),'[fm]'
        
c T-matrix,scattering amplitude
        dmu=0.8d0*dmass!reduced mass
        do i=1,100
          ddk=0.005d0
          dk=ddk*i
c          de=0.1d0*i
c          dk=dsqrt(2.d0*dmu*de/hbc**2)
c part1
          call cent_f1(tvalc1,dk,dl_rel)
          call cenl_f1(tvalcl1,dk,dl_rel)
          call lsfrs_f1(tvalls1,dk,dl_rel,dj_tot)
          ztval1=tvalc1+tvalcl1+tvalls1
c part2-            
          do j=1,nmax
            call centmat(zmatc,dk,za(j),dl_rel)
            call cenlmat(zmatcl,dk,za(j),dl_rel)
            call lsmat(zmatls,dk,za(j),dl_rel,dj_tot)
            zmat(j)=zmatc+zmatcl+zmatls
          enddo
          ztval2=0.d0
          dei=(hbc*dk)**2.d0/2.d0/dmu
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
         do j=1,nz
           do k=1,nmax
             do l=1,nmax
               ztval2=ztval2+1.d0/4.d0/pi
     &                    *zc(k,j)*zc(l,j)*zmat(k)*zmat(l)/(dei-ze(j))
c     &                    *ddk*dk**2.d0
             enddo
           enddo
         enddo
          
          ztval=ztval1+ztval2          
          zscamp(i)=(-2.d0*dmu/hbc**2.d0/dk**2.d0)*ztval!*(hbc**2*dk/dmu)*tval
          zsval=1.d0-zi*zscamp(i)*dk*2.d0!dk*2.d0/zi*zscamp(i)
          
          write(57,*) dk,dei,ztval,zsval
     &                 
        enddo  
      enddo!end tragect gamma
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


