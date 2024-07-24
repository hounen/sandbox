c==== main program =====================================================
      program main
      include 'DIM_INC.f'
      dimension cld(0:10000),cldreso(0:10000),cldconti(0:10000)
     &         ,phs(0:10000),phsreso(0:10000),phsconti(0:10000)
     &         ,cross(0:10000),crossreso(0:10000),crossconti(0:10000)

      open(unit=33,file='ENE.d',status='unknown')!Energy
      open(unit=55,file='CLD.d',status='unknown')!ContinuumLevelDensity
      open(unit=56,file='PHS.d',status='unknown')!PhaseShift
      open(unit=57,file='CROSS.d',status='unknown')!CrossSection
c  
c version report
c
      write(6,fmt=100) 
      write(6,fmt=101)
      write(6,fmt=100)
      write(33,fmt=100)
      write(33,fmt=101)
      write(33,fmt=100)
      rewind 5
      read(5,'(a)') memo
      read(5,'(a)') memo
      read(5,'(a)') memo
      read(5,*)     ltot,db0,dgam
      read(5,'(a)') memo
      read(5,'(a)') memo
      read(5,*)     degi,degf,ddeg
      read(5,'(a)') memo
      read(5,*)     bbi,bbf,dbb
      read(5,'(a)') memo
      read(5,*)     nni,nnf,dnn
c
c trajectory
c
      no_tr=0
      ndeg=idint((degf-degi)/ddeg+0.01d0)+1
      nbb =idint((bbf-bbi)/dbb+0.01d0)+1
      ndn =idint((nnf-nni)/dnn+0.01d0)+1
      do ideg=1,ndeg
        deg=degi+ddeg*(ideg-1)
      do ibb=1,nbb
        bb=bbi+dbb*(ibb-1)
        no_tr=no_tr+1 
      do inn=1,ndn
        nb=nni+dnn*(inn-1)
        write(6,*)
        write(6,fmt=102) no_tr
        write(6,fmt=103) deg,bb
        write(33,*)
        write(33,fmt=102) no_tr
        write(33,fmt=103) deg,bb
        call set_fac
        call set_base(deg,bb)
        call set_mat
c----------------------------------------------------------------------
c comments on declaration of variables
c energy eigenvalus are set into ze, which is given as a one-dimensonal
c array. the size of ze is equal to ntot.
c number of states is given by nz. initial value of nz is dummy, and nz
c is replaced by a integer value in subroutine named eigenh.
c----------------------------------------------------------------------
        call eigenh(dn,zh,ze,deg,nz,nb,ntot)
        call eigenh(dn0,zh0,ze0,deg,nz0,nb,ntot)

        dele=0.005d0
        write(55,*) 'energy,cld,cldreso,cldconti'
        write(56,*) 'energy,phs,phsreso,phsconti' 

        do i=1,10000
         eval=dele*i
         !scattering
         cld(i)=0.d0
         do j=1,nz
           cld(i)=cld(i)-(1.d0/pi)*dimag(1.d0/(eval-ze(j)))
         enddo
         do j=1,nz0
           cld(i)=cld(i)+(1.d0/pi)*dimag(1.d0/(eval-ze0(j)))
         enddo
         
        !resonant 
c----------------------------------------------------------------------
        j=14
        l=19 !resonant state 
c----------------------------------------------------------------------
        cldreso(i)=0.d0
        cldconti(i)=0.d0
        if (l.gt.j) then
          do a=j,l,l-j
            cldreso(i)=cldreso(i)-(1.d0/pi)*dimag(1.d0/(eval-ze(a)))
            cldconti(i)=cld(i)-cldreso(i)
          enddo
        end if
        if (j.ge.l) then
         cldreso(i)=cldreso(i)-(1.d0/pi)*dimag(1.d0/(eval-ze(j)))
         cldconti(i)=cld(i)-cldreso(i)
        end if
        write(55,fmt=104) eval,cld(i),cldreso(i),cldconti(i)

c phase shift~lorentzian analysis---------------------------------------
c----scattering----       
        phs(i)=0.d0
        do k=1,nz
          fina=(dble(ze(k))-eval)/dimag(ze(k))
          dini=dble(ze(k))/dimag(ze(k))
          phs(i)=phs(i)+atan(fina)-atan(dini)    
        enddo
        do k=1,nz0
          fina=(dble(ze0(k))-eval)/dimag(ze0(k))
          dini=dble(ze0(k))/dimag(ze0(k))
          phs(i)=phs(i)-atan(fina)+atan(dini)    
        enddo

c----resonance contribution----        
        phsreso(i)=0.d0
        phsconti(i)=0.d0
        if (l.gt.j) then
          do a=j,l,l-j
            fina=dble(ze(a)-eval)/dimag(ze(a))
            dini=dble(ze(a))/dimag(ze(a))
            phsreso(i)=phsreso(i)+atan(fina)-atan(dini)
          enddo
        end if
        if (j.ge.l) then
          fina=dble(ze(j)-eval)/dimag(ze(j))
          dini=dble(ze(j))/dimag(ze(j))
          phsreso(i)=phsreso(i)+atan(fina)-atan(dini)
        endif
c---continuum contribution----
        phsconti(i)=phs(i)-phsreso(i)
        write(56,fmt=104) eval
     &   ,phs(i)*180.d0/pi,phsreso(i)*180.d0/pi,phsconti(i)*180.d0/pi

c---cross section-----------------------------------------------------
        dmh=hbc**2.d0/1.6d0/dmass
        cross(i)=4.d0*pi*dmh/eval*(2.d0*ltot+1.d0)*(dsin(phs(i)))**2.d0
c----resonance contribution----        
        crossreso(i)=4.d0*pi*dmh/eval*(2.d0*ltot+1.d0)
     &              *(dsin(phsreso(i)))**2.d0
c        breit=(dimag(zh(j,j)))**2.d0/((eval-dble(zh(j,j)))**2.d0
c     &       +(dimag(zh(j,j)))**2.d0)
c        crosereso(i)=4.d0*pi*dmh/eval*(2.d0*dl+1.d0)*breit
c----continuum contribution----        
        crossconti(i)=cross(i)-crossreso(i)
        write(57,*) eval,cross(i),crossreso(i),crossconti(i)
        enddo
c----------------------------------------------------------------------
        write(6,fmt=100)
        write(33,fmt=100)
        write(55,*)
      enddo
      enddo
      enddo
      close(33)
      close(55)
      close(56)
      close(57)
      stop
c
c format
c
  100 format('#-------------------------------------------------------',
     &       '---------------')
  101 format('# program for 16o+4hE system'/
     &      ,'# mdoel : gaussian expansion method + complex scaling'/
     &      ,'# last update : 23th feb. 2009'/
     &      ,'# this program is writeen by yuma kikuchi')
  102 format('# number of trajectory :',i3)
  103 format('# ( theta , bi/b0 ) = (',f6.2,' [DEG] ,',f6.2,' [FM] )'/)
  104 format(f6.2,' ',f15.7,' ',f15.7,' ',f15.7)        
      end
c=======================================================================
