!#ident ttbr.F from deck tb                                                     !/*tb          1*/
      subroutine ttbr(d)                                                        !/*tb          2*/
!        generate bremsstrahlung photons with the thick-target                  !/*tb          3*/
!        bremsstrahlung approximation.                                          !/*tb          4*/
!        d = length of path segment.                                            !/*tb          5*/
      use global_data                                                           !/*tbx         1*/
!                                                                               !/*tbx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*tbx         3*/
      implicit integer(kindi) (i-n)                                             !/*tbx         4*/
!                                                                               !/*tbx         5*/
!                                                                               !/*tb4c2       1*/
!        no need to do ttbr below energy threshold.                             !/*tb4c2       2*/
      if(dbcn(20).eq.0.)then                                                    !/*tb4c2       3*/
      if(erg.le.eee(leee+3,nee(3)))return                                       !/*tbx         6*/
      endif                                                                     !/*tb4c2       5*/
!                                                                               !/*tb          7*/
!        find the electron energy group.                                        !/*tb          8*/
      m=1                                                                       !/*tb          9*/
      ib=nee(3)                                                                 !/*tbx         7*/
   10 if(ib-m.eq.1)go to 30                                                     !/*tb         11*/
      ih=(m+ib)/2                                                               !/*tb         12*/
      if(erg.ge.eee(leee+3,ih))go to 20                                         !/*tbx         8*/
      m=ih                                                                      !/*tb         14*/
      go to 10                                                                  !/*tb         15*/
   20 ib=ih                                                                     !/*tb         16*/
      go to 10                                                                  !/*tb         17*/
   30 f=(eee(leee+3,m)-erg)/(eee(leee+3,m)-eee(leee+3,m+1))                     !/*tbx         9*/
!                                                                               !/*tb         19*/
!        decide how many photons to make.                                       !/*tb         20*/
      mkc=mat(lmat+icl)                                                         !/*tb         21*/
      pr=(1.-f)*pbt(lpbt+m+nee(3)*(mkc-1))+                                     !/*tbx        10*/
     & f*pbt(lpbt+m+1+nee(3)*(mkc-1))                                           !/*tbx        11*/
      pr=pr*abs(bnum)                                                           !/*tb4c        1*/
      ni=pr+rang()                                                              !/*tb         24*/
      if(ni.eq.0)return                                                         !/*tb         25*/
!                                                                               !/*tb         26*/
!        prepare to make photons; protect variables from tallyd, dxtran.        !/*tb4a        1*/
      call savpar(0,3)                                                          !/*tb4c2       6*/
      if(d.ne.0.)call uplpos(xxx,uold,lev,.5*d,vel,1)                           !/*tb4b        1*/
      u=uold(1)                                                                 !/*tb4b        2*/
      v=uold(2)                                                                 !/*tb4b        3*/
      w=uold(3)                                                                 !/*tb4b        4*/
      uold(1)=uuu                                                               !/*tb4b        5*/
      uold(2)=vvv                                                               !/*tb4b        6*/
      uold(3)=www                                                               !/*tb4b        7*/
      if(abs(bnum).gt.0..and.bnum.ne.1.)wgt=wgt/abs(bnum)                       !/*tb4c        2*/
      wt=wgt                                                                    !/*tb         47*/
      ipt=2                                                                     !/*tb         48*/
      jan=0                                                                     !/*tbxc        1*/
      if(jgp.le.-2)jgp=jgp+2                                                    !/*cor4-1    154*/
      vel=slite                                                                 !/*tb4a        2*/
      npa=1                                                                     !/*tb         52*/
      ncp=0                                                                     !/*tb         53*/
#ifdef MESHTAL                                                                  /*!tbx        12*/
      mspf=2                                                                    !/*tbx        13*/
#endif                                                                          /*!tbx        14*/
      jsu=0                                                                     !/*tb         54*/
      idx=0                                                                     !/*tb         55*/
      do 100 i=1,ndx(2)                                                         !/*tb         56*/
  100 if((xxx-dxx(2,1,i))**2+(yyy-dxx(2,2,i))**2+                               !/*tb         57*/
     & (zzz-dxx(2,3,i))**2.lt.dxx(2,5,i))idx=i                                  !/*tb         58*/
      ipsc=13                                                                   !/*tb         59*/
      deb=sqrt(erg*(erg+2.*gpt(3)))/(erg+gpt(3))                                !/*tb         60*/
!                                                                               !/*tb         62*/
!        make photons and bank them.                                            !/*tb         63*/
!        approximation:  photon direction equals electron direction.            !/*tb         64*/
      do 160 n=1,ni                                                             !/*tb         65*/
      rn=rang()                                                                 !/*tb         66*/
      pk=qpol(rn,ebt(lebt+1,m+nee(3)*(mkc-1)),rkt,ntop)                         !/*tbx        15*/
      pa=qpol(rn,ebt(lebt+1,m+1+nee(3)*(mkc-1)),rkt,ntop)                       !/*tbx        16*/
      erg=((1.-f)*pk+f*pa)*erg9(npb)                                            !/*tb         69*/
      if(erg.lt.elc(2))then                                                     !/*tb4c2       7*/
         if(dbcn(20).ne.0.)go to 160                                            !/*tb4c2       8*/
      call gain_pax(igain_photon_bremsstrahlung, 2, 0, zero, wgt*erg)           !/*tbx        17*/
      call lose_pax (loss_all_pls_energy_cutoff, 2, 0, zero, wgt*erg)           !/*tbx        18*/
         go to 160                                                              !/*tb4c2      11*/
      endif                                                                     !/*tb4c2      12*/
      wgt=wt                                                                    !/*tb         71*/
      if(mbi(lmbi+mkc).eq.0)go to 150                                           !/*tb         72*/
      do 110 j1=2,ntop-1                                                        !/*tb4c        5*/
  110 if(pk.le.rkt(j1))go to 120                                                !/*tb         74*/
  120 do 130 j=2,ntop-1                                                         !/*tb4c        6*/
  130 if(pa.le.rkt(j))go to 140                                                 !/*tb         76*/
  140 wgt=wt*((1.-f)*ftt(lftt+m+nee(3)*(mkc-1))*bbrem(j1)+                      !/*tbx        19*/
     & f*ftt(lftt+m+1+nee(3)*(mkc-1))*bbrem(j))                                 !/*tbx        20*/
  150 call gain_pax (igain_photon_bremsstrahlung, 2, 1, wgt, wgt*erg)           !/*tbx        21*/
      call contrib_pwb (2,ipwb_photon_bremsstrahlung,icl,wgt)                   !/*tbx        22*/
      if(ndet(2).ne.0)call tallyd                                               !/*tb         83*/
      if(kdb.ne.0)return                                                        !/*tb         84*/
      if(ndx(2).gt.1.or.ndx(2).eq.1.and.idx.eq.0)call dxtran                    !/*tb         85*/
      if(kdb.ne.0)return                                                        !/*tb         86*/
      call bankit(16)                                                           !/*tb4a        3*/
  160 continue                                                                  !/*tb         88*/
!                                                                               !/*tb         89*/
      call getpar(0,3)                                                          !/*tb4c2      13*/
      uold(1)=u                                                                 !/*tb4b        8*/
      uold(2)=v                                                                 !/*tb4b        9*/
      uold(3)=w                                                                 !/*tb4b       10*/
      return                                                                    !/*tb        100*/
      end                                                                       !/*tb        101*/
