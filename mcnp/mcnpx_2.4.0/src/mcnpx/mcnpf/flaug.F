!#ident flaug.F from deck fl4c                                                  !/*fl4c        1*/
#ifdef MESHTAL                                                                  /*!flx         1*/
      subroutine flaug(er,ie,mk,ik,qx)                                          !/*flx         2*/
#else                                                                           /*!flx         3*/
      subroutine flaug(er,ie,mk,ik)                                             !/*flx         4*/
#endif                                                                          /*!flx         5*/
!        generate fluourescent photons or auger electrons to fill               !/*fl4c        3*/
!        vacancies.  called by colidp and kxray.                                !/*fl4c        4*/
!                                                                               !/*fl4c        5*/
!        input arguments (not changed by subroutine):                           !/*fl4c        6*/
!           er=energy of absorbed photon or edge energy for kxray.              !/*fl4c        7*/
!           ie=element that is undergoing the interaction; for kxray            !/*fl4c        8*/
!              it is the highest z element.                                     !/*fl4c        9*/
!           mk=material.                                                        !/*fl4c       10*/
!           ik=ipt particle type of parent particle used for bookkeeping        !/*fl4c       11*/
!              and variance reduction; =2 for colidp; =3 for kxray.             !/*fl4c       12*/
      use global_data                                                           !/*flx         6*/
!                                                                               !/*flx         7*/
      implicit real(kindr) (a-h,o-z)                                            !/*flx         8*/
      implicit integer(kindi) (i-n)                                             !/*flx         9*/
!                                                                               !/*flx        10*/
      ntyn=3                                                                    !/*fl4c       14*/
      mtp=-3                                                                    !/*fl4c       15*/
      rp=0.                                                                     !/*fl4c       16*/
!                                                                               !/*fl4c       17*/
!        if incident particle is electron from kxray, setup photon              !/*fl4c       18*/
!        parameters and sample auger or photoelectric electrons.                !/*fl4c       19*/
      if(ik.eq.2)go to 10                                                       !/*fl4c       20*/
      ipt=2                                                                     !/*fl4c       21*/
      jan=0                                                                     !/*flxc        1*/
      vel=slite                                                                 !/*fl4c       22*/
      ncp=0                                                                     !/*fl4c       23*/
#ifdef MESHTAL                                                                  /*!flx        11*/
      mspf=2                                                                    !/*flx        12*/
#endif                                                                          /*!flx        13*/
      idx=0                                                                     !/*fl4c       24*/
      if(rang().gt.wwk(lwwk+mk))go to 130                                       !/*fl4c       25*/
!                                                                               !/*fl4c       26*/
!        integrated tiger series (its) method before its3.0.                    !/*fl4c       27*/
      if(nxs(lnxs+16,lme(llme+3,jmd(ljmd+mk))).eq.3)go to 10                    !/*fl4c       28*/
      if(iphot.ne.0.or.fiml(2).eq.0..or.eek(leek+mk).le.elc(2))return           !/*fl4c       29*/
      erg=eek(leek+mk)                                                          !/*fl4c       30*/
      call isos(uuu,lev)                                                        !/*fl4c       31*/
      ipsc=102                                                                  !/*fl4c       32*/
      go to 110                                                                 !/*fl4c       33*/
!                                                                               !/*fl4c       34*/
!       check for electron emission from impact ionzation.                      !/*fl4c       35*/
   10 if(nxs(lnxs+2,ie).le.11)go to 140                                         !/*fl4c       36*/
      i1=jxs(ljxs+4,ie)+2                                                       !/*fl4c       37*/
      if(er.le.xss(i1-1))go to 140                                              !/*fl4c       38*/
      do 20 ka=i1,i1-3+nxs(lnxs+4,ie)                                           !/*fl4c       39*/
   20 if(er.le.xss(ka))go to 30                                                 !/*fl4c       40*/
   30 k0=ka+nxs(lnxs+4,ie)-1                                                    !/*fl4c       41*/
   40 ka=k0                                                                     !/*fl4c       42*/
      t3=xss(ka)*rang()                                                         !/*fl4c       43*/
      ka=ka+nxs(lnxs+4,ie)                                                      !/*fl4c       44*/
      if(t3.lt.xss(ka))go to 50                                                 !/*fl4c       45*/
      if(ik.eq.2)go to 140                                                      !/*fl4c       46*/
      go to 40                                                                  !/*fl4c       47*/
!                                                                               !/*fl4c       48*/
!        at least single fluorescence or auger electron.                        !/*fl4c       49*/
   50 ka=ka-1                                                                   !/*fl4c       50*/
      if(t3.lt.xss(ka))go to 50                                                 !/*fl4c       51*/
      rp=1.                                                                     !/*fl4c       52*/
      eg=xss(ka-2*nxs(lnxs+4,ie)+1)                                             !/*fl4c       53*/
      if(kpt(3).eq.0)go to 60                                                   !/*fl4c       54*/
      if(ik.ne.2)go to 60                                                       !/*fl4c       56*/
      if(rang().gt.wwk(lwwk+mk))go to 130                                       !/*fl4c       57*/
   60 e=xss(nxs(lnxs+4,ie)+ka+1)                                                !/*fl4c       58*/
      if(e.lt.elc(2))go to 140                                                  !/*fl4c       59*/
      if(ik.ne.2)go to 70                                                       !/*fl4c       60*/
      call lose_pax (loss_photon_capture, 2, 1, wgt, wgt*erg)                   !/*flx        14*/
      call contrib_pwb (2,ipwb_photon_capture,icl,-wgt)                         !/*flx        15*/
      pan(kpan+2,3,mpan)=pan(kpan+2,3,mpan)+wgt                                 !/*fl4c       65*/
   70 ed=0.                                                                     !/*fl4c       66*/
      if(nxs(lnxs+2,ie).le.30)go to 100                                         !/*fl4c       67*/
      i=ka+1-2*nxs(lnxs+4,ie)                                                   !/*fl4c       68*/
      if(i.ne.i1.and.i.ne.i1+1)go to 100                                        !/*fl4c       69*/
      i=i1-2+nxs(lnxs+4,ie)                                                     !/*fl4c       70*/
      ka=nxs(lnxs+4,ie)+i+1                                                     !/*fl4c       71*/
      if(rang().ge.xss(ka)/(1.-xss(i)))go to 100                                !/*fl4c       72*/
!                                                                               !/*fl4c       73*/
!        double fluorescence.  bank the second photon.                          !/*fl4c       74*/
      erg=xss(nxs(lnxs+4,ie)+ka)                                                !/*fl4c       75*/
      if(erg.lt.elc(2))go to 100                                                !/*fl4c       76*/
      ntyn=4                                                                    !/*fl4c       77*/
      call isos(uuu,lev)                                                        !/*fl4c       78*/
      sw=wgt                                                                    !/*fl4c       79*/
      if(ik.eq.2)go to 80                                                       !/*fl4c       80*/
      call gain_pax (igain_photon_electron_x_rays,2,1,wgt,wgt*erg)              !/*flx        16*/
      call contrib_pwb (2,ipwb_photon_electron_x_rays,icl,wgt)                  !/*flx        17*/
      go to 90                                                                  !/*fl4c       85*/
   80 call gain_pax (igain_photon_2nd_fluorescence,2,1,wgt,wgt*erg)             !/*flx        18*/
      call contrib_pwb (2,ipwb_photon_flourescence,icl,wgt)                     !/*flx        19*/
      if(wwp(2,4).ne.0.)call wtwndo(4)                                          !/*fl4c       90*/
      if(nter.eq.0.and.espl(2,1).ne.0.)call ergimp                              !/*fl4c       91*/
      if(nter.eq.0)go to 90                                                     !/*fl4c       92*/
      call lose_pax (nter, 2, 1, wgt, wgt*erg)                                  !/*flx        20*/
      call contrib_pwb (2,jrwb(nter,2),icl,-wgt)                                !/*flx        21*/
      nter=0                                                                    !/*fl4c       97*/
      go to 100                                                                 !/*fl4c       98*/
   90 npa=1                                                                     !/*fl4c       99*/
      call bankit(9)                                                            !/*fl4c      100*/
#ifdef MESHTAL                                                                  /*!flx        22*/
      qx=qx-erg                                                                 !/*flx        23*/
#endif                                                                          /*!flx        24*/
      ed=erg                                                                    !/*fl4c      101*/
      wgt=sw                                                                    !/*fl4c      102*/
!                                                                               !/*fl4c      103*/
!        continue with first fluorescent photon.                                !/*fl4c      104*/
  100 if(ides.eq.0.and.ik.eq.2.and.eg0-eg-ed.gt.elc(3))                         !/*fl4c      105*/
     & call emaker(1,eg0-eg-ed)                                                 !/*fl4c      106*/
      erg=e                                                                     !/*fl4c      107*/
      ipsc=99+ntyn                                                              !/*fl4c      108*/
      call isos(uuu,lev)                                                        !/*fl4c      109*/
      if(ik.ne.2)go to 110                                                      !/*fl4c      110*/
      call gain_pax (igain_photon_1st_fluorescence, 2, 1, wgt, wgt*erg)         !/*flx        25*/
      call contrib_pwb (2,ipwb_photon_flourescence,icl,wgt)                     !/*flx        26*/
      return                                                                    !/*fl4c      115*/
!                                                                               !/*fl4c      116*/
!        electron xray. go to detector, dxtran, and bank.                       !/*fl4c      117*/
  110 if(jgp.le.-2)jgp=jgp+2                                                    !/*fl4c      118*/
      call gain_pax (igain_photon_electron_x_rays, 2, 1, wgt, wgt*erg)          !/*flx        27*/
      call contrib_pwb (2,ipwb_photon_electron_x_rays,icl,wgt)                  !/*flx        28*/
      do 120 i=1,ndx(2)                                                         !/*fl4c      123*/
  120 if((xxx-dxx(2,1,i))**2+(yyy-dxx(2,2,i))**2+                               !/*fl4c      124*/
     & (zzz-dxx(2,3,i))**2.lt.dxx(2,5,i))idx=i                                  !/*fl4c      125*/
      if(ndet(2).ne.0)call tallyd                                               !/*fl4c      126*/
      if(kdb.ne.0)return                                                        !/*fl4c      127*/
      if(ndx(2).gt.1.or.ndx(2).eq.1.and.idx.eq.0)call dxtran                    !/*fl4c      128*/
      if(kdb.ne.0)return                                                        !/*fl4c      129*/
      call bankit(18)                                                           !/*fl4c      130*/
#ifdef MESHTAL                                                                  /*!flx        29*/
      qx=qx-erg                                                                 !/*flx        30*/
#endif                                                                          /*!flx        31*/
      return                                                                    !/*fl4c      131*/
!                                                                               !/*fl4c      132*/
!        make auger electrons.                                                  !/*fl4c      133*/
!        set ipt=ik for bookkeeping.                                            !/*fl4c      134*/
  130 ipt=ik                                                                    !/*fl4c      135*/
      jan=not_self_anti(ipt)                                                    !/*flxc        2*/
      if(ides.eq.0.and.eek(leek+mk).gt.elc(3).and.er.ge.edg(ledg+mk))           !/*fl4c      136*/
     & call emaker(4,eek(leek+mk))                                              !/*fl4c      137*/
!                                                                               !/*fl4c      138*/
!        make photoelectric electrons.                                          !/*fl4c      139*/
  140 if(ik.ne.2)return                                                         !/*fl4c      140*/
      if(ides.eq.0.and.eg0-rp*edg(ledg+mk).gt.elc(3))                           !/*fl4c      141*/
     & call emaker(1,eg0-rp*edg(ledg+mk))                                       !/*fl4c      142*/
      nter = loss_photon_capture                                                !/*flx        32*/
      pan(kpan+2,3,mpan)=pan(kpan+2,3,mpan)+wgt                                 !/*fl4c      144*/
      return                                                                    !/*fl4c      145*/
      end                                                                       !/*fl4c      146*/
