!#ident hstory.F from deck hs                                                   !/*hs          1*/
      subroutine hstory                                                         !/*hs          2*/
!        run the complete history of a source particle.                         !/*hs          3*/
      use global_data                                                           !/*hsx         1*/
!                                                                               !/*hsx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*hsx         3*/
      implicit integer(kindi) (i-n)                                             !/*hsx         4*/
!                                                                               !/*hsx         5*/
#ifdef MESHTAL                                                                  /*!hsx         6*/
      dimension zd(10),jz(5)                                                    !/*hsx         7*/
#endif                                                                          /*!hsx         8*/
      character he*4                                                            !/*hs          6*/
!                                                                               !/*hs          7*/
!        debug features:  set up event log.  print debug line.                  !/*hs          8*/
      krflg=0                                                                   !/*hs          9*/
      if(npstc.ge.dbcn(3).and.npstc.le.dbcn(4))krflg=1                          !/*hs         10*/
      if(dbcn(2).eq.0.)go to 20                                                 !/*hs         11*/
      if(mod(npstc,int(dbcn(2))).ne.0)go to 20                                  !/*hsx         9*/
      c=0.                                                                      !/*hsx        10*/
      do 10 i=1,nipt                                                            !/*hsx        11*/
   10 c=c+coll(i)+colltc(i)                                                     !/*hsx        12*/
      write(iuo,'(5h dbcn,i10,f21.0,tl1,f21.0,tl1,f21.0,tl1,1h )')              !/*hsx        13*/
     & npstc,c,rnr+rnrtc,rijktc                                                 !/*hsx        14*/
!                                                                               !/*hs         15*/
!        start a particle from the source.                                      !/*hs         16*/
   20 continue                                                                  !/*hsx        15*/
      call z_hold                                                               !/*hsx        17*/
      call startp                                                               !/*hsx        18*/
      if(nomore.ne.0) then                                                      !/*hsxk        1*/
#ifdef MULTT                                                                    /*!hsxk        2*/
        call thread_lockoff(jlock)                                              !/*hsxk        3*/
#endif                                                                          /*!hsxk        4*/
        return                                                                  !/*hsxk        5*/
      else                                                                      !/*hsxk        6*/
#ifdef MULTT                                                                    /*!hsxk        7*/
        call thread_lockoff(jlock)                                              !/*hsxk        8*/
#endif                                                                          /*!hsxk        9*/
      endif                                                                     !/*hsxk       10*/
#ifdef RADIOG                                                                   /*!hsx        19*/
      if(notrn.ne.0)nter=loss_all_pls_escape                                    !/*hsx        20*/
#endif                                                                          /*!hsx        21*/
      if(nter.ne.0)go to 260                                                    !/*hs         18*/
      if(kdb.ne.0)go to 390                                                     !/*hs         19*/
!                                                                               !/*hs         21*/
!        terminate conventional particle if its energy is below cutoff.         !/*hsx        22*/
!        most banked particles come back here.                                  !/*hs         23*/
   30 if(ipt.eq.2.or.ipt.eq.3.or.ipt.eq.1.and.jan.eq.1) then                    !/*hsx        23*/
         if(erg.lt.elc(ipt).eqv.mcal.lt.2)go to 220                             !/*hsx        24*/
      endif                                                                     !/*hsx        25*/
!                                                                               !/*hsx        26*/
!        run various particles separately.                                      !/*hsx        27*/
      if (ipt.eq.1.and.jan.eq.1.and.erg.le.cut_n.and.                           !/*hsx        28*/
     & lcaopt(8).ne.-1) then                                                    !/*hsx        29*/
         go to 40                                                               !/*hsx        30*/
      elseif(ipt.eq.2) then                                                     !/*hsx        31*/
         go to 40                                                               !/*hsx        32*/
      elseif(ipt.eq.3) then                                                     !/*hsx        33*/
         call electr                                                            !/*hsx        34*/
      elseif(ipt.eq.9.and.jan.eq.1.and.erg.le.cut_h) then                       !/*hsx        35*/
         call low_prot                                                          !/*hsx        36*/
      elseif(charge(ipt).ne.zero) then                                          !/*hsx        37*/
         call chg_pl                                                            !/*hsx        38*/
      else                                                                      !/*hsx        39*/
         call neu_pl                                                            !/*hsx        40*/
      endif                                                                     !/*hsx        41*/
      if(kdb.ne.0)go to 390                                                     !/*hs         29*/
      go to 260                                                                 !/*hs         30*/
!                                                                               !/*hs         31*/
!        calculate the distance to the cell boundary, dls.                      !/*hs         32*/
   40 if(lca(llca+icl).lt.0)call chkcel(icl,3,j)                                !/*hs         33*/
   50 if(wgt.le.0.)go to 380                                                    !/*hs         34*/
      call track(icl)                                                           !/*hs         35*/
      if(kdb.ne.0)go to 390                                                     !/*hs         36*/
!                                                                               !/*hs         37*/
!        calculate the distance to the nearest dxtran sphere, dxl.              !/*hs         38*/
      dxl=huge                                                                  !/*hs         39*/
      do 60 i=1,ndx(ipt)                                                        !/*hs         40*/
      if(idx.eq.i)go to 60                                                      !/*hs         41*/
      if(lev.ne.0)go to 55                                                      !/*hs4c        2*/
      f=dxx(ipt,1,i)-xxx                                                        !/*hs         42*/
      g=dxx(ipt,2,i)-yyy                                                        !/*hs         43*/
      h=dxx(ipt,3,i)-zzz                                                        !/*hs         44*/
      q=f*uuu+g*vvv+h*www                                                       !/*hs         45*/
      c=min(max(zero,q),dxl)                                                    !/*hs         46*/
      if((f-uuu*c)**2+(g-vvv*c)**2+(h-www*c)**2.lt.dxx(ipt,5,i))                !/*hs         47*/
     & dxl=min(dxl,q-sqrt(max(zero,q**2+dxx(ipt,5,i)-f**2-g**2-h**2)))          !/*hs         48*/
      go to 60                                                                  !/*hs4b        2*/
   55 f=dxx(ipt,1,i)-udt(1,0)                                                   !/*hs4b        3*/
      g=dxx(ipt,2,i)-udt(2,0)                                                   !/*hs4b        4*/
      h=dxx(ipt,3,i)-udt(3,0)                                                   !/*hs4b        5*/
      q=f*udt(4,0)+g*udt(5,0)+h*udt(6,0)                                        !/*hs4b        6*/
      c=min(max(zero,q),dxl)                                                    !/*hs4b        7*/
      if((f-udt(4,0)*c)**2+(g-udt(5,0)*c)**2+(h-udt(6,0)*c)**2                  !/*hs4b        8*/
     & .lt.dxx(ipt,5,i))dxl=min(dxl,q-sqrt(max(zero,q**2+dxx(ipt,5,i)-          !/*hs4b        9*/
     & f**2-g**2-h**2)))                                                        !/*hs4b       10*/
   60 continue                                                                  !/*hs         49*/
!                                                                               !/*hs         50*/
!        calculate the distance to time cutoff, dtc.                            !/*hs         51*/
      dtc=vel*(tco(ipt)-tme)                                                    !/*hs         52*/
!                                                                               !/*hs         53*/
!        calculate the cross sections in this cell.                             !/*hs         54*/
      rh=rho(lrho+icl)                                                          !/*hs4c        3*/
      mk=mat(lmat+icl)                                                          !/*hs         55*/
      totm=0.                                                                   !/*hs         56*/
      pfp=0.                                                                    !/*hs         57*/
      stp=0.                                                                    !/*hs         58*/
      deb=huge                                                                  !/*hs         59*/
      if(mk.eq.0)go to 70                                                       !/*hs         60*/
      if(ipt.eq.1)call acetot(0,mk)                                             !/*hs         61*/
      if(ipt.eq.2)call photot(mk)                                               !/*hs         62*/
!                                                                               !/*hs         63*/
!        special treatment for multigroup electrons.                            !/*hs         64*/
      if(mcal.eq.0)go to 70                                                     !/*hs         65*/
      pfp=10.*pfp                                                               !/*hs         66*/
      if(stp.eq.0.)go to 70                                                     !/*hs         67*/
      m=jxs(ljxs+1,mgegbt(1))+jgp-1                                             !/*hs         68*/
      if(mcal.eq.1)deb=(erg-xss(m)+.5*xss(m+jgm(1)))/stp                        !/*hs         69*/
      if(mcal.eq.2)deb=(xss(m)+.5*xss(m+jgm(1))-erg)/stp                        !/*hs         70*/
!                                                                               !/*hs         71*/
!        calculate the mean free path, gs, and its reciprocal, qpl.             !/*hs         72*/
   70 gs=0.                                                                     !/*hs         73*/
      pmf=huge                                                                  !/*hs         74*/
      dw=huge                                                                   !/*hs4b       11*/
      qpl=(totm+pfp)*rh                                                         !/*hs4c        4*/
      ple=qpl                                                                   !/*hs         76*/
      if(ple.eq.0.)go to 90                                                     !/*hs         77*/
      if(pfp.ne.0.)pfp=pfp/(totm+pfp)                                           !/*hs4c        6*/
      if(qax(lqax+ipt,icl).ne.0.)call extran                                    !/*hs         79*/
      if(qpl.le.0.)go to 90                                                     !/*hs         80*/
      gs=1./qpl                                                                 !/*hs         81*/
!                                                                               !/*hs4b       12*/
!        if using a weight-window mesh, limit track to 1 mfp.                   !/*hs4b       13*/
      if(wwp(ipt,5).lt.0.)dw=gs                                                 !/*hs4b       14*/
!                                                                               !/*hs         82*/
!        decide whether to force a collision.                                   !/*hs         83*/
      if(for(lfor+ipt,icl).eq.0)go to 80                                        !/*hs         84*/
      call forcol                                                               !/*hs         85*/
      if(kdb.ne.0)go to 390                                                     !/*hs         86*/
      if(nter.ne.0)go to 260                                                    !/*hs         87*/
      dw=huge                                                                   !/*hs4c        7*/
      go to 90                                                                  !/*hs         88*/
!                                                                               !/*hs         89*/
!        sample the distance to collision, pmf, normally.                       !/*hs         90*/
   80 pmf=-log(rang())*gs                                                       !/*hs         91*/
!                                                                               !/*hs         92*/
!        get the track length, d.                                               !/*hs4c        8*/
!        banked uncollided forced collision part comes back here.               !/*hs4c        9*/
!        1 mfp mesh-based weight window check survivors come back here.         !/*hs4c       10*/
   90 d=min(pmf,dls,dxl,dtc,deb,dw)                                             !/*hs4b       15*/
!                                                                               !/*hs4c2       1*/
!        adjust the weight for exponential transformation.                      !/*hs4c2       2*/
      if(qax(lqax+ipt,icl).eq.0.)go to 95                                       !/*hs4c2       3*/
      if ((qpl-ple)*d.lt.-30.) nter = loss_all_pls_exp_transform                !/*hsx        44*/
      if(nter.ne.0)go to 260                                                    !/*hs4c2       5*/
      t1=wgt                                                                    !/*hs4c2       6*/
      wgt=wgt*exp((qpl-ple)*d)                                                  !/*hs4c2       7*/
      if(d.eq.pmf)wgt=wgt*ple*gs                                                !/*hs4c2       8*/
      i=2                                                                       !/*hs4c2       9*/
      if(t1.gt.wgt)i=5                                                          !/*hs4c2      10*/
      if (t1.gt.wgt) then                                                       !/*hsx        45*/
         call lose_pax (loss_all_pls_exp_transform, ipt,                        !/*hsx        46*/
     &                  0, abs(t1-wgt), abs(t1-wgt)*erg)                        !/*hsx        47*/
      else                                                                      !/*hsx        48*/
         call gain_pax (igain_all_pls_exp_transform, ipt,                       !/*hsx        49*/
     &                  0, abs(t1-wgt), abs(t1-wgt)*erg)                        !/*hsx        50*/
      endif                                                                     !/*hsx        51*/
      call contrib_pwb (ipt,ipwb_variance_exp_transform,icl,-(t1-wgt))          !/*hsx        52*/
!                                                                               !/*hs4c2      14*/
!        generate weight windows and accumulate perturbations.                  !/*hs4c2      15*/
   95 if(iwwg.gt.0)call avrwwg(d)                                               !/*hs4c2      16*/
      if(npert.ne.0)call pertub(d)                                              !/*hs4c       12*/
!                                                                               !/*hs4c       13*/
!        track length estimate of keff and keff perturbations.                  !/*hs4c       14*/
      if(nsr.ne.71)go to 110                                                    !/*hs         95*/
      if(ipt.ne.1)go to 110                                                     !/*hs4b       16*/
      rlttc(3,1)=rlttc(3,1)+wgt*d/vel                                           !/*hs4b       17*/
      if(lfcl(llfc+icl).eq.0)go to 110                                          !/*hs4b       18*/
      fm=0.                                                                     !/*hs         97*/
      if(npert.ne.0)call pertc(1,0,0,0,0,zero)                                  !/*hs4c       15*/
      do 100 m=jmd(ljmd+mk),jmd(ljmd+mk+1)-1                                    !/*hs         98*/
      ix=lme(llme+1,m)                                                          !/*hs4c       16*/
      f=fme(lfme+m)*rtc(krtc+10,ix)*rtc(krtc+8,ix)                              !/*hs4c       17*/
      if(npert.ne.0)call pertc(2,mk,m,-6,0,f)                                   !/*hs4c       18*/
  100 fm=fm+f                                                                   !/*hs4c       19*/
      t=fm*d*wgt*rh                                                             !/*hs4c       20*/
      sumktc(3)=sumktc(3)+t                                                     !/*hs4c       21*/
      do 105 i=1,npert                                                          !/*hs4c       22*/
  105 sump(ksum+i)=sump(ksum+i)+t*ptb(kptb+4,i)                                 !/*hs4c       23*/
!                                                                               !/*hs4c       49*/
!        track length cell tallies.                                             !/*hs4c       50*/
  110 l=locct(llct+ipt,icl)                                                     !/*hs4c       51*/
      if(l.ne.0)call tally(                                                     !/*hsx        56*/
#ifdef MESHTAL                                                                  /*!hsx        57*/
     & zero,0,                                                                  !/*hsx        58*/
#endif                                                                          /*!hsx        59*/
     & l,d)                                                                     !/*hsx        60*/
      if(kdb.ne.0)go to 390                                                     !/*hs        104*/
      do 120 i=0,lev-1                                                          !/*hs        105*/
      l=locct(llct+ipt,int(udt(7,i)))                                           !/*hs        106*/
      if(l.ne.0)call tally(                                                     !/*hsx        61*/
#ifdef MESHTAL                                                                  /*!hsx        62*/
     & zero,0,                                                                  !/*hsx        63*/
#endif                                                                          /*!hsx        64*/
     & l,d)                                                                     !/*hsx        65*/
  120 if(kdb.ne.0)go to 390                                                     !/*hs4c       53*/
#ifdef MESHTAL                                                                  /*!hsx        66*/
      if (mtlflg.ne.0.or.medflg.ne.0) then                                      !/*hsx        67*/
         do 125 i=1,6                                                           !/*hsx        68*/
            if(lev.eq.0) then                                                   !/*hsx        69*/
               zd(i)=gpblcm(i)                                                  !/*hsx        70*/
            else                                                                !/*hsx        71*/
               zd(i)=udt(i,0)                                                   !/*hsx        72*/
            endif                                                               !/*hsx        73*/
  125       continue                                                            !/*hsx        74*/
         zd(7)=wgt                                                              !/*hsx        75*/
         zd(8)=erg                                                              !/*hsx        76*/
         if(mk.ne.0) then                                                       !/*hsx        77*/
            zd(9)=d*heatf(mk,0)*rho(lrho+icl)                                   !/*hsx        78*/
         else                                                                   !/*hsx        79*/
            zd(9)=zero                                                          !/*hsx        80*/
         endif                                                                  !/*hsx        81*/
         jz(1)=mk                                                               !/*hsx        82*/
         jz(2)=mspf                                                             !/*hsx        83*/
         jz(5)=4                                                                !/*hsx        84*/
         call gdtal(2,d,zd,jz)                                                  !/*hsx        85*/
      endif                                                                     !/*hsx        86*/
      if(ipt.eq.2.and.kpt(3).eq.0) then                                         !/*hsx        87*/
         fd=heatf(mk,0)*d*rho(lrho+icl)                                         !/*hsx        88*/
         l=locct(llct+1,icl)                                                    !/*hsx        89*/
         if(l.ne.0)call tally(fd,1,l,d)                                         !/*hsx        90*/
      endif                                                                     !/*hsx        91*/
#endif                                                                          /*!hsx        92*/
      jsu=jap                                                                   !/*hs        108*/
!                                                                               !/*hs        109*/
!        increment the summary accounts.                                        !/*hs        110*/
      dt=d/vel                                                                  !/*hs        111*/
      pac(kpac+ipt,5,icl)=pac(kpac+ipt,5,icl)+wgt*dt*erg                        !/*hs        112*/
      pac(kpac+ipt,6,icl)=pac(kpac+ipt,6,icl)+wgt*d*erg                         !/*hs        113*/
      pac(kpac+ipt,7,icl)=pac(kpac+ipt,7,icl)+d                                 !/*hs        114*/
      if(ple.ne.0.)pac(kpac+ipt,8,icl)=pac(kpac+ipt,8,icl)+wgt*d/ple            !/*hs        115*/
      pac(kpac+ipt,9,icl)=pac(kpac+ipt,9,icl)+wgt*dt                            !/*hs        116*/
      pac(kpac+ipt,10,icl)=pac(kpac+ipt,10,icl)+wgt*d                           !/*hs        117*/
!                                                                               !/*hs        118*/
!        update the particle to the surface, collision, or termination.         !/*hs        119*/
  130 continue                                                                  !/*hsx        93*/
      tme=tme+dt                                                                !/*hsx        94*/
      xxx=xxx+uuu*d                                                             !/*hs        122*/
      yyy=yyy+vvv*d                                                             !/*hs        123*/
      zzz=zzz+www*d                                                             !/*hs        124*/
      do 140 l=0,lev-1                                                          !/*hs        125*/
      udt(1,l)=udt(1,l)+udt(4,l)*d                                              !/*hs        126*/
      udt(2,l)=udt(2,l)+udt(5,l)*d                                              !/*hs        127*/
  140 udt(3,l)=udt(3,l)+udt(6,l)*d                                              !/*hs        128*/
!                                                                               !/*hs        129*/
!        special treatment for multigroup electrons.                            !/*hs        130*/
      if(stp.eq.0.)go to 150                                                    !/*hs        131*/
      t1=d*stp                                                                  !/*hs        132*/
      if(mcal.eq.2)t1=-t1                                                       !/*hs        133*/
      erg=erg-t1                                                                !/*hs        134*/
!        this section should only be visited for electrons masquerading         !/*hsx        95*/
!        as neutrons in a multigroup calculation.  Therefore ipt == 1.          !/*hsx        96*/
      call lose_pax (loss_neutron_downscattering, ipt, 0, zero, t1*wgt)         !/*hsx        97*/
      if(erg.le.elc(ipt).eqv.mcal.lt.2)go to 220                                !/*hs        136*/
      rm=xss(jxs(ljxs+1,mgegbt(1))+jgp-1+2*jgm(1))                              !/*hs        137*/
      vel=slite*sqrt(erg*(erg+2.*rm))/(erg+rm)                                  !/*hs        138*/
  150 eg0=erg                                                                   !/*hs        139*/
!                                                                               !/*hs        140*/
!        score flux in cell for multigroup weight-window generation.            !/*hs        141*/
      if(icw.ne.0)flx(kflx+icl+mxa*(jgp-1+mgm(ipt)))=                           !/*hs        142*/
     & flx(kflx+icl+mxa*(jgp-1+mgm(ipt)))+d*wgt                                 !/*hs        143*/
      if(d.eq.dtc)go to 230                                                     !/*hs        144*/
      if(d.eq.dxl)go to 250                                                     !/*hs        145*/
!                                                                               !/*hs        146*/
!        process dxtran particle as it leaves its sphere.                       !/*hs        147*/
      if(idx.eq.0)go to 170                                                     !/*hs4c2      17*/
      if((xxx-dxx(ipt,1,idx))**2+(yyy-dxx(ipt,2,idx))**2+                       !/*hs        149*/
     & (zzz-dxx(ipt,3,idx))**2.lt.dxx(ipt,5,idx))go to 170                      !/*hs4c2      18*/
      idx=0                                                                     !/*hs        151*/
      if(wwp(ipt,4).ne.0.)go to 170                                             !/*hs4c2      19*/
      if(wgt*gamef*fiml(1).gt.fiml9(1,1)*wcs2tc(ipt))go to 170                  !/*hsx        98*/
      t1=wcs1tc(ipt)*fiml9(1,1)/(gamef*fiml(1))                                 !/*hsx        99*/
      if(t1.eq.0.)go to 170                                                     !/*hs4c2      21*/
      if(wgt.lt.t1*rang())go to 240                                             !/*hs        156*/
!     QUESTION: Can t1 - wgt < 0 here?                                          !/*hsx       100*/
      call gain_pax (igain_all_pls_weight_cutoff, ipt,                          !/*hsx       101*/
     &               0, t1-wgt, (t1-wgt)*erg)                                   !/*hsx       102*/
      call contrib_pwb (ipt,ipwb_variance_weight_cutoff,icl,t1-wgt)             !/*hsx       103*/
      wgt=t1                                                                    !/*hs        160*/
!                                                                               !/*hs4b       23*/
!        only play the weight-window game and return.                           !/*hs4b       24*/
  170 if(d.ne.dw)go to 175                                                      !/*hs4b       25*/
      jsu=0                                                                     !/*hs4b       26*/
      call wtwndo(24)                                                           !/*hs4c       56*/
      if(nter.ne.0)go to 260                                                    !/*hs4b       28*/
      pmf=pmf-dw                                                                !/*hs4c       57*/
      dls=dls-dw                                                                !/*hs4c       58*/
      dxl=dxl-dw                                                                !/*hs4c       59*/
      dtc=dtc-dw                                                                !/*hs4c       60*/
      deb=deb-dw                                                                !/*hs4c       61*/
      go to 90                                                                  !/*hs4c       62*/
!                                                                               !/*hs4b       30*/
!        process the particle thru the cell boundary if no collision.           !/*hs4b       31*/
  175 if(d.ne.dls)go to 180                                                     !/*hs4b       32*/
      iexp=0                                                                    !/*hs4b       33*/
      call surfac                                                               !/*hs        175*/
      if(krflg.ne.0)call eventp(3)                                              !/*hs        176*/
      if(iptr.ne.0)call ptrak(3)                                                !/*hs4a        4*/
      if(kdb.ne.0)go to 390                                                     !/*hs        177*/
      if(nter.ne.0)go to 260                                                    !/*hs        178*/
      go to 50                                                                  !/*hs        179*/
!                                                                               !/*hs        180*/
!        calculate everything about the collision.                              !/*hs        181*/
  180 pac(kpac+ipt,3,icl)=pac(kpac+ipt,3,icl)+1.                                !/*hs        182*/
      pac(kpac+ipt,4,icl)=pac(kpac+ipt,4,icl)+wgt                               !/*hs        183*/
      if(nmaz.ne.0)call rslmaz(3,1)                                             !/*hs4b       34*/
      nch(ipt)=nch(ipt)+1                                                       !/*hs        184*/
      ncp=ncp+1                                                                 !/*hs        185*/
#ifdef MESHTAL                                                                  /*!hsx       104*/
      mspf=0                                                                    !/*hsx       105*/
#endif                                                                          /*!hsx       106*/
  190 if(ipt.eq.1)call colidn                                                   !/*hs        187*/
      if(ipt.eq.2)call colidp                                                   !/*hs        188*/
      if(kdb.ne.0)go to 390                                                     !/*hs        189*/
      if(nter.ne.0)go to 260                                                    !/*hs        190*/
!                                                                               !/*hs        191*/
!        tally detectors and create dxtran particles.                           !/*hs        192*/
      if(ndet(ipt).ne.0)call tallyd                                             !/*hs        193*/
      if(kdb.ne.0)go to 390                                                     !/*hs        194*/
      if(ndx(ipt).eq.0)go to 200                                                !/*hs        195*/
      if(ndx(ipt).gt.1.or.idx.eq.0)call dxtran                                  !/*hs        196*/
      if(kdb.ne.0)go to 390                                                     !/*hs        197*/
!                                                                               !/*hs        198*/
!        play the weight-window and energy-splitting games.                     !/*hs        199*/
  200 if(wwp(ipt,4).ne.0.)call wtwndo(4)                                        !/*hs4c       86*/
      if(nter.ne.0)go to 260                                                    !/*hs        201*/
      if(espl(ipt,1).ne.0.)call ergimp                                          !/*hs        202*/
      if(nter.ne.0)go to 260                                                    !/*hs        203*/
      go to 30                                                                  !/*hs        204*/
!                                                                               !/*hs        208*/
!******************  process terminated particles.  ********************        !/*hs        209*/
!                                                                      *        !/*hs        210*/
  220 nter = loss_all_pls_energy_cutoff                                         !/*hsx       111*/
#ifdef MESHTAL                                                                  /*!hsx       112*/
!      if(kpt(2).ne.0.and.jgp.lt.0)go to 260                                    !/*hsx       113*/
      if(ipt.lt.3)go to 260                                                     !/*hsx       114*/
      l=locct(llct+1,icl)                                                       !/*hsx       115*/
      if(l.ne.0)call tally(erg,1,l,d)                                           !/*hsx       116*/
      l=locct(llct+ipt,icl)                                                     !/*hsx       117*/
      if(l.ne.0)call tally(erg,2,l,d)                                           !/*hsx       118*/
      if(kdb.ne.0)return                                                        !/*hsx       119*/
      do 222 i=0,lev-1                                                          !/*hsx       120*/
      l=locct(llct+1,int(udt(7,i)))                                             !/*hsx       121*/
      if(l.ne.0)call tally(erg,1,l,d)                                           !/*hsx       122*/
      l=locct(llct+ipt,int(udt(7,i)))                                           !/*hsx       123*/
      if(l.ne.0)call tally(erg,2,l,d)                                           !/*hsx       124*/
  222 continue                                                                  !/*hsx       125*/
      if(mtlflg.ne.0.or.medflg.ne.0) then                                       !/*hsx       126*/
         do 225 i=1,6                                                           !/*hsx       127*/
         if(lev.eq.0) then                                                      !/*hsx       128*/
            zd(i)=gpblcm(i)                                                     !/*hsx       129*/
         else                                                                   !/*hsx       130*/
            zd(i)=udt(i,0)                                                      !/*hsx       131*/
         endif                                                                  !/*hsx       132*/
  225    continue                                                               !/*hsx       133*/
         zd(7)=wgt                                                              !/*hsx       134*/
         zd(8)=erg                                                              !/*hsx       135*/
         zd(9)=erg                                                              !/*hsx       136*/
         jz(1)=mkc                                                              !/*hsx       137*/
         jz(2)=mspf                                                             !/*hsx       138*/
         jz(5)=2                                                                !/*hsx       139*/
         call gdtal(8,zero,zd,jz)                                               !/*hsx       140*/
      endif                                                                     !/*hsx       141*/
#endif                                                                          /*!hsx       142*/
      go to 260                                                                 !/*hs        212*/
  230 nter = loss_all_pls_time_cutoff                                           !/*hsx       143*/
      go to 260                                                                 !/*hs        214*/
  240 nter = loss_all_pls_weight_cutoff                                         !/*hsx       144*/
      go to 260                                                                 !/*hs        216*/
  250 nter = loss_all_pls_dxtran                                                !/*hsx       145*/
!                                                                               !/*hs        218*/
!        increment particle statistics for termination type nter.               !/*hs        219*/
  260 if(krflg.ne.0)call eventp(5)                                              !/*hs        220*/
      if(iptr.ne.0)call ptrak(5)                                                !/*hs4a        5*/
      if (ipt.le.3) then                                                        !/*hsx       146*/
         j=jrwb(nter,ipt)                                                       !/*hsx       147*/
      else if (charge(ipt).eq.zero) then                                        !/*hsx       148*/
         j=jrwb(nter,id_category_gen_neu)                                       !/*hsx       149*/
      else                                                                      !/*hsx       150*/
         j=jrwb(nter,id_category_gen_chg)                                       !/*hsx       151*/
      endif                                                                     !/*hsx       152*/
      if(nter.eq.loss_all_pls_escape)tmavtc(ipt,1)=tmavtc(ipt,1)+tme*wgt        !/*hsx       153*/
      if (ipt.eq.1 .and. nter.eq.loss_neutron_capture .or.                      !/*hsx       154*/
     & ipt.eq.2 .and. nter.eq.loss_photon_capture  .or.                         !/*hsx       155*/
     & ipt.eq.2 .and. nter.eq.loss_photon_photonuclear)                         !/*hsx       156*/
     & tmavtc(ipt,2)=tmavtc(ipt,2)+tme*wgt                                      !/*hsx       157*/
      tmavtc(ipt,3)=tmavtc(ipt,3)+tme*wgt                                       !/*hsx       158*/
      call lose_pax (nter, ipt, 1, wgt, wgt*erg)                                !/*hsx       159*/
      if(j.ne.0)call contrib_pwb (ipt,j,icl,-wgt)                               !/*hsx       160*/
      if(nsr.ne.71)go to 270                                                    !/*hs        230*/
      if(ipt.ne.1)go to 270                                                     !/*hs        231*/
      if(nter.ne.0 .and. nter.ne.loss_all_pls_escape)go to 270                  !/*hsx       161*/
      rlttc(1,1)=rlttc(1,1)+wgt*tme                                             !/*hs4b       39*/
      rlttc(2,1)=rlttc(2,1)+wgt*tme                                             !/*hs4b       40*/
      rlttc(1,2)=rlttc(1,2)+wgt                                                 !/*hs4b       41*/
      rlttc(2,2)=rlttc(2,2)+wgt                                                 !/*hs4b       42*/
  270 nter=0                                                                    !/*hs        235*/
!                                                                               !/*hs        236*/
!        get the next particle from the bank, if there are any.                 !/*hs        237*/
      if(nbnk.eq.0)go to 350                                                    !/*hs        238*/
      do 280 i=1,mxa                                                            !/*hs        239*/
  280 ipac2(kpc2+i)=0                                                           !/*hs        240*/
      call bankit(100)                                                          !/*hs4a       10*/
      if(iwwg.gt.0)call avrnrm(1)                                               !/*hs4c       88*/
      if(jsu.lt.0)go to 340                                                     !/*hs        242*/
#ifdef MESHTAL                                                                  /*!hsx       162*/
!     QUESTION:  Does the second condition (with the line jz(2)=0)              !/*hsx       163*/
!                depend on other unresolved questions about ncp?                !/*hsx       164*/
      if (mspflg.ne.0.and.mspf.ne.0) then                                       !/*hsxk       11*/
         do 285 i=1,6                                                           !/*hsx       166*/
            if(lev.eq.0) then                                                   !/*hsx       167*/
               zd(i)=gpblcm(i)                                                  !/*hsx       168*/
            else                                                                !/*hsx       169*/
               zd(i)=udt(i,0)                                                   !/*hsx       170*/
            endif                                                               !/*hsx       171*/
  285       continue                                                            !/*hsx       172*/
         zd(7)=wgt                                                              !/*hsx       173*/
         zd(8)=erg                                                              !/*hsx       174*/
         zd(9)=0.0                                                              !/*hsx       175*/
         jz(1)=mat(lmat+icl)                                                    !/*hsx       176*/
         jz(2)=mspf                                                             !/*hsx       177*/
         d=1.0                                                                  !/*hsx       178*/
         call gdtal(1,d,zd,jz)                                                  !/*hsx       179*/
      endif                                                                     !/*hsx       180*/
#endif                                                                          /*!hsx       181*/
      if(kfl.eq.0)go to 320                                                     !/*hs        243*/
      if(kfl.eq.1)go to 300                                                     !/*hs        244*/
      do 290 i=1,mxa                                                            !/*hs        245*/
  290 if(ifl(kifl+i).ge.node)ifl(kifl+i)=0                                      !/*hs        246*/
      if(kfl.eq.2)go to 320                                                     !/*hs        247*/
  300 do 310 j=1,mxj                                                            !/*hs        248*/
  310 if(jfl(kjfl+j).ge.node)jfl(kjfl+j)=0                                      !/*hs        249*/
  320 if(krflg.ne.0)call eventp(2)                                              !/*hs        250*/
      if(iptr.ne.0)call ptrak(2)                                                !/*hs4a       11*/
      pac(kpac+ipt,2,icl)=pac(kpac+ipt,2,icl)+1.                                !/*hs        251*/
      ipac2(kpc2+icl)=1                                                         !/*hs        252*/
      if(npa.ge.0)go to 30                                                      !/*hs        253*/
!                                                                               !/*hs        254*/
!        short loop if particle is uncollided part of forced collision.         !/*hs        255*/
      levp=jsu                                                                  !/*hs4c       89*/
      jap=npa+1000000                                                           !/*hs4c       90*/
      rh=rho(lrho+icl)                                                          !/*hs4c       91*/
      mk=mat(lmat+icl)                                                          !/*hs4c       92*/
      totm=0.                                                                   !/*hs4c       93*/
      deb=huge                                                                  !/*hs4c       94*/
      if(ipt.eq.1.and.mk.ne.0)call acetot(0,mk)                                 !/*hs4c       95*/
      if(ipt.eq.2.and.mk.ne.0)call photot(mk)                                   !/*hs4c       96*/
      pmf=huge                                                                  !/*hs4c       97*/
      dw=huge                                                                   !/*hs4c       98*/
      qpl=totm*rh                                                               !/*hs4c       99*/
      ple=qpl                                                                   !/*hs4c      101*/
      go to 90                                                                  !/*hs4c      102*/
!                                                                               !/*hs        261*/
!        process banked surface-source particle.                                !/*hs        262*/
  340 continue                                                                  !/*hsx       184*/
#ifdef MESHTAL                                                                  /*!hsx       185*/
      if (mspflg.ne.0) then                                                     !/*hsx       186*/
         do 345 i=1,6                                                           !/*hsx       187*/
            if(lev.eq.0) then                                                   !/*hsx       188*/
               zd(i)=gpblcm(i)                                                  !/*hsx       189*/
            else                                                                !/*hsx       190*/
               zd(i)=udt(i,0)                                                   !/*hsx       191*/
            endif                                                               !/*hsx       192*/
  345       continue                                                            !/*hsx       193*/
         zd(7)=wgt                                                              !/*hsx       194*/
         zd(8)=erg                                                              !/*hsx       195*/
         zd(9)=0.0                                                              !/*hsx       196*/
         jz(1)=mat(lmat+icl)                                                    !/*hsx       197*/
         jz(2)=mspf                                                             !/*hsx       198*/
         d=1.0                                                                  !/*hsx       199*/
         call gdtal(1,d,zd,jz)                                                  !/*hsx       200*/
      endif                                                                     !/*hsx       201*/
#endif                                                                          /*!hsx       202*/
      call ypbssp                                                               !/*hsx       203*/
      if(kdb.eq.0)go to 30                                                      !/*cor4-1    110*/
      go to 390                                                                 !/*cor4-1    111*/
!                                                                               !/*hs        265*/
!        the history is complete.                                               !/*hs        266*/
!        add the tally data of this history to the total tally data.            !/*hs        267*/
  350 do 360 i=1,nipt                                                           !/*hsx       204*/
  360 colltc(i)=colltc(i)+nch(i)                                                !/*hs4a       16*/
      if(kpt(1).eq.0)go to 370                                                  !/*hs        270*/
      smultc(2)=smultc(2)+smultc(1)                                             !/*hs        271*/
      smultc(3)=smultc(3)+smultc(1)**2                                          !/*hs        272*/
      smultc(5)=smultc(5)+smultc(4)                                             !/*hs4c2      29*/
      smultc(6)=smultc(6)+smultc(4)*smultc(1)                                   !/*hs4c2      30*/
      smultc(7)=smultc(7)+smultc(4)**2                                          !/*hs4c2      31*/
  370 if(kf8.ne.0)call finpht                                                   !/*hs        273*/
      if(iptr.ne.0)call ptrak(7)                                                !/*hs4a       17*/
      if(jtls.gt.0)call talshf                                                  !/*hs        274*/
#ifdef MESHTAL                                                                  /*!hsx       205*/
      if(nmesh.gt.0)call gdtal(3,zero,zd,jz)                                    !/*hsx       206*/
#endif                                                                          /*!hsx       207*/
      if(rnrtc-rnrtc0.ge.nstrid)nrnhtc(1)=nrnhtc(1)+1                           !/*hs        275*/
      if(rnrtc-rnrtc0.le.nrnhtc(2))return                                       !/*hs        276*/
      nrnhtc(2)=rnrtc-rnrtc0                                                    !/*hs        277*/
      nrnhtc(3)=npstc                                                           !/*hs        278*/
      return                                                                    !/*hs        279*/
!                                                                               !/*hs        280*/
!********************  process troubled particle.  *********************        !/*hs        281*/
!                                                                      *        !/*hs        282*/
  380 if(for(lfor+ipt,icl).ne.0)go to 240                                       !/*hsx       208*/
      call expirx(1,'hstory',                                                   !/*hs4c      104*/
     & 'the weight of the current particle is zero or less.')                   !/*hs        284*/
  390 if(kdb.lt.0)return                                                        !/*hs        285*/
      do 400 i=1,mxf                                                            !/*hs        286*/
  400 tal(ktal+i)=0.                                                            !/*hs        287*/
!                                                                               !/*hs        288*/
!        rerun history with full surface sense check and event printing.        !/*hs        289*/
      if(ntasks.gt.1.or.krflg.eq.2.or.nerr.ge.lost(2))go to 410                 !/*hs        290*/
      call backup                                                               !/*hs        291*/
      go to 20                                                                  !/*hs        292*/
!                                                                               !/*hs        293*/
!         print lost particle debug information and start a new history.        !/*hs        294*/
  410 if(kdb.ge.11)go to 420                                                    !/*hs        295*/
      call prlost                                                               !/*hs        296*/
      return                                                                    !/*hs        297*/
!                                                                               !/*hs        298*/
!        terminate long history and prepare continue-run rerun.                 !/*hs        299*/
  420 write(he,'(i4)')2*int(dbcn(10))                                           !/*hs        300*/
      call expirx(1,'hstory','the next history takes more than'                 !/*hs        301*/
     & //he//' seconds.')                                                       !/*hs        302*/
      return                                                                    !/*hs        303*/
      end                                                                       !/*hs        304*/
