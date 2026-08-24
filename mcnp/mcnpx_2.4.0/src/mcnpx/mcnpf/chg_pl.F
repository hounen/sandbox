!#ident chg_pl.F from deck xcp                                                  !/*xcp         1*/
      subroutine chg_pl                                                         !/*xcp         2*/
!        run one charged particle track.                                        !/*xcp         3*/
      use global_data                                                           !/*xcp         4*/
!                                                                               !/*xcp         5*/
      implicit real(kindr) (a-h,o-z)                                            !/*xcp         6*/
      implicit integer(kindi) (i-n)                                             !/*xcp         7*/
!                                                                               !/*xcp         8*/
#include <globals/interact_outcomes.h>                                          /*!xcp         9*/
#ifdef MESHTAL                                                                  /*!xcp        10*/
      dimension zd(10),jz(5)                                                    !/*xcp        11*/
#endif                                                                          /*!xcp        12*/
      external rang                                                             !/*xcp        13*/
!                                                                               !/*xcp        14*/
      if (ntasks.gt.1) then                                                     !/*xcpk        1*/
         call expirx(1,'chg_pl',                                                !/*xcpk        2*/
     &   'threading not available above tabular region.')                       !/*xcpk        3*/
         return                                                                 !/*xcpk        4*/
      endif                                                                     !/*xcpk        5*/
      lp=ltran(ipt,jan)                                                         !/*xcp        15*/
      if (lp.eq.0) then                                                         !/*xcp        16*/
         call expirx(1,'chg_pl','illegal particle type.')                       !/*xcp        17*/
         return                                                                 !/*xcp        18*/
      endif                                                                     !/*xcp        19*/
!                                                                               !/*xcp        20*/
      if (erg.le.elc(ipt)) then                                                 !/*xcp        21*/
         mkc = mat(lmat+icl)                                                    !/*xcpa        1*/
!        NBETA == 3 source or bank particle below cutoff.                       !/*xcpa        2*/
         nb = 3                                                                 !/*xcpa        3*/
         d = zero                                                               !/*xcpa        4*/
         go to 500                                                              !/*xcp        24*/
      endif                                                                     !/*xcp        25*/
!                                                                               !/*xcp        26*/
!        find the energy group.                                                 !/*xcp        27*/
      ngp=1                                                                     !/*xcp        28*/
      ib=nee(ipt)                                                               !/*xcp        29*/
   10 if(ib-ngp.eq.1)go to 30                                                   !/*xcp        30*/
      ih=(ngp+ib)/2                                                             !/*xcp        31*/
      if(erg.ge.eee(leee+ipt,ih))go to 20                                       !/*xcp        32*/
      ngp=ih                                                                    !/*xcp        33*/
      go to 10                                                                  !/*xcp        34*/
   20 ib=ih                                                                     !/*xcp        35*/
      go to 10                                                                  !/*xcp        36*/
!                                                                               !/*xcp        37*/
!        begin the track, or enter a new cell.                                  !/*xcp        38*/
!        set up the material index and preset number of substeps.               !/*xcp        39*/
   30 continue                                                                  !/*xcp        40*/
! ... BEGIN PROTON TABLES                                                       !/*xcp        41*/
      if (ipt.eq.9.and.jan.eq.1.and.erg.le.cut_h) then                          !/*xcp        42*/
         call low_prot                                                          !/*xcp        43*/
         return                                                                 !/*xcp        44*/
      endif                                                                     !/*xcp        45*/
! ... END PROTON TABLES                                                         !/*xcp        46*/
      rr=0.                                                                     !/*xcp        47*/
      mkc=mat(lmat+icl)                                                         !/*xcp        48*/
      nm=1                                                                      !/*xcp        49*/
!        MIPT note:  nsb() can be generalized.                                  !/*xcp        50*/
      if(mkc.ne.0.and.lcaopt(8).ne.-1)nm=nsb(lnsb+mkc)                          !/*xcp        51*/
!                                                                               !/*xcp        52*/
!        begin an energy step (a set of substeps).                              !/*xcp        53*/
   40 continue                                                                  !/*xcp        54*/
! ... BEGIN PROTON TABLES                                                       !/*xcp        55*/
      if (ipt.eq.9.and.jan.eq.1.and.erg.le.cut_h) then                          !/*xcp        56*/
         call low_prot                                                          !/*xcp        57*/
         return                                                                 !/*xcp        58*/
      endif                                                                     !/*xcp        59*/
! ... END PROTON TABLES                                                         !/*xcp        60*/
      pmf=huge                                                                  !/*xcpa        5*/
      if (mkc.eq.0) go to 50                                                    !/*xcpa        6*/
      if (lcaopt(8).eq.-1) go to 50                                             !/*xcpa        7*/
      pmf=drs(ldrs+ipt,ngp+nee_max*(mkc-1))/rho(lrho+icl)                       !/*xcpa        8*/
      qs=qav(lqav+ipt,ngp+nee_max*(mkc-1))*rho(lrho+icl)                        !/*xcpa        9*/
!                                                                               !/*xcpa       10*/
!        if istrg(ipt) < 0 use MCNPX_2.2.4 straggling model.                    !/*xcpa       11*/
      if (istrg(ipt).ge.0) go to 50                                             !/*xcpa       12*/
      q1=qs                                                                     !/*xcpa       13*/
      ch=charge(ipt)                                                            !/*xcpa       14*/
      if(dbcn(28).gt.0.)ch=one                                                  !/*xcpa       15*/
      p=pmf*nsb(lnsb+mkc)                                                       !/*xcpa       16*/
      iz=0                                                                      !/*xcpa       17*/
   45 call glando(rang,p,ch,avz(lavz+mkc),                                      !/*xcpa       18*/
     & ava(lava+mkc),den(lden+icl),erg,gpt(ipt),de,ii)                          !/*xcpa       19*/
      qs=q1+de/p                                                                !/*xcpa       20*/
      if(qs.gt.0.) go to 50                                                     !/*xcpa       21*/
      iz=iz+1                                                                   !/*xcpa       22*/
      if(iz.le.1000) go to 45                                                   !/*xcpa       23*/
      call expirx(1,'chg_pl','more than 1000 resamplings of glando.')           !/*xcpa       24*/
!                                                                               !/*xcp        94*/
!        substep loop.                                                          !/*xcp        95*/
   50 do 150 ns=nm,1,-1                                                         !/*xcpa       25*/
! ... BEGIN PROTON TABLES                                                       !/*xcp        97*/
      if (ipt.eq.9.and.jan.eq.1.and.erg.le.cut_h) then                          !/*xcp        98*/
         call low_prot                                                          !/*xcp        99*/
         return                                                                 !/*xcp       100*/
      endif                                                                     !/*xcp       101*/
! ... END PROTON TABLES                                                         !/*xcp       102*/
      if (wgt.le.0.) then                                                       !/*xcp       103*/
         call expirx(1,'chg_pl',                                                !/*xcp       104*/
     &  'the weight of the current particle is zero or less.')                  !/*xcp       105*/
         return                                                                 !/*xcp       106*/
      endif                                                                     !/*xcp       107*/
      nch(ipt)=nch(ipt)+1                                                       !/*xcp       108*/
      ncp=ncp+1                                                                 !/*xcp       109*/
      n1=ngp                                                                    !/*xcp       110*/
#ifdef MESHTAL                                                                  /*!xcp       111*/
      fd=0.0                                                                    !/*xcp       112*/
      mspf=0                                                                    !/*xcp       113*/
#endif                                                                          /*!xcp       114*/
!                                                                               !/*xcp       115*/
!        calculate distances to time and energy cutoff.                         !/*xcp       116*/
      dtc=vel*(tco(ipt)-tme)                                                    !/*xcp       117*/
      dc=huge                                                                   !/*xcp       118*/
      if(mkc.ne.0.and.lcaopt(8).ne.-1)dc=(erg-elc(ipt))/qs                      !/*xcp       119*/
!                                                                               !/*xcp       120*/
!        calculate the distance to interaction.                                 !/*xcp       121*/
      di=huge                                                                   !/*xcp       122*/
      if (ipt.eq.9.and.jan.eq.1) then                                           !/*xcp       123*/
         call ace_tot(erg,lp,mkc,nel(lnel+1),mel(lmel+1),                       !/*xcp       124*/
     &    aa9(laa9+1),zz9(lzz9+1),fmf(lfmf+1),                                  !/*xcp       125*/
     &    sigg(lsgg+1,1),hsigg(lhsg+1,1),xsiso(lxso+1,1),                       !/*xcp       126*/
     &    esxmp(lesp+1),esxln(lexn+1,1),rho(lrho+icl),di)                       !/*xcp       127*/
      else                                                                      !/*xcp       128*/
         call get_tot(erg,lp,mkc,nel(lnel+1),mel(lmel+1),                       !/*xcp       129*/
#ifdef CEM                                                                      /*!xcp       130*/
     &    ipnf(lipnf+1),                                                        !/*xcp       131*/
#endif                                                                          /*!xcp       132*/
     &    aa9(laa9+1),zz9(lzz9+1),fmf(lfmf+1),                                  !/*xcp       133*/
     &    sigg(lsgg+1,1),hsigg(lhsg+1,1),xsiso(lxso+1,1),                       !/*xcp       134*/
#ifdef CEM                                                                      /*!xcp       135*/
     &    epnmin(lepnm+1),                                                      !/*xcp       136*/
#endif                                                                          /*!xcp       137*/
     &    rho(lrho+icl),di)                                                     !/*xcp       138*/
      endif                                                                     !/*xcp       139*/
      if (lcaopt(8).eq.-1) then                                                 !/*xcp       140*/
         d=zero                                                                 !/*xcp       141*/
         di=zero                                                                !/*xcp       142*/
         go to 101                                                              !/*xcp       143*/
      endif                                                                     !/*xcp       144*/
!                                                                               !/*xcp       145*/
!        calculate the distance to the cell boundary, dls.                      !/*xcp       146*/
      d1=min(pmf,dtc,di,dc)                                                     !/*xcp       147*/
      if(mbd(lmbd+icl).ne.0.or.jsu.ne.0)go to 60                                !/*xcp       148*/
      dls=huge                                                                  !/*xcp       149*/
      rr=rr-d1                                                                  !/*xcp       150*/
      if(rr.gt.0.)go to 70                                                      !/*xcp       151*/
      rr=dbmin()-d1                                                             !/*xcp       152*/
      if(rr.gt.0.)go to 70                                                      !/*xcp       153*/
   60 if(lca(llca+icl).lt.0)call chkcel(icl,3,j)                                !/*xcp       154*/
      call track(icl)                                                           !/*xcp       155*/
      if(kdb.ne.0)return                                                        !/*xcp       156*/
      jsu=jap                                                                   !/*xcp       157*/
   70 continue                                                                  !/*xcp       158*/
!                                                                               !/*xcp       159*/
!        tally the track length in the cell.                                    !/*xcp       160*/
      d=min(dls,d1)                                                             !/*xcp       161*/
! DEBUG BEGIN                                                                   !/*xcp       162*/
      if (d.le.0.) then                                                         !/*xcp       163*/
         write(iuo,1)nps,ipt,jan,lp,ngp,pmf,dtc,di,dc,dls,erg                   !/*xcp       164*/
    1    format(                                                                !/*xcp       165*/
     &  ' CHG_PL DEBUG: nps,ipt,jan,lp,ngp,pmf,dtc,di,dc,dls,erg'/              !/*xcp       166*/
     &    5i9,6e14.5)                                                           !/*xcp       167*/
         stop                                                                   !/*xcp       168*/
      endif                                                                     !/*xcp       169*/
! DEBUG END                                                                     !/*xcp       170*/
      l=locct(llct+ipt,icl)                                                     !/*xcp       171*/
      if(l.ne.0)call tally(                                                     !/*xcp       172*/
#ifdef MESHTAL                                                                  /*!xcp       173*/
     & zero,0,                                                                  !/*xcp       174*/
#endif                                                                          /*!xcp       175*/
     & l,d)                                                                     !/*xcp       176*/
      if(kdb.ne.0)return                                                        !/*xcp       177*/
      do 72 i=0,lev-1                                                           !/*xcpa       26*/
         l=locct(llct+ipt,int(udt(7,i)))                                        !/*xcp       179*/
         if(l.ne.0)call tally(                                                  !/*xcp       180*/
#ifdef MESHTAL                                                                  /*!xcp       181*/
     &    zero,0,                                                               !/*xcp       182*/
#endif                                                                          /*!xcp       183*/
     &    l,d)                                                                  !/*xcp       184*/
   72 continue                                                                  !/*xcpa       27*/
!                                                                               !/*xcp       186*/
!        increment the summary accounts.                                        !/*xcp       187*/
      dt=d/vel                                                                  !/*xcp       188*/
      pac(kpac+ipt,5,icl)=pac(kpac+ipt,5,icl)+wgt*dt*erg                        !/*xcp       189*/
      pac(kpac+ipt,6,icl)=pac(kpac+ipt,6,icl)+wgt*d*erg                         !/*xcp       190*/
      pac(kpac+ipt,7,icl)=pac(kpac+ipt,7,icl)+d                                 !/*xcp       191*/
      if(pmf.ne.huge)pac(kpac+ipt,8,icl)=pac(kpac+ipt,8,icl)+wgt*d*pmf          !/*xcp       192*/
      pac(kpac+ipt,9,icl)=pac(kpac+ipt,9,icl)+wgt*dt                            !/*xcp       193*/
      pac(kpac+ipt,10,icl)=pac(kpac+ipt,10,icl)+wgt*d                           !/*xcp       194*/
#ifdef MESHTAL                                                                  /*!xcp       195*/
      if (mkc.eq.0) then                                                        !/*xcp       196*/
         if (mtlflg.ne.0) then                                                  !/*xcp       197*/
            do 74 i=1,6                                                         !/*xcp       198*/
               if (lev.eq.0) then                                               !/*xcp       199*/
                  zd(i)=gpblcm(i)                                               !/*xcp       200*/
               else                                                             !/*xcp       201*/
                  zd(i)=udt(i,0)                                                !/*xcp       202*/
               endif                                                            !/*xcp       203*/
   74       continue                                                            !/*xcp       204*/
            zd(7)=wgt                                                           !/*xcp       205*/
            zd(8)=erg                                                           !/*xcp       206*/
            zd(9)=zero                                                          !/*xcp       207*/
            jz(1)=0                                                             !/*xcp       208*/
            jz(2)=mspf                                                          !/*xcp       209*/
            jz(5)=0                                                             !/*xcp       210*/
            call gdtal(2,d,zd,jz)                                               !/*xcp       211*/
         endif                                                                  !/*xcp       212*/
      endif                                                                     !/*xcp       213*/
#endif                                                                          /*!xcp       214*/
!                                                                               !/*xcp       215*/
!        scatter the particle.                                                  !/*xcp       216*/
      eg0=erg                                                                   !/*xcp       217*/
      uold(1)=uuu                                                               !/*xcp       218*/
      uold(2)=vvv                                                               !/*xcp       219*/
      uold(3)=www                                                               !/*xcp       220*/
      if (mkc.ne.0) then                                                        !/*xcp       221*/
         call mscat3(lp,d,arg(larg+mkc),erg,th)                                 !/*xcp       222*/
         call rotas(cos(th),uold,uuu,lev,irt)                                   !/*xcp       223*/
!                                                                               !/*xcpa       28*/
!        Calculate energy loss to ionization along the track, ei.               !/*xcpa       29*/
         ei=d*qs                                                                !/*xcpa       30*/
         if (istrg(ipt).eq.0) then                                              !/*xcpa       31*/
!                                                                               !/*xcpa       32*/
!           First get better estimate of CSDA over multiple groups.             !/*xcpa       33*/
            e = get_csda_energy (eg0, d)                                        !/*xcpa       34*/
!                                                                               !/*xcpa       35*/
!           Now get straggling around CSDA value.                               !/*xcpa       36*/
            iz = 0                                                              !/*xcpk        6*/
   75       call glando(rang,d,charge(ipt),avz(lavz+mkc),                       !/*xcpa       38*/
     &              ava(lava+mkc),den(lden+icl),eg0,gpt(ipt),de,ii)             !/*xcpa       39*/
!                                                                               !/*xcpa       40*/
            ei = eg0 - e + de                                                   !/*xcpa       41*/
            if (ei.le.zero) then                                                !/*xcpa       42*/
               iz = iz + 1                                                      !/*xcpk        7*/
               if(iz.gt.1000)call expirx(1,'chg_pl',                            !/*xcpa       44*/
     &               'more than 1000 resamplings of glando.')                   !/*xcpa       45*/
               go to 75                                                         !/*xcpa       46*/
            endif                                                               !/*xcpa       47*/
         endif                                                                  !/*xcpa       48*/
!                                                                               !/*xcpa       49*/
#ifdef MESHTAL                                                                  /*!xcpa       50*/
!                                                                               !/*xcpa       51*/
!        Tally ionization energy deposition to mesh and type 6 tallies.         !/*xcpa       52*/
!                                                                               !/*xcp       225*/
!        Tally energy deposition due to ionization.                             !/*xcp       226*/
      fd=ei                                                                     !/*xcpa       53*/
         erg=eg0                                                                !/*xcp       228*/
         l=locct(llct+1,icl)                                                    !/*xcp       229*/
         if(l.ne.0)call tally(fd,1,l,d)                                         !/*xcp       230*/
         l=locct(llct+ipt,icl)                                                  !/*xcp       231*/
         if(l.ne.0)call tally(fd,2,l,d)                                         !/*xcp       232*/
         if(kdb.ne.0)return                                                     !/*xcp       233*/
         do 76 i=0,lev-1                                                        !/*xcp       234*/
            l=locct(llct+1,int(udt(7,i)))                                       !/*xcp       235*/
            if(l.ne.0)call tally(fd,1,l,d)                                      !/*xcp       236*/
            l=locct(llct+ipt,int(udt(7,i)))                                     !/*xcp       237*/
            if(l.ne.0)call tally(fd,2,l,d)                                      !/*xcp       238*/
   76    continue                                                               !/*xcp       239*/
         if (mtlflg.ne.0.or.medflg.ne.0.and.fd.ne.0.) then                      !/*xcp       240*/
            do 78 i=1,6                                                         !/*xcp       241*/
               if (lev.eq.0) then                                               !/*xcp       242*/
                  zd(i)=gpblcm(i)                                               !/*xcp       243*/
               else                                                             !/*xcp       244*/
                  zd(i)=udt(i,0)                                                !/*xcp       245*/
               endif                                                            !/*xcp       246*/
   78       continue                                                            !/*xcp       247*/
            zd(7)=wgt                                                           !/*xcp       248*/
            zd(8)=erg                                                           !/*xcp       249*/
            zd(9)=fd                                                            !/*xcp       250*/
            jz(1)=mkc                                                           !/*xcp       251*/
            jz(2)=mspf                                                          !/*xcp       252*/
            jz(5)=2                                                             !/*xcp       253*/
            call gdtal(2,d,zd,jz)                                               !/*xcp       254*/
         endif                                                                  !/*xcp       255*/
#endif                                                                          /*!xcp       256*/
!                                                                               !/*xcp       257*/
!           lose energy to ionization along the track.                          !/*xcp       258*/
         erg=eg0-ei                                                             !/*xcpa       54*/
!                                                                               !/*xcpa       55*/
         if (erg.le.elc(ipt)) then                                              !/*xcpa       56*/
            call uplpos(xxx,uold,lev,d,vel,1)                                   !/*xcpa       57*/
            erg=elc(ipt)                                                        !/*xcpa       58*/
            call lose_pax (loss_gen_chg_multiple_scatter, ipt,                  !/*xcpa       59*/
     &                     0, zero, wgt*(eg0-erg))                              !/*xcpa       60*/
            jsu=0                                                               !/*xcpa       61*/
            nb=2                                                                !/*xcpa       62*/
            go to 500                                                           !/*xcpa       63*/
         else                                                                   !/*xcpa       64*/
            call lose_pax (loss_gen_chg_multiple_scatter, ipt,                  !/*xcpa       65*/
     &                     0, zero, wgt*ei)                                     !/*xcpa       66*/
         endif                                                                  !/*xcpa       67*/
!                                                                               !/*xcp       270*/
         if (erg.lt.eee(leee+ipt,ngp+1)) then                                   !/*xcp       271*/
  100       ngp=ngp+1                                                           !/*xcp       272*/
            if(erg.lt.eee(leee+ipt,ngp+1))go to 100                             !/*xcp       273*/
         endif                                                                  !/*xcp       274*/
      endif                                                                     !/*xcp       275*/
!                                                                               !/*xcp       276*/
!        move particle to surface, substep, interaction, or termination.        !/*xcp       277*/
!        produce secondary particles.                                           !/*xcp       278*/
!                                                                               !/*xcp       279*/
      call uplpos(xxx,uold,lev,d,vel,1)                                         !/*xcp       280*/
  101 continue                                                                  !/*xcp       281*/
      if (d.eq.di) then                                                         !/*xcp       282*/
         ec=erg                                                                 !/*xcp       283*/
         wt=wgt                                                                 !/*xcp       284*/
         jsu=0                                                                  !/*xcp       285*/
!        Convert from shakes to nsec.                                           !/*xcp       286*/
         tc=ten*tme                                                             !/*xcp       287*/
         call interact(1,lp,erg,elc(ipt),icl,mkc,0,tc,uuu,vvv,www,wgt,          !/*xcp       288*/
     &    xxx,yyy,zzz,nel(lnel+1),mel(lmel+1),aa9(laa9+1),zz9(lzz9+1),          !/*xcp       289*/
     &    iazdex(liax+1),fmf(lfmf+1),rho(lrho+1),pcap(lpcp+1),                  !/*xcp       290*/
     &    hsigg(lhsg+1,1),xsiso(lxso+1,1),esxlp(lexp+1,1),                      !/*xcp       291*/
#ifdef HISTP                                                                    /*!xcp       292*/
     &    npstc,                                                                !/*xcp       293*/
#endif                                                                          /*!xcp       294*/
#ifdef MESHTAL                                                                  /*!xcp       295*/
     &    eg_rc,eg_nt,                                                          !/*xcp       296*/
#endif                                                                          /*!xcp       297*/
     &    nr)                                                                   !/*xcp       298*/
#ifdef MESHTAL                                                                  /*!xcp       299*/
         ej=erg                                                                 !/*xcp       300*/
         wj=wgt                                                                 !/*xcp       301*/
         erg=ec                                                                 !/*xcp       302*/
         wgt=wt                                                                 !/*xcp       303*/
         fd=eg_rc+eg_nt                                                         !/*xcp       304*/
         l=locct(llct+1,icl)                                                    !/*xcp       305*/
         if(l.ne.0)call tally(fd,1,l,d)                                         !/*xcp       306*/
         l=locct(llct+ipt,icl)                                                  !/*xcp       307*/
         if(l.ne.0)call tally(fd,2,l,d)                                         !/*xcp       308*/
         if(kdb.ne.0)return                                                     !/*xcp       309*/
         do 105 i=0,lev-1                                                       !/*xcp       310*/
            l=locct(llct+1,int(udt(7,i)))                                       !/*xcp       311*/
            if(l.ne.0)call tally(fd,1,l,d)                                      !/*xcp       312*/
            l=locct(llct+ipt,int(udt(7,i)))                                     !/*xcp       313*/
            if(l.ne.0)call tally(fd,2,l,d)                                      !/*xcp       314*/
  105    continue                                                               !/*xcp       315*/
         if (mtlflg.ne.0.or.medflg.ne.0.and.fd.ne.zero) then                    !/*xcp       316*/
            do 110 i=1,6                                                        !/*xcp       317*/
               if (lev.eq.0) then                                               !/*xcp       318*/
                  zd(i)=gpblcm(i)                                               !/*xcp       319*/
               else                                                             !/*xcp       320*/
                  zd(i)=udt(i,0)                                                !/*xcp       321*/
               endif                                                            !/*xcp       322*/
  110       continue                                                            !/*xcp       323*/
            zd(7)=wgt                                                           !/*xcp       324*/
            zd(8)=erg                                                           !/*xcp       325*/
            zd(9)=eg_rc                                                         !/*xcp       326*/
            jz(1)=mkc                                                           !/*xcp       327*/
            jz(2)=mspf                                                          !/*xcp       328*/
            jz(5)=3                                                             !/*xcp       329*/
            call gdtal(8,zero,zd,jz)                                            !/*xcp       330*/
            zd(9)=eg_nt                                                         !/*xcp       331*/
            jz(5)=5                                                             !/*xcp       332*/
            call gdtal(8,zero,zd,jz)                                            !/*xcp       333*/
         endif                                                                  !/*xcp       334*/
         erg=ej                                                                 !/*xcp       335*/
         wgt=wj                                                                 !/*xcp       336*/
#endif                                                                          /*!xcp       337*/
         if (nr.eq.id_nonelastic_collisn_terminal) then                         !/*xcp       338*/
            nter=loss_gen_chg_nucl_interaction                                  !/*xcp       339*/
            erg=ec                                                              !/*xcp       340*/
            wgt=wt                                                              !/*xcp       341*/
            return                                                              !/*xcp       342*/
         else if (nr.eq.id_elastic_collisn_nonterminal) then                    !/*xcp       343*/
            call lose_pax (loss_gen_chg_elastic_scatter, ipt,                   !/*xcp       344*/
     &                     0, zero, wgt*(ec-erg))                               !/*xcp       345*/
            if (wgt.lt.wt) then                                                 !/*xcp       346*/
               call lose_pax (loss_gen_chg_nucl_interaction, ipt,               !/*xcp       347*/
     &                        0, wt-wgt, (wt-wgt)*ec)                           !/*xcp       348*/
               call contrib_pwb (ipt,ipwb_gen_chg_nuc_interaction,icl,          !/*xcp       349*/
     &                           wgt-wt)                                        !/*xcp       350*/
!              QUESTION:  play weight-cutoff game???                            !/*xcp       351*/
            endif                                                               !/*xcp       352*/
            if (erg.le.elc(ipt)) then                                           !/*xcp       353*/
               nb=3                                                             !/*xcp       354*/
               go to 500                                                        !/*xcp       355*/
            else if (erg.lt.eee(leee+ipt,ngp+1)) then                           !/*xcp       356*/
  130          ngp=ngp+1                                                        !/*xcp       357*/
               if(erg.lt.eee(leee+ipt,ngp+1))go to 130                          !/*xcp       358*/
            endif                                                               !/*xcp       359*/
         else if (nr.eq.id_true_pseudo_nonterminal) then                        !/*xcp       360*/
            continue                                                            !/*xcp       361*/
!           could count pseudocollisions here.                                  !/*xcp       362*/
         else if (nr.eq.id_fulldecay_pseudo_terminal) then                      !/*xcp       363*/
            nter=loss_gen_chg_particle_decay                                    !/*xcp       364*/
            erg=ec                                                              !/*xcp       365*/
            wgt=wt                                                              !/*xcp       366*/
            return                                                              !/*xcp       367*/
         else if (nr.eq.id_partdecay_pseudo_nonterminal) then                   !/*xcp       368*/
            call lose_pax (loss_gen_chg_particle_decay, ipt,                    !/*xcp       369*/
     &                     0, wt-wgt, (wt-wgt)*erg)                             !/*xcp       370*/
            call contrib_pwb (ipt,ipwb_gen_chg_particle_decay,icl,              !/*xcp       371*/
     &                        wgt-wt)                                           !/*xcp       372*/
!           QUESTION:  play weight-cutoff game???                               !/*xcp       373*/
         else                                                                   !/*xcp       374*/
            call expirx(1,'chg_pl',                                             !/*xcp       375*/
     &     'bad result from interact for particle at collision.')               !/*xcp       376*/
         endif                                                                  !/*xcp       377*/
      endif                                                                     !/*xcp       378*/
!                                                                               !/*xcp       379*/
!        terminate to time cutoff.                                              !/*xcp       380*/
      if (d.eq.dtc) then                                                        !/*xcp       381*/
         nter=loss_all_pls_time_cutoff                                          !/*xcp       382*/
         return                                                                 !/*xcp       383*/
      endif                                                                     !/*xcp       384*/
      vel=slite*sqrt(erg*(erg+2.*gpt(ipt)))/(erg+gpt(ipt))                      !/*xcp       385*/
!                                                                               !/*xcp       386*/
!        process the end of the substep.                                        !/*xcp       387*/
      if (d.lt.dls) then                                                        !/*xcp       388*/
!        particle remains in the same cell.                                     !/*xcp       389*/
         pac(kpac+ipt,3,icl)=pac(kpac+ipt,3,icl)+1.                             !/*xcp       390*/
         pac(kpac+ipt,4,icl)=pac(kpac+ipt,4,icl)+wgt                            !/*xcp       391*/
         if(nmaz.ne.0)call rslmaz(3,1)                                          !/*xcp       392*/
         jsu=0                                                                  !/*xcp       393*/
!        Play the weight-window and energy-splitting games.                     !/*xcp       394*/
         if(wwp(ipt,4).ne.0..and.ns.eq.2)call wtwndo(4)                         !/*xcp       395*/
         if(nter.ne.0)return                                                    !/*xcp       396*/
         if(espl(ipt,1).ne.0.)call ergimp                                       !/*xcp       397*/
         if(nter.ne.0)return                                                    !/*xcp       398*/
      else                                                                      !/*xcp       399*/
!        see if deflection keeps particle in the same cell.                     !/*xcp       400*/
         call echkcl(j)                                                         !/*xcp       401*/
         if (j.ne.0) then                                                       !/*xcp       402*/
!           shove the particle thru the cell boundary.                          !/*xcp       403*/
            call surfac                                                         !/*xcp       404*/
            if(krflg.ne.0)call eventp(3)                                        !/*xcp       405*/
            if(iptr.ne.0)call ptrak(3)                                          !/*xcp       406*/
            if(kdb.ne.0)return                                                  !/*xcp       407*/
            if(nter.ne.0)return                                                 !/*xcp       408*/
            go to 30                                                            !/*xcp       409*/
         endif                                                                  !/*xcp       410*/
      endif                                                                     !/*xcp       411*/
!                                                                               !/*xcp       412*/
!        complete energy substep for particle remaining in cell icl.            !/*xcp       413*/
  150 if(n1.ne.ngp)go to 40                                                     !/*xcp       414*/
      go to 40                                                                  !/*xcp       415*/
!                                                                               !/*xcp       416*/
  500 ec=erg                                                                    !/*xcp       417*/
      wt=wgt                                                                    !/*xcp       418*/
      jsu=0                                                                     !/*xcp       419*/
!     Convert from shakes to nsec.                                              !/*xcp       420*/
      tc=ten*tme                                                                !/*xcp       421*/
      call interact(nb,lp,erg,elc(ipt),icl,mkc,0,tc,uuu,vvv,www,wgt,            !/*xcp       422*/
     & xxx,yyy,zzz,nel(lnel+1),mel(lmel+1),aa9(laa9+1),zz9(lzz9+1),             !/*xcp       423*/
     & iazdex(liax+1),fmf(lfmf+1),rho(lrho+1),pcap(lpcp+1),                     !/*xcp       424*/
     & hsigg(lhsg+1,1),xsiso(lxso+1,1),esxlp(lexp+1,1),                         !/*xcp       425*/
#ifdef HISTP                                                                    /*!xcp       426*/
     & npstc,                                                                   !/*xcp       427*/
#endif                                                                          /*!xcp       428*/
#ifdef MESHTAL                                                                  /*!xcp       429*/
     & eg_rc,eg_nt,                                                             !/*xcp       430*/
#endif                                                                          /*!xcp       431*/
     & nr)                                                                      !/*xcp       432*/
      ej=erg                                                                    !/*xcpa       68*/
      erg=ec                                                                    !/*xcp       433*/
      wgt=wt                                                                    !/*xcp       434*/
#ifdef MESHTAL                                                                  /*!xcp       435*/
      fd=ej+eg_rc+eg_nt                                                         !/*xcpa       69*/
      l=locct(llct+1,icl)                                                       !/*xcp       437*/
      if(l.ne.0)call tally(fd,1,l,d)                                            !/*xcp       438*/
      l=locct(llct+ipt,icl)                                                     !/*xcp       439*/
      if(l.ne.0)call tally(fd,2,l,d)                                            !/*xcp       440*/
      if(kdb.ne.0)return                                                        !/*xcp       441*/
      do 505 i=0,lev-1                                                          !/*xcp       442*/
         l=locct(llct+1,int(udt(7,i)))                                          !/*xcp       443*/
         if(l.ne.0)call tally(fd,1,l,d)                                         !/*xcp       444*/
         l=locct(llct+ipt,int(udt(7,i)))                                        !/*xcp       445*/
         if(l.ne.0)call tally(fd,2,l,d)                                         !/*xcp       446*/
  505 continue                                                                  !/*xcp       447*/
      if (mtlflg.ne.0.or.medflg.ne.0.and.fd.ne.zero) then                       !/*xcpa       70*/
         do 510 i=1,6                                                           !/*xcp       449*/
            if (lev.eq.0) then                                                  !/*xcp       450*/
               zd(i)=gpblcm(i)                                                  !/*xcp       451*/
            else                                                                !/*xcp       452*/
               zd(i)=udt(i,0)                                                   !/*xcp       453*/
            endif                                                               !/*xcp       454*/
  510    continue                                                               !/*xcp       455*/
         zd(7)=wgt                                                              !/*xcp       456*/
         zd(8)=erg                                                              !/*xcp       457*/
         zd(9)=eg_rc                                                            !/*xcp       458*/
         jz(1)=mkc                                                              !/*xcp       459*/
         jz(2)=mspf                                                             !/*xcp       460*/
         jz(5)=3                                                                !/*xcp       461*/
         call gdtal(8,zero,zd,jz)                                               !/*xcp       462*/
         zd(9)=ej+eg_nt                                                         !/*xcpa       71*/
         jz(5)=5                                                                !/*xcp       464*/
         call gdtal(8,zero,zd,jz)                                               !/*xcp       465*/
      endif                                                                     !/*xcp       466*/
#endif                                                                          /*!xcp       467*/
      if (nr.eq.id_nonelastic_collisn_terminal) then                            !/*xcp       468*/
         nter=loss_gen_chg_nucl_interaction                                     !/*xcp       469*/
      else if (nr.eq.id_fulldecay_pseudo_terminal) then                         !/*xcp       470*/
         nter=loss_gen_chg_particle_decay                                       !/*xcp       471*/
      else if (nr.eq.id_stopped_particle_terminal) then                         !/*xcp       472*/
         nter=loss_all_pls_energy_cutoff                                        !/*xcp       473*/
      else if (nr.eq.id_elastic_collisn_nonterminal) then                       !/*xcpa       72*/
         nter=loss_all_pls_energy_cutoff                                        !/*xcpa       73*/
      else                                                                      !/*xcp       474*/
         call expirx(1,'chg_pl',                                                !/*xcp       475*/
     &   'bad result from interact for particle below cutoff.')                 !/*xcp       476*/
      endif                                                                     !/*xcp       477*/
      return                                                                    !/*xcp       478*/
      end                                                                       !/*xcp       479*/
