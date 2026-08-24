!#ident surfac.F from deck su                                                   !/*su          1*/
      subroutine surfac                                                         !/*su          2*/
!        process the particle thru surface jsu into the next cell.              !/*su          3*/
      use global_data                                                           !/*sux         1*/
!                                                                               !/*sux         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*sux         3*/
      implicit integer(kindi) (i-n)                                             !/*sux         4*/
!                                                                               !/*sux         5*/
!                                                                               !/*su          5*/
!        set npa to zero for the benefit of eventp.                             !/*su          6*/
      npa=0                                                                     !/*su          7*/
      if(ksu(lksu+jsu).lt.0)go to 210                                           !/*su4a        2*/
!                                                                               !/*su4a        3*/
!        score f8 tally before exiting current cell                             !/*su4a        4*/
      if(locph(llph+icl).ne.0)call talph(icl,-erg)                              !/*sux         6*/
      if(kdb.ne.0)return                                                        !/*su4a        7*/
      if(ksu(lksu+jsu).gt.0)call refpbc                                         !/*su4a        8*/
!                                                                               !/*su          9*/
!        set indicators for tallying and for surface-source writing.            !/*su         10*/
      l=locst(llst+ipt,jsu)                                                     !/*su         11*/
      if(issw.eq.0)go to 20                                                     !/*su         12*/
      if(ipty(ipt).eq.0)go to 20                                                !/*su         13*/
      if(kc8.gt.0)go to 20                                                      !/*su4b        1*/
      do 10 m=1,njss                                                            !/*su         15*/
      if(abs(jss(ljss+m)).ne.nsf(lnsf+jsu))go to 10                             !/*su4c2       1*/
      if(jsq(ljsq+m).eq.kfq)go to 30                                            !/*su4c2       2*/
   10 continue                                                                  !/*su4c2       3*/
   20 m=0                                                                       !/*su         17*/
!                                                                               !/*su         18*/
!        save pblcom for possible use by tallyx.                                !/*su         19*/
   30 call savpar(mpb,0)                                                        !/*su4c2       4*/
      i1=icl                                                                    !/*su4a       10*/
!                                                                               !/*su         24*/
!        identify the cell on the other side of the surface.                    !/*su         25*/
      cs=l+m                                                                    !/*su         26*/
      call newcel(cs)                                                           !/*su         27*/
      if(kdb.ne.0)go to 215                                                     !/*su4a       11*/
!                                                                               !/*su         29*/
!        set the surface tally flag and score tallies.                          !/*su         30*/
      if(abs(kfl-1).eq.1)go to 60                                               !/*su         31*/
      if(jfl(kjfl+jsu).eq.0)jfl(kjfl+jsu)=node                                  !/*su         32*/
   60 if(l.ne.0)call tally(                                                     !/*sux         7*/
#ifdef MESHTAL                                                                  /*!sux         8*/
     & zero,0,                                                                  !/*sux         9*/
#endif                                                                          /*!sux        10*/
     & l,cs)                                                                    !/*sux        11*/
      if(kdb.ne.0)return                                                        !/*su         34*/
      if(locph(llph+iap).ne.0)call talph(iap,erg)                               !/*su         40*/
      if(kdb.ne.0)return                                                        !/*su         41*/
!                                                                               !/*su         42*/
!        write to the surface-source scratch file if required.                  !/*su         43*/
      if(m.eq.0)go to 110                                                       !/*sux        12*/
      if(nsph.ne.2.and.cs*jss(ljss+m).le.0.)go to 110                           !/*su4b        2*/
      if(jss(ljss+njss+nilw+m).eq.jss(ljss+njss+nilw+m+1))go to 100             !/*su         46*/
      do 90 i=jss(ljss+njss+nilw+m),jss(ljss+njss+nilw+m+1)-1                   !/*su         47*/
   90 if(jss(i).eq.ncl(lncl+iap).or.jss(i).eq.-ncl(lncl+icl))go to 100          !/*su         48*/
      go to 110                                                                 !/*su         49*/
  100 call sufwrt(m,cs)                                                         !/*su         50*/
!                                                                               !/*su         51*/
!        increment the weight-balance tables.                                   !/*su         52*/
  110 call contrib_pwb (ipt,ipwb_external_exiting,i1,-wgt)                      !/*sux        13*/
      call contrib_pwb (ipt,ipwb_external_entering,iap,wgt)                     !/*sux        14*/
      if(nter.ne.0)return                                                       !/*su         55*/
!                                                                               !/*su         56*/
!        set the cell tally flag and increment the node.                        !/*su         57*/
      if(kfl.lt.2)go to 120                                                     !/*su         58*/
      if(ifl(kifl+i1).eq.0)ifl(kifl+i1)=node                                    !/*su4a       13*/
  120 node=node+1                                                               !/*su         60*/
      icl=iap                                                                   !/*su         62*/
      if(abs(iptr).eq.2)call ptrak(6)                                           !/*su4b        3*/
!                                                                               !/*su         64*/
!        check for a zero-importance or void new cell.                          !/*su         65*/
      if(fim(lfim+ipt,icl).eq.0.)go to 220                                      !/*su         66*/
      do 130 i=1,nipt                                                           !/*sux        15*/
      elc(i)=elp(lelp+i,icl)                                                    !/*su         68*/
      do 130 l=0,lev-1                                                          !/*su         69*/
  130 elc(i)=max(elc(i),elp(lelp+i,int(udt(7,l))))                              !/*su         70*/
      if(iplt.eq.1)go to 160                                                    !/*su         71*/
      if(erg.lt.elc(ipt).eqv.mcal.lt.2)go to 240                                !/*cor4-1    143*/
      if(mat(lmat+icl).eq.0)go to 180                                           !/*su         72*/
      f=fiml(ipt)                                                               !/*su         73*/
      do 140 i=1,nipt                                                           !/*sux        16*/
      fiml(i)=fim(lfim+i,icl)                                                   !/*su         75*/
      do 140 l=0,lev-1                                                          !/*su         76*/
  140 fiml(i)=fiml(i)*fim(lfim+i,int(udt(7,l)))                                 !/*su         77*/
      if(fiml(ipt).eq.0.)go to 220                                              !/*su         78*/
!                                                                               !/*su         79*/
!        check weight window if required.                                       !/*su         80*/
      if(wwp(ipt,4).eq.0.)go to 150                                             !/*su4c        1*/
      call wtwndo(3)                                                            !/*su4c        2*/
      if(nter.ne.0)return                                                       !/*su4c        3*/
      go to 180                                                                 !/*su4c        4*/
!                                                                               !/*su         87*/
!        importance sampling:  continue, terminate, or split.                   !/*su         88*/
  150 if(f.eq.fiml(ipt))go to 180                                               !/*su         89*/
      t1=fiml(ipt)/f                                                            !/*su         90*/
      go to 170                                                                 !/*su         91*/
!                                                                               !/*su         92*/
!        case of multigroup energy-dependent importances.                       !/*su         93*/
  160 l=lcmg+mxa*(jgp-1+mgm(ipt))                                               !/*su         94*/
      if(cmg(l+iap).eq.cmg(l+i1))go to 180                                      !/*su         95*/
      t1=cmg(l+iap)/cmg(l+i1)                                                   !/*su         96*/
      if(t1.gt.1.)t1=min(6,int(t1+.5))                                          !/*su         97*/
      t1=max(t1,.5*third)                                                       !/*su         98*/
      fismg=fismg*t1                                                            !/*su         99*/
!                                                                               !/*su        100*/
!        do russian roulette if the importance decreases.                       !/*su        101*/
  170 if(t1.gt.1.)go to 190                                                     !/*su        102*/
      if(t1.le.rang())go to 230                                                 !/*su        103*/
      wgt=wgt/t1                                                                !/*su        104*/
      call gain_pax (igain_all_pls_cell_importance, ipt,                        !/*sux        17*/
     &               0, wgt-t1*wgt, (wgt-t1*wgt)*erg)                           !/*sux        18*/
      call contrib_pwb (ipt,ipwb_variance_cell_importance,icl,                  !/*sux        19*/
     &                  wgt-t1*wgt)                                             !/*sux        20*/
!                                                                               !/*su        108*/
!        increment accounts and continue.                                       !/*su        109*/
  180 pac(kpac+ipt,1,icl)=pac(kpac+ipt,1,icl)+1.                                !/*su        110*/
      if(ipac2(kpc2+icl).eq.0)pac(kpac+ipt,2,icl)=pac(kpac+ipt,2,icl)+1.        !/*su        111*/
      ipac2(kpc2+icl)=1                                                         !/*su        112*/
      if(nmaz.ne.0)call rslmaz(2,1)                                             !/*su4b        6*/
      return                                                                    !/*su        113*/
!                                                                               !/*su        114*/
!        split if the importance increases.                                     !/*su        115*/
  190 npa=(t1-1.)+1e-10                                                         !/*su        116*/
      wgt=wgt/t1                                                                !/*su        117*/
      if(abs(t1-anint(t1)).lt.t1*1.e-10)go to 200                               !/*su        118*/
      npa=t1-rang()                                                             !/*su        119*/
      if (npa+1-t1.lt.zero) then                                                !/*sux        21*/
         call lose_pax (loss_all_pls_cell_importance, ipt,                      !/*sux        22*/
     &                  0, abs(npa+1-t1)*wgt, abs(npa+1-t1)*wgt*erg)            !/*sux        23*/
      else                                                                      !/*sux        24*/
         call gain_pax (igain_all_pls_cell_importance, ipt,                     !/*sux        25*/
     &                  0, abs(npa+1-t1)*wgt, abs(npa+1-t1)*wgt*erg)            !/*sux        26*/
      endif                                                                     !/*sux        27*/
      call contrib_pwb (ipt,ipwb_variance_cell_importance,icl,                  !/*sux        28*/
     &                  wgt*(npa+1-t1))                                         !/*sux        29*/
  200 pac(kpac+ipt,1,icl)=pac(kpac+ipt,1,icl)+npa+1                             !/*su        125*/
      if(ipac2(kpc2+icl).eq.0)pac(kpac+ipt,2,icl)=pac(kpac+ipt,2,icl)+1.        !/*su        126*/
      ipac2(kpc2+icl)=1                                                         !/*su        127*/
      if(nmaz.ne.0)call rslmaz(2,npa+1)                                         !/*su4b        7*/
      if(npa.eq.0)return                                                        !/*su        128*/
      call gain_pax (igain_all_pls_cell_importance,ipt,npa,zero,zero)           !/*sux        30*/
      if(erg.lt.xunru.and.erg.gt.xunrl.and.ipt.eq.1.and.mat(lmat+icl)           !/*su4c        5*/
     & .ne.0)call acetot(0,mat(lmat+icl))                                       !/*su4c        6*/
      call bankit(6)                                                            !/*su4a       14*/
      return                                                                    !/*su        132*/
!                                                                               !/*su        133*/
!        reflect the particle if the surface is that sort.                      !/*su        134*/
  210 call reflec                                                               !/*su        135*/
      if(abs(iptr).eq.2)call ptrak(6)                                           !/*su4b        8*/
      return                                                                    !/*su        136*/
!                                                                               !/*su        137*/
!        terminate:  zero importance, russian roulette, energy cutoff.          !/*su        138*/
  215 if(iap.ne.-12345)return                                                   !/*su4a       15*/
      kdb=0                                                                     !/*su4a       16*/
  220 nter = loss_all_pls_escape                                                !/*sux        31*/
      return                                                                    !/*su        140*/
  230 nter = loss_all_pls_cell_importance                                       !/*sux        32*/
      return                                                                    !/*su        142*/
  240 nter = loss_all_pls_energy_cutoff                                         !/*sux        33*/
      return                                                                    !/*cor4-1    145*/
      end                                                                       !/*su        143*/
