!#ident ypbssp.F from deck yp                                                   !/*yp          1*/
      subroutine ypbssp                                                         !/*yp          2*/
!        start a banked surface-source particle.                                !/*yp          3*/
      use global_data                                                           !/*ypx         1*/
!                                                                               !/*ypx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*ypx         3*/
      implicit integer(kindi) (i-n)                                             !/*ypx         4*/
!                                                                               !/*ypx         5*/
!                                                                               !/*yp          7*/
      if(kfl.eq.0)go to 40                                                      !/*yp          8*/
      if(kfl.eq.1)go to 20                                                      !/*yp          9*/
      do 10 i=1,mxa                                                             !/*yp         10*/
   10 ifl(kifl+i)=0                                                             !/*yp         11*/
      if(kfl.eq.2)go to 40                                                      !/*yp         12*/
   20 do 30 j=1,mxj                                                             !/*yp         13*/
   30 jfl(kjfl+j)=0                                                             !/*yp         14*/
   40 do 45 i=1,nipt                                                            !/*ypx         6*/
      if(wc1(i).ge.0.)go to 45                                                  !/*yp4c        1*/
      wcs1tc(i)=-wc1(i)*wgt                                                     !/*yp4c        2*/
      wcs2tc(i)=-wc2(i)*wgt                                                     !/*yp4c        3*/
   45 continue                                                                  !/*yp4b        5*/
   50 pac(kpac+ipt,2,icl)=pac(kpac+ipt,2,icl)+1.                                !/*yp         18*/
      ipac2(kpc2+icl)=1                                                         !/*yp         19*/
      ipsc=12                                                                   !/*yp         20*/
      swtm=wgt                                                                  !/*yp         21*/
!                                                                               !/*yp         22*/
!        check if the source weight is in the weight window.                    !/*yp         23*/
      if(nww(ipt).eq.0.or.mcal.ne.0.and.iplt.ne.2)go to 110                     !/*yp         24*/
      t1=erg                                                                    !/*yp4c        4*/
      if(wwp(ipt,6).ne.0.)t1=tme                                                !/*yp4c        5*/
      ic=0                                                                      !/*yp         25*/
      ib=nww(ipt)                                                               !/*yp         26*/
   60 if(ib-ic.eq.1)go to 80                                                    !/*yp         27*/
      ih=(ic+ib)/2                                                              !/*yp         28*/
      if(t1.lt.wwe(lwwe+ih+mww(ipt)))go to 70                                   !/*yp4c        6*/
      ic=ih                                                                     !/*yp         30*/
      go to 60                                                                  !/*yp         31*/
   70 ib=ih                                                                     !/*yp         32*/
      go to 60                                                                  !/*yp         33*/
   80 ww=wwval(ipt,icl,ib,1,ix)                                                 !/*yp4c2       1*/
      if(ww.eq.0.)go to 110                                                     !/*yp         35*/
      if(wgt.ge.ww.and.wgt.le.wwp(ipt,1)*ww)go to 110                           !/*yp         36*/
      i=1                                                                       !/*yp         37*/
      if(wgt.lt.ww)i=2                                                          !/*yp         38*/
      if(i.eq.1)call errprn(1,nwsg(i),5,erg,wgt,'erg','wgt',                    !/*yp4b        7*/
     & 'weight of source particle is above window.')                            !/*yp4a        3*/
      if(i.eq.2)call errprn(1,nwsg(i),5,erg,wgt,'erg','wgt',                    !/*yp4b        8*/
     & 'weight of source particle is below window.')                            !/*yp4a        5*/
      nwws(i,ib)=nwws(i,ib)+1                                                   !/*yp4b        9*/
!                                                                               !/*yp         49*/
!        report and count cases where source weight is below cutoff.            !/*yp         50*/
  110 if(wgt.gt.wcs2tc(ipt))go to 150                                           !/*yp         51*/
      if(erg.le.emcf(ipt))go to 150                                             !/*yp         52*/
      if(nww(ipt).eq.0)go to 120                                                !/*yp         53*/
      if(wwval(ipt,icl,1,0,ix).ne.0.)go to 150                                  !/*yp4c2       2*/
  120 continue                                                                  !/*yp         55*/
      call errprn(1,nwsb,4,wgt,zero,'wgt',' ',                                  !/*yp4b       11*/
     & 'source particle weight is below cutoff.')                               !/*yp4a        7*/
  150 if(krflg.ne.0)call eventp(1)                                              !/*yp         65*/
      if(iptr.ne.0)call ptrak(1)                                                !/*yp4a        8*/
!                                                                               !/*yp         66*/
!        score tallies and create dxtran particles.                             !/*yp         67*/
      if(locph(llph+icl).ne.0)call talph(icl,erg)                               !/*ypx         7*/
      if(kdb.ne.0)return                                                        !/*yp         69*/
      if(jsu.eq.-1234567)jsu=0                                                  !/*yp4b       12*/
      jsu=abs(jsu)                                                              !/*yp         70*/
      if(ndet(ipt).ne.0)call tallyd                                             !/*yp         71*/
      if(kdb.ne.0)return                                                        !/*yp         72*/
      if(ndx(ipt).gt.1.or.ndx(ipt).eq.1.and.idx.eq.0)call dxtran                !/*yp         73*/
      if(kdb.ne.0)return                                                        !/*yp         74*/
!                                                                               !/*yp         75*/
!        play the weight-window game.                                           !/*yp         76*/
      if(wwp(ipt,4).ne.0.)call wtwndo(3)                                        !/*yp4c        8*/
      return                                                                    !/*yp         78*/
      end                                                                       !/*yp         79*/
