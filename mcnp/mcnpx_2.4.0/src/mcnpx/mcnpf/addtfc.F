!#ident addtfc.F from deck ad                                                   !/*ad          1*/
      subroutine addtfc(ln,ls,lp)                                               !/*ad4a        1*/
!        add a line to the tally fluctuation charts.                            !/*ad          3*/
      use global_data                                                           !/*adx         1*/
!                                                                               !/*adx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*adx         3*/
      implicit integer(kindi) (i-n)                                             !/*adx         4*/
!                                                                               !/*adx         5*/
      dimension ks(0:ntp)                                                       !/*ad4a        2*/
      data ks/0,ntp*0/                                                          !/*ad4a        3*/
!                                                                               !/*ad          5*/
!        make room for the new line if necessary.                               !/*ad          6*/
      do 10 l=1,20                                                              !/*ad          7*/
   10 if(npc(l).eq.0)go to 30                                                   !/*ad          8*/
      do 20 l=1,10                                                              !/*ad          9*/
      npc(l)=npc(2*l)                                                           !/*ad         10*/
      npc(2*l+9*(1-l/6))=0                                                      !/*ad         11*/
      do 20 ip=0,npert                                                          !/*ad4b        1*/
      do 20 ital=1,ntal                                                         !/*ad4b        2*/
      it=ital+ip*ntal                                                           !/*ad4b        3*/
      do 20 j=1,5                                                               !/*ad4a        4*/
   20 tfc(ltfc+j,l,it)=tfc(ltfc+j,2*l,it)                                       !/*ad         14*/
!                                                                               !/*ad         15*/
!        calculate the entries in the new line.                                 !/*ad         16*/
   30 ft=0.                                                                     !/*ad         17*/
      t=max(1,nps)                                                              !/*ad         18*/
      if(nsr.eq.71.and.kcz.ge.ikz)t=nsrck*(kcz-ikz)                             !/*ad4c        1*/
      if(kc8.lt.0)t=t+nsrck-wt0*nsa                                             !/*ad4c        3*/
      if(knrm.ne.0)t=pax(1,1,1)                                                 !/*ad4c        4*/
      if(nsr.eq.6.and.nrrs.ge.nrss)t=max(1,np1)                                 !/*ad         19*/
      if(nsr.eq.6.and.nrrs.lt.nrss)t=max(1,npsr)                                !/*ad         20*/
      if(t.gt..5)ft=1./t                                                        !/*ad4b        7*/
      npc(l)=nps                                                                !/*ad4b        8*/
!                                                                               !/*ad4a        6*/
!        calculate quantities for each tally fluctuation chart bin.             !/*ad4a        7*/
      do 140 iper=0,npert                                                       !/*ad4b        9*/
      do 140 ital=1,ntal                                                        !/*ad4b       10*/
      it=ital+iper*ntal                                                         !/*ad4b       11*/
      do 60 j=1,6                                                               !/*ad4a        9*/
   60 tfc(ltfc+j,l,it)=0.                                                       !/*ad4a       10*/
      nhsd(ln,it)=0                                                             !/*ad4a       11*/
      k=ltal+jptal(ljpt+7,ital)+iper*mxfp                                       !/*ad4b       12*/
      t=tal(k+mxf)                                                              !/*ad         34*/
      if(t.eq.0..or.ft.eq.0..or.tal(k+2*mxf).le.0.)go to 140                    !/*ad4a       12*/
      tfc(ltfc+2,l,it)=min(tal(k+2*mxf)/t**2-ft,one)                            !/*ad4a       13*/
      if(tfc(ltfc+2,l,it).le.1.e-8)tfc(ltfc+2,l,it)=0.                          !/*ad4a       14*/
      tfc(ltfc+2,l,it)=sqrt(tfc(ltfc+2,l,it))                                   !/*ad4a       15*/
      if(iptal(lipt+4,2,ital).ne.0)t=t/tds(iptal(lipt+4,2,ital)+                !/*ad4b       13*/
     & iptal(lipt+4,3,ital)*(jtf(ljtf+1,ital)-1)+jtf(ljtf+4,ital))              !/*ad4b       14*/
      if((jptal(ljpt+2,ital).eq.6.or.jptal(ljpt+2,ital).eq.7).and.              !/*ad4b       15*/
     & jptal(ljpt+4,ital).eq.1)t=t*1.60219e-22                                  !/*adx         8*/
      tfc(ltfc+1,l,it)=t*ft                                                     !/*ad         42*/
      tfc(ltfc+3,l,it)=(cts-cpk)*tfc(ltfc+2,l,it)**2                            !/*adx         9*/
!                                                                               !/*ad4a       16*/
!        calculate and store the variance of the variance for tfc bin.          !/*ad4a       17*/
      if(tfc(ltfc+2,l,it).eq.0.)go to 70                                        !/*ad4a       18*/
      t1=tal(k+2*mxf)-ft*tal(k+mxf)**2                                          !/*ad4a       19*/
      t4=shsd(lshs+nsp+4,it)-4.*tal(k+mxf)*shsd(lshs+nsp+3,it)*ft+              !/*ad4a       20*/
     & 8.*tal(k+2*mxf)*(tal(k+mxf)*ft)**2-4.*tal(k+mxf)**4*ft**3-               !/*ad4a       21*/
     & ft*tal(k+2*mxf)**2                                                       !/*ad4a       22*/
      if(t4.le.0..or.t1.le.0.)go to 70                                          !/*ad4a       23*/
      tfc(ltfc+4,l,it)=max(zero,min(t4/t1**2,one))                              !/*ad4a       24*/
      if(t4.lt.1.e-30.and.tfc(ltfc+4,l,it).eq.one)tfc(ltfc+4,l,it)=0.           !/*ad4a       25*/
      if(tfc(ltfc+3,l,it).ne.0.)tfc(ltfc+3,l,it)=60./tfc(ltfc+3,l,it)           !/*ad4a       26*/
   70 if(tfc(ltfc+2,l,it).eq.0.)tfc(ltfc+3,l,it)=1.e30                          !/*ad4a       27*/
!                                                                               !/*ad4a       28*/
!        calculate slope (using a pareto) of tail of history tally pdf.         !/*ad4a       29*/
!        the tail is defined as the extreme 5% with a minimum of 25             !/*ad4a       30*/
!        history tallies for analysis to fit the pareto parameters.             !/*ad4a       31*/
      if(nhsd(ln-3,it)-nhsd(lnhs+1,it)-nhsd(ln-5,it).lt.500)go to 140           !/*ad4a       32*/
!                                                                               !/*ad4a       33*/
!        check for the appearance of a bounded extreme tally value.             !/*ad4a       34*/
      if(shsd(lp,it)/shsd(lp-100,it).lt.1.01)tfc(ltfc+5,l,it)=10.               !/*ad4a       35*/
      if(tfc(ltfc+5,l,it).eq.10.)go to 140                                      !/*ad4a       36*/
!                                                                               !/*ad4a       37*/
!        check to see if there are enough different tally values.               !/*ad4a       38*/
!        m is the number of different values of the large scores.               !/*ad4a       39*/
!        ks(i) shows where the last point of different value i is.              !/*ad4a       40*/
      m=1                                                                       !/*ad4a       41*/
      ks(1)=1                                                                   !/*ad4a       42*/
      do 80 j=1,ntp-1                                                           !/*ad4a       43*/
      if(shsd(ls+j+1,it)/shsd(ls+j,it).gt.1.0001)m=m+1                          !/*ad4a       44*/
   80 ks(m)=j+1                                                                 !/*ad4a       45*/
!                                                                               !/*ad4a       46*/
!        use only the number of large tallies to be fit nf.                     !/*ad4a       47*/
      nf=min(ntp-1,int(.05*nhsd(ln-3,it)))                                      !/*ad4a       48*/
      do 90 ms=2,ntp-1                                                          !/*ad4a       49*/
   90 if(ntp-ks(ms).lt.nf)go to 100                                             !/*ad4a       50*/
  100 ms=ms-1                                                                   !/*ad4a       51*/
!                                                                               !/*ad4a       52*/
!        check if enough different history scores m-ms exist for fit.           !/*ad4a       53*/
      if(ms.eq.1.and.m.gt.3.and.nf+1.eq.ntp.and.ks(2).gt.2)ms=2                 !/*ad4a       54*/
      if(m-ms-1)140,120,110                                                     !/*ad4a       55*/
!                                                                               !/*ad4a       56*/
!        find tail starting value ts and drop the first ks(ms) points.          !/*ad4a       57*/
!        ms is the number of values to be skipped after finding ts.             !/*ad4a       58*/
  110 ts=0.5*(shsd(ls+ks(ms),it)+shsd(ls+ks(ms)+1,it))                          !/*ad4a       59*/
      np=ntp-ks(ms)                                                             !/*ad4a       60*/
      if(np.ge.25)go to 130                                                     !/*ad4a       61*/
!                                                                               !/*ad4a       62*/
!        find pareto fit start value ts for exactly two values or for           !/*ad4a       63*/
!        case where there too few points left when ks(ms) are deleted.          !/*ad4a       64*/
!        do not throw away any history score values for this case.              !/*ad4a       65*/
  120 ts=shsd(ls+ks(ms),it)-(shsd(ls+ks(ms+1),it)-shsd(ls+ks(ms),it))*          !/*ad4a       66*/
     & (ks(ms+1)-ks(ms))/(ks(ms)-ks(ms-1))                                      !/*ad4a       67*/
      ms=ms-1                                                                   !/*ad4a       68*/
      np=ntp-ks(ms)                                                             !/*ad4a       69*/
!                                                                               !/*ad4a       70*/
!        store np and ts for later use in the prints.                           !/*ad4a       71*/
  130 lk=ls+ks(ms)                                                              !/*ad4a       72*/
      nhsd(ln,it)=np                                                            !/*ad4a       73*/
      shsd(lp+2,it)=ts                                                          !/*ad4a       74*/
!                                                                               !/*ad4a       76*/
!        find pareto parameters that best fit the np extreme tallies.           !/*ad4a       77*/
      call simplx(l,lk,np,ts)                                                   !/*ad4a       78*/
  140 continue                                                                  !/*ad4a       79*/
      if(l.eq.20.and.npc(19)+npd.eq.npc(20))npd=2*npd                           !/*adx        12*/
      return                                                                    !/*ad         47*/
      end                                                                       !/*ad         48*/
