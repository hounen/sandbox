!#ident tallyp.F from deck tp                                                   !/*tp          1*/
      subroutine tallyp                                                         !/*tp          2*/
!        print the tallies.                                                     !/*tp          3*/
      use global_data                                                           !/*tpx         1*/
!                                                                               !/*tpx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*tpx         3*/
      implicit integer(kindi) (i-n)                                             !/*tpx         4*/
!                                                                               !/*tpx         5*/
      dimension iv(8),ip(8),vv(10)                                              !/*tp4a        1*/
      character hb(9)*8,hc*1,ht*102                                             !/*tp4c        1*/
      data hb/2*'        ','user bin',' segment','mult bin','  cosine',         !/*tpx         6*/
     & '  energy','    time','        '/,hc/'='/                                !/*tpx         7*/
!                                                                               !/*tp          9*/
!        return if kcode problem has not yet tallied.                           !/*tp         10*/
      if(kc8.gt.0)return                                                        !/*tpx        10*/
!                                                                               !/*tp4a        3*/
!        find how many lines are in the tfc charts.                             !/*tp4a        4*/
      do 15 i=1,20                                                              !/*tp4a        5*/
   15 if(npc(i).ne.0)nn=i                                                       !/*tp4a        6*/
!                                                                               !/*tp         19*/
!        do all of the tallies in the problem.                                  !/*tp         20*/
      do 160 iper=0,npert                                                       !/*tp4b        7*/
      if(iper.gt.0)write(iuo,17)(hc,j=1,121),iptb(lipb+1,iper),                 !/*tp4b        8*/
     & (hc,j=1,121)                                                             !/*tp4b        9*/
   17 format(1h1,/1x,121a1,/2h =,119x,1h=,                                      !/*tp4b       10*/
     & /2h =,5x,50hthe following output gives the predicted change in,          !/*tp4b       11*/
     & 26h a tally for perturbation ,i5,1h.,32x,1h=,                            !/*tp4b       12*/
     & /2h =,5x,51hthe differential operator method was used to obtain,         !/*tp4b       13*/
     & 38h these results (1st and/or 2nd order).,25x,1h=,                       !/*tp4b       14*/
     & /2h =,119x,1h=,/1x,121a1,/)                                              !/*tp4b       17*/
      do 160 ital=1,ntal                                                        !/*tp4b       18*/
      it=ital+iper*ntal                                                         !/*tp4b       19*/
      iy=jptal(ljpt+2,ital)                                                     !/*tp         22*/
      hb(1)='    cell'                                                          !/*tp         23*/
      if(iy.lt.4)hb(1)=' surface'                                               !/*tp         24*/
      if(iy.eq.5)hb(1)='detector'                                               !/*tp         25*/
      do 20 i=1,8                                                               !/*tp         26*/
   20 ip(i)=i                                                                   !/*tp         27*/
      nhsd(lnhs+nsp+8,it)=1                                                     !/*tp4b       20*/
      do 25 i=4,7                                                               !/*tp4a        8*/
   25 nhsd(lnhs+nsp+5+i,it)=0                                                   !/*tp4b       21*/
      ac=0.1                                                                    !/*tp4a       10*/
      if(iy.eq.5)ac=.05                                                         !/*tp4a       11*/
      k=ital                                                                    !/*tp         28*/
      if(jfq(ljfq+1,ital).eq.0)k=0                                              !/*tp         29*/
      if(jfq(ljfq+1,k).eq.0)go to 40                                            !/*tp         30*/
      do 30 i=1,8                                                               !/*tp         31*/
   30 ip(i)=abs(jfq(ljfq+i,k))                                                  !/*tp         32*/
!                                                                               !/*tp         33*/
!        print descriptive information about the tally.                         !/*tp         34*/
   40 continue                                                                  !/*tpx        12*/
#ifdef RADIOG                                                                   /*!tpx        13*/
!       Check for talnp options.                                                !/*tpx        14*/
      if(notal.eq.0)go to 48                                                    !/*tpx        15*/
      if(notal.gt.0)go to 43                                                    !/*tpx        16*/
      call tallyh                                                               !/*tpx        17*/
      go to 125                                                                 !/*tpx        18*/
   43 do 45 i=1,notal                                                           !/*tpx        19*/
   45 if(jptal(ljpt+1,ital).eq.ntprt(i))go to 46                                !/*tpx        20*/
      go to 48                                                                  !/*tpx        21*/
   46 call tallyh                                                               !/*tpx        22*/
      go to 125                                                                 !/*tpx        23*/
   48 continue                                                                  !/*tpx        24*/
#endif                                                                          /*!tpx        25*/
      call tallyq(ip)                                                           !/*tpx        26*/
!                                                                               !/*tp         36*/
!        do the outer six levels of the printing hierarchy.                     !/*tp         37*/
      do 120 i1=1,iptal(lipt+ip(1),3,ital)                                      !/*tp         38*/
      iv(ip(1))=i1                                                              !/*tp         39*/
      do 110 i2=1,iptal(lipt+ip(2),3,ital)                                      !/*tp         40*/
      iv(ip(2))=i2                                                              !/*tp         41*/
      do 110 i3=1,iptal(lipt+ip(3),3,ital)                                      !/*tp         42*/
      iv(ip(3))=i3                                                              !/*tp         43*/
      do 110 i4=1,iptal(lipt+ip(4),3,ital)                                      !/*tp         44*/
      iv(ip(4))=i4                                                              !/*tp         45*/
      do 110 i5=1,iptal(lipt+ip(5),3,ital)                                      !/*tp         46*/
      iv(ip(5))=i5                                                              !/*tp         47*/
      do 110 i6=1,iptal(lipt+ip(6),3,ital)                                      !/*tp         48*/
      iv(ip(6))=i6                                                              !/*tp         49*/
!                                                                               !/*tp         50*/
!        print long form bin descriptions.                                      !/*tp         51*/
      write(iuo,'(1h )')                                                        !/*tp         52*/
      do 50 i=1,6                                                               !/*tp         53*/
   50 if(iptal(lipt+ip(i),1,ital).ne.0)call binlin(iv(ip(i)),ip(i),' ')         !/*tp         54*/
!                                                                               !/*tp         55*/
!        print the tally table, more than one if necessary.                     !/*tp         56*/
      mk=iptal(lipt+ip(8),3,ital)                                               !/*tp         57*/
      do 90 ik=1,mk,5                                                           !/*tp         58*/
      n=min(5,mk-ik+1)                                                          !/*tp         59*/
!                                                                               !/*tp         60*/
!        print the column heading.                                              !/*tp         61*/
      if(ik.ne.1)write(iuo,'(1h )')                                             !/*tp         62*/
      if(iptal(lipt+ip(8),1,ital).eq.0)go to 70                                 !/*tp         63*/
      ht=' '                                                                    !/*tp         64*/
      if(ip(8).ne.2)ht(1:1)=':'                                                 !/*tp         65*/
      do 60 i=1,n                                                               !/*tp         66*/
   60 call binval(ik+i-1,ip(8),ht(21*i-13:21*i-3),j)                            !/*tp         67*/
      write(iuo,'(5x,a8,a102)')hb(ip(8)),ht                                     !/*tp         68*/
   70 if(iptal(lipt+ip(7),1,ital).ne.0.and.ip(7).ne.2)                          !/*tp         69*/
     & write(iuo,'(4x,a8)')hb(ip(7))                                            !/*tp         70*/
!                                                                               !/*tp         71*/
!        do the lines in the tally table.                                       !/*tp         72*/
      do 90 i7=1,iptal(lipt+ip(7),3,ital)                                       !/*tp         73*/
      iv(ip(7))=i7                                                              !/*tp         74*/
      ht=' '                                                                    !/*tp         75*/
      if(iptal(lipt+ip(7),1,ital).ne.0)call binval(i7,ip(7),ht,i)               !/*tp         76*/
      ts=0.                                                                     !/*tp4a       12*/
!                                                                               !/*tp         77*/
!        compute the tally values and errors for the table line.                !/*tp         78*/
      do 80 i=1,n                                                               !/*tp         79*/
      iv(ip(8))=ik+i-1                                                          !/*tp         80*/
      j=ltal+jptal(ljpt+5,ital)+iv(8)+iptal(lipt+8,5,ital)*(iv(7)-1)+           !/*tp         81*/
     & iptal(lipt+7,5,ital)*(iv(6)-1)+iptal(lipt+6,5,ital)*(iv(5)-1)+           !/*tp         82*/
     & iptal(lipt+5,5,ital)*(iv(4)-1)+iptal(lipt+4,5,ital)*(iv(3)-1)+           !/*tp         83*/
     & iptal(lipt+3,5,ital)*(iv(2)-1)+iptal(lipt+2,5,ital)*(iv(1)-1)+mxf        !/*tp         84*/
      j=j+iper*mxfp                                                             !/*tp4b       22*/
      tpp(i)=0.                                                                 !/*tp4a       13*/
      tpp(5+i)=0.                                                               !/*tp         85*/
      vv(i)=0.                                                                  !/*tp4a       14*/
      vv(i+5)=0.                                                                !/*tp4a       15*/
      if(tal(j).eq.0.)nhsd(lnhs+nsp+10,it)=nhsd(lnhs+nsp+10,it)+1               !/*tp4b       23*/
      if(tal(j).eq.0..or.tal(mxf+j).eq.0.)go to 80                              !/*tp4a       17*/
!                                                                               !/*tp4a       18*/
!        calculate estimated relative error using first two moments.            !/*tp4a       19*/
      t=tal(j)                                                                  !/*tp4a       20*/
      tpp(5+i)=sqrt(max(zero,min(tal(mxf+j)/t**2-fpi,one)))                     !/*tp4a       21*/
      if(tpp(5+i).ge.ac)nhsd(lnhs+nsp+11,it)=nhsd(lnhs+nsp+11,it)+1             !/*tp4b       24*/
!                                                                               !/*tp4a       23*/
!        assume a very small rel error is round off and set to zero.            !/*tp4a       24*/
      if(tpp(5+i).le.1.e-7)tpp(5+i)=0.                                          !/*tp4a       25*/
      if(iptal(lipt+4,2,ital).ne.0)t=t/tds(iptal(lipt+4,2,ital)+                !/*tp         90*/
     & iptal(lipt+4,3,ital)*(iv(1)-1)+iv(4))                                    !/*tp         91*/
      if((iy.eq.6.or.iy.eq.7).and.jptal(ljpt+4,ital).eq.1)                      !/*tpx        27*/
     & t=t*1.60219e-22                                                          !/*tp         93*/
      tpp(i)=t*fpi                                                              !/*tp4a       26*/
      if(nmxf.eq.3.or.dbcn(15).eq.0.)go to 80                                   !/*tp4a       27*/
!                                                                               !/*tp4a       28*/
!        calculate variance of variance(vov) using first four moments.          !/*tp4a       29*/
!        see a-1/87-749 and td-6-27-78 for the vov equation.                    !/*tp4a       30*/
!        the equation in td-6-27-78 is the square root of the vov.              !/*tp4a       31*/
      ts=ts+t                                                                   !/*tp4a       32*/
      if(tpp(5+i).eq.0.)go to 80                                                !/*tp4a       33*/
      t1=tal(mxf+j)-fpi*tal(j)**2                                               !/*tp4a       34*/
      t2=tal(3*mxf+j)-4.*tal(j)*tal(2*mxf+j)*fpi-4.*tal(j)**4*fpi**3+           !/*tp4a       35*/
     & 8.*tal(mxf+j)*(tal(j)*fpi)**2-fpi*tal(mxf+j)**2                          !/*tp4a       36*/
      if(t2.le.0..or.t1.le.0..or.tal(mxf+j).le.0)go to 80                       !/*tp4a       37*/
      vv(i+5)=max(zero,min(t2/t1**2,one))                                       !/*tp4a       38*/
      if(t2.lt.1.e-30.and.vv(i+5).eq.one)vv(i+5)=0.                             !/*tp4a       39*/
      if(vv(i+5).ge.0.1)nhsd(lnhs+nsp12,it)=nhsd(lnhs+nsp12,it)+1               !/*tp4b       25*/
!                                                                               !/*tp4a       41*/
!        calculate asymmetric confidence interval shift(w/scale factor).        !/*tp4a       42*/
!        see a-1/87-749 for the confidence interval center shift.               !/*tp4a       43*/
!        shift is scaled to normalized mean sum t from unnrm sum tal(j).        !/*tp4a       44*/
      vv(i)=tpp(i)+sign(max(zero,.5*fpi*t*(tal(2*mxf+j)-3.*tal(mxf+j)*          !/*tp4a       45*/
     & tal(j)*fpi+2.*tal(j)**3*fpi**2))/(t1*tal(j)),tpp(i))                     !/*tp4a       46*/
   80 continue                                                                  !/*tp4a       47*/
      nhsd(lnhs+nsp+9,it)=nhsd(lnhs+nsp+9,it)+n                                 !/*tp4b       26*/
!                                                                               !/*tp         95*/
!        print the tally table line.                                            !/*tp         96*/
      write(iuo,85)ht(1:11),(tpp(i),tpp(i+5),i=1,n)                             !/*tp4a       49*/
   85 format(3x,a11,5(1pe14.5,0pf7.4))                                          !/*tp4a       50*/
   90 if(ts.ne.0.)write(iuo,95)(vv(i),vv(i+5),i=1,n)                            !/*tp4a       51*/
   95 format(14x,5(1pe14.5,0pf7.4))                                             !/*tp4a       52*/
  110 continue                                                                  !/*tp         99*/
!                                                                               !/*tp        100*/
!        print detector diagnostics with the detector if possible.              !/*tp        101*/
  120 if(ip(1).eq.1.and.iy.eq.5.and.nps.ne.0)call dddiag(i1,0)                  !/*tp        102*/
!                                                                               !/*tp        103*/
!        otherwise print detector diagnostics at the end of the tally.          !/*tp        104*/
      if(ip(1).eq.1)go to 140                                                   !/*tp4c        4*/
#ifdef RADIOG                                                                   /*!tpx        28*/
  125 continue                                                                  !/*tpx        29*/
#endif                                                                          /*!tpx        30*/
      if(iy.ne.5.or.nps.eq.0)go to 140                                          !/*tpx        31*/
      do 130 i1=1,iptal(lipt+1,3,ital)                                          !/*tp        106*/
  130 call dddiag(i1,1)                                                         !/*tp        107*/
!                                                                               !/*tp        108*/
!        print history tally information in tally fluctuation chart bin.        !/*tp4a       53*/
  140 call prstat(nn)                                                           !/*tp4a       54*/
!                                                                               !/*tp        120*/
  160 continue                                                                  !/*tp        121*/
!                                                                               !/*tp        122*/
!        print table of tally scores beyond last bin, if any.                   !/*tp        123*/
  165 k=0                                                                       !/*tp4c        6*/
      m=4                                                                       !/*tp4c        7*/
      if(hb(9).ne.' ')m=5                                                       !/*tp4c        8*/
      do 210 i=1,ntal                                                           !/*tp4a       56*/
      do 170 j=1,m                                                              !/*tp4c        9*/
  170 if(ntbb(lntb+j,i).ne.0)go to 180                                          !/*tp4a       58*/
      go to 210                                                                 !/*tp4a       59*/
  180 if(k.eq.0)write(iuo,190)hb(9)                                             !/*tp4c       10*/
  190 format(1h1,4x,35hsome tally scores were not made for,                     !/*tp4a       61*/
     & 17h various reasons://34x,15hbeyond last bin,5x,6hnot in,                !/*tp4a       62*/
     & /19x,5htally,6x,5hangle,3x,6henergy,3x,4htime,4x,4huser,                 !/*tp4c       11*/
     & 1x,a8)                                                                   !/*tp4c       12*/
      k=1                                                                       !/*tp4a       64*/
      write(iuo,200)jptal(ljpt+1,i),(ntbb(lntb+l,i),l=1,m)                      !/*tp4c       13*/
  200 format(i23,i11,4i8)                                                       !/*tp4c       14*/
  210 continue                                                                  !/*tp        136*/
      if(nps.le.0)return                                                        !/*tp4c       15*/
!                                                                               !/*tp4c       16*/
!        print tally fluctuation charts.                                        !/*tp4c       17*/
      call ptfc(nn)                                                             !/*tp4c       18*/
      return                                                                    !/*tp        140*/
      end                                                                       !/*tp        141*/
