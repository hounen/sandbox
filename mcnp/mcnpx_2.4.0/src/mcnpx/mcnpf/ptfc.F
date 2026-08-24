!#ident ptfc.F from deck pk                                                     !/*pk          1*/
      subroutine ptfc(nn)                                                       !/*pk4a        1*/
!        print the tally fluctuation charts.                                    !/*pk4a        2*/
      use global_data                                                           !/*pkx         1*/
!                                                                               !/*pkx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*pkx         3*/
      implicit integer(kindi) (i-n)                                             !/*pkx         4*/
!                                                                               !/*pkx         5*/
      dimension t5(3)                                                           !/*pk4a        4*/
      character ha(3)*8,hc*25,hp*6,ht(15)*90                                    !/*pk4b        1*/
      data ht/'no nonzero tallies were made in the tally fluctuation cha        !/*pk4a        6*/
     &rt bin','the estimated mean has a trend during the last half of th        !/*pk4a        7*/
     &e problem','the relative error exceeds the recommended value of 0.        !/*pk4a        8*/
     &1 for nonpoint detector tallies','the relative error exceeds the r        !/*pk4a        9*/
     &ecommended value of 0.05 for point detector tallies','the relative        !/*pk4a       10*/
     & error does not monotonically decrease over the last half of the p        !/*pk4a       11*/
     &roblem','the relative error appears not to decrease as 1/sqrt(nps)        !/*pk4a       12*/
     & for last half of problem','the variance of the variance exceeds t        !/*pk4a       13*/
     &he recommended value of 0.1','the variance of the variance does no        !/*pk4a       14*/
     &t monotonically decrease over the last half of problem', 'the vari        !/*pkx         6*/
     &ance of the variance appears not to decrease as 1/nps for the last        !/*pkx         7*/
     & half of problem','the figure of merit does not appear to be a con        !/*pkx         8*/
     &stant for the last half of the problem','the figure of merit has a        !/*pkx         9*/
     & trend during the last half of the problem','the slope of decrease        !/*pkx        10*/
     & of largest tallies is less than the minimum acceptable value of 3        !/*pkx        11*/
     &.0','there is insufficient tfc bin tally information to estimate t        !/*pkx        12*/
     &he large tally slope reliably','the large tally slope was not esti        !/*pkx        13*/
     &mated because the fitting algorithm did not converge',                    !/*pkx        14*/
     & 'passed the 10 statistical checks for the tally fluctuation chart        !/*pkx        15*/
     & bin result'/                                                             !/*pkx        16*/
!                                                                               !/*pk4a       25*/
!        print a status table of tally statistical checks for tfc bin.          !/*pk4a       26*/
      write(iuo,10)                                                             !/*pk4a       27*/
   10 format(46h1status of the statistical checks used to form,                 !/*pk4a       28*/
     & 53h confidence intervals for the mean for each tally bin///              !/*pk4a       29*/
     & 53h tally   result of statistical checks for the tfc bin,                !/*pk4a       30*/
     & 59h (the first check not passed is listed) and error magnitude,          !/*pk4a       31*/
     & 19h check for all bins)                                                  !/*pk4a       32*/
      nb=0                                                                      !/*pk4a       33*/
      nf=0                                                                      !/*pk4a       34*/
      nt=0                                                                      !/*pk4a       35*/
      nz=0                                                                      !/*pk4a       36*/
      n0=0                                                                      !/*pk4a       37*/
      do 110 ip=0,npert                                                         !/*pk4b        2*/
      if(ip.gt.0)write(iuo,'(/18h for perturbation ,i5)')iptb(lipb+1,ip)        !/*pk4b        3*/
      do 110 ital=1,ntal                                                        !/*pk4b        4*/
      it=ital+ip*ntal                                                           !/*pk4b        5*/
      nw=nhsd(lnhs+nsp+7,it)                                                    !/*pk4a       39*/
      nhsd(lnhs+nsp+7,it)=0                                                     !/*pk4a       40*/
      if(nhsd(lnhs+nsp+8,it).eq.1)nt=nt+1                                       !/*pk4a       41*/
      if(nw.eq.0)write(iuo,20)jptal(ljpt+1,ital),ht(nhsd(lnhs+nsp+8,it))        !/*pk4b        6*/
   20 format(/i6,3x,a90)                                                        !/*pk4a       43*/
      if(nw.gt.0)write(iuo,30)jptal(ljpt+1,ital),nw,                            !/*pk4b        7*/
     & ht(nhsd(lnhs+nsp+8,it))                                                  !/*pk4a       45*/
   30 format(/i6,2x,7h missed,i3,23h of 10 tfc bin checks: ,a90)                !/*pk4a       46*/
      if(nw.gt.0)nf=nf+1                                                        !/*pk4a       47*/
!                                                                               !/*pk4a       48*/
!        check status of relative errors and vov checks of tally bins.          !/*pk4a       49*/
      ac=0.1                                                                    !/*pk4a       50*/
      if(jptal(ljpt+2,ital).eq.5)ac=.05                                         !/*pk4b        8*/
      if(nhsd(lnhs+nsp+10,it)+nhsd(lnhs+nsp+11,it)+nhsd(lnhs+nsp+12,it)         !/*pk4a       52*/
     & .gt.0)go to 60                                                           !/*pk4a       53*/
      if(nmxf.eq.3)write(iuo,40)nhsd(lnhs+nsp+9,it),ac                          !/*pk4a       54*/
   40 format(8x,28h passed all bin error check:,i6,                             !/*pk4a       55*/
     & 46h tally bins all have relative errors less than,f5.2,                  !/*pk4a       56*/
     & 18h with no zero bins)                                                   !/*pk4a       57*/
      if(nmxf.eq.5)write(iuo,50)nhsd(lnhs+nsp+9,it),ac                          !/*pk4a       58*/
   50 format(8x,28h passed all bin error check:,i6,                             !/*pk4a       59*/
     & 38h tally bins all have relative errors <,f5.2,                          !/*pk4a       60*/
     & 33h and vovs < 0.1 with no zero bins)                                    !/*pk4a       61*/
      go to 110                                                                 !/*pk4a       62*/
!                                                                               !/*pk4a       63*/
!        check for tallies with all zeros and print the information.            !/*pk4a       64*/
   60 if(nhsd(lnhs+nsp+9,it).gt.nhsd(lnhs+nsp+10,it))go to 80                   !/*pk4a       65*/
      write(iuo,70)nhsd(lnhs+nsp+9,it)                                          !/*pk4a       66*/
   70 format(8x,47h no nonzero tallies were made in any one of the,i7,          !/*pk4a       67*/
     & 11h tally bins)                                                          !/*pk4a       68*/
      n0=n0+1                                                                   !/*pk4a       69*/
      go to 110                                                                 !/*pk4a       70*/
!                                                                               !/*pk4a       71*/
!        print number of bins not meeting magnitude error convergence.          !/*pk4a       72*/
   80 hp='missed'                                                               !/*pk4a       73*/
      if(nhsd(lnhs+nsp+11,it)+nhsd(lnhs+nsp+12,it).eq.0)hp='passed'             !/*pk4a       74*/
      if(hp.eq.'missed')nb=nb+1                                                 !/*pk4a       75*/
      if(nmxf.eq.3)write(iuo,90)hp,nhsd(lnhs+nsp+9,it),                         !/*pk4a       76*/
     & nhsd(lnhs+nsp+10,it),nhsd(lnhs+nsp+11,it),ac                             !/*pk4a       77*/
   90 format(9x,a6,21h all bin error check:,i6,15h tally bins had,              !/*pk4a       78*/
     & i6,20h bins with zeros and,i6,26h bins with relative errors,             !/*pk4a       79*/
     & 10h exceeding,f5.2)                                                      !/*pk4a       80*/
      if(nmxf.eq.5)write(iuo,100)hp,nhsd(lnhs+nsp+9,it),                        !/*pk4a       81*/
     & nhsd(lnhs+nsp+10,it),nhsd(lnhs+nsp+11,it),ac,nhsd(lnhs+nsp+12,it)        !/*pk4a       82*/
  100 format(9x,a6,21h all bin error check:,i6,15h tally bins had,              !/*pk4a       83*/
     & i6,6h zeros,i6,18h relative errors >,f5.2,5h, and,i6,                    !/*pk4a       84*/
     & 11h vovs > 0.1)                                                          !/*pk4a       85*/
      if(nhsd(lnhs+nsp+10,it).gt.0)nz=1                                         !/*pk4a       86*/
  110 continue                                                                  !/*pk4a       87*/
!                                                                               !/*pk4a       88*/
!        print some informational and warning messages if required.             !/*pk4a       89*/
      write(iuo,120)                                                            !/*pk4a       90*/
  120 format(//39h the 10 statistical checks are only for,                      !/*pk4a       91*/
     & 58h the tally fluctuation chart bin and do not apply to other,           !/*pk4a       92*/
     & 12h tally bins./)                                                        !/*pk4a       93*/
      if(nz.eq.1)write(iuo,130)                                                 !/*pk4a       94*/
  130 format(53h the tally bins with zeros may or may not be correct:,          !/*pk4a       95*/
     & 61h compare the source, cutoffs, multipliers, et cetera with the,        !/*pk4a       96*/
     & 12h tally bins./)                                                        !/*pk4a       97*/
      if(nf+nt.eq.0)go to 150                                                   !/*pk4a       98*/
      write(iuo,140)nf+nt,ntal*(npert+1)                                        !/*pk4b        9*/
  140 format(11h warning.  ,i4,7h of the,i4,18h tally fluctuation,              !/*pk4a      100*/
     & 51h chart bins did not pass all 10 statistical checks.)                  !/*pk4a      101*/
      call erprnt(1,2,2,nf+nt,ntal*(npert+1),0,0,-1,                            !/*pk4b       10*/
     &'i4,3h of,i4,48h tallies did not pass all 10 statistical checks.')        !/*pk4a      103*/
  150 if(n0.eq.0)go to 170                                                      !/*pk4a      104*/
      write(iuo,160)n0,ntal*(npert+1)                                           !/*pk4b       11*/
  160 format(11h warning.  ,i4,7h of the,i4,24h tallies were all zeros.)        !/*pk4a      106*/
      call erprnt(1,2,2,n0,ntal*(npert+1),0,0,-1,                               !/*pk4b       12*/
     & 'i4,3h of,i4,24h tallies were all zeros.')                               !/*pk4a      108*/
  170 if(nb.eq.0)go to 210                                                      !/*pk4a      109*/
      if(nmxf.eq.5)go to 190                                                    !/*pk4a      110*/
      write(iuo,180)nb,ntal*(npert+1)                                           !/*pk4b       13*/
  180 format(11h warning.  ,i4,7h of the,i4,22h tallies had bins with,          !/*pk4a      112*/
     & 42h relative errors greater than recommended.)                           !/*pk4a      113*/
      call erprnt(1,2,2,nb,ntal*(npert+1),0,0,-1,                               !/*pk4b       14*/
     & 'i4,3h of,i4,45h tallies had bins with large relative errors.')          !/*pk4a      115*/
      go to 210                                                                 !/*pk4a      116*/
  190 write(iuo,200)nb,ntal*(npert+1)                                           !/*pk4b       15*/
  200 format(11h warning.  ,i4,7h of the,i4,22h tallies had bins with,          !/*pk4a      118*/
     & 54h relative errors and/or vovs greater than recommended.)               !/*pk4a      119*/
      call erprnt(1,2,2,nb,ntal*(npert+1),0,0,-1,                               !/*pk4b       16*/
     & 'i4,3h of,i4,42h tallies had large vov or relative errors.')             !/*pk4a      121*/
!                                                                               !/*pk4a      122*/
!        print 3 tallies across, with page ejects where needed.                 !/*pk4a      123*/
  210 hc=' '                                                                    !/*pk4b       17*/
      do 260 ip=0,npert                                                         !/*pk4b       18*/
      k=ip*ntal                                                                 !/*pk4b       19*/
      if(ip.gt.0)write(hc,'(20h - for perturbation ,i5)')iptb(lipb+1,ip)        !/*pk4b       20*/
      do 260 jt=1,ntal,3                                                        !/*pk4b       21*/
      la=min(jt+2,ntal)                                                         !/*pk4a      125*/
      if(mod(jt/3,60/(nn+5)).eq.0)write(iuo,220)hc                              !/*pk4b       22*/
  220 format(25h1tally fluctuation charts,a25)                                  !/*pk4b       23*/
      if(mod(jt/3,60/(nn+5)).ne.0)write(iuo,'(1h )')                            !/*pk4a      128*/
      write(iuo,230)('tally',jptal(ljpt+1,i),i=jt,la)                           !/*pk4a      129*/
  230 format(/23x,3(a5,i5,32x))                                                 !/*pk4a      130*/
      write(iuo,240)('mean','error','vov','slope','fom',i=jt,la)                !/*pk4a      131*/
  240 format(7x,3hnps,3(6x,a4,5x,a5,3x,a3,2x,a5,4x,a3))                         !/*pk4b       24*/
!                                                                               !/*pk4a      133*/
!        print the npc, tfc table.                                              !/*pk4a      134*/
      do 260 l=1,nn                                                             !/*pk4a      135*/
      do 250 i=jt,la                                                            !/*pk4a      136*/
      t5(i-jt+1)=min(ten,tfc(ltfc+5,l,i+k))                                     !/*pkx        17*/
      if(t5(i-jt+1).le.1..and.t5(i-jt+1).ne.0.)t5(i-jt+1)=10.                   !/*pk4a      138*/
      t=tfc(ltfc+3,l,i+k)                                                       !/*pk4b       26*/
      if(t.le.9999999..and.t.ge.9.95)write(ha(i-jt+1),'(i8)')nint(t)            !/*pk4a      140*/
      if(t.gt.9999999..or.t.lt.9.95)write(ha(i-jt+1),'(1pe8.1)')t               !/*pk4a      141*/
  250 if(mct.lt.0)ha(i-jt+1)=' '                                                !/*pk4a      142*/
  260 write(iuo,270)npc(l),((tfc(ltfc+j,l,i+k),j=1,2),tfc(ltfc+4,l,i+k),        !/*pk4b       27*/
     & t5(i-jt+1),ha(i-jt+1),i=jt,la)                                           !/*pk4a      144*/
  270 format(i10,3(2x,1pe11.4,0p2f7.4,f5.1,a8))                                 !/*pk4b       28*/
      return                                                                    !/*pk4a      146*/
      end                                                                       !/*pk4a      147*/
