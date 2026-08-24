!#ident prtfcc.F from deck pm4a                                                 !/*pm4a        1*/
      subroutine prtfcc(ac,nn)                                                  !/*pm4a        2*/
!        print the 10 tfc bin statistical check table.                          !/*pm4a        3*/
!        ac=shifted confidence interval center.                                 !/*pm4a        4*/
!        nn=number of entries in the tfc nps array npc.                         !/*pm4a        5*/
      use global_data                                                           !/*pmx         1*/
!                                                                               !/*pmx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*pmx         3*/
      implicit integer(kindi) (i-n)                                             !/*pmx         4*/
!                                                                               !/*pmx         5*/
      character ch,fd(4)*3,mc(3)*8,sr(10)*4                                     !/*pm4a        7*/
!                                                                               !/*pm4a        8*/
!        set tfc quantities for later use.                                      !/*pm4a        9*/
      it=ital+iper*ntal                                                         !/*pm4b        1*/
      t1=tfc(ltfc+1,nn,it)                                                      !/*pm4b        2*/
      t2=tfc(ltfc+2,nn,it)                                                      !/*pm4b        3*/
      t4=tfc(ltfc+4,nn,it)                                                      !/*pm4b        4*/
      t5=min(ten,tfc(ltfc+5,nn,it))                                             !/*pmx         6*/
      if(t5.le.1..and.t5.ne.0.)t5=10.                                           !/*pm4a       14*/
!                                                                               !/*pm4a       15*/
!        set the statistical check parameters to pass all of the tests.         !/*pm4a       16*/
      nw=0                                                                      !/*pm4a       17*/
      do 10 i=1,4                                                               !/*pm4a       18*/
   10 fd(i)='yes'                                                               !/*pm4a       19*/
      do 20 i=1,10                                                              !/*pm4a       20*/
   20 sr(i)=' yes'                                                              !/*pm4a       21*/
      mc(1)=' random '                                                          !/*pm4a       22*/
      mc(2)='constant'                                                          !/*pm4a       23*/
      mc(3)=' random '                                                          !/*pm4a       24*/
      rd=0.1                                                                    !/*pm4a       25*/
      if(jptal(ljpt+2,ital).eq.5)rd=.05                                         !/*pm4a       26*/
!                                                                               !/*pm4a       27*/
!        do not perform the fom checks when timing is not desired.              !/*pm4a       28*/
      if(mct.ge.0)go to 30                                                      !/*pm4a       29*/
      sr(8)='    '                                                              !/*pm4a       30*/
      sr(9)='    '                                                              !/*pm4a       31*/
      mc(2)='        '                                                          !/*pm4a       32*/
      mc(3)='        '                                                          !/*pm4a       33*/
!                                                                               !/*pm4a       34*/
!        find number of nonzero tfc means and check for constant value.         !/*pm4a       35*/
!        this test requires at least 5 nonzero estimated mean values.           !/*pm4a       36*/
   30 nm=max(1,min(nn-4,nn/2+1))                                                !/*pm4a       37*/
      do 40 i=nm,nn                                                             !/*pm4a       38*/
   40 if(tfc(ltfc+1,i,it).ne.0.)go to 50                                        !/*pm4b        6*/
   50 nb=i                                                                      !/*pm4a       40*/
      nr=nb                                                                     !/*pm4a       41*/
      if(nn-nb.le.3)go to 80                                                    !/*pm4a       42*/
      if(abs(1.-tfc(ltfc+1,nb,it)/t1).gt.1.e-7)                                 !/*pm4b        7*/
     & go to 60                                                                 !/*pm4a       44*/
      mc(1)='constant'                                                          !/*pm4a       45*/
      go to 80                                                                  !/*pm4a       46*/
!                                                                               !/*pm4a       47*/
!        check for decreasing (is=1) or increasing (is=2) mean.                 !/*pm4a       48*/
   60 is=1                                                                      !/*pm4a       49*/
      if(tfc(ltfc+1,nb+1,it).gt.tfc(ltfc+1,nb,it))is=2                          !/*pm4b        8*/
      do 70 j=nb+1,nn-1                                                         !/*pm4a       51*/
   70 if(tfc(ltfc+1,j+1,it).gt.tfc(ltfc+1,j,it).and.is.eq.1.or.                 !/*pm4b        9*/
     & tfc(ltfc+1,j+1,it).lt.tfc(ltfc+1,j,it).and.is.eq.2)go to 80              !/*pm4b       10*/
      nw=nw+1                                                                   !/*pm4a       54*/
      nhsd(lnhs+nsp+8,it)=2                                                     !/*pm4b       11*/
      sr(1)='  no'                                                              !/*pm4a       56*/
      mc(1)='decrease'                                                          !/*pm4a       57*/
      if(is.eq.2)mc(1)='increase'                                               !/*pm4a       58*/
!                                                                               !/*pm4a       59*/
!        check for an acceptable value of the relative error in tfc bin.        !/*pm4a       60*/
   80 if(t2.eq.0.)go to 140                                                     !/*pm4a       61*/
      if(t2.lt.rd)go to 90                                                      !/*pm4a       62*/
      nw=nw+1                                                                   !/*pm4a       63*/
      sr(2)='  no'                                                              !/*pm4a       64*/
      nhsd(lnhs+nsp+8,it)=3                                                     !/*pm4b       12*/
      if(jptal(ljpt+2,ital).eq.5)nhsd(lnhs+nsp+8,it)=4                          !/*pm4b       13*/
!                                                                               !/*pm4a       67*/
!        check for a decreasing relative error for last half of problem.        !/*pm4a       68*/
   90 do 100 i=nb+1,nn                                                          !/*pm4a       69*/
      if(tfc(ltfc+2,i-1,it).eq.0.)nr=nr+1                                       !/*pm4b       14*/
      if(tfc(ltfc+2,i,it).le.tfc(ltfc+2,i-1,it))go to 100                       !/*pm4b       15*/
      t=sqrt(float(npc(i-1)-nskk)/float(npc(i)-nskk))                           !/*pm4c        1*/
      if(tfc(ltfc+2,i,it).gt.tfc(ltfc+2,i-1,it)*t*max(1.1+zero,1.+              !/*pm4c        5*/
     & sqrt(tfc(ltfc+4,i-1,it))+2.5*tfc(ltfc+4,i-1,it)))go to 110               !/*pm4c        6*/
  100 continue                                                                  !/*pm4a       75*/
      go to 120                                                                 !/*pm4a       76*/
  110 nw=nw+1                                                                   !/*pm4a       77*/
      if(nw.eq.1)nhsd(lnhs+nsp+8,it)=5                                          !/*pm4b       18*/
      sr(3)='  no'                                                              !/*pm4a       79*/
      fd(1)=' no'                                                               !/*pm4a       80*/
      go to 130                                                                 !/*pm4a       81*/
!                                                                               !/*pm4a       82*/
!        check 1/sqrt(nps) rel error for max(5,last half of problem).           !/*pm4a       83*/
  120 t=sqrt(float(npc(nr)-nskk)/(nps-nskk))                                    !/*pm4c        7*/
      r=t*tfc(ltfc+2,nr,it)/t2                                                  !/*pm4c       11*/
      t7=max(tfc(ltfc+4,nr,it),t4)                                              !/*pm4b       20*/
      if(max(r,1./r).le.max(1.05+zero,sqrt(1.+5.*sqrt(t7)+12.5*t7)))            !/*pm4a       86*/
     & go to 140                                                                !/*pm4a       87*/
  130 nw=nw+1                                                                   !/*pm4a       88*/
      if(nw.eq.1)nhsd(lnhs+nsp+8,it)=6                                          !/*pm4b       21*/
      sr(4)='  no'                                                              !/*pm4a       90*/
      fd(2)=' no'                                                               !/*pm4a       91*/
!                                                                               !/*pm4a       92*/
!        check for an acceptable variance of the variance in tfc bin.           !/*pm4a       93*/
  140 if(t4.eq.0.)go to 200                                                     !/*pm4a       94*/
      if(t4.lt.0.1)go to 150                                                    !/*pm4a       95*/
      nw=nw+1                                                                   !/*pm4a       96*/
      if(nw.eq.1)nhsd(lnhs+nsp+8,it)=7                                          !/*pm4b       22*/
      sr(5)='  no'                                                              !/*pm4a       98*/
!                                                                               !/*pm4a       99*/
!        check for a decreasing var of the var for last half of problem.        !/*pm4a      100*/
  150 nr=nb                                                                     !/*pm4a      101*/
      do 160 i=nb+1,nn                                                          !/*pm4a      102*/
      if(tfc(ltfc+4,i-1,it).eq.0.)nr=nr+1                                       !/*pm4b       23*/
  160 if(tfc(ltfc+4,i,it).gt.2.*tfc(ltfc+4,i-1,it))go to 170                    !/*pm4b       24*/
      go to 180                                                                 !/*pm4a      105*/
  170 nw=nw+1                                                                   !/*pm4a      106*/
      if(nw.eq.1)nhsd(lnhs+nsp+8,it)=8                                          !/*pm4b       25*/
      sr(6)='  no'                                                              !/*pm4a      108*/
      fd(3)=' no'                                                               !/*pm4a      109*/
      go to 190                                                                 !/*pm4a      110*/
!                                                                               !/*pm4a      111*/
!        check for a linearly decreasing variance of the variance.              !/*pm4a      112*/
  180 t=float(npc(nr)-nskk)/float(nps-nskk)                                     !/*pm4c       12*/
      r=t*tfc(ltfc+4,nr,it)/t4                                                  !/*pm4c       15*/
      if(max(r,1./r).lt.1.5)go to 200                                           !/*pm4a      114*/
  190 nw=nw+1                                                                   !/*pm4a      115*/
      if(nw.eq.1)nhsd(lnhs+nsp+8,it)=9                                          !/*pm4b       27*/
      sr(7)='  no'                                                              !/*pm4a      117*/
      fd(4)=' no'                                                               !/*pm4a      118*/
!                                                                               !/*pm4a      119*/
!        check for a statistically constant figure of merit.                    !/*pm4a      120*/
  200 if(t2.eq.0..or.mct.lt.0)go to 230                                         !/*pm4a      121*/
      r=tfc(ltfc+3,nb,it)/tfc(ltfc+3,nn,it)                                     !/*pm4b       28*/
      t7=max(tfc(ltfc+4,nb,it),t4)                                              !/*pm4b       29*/
      if(max(r,1./r).lt.max(1.02+zero,min(1.5+zero,                             !/*pm4a      124*/
     & 1.+5.*sqrt(t7)+12.5*t7)))go to 210                                       !/*pm4a      125*/
      nw=nw+1                                                                   !/*pm4a      126*/
      if(nw.eq.1)nhsd(lnhs+nsp+8,it)=10                                         !/*pm4b       30*/
      sr(8)='  no'                                                              !/*pm4a      128*/
      mc(2)='decrease'                                                          !/*pm4a      129*/
      if(r.lt.1.)mc(2)='increase'                                               !/*pm4a      130*/
!                                                                               !/*pm4a      131*/
!        check for decreasing(is=1) or increasing(is=2) figure of merit.        !/*pm4a      132*/
  210 if(nn-nb.le.3)go to 230                                                   !/*pm4a      133*/
      is=1                                                                      !/*pm4a      134*/
      if(tfc(ltfc+3,nb+1,it).gt.tfc(ltfc+3,nb,it))is=2                          !/*pm4b       31*/
      do 220 j=nb+1,nn-1                                                        !/*pm4a      136*/
  220 if(tfc(ltfc+3,j+1,it).gt.tfc(ltfc+3,j,it).and.is.eq.1.or.                 !/*pm4b       32*/
     & tfc(ltfc+3,j+1,it).lt.tfc(ltfc+3,j,it).and.is.eq.2)go to 230             !/*pm4b       33*/
      nw=nw+1                                                                   !/*pm4a      139*/
      if(nw.eq.1)nhsd(lnhs+nsp+8,it)=11                                         !/*pm4b       34*/
      sr(9)='  no'                                                              !/*pm4a      141*/
      mc(3)='decrease'                                                          !/*pm4a      142*/
      if(is.eq.2)mc(3)='increase'                                               !/*pm4a      143*/
!                                                                               !/*pm4a      144*/
!        check for a steep enough slope in the extreme history tallies.         !/*pm4a      145*/
  230 if(t5.ge.3.)go to 240                                                     !/*pm4a      146*/
      nw=nw+1                                                                   !/*pm4a      147*/
      sr(10)='  no'                                                             !/*pm4a      148*/
      if(nw.gt.1)go to 240                                                      !/*pm4a      149*/
      if(t5.ne.0.)nhsd(lnhs+nsp+8,it)=12                                        !/*pm4b       35*/
      if(t5.eq.0..and.nhsd(lnhs+nsp+5,it).eq.0)                                 !/*pm4b       36*/
     & nhsd(lnhs+nsp+8,it)=13                                                   !/*pm4b       37*/
      if(t5.eq.0..and.nhsd(lnhs+nsp+5,it).gt.0)                                 !/*pm4b       38*/
     & nhsd(lnhs+nsp+8,it)=14                                                   !/*pm4b       39*/
!                                                                               !/*pm4a      155*/
!        write the table for the 10 statistical checks.                         !/*pm4a      156*/
  240 nhsd(lnhs+nsp+7,it)=nw                                                    !/*pm4b       40*/
      if(nw.eq.0)nhsd(lnhs+nsp+8,it)=15                                         !/*pm4b       41*/
      if(ink(160).eq.0)return                                                   !/*pm4a      159*/
      if(nn-nb.le.3)write(iuo,250)nn-nb+1                                       !/*pm4a      160*/
  250 format(//50h ***** the nps-dependent tfc bin check results are,           !/*pm4a      161*/
     & 31h suspect because there are only,i3,20h nps tally values to,           !/*pm4a      162*/
     & 14h analyze *****)                                                       !/*pm4a      163*/
      ch='='                                                                    !/*pm4a      164*/
      write(iuo,260)(ch,j=1,131),jptal(ljpt+1,ital),rd,mc(1),t2,fd(1),          !/*pm4a      165*/
     & fd(2),t4,fd(3),fd(4),mc(2),mc(3),t5,(sr(i),i=1,10),(ch,j=1,131)          !/*pm4a      166*/
  260 format(//1x,131a1//10x,14h results of 10,                                 !/*pm4a      167*/
     & 58h statistical checks for the estimated answer for the tally,           !/*pm4a      168*/
     & 37h fluctuation chart (tfc) bin of tally,i4//8h tfc bin,5x,              !/*pm4a      169*/
     & 8h--mean--,6x,32h---------relative error---------,6x,                    !/*pm4a      170*/
     & 32h----variance of the variance----,6x,19h--figure of merit--,           !/*pm4a      171*/
     & 5x,5h-pdf-,/9h behavior,3x,9h behavior,5x,6h value,2x,                   !/*pm4a      172*/
     & 9h decrease,2x,14h decrease rate,5x,6h value,2x,9h decrease,2x,          !/*pm4a      173*/
     & 14h decrease rate,6x,6h value,4x,9h behavior,4x,6h slope//               !/*pm4a      174*/
     & 8h desired,5x,7h random,7x,1h<,f4.2,5x,4h yes,5x,12h 1/sqrt(nps),        !/*pm4a      175*/
     & 6x,6h <0.10,5x,4h yes,7x,6h 1/nps,10x,9h constant,3x,7h random,          !/*pm4a      176*/
     & 6x,5h>3.00/9h observed,4x,a8,6x,f5.2,6x,a3,10x,a3,11x,f5.2,6x,a3,        !/*pm4a      177*/
     & 9x,a3,12x,a8,3x,a8,5x,f5.2/8h passed?,7x,a4,9x,a4,5x,a4,9x,a4,           !/*pm4a      178*/
     & 12x,a4,5x,a4,8x,a4,14x,a4,7x,a4,8x,a4//1x,131a1//)                       !/*pm4b       42*/
!                                                                               !/*pm4a      180*/
!        print a warning if any tfc bin statistical check is not passed.        !/*pm4a      181*/
      if(nw.eq.0)go to 280                                                      !/*pm4a      182*/
      write(iuo,270)nw                                                          !/*pm4a      183*/
  270 format(51h warning.  the tally in the tally fluctuation chart,            !/*pm4a      184*/
     & 17h bin did not pass,i3,30h of the 10 statistical checks.)               !/*pm4a      185*/
      call erprnt(1,2,2,jptal(ljpt+1,ital),nw,0,0,-1,'5htally,i4,'//            !/*pm4a      186*/
     & '21h tfc bin did not pass,i3,26h of 10 statistical checks.')             !/*pm4a      187*/
      return                                                                    !/*pm4a      188*/
!                                                                               !/*pm4a      189*/
!        the tally in the tfc bin passed all 10 statistical checks.             !/*pm4a      190*/
!        print asymmetric and symmetric confidence intervals.                   !/*pm4a      191*/
  280 write(iuo,290)max(zero,ac*(1.-t2)),ac*(1.+t2),max(ac*(1.-2.*t2),          !/*pm4a      192*/
     & zero),ac*(1.+2.*t2),max(zero,ac*(1.-3.*t2)),ac*(1.+3.*t2),               !/*pm4a      193*/
     & max(zero,t1*(1.-t2)),t1*(1.+t2),max(zero,t1*(1.-2.*t2)),                 !/*pm4a      194*/
     & t1*(1.+2.*t2),max(zero,t1*(1.-3.*t2)),t1*(1.+3.*t2)                      !/*pm4a      195*/
  290 format(//47h this tally meets the statistical criteria used,              !/*pm4a      196*/
     & 46h to form confidence intervals: check the tally,                       !/*pm4a      197*/
     & 29h fluctuation chart to verify./26h the results in other bins,          !/*pm4a      198*/
     & 58h associated with this tally may not meet these statistical,           !/*pm4a      199*/
     & 10h criteria.//41h estimated asymmetric confidence interval,             !/*pm4b       43*/
     & 14h(1,2,3 sigma):,1pe11.4,3h to,e11.4,1h;,e11.4,3h to,e11.4,             !/*pm4a      201*/
     & 1h;,e11.4,3h to,e11.4/32h estimated  symmetric confidence,               !/*pm4a      202*/
     & 23h interval(1,2,3 sigma):,1pe11.4,3h to,e11.4,1h;,e11.4,3h to,          !/*pm4b       44*/
     & e11.4,1h;,e11.4,3h to,e11.4)                                             !/*pm4a      204*/
      return                                                                    !/*pm4a      205*/
      end                                                                       !/*pm4a      206*/
