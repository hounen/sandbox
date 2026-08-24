!#ident prplot.F from deck kr4a                                                 !/*kr4a        1*/
      subroutine prplot(np)                                                     !/*kr4a        2*/
!        create a printed plot of keff results by cycle up to 122 lines.        !/*kr4a        3*/
!        called twice from subroutine kprint with different np values.          !/*kr4a        4*/
!                                                                               !/*kr4a        5*/
!        hc is the character information for each printed plot line.            !/*kr4a        6*/
!        hd is the dashed line with tick marks at the label locations.          !/*kr4a        7*/
!        hn contains numerical labels for the plot as characters.               !/*kr4a        8*/
!        ll is the location of the tick marks on the horizontal axis.           !/*kr4a        9*/
!        lp is the number of lines allowed in a plot.                           !/*kr4a       10*/
!        nl(i) is max number of labels allowed for i significant digits.        !/*kr4a       11*/
!        np is plot type: np=1, keff avgs; np=2, keff by cycles skipped,        !/*kr4c        1*/
!        vk saves upper, lower, and center confidence interval values.          !/*kr4c        4*/
!        yv is the axis label values for the printed plot.                      !/*kr4a       14*/
!                                                                               !/*kr4a       15*/
      use global_data                                                           !/*krx         3*/
!                                                                               !/*krx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*krx         5*/
      implicit integer(kindi) (i-n)                                             !/*krx         6*/
!                                                                               !/*krx         7*/
      parameter (lp=122)                                                        !/*kr4a       17*/
      character ha*6,hc(102),hd(102),hk,hn*105,hs                               !/*kr4c        5*/
      dimension ll(15),nl(4),vk(3,lp),yv(102)                                   !/*kr4c        6*/
      data nl/16,12,9,7/                                                        !/*kr4a       20*/
!                                                                               !/*kr4a       21*/
!        allow only lp lines to be plotted to keep plots to 2 pages.            !/*kr4a       22*/
      ln=0                                                                      !/*kr4a       23*/
      mk=min(kcz,mrkp)                                                          !/*kr4a       24*/
      ff=1.                                                                     !/*kr4c        7*/
      hk='k'                                                                    !/*kr4c        8*/
      if(np.gt.2)hk='a'                                                         !/*kr4c        9*/
      ik=ikz+5                                                                  !/*kr4c       10*/
      ir=12                                                                     !/*kr4c       11*/
      iz=ikz                                                                    !/*kr4c       12*/
      if(np.eq.2.or.np.eq.4)go to 20                                            !/*kr4c       16*/
!                                                                               !/*kr4a       26*/
!        load vk with avg keff plot: vk(1,i),vk(2,i)=lower, upper limit.        !/*kr4a       27*/
!        start plot with active cycle 6 so 68% multiplier is about one,         !/*kr4a       28*/
!        which gives the col/abs/tl keff estimator 3 degrees of freedom.        !/*kr4a       29*/
      if(np.eq.1)ms=(mk-ikz-6)/lp+1                                             !/*kr4c       17*/
      do 10 i=ik+ms,mk,ms                                                       !/*kr4c       20*/
      ln=ln+1                                                                   !/*kr4a       32*/
      vk(3,ln)=rkpl(lrkp+ir,i)                                                  !/*kr4c       21*/
      if(ir.eq.22)ff=abs(vk(3,ln))                                              !/*kr4c       22*/
      vk(1,ln)=max(zero,vk(3,ln)-ff*rkpl(lrkp+ir+1,i))                          !/*kr4c       23*/
      if(np.eq.3)vk(1,ln)=vk(3,ln)-ff*rkpl(lrkp+ir+1,i)                         !/*kr4c       24*/
   10 vk(2,ln)=vk(3,ln)+ff*rkpl(lrkp+ir+1,i)                                    !/*kr4c       25*/
!                                                                               !/*kr4a       35*/
!        make sure last plotted interval is final combined keff result.         !/*kr4a       36*/
      if(ik+ln*ms.eq.kcz)go to 40                                               !/*kr4c       26*/
      ln=ln+1                                                                   !/*kr4a       38*/
      vk(3,ln)=rkpl(lrkp+ir,mk)                                                 !/*kr4c       27*/
      if(np.eq.3)ff=abs(vk(3,ln))                                               !/*kr4c       28*/
      vk(1,ln)=max(zero,vk(3,ln)-ff*rkpl(lrkp+ir+1,mk))                         !/*kr4c       29*/
      if(np.eq.3)vk(1,ln)=vk(3,ln)-ff*rkpl(lrkp+ir+1,mk)                        !/*kr4c       30*/
      vk(2,ln)=vk(3,ln)+ff*rkpl(lrkp+ir+1,mk)                                   !/*kr4c       31*/
      go to 40                                                                  !/*kr4a       41*/
!                                                                               !/*kr4a       42*/
!        load vk with active cycle dependent keff confidence intervals.         !/*kr4a       43*/
   20 ms=min(kcz-9,mrkp)/lp+1                                                   !/*kr4a       44*/
      il=10                                                                     !/*kr4c       32*/
      if(np.ne.2)go to 30                                                       !/*kr4c       33*/
      do 25 i=mk,il,-ms                                                         !/*kr4c       34*/
      ln=ln+1                                                                   !/*kr4c       35*/
      vk(3,ln)=rkpl(lrkp+14,mk-i+1)                                             !/*kr4c       36*/
      vk(1,ln)=max(zero,vk(3,ln)-rkpl(lrkp+15,mk-i+1))                          !/*kr4c       37*/
   25 vk(2,ln)=vk(3,ln)+rkpl(lrkp+15,mk-i+1)                                    !/*kr4c       38*/
      go to 40                                                                  !/*kr4c       39*/
   30 continue                                                                  !/*krx        14*/
!                                                                               !/*kr4a       49*/
!        find smallest,largest confidence interval values to be plotted.        !/*kr4a       50*/
   40 ss=vk(1,1)                                                                !/*kr4a       51*/
      sl=vk(2,1)                                                                !/*kr4a       52*/
      do 50 i=2,ln                                                              !/*kr4a       53*/
      if(vk(1,i).lt.ss)ss=vk(1,i)                                               !/*kr4a       54*/
   50 if(vk(2,i).gt.sl)sl=vk(2,i)                                               !/*kr4a       55*/
!                                                                               !/*kr4a       56*/
!        set the final keff value, rk.                                          !/*krx        18*/
      rk=rkpl(lrkp+ir,mk)                                                       !/*kr4c       47*/
!                                                                               !/*kr4a       59*/
!        find significant decimal places nc and limits yf,yl for y-axis.        !/*kr4a       60*/
      dl=sl-ss                                                                  !/*kr4a       61*/
      tn=1.                                                                     !/*kr4a       62*/
      do 60 nc=1,10                                                             !/*kr4a       63*/
      tn=10.*tn                                                                 !/*kr4a       64*/
   60 if(dl*tn.gt.1.)go to 70                                                   !/*kr4a       65*/
   70 ti=1./tn                                                                  !/*kr4a       66*/
      nc=min(nc,4)                                                              !/*kr4a       67*/
      if(ss.ge.10.)nc=min(nc+int(log10(ss)),4)                                  !/*kr4c       48*/
      yf=ti*int(ss*tn)                                                          !/*kr4a       68*/
      if(ss.lt.0.)yf=ti*(int(ss*tn)-1.)                                         !/*kr4c       49*/
      yl=ti*(int(sl*tn)+1.)                                                     !/*kr4a       69*/
      yd=(yl-yf)*0.01                                                           !/*kr4a       70*/
      hc(1)='|'                                                                 !/*kr4a       71*/
!                                                                               !/*kr4a       72*/
!        construct the y-value yv grid to plot the label values.                !/*kr4a       73*/
!        yf is the first value and ye is the end value.                         !/*kr4a       74*/
      ye=yf+102.*yd                                                             !/*kr4a       75*/
      yv(1)=yf                                                                  !/*kr4a       76*/
      ts=ti                                                                     !/*kr4a       77*/
!                                                                               !/*kr4a       78*/
!        keep number of label values nv within nl limits for nc places.         !/*kr4a       79*/
   80 do 90 i=2,102                                                             !/*kr4a       80*/
      yv(i)=yv(i-1)+ts                                                          !/*kr4a       81*/
   90 if(yv(i).ge.ye)go to 100                                                  !/*kr4a       82*/
  100 if(i.le.nl(nc))go to 110                                                  !/*kr4a       83*/
      ts=ts+ti                                                                  !/*kr4a       84*/
      go to 80                                                                  !/*kr4a       85*/
  110 nv=i-1                                                                    !/*kr4a       86*/
!                                                                               !/*kr4a       87*/
!        print the titles for the plot.                                         !/*kr4a       88*/
      if(np.eq.2)go to 140                                                      !/*kr4a       89*/
      write(iuo,120)rk                                                          !/*kr4a       90*/
  120 format(52h1plot of the estimated col/abs/track-length keff one,           !/*kr4a       91*/
     & 48h standard deviation interval versus cycle number,                     !/*kr4a       92*/
     & 18h (| = final keff =,f8.5,1h)/)                                         !/*kr4a       93*/
      if(kcz.gt.mrkp)write(iuo,130)kcz,mrkp                                     !/*kr4a       94*/
  130 format(35h this plot is truncated because the,i5,12h keff cycles,         !/*kr4a       95*/
     & 11h the mrkp =,i5,15h cycle storage./)                                   !/*kr4a       96*/
      go to 160                                                                 !/*kr4a       97*/
  140 write(iuo,150)rk                                                          !/*kr4a       98*/
  150 format(52h1plot of the estimated col/abs/track-length keff one,           !/*kr4a       99*/
     & 51h standard deviation interval by active cycle number,                  !/*kr4a      100*/
     & 18h (| = final keff =,f8.5,1h)/)                                         !/*kr4a      101*/
      go to 160                                                                 !/*kr4c       52*/
!                                                                               !/*kr4a      102*/
!        define the axis line hd with tick marks for the printed plot.          !/*kr4a      103*/
  160 hd(1)='|'                                                                 !/*kr4a      104*/
      do 170 i=2,102                                                            !/*kr4a      105*/
  170 hd(i)='-'                                                                 !/*kr4a      106*/
      ll(1)=1                                                                   !/*kr4a      107*/
      do 180 i=2,nv                                                             !/*kr4a      108*/
      ll(i)=(yv(i)-yv(1))/yd                                                    !/*kr4a      109*/
  180 hd(ll(i))='|'                                                             !/*kr4a      110*/
!                                                                               !/*kr4a      111*/
!        create the y-axis label scale in hn and print scale and axis.          !/*kr4a      112*/
      do 190 i=1,105                                                            !/*kr4a      113*/
  190 hn(i:i)=' '                                                               !/*kr4a      114*/
      do 200 i=1,nv                                                             !/*kr4a      115*/
      write(ha,'(f6.4)')yv(i)                                                   !/*kr4a      116*/
      if(yv(i).ge.10.)write(ha,'(f6.3)')yv(i)                                   !/*kr4c       61*/
      if(yv(i).ge.100.)write(ha,'(f6.2)')yv(i)                                  !/*kr4c       62*/
  200 hn(ll(i):ll(i)+nc+2)=ha(1:nc+2)                                           !/*kr4a      117*/
      if(np.eq.1.or.np.eq.3)write(iuo,210)hn,(hd(i),i=1,ll(nv))                 !/*kr4c       63*/
  210 format(19h    cycle   active ,a105/20h   number   cycles  ,102a1)         !/*kr4a      119*/
      if(np.eq.2.or.np.eq.4)write(iuo,220)hn,(hd(i),i=1,ll(nv))                 !/*kr4c       64*/
  220 format(19h inactive   active ,a105/20h   cycles   cycles  ,102a1)         !/*kr4a      121*/
!                                                                               !/*kr4a      122*/
!        set the location in hc of the final combined keff with a "|".          !/*kr4a      123*/
      lm=(rk-yv(1))/yd                                                          !/*kr4a      124*/
!                                                                               !/*kr4a      125*/
!        set the locations of the characters (,),-,+,|,*, and k in hc.          !/*kr4a      126*/
      do 250 i=1,ln                                                             !/*kr4a      127*/
      do 230 j=1,102                                                            !/*kr4a      128*/
  230 hc(j)=' '                                                                 !/*kr4a      129*/
      if(rk.ne.0.)hc(lm)='|'                                                    !/*kr4a      130*/
      l1=max(1,int((vk(1,i)-yv(1))/yd))                                         !/*kr4c       65*/
      l2=(vk(2,i)-yv(1))/yd                                                     !/*kr4a      132*/
      if(l1.ne.lm)hc(l1)='('                                                    !/*kr4a      133*/
      if(l2.ne.lm)hc(l2)=')'                                                    !/*kr4a      134*/
      do 240 j=l1+1,l2-1                                                        !/*kr4a      135*/
  240 if(j.ne.lm)hc(j)='-'                                                      !/*kr4a      136*/
      hc(int((vk(3,i)-yv(1))/yd))=hk                                            !/*kr4c       66*/
      m=ik+i*ms                                                                 !/*kr4c       67*/
      if((np.eq.1.or.np.eq.3).and.i.eq.ln)m=kcz                                 !/*kr4c       68*/
      if(np.eq.2)m=mk-(i-1)*ms                                                  !/*kr4c       69*/
      hs='|'                                                                    !/*kr4a      141*/
      if(mod(i,10).eq.0)hs='+'                                                  !/*kr4a      142*/
      if(np.eq.2.and.kcz-m.eq.ikz)hs='*'                                        !/*kr4a      143*/
!                                                                               !/*kr4a      144*/
!        print the ith line of the printed plot of type np.                     !/*kr4a      145*/
      if(np.eq.1.or.np.eq.3)                                                    !/*kr4c       72*/
     & write(iuo,260)m,m-iz,hs,(hc(j),j=2,ll(nv)-1),hs                          !/*kr4c       73*/
  250 if(np.eq.2.or.np.eq.4)                                                    !/*kr4c       74*/
     & write(iuo,260)kcz-m,m,hs,(hc(j),j=2,ll(nv)-1),hs                         !/*kr4c       75*/
  260 format(2i9,2x,a1,103a1)                                                   !/*kr4a      148*/
!                                                                               !/*kr4a      149*/
!        print the bottom two label lines of the printed plot.                  !/*kr4a      150*/
      write(iuo,270)(hd(i),i=1,ll(nv))                                          !/*kr4a      151*/
  270 format(20x,102a1)                                                         !/*kr4a      152*/
      write(iuo,280)hn                                                          !/*kr4a      153*/
  280 format(19x,a105)                                                          !/*kr4a      154*/
      return                                                                    !/*kr4a      155*/
      end                                                                       !/*kr4a      156*/
