!#ident wgtul.F from deck wl                                                    !/*wl          1*/
      subroutine wgtul(wl,wu,ir,jf)                                             !/*wl          2*/
!        calculate the minimum, wl, and maximum, wu, source weight              !/*wl          3*/
!        factors for distribution ir, including all possible effects of         !/*wl          4*/
!        the s option.  set jf=1 if that can not be done.  the peculiar         !/*wl          5*/
!        structure of this subroutine is due to the fact that it is             !/*wl          6*/
!        recursive.  in effect, it calls itself.                                !/*wl          7*/
      use global_data                                                           !/*wlx         1*/
      use imcn_data                                                             !/*wlx         2*/
!                                                                               !/*wlx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*wlx         4*/
      implicit integer(kindi) (i-n)                                             !/*wlx         5*/
!                                                                               !/*wlx         6*/
      dimension ms(20),ns(20),wa(20),wb(20)                                     !/*wl         10*/
      k(i)=ksd(lksd+i,n)                                                        !/*wl         11*/
      q(i)=sqq(lsqq+i,n)                                                        !/*wl         12*/
!                                                                               !/*wl         13*/
      l=1                                                                       !/*wl         14*/
      n=ir                                                                      !/*wl         15*/
   10 wl=1.                                                                     !/*wl         16*/
      wu=1.                                                                     !/*wl         17*/
      if(n.eq.0)go to 90                                                        !/*wl         18*/
      if(k(4).ne.0)go to 30                                                     !/*wl         19*/
!                                                                               !/*wl         20*/
!        function 21 -- power law.                                              !/*wl         21*/
      if(k(2).ne.7)go to 20                                                     !/*wl         22*/
      if(q(2).eq.q(5))go to 90                                                  !/*wl         23*/
      if(q(2).eq.12345.)jf=1                                                    !/*wl         24*/
      if(q(2).eq.12345.)go to 90                                                !/*wl         25*/
      a=0.                                                                      !/*wl         26*/
      if(q(2).lt.q(5))a=huge                                                    !/*wl         27*/
      if(q(6).ne.0.)a=abs(q(6))**(q(2)-q(5))                                    !/*wl         28*/
      b=0.                                                                      !/*wl         29*/
      if(q(2).lt.q(5))b=huge                                                    !/*wl         30*/
      if(q(7).ne.0.)b=abs(q(7))**(q(2)-q(5))                                    !/*wl         31*/
      wl=wl*q(3)*min(a,b)                                                       !/*wl         32*/
      w=max(a,b)                                                                !/*wl         33*/
      if(w.eq.huge)wu=huge                                                      !/*wl         34*/
      if(wu.ne.huge)wu=wu*q(3)*w                                                !/*wl         35*/
      go to 90                                                                  !/*wl         36*/
!                                                                               !/*wl         37*/
!        function 31 -- exponential.                                            !/*wl         38*/
   20 if(k(2).ne.8)go to 90                                                     !/*wl         39*/
      if(q(2).eq.q(5))go to 90                                                  !/*wl         40*/
      a=exp((q(2)-q(5))*q(6))                                                   !/*wl         41*/
      b=exp((q(2)-q(5))*q(7))                                                   !/*wl         42*/
      wl=wl*q(3)*min(a,b)                                                       !/*wl         43*/
      wu=wu*q(3)*max(a,b)                                                       !/*wl         44*/
      go to 90                                                                  !/*wl         45*/
!                                                                               !/*wl         46*/
!        distribution table.                                                    !/*wl         47*/
   30 m=1                                                                       !/*wl         48*/
   40 w1=1.                                                                     !/*wl         49*/
      w2=1.                                                                     !/*wl         50*/
      if(k(6).eq.0)go to 60                                                     !/*wl         51*/
      if(l.gt.20)return                                                         !/*wl         52*/
      ms(l)=m                                                                   !/*wl         53*/
      ns(l)=n                                                                   !/*wl         54*/
      wa(l)=wl                                                                  !/*wl         55*/
      wb(l)=wu                                                                  !/*wl         56*/
      l=l+1                                                                     !/*wl         57*/
      n=spf(k(13)+1,m*k(21))                                                    !/*wl         58*/
      go to 10                                                                  !/*wl         59*/
   50 l=l-1                                                                     !/*wl         60*/
      w1=wl                                                                     !/*wl         61*/
      w2=wu                                                                     !/*wl         62*/
      wl=wa(l)                                                                  !/*wl         63*/
      wu=wb(l)                                                                  !/*wl         64*/
      n=ns(l)                                                                   !/*wl         65*/
      m=ms(l)                                                                   !/*wl         66*/
      if(k(7).ne.0)go to 70                                                     !/*wl         67*/
   60 w=spf(k(13)+3,m)                                                          !/*wl         68*/
      if(k(19).ne.0)w=1.-frc(lfrc+n)                                            !/*wl         69*/
      if(spf(k(13)+3,m).eq.0.)go to 80                                          !/*wl         70*/
      if(k(19)*k(18).ne.0)w=w*spf(k(13)+2,m)/spf(k(13)+3,m)                     !/*wl         71*/
      w1=w1*w                                                                   !/*wl         72*/
      if(w2.ne.huge)w2=w2*w                                                     !/*wl         73*/
   70 wl=min(wl,w1)                                                             !/*wl         74*/
      if(w2.eq.huge)wu=huge                                                     !/*wl         75*/
      if(wu.ne.huge)wu=max(wu,w2)                                               !/*wl         76*/
   80 m=m+1                                                                     !/*wl         77*/
      if(m.le.k(4))go to 40                                                     !/*wl         78*/
   90 if(l.gt.1)go to 50                                                        !/*wl         79*/
      return                                                                    !/*wl         80*/
      end                                                                       !/*wl         81*/
