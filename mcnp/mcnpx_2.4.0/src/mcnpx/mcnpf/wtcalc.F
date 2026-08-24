!#ident wtcalc.F from deck wc                                                   !/*wc          1*/
      subroutine wtcalc                                                         !/*wc          2*/
!        calculate and print the minimum and maximum source weights.            !/*wc          3*/
      use global_data                                                           !/*wcx         1*/
      use imcn_data                                                             !/*wcx         2*/
!                                                                               !/*wcx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*wcx         4*/
      implicit integer(kindi) (i-n)                                             !/*wcx         5*/
!                                                                               !/*wcx         6*/
      k(i,j)=ksd(lksd+i,j)                                                      !/*wc          6*/
!                                                                               !/*wc          7*/
      jf=0                                                                      !/*wc          8*/
      w1=1.                                                                     !/*wc          9*/
      w2=1.                                                                     !/*wc         10*/
!                                                                               !/*wc         11*/
!        do all variables with independent distributions.                       !/*wc         12*/
      do 140 iv=1,maxv                                                          !/*wcx         7*/
      if(ivdis(iv).eq.0.or.ivdd(iv).ne.iv)go to 140                             !/*wc         14*/
      do 10 jv=1,maxv                                                           !/*wcx         8*/
      if(jv.eq.iv.or.ivdd(jv).ne.iv)go to 10                                    !/*wc         16*/
      call wgtul(wl,wu,ivdis(jv),jf)                                            !/*wc         17*/
      if(wl.ne.wu)go to 20                                                      !/*wc         18*/
   10 continue                                                                  !/*wc         19*/
!                                                                               !/*wc         20*/
!        case of no biased dependent variables.                                 !/*wc         21*/
      call wgtul(wl,wu,ivdis(iv),jf)                                            !/*wc         22*/
      w1=w1*wl                                                                  !/*wc         23*/
      if(wu.eq.huge)w2=huge                                                     !/*wc         24*/
      if(w2.ne.huge)w2=w2*wu                                                    !/*wc         25*/
      go to 140                                                                 !/*wc         26*/
!                                                                               !/*wc         27*/
!        case of biased dependent variables.                                    !/*wc         28*/
   20 ji=ivdis(iv)                                                              !/*wc         29*/
      if(k(2,ji).ne.0)go to 80                                                  !/*wc         30*/
      do 30 jv=1,maxv                                                           !/*wcx         9*/
      if(jv.eq.iv.or.ivdd(jv).ne.iv)go to 30                                    !/*wc         32*/
      if(k(8,ivdis(jv)).ne.0)go to 80                                           !/*wc         33*/
   30 continue                                                                  !/*wc         34*/
!                                                                               !/*wc         35*/
!        case of no dependent variables with q option.                          !/*wc         36*/
      wa=1.                                                                     !/*wc         37*/
      wb=1.                                                                     !/*wc         38*/
      do 70 i=1,k(4,ji)-1+k(5,ji)                                               !/*wc         39*/
      wp=1.                                                                     !/*wc         40*/
      wq=1.                                                                     !/*wc         41*/
      if(k(18,ji).eq.0)go to 50                                                 !/*wc         42*/
      if(k(19,ji).ne.0)go to 40                                                 !/*wc         43*/
      wp=spf(k(13,ji)+3,i+1-k(5,ji))                                            !/*wc         44*/
      wq=wp                                                                     !/*wc         45*/
      go to 50                                                                  !/*wc         46*/
   40 a=spf(k(13,ji)+2,i)/spf(k(13,ji)+3,i)                                     !/*wc         47*/
      b=spf(k(13,ji)+2,i+1)/spf(k(13,ji)+3,i+1)                                 !/*wc         48*/
      wp=min(a,b)                                                               !/*wc         49*/
      wq=max(a,b)                                                               !/*wc         50*/
   50 do 60 jv=1,maxv                                                           !/*wcx        10*/
      if(jv.eq.iv.or.ivdd(jv).ne.iv)go to 60                                    !/*wc         52*/
      jd=ivdis(jv)                                                              !/*wc         53*/
      if(k(6,jd).eq.0)go to 60                                                  !/*wc         54*/
      call wgtul(wl,wu,int(spf(k(13,jd)+1,i)),jf)                               !/*wc         55*/
      wp=wp*wl                                                                  !/*wc         56*/
      if(wu.eq.huge)wq=huge                                                     !/*wc         57*/
      if(wq.ne.huge)wq=wq*wu                                                    !/*wc         58*/
   60 continue                                                                  !/*wc         59*/
      wa=min(wa,wp)                                                             !/*wc         60*/
   70 wb=max(wb,wq)                                                             !/*wc         61*/
      w1=w1*wa                                                                  !/*wc         62*/
      if(wb.eq.huge)w2=huge                                                     !/*wc         63*/
      if(w2.ne.huge)w2=w2*wb                                                    !/*wc         64*/
      go to 140                                                                 !/*wc         65*/
!                                                                               !/*wc         66*/
!        case of dependent variables with q option.                             !/*wc         67*/
   80 if(k(18,ji).ne.0)go to 130                                                !/*wc         68*/
      m=0                                                                       !/*wc         69*/
      do 100 jv=1,maxv                                                          !/*wcx        11*/
      if(jv.eq.iv.or.ivdd(jv).ne.iv)go to 100                                   !/*wc         71*/
      jd=ivdis(jv)                                                              !/*wc         72*/
      call wgtul(wl,wu,ivdis(jv),jf)                                            !/*wc         73*/
      if(wl.eq.wu)go to 100                                                     !/*wc         74*/
      if(m.eq.0)m=jd                                                            !/*wc         75*/
      if(k(8,jd).eq.0.or.jd.eq.m)go to 100                                      !/*wc         76*/
      do 90 i=1,k(4,jd)*2,2                                                     !/*wc         77*/
   90 if(spf(k(13,jd)+1,i).ne.spf(k(13,m)+1,i))go to 130                        !/*wc         78*/
  100 continue                                                                  !/*wc         79*/
      wa=1.                                                                     !/*wc         80*/
      wb=1.                                                                     !/*wc         81*/
      do 120 i=2,k(4,m)*2,2                                                     !/*wc         82*/
      wp=1.                                                                     !/*wc         83*/
      wq=1.                                                                     !/*wc         84*/
      do 110 jv=1,maxv                                                          !/*wcx        12*/
      if(jv.eq.iv.or.ivdd(jv).ne.iv)go to 110                                   !/*wc         86*/
      jd=ivdis(jv)                                                              !/*wc         87*/
      call wgtul(wl,wu,jd,jf)                                                   !/*wc         88*/
      if(wu.eq.wl.or.k(8,jd).eq.0)go to 110                                     !/*wc         89*/
      call wgtul(wl,wu,int(spf(k(13,jd)+1,i)),jf)                               !/*wc         90*/
      wp=wp*wl                                                                  !/*wc         91*/
      if(wu.eq.huge)wq=huge                                                     !/*wc         92*/
      if(wq.ne.huge)wq=wq*wu                                                    !/*wc         93*/
  110 continue                                                                  !/*wc         94*/
      wa=min(wa,wp)                                                             !/*wc         95*/
  120 wb=max(wb,wq)                                                             !/*wc         96*/
      w1=w1*wa                                                                  !/*wc         97*/
      if(wb.eq.huge)w2=huge                                                     !/*wc         98*/
      if(w2.ne.huge)w2=w2*wb                                                    !/*wc         99*/
      go to 140                                                                 !/*wc        100*/
  130 jf=1                                                                      !/*wc        101*/
  140 continue                                                                  !/*wc        102*/
!                                                                               !/*wc        103*/
!        set the minimum source weight for weight cutoff.                       !/*wc        104*/
      if(swtm.gt.0.)swtm=swtm*srv(1,17)                                         !/*wc        105*/
      if(swtm.eq.0.)swtm=w1*srv(1,17)                                           !/*wc        106*/
      if(swtm.eq.-1.)swtm=0.                                                    !/*wc        107*/
!                                                                               !/*wc        108*/
!        print the minimum and maximum source weights.                          !/*wc        109*/
      w=huge                                                                    !/*wc        110*/
      if(w2.lt..01*huge)w=w2*srv(1,17)                                          !/*wc        111*/
      write(iuo,150)w1*srv(1,17),w                                              !/*wc        112*/
  150 format(/4x,23hminimum source weight =,1pe11.4,                            !/*wc        113*/
     & 4x,23hmaximum source weight =,1pe11.4)                                   !/*wc        114*/
      if(jf.ne.0)write(iuo,160)                                                 !/*wc        115*/
  160 format(55h these limits may not be correct because the effects of,        !/*wc        116*/
     & 49h some complicated dependencies were not included.)                    !/*wc        117*/
      return                                                                    !/*wc        118*/
      end                                                                       !/*wc        119*/
