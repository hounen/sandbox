!#ident pltadd.F from deck pad4c2                                               !/*pad4c2      1*/
      subroutine pltadd(p,iq,js,ss,tt,ii,la,lp,nc)                              !/*pad4c2      3*/
#ifdef PLOT                                                                     /*!padx        1*/
!        add points ss,tt to the plot list, zst.                                !/*pad4c2      4*/
!            p = parametric curve (coe array) coordinate (input).               !/*pad4c2      5*/
!           iq = 1/2 = sheet of parametric curve (input).                       !/*pad4c2      6*/
!           js = surface number of curve to be plotted (input).                 !/*pad4c2      7*/
!           ss = s-coordinate in plot frame (output).                           !/*pad4c2      8*/
!           tt = t-coordinate in plot frame (output).                           !/*pad4c2      9*/
!           ii = label value of arc between points (input).                     !/*pad4c2     10*/
!           la = starting location of arc in zst (input/output).                !/*pad4c2     11*/
!           lp = location of next point in zst (input/output).                  !/*pad4c2     12*/
!           nc = number of arcs so far in zst array (input/output).             !/*pad4c2     13*/
!           tpp(i) is used to store last values of d2,ss,tt, where              !/*pad4c2     14*/
!              d2=0 if ss,tt are in the plot frame.                             !/*pad4c2     15*/
      use allocate_memory                                                       !/*padx        2*/
      use global_data                                                           !/*padx        3*/
      use plot_data                                                             !/*padx        4*/
!                                                                               !/*padx        5*/
      implicit real(kindr) (a-h,o-z)                                            !/*padx        6*/
      implicit integer(kindi) (i-n)                                             !/*padx        7*/
!                                                                               !/*padx        8*/
!                                                                               !/*pad4c2     18*/
!        convert from parametric form to ss,tt plot frame coordinates.          !/*pad4c2     19*/
      call ptost(p,iq,js,ss,tt)                                                 !/*pad4c2     20*/
!                                                                               !/*pad4c2     21*/
!        do not store duplicate points.                                         !/*pad4c2     22*/
      if(tpp(1).ge.0..and.tpp(2).eq.ss.and.tpp(3).eq.tt)return                  !/*pad4c2     23*/
!                                                                               !/*pad4c2     24*/
!        branch according to whether arc endpoints are in frame.                !/*pad4c2     25*/
      d2=max(zero,abs(ss)-1.,abs(tt)-1.)                                        !/*pad4c2     26*/
      if((tpp(2).eq.ss.and.abs(ss).gt.1.).or.                                   !/*pad4c2     27*/
     & (tpp(3).eq.tt.and.abs(tt).gt.1.))go to 60                                !/*pad4c2     28*/
      if(tpp(1).lt.0.)go to 60                                                  !/*pad4c2     29*/
      sa=tpp(2)                                                                 !/*pad4c2     30*/
      sb=ss                                                                     !/*pad4c2     31*/
      ta=tpp(3)                                                                 !/*pad4c2     32*/
      tb=tt                                                                     !/*pad4c2     33*/
      if(tpp(1).eq.0..and.d2.eq.0.)go to 40                                     !/*pad4c2     34*/
!                                                                               !/*pad4c2     35*/
!        adjust points if arc crosses frame boundary.                           !/*pad4c2     36*/
      if(tpp(2).eq.ss)go to 10                                                  !/*pad4c2     37*/
      sa=max(-one,min(one,sa))                                                  !/*pad4c2     38*/
      sb=max(-one,min(one,sb))                                                  !/*pad4c2     39*/
      if(sa.eq.sb)go to 60                                                      !/*pad4c2     40*/
      if(sa.ne.tpp(2))ta=tpp(3)+(sa-tpp(2))*(tt-tpp(3))/(ss-tpp(2))             !/*pad4c2     41*/
      if(sb.ne.ss)tb=tpp(3)+(sb-tpp(2))*(tt-tpp(3))/(ss-tpp(2))                 !/*pad4c2     42*/
   10 if(tpp(3).eq.tt)go to 40                                                  !/*pad4c2     43*/
      if(abs(ta).le.1.)go to 20                                                 !/*pad4c2     44*/
      ta=max(-one,min(one,ta))                                                  !/*pad4c2     45*/
      sa=tpp(2)+(ta-tpp(3))*(ss-tpp(2))/(tt-tpp(3))                             !/*pad4c2     46*/
   20 if(abs(tb).lt.1.)go to 40                                                 !/*pad4c2     47*/
      tb=max(-one,min(one,tb))                                                  !/*pad4c2     48*/
      if(ta.eq.tb)go to 60                                                      !/*pad4c2     49*/
      sb=tpp(2)+(tb-tpp(3))*(ss-tpp(2))/(tt-tpp(3))                             !/*pad4c2     50*/
!                                                                               !/*pad4c2     51*/
!        start a new arc if required.                                           !/*pad4c2     52*/
   40 if(lp.gt.la)go to 50                                                      !/*pad4c2     53*/
      zst(la)=ii                                                                !/*pad4c2     54*/
      zst(la+1)=1                                                               !/*pad4c2     55*/
      zst(la+2)=1                                                               !/*pad4c2     56*/
      zst(la+3)=sa*extent(1)                                                    !/*pad4c2     57*/
      zst(la+4)=ta*extent(2)                                                    !/*pad4c2     58*/
      lp=la+5                                                                   !/*pad4c2     59*/
      nc=nc+1                                                                   !/*pad4c2     60*/
!                                                                               !/*pad4c2     61*/
!        add the new point to the arc.                                          !/*pad4c2     62*/
   50 zst(lp)=sb*extent(1)                                                      !/*pad4c2     63*/
      zst(lp+1)=tb*extent(2)                                                    !/*pad4c2     64*/
      lp=lp+2                                                                   !/*pad4c2     65*/
      zst(la+1)=zst(la+1)+1                                                     !/*pad4c2     66*/
      if(size(zst).lt.(lp+10)) then                                             !/*padx        9*/
        zst => chg_mem (zst,lp+5010,is)                                         !/*padx       10*/
        if(is.ne.0)call expire(0,'pltadd','chg_mem error')                      !/*padx       11*/
      endif                                                                     !/*padx       12*/
!                                                                               !/*pad4c2     69*/
!        save the new point as the old point.                                   !/*pad4c2     70*/
   60 tpp(2)=ss                                                                 !/*pad4c2     71*/
      tpp(3)=tt                                                                 !/*pad4c2     72*/
      if(tpp(1).ne.0..or.d2.ne.0.)la=lp                                         !/*pad4c2     73*/
      tpp(1)=d2                                                                 !/*pad4c2     74*/
#endif                                                                          /*!padx       13*/
      return                                                                    !/*pad4c2     75*/
      end                                                                       !/*pad4c2     76*/
