!#ident transm.F from deck tz                                                   !/*tz          1*/
      subroutine transm(dd,st,iz)                                               !/*tz4b        1*/
!        calculate the attenuation amfp over the distance dd from               !/*tz          3*/
!        xxx,yyy,zzz in the direction uuu,vvv,www.                              !/*tz          4*/
!        st is the russian roulette level.  iz is the no score flag.            !/*tz4b        2*/
      use global_data                                                           !/*tzx         1*/
!                                                                               !/*tzx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*tzx         3*/
      implicit integer(kindi) (i-n)                                             !/*tzx         4*/
!                                                                               !/*tzx         5*/
!                                                                               !/*tz          7*/
      iz=0                                                                      !/*tz4b        3*/
      sd=0.                                                                     !/*tz          8*/
      amfp=0.                                                                   !/*tz          9*/
      if(lca(llca+icl).lt.0)call chkcel(icl,3,j)                                !/*tz         10*/
      ft=st                                                                     !/*tz         11*/
      if(ft.ne.0.)tt=-log(ft)                                                   !/*tz         12*/
!                                                                               !/*tz         13*/
!        do russian roulette on small scores.                                   !/*tz         14*/
   10 if(ft.eq.0.)go to 20                                                      !/*tz         15*/
      if(amfp.le.tt)go to 20                                                    !/*tz4c        1*/
      t=exp(-amfp)                                                              !/*tz         17*/
      if(ft*rang().gt.t)go to 100                                               !/*tz         18*/
      wgt=wgt*ft/t                                                              !/*tz         19*/
      ft=t                                                                      !/*tz         20*/
      tt=amfp                                                                   !/*tz         21*/
!                                                                               !/*tz         22*/
!        calculate the attenuation for this section of the track.               !/*tz         23*/
   20 call track(icl)                                                           !/*tz         24*/
      if(kdb.ne.0)go to 90                                                      !/*tz4b        5*/
      totm=0.                                                                   !/*tz         26*/
      if(mat(lmat+icl).eq.0)go to 30                                            !/*tz         27*/
      if(ipt.eq.1)call acetot(0,mat(lmat+icl))                                  !/*tz         28*/
      if(ipt.eq.2)call photot(mat(lmat+icl))                                    !/*tz         29*/
   30 ple=totm*rho(lrho+icl)                                                    !/*tz         30*/
      d=min(dls,dd-sd)                                                          !/*tz         31*/
      amfp=amfp+ple*d                                                           !/*tz         32*/
      if(amfp.gt.80.)go to 110                                                  !/*tz         33*/
      sd=sd+dls                                                                 !/*tz         34*/
!                                                                               !/*tz         35*/
!        update the location and the new cell.                                  !/*tz         36*/
      xxx=xxx+uuu*d                                                             !/*tz         37*/
      yyy=yyy+vvv*d                                                             !/*tz         38*/
      zzz=zzz+www*d                                                             !/*tz         39*/
      do 40 l=0,lev-1                                                           !/*tz         40*/
      udt(1,l)=udt(1,l)+udt(4,l)*d                                              !/*tz         41*/
      udt(2,l)=udt(2,l)+udt(5,l)*d                                              !/*tz         42*/
   40 udt(3,l)=udt(3,l)+udt(6,l)*d                                              !/*tz         43*/
      tme=tme+d/vel                                                             !/*tz         44*/
      if(sd.ge.dd)return                                                        !/*tz         45*/
      jsu=jap                                                                   !/*tz         46*/
      call newcel(zero)                                                         !/*tz         47*/
      if(kdb.ne.0)go to 90                                                      !/*tz4b        6*/
      icl=iap                                                                   !/*tz         49*/
      if(fim(lfim+ipt,iap).ne.0.)go to 10                                       !/*tz         50*/
!                                                                               !/*tz         51*/
!        set no score flag and return.                                          !/*tz4b        7*/
!        zero importance cell.                                                  !/*tz4b        8*/
      iz=5                                                                      !/*tz4b        9*/
      return                                                                    !/*tz4b       10*/
!        lost particle in track or newcel.  kdb.ne.0                            !/*tz4b       11*/
   90 iz=-1                                                                     !/*tz4b       12*/
      return                                                                    !/*tz4b       13*/
!        russian roulette in transmission from dd card.                         !/*tz4b       14*/
  100 iz=3                                                                      !/*tz4b       15*/
      return                                                                    !/*tz4b       16*/
!        underflow in transmission. heavily attenuating path.                   !/*tz4b       17*/
  110 iz=4                                                                      !/*tz4b       18*/
      return                                                                    !/*tz         88*/
      end                                                                       !/*tz         89*/
