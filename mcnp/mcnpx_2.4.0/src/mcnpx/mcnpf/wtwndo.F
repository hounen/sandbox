!#ident wtwndo.F from deck ww                                                   !/*ww          1*/
      subroutine wtwndo(mm)                                                     !/*ww4c        1*/
!        do weight window splitting and russian roulette.                       !/*ww          3*/
      use global_data                                                           !/*wwx         1*/
!                                                                               !/*wwx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*wwx         3*/
      implicit integer(kindi) (i-n)                                             !/*wwx         4*/
!                                                                               !/*wwx         5*/
!                                                                               !/*ww          6*/
!        no window game for surface-only windows at collisions,                 !/*ww4c        2*/
!        collision-only windows at surfaces, nor cell-based windows             !/*ww4c        3*/
!        in dxtran spheres.                                                     !/*ww4c        4*/
      if(idx.ne.0.and.wwp(ipt,5).ge.0.)go to 40                                 !/*ww4c        5*/
      if(jsu.eq.0.and.wwp(ipt,4).gt.2.)return                                   !/*ww4c        6*/
      if(jsu.ne.0.and.wwp(ipt,4).lt.0.)return                                   !/*ww4c        7*/
!                                                                               !/*ww4c        8*/
!        find the index of the energy or time in wwe.                           !/*ww4c        9*/
      t1=erg                                                                    !/*ww4c       10*/
      if(wwp(ipt,6).ne.0.)t1=tme                                                !/*ww4c       11*/
      ic=0                                                                      !/*ww          8*/
      ib=nww(ipt)                                                               !/*ww          9*/
   10 if(ib-ic.eq.1)go to 30                                                    !/*ww         10*/
      ih=(ic+ib)/2                                                              !/*ww         11*/
      if(t1.lt.wwe(lwwe+ih+mww(ipt)))go to 20                                   !/*ww4c       12*/
      ic=ih                                                                     !/*ww         13*/
      go to 10                                                                  !/*ww         14*/
   20 ib=ih                                                                     !/*ww         15*/
      go to 10                                                                  !/*ww         16*/
!                                                                               !/*ww         17*/
!        return if weight is within window.                                     !/*ww         18*/
   30 ww=wwval(ipt,icl,ib,1,ix)                                                 !/*ww4c2       1*/
      if(ww.eq.0.)go to 50                                                      !/*ww         20*/
      if(wgt.lt.ww)go to 60                                                     !/*ww4c       13*/
      t1=wwp(ipt,1)*ww                                                          !/*ww         22*/
      if(wgt.le.t1)return                                                       !/*ww         23*/
!                                                                               !/*ww         24*/
!        split if the weight is above the window.                               !/*ww         25*/
      npa=min(wwp(ipt,3)-1.,wgt/t1)                                             !/*ww         26*/
      wgt=wgt/(npa+1)                                                           !/*ww         27*/
      call gain_pax (igain_all_pls_weight_window,ipt,npa,zero,zero)             !/*wwx         6*/
      if(erg.lt.xunru.and.erg.gt.xunrl.and.ipt.eq.1.and.mat(lmat+icl)           !/*ww4c       14*/
     & .ne.0)call acetot(0,mat(lmat+icl))                                       !/*ww4c       15*/
      call bankit(mm)                                                           !/*ww4b        3*/
      return                                                                    !/*ww         31*/
!                                                                               !/*ww         32*/
!        cell-based windows in dxtran spheres use dxtran weight cutoff.         !/*ww4c       16*/
   40 if(dxw(ipt,1).le.0.)return                                                !/*ww4c       17*/
      if(wgt*gamef*fiml(1).gt.fiml9(1,1)*dxw(ipt,2))return                      !/*wwx         7*/
      t2=dxw(ipt,1)*fiml9(1,1)/(gamef*fiml(1))                                  !/*wwx         8*/
      t2=min(2.*wgt,t2)                                                         !/*ww4c2       2*/
      nx = igain_all_pls_weight_cutoff                                          !/*wwx         9*/
      ny = loss_all_pls_weight_cutoff                                           !/*wwx        10*/
      nb = ipwb_variance_weight_cutoff                                          !/*wwx        11*/
      go to 70                                                                  !/*ww4c       22*/
!                                                                               !/*ww4c       23*/
!        play weight cutoff game at collisions if zero window and               !/*ww4c       24*/
!        implicit capture (wcs2tc(ipt).ne.0).  1-for-2 split limit.             !/*ww4c       25*/
   50 if(jsu.ne.0.or.mm.eq.24)return                                            !/*ww4c       26*/
      if(wgt*gamef*fiml(1).gt.fiml9(1,1)*wcs2tc(ipt))return                     !/*wwx        12*/
      t2=wcs1tc(ipt)*fiml9(1,1)/(gamef*fiml(1))                                 !/*wwx        13*/
      t2=min(2.*wgt,t2)                                                         !/*ww4c2       3*/
      nx = igain_all_pls_weight_cutoff                                          !/*wwx        14*/
      ny = loss_all_pls_weight_cutoff                                           !/*wwx        15*/
      nb = ipwb_variance_weight_cutoff                                          !/*wwx        16*/
      go to 70                                                                  !/*ww4c       31*/
!                                                                               !/*ww4c       32*/
!        do russian roulette if the weight is below the window.                 !/*ww4c       33*/
   60 t2=min(wgt*wwp(ipt,3),ww*wwp(ipt,2))                                      !/*ww4c       34*/
      nx = igain_all_pls_weight_window                                          !/*wwx        17*/
      ny = loss_all_pls_weight_window                                           !/*wwx        18*/
      nb = ipwb_variance_weight_window                                          !/*wwx        19*/
!                                                                               !/*ww4c       36*/
   70 if (wgt.lt.t2*rang()) nter = ny                                           !/*wwx        20*/
      if(nter.ne.0)return                                                       !/*wwx        21*/
!     QUESTION: Revisit this to see if both t2 - wgt > 0                        !/*wwx        22*/
!     and t2 - wgt < 0 occur.  If so, losses should probably                    !/*wwx        23*/
!     be in the loss columns rather than negative contributions                 !/*wwx        24*/
!     to the gain columns.                                                      !/*wwx        25*/
      call gain_pax (nx, ipt, 0, t2-wgt, (t2-wgt)*erg)                          !/*wwx        26*/
      call contrib_pwb (ipt,nb,icl,t2-wgt)                                      !/*wwx        27*/
      wgt=t2                                                                    !/*ww         49*/
      return                                                                    !/*ww         50*/
      end                                                                       !/*ww         51*/
