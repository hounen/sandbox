!#ident forcol.F from deck fc                                                   !/*fc          1*/
      subroutine forcol                                                         !/*fc          2*/
!        force collisions.                                                      !/*fc          3*/
      use global_data                                                           !/*fcx         1*/
!                                                                               !/*fcx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*fcx         3*/
      implicit integer(kindi) (i-n)                                             !/*fcx         4*/
!                                                                               !/*fcx         5*/
!                                                                               !/*fc          5*/
!        separate collided particles from ones entering or born in cell.        !/*fc          6*/
      if(jsu.ne.0.or.ncp.eq.0)go to 40                                          !/*fc          7*/
!                                                                               !/*fc          8*/
!        russian roulette collided particles below weight cutoff.               !/*fc          9*/
      if(for(lfor+ipt,icl).lt.0.)go to 110                                      !/*fc         10*/
      if(idx.eq.0)go to 10                                                      !/*fc         12*/
      if(wwp(ipt,5).lt.0.)go to 10                                              !/*fc4c        1*/
      if(dxw(ipt,1).le.0.)go to 110                                             !/*fc         13*/
      if(wgt*gamef*fiml(1).gt.fiml9(1,1)*dxw(ipt,2))go to 40                    !/*fcx         6*/
      t1=dxw(ipt,1)*fiml9(1,1)/(gamef*fiml(1))                                  !/*fcx         7*/
      go to 20                                                                  !/*fc         16*/
   10 if(wwp(ipt,4).eq.0.)go to 15                                              !/*fc4c        2*/
      if(wwp(ipt,4).lt.2.)go to 40                                              !/*fc4c        3*/
      go to 110                                                                 !/*fc4c        4*/
   15 if(wcs1tc(ipt).le.0.)go to 110                                            !/*fc4c        5*/
      if(wgt*gamef*fiml(1).gt.fiml9(1,1)*wcs2tc(ipt))go to 40                   !/*fcx         8*/
      t1=wcs1tc(ipt)*fiml9(1,1)/(gamef*fiml(1))                                 !/*fcx         9*/
   20 if(wgt.ge.t1*rang())go to 30                                              !/*fc         20*/
      nter = loss_all_pls_weight_cutoff                                         !/*fcx        10*/
      return                                                                    !/*fc         22*/
   30 call gain_pax (igain_all_pls_weight_cutoff, ipt,                          !/*fcx        11*/
     &               0, t1-wgt, (t1-wgt)*erg)                                   !/*fcx        12*/
      call contrib_pwb (ipt,ipwb_variance_weight_cutoff,icl,t1-wgt)             !/*fcx        13*/
      wgt=t1                                                                    !/*fc         26*/
!                                                                               !/*fc         27*/
!       russian roulette forced collided part for fractional forcing.           !/*fc         28*/
   40 d=min(dls,dxl,dtc)                                                        !/*fc         29*/
      t1=qpl*d                                                                  !/*fc         30*/
      if(t1.gt.25.)go to 110                                                    !/*fc         31*/
      f=abs(for(lfor+ipt,icl))                                                  !/*fc         32*/
      t2=1.-t1                                                                  !/*fc         33*/
      if(t1.le.1e-7)go to 50                                                    !/*fc         34*/
      t2=exp(-t1)                                                               !/*fc         35*/
      t1=1.-t2                                                                  !/*fc         36*/
   50 if(dbcn(24).eq.zero.and.abs(f-one).lt..01)go to 60                        !/*fcxc        1*/
      if(dbcn(24).ne.zero.and.f.eq.1.)go to 60                                  !/*fcxc        2*/
      if(rang().lt.f)go to 60                                                   !/*fc         38*/
      t1=wgt*t1                                                                 !/*fc         39*/
      call lose_pax (loss_all_pls_forced_collisions, ipt,                       !/*fcx        14*/
     &               0, t1, t1*erg)                                             !/*fcx        15*/
      call contrib_pwb (ipt,ipwb_variance_forced_coll,icl,-t1)                  !/*fcx        16*/
      wgt=wgt*t2                                                                !/*fc         43*/
      if (wgt.le.zero) nter = loss_all_pls_weight_cutoff                        !/*fcx        17*/
      return                                                                    !/*fc         45*/
!                                                                               !/*fc         46*/
!        determine the collided and uncollided weights, wc and wgt.             !/*fc         47*/
   60 gs=1./qpl                                                                 !/*fc         48*/
      if(t1.le.1e-7)pmf=rang()*d                                                !/*fc         49*/
      if(t1.gt.1e-7)pmf=-log(1.-rang()*t1)*gs                                   !/*fc         50*/
      wc=t1*wgt/f                                                               !/*fc         51*/
      wgt=t2*wgt                                                                !/*fc         52*/
      if(f.eq.1.)go to 70                                                       !/*fc         53*/
      t1=(1.-f)*wc                                                              !/*fc         54*/
      call gain_pax (igain_all_pls_forced_collisions, ipt,                      !/*fcx        18*/
     &               0, t1, t1*erg)                                             !/*fcx        19*/
      call contrib_pwb (ipt,ipwb_variance_forced_coll,icl,t1)                   !/*fcx        20*/
!                                                                               !/*fc         58*/
!        bank and tally the uncollided part.                                    !/*fc         59*/
   70 call gain_pax (igain_all_pls_forced_collisions,ipt,1,zero,zero)           !/*fcx        21*/
      npa=jap-1000000                                                           !/*fc         62*/
      jsu=levp                                                                  !/*fc4a        1*/
      call bankit(5)                                                            !/*fc4a        2*/
!                                                                               !/*fc         86*/
!        continue transport of collided part.                                   !/*fc         87*/
      wgt=wc                                                                    !/*fc         88*/
      return                                                                    !/*fc         89*/
!                                                                               !/*fc         90*/
!        no forced collision.  sample distance to collision normally.           !/*fc         91*/
  110 pmf=-log(rang())/qpl                                                      !/*fc         92*/
      return                                                                    !/*fc         93*/
      end                                                                       !/*fc         94*/
