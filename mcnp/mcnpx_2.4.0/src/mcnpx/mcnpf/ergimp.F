!#ident ergimp.F from deck ei                                                   !/*ei          1*/
      subroutine ergimp                                                         !/*ei          2*/
!        do energy splitting and russian roulette.                              !/*ei          3*/
      use global_data                                                           !/*eix         1*/
!                                                                               !/*eix         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*eix         3*/
      implicit integer(kindi) (i-n)                                             !/*eix         4*/
!                                                                               !/*eix         5*/
!                                                                               !/*ei          5*/
!        calculate the importance ratio.  return if it is 1.                    !/*ei          6*/
      if(eg0.lt.erg.and.espl(ipt,1).lt.0.)return                                !/*ei          7*/
      a=max(eg0,erg)                                                            !/*ei          8*/
      b=min(eg0,erg)                                                            !/*ei          9*/
      r=1.                                                                      !/*ei         10*/
      do 10 i=1,9,2                                                             !/*ei         11*/
      if(espl(ipt,i).eq.0.)go to 20                                             !/*ei         12*/
   10 if(a.gt.espl(ipt,i+1).and.b.le.espl(ipt,i+1))r=r*espl(ipt,i)              !/*ei         13*/
   20 if(r.eq.1.)return                                                         !/*ei         14*/
      r=abs(r)                                                                  !/*ei         15*/
      if(erg.gt.eg0)r=1./r                                                      !/*ei         16*/
!                                                                               !/*ei         17*/
!        sample for splitting, continuation, or termination.                    !/*ei         18*/
      s=r-rang()                                                                !/*ei         19*/
      if(s.lt.0.)go to 30                                                       !/*ei         20*/
      npa=s                                                                     !/*ei         21*/
      wgt=wgt/r                                                                 !/*ei         22*/
!                                                                               !/*ei         23*/
!        increment the summary accounts.                                        !/*ei         24*/
      if (npa+1-r.lt.zero) then                                                 !/*eix         6*/
         nx = loss_all_pls_energy_importance                                    !/*eix         7*/
         call lose_pax (nx, ipt,                                                !/*eix         8*/
     &                  0, abs(npa+1-r)*wgt, abs(npa+1-r)*wgt*erg)              !/*eix         9*/
      else                                                                      !/*eix        10*/
         nx = igain_all_pls_energy_importance                                   !/*eix        11*/
         call gain_pax (nx, ipt,                                                !/*eix        12*/
     &                  0, abs(npa+1-r)*wgt, abs(npa+1-r)*wgt*erg)              !/*eix        13*/
      endif                                                                     !/*eix        14*/
      call contrib_pwb (ipt,ipwb_variance_energy_import,icl,                    !/*eix        15*/
     &                  (npa+1-r)*wgt)                                          !/*eix        16*/
!                                                                               !/*ei         30*/
!        bank the additional particles if splitting occurs.                     !/*ei         31*/
      if(npa.eq.0)return                                                        !/*ei         32*/
!     QUESTION:  Consider whether this could be in the loss column.             !/*eix        17*/
!                That is, could npa ever be < 0 ???                             !/*eix        18*/
      call gain_pax (nx, ipt, npa, zero, zero)                                  !/*eix        19*/
      if(erg.lt.xunru.and.erg.gt.xunrl.and.ipt.eq.1.and.mat(lmat+icl)           !/*ei4c        1*/
     & .ne.0)call acetot(0,mat(lmat+icl))                                       !/*ei4c        2*/
      call bankit(2)                                                            !/*ei4a        1*/
      return                                                                    !/*ei         38*/
!                                                                               !/*ei         39*/
!        set termination flag if russian roulette loses.                        !/*ei         40*/
   30 nter = loss_all_pls_energy_importance                                     !/*eix        20*/
      return                                                                    !/*ei         42*/
      end                                                                       !/*ei         43*/
