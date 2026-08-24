!#ident pertub.F from deck zp4b                                                 !/*zp4b        1*/
      subroutine pertub(d)                                                      !/*zp4b        2*/
!        calculate the perturbation coefficients.                               !/*zp4b        3*/
      use global_data                                                           !/*zpx         1*/
!                                                                               !/*zpx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*zpx         3*/
      implicit integer(kindi) (i-n)                                             !/*zpx         4*/
!                                                                               !/*zpx         5*/
!                                                                               !/*zp4b        5*/
!        return if kcode problem is not settled.                                !/*zp4b        6*/
      if(kc8.gt.0)return                                                        !/*zp4b        7*/
!                                                                               !/*zp4b        8*/
!        loop over all the perturbations.                                       !/*zp4b        9*/
      if(totm.eq.0.)return                                                      !/*zp4b       10*/
      ix=0                                                                      !/*zp4b       11*/
      if(iexp.ne.0)ix=lme(llme+ipt,iexp)                                        !/*zp4b       12*/
      do 70 ip=1,npert                                                          !/*zp4b       13*/
      jptb(kjpb+ip)=0                                                           !/*zp4c        1*/
!                                                                               !/*zp4b       14*/
!        check the particle type, energy, and cell.                             !/*zp4b       15*/
      if(kpptb(lpptb+ipt,ip).eq.0)go to 70                                      !/*zpx         6*/
      dg=one                                                                    !/*zp4b       17*/
      d0=one                                                                    !/*zp4b       18*/
      if(ix.eq.0)d0=0.                                                          !/*zp4c        2*/
      if(iptb(lipb+11,ip).eq.0)go to 10                                         !/*zp4b       20*/
      lr=lrpt+iptb(lipb+12,ip)                                                  !/*zp4b       21*/
      if(erg.le.rptb(lr).or.erg.gt.rptb(lr+1))dg=0.                             !/*zp4b       22*/
      if(eg0.le.rptb(lr).or.eg0.gt.rptb(lr+1))d0=0.                             !/*zp4b       23*/
      if(dg+d0.eq.0.)go to 70                                                   !/*zp4c        3*/
   10 lr=lrpt+iptb(lipb+4,ip)-1                                                 !/*zp4b       25*/
      do 20 i=1,iptb(lipb+3,ip)                                                 !/*zp4b       26*/
   20 if(icl.eq.nint(rptb(lr+i)))go to 30                                       !/*zp4b       27*/
      go to 70                                                                  !/*zp4b       28*/
!                                                                               !/*zp4b       29*/
!        loop over all perturbed elements.                                      !/*zp4b       30*/
   30 dc=0.                                                                     !/*zp4c        4*/
      if(d.eq.min(pmf,deb))dc=one                                               !/*zp4b       32*/
      if(dg.ne.0.)jptb(kjpb+ip)=1                                               !/*zp4c        5*/
      do 60 ni=nptb(lnpt+ip)+1,nptb(lnpt+ip+1)                                  !/*zp4b       33*/
      m=nint(dptb(ldpt+1,ni))                                                   !/*zp4b       34*/
      iex=lme(llme+ipt,m)                                                       !/*zp4b       35*/
!                                                                               !/*zp4b       36*/
!        get the microscopic x-s for the desired reaction mt.                   !/*zp4b       37*/
!                                                                               !/*zp4c        6*/
!        mt=total or no reactions:                                              !/*zp4c        7*/
      n=iptb(lipb+9,ip)                                                         !/*zp4c        8*/
      if(n.ne.0)go to 40                                                        !/*zp4c        9*/
      db=one                                                                    !/*zp4c       10*/
      xs=rtc(krtc+5,iex)                                                        !/*zp4c       11*/
      if(mcal.eq.0.and.ipt.eq.2)xs=rtc(krtc+4,iex)                              !/*zp4c       12*/
      go to 55                                                                  !/*zp4c       13*/
!                                                                               !/*zp4c       14*/
!        neutron or multigroup reactions.                                       !/*zp4c       15*/
   40 xs=0.                                                                     !/*zp4c       16*/
      db=0.                                                                     !/*zp4c       17*/
      if(ipt.eq.2.and.mcal.eq.0)go to 46                                        !/*zp4c       18*/
      do 44 i=1,n                                                               !/*zp4c       19*/
      mt=nint(rptb(lrpt+iptb(lipb+10,ip)+i-1))                                  !/*zp4c       20*/
      if(mt.gt.0)go to 42                                                       !/*zp4c       21*/
      if(mcal.eq.0.and.mt.eq.-6.and.(mtp.eq.18.or.mtp.eq.19))db=one             !/*zp4c       22*/
      if(mcal.eq.0.and.mt.eq.-3.and.mtp.eq.2)db=one                             !/*zp4c       23*/
      if(mcal.ne.0.and.mt.eq.-2.and.mtp.eq.18)db=one                            !/*zp4c       24*/
   42 if(mt.eq.mtp)db=one                                                       !/*zp4c       25*/
      g=getxs(mt)                                                               !/*zp4c       26*/
   44 xs=xs+g                                                                   !/*zp4c       27*/
      go to 55                                                                  !/*zp4c       28*/
!                                                                               !/*zp4c       29*/
!        photon reactions.                                                      !/*zp4c       30*/
   46 do 50 i=1,n                                                               !/*zp4c       31*/
      mt=nint(rptb(lrpt+iptb(lipb+10,ip)+i-1))                                  !/*zp4c       32*/
      if(mt.lt.0)go to 48                                                       !/*zp4c       33*/
      if(mt.eq.504)mt=-1                                                        !/*zp4c       34*/
      if(mt.eq.502)mt=-2                                                        !/*zp4c       35*/
      if(mt.eq.522)mt=-3                                                        !/*zp4c       36*/
      if(mt.eq.516)mt=-4                                                        !/*zp4c       37*/
   48 if(mt.eq.mtp)db=one                                                       !/*zp4c       38*/
      g=rtc(krtc-mt,iex)                                                        !/*zp4c       39*/
      if(-mt.gt.1)g=g-rtc(krtc-mt-1,iex)                                        !/*zp4c       40*/
      if(-mt.gt.4)g=rtc(krtc-mt-1,iex)                                          !/*zp4c       41*/
   50 xs=xs+g                                                                   !/*zp4b       49*/
!                                                                               !/*zp4b       50*/
!        initialize variables.                                                  !/*zp4b       51*/
   55 bn=xs*fme(lfme+m)*rho(lrho+icl)                                           !/*zp4c       42*/
      tn=totm*rho(lrho+icl)                                                     !/*zp4b       53*/
      bo=ptb(kptb+3,ni)                                                         !/*zp4b       54*/
      to=totmp                                                                  !/*zp4b       55*/
      if(iex.ne.ix)db=0.                                                        !/*zp4c       43*/
!                                                                               !/*zp4b       58*/
!        calculate the derivative, first order - four terms.                    !/*zp4b       59*/
      x1=d0*db-d0*bo/to                                                         !/*zp4c       44*/
      x2=-dg*bn*d+dc*dg*bn/tn                                                   !/*zp4c       45*/
      ptb(kptb+1,ni)=ptb(kptb+1,ni)+x1+x2                                       !/*zp4c       46*/
!                                                                               !/*zp4b       62*/
!        calculate the derivative, second order - four terms.                   !/*zp4b       63*/
      b2=2.*d0*(-db*bo/to+bo**2/to**2)+dg*(bn*d)**2-2.*dc*dg*bn**2*d/tn         !/*zp4b       64*/
      ptb(kptb+2,ni)=ptb(kptb+2,ni)+b2-x1**2-x2**2                              !/*zp4c       47*/
   60 ptb(kptb+3,ni)=bn                                                         !/*zp4b       66*/
   70 continue                                                                  !/*zp4b       67*/
      totmp=totm*rho(lrho+icl)                                                  !/*zp4b       68*/
      return                                                                    !/*zp4b       69*/
      end                                                                       !/*zp4b       70*/
