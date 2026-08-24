!#ident pertc.F from deck zf4c                                                  !/*zf4c        1*/
      subroutine pertc(mm,mk,mx,jp,lx,xf)                                       !/*zf4c        2*/
!        mm=1: initialize perturbation tally/kcode corrections.                 !/*zf4c        3*/
!        mm=2: calculate perturbation correction if perturbed cell              !/*zf4c        4*/
!          is also a tally cell (fm card, heating tally, kcode)                 !/*zf4c        5*/
!            mk = index of tally material.                                      !/*zf4c        6*/
!            mx = nuclide index of tally material.                              !/*zf4c        7*/
!            jp = pointer to reaction numbers (mt) of fm tally.                 !/*zf4c        8*/
!               = 0 make correction regardless of fm reactions;                 !/*zf4c        9*/
!               = -6 make correction if fm reaction includes fission.           !/*zf4c       10*/
!            lx = 0 kcode keff estimate.                                        !/*zf4c       11*/
!               = -kk for fm card bin kk.                                       !/*zf4c       12*/
!               = lx bin pointer for heating tallies.                           !/*zf4c       13*/
!            xf = cross section of tally nuclide mx reaction mt.                !/*zf4c       14*/
!        ptb(kptb+4,ip) = corrected tally multiplier, perturbation ip.          !/*zf4c       15*/
!        ptb(kptb+5,ip) = cumulative tally part of perturbation.                !/*zf4c       16*/
!        ptbtc          = total cumulative tally.                               !/*zf4c       17*/
!        jptb(kjpb+ip)  = 0 if perturbation correction inappropriate.           !/*zf4c       18*/
      use global_data                                                           !/*zfx         1*/
!                                                                               !/*zfx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*zfx         3*/
      implicit integer(kindi) (i-n)                                             !/*zfx         4*/
!                                                                               !/*zfx         5*/
      if(kc8.gt.0)return                                                        !/*zf4c       20*/
      go to(10,30)mm                                                            !/*zf4c       21*/
!                                                                               !/*zf4c       22*/
!************* mm=1:  initialize perturbation corrections **************        !/*zf4c       23*/
!                                                                      *        !/*zf4c       24*/
   10 do 20 ip=1,npert                                                          !/*zf4c       25*/
      ptb(kptb+4,ip)=iptb(lipb+14,ip)                                           !/*zf4c       26*/
      do 20 j=nptb(lnpt+ip)+1,nptb(lnpt+ip+1)                                   !/*zf4c       27*/
      ptb(kptb+4,ip)=ptb(kptb+4,ip)+ptb(kptb+1,j)*dptb(ldpt+2,j)+               !/*zf4c       28*/
     & (ptb(kptb+2,j)+ptb(kptb+1,j)**2)*.5*dptb(ldpt+3,j)**2                    !/*zf4c       29*/
   20 ptb(kptb+5,j)=0.                                                          !/*zf4c       30*/
      ptbtc=0.                                                                  !/*zf4c       31*/
      return                                                                    !/*zf4c       32*/
!                                                                               !/*zf4c       33*/
!************* mm=2:  calculate perturbation corrections ***************        !/*zf4c       34*/
!                                                                      *        !/*zf4c       35*/
!        correct f4 with fm with negative constant.                             !/*zf4c       36*/
   30 if(lx.eq.0)go to 50                                                       !/*zf4c       37*/
      if(lx.gt.0)go to 40                                                       !/*zf4c       38*/
      if(jptal(ljpt+2,ital).ne.4)return                                         !/*zf4c       39*/
      if(tds(iptal(lipt+5,2,ital)-lx).lt.0.)go to 50                            !/*zf4c       40*/
      return                                                                    !/*zf4c       41*/
!                                                                               !/*zf4c       42*/
!        correct heating tallies if segment divisor = 1.                        !/*zf4c       43*/
   40 if(iptal(lipt+4,2,ital).eq.0)return                                       !/*zf4c       44*/
      ib=itds(lx+1)                                                             !/*zf4c       45*/
      sd=tds(iptal(lipt+4,2,ital)+iptal(lipt+4,3,ital)*(ib-1)+1)                !/*zf4c       46*/
      if(abs(1.-sd).gt.1.e-4)return                                             !/*zf4c       47*/
!                                                                               !/*zf4c       48*/
!        accumulate the total tally reaction xsec always.                       !/*zf4c       49*/
   50 ptbtc=ptbtc+xf                                                            !/*zf4c       50*/
!                                                                               !/*zf4c       52*/
!        loop over all perturbations. no correction needed unless               !/*zf4c       53*/
!        perturbation made in this tally cell (jptb(kjpb+ip).ne.0).             !/*zf4c       54*/
      do 110 ip=1,npert                                                         !/*zf4c       55*/
      if(jptb(kjpb+ip).eq.0)go to 110                                           !/*zf4c       56*/
!                                                                               !/*zf4c       57*/
!        loop over perturbed nuclides ni of perturbation ip.                    !/*zf4c       58*/
!        check if tally nuclide mx matches perturbed nuclide mx.                !/*zf4c       59*/
      do 100 ni=nptb(lnpt+ip)+1,nptb(lnpt+ip+1)                                 !/*zf4c       60*/
      ix=lme(llme+ipt,nint(dptb(ldpt+1,ni)))                                    !/*zf4c       61*/
      if(ix.ne.lme(llme+ipt,mx))go to 100                                       !/*zf4c       62*/
      if(jp.eq.0)go to 90                                                       !/*zf4c       63*/
!                                                                               !/*zf4c       64*/
!        accumulate ptb(kptb+5,ni), the tally reaction that's perturbed,        !/*zf4c       65*/
!        if tally reaction mt matches perturbation reaction mp.                 !/*zf4c       66*/
      np=iptb(lipb+9,ip)                                                        !/*zf4c       67*/
      if(np.eq.0)go to 90                                                       !/*zf4c       68*/
      do 80 mi=1,np                                                             !/*zf4c       69*/
      mp=nint(rptb(lrpt+iptb(lipb+10,ip)+mi-1))                                 !/*zf4c       70*/
      if(jp.gt.20)go to 60                                                      !/*zf4c       71*/
      if(mp.eq.jp)go to 90                                                      !/*zf4c       72*/
      if(jp.ne.-6)go to 80                                                      !/*zf4c       73*/
      if(mp.eq.18.or.mp.eq.19)go to 90                                          !/*zf4c       74*/
      if(mcal.ne.0.and.mp.eq.-2)go to 90                                        !/*zf4c       75*/
      go to 80                                                                  !/*zf4c       76*/
   60 do 70 n=1,mod(int(abs(tds(jp))),10000000)                                 !/*zf4c       77*/
      mt=tds(jp+n)                                                              !/*zf4c       78*/
   70 if(mp.eq.mt)go to 90                                                      !/*zf4c       79*/
   80 continue                                                                  !/*zf4c       80*/
      go to 100                                                                 !/*zf4c       81*/
   90 ptb(kptb+5,ni)=ptb(kptb+5,ni)+xf                                          !/*zf4c       82*/
  100 continue                                                                  !/*zf4c       83*/
  110 continue                                                                  !/*zf4c       84*/
!                                                                               !/*zf4c       85*/
!        correct the perturbation after summing over all nuclides mk.           !/*zf4c       86*/
!        the perturbation tally correction is the fraction of the               !/*zf4c       87*/
!        tally cross section participating in the perturbation,                 !/*zf4c       88*/
!        (ptb(kptb+5,ni)/ptbtc), times a derivative term.                       !/*zf4c       89*/
      if(mx.ne.jmd(ljmd+mk+1)-1)return                                          !/*zf4c       90*/
      if(ptbtc.eq.0.)return                                                     !/*zf4c       91*/
      f=1./ptbtc                                                                !/*zf4c       92*/
      do 120 ip=1,npert                                                         !/*zf4c       93*/
      do 120 ni=nptb(lnpt+ip)+1,nptb(lnpt+ip+1)                                 !/*zf4c       94*/
  120 ptb(kptb+4,ip)=ptb(kptb+4,ip)+f*ptb(kptb+5,ni)*                           !/*zf4c       95*/
     & (dptb(ldpt+2,ni)+ptb(kptb+1,ni)*dptb(ldpt+3,ni)**2)                      !/*zf4c       96*/
      return                                                                    !/*zf4c       97*/
      end                                                                       !/*zf4c       98*/
