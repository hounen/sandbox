!#ident tgtvel.F from deck tv4a                                                 !/*tv4a        1*/
      subroutine tgtvel(e2,r2,ar,vr,k2)                                         !/*tv4a        2*/
!        sample the velocity of the target nucleus for the neutron free         !/*tv4a        3*/
!        gas thermal treatment.                                                 !/*tv4a        4*/
!        should be used for elastic scattering only.                            !/*tv4a        5*/
!        e2=relative energy in rest frame of target motion.                     !/*tv4a        6*/
!        r2, k2 are interpolation fraction, index of energy in xss.             !/*tv4a        7*/
      use global_data                                                           !/*tvx         1*/
!                                                                               !/*tvx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*tvx         3*/
      implicit integer(kindi) (i-n)                                             !/*tvx         4*/
!                                                                               !/*tvx         5*/
      dimension vr(3)                                                           !/*tv4a        9*/
!                                                                               !/*tv4c        1*/
!        return if in the unresolved prob table treatment.                      !/*tv4c        2*/
      if(rtc(krtc+11,iex).ge.0.)return                                          !/*tv4c        3*/
!                                                                               !/*tv4a       10*/
!        sample the velocity of the target nucleus.                             !/*tv4a       11*/
      ar=awn(lawn+iex)/ttn                                                      !/*tv4a       12*/
      ycn=sqrt(erg*ar)                                                          !/*tv4a       13*/
   10 if(rang()*(ycn+1.12837917).le.ycn)go to 20                                !/*tv4a       14*/
      r1=rang()                                                                 !/*tv4a       15*/
      z2=-log(r1*rang())                                                        !/*tv4a       16*/
      go to 30                                                                  !/*tv4a       17*/
   20 r1=rang()**2                                                              !/*tv4a       18*/
      s=r1+rang()**2                                                            !/*tv4a       19*/
      if(s.gt.1.)go to 20                                                       !/*tv4a       20*/
      z2=-r1*log(s)/s-log(rang())                                               !/*tv4a       21*/
   30 z=sqrt(z2)                                                                !/*tv4a       22*/
      c=2.*rang()-1.                                                            !/*tv4a       23*/
      x2=ycn**2+z2-2.*ycn*z*c                                                   !/*tv4a       24*/
      if((rang()*(ycn+z))**2.gt.x2)go to 10                                     !/*tv4a       25*/
      call rotas(c,uuu,vtr,0,irt)                                               !/*tv4b        1*/
      vtr(1)=z*vtr(1)                                                           !/*tv4a       28*/
      vtr(2)=z*vtr(2)                                                           !/*tv4a       29*/
      vtr(3)=z*vtr(3)                                                           !/*tv4a       30*/
!                                                                               !/*tv4a       31*/
!        calculate functions of the target velocity.                            !/*tv4a       32*/
      vr(1)=ycn*uuu-vtr(1)                                                      !/*tv4a       33*/
      vr(2)=ycn*vvv-vtr(2)                                                      !/*tv4a       34*/
      vr(3)=ycn*www-vtr(3)                                                      !/*tv4a       35*/
      ssr=sqrt(vr(1)**2+vr(2)**2+vr(3)**2)                                      !/*tv4a       36*/
      e2=x2/ar                                                                  !/*tv4a       37*/
      ic=ktc(kktc+1,iex)+jxs(ljxs+1,iex)-1                                      !/*tv4a       38*/
      if(e2.le.xss(ic+1).and.e2.ge.xss(ic))go to 70                             !/*tv4a       39*/
      if(e2.gt.xss(ic))go to 50                                                 !/*tv4a       40*/
   40 if(ic.eq.jxs(ljxs+1,iex))go to 60                                         !/*tv4a       41*/
      ic=ic-1                                                                   !/*tv4a       42*/
      if(e2.lt.xss(ic))go to 40                                                 !/*tv4a       43*/
      go to 60                                                                  !/*tv4a       44*/
   50 if(ic.eq.jxs(ljxs+1,iex)+nxs(lnxs+3,iex)-2)go to 60                       !/*tv4a       45*/
      ic=ic+1                                                                   !/*tv4a       46*/
      if(e2.gt.xss(ic+1))go to 50                                               !/*tv4a       47*/
   60 k2=ic-jxs(ljxs+1,iex)+1                                                   !/*tv4a       48*/
   70 if(nty(lnty+iex).ne.2)r2=                                                 !/*tv4a       49*/
     & max(zero,min(one,(e2-xss(ic))/(xss(ic+1)-xss(ic))))                      !/*tv4a       50*/
      return                                                                    !/*tv4a       51*/
      end                                                                       !/*tv4a       52*/
