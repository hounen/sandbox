!#ident den1.F from deck d14c                                                   !/*d14c        1*/
      subroutine den_el1(az,su,pl,fo,eq,xq,nt,wt,tm)                            !/*d1x         1*/
!        calculate oscillator parameters, fo, eq, xq, and the number            !/*d14c        3*/
!        of oscillators, nt, based on shell data of carlson and the             !/*d14c        4*/
!        plasma frequency, pl, and log of the ionization potential, su.         !/*d14c        5*/
!        return the fitting parameter, wt, and minimum                          !/*d14c        6*/
!        normalized kinetic energy, tm, for use in den_el2.                     !/*d1x         2*/
!        this is eq. 10 of r. m. sternheimer, s. m. seltzer, and m. j.          !/*d14c        8*/
!        berger, "density effect for the ionization loss of charged             !/*d14c        9*/
!        particles in various substances", phys rev b, 26, 6067(1982).          !/*d14c       10*/
      use global_data                                                           !/*d1x         3*/
!                                                                               !/*d1x         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*d1x         5*/
      implicit integer(kindi) (i-n)                                             !/*d1x         6*/
!                                                                               !/*d1x         7*/
      dimension fo(1),eq(1),xq(1)                                               !/*d14c       12*/
!                                                                               !/*d14c       13*/
      nt=0                                                                      !/*d14c       14*/
      do 20 j=jmd(ljmd+mkc),jmd(ljmd+mkc+1)-1                                   !/*d14c       15*/
      iz=iza(liza+j)/1000                                                       !/*d14c       16*/
      ns=nxs(lnxs+11,lme(llme+3,j))                                             !/*d14c       17*/
      do 20 n=1,ns                                                              !/*d14c       18*/
      zs=exs(jxs(ljxs+11,lme(llme+3,j))+n-1)                                    !/*d14c       19*/
      nt=nt+1                                                                   !/*d14c       20*/
      fo(nt)=fme(lfme+j)*abs(zs)/az                                             !/*d14c       21*/
      if(jcond(ljco+mkc).ne.0.and.zs.le.0.)go to 10                             !/*d14c       22*/
      xq(nt)=(exs(jxs(ljxs+11,lme(llme+3,j))+ns+n-1)/pl)**2                     !/*d14c       23*/
      eq(nt)=2.*third*fo(nt)                                                    !/*d14c       24*/
      go to 20                                                                  !/*d14c       25*/
   10 xq(nt)=0.                                                                 !/*d14c       26*/
      eq(nt)=fo(nt)                                                             !/*d14c       27*/
   20 continue                                                                  !/*d14c       28*/
!                                                                               !/*d14c       29*/
!        find fitting parameter rho, termed wt here, following                  !/*d14c       30*/
!        sternheimer, berger and seltzer as implemented in its3.0.              !/*d14c       31*/
      mt=0                                                                      !/*d14c       32*/
      wt=2.                                                                     !/*d14c       33*/
   30 xo=wt                                                                     !/*d14c       34*/
      fu=0.                                                                     !/*d14c       35*/
      dr=0.                                                                     !/*d14c       36*/
      do 40 n=1,nt                                                              !/*d14c       37*/
      pa=wt*wt*xq(n)+eq(n)                                                      !/*d14c       38*/
      fu=fu+fo(n)*log(pa)                                                       !/*d14c       39*/
   40 dr=dr+fo(n)*2.*wt*xq(n)/pa                                                !/*d14c       40*/
      wt=wt-(fu-su)/dr                                                          !/*d14c       41*/
      mt=mt+1                                                                   !/*d14c       42*/
!                                                                               !/*d14c       43*/
!        set unconverged fitting parameter to arbitrarily small value.          !/*d14c       44*/
      if(mt.lt.25)go to 50                                                      !/*d14c       45*/
      call erprnt(1,2,0,0,0,0,0,0,                                              !/*d14c       46*/
     & '48helectron oscillator fitting convergence problem.')                   !/*d14c       47*/
      wt=1.e-4                                                                  !/*d14c       48*/
      go to 70                                                                  !/*d14c       49*/
   50 if(wt.gt.0.)go to 60                                                      !/*d14c       50*/
      wt=.5*xo                                                                  !/*d14c       51*/
      go to 30                                                                  !/*d14c       52*/
   60 if(abs(wt-xo).gt.1.e-6*xo)go to 30                                        !/*d14c       53*/
!                                                                               !/*d14c       54*/
!        fitting parameter, wt, accepted.                                       !/*d14c       55*/
   70 w2=wt**2                                                                  !/*d14c       56*/
      do 80 l=1,nt                                                              !/*d14c       57*/
      xq(l)=w2*xq(l)                                                            !/*d14c       58*/
   80 eq(l)=xq(l)+eq(l)                                                         !/*d14c       59*/
!                                                                               !/*d14c       60*/
!        find minimum normalized kinetic energy.                                !/*d14c       61*/
      tm=0.                                                                     !/*d14c       62*/
      if(jcond(ljco+mkc).gt.0)return                                            !/*d14c       63*/
      sm=0.                                                                     !/*d14c       64*/
      do 90 l=1,nt                                                              !/*d14c       65*/
   90 sm=sm+fo(l)/xq(l)                                                         !/*d14c       66*/
      bq=one/(one+sm)                                                           !/*d14c       67*/
      tm=one/sqrt(one-bq)-one                                                   !/*d14c       68*/
      return                                                                    !/*d14c       69*/
      end                                                                       !/*d14c       70*/
