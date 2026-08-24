!#ident den2.F from deck d24c                                                   !/*d24c        1*/
      subroutine den_el2(s,tm,fo,eq,xq,nt,wt,d)                                 !/*d2x         1*/
!        calculate the density effect, d, based on fitting parameter, wt        !/*d24c        3*/
!        and oscillator parameters fo, eq, xq, at normalized energy, s          !/*d24c        4*/
!        greater than tm.  d=zero otherwise.                                    !/*d24c        5*/
!        this routine solves eqs. (5) and (6) of sternheimer (1982).            !/*d24c        6*/
      use global_data                                                           !/*d2x         2*/
!                                                                               !/*d2x         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*d2x         4*/
      implicit integer(kindi) (i-n)                                             !/*d2x         5*/
!                                                                               !/*d2x         6*/
      dimension fo(1),eq(1),xq(1)                                               !/*d24c        8*/
!                                                                               !/*d24c        9*/
      d=0.                                                                      !/*d24c       10*/
      if(s.le.tm)return                                                         !/*d24c       11*/
      mt=0                                                                      !/*d24c       12*/
      x=s*(s+2.)                                                                !/*d24c       13*/
      re=1./x                                                                   !/*d24c       14*/
   10 xo=x                                                                      !/*d24c       15*/
      fn=0.                                                                     !/*d24c       16*/
      dv=0.                                                                     !/*d24c       17*/
      do 20 n=1,nt                                                              !/*d24c       18*/
      fn=fn+fo(n)/(xq(n)+x)                                                     !/*d24c       19*/
   20 dv=dv-fo(n)/((xq(n)+x)**2)                                                !/*d24c       20*/
      x=x-(fn-re)/dv                                                            !/*d24c       21*/
      mt=mt+1                                                                   !/*d24c       22*/
!                                                                               !/*d24c       23*/
!        set unconverged fitting parameter, d, to zero.                         !/*d24c       24*/
      if(mt.lt.25)go to 30                                                      !/*d24c       25*/
      call erprnt(1,2,0,0,0,0,0,0,                                              !/*d24c       26*/
     & '50hunconverged density effect correction set to zero.')                 !/*d24c       27*/
      d=0.                                                                      !/*d24c       28*/
      return                                                                    !/*d24c       29*/
   30 if(x.ge.0.)go to 40                                                       !/*d24c       30*/
      x=.5*xo                                                                   !/*d24c       31*/
      go to 10                                                                  !/*d24c       32*/
   40 if(abs(x-xo).gt.1.e-6*xo)go to 10                                         !/*d24c       33*/
!                                                                               !/*d24c       34*/
!        fitting parameter, x, accepted. get density effect, d.                 !/*d24c       35*/
   50 d=-x/(s+1.)**2                                                            !/*d24c       36*/
      do 60 l=1,nt                                                              !/*d24c       37*/
   60 d=d+fo(l)*log(one+x/eq(l))                                                !/*d24c       38*/
      return                                                                    !/*d24c       39*/
      end                                                                       !/*d24c       40*/
