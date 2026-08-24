!     $Id: ifrist.F,v 1.5 2002/03/27 18:15:07 gwm Exp $                       !/*if          1*/
      function ifrist (type,iloc,dens,ind32)                                    !/*if          2*/
#include <lcs/double.h>                                                         /*!if          3*/
!     ==================================================================        !/*if          4*/
!     ifrist calculates whether a collision with a nucleon is                   !/*if          5*/
!     forbidden now                                                             !/*if          6*/
!                                                                               !/*if          7*/
! IFRIST calls   SQRT                                                           !/*if          8*/
! called by AINTER  PSEL                                                        !/*if          9*/
!     ==================================================================        !/*if         10*/
!                                                                               !/*if         11*/
#include <lcs/vegas.h>                                                          /*!if         12*/
      ifrist=0                                                                  !/*if         13*/
!                                                                               !/*if         14*/
!      ifrist .eq. 0 ==> no restrictions occured                                !/*if         15*/
!             .gt. 1 ==>  a restriction was encountered                         !/*if         16*/
!                                                                               !/*if         17*/
      itype=type                                                                !/*if         18*/
      itloc=abs(ws(2))                                                          !/*if         19*/
!  that could be the incident particle                                          !/*if         20*/
      if (itloc.eq.5.and.st(iloc+i81).gt.dp0) return                            !/*if         21*/
      itloc=itloc+iloc+12                                                       !/*if         22*/
      iprev=st(itloc)/1.d+02                                                    !/*if         23*/
      ws15=st(itloc)-iprev*100                                                  !/*if         24*/
      delt=ws(19)-ws15                                                          !/*if         25*/
      ipre=iprev/10                                                             !/*if         26*/
      if (ipre.eq.ind32) return                                                 !/*if         27*/
      iprv=iprev-ipre*10                                                        !/*if         28*/
      if (ind32.eq.1) go to 20                                                  !/*if         29*/
!  the previous interaction was with a projectile partner                       !/*if         30*/
      if (iprv.gt.2) iprv=iprv-2                                                !/*if         31*/
      iknp=abs(knp)                                                             !/*if         32*/
      if (knp.lt.0) go to 10                                                    !/*if         33*/
      if (iprv.ne.itype) return                                                 !/*if         34*/
   10 ws15=st(itloc)-iprev*100                                                  !/*if         35*/
      zold=st(itloc+3)+dp10*voc*(ws(19)-ws15)                                   !/*if         36*/
      gamm=sqrt(dp1-voc*voc)                                                    !/*if         37*/
      d1=st(itloc+1)-gws(10)                                                    !/*if         38*/
      d2=st(itloc+2)-gws(11)                                                    !/*if         39*/
      d3=zold-gws(12)                                                           !/*if         40*/
      d3=d3/gamm                                                                !/*if         41*/
      d=d1*d1+d2*d2+d3*d3                                                       !/*if         42*/
      go to 40                                                                  !/*if         43*/
!  the previous interaction was with a target partner                           !/*if         44*/
   20 if (ipre.gt.1) iprv=(iprv+1)/2                                            !/*if         45*/
      if (iprv.ne.itype) return                                                 !/*if         46*/
      d=dp0                                                                     !/*if         47*/
      do 30 i=1,3                                                               !/*if         48*/
      k=itloc+i                                                                 !/*if         49*/
      j=9+i                                                                     !/*if         50*/
   30 d=d+(st(k)-gws(j))*(st(k)-gws(j))                                         !/*if         51*/
   40 d=sqrt(d)                                                                 !/*if         52*/
      if (iknp-2) 50,60,70                                                      !/*if         53*/
!  the "holes" are filled as particles with                                     !/*if         54*/
!  fermi velocity move in.                                                      !/*if         55*/
!  fermi velocity is .258, (10*.258)**3=17.172                                  !/*if         56*/
!                                                                               !/*if         57*/
!  the restriction is 1.1 f  (default value of "gapl" )                         !/*if         58*/
   50 if (delt.gt.0.4496) return                                                !/*if         59*/
      if (d.lt.gapl) ifrist=2                                                   !/*if         60*/
      return                                                                    !/*if         61*/
!  the restriction is v*dens=1                                                  !/*if         62*/
   60 dcubed=dp1/(4.1887902d0*dens)                                             !/*if         63*/
      if (delt*delt*delt.gt.(dcubed/17.172d0)) return                           !/*if         64*/
      if (d*d*d.lt.dcubed) ifrist=2                                             !/*if         65*/
      return                                                                    !/*if         66*/
!  the restriction is 1/cubert(density)                                         !/*if         67*/
   70 dcubed=dp1/dens                                                           !/*if         68*/
      if (delt*delt*delt.gt.(dcubed/17.172d0)) return                           !/*if         69*/
      if (d*d*d.lt.dcubed) ifrist=2                                             !/*if         70*/
      return                                                                    !/*if         71*/
      end                                                                       !/*if         72*/
