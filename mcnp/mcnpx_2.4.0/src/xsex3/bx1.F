!     $Id: bx1.F,v 1.6 2002/03/27 18:16:23 gwm Exp $                          !/*bx          1*/
      subroutine bx1 (hold,ndim1,ndim2,ergb,xmu,itype,ielstc)                   !/*bx          2*/
! binning routine for program XSEX                                              !/*bx          3*/
#include <htape3x/com32b.h>                                                     /*!bx          4*/
#include <htape3x/label.h>                                                      /*!bx          5*/
#include <htape3x/contrl.h>                                                     /*!bx          6*/
#include <htape3x/exm.h>                                                        /*!bx          7*/
#include <xsex3/charxs.h>                                                       /*!bx          8*/
      dimension hold(ndim1,ndim2,1), ergb(1), xmu(1), itype(1), ikb(lpt)        !/*bx          9*/
      data ikb /1,2,3,4,5,0,0,6,7,8,9,10,11,12,12,13,14,15,4*0/                 !/*bx         10*/
      nhi=nabov+nbelo                                                           !/*bx         11*/
      if (nopart.lt.0.or.nhi.le.0) go to 60                                     !/*bx         12*/
      do 50 j=1,nhi                                                             !/*bx         13*/
      if (j.gt.nabov) then                                                      !/*bx         14*/
        i=l02+j-nabov                                                           !/*bx         15*/
      else                                                                      !/*bx         16*/
        i=j                                                                     !/*bx         17*/
      endif                                                                     !/*bx         18*/
      ii=tipa(i)+1.0                                                            !/*bx         19*/
      if (itype(ii).eq.0) go to 50                                              !/*bx         20*/
      if (ielstc.eq.1) then                                                     !/*bx         21*/
        ipt=lpt1                                                                !/*bx         22*/
      else                                                                      !/*bx         23*/
        ipt=ikb(ii)                                                             !/*bx         24*/
      endif                                                                     !/*bx         25*/
      if (ipt.eq.0) go to 50                                                    !/*bx         26*/
      temp=ea(i)                                                                !/*bx         27*/
      if (imom.ne.0) then                                                       !/*bx         28*/
        temp=sqrt(temp*(temp+2.*pmass(ii)))                                     !/*bx         29*/
      endif                                                                     !/*bx         30*/
      do 10 ie=1,nerg                                                           !/*bx         31*/
      if (temp.le.ergb(ie)) go to 20                                            !/*bx         32*/
   10 continue                                                                  !/*bx         33*/
      ie=nerg+1                                                                 !/*bx         34*/
   20 continue                                                                  !/*bx         35*/
      do 30 it=1,nang                                                           !/*bx         36*/
      test=wa(i)                                                                !/*bx         37*/
#ifdef CHEAP                                                                    /*!bx         38*/
      if (test.gt.0.999d0) test=sqrt(dp1-ua(i)**2-va(i)**2)                     !/*bx         39*/
#endif                                                                          /*!bx         40*/
      if (test.le.xmu(it)) go to 40                                             !/*bx         41*/
   30 continue                                                                  !/*bx         42*/
      it=nang                                                                   !/*bx         43*/
   40 continue                                                                  !/*bx         44*/
      del=geomxs*wta(i)                                                         !/*bx         45*/
      hold(it,ie,ipt)=hold(it,ie,ipt)+del                                       !/*bx         46*/
      hold(nang+1,ie,ipt)=hold(nang+1,ie,ipt)+del                               !/*bx         47*/
      hold(it,nerg+2,ipt)=hold(it,nerg+2,ipt)+del                               !/*bx         48*/
      hold(nang+1,nerg+2,ipt)=hold(nang+1,nerg+2,ipt)+del                       !/*bx         49*/
   50 continue                                                                  !/*bx         50*/
   60 continue                                                                  !/*bx         51*/
      return                                                                    !/*bx         52*/
      end                                                                       !/*bx         53*/
