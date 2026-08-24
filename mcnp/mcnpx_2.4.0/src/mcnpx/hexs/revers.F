!     $Id: revers.F,v 1.4 2002/03/27 18:14:50 gwm Exp $                       !/*re          1*/
      subroutine revers (bet0,gam0)                                             !/*re          2*/
#include <lcs/double.h>                                                         /*!re          3*/
#include <lcs/lptS.h>                                                           /*!re          4*/
#include <lcs/rucom.h>                                                          /*!re          5*/
#include <lcs/cacom.h>                                                          /*!re          6*/
#include <lcs/banka.h>                                                          /*!re          7*/
!     ************************************************************              !/*re          8*/
      if (nopart.lt.0) then                                                     !/*re          9*/
        call errorf ('revers.1')                                                !/*re         10*/
      endif                                                                     !/*re         11*/
      if (apr.gt.dp0) then                                                      !/*re         12*/
!       if (apr.eq.dp1) xm=pmass(1)                                             !/*re         13*/
!       if (apr.eq.dp2) xm=pmass(8)                                             !/*re         14*/
!       if (apr.eq.dp4) xm=pmass(11)                                            !/*re         15*/
!       if (apr.eq.dp3.and.zpr.eq.dp1) xm=pmass(9)                              !/*re         16*/
!       if (apr.eq.dp3.and.zpr.eq.dp2) xm=pmass(10)                             !/*re         17*/
        xm=xmnuc (apr,zpr)                                                      !/*re         18*/
        xm=xm+ex                                                                !/*re         19*/
        pmom=sqrt(erec*(erec+dp2*xm))                                           !/*re         20*/
        unew=pmom*coslbr(1)                                                     !/*re         21*/
        vnew=-pmom*coslbr(2)                                                    !/*re         22*/
        wnew=-gam0*pmom*coslbr(3)+bet0*gam0*(erec+xm)                           !/*re         23*/
        dnom=sqrt(unew**2+vnew**2+wnew**2)                                      !/*re         24*/
        erec=gam0*erec+(gam0-dp1)*xm-bet0*gam0*pmom*coslbr(3)                   !/*re         25*/
        coslbr(1)=unew/dnom                                                     !/*re         26*/
        coslbr(2)=vnew/dnom                                                     !/*re         27*/
        coslbr(3)=wnew/dnom                                                     !/*re         28*/
      endif                                                                     !/*re         29*/
      if (nopart.eq.0) return                                                   !/*re         30*/
      do 10 i=1,nopart                                                          !/*re         31*/
      ipt=kind(i)+1                                                             !/*re         32*/
      xm=pmass(ipt)                                                             !/*re         33*/
      pmom=sqrt(ep(i)*(ep(i)+dp2*xm))                                           !/*re         34*/
      unew=pmom*alpha(i)                                                        !/*re         35*/
      vnew=-pmom*beta(i)                                                        !/*re         36*/
      wnew=-gam0*pmom*gam(i)+bet0*gam0*(ep(i)+xm)                               !/*re         37*/
      dnom=sqrt(unew**2+vnew**2+wnew**2)                                        !/*re         38*/
      ep(i)=gam0*ep(i)+(gam0-dp1)*xm-bet0*gam0*pmom*gam(i)                      !/*re         39*/
      alpha(i)=unew/dnom                                                        !/*re         40*/
      beta(i)=vnew/dnom                                                         !/*re         41*/
      gam(i)=wnew/dnom                                                          !/*re         42*/
   10 continue                                                                  !/*re         43*/
      return                                                                    !/*re         44*/
      end                                                                       !/*re         45*/
