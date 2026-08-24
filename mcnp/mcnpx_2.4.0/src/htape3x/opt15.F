!     $Id: opt15.F,v 1.4 2002/03/27 18:14:01 gwm Exp $                        !/*op          1*/
      subroutine opt15 (tlly,hold,tlsq,mxz0,mxn0)                               !/*op          2*/
#include <htape3x/com32b.h>                                                     /*!op          3*/
#include <htape3x/label.h>                                                      /*!op          4*/
#include <htape3x/contrl.h>                                                     /*!op          5*/
      dimension hold(mxn0,mxz0,1), tlly(mxn0,mxz0,1), tlsq(mxn0,mxz0,1)         !/*op          6*/
      dimension nc4(8)                                                          !/*op          7*/
      dimension flz(6), fla(6)                                                  !/*op          8*/
      dimension ila(lpt), ilz(lpt)                                              !/*op          9*/
      data flz /0.,1.,1.,1.,2.,2./, fla /1.,1.,2.,3.,3.,4./                     !/*op         10*/
      data ila /2*dp1,5*dp0,dp2,2*dp3,dp4,5*dp0,2*-1.d0,4*dp0/                  !/*op         11*/
      data ilz /dp1,dp0,dp1,dp0,-1.d0,dp1,-1.d0,2*dp1,2*dp2,dp0,dp1,            !/*op         12*/
     &2*dp0,2*-1.d0,dp0,-1.d0,dp1,2*dp0/                                        !/*op         13*/
      data nc4 /0,1,0,0,0,1,0,0/                                                !/*op         14*/
      if (ncol.ne.2.and.ncol.ne.6) go to 110                                    !/*op         15*/
   10 continue                                                                  !/*op         16*/
      if (ifeng.and.(name.ne.1)) go to 110                                      !/*op         17*/
      if (nparm.le.0) go to 30                                                  !/*op         18*/
      if (lelem.eq.-1) go to 110                                                !/*op         19*/
      if (jmat.eq.0) icomp=blz                                                  !/*op         20*/
      if (jmat.ne.0) icomp=mat                                                  !/*op         21*/
      do 20 ib=1,nparm                                                          !/*op         22*/
      if (lparm(ib).eq.icomp) go to 40                                          !/*op         23*/
   20 continue                                                                  !/*op         24*/
      go to 110                                                                 !/*op         25*/
   30 continue                                                                  !/*op         26*/
      ib=1                                                                      !/*op         27*/
   40 continue                                                                  !/*op         28*/
      if (lbunch) ib=1                                                          !/*op         29*/
      if (kopt.gt.0) call recels (ielstc)                                       !/*op         30*/
!     IELSTC=0 for non-elastic                                                  !/*op         31*/
!     IELSTC=1 for elastic                                                      !/*op         32*/
      if (kopt.eq.1.and.ielstc.eq.0) go to 110                                  !/*op         33*/
      if (kopt.eq.2.and.ielstc.ne.0) go to 110                                  !/*op         34*/
      ia=a(mel(nmed)+lelem-1)                                                   !/*op         35*/
      iz=zz(mel(nmed)+lelem-1)                                                  !/*op         36*/
      if (ia.eq.0.and.iz.eq.0) go to 110                                        !/*op         37*/
      inn=ia-iz+1                                                               !/*op         38*/
      iz=iz+1                                                                   !/*op         39*/
      if (iz.ge.mxz0.or.inn.ge.mxn0) go to 120                                  !/*op         40*/
      hold(inn,iz,ib)=hold(inn,iz,ib)+wt                                        !/*op         41*/
      hold(mxn0,iz,ib)=hold(mxn0,iz,ib)+wt                                      !/*op         42*/
      hold(inn,mxz0,ib)=hold(inn,mxz0,ib)+wt                                    !/*op         43*/
      hold(mxn0,mxz0,ib)=hold(mxn0,mxz0,ib)+wt                                  !/*op         44*/
  110 continue                                                                  !/*op         45*/
      call srchn (nc4)                                                          !/*op         46*/
      if (nocas.eq.nocas0.and.ncol.gt.0) go to 10                               !/*op         47*/
      go to 130                                                                 !/*op         48*/
  120 continue                                                                  !/*op         49*/
      ihiaz=ihiaz+1                                                             !/*op         50*/
      go to 110                                                                 !/*op         51*/
  130 continue                                                                  !/*op         52*/
      return                                                                    !/*op         53*/
!                                                                               !/*op         54*/
      end                                                                       !/*op         55*/
