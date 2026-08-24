!     $Id: opt14.F,v 1.4 2002/03/27 18:14:01 gwm Exp $                        !/*op          1*/
      subroutine opt14 (hold)                                                   !/*op          2*/
#include <htape3x/com32b.h>                                                     /*!op          3*/
#include <htape3x/label.h>                                                      /*!op          4*/
#include <htape3x/contrl.h>                                                     /*!op          5*/
#include <htape3x/bndy.h>                                                       /*!op          6*/
#include <htape3x/ctconv.h>                                                     /*!op          7*/
      dimension nc(8)                                                           !/*op          8*/
      data nc /0,1,1,0,0,1,0,0/                                                 !/*op          9*/
      if (ncol.ne.2.and.ncol.ne.3.and.ncol.ne.6) go to 100                      !/*op         10*/
   10 if (nparm.eq.0) go to 30                                                  !/*op         11*/
      if (jmat.eq.0) icomp=blz                                                  !/*op         12*/
      if (jmat.ne.0) icomp=mat                                                  !/*op         13*/
      do 20 ib=1,nparm                                                          !/*op         14*/
      if (lparm(ib).eq.icomp) go to 40                                          !/*op         15*/
   20 continue                                                                  !/*op         16*/
      go to 100                                                                 !/*op         17*/
   30 ib=0                                                                      !/*op         18*/
   40 continue                                                                  !/*op         19*/
      if (lbunch) ib=0                                                          !/*op         20*/
      if (ncol.ne.3) go to 50                                                   !/*op         21*/
!------ charged particle slowed to cutoff                                       !/*op         22*/
      ipt=tip                                                                   !/*op         23*/
      if (ipt.eq.0.or.(ipt.ge.7.and.ipt.le.10)) then                            !/*op         24*/
        if (ipt.eq.0) ipt=6                                                     !/*op         25*/
        ipt=ipt-5                                                               !/*op         26*/
        call accum (hold,nbin,ipt,ib,wt)                                        !/*op         27*/
      endif                                                                     !/*op         28*/
      go to 100                                                                 !/*op         29*/
!------ nuclear interaction or absorption                                       !/*op         30*/
   50 if (nbelo.le.0) go to 70                                                  !/*op         31*/
!------ pick up any below cutoff particles                                      !/*op         32*/
      do 60 ibelo=1,nbelo                                                       !/*op         33*/
      ipt=tipb(ibelo)                                                           !/*op         34*/
      if (ipt.eq.0.or.(ipt.ge.7.and.ipt.le.10)) then                            !/*op         35*/
        if (ipt.eq.0) ipt=6                                                     !/*op         36*/
        ipt=ipt-5                                                               !/*op         37*/
        call accum (hold,nbin,ipt,ib,wt)                                        !/*op         38*/
      endif                                                                     !/*op         39*/
   60 continue                                                                  !/*op         40*/
   70 continue                                                                  !/*op         41*/
  100 call srchn (nc)                                                           !/*op         42*/
      if (nocas.ne.nocas0.or.ncol.le.0) return                                  !/*op         43*/
      go to 10                                                                  !/*op         44*/
      end                                                                       !/*op         45*/
