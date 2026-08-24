!     $Id: bekeka.F,v 1.4 2002/03/27 18:14:39 gwm Exp $                       !/*be          1*/
      double precision function bekeka (ix,to,amss,sqamss)                      !/*be          2*/
!                                                                     *         !/*be          3*/
!---------------------------------------------------------------------*         !/*be          4*/
!                                                                     *         !/*be          5*/
#include <lcs/double.h>                                                         /*!be          6*/
#include <fluka89/flprm1.h>                                                     /*!be          7*/
#include <fluka89/flprm2.h>                                                     /*!be          8*/
#include <lcs/inout.h>                                                          /*!be          9*/
!*** calculation of average cascade and excitation energy                       !/*be         10*/
!*****ix=1 eev    2epk    3 enk     4 eex=epk+eev    5 eext=eex+enk             !/*be         11*/
      go to (10,50,70,10,10), ix                                                !/*be         12*/
   10 continue                                                                  !/*be         13*/
      if (to-0.1d0) 20,20,30                                                    !/*be         14*/
   20 continue                                                                  !/*be         15*/
      aa=0.001d0*sqamss                                                         !/*be         16*/
      go to 40                                                                  !/*be         17*/
   30 continue                                                                  !/*be         18*/
      apar=0.035d0                                                              !/*be         19*/
      bpar=3.d0                                                                 !/*be         20*/
      cpar=0.1d0                                                                !/*be         21*/
      aa=cpar*sqamss*(0.01d0+apar*(bpar+log10(to))**2)                          !/*be         22*/
   40 continue                                                                  !/*be         23*/
      if (ix.gt.3) go to 50                                                     !/*be         24*/
!     energyadjust                                                              !/*be         25*/
      enadj=1.d0                                                                !/*be         26*/
      aa=aa*enadj                                                               !/*be         27*/
      bekeka=aa                                                                 !/*be         28*/
      return                                                                    !/*be         29*/
   50 continue                                                                  !/*be         30*/
      an=bnkeka(1,to,amss,sqamss)                                               !/*be         31*/
      a=bkeka(1,to,amss)                                                        !/*be         32*/
      extoa=0.d0                                                                !/*be         33*/
      if (to.lt.5.d0*a) extoa=exp(-to/a)                                        !/*be         34*/
      tpkav=a*(1.d0-(to/a+1.)*extoa)/(1.d0-extoa)                               !/*be         35*/
      bb=tpkav*an                                                               !/*be         36*/
      if (ix.eq.4) go to 60                                                     !/*be         37*/
      if (ix.eq.5) go to 70                                                     !/*be         38*/
      bekeka=bb                                                                 !/*be         39*/
      return                                                                    !/*be         40*/
   60 continue                                                                  !/*be         41*/
      bekeka=aa+bb                                                              !/*be         42*/
      return                                                                    !/*be         43*/
   70 continue                                                                  !/*be         44*/
      an=bnkeka(2,to,amss,sqamss)                                               !/*be         45*/
      a=bkeka(2,to,amss)                                                        !/*be         46*/
      extoa=0.d0                                                                !/*be         47*/
      if (to.lt.5.d0*a) extoa=exp(-to/a)                                        !/*be         48*/
      tnkav=a*(1.d0-(to/a+1.)*extoa)/(1.d0-extoa)                               !/*be         49*/
      cc=tnkav*an                                                               !/*be         50*/
      if (ix.eq.5) go to 80                                                     !/*be         51*/
      bekeka=cc                                                                 !/*be         52*/
      return                                                                    !/*be         53*/
   80 continue                                                                  !/*be         54*/
      bekeka=aa+bb+cc                                                           !/*be         55*/
      return                                                                    !/*be         56*/
      end                                                                       !/*be         57*/
