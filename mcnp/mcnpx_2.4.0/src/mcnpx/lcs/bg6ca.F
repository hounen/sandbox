!     $Id: bg6ca.F,v 1.4 2002/03/27 18:14:55 gwm Exp $                        !/*bg          1*/
      subroutine bg6ca (k,l)                                                    !/*bg          2*/
#include <lcs/double.h>                                                         /*!bg          3*/
#include <lcs/btcom.h>                                                          /*!bg          4*/
      clsm=k                                                                    !/*bg          5*/
      if (in1) 20,10,20                                                         !/*bg          6*/
   10 curr(10)=clsm                                                             !/*bg          7*/
      curr(11)=clsm                                                             !/*bg          8*/
!     collision med. stored in region of initial coll. and                      !/*bg          9*/
!     medium where particle was born                                            !/*bg         10*/
   20 efrp=space(k+9)-7.d0                                                      !/*bg         11*/
!     proton well depth + binding energy=fermi energy protons--mev              !/*bg         12*/
      efrn=space(k+3)-7.d0                                                      !/*bg         13*/
      pm(2)=dncms                                                               !/*bg         14*/
!     nucleon mass per cm                                                       !/*bg         15*/
      pm(1)=dncms                                                               !/*bg         16*/
      if (k-l) 30,40,50                                                         !/*bg         17*/
   30 pm(1)=poms                                                                !/*bg         18*/
!     pi0 mass per cm                                                           !/*bg         19*/
      go to 50                                                                  !/*bg         20*/
   40 pm(1)=pnms                                                                !/*bg         21*/
!     pi(+or-) mass per cm                                                      !/*bg         22*/
   50 return                                                                    !/*bg         23*/
      end                                                                       !/*bg         24*/
