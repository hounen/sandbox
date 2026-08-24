!     $Id: iptx.F,v 1.4 2002/03/27 18:16:22 gwm Exp $                         !/*ip          1*/
      function iptx (iza,npt,irefx)                                             !/*ip          2*/
#include <htape3x/com32b.h>                                                     /*!ip          3*/
#include <trx/ioput.h>                                                          /*!ip          4*/
#include <htape3x/big.h>                                                        /*!ip          5*/
#include <trx/gross.h>                                                          /*!ip          6*/
      iz=iza/1000                                                               !/*ip          7*/
      iref1=iref(iz)                                                            !/*ip          8*/
      iref2=iref(iz+1)-1                                                        !/*ip          9*/
      do 10 i=iref1,iref2                                                       !/*ip         10*/
      if (iza.eq.izao(i)) go to 20                                              !/*ip         11*/
   10 continue                                                                  !/*ip         12*/
      write (nout,30) iza                                                       !/*ip         13*/
      write (nout2,30) iza                                                      !/*ip         14*/
      iptx=0                                                                    !/*ip         15*/
      return                                                                    !/*ip         16*/
   20 continue                                                                  !/*ip         17*/
      iptx=ipt(i)                                                               !/*ip         18*/
      npt=ipt(i+1)-iptx                                                         !/*ip         19*/
      irefx=i                                                                   !/*ip         20*/
      return                                                                    !/*ip         21*/
!                                                                               !/*ip         22*/
   30 format (' nuclide',i7,' not found')                                       !/*ip         23*/
      end                                                                       !/*ip         24*/
