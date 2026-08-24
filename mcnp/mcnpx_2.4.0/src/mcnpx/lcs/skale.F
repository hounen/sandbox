!     $Id: skale.F,v 1.4 2002/03/27 18:15:24 gwm Exp $                        !/*sk          1*/
      subroutine skale (f,nofas,itslo,efas,alpfas,betfas,gamfas,ehicut          !/*sk          2*/
     & ,wtfas,rmfas,exfas,refas)                                                !/*sk          3*/
! SKALE        calls...  BERT  ESKALE  MCMOSC  BTBD                             !/*sk          4*/
#include <lcs/double.h>                                                         /*!sk          5*/
      external hebd                                                             !/*sk          6*/
      dimension f(7), itslo(1), efas(1), alpfas(1), betfas(1), gamfas(1)        !/*sk          7*/
     & , wtfas(1)                                                               !/*sk          8*/
#include <lcs/l05S.h>                                                           /*!sk          9*/
      dimension eslo(l05), alpslo(l05), betslo(l05), gamslo(l05)                !/*sk         10*/
      data jelas /1/                                                            !/*sk         11*/
      nct=1                                                                     !/*sk         12*/
   10 estor=f(3)                                                                !/*sk         13*/
      f(3)=ehicut                                                               !/*sk         14*/
      call bert (f(1),noslo,itslo,eslo,alpslo,betslo,gamslo)                    !/*sk         15*/
      f(3)=estor                                                                !/*sk         16*/
      if (noslo.gt.0) go to 30                                                  !/*sk         17*/
      nofas=noslo                                                               !/*sk         18*/
   20 return                                                                    !/*sk         19*/
   30 if (jelas.eq.0) go to 40                                                  !/*sk         20*/
      call eskale (ie,ehicut,f,nofas,itslo,efas,alpfas,betfas,gamfas            !/*sk         21*/
     & ,wtfas,rmfas,exfas,refas,noslo,eslo,alpslo,betslo,gamslo)                !/*sk         22*/
      if (ie-2) 40,10,20                                                        !/*sk         23*/
   40 atar=f(1)                                                                 !/*sk         24*/
      einc=f(3)                                                                 !/*sk         25*/
      itinc=f(7)                                                                !/*sk         26*/
      call mcmosc (atar,itinc,einc,ehicut,noslo,itslo,eslo,alpslo,betslo        !/*sk         27*/
     & ,gamslo,efas,wtfas,alpfas,betfas,gamfas,rmfas,nofas,exfas,refas          !/*sk         28*/
     & ,why)                                                                    !/*sk         29*/
      if (nofas.eq.0) go to 50                                                  !/*sk         30*/
      return                                                                    !/*sk         31*/
   50 nct=nct+1                                                                 !/*sk         32*/
      go to 10                                                                  !/*sk         33*/
      end                                                                       !/*sk         34*/
