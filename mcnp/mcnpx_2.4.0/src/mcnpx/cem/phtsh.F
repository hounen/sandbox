      subroutine phtsh(a, z, ue, tres, nnph, nf)                                !/*ph          1*/
#ifdef CEM                                                                      /*!ph          2*/
! ======================================================================        !/*ph          3*/
!                                                                               !/*ph          4*/
!     Transfers CEM-data and provides LCS-data for the call of PHT              !/*ph          5*/
!     Decouples CEM from LCS and PHT                                            !/*ph          6*/
!                                                                               !/*ph          7*/
!     Called by: precof                                                         !/*ph          8*/
!                                                                               !/*ph          9*/
!     Calls: runpht                                                             !/*ph         10*/
!                                                                               !/*ph         11*/
!     Edited by F.X. Gallmeier,  ORNL CPED,  June 1998, Feb. 1999.              !/*ph         12*/
!                                                                               !/*ph         13*/
! ======================================================================        !/*ph         14*/
!                                                                               !/*ph         15*/
#include <cem/cdouble.h>                                                        /*!ph         16*/
#include <lcs/lptS.h>                                                           /*!ph         17*/
#include <lcs/rucom.h>                                                          /*!ph         18*/
#include <lcs/inout.h>                                                          /*!ph         19*/
!                                                                               !/*ph         20*/
! ======================================================================        !/*ph         21*/
!                                                                               !/*ph         22*/
      if(nf.lt.2) mnuc=0                                                        !/*ph         23*/
      nph = nnph                                                                !/*ph         24*/
      ecv = dp0                                                                 !/*ph         25*/
      uee=dp0                                                                   !/*ph         26*/
      if (ue.gt.dp100) then                                                     !/*ph         27*/
         uee=ue                                                                 !/*ph         28*/
         ue=dp100                                                               !/*ph         29*/
      endif                                                                     !/*ph         30*/
      call runpht(z,a,ue,ecv,eph,elv,tph,nph,ipht,icc,maxph)                    !/*ph         31*/
      if (uee.gt.dp100) then                                                    !/*ph         32*/
         ue=uee-dp100                                                           !/*ph         33*/
      endif                                                                     !/*ph         34*/
      if (nf.le.3) then                                                         !/*ph         35*/
         mnuc   =mnuc+1                                                         !/*ph         36*/
         zprr(mnuc)  = z                                                        !/*ph         37*/
         aprr(mnuc)  = a                                                        !/*ph         38*/
!     ???                                                                       !/*ph         39*/
         erecr(mnuc) = tres                                                     !/*ph         40*/
         uur(mnuc)   = ue                                                       !/*ph         41*/
         ecvr(mnuc)  = ecv                                                      !/*ph         42*/
!fcg        u_(mnuc) = coslbr(1)                                                !/*ph         43*/
!fcg        v_(mnuc) = coslbr(2)                                                !/*ph         44*/
!fcg        w_(mnuc) = coslbr(3)                                                !/*ph         45*/
         nnph = nph                                                             !/*ph         46*/
      else                                                                      !/*ph         47*/
         write(io,*)                                                            !/*ph         48*/
     &     'warning (PHTSH): number of residual nuclei exceeds 3.'              !/*ph         49*/
         write(itty,*)                                                          !/*ph         50*/
     &     'warning (PHTSH): number of residual nuclei exceeds 3.'              !/*ph         51*/
      endif                                                                     !/*ph         52*/
#endif                                                                          /*!ph         53*/
      return                                                                    !/*ph         54*/
      end                                                                       !/*ph         55*/
