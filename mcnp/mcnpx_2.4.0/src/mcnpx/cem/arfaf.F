      subroutine arfaf (per, ami)                                               !/*ar          1*/
#ifdef CEM                                                                      /*!ar          2*/
! ======================================================================        !/*ar          3*/
!                                                                               !/*ar          4*/
!   Renormalization of emission probabilities.                                  !/*ar          5*/
!   (due to small exponent size in real [32-bit] numbers)                       !/*ar          6*/
!                                                                               !/*ar          7*/
!   CEM95 written by S. G. Mashnik                                              !/*ar          8*/
!                                                                               !/*ar          9*/
!   Edited by A. J. Sierk,  LANL T-2, February, 1996.                           !/*ar         10*/
!   Edited by F. Gallmeier, ORNL CPED, October 1998.                            !/*ar         11*/
!                                                                               !/*ar         12*/
! ======================================================================        !/*ar         13*/
#include <cem/cdouble.h>                                                        /*!ar         14*/
      dimension ami(6)                                                          !/*ar         15*/
      common /adbf/ amf, ijsp, nhump, r0m                                       !/*ar         16*/
      common /bl1009/ afj(7)                                                    !/*ar         17*/
      common /bl1015/ rj(7)                                                     !/*ar         18*/
! ======================================================================        !/*ar         19*/
      sfix = dp30                                                               !/*ar         20*/
      smx = dp0                                                                 !/*ar         21*/
        do 10 k = 1,7                                                           !/*ar         22*/
        if (rj(k).le.dp0) then                                                  !/*ar         23*/
          q8 = dp0                                                              !/*ar         24*/
        else                                                                    !/*ar         25*/
          if (k.eq.7) then                                                      !/*ar         26*/
            amm = amf                                                           !/*ar         27*/
          else                                                                  !/*ar         28*/
            amm = ami(k)                                                        !/*ar         29*/
          endif                                                                 !/*ar         30*/
          q8 = dp2*sqrt(amm*afj(k)*rj(k))                                       !/*ar         31*/
        endif                                                                   !/*ar         32*/
        smx = max(smx, q8)                                                      !/*ar         33*/
   10   continue                                                                !/*ar         34*/
      if (smx.le.sfix) then                                                     !/*ar         35*/
        per = dp0                                                               !/*ar         36*/
      else                                                                      !/*ar         37*/
        per = smx - sfix                                                        !/*ar         38*/
      endif                                                                     !/*ar         39*/
#endif                                                                          /*!ar         40*/
      return                                                                    !/*ar         41*/
      end                                                                       !/*ar         42*/
