      subroutine chabs (l, ine, ne1, ne2, a, z)                                 !/*ch          1*/
#ifdef CEM                                                                      /*!ch          2*/
! ======================================================================        !/*ch          3*/
!                                                                               !/*ch          4*/
!     Determines isospin (p or n) of nucleons after pion absorption.            !/*ch          5*/
!     This modified version keeps track of the isospin of the original          !/*ch          6*/
!     first nucleon partner.                                                    !/*ch          7*/
!                                                                               !/*ch          8*/
!   Called by: ABSORP                                                           !/*ch          9*/
!                                                                               !/*ch         10*/
!       ine; charge of incident pion                                            !/*ch         11*/
!       ne1; upon entry: charge of first nucleon partner;                       !/*ch         12*/
!       ne1, ne2; upon return: the charges of the two outgoing nucleons.        !/*ch         13*/
!       l;   (always = 0!!)                                                     !/*ch         14*/
!                                                                               !/*ch         15*/
!   CEM95 written by S. G. Mashnik                                              !/*ch         16*/
!                                                                               !/*ch         17*/
!   Edited by A. J. Sierk  LANL  T-2  February, 1996.                           !/*ch         18*/
!   Modified  from old CHABS by AJS  July, 1997                                 !/*ch         19*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*ch         20*/
!                                                                               !/*ch         21*/
! ======================================================================        !/*ch         22*/
#include <cem/cdouble.h>                                                        /*!ch         23*/
      rn = rang()                                                               !/*ch         24*/
      t1 = z - dp1                                                              !/*ch         25*/
      t2 = a - dp1                                                              !/*ch         26*/
      if (ine.lt.0) then                                                        !/*ch         27*/
!   pi-                                                                         !/*ch         28*/
        ne2 = 0                                                                 !/*ch         29*/
        if (ne1.eq.0) then                                                      !/*ch         30*/
!                 neutron; must have proton partner; fin. st. = nn              !/*ch         31*/
          continue                                                              !/*ch         32*/
        elseif (ne1.eq.1) then                                                  !/*ch         33*/
!                                proton; final state must be nn or np:          !/*ch         34*/
          temp1 = t1/t2                                                         !/*ch         35*/
!                                Initial partner a neutron: fs = nn             !/*ch         36*/
          if (rn.gt.temp1) ne1 = 0                                              !/*ch         37*/
        endif                                                                   !/*ch         38*/
      elseif (ine.eq.0) then                                                    !/*ch         39*/
!   pi0                                                                         !/*ch         40*/
        if (ne1.eq.0) then                                                      !/*ch         41*/
!                                first partner is a neutron                     !/*ch         42*/
          temp1 = z/t2                                                          !/*ch         43*/
          if (rn.gt.temp1) then                                                 !/*ch         44*/
!                                2nd neutron in initial pair; fs = nn           !/*ch         45*/
            ne2 = 0                                                             !/*ch         46*/
          else                                                                  !/*ch         47*/
!                                proton in initial pair; fs = np                !/*ch         48*/
            ne2 = 1                                                             !/*ch         49*/
          endif                                                                 !/*ch         50*/
        elseif (ne1.eq.1) then                                                  !/*ch         51*/
!                                first partner is a proton                      !/*ch         52*/
          temp1 = t1/t2                                                         !/*ch         53*/
          if (rn.gt.temp1) then                                                 !/*ch         54*/
!                                Initial 2nd partner a neutron; fs = np         !/*ch         55*/
            ne2 = 0                                                             !/*ch         56*/
          else                                                                  !/*ch         57*/
!                              Initial 2nd partner a 2nd proton; fs = pp        !/*ch         58*/
            ne2 = 1                                                             !/*ch         59*/
          endif                                                                 !/*ch         60*/
        endif                                                                   !/*ch         61*/
      elseif (ine.eq.1) then                                                    !/*ch         62*/
!   pi+                                                                         !/*ch         63*/
        ne2 = 1                                                                 !/*ch         64*/
        if (ne1.eq.0) then                                                      !/*ch         65*/
!                                first partner is a neutron                     !/*ch         66*/
          temp1 = t1/t2                                                         !/*ch         67*/
          if (rn.gt.temp1) then                                                 !/*ch         68*/
!                                2nd nucleon is a neutron; fs = np              !/*ch         69*/
            continue                                                            !/*ch         70*/
          else                                                                  !/*ch         71*/
!                                2nd nucleon is a proton; fs = pp               !/*ch         72*/
            ne1 = 1                                                             !/*ch         73*/
          endif                                                                 !/*ch         74*/
        elseif (ne1.eq.1) then                                                  !/*ch         75*/
!                                proton; initial = np; fs = pp                  !/*ch         76*/
          continue                                                              !/*ch         77*/
        endif                                                                   !/*ch         78*/
      endif                                                                     !/*ch         79*/
#endif                                                                          /*!ch         80*/
      return                                                                    !/*ch         81*/
      end                                                                       !/*ch         82*/
