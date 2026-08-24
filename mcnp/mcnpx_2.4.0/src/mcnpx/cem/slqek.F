      subroutine slqek(l, ms, mb, ksi, me, lin, msin, mbin, mein, ln,           !/*sl          1*/
     &                 msn, mbn, men)                                           !/*sl          2*/
#ifdef CEM                                                                      /*!sl          3*/
! ======================================================================        !/*sl          4*/
!                                                                               !/*sl          5*/
!     Determine cross section type.                                             !/*sl          6*/
!                                                                               !/*sl          7*/
!     Called by: POINTE TYPINT                                                  !/*sl          8*/
!                                                                               !/*sl          9*/
!   CEM95 written by S. G. Mashnik                                              !/*sl         10*/
!                                                                               !/*sl         11*/
!   Edited by A. J. Sierk,  LANL  T-2  February-March, 1996.                    !/*sl         12*/
!   Edited by AJS, July, 1997.                                                  !/*sl         13*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*sl         14*/
!                                                                               !/*sl         15*/
! ======================================================================        !/*sl         16*/
!  ms, msin, & msn are always zero (strangeness; hardwired in cem95).           !/*sl         17*/
! ======================================================================        !/*sl         18*/
#include <cem/cdouble.h>                                                        /*!sl         19*/
      ms = msin + msn                                                           !/*sl         20*/
!  l, lin, & ln are always zero (hardwired in cem95) [related to gamma          !/*sl         21*/
!      reactions].                                                              !/*sl         22*/
      l = lin + ln                                                              !/*sl         23*/
!  Total baryon number in system:                                               !/*sl         24*/
      mb = mbin + mbn                                                           !/*sl         25*/
!  Total charge in system:                                                      !/*sl         26*/
      me = mein + men                                                           !/*sl         27*/
      if (mb.gt.1) then                                                         !/*sl         28*/
!   baryon on baryon                                                            !/*sl         29*/
        if (me.ne.1) then                                                       !/*sl         30*/
!   2 neutrons or 2 protons                                                     !/*sl         31*/
          ksi = 1                                                               !/*sl         32*/
        else                                                                    !/*sl         33*/
!   1 neutron and 1 proton                                                      !/*sl         34*/
          ksi = 2                                                               !/*sl         35*/
        endif                                                                   !/*sl         36*/
      else                                                                      !/*sl         37*/
!   meson interacting with baryon                                               !/*sl         38*/
        if (me.eq.2 .or. me.eq.-1) then                                         !/*sl         39*/
!   pi+ on proton or pi- on neutron                                             !/*sl         40*/
          ksi = 1                                                               !/*sl         41*/
        elseif (me.ne.0) then                                                   !/*sl         42*/
!   total charge = 1                                                            !/*sl         43*/
          if (mein.eq.1) then                                                   !/*sl         44*/
!   pi+ incident on neutron                                                     !/*sl         45*/
            ksi = 2                                                             !/*sl         46*/
          else                                                                  !/*sl         47*/
!   pi0 incident on proton                                                      !/*sl         48*/
            ksi = 3                                                             !/*sl         49*/
          endif                                                                 !/*sl         50*/
        else                                                                    !/*sl         51*/
!   total charge = 0                                                            !/*sl         52*/
          if (mein.eq.-1) then                                                  !/*sl         53*/
!   pi- incident on proton                                                      !/*sl         54*/
            ksi = 2                                                             !/*sl         55*/
          else                                                                  !/*sl         56*/
!   pi0 incident on neutron                                                     !/*sl         57*/
            ksi = 3                                                             !/*sl         58*/
          endif                                                                 !/*sl         59*/
        endif                                                                   !/*sl         60*/
      endif                                                                     !/*sl         61*/
! ======================================================================        !/*sl         62*/
 1000 format(//50x,' Strange particle.')                                        !/*sl         63*/
! ======================================================================        !/*sl         64*/
#endif                                                                          /*!sl         65*/
      return                                                                    !/*sl         66*/
      end                                                                       !/*sl         67*/
