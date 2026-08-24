      function geom (rbig, partin, ipatin)                                      !/*ge          1*/
! ======================================================================        !/*ge          2*/
!                                                                               !/*ge          3*/
!     Calculation of geometrical path of particle.                              !/*ge          4*/
!     Geom is distance from current point along trajectory to the first         !/*ge          5*/
!     intersection of the trajectory with the edge of the current               !/*ge          6*/
!     nuclear zone.                                                             !/*ge          7*/
!                                                                               !/*ge          8*/
!     Called by: POINTE                                                         !/*ge          9*/
!                                                                               !/*ge         10*/
!     CEM95 written by S. G. Mashnik                                            !/*ge         11*/
!                                                                               !/*ge         12*/
!     Edited by A. J. Sierk   LANL  T-2,  February, 1996.                       !/*ge         13*/
!     Edited by AJS, December, 1997.                                            !/*ge         14*/
!     Changed to double by F. Gallmeier, ORNL CPED, October 1998.               !/*ge         15*/
!                                                                               !/*ge         16*/
! ======================================================================        !/*ge         17*/
!                                                                               !/*ge         18*/
!  Definition of partin:                                                        !/*ge         19*/
!                       partin(1); Normalized x coordinate of particle          !/*ge         20*/
!                       partin(2); Normalized y coordinate of particle          !/*ge         21*/
!                       partin(3); Normalized z coordinate of particle          !/*ge         22*/
!                       partin(4); sin(theta), direction of momentum            !/*ge         23*/
!                       partin(5); cos(theta), direction of momentum            !/*ge         24*/
!                       partin(6); sin(phi), direction of momentum              !/*ge         25*/
!                       partin(7); cos(phi), direction of momentum              !/*ge         26*/
!                                                                               !/*ge         27*/
!  Definition of ipatin:                                                        !/*ge         28*/
!                       ipatin(5); zone number of nucleus where particle        !/*ge         29*/
!                                  is located.                                  !/*ge         30*/
!                                                                               !/*ge         31*/
! ======================================================================        !/*ge         32*/
#include <cem/cdouble.h>                                                        /*!ge         33*/
#ifndef CEM                                                                     /*!ge         34*/
      geom=dp0                                                                  !/*ge         35*/
#else                                                                           /*!ge         36*/
#include <lcs/inout.h>                                                          /*!ge         37*/
      dimension rbig(10), partin(9), ipatin(5)                                  !/*ge         38*/
      data zro, one /0.d0, 1.d0/                                                !/*ge         39*/
! ======================================================================        !/*ge         40*/
      rin = sqrt(partin(1)**2 + partin(2)**2 + partin(3)**2)                    !/*ge         41*/
      if (abs(rin).lt.dp1m5) then                                               !/*ge         42*/
         write (io, 9000) rin                                                   !/*ge         43*/
         write (itty, 9000) rin                                                 !/*ge         44*/
      endif                                                                     !/*ge         45*/
 9000 format (1x,'In GEOM, rin = ',1pe12.4)                                     !/*ge         46*/
      if (rin.ne.zro) then                                                      !/*ge         47*/
        ctinjb = (partin(1)*partin(4)*partin(7) + partin(2)*partin(4)*          !/*ge         48*/
     &            partin(6) + partin(3)*partin(5))/rin                          !/*ge         49*/
        if (abs(ctinjb).gt.one) ctinjb = sign (one, ctinjb)                     !/*ge         50*/
      else                                                                      !/*ge         51*/
!  Particle at origin, necessarily moving outward.                              !/*ge         52*/
        ctinjb = one                                                            !/*ge         53*/
      endif                                                                     !/*ge         54*/
      stinjb = sqrt(one - ctinjb**2)                                            !/*ge         55*/
      if (ctinjb.lt.zro) then                                                   !/*ge         56*/
!  Particle moving toward center of nucleus (r decreasing).                     !/*ge         57*/
        g = -one                                                                !/*ge         58*/
        kg = ipatin(5) - 1                                                      !/*ge         59*/
        if (kg.le.0) then                                                       !/*ge         60*/
          rl = zro                                                              !/*ge         61*/
        else                                                                    !/*ge         62*/
          rl = rbig(kg)                                                         !/*ge         63*/
        endif                                                                   !/*ge         64*/
      else                                                                      !/*ge         65*/
!  Particle moving away from center of nucleus (r increasing).                  !/*ge         66*/
        g = one                                                                 !/*ge         67*/
        kg = ipatin(5)                                                          !/*ge         68*/
        rl = rbig(kg)                                                           !/*ge         69*/
      endif                                                                     !/*ge         70*/
      iq = 0                                                                    !/*ge         71*/
   10 deltag = rl**2 - (rin*stinjb)**2                                          !/*ge         72*/
      iq = iq + 1                                                               !/*ge         73*/
      if (iq.gt.2 .and. deltag.lt.dp0) then                                     !/*ge         74*/
        if (abs(ctinjb).le.dp1m6) then                                          !/*ge         75*/
          if (abs(rl - rin).le.dp1m6) then                                      !/*ge         76*/
!   Particle is at outside boundary of zone, moving nearly tangentially.        !/*ge         77*/
            ipatin(5) = ipatin(5) + 1                                           !/*ge         78*/
            kg = ipatin(5)                                                      !/*ge         79*/
            rl = rbig(kg)                                                       !/*ge         80*/
            go to 10                                                            !/*ge         81*/
          elseif (abs(rbig(kg+1) - rin).le.dp1m6) then                          !/*ge         82*/
!   Particle is at outside boundary of next outer zone, moving nearly           !/*ge         83*/
!   tangentially.                                                               !/*ge         84*/
            ipatin(5) = ipatin(5) + 2                                           !/*ge         85*/
            kg = ipatin(5)                                                      !/*ge         86*/
            rl = rbig(kg)                                                       !/*ge         87*/
            go to 10                                                            !/*ge         88*/
          endif                                                                 !/*ge         89*/
        endif                                                                   !/*ge         90*/
        call expirx(1,'GEOM','complicated condition.')                          !/*ge         91*/
      endif                                                                     !/*ge         92*/
      if (deltag.lt.dp0) then                                                   !/*ge         93*/
!   Does not intersect inner edge of zone, look for intersection with           !/*ge         94*/
!   outer edge.                                                                 !/*ge         95*/
        g = one                                                                 !/*ge         96*/
        kg = ipatin(5)                                                          !/*ge         97*/
        rl = rbig(kg)                                                           !/*ge         98*/
        go to 10                                                                !/*ge         99*/
      else                                                                      !/*ge        100*/
!  For g = -1, distance along trajectory to intersection with inner             !/*ge        101*/
!  edge of zone; for g = +1, distance to intersection with outer edge.          !/*ge        102*/
        geom = g*sqrt(deltag) - rin*ctinjb                                      !/*ge        103*/
      endif                                                                     !/*ge        104*/
#endif                                                                          /*!ge        105*/
      return                                                                    !/*ge        106*/
      end                                                                       !/*ge        107*/
