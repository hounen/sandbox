      subroutine rotor (ar, br, pstar, pr)                                      !/*ro          1*/
#ifdef CEM                                                                      /*!ro          2*/
! ======================================================================        !/*ro          3*/
!                                                                               !/*ro          4*/
!    Coordinate rotation.                                                       !/*ro          5*/
!                                                                               !/*ro          6*/
!    Called by: ABEL DIRECT ISOBAR PRECOF STAT                                  !/*ro          7*/
!                                                                               !/*ro          8*/
!    ar is the momentum of the incident particle in the CM frame:               !/*ro          9*/
!    it defines the z-prime axis.                                               !/*ro         10*/
!    br is the velocity vector of the CM frame in the lab frame.                !/*ro         11*/
!    ar and br define a plane, which contains the x-prime axis.                 !/*ro         12*/
!    pstar is the momentum of one of the final state particles in the           !/*ro         13*/
!    CM-prime frame.                                                            !/*ro         14*/
!                                                                               !/*ro         15*/
!    This frame is defined with z-prime parallel to ar,                         !/*ro         16*/
!    x-prime perpendicular to ar, in the plane defined by ar and br,            !/*ro         17*/
!    and y-prime perpendicular to the same plane.                               !/*ro         18*/
!    The routine calculates the vector pr, which                                !/*ro         19*/
!    is the vector of final state particle momentum in the CM frame             !/*ro         20*/
!    which has its coordinate axes parallel to those of the lab frame.          !/*ro         21*/
!                                                                               !/*ro         22*/
!   CEM95 written by S. G. Mashnik                                              !/*ro         23*/
!                                                                               !/*ro         24*/
!   Edited by A. J. Sierk,  LANL  T-2  February, 1996.                          !/*ro         25*/
!   Modified by A. J. Sierk,  LANL  T-2  March, 1996.                           !/*ro         26*/
!   Further editing by AJS, July, 1997.                                         !/*ro         27*/
!   Modified by AJS; August, 1997.                                              !/*ro         28*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*ro         29*/
!                                                                               !/*ro         30*/
! ======================================================================        !/*ro         31*/
#include <cem/cdouble.h>                                                        /*!ro         32*/
#include <lcs/inout.h>                                                          /*!ro         33*/
      dimension ar(3), br(3), pstar(3), pr(3), an(3), bn(3)                     !/*ro         34*/
      dimension cn(3), dn(3)                                                    !/*ro         35*/
! ======================================================================        !/*ro         36*/
      amod = sqrt(ar(1)**2 + ar(2)**2 + ar(3)**2)                               !/*ro         37*/
      bmod = sqrt(br(1)**2 + br(2)**2 + br(3)**2)                               !/*ro         38*/
      cp = dp0                                                                  !/*ro         39*/
        do 10 ir = 1,3                                                          !/*ro         40*/
        cp = cp + ar(ir)*br(ir)                                                 !/*ro         41*/
        an(ir) = ar(ir)/amod                                                    !/*ro         42*/
        bn(ir) = br(ir)/bmod                                                    !/*ro         43*/
   10   continue                                                                !/*ro         44*/
!   cp is the cosine of the angle between vectors ar and br.                    !/*ro         45*/
      cp = cp/(amod*bmod)                                                       !/*ro         46*/
      if (abs(cp).gt.dp1) cp = sign (dp1, cp)                                   !/*ro         47*/
!   cn is the cross product of two unit vectors; its magnitude is               !/*ro         48*/
!   sin (angle);  redefined cn is the unit vector perpendicular to              !/*ro         49*/
!   the plane of ar and br.                                                     !/*ro         50*/
      cn(1) = an(2)*bn(3) - an(3)*bn(2)                                         !/*ro         51*/
      cn(2) = an(3)*bn(1) - an(1)*bn(3)                                         !/*ro         52*/
      cn(3) = an(1)*bn(2) - an(2)*bn(1)                                         !/*ro         53*/
      cmod = sqrt(cn(1)**2 + cn(2)**2 + cn(3)**2)                               !/*ro         54*/
      if (cmod.gt.dp0) then                                                     !/*ro         55*/
!  The cross product is not exactly 0 to machine precision:                     !/*ro         56*/
        fac = dp1/cmod                                                          !/*ro         57*/
        do 20 ir = 1,3                                                          !/*ro         58*/
        cn(ir) = cn(ir)*fac                                                     !/*ro         59*/
        dn(ir) = bn(ir) - cp*an(ir)                                             !/*ro         60*/
   20   continue                                                                !/*ro         61*/
      else                                                                      !/*ro         62*/
!   bn is exactly (anti-or) parallel to an to within machine precision:         !/*ro         63*/
!   Define an arbitrary coordinate system with an as its z-prime axis.          !/*ro         64*/
        cn(2) = dp0                                                             !/*ro         65*/
        if (an(3).ne.dp0) then                                                  !/*ro         66*/
          fac = dp1/sqrt(an(1)**2 + an(3)**2)                                   !/*ro         67*/
          cn(1) =  fac*an(3)                                                    !/*ro         68*/
          cn(3) = -fac*an(1)                                                    !/*ro         69*/
          if (an(2).ne.dp0) then                                                !/*ro         70*/
            dn(1) = an(1)/an(3)                                                 !/*ro         71*/
            dn(2) = -(an(1)**2 + an(3)**2)/(an(2)*an(3))                        !/*ro         72*/
            dn(3) = dp1                                                         !/*ro         73*/
          else                                                                  !/*ro         74*/
            dn(1) = dp0                                                         !/*ro         75*/
            dn(2) = -dp1                                                        !/*ro         76*/
            dn(3) = dp0                                                         !/*ro         77*/
          endif                                                                 !/*ro         78*/
        else                                                                    !/*ro         79*/
          cn(1) =  dp0                                                          !/*ro         80*/
          cn(3) = -dp1                                                          !/*ro         81*/
          dn(1) =  an(2)                                                        !/*ro         82*/
          dn(2) = -an(1)                                                        !/*ro         83*/
          dn(3) =  dp0                                                          !/*ro         84*/
        endif                                                                   !/*ro         85*/
      endif                                                                     !/*ro         86*/
      alpha2 = sqrt(dp1 - cp**2)                                                !/*ro         87*/
      if (alpha2 .gt. dp1m6) then                                               !/*ro         88*/
        do 30 ir = 1,3                                                          !/*ro         89*/
        pr(ir) = pstar(1)*dn(ir)/alpha2 + pstar(3)*an(ir)                       !/*ro         90*/
     &         + pstar(2)*cn(ir)                                                !/*ro         91*/
   30   continue                                                                !/*ro         92*/
      else                                                                      !/*ro         93*/
!  an and bn are nearly parallel or antiparallel; round-off may make            !/*ro         94*/
!  dmod = 0.0 to computer word length.                                          !/*ro         95*/
        dmod = sqrt(dn(1)**2 + dn(2)**2 + dn(3)**2)                             !/*ro         96*/
        if (dmod.ne.dp0) then                                                   !/*ro         97*/
          f2 = dp1/dmod                                                         !/*ro         98*/
        else                                                                    !/*ro         99*/
          f2 = dp0                                                              !/*ro        100*/
        endif                                                                   !/*ro        101*/
          do 40 ir = 1,3                                                        !/*ro        102*/
          pr(ir) = pstar(3)*an(ir) + pstar(2)*cn(ir) +                          !/*ro        103*/
     &             pstar(1)*dn(ir)*f2                                           !/*ro        104*/
   40     continue                                                              !/*ro        105*/
!       write (io,1000) cp, alpha2, (ar(ir),ir=1,3), (br(ir),ir=1,3),           !/*ro        106*/
!    &                              (cn(ir),ir=1,3), (pr(ir),ir=1,3)            !/*ro        107*/
!       write (itty,1000) cp, alpha2, (ar(ir),ir=1,3), (br(ir),ir=1,3),         !/*ro        108*/
!    &                              (cn(ir),ir=1,3), (pr(ir),ir=1,3)            !/*ro        109*/
      endif                                                                     !/*ro        110*/
! ======================================================================        !/*ro        111*/
 1000 format (2x,'NEW ROTOR; cp = ',1pe15.7,', sp = ',e15.7,                    !/*ro        112*/
     &       /2x,'ar = ',3e15.7/2x,'br = ',3e15.7                               !/*ro        113*/
     &       /2x,'cn = ',3e15.7/2x,'pr = ',3e15.7)                              !/*ro        114*/
! ======================================================================        !/*ro        115*/
#endif                                                                          /*!ro        116*/
      return                                                                    !/*ro        117*/
      end                                                                       !/*ro        118*/
