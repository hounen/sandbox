      subroutine absorp (partin, ipatin, partne, ipatne, mv, np, a, z,          !/*ab          1*/
     &                   v)                                                     !/*ab          2*/
#ifdef CEM                                                                      /*!ab          3*/
! ======================================================================        !/*ab          4*/
!                                                                               !/*ab          5*/
!   Calculates outgoing nucleon characteristics in pion absorption.             !/*ab          6*/
!                                                                               !/*ab          7*/
!   Called by: ABSORP TYPINT                                                    !/*ab          8*/
!                                                                               !/*ab          9*/
!   Calls: ABEL CHABS COSTA PARTN TINVU                                         !/*ab         10*/
!                                                                               !/*ab         11*/
!   CEM95 written by S. G. Mashnik                                              !/*ab         12*/
!                                                                               !/*ab         13*/
!   Edited by A. J. Sierk,  LANL  T-2  February, 1996.                          !/*ab         14*/
!   Modified by AJS, July-August, 1997.                                         !/*ab         15*/
!                                                                               !/*ab         16*/
! ======================================================================        !/*ab         17*/
!                                                                               !/*ab         18*/
!  np is the number of particles in the final state (= 2, unless                !/*ab         19*/
!  dimensions of storage array are exceeded).                                   !/*ab         20*/
!                                                                               !/*ab         21*/
!  Definition of partin:                                                        !/*ab         22*/
!                       partin(1); Normalized x coordinate of pion              !/*ab         23*/
!                       partin(2); Normalized y coordinate of pion              !/*ab         24*/
!                       partin(3); Normalized z coordinate of pion              !/*ab         25*/
!                       partin(4); sin(theta), direction of momentum            !/*ab         26*/
!                       partin(5); cos(theta), direction of momentum            !/*ab         27*/
!                       partin(6); sin(phi), direction of momentum              !/*ab         28*/
!                       partin(7); cos(phi), direction of momentum              !/*ab         29*/
!                       partin(8); kinetic energy of pion                       !/*ab         30*/
!                       partin(9); rest mass of pion                            !/*ab         31*/
!                                                                               !/*ab         32*/
!  partne is similar to partin for first nucleon of the pair which              !/*ab         33*/
!  absorbs the pion; ipatne is similar to ipatin.                               !/*ab         34*/
!                                                                               !/*ab         35*/
!  Definition of ipatin:                                                        !/*ab         36*/
!                       ipatin(1); charge of pion                               !/*ab         37*/
!                       ipatin(2); (Always 0 in CEM97a!)                        !/*ab         38*/
!                       ipatin(3); strangeness of pion (= 0!)                   !/*ab         39*/
!                       ipatin(4); particle baryon number (= 0!)                !/*ab         40*/
!                       ipatin(5); zone number of nucleus where                 !/*ab         41*/
!                                  interaction occurs.                          !/*ab         42*/
!                                                                               !/*ab         43*/
! ======================================================================        !/*ab         44*/
#include <cem/cdouble.h>                                                        /*!ab         45*/
      common /memory/ pmemo, imemo                                              !/*ab         46*/
      common /mnuc/   emnucm, emnucg, emnucb, emneut, emprot                    !/*ab         47*/
      common /pii/     pi                                                       !/*ab         48*/
      dimension partin(9), ipatin(5), partne(9), ipatne(5), par1(9),            !/*ab         49*/
     &          paf(3), v(3), pist(3), pnst(3), ipa1(5), pmemo(9,150),          !/*ab         50*/
     &          imemo(5,150)                                                    !/*ab         51*/
! ======================================================================        !/*ab         52*/
      twopi = dp2*pi                                                            !/*ab         53*/
      em1 = emneut*(dp1 - ipatne(1)) + emprot*ipatne(1)                         !/*ab         54*/
      twom1 = dp2*em1                                                           !/*ab         55*/
      pn = sqrt(partne(8)*(partne(8) + twom1))                                  !/*ab         56*/
!   Pick 2nd nucleon partner for absorption.                                    !/*ab         57*/
      call partn (partin, ipatin, par1, ipa1)                                   !/*ab         58*/
      em2 = emneut*(dp1 - ipa1(1)) + emprot*ipa1(1)                             !/*ab         59*/
      twom2 = dp2*em2                                                           !/*ab         60*/
      pn1 = sqrt(par1(8)*(par1(8) + twom2))                                     !/*ab         61*/
!   Momentum components of 2-nucleon system:                                    !/*ab         62*/
      paf(1) = pn*partne(4)*partne(7) + pn1*par1(4)*par1(7)                     !/*ab         63*/
      paf(2) = pn*partne(4)*partne(6) + pn1*par1(4)*par1(6)                     !/*ab         64*/
      paf(3) = pn*partne(5) + pn1*par1(5)                                       !/*ab         65*/
      pafm = sqrt(paf(1)**2 + paf(2)**2 + paf(3)**2)                            !/*ab         66*/
!   Cosine of angle between velocity of 2-nucleon CM system and beam            !/*ab         67*/
!   direction:                                                                  !/*ab         68*/
      ctn1 = paf(3)/pafm                                                        !/*ab         69*/
      temp1 = dp1 - ctn1**2                                                     !/*ab         70*/
      if (temp1.le.dp0) then                                                    !/*ab         71*/
        ctn1 = dp1                                                              !/*ab         72*/
        stn1 = dp0                                                              !/*ab         73*/
        sfn1 = dp0                                                              !/*ab         74*/
        cfn1 = dp1                                                              !/*ab         75*/
      else                                                                      !/*ab         76*/
        stn1 = sqrt(temp1)                                                      !/*ab         77*/
        temp2 = pafm*stn1                                                       !/*ab         78*/
        sfn1 = paf(2)/temp2                                                     !/*ab         79*/
        cfn1 = paf(1)/temp2                                                     !/*ab         80*/
      endif                                                                     !/*ab         81*/
!   Find invariant mass of 2-nucleon system:                                    !/*ab         82*/
      e1 = sqrt(pn1**2 + em1**2)                                                !/*ab         83*/
      e2 = sqrt(pn**2 + em2**2)                                                 !/*ab         84*/
      dot = pn*pn1*(partne(4)*par1(4)*(partne(7)*par1(7) +                      !/*ab         85*/
     &              partne(6)*par1(6)) + partne(5)*par1(5))                     !/*ab         86*/
      em2n = sqrt(em1**2 + em2**2 + dp2*(e1*e2 - dot))                          !/*ab         87*/
!   Kinetic energy of 2N system w. r. t. lab frame.                             !/*ab         88*/
      taf = sqrt(pafm**2 + em2n**2) - em2n                                      !/*ab         89*/
!   Find kinetic energy tin1 of pion in 2N CM frame, and velocity (v)           !/*ab         90*/
!   of 3-particle CM frame w. r. t. lab.                                        !/*ab         91*/
      call tinvu (partin(8), partin(9), taf, em2n, partin(4), partin(5),        !/*ab         92*/
     &            partin(6), partin(7), stn1, ctn1, sfn1, cfn1, v, u,           !/*ab         93*/
     &            tin1)                                                         !/*ab         94*/
!   Find isospin of outgoing channel (p-p, n-p, or n-n)                         !/*ab         95*/
!   Old method; ignore isospin of initial partners.                             !/*ab         96*/
!     call chabs (ipatin(2), ipatin(1), ne, ie, a, z)                           !/*ab         97*/
!   New method; keep track of isospin of original partner:                      !/*ab         98*/
      call chabs (ipatin(2), ipatin(1), ipatne(1), ie, a, z)                    !/*ab         99*/
      em1 = emneut*(1.d0 - ipatne(1)) + ipatne(1)*emprot                        !/*ab        100*/
      em2 = emneut*(1.d0 - ie) + ie*emprot                                      !/*ab        101*/
!   Define a random direction in space. (Direction of 2-particle                !/*ab        102*/
!   final state momenta in their rest frame, CM-prime)                          !/*ab        103*/
      b1 = rang()                                                               !/*ab        104*/
      ctst = dp1 - dp2*b1                                                       !/*ab        105*/
      b2 = rang()                                                               !/*ab        106*/
      fist = twopi*b2                                                           !/*ab        107*/
      call abel (partin, v, u, pist, pnst, ctst, fist, em1, em2)                !/*ab        108*/
!   In ABEL, we find pist and pnst; the oppositely directed momenta             !/*ab        109*/
!   of the 2 nucleons in the final state, in their center-of-mass frame.        !/*ab        110*/
      if (mv.le.147) then                                                       !/*ab        111*/
        pmemo(1, mv+3) = pist(1)                                                !/*ab        112*/
        pmemo(2, mv+3) = pist(2)                                                !/*ab        113*/
        pmemo(3, mv+3) = pist(3)                                                !/*ab        114*/
        pmemo(9, mv+3) = em2                                                    !/*ab        115*/
        imemo(1, mv+3) = ie                                                     !/*ab        116*/
        imemo(2, mv+3) = 0                                                      !/*ab        117*/
        imemo(3, mv+3) = 0                                                      !/*ab        118*/
        imemo(4, mv+3) = 1                                                      !/*ab        119*/
        pmemo(1, mv+1) = pnst(1)                                                !/*ab        120*/
        pmemo(2, mv+1) = pnst(2)                                                !/*ab        121*/
        pmemo(3, mv+1) = pnst(3)                                                !/*ab        122*/
        pmemo(9, mv+1) = em1                                                    !/*ab        123*/
        imemo(1, mv+1) = ipatne(1)                                              !/*ab        124*/
        imemo(2, mv+1) = 0                                                      !/*ab        125*/
        imemo(3, mv+1) = 0                                                      !/*ab        126*/
        imemo(4, mv+1) = 1                                                      !/*ab        127*/
        np = 2                                                                  !/*ab        128*/
      else                                                                      !/*ab        129*/
        np = 0                                                                  !/*ab        130*/
        call expirx(1,'ABSORP','memory exceeded in cascade.')                   !/*ab        131*/
      endif                                                                     !/*ab        132*/
#endif                                                                          /*!ab        133*/
      return                                                                    !/*ab        134*/
      end                                                                       !/*ab        135*/
