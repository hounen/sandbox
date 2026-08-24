      subroutine chinel (ipatin, l, ms, mb, ksi, np, mv, tin1, me,              !/*ch          1*/
     &                   ipatne, z, a)                                          !/*ch          2*/
#ifdef CEM                                                                      /*!ch          3*/
! ======================================================================        !/*ch          4*/
!                                                                               !/*ch          5*/
!     Determinine secondary particles charges in inelastic (pion prod.)         !/*ch          6*/
!     reaction.                                                                 !/*ch          7*/
!                                                                               !/*ch          8*/
!   Called by: BINEL                                                            !/*ch          9*/
!                                                                               !/*ch         10*/
!   Calls: SIGMAT                                                               !/*ch         11*/
!                                                                               !/*ch         12*/
!   CEM95 written by S. G. Mashnik                                              !/*ch         13*/
!                                                                               !/*ch         14*/
!   Edited by A. J. Sierk  LANL T-2  February, 1996.                            !/*ch         15*/
!   Edited by AJS, July-August, 1997.                                           !/*ch         16*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*ch         17*/
!                                                                               !/*ch         18*/
! ======================================================================        !/*ch         19*/
!                                                                               !/*ch         20*/
!  Definition of ipatin (ipatne similar for 2nd particle):                      !/*ch         21*/
!                       ipatin(1); charge of particle                           !/*ch         22*/
!                       ipatin(2); (Always 0 in CEM97a!)                        !/*ch         23*/
!                       ipatin(3); strangeness of particle                      !/*ch         24*/
!                       ipatin(4); particle baryon number                       !/*ch         25*/
!                       ipatin(5); zone number of nucleus where particle        !/*ch         26*/
!                                  is located.                                  !/*ch         27*/
!                                                                               !/*ch         28*/
! ======================================================================        !/*ch         29*/
#include <cem/cdouble.h>                                                        /*!ch         30*/
      dimension ipatin(5), ipatne(5), pmemo(9,150), imemo(5,150)                !/*ch         31*/
      common /memory/ pmemo, imemo                                              !/*ch         32*/
! ======================================================================        !/*ch         33*/
      if (np.eq.3) then                                                         !/*ch         34*/
!   Three-particle final state:                                                 !/*ch         35*/
!   spi0 = pi0 production cross section with no change of incident              !/*ch         36*/
!          particle identity;                                                   !/*ch         37*/
!   pp --> p p pi0; pn --> p n pi0;nn --> n n pi0;pi+ p --> p pi+ pi0           !/*ch         38*/
!   pi- p --> p pi- pi0; pi0 p --> p 2pi0; pi0 n --> n 2pi0                     !/*ch         39*/
        spi0 = sigmat (l, ms, mb, ksi, 4, tin1)                                 !/*ch         40*/
!   Total pion production cross section:                                        !/*ch         41*/
        sth = sigmat (l, ms, mb, ksi, 7, tin1)                                  !/*ch         42*/
        bpi0 = spi0/sth                                                         !/*ch         43*/
!   Argument 5 --> pp --> p n pi+; pn --> p p pi-; pi- p --> n pi- pi+;         !/*ch         44*/
!   pi+ p --> n 2pi+; pi0 p --> p pi+ pi-; pi0 n --> n pi+ pi-                  !/*ch         45*/
        bpiex = (spi0 + sigmat(l, ms, mb, ksi, 5, tin1))/sth                    !/*ch         46*/
        temp1 = rang()                                                          !/*ch         47*/
        if (temp1.lt.bpi0) then                                                 !/*ch         48*/
!   Incident particles go into mv + 1 and mv + 3; pi0 goes into mv + 2:         !/*ch         49*/
          imemo(1,mv+1) = ipatne(1)                                             !/*ch         50*/
          imemo(1,mv+2) = 0                                                     !/*ch         51*/
          imemo(1,mv+3) = ipatin(1)                                             !/*ch         52*/
        else                                                                    !/*ch         53*/
!   bpiex = 1.0 for mb,ksi = 1,1  or  2,1 pi+ + p; pi- + n; pp; or nn           !/*ch         54*/
!   bpiex < 1.0 for 1,3  or  2,2  pi0 + N or p + n                              !/*ch         55*/
          if (temp1.ge.bpiex) then                                              !/*ch         56*/
            imemo(1,mv+1) = ipatne(1) - (ipatin(4) - 1)*ipatin(1)               !/*ch         57*/
            imemo(1,mv+3) = (ipatin(4) - 1)*ipatin(1)**2 -                      !/*ch         58*/
     &                      ipatin(4)*ipatin(1) + 1                             !/*ch         59*/
          else                                                                  !/*ch         60*/
            imemo(1,mv+1) = 1 - ipatne(1)                                       !/*ch         61*/
            imemo(1,mv+3) = ipatin(1)                                           !/*ch         62*/
          endif                                                                 !/*ch         63*/
          imemo(1,mv+2) = me - imemo(1,mv+1) - imemo(1,mv+3)                    !/*ch         64*/
        endif                                                                   !/*ch         65*/
        return                                                                  !/*ch         66*/
      endif                                                                     !/*ch         67*/
   10 continue                                                                  !/*ch         68*/
      b1 = rang()                                                               !/*ch         69*/
      if (b1 .le. z/a) then                                                     !/*ch         70*/
        imemo(1,mv+1) = 1                                                       !/*ch         71*/
      else                                                                      !/*ch         72*/
        imemo(1,mv+1) = 0                                                       !/*ch         73*/
      endif                                                                     !/*ch         74*/
      if (mb.ge.1) then                                                         !/*ch         75*/
        b2 = rang()                                                             !/*ch         76*/
        if (b2 .le. z/a) then                                                   !/*ch         77*/
          imemo(1,mv+3) = 1                                                     !/*ch         78*/
        else                                                                    !/*ch         79*/
          imemo(1,mv+3) = 0                                                     !/*ch         80*/
        endif                                                                   !/*ch         81*/
      endif                                                                     !/*ch         82*/
      lambda = 2                                                                !/*ch         83*/
   20 continue                                                                  !/*ch         84*/
      if (mb.le.1 .or. lambda.ne.3) then                                        !/*ch         85*/
        temp2 = rang()                                                          !/*ch         86*/
        mtemp = mv + lambda                                                     !/*ch         87*/
        if (temp2 .lt. dpth) then                                               !/*ch         88*/
          imemo(1,mtemp) = 1                                                    !/*ch         89*/
        elseif (temp2 .lt. dp2th) then                                          !/*ch         90*/
          imemo(1,mtemp) = 0                                                    !/*ch         91*/
        else                                                                    !/*ch         92*/
          imemo(1,mtemp) = -1                                                   !/*ch         93*/
        endif                                                                   !/*ch         94*/
      endif                                                                     !/*ch         95*/
      if (lambda.lt.np) then                                                    !/*ch         96*/
        lambda = lambda + 1                                                     !/*ch         97*/
        go to 20                                                                !/*ch         98*/
      endif                                                                     !/*ch         99*/
      migq = 0                                                                  !/*ch        100*/
        do 30 i = 1,np                                                          !/*ch        101*/
        itemp = mv + i                                                          !/*ch        102*/
        migq = migq + imemo(1,itemp)                                            !/*ch        103*/
   30   continue                                                                !/*ch        104*/
      if (me.ne.migq) go to 10                                                  !/*ch        105*/
#endif                                                                          /*!ch        106*/
      return                                                                    !/*ch        107*/
      end                                                                       !/*ch        108*/
