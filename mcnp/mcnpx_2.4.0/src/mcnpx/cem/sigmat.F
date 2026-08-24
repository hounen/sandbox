      function sigmat (l, ms, mb, ksi, iks, t)                                  !/*si          1*/
! ======================================================================        !/*si          2*/
!                                                                               !/*si          3*/
!     Choose cross section type and calculate cross section for                 !/*si          4*/
!     a given struck-nucleon rest frame kinetic energy.                         !/*si          5*/
!     l always 0; ms always 0; mb = baryon number.                              !/*si          6*/
!     mb may be 1 or 2.                                                         !/*si          7*/
!     ksi = 1 for n - n, p - p, pi+ - p & pi- - n.                              !/*si          8*/
!     ksi = 2 for n - p, pi+ - n & pi- - p.                                     !/*si          9*/
!     ksi = 3 for pi0 - n, pi0 - p                                              !/*si         10*/
!     iks = 0:   total cross section                                            !/*si         11*/
!     iks = 1:   elastic cross section                                          !/*si         12*/
!     iks = 2:   pion charge exchange cross section                             !/*si         13*/
!     iks = 3:   pion absorption cross section                                  !/*si         14*/
!     iks = 4:   neutral pion production cross section                          !/*si         15*/
!     iks = 5:   charged pion production cross section                          !/*si         16*/
!     iks = 6:   target & projectile isospin change & neutral pion              !/*si         17*/
!                production (9/11/97)                                           !/*si         18*/
!     iks = 7:   total pion production cross section (9/11/97)                  !/*si         19*/
!     iks = 8:   delta production cross section                                 !/*si         20*/
!                                                                               !/*si         21*/
!   Called by: BINEL CHINEL ELEX POINTE TYPINT.                                 !/*si         22*/
!                                                                               !/*si         23*/
!   Calls: QINTS                                                                !/*si         24*/
!   CEM95 written by S. G. Mashnik                                              !/*si         25*/
!                                                                               !/*si         26*/
!   Edited by A. J. Sierk,  LANL  T-2  February-March, 1996.                    !/*si         27*/
!   Edited by AJS, July, 1997.                                                  !/*si         28*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*si         29*/
!                                                                               !/*si         30*/
! ======================================================================        !/*si         31*/
#include <cem/cdouble.h>                                                        /*!si         32*/
#ifndef CEM                                                                     /*!si         33*/
      sigmat=dp0                                                                !/*si         34*/
#else                                                                           /*!si         35*/
      common /typecs/ icst(28), nsicst(21)                                      !/*si         36*/
!   Factor with which to multiply deuteron absorption cross section to          !/*si         37*/
!   get effective absorption cross section inside nucleus:                      !/*si         38*/
      data absfct /4.0d0/                                                       !/*si         39*/
! ======================================================================        !/*si         40*/
!     data icst/                                                                !/*si         41*/
! Translation:total:|elas:   |total: |elas:  |total:  |elas:   |pi+ n or        !/*si         42*/
!             p+p or|p+p or  |       |       |pi- p or|pi- p or|pi- p  |        !/*si         43*/
!             n+n   |n+n     |p+n    |n+p    |pi+ n   |pi+ n   |SCX    |        !/*si         44*/
!   &         210,    211,    220,    221,     120,     121,     122,           !/*si         45*/
!                           | Absorp: |                                         !/*si         46*/
!  Translation:total:|elas: |pi+ np:pp|p + p->|p + p->|p + n->|p + n->|         !/*si         47*/
!            pi+ p or|pi+ p |pi- pp:np|p p pi0|p n pi+|p + n +|p + p +|         !/*si         48*/
!            pi- n   |pi- n |pi- pn:nn|n + n->|n + n->|pi0    |pi-    |         !/*si         49*/
!                           |pi+ nn:np|n n pi0|p n pi-                          !/*si         50*/
!    &         110,    111,    123,    214,     215,     224,     225,          !/*si         51*/
!                                                                               !/*si         52*/
! Transl:   pi+ p->|pi+ p->|pi- p ->|pi- + p->|pi- + p->|gam + p|gam +p|        !/*si         53*/
!        pi0 pi+ p |n +2pi+|2pi0 + n|p + pi- +|n pi- pi+|-> p + |-> n +|        !/*si         54*/
!           pi- n->|pi- n->|pi+ n ->|pi0      |pi+ + n->| pi0   |pi+            !/*si         55*/
!        pi0 pi- n |p +2pi-|2pi0 + p|         |p pi- pi+| (?)   | (?)           !/*si         56*/
!   &         114,    115,    126,     124,      125,    10111,  10112,         !/*si         57*/
!            Absorp:                                                            !/*si         58*/
! Transl:   gam +  |gam + p |gam + p |gam + p |gam + p |gam + p|gam + p|        !/*si         59*/
!           2N ->  |-> pi+ p|->pi0 p |->n pi+ |-> delta|-> N + |total  |        !/*si         60*/
!           2N     |+ pi-   |+ pi0   |+ pi0   | + pi   |2 pi(?)|       |        !/*si         61*/
!   &       10113,  10115,  10114,   10116,    10118,   10117,   10110/         !/*si         62*/
!                                                                               !/*si         63*/
!     data nsicst/   Absorp:  |  =0! |                                          !/*si         64*/
!  Key:    pi+ p or|pi+ pn:pp|pi+ p:|pi+ p:|pi+ n:  |pi0 n |pi0 n  |            !/*si         65*/
!           pi- n  |   or    |n 2pi0|N+ 2pi|N + 2pi | or   | or    |            !/*si         66*/
!           SCX=0! |pi- pn:nn|pi- n:|pi- n:|pi- p:  |pi0 p |pi0 p  |            !/*si         67*/
!                            |p 2pi0|N+ 2pi|N + 2pi |total |elastic|            !/*si         68*/
!   &         112,    113,    116,    117,    127,    130,    131,              !/*si         69*/
!                   Absorp:                                                     !/*si         70*/
!  Key:     pi0+p: |pi0+pn:pn|pi0+p:|pi0+p:  |pi0+p:  |pi0+p: |p + p: |         !/*si         71*/
!           pi+ + n|pi0+pp:pp|p 2pi0|p pi+pi-|n pi+pi0|N + 2pi| SCX   |         !/*si         72*/
!           pi0+n: |pi0+np:np|pi0+n:|pi0+n:  |pi0+n:  |pi0+n: |n + n: |         !/*si         73*/
!           pi- + p|pi0+nn:nn|n 2pi0|n pi+pi-|p pi-pi0|N + 2pi| = 0!  |         !/*si         74*/
!   &         132,    133,    134,    135,     136,     137,    212,            !/*si         75*/
!                                                                               !/*si         76*/
!  Key:     p + p: |p+p;n+n |p + p->|n + p: |n + p:  |n + p: |n + p->|          !/*si         77*/
!     pi absorption|chg exch|2N + pi| SCX   | Pion   |n p pi0|N N pi |          !/*si         78*/
!           n + n: |+ pi0   |n + n->|       |absorp. |Same as|       |          !/*si         79*/
!            = 0!  |  = 0!  |2N + pi| = 0!  | = 0!   |  224! |       |          !/*si         80*/
!   &         213,    216,    217,    222,     223,     226,    227/            !/*si         81*/
!                                                                               !/*si         82*/
! ======================================================================        !/*si         83*/
!   Start subroutine:                                                           !/*si         84*/
      ics = 10000*l + 1000*ms + 100*mb + 10*ksi + iks                           !/*si         85*/
      js = 1                                                                    !/*si         86*/
   10 if (ics.eq.icst(js)) then                                                 !/*si         87*/
!   (js will necessarily be <= 19, since l = 0 always[no photons]).             !/*si         88*/
        sigmat = qints (t, js)                                                  !/*si         89*/
        if (js.eq.10) sigmat = absfct*sigmat                                    !/*si         90*/
        return                                                                  !/*si         91*/
      endif                                                                     !/*si         92*/
!     if (js.lt.28) then                                                        !/*si         93*/
      if (js.lt.19) then                                                        !/*si         94*/
        js = js + 1                                                             !/*si         95*/
        go to 10                                                                !/*si         96*/
      endif                                                                     !/*si         97*/
      nsjs = 1                                                                  !/*si         98*/
   20 if (ics.ne.nsicst(nsjs)) then                                             !/*si         99*/
        nsjs = nsjs + 1                                                         !/*si        100*/
        if (nsjs.lt.21) go to 20                                                !/*si        101*/
      endif                                                                     !/*si        102*/
      kns = nsjs                                                                !/*si        103*/
!  1-4: pi+ - p  or  pi- - n  cross section                                     !/*si        104*/
      if (kns.eq.1) then                                                        !/*si        105*/
        sigmat = dp0                                                            !/*si        106*/
      elseif (kns.eq.2) then                                                    !/*si        107*/
        sigmat = absfct*qints (t, 10)                                           !/*si        108*/
      elseif (kns.eq.3) then                                                    !/*si        109*/
        sigmat = dp0                                                            !/*si        110*/
      elseif (kns.eq.4) then                                                    !/*si        111*/
!  [117] Total pi+ + p or pi- + n pion production cross section:                !/*si        112*/
        sigmat = qints (t, 15) + qints (t, 16)                                  !/*si        113*/
      elseif (kns.eq.5) then                                                    !/*si        114*/
!  [127] Total pi- + p or pi+ + n pion production cross section:                !/*si        115*/
        sigmat = qints (t, 17) + qints (t, 18) + qints (t, 19)                  !/*si        116*/
!  6-13: pi0-p  or  pi0-n  cross sections                                       !/*si        117*/
      elseif (kns.eq.6) then                                                    !/*si        118*/
        sigmat = (qints (t, 8) + qints (t, 5))/dp2                              !/*si        119*/
      elseif (kns.eq.7) then                                                    !/*si        120*/
        sigmat = (qints (t, 9) + qints (t, 6) - qints (t, 7))/dp2               !/*si        121*/
      elseif (kns.eq.8) then                                                    !/*si        122*/
        sigmat = qints (t, 7)                                                   !/*si        123*/
      elseif (kns.eq.9) then                                                    !/*si        124*/
        sigmat = absfct*qints (t, 10)                                           !/*si        125*/
      elseif (kns.eq.10) then                                                   !/*si        126*/
        sigmat = (qints (t, 15) + qints (t, 18))/dp2                            !/*si        127*/
      elseif (kns.eq.11) then                                                   !/*si        128*/
        sigmat = (qints (t, 16) + qints (t, 19))/dp2                            !/*si        129*/
      elseif (kns.eq.12) then                                                   !/*si        130*/
        sigmat = qints (t, 17)/dp2                                              !/*si        131*/
      elseif (kns.eq.13) then                                                   !/*si        132*/
!  [137] Total pi0+p or pi0+n pion production cross section:                    !/*si        133*/
        sigmat = (qints (t, 15) + qints (t, 16) + qints (t, 17) +               !/*si        134*/
     &            qints (t, 18) + qints (t, 19))/dp2                            !/*si        135*/
!  14-17: p-p  or  n-n  cross sections                                          !/*si        136*/
      elseif (kns.eq.14) then                                                   !/*si        137*/
        sigmat = dp0                                                            !/*si        138*/
      elseif (kns.eq.15) then                                                   !/*si        139*/
        sigmat = dp0                                                            !/*si        140*/
      elseif (kns.eq.16) then                                                   !/*si        141*/
        sigmat = dp0                                                            !/*si        142*/
      elseif (kns.eq.17) then                                                   !/*si        143*/
!  [217] Total p+p or n+n pion production cross section:                        !/*si        144*/
        sigmat = qints (t, 11) + qints (t, 12)                                  !/*si        145*/
!  18-21: p-n cross sections                                                    !/*si        146*/
      elseif (kns.eq.18) then                                                   !/*si        147*/
        sigmat = dp0                                                            !/*si        148*/
      elseif (kns.eq.19) then                                                   !/*si        149*/
        sigmat = dp0                                                            !/*si        150*/
      elseif (kns.eq.20) then                                                   !/*si        151*/
        sigmat = qints (t, 13)                                                  !/*si        152*/
      elseif (kns.eq.21) then                                                   !/*si        153*/
!  [227] Total p+n pion production cross section:                               !/*si        154*/
        sigmat = dp2*qints (t, 13) + qints (t, 14)                              !/*si        155*/
      endif                                                                     !/*si        156*/
#endif                                                                          /*!si        157*/
      return                                                                    !/*si        158*/
      end                                                                       !/*si        159*/
