      subroutine vlobd                                                          !/*vl          1*/
#ifdef CEM                                                                      /*!vl          2*/
! ======================================================================        !/*vl          3*/
!                                                                               !/*vl          4*/
!   Accumulate events in 218 particle-emission output channels                  !/*vl          5*/
!   and in spectrum and angular distribution arrays.                            !/*vl          6*/
!                                                                               !/*vl          7*/
!   CEM95 written by S. G. Mashnik                                              !/*vl          8*/
!                                                                               !/*vl          9*/
!   Edited by A. J. Sierk,  LANL  T-2  February, 1996.                          !/*vl         10*/
!   Modified by A. J. Sierk  May-June, 1996.                                    !/*vl         11*/
!   Modified by F. Gallmeier, ORNL CPED, June 1998.                             !/*vl         12*/
!                                                                               !/*vl         13*/
! ======================================================================        !/*vl         14*/
!                                                                               !/*vl         15*/
!  Definition of spt:                                                           !/*vl         16*/
!                       spt(1,k) = sin(theta.k)                                 !/*vl         17*/
!                       spt(2,k) = cos(theta.k)                                 !/*vl         18*/
!                       spt(3,k) = kinetic energy of particle k (GeV)           !/*vl         19*/
!                       spt(4,k) = charge of particle k                         !/*vl         20*/
!                       spt(5,k) = rest mass of particle k                      !/*vl         21*/
!  Definition of parz:                                                          !/*vl         22*/
!                       parz(1,k) = particle type #; 1-9 for n-pi+              !/*vl         23*/
!                       parz(2,k) = kinetic energy of particle k (GeV)          !/*vl         24*/
!                       parz(3,k) = theta of particle k                         !/*vl         25*/
!                       parz(4,k) = phi of particle k                           !/*vl         26*/
!                       parz(5,k) = index: <100 for cascade,                    !/*vl         27*/
!                       negative for hole;                                      !/*vl         28*/
!                       = 100 for preq., or = 1000 for thermal.                 !/*vl         29*/
!                       parz(6,k) = electric charge of particle k               !/*vl         30*/
!                                                                               !/*vl         31*/
! ======================================================================        !/*vl         32*/
#include <cem/cdouble.h>                                                        /*!vl         33*/
      common /begin/  cm0, t0gev, me0, mb0, ms0, l0                             !/*vl         34*/
      common /bl1003/ ut, at, zt                                                !/*vl         35*/
      common /blok77/ spt(5,150)                                                !/*vl         36*/
      common /d2sdto/ tet1(10), tet2(10), te1(140), te2(140),                   !/*vl         37*/
     &                dtt(140), se(140), dtdo(140, 10),                         !/*vl         38*/
     &                d2spec(9,10,140), d2spe(14,10,140), ntt, ntet,            !/*vl         39*/
     &                nt2, nt3                                                  !/*vl         40*/
      common /degrad/ degrad                                                    !/*vl         41*/
      common /dele/   sfu, wf, fusion, sigfw                                    !/*vl         42*/
      common /deltt/  dteta                                                     !/*vl         43*/
      common /menu/   mspec, mpyld, mchy, misy, mdubl, mang                     !/*vl         44*/
      common /menu1/  ipar1, ipar2, ifam, ib, ish, idelta, ibe, idel            !/*vl         45*/
      common /o/      anucl, znucl, eps, vpi, rsm(10), rbig(10),                !/*vl         46*/
     &                rhop(10), rhon(10), af(10), tfp(10), tfn(10), n           !/*vl         47*/
      common /pii/     pi                                                       !/*vl         48*/
      common /result/ spec(9,140), ang(9,20), chan(218), dadz(50,18)            !/*vl         49*/
      common /resmac/ spe(14,140), an(14,20)                                    !/*vl         50*/
      common /zapp/   parz(6,150)                                               !/*vl         51*/
      data zro, one, fmil /0.d0, 1.d0, 1.d3/                                    !/*vl         52*/
! ======================================================================        !/*vl         53*/
      if (mspec.ge.0 .or. mang.ge.0 .or. mdubl.ge.0 .or. mchy.ge.0 .or.         !/*vl         54*/
     &    mpyld.ge.0) then                                                      !/*vl         55*/
        nn = 0                                                                  !/*vl         56*/
        np = 0                                                                  !/*vl         57*/
        nd = 0                                                                  !/*vl         58*/
        nt = 0                                                                  !/*vl         59*/
        nhe3 = 0                                                                !/*vl         60*/
        nhe4 = 0                                                                !/*vl         61*/
        npimin = 0                                                              !/*vl         62*/
        npizer = 0                                                              !/*vl         63*/
        npiplu = 0                                                              !/*vl         64*/
          do 50 m = 1,150                                                       !/*vl         65*/
          if (spt(5,m).lt.dp1m3) go to 60                                       !/*vl         66*/
          ipar = int(parz(1,m) + dp1m1)                                         !/*vl         67*/
          ipoc = int(parz(5,m) + dp1m1)                                         !/*vl         68*/
          tt = parz(2,m)*fmil                                                   !/*vl         69*/
          itet = 1                                                              !/*vl         70*/
   20     it = 1                                                                !/*vl         71*/
   40     iu = 1                                                                !/*vl         72*/
          if (ipar.eq.1) then                                                   !/*vl         73*/
            nn = nn + 1                                                         !/*vl         74*/
            if (ipoc.lt.100) then                                               !/*vl         75*/
              ip = 1                                                            !/*vl         76*/
            elseif (ipoc.eq.100) then                                           !/*vl         77*/
              ip = 2                                                            !/*vl         78*/
            elseif (ipoc.gt.100) then                                           !/*vl         79*/
              ip = 3                                                            !/*vl         80*/
            endif                                                               !/*vl         81*/
          elseif (ipar.eq.2) then                                               !/*vl         82*/
            np = np + 1                                                         !/*vl         83*/
            if (ipoc.lt.100) then                                               !/*vl         84*/
              ip = 4                                                            !/*vl         85*/
            elseif (ipoc.eq.100) then                                           !/*vl         86*/
              ip = 5                                                            !/*vl         87*/
            elseif (ipoc.gt.100) then                                           !/*vl         88*/
              ip = 6                                                            !/*vl         89*/
            endif                                                               !/*vl         90*/
          elseif (ipar.eq.3) then                                               !/*vl         91*/
            nd = nd + 1                                                         !/*vl         92*/
            if (ipoc.lt.1000) then                                              !/*vl         93*/
              ip = 7                                                            !/*vl         94*/
            else                                                                !/*vl         95*/
              ip = 8                                                            !/*vl         96*/
            endif                                                               !/*vl         97*/
          elseif (ipar.eq.4) then                                               !/*vl         98*/
            nt = nt + 1                                                         !/*vl         99*/
            if (ipoc.lt.1000) then                                              !/*vl        100*/
              ip = 9                                                            !/*vl        101*/
            else                                                                !/*vl        102*/
              ip = 10                                                           !/*vl        103*/
            endif                                                               !/*vl        104*/
          elseif (ipar.eq.5) then                                               !/*vl        105*/
            nhe3 = nhe3 + 1                                                     !/*vl        106*/
            if (ipoc.lt.1000) then                                              !/*vl        107*/
              ip = 11                                                           !/*vl        108*/
            else                                                                !/*vl        109*/
              ip = 12                                                           !/*vl        110*/
            endif                                                               !/*vl        111*/
          elseif (ipar.eq.6) then                                               !/*vl        112*/
            nhe4 = nhe4 + 1                                                     !/*vl        113*/
            if (ipoc.lt.1000) then                                              !/*vl        114*/
              ip = 13                                                           !/*vl        115*/
            else                                                                !/*vl        116*/
              ip = 14                                                           !/*vl        117*/
            endif                                                               !/*vl        118*/
          elseif (ipar.ge.7) then                                               !/*vl        119*/
            if (ipar.eq.7) then                                                 !/*vl        120*/
              npimin = npimin + 1                                               !/*vl        121*/
            elseif (ipar.eq.8) then                                             !/*vl        122*/
              npizer = npizer + 1                                               !/*vl        123*/
            elseif (ipar.eq.9) then                                             !/*vl        124*/
              npiplu = npiplu + 1                                               !/*vl        125*/
            endif                                                               !/*vl        126*/
          endif                                                                 !/*vl        127*/
          spec(ipar,it) = spec(ipar,it) + one                                   !/*vl        128*/
          spec(ipar,nt2) = spec(ipar,nt2) + tt                                  !/*vl        129*/
          spec(ipar,nt3) = spec(ipar,nt3) + one                                 !/*vl        130*/
          d2spec(ipar,iu,it) = d2spec(ipar,iu,it) + one                         !/*vl        131*/
          d2spec(ipar,iu,nt2) = d2spec(ipar,iu,nt2) + tt                        !/*vl        132*/
          d2spec(ipar,iu,nt3) = d2spec(ipar,iu,nt3) + one                       !/*vl        133*/
          ang(ipar,itet) = ang(ipar,itet) + one                                 !/*vl        134*/
          ang(ipar,20) = ang(ipar,20) + one                                     !/*vl        135*/
          if (ipar.le.6) then                                                   !/*vl        136*/
            d2spe(ip,iu,it) = d2spe(ip,iu,it) + one                             !/*vl        137*/
            d2spe(ip,iu,nt2) = d2spe(ip,iu,nt2) + tt                            !/*vl        138*/
            d2spe(ip,iu,nt3) = d2spe(ip,iu,nt3) + one                           !/*vl        139*/
            spe(ip,it) = spe(ip,it) + one                                       !/*vl        140*/
            spe(ip,nt2) = spe(ip,nt2) + tt                                      !/*vl        141*/
            spe(ip,nt3) = spe(ip,nt3) + one                                     !/*vl        142*/
            an(ip,itet) = an(ip,itet) + one                                     !/*vl        143*/
            an(ip,20) = an(ip,20) + one                                         !/*vl        144*/
          endif                                                                 !/*vl        145*/
   50     continue                                                              !/*vl        146*/
   60   continue                                                                !/*vl        147*/
        if (mchy.lt.0) go to 70                                                 !/*vl        148*/
        if (npimin.gt.0) chan(216) = chan(216) + npimin                         !/*vl        149*/
        if (npizer.gt.0) chan(217) = chan(217) + npizer                         !/*vl        150*/
        if (npiplu.gt.0) chan(218) = chan(218) + npiplu                         !/*vl        151*/
        ic = 0                                                                  !/*vl        152*/
        if (nn.eq.1) then                                                       !/*vl        153*/
!    This is (1n) channel:                                                      !/*vl        154*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        155*/
     &                .and. nhe4.eq.0) ic = 1                                   !/*vl        156*/
!    This is (np) channel:                                                      !/*vl        157*/
          if (np.eq.1 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        158*/
     &                .and. nhe4.eq.0) ic = 26                                  !/*vl        159*/
!    This is (n2p) channel:                                                     !/*vl        160*/
          if (np.eq.2 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        161*/
     &                .and. nhe4.eq.0) ic = 45                                  !/*vl        162*/
!    This is (n3p) channel:                                                     !/*vl        163*/
          if (np.eq.3 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        164*/
     &                .and. nhe4.eq.0) ic = 63                                  !/*vl        165*/
!    This is (n4p) channel:                                                     !/*vl        166*/
          if (np.eq.4 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        167*/
     &                .and. nhe4.eq.0) ic = 80                                  !/*vl        168*/
!    This is (n5p) channel:                                                     !/*vl        169*/
          if (np.eq.5 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        170*/
     &                .and. nhe4.eq.0) ic = 96                                  !/*vl        171*/
!    This is (nd) channel:                                                      !/*vl        172*/
          if (np.eq.0 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        173*/
     &                .and. nhe4.eq.0) ic = 112                                 !/*vl        174*/
!    This is (npd) channel:                                                     !/*vl        175*/
          if (np.eq.1 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        176*/
     &                .and. nhe4.eq.0) ic = 132                                 !/*vl        177*/
!    This is (n2pd) channel:                                                    !/*vl        178*/
          if (np.eq.2 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        179*/
     &                .and. nhe4.eq.0) ic = 147                                 !/*vl        180*/
!    This is (nHe4) channel:                                                    !/*vl        181*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        182*/
     &                .and. nhe4.eq.1) ic = 162                                 !/*vl        183*/
!    This is (npHe4) channel:                                                   !/*vl        184*/
          if (np.eq.1 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        185*/
     &                .and. nhe4.eq.1) ic = 179                                 !/*vl        186*/
!    This is (n2He4) channel:                                                   !/*vl        187*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        188*/
     &                .and. nhe4.eq.2) ic = 176                                 !/*vl        189*/
        elseif (nn.eq.2) then                                                   !/*vl        190*/
!    This is (2n) channel:                                                      !/*vl        191*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        192*/
     &                .and. nhe4.eq.0) ic = 2                                   !/*vl        193*/
!    This is (2np) channel:                                                     !/*vl        194*/
          if (np.eq.1 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        195*/
     &                .and. nhe4.eq.0) ic = 27                                  !/*vl        196*/
!    This is (2n2p) channel:                                                    !/*vl        197*/
          if (np.eq.2 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        198*/
     &                .and. nhe4.eq.0) ic = 46                                  !/*vl        199*/
!    This is (2n3p) channel:                                                    !/*vl        200*/
          if (np.eq.3 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        201*/
     &                .and. nhe4.eq.0) ic = 64                                  !/*vl        202*/
!    This is (2n4p) channel:                                                    !/*vl        203*/
          if (np.eq.4 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        204*/
     &                .and. nhe4.eq.0) ic = 81                                  !/*vl        205*/
!    This is (2n5p) channel:                                                    !/*vl        206*/
          if (np.eq.5 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        207*/
     &                .and. nhe4.eq.0) ic = 97                                  !/*vl        208*/
!    This is (2nd) channel:                                                     !/*vl        209*/
          if (np.eq.0 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        210*/
     &                .and. nhe4.eq.0) ic = 113                                 !/*vl        211*/
!    This is (2npd) channel:                                                    !/*vl        212*/
          if (np.eq.1 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        213*/
     &                .and. nhe4.eq.0) ic = 133                                 !/*vl        214*/
!    This is (2n2pd) channel:                                                   !/*vl        215*/
          if (np.eq.2 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        216*/
     &                .and. nhe4.eq.0) ic = 148                                 !/*vl        217*/
!    This is (2nHe4) channel:                                                   !/*vl        218*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        219*/
     &                .and. nhe4.eq.1) ic = 163                                 !/*vl        220*/
!    This is (2npHe4) channel:                                                  !/*vl        221*/
          if (np.eq.1 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        222*/
     &                .and. nhe4.eq.1) ic = 180                                 !/*vl        223*/
!    This is (2n2He4) channel:                                                  !/*vl        224*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        225*/
     &                .and. nhe4.eq.2) ic = 177                                 !/*vl        226*/
        elseif (nn.eq.3) then                                                   !/*vl        227*/
!    This is (3n) channel:                                                      !/*vl        228*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        229*/
     &                .and. nhe4.eq.0) ic = 3                                   !/*vl        230*/
!    This is (3np) channel:                                                     !/*vl        231*/
          if (np.eq.1 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        232*/
     &                .and. nhe4.eq.0) ic = 28                                  !/*vl        233*/
!    This is (3n2p) channel:                                                    !/*vl        234*/
          if (np.eq.2 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        235*/
     &                .and. nhe4.eq.0) ic = 47                                  !/*vl        236*/
!    This is (3n3p) channel:                                                    !/*vl        237*/
          if (np.eq.3 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        238*/
     &                .and. nhe4.eq.0) ic = 65                                  !/*vl        239*/
!    This is (3n4p) channel:                                                    !/*vl        240*/
          if (np.eq.4 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        241*/
     &                .and. nhe4.eq.0) ic = 82                                  !/*vl        242*/
!    This is (3n5p) channel:                                                    !/*vl        243*/
          if (np.eq.5 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        244*/
     &                .and. nhe4.eq.0) ic = 98                                  !/*vl        245*/
!    This is (3nd) channel:                                                     !/*vl        246*/
          if (np.eq.0 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        247*/
     &                .and. nhe4.eq.0) ic = 114                                 !/*vl        248*/
!    This is (3npd) channel:                                                    !/*vl        249*/
          if (np.eq.1 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        250*/
     &                .and. nhe4.eq.0) ic = 134                                 !/*vl        251*/
!    This is (3n2pd) channel:                                                   !/*vl        252*/
          if (np.eq.2 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        253*/
     &                .and. nhe4.eq.0) ic = 149                                 !/*vl        254*/
!    This is (3nHe4) channel:                                                   !/*vl        255*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        256*/
     &                .and. nhe4.eq.1) ic = 164                                 !/*vl        257*/
!    This is (3npHe4) channel:                                                  !/*vl        258*/
          if (np.eq.1 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        259*/
     &                .and. nhe4.eq.1) ic = 181                                 !/*vl        260*/
        elseif (nn.eq.4) then                                                   !/*vl        261*/
!    This is (4n) channel:                                                      !/*vl        262*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        263*/
     &                .and. nhe4.eq.0) ic = 4                                   !/*vl        264*/
!    This is (4np) channel:                                                     !/*vl        265*/
          if (np.eq.1 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        266*/
     &                .and. nhe4.eq.0) ic = 29                                  !/*vl        267*/
!    This is (4n2p) channel:                                                    !/*vl        268*/
          if (np.eq.2 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        269*/
     &                .and. nhe4.eq.0) ic = 48                                  !/*vl        270*/
!    This is (4n3p) channel:                                                    !/*vl        271*/
          if (np.eq.3 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        272*/
     &                .and. nhe4.eq.0) ic = 66                                  !/*vl        273*/
!    This is (4n4p) channel:                                                    !/*vl        274*/
          if (np.eq.4 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        275*/
     &                .and. nhe4.eq.0) ic = 83                                  !/*vl        276*/
!    This is (4n5p) channel:                                                    !/*vl        277*/
          if (np.eq.5 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        278*/
     &                .and. nhe4.eq.0) ic = 99                                  !/*vl        279*/
!    This is (4nd) channel:                                                     !/*vl        280*/
          if (np.eq.0 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        281*/
     &                .and. nhe4.eq.0) ic = 115                                 !/*vl        282*/
!    This is (4npd) channel:                                                    !/*vl        283*/
          if (np.eq.1 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        284*/
     &                .and. nhe4.eq.0) ic = 135                                 !/*vl        285*/
!    This is (4n2pd) channel:                                                   !/*vl        286*/
          if (np.eq.2 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        287*/
     &                .and. nhe4.eq.0) ic = 150                                 !/*vl        288*/
!    This is (4nHe4) channel:                                                   !/*vl        289*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        290*/
     &                .and. nhe4.eq.1) ic = 165                                 !/*vl        291*/
!    This is (4npHe4) channel:                                                  !/*vl        292*/
          if (np.eq.1 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        293*/
     &                .and. nhe4.eq.1) ic = 182                                 !/*vl        294*/
        elseif (nn.eq.5) then                                                   !/*vl        295*/
!    This is (5n) channel:                                                      !/*vl        296*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        297*/
     &                .and. nhe4.eq.0) ic = 5                                   !/*vl        298*/
!    This is (5np) channel:                                                     !/*vl        299*/
          if (np.eq.1 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        300*/
     &                .and. nhe4.eq.0) ic = 30                                  !/*vl        301*/
!    This is (5n2p) channel:                                                    !/*vl        302*/
          if (np.eq.2 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        303*/
     &                .and. nhe4.eq.0) ic = 49                                  !/*vl        304*/
!    This is (5n3p) channel:                                                    !/*vl        305*/
          if (np.eq.3 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        306*/
     &                .and. nhe4.eq.0) ic = 67                                  !/*vl        307*/
!    This is (5n4p) channel:                                                    !/*vl        308*/
          if (np.eq.4 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        309*/
     &                .and. nhe4.eq.0) ic = 84                                  !/*vl        310*/
!    This is (5n5p) channel:                                                    !/*vl        311*/
          if (np.eq.5 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        312*/
     &                .and. nhe4.eq.0) ic = 100                                 !/*vl        313*/
!    This is (5nd) channel:                                                     !/*vl        314*/
          if (np.eq.0 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        315*/
     &                .and. nhe4.eq.0) ic = 116                                 !/*vl        316*/
!    This is (5npd) channel:                                                    !/*vl        317*/
          if (np.eq.1 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        318*/
     &                .and. nhe4.eq.0) ic = 136                                 !/*vl        319*/
!    This is (5n2pd) channel:                                                   !/*vl        320*/
          if (np.eq.2 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        321*/
     &                .and. nhe4.eq.0) ic = 151                                 !/*vl        322*/
!    This is (5nHe4) channel:                                                   !/*vl        323*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        324*/
     &                .and. nhe4.eq.1) ic = 166                                 !/*vl        325*/
!    This is (5npHe4) channel:                                                  !/*vl        326*/
          if (np.eq.1 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        327*/
     &                .and. nhe4.eq.1) ic = 183                                 !/*vl        328*/
        elseif (nn.eq.6) then                                                   !/*vl        329*/
!    This is (6n) channel:                                                      !/*vl        330*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        331*/
     &                .and. nhe4.eq.0) ic = 6                                   !/*vl        332*/
!    This is (6np) channel:                                                     !/*vl        333*/
          if (np.eq.1 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        334*/
     &                .and. nhe4.eq.0) ic = 31                                  !/*vl        335*/
!    This is (6n2p) channel:                                                    !/*vl        336*/
          if (np.eq.2 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        337*/
     &                .and. nhe4.eq.0) ic = 50                                  !/*vl        338*/
!    This is (6n3p) channel:                                                    !/*vl        339*/
          if (np.eq.3 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        340*/
     &                .and. nhe4.eq.0) ic = 68                                  !/*vl        341*/
!    This is (6n4p) channel:                                                    !/*vl        342*/
          if (np.eq.4 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        343*/
     &                .and. nhe4.eq.0) ic = 85                                  !/*vl        344*/
!    This is (6n5p) channel:                                                    !/*vl        345*/
          if (np.eq.5 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        346*/
     &                .and. nhe4.eq.0) ic = 101                                 !/*vl        347*/
!    This is (6nd) channel:                                                     !/*vl        348*/
          if (np.eq.0 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        349*/
     &                .and. nhe4.eq.0) ic = 117                                 !/*vl        350*/
!    This is (6npd) channel:                                                    !/*vl        351*/
          if (np.eq.1 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        352*/
     &                .and. nhe4.eq.0) ic = 137                                 !/*vl        353*/
!    This is (6n2pd) channel:                                                   !/*vl        354*/
          if (np.eq.2 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        355*/
     &                .and. nhe4.eq.0) ic = 152                                 !/*vl        356*/
!    This is (6nHe4) channel:                                                   !/*vl        357*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        358*/
     &                .and. nhe4.eq.1) ic = 167                                 !/*vl        359*/
!    This is (6npHe4) channel:                                                  !/*vl        360*/
          if (np.eq.1 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        361*/
     &                .and. nhe4.eq.1) ic = 184                                 !/*vl        362*/
        elseif (nn.eq.7) then                                                   !/*vl        363*/
!    This is (7n) channel:                                                      !/*vl        364*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        365*/
     &                .and. nhe4.eq.0) ic = 7                                   !/*vl        366*/
!    This is (7np) channel:                                                     !/*vl        367*/
          if (np.eq.1 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        368*/
     &                .and. nhe4.eq.0) ic = 32                                  !/*vl        369*/
!    This is (7n2p) channel:                                                    !/*vl        370*/
          if (np.eq.2 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        371*/
     &                .and. nhe4.eq.0) ic = 51                                  !/*vl        372*/
!    This is (7n3p) channel:                                                    !/*vl        373*/
          if (np.eq.3 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        374*/
     &                .and. nhe4.eq.0) ic = 69                                  !/*vl        375*/
!    This is (7n4p) channel:                                                    !/*vl        376*/
          if (np.eq.4 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        377*/
     &                .and. nhe4.eq.0) ic = 86                                  !/*vl        378*/
!    This is (7n5p) channel:                                                    !/*vl        379*/
          if (np.eq.5 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        380*/
     &                .and. nhe4.eq.0) ic = 102                                 !/*vl        381*/
!    This is (7nd) channel:                                                     !/*vl        382*/
          if (np.eq.0 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        383*/
     &                .and. nhe4.eq.0) ic = 118                                 !/*vl        384*/
!    This is (7npd) channel:                                                    !/*vl        385*/
          if (np.eq.1 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        386*/
     &                .and. nhe4.eq.0) ic = 138                                 !/*vl        387*/
!    This is (7n2pd) channel:                                                   !/*vl        388*/
          if (np.eq.2 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        389*/
     &                .and. nhe4.eq.0) ic = 153                                 !/*vl        390*/
!    This is (7nHe4) channel:                                                   !/*vl        391*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        392*/
     &                .and. nhe4.eq.1) ic = 168                                 !/*vl        393*/
!    This is (7npHe4) channel:                                                  !/*vl        394*/
          if (np.eq.1 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        395*/
     &                .and. nhe4.eq.1) ic = 185                                 !/*vl        396*/
        elseif (nn.eq.8) then                                                   !/*vl        397*/
!    This is (8n) channel:                                                      !/*vl        398*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        399*/
     &                .and. nhe4.eq.0) ic = 8                                   !/*vl        400*/
!    This is (8np) channel:                                                     !/*vl        401*/
          if (np.eq.1 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        402*/
     &                .and. nhe4.eq.0) ic = 33                                  !/*vl        403*/
!    This is (8n2p) channel:                                                    !/*vl        404*/
          if (np.eq.2 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        405*/
     &                .and. nhe4.eq.0) ic = 52                                  !/*vl        406*/
!    This is (8n3p) channel:                                                    !/*vl        407*/
          if (np.eq.3 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        408*/
     &                .and. nhe4.eq.0) ic = 70                                  !/*vl        409*/
!    This is (8n4p) channel:                                                    !/*vl        410*/
          if (np.eq.4 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        411*/
     &                .and. nhe4.eq.0) ic = 87                                  !/*vl        412*/
!    This is (8n5p) channel:                                                    !/*vl        413*/
          if (np.eq.5 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        414*/
     &                .and. nhe4.eq.0) ic = 103                                 !/*vl        415*/
!    This is (8nd) channel:                                                     !/*vl        416*/
          if (np.eq.0 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        417*/
     &                .and. nhe4.eq.0) ic = 119                                 !/*vl        418*/
!    This is (8npd) channel:                                                    !/*vl        419*/
          if (np.eq.1 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        420*/
     &                .and. nhe4.eq.0) ic = 139                                 !/*vl        421*/
!    This is (8n2pd) channel:                                                   !/*vl        422*/
          if (np.eq.2 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        423*/
     &                .and. nhe4.eq.0) ic = 154                                 !/*vl        424*/
!    This is (8nHe4) channel:                                                   !/*vl        425*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        426*/
     &                .and. nhe4.eq.1) ic = 169                                 !/*vl        427*/
!    This is (8npHe4) channel:                                                  !/*vl        428*/
          if (np.eq.1 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        429*/
     &                .and. nhe4.eq.1) ic = 186                                 !/*vl        430*/
        elseif (nn.eq.9) then                                                   !/*vl        431*/
!    This is (9n) channel:                                                      !/*vl        432*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        433*/
     &                .and. nhe4.eq.0) ic = 9                                   !/*vl        434*/
!    This is (9np) channel:                                                     !/*vl        435*/
          if (np.eq.1 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        436*/
     &                .and. nhe4.eq.0) ic = 34                                  !/*vl        437*/
!    This is (9n2p) channel:                                                    !/*vl        438*/
          if (np.eq.2 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        439*/
     &                .and. nhe4.eq.0) ic = 53                                  !/*vl        440*/
!    This is (9n3p) channel:                                                    !/*vl        441*/
          if (np.eq.3 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        442*/
     &                .and. nhe4.eq.0) ic = 71                                  !/*vl        443*/
!    This is (9n4p) channel:                                                    !/*vl        444*/
          if (np.eq.4 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        445*/
     &                .and. nhe4.eq.0) ic = 88                                  !/*vl        446*/
!    This is (9n5p) channel:                                                    !/*vl        447*/
          if (np.eq.5 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        448*/
     &                .and. nhe4.eq.0) ic = 104                                 !/*vl        449*/
!    This is (9nd) channel:                                                     !/*vl        450*/
          if (np.eq.0 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        451*/
     &                .and. nhe4.eq.0) ic = 120                                 !/*vl        452*/
!    This is (9npd) channel:                                                    !/*vl        453*/
          if (np.eq.1 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        454*/
     &                .and. nhe4.eq.0) ic = 140                                 !/*vl        455*/
!    This is (9n2pd) channel:                                                   !/*vl        456*/
          if (np.eq.2 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        457*/
     &                .and. nhe4.eq.0) ic = 155                                 !/*vl        458*/
!    This is (9nHe4) channel:                                                   !/*vl        459*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        460*/
     &                .and. nhe4.eq.1) ic = 170                                 !/*vl        461*/
!    This is (9npHe4) channel:                                                  !/*vl        462*/
          if (np.eq.1 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        463*/
     &                .and. nhe4.eq.1) ic = 187                                 !/*vl        464*/
        elseif (nn.eq.10) then                                                  !/*vl        465*/
!    This is (10n) channel:                                                     !/*vl        466*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        467*/
     &                .and. nhe4.eq.0) ic = 10                                  !/*vl        468*/
!    This is (10np) channel:                                                    !/*vl        469*/
          if (np.eq.1 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        470*/
     &                .and. nhe4.eq.0) ic = 35                                  !/*vl        471*/
!    This is (10n2p) channel:                                                   !/*vl        472*/
          if (np.eq.2 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        473*/
     &                .and. nhe4.eq.0) ic = 54                                  !/*vl        474*/
!    This is (10n3p) channel:                                                   !/*vl        475*/
          if (np.eq.3 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        476*/
     &                .and. nhe4.eq.0) ic = 72                                  !/*vl        477*/
!    This is (10n4p) channel:                                                   !/*vl        478*/
          if (np.eq.4 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        479*/
     &                .and. nhe4.eq.0) ic = 89                                  !/*vl        480*/
!    This is (10n5p) channel:                                                   !/*vl        481*/
          if (np.eq.5 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        482*/
     &                .and. nhe4.eq.0) ic = 105                                 !/*vl        483*/
!    This is (10nd) channel:                                                    !/*vl        484*/
          if (np.eq.0 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        485*/
     &                .and. nhe4.eq.0) ic = 121                                 !/*vl        486*/
!    This is (10npd) channel:                                                   !/*vl        487*/
          if (np.eq.1 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        488*/
     &                .and. nhe4.eq.0) ic = 141                                 !/*vl        489*/
!    This is (10n2pd) channel:                                                  !/*vl        490*/
          if (np.eq.2 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        491*/
     &                .and. nhe4.eq.0) ic = 156                                 !/*vl        492*/
!    This is (10nHe4) channel:                                                  !/*vl        493*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        494*/
     &                .and. nhe4.eq.1) ic = 171                                 !/*vl        495*/
!    This is (10npHe4) channel:                                                 !/*vl        496*/
          if (np.eq.1 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        497*/
     &                .and. nhe4.eq.1) ic = 188                                 !/*vl        498*/
        elseif (nn.eq.11) then                                                  !/*vl        499*/
!    This is (11n) channel:                                                     !/*vl        500*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        501*/
     &                .and. nhe4.eq.0) ic = 11                                  !/*vl        502*/
!    This is (11np) channel:                                                    !/*vl        503*/
          if (np.eq.1 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        504*/
     &                .and. nhe4.eq.0) ic = 36                                  !/*vl        505*/
!    This is (11n2p) channel:                                                   !/*vl        506*/
          if (np.eq.2 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        507*/
     &                .and. nhe4.eq.0) ic = 55                                  !/*vl        508*/
!    This is (11n3p) channel:                                                   !/*vl        509*/
          if (np.eq.3 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        510*/
     &                .and. nhe4.eq.0) ic = 73                                  !/*vl        511*/
!    This is (11n4p) channel:                                                   !/*vl        512*/
          if (np.eq.4 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        513*/
     &                .and. nhe4.eq.0) ic = 90                                  !/*vl        514*/
!    This is (11n5p) channel:                                                   !/*vl        515*/
          if (np.eq.5 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        516*/
     &                .and. nhe4.eq.0) ic = 106                                 !/*vl        517*/
!    This is (11nd) channel:                                                    !/*vl        518*/
          if (np.eq.0 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        519*/
     &                .and. nhe4.eq.0) ic = 122                                 !/*vl        520*/
!    This is (11npd) channel:                                                   !/*vl        521*/
          if (np.eq.1 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        522*/
     &                .and. nhe4.eq.0) ic = 142                                 !/*vl        523*/
!    This is (11n2pd) channel:                                                  !/*vl        524*/
          if (np.eq.2 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        525*/
     &                .and. nhe4.eq.0) ic = 157                                 !/*vl        526*/
!    This is (11nHe4) channel:                                                  !/*vl        527*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        528*/
     &                .and. nhe4.eq.1) ic = 172                                 !/*vl        529*/
!    This is (11npHe4) channel:                                                 !/*vl        530*/
          if (np.eq.1 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        531*/
     &                .and. nhe4.eq.1) ic = 189                                 !/*vl        532*/
        elseif (nn.eq.12) then                                                  !/*vl        533*/
!    This is (12n) channel:                                                     !/*vl        534*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        535*/
     &                .and. nhe4.eq.0) ic = 12                                  !/*vl        536*/
!    This is (12np) channel:                                                    !/*vl        537*/
          if (np.eq.1 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        538*/
     &                .and. nhe4.eq.0) ic = 37                                  !/*vl        539*/
!    This is (12n2p) channel:                                                   !/*vl        540*/
          if (np.eq.2 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        541*/
     &                .and. nhe4.eq.0) ic = 56                                  !/*vl        542*/
!    This is (12n3p) channel:                                                   !/*vl        543*/
          if (np.eq.3 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        544*/
     &                .and. nhe4.eq.0) ic = 74                                  !/*vl        545*/
!    This is (12n4p) channel:                                                   !/*vl        546*/
          if (np.eq.4 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        547*/
     &                .and. nhe4.eq.0) ic = 91                                  !/*vl        548*/
!    This is (12n5p) channel:                                                   !/*vl        549*/
          if (np.eq.5 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        550*/
     &                .and. nhe4.eq.0) ic = 107                                 !/*vl        551*/
!    This is (12nd) channel:                                                    !/*vl        552*/
          if (np.eq.0 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        553*/
     &                .and. nhe4.eq.0) ic = 123                                 !/*vl        554*/
!    This is (12npd) channel:                                                   !/*vl        555*/
          if (np.eq.1 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        556*/
     &                .and. nhe4.eq.0) ic = 143                                 !/*vl        557*/
!    This is (12n2pd) channel:                                                  !/*vl        558*/
          if (np.eq.2 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        559*/
     &                .and. nhe4.eq.0) ic = 158                                 !/*vl        560*/
!    This is (12nHe4) channel:                                                  !/*vl        561*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        562*/
     &                .and. nhe4.eq.1) ic = 173                                 !/*vl        563*/
!    This is (12npHe4) channel:                                                 !/*vl        564*/
          if (np.eq.1 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        565*/
     &                .and. nhe4.eq.1) ic = 190                                 !/*vl        566*/
        elseif (nn.eq.13) then                                                  !/*vl        567*/
!    This is (13n) channel:                                                     !/*vl        568*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        569*/
     &                .and. nhe4.eq.0) ic = 13                                  !/*vl        570*/
!    This is (13np) channel:                                                    !/*vl        571*/
          if (np.eq.1 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        572*/
     &                .and. nhe4.eq.0) ic = 38                                  !/*vl        573*/
!    This is (13n2p) channel:                                                   !/*vl        574*/
          if (np.eq.2 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        575*/
     &                .and. nhe4.eq.0) ic = 57                                  !/*vl        576*/
!    This is (13n3p) channel:                                                   !/*vl        577*/
          if (np.eq.3 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        578*/
     &                .and. nhe4.eq.0) ic = 75                                  !/*vl        579*/
!    This is (13n4p) channel:                                                   !/*vl        580*/
          if (np.eq.4 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        581*/
     &                .and. nhe4.eq.0) ic = 92                                  !/*vl        582*/
!    This is (13n5p) channel:                                                   !/*vl        583*/
          if (np.eq.5 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        584*/
     &                .and. nhe4.eq.0) ic = 108                                 !/*vl        585*/
!    This is (13nd) channel:                                                    !/*vl        586*/
          if (np.eq.0 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        587*/
     &                .and. nhe4.eq.0) ic = 124                                 !/*vl        588*/
!    This is (13npd) channel:                                                   !/*vl        589*/
          if (np.eq.1 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        590*/
     &                .and. nhe4.eq.0) ic = 144                                 !/*vl        591*/
!    This is (13nHe4) channel:                                                  !/*vl        592*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        593*/
     &                .and. nhe4.eq.1) ic = 174                                 !/*vl        594*/
!    This is (13npHe4) channel:                                                 !/*vl        595*/
          if (np.eq.1 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        596*/
     &                .and. nhe4.eq.1) ic = 191                                 !/*vl        597*/
        elseif (nn.eq.14) then                                                  !/*vl        598*/
!    This is (14n) channel:                                                     !/*vl        599*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        600*/
     &                .and. nhe4.eq.0) ic = 14                                  !/*vl        601*/
!    This is (14np) channel:                                                    !/*vl        602*/
          if (np.eq.1 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        603*/
     &                .and. nhe4.eq.0) ic = 39                                  !/*vl        604*/
!    This is (14n2p) channel:                                                   !/*vl        605*/
          if (np.eq.2 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        606*/
     &                .and. nhe4.eq.0) ic = 58                                  !/*vl        607*/
!    This is (14n3p) channel:                                                   !/*vl        608*/
          if (np.eq.3 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        609*/
     &                .and. nhe4.eq.0) ic = 76                                  !/*vl        610*/
!    This is (14n4p) channel:                                                   !/*vl        611*/
          if (np.eq.4 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        612*/
     &                .and. nhe4.eq.0) ic = 93                                  !/*vl        613*/
!    This is (14n5p) channel:                                                   !/*vl        614*/
          if (np.eq.5 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        615*/
     &                .and. nhe4.eq.0) ic = 109                                 !/*vl        616*/
!    This is (14nd) channel:                                                    !/*vl        617*/
          if (np.eq.0 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        618*/
     &                .and. nhe4.eq.0) ic = 125                                 !/*vl        619*/
!    This is (14npd) channel:                                                   !/*vl        620*/
          if (np.eq.1 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        621*/
     &                .and. nhe4.eq.0) ic = 145                                 !/*vl        622*/
!    This is (14nHe4) channel:                                                  !/*vl        623*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        624*/
     &                .and. nhe4.eq.1) ic = 175                                 !/*vl        625*/
!    This is (14npHe4) channel:                                                 !/*vl        626*/
          if (np.eq.1 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        627*/
     &                .and. nhe4.eq.1) ic = 192                                 !/*vl        628*/
        elseif (nn.eq.15) then                                                  !/*vl        629*/
!    This is (15n) channel:                                                     !/*vl        630*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        631*/
     &                .and. nhe4.eq.0) ic = 15                                  !/*vl        632*/
!    This is (15np) channel:                                                    !/*vl        633*/
          if (np.eq.1 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        634*/
     &                .and. nhe4.eq.0) ic = 40                                  !/*vl        635*/
!    This is (15n2p) channel:                                                   !/*vl        636*/
          if (np.eq.2 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        637*/
     &                .and. nhe4.eq.0) ic = 59                                  !/*vl        638*/
!    This is (15n3p) channel:                                                   !/*vl        639*/
          if (np.eq.3 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        640*/
     &                .and. nhe4.eq.0) ic = 77                                  !/*vl        641*/
!    This is (15n4p) channel:                                                   !/*vl        642*/
          if (np.eq.4 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        643*/
     &                .and. nhe4.eq.0) ic = 94                                  !/*vl        644*/
!    This is (15n5p) channel:                                                   !/*vl        645*/
          if (np.eq.5 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        646*/
     &                .and. nhe4.eq.0) ic = 110                                 !/*vl        647*/
!    This is (15nd) channel:                                                    !/*vl        648*/
          if (np.eq.0 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        649*/
     &                .and. nhe4.eq.0) ic = 126                                 !/*vl        650*/
        elseif (nn.eq.16) then                                                  !/*vl        651*/
!    This is (16n) channel:                                                     !/*vl        652*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        653*/
     &                .and. nhe4.eq.0) ic = 16                                  !/*vl        654*/
!    This is (16np) channel:                                                    !/*vl        655*/
          if (np.eq.1 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        656*/
     &                .and. nhe4.eq.0) ic = 41                                  !/*vl        657*/
!    This is (16n2p) channel:                                                   !/*vl        658*/
          if (np.eq.2 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        659*/
     &                .and. nhe4.eq.0) ic = 60                                  !/*vl        660*/
!    This is (16n3p) channel:                                                   !/*vl        661*/
          if (np.eq.3 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        662*/
     &                .and. nhe4.eq.0) ic = 78                                  !/*vl        663*/
!    This is (16n4p) channel:                                                   !/*vl        664*/
          if (np.eq.4 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        665*/
     &                .and. nhe4.eq.0) ic = 95                                  !/*vl        666*/
!    This is (16nd) channel:                                                    !/*vl        667*/
          if (np.eq.0 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        668*/
     &                .and. nhe4.eq.0) ic = 127                                 !/*vl        669*/
        elseif (nn.eq.17) then                                                  !/*vl        670*/
!    This is (17n) channel:                                                     !/*vl        671*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        672*/
     &                .and. nhe4.eq.0) ic = 17                                  !/*vl        673*/
!    This is (17np) channel:                                                    !/*vl        674*/
          if (np.eq.1 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        675*/
     &                .and. nhe4.eq.0) ic = 42                                  !/*vl        676*/
!    This is (17n2p) channel:                                                   !/*vl        677*/
          if (np.eq.2 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        678*/
     &                .and. nhe4.eq.0) ic = 61                                  !/*vl        679*/
!    This is (17n3p) channel:                                                   !/*vl        680*/
          if (np.eq.3 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        681*/
     &                .and. nhe4.eq.0) ic = 79                                  !/*vl        682*/
!    This is (17nd) channel:                                                    !/*vl        683*/
          if (np.eq.0 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        684*/
     &                .and. nhe4.eq.0) ic = 128                                 !/*vl        685*/
        elseif (nn.eq.18) then                                                  !/*vl        686*/
!    This is (18n) channel:                                                     !/*vl        687*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        688*/
     &                .and. nhe4.eq.0) ic = 18                                  !/*vl        689*/
!    This is (18np) channel:                                                    !/*vl        690*/
          if (np.eq.1 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        691*/
     &                .and. nhe4.eq.0) ic = 43                                  !/*vl        692*/
!    This is (18n2p) channel:                                                   !/*vl        693*/
          if (np.eq.2 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        694*/
     &                .and. nhe4.eq.0) ic = 62                                  !/*vl        695*/
!    This is (18nd) channel:                                                    !/*vl        696*/
          if (np.eq.0 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        697*/
     &                .and. nhe4.eq.0) ic = 129                                 !/*vl        698*/
        elseif (nn.eq.19) then                                                  !/*vl        699*/
!    This is (19n) channel:                                                     !/*vl        700*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        701*/
     &                .and. nhe4.eq.0) ic = 19                                  !/*vl        702*/
!    This is (19np) channel:                                                    !/*vl        703*/
          if (np.eq.1 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        704*/
     &                .and. nhe4.eq.0) ic = 44                                  !/*vl        705*/
!    This is (19nd) channel:                                                    !/*vl        706*/
          if (np.eq.0 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        707*/
     &                .and. nhe4.eq.0) ic = 130                                 !/*vl        708*/
        elseif (nn.eq.20) then                                                  !/*vl        709*/
!    This is (20n) channel:                                                     !/*vl        710*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        711*/
     &                .and. nhe4.eq.0) ic = 20                                  !/*vl        712*/
        elseif (nn.eq.0) then                                                   !/*vl        713*/
!    This is (1p) channel:                                                      !/*vl        714*/
          if (np.eq.1 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        715*/
     &                .and. nhe4.eq.0) ic = 21                                  !/*vl        716*/
!    This is (2p) channel:                                                      !/*vl        717*/
          if (np.eq.2 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        718*/
     &                .and. nhe4.eq.0) ic = 22                                  !/*vl        719*/
!    This is (3p) channel:                                                      !/*vl        720*/
          if (np.eq.3 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        721*/
     &                .and. nhe4.eq.0) ic = 23                                  !/*vl        722*/
!    This is (4p) channel:                                                      !/*vl        723*/
          if (np.eq.4 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        724*/
     &                .and. nhe4.eq.0) ic = 24                                  !/*vl        725*/
!    This is (5p) channel:                                                      !/*vl        726*/
          if (np.eq.5 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        727*/
     &                .and. nhe4.eq.0) ic = 25                                  !/*vl        728*/
!    This is (d) channel:                                                       !/*vl        729*/
          if (np.eq.0 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        730*/
     &                .and. nhe4.eq.0) ic = 111                                 !/*vl        731*/
!    This is (pd) channel:                                                      !/*vl        732*/
          if (np.eq.1 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        733*/
     &                .and. nhe4.eq.0) ic = 131                                 !/*vl        734*/
!    This is (2pd) channel:                                                     !/*vl        735*/
          if (np.eq.2 .and. nd.eq.1 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        736*/
     &                .and. nhe4.eq.0) ic = 146                                 !/*vl        737*/
!    This is (He4) channel:                                                     !/*vl        738*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        739*/
     &                .and. nhe4.eq.1) ic = 159                                 !/*vl        740*/
!    This is (2He4) channel:                                                    !/*vl        741*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        742*/
     &                .and. nhe4.eq.2) ic = 160                                 !/*vl        743*/
!    This is (3He4) channel:                                                    !/*vl        744*/
          if (np.eq.0 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        745*/
     &                .and. nhe4.eq.3) ic = 161                                 !/*vl        746*/
!    This is (pHe4) channel:                                                    !/*vl        747*/
          if (np.eq.1 .and. nd.eq.0 .and. nt.eq.0 .and. nhe3.eq.0               !/*vl        748*/
     &                .and. nhe4.eq.1) ic = 178                                 !/*vl        749*/
        endif                                                                   !/*vl        750*/
        icp = 0                                                                 !/*vl        751*/
        if (nt.eq.0 .and. nhe3.eq.0) then                                       !/*vl        752*/
          if (nd.eq.0 .and. nhe4.eq.0) then                                     !/*vl        753*/
            if (nn.eq.0) then                                                   !/*vl        754*/
!    This is (Xp) channel:                                                      !/*vl        755*/
              if (np.gt.0) icp = 194                                            !/*vl        756*/
            elseif (nn.gt.0) then                                               !/*vl        757*/
!    This is (Xn) channel:                                                      !/*vl        758*/
              if (np.eq.0) icp = 193                                            !/*vl        759*/
!    This is (Xn+Yp) channel:                                                   !/*vl        760*/
              if (np.gt.0) icp = 195                                            !/*vl        761*/
            endif                                                               !/*vl        762*/
          elseif (np.eq.0 .and. nn.gt.0) then                                   !/*vl        763*/
!    This is (Xn+d) channel:                                                    !/*vl        764*/
            if (nd.eq.1 .and. nhe4.eq.0) icp = 196                              !/*vl        765*/
!    This is (Xn+2d) channel:                                                   !/*vl        766*/
            if (nd.eq.2 .and. nhe4.eq.0) icp = 197                              !/*vl        767*/
!    This is (Xn+He4) channel:                                                  !/*vl        768*/
            if (nd.eq.0 .and. nhe4.eq.1) icp = 208                              !/*vl        769*/
!    This is (Xn+2He4) channel:                                                 !/*vl        770*/
            if (nd.eq.0 .and. nhe4.eq.2) icp = 209                              !/*vl        771*/
          elseif (nhe4.eq.0 .and. nn.gt.0) then                                 !/*vl        772*/
!    This is (Xn+p+d) channel:                                                  !/*vl        773*/
            if (nd.eq.1 .and. np.eq.1) icp = 198                                !/*vl        774*/
!    This is (Xn+2p+d) channel:                                                 !/*vl        775*/
            if (nd.eq.1 .and. np.eq.2) icp = 199                                !/*vl        776*/
!    This is (Xn+p+2d) channel:                                                 !/*vl        777*/
            if (nd.eq.2 .and. np.eq.1) icp = 200                                !/*vl        778*/
!    This is (Xn+2p+2d) channel:                                                !/*vl        779*/
            if (nd.eq.2 .and. np.eq.2) icp = 201                                !/*vl        780*/
          elseif (nd.eq.0 .and. nn.gt.0) then                                   !/*vl        781*/
!    This is (Xn+p+He4) channel:                                                !/*vl        782*/
            if (np.eq.1 .and. nhe4.eq.1) icp = 210                              !/*vl        783*/
!    This is (Xn+2p+He4) channel:                                               !/*vl        784*/
            if (np.eq.2 .and. nhe4.eq.1) icp = 211                              !/*vl        785*/
!    This is (Xn+3p+He4) channel:                                               !/*vl        786*/
            if (np.eq.3 .and. nhe4.eq.1) icp = 212                              !/*vl        787*/
          elseif (nd.eq.1 .and. nn.gt.0) then                                   !/*vl        788*/
!    This is (Xn+d+He4) channel:                                                !/*vl        789*/
            if (nhe4.eq.1 .and. np.eq.0) icp = 213                              !/*vl        790*/
          endif                                                                 !/*vl        791*/
        elseif (nt.gt.0) then                                                   !/*vl        792*/
!    This is (t + X) channel:                                                   !/*vl        793*/
!         icp = 214                                                             !/*vl        794*/
!    This is (Xn+t) channel:                                                    !/*vl        795*/
          if (np.eq.0 .and. nn.gt.0 .and. nd.eq.0 .and. nhe4.eq.0               !/*vl        796*/
     &    .and. nt.eq.1) icp = 202                                              !/*vl        797*/
!    This is (Xn+2t) channel:                                                   !/*vl        798*/
          if (np.eq.0 .and. nn.gt.0 .and. nd.eq.0 .and. nhe4.eq.0               !/*vl        799*/
     &    .and. nt.eq.2) icp = 203                                              !/*vl        800*/
!    This is (Xn+p+t) channel:                                                  !/*vl        801*/
          if (np.eq.1 .and. nn.gt.0 .and. nd.eq.0 .and. nhe4.eq.0               !/*vl        802*/
     &    .and. nt.eq.1) icp = 204                                              !/*vl        803*/
!    This is (Xn+2p+t) channel:                                                 !/*vl        804*/
          if (np.eq.2 .and. nn.gt.0 .and. nd.eq.0 .and. nhe4.eq.0               !/*vl        805*/
     &    .and. nt.eq.1) icp = 205                                              !/*vl        806*/
!    This is (Xn+d+t) channel:                                                  !/*vl        807*/
          if (np.eq.0 .and. nn.gt.0 .and. nd.eq.1 .and. nhe4.eq.0               !/*vl        808*/
     &    .and. nt.eq.1) icp = 206                                              !/*vl        809*/
!    This is (Xn+p+d+t) channel:                                                !/*vl        810*/
          if (np.eq.1 .and. nn.gt.0 .and. nd.eq.1 .and. nhe4.eq.0               !/*vl        811*/
     &    .and. nt.eq.1) icp = 207                                              !/*vl        812*/
        elseif (nhe3.gt.0) then                                                 !/*vl        813*/
!    This is (He3 + X) channel:                                                 !/*vl        814*/
          icp = 215                                                             !/*vl        815*/
        endif                                                                   !/*vl        816*/
        if (ic.ne.0) chan(ic) = chan(ic) + one                                  !/*vl        817*/
        if (icp.gt.0) chan(icp) = chan(icp) + one                               !/*vl        818*/
      endif                                                                     !/*vl        819*/
   70 continue                                                                  !/*vl        820*/
#endif                                                                          /*!vl        821*/
      return                                                                    !/*vl        822*/
      end                                                                       !/*vl        823*/
