      subroutine initial                                                        !/*in          1*/
#ifdef CEM                                                                      /*!in          2*/
! ======================================================================        !/*in          3*/
!                                                                               !/*in          4*/
!   CEM95 written by S. G. Mashnik                                              !/*in          5*/
!                                                                               !/*in          6*/
!   Initial contains the initialization of various constants and                !/*in          7*/
!   variables originally set in the main program cem95.  This is in             !/*in          8*/
!   order to shorten the program, and to remove functionality to a              !/*in          9*/
!   single-purpose routine.                                                     !/*in         10*/
!                                                                               !/*in         11*/
! ======================================================================        !/*in         12*/
!                                                                               !/*in         13*/
!   Written by A. J. Sierk,  LANL  T-2  May, 1996.                              !/*in         14*/
!   Edited by fcg, ORNL  CPED  March 1998                                       !/*in         15*/
!                                                                               !/*in         16*/
! ======================================================================        !/*in         17*/
#include <cem/cdouble.h>                                                        /*!in         18*/
      common /menu/    mspec, mpyld, mchy, misy, mdubl, mang                    !/*in         19*/
      common /menu1/   ipar1, ipar2, ifam, ib, ish, idelta, ibe, idel           !/*in         20*/
      common /begin/   cm0, t0gev, me0, mb0, ms0, l0                            !/*in         21*/
      common /d2sdto/  tet1(10), tet2(10), te1(140), te2(140), dtt(140),        !/*in         22*/
     &                 se(140), dtdo(140,10), d2spec(9,10,140),                 !/*in         23*/
     &                 d2spe(14,10,140), ntt, ntet, nt2, nt3                    !/*in         24*/
      common /degrad/  degrad                                                   !/*in         25*/
      common /dele/    sfu, wf, fusion, sigfw                                   !/*in         26*/
      common /eqidat/  eestt, eestsq, aeqtot, aeqsq, zeqtot, zeqsq,             !/*in         27*/
     &                 aemin, aemax, eletot, elesq, elemin, elemax,             !/*in         28*/
     &                 eestrmn, eestrmx, zemin, zemax, neq                      !/*in         29*/
      common /fisdat/  estart, estarsq, estrmn, estrmx,                         !/*in         30*/
     &                 elstart, elstarsq, elmmin, elmmax,                       !/*in         31*/
     &                 astart, astarsq, ammin, ammax,                           !/*in         32*/
     &                 zstart, zstarsq, zmmin, zmmax,                           !/*in         33*/
     &                 bftot, bfsq, bfmin, bfmax                                !/*in         34*/
      common /fiss/    fision, sigfis, nfis                                     !/*in         35*/
      common /mnuc/    emnucm, emnucg, emnucb, emneut, emprot                   !/*in         36*/
      common /mpi/     empich, empi0                                            !/*in         37*/
      common /o/       anucl, znucl, eps, vpi, rsm(10), rbig(10),               !/*in         38*/
     &                 rhop(10), rhon(10), af(10), tfp(10), tfn(10), n          !/*in         39*/
      common /pii/     pi                                                       !/*in         40*/
      common /predat/  epstt, epstsq, apqtot, apqsq, zpqtot, zpqsq,             !/*in         41*/
     &                 apmin, apmax, elptot, elpsq, elpmin, elpmax,             !/*in         42*/
     &                 epstrmn, epstrmx, zpmin, zpmax, npreq                    !/*in         43*/
      common /repcnt/  irpt, irep, irec, itk1, itk3                             !/*in         44*/
      common /resdat/  erkt, erksq, artot, arsq, zrtot, zrsq,                   !/*in         45*/
     &                 armin, armax, elrtot, elrsq, elrmin, elrmax,             !/*in         46*/
     &                 erkmn, erkmx, zrmin, zrmax, nres                         !/*in         47*/
      common /resmac/  spe(14,140), an(14,20)                                   !/*in         48*/
      common /result/  spec(9,140), ang(9,20), chan(218), dadz(50,18)           !/*in         49*/
      common /retdat/  arttot, artsq, artmin, artmax, zrttot, zrtsq,            !/*in         50*/
     &                 zrtmin, zrtmax, elrttot, elrtsq, elrtmin,                !/*in         51*/
     &                 elrtmax, nret                                            !/*in         52*/
      common /stopr/   istp                                                     !/*in         53*/
      common /vul/     ncas, intel, flimcc, sigom, sigomm                       !/*in         54*/
      common /phstat/ ephts, ephtsq, ephmax, ephmin,                            !/*in         55*/
     &                aphts, aphtsq, aphmax, aphmin,                            !/*in         56*/
     &                zphts, zphtsq, zphmax, zphmin, nphts                      !/*in         57*/
      common /casdat/ epcas(5), epcasq(5), epcmax(5), epcmin(5),                !/*in         58*/
     &                atcas, atcasq, atcmax, atcmin,                            !/*in         59*/
     &                ztcas, ztcasq, ztcmax, ztcmin, nccas(5)                   !/*in         60*/
      common /initiz/ inifst                                                    !/*in         61*/
      dimension afrag(200), zfrag(100)                                          !/*in         62*/
      common /fragdt/ afrag, zfrag, afrtt, afrsq, afrmin, afrmax,               !/*in         63*/
     &                zfrtt, zfrsq, zfrmin, zfrmax, ekfrtt, ekfrsq,             !/*in         64*/
     &                ekfrmn, ekfrmx, eufrtt, eufrsq, eufrmn, eufrmx,           !/*in         65*/
     &                nfrag                                                     !/*in         66*/
      data zro /dp0/                                                            !/*in         67*/
! ======================================================================        !/*in         68*/
      ncas = 0                                                                  !/*in         69*/
      intel = 0                                                                 !/*in         70*/
      infis = 0                                                                 !/*in         71*/
      irec = 0                                                                  !/*in         72*/
      irep = 0                                                                  !/*in         73*/
      irpt = 0                                                                  !/*in         74*/
      istp = 0                                                                  !/*in         75*/
      itk1 = 0                                                                  !/*in         76*/
      itk3 = 0                                                                  !/*in         77*/
      iz0 = 0                                                                   !/*in         78*/
!                                                                               !/*in         79*/
      degrad = dp180/pi                                                         !/*in         80*/
      ms0 = 0                                                                   !/*in         81*/
      l0 = 0                                                                    !/*in         82*/
!  eps is distance from Fermi surface to 0 energy.                              !/*in         83*/
      eps = dp7m3                                                               !/*in         84*/
!  vpi is pion potential strength (binding energy).                             !/*in         85*/
      vpi = dp25m3                                                              !/*in         86*/
      if (inifst.le.0) then                                                     !/*in         87*/
         inifst = 1                                                             !/*in         88*/
         flimcc = zro                                                           !/*in         89*/
!                                                                               !/*in         90*/
!  CEM input parameters                                                         !/*in         91*/
         mpyld = 1                                                              !/*in         92*/
         mchy  = 1                                                              !/*in         93*/
         misy  =-1                                                              !/*in         94*/
         mdubl =-1                                                              !/*in         95*/
         mang  =-1                                                              !/*in         96*/
         mspec =-1                                                              !/*in         97*/
         ipar1 = 1                                                              !/*in         98*/
         ipar2 = 9                                                              !/*in         99*/
         ntt   = 1                                                              !/*in        100*/
         nt2   = 2                                                              !/*in        101*/
         nt3   = 3                                                              !/*in        102*/
         do 30 k = 1,140                                                        !/*in        103*/
            do 10 j = 1,9                                                       !/*in        104*/
               spec(j,k) = zro                                                  !/*in        105*/
               do 10 i = 1,10                                                   !/*in        106*/
                  d2spec(j,i,k) = zro                                           !/*in        107*/
   10       continue                                                            !/*in        108*/
            do 20 j = 1,14                                                      !/*in        109*/
               spe(j,k) = zro                                                   !/*in        110*/
               do 20 i = 1,10                                                   !/*in        111*/
                  d2spe(j,i,k) = zro                                            !/*in        112*/
   20       continue                                                            !/*in        113*/
   30   continue                                                                !/*in        114*/
        do 60 k = 1,20                                                          !/*in        115*/
           do 40 j = 1,9                                                        !/*in        116*/
              ang(j,k) = zro                                                    !/*in        117*/
   40      continue                                                             !/*in        118*/
           do 50 j = 1,14                                                       !/*in        119*/
              an(j,k) = zro                                                     !/*in        120*/
   50      continue                                                             !/*in        121*/
   60   continue                                                                !/*in        122*/
        do 70 k = 1,218                                                         !/*in        123*/
           chan(k) = zro                                                        !/*in        124*/
   70   continue                                                                !/*in        125*/
         nfis = 0                                                               !/*in        126*/
         neq = 0                                                                !/*in        127*/
         npreq = 0                                                              !/*in        128*/
         nres = 0                                                               !/*in        129*/
         nret = 0                                                               !/*in        130*/
         fision = zro                                                           !/*in        131*/
         sigfis = zro                                                           !/*in        132*/
         sigomm = zro                                                           !/*in        133*/
         sfu = zro                                                              !/*in        134*/
         wf = zro                                                               !/*in        135*/
         sigfw = zro                                                            !/*in        136*/
         fusion = -dp1                                                          !/*in        137*/
         astart = zro                                                           !/*in        138*/
         astarsq = zro                                                          !/*in        139*/
         bftot = zro                                                            !/*in        140*/
         bfsq = zro                                                             !/*in        141*/
         bfmin = dp100                                                          !/*in        142*/
         bfmax = zro                                                            !/*in        143*/
         estart = zro                                                           !/*in        144*/
         estarsq = zro                                                          !/*in        145*/
         elstart = zro                                                          !/*in        146*/
         elstarsq = zro                                                         !/*in        147*/
         estrmn = dp1000                                                        !/*in        148*/
         estrmx = zro                                                           !/*in        149*/
         eestt = zro                                                            !/*in        150*/
         eestsq = zro                                                           !/*in        151*/
         eestrmn = dp1000                                                       !/*in        152*/
         eestrmx = zro                                                          !/*in        153*/
         epstt = zro                                                            !/*in        154*/
         epstsq = zro                                                           !/*in        155*/
         epstrmn = dp1000                                                       !/*in        156*/
         epstrmx = zro                                                          !/*in        157*/
         erkt = zro                                                             !/*in        158*/
         erksq = zro                                                            !/*in        159*/
         erkmn = dp1000                                                         !/*in        160*/
         erkmx = zro                                                            !/*in        161*/
         ammax = zro                                                            !/*in        162*/
         ammin = dp400                                                          !/*in        163*/
         aemin = dp400                                                          !/*in        164*/
         aemax = zro                                                            !/*in        165*/
         aeqtot = zro                                                           !/*in        166*/
         aeqsq = zro                                                            !/*in        167*/
         apmin = dp400                                                          !/*in        168*/
         apmax = zro                                                            !/*in        169*/
         apqtot = zro                                                           !/*in        170*/
         apqsq = zro                                                            !/*in        171*/
         armin = dp400                                                          !/*in        172*/
         armax = zro                                                            !/*in        173*/
         artot = zro                                                            !/*in        174*/
         arsq = zro                                                             !/*in        175*/
         artmin = dp400                                                         !/*in        176*/
         artmax = zro                                                           !/*in        177*/
         arttot = zro                                                           !/*in        178*/
         artsq = zro                                                            !/*in        179*/
         elmmax = zro                                                           !/*in        180*/
         elmmin = dp100                                                         !/*in        181*/
         eletot = zro                                                           !/*in        182*/
         elesq = zro                                                            !/*in        183*/
         elemin = dp100                                                         !/*in        184*/
         elemax = zro                                                           !/*in        185*/
         elptot = zro                                                           !/*in        186*/
         elpsq = zro                                                            !/*in        187*/
         elpmin = dp100                                                         !/*in        188*/
         elpmax = zro                                                           !/*in        189*/
         elrtot = zro                                                           !/*in        190*/
         elrsq = zro                                                            !/*in        191*/
         elrmin = dp100                                                         !/*in        192*/
         elrmax = zro                                                           !/*in        193*/
         elrttot = zro                                                          !/*in        194*/
         elrtsq = zro                                                           !/*in        195*/
         elrtmin = dp100                                                        !/*in        196*/
         elrtmax = zro                                                          !/*in        197*/
         zstart = zro                                                           !/*in        198*/
         zstarsq = zro                                                          !/*in        199*/
         zmmax = zro                                                            !/*in        200*/
         zmmin = dp100                                                          !/*in        201*/
         zeqtot = zro                                                           !/*in        202*/
         zeqsq = zro                                                            !/*in        203*/
         zemin = dp100                                                          !/*in        204*/
         zemax = zro                                                            !/*in        205*/
         zpqtot = zro                                                           !/*in        206*/
         zpqsq = zro                                                            !/*in        207*/
         zpmin = dp100                                                          !/*in        208*/
         zpmax = zro                                                            !/*in        209*/
         zrtot = zro                                                            !/*in        210*/
         zrsq = zro                                                             !/*in        211*/
         zrmin = dp100                                                          !/*in        212*/
         zrmax = zro                                                            !/*in        213*/
         zrttot = zro                                                           !/*in        214*/
         zrtsq = zro                                                            !/*in        215*/
         zrtmin = dp100                                                         !/*in        216*/
         zrtmax = zro                                                           !/*in        217*/
         nphts = 0                                                              !/*in        218*/
         ephts = zro                                                            !/*in        219*/
         ephtsq = zro                                                           !/*in        220*/
         ephmax = zro                                                           !/*in        221*/
         ephmin = 10.e6                                                         !/*in        222*/
         aphts = zro                                                            !/*in        223*/
         aphtsq = zro                                                           !/*in        224*/
         aphmax = zro                                                           !/*in        225*/
         aphmin = dp400                                                         !/*in        226*/
         zphts = zro                                                            !/*in        227*/
         zphtsq = zro                                                           !/*in        228*/
         zphmax = zro                                                           !/*in        229*/
         zphmin  = dp200                                                        !/*in        230*/
         do 80 i=1,5                                                            !/*in        231*/
            nccas(i) =0                                                         !/*in        232*/
            epcas(i) = zro                                                      !/*in        233*/
            epcasq(i) = zro                                                     !/*in        234*/
            epcmax(i) = zro                                                     !/*in        235*/
            epcmin(i) = dp1p7                                                   !/*in        236*/
   80    continue                                                               !/*in        237*/
         atcas = zro                                                            !/*in        238*/
         atcasq = zro                                                           !/*in        239*/
         atcmax = zro                                                           !/*in        240*/
         atcmin = dp400                                                         !/*in        241*/
         ztcas = zro                                                            !/*in        242*/
         ztcasq = zro                                                           !/*in        243*/
         ztcmax = zro                                                           !/*in        244*/
         ztcmin = dp200                                                         !/*in        245*/
         do 90 i=1,200                                                          !/*in        246*/
            afrag(i) = zro                                                      !/*in        247*/
   90    continue                                                               !/*in        248*/
         do 100 i=1,100                                                         !/*in        249*/
            zfrag(i) = zro                                                      !/*in        250*/
  100    continue                                                               !/*in        251*/
         afrtt = zro                                                            !/*in        252*/
         afrsq = zro                                                            !/*in        253*/
         afrmin = dp400                                                         !/*in        254*/
         afrmax = zro                                                           !/*in        255*/
         zfrtt = zro                                                            !/*in        256*/
         zfrsq = zro                                                            !/*in        257*/
         zfrmin = dp400                                                         !/*in        258*/
         zfrmax = zro                                                           !/*in        259*/
         ekfrtt = zro                                                           !/*in        260*/
         ekfrsq = zro                                                           !/*in        261*/
         ekfrmn = dp400                                                         !/*in        262*/
         ekfrmx = zro                                                           !/*in        263*/
         eufrtt = zro                                                           !/*in        264*/
         eufrsq = zro                                                           !/*in        265*/
         eufrmn = dp400                                                         !/*in        266*/
         eufrmx = zro                                                           !/*in        267*/
         nfrag  = 0                                                             !/*in        268*/
      endif                                                                     !/*in        269*/
#endif                                                                          /*!in        270*/
      return                                                                    !/*in        271*/
      end                                                                       !/*in        272*/
