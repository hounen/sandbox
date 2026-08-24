      subroutine ccasc(cm0d,t0mevd,anucld,znucld,frand,me0d,mb0d,ideld,         !/*cc          1*/
     &                 nel,exc0)                                                !/*cc          2*/
#ifdef CEM                                                                      /*!cc          3*/
! ----------------------------------------------------------------------        !/*cc          4*/
!                                                                               !/*cc          5*/
!     check what happens with af (set in help; reset to zro before calc)        !/*cc          6*/
!                                                                               !/*cc          7*/
! ----------------------------------------------------------------------        !/*cc          8*/
!                                                                               !/*cc          9*/
! ----------------------------------------------------------------------        !/*cc         10*/
!     called by: cascad                                                         !/*cc         11*/
!        call ccasc(pmass(ityp),ecl,a1,z1,rang(),int(charge(ityp)),             !/*cc         12*/
!                                            ibar(itypfl(ityp)),nofis)          !/*cc         13*/
!        data passed through:                                                   !/*cc         14*/
!                pmass(ityp),charge(ityp)  -> common/rucom/                     !/*cc         15*/
!                ecl,a1,z1                 -> subroutine cascad                 !/*cc         16*/
!                ibar(itypfl(ityp))        -> common/part/                      !/*cc         17*/
!                nofis                     -> common/ercom/                     !/*cc         18*/
!                                                                               !/*cc         19*/
!     calls: help, initial, ccascad, precof, derig, storep, vlobd               !/*cc         20*/
!                                                                               !/*cc         21*/
! ----------------------------------------------------------------------        !/*cc         22*/
!                                                                               !/*cc         23*/
! ======================================================================        !/*cc         24*/
!                                                                               !/*cc         25*/
!                    Program input variables:                                   !/*cc         26*/
!  cm0     = projectile mass in GeV (enter a negative number to end run)        !/*cc         27*/
!  t0mev   = initial projectile kinetic energy (in MeV)                         !/*cc         28*/
!  anucl   = target mass number.                                                !/*cc         29*/
!  znucl   = target proton number.                                              !/*cc         30*/
!  me0     = projectile charge (in units of proton charge)                      !/*cc         31*/
!  mb0     = projectile baryon number                                           !/*cc         32*/
!  nnnp    = number of initial cascade steps with detailed diagnostics.         !/*cc         33*/
!  iopt    = (1/2) switch for original (1995/1997) versions.                    !/*cc         34*/
!  wam     = ratio of saddle-point fission level density to compound            !/*cc         35*/
!            level density; af/ac.  (Usually called af/an).                     !/*cc         36*/
!  idel    = do evaporation calculations with or without fission.               !/*cc         37*/
!          = 0 do calculations without fission.                                 !/*cc         38*/
!          = 1 allow competition between fission and particle emission          !/*cc         39*/
!            at compound stage.                                                 !/*cc         40*/
!  fran    = starting random no. (double precision seed, from 0 and 1).         !/*cc         41*/
!                                                                               !/*cc         42*/
! ======================================================================        !/*cc         43*/
!                                                                               !/*cc         44*/
!   Written by F.X. Gallmeier, ORNL CPED  May, 1998.                            !/*cc         45*/
!                                                                               !/*cc         46*/
!   based on the main routine CEM97A,                                           !/*cc         47*/
!   edited by A. J. Sierk,  LANL  T-2  November-December, 1997.                 !/*cc         48*/
!                                                                               !/*cc         49*/
! ======================================================================        !/*cc         50*/
#include <cem/cdouble.h>                                                        /*!cc         51*/
      external bd1, bd2, bd3, camer70, cameron, mollnix                         !/*cc         52*/
      dimension a(10), pnucl(3), amnucl(3)                                      !/*cc         53*/
      dimension tmin(4), tmax(4), dt(4)                                         !/*cc         54*/
      common /adbf/    amf, ijsp, nhump, r0m                                    !/*cc         55*/
      common /begin/   cm0, t0gev, me0, mb0, ms0, l0                            !/*cc         56*/
      common /blok77/  spt(5,150)                                               !/*cc         57*/
      common /bl1003/  ut, at, zt                                               !/*cc         58*/
      common /constan/ thrd, twthrd, fvthrd                                     !/*cc         59*/
      common /dele/    sfu, wf, fusion, sigfw                                   !/*cc         60*/
      common /exiton/  nexip, nexin, nhol                                       !/*cc         61*/
      common /fiss/    fision, sigfis, nfis                                     !/*cc         62*/
      common /menu/    mspec, mpyld, mchy, misy, mdubl, mang                    !/*cc         63*/
      common /menu1/   ipar1, ipar2, ifam, ib, ish, idelta, ibe, idel           !/*cc         64*/
      common /menu2/   aam, isha, ipair                                         !/*cc         65*/
      common /mnuc/    emnucm, emnucg, emnucb, emneut, emprot                   !/*cc         66*/
      common /mpi/     empich, empi0                                            !/*cc         67*/
      common /o/       anucl, znucl, eps, vpi, rsm(10), rbig(10),               !/*cc         68*/
     &                 rhop(10), rhon(10), af(10), tfp(10), tfn(10), n          !/*cc         69*/
      common /outsid/  icount,ifil,nout(1000),izout(1000),inout(1000)           !/*cc         70*/
      common /pairc/   cevap, cfis                                              !/*cc         71*/
      common /pii/     pi                                                       !/*cc         72*/
      common /repcnt/  irpt, irep, irec, itk1, itk3                             !/*cc         73*/
      common /runopt/  iopt                                                     !/*cc         74*/
      common /tou/     fran, tmin, tmax, dt, am, rm, wam, dt0, t0max,           !/*cc         75*/
     &                 nnnp, lim, limc                                          !/*cc         76*/
      common /vul/     ncas, intel, flimcc, sigom, sigomm                       !/*cc         77*/
      common /zapp/    parz(6,150)                                              !/*cc         78*/
      common /cascnt/  ncaa                                                     !/*cc         79*/
!fcg                                                                            !/*cc         80*/
      common /casdat/ epcas(5), epcasq(5), epcmax(5), epcmin(5),                !/*cc         81*/
     &                atcas, atcasq, atcmax, atcmin,                            !/*cc         82*/
     &                ztcas, ztcasq, ztcmax, ztcmin, nccas(5)                   !/*cc         83*/
!fcg                                                                            !/*cc         84*/
      data zro, one, two, thr, fiv /0.d0, 1.d0, 2.d0, 3.d0, 5.d0/               !/*cc         85*/
      data a /0.95d0, 0.8d0, 0.5d0, 0.2d0, 0.1d0, 0.05d0, 0.01d0,               !/*cc         86*/
     &        3*0.0d0/                                                          !/*cc         87*/
! ======================================================================        !/*cc         88*/
!                                                                               !/*cc         89*/
      lim=0                                                                     !/*cc         90*/
      limc=0                                                                    !/*cc         91*/
      intel = 0                                                                 !/*cc         92*/
      ncas = 0                                                                  !/*cc         93*/
!                                                                               !/*cc         94*/
!  decouple variables                                                           !/*cc         95*/
      cm0   = cm0d/dp1000                                                       !/*cc         96*/
      t0mev = t0mevd                                                            !/*cc         97*/
      anucl = anucld                                                            !/*cc         98*/
      znucl = znucld                                                            !/*cc         99*/
      fran  = frand                                                             !/*cc        100*/
      me0   = me0d                                                              !/*cc        101*/
      mb0   = mb0d                                                              !/*cc        102*/
      idel  = ideld                                                             !/*cc        103*/
!                                                                               !/*cc        104*/
!  CEM program options  1/2 CEM95/CEM97                                         !/*cc        105*/
      if(znucl.lt.dp8999m2) then                                                !/*cc        106*/
         iopt = 2                                                               !/*cc        107*/
      else                                                                      !/*cc        108*/
         iopt = 1                                                               !/*cc        109*/
      endif                                                                     !/*cc        110*/
!                                                                               !/*cc        111*/
!  choose parametrized wam -> wam <0                                            !/*cc        112*/
!  (if wam positive, wam is fixed)                                              !/*cc        113*/
      wam  = -dp11m1                                                            !/*cc        114*/
!                                                                               !/*cc        115*/
!  initialization                                                               !/*cc        116*/
      idelta = 0                                                                !/*cc        117*/
      ibe = 0                                                                   !/*cc        118*/
      nhump = 1                                                                 !/*cc        119*/
      ijsp = 1                                                                  !/*cc        120*/
      am = 0.133d0                                                              !/*cc        121*/
      cevap = dp12                                                              !/*cc        122*/
      cfis = 14.0d0                                                             !/*cc        123*/
      rm = dp15m1                                                               !/*cc        124*/
      r0m = dp12m1                                                              !/*cc        125*/
      if (iopt.eq.1) then                                                       !/*cc        126*/
         if (cm0.lt.dp9m1) cm0 = 0.14d0                                         !/*cc        127*/
         if (cm0.gt.dp9m1) cm0 = 0.94d0                                         !/*cc        128*/
         ifam = 9                                                               !/*cc        129*/
         ib = 6                                                                 !/*cc        130*/
         ish = 2                                                                !/*cc        131*/
         isha = 2                                                               !/*cc        132*/
         ipair = 0                                                              !/*cc        133*/
      elseif (iopt.eq.2) then                                                   !/*cc        134*/
         ifam = 12                                                              !/*cc        135*/
         ib = 7                                                                 !/*cc        136*/
         ish = 4                                                                !/*cc        137*/
         isha = 4                                                               !/*cc        138*/
         ipair = 1                                                              !/*cc        139*/
      else                                                                      !/*cc        140*/
         call expirx(1,'CCASC','illegal value of iopt')                         !/*cc        141*/
      endif                                                                     !/*cc        142*/
      thrd = one/thr                                                            !/*cc        143*/
      twthrd = two/thr                                                          !/*cc        144*/
      fvthrd = fiv/thr                                                          !/*cc        145*/
      n = 7                                                                     !/*cc        146*/
      aam = am                                                                  !/*cc        147*/
      bn = 0.545d0                                                              !/*cc        148*/
      r0n = 1.07d0                                                              !/*cc        149*/
      rmax = 10.0d0                                                             !/*cc        150*/
!                                                                               !/*cc        151*/
!  Set up neutron and proton densities and Coulomb potential                    !/*cc        152*/
!  in the n zones of the nucleus.                                               !/*cc        153*/
      call help (a, r0n, rmax, bn)                                              !/*cc        154*/
      obr = af(n)/two                                                           !/*cc        155*/
!                                                                               !/*cc        156*/
!  Geometrical cross section using radius at which density is                   !/*cc        157*/
!  0.01 x central density.                                                      !/*cc        158*/
      sigom = two*fiv*pi*rsm(n)**2                                              !/*cc        159*/
!                                                                               !/*cc        160*/
!  Initialize various constants and arrays:                                     !/*cc        161*/
      call initial                                                              !/*cc        162*/
      if (iopt.eq.1) then                                                       !/*cc        163*/
         emneut = 0.94d0                                                        !/*cc        164*/
         emprot = 0.94d0                                                        !/*cc        165*/
         empich = 0.14d0                                                        !/*cc        166*/
         empi0 = 0.14d0                                                         !/*cc        167*/
         emnucm = 940.d0                                                        !/*cc        168*/
         emnucg = 0.94d0                                                        !/*cc        169*/
         emnucb = 0.9315014d0                                                   !/*cc        170*/
      endif                                                                     !/*cc        171*/
      t0gev = t0mev/dp1000                                                      !/*cc        172*/
!                                                                               !/*cc        173*/
!     Next do-loop: ???                                                         !/*cc        174*/
      do 45 i=1,10                                                              !/*cc        175*/
         af(i) = zro                                                            !/*cc        176*/
 45   continue                                                                  !/*cc        177*/
!                                                                               !/*cc        178*/
 70   do  90  m1 = 1,150                                                        !/*cc        179*/
         do  80  m = 1,5                                                        !/*cc        180*/
            parz(m,m1) = zro                                                    !/*cc        181*/
            spt(m,m1) = zro                                                     !/*cc        182*/
 80      continue                                                               !/*cc        183*/
         parz(6,m1) = zro                                                       !/*cc        184*/
 90   continue                                                                  !/*cc        185*/
      call ccascad (up, ap, zp, pnucl, amnucl, kstart, obr, nel)                !/*cc        186*/
      exc0=dp0                                                                  !/*cc        187*/
      if (up.lt.zro) then                                                       !/*cc        188*/
         intel = intel + 1                                                      !/*cc        189*/
      else                                                                      !/*cc        190*/
         if (nel.gt.0) then                                                     !/*cc        191*/
            intel = intel + 1                                                   !/*cc        192*/
         else                                                                   !/*cc        193*/
!   Maximum momentum possible in the system (approximate):                      !/*cc        194*/
            pmax = ap*sqrt((t0gev + 2.d0*cm0)*t0gev)/(anucl + cm0)              !/*cc        195*/
            pmax = pmax + dph                                                   !/*cc        196*/
            prec = sqrt(pnucl(1)**2 + pnucl(2)**2 + pnucl(3)**2)                !/*cc        197*/
            if (prec.gt.pmax) then                                              !/*cc        198*/
!   If recoil kinetic energy is too large, rerun cascade:                       !/*cc        199*/
               irec = irec + 1                                                  !/*cc        200*/
!              (CEM statistics is wrong in this case)                           !/*cc        201*/
               go to 70                                                         !/*cc        202*/
            endif                                                               !/*cc        203*/
            up = up*dp1000                                                      !/*cc        204*/
            exc0=up                                                             !/*cc        205*/
            pnx = pnucl(1)                                                      !/*cc        206*/
            pny = pnucl(2)                                                      !/*cc        207*/
            pnz = pnucl(3)                                                      !/*cc        208*/
            elx = amnucl(1)                                                     !/*cc        209*/
            ely = amnucl(2)                                                     !/*cc        210*/
            elz = amnucl(3)                                                     !/*cc        211*/
            n0 = nexip + nexin + nhol                                           !/*cc        212*/
            nh0 = nhol                                                          !/*cc        213*/
            npz0 = nexip                                                        !/*cc        214*/
            np0 = nexip + nexin                                                 !/*cc        215*/
            if (ap.ge.4 .and. zp.ge.1 .and. ap.ge.zp) then                      !/*cc        216*/
!                                                                               !/*cc        217*/
!  Accumulate statistics on projectile energy, target mass and charge           !/*cc        218*/
!                                                                               !/*cc        219*/
!     find out projectile type                                                  !/*cc        220*/
               if (mb0.gt.0) then                                               !/*cc        221*/
                  if (me0.eq.0) then                                            !/*cc        222*/
!     neutron                                                                   !/*cc        223*/
                     ityp=1                                                     !/*cc        224*/
                  elseif (me0.gt.0) then                                        !/*cc        225*/
!     proton                                                                    !/*cc        226*/
                     ityp=2                                                     !/*cc        227*/
                  else                                                          !/*cc        228*/
                     call expirx(1,'CCASC','particle type not allowed')         !/*cc        229*/
                  endif                                                         !/*cc        230*/
               else                                                             !/*cc        231*/
                  if (me0.lt.0) then                                            !/*cc        232*/
!     pi-                                                                       !/*cc        233*/
                     ityp=3                                                     !/*cc        234*/
                  elseif (me0.eq.0) then                                        !/*cc        235*/
!     pi0                                                                       !/*cc        236*/
                     ityp=4                                                     !/*cc        237*/
                  else                                                          !/*cc        238*/
!     pi+                                                                       !/*cc        239*/
                     ityp=5                                                     !/*cc        240*/
                  endif                                                         !/*cc        241*/
               endif                                                            !/*cc        242*/
               nccas(ityp)  = nccas(ityp) + 1                                   !/*cc        243*/
               epcas(ityp)  = epcas(ityp) + t0mev                               !/*cc        244*/
               epcasq(ityp) = epcasq(ityp) + t0mev*t0mev                        !/*cc        245*/
               epcmax(ityp) = max(epcmax(ityp), t0mev)                          !/*cc        246*/
               epcmin(ityp) = min(epcmin(ityp), t0mev)                          !/*cc        247*/
               atcas  = atcas + anucl                                           !/*cc        248*/
               atcasq = atcasq + anucl*anucl                                    !/*cc        249*/
               atcmax = max(atcmax, anucl)                                      !/*cc        250*/
               atcmin = min(atcmin, anucl)                                      !/*cc        251*/
               ztcas  = ztcas + znucl                                           !/*cc        252*/
               ztcasq = ztcasq +znucl*znucl                                     !/*cc        253*/
               ztcmax = max(ztcmax, znucl)                                      !/*cc        254*/
               ztcmin = min(ztcmin, znucl)                                      !/*cc        255*/
               sigomm = sigomm + sigom                                          !/*cc        256*/
!                                                                               !/*cc        257*/
               ncas = ncas + 1                                                  !/*cc        258*/
               ncaa = ncaa + 1                                                  !/*cc        259*/
               call precof (up, ap, zp, pnx, pny, pnz, elx, ely,                !/*cc        260*/
     &              elz, rm, kstart, n0, np0, nh0, npz0, wam)                   !/*cc        261*/
            else                                                                !/*cc        262*/
               irep = irep + 1                                                  !/*cc        263*/
!              (CEM statistics is wrong in this case)                           !/*cc        264*/
               go to 70                                                         !/*cc        265*/
            endif                                                               !/*cc        266*/
            up = up/dp1000                                                      !/*cc        267*/
! ...       if (ncaa.le.nnnp) call derig (ncas, intel)                          !/*cc        268*/
            if (fusion.gt.dp0 .and. idel.gt.0) nfis = nfis + 1                  !/*cc        269*/
            sfu = sfu + wf                                                      !/*cc        270*/
            call storep(parz, spt, anucl, znucl, at, zt, me0, mb0)              !/*cc        271*/
            call vlobd                                                          !/*cc        272*/
         endif                                                                  !/*cc        273*/
      endif                                                                     !/*cc        274*/
!                                                                               !/*cc        275*/
!   return as pseudocollision instead of forcing a collison                     !/*cc        276*/
!      if(ncas.le.0) goto 70                                                    !/*cc        277*/
      if (intel.gt.0) nel=1                                                     !/*cc        278*/
      flimcc = flimcc + ncas + intel                                            !/*cc        279*/
! ======================================================================        !/*cc        280*/
#endif                                                                          /*!cc        281*/
      return                                                                    !/*cc        282*/
      end                                                                       !/*cc        283*/
