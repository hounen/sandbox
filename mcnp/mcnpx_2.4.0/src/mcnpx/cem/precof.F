      subroutine precof (enext, atwght, charg, pnx, pny, pnz, elx, ely,         !/*pr          1*/
     &                   elz, radncl, kstart, n0, np0, nh0, npz0, wam)          !/*pr          2*/
#ifdef CEM                                                                      /*!pr          3*/
! ======================================================================        !/*pr          4*/
!                                                                               !/*pr          5*/
!   Calculates pre-equlibrium and equlibrium particle emission.                 !/*pr          6*/
!                                                                               !/*pr          7*/
!   Called by: CEM97a                                                           !/*pr          8*/
!                                                                               !/*pr          9*/
!   Calls: ARFAF BF BSFIT COLOMB DELTAM FAM GAMAGU ISANGL KINEMA                !/*pr         10*/
!          MOLNIX ROTOR SHELL TKIN TKINM1 CTRANS VHELP                          !/*pr         11*/
!                                                                               !/*pr         12*/
!   CEM95 written by S. G. Mashnik                                              !/*pr         13*/
!                                                                               !/*pr         14*/
!   Edited by A. J. Sierk,  LANL  T-2  February, 1996.                          !/*pr         15*/
!   Edited by A. J. Sierk,  LANL  T-2  November, 1997.                          !/*pr         16*/
!   Edited by F. Gallmeier,  ORNL CPED,  June 1998 (fcg)                        !/*pr         17*/
!   Edited by F. Gallmeier,  ORNL CPED,  Dec 1998 (fcg)                         !/*pr         18*/
!                                                                               !/*pr         19*/
! ======================================================================        !/*pr         20*/
#include <cem/cdouble.h>                                                        /*!pr         21*/
#include <lcs/inout.h>                                                          /*!pr         22*/
      dimension ami(6), gj(7), pevapj(6), pl(3), ps(3), uglmom(3), v(3)         !/*pr         23*/
      dimension p12(3)                                                          !/*pr         24*/
      common /adbf/    amf, ijsp, nhump, r0m                                    !/*pr         25*/
      common /ajsbar/  ainit, zinit, eb(20,50,90), egs(20,50,90)                !/*pr         26*/
      common /bl1003/  u, a, z                                                  !/*pr         27*/
      common /bl1005/  aj(6)                                                    !/*pr         28*/
      common /bl1006/  zj(6)                                                    !/*pr         29*/
      common /bl1008/  dlmo(6), dlmn(6)                                         !/*pr         30*/
      common /bl1009/  afj(7)                                                   !/*pr         31*/
      common /bl1010/  zfj(6)                                                   !/*pr         32*/
      common /bl1011/  vj(6)                                                    !/*pr         33*/
      common /bl1015/  rj(7)                                                    !/*pr         34*/
      common /bl1016/  cc(6)                                                    !/*pr         35*/
      common /blac/    ac                                                       !/*pr         36*/
      common /blalj/   alj(6)                                                   !/*pr         37*/
      common /blangl/  angl(4)                                                  !/*pr         38*/
      common /blbj/    bj(7)                                                    !/*pr         39*/
      common /blexn/   exn                                                      !/*pr         40*/
      common /blok77/  spt(5,150)                                               !/*pr         41*/
      common /blr0/    r0                                                       !/*pr         42*/
      common /br0j/    r0j(6), bn                                               !/*pr         43*/
      common /constan/ thrd, twthrd, fvthrd                                     !/*pr         44*/
      common /dele/    sfu, wf, fusion, sigfw                                   !/*pr         45*/
      common /eqidat/  eestt, eestsq, aeqtot, aeqsq, zeqtot, zeqsq,             !/*pr         46*/
     &                 aemin, aemax, eletot, elesq, elemin, elemax,             !/*pr         47*/
     &                 eestrmn, eestrmx, zemin, zemax, neq                      !/*pr         48*/
      common /fisdat/  estart, estarsq, estrmn, estrmx,                         !/*pr         49*/
     &                 elstart, elstarsq, elmmin, elmmax,                       !/*pr         50*/
     &                 astart, astarsq, ammin, ammax,                           !/*pr         51*/
     &                 zstart, zstarsq, zmmin, zmmax,                           !/*pr         52*/
     &                 bftot, bfsq, bfmin, bfmax                                !/*pr         53*/
      common /gambet/  gb(6)                                                    !/*pr         54*/
      common /jsp/     ws(51)                                                   !/*pr         55*/
      common /kktot/   ktot                                                     !/*pr         56*/
      common /lsierk/  egs0, ln                                                 !/*pr         57*/
      common /menu1/   ipar1, ipar2, ifam, ib, ish, idelta, ibe, idel           !/*pr         58*/
      common /menu2/   aam, isha, ipair                                         !/*pr         59*/
      common /mnuc/    emnucm, emnucg, emnucb, emneut, emprot                   !/*pr         60*/
      common /nix/     yy2(51), xx1(51), xx2(51)                                !/*pr         61*/
      common /pairc/   cevap, cfis                                              !/*pr         62*/
      common /pii/     pi                                                       !/*pr         63*/
      common /predat/  epstt, epstsq, apqtot, apqsq, zpqtot, zpqsq,             !/*pr         64*/
     &                 apmin, apmax, elptot, elpsq, elpmin, elpmax,             !/*pr         65*/
     &                 epstrmn, epstrmx, zpmin, zpmax, npreq                    !/*pr         66*/
      common /resdat/  erkt, erksq, artot, arsq, zrtot, zrsq,                   !/*pr         67*/
     &                 armin, armax, elrtot, elrsq, elrmin, elrmax,             !/*pr         68*/
     &                 erkmn, erkmx, zrmin, zrmax, nres                         !/*pr         69*/
      common /retdat/  arttot, artsq, artmin, artmax, zrttot, zrtsq,            !/*pr         70*/
     &                 zrtmin, zrtmax, elrttot, elrtsq, elrtmin,                !/*pr         71*/
     &                 elrtmax, nret                                            !/*pr         72*/
!fcg                                                                            !/*pr         73*/
      common /phstat/  ephts, ephtsq, ephmax, ephmin,                           !/*pr         74*/
     &                 aphts, aphtsq, aphmax, aphmin,                           !/*pr         75*/
     &                 zphts, zphtsq, zphmax, zphmin, nphts                     !/*pr         76*/
!fcg                                                                            !/*pr         77*/
      common /stopr/   istp                                                     !/*pr         78*/
      common /zapp/    parz(6,150)                                              !/*pr         79*/
      data icnt, icnt2 /0, 0/                                                   !/*pr         80*/
      data amm /0.704d0/                                                        !/*pr         81*/
      data zro, one, two, thr, for /0.d0, 1.d0, 2.d0, 3.d0, 4.d0/               !/*pr         82*/
      logical fising, fision                                                    !/*pr         83*/
! ======================================================================        !/*pr         84*/
      twpi = two*pi                                                             !/*pr         85*/
      r0 = radncl                                                               !/*pr         86*/
      wt = one                                                                  !/*pr         87*/
      fusion = -one                                                             !/*pr         88*/
!fcg                                                                            !/*pr         89*/
!fcg  counter for fission fragments in bank                                     !/*pr         90*/
!fcg  and of PHT-photons created                                                !/*pr         91*/
      nfiss = 0                                                                 !/*pr         92*/
      nnres = 0                                                                 !/*pr         93*/
      atot = dp0                                                                !/*pr         94*/
      ztot = dp0                                                                !/*pr         95*/
      fising = .false.                                                          !/*pr         96*/
      fision = .false.                                                          !/*pr         97*/
      nnph = 0                                                                  !/*pr         98*/
!fcg                                                                            !/*pr         99*/
      istart = 1                                                                !/*pr        100*/
      u = enext                                                                 !/*pr        101*/
      a = atwght                                                                !/*pr        102*/
      z = charg                                                                 !/*pr        103*/
      un = a - z                                                                !/*pr        104*/
      in = un + dp1m6                                                           !/*pr        105*/
      iz = z + dp1m6                                                            !/*pr        106*/
      in = max(1,in)                                                            !/*pr        107*/
      iz = max(1,iz)                                                            !/*pr        108*/
      aa = max (a,for)                                                          !/*pr        109*/
      emx = fmolnix (iz, in, 2)                                                 !/*pr        110*/
      remn = a*emnucb + emx/dp1000                                              !/*pr        111*/
!  Total recoil energy and kinetic energy of entire nucleus                     !/*pr        112*/
      e = sqrt(pnx**2 + pny**2 + pnz**2 + remn**2)                              !/*pr        113*/
      tres = (e - remn)*dp1000                                                  !/*pr        114*/
      v(1) = pnx/e                                                              !/*pr        115*/
      v(2) = pny/e                                                              !/*pr        116*/
      v(3) = pnz/e                                                              !/*pr        117*/
      uglmom(1) = elx                                                           !/*pr        118*/
      uglmom(2) = ely                                                           !/*pr        119*/
      uglmom(3) = elz                                                           !/*pr        120*/
      ktot = kstart - 1                                                         !/*pr        121*/
      n = n0                                                                    !/*pr        122*/
      h = nh0                                                                   !/*pr        123*/
      p = np0                                                                   !/*pr        124*/
      pz = npz0                                                                 !/*pr        125*/
      u = u - tres                                                              !/*pr        126*/
      um2 = max(uglmom(1)**2 + uglmom(2)**2 + uglmom(3)**2, zro)                !/*pr        127*/
      ln = int(sqrt(um2))                                                       !/*pr        128*/
      if (ib.eq.6 .or. iz.lt.19) then                                           !/*pr        129*/
        erotev = dp5225m2*um2/(r0m*r0m*a**fvthrd + dp10*a*amm**2)               !/*pr        130*/
        emcorr = one + dp1m3*emx/(emnucb*a)                                     !/*pr        131*/
        erotev = erotev/emcorr                                                  !/*pr        132*/
!   For very light nuclei, limit the rotational energy to 1 MeV                 !/*pr        133*/
!   per nucleon; renormalize L.                                                 !/*pr        134*/
        if (erotev.gt.a) then                                                   !/*pr        135*/
          ratio = sqrt(a/erotev)                                                !/*prxk        1*/
          erotev = a                                                            !/*pr        137*/
          uglmom(1) = ratio*uglmom(1)                                           !/*pr        138*/
          uglmom(2) = ratio*uglmom(2)                                           !/*pr        139*/
          uglmom(3) = ratio*uglmom(3)                                           !/*pr        140*/
          ln = int(ratio*sqrt(um2))                                             !/*pr        141*/
        endif                                                                   !/*pr        142*/
      elseif (ib.eq.7 .and. iz.ge.19) then                                      !/*pr        143*/
        bf0 = bf (a, z, zro)                                                    !/*pr        144*/
        erotev = egs0                                                           !/*pr        145*/
      endif                                                                     !/*pr        146*/
      if (ipair.eq.0) then                                                      !/*pr        147*/
        izo2 = iz/2                                                             !/*pr        148*/
        ino2 = in/2                                                             !/*pr        149*/
        pair = (one - z + two*izo2) + (one - un + two*ino2)                     !/*pr        150*/
        pevap = cevap*pair/sqrt(a)                                              !/*pr        151*/
      elseif (ipair.eq.1) then                                                  !/*pr        152*/
        pevap = fmolnix (iz, in, 3)                                             !/*pr        153*/
      endif                                                                     !/*pr        154*/
      ue = u - pevap - erotev                                                   !/*pr        155*/
!  Accumulate statistics on average E*, Z, A, L of nucleus at beginning         !/*pr        156*/
!  of preequilibrium decay.                                                     !/*pr        157*/
      npreq = npreq + 1                                                         !/*pr        158*/
      apqtot = apqtot + a                                                       !/*pr        159*/
      apqsq = apqsq + a**2                                                      !/*pr        160*/
      apmin = min (apmin, a)                                                    !/*pr        161*/
      apmax = max (apmax, a)                                                    !/*pr        162*/
      zpqtot = zpqtot + z                                                       !/*pr        163*/
      zpqsq = zpqsq + z**2                                                      !/*pr        164*/
      zpmin = min (zpmin, z)                                                    !/*pr        165*/
      zpmax = max (zpmax, z)                                                    !/*pr        166*/
      epstt = epstt + ue                                                        !/*pr        167*/
      epstsq = epstsq + ue**2                                                   !/*pr        168*/
      epstrmn = min (epstrmn, ue)                                               !/*pr        169*/
      epstrmx = max (epstrmx, ue)                                               !/*pr        170*/
      fln = ln                                                                  !/*pr        171*/
      elptot = elptot + fln                                                     !/*pr        172*/
      elpsq = elpsq + fln**2                                                    !/*pr        173*/
      elpmin = min (elpmin, fln)                                                !/*pr        174*/
      elpmax = max (elpmax, fln)                                                !/*pr        175*/
      if (u.lt.thr) then                                                        !/*pr        176*/
         nret = nret + 1                                                        !/*pr        177*/
         arttot = arttot + a                                                    !/*pr        178*/
         artsq = artsq + a**2                                                   !/*pr        179*/
         artmin = min (artmin, a)                                               !/*pr        180*/
         artmax = max (artmax, a)                                               !/*pr        181*/
         zrttot = zrttot + z                                                    !/*pr        182*/
         zrtsq = zrtsq + z**2                                                   !/*pr        183*/
         zrtmin = min (zrtmin, z)                                               !/*pr        184*/
         zrtmax = max (zrtmax, z)                                               !/*pr        185*/
         fln = ln                                                               !/*pr        186*/
         elrttot = elrttot + fln                                                !/*pr        187*/
         elrtsq = elrtsq + fln**2                                               !/*pr        188*/
         elrtmin = min (elrtmin, fln)                                           !/*pr        189*/
         elrtmax = max (elrtmax, fln)                                           !/*pr        190*/
!   Also add them to the residual nuclei total.                                 !/*pr        191*/
         nres = nres + 1                                                        !/*pr        192*/
         artot = artot + a                                                      !/*pr        193*/
         arsq = arsq + a**2                                                     !/*pr        194*/
         armin = min (armin, a)                                                 !/*pr        195*/
         armax = max (armax, a)                                                 !/*pr        196*/
         zrtot = zrtot + z                                                      !/*pr        197*/
         zrsq = zrsq + z**2                                                     !/*pr        198*/
         zrmin = min (zrmin, z)                                                 !/*pr        199*/
         zrmax = max (zrmax, z)                                                 !/*pr        200*/
         fln = ln                                                               !/*pr        201*/
         elrtot = elrtot + fln                                                  !/*pr        202*/
         elrsq = elrsq + fln**2                                                 !/*pr        203*/
         elrmin = min (elrmin, fln)                                             !/*pr        204*/
         elrmax = max (elrmax, fln)                                             !/*pr        205*/
         erkt = erkt + tres                                                     !/*pr        206*/
         erksq = erksq + tres*tres                                              !/*pr        207*/
         erkmn = min (erkmn, tres)                                              !/*pr        208*/
         erkmx = max (erkmx, tres)                                              !/*pr        209*/
! -------------------------------------------------------                       !/*pr        210*/
!  begin pht photon production                                                  !/*pr        211*/
!  first: accumulate statistics on projectile energy,                           !/*pr        212*/
!         target mass and charge                                                !/*pr        213*/
         nphts  = nphts + 1                                                     !/*pr        214*/
         ephts  = ephts + ue                                                    !/*pr        215*/
         ephtsq = ephtsq + ue*ue                                                !/*pr        216*/
         ephmax = max(ephmax, ue)                                               !/*pr        217*/
         ephmin = min(ephmin, ue)                                               !/*pr        218*/
         aphts  = aphts + a                                                     !/*pr        219*/
         aphtsq = aphtsq + a*a                                                  !/*pr        220*/
         aphmax = max(aphmax, a)                                                !/*pr        221*/
         aphmin = min(aphmin, a)                                                !/*pr        222*/
         zphts  = zphts + z                                                     !/*pr        223*/
         zphtsq = zphtsq + z*z                                                  !/*pr        224*/
         zphmax = max(zphmax, z)                                                !/*pr        225*/
         zphmin = min(zphmin, z)                                                !/*pr        226*/
         nnres = nnres + 1                                                      !/*pr        227*/
         call phtsh(a,z,ue,tres,nnph,nnres)                                     !/*pr        228*/
!  end pht                                                                      !/*pr        229*/
! -------------------------------------------------------                       !/*pr        230*/
         return                                                                 !/*pr        231*/
      endif                                                                     !/*pr        232*/
      do 180 k = kstart,150                                                     !/*pr        233*/
!fcg                                                                            !/*pr        234*/
!fcg   introduced label 181 for fission fragment return                         !/*pr        235*/
 181     if (a.le.for .or. z.le.two) then                                       !/*pr        236*/
!fcg                                                                            !/*pr        237*/
!            write (io, 1000) u, a, z                                           !/*pr        238*/
!            write (itty, 1000) u, a, z                                         !/*pr        239*/
 1000        format (/5x,'In PRECOF, number of nucleons too small;',            !/*pr        240*/
     &              /30x,'U = ',f11.5,', A = ',f5.1,', & Z = ',f4.1)            !/*pr        241*/
            istp = istp + 1                                                     !/*pr        242*/
! -------------------------------------------------------                       !/*pr        243*/
!  begin pht photon production                                                  !/*pr        244*/
!  first: accumulate statistics on projectile energy,                           !/*pr        245*/
!         target mass and charge                                                !/*pr        246*/
            nphts  = nphts + 1                                                  !/*pr        247*/
            ephts  = ephts + ue                                                 !/*pr        248*/
            ephtsq = ephtsq + ue*ue                                             !/*pr        249*/
            ephmax = max(ephmax, ue)                                            !/*pr        250*/
            ephmin = min(ephmin, ue)                                            !/*pr        251*/
            aphts  = aphts + a                                                  !/*pr        252*/
            aphtsq = aphtsq + a*a                                               !/*pr        253*/
            aphmax = max(aphmax, a)                                             !/*pr        254*/
            aphmin = min(aphmin, a)                                             !/*pr        255*/
            zphts  = zphts + z                                                  !/*pr        256*/
            zphtsq = zphtsq + z*z                                               !/*pr        257*/
            zphmax = max(zphmax, z)                                             !/*pr        258*/
            zphmin = min(zphmin, z)                                             !/*pr        259*/
            nnres = nnres + 1                                                   !/*pr        260*/
            call phtsh(a,z,ue,tres,nnph,nnres)                                  !/*pr        261*/
!  end pht                                                                      !/*pr        262*/
! -------------------------------------------------------                       !/*pr        263*/
!                                                                               !/*pr        264*/
!  accumulate A and Z of residual fission fragments                             !/*pr        265*/
            if (fising) then                                                    !/*pr        266*/
               atot = atot + a                                                  !/*pr        267*/
               ztot = ztot + z                                                  !/*pr        268*/
               a = atot                                                         !/*pr        269*/
               z = ztot                                                         !/*pr        270*/
               fising = .false.                                                 !/*pr        271*/
            endif                                                               !/*pr        272*/
!fcg                                                                            !/*pr        273*/
!fcg set up the next fission fragment                                           !/*pr        274*/
            if (nfiss.gt.0.and.idel.eq.1) then                                  !/*pr        275*/
               call fiscem(a,z,u,                                               !/*pr        276*/
     &             tres,e,pnx,pny,pnz,v,uglmom,n,nfiss,emnucb)                  !/*pr        277*/
               fising = .true.                                                  !/*pr        278*/
               ia = a + dp1m6                                                   !/*pr        279*/
               iz = z + dp1m6                                                   !/*pr        280*/
               goto 181                                                         !/*pr        281*/
            endif                                                               !/*pr        282*/
!fcg                                                                            !/*pr        283*/
            return                                                              !/*pr        284*/
         endif                                                                  !/*pr        285*/
         um2 = max(uglmom(1)**2 + uglmom(2)**2 + uglmom(3)**2, zro)             !/*pr        286*/
         ln = int(sqrt(um2))                                                    !/*pr        287*/
         if (ijsp.eq.0) then                                                    !/*pr        288*/
            erotev = zro                                                        !/*pr        289*/
         else                                                                   !/*pr        290*/
            if (ib.eq.7 .and. iz.ge.19) then                                    !/*pr        291*/
               bf0 = bf (a, z, zro)                                             !/*pr        292*/
!                                                                               !/*pr        293*/
!   In the case of ib = 7, erotev is the macroscopic energy                     !/*pr        294*/
!   of the rotating ground state relative to the non-rotating                   !/*pr        295*/
!   spherical nucleus.  It contains rotational, nuclear                         !/*pr        296*/
!   macroscopic, and Coulomb energies.                                          !/*pr        297*/
               erotev = egs0                                                    !/*pr        298*/
            else                                                                !/*pr        299*/
               if (ib.eq.7 .and. iz.lt.19) then                                 !/*pr        300*/
                  ib = 6                                                        !/*pr        301*/
                  bf0 = bf (a, z, zro)                                          !/*pr        302*/
                  ib = 7                                                        !/*pr        303*/
               endif                                                            !/*pr        304*/
!   For ib not = 7, the energy of the ground state is approximated              !/*pr        305*/
!   as the rotational energy of a rigid sphere.                                 !/*pr        306*/
!   Constant = 5/4 * (hbar-c)**2 / 931.5014                                     !/*pr        307*/
!   Use actual mass of nucleus from table of mass excesses                      !/*pr        308*/
!   Last term in denom. is diffuseness correction to Mom. of Inertia.           !/*pr        309*/
               un = a - z                                                       !/*pr        310*/
               in = int(un + dp1m6)                                             !/*pr        311*/
               iz = int(z + dp1m6)                                              !/*pr        312*/
               in = max(in,1)                                                   !/*pr        313*/
               iz = max(iz,1)                                                   !/*pr        314*/
               emx = fmolnix (iz, in, 2)                                        !/*pr        315*/
               erotev = dp5225m2*um2/(r0m*r0m*a**fvthrd+dp10*a*amm**2)          !/*pr        316*/
               emcorr = one + dp1m3*emx/(emnucb*a)                              !/*pr        317*/
               erotev = erotev/emcorr                                           !/*pr        318*/
!   For very light nuclei, limit the rotational energy to 1 MeV                 !/*pr        319*/
!   per nucleon; renormalize L.                                                 !/*pr        320*/
               if (erotev.gt.a) then                                            !/*pr        321*/
                  ratio = sqrt(a/erotev)                                        !/*prxk        2*/
                  erotev = a                                                    !/*pr        323*/
                  uglmom(1) = ratio*uglmom(1)                                   !/*pr        324*/
                  uglmom(2) = ratio*uglmom(2)                                   !/*pr        325*/
                  uglmom(3) = ratio*uglmom(3)                                   !/*pr        326*/
                  ln = int(ratio*sqrt(um2))                                     !/*pr        327*/
               endif                                                            !/*pr        328*/
            endif                                                               !/*pr        329*/
         endif                                                                  !/*pr        330*/
         dl = deltam (a, z, ish)                                                !/*pr        331*/
         call vhelp                                                             !/*pr        332*/
!   Pair = 2.0 for even-even; = 1.0 for odd-A;                                  !/*pr        333*/
!        = 0.0 for odd-odd nucleus.                                             !/*pr        334*/
         if (ipair.eq.0) then                                                   !/*pr        335*/
            izo2 = iz/2                                                         !/*pr        336*/
            ino2 = in/2                                                         !/*pr        337*/
            pair = (one - z + two*izo2) + (one - un + two*ino2)                 !/*pr        338*/
            pevap = cevap*pair/sqrt(a)                                          !/*pr        339*/
         elseif (ipair.eq.1) then                                               !/*pr        340*/
            pevap = fmolnix (iz, in, 3)                                         !/*pr        341*/
         endif                                                                  !/*pr        342*/
!   "Thermal" energy of nucleus at rotating ground state:                       !/*pr        343*/
         ue = u - pevap - erotev                                                !/*pr        344*/
         if (ue.le.dp1m1) then                                                  !/*pr        345*/
!   Accumulate statistics on residual nuclei average kinetic                    !/*pr        346*/
!   energy, charge, mass, and angular momentum.                                 !/*pr        347*/
            nres = nres + 1                                                     !/*pr        348*/
            artot = artot + a                                                   !/*pr        349*/
            arsq = arsq + a**2                                                  !/*pr        350*/
            armin = min (armin, a)                                              !/*pr        351*/
            armax = max (armax, a)                                              !/*pr        352*/
            zrtot = zrtot + z                                                   !/*pr        353*/
            zrsq = zrsq + z**2                                                  !/*pr        354*/
            zrmin = min (zrmin, z)                                              !/*pr        355*/
            zrmax = max (zrmax, z)                                              !/*pr        356*/
            fln = ln                                                            !/*pr        357*/
            elrtot = elrtot + fln                                               !/*pr        358*/
            elrsq = elrsq + fln**2                                              !/*pr        359*/
            elrmin = min (elrmin, fln)                                          !/*pr        360*/
            elrmax = max (elrmax, fln)                                          !/*pr        361*/
            erkt = erkt + tres                                                  !/*pr        362*/
            erksq = erksq + tres*tres                                           !/*pr        363*/
            erkmn = min (erkmn, tres)                                           !/*pr        364*/
            erkmx = max (erkmx, tres)                                           !/*pr        365*/
! -------------------------------------------------------                       !/*pr        366*/
!  begin pht photon production                                                  !/*pr        367*/
!  first: accumulate statistics on projectile energy,                           !/*pr        368*/
!         target mass and charge                                                !/*pr        369*/
            nphts  = nphts + 1                                                  !/*pr        370*/
            ephts  = ephts + ue                                                 !/*pr        371*/
            ephtsq = ephtsq + ue*ue                                             !/*pr        372*/
            ephmax = max(ephmax, ue)                                            !/*pr        373*/
            ephmin = min(ephmin, ue)                                            !/*pr        374*/
            aphts  = aphts + a                                                  !/*pr        375*/
            aphtsq = aphtsq + a*a                                               !/*pr        376*/
            aphmax = max(aphmax, a)                                             !/*pr        377*/
            aphmin = min(aphmin, a)                                             !/*pr        378*/
            zphts  = zphts + z                                                  !/*pr        379*/
            zphtsq = zphtsq + z*z                                               !/*pr        380*/
            zphmax = max(zphmax, z)                                             !/*pr        381*/
            zphmin = min(zphmin, z)                                             !/*pr        382*/
            nnres = nnres + 1                                                   !/*pr        383*/
            call phtsh(a,z,ue,tres,nnph,nnres)                                  !/*pr        384*/
!  end pht                                                                      !/*pr        385*/
!                                                                               !/*pr        386*/
!  accumulate A and Z of residual fission fragments                             !/*pr        387*/
            if (fising) then                                                    !/*pr        388*/
               atot = atot + a                                                  !/*pr        389*/
               ztot = ztot + z                                                  !/*pr        390*/
               a = atot                                                         !/*pr        391*/
               z = ztot                                                         !/*pr        392*/
               fising = .false.                                                 !/*pr        393*/
            endif                                                               !/*pr        394*/
!fcg                                                                            !/*pr        395*/
!fcg set up the next fission fragment                                           !/*pr        396*/
            if (nfiss.gt.0.and.idel.eq.1) then                                  !/*pr        397*/
               call fiscem(a,z,u,                                               !/*pr        398*/
     &             tres,e,pnx,pny,pnz,v,uglmom,n,nfiss,emnucb)                  !/*pr        399*/
               fising = .true.                                                  !/*pr        400*/
               ia = a + dp1m6                                                   !/*pr        401*/
               iz = z + dp1m6                                                   !/*pr        402*/
               goto 181                                                         !/*pr        403*/
            endif                                                               !/*pr        404*/
!fcg                                                                            !/*pr        405*/
! -------------------------------------------------------                       !/*pr        406*/
            return                                                              !/*pr        407*/
         endif                                                                  !/*pr        408*/
         am = fam (a, z, ue)                                                    !/*pr        409*/
         rr = zro                                                               !/*prxk        3*/
         do 10 i = 1,6                                                          !/*pr        410*/
            afj(i) = a - aj(i)                                                  !/*pr        411*/
            zfj(i) = z - zj(i)                                                  !/*pr        412*/
            if (afj(i).lt.for .or. zfj(i).lt.two .or. zfj(i).ge.afj(i))         !/*prxk        4*/
     &            then                                                          !/*pr        414*/
               bj(i) = zro                                                      !/*pr        415*/
               rj(i) = zro                                                      !/*pr        416*/
               vj(i) = zro                                                      !/*pr        417*/
               pevapj(i) = zro                                                  !/*pr        418*/
               go to 10                                                         !/*pr        419*/
            endif                                                               !/*pr        420*/
            unj = afj(i) - zfj(i)                                               !/*pr        421*/
            iunj = int(unj + dp1m4)                                             !/*pr        422*/
            izj = int(zfj(i) + dp1m4)                                           !/*pr        423*/
            if (ipair.eq.0) then                                                !/*pr        424*/
               izjo2 = izj/2                                                    !/*pr        425*/
               iunjo2 = iunj/2                                                  !/*pr        426*/
               pairj = (one-zfj(i)+two*izjo2) + (one-unj+two*iunjo2)            !/*pr        427*/
               pevapj(i) = cevap*pairj/sqrt(afj(i))                             !/*pr        428*/
            elseif (ipair.eq.1) then                                            !/*pr        429*/
               pevapj(i) = fmolnix (izj, iunj, 3)                               !/*pr        430*/
            endif                                                               !/*pr        431*/
!   "Free" energy in the j-th particle-emission channel:                        !/*pr        432*/
            uej = u - pevapj(i) - erotev                                        !/*pr        433*/
            if (uej.le.one) then                                                !/*pr        434*/
               rj(i) = zro                                                      !/*pr        435*/
            else                                                                !/*pr        436*/
               ami(i) = fam (afj(i), zfj(i), uej)                               !/*pr        437*/
               vj(i) = colomb (i, radncl, am)                                   !/*pr        438*/
!   Binding energy                                                              !/*pr        439*/
               if (ish.eq.2) then                                               !/*pr        440*/
                 bj(i) = deltam (afj(i), zfj(i), 2) - (dl - dlmo(i))            !/*pr        441*/
               elseif (ish.eq.4) then                                           !/*pr        442*/
                 bj(i) = deltam (afj(i), zfj(i), 4) - (dl - dlmn(i))            !/*pr        443*/
               endif                                                            !/*pr        444*/
!   Free "thermal" energy at the Coulomb barrier                                !/*pr        445*/
               rj(i) = uej - (bj(i) + vj(i))                                    !/*pr        446*/
               rj(i) = max (zro, rj(i))                                         !/*pr        447*/
            endif                                                               !/*pr        448*/
         rr = rr + rj(i)                                                        !/*prxk        5*/
  10     continue                                                               !/*pr        449*/
         afj(7) = a                                                             !/*pr        450*/
!  "alpha" for neutron emission channel from App. 6; p260.                      !/*pr        451*/
         r0j(1) = dp76m2 + dp22m1/afj(1)**thrd                                  !/*pr        452*/
!  "beta" for neutron emission channel from App. 6; p260.                       !/*pr        453*/
!  ["beta" for charged particle channels is -vj(i); Coulomb barrier.]           !/*pr        454*/
         bn = (dp212m2/afj(1)**twthrd - dp5m2)/r0j(1)                           !/*pr        455*/
         do 20 ir = 2,6                                                         !/*pr        456*/
            r0j(ir) = one + cc(ir)                                              !/*pr        457*/
   20    continue                                                               !/*pr        458*/
         nsp = int(sqrt(dp119m2*am*a*u + dph))                                  !/*pr        459*/
!  nsp is the number of excitons at which a compound nucleus is                 !/*pr        460*/
!  assumed to be formed.                                                        !/*pr        461*/
   30    if (n.lt.nsp .and. rr.gt.zro) then                                     !/*prxk        6*/
!  **************** Pre-equilibrium emission *************************          !/*pr        463*/
   40       if (p.lt.one) then                                                  !/*pr        464*/
               p = p + one                                                      !/*pr        465*/
               h = h + one                                                      !/*pr        466*/
               n = n + 2                                                        !/*pr        467*/
               go to 40                                                         !/*pr        468*/
            endif                                                               !/*pr        469*/
            exn = p + h                                                         !/*pr        470*/
            alj(1) = p*(exn - one)                                              !/*pr        471*/
            alj(2) = alj(1)                                                     !/*pr        472*/
            alj(3) = alj(2)*(p - one)*(exn - two)/two                           !/*pr        473*/
            alj(4) = alj(3)*(p - two)*(exn - thr)/dp6                           !/*pr        474*/
            alj(5) = alj(4)                                                     !/*pr        475*/
            alj(6) = alj(5)*(p - thr)*(exn - for)/dp12                          !/*pr        476*/
            gb(3) = dp16/a                                                      !/*pr        477*/
            gb(4) = dp243/a**2                                                  !/*pr        478*/
            gb(5) = gb(4)                                                       !/*pr        479*/
            gb(6) = dp256*gb(3)/a**2                                            !/*pr        480*/
            do 50 l = 1,6                                                       !/*pr        481*/
!   gj is the emission rate of particles of type j (n,p,d,t,3He,4He)            !/*pr        482*/
!   into the continuum.                                                         !/*pr        483*/
               gj(l) = zro                                                      !/*pr        484*/
               if ((p - aj(l)) .gt. -dp1m2) then                                !/*pr        485*/
                 if (p + h - aj(l) .gt. dp1m2) then                             !/*pr        486*/
                   if (pz - zj(l) .gt. -dp1m2) then                             !/*pr        487*/
                     if (rj(l).gt.zro) then                                     !/*pr        488*/
                       ac = dp595m3*fam (afj(l), zfj(l), u)                     !/*pr        489*/
                       gj(l) = gamagu (l)                                       !/*pr        490*/
                     endif                                                      !/*pr        491*/
                   endif                                                        !/*pr        492*/
                 endif                                                          !/*pr        493*/
               endif                                                            !/*pr        494*/
   50       continue                                                            !/*pr        495*/
            g = zro                                                             !/*pr        496*/
            do 60 i = 1,6                                                       !/*pr        497*/
               g = g + gj(i)                                                    !/*pr        498*/
   60       continue                                                            !/*pr        499*/
!   g is total preequilibrium particle emission rate                            !/*pr        500*/
            if (n.le.0) then                                                    !/*pr        501*/
               write(io, 1100)                                                  !/*pr        502*/
               write(itty, 1100)                                                !/*pr        503*/
 1100          format (/5x,'In PRECOF, number of Excitons = 0.')                !/*pr        504*/
! -------------------------------------------------------                       !/*pr        505*/
!  begin pht photon production                                                  !/*pr        506*/
!  first:  accumulate statistics on projectile energy,                          !/*pr        507*/
!          target mass and charge                                               !/*pr        508*/
               nphts  = nphts + 1                                               !/*pr        509*/
               ephts  = ephts + ue                                              !/*pr        510*/
               ephtsq = ephtsq + ue*ue                                          !/*pr        511*/
               ephmax = max(ephmax, ue)                                         !/*pr        512*/
               ephmin = min(ephmin, ue)                                         !/*pr        513*/
               aphts  = aphts + a                                               !/*pr        514*/
               aphtsq = aphtsq + a*a                                            !/*pr        515*/
               aphmax = max(aphmax, a)                                          !/*pr        516*/
               aphmin = min(aphmin, a)                                          !/*pr        517*/
               zphts  = zphts + z                                               !/*pr        518*/
               zphtsq = zphtsq + z*z                                            !/*pr        519*/
               zphmax = max(zphmax, z)                                          !/*pr        520*/
               zphmin = min(zphmin, z)                                          !/*pr        521*/
               nnres = nnres + 1                                                !/*pr        522*/
               call phtsh(a,z,ue,tres,nnph,nnres)                               !/*pr        523*/
!  end pht                                                                      !/*pr        524*/
! -------------------------------------------------------                       !/*pr        525*/
!                                                                               !/*pr        526*/
!  accumulate A and Z of residual fission fragments                             !/*pr        527*/
               if (fising) then                                                 !/*pr        528*/
                  atot = atot + a                                               !/*pr        529*/
                  ztot = ztot + z                                               !/*pr        530*/
                  a = atot                                                      !/*pr        531*/
                  z = ztot                                                      !/*pr        532*/
                  fising = .false.                                              !/*pr        533*/
               endif                                                            !/*pr        534*/
!fcg                                                                            !/*pr        535*/
!fcg set up the next fission fragment                                           !/*pr        536*/
               if (nfiss.gt.0.and.idel.eq.1) then                               !/*pr        537*/
                  call fiscem(a,z,u,tres,e,pnx,pny,pnz,                         !/*pr        538*/
     &                        v,uglmom,n,nfiss,emnucb)                          !/*pr        539*/
                  fising = .true.                                               !/*pr        540*/
                  ia = a + dp1m6                                                !/*pr        541*/
                  iz = z + dp1m6                                                !/*pr        542*/
                  goto 181                                                      !/*pr        543*/
               endif                                                            !/*pr        544*/
!fcg                                                                            !/*pr        545*/
               return                                                           !/*pr        546*/
            endif                                                               !/*pr        547*/
            ac = dp595m3*am                                                     !/*pr        548*/
            call ctrans (p, h, ac, c1, c2, c3)                                  !/*pr        549*/
            c = c1 + c2 + c3                                                    !/*pr        550*/
            if (c.le.0.0) go to 30                                              !/*prxk        7*/
            b2 = c + g                                                          !/*pr        552*/
            c11 = c1/c                                                          !/*prxk        8*/
            c21 = (c2 + c1)/c                                                   !/*prxk        9*/
   65       b1 = rang()                                                         !/*prxk       10*/
            if (b1.gt.g/b2) then                                                !/*pr        562*/
!  Transition in exciton number (-2, 0, or +2)                                  !/*pr        563*/
               b3 = rang()                                                      !/*pr        566*/
               if (b3.le.c21) then                                              !/*pr        567*/
                  bz = rang()                                                   !/*pr        568*/
                  bz1 = z/a                                                     !/*pr        569*/
                  if (b3.le.c11) then                                           !/*pr        570*/
                     n = n + 2                                                  !/*pr        571*/
                     p = p + one                                                !/*pr        572*/
                     h = h + one                                                !/*pr        573*/
                     fc = one                                                   !/*pr        574*/
                  elseif (b3.gt.c11) then                                       !/*pr        575*/
                     n = n - 2                                                  !/*pr        576*/
                     p = p - one                                                !/*pr        577*/
                     h = h - one                                                !/*pr        578*/
                     fc = -one                                                  !/*pr        579*/
                     if (pz.le.zro) go to 30                                    !/*pr        580*/
                  endif                                                         !/*pr        581*/
                  if (bz.le.bz1) pz = pz + fc                                   !/*pr        582*/
                  go to 30                                                      !/*prxk       11*/
               else                                                             !/*prxk       12*/
                  go to 65                                                      !/*prxk       13*/
               endif                                                            !/*pr        583*/
            else                                                                !/*prxk       14*/
!   Preequilibrium particle is emitted:                                         !/*pr        586*/
              p2 = dp250*rang()**thrd                                           !/*prxk       15*/
              ct2 = one - two*rang()                                            !/*prxk       16*/
              st2 = sqrt(one - ct2*ct2)                                         !/*prxk       17*/
              fi2 = twpi*rang()                                                 !/*prxk       18*/
              f1 = dp1000/p                                                     !/*prxk       19*/
              f2 = p2*st2                                                       !/*prxk       20*/
              p12(1) = f2*cos(fi2) + pnx*f1                                     !/*prxk       21*/
              p12(2) = f2*sin(fi2) + pny*f1                                     !/*prxk       22*/
              p12(3) = p2*ct2 + pnz*f1                                          !/*prxk       23*/
!                                                                               !/*pr        587*/
!   gj is converted to the sum up to j of all the rates for                     !/*pr        588*/
!   particles with index less than or equal to j.                               !/*pr        589*/
            do 70 j = 2,6                                                       !/*pr        590*/
               gj(j) = gj(j-1) + gj(j)                                          !/*pr        591*/
   70       continue                                                            !/*pr        592*/
!   Randomly select particle to be emitted, according to partial                !/*pr        593*/
!   emission rates.                                                             !/*pr        594*/
   80       b = rang()*g                                                        !/*pr        595*/
            do 90 j = 1,6                                                       !/*pr        596*/
               if (b.le.gj(j)) then                                             !/*pr        597*/
                  lm = j                                                        !/*pr        598*/
                  go to 100                                                     !/*pr        599*/
               endif                                                            !/*pr        600*/
   90       continue                                                            !/*pr        601*/
  100       continue                                                            !/*pr        602*/
            ep1 = tkinm1 (lm, p, h)                                             !/*pr        603*/
            ind = 1                                                             !/*pr        604*/
            endif                                                               !/*prxk       24*/
         else                                                                   !/*pr        605*/
!  ****************** equilibrium emission ***************************          !/*pr        606*/
!   Calculate statistics on average E*, Z, A, L when nucleus enters             !/*pr        607*/
!   statistical decay phase.                                                    !/*pr        608*/
            if (istart.eq.1) then                                               !/*pr        609*/
               neq = neq + 1                                                    !/*pr        610*/
               aeqtot = aeqtot + a                                              !/*pr        611*/
               aeqsq = aeqsq + a**2                                             !/*pr        612*/
               aemin = min (aemin, a)                                           !/*pr        613*/
               aemax = max (aemax, a)                                           !/*pr        614*/
               zeqtot = zeqtot + z                                              !/*pr        615*/
               zeqsq = zeqsq + z**2                                             !/*pr        616*/
               zemin = min (zemin, z)                                           !/*pr        617*/
               zemax = max (zemax, z)                                           !/*pr        618*/
               eestt = eestt + ue                                               !/*pr        619*/
               eestsq = eestsq + ue**2                                          !/*pr        620*/
               eestrmn = min (eestrmn, ue)                                      !/*pr        621*/
               eestrmx = max (eestrmx, ue)                                      !/*pr        622*/
               fln = ln                                                         !/*pr        623*/
               eletot = eletot + fln                                            !/*pr        624*/
               elesq = elesq + fln**2                                           !/*pr        625*/
               elemin = min (elemin, fln)                                       !/*pr        626*/
               elemax = max (elemax, fln)                                       !/*pr        627*/
               istart = 0                                                       !/*pr        628*/
            endif                                                               !/*pr        629*/
            if (idel.le.0) then                                                 !/*pr        630*/
               rj(7) = zro                                                      !/*pr        631*/
            else                                                                !/*pr        632*/
!   (bf0 already calculated above for ib = 7)                                   !/*pr        633*/
               if (ib.ne.7) bf0 = bf (a, z, uef)                                !/*pr        634*/
               if (bf0.le.zro) then                                             !/*pr        635*/
                  lm = 7                                                        !/*pr        636*/
                  go to 160                                                     !/*pr        637*/
               endif                                                            !/*pr        638*/
!   Pfis is the pairing energy at the saddle point.                             !/*pr        639*/
               if (ipair.eq.0) then                                             !/*pr        640*/
                  pfis = cfis*pair/sqrt(aa)                                     !/*pr        641*/
               elseif (ipair.eq.1) then                                         !/*pr        642*/
                  pr = zro                                                      !/*pr        643*/
                  if (iz.eq.2*(iz/2)) pr = pr + one/z**thrd                     !/*pr        644*/
                  if (in.eq.2*(in/2)) pr = pr + one/un**thrd                    !/*pr        645*/
                  if (z.ge.15) then                                             !/*pr        646*/
                     call bsfit (z, a, ln, bs)                                  !/*pr        647*/
                  else                                                          !/*pr        648*/
                     bs = dp125m2                                               !/*pr        649*/
                  endif                                                         !/*pr        650*/
                  pfis = dp48m1 * pr * bs                                       !/*pr        651*/
               endif                                                            !/*pr        652*/
               erotsp = erotev + bf0                                            !/*pr        653*/
!   Uef is the "excitation" energy of the nucleus when it is at the             !/*pr        654*/
!   saddle point.                                                               !/*pr        655*/
               uef = u - pfis - erotsp                                          !/*pr        656*/
               if (uef.le.zro) then                                             !/*pr        657*/
                  rj(7) = zro                                                   !/*pr        658*/
                  go to 110                                                     !/*pr        659*/
               endif                                                            !/*pr        660*/
               emone = -one                                                     !/*pr        661*/
!fcg                                                                            !/*pr        662*/
!fcg  calculate a parametrized wam                                              !/*pr        663*/
               if (wam.le.0.0) then                                             !/*pr        664*/
                  wan = wamm(a,z,ln,2)                                          !/*pr        665*/
               else                                                             !/*pr        666*/
                  wan = wam                                                     !/*pr        667*/
               endif                                                            !/*pr        668*/
!fcg                                                                            !/*pr        669*/
               amf = wan*fam (a, z, emone)                                      !/*pr        670*/
               bj(7) = bf0                                                      !/*pr        671*/
               rj(7) = uef                                                      !/*pr        672*/
            endif                                                               !/*pr        673*/
  110       call arfaf (per, ami)                                               !/*pr        674*/
            do 120 i = 1,7                                                      !/*pr        675*/
               if (rj(i).le.zro) then                                           !/*pr        676*/
                  gj(i) = zro                                                   !/*pr        677*/
               else                                                             !/*pr        678*/
                  gj(i) = gameqf (i, per, ami(i), radncl)                       !/*pr        679*/
               endif                                                            !/*pr        680*/
  120       continue                                                            !/*pr        681*/
            gt = zro                                                            !/*pr        682*/
            do 130 i = 1,7                                                      !/*pr        683*/
               gt = gt + gj(i)                                                  !/*pr        684*/
  130       continue                                                            !/*pr        685*/
            if (gt.le.zro) then                                                 !/*pr        686*/
!   Accumulate statistics on residual nuclei average kinetic                    !/*pr        687*/
!   energy, charge, mass, and angular momentum.                                 !/*pr        688*/
               nres = nres + 1                                                  !/*pr        689*/
               artot = artot + a                                                !/*pr        690*/
               arsq = arsq + a**2                                               !/*pr        691*/
               armin = min (armin, a)                                           !/*pr        692*/
               armax = max (armax, a)                                           !/*pr        693*/
               zrtot = zrtot + z                                                !/*pr        694*/
               zrsq = zrsq + z**2                                               !/*pr        695*/
               zrmin = min (zrmin, z)                                           !/*pr        696*/
               zrmax = max (zrmax, z)                                           !/*pr        697*/
!              if (ln.lt.0 .or. ln.gt.90) then                                  !/*pr        698*/
!                write (io, 2000) ln, a, z, ue                                  !/*pr        699*/
!                write (itty, 2000) ln, a, z, ue                                !/*pr        700*/
 2000            format (1x,'Unusual value of ln; ln, a, z, ue = ',             !/*pr        701*/
     &                   i4,2x,2f8.1,f9.3)                                      !/*pr        702*/
!              endif                                                            !/*pr        703*/
               fln = ln                                                         !/*pr        704*/
               elrtot = elrtot + fln                                            !/*pr        705*/
               elrsq = elrsq + fln**2                                           !/*pr        706*/
               elrmin = min (elrmin, fln)                                       !/*pr        707*/
               elrmax = max (elrmax, fln)                                       !/*pr        708*/
               erkt = erkt + tres                                               !/*pr        709*/
               erksq = erksq + tres*tres                                        !/*pr        710*/
               erkmn = min (erkmn, tres)                                        !/*pr        711*/
               erkmx = max (erkmx, tres)                                        !/*pr        712*/
! -------------------------------------------------------                       !/*pr        713*/
!  begin pht photon production                                                  !/*pr        714*/
!  first: accumulate statistics on projectile energy,                           !/*pr        715*/
!         target mass and charge                                                !/*pr        716*/
               nphts  = nphts + 1                                               !/*pr        717*/
               ephts  = ephts + ue                                              !/*pr        718*/
               ephtsq = ephtsq + ue*ue                                          !/*pr        719*/
               ephmax = max(ephmax, ue)                                         !/*pr        720*/
               ephmin = min(ephmin, ue)                                         !/*pr        721*/
               aphts  = aphts + a                                               !/*pr        722*/
               aphtsq = aphtsq + a*a                                            !/*pr        723*/
               aphmax = max(aphmax, a)                                          !/*pr        724*/
               aphmin = min(aphmin, a)                                          !/*pr        725*/
               zphts  = zphts + z                                               !/*pr        726*/
               zphtsq = zphtsq + z*z                                            !/*pr        727*/
               zphmax = max(zphmax, z)                                          !/*pr        728*/
               zphmin = min(zphmin, z)                                          !/*pr        729*/
               nnres = nnres + 1                                                !/*pr        730*/
               call phtsh(a,z,ue,tres,nnph,nnres)                               !/*pr        731*/
!  end pht                                                                      !/*pr        732*/
! -------------------------------------------------------                       !/*pr        733*/
!                                                                               !/*pr        734*/
!  accumulate A and Z of residual fission fragments                             !/*pr        735*/
               if (fising) then                                                 !/*pr        736*/
                  atot = atot + a                                               !/*pr        737*/
                  ztot = ztot + z                                               !/*pr        738*/
                  a = atot                                                      !/*pr        739*/
                  z = ztot                                                      !/*pr        740*/
                  fising = .false.                                              !/*pr        741*/
               endif                                                            !/*pr        742*/
!fcg                                                                            !/*pr        743*/
!fcg set up the next fission fragment                                           !/*pr        744*/
               if (nfiss.gt.0.and.idel.eq.1) then                               !/*pr        745*/
                  call fiscem(a,z,u,                                            !/*pr        746*/
     &               tres,e,pnx,pny,pnz,v,uglmom,n,nfiss,emnucb)                !/*pr        747*/
                  fising = .true.                                               !/*pr        748*/
                  ia = a + dp1m6                                                !/*pr        749*/
                  iz = z + dp1m6                                                !/*pr        750*/
                  goto 181                                                      !/*pr        751*/
               endif                                                            !/*pr        752*/
!fcg                                                                            !/*pr        753*/
               return                                                           !/*pr        754*/
            endif                                                               !/*pr        755*/
            wt = wt*(one - gj(7)/gt)                                            !/*pr        756*/
            wf = one - wt                                                       !/*pr        757*/
!   gj is converted from being the partial width for the j-th                   !/*pr        758*/
!   channel to being the sum of all widths up to and including the              !/*pr        759*/
!   j-th channel.                                                               !/*pr        760*/
            do 140 j = 2,7                                                      !/*pr        761*/
               gj(j) = gj(j-1) + gj(j)                                          !/*pr        762*/
  140       continue                                                            !/*pr        763*/
            b = rang()*gt                                                       !/*pr        764*/
            do 150 j = 1,7                                                      !/*pr        765*/
               if (b.lt.gj(j)) then                                             !/*pr        766*/
                  lm = j                                                        !/*pr        767*/
                  go to 160                                                     !/*pr        768*/
               endif                                                            !/*pr        769*/
  150       continue                                                            !/*pr        770*/
  160       if (lm.eq.7 .and. idel.ne.0) then                                   !/*pr        771*/
!                                                                               !/*pr        772*/
!     calculate the fission fragments with RAL fission model                    !/*pr        773*/
               if (idel.eq.1) then                                              !/*pr        774*/
                  call ralsh(a,z,ue,v,n,uglmom,nfiss,fision)                    !/*pr        775*/
                  if (fision) then                                              !/*pr        776*/
                     fusion = one                                               !/*pr        777*/
                     estart = estart + ue                                       !/*pr        778*/
                     estarsq = estarsq + ue**2                                  !/*pr        779*/
                     estrmn = min(estrmn, ue)                                   !/*pr        780*/
                     estrmx = max(estrmx, ue)                                   !/*pr        781*/
                     fln = ln                                                   !/*pr        782*/
                     elmmax = max(fln, elmmax)                                  !/*pr        783*/
                     elmmin = min(fln, elmmin)                                  !/*pr        784*/
                     astart = astart + a                                        !/*pr        785*/
                     astarsq = astarsq + a*a                                    !/*pr        786*/
                     ammin = min(a, ammin)                                      !/*pr        787*/
                     ammax = max(a, ammax)                                      !/*pr        788*/
                     zstart = zstart + z                                        !/*pr        789*/
                     zstarsq = zstarsq + z*z                                    !/*pr        790*/
                     zmmin = min(z, zmmin)                                      !/*pr        791*/
                     zmmax = max(z, zmmax)                                      !/*pr        792*/
                     bftot = bftot + bj(7)                                      !/*pr        793*/
                     bfsq = bfsq + bj(7)**2                                     !/*pr        794*/
                     bfmin = min(bfmin, bj(7))                                  !/*pr        795*/
                     bfmax = max(bfmax, bj(7))                                  !/*pr        796*/
                     if (nfiss.gt.0) then                                       !/*pr        797*/
                        call fiscem(a,z,u,                                      !/*pr        798*/
     &                       tres,e,pnx,pny,pnz,v,uglmom,n,nfiss,emnucb)        !/*pr        799*/
                        fising = .true.                                         !/*pr        800*/
                        ia = a + dp1m6                                          !/*pr        801*/
                        iz = z + dp1m6                                          !/*pr        802*/
                        goto 181                                                !/*pr        803*/
                     endif                                                      !/*pr        804*/
                  else                                                          !/*pr        805*/
!                                                                               !/*pr        806*/
!     fission of nuclides Z<71 not allowed: choose another channel              !/*pr        807*/
                     rj(7) = dp0                                                !/*pr        808*/
                     go to 110                                                  !/*pr        809*/
                  endif                                                         !/*pr        810*/
               else                                                             !/*pr        811*/
                  return                                                        !/*pr        812*/
               endif                                                            !/*pr        813*/
            endif                                                               !/*pr        814*/
!                                                                               !/*pr        815*/
            ep1 = tkin (lm, am)                                                 !/*pr        816*/
            ind = 0                                                             !/*pr        817*/
!  *******************************************************************          !/*pr        818*/
         endif                                                                  !/*pr        819*/
         ep2 = zj(lm)                                                           !/*pr        820*/
         izj = int(zj(lm) + dp1m6)                                              !/*pr        821*/
         inj = int(aj(lm) - zj(lm) + dp1m6)                                     !/*pr        822*/
         if (izj.ne.0) then                                                     !/*pr        823*/
            emxp = fmolnix (izj, inj, 2)                                        !/*pr        824*/
         else                                                                   !/*pr        825*/
            emxp = dp8071m3                                                     !/*pr        826*/
         endif                                                                  !/*pr        827*/
         ep3 = dp1000*emnucb*aj(lm) + emxp                                      !/*pr        828*/
         ktot = ktot + 1                                                        !/*pr        829*/
         p = p - aj(lm)                                                         !/*pr        830*/
         n1 = int(aj(lm))                                                       !/*pr        831*/
         n = n - n1                                                             !/*pr        832*/
         pz = pz - ep2                                                          !/*pr        833*/
         parz(6,k) = ep2                                                        !/*pr        834*/
         parz(1,k) = lm                                                         !/*pr        835*/
         u = rj(lm) - ep1 + vj(lm) + erotev + pevapj(lm)                        !/*prxk       25*/
         if (ind.le.0) then                                                     !/*pr        837*/
!  Compound decay particle                                                      !/*pr        838*/
            parz(5,k) = dp1000                                                  !/*pr        839*/
         else                                                                   !/*pr        841*/
!  Preequilibrium particle                                                      !/*pr        842*/
            parz(5,k) = dp100                                                   !/*pr        843*/
         endif                                                                  !/*pr        844*/
         a = afj(lm)                                                            !/*pr        845*/
         z = zfj(lm)                                                            !/*pr        846*/
         un = a - z                                                             !/*pr        847*/
         iz = int(z + dp1m6)                                                    !/*pr        848*/
         in = int(un + dp1m6)                                                   !/*pr        849*/
         iz = max(1,iz)                                                         !/*pr        850*/
         in = max(1,in)                                                         !/*pr        851*/
         aa = max(for,a)                                                        !/*pr        852*/
         emx = fmolnix (iz, in, 2)                                              !/*pr        853*/
         v2 = v(1)**2 + v(2)**2 + v(3)**2                                       !/*pr        854*/
         pm = sqrt(ep1*(ep1 + two*ep3))                                         !/*pr        855*/
         spt(4,k) = ep2                                                         !/*pr        856*/
         spt(5,k) = ep3                                                         !/*pr        857*/
         if (ind.ne.0 .and. v2.gt.dp1m7) then                                   !/*pr        858*/
            cx = one - two*rang()                                               !/*prxk       26*/
            sx = sqrt(one - cx*cx)                                              !/*pr        860*/
            fi = twpi*rang()                                                    !/*pr        861*/
            pl(1) = pm*sx*cos(fi)                                               !/*pr        862*/
            pl(2) = pm*sx*sin(fi)                                               !/*pr        863*/
            pl(3) = pm*cx                                                       !/*pr        864*/
            call rotor (p12, v, pl, ps)                                         !/*pr        865*/
            call kinema (ps, v, pl, cot, st, cf, sf, tl, ep3)                   !/*pr        866*/
            spt(1,k) = st                                                       !/*pr        867*/
            spt(2,k) = cot                                                      !/*pr        868*/
            spt(3,k) = tl                                                       !/*pr        869*/
         else                                                                   !/*pr        870*/
            call isangl                                                         !/*pr        871*/
            ps(1) = pm*angl(4)*angl(3)                                          !/*pr        872*/
            ps(2) = pm*angl(4)*angl(2)                                          !/*pr        873*/
            ps(3) = pm*angl(1)                                                  !/*pr        874*/
            if (v2 .le. dp1m7) then                                             !/*pr        875*/
               spt(1,k) = angl(4)                                               !/*pr        876*/
               spt(2,k) = angl(1)                                               !/*pr        877*/
               spt(3,k) = ep1                                                   !/*pr        878*/
               cot = angl(1)                                                    !/*pr        879*/
               st = angl(4)                                                     !/*pr        880*/
               cf = angl(3)                                                     !/*pr        881*/
               sf = angl(2)                                                     !/*pr        882*/
               pl(1) = ps(1)                                                    !/*pr        883*/
               pl(2) = ps(2)                                                    !/*pr        884*/
               pl(3) = ps(3)                                                    !/*pr        885*/
            else                                                                !/*pr        886*/
               call kinema (ps, v, pl, cot, st, cf, sf, tl, ep3)                !/*pr        887*/
               spt(1,k) = st                                                    !/*pr        888*/
               spt(2,k) = cot                                                   !/*pr        889*/
               spt(3,k) = tl                                                    !/*pr        890*/
            endif                                                               !/*pr        891*/
         endif                                                                  !/*pr        892*/
         pnx = pnx - pl(1)/dp1000                                               !/*pr        893*/
         pny = pny - pl(2)/dp1000                                               !/*pr        894*/
         pnz = pnz - pl(3)/dp1000                                               !/*pr        895*/
         remn = a*emnucb + emx/dp1000                                           !/*pr        896*/
         e = sqrt(pnx**2 + pny**2 + pnz**2 + remn**2)                           !/*pr        897*/
         tres = (e - remn)*dp1000                                               !/*pr        898*/
         v(1) = pnx/e                                                           !/*pr        899*/
         v(2) = pny/e                                                           !/*pr        900*/
         v(3) = pnz/e                                                           !/*pr        901*/
         spt(5,k) = spt(5,k)/dp1000                                             !/*pr        902*/
         spt(3,k) = spt(3,k)/dp1000                                             !/*pr        903*/
         parz(3,k) = atan2 (st, cot)                                            !/*pr        904*/
         fi1 = atan2 (sf, cf)                                                   !/*pr        905*/
         if (fi1.lt.zro) fi1 = pi - fi1                                         !/*pr        906*/
         parz(4,k) = fi1                                                        !/*pr        907*/
         parz(2,k) = spt(3,k)                                                   !/*pr        908*/
         almax = dp2193m4*radncl*(a**thrd +  aj(lm)**thrd)*                     !/*pr        909*/
     &             sqrt(a*aj(lm)*(ep1 - vj(lm))/(a + aj(lm)))                   !/*pr        910*/
         alp = almax*sqrt(rang())                                               !/*pr        911*/
         uglmom(1) = uglmom(1) - alp*st*cf                                      !/*pr        912*/
         uglmom(2) = uglmom(2) - alp*st*sf                                      !/*pr        913*/
         uglmom(3) = uglmom(3) - alp*cot                                        !/*pr        914*/
  180 continue                                                                  !/*pr        915*/
      write (io,1200)  u, a, z                                                  !/*pr        916*/
      write (itty,1200)  u, a, z                                                !/*pr        917*/
 1200 format (/5x,'In PRECOF, number of emitted particles exceeds ',            !/*pr        918*/
     &       '150 after evaporation;'                                           !/*pr        919*/
     &        /5x,'U = ',f11.5,', A = ',f5.1,', & Z = ',f4.1)                   !/*pr        920*/
#endif                                                                          /*!pr        921*/
      return                                                                    !/*pr        922*/
      end                                                                       !/*pr        923*/
