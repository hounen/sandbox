      subroutine ccascad (enext, atwght, charge, pnucl, amnucl, kstart,         !/*cc          1*/
     &                   obr, nel)                                              !/*cc          2*/
#ifdef CEM                                                                      /*!cc          3*/
! ======================================================================        !/*cc          4*/
!                                                                               !/*cc          5*/
!   Drives the main cascade calculation.                                        !/*cc          6*/
!                                                                               !/*cc          7*/
!   Called by: CEM97a                                                           !/*cc          8*/
!                                                                               !/*cc          9*/
!   Calls: PAULIP PINPN POINTE POTEN TYPINT WIM WOPT                            !/*cc         10*/
!                                                                               !/*cc         11*/
!   CEM95 written by S. G. Mashnik                                              !/*cc         12*/
!                                                                               !/*cc         13*/
!   Edited by A. J. Sierk,  LANL  T-2  February, 1996.                          !/*cc         14*/
!   Edited by AJS,  August, 1997.                                               !/*cc         15*/
!   Edited by AJS,  December, 1997.                                             !/*cc         16*/
!   Changed to double by F. Gallmeier, ORNL CPED, October 1998.                 !/*cc         17*/
!                                                                               !/*cc         18*/
! ======================================================================        !/*cc         19*/
!                                                                               !/*cc         20*/
!  Definition of partin:                                                        !/*cc         21*/
!                       partin(1); Normalized x coordinate of particle          !/*cc         22*/
!                       partin(2); Normalized y coordinate of particle          !/*cc         23*/
!                       partin(3); Normalized z coordinate of particle          !/*cc         24*/
!                       partin(4); sin(theta), direction of momentum            !/*cc         25*/
!                       partin(5); cos(theta), direction of momentum            !/*cc         26*/
!                       partin(6); sin(phi), direction of momentum              !/*cc         27*/
!                       partin(7); cos(phi), direction of momentum              !/*cc         28*/
!                       partin(8); kinetic energy of particle                   !/*cc         29*/
!                       partin(9); rest mass of particle                        !/*cc         30*/
!                                                                               !/*cc         31*/
!  Definition of ipatin:                                                        !/*cc         32*/
!                       ipatin(1); charge of particle                           !/*cc         33*/
!                       ipatin(2); (Always 0 in CEM97a!)                        !/*cc         34*/
!                       ipatin(3); strangeness of particle                      !/*cc         35*/
!                       ipatin(4); particle baryon number                       !/*cc         36*/
!                       ipatin(5); zone number of nucleus where particle        !/*cc         37*/
!                                  is located.                                  !/*cc         38*/
!                                                                               !/*cc         39*/
!  Definition of spt:                                                           !/*cc         40*/
!                       spt(1,k) = sin(theta.k)                                 !/*cc         41*/
!                       spt(2,k) = cos(theta.k)                                 !/*cc         42*/
!                       spt(3,k) = kinetic energy of particle k (GeV)           !/*cc         43*/
!                       spt(4,k) = charge of particle k                         !/*cc         44*/
!                       spt(5,k) = rest mass of particle k                      !/*cc         45*/
!                                                                               !/*cc         46*/
!  Definition of parz:                                                          !/*cc         47*/
!                       parz(1,k) = particle type #; 1-9 for n-pi+              !/*cc         48*/
!                       parz(2,k) = kinetic energy of particle k (GeV)          !/*cc         49*/
!                       parz(3,k) = theta of particle k                         !/*cc         50*/
!                       parz(4,k) = phi of particle k                           !/*cc         51*/
!                       parz(5,k) = index: <100 for cascade particle,           !/*cc         52*/
!  negative for hole;   = 100 for preq., or = 1000 for thermal.                 !/*cc         53*/
!                       parz(6,k) = electric charge of particle k               !/*cc         54*/
!                                                                               !/*cc         55*/
! ======================================================================        !/*cc         56*/
#include <cem/cdouble.h>                                                        /*!cc         57*/
#include <lcs/inout.h>                                                          /*!cc         58*/
      dimension partin(9), ipatin(5), partne(9), ipatne(5),                     !/*cc         59*/
     &          v(3), am0(3), pnucl(3), amnucl(3), pmemo(9,150),                !/*cc         60*/
     &          imemo(5,150)                                                    !/*cc         61*/
      common /ajsbar/ ainit, zinit, eb(20,50,90), egs(20,50,90)                 !/*cc         62*/
      common /begin/  cm0, t0gev, me0, mb0, ms0, l0                             !/*cc         63*/
      common /blok77/ spt(5,150)                                                !/*cc         64*/
      common /exiton/ nexip, nexin, nhol                                        !/*cc         65*/
      common /gener/  ngen(150), ing, igs(150), meso                            !/*cc         66*/
      common /indexx/  indi                                                     !/*cc         67*/
      common /memory/ pmemo, imemo                                              !/*cc         68*/
      common /mnuc/   emnucm, emnucg, emnucb, emneut, emprot                    !/*cc         69*/
      common /mpi/    empich, empi0                                             !/*cc         70*/
      common /nuctyp/ aa, zz                                                    !/*cc         71*/
      common /o/      anucl, znucl, eps, vpi, rsm(10), rbig(10),                !/*cc         72*/
     &                rhop(10), rhon(10), af(10), tfp(10), tfn(10), n           !/*cc         73*/
      common /pii/    pi                                                        !/*cc         74*/
      common /repcnt/ irpt, irep, irec, itk1, itk3                              !/*cc         75*/
      common /zapp/   parz(6,150)                                               !/*cc         76*/
! ======================================================================        !/*cc         77*/
    5 mv = 0                                                                    !/*cc         78*/
      irefl = 0                                                                 !/*cc         79*/
      nel = 0                                                                   !/*cc         80*/
      indi = 1                                                                  !/*cc         81*/
      meso = 0                                                                  !/*cc         82*/
      ing = 0                                                                   !/*cc         83*/
      nexip = 0                                                                 !/*cc         84*/
      nexin = 0                                                                 !/*cc         85*/
      nhol = 0                                                                  !/*cc         86*/
      nabs = 0                                                                  !/*cc         87*/
!                                                                               !/*cc         88*/
!  Determine random entry point of projectile into nucleus                      !/*cc         89*/
      call pinpn (partin, ipatin, nout, am0, p0, t3)                            !/*cc         90*/
!  Projectile baryon no:                                                        !/*cc         91*/
      temp1 = mb0                                                               !/*cc         92*/
!  l0 is hardwired to be 0 in INITIAL.                                          !/*cc         93*/
      temp2 = l0                                                                !/*cc         94*/
!  Projectile charge:                                                           !/*cc         95*/
      temp3 = me0                                                               !/*cc         96*/
      if (temp1.eq.dp0) then                                                    !/*cc         97*/
         empi = empich*abs(ipatin(1)) + empi0*(dp1 - abs(ipatin(1)))            !/*cc         98*/
      else                                                                      !/*cc         99*/
         empi = dp0                                                             !/*cc        100*/
      endif                                                                     !/*cc        101*/
      enext = t3 + temp1*eps + (dp1 - temp1)*(empi + vpi)                       !/*cc        102*/
      atwght = anucl + temp1                                                    !/*cc        103*/
      charge = znucl + temp3                                                    !/*cc        104*/
      aa = atwght                                                               !/*cc        105*/
      zz = charge                                                               !/*cc        106*/
      ainit = atwght                                                            !/*cc        107*/
      zinit = charge                                                            !/*cc        108*/
      pnucl(1) = dp0                                                            !/*cc        109*/
      pnucl(2) = dp0                                                            !/*cc        110*/
      pnucl(3) = p0                                                             !/*cc        111*/
      kstart = 1                                                                !/*cc        112*/
! (Negative of ???) angular momentum of nucleus + projectile system:            !/*cc        113*/
      amnucl(1) = am0(1)                                                        !/*cc        114*/
      amnucl(2) = am0(2)                                                        !/*cc        115*/
      amnucl(3) = am0(3)                                                        !/*cc        116*/
!  Start of main loop over cascade particles:                                   !/*cc        117*/
!  Cascade particle Baryon no:                                                  !/*cc        118*/
   10 temp1 = ipatin(4)                                                         !/*cc        119*/
!  Cascade particle Charge:                                                     !/*cc        120*/
      temp2 = ipatin(1)                                                         !/*cc        121*/
!  Nuclear zone no:                                                             !/*cc        122*/
      itemp = ipatin(5)                                                         !/*cc        123*/
      pote = poten (itemp, n, ipatin, tfp, tfn, af, vpi, eps)                   !/*cc        124*/
      cutof1 = temp1*(pote + temp2*obr) + dp1m3                                 !/*cc        125*/
      if (partin(8).gt.cutof1) then                                             !/*cc        126*/
!   Determine interaction point, and characteristics of partner.                !/*cc        127*/
        call pointe (partin, ipatin, partne, ipatne, nabs, nout, irefl,         !/*cc        128*/
     &               v, u, tin1, sigp, sign, sigabs, t3)                        !/*cc        129*/
!   If pion kinetic energy is too small, go back, discard and repeat            !/*cc        130*/
!   this cascade (Perhaps should absorb pion instead.  AJS 11/20/97).           !/*cc        131*/
!       if (partin(8).le.dp1m5 .and. ipatin(4).eq.0) then                       !/*cc        132*/
!         irpt = irpt + 1                                                       !/*cc        133*/
!         go to 5                                                               !/*cc        134*/
!       endif                                                                   !/*cc        135*/
        if (nabs.gt.0) then                                                     !/*cc        136*/
!   If nucleon stays in POINTE for many iterations, assume it is                !/*cc        137*/
!   absorbed.                                                                   !/*cc        138*/
          nexip = nexip + ipatin(1)                                             !/*cc        139*/
          nexin = nexin + 1 - ipatin(1)                                         !/*cc        140*/
        else                                                                    !/*cc        141*/
          if  (nout.le.0) then                                                  !/*cc        142*/
            if (ipatin(4).eq.0) then                                            !/*cc        143*/
!   Meson:                                                                      !/*cc        144*/
              ipa = 1                                                           !/*cc        145*/
            else                                                                !/*cc        146*/
!   Baryon:                                                                     !/*cc        147*/
              cutof3 = pote + dp51m3                                            !/*cc        148*/
              if (partin(8).le.cutof3) then                                     !/*cc        149*/
!   If kinetic energy of particle inside the nucleus < 51 MeV + V;              !/*cc        150*/
!   average imaginary potential over Fermi distribution of nucleons             !/*cc        151*/
!   inside nucleus to determine changeover to preequilibrium decay.             !/*cc        152*/
                wm = dp0                                                        !/*cc        153*/
                  do 20 i = 1,50                                                !/*cc        154*/
                  w1 = wim (1, partin, ipatin)                                  !/*cc        155*/
                  w2 = wim (0, partin, ipatin)                                  !/*cc        156*/
                  wm = wm + w1 + w2                                             !/*cc        157*/
   20             continue                                                      !/*cc        158*/
!  0.01*wm is approximate "cascade" imaginary optical potential.                !/*cc        159*/
                r2 = partin(1)**2 + partin(2)**2 + partin(3)**2                 !/*cc        160*/
                r = rsm(n)*sqrt(r2)                                             !/*cc        161*/
!   Asymptotic kinetic energy for particle when outside nucleus:                !/*cc        162*/
                ts = partin(8) - pote                                           !/*cc        163*/
                is1 = ipatin(1)                                                 !/*cc        164*/
                rtest = rsm(n) - r - dp1m3                                      !/*cc        165*/
                if (rtest.lt.dp0) then                                          !/*cc        166*/
!   r >~ rsm(n):                                                                !/*cc        167*/
                  ipa = 1                                                       !/*cc        168*/
                else                                                            !/*cc        169*/
!   w3 is "scattering phenomenology" imaginary optical potential.               !/*cc        170*/
                  w3 = wopt (anucl, znucl, ts, r, is1)                          !/*cc        171*/
                  w4 = dp1m2*wm - w3                                            !/*cc        172*/
                  w5 = abs(w4/w3)                                               !/*cc        173*/
                  if (w5.ge.dp3m1) then                                         !/*cc        174*/
                    ipa = 0                                                     !/*cc        175*/
                  else                                                          !/*cc        176*/
                    ipa = 1                                                     !/*cc        177*/
                  endif                                                         !/*cc        178*/
                endif                                                           !/*cc        179*/
              else                                                              !/*cc        180*/
!   Kinetic energy > cutof3:                                                    !/*cc        181*/
                ipa = 1                                                         !/*cc        182*/
              endif                                                             !/*cc        183*/
            endif                                                               !/*cc        184*/
!   Calculate the type and results of the interaction:                          !/*cc        185*/
            call typint(partin, ipatin, partne, ipatne, v, u, tin1,             !/*cc        186*/
     &                  sigp, sign, sigabs, mv, np, charge, atwght)             !/*cc        187*/
            if (np.le.0) then                                                   !/*cc        188*/
!   Particle is absorbed into nucleus; add an exciton:                          !/*cc        189*/
              nexip = nexip + ipatin(1)                                         !/*cc        190*/
              nexin = nexin + 1 - ipatin(1)                                     !/*cc        191*/
              go to 30                                                          !/*cc        192*/
            else                                                                !/*cc        193*/
!   Determine if scattering is allowed by exclusion princlple:                  !/*cc        194*/
              call paulip (partin, ipatin, v, tfp, tfn, mv, np, irefl,          !/*cc        195*/
     &                     ipa)                                                 !/*cc        196*/
              if (ipa.eq.2) then                                                !/*cc        197*/
!   Scattering allowed; add an exciton:                                         !/*cc        198*/
                nexip = nexip + ipatin(1)                                       !/*cc        199*/
                nexin = nexin + 1 - ipatin(1)                                   !/*cc        200*/
                go to 30                                                        !/*cc        201*/
              else                                                              !/*cc        202*/
                go to 10                                                        !/*cc        203*/
              endif                                                             !/*cc        204*/
            endif                                                               !/*cc        205*/
          endif                                                                 !/*cc        206*/
          temp1 = ipatin(4)                                                     !/*cc        207*/
          itemp = ipatin(5)                                                     !/*cc        208*/
          if (ipatin(1).le.0) then                                              !/*cc        209*/
            temp2 = dp0                                                         !/*cc        210*/
          else                                                                  !/*cc        211*/
            temp2 = ipatin(1)                                                   !/*cc        212*/
          endif                                                                 !/*cc        213*/
          cutof2 = poten (itemp, n, ipatin, tfp, tfn, af, vpi, eps) +           !/*cc        214*/
     &             (temp2*obr) + dp1m3                                          !/*cc        215*/
          if (partin(8).le.cutof2) then                                         !/*cc        216*/
!   If energy is below cutof2; absorb particle; create exciton:                 !/*cc        217*/
            nexip = nexip + ipatin(1)                                           !/*cc        218*/
            nexin = nexin + 1 - ipatin(1)                                       !/*cc        219*/
          else                                                                  !/*cc        220*/
            if (temp1.eq.dp0) then                                              !/*cc        221*/
              temp5 = ipatin(1)                                                 !/*cc        222*/
              empi = empich*abs(temp5) + empi0*(dp1 - abs(temp5))               !/*cc        223*/
            else                                                                !/*cc        224*/
              empi = dp0                                                        !/*cc        225*/
            endif                                                               !/*cc        226*/
            enext = enext - (partin(8) + temp1*eps +                            !/*cc        227*/
     &             (dp1 - temp1)*(empi + vpi))                                  !/*cc        228*/
            temp2 = ipatin(1)                                                   !/*cc        229*/
            atwght = atwght - temp1                                             !/*cc        230*/
            charge = charge - temp2                                             !/*cc        231*/
            aa = atwght                                                         !/*cc        232*/
            zz = charge                                                         !/*cc        233*/
!   |p| = m*gamma*v                                                             !/*cc        234*/
            temp3 = sqrt(partin(8)*(partin(8) + dp2*partin(9)))                 !/*cc        235*/
!   p-x/|p|                                                                     !/*cc        236*/
            temp4 = partin(4)*partin(7)                                         !/*cc        237*/
!   p-y/|p|                                                                     !/*cc        238*/
            temp5 = partin(4)*partin(6)                                         !/*cc        239*/
            pnucl(1) = pnucl(1) - temp3*temp4                                   !/*cc        240*/
            pnucl(2) = pnucl(2) - temp3*temp5                                   !/*cc        241*/
            pnucl(3) = pnucl(3) - temp3*partin(5)                               !/*cc        242*/
!  L-x:     amnucl(1) = amnucl(1) - p * (z*(py/p) - y*(pz/p))                   !/*cc        243*/
            amnucl(1) = amnucl(1) - temp3*(partin(3)*temp5 -                    !/*cc        244*/
     &                  partin(2)*partin(5))                                    !/*cc        245*/
            amnucl(2) = amnucl(2) - temp3*(partin(1)*partin(5) -                !/*cc        246*/
     &                  partin(3)*temp4)                                        !/*cc        247*/
            amnucl(3) = amnucl(3) - temp3*(partin(2)*temp4 -                    !/*cc        248*/
     &                  partin(1)*temp5)                                        !/*cc        249*/
            if (enext .le. dp1m4) then                                          !/*cc        250*/
!  E* <= 100 keV and Z & A unchanged; call it elastic scattering.               !/*cc        251*/
               if (atwght.eq.anucl .and. charge.eq.znucl) then                  !/*cc        252*/
                  nel = 1                                                       !/*cc        253*/
                  return                                                        !/*cc        254*/
               endif                                                            !/*cc        255*/
            endif                                                               !/*cc        256*/
            if (kstart.gt.150) then                                             !/*cc        257*/
               write (io, 1000)                                                 !/*cc        258*/
               write (itty, 1000)                                               !/*cc        259*/
               return                                                           !/*cc        260*/
            endif                                                               !/*cc        261*/
            spt(1,kstart) = partin(4)                                           !/*cc        262*/
            spt(2,kstart) = partin(5)                                           !/*cc        263*/
            spt(3,kstart) = partin(8)                                           !/*cc        264*/
            spt(5,kstart) = partin(9)                                           !/*cc        265*/
!  temp2 = charge of emitted particle.                                          !/*cc        266*/
            spt(4,kstart) = temp2                                               !/*cc        267*/
            parz(6,kstart) = temp2                                              !/*cc        268*/
            parz(5,kstart) = ing                                                !/*cc        269*/
            if (partin(9).le.dph) then                                          !/*cc        270*/
!   Pion:                                                                       !/*cc        271*/
              if (temp2.lt.dp0) then                                            !/*cc        272*/
!   pi-                                                                         !/*cc        273*/
                parz(1,kstart) = 7.0d0                                          !/*cc        274*/
              elseif (temp2.eq.dp0) then                                        !/*cc        275*/
!   pi0                                                                         !/*cc        276*/
                parz(1,kstart) = 8.0d0                                          !/*cc        277*/
              else                                                              !/*cc        278*/
!   pi+                                                                         !/*cc        279*/
                parz(1,kstart) = 9.0d0                                          !/*cc        280*/
              endif                                                             !/*cc        281*/
            else                                                                !/*cc        282*/
!     Nucleon:                                                                  !/*cc        283*/
              if (temp2.le.dp0) then                                            !/*cc        284*/
!     neutron:                                                                  !/*cc        285*/
                parz(1,kstart) = dp1                                            !/*cc        286*/
              else                                                              !/*cc        287*/
!     proton:                                                                   !/*cc        288*/
                parz(1,kstart) = dp2                                            !/*cc        289*/
              endif                                                             !/*cc        290*/
              if (indi.eq.3) parz(5,kstart) = -parz(5,kstart)                   !/*cc        291*/
            endif                                                               !/*cc        292*/
            parz(3,kstart) = atan2(partin(4), partin(5))                        !/*cc        293*/
            fi1 = atan2(partin(6),partin(7))                                    !/*cc        294*/
            if (fi1.lt.dp0) fi1 = pi - fi1                                      !/*cc        295*/
            parz(4,kstart) = fi1                                                !/*cc        296*/
            parz(2,kstart) = partin(8)                                          !/*cc        297*/
            igs(kstart) = ing                                                   !/*cc        298*/
            kstart = kstart + 1                                                 !/*cc        299*/
          endif                                                                 !/*cc        300*/
        endif                                                                   !/*cc        301*/
      else                                                                      !/*cc        302*/
!   Kinetic energy is below cutof1; add a particle exciton:                     !/*cc        303*/
        nexip = nexip + ipatin(1)                                               !/*cc        304*/
        nexin = nexin - ipatin(1) + 1                                           !/*cc        305*/
      endif                                                                     !/*cc        306*/
   30 if (mv.gt.0) then                                                         !/*cc        307*/
          do 40 i = 1,9                                                         !/*cc        308*/
          partin(i) = pmemo(i,mv)                                               !/*cc        309*/
   40     continue                                                              !/*cc        310*/
          do 50 i = 1,5                                                         !/*cc        311*/
          ipatin(i) = imemo(i,mv)                                               !/*cc        312*/
   50     continue                                                              !/*cc        313*/
        ing = ngen(mv)                                                          !/*cc        314*/
        mv = mv - 1                                                             !/*cc        315*/
        nabs = 0                                                                !/*cc        316*/
        nout = 0                                                                !/*cc        317*/
        irefl = 0                                                               !/*cc        318*/
        go to 10                                                                !/*cc        319*/
      else                                                                      !/*cc        320*/
!   Convert angular momentum to units of h-bar.  Constant =                     !/*cc        321*/
!   1000./(h-bar*c = 197.32858)                                                 !/*cc        322*/
        temp1 = dp5067m3*rsm(n)                                                 !/*cc        323*/
        amnucl(1) = temp1*amnucl(1)                                             !/*cc        324*/
        amnucl(2) = temp1*amnucl(2)                                             !/*cc        325*/
        amnucl(3) = temp1*amnucl(3)                                             !/*cc        326*/
      endif                                                                     !/*cc        327*/
! ======================================================================        !/*cc        328*/
 1000 format  (/5x,'More than 150 particles in spt after CASCAD.')              !/*cc        329*/
! ======================================================================        !/*cc        330*/
#endif                                                                          /*!cc        331*/
      return                                                                    !/*cc        332*/
      end                                                                       !/*cc        333*/
