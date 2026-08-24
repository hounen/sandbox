      subroutine help (a, r0n, rmax, bn)                                        !/*he          1*/
#ifdef CEM                                                                      /*!he          2*/
! ======================================================================        !/*he          3*/
!                                                                               !/*he          4*/
!     Neutron density, proton density, nucleon density .                        !/*he          5*/
!     Same functional form, different normalizations.                           !/*he          6*/
!     Determines zone boundaries in nucleus.                                    !/*he          7*/
!     Finds average: Fermi momentum, Coulomb potential, nucleon density         !/*he          8*/
!     in each zone of nucleus.                                                  !/*he          9*/
!                                                                               !/*he         10*/
!     Rsm(ia) is the value of the radius at which the Woods-Saxon               !/*he         11*/
!     nucleon density reaches the value of a(ia) * (the central value).         !/*he         12*/
!     Hin is the integral from rsm(ia-1) (or 0) to rsm(ia) of                   !/*he         13*/
!     the density * r**2.                                                       !/*he         14*/
!     Sumhin is the integral of r**2/(1 + exp((r-rn)/bn) from 0 to              !/*he         15*/
!     rsm(n)  [1/(4*pi) * (unnormalized) total mass inside of rsm(n)].          !/*he         16*/
!     Rhon0 is the density normalization factor so all nucleons are             !/*he         17*/
!     inside rsm(n).                                                            !/*he         18*/
!     By an integration by parts, the Coulomb potential at r is                 !/*he         19*/
!     proportional to:                                                          !/*he         20*/
!     1/r *Int[0,r]{r'**2*rhop(r')}dr' + Int[r,rsm(n)]{r'*rhop(r')}dr' .        !/*he         21*/
!     Fis is proportional to this quantity * r**2.                              !/*he         22*/
!     Rhon is the average nucleon density between rsm(ia-1) and rsm(ia);        !/*he         23*/
!     af is the average Coulomb potential (for unit charge) in zone ia.         !/*he         24*/
!     Tfp and tfn are the average Fermi momenta for protons and neutrons        !/*he         25*/
!     in the nuclear zone ia.                                                   !/*he         26*/
!     Rbig is the normalized [to rsm(n)] radius of the outer boundary of        !/*he         27*/
!     each zone.                                                                !/*he         28*/
!                                                                               !/*he         29*/
!     CEM95 written by S. G. Mashnik                                            !/*he         30*/
!                                                                               !/*he         31*/
!     Edited by A. J. Sierk   LANL  T-2,  February-March, 1996.                 !/*he         32*/
!     Changed to double by F. Gallmeier, ORNL CPED, October 1998.               !/*he         33*/
!                                                                               !/*he         34*/
! ======================================================================        !/*he         35*/
#include <cem/cdouble.h>                                                        /*!he         36*/
      external s2, fis                                                          !/*he         37*/
      dimension a(10), hin(10), fi(10)                                          !/*he         38*/
      common /constan/ thrd, twthrd, fvthrd                                     !/*he         39*/
      common /o/       anucl, znucl, eps, vpi, rsm(10), rbig(10),               !/*he         40*/
     &                 rhop(10), rhon(10), af(10), tfp(10), tfn(10), n          !/*he         41*/
      common /pii/     pi                                                       !/*he         42*/
      common /runopt/  iopt                                                     !/*he         43*/
! ======================================================================        !/*he         44*/
!     if (iopt.eq.1) then                                                       !/*he         45*/
!       pi0 = pi                                                                !/*he         46*/
!       pi = 3.14159                                                            !/*he         47*/
!     endif                                                                     !/*he         48*/
      frpi = dp4*pi                                                             !/*he         49*/
      esq = 0.001439976d0                                                       !/*he         50*/
!     if (iopt.eq.1) esq = 0.00144d0                                            !/*he         51*/
      rn = r0n*anucl**thrd                                                      !/*he         52*/
      ennuc = anucl - znucl                                                     !/*he         53*/
      pden = znucl/anucl                                                        !/*he         54*/
      enden = ennuc/anucl                                                       !/*he         55*/
        do 10 ia = 1,n                                                          !/*he         56*/
        rsm(ia) = rn + bn*(log((dp1 - a(ia))/a(ia)))                            !/*he         57*/
   10   continue                                                                !/*he         58*/
      rsm(n+1) = rmax                                                           !/*he         59*/
        do 20 ia = 1,n                                                          !/*he         60*/
        if (ia.eq.1) then                                                       !/*he         61*/
          fi(ia) = cfint (fis, rsm(ia), dp0, bn, rn)                            !/*he         62*/
          hin(ia) = cfint (s2, rsm(ia), dp0, bn, rn)                            !/*he         63*/
        else                                                                    !/*he         64*/
          fi(ia) = cfint (fis, rsm(ia), rsm(ia-1), bn, rn)                      !/*he         65*/
          hin(ia) = cfint (s2, rsm(ia), rsm(ia-1), bn, rn)                      !/*he         66*/
        endif                                                                   !/*he         67*/
   20   continue                                                                !/*he         68*/
      sumhin = dp0                                                              !/*he         69*/
        do 30 ia = 1,n                                                          !/*he         70*/
        sumhin = sumhin + hin(ia)                                               !/*he         71*/
   30   continue                                                                !/*he         72*/
      rhon0 = dp3*anucl/(frpi*sumhin)                                           !/*he         73*/
      fi0 = dp3*esq*znucl/sumhin                                                !/*he         74*/
        do 40 ia = 1,n                                                          !/*he         75*/
        if (ia.eq.1) then                                                       !/*he         76*/
          fac = dp1/rsm(ia)**3                                                  !/*he         77*/
        else                                                                    !/*he         78*/
          fac = dp1/(rsm(ia)**3 - rsm(ia-1)**3)                                 !/*he         79*/
        endif                                                                   !/*he         80*/
        rhon(ia) = hin(ia)*rhon0*fac                                            !/*he         81*/
        af(ia) = fi0*fi(ia)*fac                                                 !/*he         82*/
!       tfp(ia) = 0.198595d0*(rhon(ia)*pden)**twthrd                            !/*he         83*/
!       tfn(ia) = 0.198322d0*(rhon(ia)*enden)**twthrd                           !/*he         84*/
        tfp(ia) = dp1985m4*(rhon(ia)*pden)**twthrd                              !/*he         85*/
        tfn(ia) = dp1985m4*(rhon(ia)*enden)**twthrd                             !/*he         86*/
        rbig(ia) = rsm(ia)/rsm(n)                                               !/*he         87*/
   40   continue                                                                !/*he         88*/
      rbig(n+1) = rmax/rsm(n)                                                   !/*he         89*/
      af(n+1) = znucl*esq/rmax                                                  !/*he         90*/
      tfp(n+1) = dp0                                                            !/*he         91*/
      tfn(n+1) = dp0                                                            !/*he         92*/
      rhon(n+1) = dp0                                                           !/*he         93*/
      rhop(n+1) = dp0                                                           !/*he         94*/
      k = n + 2                                                                 !/*he         95*/
        do 50 ia = k,10                                                         !/*he         96*/
        rsm(ia) = dp0                                                           !/*he         97*/
        rbig(ia) = dp0                                                          !/*he         98*/
        rhon(ia) = dp0                                                          !/*he         99*/
        rhop(ia) = dp0                                                          !/*he        100*/
        af(ia) = dp0                                                            !/*he        101*/
        tfp(ia) = dp0                                                           !/*he        102*/
        tfn(ia) = dp0                                                           !/*he        103*/
   50   continue                                                                !/*he        104*/
        do 60 ia = 1,n                                                          !/*he        105*/
        rhop(ia) = rhon(ia)*pden                                                !/*he        106*/
        rhon(ia) = rhon(ia)*enden                                               !/*he        107*/
   60   continue                                                                !/*he        108*/
!     if (iopt.eq.1) pi = pi0                                                   !/*he        109*/
#endif                                                                          /*!he        110*/
      return                                                                    !/*he        111*/
      end                                                                       !/*he        112*/
