      subroutine storep(parz, spt, anucl, znucl, at, zt, me0, mb0)              !/*st          1*/
#ifdef CEM                                                                      /*!st          2*/
! ======================================================================        !/*st          3*/
!                                                                               !/*st          4*/
!   Stores the secondaries particle information produced by CEM in              !/*st          5*/
!   proper particle arrays to be processed by the subroutines                   !/*st          6*/
!   update/updat1 of MCNPX                                                      !/*st          7*/
!                                                                               !/*st          8*/
!   CEM95 written by S. G. Mashnik                                              !/*st          9*/
!                                                                               !/*st         10*/
!   Written by F. Gallmeier, ORNL CPED, June 1998                               !/*st         11*/
!   Modified by R. E. Prael, LANL XCI, April 1999                               !/*st         12*/
!                                                                               !/*st         13*/
! ======================================================================        !/*st         14*/
!                                                                               !/*st         15*/
!  Definition of spt:                                                           !/*st         16*/
!                     spt(1,k) = sin(theta.k)                                   !/*st         17*/
!                     spt(2,k) = cos(theta.k)                                   !/*st         18*/
!                     spt(3,k) = kinetic energy of particle k (GeV)             !/*st         19*/
!                     spt(4,k) = charge of particle k                           !/*st         20*/
!                     spt(5,k) = rest mass of particle k                        !/*st         21*/
!  Definition of parz:                                                          !/*st         22*/
!                     parz(1,k) = particle type #; 1-9 for n-pi+                !/*st         23*/
!                     parz(2,k) = kinetic energy of particle k (GeV)            !/*st         24*/
!                     parz(3,k) = theta of particle k                           !/*st         25*/
!                     parz(4,k) = phi of particle k                             !/*st         26*/
!                     parz(5,k) = index: <100 for cascade,                      !/*st         27*/
!                     negative for hole;                                        !/*st         28*/
!                     = 100 for preq., or = 1000 for thermal.                   !/*st         29*/
!                     parz(6,k) = electric charge of particle k                 !/*st         30*/
!                                                                               !/*st         31*/
!  Definition of LCS cascade and prequillibrium particle storage:               !/*st         32*/
!                      nopart = number of secondary particles                   !/*st         33*/
!                      kind   = particle type (cascade)                         !/*st         34*/
!                             = particle type + 200 (prequillibrium)            !/*st         35*/
!                               transform parz(1)                               !/*st         36*/
!                               (1-9)=>(n/p/d/t/he3/he4/pi-/pi0/pi+)            !/*st         37*/
!                      ep     = particle energy (1000. * parz(2))               !/*st         38*/
!                      alpha  = cos(parz(4))*sin(parz(3))                       !/*st         39*/
!                               assume theta is angle                           !/*st         40*/
!                               of direction with z-axis                        !/*st         41*/
!                      beta   = sin(parz(4))*sin(parz(3))                       !/*st         42*/
!                      gam    = cos(parz(3))                                    !/*st         43*/
!                                                                               !/*st         44*/
!  Definition of LCS evaporation particle storage:                              !/*st         45*/
!                      npart(i)     = number of secondary particles             !/*st         46*/
!                                     of typ "i"                                !/*st         47*/
!                        protons & neutrons: i=1,2                              !/*st         48*/
!                      epart(i,k)   = particle energy (1000. * parz(2))         !/*st         49*/
!                      cosevp(1,i,k)= cos(parz(4))*sin(parz(3))                 !/*st         50*/
!                                     assume theta is angle                     !/*st         51*/
!                                     of direction with z-axis                  !/*st         52*/
!                      cosevp(2,i,k)= sin(parz(4))*sin(parz(3))                 !/*st         53*/
!                      cosevp(3,i,k)= cos(parz(3))                              !/*st         54*/
!                        d,t,he3,he4: j=i-2; j=1-4                              !/*st         55*/
!                      hepart(j,k)   = particle energy (1000. * parz(2))        !/*st         56*/
!                      coshev(1,j,k)= cos(parz(4))*sin(parz(3))                 !/*st         57*/
!                                     assume theta is angle                     !/*st         58*/
!                                     of direction with z-axis                  !/*st         59*/
!                      coshev(2,j,k)= sin(parz(4))*sin(parz(3))                 !/*st         60*/
!                      coshev(3,j,k)= cos(parz(3))                              !/*st         61*/
!                                                                               !/*st         62*/
!  Definition of PHT particle storage:                                          !/*st         63*/
!                      nph    = number of PHT photons                           !/*st         64*/
!                      eph    = energy of photons                               !/*st         65*/
!                      tph    = delaytime of photon production                  !/*st         66*/
!                                                                               !/*st         67*/
! ======================================================================        !/*st         68*/
!                                                                               !/*st         69*/
!   LCS variable definition:                                                    !/*st         70*/
#include <cem/cdouble.h>                                                        /*!st         71*/
#include <lcs/lptS.h>                                                           /*!st         72*/
#include <lcs/concom.h>                                                         /*!st         73*/
#include <lcs/rucom.h>                                                          /*!st         74*/
#include <lcs/banka.h>                                                          /*!st         75*/
#include <lcs/inout.h>                                                          /*!st         76*/
!                                                                               !/*st         77*/
!                                                                               !/*st         78*/
!   CEM data transfer:                                                          !/*st         79*/
      dimension parz(6,150), spt(5,150)                                         !/*st         80*/
!                                                                               !/*st         81*/
!   transforms CEM particle type into LCS particle type "kind"                  !/*st         82*/
      dimension kindc(9)                                                        !/*st         83*/
!                                                                               !/*st         84*/
!   counter, charge and energy for secondary particles                          !/*st         85*/
      dimension nsec(9), noz(9), noa(9)                                         !/*st         86*/
      data kindc/ 1,0,7,8,9,10,4,3,2/                                           !/*st         87*/
      data noz/0,+1,+1,+1,+2,+2,-1,0,+1/                                        !/*st         88*/
      data noa/1,1,2,3,3,4,0,0,0/                                               !/*st         89*/
      data zro, one, fmil /0.d0, 1.d0, 1.d3/                                    !/*st         90*/
! ======================================================================        !/*st         91*/
!                                                                               !/*st         92*/
!  initialize general secondary particle counters                               !/*st         93*/
      do 10 i=1,9                                                               !/*st         94*/
         nsec(i) = 0                                                            !/*st         95*/
   10 continue                                                                  !/*st         96*/
!                                                                               !/*st         97*/
!  initialize process counters                                                  !/*st         98*/
!     number of cascade particles                                               !/*st         99*/
      nopart = 0                                                                !/*st        100*/
!     counters of preequilibrium and                                            !/*st        101*/
!     evaporation particle "i"                                                  !/*st        102*/
!     "i" corresponds to CEM particle type                                      !/*st        103*/
      do 20 i=1,6                                                               !/*st        104*/
         npart(i) = 0                                                           !/*st        105*/
   20 continue                                                                  !/*st        106*/
!                                                                               !/*st        107*/
!  sort particles into MCNPX arrays and make summary                            !/*st        108*/
      do 30 m=1,150                                                             !/*st        109*/
         if (spt(5,m).lt.dp1m3) go to 100                                       !/*st        110*/
         ipar = parz(1,m) + dp1m1                                               !/*st        111*/
         ipoc = parz(5,m) + dp1m1                                               !/*st        112*/
!                                                                               !/*st        113*/
!  reaction summary                                                             !/*st        114*/
         nsec(ipar) = nsec(ipar)+1                                              !/*st        115*/
!                                                                               !/*st        116*/
!  sort particles into MCNPX arrays                                             !/*st        117*/
         if (ipoc.le.1000) then                                                 !/*st        118*/
!                                                                               !/*st        119*/
            nopart = nopart+1                                                   !/*st        120*/
!  cascade particles                                                            !/*st        121*/
            kind(nopart)  = kindc(ipar)                                         !/*st        122*/
!  prequillibrium particles                                                     !/*st        123*/
            if (ipoc.eq.100) kind(nopart)  = kind(nopart) + 200                 !/*st        124*/
!  evaporation particles                                                        !/*st        125*/
            if (ipoc.eq.1000) kind(nopart)  = kind(nopart) + 500                !/*st        126*/
!                                                                               !/*st        127*/
            ep(nopart)    = parz(2,m)*1.d3                                      !/*st        128*/
            alpha(nopart) = sin(parz(3,m)) * cos(parz(4,m))                     !/*st        129*/
            beta(nopart)  = sin(parz(3,m)) * sin(parz(4,m))                     !/*st        130*/
            gam(nopart)   = cos(parz(3,m))                                      !/*st        131*/
         else                                                                   !/*st        132*/
            call expirx(1,'STOREP','undefined particle type')                   !/*st        133*/
         endif                                                                  !/*st        134*/
   30 continue                                                                  !/*st        135*/
  100 continue                                                                  !/*st        136*/
!                                                                               !/*st        137*/
!   test some conservation laws                                                 !/*st        138*/
      zbeg = znucl + me0                                                        !/*st        139*/
      abeg = anucl + mb0                                                        !/*st        140*/
      zsum = dp0                                                                !/*st        141*/
      asum = dp0                                                                !/*st        142*/
      do 200 i=1,9                                                              !/*st        143*/
         zsum = zsum + nsec(i)*noz(i)                                           !/*st        144*/
         asum = asum + nsec(i)*noa(i)                                           !/*st        145*/
  200 continue                                                                  !/*st        146*/
      zend = zt + zsum                                                          !/*st        147*/
      aend = at + asum                                                          !/*st        148*/
      if(zbeg.ne.zend) then                                                     !/*st        149*/
         write(io,'(a)') 'storep: charge violation'                             !/*st        150*/
         write(io,'(5(a,f6.0))') ' nocas=', real(nocas),                        !/*st        151*/
     &                           ' zbeg=', zbeg, ' zend=', zend,                !/*st        152*/
     &                           ' abeg=', abeg, ' aend=', aend                 !/*st        153*/
      endif                                                                     !/*st        154*/
      if(abeg.ne.aend) then                                                     !/*st        155*/
         write(io,'(a)') 'storep: baryon violation'                             !/*st        156*/
         write(io,'(5(a,f6.0))') ' nocas=', real(nocas),                        !/*st        157*/
     &                           ' zbeg=', zbeg, ' zend=', zend,                !/*st        158*/
     &                           ' abeg=', abeg, ' aend=', aend                 !/*st        159*/
      endif                                                                     !/*st        160*/
#endif                                                                          /*!st        161*/
      return                                                                    !/*st        162*/
      end                                                                       !/*st        163*/
