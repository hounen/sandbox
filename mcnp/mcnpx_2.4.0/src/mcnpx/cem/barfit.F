      subroutine barfit (a, z, il, bfis, egs)                                   !/*ba          1*/
#ifdef CEM                                                                      /*!ba          2*/
! ======================================================================        !/*ba          3*/
!                                                                               !/*ba          4*/
!***********************************************************************        !/*ba          5*/
!          S. G. Mashnik intoduced (25.10.1992) the following changes: *        !/*ba          6*/
!******************   z=zsierk, a=asierk, iz=z, ia=a   *****************        !/*ba          7*/
!                                                                      *        !/*ba          8*/
!   Removed unnecessary real*8 variables;                              *        !/*ba          9*/
!   Other changes made to fit into CEM95; A. J. Sierk, February, 1996. *        !/*ba         10*/
!   Also edited by F. Gallmeier, ORNL CPED, October 1998.              *        !/*ba         11*/
!***********************************************************************        !/*ba         12*/
!                                                                               !/*ba         13*/
!     This subroutine returns the barrier height bfis, and the rotating         !/*ba         14*/
!     ground state energy, egs (in MeV) when called with real arguments         !/*ba         15*/
!     z, the atomic number, a, the atomic mass number, and (integer) il,        !/*ba         16*/
!     the angular momentum in units of h-bar, (Planck constant divided          !/*ba         17*/
!     by 2*pi).                                                                 !/*ba         18*/
!                                                                               !/*ba         19*/
!         The fission barrier for il = 0 is calculated from a 7th order         !/*ba         20*/
!     fit in two variables to 638 calculated fission barriers for z             !/*ba         21*/
!     values from 20 to 110.  These 638 barriers are fit with an rms            !/*ba         22*/
!     deviation of 0.10 Mev by this 49-parameter function.                      !/*ba         23*/
!     If BARFIT is called with (z,a) values outside the range of the            !/*ba         24*/
!     fit the barrier height is set to 0.0, and a message is                    !/*ba         25*/
!     printed on unit 16.                                                       !/*ba         26*/
!                                                                               !/*ba         27*/
!         For il values not equal to zero, the values of                        !/*ba         28*/
!     l at which the barrier is  80%  and  20%  of the l=0 value are            !/*ba         29*/
!     respectively fit to 20-parameter functions of  z  and  a, over a          !/*ba         30*/
!     more restricted range of  a  values, than is the case for  l = 0.         !/*ba         31*/
!     The value of l where the barrier disappears, lmax, for 81 nuclei,         !/*ba         32*/
!     is fit to a 40-parameter function of z and a.                             !/*ba         33*/
!         Once again, if a (z,a) pair is outside of the range of                !/*ba         34*/
!     validity of the fit, the barrier value is set to 0.0 and a message        !/*ba         35*/
!     is printed. These three values (bfis(l=0), l-80, and l-20) and the        !/*ba         36*/
!     constraints of bfis = 0 and  d(bfis)/dl = 0 at l = lmax and               !/*ba         37*/
!     d(bfis)/dl = 0 at l = 0 lead to a fifth-order fit to bfis(l) for          !/*ba         38*/
!     l>= l-20.  The last constraint, and the values l-80 and l-20,             !/*ba         39*/
!     and bfis(l=0) lead to a third-order fit for the region l <= l-20.         !/*ba         40*/
!                                                                               !/*ba         41*/
!         The ground-state energies are calculated from a 175-parameter         !/*ba         42*/
!     fit in Z, A, and L to 329 ground-state energies for 36 different          !/*ba         43*/
!     Z  and  A  values.                                                        !/*ba         44*/
!     (The range of Z and A is the same as for L-80, L-20, and L-max)           !/*ba         45*/
!                                                                               !/*ba         46*/
!         The calculated barriers from which the fits were                      !/*ba         47*/
!     made were calculated in 1983-1985 by A. J. Sierk of Los Alamos            !/*ba         48*/
!     National Laboratory, Group T-9, using  Yukawa-plus-exponential            !/*ba         49*/
!     double folded nuclear energy, exact Couloub diffuseness                   !/*ba         50*/
!     corrections, and diffuse-matter moments of inertia. The parameters        !/*ba         51*/
!     of the model are those derived by Moller and Nix in 1979:                 !/*ba         52*/
!     r-0 = 1.16 fm, as = 21.13 MeV, kappa-s = 2.3  a = 0.68 fm.                !/*ba         53*/
!     The diffuseness of the matter and charge distributions used               !/*ba         54*/
!     corresponds to a surface diffuseness parameter (defined by Myers)         !/*ba         55*/
!     of 0.99 fm.  The fitted barriers for l = 0 are deviate from the           !/*ba         56*/
!     calculated ones by less than 0.1 MeV;  The output from this               !/*ba         57*/
!     subroutine is a little less accurate. Worst errors may be as large        !/*ba         58*/
!     as 0.5 MeV; characteristic uncertainty is in the range of 0.1-0.2         !/*ba         59*/
!     MeV.  The values of egs are generally approximated to within              !/*ba         60*/
!     about 0.1-0.2 MeV;  the largest deviation is about 0.5 MeV,               !/*ba         61*/
!     near L-I for light nuclei.                                                !/*ba         62*/
!         The rms deviation of lmax from the 81 input values is ~0.1            !/*ba         63*/
!     h-bar.  The approximate value found by this code is nearly always         !/*ba         64*/
!     within 0.5 h-bar of the calculated one.                                   !/*ba         65*/
!                                                                               !/*ba         66*/
!     Below is a table of test values to check implementation                   !/*ba         67*/
!     of the program:                                                           !/*ba         68*/
!               z, a, il  fiss bar lmax    egs                                  !/*ba         69*/
!                                                                               !/*ba         70*/
!              28, 58, 0   33.14   45.8   0.00                                  !/*ba         71*/
!                    ,25   19.50         21.61                                  !/*ba         72*/
!                    ,40    2.88         50.12                                  !/*ba         73*/
!                    ,45.   0.07         58.18                                  !/*ba         74*/
!              65,153, 0   28.88   82.1   0.00                                  !/*ba         75*/
!                    ,50   16.16         19.07                                  !/*ba         76*/
!                    ,80    0.23         45.37                                  !/*ba         77*/
!                    ,82.   0.00         46.95                                  !/*ba         78*/
!              93,229, 0    3.76   68.1   0.00                                  !/*ba         79*/
!                    ,45    1.26          8.22                                  !/*ba         80*/
!                    ,68.   0.00         17.93                                  !/*ba         81*/
!                                                                               !/*ba         82*/
! ======================================================================        !/*ba         83*/
!                                                                               !/*ba         84*/
!     written by A. J. Sierk,  LANL  T-9                                        !/*ba         85*/
!     version 1.0   February, 1984                                              !/*ba         86*/
!     version 1.1   January, 1985: improved coefficients in egs and lmax        !/*ba         87*/
!     version 1.2   September, 1985:  improved lmax, egs coefficients           !/*ba         88*/
!     version 1.21  June, 1986:   minor changes made                            !/*ba         89*/
!     version 1.3   February, 1996: Moved elmax to subroutine, improved         !/*ba         90*/
!                  elmax coefficients.                                          !/*ba         91*/
!                                                                               !/*ba         92*/
! ======================================================================        !/*ba         93*/
!                                                                               !/*ba         94*/
!      Copyright, 1996,  the Regents of the University of California.           !/*ba         95*/
!      This software was produced under a U. S. government contract             !/*ba         96*/
!      (w-7405-eng-36) by the Los Alamos National Laboratory, which is          !/*ba         97*/
!      operated by the University of California for the U. S. Department        !/*ba         98*/
!      of Energy.  The U. S. government is licensed to use, reproduce,          !/*ba         99*/
!      and distribute this software. Permission is granted to the public        !/*ba        100*/
!      to copy and use this software without charge, provided that this         !/*ba        101*/
!      notice and any statement of authorship are reproduced on all             !/*ba        102*/
!      copies.  Neither the government nor the University makes any             !/*ba        103*/
!      warranty, expressed or implied, or assumes any liability                 !/*ba        104*/
!      or responsibility for the use of this software.                          !/*ba        105*/
!                                                                               !/*ba        106*/
! ======================================================================        !/*ba        107*/
#include <cem/cdouble.h>                                                        /*!ba        108*/
#include <lcs/inout.h>                                                          /*!ba        109*/
      dimension elzcof(7,7), elmcof(5,4), emncof(5,4), pa(7), pl(10),           !/*ba        110*/
     &          pz(7)                                                           !/*ba        111*/
      dimension egs1(5,7), egs2(5,7), egs3(5,7), egs4(5,7), egs5(5,7),          !/*ba        112*/
     &          egscof(5,7,5)                                                   !/*ba        113*/
      equivalence (egs1,egscof),(egs2,egscof(1,1,2)),                           !/*ba        114*/
     &            (egs3,egscof(1,1,3)),(egs4,egscof(1,1,4)),                    !/*ba        115*/
     &            (egs5,egscof(1,1,5))                                          !/*ba        116*/
      data emncof                                                               !/*ba        117*/
     &/-9.01100d+2,-1.40818d+3, 2.77000d+3,-7.06695d+2, 8.89867d+2,             !/*ba        118*/
     &  1.35355d+4,-2.03847d+4, 1.09384d+4,-4.86297d+3,-6.18603d+2,             !/*ba        119*/
     & -3.26367d+3, 1.62447d+3, 1.36856d+3, 1.31731d+3, 1.53372d+2,             !/*ba        120*/
     &  7.48863d+3,-1.21581d+4, 5.50281d+3,-1.33630d+3, 5.05367d-2/             !/*ba        121*/
      data elmcof                                                               !/*ba        122*/
     & /1.84542d+3,-5.64002d+3, 5.66730d+3,-3.15150d+3, 9.54160d+2,             !/*ba        123*/
     & -2.24577d+3, 8.56133d+3,-9.67348d+3, 5.81744d+3,-1.86997d+3,             !/*ba        124*/
     &  2.79772d+3,-8.73073d+3, 9.19706d+3,-4.91900d+3, 1.37283d+3,             !/*ba        125*/
     & -3.01866d+1, 1.41161d+3,-2.85919d+3, 2.13016d+3,-6.49072d+2/             !/*ba        126*/
      data elzcof                                                               !/*ba        127*/
     & /5.11819909d+5,-1.30303186d+6, 1.90119870d+6,-1.20628242d+6,             !/*ba        128*/
     &  5.68208488d+5, 5.48346483d+4,-2.45883052d+4,                            !/*ba        129*/
     & -1.13269453d+6, 2.97764590d+6,-4.54326326d+6, 3.00464870d+6,             !/*ba        130*/
     & -1.44989274d+6,-1.02026610d+5, 6.27959815d+4,                            !/*ba        131*/
     &  1.37543304d+6,-3.65808988d+6, 5.47798999d+6,-3.78109283d+6,             !/*ba        132*/
     &  1.84131765d+6, 1.53669695d+4,-6.96817834d+4,                            !/*ba        133*/
     & -8.56559835d+5, 2.48872266d+6,-4.07349128d+6, 3.12835899d+6,             !/*ba        134*/
     & -1.62394090d+6, 1.19797378d+5, 4.25737058d+4,                            !/*ba        135*/
     &  3.28723311d+5,-1.09892175d+6, 2.03997269d+6,-1.77185718d+6,             !/*ba        136*/
     &  9.96051545d+5,-1.53305699d+5,-1.12982954d+4,                            !/*ba        137*/
     &  4.15850238d+4, 7.29653408d+4,-4.93776346d+5, 6.01254680d+5,             !/*ba        138*/
     & -4.01308292d+5, 9.65968391d+4,-3.49596027d+3,                            !/*ba        139*/
     & -1.82751044d+5, 3.91386300d+5,-3.03639248d+5, 1.15782417d+5,             !/*ba        140*/
     & -4.24399280d+3,-6.11477247d+3, 3.66982647d+2/                            !/*ba        141*/
      data egs1 /                                                               !/*ba        142*/
     & -1.781665232d6,-2.849020290d6, 9.546305856d5, 2.453904278d5,             !/*ba        143*/
     &  3.656148926d5,                                                          !/*ba        144*/
     &  4.358113622d6, 6.960182192d6,-2.381941132d6,-6.262569370d5,             !/*ba        145*/
     & -9.026606463d5,                                                          !/*ba        146*/
     & -4.804291019d6,-7.666333374d6, 2.699742775d6, 7.415602390d5,             !/*ba        147*/
     &  1.006008724d6,                                                          !/*ba        148*/
     &  3.505397297d6, 5.586825123d6,-2.024820713d6,-5.818008462d5,             !/*ba        149*/
     & -7.353683218d5,                                                          !/*ba        150*/
     & -1.740990985d6,-2.759325148d6, 1.036253535d6, 3.035749715d5,             !/*ba        151*/
     &  3.606919356d5,                                                          !/*ba        152*/
     &  5.492532874d5, 8.598827288d5,-3.399809581d5,-9.852362945d4,             !/*ba        153*/
     & -1.108872347d5,                                                          !/*ba        154*/
     & -9.229576432d4,-1.431344258d5, 5.896521547d4, 1.772385043d4,             !/*ba        155*/
     &  1.845424227d4/                                                          !/*ba        156*/
      data egs2 /                                                               !/*ba        157*/
     &  4.679351387d6, 7.707630513d6,-2.718115276d6,-9.845252314d5,             !/*ba        158*/
     & -1.107173456d6,                                                          !/*ba        159*/
     & -1.137635233d7,-1.870617878d7, 6.669154225d6, 2.413451470d6,             !/*ba        160*/
     &  2.691480439d6,                                                          !/*ba        161*/
     &  1.237627138d7, 2.030222826d7,-7.334289876d6,-2.656357635d6,             !/*ba        162*/
     & -2.912593917d6,                                                          !/*ba        163*/
     & -8.854155353d6,-1.446966194d7, 5.295832834d6, 1.909275233d6,             !/*ba        164*/
     &  2.048899787d6,                                                          !/*ba        165*/
     &  4.290642787d6, 6.951223648d6,-2.601557110d6,-9.129731614d5,             !/*ba        166*/
     & -9.627344865d5,                                                          !/*ba        167*/
     & -1.314924218d6,-2.095971932d6, 8.193066795d5, 2.716279969d5,             !/*ba        168*/
     &  2.823297853d5,                                                          !/*ba        169*/
     &  2.131536582d5, 3.342907992d5,-1.365390745d5,-4.417841315d4,             !/*ba        170*/
     & -4.427025540d4/                                                          !/*ba        171*/
      data egs3 /                                                               !/*ba        172*/
     & -3.600471364d6,-5.805932202d6, 1.773029253d6, 4.064280430d5,             !/*ba        173*/
     &  7.419581557d5,                                                          !/*ba        174*/
     &  8.829126250d6, 1.422377198d7,-4.473342834d6,-1.073350611d6,             !/*ba        175*/
     & -1.845960521d6,                                                          !/*ba        176*/
     & -9.781712604d6,-1.575666314d7, 5.161226883d6, 1.341287330d6,             !/*ba        177*/
     &  2.083994843d6,                                                          !/*ba        178*/
     &  7.182555931d6, 1.156915972d7,-3.941330542d6,-1.108259560d6,             !/*ba        179*/
     & -1.543982755d6,                                                          !/*ba        180*/
     & -3.579820035d6,-5.740079339d6, 2.041827680d6, 5.981648181d5,             !/*ba        181*/
     &  7.629263278d5,                                                          !/*ba        182*/
     &  1.122573403d6, 1.777161418d6,-6.714631146d5,-1.952833263d5,             !/*ba        183*/
     & -2.328129775d5,                                                          !/*ba        184*/
     & -1.839672155d5,-2.871137706d5, 1.153532734d5, 3.423868607d4,             !/*ba        185*/
     &  3.738902942d4/                                                          !/*ba        186*/
      data egs4 /                                                               !/*ba        187*/
     &  2.421750735d6, 4.107929841d6,-1.302310290d6,-5.267906237d5,             !/*ba        188*/
     & -6.197966854d5,                                                          !/*ba        189*/
     & -5.883394376d6,-9.964568970d6, 3.198405768d6, 1.293156541d6,             !/*ba        190*/
     &  1.506909314d6,                                                          !/*ba        191*/
     &  6.387411818d6, 1.079547152d7,-3.517981421d6,-1.424705631d6,             !/*ba        192*/
     & -1.629099740d6,                                                          !/*ba        193*/
     & -4.550695232d6,-7.665548805d6, 2.530844204d6, 1.021187317d6,             !/*ba        194*/
     &  1.141553709d6,                                                          !/*ba        195*/
     &  2.182540324d6, 3.646532772d6,-1.228378318d6,-4.813626449d5,             !/*ba        196*/
     & -5.299974544d5,                                                          !/*ba        197*/
     & -6.518758807d5,-1.070414288d6, 3.772592079d5, 1.372024952d5,             !/*ba        198*/
     &  1.505359294d5,                                                          !/*ba        199*/
     &  9.952777968d4, 1.594230613d5,-6.029082719d4,-2.023689807d4,             !/*ba        200*/
     & -2.176008230d4/                                                          !/*ba        201*/
      data egs5 /                                                               !/*ba        202*/
     & -4.902668827d5,-8.089034293d5, 1.282510910d5,-1.704435174d4,             !/*ba        203*/
     &  8.876109934d4,                                                          !/*ba        204*/
     &  1.231673941d6, 2.035989814d6,-3.727491110d5, 4.071377327d3,             !/*ba        205*/
     & -2.375344759d5,                                                          !/*ba        206*/
     & -1.429330809d6,-2.376692769d6, 5.216954243d5, 7.268703575d4,             !/*ba        207*/
     &  3.008350125d5,                                                          !/*ba        208*/
     &  1.114306796d6, 1.868800148d6,-4.718718351d5,-1.215904582d5,             !/*ba        209*/
     & -2.510379590d5,                                                          !/*ba        210*/
     & -5.873353309d5,-9.903614817d5, 2.742543392d5, 9.055579135d4,             !/*ba        211*/
     &  1.364869036d5,                                                          !/*ba        212*/
     &  1.895325584d5, 3.184776808d5,-9.500485442d4,-3.406036086d4,             !/*ba        213*/
     & -4.380685984d4,                                                          !/*ba        214*/
     & -2.969272274d4,-4.916872669d4, 1.596305804d4, 5.741228836d3,             !/*ba        215*/
     &  6.669912421d3/                                                          !/*ba        216*/
! ======================================================================        !/*ba        217*/
!                                                                               !/*ba        218*/
!     The program starts here                                                   !/*ba        219*/
!                                                                               !/*ba        220*/
      iz = z + dp1m6                                                            !/*ba        221*/
      ia = a + dp1m6                                                            !/*ba        222*/
      el = il                                                                   !/*ba        223*/
      if (iz.lt.19 .or. iz.gt.111) go to 900                                    !/*ba        224*/
      if (iz.gt.102 .and. il.gt.0) go to 910                                    !/*ba        225*/
      amin = dp12m1*z + dp1m2*z*z                                               !/*ba        226*/
      amax = dp58m1*z - dp24m3*z*z                                              !/*ba        227*/
      if (a.lt.amin .or. a.gt.amax) go to 920                                   !/*ba        228*/
      aa = dp25m4*a                                                             !/*ba        229*/
      zz = dp1m2*z                                                              !/*ba        230*/
      bfis0 = 0.d0                                                              !/*ba        231*/
      call lpoly2 (zz, pz, 7)                                                   !/*ba        232*/
      call lpoly2 (aa, pa, 7)                                                   !/*ba        233*/
        do 10 i = 1,7                                                           !/*ba        234*/
        do 10 j = 1,7                                                           !/*ba        235*/
        bfis0 = bfis0 + elzcof(j,i)*pz(j)*pa(i)                                 !/*ba        236*/
 10     continue                                                                !/*ba        237*/
      bfis = bfis0                                                              !/*ba        238*/
      egs = dp0                                                                 !/*ba        239*/
      if (il.lt.1) return                                                       !/*ba        240*/
      amin = dp14m1*z + dp9m3*z*z                                               !/*ba        241*/
      amax = dp20 + dp3*z                                                       !/*ba        242*/
      if (a.lt.amin-dp5 .or. a.gt.amax+dp10) go to 930                          !/*ba        243*/
      el80 = dp0                                                                !/*ba        244*/
      el20 = dp0                                                                !/*ba        245*/
        do 20 i = 1,4                                                           !/*ba        246*/
        do 20 j = 1,5                                                           !/*ba        247*/
        el80 = el80 + elmcof(j,i)*pz(j)*pa(i)                                   !/*ba        248*/
        el20 = el20 + emncof(j,i)*pz(j)*pa(i)                                   !/*ba        249*/
 20     continue                                                                !/*ba        250*/
      zz = z                                                                    !/*ba        251*/
      aa = a                                                                    !/*ba        252*/
      call elmaxc (zz, aa, elmax)                                               !/*ba        253*/
      sel80 = el80                                                              !/*ba        254*/
      sel20 = el20                                                              !/*ba        255*/
      selmax = elmax                                                            !/*ba        256*/
      x = sel20/selmax                                                          !/*ba        257*/
      y = sel80/selmax                                                          !/*ba        258*/
      z1 = el/selmax                                                            !/*ba        259*/
      if (el.le.sel20) then                                                     !/*ba        260*/
        q = dp2m1/(sel20**2*sel80**2*(sel20 - sel80))                           !/*ba        261*/
        qa =  q*(dp4*sel80**3 - sel20**3)                                       !/*ba        262*/
        qb = -q*(dp4*sel80**2 - sel20**2)                                       !/*ba        263*/
        bfis = bfis*(dp1 + qa*el**2 + qb*el**3)                                 !/*ba        264*/
      else                                                                      !/*ba        265*/
        aj = (-dp20*x**5 + dp25*x**4 - dp4)*(y - dp1)**2*y*y                    !/*ba        266*/
        ak = (-dp20*y**5 + dp25*y**4 - dp1)*(x - dp1)**2*x*x                    !/*ba        267*/
        q = dp2m1/((y - x)*((dp1 - x)*(dp1 - y)*x*y)**2)                        !/*ba        268*/
        qa =  q*(aj*y - ak*x)                                                   !/*ba        269*/
        qb = -q*(aj*(dp2*y + dp1) - ak*(dp2*x + dp1))                           !/*ba        270*/
        a1 = dp4*z1**5 - dp5*z1**4 + dp1                                        !/*ba        271*/
        a2 = qa*(dp2*z1 + dp1)                                                  !/*ba        272*/
        bfis = bfis*(a1 + (z1 - dp1)*(a2 + qb*z1)*z1*z1*(z1 - dp1))             !/*ba        273*/
      endif                                                                     !/*ba        274*/
      if (bfis.le.dp0) bfis = dp0                                               !/*ba        275*/
      if (el.gt.selmax) then                                                    !/*ba        276*/
        bfis = dp0                                                              !/*ba        277*/
        return                                                                  !/*ba        278*/
      endif                                                                     !/*ba        279*/
!                                                                               !/*ba        280*/
!     Now calculate rotating ground-state energy                                !/*ba        281*/
!                                                                               !/*ba        282*/
      ell = z1                                                                  !/*ba        283*/
      call lpoly2 (ell, pl, 9)                                                  !/*ba        284*/
      degs = 0.d0                                                               !/*ba        285*/
        do 60 k = 1,5                                                           !/*ba        286*/
!   Use powers of (A/250) from 0-4.                                             !/*ba        287*/
          do 60 l = 1,7                                                         !/*ba        288*/
!   Use powers of (Z/100) from 0-6.                                             !/*ba        289*/
            do 60 m = 1,5                                                       !/*ba        290*/
!   Use even powers of (l/lmax) from 0-8.                                       !/*ba        291*/
            degs = degs + egscof(m,l,k)*pz(l)*pa(k)*pl(2*m-1)                   !/*ba        292*/
   60       continue                                                            !/*ba        293*/
      egs = degs                                                                !/*ba        294*/
      egs = max(egs, dp0)                                                       !/*ba        295*/
      return                                                                    !/*ba        296*/
  900 write(io, 1000) iz, ia                                                    !/*ba        297*/
      write(itty, 1000) iz, ia                                                  !/*ba        298*/
      go to 999                                                                 !/*ba        299*/
  910 write(io, 1010) iz                                                        !/*ba        300*/
      write(itty, 1010) iz                                                      !/*ba        301*/
      go to 999                                                                 !/*ba        302*/
  920 continue                                                                  !/*ba        303*/
! 920 write(io, 1020) ia, iz                                                    !/*ba        304*/
!     write(itty, 1020) ia, iz                                                  !/*ba        305*/
!     go to 999                                                                 !/*ba        306*/
      go to 940                                                                 !/*ba        307*/
  930 continue                                                                  !/*ba        308*/
! 930 write(io, 1030) ia, iz, il                                                !/*ba        309*/
!     write(itty, 1030) ia, iz, il                                              !/*ba        310*/
!     go to 999                                                                 !/*ba        311*/
  940 call bf2 (a, z, dp0)                                                      !/*ba        312*/
      return                                                                    !/*ba        313*/
  999 bfis = dp0                                                                !/*ba        314*/
! ======================================================================        !/*ba        315*/
 1000 format(/1x,'* * Barfit is called with  Z = ',i3,' and ',                  !/*ba        316*/
     & 'A = ',i3,'.  Bfis is set to 0.0 . * *')                                 !/*ba        317*/
 1010 format(/1x,'* * Barfit is called with  Z = ',i3,                          !/*ba        318*/
     & ' and  L  not equal to zero.'/1x,'Bfis is set to 0.0 . * *')             !/*ba        319*/
 1020 format(/1x,'* * Barfit is called with  A =',i3,', outside ',              !/*ba        320*/
     & 'the allowed values for Z =',i3,' * *')                                  !/*ba        321*/
 1030 format(/1x,'* * Barfit is called with  A = ',i3,', outside',              !/*ba        322*/
     & ' the allowed values for Z =',i3/1x,'and L =',i3,                        !/*ba        323*/
     & '  * *')                                                                 !/*ba        324*/
! ======================================================================        !/*ba        325*/
#endif                                                                          /*!ba        326*/
      return                                                                    !/*ba        327*/
      end                                                                       !/*ba        328*/
