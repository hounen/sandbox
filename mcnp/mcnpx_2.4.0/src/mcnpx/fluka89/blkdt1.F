!     $Id: blkdt1.F,v 1.6 2002/03/27 18:14:40 gwm Exp $                       !/*bl          1*/
      block data blkdt1                                                         !/*bl          2*/
!                                                                     *         !/*bl          3*/
!---------------------------------------------------------------------*         !/*bl          4*/
!                                                                     *         !/*bl          5*/
#include <lcs/double.h>                                                         /*!bl          6*/
#include <fluka89/flprm1.h>                                                     /*!bl          7*/
!                                                                     *         !/*bl          8*/
#include <fluka89/flprm2.h>                                                     /*!bl          9*/
!----------------------------------------------------------------------*        !/*bl         10*/
!     version may 81                 pertti aarnio                     *        !/*bl         11*/
!                                    helsinki university of            *        !/*bl         12*/
!                                    technology, finland               *        !/*bl         13*/
!     last change 20. sep. 89 by     alfredo ferrari                   *        !/*bl         14*/
!                                                                      *        !/*bl         15*/
!                                                                      *        !/*bl         16*/
!     this is a block data subroutine of fluka86, which should contain *        !/*bl         17*/
!     the common blocks, which can and should be initialised before    *        !/*bl         18*/
!     the run.                                                         *        !/*bl         19*/
!                                                                      *        !/*bl         20*/
!     note that most of the default values for input parameters are    *        !/*bl         21*/
!     set here.                                                        *        !/*bl         22*/
!----------------------------------------------------------------------*        !/*bl         23*/
!                 ---> now from flukadd maclib  (a. fasso, 1989)       *        !/*bl         24*/
!----------------------------------------------------------------------*        !/*bl         25*/
!                                                                      *        !/*bl         26*/
!*ca acracy                                                                     !/*bl         27*/
!*ca beam                                                                       !/*bl         28*/
!*ca bemit                                                                      !/*bl         29*/
!*ca caslim                                                                     !/*bl         30*/
!*ca comcon                                                                     !/*bl         31*/
!*ca count                                                                      !/*bl         32*/
#include <fluka89/countq.h>                                                     /*!bl         33*/
!*ca dltadp                                                                     !/*bl         34*/
!*ca dmptap                                                                     !/*bl         35*/
!*ca evesel                                                                     !/*bl         36*/
!*ca fluxes                                                                     !/*bl         37*/
!*ca geosel                                                                     !/*bl         38*/
!*ca magpar                                                                     !/*bl         39*/
#include <fluka89/mapa.h>                                                       /*!bl         40*/
#include <fluka89/paprop.h>                                                     /*!bl         41*/
!*ca stars                                                                      !/*bl         42*/
!*ca thr                                                                        !/*bl         43*/
!----------------------------------------------------------------------*        !/*bl         44*/
!                                                                      *        !/*bl         45*/
! /paprop/                                                             *        !/*bl         46*/
      data btype/'proton  ','aproton ','electron','positron','neutrie ',        !/*bl         47*/
     &'aneutrie','photon  ','neutron ','aneutron','muon+   ','muon-   ',        !/*bl         48*/
     &'kaonlong','pion+   ','pion-   ','kaon+   ','kaon-   ','lambda  ',        !/*bl         49*/
     &'alambda ','kaonshrt','sigma-  ','sigma+  ','sigmazer','pizero  ',        !/*bl         50*/
     &'kaonzero','akaonzer','reserved','blank   ','blank   ','blank   ',        !/*bl         51*/
     &'heavyion'/                                                               !/*bl         52*/
      data am /0.9382796d0,0.9382796d0,0.51099906d-03,0.51099906d-03,0          !/*bl         53*/
     & .d0,0.d0,0.d0,0.9395731d0,0.9395731d0,0.10565946d0,0.10565946d0,0        !/*bl         54*/
     & .49767d0,0.1395669d0,0.1395669d0,0.493668d0,0.493668d0,1.1156d0,1        !/*bl         55*/
     & .1156d0,0.49767d0,1.19735d0,1.18937d0,1.19247d0,0.1349626d0,0.497        !/*bl         56*/
     & 67d0,0.49767d0,0.1349626d0,0.d0,0.d0,0.d0,0.9395731d0/                   !/*bl         57*/
      data ichrge /1,2*-1,1,5*0,1,-1,0,1,-1,1,-1,3*0,-1,1,9*0/                  !/*bl         58*/
      data iscore /201,9*0/                                                     !/*bl         59*/
      data genpar/'all-part','all-char','all-neut','all-nega',                  !/*bl         60*/
     &'all-posi','nucleons','nuc&pi+-','energy  ','pions+- ','beampart',        !/*bl         61*/
     &'em-enrgy',19*'blank   '/                                                 !/*bl         62*/
      data ijdisc /30*0/                                                        !/*bl         63*/
      data thalf /7*1.d+20,2*925.d0,2*2.19714d-6,5.183d-8,2*2.603d-8,2*1        !/*bl         64*/
     & .2371d-8,2*2.632d-10,.8923d-10,1.482d-10,.8d-10,5.8d-20,.83d-16,2        !/*bl         65*/
     & *2.64d-8,5*1.d-20/                                                       !/*bl         66*/
! /beam/                                                               *        !/*bl         67*/
!     data pbeam /200.d0/, dpbeam, divbm, xspot, yspot /4*0.d0/, xina,          !/*bl         68*/
!    & yina, zina, tinx, tiny, tinz /5*0.d0,1.d0/, nforce, xfor, yfor,          !/*bl         69*/
!    & zfor, disfor, wfor /0,3*0.d0,1.d+20,1.d0/, ijbeam, ipbite, idiv,         !/*bl         70*/
!    & ixspot, iyspot, beawei /1,4*0,1.d0/                                      !/*bl         71*/
! /bemit/                                                              *        !/*bl         72*/
!     data verts, vertl, verte, hors, horl, hore, vsig, vpsig, hsig,            !/*bl         73*/
!    & hpsig, txv, tyv, tzv, txh, tyh, tzh, ibemit /0.d0,2*0.1d0,0.d0,3*        !/*bl         74*/
!    & .1d0,1.d0,.1d0,2*1.d0,3*0.d0,1.d0,0.d0,0/                                !/*bl         75*/
! /mapa/                                                               *        !/*bl         76*/
!     data amss /2*0.d0,1.0079d0,4.0026d0,9.0122d0,12.011d0,14.0067d0,15        !/*bl         77*/
!    & .9994d0,24.305d0,26.982d0,55.847d0,63.546d0,107.868d0,195.09d0,19        !/*bl         78*/
!    & 6.9665d0,200.59d0,207.2d0,238.029d0,239.13d0,11.2d0,20.5d0,119.d0        !/*bl         79*/
!    & ,184.d0,14.4d0,20.5d0,0.d0,24*0.d0/                                      !/*bl         80*/
!     data rho /0.d0,1.d-20,.08988d-3,.1785d-3,1.848d0,2.d0,1.2506d-03,1        !/*bl         81*/
!    & .429d-3,1.738d0,2.6989d0,7.877d0,8.96d0,10.5d0,21.45d0,19.32d0,13        !/*bl         82*/
!    & .546d0,11.35d0,18.95d0,19.84d0,1.d0,2.15d0,7.31d0,19.3d0,1.29d-3,        !/*bl         83*/
!    & 2.5d0,0.d0,24*0.d0/                                                      !/*bl         84*/
! /caslim/                                                             *        !/*bl         85*/
!     data tlim, tmean, tmax, ttot /20.d0,3*0.d0/, ncases, nstars /2*500        !/*bl         86*/
!    & 0/                                                                       !/*bl         87*/
!     data ncase /0/, ncoinc /0/                                                !/*bl         88*/
! /count/                                                              *        !/*bl         89*/
!     data nel, nol /2340*0/                                                    !/*bl         90*/
!     data pel, del, delab, topp, pes, supp /2102*0.d0/                         !/*bl         91*/
! /dltadp/                                                             *        !/*bl         92*/
!      data fu, lmat, mmat /6015*0.d0,7*0/                                      !/*bl         93*/
! /stars/                                                              *        !/*bl         94*/
!     data ntstar /0/                                                           !/*bl         95*/
!     data wstars, westar, westop, wstop /62*0.d0/                              !/*bl         96*/
!     data weisec, weipri /31*0.d0/                                             !/*bl         97*/
!     data wdec, wdau, wtdec, wtdau, numdec /62*0.d0,0/                         !/*bl         98*/
!     data edi /5*0.d0/                                                         !/*bl         99*/
! /geosel/                                                             *        !/*bl        100*/
!     data geocod /'cartesia','r-z     ','r-theta ','us1geom ',                 !/*bl        101*/
!    & 'us2geom ','us3geom ','us4geom ','us5geom ','us6geom ',                  !/*bl        102*/
!    & 'combinat'/                                                              !/*bl        103*/
!     data igeo /0/, ipri /0/                                                   !/*bl        104*/
! /evesel/                                                             *        !/*bl        105*/
!     data evecod /'inclusiv','kaspro  ','quark   ','evap    ',                 !/*bl        106*/
!    & 'us1prod ','us2prod ','us3prod ','us4prod ','us5prod ',                  !/*bl        107*/
!    & 'us6prod ','us7prod '/                                                   !/*bl        108*/
!     data ieve /3/                                                             !/*bl        109*/
! /thr/                                                                *        !/*bl        110*/
!     data ethr, fluthr /.05d0,0.d0/                                            !/*bl        111*/
! /fluxes/                                                             *        !/*bl        112*/
!     data dispth /10000*0.d0/, disp /200*0.d0/, disth /200*0.d0/               !/*bl        113*/
!     data pbig, psmal, plow, phigh, pbin /11*0.d0/                             !/*bl        114*/
!     data thbig, thsmal, thlow, thhigh, thbin /9*0.d0,.523599d0,0.d0/          !/*bl        115*/
!     data iflrg1, iflrg2, iflux, irap /20*0,3,0/                               !/*bl        116*/
!     data flurad, fluxx, fluyy, fluzz, flutx, fluty, flutz /1.d2,5*0.d0        !/*bl        117*/
!    & ,1.d0/                                                                   !/*bl        118*/
!     data ifluwt /0/                                                           !/*bl        119*/
!     data lfluw1, lfluw2, lfluw3 /3*.false./                                   !/*bl        120*/
!     data fluw1, fluw2, fluw3 /1.d0,2*0.d0/                                    !/*bl        121*/
!     data disfi /200*0.d0/                                                     !/*bl        122*/
!     data fibig, fismal, filow, fihigh, fibin /9*0.d0,.523599d0,0.d0/          !/*bl        123*/
!     data fitx, fity, fitz /1.d0,2*0.d0/                                       !/*bl        124*/
!     data ifisco /0/                                                           !/*bl        125*/
! /dmptap/                                                             *        !/*bl        126*/
!REP  data idump /11/                                                           !/*bl        127*/
!     data dump1, dump2, dump3, dump4, dump5, dump6, dump7, dump8 /8*           !/*bl        128*/
!    & .false./                                                                 !/*bl        129*/
!     data dump9, dump10, dump11, dump12, dump13 /5*.false./                    !/*bl        130*/
!     data dump14, dump15, dump16, dump17, dump18 /5*.false./                   !/*bl        131*/
! /acracy/                                                             *        !/*bl        132*/
!     data estep, deferr, mulpri, mulsec, iostep, ishow /0.d0,.01d0,2*0,        !/*bl        133*/
!    & 30,10/                                                                   !/*bl        134*/
! /countq/                                                             *        !/*bl        135*/
      data sopp /2*0.d0/                                                        !/*bl        136*/
! /comcon/                                                             *        !/*bl        137*/
!     data icompl, matnum, contnt /50*1,icomax*0,icomax*0.d0/                   !/*bl        138*/
!     data sumacn /icomax*0.d0/                                                 !/*bl        139*/
! /magpar/                                                             *        !/*bl        140*/
!     data angle, delta, dstep /1.57079632679489661923d0,.1d0,.75d0/            !/*bl        141*/
!     data bcom, btxcom, btycom, btzcom, magnon /4*0.d0,500*.false./            !/*bl        142*/
!=                                               end*block.blkdt1      *        !/*bl        143*/
      end                                                                       !/*bl        144*/
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$        !/*bl        145*/
