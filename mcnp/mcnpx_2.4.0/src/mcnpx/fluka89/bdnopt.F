!     $Id: bdnopt.F,v 1.5 2002/03/27 18:14:39 gwm Exp $                       !/*bd          1*/
      block data bdnopt                                                         !/*bd          2*/
!                                                                     *         !/*bd          3*/
!         axcssv = this parameter should be set equal to the number   *         !/*bd          4*/
!                  for which unity is negligible for the machine      *         !/*bd          5*/
!                   accuracy                                          *         !/*bd          6*/
!         andrfl = "underflow" of the machine for floating point      *         !/*bd          7*/
!                   operation                                         *         !/*bd          8*/
!         avrflw = "overflow"  of the machine for floating point      *         !/*bd          9*/
!                   operation                                         *         !/*bd         10*/
!         ainfnt = code "infinite"                                    *         !/*bd         11*/
!         azrzrz = code "zero"                                        *         !/*bd         12*/
!                                                                     *         !/*bd         13*/
!---------------------------------------------------------------------*         !/*bd         14*/
!                                                                     *         !/*bd         15*/
#include <lcs/double.h>                                                         /*!bd         16*/
#include <fluka89/flprm1.h>                                                     /*!bd         17*/
#include <fluka89/flprm2.h>                                                     /*!bd         18*/
!----------------------------------------------------------------------*        !/*bd         19*/
!                                                                               !/*bd         20*/
!   created on 20-sep-1989 by a. ferrari - infn milan                           !/*bd         21*/
!                                                                               !/*bd         22*/
!----------------------------------------------------------------------*        !/*bd         23*/
!                                                                               !/*bd         24*/
#include <fluka89/blnkcm.h>                                                     /*!bd         25*/
!*ca cmmdnr                                                                     !/*bd         26*/
!*ca ctitle                                                                     !/*bd         27*/
!*ca detect                                                                     !/*bd         28*/
!*ca detloc                                                                     !/*bd         29*/
!*ca fheavy                                                                     !/*bd         30*/
#include <fluka89/finuc1.h>                                                     /*!bd         31*/
!*ca mulhd                                                                      !/*bd         32*/
#include <fluka89/parevt.h>                                                     /*!bd         33*/
!*ca stepsz                                                                     !/*bd         34*/
!*ca trackr                                                                     !/*bd         35*/
!*ca usrbdx                                                                     !/*bd         36*/
!*ca usrbin                                                                     !/*bd         37*/
! /blnkcm/                                                                      !/*bd         38*/
      parameter (mxdumm=kalgnm*nblnmx)                                          !/*bd         39*/
      data mblnmx /mxdumm/                                                      !/*bd         40*/
!REP  data kblnkl, kgmbgn, kgmlst, kdtbgn, kdtlst, kubbgn, kublst,              !/*bd         41*/
!REP 1 kuxbgn, kuxlst /9*0/                                                     !/*bd         42*/
! /cmmdnr/                                                                      !/*bd         43*/
!     data ddnear /0.d+00/, lfldnr /.false./                                    !/*bd         44*/
! /ctitle/                                                                      !/*bd         45*/
!     data runtit(1:40) /'****************************************'/            !/*bd         46*/
!     data runtit(41:80) /'****************************************'/           !/*bd         47*/
! /detect/                                                                      !/*bd         48*/
!     parameter (nnn1=nrgnmx*ndtcmx)                                            !/*bd         49*/
!     parameter (nnn2=nscrmx*ndtcmx)                                            !/*bd         50*/
!     data ldtctr /.false./, ndtsco /0/, ndtdet /0/                             !/*bd         51*/
!     data edtmin /ndtcmx*0.d0/, edtbin /ndtcmx*0.d0/, edtcut /ndtcmx*0         !/*bd         52*/
!    & .d0/                                                                     !/*bd         53*/
!     data kdtreg /nnn1*0/, kdtdet /nnn2*0/                                     !/*bd         54*/
!     data idtreg /mxxrgn*0/, kdtscd /nscrmx*0/                                 !/*bd         55*/
!     data titdet /ndtcmx*'          '/, titsco /nscrmx*'          '/           !/*bd         56*/
! /detloc/                                                                      !/*bd         57*/
!     data accump /ndtcm2*0.d0/, accumn /ndtcm2*0.d0/, icoinc /ndtcm2*0/        !/*bd         58*/
!     data nclas /0/                                                            !/*bd         59*/
! /fheavy/                                                                      !/*bd         60*/
!     data amheav /6*0.d+00/                                                    !/*bd         61*/
!     data anheav /'neutron ','proton  ','deuteron','3-h     ',                 !/*bd         62*/
!    & '3-he    ','4-he    '/                                                   !/*bd         63*/
!     data icheav /0,1,1,1,2,2/, ibheav /1,1,2,3,3,4/                           !/*bd         64*/
!     data npheav /0/                                                           !/*bd         65*/
! /finuc/                                                                       !/*bd         66*/
      data np /0/, tv /0.d+00/, tvcms /0.d+00/, tvrecl /0.d+00/, tvheav         !/*bd         67*/
     & /0.d+00/                                                                 !/*bd         68*/
! /mulhd/                                                                       !/*bd         69*/
!     data jtokpa /11*1,10*2,9*1/, ltopt /.true./, nfscat /0/                   !/*bd         70*/
!     data estepf /mxxmdf*0.1d0/                                                !/*bd         71*/
! /parevt/                                                                      !/*bd         72*/
      data ldiffr /.true.,.false.,6*.true.,.false.,8*.true.,.false.,4*          !/*bd         73*/
     & .true.,.false.,2*.true.,5*.false./                                       !/*bd         74*/
      data linctv /.true./                                                      !/*bd         75*/
! /stepsz/                                                                      !/*bd         76*/
!     data stepmn /mxxrgn*0.d0/, stepmx /mxxrgn*0.d0/                           !/*bd         77*/
! /trackr/                                                                      !/*bd         78*/
!     data xtrack /0.d0,mxtrck*0.d0/, ytrack /0.d0,mxtrck*0.d0/, ztrack         !/*bd         79*/
!    & /0.d0,mxtrck*0.d0/, ttrack /mxtrck*0.d0/, dtrack /mxtrck*0.d0/,          !/*bd         80*/
!    & etrack /0.d0/, wtrack /0.d0/ntrack /0/, mtrack /0/, jtrack /0/           !/*bd         81*/
! /usrbin/                                                                      !/*bd         82*/
!     data lusbin /.false./, nusrbn /0/                                         !/*bd         83*/
! /usrbdx/                                                                      !/*bd         84*/
!     data lusbdx /.false./, nusrbx /0/                                         !/*bd         85*/
!=                                               end*block.bdnopt      *        !/*bd         86*/
      end                                                                       !/*bd         87*/
