!     $Id: branch.F,v 1.5 2002/03/27 18:14:56 gwm Exp $                       !/*br          1*/
      subroutine branch (tcm,i9,i10,i11,finel,finex)                            !/*br          2*/
#include <lcs/double.h>                                                         /*!br          3*/
!     ==================================================================        !/*br          4*/
!     program calculates the probability of an inelastic collision(finel        !/*br          5*/
!     and the probability of an inelastic or charge exchange collision          !/*br          6*/
!     (finex).program does linear interpolation on equally spaced data          !/*br          7*/
!                                                                               !/*br          8*/
! BRANCH calls   ERRORF                                                         !/*br          9*/
! called by GAMMA                                                               !/*br         10*/
!     ==================================================================        !/*br         11*/
#include <lcs/inout.h>                                                          /*!br         12*/
!                                                                               !/*br         13*/
      dimension finii(36), finij(42), fini0(42), finxij(42), finxi0(42)         !/*br         14*/
!                                                                               !/*br         15*/
      data finii /dp0,.034d0,.037d0,.080d0,.125d0,.136d0,.143d0,.200d0,.        !/*br         16*/
     & 211d0,.222d0,.235d0,.294d0,.313d0,.313d0,.375d0,.375d0,.438d0,.47        !/*br         17*/
     & 1d0,.471d0,.556d0,.556d0,.526d0,.550d0,.524d0,.545d0,.522d0,.500d        !/*br         18*/
     & 0,.500d0,.480d0,.480d0,.500d0,.462d0,.462d0,.481d0,.481d0,.481d0/        !/*br         19*/
      data finij /dp0,dp0,.037d0,.074d0,.111d0,.111d0,.148d0,.148d0,.143        !/*br         20*/
     & d0,.207d0,.200d0,.194d0,.273d0,.265d0,.278d0,.316d0,.357d0,.400d0        !/*br         21*/
     & ,.426d0,.438d0,.447d0,.444d0,.429d0,.450d0,.447d0,.447d0,.474d0,.        !/*br         22*/
     & 487d0,.500d0,.500d0,.489d0,.510d0,.481d0,.446d0,.431d0,.407d0,.41        !/*br         23*/
     & 4d0,.421d0,.426d0,.420d0,.447d0,.467d0/                                  !/*br         24*/
      data fini0 /dp0,dp0,.014d0,.029d0,.047d0,.050d0,.069d0,.089d0,.089        !/*br         25*/
     & d0,.148d0,.167d0,.167d0,.222d0,.241d0,.250d0,.286d0,.317d0,.371d0        !/*br         26*/
     & ,.391d0,.406d0,.422d0,.419d0,.431d0,.448d0,.446d0,.482d0,.500d0,.        !/*br         27*/
     & 500d0,.517d0,.500d0,.500d0,.514d0,.487d0,.463d0,.440d0,.429d0,.44        !/*br         28*/
     & 1d0,.429d0,.438d0,.436d0,.459d0,.472/                                    !/*br         29*/
      data finxi0 /.405d0,.410d0,.431d0,.441d0,.453d0,.483d0,.483d0,.518        !/*br         30*/
     & d0,.518d0,.555d0,.574d0,.574d0,.592d0,.611d0,.607d0,.607d0,.617d0        !/*br         31*/
     & ,.629d0,.641d0,.656d0,.641d0,.645d0,.672d0,.655d0,.660d0,.696d0,.        !/*br         32*/
     & 679d0,.672d0,.684d0,.656d0,.647d0,.653d0,.645d0,.638d0,.630d0,.64        !/*br         33*/
     & 3d0,.631d0,.619d0,.613d0,.590d0,.594d0,.583d0/                           !/*br         34*/
      data finxij /.607d0,.593d0,.593d0,.593d0,.592d0,.592d0,.592d0,.592        !/*br         35*/
     & d0,.572d0,.586d0,.567d0,.549d0,.576d0,.559d0,.556d0,.553d0,.571d0        !/*br         36*/
     & ,.578d0,.596d0,.605d0,.596d0,.600d0,.596d0,.600d0,.605d0,.605d0,.        !/*br         37*/
     & 606d0,.615d0,.625d0,.619d0,.600d0,.612d0,.596d0,.571d0,.569d0,.56        !/*br         38*/
     & 0d0,.552d0,.561d0,.556d0,.540d0,.553d0,.556d0/                           !/*br         39*/
! for i-i events data are from tcm=310 mev to tcm=660 mev,for i-j and           !/*br         40*/
! i-j0 events data are from tcm=250 mev to tcm=660 mev                          !/*br         41*/
      finel=dp0                                                                 !/*br         42*/
      finex=dp0                                                                 !/*br         43*/
      if (tcm.ge.66.d1) go to 30                                                !/*br         44*/
      if (i9.eq.1) go to 20                                                     !/*br         45*/
! have an i-j or i-j0 interaction                                               !/*br         46*/
      if (tcm.lt.25.d1) return                                                  !/*br         47*/
      re=0.1d0*tcm-25.d0                                                        !/*br         48*/
      i=re                                                                      !/*br         49*/
      i=i+1                                                                     !/*br         50*/
      de=re-real(i-1)                                                           !/*br         51*/
      ae=dp1-de                                                                 !/*br         52*/
      if (i10.eq.1) go to 10                                                    !/*br         53*/
! have i-j0 interaction                                                         !/*br         54*/
      finel=ae*fini0(i)+de*fini0(i+1)                                           !/*br         55*/
      finex=ae*finxi0(i)+de*finxi0(i+1)                                         !/*br         56*/
      return                                                                    !/*br         57*/
   10 continue                                                                  !/*br         58*/
! have an i-j interaction                                                       !/*br         59*/
      finel=ae*finij(i)+de*finij(i+1)                                           !/*br         60*/
      finex=ae*finxij(i)+de*finxij(i+1)                                         !/*br         61*/
      return                                                                    !/*br         62*/
   20 continue                                                                  !/*br         63*/
! have an i-i interaction                                                       !/*br         64*/
      if (tcm.lt.31.d1) return                                                  !/*br         65*/
      re=0.1d0*tcm-3.1d1                                                        !/*br         66*/
      i=re                                                                      !/*br         67*/
      i=i+1                                                                     !/*br         68*/
      de=re-real(i-1)                                                           !/*br         69*/
      ae=dp1-de                                                                 !/*br         70*/
      finel=ae*finii(i)+de*finii(i+1)                                           !/*br         71*/
      finex=finel                                                               !/*br         72*/
      return                                                                    !/*br         73*/
   30 continue                                                                  !/*br         74*/
      write (io,40) tcm,i9,i10,i11                                              !/*br         75*/
      call expirx(1,'BRANCH','tcm.ge.660.')                                     !/*br         76*/
!                                                                               !/*br         77*/
   40 format ('1',' energy too large in branch.e=',f10.4,                       !/*br         78*/
     & ' i9,i10, and i11=',3i10)                                                !/*br         79*/
      end                                                                       !/*br         80*/
