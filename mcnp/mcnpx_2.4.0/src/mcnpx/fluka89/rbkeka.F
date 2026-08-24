!     $Id: rbkeka.F,v 1.4 2002/03/27 18:14:47 gwm Exp $                       !/*rb          1*/
      subroutine rbkeka (it,to,amss,sqamss,t,p,de)                              !/*rb          2*/
!                                                                     *         !/*rb          3*/
!---------------------------------------------------------------------*         !/*rb          4*/
!                                                                     *         !/*rb          5*/
#include <lcs/double.h>                                                         /*!rb          6*/
#include <fluka89/flprm1.h>                                                     /*!rb          7*/
#include <fluka89/flprm2.h>                                                     /*!rb          8*/
#include <lcs/inout.h>                                                          /*!rb          9*/
!*** sampling of cascade nucleon angle and energy                               !/*rb         10*/
!     write(io,*)it,to,amss,sqamss,t,p,de                                       !/*rb         11*/
#include <fluka89/ablts1.h>                                                     /*!rb         12*/
      ap=bkeka(it,to,amss)                                                      !/*rb         13*/
      tmin=0.020d0                                                              !/*rb         14*/
      ap=ap-(ap+tmin)*0.15d0                                                    !/*rb         15*/
      v=rang()                                                                  !/*rb         16*/
!     energyadjust                                                              !/*rb         17*/
      vtest=0.6d0                                                               !/*rb         18*/
      enadj=1.d0                                                                !/*rb         19*/
      vtest=vtest/enadj                                                         !/*rb         20*/
      if (v.lt.vtest) ap=ap/6.d0                                                !/*rb         21*/
   10 continue                                                                  !/*rb         22*/
      app=1.d0/ap                                                               !/*rb         23*/
!     t=rexp(app)                                                               !/*rb         24*/
      t=-ap*log(rang())                                                         !/*rb         25*/
      t=t+tmin                                                                  !/*rb         26*/
      if (t.gt.to) go to 10                                                     !/*rb         27*/
      iitt=it                                                                   !/*rb         28*/
      if (it.eq.2) iitt=8                                                       !/*rb         29*/
      p=sqrt(t*(t+2.d0*am(iitt)))                                               !/*rb         30*/
      ade=0.090d0*(1.d0+0.081d0*amss**.3333d0)/t                                !/*rb         31*/
      dex=exp(-1.57d0*1.57d0/ade)                                               !/*rb         32*/
      an1=(1.d0-dex)*ade/2.d0                                                   !/*rb         33*/
      an2=dex*1.57d0                                                            !/*rb         34*/
      an=an1+an2                                                                !/*rb         35*/
      an1=an1/an                                                                !/*rb         36*/
      v=rang()                                                                  !/*rb         37*/
      if (v.gt.an1) go to 30                                                    !/*rb         38*/
   20 continue                                                                  !/*rb         39*/
      v=rang()                                                                  !/*rb         40*/
      de=sqrt(-ade*log(1.d0-v*(1.d0-dex)))                                      !/*rb         41*/
      if (de.gt.1.57d0) go to 20                                                !/*rb         42*/
!     write(io,*)it,to,amss,sqamss,t,p,de                                       !/*rb         43*/
      return                                                                    !/*rb         44*/
   30 continue                                                                  !/*rb         45*/
      v=rang()                                                                  !/*rb         46*/
      de=-v                                                                     !/*rb         47*/
      de=atan2(sqrt(dp1-de**2),de)                                              !/*rb         48*/
!     write(io,*)it,to,amss,sqamss,t,p,de                                       !/*rb         49*/
      return                                                                    !/*rb         50*/
      end                                                                       !/*rb         51*/
