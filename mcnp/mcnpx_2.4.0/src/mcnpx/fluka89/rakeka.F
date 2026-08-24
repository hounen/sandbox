!     $Id: rakeka.F,v 1.4 2002/03/27 18:14:47 gwm Exp $                       !/*ra          1*/
      subroutine rakeka (it,to,amss,t,p,de)                                     !/*ra          2*/
!                                                                     *         !/*ra          3*/
!---------------------------------------------------------------------*         !/*ra          4*/
!                                                                     *         !/*ra          5*/
#include <lcs/double.h>                                                         /*!ra          6*/
#include <fluka89/flprm1.h>                                                     /*!ra          7*/
#include <fluka89/flprm2.h>                                                     /*!ra          8*/
#include <lcs/inout.h>                                                          /*!ra          9*/
!********************************************************************           !/*ra         10*/
!     version by                     j. ranft                                   !/*ra         11*/
!                                    leipzig                                    !/*ra         12*/
!     last change 05. december 83 by pertti aarnio                              !/*ra         13*/
!                                    helsinki university of                     !/*ra         14*/
!                                    technology, finland                        !/*ra         15*/
!                                                                               !/*ra         16*/
!     to be called from the high energy production                              !/*ra         17*/
!                                                                               !/*ra         18*/
!     this is a subroutine of fluka to sample intranuclear cascade              !/*ra         19*/
!     particles.                                                                !/*ra         20*/
!                                                                               !/*ra         21*/
!     input variables:                                                          !/*ra         22*/
!        it    = type of the secondary requested; 1=proton, 2=neutron           !/*ra         23*/
!        to    = kinetic energy of the primary in gev                           !/*ra         24*/
!        amss  = atomic weight of the medium                                    !/*ra         25*/
!                                                                               !/*ra         26*/
!     output variables:                                                         !/*ra         27*/
!        t     = kinetic energy of the secondary in gev                         !/*ra         28*/
!        p     = momentum of the secondary in gev/c                             !/*ra         29*/
!        de    = angle between the directions of the primary                    !/*ra         30*/
!                and secondary particles                                        !/*ra         31*/
!                                                                               !/*ra         32*/
!********************************************************************           !/*ra         33*/
!                                                                               !/*ra         34*/
      dimension am(2)                                                           !/*ra         35*/
      data am /0.9382796d0,0.9395731d0/                                         !/*ra         36*/
!******************* sample the kinetic energy **********************           !/*ra         37*/
!                                                                               !/*ra         38*/
      ap=akeka(it,to,amss)                                                      !/*ra         39*/
!     akeka is giving the high-energy slope which is 6 times the low            !/*ra         40*/
!     energy slope. multiplicity of the low energy particles is                 !/*ra         41*/
!     2 times the multiplicity of the high energy particles.                    !/*ra         42*/
      if (rang().lt.0.6d0) ap=ap/6.d0                                           !/*ra         43*/
      app=1.d0/ap                                                               !/*ra         44*/
   10 t=-log(rang())/app                                                        !/*ra         45*/
      if (t.gt.to) go to 10                                                     !/*ra         46*/
      p=sqrt((t+2.d0*am(it))*t)                                                 !/*ra         47*/
!                                                                               !/*ra         48*/
!******************** sample the angle *******************************        !/*ra         49*/
!                                                                               !/*ra         50*/
!     normalization constants an1 and an2                                       !/*ra         51*/
!                                                                               !/*ra         52*/
      ade=0.02d0*(1.d0+0.003d0*amss)/t*6.d0                                     !/*ra         53*/
      dex=exp(-1.57d0*1.57d0/ade)                                               !/*ra         54*/
      an1=(1.d0-dex)*ade/2.d0                                                   !/*ra         55*/
      an2=dex*1.57d0                                                            !/*ra         56*/
      an=an1+an2                                                                !/*ra         57*/
      an1=an1/an                                                                !/*ra         58*/
!                                                                               !/*ra         59*/
!     which part to sample?                                                     !/*ra         60*/
!                                                                               !/*ra         61*/
      if (rang().gt.an1) go to 30                                               !/*ra         62*/
!                                                                               !/*ra         63*/
!     sample from an1-part                                                      !/*ra         64*/
!                                                                               !/*ra         65*/
   20 de=sqrt(-ade*log(1.d0-rang()*(1.d0-dex)))                                 !/*ra         66*/
      if (de.gt.1.57d0) go to 20                                                !/*ra         67*/
      return                                                                    !/*ra         68*/
!                                                                               !/*ra         69*/
!     sample from an2-part                                                      !/*ra         70*/
!                                                                               !/*ra         71*/
   30 continue                                                                  !/*ra         72*/
      de=-rang()                                                                !/*ra         73*/
      de=atan2(sqrt(dp1-de**2),de)                                              !/*ra         74*/
      return                                                                    !/*ra         75*/
      end                                                                       !/*ra         76*/
