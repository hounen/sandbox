!     $Id: ekeka.F,v 1.4 2002/03/27 18:14:42 gwm Exp $                        !/*ek          1*/
      double precision function ekeka (ix,to,amss,sqamss)                       !/*ek          2*/
!                                                                     *         !/*ek          3*/
!---------------------------------------------------------------------*         !/*ek          4*/
!                                                                     *         !/*ek          5*/
#include <lcs/double.h>                                                         /*!ek          6*/
#include <fluka89/flprm1.h>                                                     /*!ek          7*/
#include <fluka89/flprm2.h>                                                     /*!ek          8*/
#include <lcs/inout.h>                                                          /*!ek          9*/
!********************************************************************           !/*ek         10*/
!     version by                     j. ranft                                   !/*ek         11*/
!                                    leipzig                                    !/*ek         12*/
!     last change 05. december by    pertti aarnio                              !/*ek         13*/
!                                    helsinki university of                     !/*ek         14*/
!                                    technology, finland                        !/*ek         15*/
!                                                                               !/*ek         16*/
!     to be called from the high energy production                              !/*ek         17*/
!                                                                               !/*ek         18*/
!     this is a subroutine of fluka to calculate the energy available           !/*ek         19*/
!     for the reaction "ix".                                                    !/*ek         20*/
!                                                                               !/*ek         21*/
!     note!!!!!!! reaction numbering is not                                     !/*ek         22*/
!     compatible with particle numbering                                        !/*ek         23*/
!                                                                               !/*ek         24*/
!     input variables:                                                          !/*ek         25*/
!        ix     = type of the reaction                                          !/*ek         26*/
!                 1=nuclear excitation                                          !/*ek         27*/
!                 2=intranuclear proton                                         !/*ek         28*/
!                 3=intranuclear neutron                                        !/*ek         29*/
!                 4=1+2                                                         !/*ek         30*/
!                 5=1+2+3                                                       !/*ek         31*/
!        to     = kinetic energy of the colliding particle in gev               !/*ek         32*/
!        amss   = atomic weight of the medium                                   !/*ek         33*/
!        sqamss = sqrt(amss)                                                    !/*ek         34*/
!                                                                               !/*ek         35*/
!     see ranft/routti particle acc vol 4 p 106                                 !/*ek         36*/
!                                                                               !/*ek         37*/
!     note that in intranuclear part average total energy is                    !/*ek         38*/
!     obtained by multplying the average energy of the high energy              !/*ek         39*/
!     particles by the multiplicity of the high energy particles                !/*ek         40*/
!     i.1. e-tot,av=2.5*n2*e-av(alfa-2).  the factor 2.5 is                     !/*ek         41*/
!     needed to take into account the low energy part also.                     !/*ek         42*/
!     2.5 is based on the assumption that n1/n2=9 and that                      !/*ek         43*/
!     (e-av(alfa-2))/(e-av(alfa-1))=6.                                          !/*ek         44*/
!********************************************************************           !/*ek         45*/
!                                                                               !/*ek         46*/
      go to (10,40,50,10,10), ix                                                !/*ek         47*/
!                                                                               !/*ek         48*/
   10 continue                                                                  !/*ek         49*/
      if (to.gt.01d0) go to 20                                                  !/*ek         50*/
      aa=0.001d0*sqamss                                                         !/*ek         51*/
      go to 30                                                                  !/*ek         52*/
   20 continue                                                                  !/*ek         53*/
      apar=0.035d0                                                              !/*ek         54*/
      bpar=3.d0                                                                 !/*ek         55*/
      cpar=0.1d0                                                                !/*ek         56*/
      aa=cpar*sqamss*(0.01d0+apar*(bpar+log10(to))**2)                          !/*ek         57*/
!                                                                               !/*ek         58*/
   30 continue                                                                  !/*ek         59*/
      if (ix.gt.3) go to 40                                                     !/*ek         60*/
      ekeka=aa                                                                  !/*ek         61*/
      return                                                                    !/*ek         62*/
!                                                                               !/*ek         63*/
   40 continue                                                                  !/*ek         64*/
      an=ankeka(1,to,amss,sqamss)                                               !/*ek         65*/
      a=akeka(1,to,amss)                                                        !/*ek         66*/
      extoa=0.d0                                                                !/*ek         67*/
      if (to.lt.5.d0*a) extoa=exp(-to/a)                                        !/*ek         68*/
      tpkav=a*(1.d0-(to/a+1.)*extoa)/(1.d0-extoa)                               !/*ek         69*/
      bb=2.5d0*tpkav*an                                                         !/*ek         70*/
!                                                                               !/*ek         71*/
      if (ix.eq.4) go to 60                                                     !/*ek         72*/
      if (ix.eq.5) go to 50                                                     !/*ek         73*/
      ekeka=bb                                                                  !/*ek         74*/
      return                                                                    !/*ek         75*/
!                                                                               !/*ek         76*/
   50 continue                                                                  !/*ek         77*/
      an=ankeka(2,to,amss,sqamss)                                               !/*ek         78*/
      a=akeka(2,to,amss)                                                        !/*ek         79*/
      extoa=0.d0                                                                !/*ek         80*/
      if (to.lt.5.d0*a) extoa=exp(-to/a)                                        !/*ek         81*/
      tnkav=a*(1.d0-(to/a+1.d0)*extoa)/(1.d0-extoa)                             !/*ek         82*/
      cc=2.5d0*tnkav*an                                                         !/*ek         83*/
!                                                                               !/*ek         84*/
      if (ix.eq.5) go to 70                                                     !/*ek         85*/
      ekeka=cc                                                                  !/*ek         86*/
      return                                                                    !/*ek         87*/
!                                                                               !/*ek         88*/
   60 continue                                                                  !/*ek         89*/
      ekeka=aa+bb                                                               !/*ek         90*/
      return                                                                    !/*ek         91*/
!                                                                               !/*ek         92*/
   70 continue                                                                  !/*ek         93*/
      ekeka=aa+bb+cc                                                            !/*ek         94*/
      return                                                                    !/*ek         95*/
      end                                                                       !/*ek         96*/
