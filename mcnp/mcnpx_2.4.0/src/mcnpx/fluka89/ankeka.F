!     $Id: ankeka.F,v 1.4 2002/03/27 18:14:39 gwm Exp $                       !/*an          1*/
      double precision function ankeka (it,to,amss,sqamss)                      !/*an          2*/
!                                                                     *         !/*an          3*/
!---------------------------------------------------------------------*         !/*an          4*/
!                                                                     *         !/*an          5*/
#include <lcs/double.h>                                                         /*!an          6*/
#include <fluka89/flprm1.h>                                                     /*!an          7*/
#include <fluka89/flprm2.h>                                                     /*!an          8*/
#include <lcs/inout.h>                                                          /*!an          9*/
!********************************************************************           !/*an         10*/
!     version by                     j. ranft                                   !/*an         11*/
!                                    leipzig                                    !/*an         12*/
!     last change 05. december 83    pertti aarnio                              !/*an         13*/
!                                    helsinki university of                     !/*an         14*/
!                                    technology, finland                        !/*an         15*/
!                                                                               !/*an         16*/
!     to be called from the high energy production                              !/*an         17*/
!                                                                               !/*an         18*/
!     to give the proton and neutron multiplicities of the high                 !/*an         19*/
!     energy part of the intranuclear cascade  (n-2p,n-2n).                     !/*an         20*/
!     note that it is supposed in ekeka and rakeka that                         !/*an         21*/
!     n-1p=9*n-2p, n-1n=9*n-2n.                                                 !/*an         22*/
!                                                                               !/*an         23*/
!     note!!!!!!! non-standard particle numbering                               !/*an         24*/
!                                                                               !/*an         25*/
!     input variables:                                                          !/*an         26*/
!        it     = type of the particle                                          !/*an         27*/
!                 1=intranuclear proton                                         !/*an         28*/
!                 2=intranuclear neutron                                        !/*an         29*/
!        to     = energy of the colliding particle in gev                       !/*an         30*/
!        amss   = atomic weight of the medium                                   !/*an         31*/
!        sqamss = sqrt(amss)                                                    !/*an         32*/
!                                                                               !/*an         33*/
!     see ranft/routti particle acc. vol 4 p 105                                !/*an         34*/
!********************************************************************           !/*an         35*/
!                                                                               !/*an         36*/
      dimension a(2), b(2), c(2), d(2)                                          !/*an         37*/
!                                                                               !/*an         38*/
!     these are magko multiplicities - but interpreted as being                 !/*an         39*/
!     multiplicities of the high energy part - magko thinks that                !/*an         40*/
!     these are the multiplicities of the low energy part.                      !/*an         41*/
!     this scheme fits well with t7short experiment.                            !/*an         42*/
!                                                                               !/*an         43*/
!     data a/1.d0,1.3d0/                                                        !/*an         44*/
!     data b/0.5d0,0.6d0/                                                       !/*an         45*/
!     data c/3.5d0,4.5d0/                                                       !/*an         46*/
!     data d/1.d0,1.8d0/                                                        !/*an         47*/
!                                                                               !/*an         48*/
!     these are effective magko multiplicities. i.e. these                      !/*an         49*/
!     should give the same results as magko.                                    !/*an         50*/
!                                                                               !/*an         51*/
!     data a/1.d0,1.3d0/                                                        !/*an         52*/
!     data b/0.2d0,0.24d0/                                                      !/*an         53*/
!     data c/1.4,d01.8d0/                                                       !/*an         54*/
!     data d/0.4d0,0.4d0/                                                       !/*an         55*/
!                                                                               !/*an         56*/
!     these are effective fluka81 multiplicities                                !/*an         57*/
!                                                                               !/*an         58*/
      data a /1.d0,1.3d0/                                                       !/*an         59*/
      data b /0.06d0,0.09d0/                                                    !/*an         60*/
      data c /1.104d0,1.90d0/                                                   !/*an         61*/
      data d /0.14d0,0.19d0/                                                    !/*an         62*/
!                                                                               !/*an         63*/
!      nucrin 83 multiplicities                                                 !/*an         64*/
!                                                                               !/*an         65*/
!     data a/1.d0,1.3d0/                                                        !/*an         66*/
!     data b/0.15d0,0.225d0/                                                    !/*an         67*/
!     data c/1.35d0,2.28d0/                                                     !/*an         68*/
!     data d/0.07d0,0.09d0/                                                     !/*an         69*/
!                                                                               !/*an         70*/
!     these are the multiplicities from particle accelerators                   !/*an         71*/
!                                                                               !/*an         72*/
!     data a/1.d0,1.08d0/                                                       !/*an         73*/
!     data b/0.036d0,0.042d0/                                                   !/*an         74*/
!     data c/0.24d0,0.30d0/                                                     !/*an         75*/
!     data d/0.07d0,0.084d0/                                                    !/*an         76*/
!                                                                               !/*an         77*/
      if (to.gt.0.1d0) go to 10                                                 !/*an         78*/
      ankeka=sqamss*b(it)*0.1d0                                                 !/*an         79*/
      return                                                                    !/*an         80*/
   10 continue                                                                  !/*an         81*/
      if (to.ge.10.d0) go to 20                                                 !/*an         82*/
      ankeka=0.1d0*sqamss*(0.5d0+a(it)*(1.d0+log10(to))**2)*d(it)               !/*an         83*/
      return                                                                    !/*an         84*/
   20 continue                                                                  !/*an         85*/
      ankeka=sqamss*0.1d0*c(it)                                                 !/*an         86*/
      return                                                                    !/*an         87*/
      end                                                                       !/*an         88*/
