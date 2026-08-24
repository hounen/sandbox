!     $Id: fluxes.h,v 1.4 2002/03/27 18:14:15 gwm Exp $                       !/*fl          1*/
!----------------------------------------------------------------------*        !/*fl          2*/
!     include file: fluxes copy              created 26/11/86 by pa    *        !/*fl          3*/
!     changes: none                                                    *        !/*fl          4*/
!     included in the following subroutines or functions: not updated  *        !/*fl          5*/
!                                                                      *        !/*fl          6*/
!     description of the common block(s) and variable(s)               *        !/*fl          7*/
!                                                                      *        !/*fl          8*/
!                                                                      *        !/*fl          9*/
!     /fluxes/ contains the variables needed in flux scoring/plotting  *        !/*fl         10*/
!        dispth = flux as a function of momentum and polar angle       *        !/*fl         11*/
!        disp   = flux as a function of momentum                       *        !/*fl         12*/
!        disth  = flux as a function of polar angle                    *        !/*fl         13*/
!        psmal  = particle flux below the lower momentum limit         *        !/*fl         14*/
!        pbig   = particle flux above the upper momentum limit         *        !/*fl         15*/
!        plow   = lower momentum limit in gev/c                        *        !/*fl         16*/
!        phigh  = higher momentum limit in gev/c                       *        !/*fl         17*/
!        pbin   = momentum bin size in gev/c                           *        !/*fl         18*/
!        thsmal = particle flux below lower angle                      *        !/*fl         19*/
!        thbig  = particle flux above higher angle                     *        !/*fl         20*/
!        thlow  = lower angle limit in radians or rapidity             *        !/*fl         21*/
!        thhigh = higher angle limit in radians or rapidity            *        !/*fl         22*/
!        thbin  = angle bin size in radians or rapidity                *        !/*fl         23*/
!        iflrg1 = region numbers at this side of the surface           *        !/*fl         24*/
!        iflrg2 = region numbers at the other side of the surface      *        !/*fl         25*/
!        iflux  = flux type                                            *        !/*fl         26*/
!        irap   = 0 means radians; = 1 means rapidity                  *        !/*fl         27*/
!        flurad = radius of the sphere of flux scoring                 *        !/*fl         28*/
!        fluxx,fluyy,fluzz = coordinates of the centre of the flux     *        !/*fl         29*/
!                            scoring sphere                            *        !/*fl         30*/
!        flutx,fluty,flutz = direction cosines of the symmetry axis    *        !/*fl         31*/
!                            of flux scoring                           *        !/*fl         32*/
!        ifluwt = 0 cosine weighting of the flux,                      *        !/*fl         33*/
!                 1 no cosine weighting of the flux                    *        !/*fl         34*/
!                 3 or greater means that dose is calculated           *        !/*fl         35*/
!                 in material number ifluwt                            *        !/*fl         36*/
!        lfluw1,fluw1 = if lfluw1.eq.true fluxes are multiplied by     *        !/*fl         37*/
!                       fluw1 when output                              *        !/*fl         38*/
!        lfluw2,fluw2 = if lfluw2.eq.true fluxes are multiplied by     *        !/*fl         39*/
!                       fluw2-function when output                     *        !/*fl         40*/
!        lfluw3,fluw3 = if lfluw3.eq.true fluxes are multiplied by     *        !/*fl         41*/
!                       fluw3-function when scored                     *        !/*fl         42*/
!     disfi  = flux as a function of azimuthal angle                   *        !/*fl         43*/
!     fismal = flux falling below the smallest azim. angle             *        !/*fl         44*/
!     fibig  = flux above the biggest azimuthal angle                  *        !/*fl         45*/
!     filow  = smallest azimuthal angle                                *        !/*fl         46*/
!     fihigh = biggest azimuthal angle                                 *        !/*fl         47*/
!     fibin  = size of the azimuthal bins                              *        !/*fl         48*/
!     fitx,fity,fitz = direction with respect to which                 *        !/*fl         49*/
!                      the azimuthal angle is calculated               *        !/*fl         50*/
!     ifisco = if .ne. 0, azimuthal scoring is done                    *        !/*fl         51*/
!     lbnflx = if lbnflx.eq.true fluxes at boundaries have to be       *        !/*fl         52*/
!              scored or by iflux.gt.2 or by boundary crossing         *        !/*fl         53*/
!              estimators both usual or user defined                   *        !/*fl         54*/
!              (a. ferrari 1-july-89)                                  *        !/*fl         55*/
!     lbxflx = if lbxflx.eq.true fluxes at boundaries have to be       *        !/*fl         56*/
!              scored by boundary crossing                             *        !/*fl         57*/
!              estimators both usual or user defined                   *        !/*fl         58*/
!              (a. ferrari 1-july-89)                                  *        !/*fl         59*/
!     ltrflx = if ltrflx.eq.true track length has to be                *        !/*fl         60*/
!              scored or by user defined tracklength or by usual       *        !/*fl         61*/
!              estimators (a. ferrari 17-may-90)                       *        !/*fl         62*/
!     lclflx = if lclflx.eq.true collision density has to be           *        !/*fl         63*/
!              scored or by user defined or by usual                   *        !/*fl         64*/
!              estimators (a. ferrari 17-may-90)                       *        !/*fl         65*/
!     lfluxs = if lfluxs.eq.true  fluxes at boundaries or tracklength  *        !/*fl         66*/
!              or collision density have to be scored                  *        !/*fl         67*/
!              (a. ferrari 17-may-90)                                  *        !/*fl         68*/
!----------------------------------------------------------------------*        !/*fl         69*/
!                                                                               !/*fl         70*/
      logical lfluw1, lfluw2, lfluw3, lbnflx, ltrflx, lfluxs                    !/*fl         71*/
      common /fluxes/ dispth(50,50,4), disp(50,4), disth(50,4), psmal(4)        !/*fl         72*/
     & , pbig(4), plow, phigh, pbin, thsmal(4), thbig(4), thlow, thhigh,        !/*fl         73*/
     & thbin, iflrg1(10), iflrg2(10), iflux, irap, flurad, fluxx, fluyy,        !/*fl         74*/
     & fluzz, flutx, fluty, flutz, ifluwt, lfluw1, lfluw2, lfluw3, fluw1        !/*fl         75*/
     & , fluw2, fluw3, disfi(50,4), fismal(4), fibig(4), filow, fihigh,         !/*fl         76*/
     & fibin, fitx, fity, fitz, ifisco, lbnflx, lbxflx, ltrflx, lclflx,         !/*fl         77*/
     & lfluxs                                                                   !/*fl         78*/
