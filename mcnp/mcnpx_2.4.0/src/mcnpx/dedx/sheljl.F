!     $Id: sheljl.F,v 1.4 2002/03/27 18:14:33 gwm Exp $                       !/*sh          1*/
      function sheljl (znumb,bsq,cl1,cl2,ptraut)                                !/*sh          2*/
#include <lcs/double.h>                                                         /*!sh          3*/
!                                                                               !/*sh          4*/
!        this function computes the l-shell correction                          !/*sh          5*/
!        using the janni method.                                                !/*sh          6*/
!                                                                               !/*sh          7*/
!        ptraut - variable specifying to write out author                       !/*sh          8*/
!                 and reference information to scratch file.                    !/*sh          9*/
!                                                                               !/*sh         10*/
      dimension zst(5)                                                          !/*sh         11*/
      dimension pz(5)                                                           !/*sh         12*/
      dimension qz(5)                                                           !/*sh         13*/
      dimension rl(5)                                                           !/*sh         14*/
      dimension sl(5)                                                           !/*sh         15*/
      dimension ble(16)                                                         !/*sh         16*/
      dimension bld(16,5)                                                       !/*sh         17*/
      character*3 ildaln                                                        !/*sh         18*/
      logical ptraut                                                            !/*sh         19*/
!                                                                               !/*sh         20*/
      data zst /10.0,20.0,30.0,45.0,103.0/                                      !/*sh         21*/
!     data pz/1.12,1.50,1.88,1.99,2.0/                                          !/*sh         22*/
      data pz /0.55,1.50,1.88,1.99,2.0/                                         !/*sh         23*/
!     data qz/1.357,1.6934,2.0298,2.1358,2.1519/                                !/*sh         24*/
      data qz /0.8524,1.6934,2.0298,2.1358,2.1519/                              !/*sh         25*/
!     data rl/12.162,10.037,7.912,6.745,6.034/                                  !/*sh         26*/
      data rl /13.756,10.037,7.912,6.745,6.034/                                 !/*sh         27*/
      data sl /31.840,28.145,24.450,21.906,20.015/                              !/*sh         28*/
      data ble /0.,.05,.1,.15,.2,.25,.3,.35,.4,.45,.5,.55,.6,.65,.7,.75/        !/*sh         29*/
      data bld /0.0,2.25,5.10,7.64,10.4,12.94,14.96,16.88,18.63,20.02,21        !/*sh         30*/
     & .38,22.55,23.72,24.70,25.68,26.64,0.,1.85,4.,6.4,8.8,11.,12.82,14        !/*sh         31*/
     & .5,16.,17.26,18.44,19.47,20.5,21.38,22.26,23.08,0.,1.4,3.1,5.16,7        !/*sh         32*/
     & .2,9.06,10.68,12.12,13.37,14.5,15.5,16.39,17.28,18.06,18.84,19.52        !/*sh         33*/
     & ,0.,1.1,2.53,4.27,6.1,7.76,9.26,10.56,11.72,12.7,13.6,14.39,15.18        !/*sh         34*/
     & ,15.84,16.5,17.12,0.,.7,1.9,3.52,5.2,6.73,8.1,9.36,10.4,11.33,12.        !/*sh         35*/
     & 2,12.93,13.66,14.28,14.9,15.46/                                          !/*sh         36*/
!                                                                               !/*sh         37*/
!        note that the following values for pz,qz,rl,and sl were                !/*sh         38*/
!        obtained by linear extrapolation to z=10.                              !/*sh         39*/
!          1.12,1.357,12.162,31.84     direct linear extrapolation              !/*sh         40*/
!          0.55,0.8524,13.756,31.84     final values based on aluminum          !/*sh         41*/
!        these values were empirically modified to obtain a best fit            !/*sh         42*/
!        in aluminum.  the bld values now in the array for z=10 are             !/*sh         43*/
!        directly obtained by linear extrapolation without alteration.          !/*sh         44*/
!                                                                               !/*sh         45*/
      z=znumb                                                                   !/*sh         46*/
!                                                                               !/*sh         47*/
!        init old flag to specify new calculation each use                      !/*sh         48*/
!        of alin and alin3d.                                                    !/*sh         49*/
!                                                                               !/*sh         50*/
      ildaln='new'                                                              !/*sh         51*/
!                                                                               !/*sh         52*/
!        initialize the subshell and shell corrections to zero.                 !/*sh         53*/
!                                                                               !/*sh         54*/
      cl1=0.0                                                                   !/*sh         55*/
      cl2=0.0                                                                   !/*sh         56*/
      sheljl=0.0                                                                !/*sh         57*/
!                                                                               !/*sh         58*/
      if (z.le.6.0) return                                                      !/*sh         59*/
!                                                                               !/*sh         60*/
      slater=4.15                                                               !/*sh         61*/
      etaval=(18800.0*bsq)/((z-slater)*(z-slater)*(1.0-bsq))                    !/*sh         62*/
!                                                                               !/*sh         63*/
      if (z.lt.10.0) z=10.0                                                     !/*sh         64*/
!                                                                               !/*sh         65*/
      neta=16                                                                   !/*sh         66*/
      nz=5                                                                      !/*sh         67*/
!                                                                               !/*sh         68*/
      reta=1.0/etaval                                                           !/*sh         69*/
      if (etaval.gt.3.0) sheljl=reta*(alin(z,zst,pz,nz,ildaln)+reta*(1.5        !/*sh         70*/
     & +reta*(-4.0+reta*4.4)))                                                  !/*sh         71*/
!                                                                               !/*sh         72*/
      if ((etaval.gt.0.7).and.(etaval.le.3.0)) sheljl=alin(z,zst,qz,nz          !/*sh         73*/
     & ,ildaln)/etaval                                                          !/*sh         74*/
      if (etaval.le.0.7) sheljl=alin(z,zst,rl,nz,ildaln)*log(etaval)            !/*sh         75*/
     & +alin(z,zst,sl,nz,ildaln)-alin3d(etaval,z,ble,zst,bld,neta,nz            !/*sh         76*/
     & ,ildaln)                                                                 !/*sh         77*/
!                                                                               !/*sh         78*/
      if (znumb.lt.10.0) sheljl=sheljl*(znumb-2.0)/8.0                          !/*sh         79*/
!                                                                               !/*sh         80*/
!        write out the author and reference information to                      !/*sh         81*/
!        scratch file if ptraut is true.                                        !/*sh         82*/
!                                                                               !/*sh         83*/
      if (ptraut) write (10,10)                                                 !/*sh         84*/
!                                                                               !/*sh         85*/
      return                                                                    !/*sh         86*/
!                                                                               !/*sh         87*/
   10 format ('l-shell - janni''s total shell method.')                         !/*sh         88*/
      end                                                                       !/*sh         89*/
