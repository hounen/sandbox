!     $Id: shelwk.F,v 1.5 2002/03/27 18:14:35 gwm Exp $                       !/*sh          1*/
      function shelwk (z,betasq,ptraut)                                         !/*sh          2*/
#include <lcs/double.h>                                                         /*!sh          3*/
!                                                                               !/*sh          4*/
!        purpose :  this function computes the k-shell correction               !/*sh          5*/
!                   using the physics discribed in reference 129:               !/*sh          6*/
!                                                                               !/*sh          7*/
!                     m. walske, "stopping power of k-electrons,"               !/*sh          8*/
!                     physical review,  vol 88, number 6 (1952).                !/*sh          9*/
!                                                                               !/*sh         10*/
!        author :   daniel pickens                                              !/*sh         11*/
!                   computer sciences corporation                               !/*sh         12*/
!                   13 may 1984                                                 !/*sh         13*/
!                                                                               !/*sh         14*/
!        inputs :   z      - the atomic number of the element.                  !/*sh         15*/
!                   betasq - the converted kinetic energy to                    !/*sh         16*/
!                            velocity squared.                                  !/*sh         17*/
!                   ptraut  - logical variable,if true then author and          !/*sh         18*/
!                            reference information written to scratch fi        !/*sh         19*/
!                                                                               !/*sh         20*/
!        outputs :  shelwk - the k-shell correction as computed using           !/*sh         21*/
!                            Walske method and data.                            !/*sh         22*/
!                                                                               !/*sh         23*/
!        errors :   none.                                                       !/*sh         24*/
!                                                                               !/*sh         25*/
#include <dedx/block8.h>                                                        /*!sh         26*/
      dimension etagd1(26), thtgd1(3), shlkgd(26,3), thtgd2(5), s(5), t(        !/*sh         27*/
     & 5), u(5), v(5), w(5), etagd2(12), b(12,3)                                !/*sh         28*/
      character*3 ildaln                                                        !/*sh         29*/
      logical ptraut                                                            !/*sh         30*/
!                                                                               !/*sh         31*/
!        note, the eta grid, theta 1 grid, and the k-shell correction           !/*sh         32*/
!        grid below is taken from an article by bichsel.                        !/*sh         33*/
!                                                                               !/*sh         34*/
      data etagd1 /0.500,0.526,0.556,0.588,0.625,0.645,0.714,0.769,0.833        !/*sh         35*/
     & ,0.909,1.000,1.110,1.250,1.430,1.540,1.670,1.820,2.000,2.220,2.50        !/*sh         36*/
     & 0,2.860,3.330,4.000,5.000,10.00,20.00/                                   !/*sh         37*/
!                                                                               !/*sh         38*/
      data thtgd1 /0.7,0.8,0.9/                                                 !/*sh         39*/
!                                                                               !/*sh         40*/
      data (shlkgd(i,1),i=1,26) /0.619,0.667,0.711,0.753,0.796,0.836,0.8        !/*sh         41*/
     & 73,0.907,0.936,0.958,0.974,0.983,0.982,0.961,0.941,0.916,0.885,0.        !/*sh         42*/
     & 849,0.805,0.755,0.694,0.621,0.537,0.450,0.2407,0.1153/                   !/*sh         43*/
      data (shlkgd(i,2),i=1,26) /0.710,0.757,0.797,0.833,0.870,0.903,0.9        !/*sh         44*/
     & 35,0.966,0.991,1.013,1.025,1.029,1.019,0.993,0.973,0.947,0.916,0.        !/*sh         45*/
     & 878,0.831,0.776,0.708,0.635,0.555,0.460,0.2460,0.1180/                   !/*sh         46*/
      data (shlkgd(i,3),i=1,26) /0.745,0.784,0.824,0.862,0.899,0.933,0.9        !/*sh         47*/
     & 64,0.991,1.013,1.031,1.041,1.043,1.033,1.003,0.981,0.955,0.923,0.        !/*sh         48*/
     & 886,0.840,0.783,0.720,0.645,0.560,0.465,0.2472,0.1186/                   !/*sh         49*/
!                                                                               !/*sh         50*/
      data thtgd2 /0.700,0.750,0.800,0.850,0.900/                               !/*sh         51*/
!                                                                               !/*sh         52*/
      data s /1.8133,1.7223,1.6457,1.5807,1.5250/                               !/*sh         53*/
!                                                                               !/*sh         54*/
      data t /2.4603,2.4044,2.3462,2.2868,2.2273/                               !/*sh         55*/
!                                                                               !/*sh         56*/
      data u /2.0662,2.0999,2.1196,2.1290,2.1309/                               !/*sh         57*/
!                                                                               !/*sh         58*/
      data v /7.3246,7.3194,7.3191,7.3218,7.3263/                               !/*sh         59*/
!                                                                               !/*sh         60*/
      data w /45.0,45.0,45.0,45.0,45.0/                                         !/*sh         61*/
!                                                                               !/*sh         62*/
      data etagd2 /0.0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55        !/*sh         63*/
     & /                                                                        !/*sh         64*/
!                                                                               !/*sh         65*/
      data b /0.0,0.0,0.01,0.05,0.11,0.17,0.24,0.32,0.40,0.49,0.58,0.67,        !/*sh         66*/
     & 0.0,0.0,0.01,0.04,0.07,0.13,0.19,0.26,0.33,0.41,0.49,0.58,0.0,0.0        !/*sh         67*/
     & ,0.01,0.03,0.06,0.10,0.15,0.21,0.28,0.35,0.44,0.46/                      !/*sh         68*/
!                                                                               !/*sh         69*/
      iz=nint(z)                                                                !/*sh         70*/
      shelwk=0.0                                                                !/*sh         71*/
!                                                                               !/*sh         72*/
!        if the element is hydrogen then return.                                !/*sh         73*/
!                                                                               !/*sh         74*/
      if (iz.eq.1) return                                                       !/*sh         75*/
!                                                                               !/*sh         76*/
!        obtain the theta value.                                                !/*sh         77*/
!                                                                               !/*sh         78*/
      thetak=thetas(1,iz)                                                       !/*sh         79*/
!                                                                               !/*sh         80*/
!        compute the eta value then check its range to determine                !/*sh         81*/
!        which computation to use for shelwk                                    !/*sh         82*/
!                                                                               !/*sh         83*/
      etaval=eta(z,1,betasq)                                                    !/*sh         84*/
!                                                                               !/*sh         85*/
!        init old flag to specify new calculation each use                      !/*sh         86*/
!        of alin and alin3d.                                                    !/*sh         87*/
!                                                                               !/*sh         88*/
      ildaln='new'                                                              !/*sh         89*/
!                                                                               !/*sh         90*/
!        evaluate for the low eta values, eta less than 0.5.                    !/*sh         91*/
!                                                                               !/*sh         92*/
      if ((etaval.ge.0.0).and.(etaval.lt.0.50)) then                            !/*sh         93*/
!                                                                               !/*sh         94*/
!        compute the 'b' value first.                                           !/*sh         95*/
!        interpolate to obtain the b value since data is available.             !/*sh         96*/
!                                                                               !/*sh         97*/
        ngrid=12                                                                !/*sh         98*/
        ngrid2=3                                                                !/*sh         99*/
        bvalue=alin3d(etaval,thetak,etagd2,thtgd1,b,ngrid,ngrid2,ildaln)        !/*sh        100*/
!                                                                               !/*sh        101*/
!         actual computation of the low eta k-shell correction.                 !/*sh        102*/
!                                                                               !/*sh        103*/
        ngrid3=5                                                                !/*sh        104*/
        shelwk=alin(thetak,thtgd2,s,ngrid3,ildaln)*log(etaval)+alin             !/*sh        105*/
     &  (thetak,thtgd2,t,ngrid3,ildaln)-bvalue                                  !/*sh        106*/
!                                                                               !/*sh        107*/
      endif                                                                     !/*sh        108*/
!                                                                               !/*sh        109*/
!        evaluate the k-shell correction medium eta values.                     !/*sh        110*/
!                                                                               !/*sh        111*/
      if ((etaval.ge.0.50).and.(etaval.le.20.0)) then                           !/*sh        112*/
        ngrid=26                                                                !/*sh        113*/
        ngrid2=3                                                                !/*sh        114*/
        shelwk=alin3d(etaval,thetak,etagd1,thtgd1,shlkgd,ngrid,ngrid2           !/*sh        115*/
     &  ,ildaln)                                                                !/*sh        116*/
!                                                                               !/*sh        117*/
      endif                                                                     !/*sh        118*/
!                                                                               !/*sh        119*/
!        evaluate the k-shell correction for high eta values.                   !/*sh        120*/
!                                                                               !/*sh        121*/
      ngrid3=5                                                                  !/*sh        122*/
      if (etaval.gt.20.0) shelwk=alin(thetak,thtgd2,u,ngrid3,ildaln)            !/*sh        123*/
     & /etaval+alin(thetak,thtgd2,v,ngrid3,ildaln)/(etaval**2)-alin             !/*sh        124*/
     & (thetak,thtgd2,w,ngrid3,ildaln)/(etaval**3)                              !/*sh        125*/
!                                                                               !/*sh        126*/
!        write out the author and reference information to                      !/*sh        127*/
!        scratch file if ptraut is true.                                        !/*sh        128*/
!                                                                               !/*sh        129*/
      if (ptraut) write (10,10)                                                 !/*sh        130*/
!                                                                               !/*sh        131*/
      return                                                                    !/*sh        132*/
!                                                                               !/*sh        133*/
   10 format('k-shell - walske''s total shell method, physical review,',        !/*sh        134*/
     & ' vol. 88 (1952).')                                                      !/*sh        135*/
      end                                                                       !/*sh        136*/
