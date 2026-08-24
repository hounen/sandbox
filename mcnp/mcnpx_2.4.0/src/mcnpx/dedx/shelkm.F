!     $Id: shelkm.F,v 1.5 2002/03/27 18:14:34 gwm Exp $                       !/*sh          1*/
      function shelkm (z,betasq,cm1,cm2,cm3,ptraut)                             !/*sh          2*/
#include <lcs/double.h>                                                         /*!sh          3*/
!                                                                               !/*sh          4*/
!        purpose :  this function computes the m-shell correction               !/*sh          5*/
!                   using the physics discribed in reference 137:               !/*sh          6*/
!                                                                               !/*sh          7*/
!                     g. khandelwal and e. merzbacher,                          !/*sh          8*/
!                     "stopping power of m electrons,"                          !/*sh          9*/
!                     physical review, vol 144, p 349 (1966)                    !/*sh         10*/
!                                                                               !/*sh         11*/
!        author :   daniel pickens                                              !/*sh         12*/
!                   computer sciences corporation                               !/*sh         13*/
!                   20 may 1984                                                 !/*sh         14*/
!                                                                               !/*sh         15*/
!        inputs :   z      - the atomic number of the element.                  !/*sh         16*/
!                   betasq - the converted kinetic energy to                    !/*sh         17*/
!                            velocity squared.                                  !/*sh         18*/
!                   ptraut  - logical variable, if true then author and         !/*sh         19*/
!                            reference information written to scratch fi        !/*sh         20*/
!                                                                               !/*sh         21*/
!        outputs :  shelkm - the m-shell correction as computed using           !/*sh         22*/
!                            Khandelwal method and data.                        !/*sh         23*/
!                   cm1    - s subshell correction (set to zero, used           !/*sh         24*/
!                            here for consistency).                             !/*sh         25*/
!                   cm2    - p subshell correction (set to zero, used           !/*sh         26*/
!                            here for consistency).                             !/*sh         27*/
!                   cm3    - d subshell correction (set to zero, used           !/*sh         28*/
!                            here for consistency).                             !/*sh         29*/
!                                                                               !/*sh         30*/
!        errors :   none.                                                       !/*sh         31*/
!                                                                               !/*sh         32*/
#include <dedx/block8.h>                                                        /*!sh         33*/
      dimension etagd(25), thetgd(3), b(25,3), s(3), t(3), u(3)                 !/*sh         34*/
      character*3 ildaln                                                        !/*sh         35*/
      logical ptraut                                                            !/*sh         36*/
!                                                                               !/*sh         37*/
      data etagd /0.0,0.005,0.01,0.03,0.05,0.08,0.1,0.2,0.3,0.4,0.5,0.6,        !/*sh         38*/
     & 0.7,0.8,0.9,1.0,1.1,1.25,1.5,1.75,2.0,2.25,3.5,5.0,10.0/                 !/*sh         39*/
!                                                                               !/*sh         40*/
      data thetgd /0.35,0.450,0.55/                                             !/*sh         41*/
!                                                                               !/*sh         42*/
      data s /41.37,24.29,17.88/                                                !/*sh         43*/
!                                                                               !/*sh         44*/
      data t /129.8,89.92,72.28/                                                !/*sh         45*/
!                                                                               !/*sh         46*/
      data u /0.24,0.39,0.75/                                                   !/*sh         47*/
!                                                                               !/*sh         48*/
      data (b(i,1),i=1,25) /0.0,2.2,4.2,12.2,19.8,30.2,36.70,62.43,79.08        !/*sh         49*/
     & ,91.12,100.48,108.13,114.60,120.22,125.12,129.53,133.52,138.84,14        !/*sh         50*/
     & 6.42,152.84,158.39,167.60,181.54,196.37,225.07/                          !/*sh         51*/
!                                                                               !/*sh         52*/
      data (b(i,2),i=1,25) /0.0,1.8,3.2,9.0,14.3,22.0,26.44,44.97,56.52,        !/*sh         53*/
     & 64.61,70.74,75.87,79.78,83.32,86.37,89.10,91.55,94.80,99.39,103.2        !/*sh         54*/
     & 5,106.57,112.03,120.24,128.95,145.83/                                    !/*sh         55*/
      data (b(i,3),i=1,25) /0.0,1.3,2.5,7.0,11.3,17.4,20.85,36.13,45.53,        !/*sh         56*/
     & 52.02,56.87,60.94,63.93,66.67,69.01,71.10,72.97,75.44,78.91,81.81        !/*sh         57*/
     & ,84.30,88.36,94.45,100.91,113.38/                                        !/*sh         58*/
!                                                                               !/*sh         59*/
!        initialize the m-shell correction value to zero.                       !/*sh         60*/
!                                                                               !/*sh         61*/
      iz=nint(z)                                                                !/*sh         62*/
      shelkm=0.0                                                                !/*sh         63*/
      cm1=0.0                                                                   !/*sh         64*/
      cm2=0.0                                                                   !/*sh         65*/
      cm3=0.0                                                                   !/*sh         66*/
!                                                                               !/*sh         67*/
!        first check to make sure that there are electrons in this              !/*sh         68*/
!        shell. if there are no electrons, return.                              !/*sh         69*/
!                                                                               !/*sh         70*/
      if ((shel(4,iz).eq.0.0).and.(shel(5,iz).eq.0.0).and.(shel(6,iz).eq        !/*sh         71*/
     & .0.0)) return                                                            !/*sh         72*/
!                                                                               !/*sh         73*/
!        compute the average theta value. then, check the                       !/*sh         74*/
!        theta, if it is zero then return.                                      !/*sh         75*/
!                                                                               !/*sh         76*/
      thetam=((shel(4,iz)*thetas(4,iz))+(shel(5,iz)*thetas(5,iz))+(shel(        !/*sh         77*/
     & 6,iz)*thetas(6,iz)))/(shel(4,iz)+shel(5,iz)+shel(6,iz))                  !/*sh         78*/
!                                                                               !/*sh         79*/
      if (thetam.le.0.0) return                                                 !/*sh         80*/
!                                                                               !/*sh         81*/
!        compute the average eta value then check its range to                  !/*sh         82*/
!        determine which computation to use for shelkm.                         !/*sh         83*/
!                                                                               !/*sh         84*/
      etaval=((eshel(4,iz)*eta(z,4,betasq))+(eshel(5,iz)*eta(z,5,betasq)        !/*sh         85*/
     & )+(eshel(6,iz)*eta(z,6,betasq)))/(eshel(4,iz)+eshel(5,iz)+eshel(6        !/*sh         86*/
     & ,iz))                                                                    !/*sh         87*/
!                                                                               !/*sh         88*/
!        initialize the grid size constants.                                    !/*sh         89*/
!                                                                               !/*sh         90*/
      numgrd=3                                                                  !/*sh         91*/
      numgd2=25                                                                 !/*sh         92*/
!                                                                               !/*sh         93*/
!        init old flag to specify new calculation each use                      !/*sh         94*/
!        of alin and alin3d.                                                    !/*sh         95*/
!                                                                               !/*sh         96*/
      ildaln='new'                                                              !/*sh         97*/
!                                                                               !/*sh         98*/
!        evaluate for the low eta values (eta less than                         !/*sh         99*/
!        or equal to 2).                                                        !/*sh        100*/
!                                                                               !/*sh        101*/
      if ((etaval.ge.0.0).and.(etaval.le.2.0)) then                             !/*sh        102*/
!                                                                               !/*sh        103*/
!        compute the 'b' value first. interpolate to obtain                     !/*sh        104*/
!        the b value from available data.                                       !/*sh        105*/
!                                                                               !/*sh        106*/
        bvalue=alin3d(etaval,thetam,etagd,thetgd,b,numgd2,numgrd,ildaln)        !/*sh        107*/
!                                                                               !/*sh        108*/
!         actual computation of the low eta m-shell correction.                 !/*sh        109*/
!                                                                               !/*sh        110*/
        shelkm=alin(thetam,thetgd,s,numgrd,ildaln)*log(etaval)+alin             !/*sh        111*/
     &  (thetam,thetgd,t,numgrd,ildaln)-bvalue                                  !/*sh        112*/
!                                                                               !/*sh        113*/
      endif                                                                     !/*sh        114*/
!                                                                               !/*sh        115*/
!        evaluate the m-shell correction for higher eta values.                 !/*sh        116*/
!                                                                               !/*sh        117*/
      if (etaval.gt.2.0) shelkm=alin(thetam,thetgd,u,numgrd,ildaln)             !/*sh        118*/
     & /etaval                                                                  !/*sh        119*/
!                                                                               !/*sh        120*/
!        write out the author and reference information to                      !/*sh        121*/
!        scratch file if ptraut if true.                                        !/*sh        122*/
!                                                                               !/*sh        123*/
      if (ptraut) write (10,10)                                                 !/*sh        124*/
!                                                                               !/*sh        125*/
      return                                                                    !/*sh        126*/
!                                                                               !/*sh        127*/
   10 format ('m-shell - khandelwal''s total shell method,',                    !/*sh        128*/
     & ' physical review, vol. 144, p 349 (1966).')                             !/*sh        129*/
      end                                                                       !/*sh        130*/
