!     $Id: shelwl.F,v 1.5 2002/03/27 18:14:36 gwm Exp $                       !/*sh          1*/
      function shelwl (z,betasq,cl1,cl2,ptraut)                                 !/*sh          2*/
#include <lcs/double.h>                                                         /*!sh          3*/
!                                                                               !/*sh          4*/
!        purpose :  this function computes the l-shell correction               !/*sh          5*/
!                   using the physics discribed in reference 130:               !/*sh          6*/
!                                                                               !/*sh          7*/
!                     m. walske, "stopping power of l-electrons,"               !/*sh          8*/
!                     physical review,  vol 101, number 26 (1956).              !/*sh          9*/
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
!        outputs :  shelwl - the l-shell correction as computed using           !/*sh         21*/
!                            Walske method and data.                            !/*sh         22*/
!                   cl1    - the l-shell, s-subshell correction.                !/*sh         23*/
!                   cl2    - the l-shell, p-subshell correction.                !/*sh         24*/
!                                                                               !/*sh         25*/
!        errors :   none.                                                       !/*sh         26*/
!                                                                               !/*sh         27*/
#include <dedx/block8.h>                                                        /*!sh         28*/
      dimension etagd1(5), thtgd(4), shllgd(6,4), s(4), t(4), u(4),             !/*sh         29*/
     & etagd2(6), b(5,4)                                                        !/*sh         30*/
      character*3 ildaln                                                        !/*sh         31*/
      logical ptraut                                                            !/*sh         32*/
!                                                                               !/*sh         33*/
      data etagd1 /0.0,0.25,0.50,0.75,1.0/                                      !/*sh         34*/
!                                                                               !/*sh         35*/
      data thtgd /0.35,0.45,0.55,0.65/                                          !/*sh         36*/
!                                                                               !/*sh         37*/
      data s /10.0371,7.9116,6.7451,6.0345/                                     !/*sh         38*/
!                                                                               !/*sh         39*/
      data t /28.1449,24.4501,21.9061,20.0154/                                  !/*sh         40*/
!                                                                               !/*sh         41*/
      data u /1.5032,1.8756,1.9890,2.0040/                                      !/*sh         42*/
!                                                                               !/*sh         43*/
      data etagd2 /1.0,3.5,10.0,25.0,100.0,1000.0/                              !/*sh         44*/
!                                                                               !/*sh         45*/
      data (shllgd(i,1),i=1,6) /1.6934,0.4917,0.1619,0.06235,0.01516,0.0        !/*sh         46*/
     & 01505/                                                                   !/*sh         47*/
      data (shllgd(i,2),i=1,6) /2.0298,0.5950,0.1988,0.07719,0.01888,0.0        !/*sh         48*/
     & 01877/                                                                   !/*sh         49*/
      data (shllgd(i,3),i=1,6) /2.1358,0.6268,0.2100,0.08171,0.02001,0.0        !/*sh         50*/
     & 01991/                                                                   !/*sh         51*/
      data (shllgd(i,4),i=1,6) /2.1519,0.6312,0.2116,0.08232,0.02016,0.0        !/*sh         52*/
     & 02006/                                                                   !/*sh         53*/
!                                                                               !/*sh         54*/
      data b /0.0,11.0,18.40,23.10,26.52,0.0,9.80,15.60,19.70,22.48,0.0,        !/*sh         55*/
     & 7.90,13.70,17.10,19.8,0.0,6.80,12.20,15.50,17.89/                        !/*sh         56*/
!                                                                               !/*sh         57*/
!        initialize.                                                            !/*sh         58*/
!                                                                               !/*sh         59*/
      iz=nint(z)                                                                !/*sh         60*/
      cl1=0.0                                                                   !/*sh         61*/
      cl2=0.0                                                                   !/*sh         62*/
      shelwl=0.0                                                                !/*sh         63*/
!                                                                               !/*sh         64*/
!        first check to make sure that there are electrons in this              !/*sh         65*/
!        shell. if there are no electrons, return.                              !/*sh         66*/
!                                                                               !/*sh         67*/
      if ((shel(2,iz).eq.0.0).and.(shel(3,iz).eq.0.0)) return                   !/*sh         68*/
!                                                                               !/*sh         69*/
!        compute the average theta value. then, check the value,                !/*sh         70*/
!        if the theta is zero, then return                                      !/*sh         71*/
!                                                                               !/*sh         72*/
      thetal=((shel(2,iz)*thetas(2,iz))+(shel(3,iz)*thetas(3,iz)))/(shel        !/*sh         73*/
     & (2,iz)+shel(3,iz))                                                       !/*sh         74*/
!                                                                               !/*sh         75*/
      if (thetal.le.0.0) return                                                 !/*sh         76*/
!                                                                               !/*sh         77*/
!        compute the average eta value then check its range to                  !/*sh         78*/
!        determine which computation to use for shelwl.                         !/*sh         79*/
!                                                                               !/*sh         80*/
      etaval=((eshel(2,iz)*eta(z,2,betasq))+(eshel(3,iz)*eta(z,3,betasq)        !/*sh         81*/
     & ))/(eshel(2,iz)+eshel(3,iz))                                             !/*sh         82*/
!                                                                               !/*sh         83*/
!        init old flag to specify new calculation each use                      !/*sh         84*/
!        of alin and alin3d.                                                    !/*sh         85*/
!                                                                               !/*sh         86*/
      ildaln='new'                                                              !/*sh         87*/
!                                                                               !/*sh         88*/
!        evaluate for the low eta values (eta less than 1.0).                   !/*sh         89*/
!                                                                               !/*sh         90*/
      if ((etaval.lt.1.0).and.(etaval.ge.0.0)) then                             !/*sh         91*/
!                                                                               !/*sh         92*/
!        compute the 'b' value first.                                           !/*sh         93*/
!        interpolate to obtain the b value since data is available.             !/*sh         94*/
!                                                                               !/*sh         95*/
        ngrid=5                                                                 !/*sh         96*/
        ngrid2=4                                                                !/*sh         97*/
        bvalue=alin3d(etaval,thetal,etagd1,thtgd,b,ngrid,ngrid2,ildaln)         !/*sh         98*/
!                                                                               !/*sh         99*/
!        actual computation of the low eta l-shell correction.                  !/*sh        100*/
!                                                                               !/*sh        101*/
        shelwl=alin(thetal,thtgd,s,ngrid2,ildaln)*log(etaval)+alin              !/*sh        102*/
     &  (thetal,thtgd,t,ngrid2,ildaln)-bvalue                                   !/*sh        103*/
!                                                                               !/*sh        104*/
      endif                                                                     !/*sh        105*/
!                                                                               !/*sh        106*/
!        evaluate the l-shell correction medium eta values.                     !/*sh        107*/
!                                                                               !/*sh        108*/
      ngrid=6                                                                   !/*sh        109*/
      ngrid2=4                                                                  !/*sh        110*/
      if ((etaval.ge.1.0).and.(etaval.le.1000.0)) shelwl=alin3d(etaval          !/*sh        111*/
     & ,thetal,etagd2,thtgd,shllgd,ngrid,ngrid2,ildaln)                         !/*sh        112*/
!                                                                               !/*sh        113*/
!        evaluate the l-shell correction for high eta values.                   !/*sh        114*/
!                                                                               !/*sh        115*/
      ngrid=4                                                                   !/*sh        116*/
      if (etaval.gt.1000.0) shelwl=alin(thetal,thtgd,u,ngrid,ildaln)            !/*sh        117*/
     & /etaval                                                                  !/*sh        118*/
!                                                                               !/*sh        119*/
!        write out the author and reference information to                      !/*sh        120*/
!        scratch file if ptraut is true.                                        !/*sh        121*/
      if (ptraut) write (10,10)                                                 !/*sh        122*/
!                                                                               !/*sh        123*/
      return                                                                    !/*sh        124*/
!                                                                               !/*sh        125*/
   10 format ('l-shell -  walske''s total shell method, ',                      !/*sh        126*/
     & 'physical review, vol. 101 (1956).')                                     !/*sh        127*/
      end                                                                       !/*sh        128*/
