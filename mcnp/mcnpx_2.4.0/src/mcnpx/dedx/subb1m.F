!     $Id: subb1m.F,v 1.5 2002/03/27 18:14:36 gwm Exp $                       !/*su          1*/
      function subb1m (z,betasq)                                                !/*su          2*/
#include <lcs/double.h>                                                         /*!su          3*/
!                                                                               !/*su          4*/
!        purpose :  this function computes the m-shell, s-subshell              !/*su          5*/
!                   correction using the physics discribed in the               !/*su          6*/
!                   reference :                                                 !/*su          7*/
!                                                                               !/*su          8*/
!                     h. bichsel, "stopping power of m electrons                !/*su          9*/
!                     for heavy charged particles," physical review a,          !/*su         10*/
!                     vol. 28, p1147 (1983) and unpublished                     !/*su         11*/
!                     private communication 9 oct 1982.                         !/*su         12*/
!                                                                               !/*su         13*/
!        author :   daniel pickens                                              !/*su         14*/
!                   computer sciences corporation                               !/*su         15*/
!                   10 june 1984                                                !/*su         16*/
!                                                                               !/*su         17*/
!        inputs :   z      - the atomic number of the element.                  !/*su         18*/
!                   betasq - the converted kinetic energy to                    !/*su         19*/
!                            velocity squared.                                  !/*su         20*/
!                                                                               !/*su         21*/
!        outputs :  subb1m - the m-shell, s-subshell correction as              !/*su         22*/
!                            computed using Bichsel method and data.            !/*su         23*/
!                                                                               !/*su         24*/
!        errors :   none.                                                       !/*su         25*/
!                                                                               !/*su         26*/
#include <dedx/block8.h>                                                        /*!su         27*/
      dimension etagd(34), thetgd(8), s(8), t(8), u(8), shelgd(34,8)            !/*su         28*/
      character*3 ildaln                                                        !/*su         29*/
!                                                                               !/*su         30*/
      data etagd /0.0013,0.0016,0.0020,0.0025,0.0032,0.0040,0.0050,0.006        !/*su         31*/
     & 3,0.0079,0.0100,0.0126,0.0158,0.0200,0.0251,0.0316,0.0398,0.0501,        !/*su         32*/
     & 0.0631,0.0794,0.1000,0.1259,0.1585,0.1995,0.2512,0.3162,0.3981,0.        !/*su         33*/
     & 5012,0.6310,0.7943,1.0000,1.2589,1.5849,1.9953,2.5119/                   !/*su         34*/
!                                                                               !/*su         35*/
      data thetgd /0.27,0.315,0.36,0.45,0.54,0.63,0.72,0.81/                    !/*su         36*/
!                                                                               !/*su         37*/
      data s /2.1737,2.0136,1.8891,1.7087,1.5845,1.4941,1.4256,1.3720/          !/*su         38*/
!                                                                               !/*su         39*/
      data t /8.5062,7.8469,7.3328,6.5745,6.0342,5.6239,5.2982,5.0310/          !/*su         40*/
!                                                                               !/*su         41*/
      data u /0.2539,0.2539,0.2541,0.2547,0.2551,0.2553,0.2552,0.2548/          !/*su         42*/
!                                                                               !/*su         43*/
      data v /0.01/                                                             !/*su         44*/
!                                                                               !/*su         45*/
!        theta is 0.27.                                                         !/*su         46*/
!                                                                               !/*su         47*/
      data (shelgd(i,1),i=1,34) /-6.009,-5.508,-5.009,-4.510,-4.013,-3.5        !/*su         48*/
     & 20,-3.033,-2.556,-2.094,-1.651,-1.235,-0.853,-0.511,-0.215,0.030,        !/*su         49*/
     & 0.223,0.366,0.464,0.523,0.550,0.552,0.536,0.507,0.468,0.425,0.380        !/*su         50*/
     & ,0.336,0.293,0.253,0.216,0.182,0.151,0.124,0.101/                        !/*su         51*/
!                                                                               !/*su         52*/
!        theta is 0.315.                                                        !/*su         53*/
!                                                                               !/*su         54*/
      data (shelgd(i,2),i=1,34) /-5.599,-5.135,-4.672,-4.209,-3.747,-3.2        !/*su         55*/
     & 88,-2.833,-2.387,-1.952,-1.535,-1.142,-0.780,-0.456,-0.175,0.058,        !/*su         56*/
     & 0.241,0.377,0.469,0.524,0.549,0.550,0.534,0.504,0.466,0.423,0.379        !/*su         57*/
     & ,0.335,0.292,0.252,0.215,0.181,0.151,0.124,0.101/                        !/*su         58*/
!                                                                               !/*su         59*/
!        theta is 0.360.                                                        !/*su         60*/
!                                                                               !/*su         61*/
      data (shelgd(i,3),i=1,34) /-5.282,-4.847,-4.412,-3.978,-3.544,-3.1        !/*su         62*/
     & 11,-2.682,-2.258,-1.845,-1.448,-1.072,-0.724,-0.413,-0.143,0.082,        !/*su         63*/
     & 0.258,0.388,0.476,0.529,0.552,0.552,0.535,0.505,0.467,0.424,0.379        !/*su         64*/
     & ,0.335,0.293,0.253,0.215,0.181,0.151,0.124,0.101/                        !/*su         65*/
!                                                                               !/*su         66*/
!        theta is 0.45.                                                         !/*su         67*/
!                                                                               !/*su         68*/
      data (shelgd(i,4),i=1,34) /-4.835,-4.442,-4.049,-3.655,-3.262,-2.8        !/*su         69*/
     & 69,-2.478,-2.090,-1.708,-1.337,-0.984,-0.656,-0.359,-0.102,0.112,        !/*su         70*/
     & 0.281,0.405,0.489,0.538,0.559,0.558,0.539,0.508,0.469,0.426,0.381        !/*su         71*/
     & ,0.336,0.294,0.253,0.216,0.182,0.151,0.124,0.101/                        !/*su         72*/
!                                                                               !/*su         73*/
!        theta is 0.54.                                                         !/*su         74*/
!                                                                               !/*su         75*/
      data (shelgd(i,5),i=1,34) /-4.546,-4.181,-3.817,-3.452,-3.087,-2.7        !/*su         76*/
     & 23,-2.359,-1.996,-1.636,-1.284,-0.946,-0.629,-0.340,-0.087,0.123,        !/*su         77*/
     & 0.290,0.412,0.495,0.543,0.563,0.561,0.542,0.510,0.471,0.427,0.382        !/*su         78*/
     & ,0.337,0.294,0.254,0.216,0.182,0.152,0.125,0.101/                        !/*su         79*/
!                                                                               !/*su         80*/
!        theta is 0.63.                                                         !/*su         81*/
!                                                                               !/*su         82*/
      data (shelgd(i,6),i=1,34) /-4.353,-4.009,-3.665,-3.321,-2.977,-2.6        !/*su         83*/
     & 33,-2.290,-1.946,-1.605,-1.267,-0.940,-0.629,-0.343,-0.092,0.119,        !/*su         84*/
     & 0.287,0.410,0.494,0.543,0.563,0.561,0.542,0.511,0.471,0.427,0.382        !/*su         85*/
     & ,0.337,0.294,0.254,0.217,0.182,0.152,0.125,0.101/                        !/*su         86*/
!                                                                               !/*su         87*/
!        theta is 0.72.                                                         !/*su         88*/
!                                                                               !/*su         89*/
      data (shelgd(i,7),i=1,34) /-4.221,-3.893,-3.565,-3.236,-2.908,-2.5        !/*su         90*/
     & 80,-2.252,-1.925,-1.597,-1.272,-0.953,-0.647,-0.363,-0.110,0.104,        !/*su         91*/
     & 0.275,0.401,0.487,0.538,0.560,0.559,0.540,0.509,0.470,0.427,0.382        !/*su         92*/
     & ,0.337,0.294,0.254,0.216,0.182,0.152,0.125,0.101/                        !/*su         93*/
!                                                                               !/*su         94*/
!        theta is 0.81.                                                         !/*su         95*/
!                                                                               !/*su         96*/
      data (shelgd(i,8),i=1,34) /-4.131,-3.815,-3.499,-3.183,-2.867,-2.5        !/*su         97*/
     & 51,-2.236,-1.920,-1.605,-1.291,-0.980,-0.678,-0.394,-0.138,0.081,        !/*su         98*/
     & 0.256,0.387,0.477,0.530,0.554,0.554,0.537,0.507,0.468,0.425,0.380        !/*su         99*/
     & ,0.336,0.294,0.253,0.216,0.182,0.151,0.124,0.101/                        !/*su        100*/
!                                                                               !/*su        101*/
!        initialize the subshell correction value to zero.                      !/*su        102*/
!                                                                               !/*su        103*/
      iz=nint(z)                                                                !/*su        104*/
      subb1m=0.0                                                                !/*su        105*/
!                                                                               !/*su        106*/
!        obtain the theta value, then check the value, if                       !/*su        107*/
!        theta is zero, then return.                                            !/*su        108*/
!                                                                               !/*su        109*/
      thetam=thetas(4,iz)                                                       !/*su        110*/
      if (thetam.le.0.0) return                                                 !/*su        111*/
!                                                                               !/*su        112*/
!        compute the eta value then check its range to determine                !/*su        113*/
!        which computation to use for subb1m.                                   !/*su        114*/
!                                                                               !/*su        115*/
      etaval=eta(z,4,betasq)                                                    !/*su        116*/
!                                                                               !/*su        117*/
!        init old flag to specify new calculation each use                      !/*su        118*/
!        of alin and alin3d.                                                    !/*su        119*/
!                                                                               !/*su        120*/
      ildaln='new'                                                              !/*su        121*/
!                                                                               !/*su        122*/
!        compute subb1m for very low eta values (eta less                       !/*su        123*/
!        than 0.0013).                                                          !/*su        124*/
!        note, b is very small and is approximated by zero in the               !/*su        125*/
!        equation below.                                                        !/*su        126*/
!                                                                               !/*su        127*/
      ngrid=8                                                                   !/*su        128*/
      if (etaval.lt.0.0013) subb1m=alin(thetam,thetgd,s,ngrid,ildaln)           !/*su        129*/
     & *log(etaval)+alin(thetam,thetgd,t,ngrid,ildaln)-0.0                      !/*su        130*/
!                                                                               !/*su        131*/
!        evaluate for the low eta values, eta greater than or                   !/*su        132*/
!        equal to 0.0013 but less than or equal to 2.5119.                      !/*su        133*/
!                                                                               !/*su        134*/
      ngrid=34                                                                  !/*su        135*/
      ngrid2=8                                                                  !/*su        136*/
      if ((etaval.ge.0.0013).and.(etaval.le.2.5119)) subb1m=alin3d              !/*su        137*/
     & (etaval,thetam,etagd,thetgd,shelgd,ngrid,ngrid2,ildaln)                  !/*su        138*/
!                                                                               !/*su        139*/
!        evaluate the s-subshell correction for high eta values.                !/*su        140*/
!                                                                               !/*su        141*/
      ngrid=8                                                                   !/*su        142*/
      if (etaval.gt.2.5119) subb1m=alin(thetam,thetgd,u,ngrid,ildaln)           !/*su        143*/
     & /etaval+v/etaval**2                                                      !/*su        144*/
!                                                                               !/*su        145*/
!        now normalize the whole subshell correction.                           !/*su        146*/
!                                                                               !/*su        147*/
      subb1m=shel(4,iz)*subb1m/2.0                                              !/*su        148*/
!                                                                               !/*su        149*/
      return                                                                    !/*su        150*/
      end                                                                       !/*su        151*/
