!     $Id: subb3m.F,v 1.5 2002/03/27 18:14:37 gwm Exp $                       !/*su          1*/
      function subb3m (z,betasq)                                                !/*su          2*/
#include <lcs/double.h>                                                         /*!su          3*/
!                                                                               !/*su          4*/
!        purpose :  this function computes the m-shell, d-subshell              !/*su          5*/
!                   correction using the physics discribed in the               !/*su          6*/
!                   reference :                                                 !/*su          7*/
!                                                                               !/*su          8*/
!                     h. bichsel, "stopping power of m electrons                !/*su          9*/
!                     for heavy charged particles," physical review a,          !/*su         10*/
!                     vol. 28, p1147 (1983)                                     !/*su         11*/
!                                                                               !/*su         12*/
!        author :   daniel pickens                                              !/*su         13*/
!                   computer sciences corporation                               !/*su         14*/
!                   10 june 1984                                                !/*su         15*/
!                                                                               !/*su         16*/
!        inputs :   z     - the atomic number of the element.                   !/*su         17*/
!                   betasq - the converted kinetic energy to                    !/*su         18*/
!                            velocity squared.                                  !/*su         19*/
!                                                                               !/*su         20*/
!        outputs :  subb3m - the m-shell, d-subshell correction as              !/*su         21*/
!                            computed using Bichsel method and data.            !/*su         22*/
!                                                                               !/*su         23*/
!        errors :   none.                                                       !/*su         24*/
!                                                                               !/*su         25*/
#include <dedx/block8.h>                                                        /*!su         26*/
      dimension etagd(30), thetgd(6), s(6), t(6), u(6), shelgd(30,6)            !/*su         27*/
      character*3 ildaln                                                        !/*su         28*/
!                                                                               !/*su         29*/
      data etagd /0.0013,0.0016,0.0020,0.0025,0.0032,0.0040,0.0050,0.006        !/*su         30*/
     & 3,0.0079,0.0100,0.0126,0.0158,0.0200,0.0251,0.0316,0.0398,0.0501,        !/*su         31*/
     & 0.0631,0.0794,0.1000,0.1259,0.1585,0.1995,0.2512,0.3162,0.3981,0.        !/*su         32*/
     & 5012,0.6310,0.7943,1.0000/                                               !/*su         33*/
!                                                                               !/*su         34*/
      data thetgd /0.36,0.45,0.54,0.63,0.72,0.81/                               !/*su         35*/
!                                                                               !/*su         36*/
      data s /10.4696,8.3353,7.1929,6.5217,6.0999,5.8209/                       !/*su         37*/
!                                                                               !/*su         38*/
      data t /45.6579,39.4198,35.2355,32.2033,29.8843,28.0388/                  !/*su         39*/
!                                                                               !/*su         40*/
      data u /1.0397,1.1432,1.1312,1.0848,1.0310,0.9791/                        !/*su         41*/
!                                                                               !/*su         42*/
      data v /0.138/                                                            !/*su         43*/
!                                                                               !/*su         44*/
!        theta is 0.360.                                                        !/*su         45*/
!                                                                               !/*su         46*/
      data (shelgd(i,1),i=1,30) /-24.253,-21.844,-19.437,-17.033,-14.633        !/*su         47*/
     & ,-12.240,-9.860,-7.499,-5.170,-2.889,-0.683,1.414,3.361,5.105,6.5        !/*su         48*/
     & 90,7.760,8.563,8.969,8.971,8.600,7.921,7.027,6.022,5.007,4.058,3.        !/*su         49*/
     & 224,2.526,1.963,1.520,1.176/                                             !/*su         50*/
!                                                                               !/*su         51*/
!        theta is 0.45.                                                         !/*su         52*/
!                                                                               !/*su         53*/
      data (shelgd(i,2),i=1,30) /-16.239,-14.321,-12.403,-10.487,-8.573,        !/*su         54*/
     & -6.665,-4.764,-2.881,-1.009,0.824,2.604,4.301,5.876,7.281,8.460,9        !/*su         55*/
     & .355,9.916,10.109,9.927,9.396,8.580,7.570,6.468,5.370,4.354,3.464        !/*su         56*/
     & ,2.720,2.119,1.645,1.276/                                                !/*su         57*/
!                                                                               !/*su         58*/
!        theta is 0.54.                                                         !/*su         59*/
!                                                                               !/*su         60*/
      data (shelgd(i,3),i=1,30) /-12.795,-11.139,-9.483,-7.829,-6.175,-4        !/*su         61*/
     & .526,-2.881,-1.246,0.375,1.971,3.530,5.025,6.423,7.678,8.736,9.53        !/*su         62*/
     & 6,10.026,10.168,9.950,9.397,8.568,7.550,6.445,5.347,4.332,3.444,2        !/*su         63*/
     & .703,2.104,1.633,1.266/                                                  !/*su         64*/
!                                                                               !/*su         65*/
!        theta is 0.63.                                                         !/*su         66*/
!                                                                               !/*su         67*/
      data (shelgd(i,4),i=1,30) /-11.345,-9.844,-8.342,-6.841,-5.341,-3.        !/*su         68*/
     & 843,-2.349,-0.861,0.615,2.074,3.504,4.887,6.192,7.379,8.394,9.175        !/*su         69*/
     & ,9.667,9.825,9.635,9.115,8.321,7.338,6.265,5.197,4.208,3.342,2.62        !/*su         70*/
     & 0,2.037,1.578,1.222/                                                     !/*su         71*/
!                                                                               !/*su         72*/
!        theta is 0.72.                                                         !/*su         73*/
!                                                                               !/*su         74*/
      data (shelgd(i,5),i=1,30) /-10.848,-9.443,-8.039,-6.635,-5.231,-3.        !/*su         75*/
     & 828,-2.428,-1.033,0.354,1.727,3.077,4.391,5.644,6.800,7.806,8.603        !/*su         76*/
     & ,9.129,9.335,9.199,8.736,7.997,7.066,6.039,5.011,4.055,3.218,2.51        !/*su         77*/
     & 9,1.956,1.513,1.171/                                                     !/*su         78*/
!                                                                               !/*su         79*/
!        theta is 0.81.                                                         !/*su         80*/
!                                                                               !/*su         81*/
      data (shelgd(i,6),i=1,30) /-10.830,-9.490,-8.150,-6.809,-5.469,-4.        !/*su         82*/
     & 130,-2.792,-1.458,-0.130,1.187,2.485,3.755,4.976,6.117,7.131,7.95        !/*su         83*/
     & 8,8.532,8.799,8.730,8.334,7.659,6.785,5.808,4.822,3.902,3.095,2.4        !/*su         84*/
     & 20,1.877,1.450,1.120/                                                    !/*su         85*/
!                                                                               !/*su         86*/
!        initialize the subshell correction value to zero.                      !/*su         87*/
!                                                                               !/*su         88*/
      iz=nint(z)                                                                !/*su         89*/
      subb3m=0.0                                                                !/*su         90*/
!                                                                               !/*su         91*/
!        obtain the theta value, then check the value, if                       !/*su         92*/
!        the theta is zero, return.                                             !/*su         93*/
!                                                                               !/*su         94*/
      thetam=thetas(6,iz)                                                       !/*su         95*/
      if (thetam.le.0.0) return                                                 !/*su         96*/
!                                                                               !/*su         97*/
!        compute the eta value then check its range to determine                !/*su         98*/
!        which computation to use for subb3m.                                   !/*su         99*/
!                                                                               !/*su        100*/
      etaval=eta(z,6,betasq)                                                    !/*su        101*/
!                                                                               !/*su        102*/
!        init old flag to specify new calculation each use                      !/*su        103*/
!        of alin and alin3d.                                                    !/*su        104*/
!                                                                               !/*su        105*/
      ildaln='new'                                                              !/*su        106*/
!                                                                               !/*su        107*/
!        compute subb3m for very low eta values (eta less                       !/*su        108*/
!        than 0.0013).                                                          !/*su        109*/
!        note, b is very small and is approximated by zero in the               !/*su        110*/
!        equation below.                                                        !/*su        111*/
!                                                                               !/*su        112*/
      ngrid=6                                                                   !/*su        113*/
      if (etaval.lt.0.0013) subb3m=alin(thetam,thetgd,s,ngrid,ildaln)           !/*su        114*/
     & *log(etaval)+alin(thetam,thetgd,t,ngrid,ildaln)-0.0                      !/*su        115*/
!                                                                               !/*su        116*/
!        evaluate for the low eta values, eta greater than or                   !/*su        117*/
!        equal to 0.0013 but less than or equal to 1.000.                       !/*su        118*/
!                                                                               !/*su        119*/
      ngrid=30                                                                  !/*su        120*/
      ngrid2=6                                                                  !/*su        121*/
      if ((etaval.ge.0.0013).and.(etaval.le.1.0000)) subb3m=alin3d              !/*su        122*/
     & (etaval,thetam,etagd,thetgd,shelgd,ngrid,ngrid2,ildaln)                  !/*su        123*/
!                                                                               !/*su        124*/
!        evaluate the d-subshell correction for high eta values.                !/*su        125*/
!                                                                               !/*su        126*/
      ngrid=6                                                                   !/*su        127*/
      if (etaval.gt.1.0000) subb3m=alin(thetam,thetgd,u,ngrid,ildaln)           !/*su        128*/
     & /etaval+v/etaval**2                                                      !/*su        129*/
!                                                                               !/*su        130*/
!        now normalize the whole subshell correction.                           !/*su        131*/
!                                                                               !/*su        132*/
      subb3m=shel(6,iz)*subb3m/10.0                                             !/*su        133*/
!                                                                               !/*su        134*/
      return                                                                    !/*su        135*/
      end                                                                       !/*su        136*/
