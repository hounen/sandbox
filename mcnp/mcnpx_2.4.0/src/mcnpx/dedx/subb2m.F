!     $Id: subb2m.F,v 1.5 2002/03/27 18:14:36 gwm Exp $                       !/*su          1*/
      function subb2m (z,betasq)                                                !/*su          2*/
#include <lcs/double.h>                                                         /*!su          3*/
!                                                                               !/*su          4*/
!        purpose :  this function computes the m-shell, p-subshell              !/*su          5*/
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
!        inputs :   z      - the atomic number of the element.                  !/*su         17*/
!                   betasq - the converted kinetic energy to                    !/*su         18*/
!                            velocity squared.                                  !/*su         19*/
!                                                                               !/*su         20*/
!        outputs :  subb2m - the m-shell, p-subshell correction as              !/*su         21*/
!                            computed using Bichsel method and data.            !/*su         22*/
!                                                                               !/*su         23*/
!        errors :   none.                                                       !/*su         24*/
!                                                                               !/*su         25*/
#include <dedx/block8.h>                                                        /*!su         26*/
      dimension etagd(34), thetgd(7), s(7), t(7), u(7), shelgd(34,7)            !/*su         27*/
      character*3 ildaln                                                        !/*su         28*/
!                                                                               !/*su         29*/
      data etagd /0.0013,0.0016,0.0020,0.0025,0.0032,0.0040,0.0050,0.006        !/*su         30*/
     & 3,0.0079,0.0100,0.0126,0.0158,0.0200,0.0251,0.0316,0.0398,0.0501,        !/*su         31*/
     & 0.0631,0.0794,0.1000,0.1259,0.1585,0.1995,0.2512,0.3162,0.3981,0.        !/*su         32*/
     & 5012,0.6310,0.7943,1.0000,1.2589,1.5849,1.9953,2.5119/                   !/*su         33*/
!                                                                               !/*su         34*/
      data thetgd /0.315,0.36,0.45,0.54,0.63,0.72,0.81/                         !/*su         35*/
!                                                                               !/*su         36*/
      data s /6.4289,5.8995,5.1587,4.6727,4.3349,4.0897,3.9058/                 !/*su         37*/
!                                                                               !/*su         38*/
      data t /26.0020,24.0564,21.2825,19.3686,17.9452,16.8309,15.9256/          !/*su         39*/
!                                                                               !/*su         40*/
      data u /0.6524,0.6431,0.6457,0.6520,0.6546,0.6531,0.6484/                 !/*su         41*/
!                                                                               !/*su         42*/
      data v /0.188/                                                            !/*su         43*/
!                                                                               !/*su         44*/
!        theta is 0.315.                                                        !/*su         45*/
!                                                                               !/*su         46*/
      data (shelgd(i,1),i=1,34) /-16.928,-15.448,-13.969,-12.493,-11.020        !/*su         47*/
     & ,-9.554,-8.100,-6.666,-5.264,-3.908,-2.617,-1.412,-0.318,0.640,1.        !/*su         48*/
     & 442,2.070,2.521,2.799,2.924,2.924,2.827,2.662,2.450,2.207,1.946,1        !/*su         49*/
     & .679,1.417,1.173,0.953,0.763,0.604,0.474,0.371,0.290/                    !/*su         50*/
!                                                                               !/*su         51*/
!        theta is 0.360.                                                        !/*su         52*/
!                                                                               !/*su         53*/
      data (shelgd(i,2),i=1,34) /-15.338,-13.980,-12.623,-11.266,-9.912,        !/*su         54*/
     & -8.563,-7.223,-5.899,-4.600,-3.341,-2.139,-1.016,0.005,0.898,1.64        !/*su         55*/
     & 3,2.222,2.632,2.877,2.975,2.954,2.842,2.667,2.448,2.200,1.937,1.6        !/*su         56*/
     & 69,1.408,1.164,0.945,0.756,0.598,0.470,0.367,0.286/                      !/*su         57*/
!                                                                               !/*su         58*/
!        theta is 0.45.                                                         !/*su         59*/
!                                                                               !/*su         60*/
      data (shelgd(i,3),i=1,34) /-12.165,-11.977,-10.790,-9.603,-8.417,-        !/*su         61*/
     & 7.233,-6.054,-4.884,-3.730,-2.605,-1.524,-0.507,0.420,1.233,1.910        !/*su         62*/
     & ,2.434,2.797,3.004,3.072,3.028,2.897,2.707,2.478,2.223,1.954,1.68        !/*su         63*/
     & 1,1.417,1.171,0.950,0.760,0.601,0.472,0.369,0.288/                       !/*su         64*/
!                                                                               !/*su         65*/
!        theta is 0.54.                                                         !/*su         66*/
!                                                                               !/*su         67*/
      data (shelgd(i,4),i=1,34) /-11.834,-10.758,-9.682,-8.607,-7.532,-6        !/*su         68*/
     & .457,-5.386,-4.319,3.263,-2.225,-1.221,-0.269,0.606,1.379,2.025,2        !/*su         69*/
     & .525,2.870,3.064,3.122,3.069,2.932,2.736,2.502,2.243,1.970,1.695,        !/*su         70*/
     & 1.428,1.180,0.957,0.766,0.606,0.476,0.372,0.290/                         !/*su         71*/
!                                                                               !/*su         72*/
!        theta is 0.63.                                                         !/*su         73*/
!                                                                               !/*su         74*/
      data (shelgd(i,5),i=1,34) /-11.001,-10.003,-9.005,-8.007,-7.009,-6        !/*su         75*/
     & .012,-5.016,-4.023,-3.036,-2.061,-1.110,-0.201,0.644,1.397,2.032,        !/*su         76*/
     & 2.527,2.871,3.066,3.125,3.073,2.937,2.742,2.508,2.248,1.975,1.699        !/*su         77*/
     & ,1.432,1.183,0.960,0.768,0.608,0.477,0.373,0.291/                        !/*su         78*/
!                                                                               !/*su         79*/
!        theta is 0.72.                                                         !/*su         80*/
!                                                                               !/*su         81*/
      data (shelgd(i,6),i=1,34) /-10.478,-9.536,-8.595,-7.653,-6.712,-5.        !/*su         82*/
     & 771,-4.830,-3.891,-2.956,-2.029,-1.118,-0.239,0.587,1.333,1.968,2        !/*su         83*/
     & .470,2.823,3.027,3.095,3.050,2.919,2.728,2.497,2.240,1.969,1.695,        !/*su         84*/
     & 1.428,1.180,0.958,0.766,0.606,0.476,0.372,0.291/                         !/*su         85*/
!                                                                               !/*su         86*/
!        theta is 0.81.                                                         !/*su         87*/
!                                                                               !/*su         88*/
      data (shelgd(i,7),i=1,34) /-10.155,-9.256,-8.357,-7.457,-6.558,-5.        !/*su         89*/
     & 659,-4.761,-3.863,-2.967,-2.077,-1.198,-0.342,0.471,1.214,1.857,2        !/*su         90*/
     & .372,2.740,2.959,3.040,3.006,2.884,2.701,2.475,2.223,1.955,1.683,        !/*su         91*/
     & 1.419,1.173,0.952,0.762,0.603,0.473,0.370,0.289/                         !/*su         92*/
!                                                                               !/*su         93*/
!        initialize the subshell correction value to zero.                      !/*su         94*/
!                                                                               !/*su         95*/
      iz=nint(z)                                                                !/*su         96*/
      subb2m=0.0                                                                !/*su         97*/
!                                                                               !/*su         98*/
!        obtain the theta value, then check the value, if                       !/*su         99*/
!        theta is zero, then return.                                            !/*su        100*/
!                                                                               !/*su        101*/
      thetam=thetas(5,iz)                                                       !/*su        102*/
      if (thetam.le.0.0) return                                                 !/*su        103*/
!                                                                               !/*su        104*/
!        compute the eta value then check its range to determine                !/*su        105*/
!        which computation to use for subb2m.                                   !/*su        106*/
!                                                                               !/*su        107*/
      etaval=eta(z,5,betasq)                                                    !/*su        108*/
!                                                                               !/*su        109*/
!        init old flag to specify new calculation each use                      !/*su        110*/
!        of alin and alin3d.                                                    !/*su        111*/
!                                                                               !/*su        112*/
      ildaln='new'                                                              !/*su        113*/
!                                                                               !/*su        114*/
!        compute subb2m for very low eta values (eta less                       !/*su        115*/
!        than 0.0013).                                                          !/*su        116*/
!        note, b is very small and is approximated by zero in the               !/*su        117*/
!        equation below.                                                        !/*su        118*/
!                                                                               !/*su        119*/
      ngrid=7                                                                   !/*su        120*/
      if (etaval.lt.0.0013) subb2m=alin(thetam,thetgd,s,ngrid,ildaln)           !/*su        121*/
     & *log(etaval)+alin(thetam,thetgd,t,ngrid,ildaln)-0.0                      !/*su        122*/
!                                                                               !/*su        123*/
!        evaluate for the low eta values, eta greater than or                   !/*su        124*/
!        equal to 0.0013 but less than or equal to 2.5119.                      !/*su        125*/
!                                                                               !/*su        126*/
      ngrid=34                                                                  !/*su        127*/
      ngrid2=7                                                                  !/*su        128*/
      if ((etaval.ge.0.0013).and.(etaval.le.2.5119)) subb2m=alin3d              !/*su        129*/
     & (etaval,thetam,etagd,thetgd,shelgd,ngrid,ngrid2,ildaln)                  !/*su        130*/
!                                                                               !/*su        131*/
!        evaluate the p-subshell correction for high eta values.                !/*su        132*/
!                                                                               !/*su        133*/
      ngrid=7                                                                   !/*su        134*/
      if (etaval.gt.2.5119) subb2m=alin(thetam,thetgd,u,ngrid,ildaln)           !/*su        135*/
     & /etaval+v/etaval**2                                                      !/*su        136*/
!                                                                               !/*su        137*/
!        now normalize the whole subshell correction.                           !/*su        138*/
!                                                                               !/*su        139*/
      subb2m=shel(5,iz)*subb2m/6.0                                              !/*su        140*/
!                                                                               !/*su        141*/
      return                                                                    !/*su        142*/
      end                                                                       !/*su        143*/
