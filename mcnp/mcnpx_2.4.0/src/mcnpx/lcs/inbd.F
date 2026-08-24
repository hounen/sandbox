!     $Id: inbd.F,v 1.12 2002/06/14 17:05:42 jxh Exp $                          !/*in          1*/
      block data inbd                                                           !/*in          2*/
#include <lcs/double.h>                                                         /*!in          3*/
#ifndef OLDM                                                                    /*!in          4*/
#include <lcs/phprm2.h>                                                         /*!in          5*/
#endif                                                                          /*!in          6*/
#include <lcs/inout.h>                                                          /*!in          7*/
#include <lcs/lptS.h>                                                           /*!in          8*/
#include <lcs/rucom.h>                                                          /*!in          9*/
#include <lcs/charla.h>                                                         /*!in         10*/
#include <lcs/concom.h>                                                         /*!in         11*/
#include <lcs/rdata0.h>                                                         /*!in         12*/
#include <lcs/rancom.h>                                                         /*!in         13*/
      data geomxs /dp0/                                                         !/*in         14*/
      data gauhold /dp0/, bethold /dp0/                                         !/*in         15*/
      data msub /'inh','outh','histp','neutp','bertin','elstin','histin'        !/*in         16*/
     & ,'histina','histinb','histinc','phtlib'/                                 !/*in         17*/
      data mdrp /'        '/                                                    !/*in         18*/
      data isub /ndef*' '/                                                      !/*in         19*/
      data aid /' NEUTP file written by LAHET in RSSA format'/                  !/*in         20*/
      data probid /' '/                                                         !/*in         21*/
! record 4                                                                      !/*in         22*/
      data maxcas /0/                                                           !/*in         23*/
      data maxbch /1/                                                           !/*in         24*/
      data mxmat /0/                                                            !/*in         25*/
      data nlimit /0/                                                           !/*in         26*/
      data neutct /0/                                                           !/*in         27*/
      data lhist /23068672/                                                     !/*inlk        1*/
      data nbmx /256/                                                           !/*in         29*/
      data jcasc /1/                                                            !/*in         30*/
! record 5                                                                      !/*in         31*/
      data ibertp /1/                                                           !/*in         32*/
      data lneutp /1/                                                           !/*in         33*/
      data nhstp /0/                                                            !/*in         34*/
      data mxhist /27/                                                          !/*in         35*/
      data nstrag /1/                                                           !/*in         36*/
      data noskip /10/                                                          !/*in         37*/
! record 5a                                                                     !/*in         38*/
      data krec /4*0,-1,2*0,1/                                                  !/*in         39*/
! record 5b                                                                     !/*in         40*/
      data kpart /lpt*0/                                                        !/*in         41*/
! record 6                                                                      !/*in         42*/
      data nspred /1/                                                           !/*in         43*/
      data n1col /0/                                                            !/*in         44*/
      data nlrnge /0/                                                           !/*in         45*/
      data isrfsr /0/                                                           !/*in         46*/
! record 7                                                                      !/*in         47*/
      data emax /3495.d0/                                                       !/*in         48*/
#ifdef LCS                                                                      /*!in         49*/
      data emin /lpt*dp0/                                                       !/*in         50*/
#endif                                                                          /*!in         51*/
      data itrans /11*1,0,6*1,4*0/                                              !/*in         52*/
      data wtmu /dp1/                                                           !/*in         53*/
      data wtpi /dp1/                                                           !/*in         54*/
      data swtm /dp1/                                                           !/*in         55*/
      data nbertp /28/                                                          !/*in         56*/
      data iscrt /3/                                                            !/*in         57*/
      data nhstin /9/                                                           !/*in         58*/
! other data statements                                                         !/*in         59*/
      data lbb, nbov, nbhw /3*0/                                                !/*in         60*/
      data nbt /lpt*0/                                                          !/*in         61*/
      data wtneut /4*dp0/                                                       !/*in         62*/
      data neutnt /4*0/                                                         !/*in         63*/
      data wtnt2 /4*dp0/                                                        !/*in         64*/
      data wtnt1 /4*dp0/                                                        !/*in         65*/
      data ifisct, fiswt, fiswt1, fiswt2 /0,3*dp0/                              !/*in         66*/
      data sprlm1 /0.1d0/, sprlm2 /0.1d0/                                       !/*in         67*/
      data exlow /dp0,1.0d+30,dp0,-1.0d+30,dp0/, nrnorm /4*0/                   !/*in         68*/
      data negaz /0/                                                            !/*in         69*/
      data lowaz /0/, neutno /4*0/, nobch /0/, negex /0/, nocas /1/             !/*in         70*/
      data charge /dp1,dp0,dp1,dp0,-1.d0,dp1,-1.d0,2*dp1,dp2,dp2,dp0,dp1        !/*in         71*/
     & ,dp0,dp0,-1.d0,-1.d0,dp0,dp1,-1.d0,2*dp0/                                !/*in         72*/
      data instab /2*0,5*1,5*0,4*1,6*0/                                         !/*in         73*/
#ifndef OLDM                                                                    /*!in         74*/
      data pmass /wprot,wneut,wpion,wpi0,                                       !/*in         75*/
     & wpion,2*wmuon,wdeut,wtrit,w_he3,                                         !/*in         76*/
     & walph,dp0,wkaon,2*wka0,wkaon,wprot,                                      !/*in         77*/
     & wneut,2*elmass,2*dp0/                                                    !/*in         78*/
#else                                                                           /*!in         79*/
      data pmass /938.280d0,939.573d0,139.567d0,134.96d0,139.567d0,2*105        !/*in         80*/
     & .66d0,1875.627d0,2808.951d0,2808.421d0,3727.418d0,dp0,493.67d0,2*        !/*in         81*/
     & 497.67d0,493.67d0,938.280d0,939.573d0,2*0.511d0,2*dp0/                   !/*in         82*/
#endif                                                                          /*!in         83*/
      data dkcons /dp0,dp0,0.001281d0,4.0d+05,0.001281d0,1.518d-5,1.518d        !/*in         84*/
     & -5,5*dp0,0.002696d0,0.0006435d0,0.3738d0,0.002696d0,6*dp0/               !/*in         85*/
      data napart /'proton','neutron','pi+','pi0','pi-','mu+','mu-',            !/*in         86*/
     & 'deuteron','triton','3He','alpha','photon','K+','K0 long',               !/*in         87*/
     & 'K0 short','K-','p-bar','n-bar','e+','e-','nu','nu-bar',                 !/*in         88*/
     & 'pi decay','surface'/                                                    !/*in         89*/
      data ncolc /13*0/                                                         !/*in         90*/
      data ehin /3495.d0/                                                       !/*in         91*/
      data ehipi /2495.d0/                                                      !/*in         92*/
      data esminp /50.d0/                                                       !/*in         93*/
      data totels /dp0/                                                         !/*in         94*/
!***********************************************************************        !/*in         95*/
      data peiv /lpt*dp0/                                                       !/*in         96*/
      data nsav /0/                                                             !/*in         97*/
      data rms /dp1,dp0,6.7228d0,dp0,6.7228d0,8.8803d0,8.8803d0,.50025d0        !/*in         98*/
     & ,.33397d0,1.32723d0,dp1,dp0,1.9006d0,2*dp0,1.9006d0,dp1,5*dp0/           !/*in         99*/
      data rmsinv /dp1,dp0,.14875d0,dp0,.14875d0,.11261d0,.11261d0,1.999        !/*in        100*/
     & d0,2.9943d0,.75345d0,dp1,dp0,.52614d0,2*dp0,.52614d0,dp1,5*dp0/          !/*in        101*/
!                                                                               !/*in        102*/
      data mxmel /0/                                                            !/*in        103*/
      data mxelm /0/                                                            !/*in        104*/
!                                                                               !/*in        105*/
      data sring1, sring2, sprob /lring*dp0,lring*dp0,lring*dp0/                !/*in        106*/
      data esrc, eprb /lesrc*dp0,lesrc*dp0/                                     !/*in        107*/
      data tsrc, tprb /ltsrc*dp0,ltsrc*dp0/                                     !/*in        108*/
      data tip0, x_0, y_0, z_0, a_0, b_0, cuts, smu /8*dp0/, isopt /0/,         !/*in        109*/
     & e_0 /8.d2/                                                               !/*in        110*/
      data itopt /-1/, twit /dp0/, tpeak /dp0/, tsig /dp0/                      !/*in        111*/
!REP                                                                            !/*in        112*/
#ifndef OLDM                                                                    /*!in        113*/
      data vmass /wprot,wneut,wpion,wpi0,                                       !/*in        114*/
     & wpion,2*wmuon,wdeut,wtrit,w_he3,                                         !/*in        115*/
     & walph,dp0,wkaon,2*wka0,wkaon,wprot,                                      !/*in        116*/
     & wneut,2*elmass,2*dp0/                                                    !/*in        117*/
#else                                                                           /*!in        118*/
      data vmass /938.280d0,939.573d0,139.567d0,134.96d0,139.567d0,2*105        !/*in        119*/
     & .66d0,1875.627d0,2808.951d0,2808.421d0,3727.418d0,dp0,493.67d0,2*        !/*in        120*/
     & 497.67d0,493.67d0,938.280d0,939.573d0,2*0.511008d0,2*dp0/                !/*in        121*/
#endif                                                                          /*!in        122*/
!                                                                               !/*in        123*/
#ifndef OLDM                                                                    /*!in        124*/
      data lmxs /5*0,2*1,4*1,1,6*0,4*1/                                         !/*in        125*/
#else                                                                           /*!in        126*/
      data lmxs /5*0,2*1,4*1,1,6*1,4*1/                                         !/*in        127*/
#endif                                                                          /*!in        128*/
      end                                                                       !/*in        129*/
