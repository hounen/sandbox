!     $Id: prob.F,v 1.4 2002/03/27 18:15:16 gwm Exp $                         !/*pr          1*/
      subroutine prob (itype,einc,pinc,pb)                                      !/*pr          2*/
! PROB       calls...  ERRORF                                                   !/*pr          3*/
!     PROB   called by...   NUCNUC  PINUC                                       !/*pr          4*/
#include <lcs/double.h>                                                         /*!pr          5*/
      dimension pb(5), pz(12), cnu(5), et(5)                                    !/*pr          6*/
      dimension cpnu(3,12), cpk(3,12), cpipnu(3,12), cpipk(3,12), cpimnu        !/*pr          7*/
     & (3,12), cpimk(3,12)                                                      !/*pr          8*/
      data cpnu /-1.3039768d-02,1.3335037d-01,6.7083645d-01,-2.2712350d-        !/*pr          9*/
     & 03,4.0141106d-02,8.7221718d-01,-6.1076880d-04,1.6270638d-02,9.578        !/*pr         10*/
     & 4855d-01,-2.3752451d-04,8.5802078d-03,9.9742126d-01,-7.3343515d-0        !/*pr         11*/
     & 5,3.9319992d-03,1.0303373d00,-1.8943101d-05,1.5830994d-03,1.05568        !/*pr         12*/
     & 31d00,-5.4761767d-06,6.8706274d-04,1.0705557d00,-1.2256205d-06,2.        !/*pr         13*/
     & 5695562d-04,1.0814734d00,-1.9255094d-07,7.5042248d-05,1.0894747d0        !/*pr         14*/
     & 0,-3.3469405d-08,2.3089349d-05,1.0937023d00,-6.9267116d-09,8.0280        !/*pr         15*/
     & 006d-06,1.0958271d00,-1.0377335d-09,2.3245811d-06,1.0972347d00/          !/*pr         16*/
      data cpk /-1.4113426d-02,5.3243160d-01,3.4706497d-01,-3.8183928d-0        !/*pr         17*/
     & 3,4.4261360d-01,5.4272461d-01,-1.6031265d-03,4.1049194d-01,6.5904        !/*pr         18*/
     & 236d-01,-8.7887049d-04,3.9549255d-01,7.3689270d-01,-4.1687489d-04        !/*pr         19*/
     & ,3.8226700d-01,8.3128357d-01,-1.7768145d-04,3.7179470d-01,9.45663        !/*pr         20*/
     & 45d-01,-8.0645084d-05,3.6527634d-01,1.0555573d00,-3.0577183d-05,3        !/*pr         21*/
     & .6011600d-01,1.1883698d00,-9.1493130d-06,3.5625267d-01,1.3625183e        !/*pr         22*/
     & 00,-2.9318035d-06,3.5418224d-01,1.5346680d00,-1.0281801d-06,3.530        !/*pr         23*/
     & 8743d-01,1.6921387d00,-2.9010698d-07,3.5234547d-01,1.8784180d00/         !/*pr         24*/
      data cpipnu /-1.3925135d-02,1.9439220d-01,2.6330948d-02,-4.9353242        !/*pr         25*/
     & d-03,1.1407471d-01,2.0571613d-01,-1.9524097d-03,7.0723534d-02,3.6        !/*pr         26*/
     & 305332d-01,-9.7447634d-04,5.0484657d-02,4.6774292d-01,-4.1649491e        !/*pr         27*/
     & -04,3.4583092d-02,5.8101559d-01,-1.6780943d-04,2.3748994d-02,6.99        !/*pr         28*/
     & 00227d-01,-7.8253448d-05,1.7732978d-02,7.9998779d-01,-3.3177203d-        !/*pr         29*/
     & 05,1.3085365d-02,9.1971684d-01,-1.2346776d-05,9.3094110d-03,1.090        !/*pr         30*/
     & 8327d00,-5.0680246d-06,6.8699718d-03,1.2951612d00,-2.2703025d-06,        !/*pr         31*/
     & 5.2463412d-03,1.5307465d00,-8.7339140d-07,3.8301349d-03,1.8892488        !/*pr         32*/
     & d00/                                                                     !/*pr         33*/
      data cpipk /-5.6201220d-04,1.8275261d-01,-1.3130379d-01,-9.0676546        !/*pr         34*/
     & d-04,1.8500519d-01,-1.3445663d-01,-4.1097403d-04,1.7770004d-01,-1        !/*pr         35*/
     & .0755920d-01,-1.9192696d-04,1.7316246d-01,-8.4091187d-02,-6.65187        !/*pr         36*/
     & 84d-05,1.6959763d-01,-5.8746338d-02,-1.8179417d-05,1.6750813d-01,        !/*pr         37*/
     & -3.6132813d-02,-5.3644180d-06,1.6665268d-01,-2.1896362d-02,-1.199        !/*pr         38*/
     & 5435d-06,1.6622925d-01,-1.1184692d-02,-1.8626451d-07,1.6604996d-0        !/*pr         39*/
     & 1,-3.3721924d-03,-2.9802322d-08,1.6599941d-01,4.8828125d-04,-3.72        !/*pr         40*/
     & 52903d-09,1.6598701d-01,2.6855469d-03,-1.1641532d-09,1.6598129d-0        !/*pr         41*/
     & 1,3.6621094d-03/                                                         !/*pr         42*/
      data cpimnu /-6.8021417d-03,1.0507488d-01,2.5606155d-02,-2.4644732        !/*pr         43*/
     & d-03,6.6451073d-02,1.1157703d-01,-1.0727644d-03,4.6208382d-02,1.8        !/*pr         44*/
     & 512058d-01,-5.9020519d-04,3.6210060d-02,2.3689651d-01,-2.8741732e        !/*pr         45*/
     & -04,2.7553260d-02,2.9872513d-01,-1.3258681d-04,2.0779014d-02,3.72        !/*pr         46*/
     & 83039d-01,-6.7789108d-05,1.6413033d-02,4.4637489d-01,-3.0794181d-        !/*pr         47*/
     & 05,1.2584507d-02,5.4531479d-01,-1.1968659d-05,9.1618299d-03,7.008        !/*pr         48*/
     & 7528d-01,-5.0016679d-06,6.8241954d-03,8.9691067d-01,-2.2565946d-0        !/*pr         49*/
     & 6,5.2303076d-03,1.1282806d00,-8.7132503d-07,3.8254932d-03,1.48402        !/*pr         50*/
     & 40d00/                                                                   !/*pr         51*/
      data cpimk /-9.9664927d-04,1.0531235d-01,-6.1017036d-02,-4.5859814        !/*pr         52*/
     & d-04,1.0030270d-01,-4.9365997d-02,-1.6081333d-04,9.5970154d-02,-3        !/*pr         53*/
     & .3605576d-02,-6.6816807d-05,9.4027519d-02,-2.3590088d-02,-2.14576        !/*pr         54*/
     & 72d-05,9.2741013d-02,-1.4461517d-02,-5.5730343d-06,9.2055321d-02,        !/*pr         55*/
     & -7.0800781d-03,-1.6018748d-06,9.1790199d-02,-2.6702881d-03,-3.576        !/*pr         56*/
     & 2787d-07,9.1663361d-02,5.1879883d-04,-5.2154064d-08,9.1610909d-02        !/*pr         57*/
     & ,2.8533936d-03,-5.8207661d-09,9.1595650d-02,4.2114258d-03,-1.8626        !/*pr         58*/
     & 451d-09,9.1592789d-02,4.7149658d-03,-4.6566129d-10,9.1590405d-02,        !/*pr         59*/
     & 4.7454834d-03/                                                           !/*pr         60*/
      data pz /4.5d0,7.5d0,10.5d0,14.d0,22.d0,34.d0,50.d0,90.d0,170.d0,2        !/*pr         61*/
     & 90.d0,490.d0,1.d3/, sumas /1.079965d0/, tpmas /1.88015d0/                !/*pr         62*/
      data cpn /1.75d0/, cpp /0.875d0/                                          !/*pr         63*/
      data witty /dp2/, cnunn /dp2/, cnunp /dp1/, abc /dph/, witt2 /dp1/        !/*pr         64*/
     & , ab2 /dp1/, aaa /dph/, wit /dp1/, abk /dp1/                             !/*pr         65*/
      data cc3 /dp0/, cc4 /dp0/                                                 !/*pr         66*/
!     witty used in p + p to get nu,n                                           !/*pr         67*/
!     cnunp and cnunn used in n + p to get nu,p and nu,n                        !/*pr         68*/
!     witt2, abc, and ab2 used in pi+ + p to get nu,p, nu,n, and nu,pi+         !/*pr         69*/
!     wit, aaa, abk used in pi- + p to get nu,p, nu,n, and nu,pi-               !/*pr         70*/
!     cc3 used in pi+ + p to get nu,pi-                                         !/*pr         71*/
!     cc4 used in pi- + p to get nu,pi+                                         !/*pr         72*/
!     initialize pb array                                                       !/*pr         73*/
      do 10 i=1,5                                                               !/*pr         74*/
   10 pb(i)=dp0                                                                 !/*pr         75*/
!     find pinc position in pz array to get all coefficients                    !/*pr         76*/
      if (pinc.lt.pz(6)) go to 30                                               !/*pr         77*/
      if (pinc.lt.pz(9)) go to 20                                               !/*pr         78*/
      inx=10                                                                    !/*pr         79*/
      if (pinc.lt.pz(10)) go to 60                                              !/*pr         80*/
      inx=11                                                                    !/*pr         81*/
      if (pinc.gt.pz(11)) inx=12                                                !/*pr         82*/
      go to 60                                                                  !/*pr         83*/
   20 inx=7                                                                     !/*pr         84*/
      if (pinc.lt.pz(7)) go to 60                                               !/*pr         85*/
      inx=8                                                                     !/*pr         86*/
      if (pinc.gt.pz(8)) inx=9                                                  !/*pr         87*/
      go to 60                                                                  !/*pr         88*/
   30 if (pinc.lt.pz(4)) go to 40                                               !/*pr         89*/
      inx=5                                                                     !/*pr         90*/
      if (pinc.gt.pz(5)) inx=6                                                  !/*pr         91*/
      go to 60                                                                  !/*pr         92*/
   40 if (pinc.gt.pz(2)) go to 50                                               !/*pr         93*/
      inx=1                                                                     !/*pr         94*/
      if (pinc.gt.pz(1)) inx=2                                                  !/*pr         95*/
      go to 60                                                                  !/*pr         96*/
   50 inx=3                                                                     !/*pr         97*/
      if (pinc.gt.pz(3)) inx=4                                                  !/*pr         98*/
   60 pinc2=pinc*pinc                                                           !/*pr         99*/
      go to (70,80,110,120,130), itype                                          !/*pr        100*/
!     p + p collisions                                                          !/*pr        101*/
   70 etot=einc+tpmas                                                           !/*pr        102*/
      cnu(1)=pinc2*cpnu(1,inx)+pinc*cpnu(2,inx)+cpnu(3,inx)                     !/*pr        103*/
      cnu(3)=pinc2*cpipnu(1,inx)+pinc*cpipnu(2,inx)+cpipnu(3,inx)               !/*pr        104*/
      cnu(5)=pinc2*cpimnu(1,inx)+pinc*cpimnu(2,inx)+cpimnu(3,inx)               !/*pr        105*/
      et(1)=pinc2*cpk(1,inx)+pinc*cpk(2,inx)+cpk(3,inx)                         !/*pr        106*/
      et(3)=pinc2*cpipk(1,inx)+pinc*cpipk(2,inx)+cpipk(3,inx)                   !/*pr        107*/
      et(5)=pinc2*cpimk(1,inx)+pinc*cpimk(2,inx)+cpimk(3,inx)                   !/*pr        108*/
      dnu1=cnu(1)                                                               !/*pr        109*/
      dnu2=witty-cnu(1)                                                         !/*pr        110*/
      et2=et(1)*dnu2/cnu(1)                                                     !/*pr        111*/
      et1=et(1)                                                                 !/*pr        112*/
      if (pinc.lt.18.d0) go to 100                                              !/*pr        113*/
      cnu(2)=dnu2                                                               !/*pr        114*/
      et(2)=et2                                                                 !/*pr        115*/
      fune=etot-et(1)-et(2)-et(3)-et(5)                                         !/*pr        116*/
      dene=et(3)/cnu(3)                                                         !/*pr        117*/
      go to 140                                                                 !/*pr        118*/
!     n + p collisions                                                          !/*pr        119*/
   80 etot=einc+tpmas                                                           !/*pr        120*/
      cnu(1)=pinc2*cpnu(1,inx)+pinc*cpnu(2,inx)+cpnu(3,inx)                     !/*pr        121*/
      cnu(3)=pinc2*cpipnu(1,inx)+pinc*cpipnu(2,inx)+cpipnu(3,inx)               !/*pr        122*/
      cnu(5)=pinc2*cpimnu(1,inx)+pinc*cpimnu(2,inx)+cpimnu(3,inx)               !/*pr        123*/
      et(1)=pinc2*cpk(1,inx)+pinc*cpk(2,inx)+cpk(3,inx)                         !/*pr        124*/
      et(3)=pinc2*cpipk(1,inx)+pinc*cpipk(2,inx)+cpipk(3,inx)                   !/*pr        125*/
      et(5)=pinc2*cpimk(1,inx)+pinc*cpimk(2,inx)+cpimk(3,inx)                   !/*pr        126*/
      dnu2=cnunn-cnunp                                                          !/*pr        127*/
      et2=et(1)*dnu2/cnu(1)                                                     !/*pr        128*/
      et1=et(1)*cnunp/cnu(1)                                                    !/*pr        129*/
      dnu1=cnunp                                                                !/*pr        130*/
      if (pinc.lt.18.d0) go to 100                                              !/*pr        131*/
      cnu(2)=dnu2                                                               !/*pr        132*/
      et(2)=et2                                                                 !/*pr        133*/
      et(1)=et1                                                                 !/*pr        134*/
      cnu(1)=dnu1                                                               !/*pr        135*/
   90 dene=et(3)/cnu(3)                                                         !/*pr        136*/
      fune=etot-et(1)-et(2)-et(3)-et(5)                                         !/*pr        137*/
      go to 140                                                                 !/*pr        138*/
  100 cnu(2)=cpn-cpp                                                            !/*pr        139*/
      et(2)=et(1)*cnu(2)/cnu(1)                                                 !/*pr        140*/
      et(1)=et(1)*cpp/cnu(1)                                                    !/*pr        141*/
      cnu(1)=cpp                                                                !/*pr        142*/
      fectr=(18.d0-pinc)/15.5d0                                                 !/*pr        143*/
      factr=1.00001d0-fectr                                                     !/*pr        144*/
      cnu(2)=cnu(2)*fectr+dnu2*factr                                            !/*pr        145*/
      cnu(1)=cnu(1)*fectr+dnu1*factr                                            !/*pr        146*/
      et(2)=et(2)*fectr+et2*factr                                               !/*pr        147*/
      et(1)=et(1)*fectr+et1*factr                                               !/*pr        148*/
      go to 90                                                                  !/*pr        149*/
!     pi-plus + p collisions                                                    !/*pr        150*/
  110 etot=einc+sumas                                                           !/*pr        151*/
      cnu(1)=abc                                                                !/*pr        152*/
      cnu(2)=witt2-abc                                                          !/*pr        153*/
      cnu(3)=pinc2*cpipnu(1,inx)+pinc*cpipnu(2,inx)+cpipnu(3,inx)               !/*pr        154*/
      cnu(5)=pinc2*cpimnu(1,inx)+pinc*cpimnu(2,inx)+cpimnu(3,inx)               !/*pr        155*/
      et(3)=pinc2*cpipk(1,inx)+pinc*cpipk(2,inx)+cpipk(3,inx)                   !/*pr        156*/
      et(5)=pinc2*cpimk(1,inx)+pinc*cpimk(2,inx)+cpimk(3,inx)                   !/*pr        157*/
      dene=et(3)/cnu(3)                                                         !/*pr        158*/
      cnu(3)=cnu(3)+ab2                                                         !/*pr        159*/
      et(1)=dene*cnu(1)                                                         !/*pr        160*/
      et(2)=dene*cnu(2)                                                         !/*pr        161*/
      et(3)=dene*cnu(3)                                                         !/*pr        162*/
      et(5)=et(5)*(dp1+cc3/cnu(5))                                              !/*pr        163*/
      cnu(5)=cnu(5)+cc3                                                         !/*pr        164*/
      fune=etot-et(1)-et(2)-et(3)-et(5)                                         !/*pr        165*/
      go to 140                                                                 !/*pr        166*/
!     pi-zero + p collisions                                                    !/*pr        167*/
!     you cant get here from there                                              !/*pr        168*/
  120 call errorf ('prob')                                                      !/*pr        169*/
!     pi-minus + p collisions                                                   !/*pr        170*/
  130 etot=einc+sumas                                                           !/*pr        171*/
      cnu(1)=aaa                                                                !/*pr        172*/
      cnu(2)=wit-aaa                                                            !/*pr        173*/
      cnu(3)=pinc2*cpipnu(1,inx)+pinc*cpipnu(2,inx)+cpipnu(3,inx)               !/*pr        174*/
      cnu(5)=pinc2*cpimnu(1,inx)+pinc*cpimnu(2,inx)+cpimnu(3,inx)               !/*pr        175*/
      et(3)=pinc2*cpipk(1,inx)+pinc*cpipk(2,inx)+cpipk(3,inx)                   !/*pr        176*/
      et(5)=pinc2*cpimk(1,inx)+pinc*cpimk(2,inx)+cpimk(3,inx)                   !/*pr        177*/
      dene=et(3)/cnu(3)                                                         !/*pr        178*/
      et(1)=dene*cnu(1)                                                         !/*pr        179*/
      et(2)=dene*cnu(2)                                                         !/*pr        180*/
      hold=et(5)/cnu(5)                                                         !/*pr        181*/
      cnu(5)=cnu(5)+abk                                                         !/*pr        182*/
      et(5)=hold*cnu(5)                                                         !/*pr        183*/
      et(3)=et(3)*(dp1+cc4/cnu(3))                                              !/*pr        184*/
      cnu(3)=cnu(3)+cc4                                                         !/*pr        185*/
      fune=etot-et(1)-et(2)-et(3)-et(5)                                         !/*pr        186*/
  140 cnu(4)=fune/dene                                                          !/*pr        187*/
      pb(1)=cnu(1)                                                              !/*pr        188*/
      pb(2)=cnu(2)+pb(1)                                                        !/*pr        189*/
      pb(3)=cnu(3)+pb(2)                                                        !/*pr        190*/
      pb(4)=cnu(4)+pb(3)                                                        !/*pr        191*/
      pb(5)=cnu(5)+pb(4)                                                        !/*pr        192*/
      do 150 i=1,5                                                              !/*pr        193*/
  150 pb(i)=pb(i)/pb(5)                                                         !/*pr        194*/
      return                                                                    !/*pr        195*/
      end                                                                       !/*pr        196*/
