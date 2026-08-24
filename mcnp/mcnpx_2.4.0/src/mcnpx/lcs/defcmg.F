!     $Id: defcmg.F,v 1.4 2002/03/27 18:15:00 gwm Exp $                       !/*de          1*/
      subroutine defcmg                                                         !/*de          2*/
#include <lcs/double.h>                                                         /*!de          3*/
      parameter (pi=3.141593d0, tenpi=1.d1*pi, rcomp=1.23d0)                    !/*de          4*/
#include <lcs/cmgcom.h>                                                         /*!de          5*/
      data ecomp /25.d0/                                                        !/*de          6*/
      it=itt(1)                                                                 !/*de          7*/
      p0(1)=dp0                                                                 !/*de          8*/
      p1(1)=dp0                                                                 !/*de          9*/
      p2(1)=dp0                                                                 !/*de         10*/
      mu2(1)=dp0                                                                !/*de         11*/
      mu3(1)=dp0                                                                !/*de         12*/
      lamda2(1)=dp0                                                             !/*de         13*/
      lamda3(1)=dp0                                                             !/*de         14*/
      if (it.eq.1) then                                                         !/*de         15*/
! Neutron potential: Mani et al. (1963)                                         !/*de         16*/
        lamda0(1)=15.3d0                                                        !/*de         17*/
        lamda1(1)=-14.98d0                                                      !/*de         18*/
        mu0(1)=297.7d0                                                          !/*de         19*/
        mu1(1)=30.58d0                                                          !/*de         20*/
        nu0(1)=0.32d0                                                           !/*de         21*/
        nu1(1)=-47.08d0                                                         !/*de         22*/
        nu2(1)=74.19d0                                                          !/*de         23*/
      elseif (it.eq.2) then                                                     !/*de         24*/
! Neutron potential: Becchetti and Greenlees (1969)                             !/*de         25*/
        lamda0(1)=18.57d0                                                       !/*de         26*/
        lamda1(1)=-22.93d0                                                      !/*de         27*/
        mu0(1)=381.7d0                                                          !/*de         28*/
        mu1(1)=24.31d0                                                          !/*de         29*/
        nu0(1)=0.172d0                                                          !/*de         30*/
        nu1(1)=-15.39d0                                                         !/*de         31*/
        nu2(1)=804.8d0                                                          !/*de         32*/
      elseif (it.eq.3) then                                                     !/*de         33*/
! Neutron potential: Wilmore and Hodgson (1964)                                 !/*de         34*/
        lamda0(1)=31.05d0                                                       !/*de         35*/
        lamda1(1)=-25.91d0                                                      !/*de         36*/
        mu0(1)=342.4d0                                                          !/*de         37*/
        mu1(1)=21.89d0                                                          !/*de         38*/
        nu0(1)=0.223d0                                                          !/*de         39*/
        nu1(1)=0.673d0                                                          !/*de         40*/
        nu2(1)=617.4d0                                                          !/*de         41*/
      elseif (it.eq.4) then                                                     !/*de         42*/
! Neutron potential: Mani et al. (PRECO) DEFAULT                                !/*de         43*/
        p0(1)=-312.d0                                                           !/*de         44*/
        lamda0(1)=12.1d0                                                        !/*de         45*/
        lamda1(1)=-11.27d0                                                      !/*de         46*/
        mu0(1)=234.1d0                                                          !/*de         47*/
        mu1(1)=38.26d0                                                          !/*de         48*/
        nu0(1)=1.55d0                                                           !/*de         49*/
        nu1(1)=-106.1d0                                                         !/*de         50*/
        nu2(1)=1280.8d0                                                         !/*de         51*/
      endif                                                                     !/*de         52*/
      it=itt(2)                                                                 !/*de         53*/
      if (it.eq.1) then                                                         !/*de         54*/
! Proton potential: Mani et al. (1963)                                          !/*de         55*/
        p0(2)=12.97d0                                                           !/*de         56*/
        p1(2)=15.65d0                                                           !/*de         57*/
        p2(2)=-348.5d0                                                          !/*de         58*/
        lamda0(2)=0.0247d0                                                      !/*de         59*/
        lamda1(2)=-7.01d0                                                       !/*de         60*/
        mu0(2)=300.4d0                                                          !/*de         61*/
        mu1(2)=0.40d0                                                           !/*de         62*/
        nu0(2)=330.d0                                                           !/*de         63*/
        nu1(2)=-223.6d0                                                         !/*de         64*/
        nu2(2)=-3.251d0                                                         !/*de         65*/
      elseif (it.eq.2) then                                                     !/*de         66*/
! Proton potential: Perey (1963)                                                !/*de         67*/
        p0(2)=10.53d0                                                           !/*de         68*/
        p1(2)=52.53d0                                                           !/*de         69*/
        p2(2)=-414.d0                                                           !/*de         70*/
        lamda0(2)=0.031d0                                                       !/*de         71*/
        lamda1(2)=-10.16d0                                                      !/*de         72*/
        mu0(2)=327.9d0                                                          !/*de         73*/
        mu1(2)=0.40d0                                                           !/*de         74*/
        nu0(2)=323.7d0                                                          !/*de         75*/
        nu1(2)=-243.4d0                                                         !/*de         76*/
        nu2(2)=-3.037d0                                                         !/*de         77*/
      elseif (it.eq.3) then                                                     !/*de         78*/
! Proton potential: Becchetti and Greenlees (1969) DEFAULT                      !/*de         79*/
        p0(2)=15.72d0                                                           !/*de         80*/
        p1(2)=9.65d0                                                            !/*de         81*/
        p2(2)=-449.d0                                                           !/*de         82*/
        lamda0(2)=0.00437d0                                                     !/*de         83*/
        lamda1(2)=-16.58d0                                                      !/*de         84*/
        mu0(2)=244.7d0                                                          !/*de         85*/
        mu1(2)=0.503d0                                                          !/*de         86*/
        nu0(2)=273.1d0                                                          !/*de         87*/
        nu1(2)=-182.4d0                                                         !/*de         88*/
        nu2(2)=-1.872d0                                                         !/*de         89*/
      elseif (it.eq.4) then                                                     !/*de         90*/
! Proton potential: Menet et al. (1971)                                         !/*de         91*/
        p0(2)=16.99d0                                                           !/*de         92*/
        p1(2)=46.13d0                                                           !/*de         93*/
        p2(2)=-1298.d0                                                          !/*de         94*/
        lamda0(2)=-0.0795d0                                                     !/*de         95*/
        lamda1(2)=-11.9d0                                                       !/*de         96*/
        mu0(2)=127.8d0                                                          !/*de         97*/
        mu1(2)=0.66d0                                                           !/*de         98*/
        nu0(2)=306.8d0                                                          !/*de         99*/
        nu1(2)=-118.5d0                                                         !/*de        100*/
        nu2(2)=0.225d0                                                          !/*de        101*/
      endif                                                                     !/*de        102*/
      it=itt(3)                                                                 !/*de        103*/
      if (it.eq.1) then                                                         !/*de        104*/
! Deuteron potential: Schwandt and Haeberli (1969)                              !/*de        105*/
        p0(3)=-43.73d0                                                          !/*de        106*/
        p1(3)=934.1d0                                                           !/*de        107*/
        p2(3)=-2518.d0                                                          !/*de        108*/
        lamda0(3)=-0.0761d0                                                     !/*de        109*/
        lamda1(3)=-8.83d0                                                       !/*de        110*/
        mu0(3)=337.5d0                                                          !/*de        111*/
        mu1(3)=0.49d0                                                           !/*de        112*/
        nu0(3)=417.5d0                                                          !/*de        113*/
        nu1(3)=-353.d0                                                          !/*de        114*/
        nu2(3)=5.515d0                                                          !/*de        115*/
      elseif (it.eq.2) then                                                     !/*de        116*/
! Deuteron potential: Lohr and Haeberli (1974)                                  !/*de        117*/
        p0(3)=-38.21d0                                                          !/*de        118*/
        p1(3)=922.6d0                                                           !/*de        119*/
        p2(3)=-2804.d0                                                          !/*de        120*/
        lamda0(3)=-0.0323d0                                                     !/*de        121*/
        lamda1(3)=-5.48d0                                                       !/*de        122*/
        mu0(3)=336.1d0                                                          !/*de        123*/
        mu1(3)=0.48d0                                                           !/*de        124*/
        nu0(3)=524.3d0                                                          !/*de        125*/
        nu1(3)=-371.8d0                                                         !/*de        126*/
        nu2(3)=-5.924d0                                                         !/*de        127*/
      elseif (it.eq.3) then                                                     !/*de        128*/
! Deuteron potential: Perey and Perey (from PRECO) DEFAULT                      !/*de        129*/
        p0(3)=0.798d0                                                           !/*de        130*/
        p1(3)=420.3d0                                                           !/*de        131*/
        p2(3)=-1651.d0                                                          !/*de        132*/
        lamda0(3)=0.00619d0                                                     !/*de        133*/
        lamda1(3)=-7.54d0                                                       !/*de        134*/
        mu0(3)=583.5d0                                                          !/*de        135*/
        mu1(3)=0.337d0                                                          !/*de        136*/
        nu0(3)=421.8d0                                                          !/*de        137*/
        nu1(3)=-474.5d0                                                         !/*de        138*/
        nu2(3)=-3.592d0                                                         !/*de        139*/
      elseif (it.eq.4) then                                                     !/*de        140*/
! Deuteron potential: Lohr and Haeberli (modified)                              !/*de        141*/
        p0(3)=-38.21d0                                                          !/*de        142*/
        p1(3)=922.6d0                                                           !/*de        143*/
        p2(3)=-2804.d0                                                          !/*de        144*/
        lamda0(3)=-0.0323d0                                                     !/*de        145*/
        lamda1(3)=-5.48d0                                                       !/*de        146*/
        mu0(3)=336.1d0                                                          !/*de        147*/
        mu1(3)=0.48d0                                                           !/*de        148*/
        nu0(3)=524.3d0                                                          !/*de        149*/
        nu1(3)=-371.8d0                                                         !/*de        150*/
!        nu2(3)   =-5.924d0                                                     !/*de        151*/
        nu2(3)=+5.924d0                                                         !/*de        152*/
      endif                                                                     !/*de        153*/
      it=itt(4)                                                                 !/*de        154*/
      if (it.eq.1) then                                                         !/*de        155*/
! Triton potential: Becchetti and Greenlees (1971)                              !/*de        156*/
        p0(4)=-11.04d0                                                          !/*de        157*/
        p1(4)=619.1d0                                                           !/*de        158*/
        p2(4)=-2147.d0                                                          !/*de        159*/
        lamda0(4)=-0.0426d0                                                     !/*de        160*/
        lamda1(4)=-10.33d0                                                      !/*de        161*/
        mu0(4)=601.9d0                                                          !/*de        162*/
        mu1(4)=0.37d0                                                           !/*de        163*/
        nu0(4)=583.d0                                                           !/*de        164*/
        nu1(4)=-546.2d0                                                         !/*de        165*/
        nu2(4)=1.718d0                                                          !/*de        166*/
      else                                                                      !/*de        167*/
! Triton potential: Hafele, Flynn, et al (from PRECO) DEFAULT                   !/*de        168*/
        p0(4)=-21.45d0                                                          !/*de        169*/
        p1(4)=484.7d0                                                           !/*de        170*/
        p2(4)=-1608.d0                                                          !/*de        171*/
        lamda0(4)=0.0186d0                                                      !/*de        172*/
        lamda1(4)=-8.9d0                                                        !/*de        173*/
        mu0(4)=686.3d0                                                          !/*de        174*/
        mu1(4)=0.325d0                                                          !/*de        175*/
        nu0(4)=368.9d0                                                          !/*de        176*/
        nu1(4)=-522.2d0                                                         !/*de        177*/
        nu2(4)=-4.998d0                                                         !/*de        178*/
      endif                                                                     !/*de        179*/
      it=itt(5)                                                                 !/*de        180*/
      if (it.eq.1) then                                                         !/*de        181*/
! Tau potential: Becchetti and Greenlees (1971)                                 !/*de        182*/
        p0(5)=-3.06d0                                                           !/*de        183*/
        p1(5)=278.5d0                                                           !/*de        184*/
        p2(5)=-1389.d0                                                          !/*de        185*/
        lamda0(5)=-0.00535d0                                                    !/*de        186*/
        lamda1(5)=-11.16d0                                                      !/*de        187*/
        mu0(5)=555.5d0                                                          !/*de        188*/
        mu1(5)=0.40d0                                                           !/*de        189*/
        nu0(5)=687.4d0                                                          !/*de        190*/
        nu1(5)=-476.3d0                                                         !/*de        191*/
        nu2(5)=0.509d0                                                          !/*de        192*/
      else                                                                      !/*de        193*/
! Tau potential: Gibson et al (from PRECO) DEFAULT                              !/*de        194*/
        p0(5)=-2.88d0                                                           !/*de        195*/
        p1(5)=205.6d0                                                           !/*de        196*/
        p2(5)=-1487.d0                                                          !/*de        197*/
        lamda0(5)=0.00459d0                                                     !/*de        198*/
        lamda1(5)=-8.93d0                                                       !/*de        199*/
        mu0(5)=611.2d0                                                          !/*de        200*/
        mu1(5)=0.35d0                                                           !/*de        201*/
        nu0(5)=473.8d0                                                          !/*de        202*/
        nu1(5)=-468.2d0                                                         !/*de        203*/
        nu2(5)=-2.225d0                                                         !/*de        204*/
      endif                                                                     !/*de        205*/
! Alpha potential: Huizenga and Igo (1961)                                      !/*de        206*/
      p0(6)=10.95d0                                                             !/*de        207*/
      p1(6)=-85.2d0                                                             !/*de        208*/
      p2(6)=1146.d0                                                             !/*de        209*/
      lamda0(6)=0.0643d0                                                        !/*de        210*/
      lamda1(6)=-13.96d0                                                        !/*de        211*/
      mu0(6)=781.2d0                                                            !/*de        212*/
      mu1(6)=0.29d0                                                             !/*de        213*/
      nu0(6)=-304.7d0                                                           !/*de        214*/
      nu1(6)=-470.d0                                                            !/*de        215*/
      nu2(6)=-8.58d0                                                            !/*de        216*/
      acrit=-lamda1(6)/lamda0(6)                                                !/*de        217*/
      rad0=sqrt(mu0(6)/tenpi)*acrit**(dph*mu1(6))                               !/*de        218*/
      rcompa=rad0/acrit**dpth                                                   !/*de        219*/
      rcompb=(rad0-delta(6))/acrit**dpth                                        !/*de        220*/
      deltab=rad0-rcomp*acrit**dpth                                             !/*de        221*/
      rfix1=(deltab-delta(6))/acrit**dp2th                                      !/*de        222*/
      rfix2=(deltab-delta(6))/acrit                                             !/*de        223*/
!                                                                               !/*de        224*/
      return                                                                    !/*de        225*/
      end                                                                       !/*de        226*/
