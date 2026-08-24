!     $Id: aitota.F,v 1.5 2002/03/27 18:14:53 gwm Exp $                       !/*ai          1*/
      subroutine aitota (e,beta,dens,u,v,w)                                     !/*ai          2*/
#include <lcs/double.h>                                                         /*!ai          3*/
!     ==================================================================        !/*ai          4*/
!     aitota calculates isobar interaction cross section, that                  !/*ai          5*/
!     is decay, scattering and capture.                                         !/*ai          6*/
!                                                                               !/*ai          7*/
! AIOUTA called by AIOUT                                                        !/*ai          8*/
! AITOTA calls   FIND  BINTER  EN  SQRT                                         !/*ai          9*/
!     ==================================================================        !/*ai         10*/
#include <lcs/vegas.h>                                                          /*!ai         11*/
      dimension w(32), ener(40), enec(40), scat(21), capa(37), capb(37),        !/*ai         12*/
     & capc(37)                                                                 !/*ai         13*/
      data ener /dp0,.5d2,1.d2,1.5d2,2.d2,2.5d2,3.d2,3.5d2,40.d1,4.5d2,5        !/*ai         14*/
     & .d2,5.5d2,6.d2,6.5d2,7.d2,7.5d2,8.d2,850.d0,9.d2,9.5d2,10.d2,19          !/*ai         15*/
     & *dp0/, enec /2.1d0,2.15d0,2.2d0,2.25d0,2.3d0,2.35d0,2.4d0,2.45d0,        !/*ai         16*/
     & 2.5d0,2.55d0,2.6d0,2.65d0,2.7d0,2.75d0,2.8d0,2.85d0,2.9d0,2.95d0,        !/*ai         17*/
     & 3.d0,3.05d0,3.1d0,3.15d0,3.25d0,3.3d0,3.35d0,3.4d0,3.45d0,3.5d0,3        !/*ai         18*/
     & .55d0,3.6d0,3.65d0,3.7d0,3.75d0,3.85d0,3.9d0,3.95d0,4.d0,3*dp0/,         !/*ai         19*/
     & scat /245.d0,216.1d0,192.6d0,172.4d0,153.5d0,137.8d0,124.8d0,114.        !/*ai         20*/
     & 3d0,105.4d0,97.7d0,90.9d0,84.9d0,79.7d0,74.9d0,70.7d0,67.1d0,63.7        !/*ai         21*/
     & d0,60.7d0,57.9d0,55.3d0,53.1d0/, capa /16253.76d0,14123.25d0,5729        !/*ai         22*/
     & .29d0,3004.16d0,1739.6d0,761.59d0,217.32d0,95.61d0,49.75d0,28.34d        !/*ai         23*/
     & 0,17.17d0,11.06d0,7.42d0,5.36d0,3.95d0,2.91d0,2.42d0,1.89d0,1.68d        !/*ai         24*/
     & 0,1.42d0,1.08d0,1.12d0,.97d0,.79d0,.83d0,.72d0,.54d0,.59d0,0.58d0        !/*ai         25*/
     & ,.41d0,.62d0,.47d0,.31d0,.3d0,.26d0,.19d0,.17d0/                         !/*ai         26*/
      data capb /-34794.55d0,-31778.34d0,-13042.64d0,-6933.94d0,-4068.27        !/*ai         27*/
     & d0,-1778.72d0,-483.33d0,-201.23d0,-97.89d0,-51.15d0,-27.65d0,-15.        !/*ai         28*/
     & 44d0,-8.61d0,-5.15d0,-2.95d0,-1.38d0,-1.01d0,-0.39d0,-0.4d0,-0.23        !/*ai         29*/
     & d0,.21d0,-0.25d0,-0.41d0,-0.17d0,-0.45d0,-0.36d0,-0.07d0,-0.31d0,        !/*ai         30*/
     & -0.41d0,-0.06d0,-0.67d0,-0.39d0,-0.06d0,-0.16d0,-0.11d0,.03d0,0.0        !/*ai         31*/
     & 2d0/, capc /18664.23d0,17924.52d0,7458.38d0,4028.7d0,2400.83d0,10        !/*ai         32*/
     & 58.04d0,258.24d0,119.44d0,59.34d0,32.35d0,18.82d0,11.8d0,7.85d0,5        !/*ai         33*/
     & .82d0,4.49d0,3.48d0,3.19d0,2.73d0,2.64d0,2.43d0,2.06d0,2.25d0,2.1        !/*ai         34*/
     & 6d0,1.91d0,2.d0,1.87d0,1.6d0,1.68d0,1.67d0,1.38d0,1.7d0,1.47d0,1.        !/*ai         35*/
     & 21d0,1.17d0,1.09d0,.95d0,.92d0/                                          !/*ai         36*/
      efp=dp0                                                                   !/*ai         37*/
      efn=dp0                                                                   !/*ai         38*/
      if (ind(32).eq.2) go to 20                                                !/*ai         39*/
      if (ws(31).ne.dp0) go to 10                                               !/*ai         40*/
      if (istept.eq.0) go to 20                                                 !/*ai         41*/
      efp=ef(1,istept)                                                          !/*ai         42*/
      efn=ef(2,istept)                                                          !/*ai         43*/
      go to 20                                                                  !/*ai         44*/
   10 if (istepp.eq.0) go to 20                                                 !/*ai         45*/
      efp=pef(1,istepp)                                                         !/*ai         46*/
      efn=pef(2,istepp)                                                         !/*ai         47*/
   20 ind(25)=0                                                                 !/*ai         48*/
      if (isonsw.lt.1) go to 30                                                 !/*ai         49*/
      aiscat=dp0                                                                !/*ai         50*/
      go to 160                                                                 !/*ai         51*/
   30 if (e-95.d1) 40,40,540                                                    !/*ai         52*/
   40 ik=20                                                                     !/*ai         53*/
      call find (e,ener,ie,ik)                                                  !/*ai         54*/
      call binter (e,ener,ie,aint3,aint4,aint5)                                 !/*ai         55*/
!                                                                               !/*ai         56*/
!       aiscat is the "scattering+charge-exchange" cross section.               !/*ai         57*/
!       whose tabulation is in harp, et al, phys rev c8(73) 581                 !/*ai         58*/
!                                                                               !/*ai         59*/
      aiscat=scat(ie-1)*aint3+scat(ie)*aint4+scat(ie+1)*aint5                   !/*ai         60*/
      if (ind(12)-1) 100,50,100                                                 !/*ai         61*/
   50 if (ws(3)-13.d0) 90,60,90                                                 !/*ai         62*/
   60 if (w(3)-dp1) 80,70,80                                                    !/*ai         63*/
!                                                                               !/*ai         64*/
! fscat is the branching ratio for exchange scattering.  see zeev               !/*ai         65*/
!  fraenkels notes.                                                             !/*ai         66*/
! up to statement 12 scattering cross section is calculated.                    !/*ai         67*/
!                                                                               !/*ai         68*/
   70 fscat=dp1                                                                 !/*ai         69*/
      go to 150                                                                 !/*ai         70*/
   80 fscat=0.33333d0                                                           !/*ai         71*/
      go to 150                                                                 !/*ai         72*/
   90 if (w(3)-dp1) 70,80,70                                                    !/*ai         73*/
  100 if (ws(3)-14.d0) 140,110,140                                              !/*ai         74*/
  110 if (w(3)-dp1) 130,120,130                                                 !/*ai         75*/
  120 fscat=0.77778d0                                                           !/*ai         76*/
      go to 150                                                                 !/*ai         77*/
  130 fscat=0.55555d0                                                           !/*ai         78*/
      go to 150                                                                 !/*ai         79*/
  140 if (w(3)-dp1) 120,130,120                                                 !/*ai         80*/
  150 aiscat=fscat*aiscat                                                       !/*ai         81*/
  160 if (ind(32).eq.2) go to 390                                               !/*ai         82*/
!                                                                               !/*ai         83*/
! up to statement 49 decay cross section is calculated.                         !/*ai         84*/
! change made 3/23/71 to force isobars with mass lt 1080 mev to                 !/*ai         85*/
! capture                                                                       !/*ai         86*/
      if (ws(4).lt.108.d1) go to 390                                            !/*ai         87*/
      if (ind(12)-1) 210,170,210                                                !/*ai         88*/
  170 if (ws(3)-13.d0) 190,180,190                                              !/*ai         89*/
!                                                                               !/*ai         90*/
! dmass and pmass are mass of nucleon and pion.                                 !/*ai         91*/
!                                                                               !/*ai         92*/
  180 dmass=amass(1)                                                            !/*ai         93*/
      ws24=efp                                                                  !/*ai         94*/
      go to 200                                                                 !/*ai         95*/
  190 dmass=amass(2)                                                            !/*ai         96*/
      ws24=efn                                                                  !/*ai         97*/
      go to 200                                                                 !/*ai         98*/
  200 pmass=amass(3)                                                            !/*ai         99*/
!                                                                               !/*ai        100*/
! fcn en calculates energy of the decay nucleon.  this is done to               !/*ai        101*/
!  check if decay is kinematically allowed (the decay nucleon has to            !/*ai        102*/
!  stay above the fermi level)                                                  !/*ai        103*/
!                                                                               !/*ai        104*/
      if (en(dmass,pmass,ws(4),beta)-dmass-ws24) 390,390,400                    !/*ai        105*/
!                                                                               !/*ai        106*/
! ind(25)=1 when i+ to n and pion+ or i0 to n and pion0 not allowed.            !/*ai        107*/
! ind(25)=2 when i+ to p and pion0 or i0 to p and pion- not allowed.            !/*ai        108*/
! ind(25)=3 when i+ and i0 not allowed to decay into either of the              !/*ai        109*/
!  modes.                                                                       !/*ai        110*/
! ind(25)=0 when all modes are allowed.                                         !/*ai        111*/
! first ask which mode or all modes allowed.  thus ind(25) is set.              !/*ai        112*/
!  then calculate the fraction the allowed mode is.  then calculate             !/*ai        113*/
!  unreduced decay cross section.  then calculate the reduced decay             !/*ai        114*/
!  cross section using the fraction of the allowed mode.                        !/*ai        115*/
!                                                                               !/*ai        116*/
  210 if (ws(3)-14.d0) 320,220,320                                              !/*ai        117*/
  220 dmass=amass(2)                                                            !/*ai        118*/
      pmass=amass(3)                                                            !/*ai        119*/
      ws24=efn                                                                  !/*ai        120*/
      if (en(dmass,pmass,ws(4),beta)-dmass-ws24) 230,230,240                    !/*ai        121*/
  230 ind(25)=ind(25)+1                                                         !/*ai        122*/
  240 dmass=amass(1)                                                            !/*ai        123*/
      pmass=amass(4)                                                            !/*ai        124*/
      ws24=efp                                                                  !/*ai        125*/
      if (en(dmass,pmass,ws(4),beta)-dmass-ws24) 250,250,260                    !/*ai        126*/
  250 ind(25)=ind(25)+2                                                         !/*ai        127*/
  260 if (ind(25)-3) 270,390,270                                                !/*ai        128*/
  270 if (ind(25)) 280,400,280                                                  !/*ai        129*/
  280 continue                                                                  !/*ai        130*/
! p1 is cross section for ij cross section.                                     !/*ai        131*/
      p1=0.33333333333333d0                                                     !/*ai        132*/
! p2 is cross section for 0 cross section.                                      !/*ai        133*/
      p2=0.66666666666667d0                                                     !/*ai        134*/
  290 pp1=p1/(p1+p2)                                                            !/*ai        135*/
      pp2=p2/(p1+p2)                                                            !/*ai        136*/
      if (ind(25)-1) 300,310,300                                                !/*ai        137*/
! p is the fraction of allowed mode.                                            !/*ai        138*/
  300 p=pp1                                                                     !/*ai        139*/
      go to 400                                                                 !/*ai        140*/
  310 p=pp2                                                                     !/*ai        141*/
      go to 400                                                                 !/*ai        142*/
  320 dmass=amass(2)                                                            !/*ai        143*/
      pmass=amass(4)                                                            !/*ai        144*/
      ws24=efn                                                                  !/*ai        145*/
      if (en(dmass,pmass,ws(4),beta)-dmass-ws24) 330,330,340                    !/*ai        146*/
  330 ind(25)=ind(25)+1                                                         !/*ai        147*/
  340 dmass=amass(1)                                                            !/*ai        148*/
      pmass=amass(3)                                                            !/*ai        149*/
      ws24=efp                                                                  !/*ai        150*/
      if (en(dmass,pmass,ws(4),beta)-dmass-ws24) 350,350,360                    !/*ai        151*/
  350 ind(25)=ind(25)+2                                                         !/*ai        152*/
  360 if (ind(25)-3) 370,390,390                                                !/*ai        153*/
  370 if (ind(25)) 380,400,380                                                  !/*ai        154*/
  380 continue                                                                  !/*ai        155*/
! p1 is cross section for 0 cross section                                       !/*ai        156*/
      p1=0.66666666666667d0                                                     !/*ai        157*/
! p2 is cross section for ij cross section.                                     !/*ai        158*/
      p2=0.33333333333333d0                                                     !/*ai        159*/
      go to 290                                                                 !/*ai        160*/
! all modes are not allowed.  then decay cross section is set to zero.          !/*ai        161*/
!                                                                               !/*ai        162*/
  390 aidec=dp0                                                                 !/*ai        163*/
      go to 420                                                                 !/*ai        164*/
!                                                                               !/*ai        165*/
! the following change was made 1/21/71 because a factor of 10                  !/*ai        166*/
! was left out of the decay cross section                                       !/*ai        167*/
! i.e.the conversion factor should have been 1.e27/(1.e39*3.e10*.73e-23)        !/*ai        168*/
! which is 10.d0/2.19 not 1.d0/2.19 to get the cross section in mb.             !/*ai        169*/
!                                                                               !/*ai        170*/
! energy dependent width for isobars a la ginocchio                             !/*ai        171*/
! the following change was made 20/8/80                                         !/*ai        172*/
!         see ginocchio, phys rev c17 (1978) 195                                !/*ai        173*/
!                                                                               !/*ai        174*/
  400 an=ws(4)*1.d-03                                                           !/*ai        175*/
      r1=0.83d0                                                                 !/*ai        176*/
      r2=0.62d0                                                                 !/*ai        177*/
!                                                                               !/*ai        178*/
!      A correction now implemented:                                            !/*ai        179*/
!      0.0172 appears to be m(pi)**2 * m(nuc)**2,                               !/*ai        180*/
!      and not 4 times that quantity ( = 0.06537 )                              !/*ai        181*/
!           observation made 8 nov 83 [mrc]                                     !/*ai        182*/
!                                                                               !/*ai        183*/
!      an*an = m(del)**2, 0.9 = m(pi)**2+m(nuc)**2                              !/*ai        184*/
!      then aaa will be 4. * m(nuc)**2 * plab(pi)**2                            !/*ai        185*/
!                                                                               !/*ai        186*/
      aaa=(an**2-0.9d0)**2-0.06537d0                                            !/*ai        187*/
      aidec=dp0                                                                 !/*ai        188*/
      if (aaa.lt.dp0) go to 410                                                 !/*ai        189*/
!                                                                               !/*ai        190*/
!     root(aaa)/an/2 = plab(pi) * m(nuc)/ecm = pcm(pi)                          !/*ai        191*/
!                                                                               !/*ai        192*/
      q=sqrt(aaa)/(dp2*an)                                                      !/*ai        193*/
!               convert to [1/fermi] from [gev/c]                               !/*ai        194*/
      q=q*1.d+03/197.32d0                                                       !/*ai        195*/
!                                                                               !/*ai        196*/
!           159.766 = 112/rho(t0) in ginnochio                                  !/*ai        197*/
      width=159.766d0*(q**3)/(dp1+(r1*q)**2+(r2*q)**4)                          !/*ai        198*/
!                                                                               !/*ai        199*/
!            cugnon constant isobar width  (gamiso = 112 mev)                   !/*ai        200*/
!            or pre-1978 "vegas" calculations(gamiso = 90 mev)                  !/*ai        201*/
!                                                                               !/*ai        202*/
      if (gamiso.ne.dp0) width=gamiso                                           !/*ai        203*/
!                                                                               !/*ai        204*/
!          sig(decay) = 1/rho(r)/mfp(decay), 10. converts fm**2 to mb           !/*ai        205*/
      aidec=(sqrt(dp1-beta*beta)*width*dp10)/(beta*dens*197.32d0)               !/*ai        206*/
!                                                                               !/*ai        207*/
  410 if (ind(25).lt.1) go to 420                                               !/*ai        208*/
!                                                                               !/*ai        209*/
! reduced decay cross section is calculated.                                    !/*ai        210*/
      aidec=p*aidec                                                             !/*ai        211*/
!                                                                               !/*ai        212*/
! fcapt is the factor for capture that cross section obtained by opem           !/*ai        213*/
!  programs must be multiplied in order to obtain the contribution of           !/*ai        214*/
!  the diagram to the total cross section of the process.                       !/*ai        215*/
!  statement 420 starts calculating capture cross section.                      !/*ai        216*/
!  for i++ and p or i- and n capture is not possible.  cross section for        !/*ai        217*/
!  capture is set to zero.                                                      !/*ai        218*/
!                                                                               !/*ai        219*/
  420 if (ind(12)-1) 430,490,430                                                !/*ai        220*/
  430 fcapt=0.66667d0                                                           !/*ai        221*/
!                                                                               !/*ai        222*/
! change made 7/6/71.the capture factors for i+ +n and i0 +p are 4/3            !/*ai        223*/
! not 2/3 because the final states for these reactions lead to p + n            !/*ai        224*/
! and the factors for the reverse reactions need not be divided by 2.           !/*ai        225*/
! see z.fraenkel paper on isobar capture: phys rev 130(1963)2407                !/*ai        226*/
!                  and                nuovo comento 30(1963)512                 !/*ai        227*/
!                                                                               !/*ai        228*/
      if ((ws(3).eq.14.d0).and.(w(3).eq.dp2)) fcapt=1.33333d0                   !/*ai        229*/
      if ((ws(3).eq.24.d0).and.(w(3).eq.dp1)) fcapt=1.33333d0                   !/*ai        230*/
  440 ik=36                                                                     !/*ai        231*/
      if (u-2.1d0) 450,450,460                                                  !/*ai        232*/
  450 a=capa(1)                                                                 !/*ai        233*/
      b=capb(1)                                                                 !/*ai        234*/
      c=capc(1)                                                                 !/*ai        235*/
      go to 470                                                                 !/*ai        236*/
  460 call find (u,enec,ie,ik)                                                  !/*ai        237*/
      call binter (u,enec,ie,aint3,aint4,aint5)                                 !/*ai        238*/
      a=capa(ie-1)*aint3+capa(ie)*aint4+capa(ie+1)*aint5                        !/*ai        239*/
      b=capb(ie-1)*aint3+capb(ie)*aint4+capb(ie+1)*aint5                        !/*ai        240*/
      c=capc(ie-1)*aint3+capc(ie)*aint4+capc(ie+1)*aint5                        !/*ai        241*/
  470 aicap=(a*v*v+b*v+c)*fcapt                                                 !/*ai        242*/
  480 aitot=aiscat+aidec+aicap                                                  !/*ai        243*/
      return                                                                    !/*ai        244*/
  490 if (ws(3)-13.d0) 520,500,520                                              !/*ai        245*/
  500 if (w(3)-dp1) 510,530,510                                                 !/*ai        246*/
  510 fcapt=dp2                                                                 !/*ai        247*/
      go to 440                                                                 !/*ai        248*/
  520 if (w(3)-dp1) 530,510,530                                                 !/*ai        249*/
  530 aicap=dp0                                                                 !/*ai        250*/
      go to 480                                                                 !/*ai        251*/
  540 continue                                                                  !/*ai        252*/
      aiscat=dp0                                                                !/*ai        253*/
      go to 160                                                                 !/*ai        254*/
      end                                                                       !/*ai        255*/
