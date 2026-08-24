!     $Id: dfact.F,v 1.5 2002/03/27 18:16:18 gwm Exp $                        !/*df          1*/
      function dfact(id,ic,en,it,iu,acr)                                        !/*df          2*/
#ifndef MESHTAL                                                                 /*!df          3*/
      dfact=0.                                                                  !/*df          4*/
#else                                                                           /*!df          5*/
#ifdef CHEAP                                                                    /*!df          6*/
      implicit double precision (a-h,o-z)                                       !/*df          7*/
#endif                                                                          /*!df          8*/
!                                                                               !/*df          9*/
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc              !/*df         10*/
!                                                                               !/*df         11*/
!       returns tissue equivalent fluence to dose conversion factor in          !/*df         12*/
!             (rem/hr)/(particles/cm^2-sec)                                     !/*df         13*/
!       id=particle id number                                                   !/*df         14*/
!               1=neutron                                                       !/*df         15*/
!               2=photon                                                        !/*df         16*/
!               3=electron                                                      !/*df         17*/
!               4=positron                                                      !/*df         18*/
!       ic=choice of conversion factor                                          !/*df         19*/
!         (feel free to add others, just try to stay consistent with            !/*df         20*/
!           variable names and numbering system)                                !/*df         21*/
!             Note:  10 and 20 are Dose Equivalent (H): absorbed dose at        !/*df         22*/
!                     "point" in tissued weighted by a distribution of Q        !/*df         23*/
!                     (quality factors) related to the LET distribution         !/*df         24*/
!                     of radiation at that point.                               !/*df         25*/
!                    30 ... 39 are Equivalent Dose (H_t): based on an           !/*df         26*/
!                     average absorbed dose in the tissue or organ (D_t)        !/*df         27*/
!                     and weighted by the radiation weighting factor            !/*df         28*/
!                     (w_r), summed over all component radiations               !/*df         29*/
!         neutrons:                                                             !/*df         30*/
!                   10 = ICRP-21 1971                                           !/*df         31*/
!                   20 = NCRP-38 1971, ANSI/ANS-6.1.1-1977                      !/*df         32*/
!                   31 = ANSI/ANS-6.1.1-1991 (AP anterior-posterior)            !/*df         33*/
!                   32                       (PA posterior-anterior)            !/*df         34*/
!                   33                       (LAT side exposure)                !/*df         35*/
!                   34                       (ROT normal to length &            !/*df         36*/
!                                                rotationally symmetric)        !/*df         37*/
!                   40 = ICRP-74 1996 ambient dose equivalent                   !/*df         38*/
!                                                                               !/*df         39*/
!         photons:  10 = ICRP-21 1971                                           !/*df         40*/
!                   20 = Claiborne & Trubey, ANSI/ANS 6.1.1-1977                !/*df         41*/
!                   31 = ANSI/ANS-6.1.1-1991 (AP anterior-posterior)            !/*df         42*/
!                   32                       (PA posterior-anterior)            !/*df         43*/
!                   33                       (LAT side exposure)                !/*df         44*/
!                   34                       (ROT normal to length &            !/*df         45*/
!                                                rotationally symmetric)        !/*df         46*/
!                   35                       (ISO isotropic)                    !/*df         47*/
!       en=particle energy                                                      !/*df         48*/
!       it=interpolation method                                                 !/*df         49*/
!         1 = logarythmic interpolation                                         !/*df         50*/
!         2 = linear interpolation                                              !/*df         51*/
!         3 = recommended analytic parametarization                             !/*df         52*/
!              (not available for ic=10 and ic = 40)                            !/*df         53*/
!       iu=units of dfact                                                       !/*df         54*/
!         1 = (rem/hr)(particles/cm^2-sec)                                      !/*df         55*/
!         2 = (seiverts/hr)(particles/cm^2-sec)                                 !/*df         56*/
!       acr=normalize factor for neutron dose                                   !/*df         57*/
!      -1.0 =  normalize dfact to Q=20 by dividing out parametric form          !/*df         58*/
!            Q (or w_r)=5.0+17.0*exp(-(ln(2E))*2/6)                             !/*df         59*/
!            from ICRP60 (1990) paragraph A12                                   !/*df         60*/
!      -2.0 =  apply LAMPF albatross correction factors                         !/*df         61*/
!            (use this with ic=10 per work by WNR)                              !/*df         62*/
!      dfact will be multiplied by any factor greater or equal to 0.0           !/*df         63*/
!            e.g., acr=1.0 means no change                                      !/*df         64*/
!                                                                               !/*df         65*/
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc          !/*df         66*/
!                                                                               !/*df         67*/
      parameter (dp0=0.d0, dp1=1.d0, dp2=2.d0, dp3=3.d0, dp4=4.d0,              !/*dfxc        1*/
     & dph=.5d0, dp5=5.d0, dp10=1.d1, dp50=50.d0, dpth=dp1/dp3,                 !/*dfxc        2*/
     & dp2th=dp2/dp3, dppi=3.1415926535898d0)                                   !/*dfxc        3*/
!                                                                               !/*df         71*/
      dimension elim_n_1(21),fct_n_1(21),q_n_1(21),                             !/*df         72*/
     &          elim_n_2(22),fct_n_2(22),q_n_2(22),                             !/*df         73*/
     &          elim_n_3(23),fct_n_3(23,4),                                     !/*df         74*/
     &          elim_n_4(55),fct_n_4(55),                                       !/*df         75*/
     &          ana_n_3_lo(5,4),ana_n_3_md(5,4),ana_n_3_hi(5,4),                !/*df         76*/
     &          elim_p_1(25),fct_p_1(25),                                       !/*df         77*/
     &          elim_p_2(38),fct_p_2(38),                                       !/*df         78*/
     &          elim_p_3(26),fct_p_3(26,5),                                     !/*df         79*/
     &          ana_p_3_lo(5,5),ana_p_3_md(5,5),ana_p_3_hi(5,5)                 !/*df         80*/
!                                                                               !/*df         81*/
      data elim_n_1/2.5e-8,1.0e-7,1.0e-6,1.0e-5,1.0e-4,1.0e-3,1.0e-2,           !/*df         82*/
     & 1.0e-1,5.0e-1,1.0,2.0,5.0,10.0,20.0,50.0,100.0,200.0,500.0,              !/*df         83*/
     & 1000.0,2000.0,3000.0/                                                    !/*df         84*/
      data fct_n_1/3.85e-6,4.17e-6,4.55e-6,4.35e-6,4.17e-6,3.70e-6,             !/*df         85*/
     & 3.57e-6,2.08e-5,7.14e-5,1.18e-4,1.43e-4,1.47e-4,1.47e-4,                 !/*df         86*/
     & 1.54e-4,1.64e-4,1.79e-4,1.96e-4,2.78e-4,4.55e-4,6.24e-4,                 !/*df         87*/
     & 7.14e-4/                                                                 !/*df         88*/
      data q_n_1/2.3,2.0,2.0,2.0,2.0,2.0,2.0,7.4,11.0,10.6,9.3,7.8,             !/*df         89*/
     & 6.8,6.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0/                                     !/*df         90*/
      data elim_n_2/2.5e-8,1.0e-7,1.0e-6,1.0e-5,1.0e-4,1.0e-3,1.0e-2,           !/*df         91*/
     & 1.0e-1,5.0e-1,1.0,2.5,5.0,7.0,10.0,14.0,20.0,40.0,60.0,100.0,            !/*df         92*/
     & 200.0,300.0,400.0/                                                       !/*df         93*/
      data fct_n_2/3.67e-6,3.67e-6,4.46e-6,4.54e-6,4.18e-6,3.76e-6,             !/*df         94*/
     & 3.56e-6,2.17e-5,9.26e-5,1.32e-4,1.25e-4,1.56e-4,1.47e-4,                 !/*df         95*/
     & 1.47e-4,2.08e-4,2.27e-4,2.5E-4,2.27e-4,1.78e-4,1.92e-4,                  !/*df         96*/
     & 2.27e-4,2.5e-4/                                                          !/*df         97*/
      data q_n_2/2.0,2.0,2.0,2.0,2.0,2.0,2.5,7.5,11.0,11.0,9.0,8.0,             !/*df         98*/
     & 7.0,6.5,7.5,8.0,7.0,5.5,4.0,3.5,3.5,3.5/                                 !/*df         99*/
      data elim_n_3/2.5e-8,1.0e-7,1.0e-6,1.0e-5,1.0e-4,1.0e-3,1.0e-2,           !/*df        100*/
     & 2.0e-2,5.0e-2,1.0e-1,2.0e-1,5.0e-1,1.0,1.5,2.0,3.0,4.0,5.0,              !/*df        101*/
     & 6.0,7.0,8.0,10.0,14.0/                                                   !/*df        102*/
      data fct_n_3/4.0,4.4,4.82,4.46,4.14,3.83,4.53,5.87,10.9,19.8,             !/*df        103*/
     & 38.6,87.0,143.0,183.0,214.0,264.0,300.0,327.0,347.0,365.0,               !/*df        104*/
     & 380.0,410.0,480.0,                                                       !/*df        105*/
     & 2.6,2.7,2.81,2.78,2.63,2.49,2.58,2.79,3.64,5.69,8.6,30.8,                !/*df        106*/
     & 53.5,85.8,120.0,174.0,215.0,244.0,265.0,283.0,296.0,321.0,               !/*df        107*/
     & 415.0,                                                                   !/*df        108*/
     & 1.3,1.4,1.43,1.33,1.27,1.19,1.27,1.46,2.14,3.57,6.94,18.7,               !/*df        109*/
     & 33.3,52.1,71.8,105.0,131.0,151.0,167.0,181.0,194.0,218.0,                !/*df        110*/
     & 280.0,                                                                   !/*df        111*/
     & 2.3,2.4,2.63,2.48,2.33,2.18,2.41,2.89,4.7,8.15,15.3,38.8,                !/*df        112*/
     & 65.7,93.7,120.0,162.0,195.0,219.0,237.0,253.0,266.0,292.0,               !/*df        113*/
     & 365.0/                                                                   !/*df        114*/
      data ana_n_3_lo/                                                          !/*df        115*/
     & 3.430895e+0,7.725710e-1,9.834081e-2,4.903466e-3,8.149667e-5,             !/*df        116*/
     & 3.687778e+0,1.240448e+0,1.888331e-1,1.164621e-2,2.516630e-4,             !/*df        117*/
     & 8.677172e-1,2.290812e-1,2.457221e-2,7.646448e-4,0.0,                     !/*df        118*/
     & 2.436148e+0,6.502219e-1,8.819417e-2,4.752092e-3,8.782611e-5/             !/*df        119*/
      data ana_n_3_md/                                                          !/*df        120*/
     & 0.0,0.0,0.0,0.0,0.0,                                                     !/*df        121*/
     & 3.965468e+0,1.012777e+0,3.608077e-1,-5.689570e-2,-2.047265e-1,           !/*df        122*/
     & 0.0,0.0,0.0,0.0,0.0,                                                     !/*df        123*/
     & 0.0,0.0,0.0,0.0,0.0/                                                     !/*df        124*/
      data ana_n_3_hi/                                                          !/*df        125*/
     & 4.952167e+0,6.644235e-1,-1.017445e-1,-1.496004e-3,3.636748e-3,           !/*df        126*/
     & 3.965468e+0,1.078553e+0,3.607919e-1,-4.288176e-1,9.411842e-2,            !/*df        127*/
     & 3.577644e+0,1.005248e+0,-4.227422e+0,-2.054736e-2,4.461858e-4,           !/*df        128*/
     & 4.216783e+0,8.470534e-1,-6.771566e-2,-1.213208e-2,1.829400e-3/           !/*df        129*/
!                                                                               !/*df        130*/
!   ICRP-74, 1996                                                               !/*df        131*/
!                                                                               !/*df        132*/
      data elim_n_4/                                                            !/*df        133*/
     & 1.E-09,1.E-08,2.53E-08,1.E-07,2.E-07,5.E-07,1.E-06,2.E-06,               !/*df        134*/
     & 5.E-06,1.E-05,2.E-05,5.E-05,1.E-04,2.E-04,5.E-04,1.E-03,                 !/*df        135*/
     & 2.E-03,5.E-03,0.01,0.02,0.03,0.05,0.07,0.1,0.15,0.2,0.3,0.5,             !/*df        136*/
     & 0.7,0.9,1.,1.2,2.,3.,4.,5.,6.,7.,8.,9.,10.,12.,14.,15.,16.,              !/*df        137*/
     & 18.,20.,30.,50.,70.,100.,125.,150.,175.,201./                            !/*df        138*/
!                                                                               !/*df        139*/
      data fct_n_4/                                                             !/*df        140*/
     & 2.376E-06,3.240E-06,3.816E-06,4.644E-06,4.860E-06,4.896E-06,             !/*df        141*/
     & 4.788E-06,4.644E-06,4.320E-06,4.068E-06,3.816E-06,3.564E-06,             !/*df        142*/
     & 3.384E-06,3.204E-06,2.988E-06,2.844E-06,2.772E-06,2.880E-06,             !/*df        143*/
     & 3.780E-06,5.976E-06,8.532E-06,1.480E-05,2.160E-05,3.168E-05,             !/*df        144*/
     & 4.752E-05,6.120E-05,8.388E-05,1.159E-04,1.350E-04,1.440E-04,             !/*df        145*/
     & 1.498E-04,1.530E-04,1.512E-04,1.483E-04,1.469E-04,1.458E-04,             !/*df        146*/
     & 1.440E-04,1.458E-04,1.472E-04,1.512E-04,1.584E-04,1.728E-04,             !/*df        147*/
     & 1.872E-04,1.944E-04,1.998E-04,2.052E-04,2.160E-04,1.854E-04,             !/*df        148*/
     & 1.440E-04,1.188E-04,1.026E-04,9.360E-05,8.820E-05,9.000E-05,             !/*df        149*/
     & 9.360E-05/                                                               !/*df        150*/
!                                                                               !/*df        151*/
      data elim_p_1/.01,.015,.02,.03,.04,.05,.06,.08,.1,.15,.2,.3,              !/*df        152*/
     & .4,.5,.6,.8,1.0,1.5,2.,3.,4.,5.,6.,8.,10./                               !/*df        153*/
      data fct_p_1/2.78e-6,1.11e-6,5.88e-7,2.56e-7,1.56e-7,1.20e-7,             !/*df        154*/
     & 1.11e-7,1.20e-7,1.47e-7,2.38e-7,3.45e-7,5.56e-7,7.69e-7,                 !/*df        155*/
     & 9.09e-7,1.14e-6,1.47e-6,1.79e-6,2.44e-6,3.03e-6,4.00e-6,                 !/*df        156*/
     & 4.76e-6,5.56e-6,6.25e-6,7.69e-6,9.09e-6/                                 !/*df        157*/
      data elim_p_2/.01,.03,.05,.07,.1,.15,.2,.25,.3,.35,.4,.45,.5,             !/*df        158*/
     & .55,.6,.65,.7,.8,1.0,1.4,1.8,2.2,2.6,2.8,3.25,3.75,4.25,4.75,            !/*df        159*/
     & 5.0,5.25,5.75,6.25,6.75,7.5,9.0,11.0,13.0,15.0/                          !/*df        160*/
      data fct_p_2/3.96e-6,5.82e-7,2.9e-7,2.58e-7,2.83e-7,3.79e-7,              !/*df        161*/
     & 5.01e-7,6.31e-7,7.59e-7,8.78e-7,9.85e-7,1.08e-6,1.17e-6,                 !/*df        162*/
     & 1.27e-6,1.36e-6,1.44e-6,1.52e-6,1.68e-6,1.98e-6,2.51e-6,                 !/*df        163*/
     & 2.99e-6,3.42e-6,3.82e-6,4.01e-6,4.41e-6,4.83e-6,5.23e-6,                 !/*df        164*/
     & 5.60e-6,5.80e-6,6.01e-6,6.37e-6,6.74e-6,7.11e-6,7.66e-6,                 !/*df        165*/
     & 8.77e-6,1.03e-5,1.18e-5,1.33e-5/                                         !/*df        166*/
      data elim_p_3/.01,.015,.02,.03,.04,.05,.06,.08,.1,.15,.2,.3,              !/*df        167*/
     & .4,.5,.6,.8,1.0,1.5,2.0,3.0,4.0,5.0,6.0,8.0,10.0,12.0/                   !/*df        168*/
      data fct_p_3/.062,.157,.238,.329,.365,.384,.4,.451,.533,.777,             !/*df        169*/
     & 1.03,1.56,2.06,2.54,2.99,3.83,4.6,6.24,7.66,10.2,12.5,14.7,              !/*df        170*/
     & 16.7,20.8,24.7,28.9,                                                     !/*df        171*/
     & .0001,.031,.0868,.161,.222,.26,.286,.344,.418,.624,.844,1.3,             !/*df        172*/
     & 1.76,2.2,2.62,3.43,4.18,5.8,7.21,9.71,12.0,14.1,16.2,20.2,               !/*df        173*/
     & 24.2,28.8,                                                               !/*df        174*/
     & .02,.033,.0491,.0863,.123,.152,.17,.212,.258,.396,.557,.891,             !/*df        175*/
     & 1.24,1.58,1.92,2.6,3.24,4.7,6.02,8.4,10.6,12.6,14.6,                     !/*df        176*/
     & 18.5,22.3,26.4,                                                          !/*df        177*/
     & .029,.071,.11,.166,.199,.222,.24,.293,.357,.534,.731,1.14,               !/*df        178*/
     & 1.55,1.96,2.34,3.07,3.75,5.24,6.56,8.9,11.0,13.0,14.9,18.9,              !/*df        179*/
     & 22.9,27.6,                                                               !/*df        180*/
     & .022,.057,.0912,.138,.163,.18,.196,.237,.284,.436,.602,.949,             !/*df        181*/
     & 1.3,1.64,1.98,2.64,3.27,4.68,5.93,8.19,10.2,12.1,14.0,17.8,              !/*df        182*/
     & 21.6,25.8/                                                               !/*df        183*/
      data ana_p_3_lo/                                                          !/*df        184*/
     & 2.30402,.75167,-1.04725,-.50091,-0.06302,                                !/*df        185*/
     & 56.0900,14.17975,0.0,0.0,0.0,                                            !/*df        186*/
     & 13.3185,18.22077,8.61214,1.82597,.13867,                                 !/*df        187*/
     & 5.15939,5.13418,1.21881,0.02805,-0.01816,                                !/*df        188*/
     & 1.22920,0.0,-1.37145,-0.53901,-0.06363/                                  !/*df        189*/
      data ana_p_3_md/                                                          !/*df        190*/
     & 0.0,0.0,0.0,0.0,0.0,                                                     !/*df        191*/
     & -4.27492,-9.52586,-7.06215,-2.02976,-0.20889,                            !/*df        192*/
     & 0.0,0.0,0.0,0.0,0.0,                                                     !/*df        193*/
     & 0.0,0.0,0.0,0.0,0.0,                                                     !/*df        194*/
     & 0.0,0.0,0.0,0.0,0.0/                                                     !/*df        195*/
      data ana_p_3_hi/                                                          !/*df        196*/
     & 1.52070,.79329,-0.07261,0.01228,0.00347,                                 !/*df        197*/
     & 1.42685,0.86211,-0.09311,0.00402,0.00800,                                !/*df        198*/
     & 1.17185,0.96410,-0.09305,0.00211,0.00650,                                !/*df        199*/
     & 1.32196,0.87836,-0.09933,0.00517,0.00908,                                !/*df        200*/
     & 1.18229,0.92497,-0.09282,0.00305,0.00772/                                !/*df        201*/
!                                                                               !/*df        202*/
      integer p_1,p_2,p_3                                                       !/*df        203*/
      data n_1/21/                                                              !/*df        204*/
      data n_2/22/                                                              !/*df        205*/
      data n_3/23/                                                              !/*df        206*/
      data n_4/55/                                                              !/*df        207*/
      data p_1/25/                                                              !/*df        208*/
      data p_2/38/                                                              !/*df        209*/
      data p_3/26/                                                              !/*df        210*/
!                                                                               !/*df        211*/
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc               !/*df        212*/
!                                                                               !/*df        213*/
      index=mod(ic,10)                                                          !/*df        214*/
      goto(1,2,3,3),id                                                          !/*df        215*/
! section for neutrons                                                          !/*df        216*/
    1 goto(10,20,30,40),(ic-index)/10                                           !/*df        217*/
! ic=10 neutrons ICRP-21, 1971                                                  !/*df        218*/
   10 goto(11,11,13)it                                                          !/*df        219*/
   11 if(en.lt.elim_n_1(1))then                                                 !/*df        220*/
         dfact=fct_n_1(1)                                                       !/*df        221*/
         goto 500                                                               !/*df        222*/
      elseif(en.ge.elim_n_1(n_1))then                                           !/*df        223*/
         dfact=fct_n_1(n_1)                                                     !/*df        224*/
         goto 500                                                               !/*df        225*/
      endif                                                                     !/*df        226*/
      do i=1,n_1-1                                                              !/*df        227*/
         if(en.ge.elim_n_1(i) .and. en.lt.elim_n_1(i+1))then                    !/*df        228*/
            if(it.eq.1) dfact=fct_n_1(i)+                                       !/*df        229*/
     &         ( fct_n_1(i+1)-fct_n_1(i) )*                                     !/*df        230*/
     &         ( log(en)-log(elim_n_1(i)) )/                                    !/*df        231*/
     &         ( log(elim_n_1(i+1))-log(elim_n_1(i)) )                          !/*df        232*/
            if(it.eq.2) dfact=fct_n_1(i)+                                       !/*df        233*/
     &         ( fct_n_1(i+1)-fct_n_1(i) )*                                     !/*df        234*/
     &         (en-elim_n_1(i))/(elim_n_1(i+1)-elim_n_1(i))                     !/*df        235*/
            goto 500                                                            !/*df        236*/
         endif                                                                  !/*df        237*/
      end do                                                                    !/*df        238*/
   13 goto 11                                                                   !/*df        239*/
! ic=20 neutrons NCRP-38 1971, ANSI/ANS-6.1.1-1977                              !/*df        240*/
   20 goto(21,21,23),it                                                         !/*df        241*/
   21 if(en.lt.elim_n_2(1))then                                                 !/*df        242*/
         dfact=fct_n_2(1)                                                       !/*df        243*/
         goto 500                                                               !/*df        244*/
      elseif(en.ge.elim_n_2(n_2))then                                           !/*df        245*/
         dfact=fct_n_2(n_2)                                                     !/*df        246*/
         goto 500                                                               !/*df        247*/
         endif                                                                  !/*df        248*/
      do i=1,n_2-1                                                              !/*df        249*/
         if(en.ge.elim_n_2(i) .and. en.lt.elim_n_2(i+1))then                    !/*df        250*/
            if(it.eq.1) dfact=fct_n_2(i)+                                       !/*df        251*/
     &         ( fct_n_2(i+1)-fct_n_2(i) )*                                     !/*df        252*/
     &         ( log(en)-log(elim_n_2(i)) )/                                    !/*df        253*/
     &         ( log(elim_n_2(i+1))-log(elim_n_2(i)) )                          !/*df        254*/
            if(it.eq.2) dfact=fct_n_2(i)+                                       !/*df        255*/
     &         ( fct_n_2(i+1)-fct_n_2(i) )*                                     !/*df        256*/
     &         (en-elim_n_2(i))/(elim_n_2(i+1)-elim_n_2(i))                     !/*df        257*/
            goto 500                                                            !/*df        258*/
         endif                                                                  !/*df        259*/
      end do                                                                    !/*df        260*/
   23 X=log(en)                                                                 !/*df        261*/
      if(en.le.1.0e-7)then                                                      !/*df        262*/
         dfact=-1.2514e+1                                                       !/*df        263*/
      elseif(en.gt.1.0e-7 .and. en.le.1.0e-2)then                               !/*df        264*/
         dfact=-1.2210e+1+1.7165e-1*X+2.6034e-2*X**2+1.0273e-3*X**3             !/*df        265*/
      elseif(en.gt..01 .and. en.le..1)then                                      !/*df        266*/
         dfact=-8.9302+7.8440e-1*X                                              !/*df        267*/
      elseif(en.gt..1 .and. en.le..5)then                                       !/*df        268*/
         dfact=-8.6632+9.0037e-1*X                                              !/*df        269*/
      elseif(en.gt..5 .and. en.le.1.0)then                                      !/*df        270*/
         dfact=-8.9359+5.0696e-1*X                                              !/*df        271*/
      elseif(en.gt.1.0 .and. en.le.2.5)then                                     !/*df        272*/
         dfact=-8.9359-5.5979e-2*X                                              !/*df        273*/
      elseif(en.gt.2.5 .and. en.le.5.0)then                                     !/*df        274*/
         dfact=-9.2822+3.2193e-1*X                                              !/*df        275*/
      elseif(en.gt.5.0 .and. en.le.7.0)then                                     !/*df        276*/
         dfact=-8.47441-1.8018e-1*X                                             !/*df        277*/
      elseif(en.gt.7.0 .and. en.le.10.0)then                                    !/*df        278*/
         dfact=-8.8247                                                          !/*df        279*/
      elseif(en.gt.10.0 .and. en.le.14.0)then                                   !/*df        280*/
         dfact=-1.1208e+1+1.0352*X                                              !/*df        281*/
      elseif(en.gt.14.0)then                                                    !/*df        282*/
         dfact=-9.1202+2.4395e-1*X                                              !/*df        283*/
      endif                                                                     !/*df        284*/
      dfact=exp(max(min(dp50,dfact),-dp50))                                     !/*dfxc        4*/
      goto 500                                                                  !/*df        286*/
! ic=31 to 34 neutrons                                                          !/*df        287*/
   30 goto(31,31,33),it                                                         !/*df        288*/
   31 if(en.lt.elim_n_3(1))then                                                 !/*df        289*/
         dfact=fct_n_3(1,index)*1.0e-12*100.0*3600.0                            !/*df        290*/
         goto 500                                                               !/*df        291*/
      elseif(en.ge.elim_n_3(n_3))then                                           !/*df        292*/
         dfact=fct_n_3(n_3,index)*1.0e-12*100.0*3600.0                          !/*df        293*/
         goto 500                                                               !/*df        294*/
      endif                                                                     !/*df        295*/
      do i=1,n_3-1                                                              !/*df        296*/
         if(en.ge.elim_n_3(i) .and. en.lt.elim_n_3(i+1))then                    !/*df        297*/
            if(it.eq.1) dfact=fct_n_3(i,index)+                                 !/*df        298*/
     &         ( fct_n_3(i+1,index)-fct_n_3(i,index) )*                         !/*df        299*/
     &         ( log(en)-log(elim_n_3(i)) )/                                    !/*df        300*/
     &         ( log(elim_n_3(i+1))-log(elim_n_3(i)) )                          !/*df        301*/
            if(it.eq.2) dfact=fct_n_3(i,index)+                                 !/*df        302*/
     &         ( fct_n_3(i+1,index)-fct_n_3(i,index) )*                         !/*df        303*/
     &         (en-elim_n_3(i))/(elim_n_3(i+1)-elim_n_3(i))                     !/*df        304*/
            dfact=dfact*1.0e-12*100.0*3600.0                                    !/*df        305*/
            goto 500                                                            !/*df        306*/
         endif                                                                  !/*df        307*/
      end do                                                                    !/*df        308*/
   33 X=log(en)                                                                 !/*df        309*/
      dfact=0.0                                                                 !/*df        310*/
      do i=1,5                                                                  !/*df        311*/
         if(index.ne.2)then                                                     !/*df        312*/
            if(en.le..01)then                                                   !/*df        313*/
               dfact=dfact+ana_n_3_lo(i,index)*X**(i-1)                         !/*df        314*/
            elseif(en.gt..01)then                                               !/*df        315*/
               dfact=dfact+ana_n_3_hi(i,index)*X**(i-1)                         !/*df        316*/
            endif                                                               !/*df        317*/
         elseif(index.eq.2)then                                                 !/*df        318*/
            if(en.le..2)then                                                    !/*df        319*/
               dfact=dfact+ana_n_3_lo(i,index)*X**(i-1)                         !/*df        320*/
            elseif(en.gt..2 .and. en.le.1.0)then                                !/*df        321*/
               dfact=dfact+ana_n_3_md(i,index)*X**(i-1)                         !/*df        322*/
            elseif(en.gt.1.0)then                                               !/*df        323*/
               dfact=dfact+ana_n_3_hi(i,index)*X**(i-1)                         !/*df        324*/
            endif                                                               !/*df        325*/
         endif                                                                  !/*df        326*/
      end do                                                                    !/*df        327*/
      dfact=exp(max(min(dp50,dfact),-dp50))                                     !/*dfxc        5*/
      dfact=dfact*1.0e-12*100.0*3600.0                                          !/*df        329*/
      goto 500                                                                  !/*df        330*/
!                                                                               !/*df        331*/
! ic=40 neutrons ICRP-74, 1996                                                  !/*df        332*/
   40 goto(41,41,43)it                                                          !/*df        333*/
   41 if(en.lt.elim_n_4(1))then                                                 !/*df        334*/
         dfact=fct_n_4(1)                                                       !/*df        335*/
         goto 500                                                               !/*df        336*/
      elseif(en.ge.elim_n_4(n_4))then                                           !/*df        337*/
         dfact=fct_n_4(n_4)                                                     !/*df        338*/
         goto 500                                                               !/*df        339*/
      endif                                                                     !/*df        340*/
      do i=1,n_4-1                                                              !/*df        341*/
         if(en.ge.elim_n_4(i) .and. en.lt.elim_n_4(i+1))then                    !/*df        342*/
            if(it.eq.1) dfact=fct_n_4(i)+                                       !/*df        343*/
     &         ( fct_n_4(i+1)-fct_n_4(i) )*                                     !/*df        344*/
     &         ( log(en)-log(elim_n_4(i)) )/                                    !/*df        345*/
     &         ( log(elim_n_4(i+1))-log(elim_n_4(i)) )                          !/*df        346*/
            if(it.eq.2) dfact=fct_n_4(i)+                                       !/*df        347*/
     &         ( fct_n_4(i+1)-fct_n_4(i) )*                                     !/*df        348*/
     &         (en-elim_n_4(i))/(elim_n_4(i+1)-elim_n_4(i))                     !/*df        349*/
            goto 500                                                            !/*df        350*/
         endif                                                                  !/*df        351*/
      end do                                                                    !/*df        352*/
   43 goto 41                                                                   !/*df        353*/
!                                                                               !/*df        354*/
! section for photons                                                           !/*df        355*/
    2 goto(100,200,300),(ic-index)/10                                           !/*df        356*/
! ic=10 photons                                                                 !/*df        357*/
  100 goto(101,101,103)it                                                       !/*df        358*/
  101 if(en.lt.elim_p_1(1))then                                                 !/*df        359*/
         dfact=fct_p_1(1)                                                       !/*df        360*/
         goto 500                                                               !/*df        361*/
      elseif(en.ge.elim_p_1(p_1))then                                           !/*df        362*/
         dfact=fct_p_1(p_1)                                                     !/*df        363*/
         goto 500                                                               !/*df        364*/
      endif                                                                     !/*df        365*/
      do i=1,p_1-1                                                              !/*df        366*/
         if(en.ge.elim_p_1(i) .and. en.lt.elim_p_1(i+1))then                    !/*df        367*/
            if(it.eq.1) dfact=fct_p_1(i)+                                       !/*df        368*/
     &         ( fct_p_1(i+1)-fct_p_1(i) )*                                     !/*df        369*/
     &         ( log(en)-log(elim_p_1(i)) )/                                    !/*df        370*/
     &         ( log(elim_p_1(i+1))-log(elim_p_1(i)) )                          !/*df        371*/
            if(it.eq.2) dfact=fct_p_1(i)+                                       !/*df        372*/
     &         ( fct_p_1(i+1)-fct_p_1(i) )*                                     !/*df        373*/
     &         (en-elim_p_1(i))/(elim_p_1(i+1)-elim_p_1(i))                     !/*df        374*/
            goto 500                                                            !/*df        375*/
         endif                                                                  !/*df        376*/
      end do                                                                    !/*df        377*/
  103 goto 101                                                                  !/*df        378*/
! ic=20 photons                                                                 !/*df        379*/
  200 goto(201,201,203),it                                                      !/*df        380*/
  201 if(en.lt.elim_p_2(1))then                                                 !/*df        381*/
         dfact=fct_p_2(1)                                                       !/*df        382*/
         goto 500                                                               !/*df        383*/
      elseif(en.ge.elim_p_2(p_2))then                                           !/*df        384*/
         dfact=fct_p_2(p_2)                                                     !/*df        385*/
         goto 500                                                               !/*df        386*/
      endif                                                                     !/*df        387*/
      do i=1,p_2-1                                                              !/*df        388*/
         if(en.ge.elim_p_2(i) .and.                                             !/*df        389*/
     &         en.lt.elim_p_2(i+1))then                                         !/*df        390*/
            if(it.eq.1) dfact=fct_p_2(i)+                                       !/*df        391*/
     &         ( fct_p_2(i+1)-fct_p_2(i) )*                                     !/*df        392*/
     &         ( log(en)-log(elim_p_2(i)) )/                                    !/*df        393*/
     &         ( log(elim_p_2(i+1))-log(elim_p_2(i)) )                          !/*df        394*/
            if(it.eq.2) dfact=fct_p_2(i)+                                       !/*df        395*/
     &         ( fct_p_2(i+1)-fct_p_2(i) )*                                     !/*df        396*/
     &         (en-elim_p_2(i))/(elim_p_2(i+1)-elim_p_2(i))                     !/*df        397*/
            goto 500                                                            !/*df        398*/
         endif                                                                  !/*df        399*/
      end do                                                                    !/*df        400*/
  203 X=log(en)                                                                 !/*df        401*/
      if(en.le..01)then                                                         !/*df        402*/
         dfact=-12.43926653                                                     !/*df        403*/
      elseif(en.gt..01 .and. en.le..03)then                                     !/*df        404*/
         dfact=-2.0477e+1-1.7454*X                                              !/*df        405*/
      elseif(en.gt..03 .and. en.le..5)then                                      !/*df        406*/
         dfact=-1.3626e+1-5.7117e-1*X-1.0954*X**2-2.4897e-1*X**3                !/*df        407*/
      elseif(en.gt..5 .and. en.le.5.0)then                                      !/*df        408*/
         dfact=-1.3133e+1+7.2008e-1*X-3.3603e-2*X**2                            !/*df        409*/
      elseif(en.gt.5.0 .and. en.le.15.0)then                                    !/*df        410*/
         dfact=-1.2791e+1+2.8309e-1*X+1.0873e-1*X**2                            !/*df        411*/
      elseif(en.gt.15.0)then                                                    !/*df        412*/
         dfact=-11.22774652                                                     !/*df        413*/
      endif                                                                     !/*df        414*/
      dfact=exp(max(min(dp50,dfact),-dp50))                                     !/*dfxc        6*/
      goto 500                                                                  !/*df        416*/
! it=31 to 35 photons                                                           !/*df        417*/
  300 goto(301,301,303),it                                                      !/*df        418*/
  301 if(en.lt.elim_p_3(1))then                                                 !/*df        419*/
         dfact=fct_p_3(1,index)*1.0e-12*100.0*3600.0                            !/*df        420*/
         goto 500                                                               !/*df        421*/
      elseif(en.ge.elim_p_3(p_3))then                                           !/*df        422*/
         dfact=fct_p_3(p_3,index)*1.0e-12*100.0*3600.0                          !/*df        423*/
         goto 500                                                               !/*df        424*/
      endif                                                                     !/*df        425*/
      do i=1,p_3-1                                                              !/*df        426*/
         if(en.ge.elim_p_3(i) .and. en.lt.elim_p_3(i+1))then                    !/*df        427*/
            if(it.eq.1) dfact=fct_p_3(i,index)+                                 !/*df        428*/
     &         ( fct_p_3(i+1,index)-fct_p_3(i,index) )*                         !/*df        429*/
     &         ( log(en)-log(elim_p_3(i)) )/                                    !/*df        430*/
     &         ( log(elim_p_3(i+1))-log(elim_p_3(i)) )                          !/*df        431*/
            if(it.eq.2) dfact=fct_p_3(i,index)+                                 !/*df        432*/
     &         ( fct_p_3(i+1,index)-fct_p_3(i,index) )*                         !/*df        433*/
     &         (en-elim_p_3(i))/(elim_p_3(i+1)-elim_p_3(i))                     !/*df        434*/
            dfact=dfact*1.0e-12*100.0*3600.0                                    !/*df        435*/
            goto 500                                                            !/*df        436*/
         endif                                                                  !/*df        437*/
      end do                                                                    !/*df        438*/
  303 X=log(en)                                                                 !/*df        439*/
      dfact=0.0                                                                 !/*df        440*/
      do i=1,5                                                                  !/*df        441*/
         if(index.ne.2 .and. index.ne.4)then                                    !/*df        442*/
            if(en.le..15)then                                                   !/*df        443*/
               dfact=dfact+ana_p_3_lo(i,index)*X**(i-1)                         !/*df        444*/
            elseif(en.gt..15)then                                               !/*df        445*/
               dfact=dfact+ana_p_3_hi(i,index)*X**(i-1)                         !/*df        446*/
            endif                                                               !/*df        447*/
         elseif(index.eq.2)then                                                 !/*df        448*/
            if(en.le..015)then                                                  !/*df        449*/
               dfact=dfact+ana_p_3_lo(i,index)*X**(i-1)                         !/*df        450*/
            elseif(en.gt..015 .and. en.le.0.1)then                              !/*df        451*/
               dfact=dfact+ana_p_3_md(i,index)*X**(i-1)                         !/*df        452*/
            elseif(en.gt.0.1)then                                               !/*df        453*/
               dfact=dfact+ana_p_3_hi(i,index)*X**(i-1)                         !/*df        454*/
            endif                                                               !/*df        455*/
         elseif(index.eq.4)then                                                 !/*df        456*/
            if(en.le..1)then                                                    !/*df        457*/
               dfact=dfact+ana_p_3_lo(i,index)*X**(i-1)                         !/*df        458*/
            else                                                                !/*df        459*/
               dfact=dfact+ana_p_3_hi(i,index)*X**(i-1)                         !/*df        460*/
            endif                                                               !/*df        461*/
         endif                                                                  !/*df        462*/
      end do                                                                    !/*df        463*/
      dfact=exp(max(min(dp50,dfact),-dp50))                                     !/*dfxc        7*/
      dfact=dfact*1.0e-12*100.0*3600.0                                          !/*df        465*/
      goto 500                                                                  !/*df        466*/
! section for electrons and positrons                                           !/*df        467*/
   3  dfact=0.0                                                                 !/*df        468*/
      goto 500                                                                  !/*df        469*/
!                                                                               !/*df        470*/
 500  continue                                                                  !/*df        471*/
! convert to sieverts                                                           !/*df        472*/
      if(iu.eq.2)dfact=dfact/100.0                                              !/*df        473*/
! apply correction factors                                                      !/*df        474*/
      if(acr.eq.-1.0 .and. id.eq.1)then                                         !/*df        475*/
         dfact=dfact*20.0/(5.0+17.0*exp(-1.0*(log(2.0*en))**2/6.0))             !/*df        476*/
      elseif(acr.eq.-2.0 .and. id.eq.1)then                                     !/*df        477*/
         if(en.lt..25)then                                                      !/*df        478*/
            dfact=dfact*2.5                                                     !/*df        479*/
         else                                                                   !/*df        480*/
            X=log10(en)                                                         !/*df        481*/
            dfact=(1.4229-1.152*x+.78732*x**2-.24341*x**3)*dfact                !/*df        482*/
         endif                                                                  !/*df        483*/
      else                                                                      !/*df        484*/
         dfact=dfact*acr                                                        !/*df        485*/
      endif                                                                     !/*df        486*/
!                                                                               !/*df        487*/
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc          !/*df        488*/
!                                                                               !/*df        489*/
#endif                                                                          /*!df        490*/
      return                                                                    !/*df        491*/
      end                                                                       !/*df        492*/
