!#ident emaker.F from deck ek                                                   !/*ek          1*/
      subroutine emaker(m,cs)                                                   !/*ek          2*/
!        make one or two electrons in a photon collision of type m.             !/*ek          3*/
!        m=1  for photoelectric effect.                                         !/*ek          4*/
!        m=2  for incoherent (compton) scattering.                              !/*ek          5*/
!        m=3  for pair production.                                              !/*ek          6*/
!        m=4  for auger emission from positrons, electrons or photons.          !/*ek4a        1*/
!        cs = electron energy for m=1,4, cosine for m=2, flag for m=3.          !/*ek          8*/
      use global_data                                                           !/*ekx         1*/
!                                                                               !/*ekx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*ekx         3*/
      implicit integer(kindi) (i-n)                                             !/*ekx         4*/
!                                                                               !/*ekx         5*/
#ifdef MESHTAL                                                                  /*!ekx         6*/
      dimension zd(10),jz(5)                                                    !/*ekx         7*/
#endif                                                                          /*!ekx         8*/
      dimension ee(13),ce(21,13),ep(9),dp(21,9)                                 !/*ek         10*/
      data ee/100.,50.,20.,10.,5.,2.,1.,.5,.2,.1,.05,.02,.01/                   !/*ek         11*/
      data (ce(i, 1),i=1,21)/    5.0000000e-1,1.8391734e+2,3.7051644e+2,        !/*ek         12*/
     & 5.6409171e+2,8.5433494e+2,8.6549644e+2,1.1960445e+3,1.8133120e+3,        !/*ek         13*/
     & 3.3154915e+3,1.1560527e+4,7.7358920e+4,7.7358920e+4,7.7358920e+4,        !/*ek         14*/
     & 7.7358920e+4,7.7358920e+4,7.7358920e+4,7.7358920e+4,7.7358920e+4,        !/*ek         15*/
     & 7.7358920e+4,7.7358920e+4,7.7358920e+4/                                  !/*ek         16*/
      data (ce(i, 2),i=1,21)/    5.0001000e-1,1.5734214e+2,3.1179562e+2,        !/*ek         17*/
     & 4.6600627e+2,6.8708044e+2,7.2630997e+2,9.3864458e+2,1.2726402e+3,        !/*ek         18*/
     & 1.8585851e+3,3.1036588e+3,7.2354569e+3,1.9539631e+4,1.9539631e+4,        !/*ek         19*/
     & 1.9539631e+4,1.9539631e+4,1.9539631e+4,1.9539631e+4,1.9539631e+4,        !/*ek         20*/
     & 1.9539631e+4,1.9539631e+4,1.9539631e+4/                                  !/*ek         21*/
      data (ce(i, 3),i=1,21)/    5.0008000e-1,7.3991679e+1,1.4697244e+2,        !/*ek         22*/
     & 2.2059103e+2,2.9650433e+2,3.6916714e+2,4.5806222e+2,5.6159689e+2,        !/*ek         23*/
     & 5.9186199e+2,6.8870573e+2,8.0858436e+2,9.5808897e+2,1.1453783e+3,        !/*ek         24*/
     & 1.3799180e+3,1.6705798e+3,2.0208947e+3,2.4192528e+3,2.8236151e+3,        !/*ek         25*/
     & 3.1519086e+3,3.2220501e+3,3.2220501e+3/                                  !/*ek         26*/
      data (ce(i, 4),i=1,21)/    5.0030000e-1,2.5031580e+1,4.8446990e+1,        !/*ek         27*/
     & 7.2297589e+1,9.6665639e+1,1.2168910e+2,1.4755735e+2,1.7420693e+2,        !/*ek         28*/
     & 2.0162045e+2,2.2944535e+2,2.6029701e+2,2.8916076e+2,3.2332973e+2,        !/*ek         29*/
     & 3.6023191e+2,3.8754624e+2,4.2971674e+2,4.8031309e+2,5.4202665e+2,        !/*ek         30*/
     & 6.1881714e+2,7.1677975e+2,8.4578033e+2/                                  !/*ek         31*/
      data (ce(i, 5),i=1,21)/    5.0108000e-1,8.5127000e+0,1.5526810e+1,        !/*ek         32*/
     & 2.2556960e+1,2.9680480e+1,3.6933560e+1,4.4343450e+1,5.1940279e+1,        !/*ek         33*/
     & 5.9746320e+1,6.7800979e+1,7.6153979e+1,8.4843859e+1,9.3893880e+1,        !/*ek         34*/
     & 1.0344828e+2,1.1366273e+2,1.2457488e+2,1.3645768e+2,1.4961923e+2,        !/*ek         35*/
     & 1.6563791e+2,1.8436704e+2,2.3213951e+2/                                  !/*ek         36*/
      data (ce(i, 6),i=1,21)/    5.0528999e-1,2.9538700e+0,4.7418300e+0,        !/*ek         37*/
     & 6.4446200e+0,8.1125700e+0,9.7666099e+0,1.1419390e+1,1.3080480e+1,        !/*ek         38*/
     & 1.4758300e+1,1.6461250e+1,1.8198290e+1,1.9979720e+1,2.1817860e+1,        !/*ek         39*/
     & 2.3728780e+1,2.5733030e+1,2.7860760e+1,3.0156810e+1,3.2697300e+1,        !/*ek         40*/
     & 3.5626950e+1,3.9331470e+1,4.7791030e+1/                                  !/*ek         41*/
      data (ce(i, 7),i=1,21)/    5.1518000e-1,1.7938600e+0,2.5771400e+0,        !/*ek         42*/
     & 3.2745600e+0,3.9273000e+0,4.5526800e+0,5.1606300e+0,5.7578800e+0,        !/*ek         43*/
     & 6.3496200e+0,6.9402800e+0,7.5340199e+0,8.1350700e+0,8.7480799e+0,        !/*ek         44*/
     & 9.3785600e+0,1.0033510e+1,1.0722500e+1,1.1459730e+1,1.2268640e+1,        !/*ek         45*/
     & 1.3194470e+1,1.4353420e+1,1.6972980e+1/                                  !/*ek         46*/
      data (ce(i, 8),i=1,21)/    5.3681000e-1,1.2748100e+0,1.6511800e+0,        !/*ek         47*/
     & 1.9657000e+0,2.2487300e+0,2.5125400e+0,2.7637900e+0,3.0066900e+0,        !/*ek         48*/
     & 3.2442600e+0,3.4788700e+0,3.7125900e+0,3.9473700e+0,4.1852200e+0,        !/*ek         49*/
     & 4.4284100e+0,4.6797000e+0,4.9428000e+0,5.2230800e+0,5.5293500e+0,        !/*ek         50*/
     & 5.8785000e+0,6.3138799e+0,7.2922699e+0/                                  !/*ek         51*/
      data (ce(i, 9),i=1,21)/    5.8986000e-1,9.4238999e-1,1.1027300e+0,        !/*ek         52*/
     & 1.2324200e+0,1.3469100e+0,1.4522500e+0,1.5516100e+0,1.6469600e+0,        !/*ek         53*/
     & 1.7396700e+0,1.8307700e+0,1.9211600e+0,2.0116400e+0,2.1030300e+0,        !/*ek         54*/
     & 2.1962100e+0,2.2922600e+0,2.3926000e+0,2.4992800e+0,2.6156400e+0,        !/*ek         55*/
     & 2.7480400e+0,2.9128300e+0,3.2821700e+0/                                  !/*ek         56*/
      data (ce(i,10),i=1,21)/    6.4590000e-1,8.5676000e-1,9.5119999e-1,        !/*ek         57*/
     & 1.0272100e+0,1.0941100e+0,1.1555200e+0,1.2133500e+0,1.2687700e+0,        !/*ek         58*/
     & 1.3225900e+0,1.3754400e+0,1.4278200e+0,1.4802100e+0,1.5330900e+0,        !/*ek         59*/
     & 1.5869800e+0,1.6425100e+0,1.7004800e+0,1.7620900e+0,1.8292500e+0,        !/*ek         60*/
     & 1.9056400e+0,2.0006700e+0,2.2135200e+0/                                  !/*ek         61*/
      data (ce(i,11),i=1,21)/    7.0787000e-1,8.3725999e-1,8.9556000e-1,        !/*ek         62*/
     & 9.4266000e-1,9.8425999e-1,1.0225600e+0,1.0587400e+0,1.0935100e+0,        !/*ek         63*/
     & 1.1273700e+0,1.1607100e+0,1.1938500e+0,1.2270900e+0,1.2607400e+0,        !/*ek         64*/
     & 1.2951300e+0,1.3306700e+0,1.3679000e+0,1.4075900e+0,1.4510100e+0,        !/*ek         65*/
     & 1.5006000e+0,1.5626000e+0,1.7026900e+0/                                  !/*ek         66*/
      data (ce(i,12),i=1,21)/    7.8624000e-1,8.6494000e-1,9.0017000e-1,        !/*ek         67*/
     & 9.2852999e-1,9.5349000e-1,9.7640999e-1,9.9800000e-1,1.0187000e+0,        !/*ek         68*/
     & 1.0388100e+0,1.0585700e+0,1.0781600e+0,1.0977700e+0,1.1175700e+0,        !/*ek         69*/
     & 1.1377600e+0,1.1585700e+0,1.1803100e+0,1.2034400e+0,1.2286600e+0,        !/*ek         70*/
     & 1.2573800e+0,1.2931400e+0,1.3733800e+0/                                  !/*ek         71*/
      data (ce(i,13),i=1,21)/    8.3683000e-1,8.9150999e-1,9.1594999e-1,        !/*ek         72*/
     & 9.3561000e-1,9.5291000e-1,9.6878000e-1,9.8372000e-1,9.9804000e-1,        !/*ek         73*/
     & 1.0119400e+0,1.0255900e+0,1.0391300e+0,1.0526600e+0,1.0663200e+0,        !/*ek         74*/
     & 1.0802400e+0,1.0945900e+0,1.1095700e+0,1.1254900e+0,1.1428500e+0,        !/*ek         75*/
     & 1.1625900e+0,1.1871600e+0,1.2422200e+0/                                  !/*ek         76*/
      data ep/200.,80.,40.,20.,10.,6.,4.,3.,2./                                 !/*ek         77*/
      data (dp(i,1),i=1,21)/       .50000,.47443,.44894,.42358,.39836,          !/*ek         78*/
     & .37338,.34863,.32410,.29982,.27581,.25210,.22872,.20568,.18287,          !/*ek         79*/
     & .16023,.13762,.11495,.09196,.06816,.04245,.00000/                        !/*ek         80*/
      data (dp(i,2),i=1,21)/       .50000,.47577,.45159,.42753,.40359,          !/*ek         81*/
     & .37982,.35621,.33274,.30944,.28629,.26323,.24027,.21735,.19441,          !/*ek         82*/
     & .17135,.14807,.12443,.10011,.07446,.04614,.00000/                        !/*ek         83*/
      data (dp(i,3),i=1,21)/       .50000,.47705,.45413,.43123,.40838,          !/*ek         84*/
     & .38560,.36288,.34020,.31753,.29485,.27218,.24948,.22670,.20379,          !/*ek         85*/
     & .18069,.15727,.13336,.10857,.08217,.05237,.00000/                        !/*ek         86*/
      data (dp(i,4),i=1,21)/       .50000,.47861,.45720,.43578,.41431,          !/*ek         87*/
     & .39280,.37125,.34962,.32793,.30614,.28417,.26204,.23967,.21699,          !/*ek         88*/
     & .19381,.16996,.14517,.11897,.09064,.05895,.00000/                        !/*ek         89*/
      data (dp(i,5),i=1,21)/       .50000,.47973,.45943,.43907,.41864,          !/*ek         90*/
     & .39813,.37751,.35676,.33583,.31465,.29318,.27137,.24920,.22657,          !/*ek         91*/
     & .20330,.17918,.15390,.12696,.09752,.06325,.00000/                        !/*ek         92*/
      data (dp(i,6),i=1,21)/       .50000,.47973,.45946,.43919,.41892,          !/*ek         93*/
     & .39864,.37830,.35772,.33692,.31587,.29454,.27275,.25049,.22764,          !/*ek         94*/
     & .20390,.17930,.15363,.12622,.09608,.06173,.00000/                        !/*ek         95*/
      data (dp(i,7),i=1,21)/       .50000,.47962,.45925,.43887,.41850,          !/*ek         96*/
     & .39812,.37777,.35718,.33635,.31545,.29451,.27329,.25155,.22932,          !/*ek         97*/
     & .20625,.18221,.15717,.13011,.10020,.06496,.00000/                        !/*ek         98*/
      data (dp(i,8),i=1,21)/       .50000,.47915,.45830,.43744,.41659,          !/*ek         99*/
     & .39574,.37489,.35403,.33318,.31248,.29120,.26904,.24600,.22301,          !/*ek        100*/
     & .19878,.17326,.14770,.12050,.09134,.05931,.00000/                        !/*ek        101*/
      data (dp(i,9),i=1,21)/       .50000,.47500,.45000,.42500,.40000,          !/*ek        102*/
     & .37500,.35000,.32500,.30000,.27500,.25000,.22500,.20000,.17500,          !/*ek        103*/
     & .15000,.12500,.10000,.07500,.05000,.02500,.00000/                        !/*ek        104*/
!                                                                               !/*ek        105*/
!        save photon parameters while electrons are produced.                   !/*ek        106*/
      if(fiml(3).eq.0..and.kpt(3).ne.0)return                                   !/*cor4-1    151*/
      call savpar(0,1)                                                          !/*ek4c2       1*/
      ipt=3                                                                     !/*ek        114*/
      jan=1                                                                     !/*ekxc        1*/
      if(jgp.eq.0)jgp=-1                                                        !/*ek4a        3*/
      npa=1                                                                     !/*ek        116*/
      mm=m                                                                      !/*ek4a        5*/
!                                                                               !/*ek        118*/
!        roulette or split according to enum bias.                              !/*ek        119*/
      if(enum.eq.1.)go to 40                                                    !/*ek        120*/
      if(ipt9(npb).ne.2)go to 40                                                !/*ek        121*/
      npa=enum+rang()                                                           !/*ek        122*/
      if(npa.eq.0)go to 200                                                     !/*ek        123*/
      wgt=wgt/enum                                                              !/*ek        124*/
!                                                                               !/*ek        125*/
!        branch according to photon collision type.                             !/*ek        126*/
   40 go to(50,90,100,160)m                                                     !/*ek        127*/
!                                                                               !/*ek        128*/
! >>>>>  m=1 -- make a photoelectron.                                           !/*ek        129*/
   50 ix = igain_electron_photo_electric                                        !/*ekx         9*/
      nb = ipwb_electron_photo_electric                                         !/*ekx        10*/
      erg=cs                                                                    !/*ek        131*/
      do 60 k=2,13                                                              !/*ek        132*/
   60 if(cs.ge.ee(k))go to 80                                                   !/*ek        133*/
   70 am=1.-2.*rang()                                                           !/*ek        134*/
      if(rang().lt.am**2)go to 70                                               !/*ek        135*/
      call rotas(am,uold,uuu,lev,irt)                                           !/*ek4b        1*/
      go to 170                                                                 !/*ek        137*/
   80 ek=ee(k-1)-cs                                                             !/*ek        138*/
      e1=cs-ee(k)                                                               !/*ek        139*/
      r=rang()*20.+1.                                                           !/*ek        140*/
      l=r                                                                       !/*ek        141*/
      pl=r-l                                                                    !/*ek        142*/
      am=min(one,max(-one,(1.-(ee(k-1)-ee(k))/((1.-pl)*e1*ce(l,k-1)+            !/*ek4b        2*/
     & pl*e1*ce(l+1,k-1)+(1.-pl)*ek*ce(l,k)+pl*ek*ce(l+1,k)))*                  !/*ek4b        3*/
     & (eg0+gpt(3))/sqrt(eg0*(eg0+2.*gpt(3)))))                                 !/*ek4b        4*/
      call rotas(am,uold,uuu,lev,irt)                                           !/*ek4b        5*/
      go to 170                                                                 !/*ek        146*/
!                                                                               !/*ek        147*/
! >>>>>  m=2 -- make a compton recoil electron.                                 !/*ek        148*/
   90 bt=eg0/erg                                                                !/*ek        149*/
      erg=eg0-erg                                                               !/*ek        150*/
      ec=min((bt-cs)/sqrt(bt*(bt-2.*cs)+1.),one)                                !/*ek        151*/
      b=-sqrt((1.-ec**2)/(1.-cs**2))                                            !/*ek        152*/
      a=ec-b*cs                                                                 !/*ek        153*/
      uuu=a*uold(1)+b*uuu                                                       !/*ek4b        6*/
      vvv=a*uold(2)+b*vvv                                                       !/*ek4b        7*/
      www=a*uold(3)+b*www                                                       !/*ek4b        8*/
      t1=1./sqrt(uuu**2+vvv**2+www**2)                                          !/*ek        157*/
      uuu=t1*uuu                                                                !/*ek        158*/
      vvv=t1*vvv                                                                !/*ek        159*/
      www=t1*www                                                                !/*ek        160*/
      if(lev.ne.0)call uplev(uuu)                                               !/*ek4b        9*/
      ix = igain_electron_compton_recoil                                        !/*ekx        11*/
      nb = ipwb_electron_compton_recoil                                         !/*ekx        12*/
      go to 170                                                                 !/*ek        163*/
!                                                                               !/*ek        164*/
! >>>>>  m=3 -- make an electron-positron pair.                                 !/*ek        165*/
!        first sample the electron.                                             !/*ek        166*/
  100 s=min(erg/gpt(3),ep(1))                                                   !/*ek        167*/
      do 110 k=2,8                                                              !/*ek        168*/
  110 if(s.ge.ep(k))go to 120                                                   !/*ek        169*/
  120 ek=ep(k-1)-s                                                              !/*ek        170*/
      e1=s-ep(k)                                                                !/*ek        171*/
      r=rang()*20.+1.                                                           !/*ek        172*/
      l=r                                                                       !/*ek        173*/
      pl=r-l                                                                    !/*ek        174*/
      em=erg-2.*gpt(3)                                                          !/*ek        175*/
      erg=em*((1.-pl)*e1*dp(l,k-1)+pl*e1*dp(l+1,k-1)+                           !/*ek        176*/
     & (1.-pl)*ek*dp(l,k)+pl*ek*dp(l+1,k))/(ep(k-1)-ep(k))                      !/*ek        177*/
      if(rang().gt..5)erg=em-erg                                                !/*ek        178*/
      b=sqrt(eg0*(eg0+2.*gpt(3)))/(eg0+gpt(3))                                  !/*ek        179*/
      r=2.*rang()                                                               !/*ek        180*/
      e1=min(max(-one,(r-1.-b)/(r*b-1.-b)),one)                                 !/*ek        181*/
      call rotas(e1,uold,uuu,lev,irt)                                           !/*ek4b       10*/
      if(erg.lt.elc(3))go to 150                                                !/*ek        183*/
      if(kpt(3).eq.0)go to 130                                                  !/*ek        184*/
      call gain_pax (igain_electron_pair_production, 3,                         !/*ekx        13*/
     &               npa, npa*wgt, npa*wgt*erg)                                 !/*ekx        14*/
      call contrib_pwb (3,ipwb_electron_pair_production,icl,npa*wgt)            !/*ekx        15*/
      vel=slite*sqrt(erg*(erg+2.*gpt(3)))/(erg+gpt(3))                          !/*ek        189*/
      call bankit(10+mm)                                                        !/*ek4a        6*/
      go to 150                                                                 !/*ek        191*/
  130 if(erg.le.elc(2))go to 150                                                !/*ek        192*/
      do 140 j=1,npa                                                            !/*ek        193*/
  140 call ttbr(zero)                                                           !/*ek        194*/
!                                                                               !/*ek        195*/
!        then sample the positron.                                              !/*ek        196*/
  150 erg=em-erg                                                                !/*ek        197*/
      if(erg.lt.elc(3).or.kpt(3).eq.0)cs=1.                                     !/*ek        198*/
      if(erg.lt.elc(3))go to 200                                                !/*ek        199*/
      r=2.*rang()                                                               !/*ek        200*/
      e2=min(max(-one,(r-1.-b)/(r*b-1.-b)),one)                                 !/*ek        201*/
      b=-sqrt((1.-e2**2)/(1.-e1**2))                                            !/*ek        202*/
      a=e2-b*e1                                                                 !/*ek        203*/
      uuu=a*uold(1)+b*uuu                                                       !/*ek4b       11*/
      vvv=a*uold(2)+b*vvv                                                       !/*ek4b       12*/
      www=a*uold(3)+b*www                                                       !/*ek4b       13*/
      t1=1./sqrt(uuu**2+vvv**2+www**2)                                          !/*ek        207*/
      uuu=t1*uuu                                                                !/*ek        208*/
      vvv=t1*vvv                                                                !/*ek        209*/
      www=t1*www                                                                !/*ek        210*/
      if(lev.ne.0)call uplev(uuu)                                               !/*ek4b       14*/
      ix = igain_electron_pair_production                                       !/*ekx        16*/
      nb = ipwb_electron_pair_production                                        !/*ekx        17*/
      jgp=-3                                                                    !/*cor4-1    153*/
      jan=-1                                                                    !/*ekxc        2*/
      mm=5                                                                      !/*ek4a        7*/
      go to 170                                                                 !/*ek        215*/
!                                                                               !/*ek        216*/
! >>>>>  m=4 -- make an auger electron.                                         !/*ek        217*/
  160 erg=cs                                                                    !/*ek        218*/
      call isos(uuu,lev)                                                        !/*ek4b       15*/
      if (ipt9(npb).eq.2) then                                                  !/*ekx        18*/
         ix = igain_electron_photon_auger                                       !/*ekx        19*/
         nb = ipwb_electron_photon_auger                                        !/*ekx        20*/
      else if (ipt9(npb).eq.3) then                                             !/*ekx        21*/
         ix = igain_electron_electron_auger                                     !/*ekx        22*/
         nb = ipwb_electron_electron_auger                                      !/*ekx        23*/
      else                                                                      !/*ekx        24*/
         call expirx(1,'emaker','unexpected particle type.')                    !/*ekx        25*/
         return                                                                 !/*ekx        26*/
      endif                                                                     !/*ekx        27*/
      if(jgp.le.-2)jgp=jgp+2                                                    !/*ek4b       16*/
!                                                                               !/*ek        221*/
!        bank the new electron or make ttb photons.                             !/*ek        222*/
  170 if(kpt(3).eq.0)go to 180                                                  !/*ek        223*/
      call gain_pax (ix, 3, npa, npa*wgt, npa*wgt*erg)                          !/*ekx        28*/
      call contrib_pwb (3,nb,icl,npa*wgt)                                       !/*ekx        29*/
      vel=slite*sqrt(erg*(erg+2.*gpt(3)))/(erg+gpt(3))                          !/*ek        228*/
      call bankit(10+mm)                                                        !/*ek4a        9*/
#ifdef MESHTAL                                                                  /*!ekx        30*/
      if(m.ne.4.or.ipt9(npb).ne.3) go to 200                                    !/*ekx        31*/
      qx=-erg                                                                   !/*ekx        32*/
      erg=eg0                                                                   !/*ekx        33*/
      l=locct(llct+1,icl)                                                       !/*ekx        34*/
      if(l.ne.0)call tally(qx,1,l,one)                                          !/*ekx        35*/
      l=locct(llct+ipt,icl)                                                     !/*ekx        36*/
      if(l.ne.0)call tally(qx,2,l,one)                                          !/*ekx        37*/
      if(kdb.ne.0)return                                                        !/*ekx        38*/
      do 173 i=0,lev-1                                                          !/*ekx        39*/
         l=locct(llct+1,int(udt(7,i)))                                          !/*ekx        40*/
         if(l.ne.0)call tally(qx,1,l,one)                                       !/*ekx        41*/
         l=locct(llct+ipt,int(udt(7,i)))                                        !/*ekx        42*/
         if(l.ne.0)call tally(qx,2,l,one)                                       !/*ekx        43*/
  173    continue                                                               !/*ekx        44*/
      if (mtlflg.ne.0.or.medflg.ne.0) then                                      !/*ekx        45*/
         do 175 i=1,6                                                           !/*ekx        46*/
         if (lev.eq.0) then                                                     !/*ekx        47*/
            zd(i)=gpblcm(i)                                                     !/*ekx        48*/
         else                                                                   !/*ekx        49*/
            zd(i)=udt(i,0)                                                      !/*ekx        50*/
         endif                                                                  !/*ekx        51*/
  175    continue                                                               !/*ekx        52*/
         zd(7)=wgt                                                              !/*ekx        53*/
         zd(8)=erg                                                              !/*ekx        54*/
         zd(9)=qx                                                               !/*ekx        55*/
         jz(1)=mkc                                                              !/*ekx        56*/
         jz(2)=mspf                                                             !/*ekx        57*/
         jz(5)=2                                                                !/*ekx        58*/
         call gdtal(8,zero,zd,jz)                                               !/*ekx        59*/
      endif                                                                     !/*ekx        60*/
#endif                                                                          /*!ekx        61*/
      go to 200                                                                 !/*ek        230*/
!                                                                               !/*ekx        62*/
  180 if(erg.le.elc(2))go to 200                                                !/*ek        231*/
      do 190 j=1,npa                                                            !/*ek        232*/
  190 call ttbr(zero)                                                           !/*ek        233*/
!                                                                               !/*ek        234*/
  200 call getpar(0,1)                                                          !/*ek4c2       2*/
      return                                                                    !/*ek        242*/
      end                                                                       !/*ek        243*/
