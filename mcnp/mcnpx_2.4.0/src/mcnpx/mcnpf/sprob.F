!#ident sprob.F from deck bs                                                    !/*bs          1*/
      subroutine sprob                                                          !/*bs          2*/
!        set up a built-in source function for distribution irs.                !/*bs          3*/
      use global_data                                                           !/*bsx         1*/
      use imcn_data                                                             !/*bsx         2*/
!                                                                               !/*bsx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*bsx         4*/
      implicit integer(kindi) (i-n)                                             !/*bsx         5*/
!                                                                               !/*bsx         6*/
      character ht*75                                                           !/*bs          6*/
      k(i)=ksd(lksd+i,irs)                                                      !/*bs          7*/
      q(i)=sqq(lsqq+i,irs)                                                      !/*bs          8*/
!                                                                               !/*bs          9*/
!        convert function number from problem name to index.                    !/*bs         10*/
      do 10 j=1,mjsf                                                            !/*bs         11*/
   10 if(k(2).eq.-jsf(j))ksd(lksd+2,irs)=j                                      !/*bs         12*/
!                                                                               !/*bs         13*/
!        print the distribution number, variable, and comment.                  !/*bs         14*/
      if(ink(10).eq.0)go to 50                                                  !/*bs         15*/
      kr=1                                                                      !/*bs         16*/
      if(nsr.eq.6)kr=2                                                          !/*bs         17*/
      write(iuo,20)k(1),hbln(k(12),kr)                                          !/*bs         18*/
   20 format(///25h probability distribution,i4,                                !/*bs         19*/
     & 21h for source variable ,a3)                                             !/*bs         20*/
      do 40 i=k(15)+1,k(15)+k(3),25                                             !/*bs         21*/
      do 30 j=i,i+24                                                            !/*bs         22*/
   30 ht((j-i)*3+1:(j-i)*3+3)=char(jscn(j)/65536)//                             !/*bs         23*/
     & char(mod(jscn(j),65536)/256)//char(mod(jscn(j),256))                     !/*bs         24*/
   40 write(iuo,'(10x,a75)')ht                                                  !/*bs         25*/
!                                                                               !/*bs         26*/
!        set up the particular function.                                        !/*bs         27*/
   50 go to(60,80,100,140,160,180,190,300,390)k(2)                              !/*bs         28*/
!                                                                               !/*bs         29*/
! >>>>>  spectrum 2 -- maxwell fission spectrum (endf law 7).                   !/*bs         30*/
   60 if(q(2).le.0.)sqq(lsqq+2,irs)=1.2895                                      !/*bs         31*/
      sqq(lsqq+3,irs)=2./(q(2)*sqrt(q(2)*pie))                                  !/*bs         32*/
      sqq(lsqq+12,irs)=.5*q(2)                                                  !/*bs         33*/
      eaa(leaa+irs)=1.5*q(2)                                                    !/*bs         34*/
      if(ink(10).ne.0)write(iuo,70)q(2),q(3)                                    !/*bs         35*/
   70 format(18h energy function 2,                                             !/*bs         36*/
     & 42h:  maxwell (fission) spectrum (endf law 7)//                          !/*bs         37*/
     & 5x,24hf(e)=c*sqrt(e)*exp(-e/a)/5x,3ha =,1pe11.4,5x,3hc =,e11.4)          !/*bs         38*/
      go to 410                                                                 !/*bs         39*/
!                                                                               !/*bs         40*/
! >>>>>  spectrum 3 -- watt (fission) spectrum (endf law 10).                   !/*bs         41*/
   80 sqq(lsqq+9,irs)=q(2)                                                      !/*bs         42*/
      if(q(2).eq.0.)sqq(lsqq+9,irs)=.965                                        !/*bs         43*/
      sqq(lsqq+8,irs)=q(3)                                                      !/*bs         44*/
      if(q(3).eq.0.)sqq(lsqq+8,irs)=2.29                                        !/*bs         45*/
      t2=q(9)*q(8)                                                              !/*bs         46*/
      t1=1.+.125*t2                                                             !/*bs         47*/
      sqq(lsqq+5,irs)=t1+sqrt(t1**2-1.)-1.                                      !/*bs         48*/
      sqq(lsqq+6,irs)=q(9)*(q(5)+1.)                                            !/*bs         49*/
      sqq(lsqq+7,irs)=2.*exp(-.25*t2)/sqrt(pie*t2*q(9)**2)                      !/*bs         50*/
      sqq(lsqq+12,irs)=1.                                                       !/*bs         51*/
      eaa(leaa+irs)=q(9)*(1.5+.25*t2)                                           !/*bs         52*/
      if(ink(10).ne.0)write(iuo,90)q(9),q(8),q(7)                               !/*bs         53*/
   90 format(18h energy function 3,                                             !/*bs         54*/
     & 40h:  watt (fission) spectrum (endf law 10)//                            !/*bs         55*/
     & 5x,32hf(e)=c*exp(-e/a)*sinh(sqrt(b*e))/                                  !/*bs         56*/
     & 5x,3ha =,1pe11.4,5x,3hb =,e11.4,5x,3hc =,e11.4)                          !/*bs         57*/
      go to 410                                                                 !/*bs         58*/
!                                                                               !/*bs         59*/
! >>>>>  spectrum 4 -- gaussian (fusion) spectrum.                              !/*bs         60*/
  100 t1=-q(2)                                                                  !/*bs         61*/
      if(t1.lt.1.e-6.or.t1.gt..1)t1=.01                                         !/*bs         62*/
      if(q(2).lt.0..and.q(3).gt.0.)call erprnt(1,1,1,irs,0,0,0,0,               !/*bs         63*/
     & '26hinconsistent entries on sp,i3,6h card.')                             !/*bs         64*/
      if(q(3).gt.0.)go to 120                                                   !/*bs         65*/
      if(q(3).gt.-1.5)go to 110                                                 !/*bs         66*/
      sqq(lsqq+3,irs)=2.449408+.4697881*t1**(2.*third)+.37570337*t1             !/*bs         67*/
      if(q(2).le.0.)sqq(lsqq+2,irs)=sqrt(1.0018756*q(3)*t1)                     !/*bs         68*/
      go to 120                                                                 !/*bs         69*/
  110 sqq(lsqq+3,irs)=14.048204+.53199914*t1**(2.*third)+.30085131*t1           !/*bs         70*/
      if(q(2).le.0.)sqq(lsqq+2,irs)=sqrt(.80227015*q(3)*t1)                     !/*bs         71*/
  120 sqq(lsqq+4,irs)=1./(q(2)*sqrt(pie))                                       !/*bs         72*/
      sqq(lsqq+11,irs)=1.                                                       !/*bs         73*/
      eaa(leaa+irs)=q(3)                                                        !/*bs         74*/
      if(ink(10).ne.0)write(iuo,130)t1,(q(i),i=2,4),1.6651092*q(2)              !/*bs         75*/
  130 format(18h energy function 4,                                             !/*bs         76*/
     & 29h:  gaussian (fusion) spectrum//                                       !/*bs         77*/
     & 5x,25hf(e)=c*exp(-((e-b)/a)**2)//5(8x,6hfusion)/                         !/*bs         78*/
     & 3x,11htemperature,6x,8hwidth(a),5x,9henergy(b),3x,11hconstant(c),        !/*bs         79*/
     & 10x,4hfwhm/1p5e14.4)                                                     !/*bs         80*/
      go to 410                                                                 !/*bs         81*/
!                                                                               !/*bs         82*/
! >>>>>  spectrum 5 -- evaporation spectrum (endf law 9).                       !/*bs         83*/
  140 if(q(2).le.0.)sqq(lsqq+2,irs)=1.2895                                      !/*bs         84*/
      sqq(lsqq+3,irs)=1./q(2)**2                                                !/*bs         85*/
      sqq(lsqq+12,irs)=q(2)                                                     !/*bs         86*/
      eaa(leaa+irs)=2.*q(2)                                                     !/*bs         87*/
      if(ink(10).ne.0)write(iuo,150)q(2),q(3)                                   !/*bs         88*/
  150 format(18h energy function 5,                                             !/*bs         89*/
     & 36h:  evaporation spectrum (endf law 9)//                                !/*bs         90*/
     & 5x,18hf(e)=c*e*exp(-e/a)/5x,3ha =,1pe11.4,5x,3hc =,e11.4)                !/*bs         91*/
      go to 410                                                                 !/*bs         92*/
!                                                                               !/*bs         93*/
! >>>>>  spectrum 6 -- modified gaussian (fusion) spectrum.                     !/*bs         94*/
  160 t1=-q(2)                                                                  !/*bs         95*/
      if(t1.lt.1.e-6.or.t1.gt..1)t1=.01                                         !/*bs         96*/
      if(q(2).le.0..and.q(3).eq.-2.)sqq(lsqq+2,irs)=sqrt(.25046891*t1)          !/*bs         97*/
      if(q(2).le.0..and.q(3).le.0.)sqq(lsqq+2,irs)=sqrt(.20056755*t1)           !/*bs         98*/
      if(q(3).lt.-1.5)                                                          !/*bs         99*/
     & sqq(lsqq+3,irs)=2.449408+.4697881*t1**(2.*third)+.37570337*t1            !/*bs        100*/
      if(q(3).le.0.)                                                            !/*bs        101*/
     & sqq(lsqq+3,irs)=14.048204+.53199914*t1**(2.*third)+.30085131*t1          !/*bs        102*/
      sqq(lsqq+3,irs)=sqrt(q(3))                                                !/*bs        103*/
      t2=q(3)*q(2)*sqrt(pie)*(1.+erf2(q(3)/q(2)))                               !/*bs        104*/
      sqq(lsqq+4,irs)=1./(t2+q(2)**2*exp(-(q(3)/q(2))**2))                      !/*bs        105*/
      sqq(lsqq+12,irs)=q(3)**2                                                  !/*bs        106*/
      eaa(leaa+irs)=q(3)**2+q(2)**2*(1.+.5*q(4)*t2)                             !/*bs        107*/
      t3=2.*q(2)*(q(2)*.69314718+2.*sqrt(.69314718*q(3)**2))                    !/*bs        108*/
      if(ink(10).ne.0)write(iuo,170)t1,q(2),q(3)**2,q(4),t3                     !/*bs        109*/
  170 format(18h energy function 6,                                             !/*bs        110*/
     & 38h:  modified gaussian (fusion) spectrum//                              !/*bs        111*/
     & 5x,37hf(e)=c*exp(-((sqrt(e)-sqrt(b))/a)**2),                             !/*bs        112*/
     & //5(8x,6hfusion)/3x,11htemperature,6x,8hwidth(a),5x,9henergy(b),         !/*bs        113*/
     & 3x,11hconstant(c),10x,4hfwhm/1p5e14.4)                                   !/*bs        114*/
      go to 410                                                                 !/*bs        115*/
!                                                                               !/*bs        116*/
! >>>>>  spectrum 7 -- spare.                                                   !/*bs        117*/
  180 call erprnt(1,1,1,irs,0,0,0,0,                                            !/*bs        118*/
     & '34hfunction -7 of source distribution,i3,16h is not defined.')          !/*bs        119*/
      go to 420                                                                 !/*bs        120*/
!                                                                               !/*bs        121*/
! >>>>>  function 21 -- power law.                                              !/*bs        122*/
  190 i=k(12)                                                                   !/*bs        123*/
      if(i.ne.5.and.i.ne.10.and.i.ne.11)go to 440                               !/*bs        124*/
      go to(200,210,230)min(3,k(4)+1)                                           !/*bs        125*/
  200 sqq(lsqq+7,irs)=1.                                                        !/*bs        126*/
      if(k(12).ne.5)go to 380                                                   !/*bs        127*/
      go to 240                                                                 !/*bs        128*/
  210 if(i.ne.5)go to 220                                                       !/*bs        129*/
      sqq(lsqq+6,irs)=spf(k(13)+1,1)                                            !/*bs        130*/
      sqq(lsqq+7,irs)=1.                                                        !/*bs        131*/
      if(q(6).lt.0..or.q(6).gt.1.)go to 380                                     !/*bs        132*/
      go to 240                                                                 !/*bs        133*/
  220 if(i.eq.11)sqq(lsqq+6,irs)=-abs(spf(k(13)+1,1))                           !/*bs        134*/
      sqq(lsqq+7,irs)=abs(spf(k(13)+1,1))                                       !/*bs        135*/
      go to 240                                                                 !/*bs        136*/
  230 sqq(lsqq+6,irs)=spf(k(13)+1,1)                                            !/*bs        137*/
      sqq(lsqq+7,irs)=spf(k(13)+1,k(4))                                         !/*bs        138*/
  240 if(q(6).gt.q(7))go to 380                                                 !/*bs        139*/
      if(k(4).lt.3)ksd(lksd+4,irs)=0                                            !/*bs        140*/
      if(q(1).ne.0..and.q(2).ne.12345.)go to 250                                !/*bs        141*/
      sqq(lsqq+2,irs)=12345.                                                    !/*bs        142*/
      if(i.eq.5)sqq(lsqq+2,irs)=1.                                              !/*bs        143*/
      if(i.eq.11)sqq(lsqq+2,irs)=0.                                             !/*bs        144*/
      if(i.eq.10.and.idefv(12).ne.0.and.ivdis(12).eq.0)sqq(lsqq+2,irs)=1        !/*bs        145*/
  250 if(q(4).eq.0..or.q(5).eq.12345.)sqq(lsqq+5,irs)=q(2)                      !/*bs        146*/
      if(q(2).eq.-1..or.q(5).eq.-1.)go to 380                                   !/*bs        147*/
      sqq(lsqq+3,irs)=1.                                                        !/*bs        148*/
      if(q(5).eq.12345.)go to 260                                               !/*bs        149*/
      sqq(lsqq+8,irs)=sign(abs(q(6))**(q(5)+1.),q(6))                           !/*bs        150*/
      sqq(lsqq+9,irs)=sign(abs(q(7))**(q(5)+1.),q(7))-q(8)                      !/*bs        151*/
      sqq(lsqq+10,irs)=1./(q(5)+1.)                                             !/*bs        152*/
      if(q(2).ne.12345..and.q(2).ne.q(5))                                       !/*bs        153*/
     & sqq(lsqq+3,irs)=(q(2)+1.)*q(9)*q(10)/                                    !/*bs        154*/
     & (sign(abs(q(7))**(q(2)+1.),q(7))-sign(abs(q(6))**(q(2)+1.),q(6)))        !/*bs        155*/
  260 if(k(4).ne.0.and.q(2).eq.12345.)go to 380                                 !/*bs        156*/
      if(ink(10).ne.0.and.q(2).ne.12345.)write(iuo,270)q(2)                     !/*bs        157*/
  270 format(14h power law 21:,                                                 !/*bs        158*/
     & 5x,16hf(x)=c*abs(x)**k,10x,3hk =,1pe11.4)                                !/*bs        159*/
      if(ink(10).ne.0.and.q(2).eq.12345.)write(iuo,280)                         !/*bs        160*/
  280 format(14h power law 21:,5x,16hf(x)=c*abs(x)**k)                          !/*bs        161*/
      if(ink(10).ne.0.and.q(5).ne.q(2))write(iuo,290)q(5)                       !/*bs        162*/
  290 format(5x,15hbiased with k =,1pe11.4)                                     !/*bs        163*/
      if(i.ne.10.or.idefv(12).eq.0.or.ivdis(12).ne.0.or.q(2).eq.1)              !/*bs        164*/
     & go to 420                                                                !/*bs        165*/
      jrad=1                                                                    !/*bs        166*/
      call erprnt(1,2,0,0,0,0,0,0,                                              !/*bs        167*/
     & '53ha<>1 for sampling source rad in cylindrical geometry.')              !/*bs        168*/
      go to 420                                                                 !/*bs        169*/
!                                                                               !/*bs        170*/
! >>>>>  function 31 -- exponential.                                            !/*bs        171*/
  300 if(k(12).ne.5.and.k(12).ne.11)go to 440                                   !/*bs        172*/
      sqq(lsqq+6,irs)=-1.                                                       !/*bs        173*/
      sqq(lsqq+7,irs)=1.                                                        !/*bs        174*/
      if(k(12).eq.11.or.k(4).eq.0)go to 310                                     !/*bs        175*/
      sqq(lsqq+6,irs)=spf(k(13)+1,1)                                            !/*bs        176*/
      if(k(4).ge.2)sqq(lsqq+7,irs)=spf(k(13)+1,k(4))                            !/*bs        177*/
  310 if(q(6).gt.q(7))go to 380                                                 !/*bs        178*/
      if(abs(q(2)).lt..003)sqq(lsqq+2,irs)=0.                                   !/*bs        179*/
      if(abs(q(5)).lt..003)sqq(lsqq+5,irs)=0.                                   !/*bs        180*/
      if(q(4).eq.0.)sqq(lsqq+5,irs)=q(2)                                        !/*bs        181*/
      sqq(lsqq+3,irs)=1.                                                        !/*bs        182*/
      sqq(lsqq+8,irs)=q(6)                                                      !/*bs        183*/
      sqq(lsqq+9,irs)=q(7)-q(6)                                                 !/*bs        184*/
      if(q(5).eq.0.)go to 320                                                   !/*bs        185*/
      sqq(lsqq+8,irs)=exp(q(5)*q(6))                                            !/*bs        186*/
      sqq(lsqq+9,irs)=exp(q(5)*q(7))-q(8)                                       !/*bs        187*/
      sqq(lsqq+10,irs)=1./q(5)                                                  !/*bs        188*/
  320 if(k(4).lt.3)ksd(lksd+4,irs)=0                                            !/*bs        189*/
      if(q(5).eq.q(2))go to 330                                                 !/*bs        190*/
      sqq(lsqq+3,irs)=q(9)                                                      !/*bs        191*/
      if(q(2).eq.0.)sqq(lsqq+3,irs)=q(3)/(q(7)-q(6))                            !/*bs        192*/
      if(q(5).ne.0.)sqq(lsqq+3,irs)=q(3)/q(5)                                   !/*bs        193*/
      if(q(2).ne.0.)sqq(lsqq+3,irs)=q(3)*q(2)/                                  !/*bs        194*/
     & (exp(q(2)*q(7))-exp(q(2)*q(6)))                                          !/*bs        195*/
  330 if(ink(10).ne.0)write(iuo,340)q(2)                                        !/*bs        196*/
  340 format(25h exponential function 31:,                                      !/*bs        197*/
     & 5x,15hf(x)=c*exp(k*x),10x,3hk =,1pe11.4)                                 !/*bs        198*/
      if(ink(10).eq.0.or.q(5).eq.q(2))go to 420                                 !/*bs        199*/
      write(iuo,350)q(5)                                                        !/*bs        200*/
  350 format(5x,15hbiased with k =,1pe11.4//12h  cumulative,13x,                !/*bs        201*/
     & 6hweight/31h  probability     mu     factor/)                            !/*bs        202*/
      do 360 i=1,5                                                              !/*bs        203*/
      c=q(8)+q(9)*(i-1)/4.                                                      !/*bs        204*/
      if(q(5).ne.0.)c=q(10)*log(c)                                              !/*bs        205*/
  360 write(iuo,370)(i-1)/4.,c,q(3)*exp(c*(q(2)-q(5)))                          !/*bs        206*/
  370 format(f9.2,f12.3,f9.3)                                                   !/*bs        207*/
      go to 420                                                                 !/*bs        208*/
  380 call erprnt(1,1,1,k(1),0,0,0,0,                                           !/*bs        209*/
     & '19hsource distribution,i4,24h is incorrectly defined.')                 !/*bs        210*/
      return                                                                    !/*bs        211*/
!                                                                               !/*bs        212*/
! >>>>>  spectrum 41 -- normal distribution.                                    !/*bsxk        1*/
  390 sqq(lsqq+2,irs)=q(2)/1.6651092                                            !/*bs        214*/
      if(q(2).eq.0.)call erprnt(1,1,0,0,0,0,0,0,                                !/*bs        215*/
     & '41hsource sp function -41 has zero argument.')                          !/*bs        216*/
      if(q(2).eq.0.)sqq(lsqq+2,irs)=1.                                          !/*bs        217*/
      sqq(lsqq+4,irs)=1./(q(2)*sqrt(pie))                                       !/*bs        218*/
      sqq(lsqq+11,irs)=1.                                                       !/*bs        219*/
      eaa(leaa+irs)=q(3)                                                        !/*bs        220*/
      if(ink(10).ne.0)write(iuo,400)(q(i),i=2,4),1.6651092*q(2)                 !/*bs        221*/
  400 format(34h spectrum 41:  normal distribution,                             !/*bsxk        2*/
     & 5x,25hf(t)=c*exp(-((t-b)/a)**2)//                                        !/*bs        223*/
     & 6x,8hwidth(a),7x,7hmean(b),3x,11hconstant(c),10x,4hfwhm/1p4e14.4)        !/*bs        224*/
      if(k(12).ne.4.and.k(12).ne.7.and.k(12).ne.8.and.k(12).ne.9)               !/*bsx         7*/
     & go to 440                                                                !/*bsx         8*/
      go to 420                                                                 !/*bs        226*/
!                                                                               !/*bs        227*/
!        set up equiprobable bins if necessary.                                 !/*bs        228*/
  410 if(k(4).eq.0.and.ecf(nips).ge.eaa(leaa+irs))call erprnt                   !/*bs        229*/
     & (1,2,0,0,0,0,0,0,'33hinefficient source.  use si card.')                 !/*bs        230*/
      if(k(12).ne.3)go to 440                                                   !/*bs        231*/
  420 if(k(4).ne.0)call eqpbbn                                                  !/*bs        232*/
      if(ink(10).ne.0.and.eaa(leaa+irs).ne.0.)                                  !/*bs        233*/
     & write(iuo,430)k(1),eaa(leaa+irs)                                         !/*bs        234*/
  430 format(/5x,31hthe mean of source distribution,i4,3h is,1pe12.4)           !/*bs        235*/
      return                                                                    !/*bs        236*/
!                                                                               !/*bs        237*/
!        fatal error if function is used for wrong variable.                    !/*bs        238*/
  440 ht='8hfunction,i4,16h of distribution,i4,16h is used for '//              !/*bs        239*/
     & hbln(k(12),1)                                                            !/*bs        240*/
      call erprnt(1,1,2,jsf(k(2)),k(1),0,0,0,ht)                                !/*bs        241*/
      return                                                                    !/*bs        242*/
      end                                                                       !/*bs        243*/
