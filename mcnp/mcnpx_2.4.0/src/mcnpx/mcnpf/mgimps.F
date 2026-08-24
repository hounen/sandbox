!#ident mgimps.F from deck mg                                                   !/*mg          1*/
      subroutine mgimps                                                         !/*mg          2*/
!        check multigroup tables and calculate multigroup arrays.               !/*mg          3*/
      use global_data                                                           !/*mgx         1*/
!                                                                               !/*mgx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*mgx         3*/
      implicit integer(kindi) (i-n)                                             !/*mgx         4*/
!                                                                               !/*mgx         5*/
!                                                                               !/*mg          5*/
!        check igm.                                                             !/*mg          6*/
      m=0                                                                       !/*mg          7*/
      do 10 i=1,mipt                                                            !/*mg          8*/
   10 m=m+jgm(i)                                                                !/*mg          9*/
      if(m.ne.igm)call erprnt(1,1,0,0,0,0,0,0,                                  !/*mg         10*/
     & '39higm .ne. total number of energy groups.')                            !/*mg         11*/
!                                                                               !/*mg         14*/
!        check that the cross-section tables are compatible.                    !/*mg         15*/
      m1=0                                                                      !/*mg         16*/
      m2=0                                                                      !/*mg         17*/
      do 20 i=1,ntal                                                            !/*mg         18*/
   20 if(jptal(ljpt+2,i).eq.5)m2=1                                              !/*mg         19*/
      do 70 ie=1,mxe                                                            !/*mg         20*/
      ik=mgegbt(max(1,nxs(lnxs+12,ie)))                                         !/*mg         21*/
!                                                                               !/*mg         22*/
!        check for inconsistent energy-group structures.                        !/*mg         23*/
      do 30 i=4,7                                                               !/*mg4c        1*/
   30 if(i.ne.4.and.nxs(lnxs+i,ie).ne.nxs(lnxs+i,ik))m1=1                       !/*mg         25*/
      do 40 i=1,jgm(max(1,nxs(lnxs+12,ie)))*2                                   !/*mg         26*/
   40 if(abs(xss(jxs(ljxs+1,ie)+i-1)-xss(jxs(ljxs+1,ik)+i-1)).gt.               !/*mg         27*/
     & 1e-6*xss(jxs(ljxs+1,ik)+i-1))m1=1                                        !/*mg         28*/
      do 50 j=1,nxs(lnxs+8,ie)                                                  !/*mg         29*/
      k=nint(xss(jxs(ljxs+11,ie)+j-1))                                          !/*mgx         6*/
      l=jxs(ljxs+1,ie)-1+nint(xss(jxs(ljxs+12,ie)+j-1))                         !/*mg4c        2*/
      if(nint(xss(l)).ne.jgm(k))m1=1                                            !/*mgx         7*/
      do 50 i=1,jgm(k)*2                                                        !/*mg         33*/
   50 if(abs(xss(l+i)-xss(jxs(ljxs+1,mgegbt(k))+i-1)).gt.                       !/*mg         34*/
     & 1e-6*xss(jxs(ljxs+1,mgegbt(k))+i-1))m1=1                                 !/*mg         35*/
!                                                                               !/*mg         36*/
!        check for detectors with discrete-angle scattering.                    !/*mg         37*/
      if(m2.ne.1)go to 70                                                       !/*mg         38*/
      if(nxs(lnxs+9,ie).ne.0)m2=2                                               !/*mg         39*/
      do 60 i=1,nxs(lnxs+8,ie)                                                  !/*mg         40*/
   60 if(nint(xss(jxs(ljxs+14,ie)+i-1)).ne.0)m2=2                               !/*mgx         8*/
   70 continue                                                                  !/*mg         42*/
      if(m1.ne.0)call erprnt(1,1,0,0,0,0,0,0,                                   !/*mg         43*/
     & '42hthe cross-section tables are incompatible.')                         !/*mg         44*/
      if(m2.eq.2)call erprnt(1,1,0,0,0,0,0,0,                                   !/*mg         45*/
     & '52hdetectors are incompatible with discrete scattering.')               !/*mg         46*/
!                                                                               !/*mg4b        1*/
!        load photon group boundaries for photon production summary.            !/*mg4b        2*/
      if(jgm(2).eq.0)go to 77                                                   !/*mg4c2       1*/
      if(mgegbt(2).ne.0)l=jxs(ljxs+1,mgegbt(2))-1                               !/*mg4b        3*/
      do 75 i=1,jgm(2)                                                          !/*mg4b        4*/
   75 ebl(lebl+i)=xss(l+i)-.5*xss(l+i+jgm(2))                                   !/*mg4b        5*/
      ebl(lebl+jgm(2))=max(ebl(lebl+jgm(2)),zero)                               !/*mg4b        6*/
!                                                                               !/*mg         47*/
!        accumulate the numbers of groups.                                      !/*mg         48*/
   77 do 80 i=2,mipt+1                                                          !/*mg4c2       2*/
   80 mgm(i)=mgm(i-1)+jgm(i-1)                                                  !/*mg         50*/
!                                                                               !/*mg         51*/
!        calculate group-center velocities.                                     !/*mg         52*/
      do 90 ip=1,mipt                                                           !/*mg         53*/
      ie=mgegbt(ip)                                                             !/*mg         54*/
      do 90 i=1,jgm(ip)                                                         !/*mg         55*/
      e=xss(jxs(ljxs+1,ie)+i-1)                                                 !/*mg         56*/
      gvl(lgvl+i+mgm(ip))=slite                                                 !/*mg         57*/
   90 if(ip.ne.2)gvl(lgvl+i+mgm(ip))=slite*sqrt(e*(e+2.*gpt(ip)))/              !/*mg         58*/
     & (e+gpt(ip))                                                              !/*mg         59*/
!                                                                               !/*mg         60*/
!        convert kcode energies into group numbers.                             !/*mg         61*/
      do 120 i=4,6*msrk,6                                                       !/*mg         62*/
      if(fso(lfso+i).eq.0.)go to 120                                            !/*mg         63*/
      do 100 j=1,jgm(1)-1                                                       !/*mg         64*/
  100 if(fso(lfso+i).gt.xss(jxs(ljxs+1,mgegbt(1))+j-1)-                         !/*mg         65*/
     & .5*xss(jxs(ljxs+1,mgegbt(1))+jgm(1)+j-1))go to 110                       !/*mg         66*/
  110 fso(lfso+i)=j                                                             !/*mg         67*/
  120 continue                                                                  !/*mg         68*/
!                                                                               !/*mg         69*/
!        derive other-way fluxes from weight windows.                           !/*mg         70*/
      if(iplt.ne.1.and.isb.ne.1)go to 170                                       !/*mg         71*/
      if(wwm(1).eq.0.)go to 125                                                 !/*mg4b        7*/
      call erprnt(1,1,0,0,0,0,0,0,                                              !/*mg4b        8*/
     & '53hweight-windows from wwinp, other-way fluxes disabled.')              !/*mgx         9*/
      return                                                                    !/*mg4b       10*/
  125 do 140 ip=1,mipt                                                          !/*mg4b       11*/
      if(nww(ip).eq.jgm(ip))go to 130                                           !/*mg         73*/
      call erprnt(1,1,0,0,0,0,0,0,                                              !/*mg         74*/
     & '50hweight windows have wrong number of energy groups.')                 !/*mg         75*/
      return                                                                    !/*mg         76*/
  130 do 140 j=1,jgm(ip)                                                        !/*mg         77*/
      d=1.                                                                      !/*mg         78*/
      if(img.eq.0)d=xss(jxs(ljxs+1,1)+jgm(1)+j-1)                               !/*mg         79*/
      do 140 i=1,mxa                                                            !/*mg         80*/
      l=lwwf+i+mxa*(nww(ip)-j+mww(ip))                                          !/*mg         81*/
  140 if(wwf(l).ne.0.)gmg(lgmg+i+mxa*(j-1+mgm(ip)))=d/wwf(l)                    !/*mg         82*/
!                                                                               !/*mg         83*/
!        derive energy-dependent importances from other-way fluxes.             !/*mg         84*/
      if(iplt.ne.1)go to 170                                                    !/*mg         85*/
      do 160 ip=1,mipt                                                          !/*mg         86*/
      do 160 jg=1,jgm(ip)                                                       !/*mg         87*/
      t=huge                                                                    !/*mg         88*/
      do 150 ia=1,mxa                                                           !/*mg         89*/
  150 if(fim(lfim+ip,ia).ne.0..and.gmg(lgmg+ia+mxa*(jg-1+mgm(ip))).gt.          !/*mg         90*/
     & 0.)t=min(t,gmg(lgmg+ia+mxa*(jg-1+mgm(ip))))                              !/*mg         91*/
      do 160 ia=1,mxa                                                           !/*mg         92*/
  160 cmg(lcmg+ia+mxa*(jg-1+mgm(ip)))=                                          !/*mg         93*/
     & max(t,gmg(lgmg+ia+mxa*(jg-1+mgm(ip))))                                   !/*mg         94*/
!                                                                               !/*mg         95*/
!        calculate tables for biased adjoint energy sampling.                   !/*mg         96*/
  170 if(mcal.ne.2)return                                                       !/*mg         97*/
      do 270 ip=1,mipt                                                          !/*mg         98*/
      if(jgm(ip).eq.0)go to 270                                                 !/*mg         99*/
      do 260 ia=1,mxa                                                           !/*mg        100*/
      if(mat(lmat+ia).eq.0)go to 260                                            !/*mg        101*/
      go to(210,200,180)isb                                                     !/*mg        102*/
!                                                                               !/*mg        103*/
! >>>>>  isb=2 -- set up for unbiased sampling.                                 !/*mg        104*/
  180 do 190 jg=1,jgm(ip)                                                       !/*mg        105*/
  190 gmg(lgmg+ia+mxa*(jg-1+mgm(ip)))=1.                                        !/*mg        106*/
      go to 260                                                                 !/*mg        107*/
!                                                                               !/*mg        108*/
! >>>>>  isb=1 -- use recorded fluxes, if any.                                  !/*mg        109*/
  200 if(gmg(lgmg+ia+mxa*mgm(ip)).gt.0.)go to 260                               !/*mg        110*/
!                                                                               !/*mg        111*/
! >>>>>  isb=0 or isb=1 with no recorded fluxes.                                !/*mg        112*/
!        use infinite-medium fluxes.                                            !/*mg        113*/
  210 j=mat(lmat+ia)                                                            !/*mg        114*/
      do 230 jg=1,jgm(ip)                                                       !/*mg        115*/
      s=0.                                                                      !/*mg        116*/
      r=0.                                                                      !/*mg        117*/
      do 220 m=jmd(ljmd+j),jmd(ljmd+j+1)-1                                      !/*mg        118*/
      ie=lme(llme+ip,m)                                                         !/*mg        119*/
      lb=jxs(ljxs+1,ie)+nint(xss(jxs(ljxs+13,ie)))+                             !/*mg4c        3*/
     & nint(xss(jxs(ljxs+18,ie)+jg-1))+min(jg-1,nxs(lnxs+6,ie))-2               !/*mg4c        4*/
      s=s+xss(lb)*fme(lfme+m)                                                   !/*mg        122*/
  220 r=r+xss(jxs(ljxs+2,ie)-1+jg)*fme(lfme+m)                                  !/*mg        123*/
      l=lgmg+ia+mxa*(jg-1+mgm(ip))                                              !/*mg        124*/
      if(jg.eq.1)gmg(l)=1./(r*rho(lrho+ia))                                     !/*mg        125*/
      if(jg.eq.1)go to 230                                                      !/*mg        126*/
      gmg(l)=gmg(l-mxa)                                                         !/*mg        127*/
      if(s.lt.r)gmg(l)=max(.1*gmg(l),                                           !/*mg        128*/
     & min(10.*gmg(l),1./((r-s)*rho(lrho+ia))))                                 !/*mg        129*/
  230 continue                                                                  !/*mg        130*/
      if(isb.eq.1)write(iuo,240)ncl(lncl+ia)                                    !/*mg        131*/
  240 format(/21h  flux values in cell,i6,26h have been set to infinite,        !/*mg        132*/
     & 15h medium values. )                                                     !/*mg        133*/
      if(img.eq.0)go to 260                                                     !/*mg        134*/
      do 250 jg=1,jgm(ip)                                                       !/*mg        135*/
      l=lgmg+ia+mxa*(jg-1+mgm(ip))                                              !/*mg        136*/
  250 gmg(l)=gmg(l)/xss(jxs(ljxs+1,ie)+jgm(ip)+jg-1)                            !/*mg        137*/
  260 continue                                                                  !/*mg        138*/
  270 continue                                                                  !/*mg        139*/
!                                                                               !/*mg        140*/
!        normalize gmg values so that fmg, pmg, and smg are                     !/*mg        141*/
!        comparable between runs.                                               !/*mg        142*/
      t=0.                                                                      !/*mg        143*/
      do 280 ip=1,mipt                                                          !/*mg        144*/
      do 280 jg=1,jgm(ip)                                                       !/*mg        145*/
      do 280 i=1,mxa                                                            !/*mg        146*/
  280 t=t+gmg(lgmg+i+mxa*(jg-1+mgm(ip)))/gvl(lgvl+jg+mgm(ip))                   !/*mg        147*/
      do 290 ip=1,mipt                                                          !/*mg        148*/
      do 290 jg=1,jgm(ip)                                                       !/*mg        149*/
      do 290 i=1,mxa                                                            !/*mg        150*/
  290 gmg(lgmg+i+mxa*(jg-1+mgm(ip)))=gmg(lgmg+i+mxa*(jg-1+mgm(ip)))/t           !/*mg        151*/
!                                                                               !/*mg        152*/
!        calculate pmg, smg, and fmg.                                           !/*mg        153*/
      do 370 ip=1,mipt                                                          !/*mg        154*/
      if(jgm(ip).eq.0)go to 370                                                 !/*mg        155*/
      do 360 ia=1,mxa                                                           !/*mg        156*/
      j=mat(lmat+ia)                                                            !/*mg        157*/
      do 350 m=jmd(ljmd+j),jmd(ljmd+j+1)-1                                      !/*mg        158*/
      kp=ipan(lipa+ia)+m-jmd(ljmd+j)                                            !/*mg        159*/
      ie=lme(llme+ip,m)                                                         !/*mg        160*/
      l=jxs(ljxs+1,ie)-1+nint(xss(jxs(ljxs+13,ie)))                             !/*mg4c        5*/
      do 320 jg=1,jgm(ip)                                                       !/*mg        162*/
      f=gmg(lgmg+ia+mxa*(jg-1+mgm(ip)))                                         !/*mg        163*/
      if(jxs(ljxs+4,ie).eq.0)go to 300                                          !/*mg        164*/
      t1=xss(jxs(ljxs+4,ie)-1+jg)*xss(jxs(ljxs+3,ie)-1+jg)*f                    !/*mg        165*/
      fmg(lfmg+kp)=fmg(lfmg+kp)+t1                                              !/*mg        166*/
  300 do 310 jh=1,jgm(ip)                                                       !/*mg        167*/
      k=lpmg+kp+npn*(jh-1+mgm(ip))                                              !/*mg        168*/
      if(jxs(ljxs+5,ie).ne.0)pmg(k)=pmg(k)+t1*xss(jxs(ljxs+5,ie)-1+jh)          !/*mg        169*/
      if(jg-jh.gt.nxs(lnxs+6,ie).or.jh-jg.gt.nxs(lnxs+7,ie))go to 310           !/*mg        170*/
      pmg(k)=pmg(k)+xss(l)*f                                                    !/*mg        171*/
      l=l+1                                                                     !/*mg        172*/
  310 continue                                                                  !/*mg        173*/
  320 continue                                                                  !/*mg        174*/
      if(ip.ne.2)go to 340                                                      !/*mg        175*/
      do 330 jg=1,jgm(1)                                                        !/*mg        176*/
      f=gmg(lgmg+ia+mxa*(jg-1+mgm(1)))                                          !/*mg        177*/
      l=jxs(ljxs+1,lme(llme+1,m))+nint(xss(jxs(ljxs+13,                         !/*mgx        10*/
     & lme(llme+1,m))+1))+jgm(2)*(jg-1)-2                                       !/*mgx        11*/
      do 330 jh=1,jgm(2)                                                        !/*mg        180*/
      k=lpmg+kp+npn*(jh-1+mgm(2))                                               !/*mg        181*/
  330 pmg(k)=pmg(k)+xss(l+jh)*f                                                 !/*mg        182*/
  340 do 350 jh=1,jgm(ip)                                                       !/*mg        183*/
      k=lsmg+ia+mxa*(jh-1+mgm(ip))                                              !/*mg        184*/
  350 smg(k)=smg(k)+pmg(lpmg+kp+npn*(jh-1+mgm(ip)))*fme(lfme+m)                 !/*mg        185*/
  360 continue                                                                  !/*mg        186*/
  370 continue                                                                  !/*mg        187*/
!                                                                               !/*mg        188*/
!        print all this stuff if required.                                      !/*mg        189*/
      if(ink(85).eq.0)return                                                    !/*mg        190*/
      do 500 ip=1,mipt                                                          !/*mg        191*/
      if(jgm(ip).eq.0)go to 500                                                 !/*mg        192*/
      write(iuo,380)hnp(ip)                                                     !/*mg        193*/
  380 format(1h1,a11,46h flux values input to bias adjoint calculation)         !/*mgx        12*/
      do 390 ia=1,mxa                                                           !/*mg        195*/
  390 if(mat(lmat+ia).ne.0)write(iuo,400)ncl(lncl+ia),                          !/*mg        196*/
     & (gmg(lgmg+ia+mxa*(j-1+mgm(ip))),j=1,jgm(ip))                             !/*mg        197*/
  400 format(/21h flux values for cell,i6,24h ordered by energy group/          !/*mg        198*/
     & (1p10e12.3))                                                             !/*mg        199*/
      ml=0                                                                      !/*mg        200*/
      do 460 ia=1,mxa                                                           !/*mg        201*/
      j=mat(lmat+ia)                                                            !/*mg        202*/
      if(npq(lnpq+j).le.1)go to 460                                             !/*mg        203*/
      if(ml.eq.0)write(iuo,410)hnp(ip)                                          !/*mg        204*/
  410 format(///1x,a11,26h adjoint density functions/                           !/*mgx        13*/
     & /32h pmg matrix for sampling nuclide)                                    !/*mg        206*/
      ml=1                                                                      !/*mg        207*/
      write(iuo,420)ncl(lncl+ia)                                                !/*mg        208*/
  420 format(/5h cell,i6)                                                       !/*mg        209*/
      k=lpmg+ipan(lipa+ia)-jmd(ljmd+j)+npn*mgm(ip)                              !/*mg4c        6*/
      do 440 jg=1,jgm(ip),9                                                     !/*mg        211*/
      l=min(jg+8,jgm(ip))                                                       !/*mg        212*/
      write(iuo,430)(i,i=jg,l)                                                  !/*mg        213*/
  430 format(/10x,9(3x,5hgroup,i4:))                                            !/*mg        214*/
      do 440 m=jmd(ljmd+j),jmd(ljmd+j+1)-1                                      !/*mg        215*/
  440 write(iuo,450)iza(liza+m),(pmg(k+(i-1)*npn+m),i=jg,l)                     !/*mg        216*/
  450 format(i11,1p9e12.4)                                                      !/*mg        217*/
  460 continue                                                                  !/*mg        218*/
      if(ml.eq.0)go to 500                                                      !/*mg        219*/
      write(iuo,470)hnp(ip)                                                     !/*mg        220*/
  470 format(///1x,a11,37h smg matrix, which is sum of pmg over,                !/*mgx        14*/
     & 38h nuclides weighted with atom fractions)                               !/*mgx        15*/
      do 480 ia=1,mxa                                                           !/*mg        223*/
  480 if(npq(lnpq+mat(lmat+ia)).gt.1)write(iuo,490)ncl(lncl+ia),                !/*mg        224*/
     & (smg(lsmg+ia+mxa*(j-1+mgm(ip))),j=1,jgm(ip))                             !/*mg        225*/
  490 format(/13h smg for cell,i6,24h ordered by energy group/                  !/*mg        226*/
     & (1p10e12.3))                                                             !/*mg        227*/
  500 continue                                                                  !/*mg        228*/
      if(kpt(1).eq.0)return                                                     !/*mg        229*/
      k=0                                                                       !/*mg        230*/
      do 570 ia=1,mxa                                                           !/*mg        231*/
      j=mat(lmat+ia)                                                            !/*mg        232*/
      do 510 m=jmd(ljmd+j),jmd(ljmd+j+1)-1                                      !/*mg        233*/
  510 if(jxs(ljxs+4,lme(llme+1,m)).ne.0)go to 520                               !/*mg        234*/
      go to 570                                                                 !/*mg        235*/
  520 if(k.eq.0)write(iuo,530)                                                  !/*mg        236*/
  530 format(///32h fmg matrix for sampling fission)                            !/*mg        237*/
      k=1                                                                       !/*mg        238*/
  540 write(iuo,550)ncl(lncl+ia),                                               !/*mg        239*/
     & (iza(liza+m),m=jmd(ljmd+j),jmd(ljmd+j+1)-1)                              !/*mg        240*/
  550 format(/5h cell,i6,(9(i12)))                                              !/*mg        241*/
      write(iuo,560)(fmg(lfmg+ipan(lipa+ia)+m),m=1,npq(lnpq+j))                 !/*mg        242*/
  560 format(10x,1p9e12.4)                                                      !/*mg        243*/
  570 continue                                                                  !/*mg        244*/
      return                                                                    !/*mg        245*/
      end                                                                       !/*mg        246*/
