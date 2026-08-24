!#ident smpsrc.F from deck es                                                   !/*es          1*/
      subroutine smpsrc(sf,jc,ib,fi)                                            !/*es          2*/
!        sample source distribution jc and adjust the weight for bias.          !/*es          3*/
!        sf=array of values to return                                           !/*es          4*/
!        ib=index of the chosen bin      fi=interpolation fraction              !/*es          5*/
      use global_data                                                           !/*esx         1*/
!                                                                               !/*esx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*esx         3*/
      implicit integer(kindi) (i-n)                                             !/*esx         4*/
!                                                                               !/*esx         5*/
      dimension sf(*)                                                           !/*es          7*/
      k(i)=ksd(lksd+i,jc)                                                       !/*es          8*/
      q(i)=sqq(lsqq+i,jc)                                                       !/*es          9*/
!                                                                               !/*es         10*/
!        save distribution number.                                              !/*es         11*/
      if(isic(k(12)).ge.0)isic(k(12))=jc                                        !/*es         12*/
!                                                                               !/*es         13*/
!        sample from a table.                                                   !/*es         14*/
      if(k(4).eq.0)go to 90                                                     !/*es         15*/
      j=k(13)+2+2*k(19)                                                         !/*es         16*/
      rn=rang()                                                                 !/*es         17*/
      ib=k(4)                                                                   !/*es         18*/
      ic=1-k(5)                                                                 !/*es         19*/
   10 if(ib-ic.eq.1)go to 30                                                    !/*es         20*/
      ih=(ib+ic)/2                                                              !/*es         21*/
      if(rn.lt.spf(j,ih))go to 20                                               !/*es         22*/
      ic=ih                                                                     !/*es         23*/
      go to 10                                                                  !/*es         24*/
   20 ib=ih                                                                     !/*es         25*/
      go to 10                                                                  !/*es         26*/
!                                                                               !/*es         27*/
!        histogram or discrete probability distribution.                        !/*es         28*/
   30 if(k(17).ne.0)go to 80                                                    !/*es         29*/
      if(k(19).ne.0)go to 50                                                    !/*es         30*/
      if(ic.eq.0)fi=rn/spf(k(13)+2,ib)                                          !/*es         31*/
      if(ic.ne.0)fi=(rn-spf(k(13)+2,ic))/                                       !/*es         32*/
     & (spf(k(13)+2,ib)-spf(k(13)+2,ic))                                        !/*es         33*/
      l=k(21)*ic                                                                !/*es         34*/
      do 40 i=1,k(21)                                                           !/*es         35*/
      sf(i)=spf(k(13)+1,l+i)                                                    !/*es         36*/
   40 if(k(5).eq.0)sf(i)=spf(k(13)+1,l+i-k(21))+                                !/*es         37*/
     & fi*(sf(i)-spf(k(13)+1,l+i-k(21)))                                        !/*es         38*/
      wgt=wgt*spf(k(13)+3,ib)                                                   !/*es         39*/
      if(k(12).eq.5)cbwf=1./spf(k(13)+3,ib)                                     !/*es4a        1*/
      go to 60                                                                  !/*es         40*/
!                                                                               !/*es         41*/
!        interpolated probability density.                                      !/*es         42*/
   50 j=k(13)+2+k(18)                                                           !/*es         43*/
      b=(spf(j,ib)-spf(j,ic))/(spf(k(13)+1,ib)-spf(k(13)+1,ic))                 !/*es         44*/
      if(b.eq.0.)sf(1)=spf(k(13)+1,ic)+(rn-spf(k(13)+4,ic))/spf(j,ic)           !/*es         45*/
      if(b.ne.0.)sf(1)=spf(k(13)+1,ic)+(-spf(j,ic)+                             !/*es         46*/
     & sqrt(spf(j,ic)**2+2.*b*(rn-spf(k(13)+4,ic))))/b                          !/*es         47*/
      fi=(sf(1)-spf(k(13)+1,ic))/(spf(k(13)+1,ib)-spf(k(13)+1,ic))              !/*es         48*/
      wgt=wgt*(1.-frc(lfrc+jc))                                                 !/*es         49*/
      w=wgt                                                                     !/*es4a        2*/
      if(k(18).ne.0)wgt=wgt*                                                    !/*es         50*/
     & (spf(k(13)+2,ic)+fi*(spf(k(13)+2,ib)-spf(k(13)+2,ic)))/                  !/*es         51*/
     & (spf(k(13)+3,ic)+fi*(spf(k(13)+3,ib)-spf(k(13)+3,ic)))                   !/*es         52*/
      if(k(12).eq.5)cbwf=w/wgt                                                  !/*es4a        3*/
!                                                                               !/*es         53*/
!        tabulate in frequency table.                                           !/*es         54*/
   60 if(isic(k(12)).lt.0.or.nsr.ne.40)return                                   !/*es         55*/
      if(fdd(kfdd+1,jc).eq.0.)go to 70                                          !/*es         56*/
      id=fdd(kfdd+1,jc)                                                         !/*es         57*/
      wns(kwns+k(16)+1,id)=wns(kwns+k(16)+1,id)-1.                              !/*es         58*/
      wns(kwns+k(16)+2,id)=wns(kwns+k(16)+2,id)-fdd(kfdd+2,jc)                  !/*es         59*/
   70 wns(kwns+k(16)+1,ib)=wns(kwns+k(16)+1,ib)+1.                              !/*es         60*/
      wns(kwns+k(16)+2,ib)=wns(kwns+k(16)+2,ib)+wgt                             !/*es         61*/
      fdd(kfdd+1,jc)=ib                                                         !/*es         62*/
      fdd(kfdd+2,jc)=wgt                                                        !/*es         63*/
      ib=ib-1+k(5)                                                              !/*es         64*/
      return                                                                    !/*es         65*/
!                                                                               !/*es         66*/
!        equiprobable bins table for biased built-in source function.           !/*es         67*/
   80 t1=rang()*k(17)                                                           !/*es         68*/
      kr=t1                                                                     !/*es         69*/
      i=k(14)+(ic-1)*k(17)+kr+1                                                 !/*es         70*/
      sf(1)=sso(i)+(t1-kr)*(sso(i+1)-sso(i))                                    !/*es         71*/
      wgt=wgt*spf(k(13)+3,ib)                                                   !/*es         72*/
      if(k(12).eq.5)cbwf=1./spf(k(13)+3,ib)                                     !/*es4a        4*/
      ib=ib-1+k(5)                                                              !/*es         73*/
      if(isic(k(12)).ge.0.and.nsr.eq.40)fdd(kfdd+1,jc)=1.                       !/*es         74*/
      return                                                                    !/*es         75*/
!                                                                               !/*es         76*/
!        sample from a built-in source function.                                !/*es         77*/
   90 if(isic(k(12)).ge.0.and.nsr.eq.40)fdd(kfdd+1,jc)=1.                       !/*es         78*/
      go to(100,110,120,130,140,150,160,200,120)k(2)                            !/*es         79*/
!                                                                               !/*es         80*/
! >>>>>  spectrum 2 -- maxwell fission spectrum (endf law 7).                   !/*es         81*/
  100 t1=rang()**2                                                              !/*es         82*/
      t2=t1+rang()**2                                                           !/*es         83*/
      if(t2.gt.1.)go to 100                                                     !/*es         84*/
      t2=log(rang())*t1/t2                                                      !/*es         85*/
      sf(1)=-q(2)*(t2+log(rang()))                                              !/*es         86*/
      return                                                                    !/*es         87*/
!                                                                               !/*es         88*/
! >>>>>  spectrum 3 -- watt spectrum (endf law 10).                             !/*es         89*/
  110 t1=-log(rang())                                                           !/*es         90*/
      if((-log(rang())-q(5)*(1.+t1))**2.gt.q(8)*q(6)*t1)go to 110               !/*es         91*/
      sf(1)=q(6)*t1                                                             !/*es         92*/
      return                                                                    !/*es         93*/
!                                                                               !/*es         94*/
! >>>>>  spectrum 4 -- gaussian (fusion), and                                   !/*es         95*/
!        spectrum 41 -- normal distribution.                                    !/*esxk        1*/
  120 t1=2.*rang()-1.                                                           !/*es         97*/
      t2=t1**2+rang()**2                                                        !/*es         98*/
      if(t2.ge.1.)go to 120                                                     !/*es         99*/
      sf(1)=q(3)+q(2)*t1*sqrt(-log(t2)/t2)                                      !/*es        100*/
      return                                                                    !/*es        101*/
!                                                                               !/*es        102*/
! >>>>>  spectrum 5 -- evaporation spectrum (endf law 9).                       !/*es        103*/
  130 rn=rang()                                                                 !/*es        104*/
      sf(1)=-q(2)*log(rn*rang())                                                !/*es        105*/
      return                                                                    !/*es        106*/
!                                                                               !/*es        107*/
! >>>>>  spectrum 6 -- modified gaussian (fusion) spectrum.                     !/*es        108*/
  140 t1=2.*rang()-1.                                                           !/*es        109*/
      t2=t1**2+rang()**2                                                        !/*es        110*/
      if(t2.gt.1.)go to 140                                                     !/*es        111*/
      sf(1)=(q(3)+q(2)*t1*sqrt(-log(t2)/t2))**2                                 !/*es        112*/
      return                                                                    !/*es        113*/
!                                                                               !/*es        114*/
! >>>>>  spectrum 7 -- spare.                                                   !/*es        115*/
  150 return                                                                    !/*es        116*/
!                                                                               !/*es        117*/
! >>>>>  function 21 -- power law.                                              !/*es        118*/
  160 w=wgt                                                                     !/*es4a        5*/
      if(q(2).eq.12345.)go to 180                                               !/*es4a        6*/
      b=q(8)+rang()*q(9)                                                        !/*es        120*/
      sf(1)=sign(abs(b)**q(10),b)                                               !/*es        121*/
      if(q(3).ne.1.)wgt=wgt*q(3)*abs(sf(1))**(q(2)-q(5))                        !/*es        122*/
      if(k(12).eq.5)cbwf=w/wgt                                                  !/*es4a        7*/
      if(q(2).eq.1.)imd=1                                                       !/*es        123*/
      if(jrad.ne.0.or.k(12).ne.10)return                                        !/*es        124*/
      if(q(2).eq.2..and.iax.eq.0.and.jsu.eq.0)return                            !/*es        125*/
      if(q(2).eq.1..and.(iax.ne.0.or.jsu.ne.0))return                           !/*es        126*/
      call errprn(1,jrad,0,zero,zero,' ',' ',                                   !/*es4b        1*/
     & 'unusual exponent for sampling source rad.')                             !/*es4a        9*/
      return                                                                    !/*es        134*/
  180 j=2-iax                                                                   !/*es        135*/
      if(jsu.ne.0)j=1                                                           !/*es        136*/
      imd=1                                                                     !/*es        137*/
      if(q(5).ne.12345.)go to 190                                               !/*es        138*/
      a=sign(abs(q(6))**(j+1),q(6))                                             !/*es        139*/
      b=a+rang()*(sign(abs(q(7))**(j+1),q(7))-a)                                !/*es        140*/
      sf(1)=sign(abs(b)**(1./(j+1)),b)                                          !/*es        141*/
      return                                                                    !/*es        142*/
  190 b=q(8)+rang()*q(9)                                                        !/*es        143*/
      sf(1)=sign(abs(b)**q(10),b)                                               !/*es        144*/
      if(j.ne.q(5))wgt=wgt*abs(sf(1))**(j-q(5))*(j+1)*q(9)*q(10)/               !/*es        145*/
     & (sign(abs(q(7))**(j+1),q(7))-sign(abs(q(6))**(j+1),q(6)))                !/*es        146*/
      if(k(12).eq.5)cbwf=w/wgt                                                  !/*es4a       10*/
      return                                                                    !/*es        147*/
!                                                                               !/*es        148*/
! >>>>>  function 31 -- exponential.                                            !/*es        149*/
  200 sf(1)=q(8)+rang()*q(9)                                                    !/*es        150*/
      if(q(5).ne.0.)sf(1)=q(10)*log(sf(1))                                      !/*es        151*/
      w=wgt                                                                     !/*es4a       11*/
      if(q(3).ne.1.)wgt=wgt*q(3)*exp(sf(1)*(q(2)-q(5)))                         !/*es        152*/
      if(k(12).eq.5)cbwf=w/wgt                                                  !/*es4a       12*/
      return                                                                    !/*es        153*/
      end                                                                       !/*es        154*/
