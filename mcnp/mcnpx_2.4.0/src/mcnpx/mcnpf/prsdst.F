!#ident prsdst.F from deck pr                                                   !/*pr          1*/
      subroutine prsdst(ir)                                                     !/*pr          2*/
!        print source probability distribution ir.                              !/*pr          3*/
      use global_data                                                           !/*prx         1*/
      use imcn_data                                                             !/*prx         2*/
!                                                                               !/*prx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*prx         4*/
      implicit integer(kindi) (i-n)                                             !/*prx         5*/
!                                                                               !/*prx         6*/
      character ht*75,hs(2)*9,ha*1,hh*80,hq*60                                  !/*prxa        1*/
      data hs/'histogram','discrete'/                                           !/*pr          7*/
      k(i)=ksd(lksd+i,ir)                                                       !/*pr          8*/
      s(i,j)=spf(k(13)+i,j)                                                     !/*pr          9*/
!                                                                               !/*pr         10*/
!        print the distribution number, variable, and comment.                  !/*pr         11*/
      if(k(10).ne.0)write(iuo,'(/26h1source (volume dependent),78x,             !/*prxc        1*/
     & 14hprint table 10)')                                                     !/*prxc        2*/
      kr=1                                                                      !/*pr         12*/
      if(nsr.eq.6)kr=2                                                          !/*pr         13*/
      write(iuo,10)k(1),hbln(k(12),kr)                                          !/*pr         14*/
   10 format(///25h probability distribution,i4,                                !/*pr         15*/
     & 21h for source variable ,a3)                                             !/*pr         16*/
      do 30 i=k(15)+1,k(15)+k(3),25                                             !/*pr         17*/
      do 20 j=i,i+24                                                            !/*pr         18*/
   20 ht((j-i)*3+1:(j-i)*3+3)=char(jscn(j)/65536)//                             !/*pr         19*/
     & char(mod(jscn(j),65536)/256)//char(mod(jscn(j),256))                     !/*pr         20*/
   30 write(iuo,'(10x,a75)')ht                                                  !/*pr         21*/
!                                                                               !/*pr         22*/
!        print dependent source distribution.                                   !/*pr         23*/
      if(k(7).eq.0)go to 140                                                    !/*pr         24*/
      if(ivdd(k(12)).ne.0)write(iuo,40)hbln(ksd(lksd+12,                        !/*cor4-1     69*/
     & ivdis(ivdd(k(12)))),kr)                                                  !/*cor4-1     70*/
   40 format(30h distribution is dependent on ,a3/)                             !/*pr         26*/
      ha=' '                                                                    !/*pr         27*/
      if(k(5).ne.0)ha='l'                                                       !/*pr         28*/
      if(k(9).ne.0)ha='t'                                                       !/*pr         29*/
      if(k(6).eq.0.and.k(21).ne.3)                                              !/*pr         30*/
     & write(iuo,50)ha,(s(1,i),i=1,k(4)*k(21))                                  !/*pr         31*/
   50 format(1x,a1,1p8e14.4:/(2x,8e14.4))                                       !/*pr         32*/
      if(k(6).eq.0.and.k(21).eq.3)write(iuo,60)ha,(s(1,i),i=1,k(4)*3)           !/*pr         33*/
   60 format(1x,a1,1p6e14.4:/(2x,6e14.4))                                       !/*pr         34*/
      if(k(6).eq.0)return                                                       !/*pr         35*/
      if(k(8).ne.0)go to 100                                                    !/*pr         36*/
      ha='s'                                                                    !/*pr         37*/
      do 90 i=1,k(4),16                                                         !/*pr         38*/
      do 70 j=1,min(16,k(4)-i+1)                                                !/*pr         39*/
      tpp(j)=s(1,i+j-1)                                                         !/*pr         40*/
   70 if(tpp(j).ne.0.)tpp(j)=ksd(lksd+1,int(tpp(j)))                            !/*pr         41*/
      write(iuo,80)ha,(int(tpp(m)),m=1,min(16,k(4)-i+1))                        !/*pr         42*/
   80 format(1x,a1,16i6)                                                        !/*pr         43*/
   90 ha=' '                                                                    !/*pr         44*/
      return                                                                    !/*pr         45*/
  100 ha='q'                                                                    !/*pr         46*/
      do 130 i=1,k(4),5                                                         !/*pr         47*/
      do 110 j=1,min(5,k(4)-i+1)                                                !/*pr         48*/
      tpp(j)=s(1,2*(i+j-1))                                                     !/*pr         49*/
  110 if(tpp(j).ne.0.)tpp(j)=ksd(lksd+1,int(tpp(j)))                            !/*pr         50*/
      write(iuo,120)ha,(s(1,2*(i+j-1)-1),int(tpp(j)),                           !/*pr         51*/
     & j=1,min(5,k(4)-i+1))                                                     !/*pr         52*/
  120 format(1x,a1,5(1pe14.4,i6))                                               !/*pr         53*/
  130 ha=' '                                                                    !/*pr         54*/
      return                                                                    !/*pr         55*/
!                                                                               !/*pr         56*/
!        print unbiased discrete or histogram source distribution.              !/*pr         57*/
  140 if(k(18).ne.0)go to 230                                                   !/*pr         58*/
      if(k(19).ne.0)go to 210                                                   !/*pr         59*/
      write(iuo,150)hs(1+k(5))                                                  !/*pr         60*/
  150 format(10h unbiased ,a9,13h distribution//                                !/*pr         61*/
     & 7h source,8x,6hsource,11x,10hcumulative,10x,11hprobability/              !/*pr         62*/
     & 7h  entry,9x,5hvalue,11x,11hprobability,11x,6hof bin/)                   !/*pr         63*/
      do 200 i=1,k(4)                                                           !/*pr         64*/
      j=k(21)*(i-1)+1                                                           !/*pr         65*/
      p=s(2,i)                                                                  !/*pr         66*/
      if(i.gt.1)p=p-s(2,i-1)                                                    !/*pr         67*/
      if(k(6).ne.0.or.k(12).le.2.or.k(12).eq.14)go to 180                       !/*pr         68*/
      write(iuo,160)i,s(1,j),s(2,i),p                                           !/*pr         69*/
  160 format(i5,1pe18.5,2e20.6)                                                 !/*pr         70*/
      if(k(21).ne.1)write(iuo,170)s(1,j+1),s(1,j+2)                             !/*pr         71*/
  170 format(1pe23.5)                                                           !/*pr         72*/
      go to 200                                                                 !/*pr         73*/
  180 jf=0                                                                      !/*prxa        2*/
      m=namrsd(ir,j,1,hh,mkcp,jf)                                               !/*prxa        3*/
      if(jf.ne.0.and.jf.le.23)write(iuo,185)i,hh(1:23),s(2,i),p                 !/*prxa        4*/
  185 format(i5,1x,a23,1pe14.6,e20.6)                                           !/*prxa        5*/
      if(jf.ne.0.and.jf.gt.23)write(iuo,186)i,s(2,i),p                          !/*prxa        6*/
  186 format(i5,24x,1pe14.6,e20.6)                                              !/*prxa        7*/
      if(jf.gt.23)hq(1:60)=hh(1:60)                                             !/*prxa        8*/
      if(jf.ne.0.and.jf.gt.23.and.jf.le.60)write(iuo,187)i,hq(1:60)             !/*prxa        9*/
  187 format(i5,12h cell path    ,a60)                                          !/*prxa       10*/
      if(jf.ne.0.and.jf.gt.23.and.jf.gt.60)write(iuo,188)i,hh(1:80)             !/*prxa       11*/
  188 format(i5,12h cell path    ,a80)                                          !/*prxa       12*/
      if(jf.ne.0)go to 200                                                      !/*prxa       13*/
      write(iuo,190)i,m,s(2,i),p                                                !/*pr         77*/
  190 format(i5,i15,1pe23.6,e20.6)                                              !/*pr         78*/
  200 continue                                                                  !/*pr         79*/
      go to 340                                                                 !/*pr         80*/
!                                                                               !/*pr         81*/
!        print unbiased interpolated source distribution.                       !/*pr         82*/
  210 write(iuo,220)(i,s(1,i),s(4,i),s(2,i),i=1,k(4))                           !/*pr         83*/
  220 format(35h unbiased interpolated distribution//                           !/*pr         84*/
     & 7h source,8x,6hsource,11x,10hcumulative,10x,11hprobability/              !/*pr         85*/
     & 7h  entry,9x,5hvalue,11x,11hprobability,11x,7hdensity//                  !/*pr         86*/
     & (i5,1pe18.5,2e20.6))                                                     !/*pr         87*/
      go to 340                                                                 !/*pr         88*/
!                                                                               !/*pr         89*/
!        print biased discrete or histogram source distribution.                !/*pr         90*/
  230 if(k(19).ne.0)go to 300                                                   !/*pr         91*/
      write(iuo,240)hs(1+k(5))                                                  !/*pr         92*/
  240 format(8h biased ,a9,13h distribution//                                   !/*pr         93*/
     & 7h source,8x,6hsource,10x,10hcumulative,11x,6hbiased,                    !/*pr         94*/
     & 11x,11hprobability,10x,6hbiased,13x,6hweight/                            !/*pr         95*/
     & 7h  entry,9x,5hvalue,10x,11hprobability,8x,10hcumulative,                !/*pr         96*/
     & 11x,6hof bin,11x,11hprobability,8x,10hmultiplier/)                       !/*pr         97*/
      cd=0.                                                                     !/*pr         98*/
      do 290 i=1,k(4)                                                           !/*pr         99*/
      j=k(21)*(i-1)+1                                                           !/*pr        100*/
      bp=s(2,i)                                                                 !/*pr        101*/
      if(i.gt.1)bp=bp-s(2,i-1)                                                  !/*pr        102*/
      pd=bp*s(3,i)/(1.-frc(lfrc+ir))                                            !/*pr        103*/
      cd=cd+pd                                                                  !/*pr        104*/
      if(k(6).ne.0.or.k(12).le.2.or.k(12).eq.14)go to 270                       !/*pr        105*/
      write(iuo,250)i,s(1,j),cd,s(2,i),pd,bp,s(3,i)                             !/*pr        106*/
  250 format(i5,1pe18.5,5e19.6)                                                 !/*pr        107*/
      if(k(21).ne.1)write(iuo,260)s(1,j+1),s(1,j+2)                             !/*pr        108*/
  260 format(1pe23.5)                                                           !/*pr        109*/
      go to 290                                                                 !/*pr        110*/
  270 jf=0                                                                      !/*prxa       14*/
      m=namrsd(ir,j,1,hh,mkcp,jf)                                               !/*prxa       15*/
      if(jf.ne.0.and.jf.le.23)write(iuo,275)i,hh(1:23),cd,s(2,i),pd,bp,         !/*prxa       16*/
     & s(3,i)                                                                   !/*prxa       17*/
  275 format(i5,1x,a23,1pe13.6,4e19.6)                                          !/*prxa       18*/
      if(jf.ne.0.and.jf.gt.23)write(iuo,276)i,cd,s(2,i),pd,bp,s(3,i)            !/*prxa       19*/
  276 format(i5,24x,1pe13.6,4e19.6)                                             !/*prxa       20*/
      if(jf.gt.23)hq(1:60)=hh(1:60)                                             !/*prxa       21*/
      if(jf.ne.0.and.jf.gt.23.and.jf.le.60)write(iuo,187)i,hq(1:60)             !/*prxa       22*/
      if(jf.ne.0.and.jf.gt.23.and.jf.gt.60)write(iuo,188)i,hh(1:80)             !/*prxa       23*/
      if(jf.ne.0)go to 290                                                      !/*prxa       24*/
      write(iuo,280)i,m,cd,s(2,i),pd,bp,s(3,i)                                  !/*pr        114*/
  280 format(i5,i15,1pe22.6,4e19.6)                                             !/*pr        115*/
  290 continue                                                                  !/*pr        116*/
      go to 340                                                                 !/*pr        117*/
!                                                                               !/*pr        118*/
!        print biased interpolated source distribution.                         !/*pr        119*/
  300 write(iuo,310)                                                            !/*pr        120*/
  310 format(33h biased interpolated distribution//                             !/*pr        121*/
     & 7h source,8x,6hsource,10x,10hcumulative,11x,6hbiased,                    !/*pr        122*/
     & 11x,11hprobability,10x,6hbiased,13x,6hweight/                            !/*pr        123*/
     & 7h  entry,9x,5hvalue,10x,11hprobability,8x,10hcumulative,                !/*pr        124*/
     & 11x,7hdensity,10x,11hprobability,8x,10hmultiplier/)                      !/*pr        125*/
      cd=0.                                                                     !/*pr        126*/
      do 320 i=1,k(4)                                                           !/*pr        127*/
      w=1.                                                                      !/*pr        128*/
      if(s(3,i).ne.0.)w=(1.-frc(lfrc+ir))*s(2,i)/s(3,i)                         !/*pr        129*/
      if(i.eq.1)go to 320                                                       !/*pr        130*/
      cd=cd+0.5*(s(1,i)-s(1,i-1))*(s(2,i)+s(2,i-1))                             !/*pr        131*/
  320 write(iuo,330)i,s(1,i),cd,s(4,i),s(2,i),s(3,i),w                          !/*pr        132*/
  330 format(i5,1pe18.5,5e19.6)                                                 !/*pr        133*/
!                                                                               !/*pr        134*/
!        print additional remarks.                                              !/*pr        135*/
  340 if(eaa(leaa+ir).ne.0.)write(iuo,350)k(1),eaa(leaa+ir)                     !/*pr        136*/
  350 format(/5x,31hthe mean of source distribution,i4,3h is,1pe12.4)           !/*pr        137*/
      if(frc(lfrc+ir).ne.0.)write(iuo,360)frc(lfrc+ir)                          !/*pr        138*/
  360 format(/4x,47hfraction of energy distribution outside range =,            !/*pr        139*/
     & 1pe11.4)                                                                 !/*pr        140*/
      return                                                                    !/*pr        141*/
      end                                                                       !/*pr        142*/
