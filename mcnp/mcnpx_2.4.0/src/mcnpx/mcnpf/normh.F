!#ident normh.F from deck nh                                                    !/*nh          1*/
      subroutine normh(ir)                                                      !/*nh          2*/
!        normalize histogram or discrete probability distribution ir.           !/*nh          3*/
      use global_data                                                           !/*nhx         1*/
      use imcn_data                                                             !/*nhx         2*/
!                                                                               !/*nhx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*nhx         4*/
      implicit integer(kindi) (i-n)                                             !/*nhx         5*/
!                                                                               !/*nhx         6*/
      k(i)=ksd(lksd+i,ir)                                                       !/*nh          6*/
      s(i,j)=spf(k(13)+i,j)                                                     !/*nh          7*/
!                                                                               !/*nh          8*/
!        trim the distribution if it is an energy distribution.                 !/*nh          9*/
      if(nsr.eq.6.or.k(12).ne.3.or.k(6).ne.0.or.mcal.eq.2)go to 100             !/*nh         10*/
!                                                                               !/*nh         11*/
!        check the range of the energy distribution.                            !/*nh         12*/
      if(s(1,1).ge.ecf(nips).and.s(1,k(4)).le.emx(1))go to 100                  !/*nh         13*/
      if(s(1,1).le.emx(1).and.s(1,k(4)).gt.ecf(nips))go to 10                   !/*nh         14*/
      call erprnt(1,1,1,k(1),0,0,0,0,                                           !/*nh         15*/
     & '19hsource distribution,i4,30h is outside the problem range.')           !/*nh         16*/
      return                                                                    !/*nh         17*/
!                                                                               !/*nh         18*/
!        find range limits and calculate the initial total probability.         !/*nh         19*/
   10 p1=0.                                                                     !/*nh         20*/
      do 20 i=1,k(4)                                                            !/*nh         21*/
      if(s(1,k(4)+1-i).ge.ecf(nips))m=k(4)+1-i                                  !/*nh         22*/
      if(s(1,i).le.emx(1))l=i                                                   !/*nh         23*/
   20 p1=p1+s(2,i)                                                              !/*nh         24*/
!                                                                               !/*nh         25*/
!        make adjustments at the ends of the range.                             !/*nh         26*/
      g=0.                                                                      !/*nh         27*/
      f=1.                                                                      !/*nh         28*/
      if(l.eq.k(4))go to 30                                                     !/*nh         29*/
      ksd(lksd+4,ir)=l+1-k(5)                                                   !/*nh         30*/
      g=(emx(1)-s(1,l))/(s(1,l+1)-s(1,l))                                       !/*nh         31*/
      if(k(5).ne.0)go to 30                                                     !/*nh         32*/
      spf(k(13)+1,l+1)=emx(1)                                                   !/*nh         33*/
      spf(k(13)+2,l+1)=g*s(2,l+1)                                               !/*nh         34*/
      spf(k(13)+3,l+1)=g*s(3,l+1)                                               !/*nh         35*/
   30 if(m.eq.1)go to 60                                                        !/*nh         36*/
      f=(s(1,m)-ecf(nips))/(s(1,m)-s(1,m-1))                                    !/*nh         37*/
      if(k(5).ne.0)go to 40                                                     !/*nh         38*/
      spf(k(13)+1,1)=ecf(nips)                                                  !/*nh         39*/
      spf(k(13)+2,m)=f*s(2,m)                                                   !/*nh         40*/
      spf(k(13)+3,m)=f*s(3,m)                                                   !/*nh         41*/
      m=m-1                                                                     !/*nh         42*/
!                                                                               !/*nh         43*/
!        shift the table.                                                       !/*nh         44*/
   40 ksd(lksd+4,ir)=k(4)-m+1                                                   !/*nh         45*/
      do 50 i=2-k(5),k(4)                                                       !/*nh         46*/
      spf(k(13)+1,i)=s(1,i+m-1)                                                 !/*nh         47*/
      spf(k(13)+2,i)=s(2,i+m-1)                                                 !/*nh         48*/
   50 spf(k(13)+3,i)=s(3,i+m-1)                                                 !/*nh         49*/
!                                                                               !/*nh         50*/
!        calculate the fraction of probability removed.                         !/*nh         51*/
   60 p2=0.                                                                     !/*nh         52*/
      do 70 i=1,k(4)                                                            !/*nh         53*/
   70 p2=p2+s(2,i)                                                              !/*nh         54*/
      if(p1.ne.0.)frc(lfrc+ir)=1.-p2/p1                                         !/*nh         55*/
!                                                                               !/*nh         56*/
!        trim any distributions dependent on ir.                                !/*nh         57*/
      if(ivdis(k(12)).ne.ir)go to 100                                           !/*nh         58*/
      do 90 j=1,msd                                                             !/*nh         59*/
      if(ksd(lksd+7,j).eq.0.or.ivdd(ksd(lksd+12,j)).ne.k(12).or.                !/*nh         60*/
     & j.eq.ir.or.ksd(lksd+8,j)+ksd(lksd+9,j).ne.0)go to 90                     !/*nh         61*/
      ksd(lksd+4,j)=k(4)-ksd(lksd+5,j)+k(5)                                     !/*nh         62*/
      nn=ksd(lksd+21,j)                                                         !/*nh         63*/
      do 80 i=1,ksd(lksd+4,j)                                                   !/*nh         64*/
      do 80 n=nn*(i-1)+1,nn*i,nn                                                !/*nh         65*/
   80 spf(ksd(lksd+13,j)+1,n)=spf(ksd(lksd+13,j)+1,nn*(m-1)+n)                  !/*nh         66*/
      if(ksd(lksd+5,j).ne.0)go to 90                                            !/*nh         67*/
      spf(ksd(lksd+13,j)+1,1)=spf(ksd(lksd+13,j)+1,1)+                          !/*nh         68*/
     & (1.-f)*(spf(ksd(lksd+13,j)+1,2)-spf(ksd(lksd+13,j)+1,1))                 !/*nh         69*/
      spf(ksd(lksd+13,j)+1,k(4))=spf(ksd(lksd+13,j)+1,k(4)-1)+                  !/*nh         70*/
     & g*(spf(ksd(lksd+13,j)+1,k(4))-spf(ksd(lksd+13,j)+1,k(4)-1))              !/*nh         71*/
   90 continue                                                                  !/*nh         72*/
!                                                                               !/*nh         73*/
!        check for absent data.                                                 !/*nh         74*/
  100 b=0.                                                                      !/*nh         75*/
      c=0.                                                                      !/*nh         76*/
      do 110 i=1,k(4)                                                           !/*nh         77*/
!ccc     extra parenthesis required (next 2 lines) for convex compilers.        !/*nh4b        1*/
      b=(b+s(2,i))                                                              !/*nh4b        2*/
  110 c=(c+s(3,i))                                                              !/*nh4b        3*/
      if(k(18).ne.0.and.c.eq.0.)go to 120                                       !/*nh         80*/
      if(b.ne.0.)go to 130                                                      !/*nh         81*/
      if(nsr.eq.6.or.k(12).ne.10.and.k(12).ne.11)go to 120                      !/*nh         82*/
      ksd(lksd+2,ir)=-21                                                        !/*nh         83*/
      return                                                                    !/*nh         84*/
  120 call erprnt(1,1,1,k(1),0,0,0,0,                                           !/*nh         85*/
     & '19hsource distribution,i4,9h is zero.')                                 !/*nh         86*/
      return                                                                    !/*nh         87*/
!                                                                               !/*nh         88*/
!        normalize and calculate the mean of the distribution.                  !/*nh         89*/
  130 if(k(5).eq.0.and.s(2,1).ne.0.)call erprnt(1,1,1,k(1),0,0,0,0,             !/*nh         90*/
     & '17hfirst entry on sp,i3,18h card is not zero.')                         !/*nh         91*/
      do 140 i=1,k(4)                                                           !/*nh         92*/
      spf(k(13)+2,i)=s(2,i)/b                                                   !/*nh         93*/
      if(k(18).ne.0)spf(k(13)+3,i)=s(3,i)/c                                     !/*nh         94*/
      if(k(18).eq.0)spf(k(13)+3,i)=1.-frc(lfrc+ir)                              !/*nh         95*/
      if(k(12).le.2.or.k(12).eq.6.or.k(12).ge.12.and.k(12).le.15.or.            !/*nh         96*/
     & k(6)+k(11).ne.0)go to 140                                                !/*nh         97*/
      a=s(1,i)                                                                  !/*nh         98*/
      if(k(5).eq.0.and.i.ne.1)a=.5*(a+s(1,i-1))                                 !/*nh         99*/
      eaa(leaa+ir)=eaa(leaa+ir)+a*s(2,i)                                        !/*nh        100*/
  140 continue                                                                  !/*nh        101*/
!                                                                               !/*nh        102*/
!        set up source weight multipliers if necessary.                         !/*nh        103*/
      if(k(18).eq.0)go to 160                                                   !/*nh        104*/
      if(k(5).eq.0.and.s(3,1).ne.0.)call erprnt(1,1,1,k(1),0,0,0,0,             !/*nh        105*/
     & '17hfirst entry on sb,i3,18h card is not zero.')                         !/*nh        106*/
      m=0                                                                       !/*nh        107*/
      do 150 i=1,k(4)                                                           !/*nh        108*/
      a=s(2,i)                                                                  !/*nh        109*/
      b=s(3,i)                                                                  !/*nh        110*/
      if(a.eq.0..and.b.ne.0.)m=1                                                !/*nh        111*/
      spf(k(13)+2,i)=s(3,i)                                                     !/*nh        112*/
      spf(k(13)+3,i)=1.-frc(lfrc+ir)                                            !/*nh        113*/
  150 if(b.ne.0.)spf(k(13)+3,i)=s(3,i)*a/b                                      !/*nh        114*/
      if(m.ne.0)call erprnt(1,1,1,k(1),0,0,0,0,                                 !/*nh        115*/
     & '30hbiasing of source distribution,i4,13h is infinite.')                 !/*nh        116*/
!                                                                               !/*nh        117*/
!        make the distribution cumulative.                                      !/*nh        118*/
  160 do 170 i=2,k(4)                                                           !/*nh        119*/
  170 spf(k(13)+2,i)=spf(k(13)+2,i-1)+spf(k(13)+2,i)                            !/*nh        120*/
      spf(k(13)+2,k(4))=1.                                                      !/*nh        121*/
      if(nsr.eq.6.or.k(12).ne.10.or.k(4).ne.2.or.k(5).ne.0)return               !/*nh        122*/
      jrad=1                                                                    !/*nh        123*/
      call erprnt(1,2,0,0,0,0,0,0,                                              !/*nh        124*/
     & '41hsource variable rad is sampled uniformly.')                          !/*nh        125*/
      return                                                                    !/*nh        126*/
      end                                                                       !/*nh        127*/
