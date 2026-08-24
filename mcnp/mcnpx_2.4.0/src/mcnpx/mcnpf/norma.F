!#ident norma.F from deck na                                                    !/*na          1*/
      subroutine norma(ir)                                                      !/*na          2*/
!        normalize interpolated probability distribution ir.                    !/*na          3*/
      use global_data                                                           !/*nax         1*/
      use imcn_data                                                             !/*nax         2*/
!                                                                               !/*nax         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*nax         4*/
      implicit integer(kindi) (i-n)                                             !/*nax         5*/
!                                                                               !/*nax         6*/
      k(i)=ksd(lksd+i,ir)                                                       !/*na          6*/
      s(i,j)=spf(k(13)+i,j)                                                     !/*na          7*/
!                                                                               !/*na          8*/
!        trim the distribution if it is an energy distribution.                 !/*na          9*/
      if(k(12).ne.3)go to 90                                                    !/*na         10*/
!                                                                               !/*na         11*/
!        check the range of the energy distribution.                            !/*na         12*/
      if(s(1,1).ge.ecf(nips).and.s(1,k(4)).le.emx(1))go to 90                   !/*na         13*/
      if(s(1,1).le.emx(1).and.s(1,k(4)).ge.ecf(nips))go to 10                   !/*na         14*/
      call erprnt(1,1,1,k(1),0,0,0,0,                                           !/*na         15*/
     & '19hsource distribution,i4,30h is outside the problem range.')           !/*na         16*/
      return                                                                    !/*na         17*/
!                                                                               !/*na         18*/
!        find range limits and calculate the initial total probability.         !/*na         19*/
   10 p1=0.                                                                     !/*na         20*/
      b=0.                                                                      !/*na         21*/
      do 20 i=1,k(4)                                                            !/*na         22*/
      if(s(1,k(4)+1-i).ge.ecf(nips))m=k(4)+1-i                                  !/*na         23*/
      if(i.eq.1)go to 20                                                        !/*na         24*/
      if(s(1,i).ne.s(1,i-1))b=(s(2,i)-s(2,i-1))/(s(1,i)-s(1,i-1))               !/*na         25*/
      p1=p1+(s(2,i-1)-b*s(1,i-1))*(s(1,i)-s(1,i-1))+                            !/*na         26*/
     & .5*b*(s(1,i)**2-s(1,i-1)**2)                                             !/*na         27*/
   20 if(s(1,i).le.emx(1))l=i                                                   !/*na         28*/
!                                                                               !/*na         29*/
!        make adjustments at the ends of the range.                             !/*na         30*/
      g=0.                                                                      !/*na         31*/
      f=1.                                                                      !/*na         32*/
      if(l.eq.k(4))go to 30                                                     !/*na         33*/
      ksd(lksd+4,ir)=l+1                                                        !/*na         34*/
      g=(emx(1)-s(1,l))/(s(1,l+1)-s(1,l))                                       !/*na         35*/
      spf(k(13)+1,l+1)=emx(1)                                                   !/*na         36*/
      spf(k(13)+2,l+1)=s(2,l)+g*(s(2,l+1)-s(2,l))                               !/*na         37*/
      spf(k(13)+3,l+1)=s(3,l)+g*(s(3,l+1)-s(3,l))                               !/*na         38*/
   30 if(m.eq.1)go to 50                                                        !/*na         39*/
      f=(s(1,m)-ecf(nips))/(s(1,m)-s(1,m-1))                                    !/*na         40*/
      m=m-1                                                                     !/*na         41*/
      spf(k(13)+1,m)=ecf(nips)                                                  !/*na         42*/
      spf(k(13)+2,m)=s(2,m+1)-f*(s(2,m+1)-s(2,m))                               !/*na         43*/
      spf(k(13)+3,m)=s(3,m+1)-f*(s(3,m+1)-s(3,m))                               !/*na         44*/
!                                                                               !/*na         45*/
!        shift the table.                                                       !/*na         46*/
      ksd(lksd+4,ir)=k(4)-m+1                                                   !/*na         47*/
      do 40 i=1,k(4)                                                            !/*na         48*/
      spf(k(13)+1,i)=s(1,i+m-1)                                                 !/*na         49*/
      spf(k(13)+2,i)=s(2,i+m-1)                                                 !/*na         50*/
   40 spf(k(13)+3,i)=s(3,i+m-1)                                                 !/*na         51*/
!                                                                               !/*na         52*/
!        calculate the fraction of probability removed.                         !/*na         53*/
   50 p2=0.                                                                     !/*na         54*/
      do 60 i=2,k(4)                                                            !/*na         55*/
      if(s(1,i).ne.s(1,i-1))b=(s(2,i)-s(2,i-1))/(s(1,i)-s(1,i-1))               !/*na         56*/
   60 p2=p2+(s(2,i-1)-b*s(1,i-1))*(s(1,i)-s(1,i-1))+                            !/*na         57*/
     & .5*b*(s(1,i)**2-s(1,i-1)**2)                                             !/*na         58*/
      if(p1.ne.0.)frc(lfrc+ir)=1.-p2/p1                                         !/*na         59*/
!                                                                               !/*na         60*/
!        trim any distributions dependent on ir.                                !/*na         61*/
      if(ivdis(k(12)).ne.ir)go to 90                                            !/*na         62*/
      do 80 j=1,msd                                                             !/*na         63*/
      if(ksd(lksd+7,j).eq.0.or.ivdd(ksd(lksd+12,j)).ne.k(12).or.                !/*na         64*/
     & j.eq.ir.or.ksd(lksd+8,j)+ksd(lksd+9,j).ne.0)go to 80                     !/*na         65*/
      ksd(lksd+4,j)=k(4)-ksd(lksd+5,j)                                          !/*na         66*/
      do 70 i=1,ksd(lksd+4,j)                                                   !/*na         67*/
   70 spf(ksd(lksd+13,j)+1,i)=spf(ksd(lksd+13,j)+1,m-1+i)                       !/*na         68*/
      if(ksd(lksd+5,j).ne.0)go to 80                                            !/*na         69*/
      spf(ksd(lksd+13,j)+1,1)=spf(ksd(lksd+13,j)+1,1)+                          !/*na         70*/
     & (1.-f)*(spf(ksd(lksd+13,j)+1,2)-spf(ksd(lksd+13,j)+1,1))                 !/*na         71*/
      spf(ksd(lksd+13,j)+1,k(4))=spf(ksd(lksd+13,j)+1,k(4)-1)+                  !/*na         72*/
     & g*(spf(ksd(lksd+13,j)+1,k(4))-spf(ksd(lksd+13,j)+1,k(4)-1))              !/*na         73*/
   80 continue                                                                  !/*na         74*/
!                                                                               !/*na         75*/
!        check for absent data and calculate cumulative distribution.           !/*na         76*/
!        reference:  x-6:raf-90-265 for following mathematical formulas.        !/*na         77*/
   90 a=0.                                                                      !/*na         78*/
      b=0.                                                                      !/*na         79*/
      c=0.                                                                      !/*na         80*/
      m=0                                                                       !/*na         81*/
      do 100 i=1,k(4)                                                           !/*na         82*/
      a=a+abs(s(1,i))                                                           !/*na         83*/
      if(i.eq.1)go to 100                                                       !/*na         84*/
      c=c+0.5*(s(1,i)-s(1,i-1))*(s(2,i)+s(2,i-1))                               !/*na         85*/
      spf(k(13)+4,i)=c                                                          !/*na         86*/
      if(k(18).eq.0)go to 100                                                   !/*na         87*/
      spf(k(13)+4,i)=s(4,i-1)+0.5*(s(1,i)-s(1,i-1))*(s(3,i)+s(3,i-1))           !/*na         88*/
      if(s(2,i).eq.0..neqv.s(3,i).eq.0.)m=1                                     !/*na         89*/
  100 continue                                                                  !/*na         90*/
      if(a.ne.0..and.c.ne.0..and.s(4,k(4)).ne.0.)go to 110                      !/*na         91*/
      call erprnt(1,1,1,k(1),0,0,0,0,                                           !/*na         92*/
     & '19hsource distribution,i4,9h is zero.')                                 !/*na         93*/
      return                                                                    !/*na         94*/
  110 if(m.ne.0)call erprnt(1,1,1,k(1),0,0,0,0,                                 !/*na         95*/
     & '30hbiasing of source distribution,i4,21h is zero or infinite.')         !/*na         96*/
!                                                                               !/*na         97*/
!        normalize and calculate the mean of the distribution.                  !/*na         98*/
      do 120 i=1,k(4)                                                           !/*na         99*/
      spf(k(13)+2,i)=s(2,i)/c                                                   !/*na        100*/
      spf(k(13)+3,i)=s(3,i)/s(4,k(4))                                           !/*na        101*/
      spf(k(13)+4,i)=s(4,i)/s(4,k(4))                                           !/*na        102*/
      if(i.eq.1)go to 120                                                       !/*na        103*/
      eaa(leaa+ir)=eaa(leaa+ir)+(s(1,i)-s(1,i-1))*                              !/*na        104*/
     & (s(1,i)*(s(2,i-1)+2.*s(2,i))+s(1,i-1)*(s(2,i)+2.*s(2,i-1)))/6            !/*na        105*/
  120 continue                                                                  !/*na        106*/
      return                                                                    !/*na        107*/
      end                                                                       !/*na        108*/
