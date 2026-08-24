!#ident eqpbbn.F from deck eb                                                   !/*eb          1*/
      subroutine eqpbbn                                                         !/*eb          2*/
!        calculate equiprobable bin bounds for each source input                !/*eb          3*/
!        group from the probability density function p(e)=spec(e).              !/*eb          4*/
      use global_data                                                           !/*ebx         1*/
      use imcn_data                                                             !/*ebx         2*/
!                                                                               !/*ebx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*ebx         4*/
      implicit integer(kindi) (i-n)                                             !/*ebx         5*/
!                                                                               !/*ebx         6*/
      external spec                                                             !/*eb          7*/
      parameter (er=1e-4)                                                       !/*eb          8*/
      k(i)=ksd(lksd+i,irs)                                                      !/*eb          9*/
      q(i)=sqq(lsqq+i,irs)                                                      !/*eb         10*/
      s(i,j)=spf(k(13)+i,j)                                                     !/*eb         11*/
!                                                                               !/*eb         12*/
      if(ink(10).ne.0)write(iuo,10)k(1)                                         !/*eb         13*/
   10 format(//5x,37hequiprobable bin distribution used to,                     !/*eb         14*/
     & 26h model source distribution,i4/)                                       !/*eb         15*/
!                                                                               !/*eb         16*/
!        set up nb equiprobable bins within each of ms groups.                  !/*eb         17*/
      eaa(leaa+irs)=0.                                                          !/*eb         18*/
      ms=k(4)-1                                                                 !/*eb         19*/
      nb=(mseb-1)/ms                                                            !/*eb         20*/
      ksd(lksd+17,irs)=nb                                                       !/*eb         21*/
      prb(lprb+1)=0.                                                            !/*eb         22*/
      ksd(lksd+14,irs)=lsso                                                     !/*eb         23*/
      do 20 i=1,irs-1                                                           !/*eb         24*/
   20 if(ksd(lksd+14,i).ne.0)ksd(lksd+14,irs)=k(14)+mseb                        !/*eb         25*/
      sso(k(14)+1)=s(1,1)                                                       !/*eb         26*/
      em=q(12)                                                                  !/*eb         27*/
      do 130 je=1,ms                                                            !/*eb         28*/
      ks=k(14)+(je-1)*nb                                                        !/*eb         29*/
!                                                                               !/*eb         30*/
!        iterate until the bins within the group are equiprobable.              !/*eb         31*/
      do 30 i=1,nb                                                              !/*eb         32*/
   30 sso(ks+i+1)=s(1,je)+((s(1,je+1)-s(1,je))*i)/nb                            !/*eb         33*/
      do 100 ni=1,20                                                            !/*eb         34*/
      do 40 i=1,nb                                                              !/*eb         35*/
      ke=0                                                                      !/*eb         36*/
      ie=0                                                                      !/*eb         37*/
      kl=0                                                                      !/*eb         38*/
      p=0.                                                                      !/*eb         39*/
      e1=sso(ks+i)                                                              !/*eb         40*/
      e2=sso(ks+i+1)                                                            !/*eb         41*/
      if(q(11).ne.0.)p=.5*q(11)*                                                !/*eb         42*/
     & (erf2((e2-q(3))/q(2))-erf2((e1-q(3))/q(2)))                              !/*eb         43*/
      if(q(11).eq.1.)go to 40                                                   !/*eb         44*/
      if(em.le.e1.or.em.ge.e2)p=p+(1.-q(11))*qnc7(spec,e1,e2,er,1000,ke)        !/*eb         45*/
      if(em.gt.e1.and.em.lt.e2)p=p+(1.-q(11))*                                  !/*eb         46*/
     & (qnc7(spec,e1,em,er,1000,ie)+qnc7(spec,em,e2,er,1000,kl))                !/*eb         47*/
      if(min(ke,ie,kl).lt.0)call erprnt(1,1,1,k(1),0,0,0,0,                     !/*eb         48*/
     & '47hqnc7 integration error in eqpbbn.  distribution,i4')                 !/*eb         49*/
   40 prb(lprb+i+1)=p+prb(lprb+i)                                               !/*eb         50*/
      if(prb(lprb+nb+1).gt.0.)go to 50                                          !/*eb         51*/
      call erprnt(1,1,2,je,k(1),0,0,0,                                          !/*eb         52*/
     & '23hzero probability in bin,i4,23h of source distribution,i4')           !/*eb         53*/
      go to 110                                                                 !/*eb         54*/
   50 d=0.                                                                      !/*eb         55*/
      j1=1                                                                      !/*eb         56*/
      do 80 i=2,nb                                                              !/*eb         57*/
      do 60 j=j1,nb                                                             !/*eb         58*/
   60 if(prb(lprb+j+1).gt.(prb(lprb+nb+1)*(i-1))/nb)go to 70                    !/*eb         59*/
   70 j1=j                                                                      !/*eb         60*/
      d=max(d,abs(prb(lprb+i)-(prb(lprb+nb+1)*(i-1))/nb))                       !/*eb         61*/
   80 bbv(lbbv+i-1)=sso(ks+j)+(sso(ks+j+1)-sso(ks+j))*((prb(lprb+nb+1)*         !/*eb         62*/
     & (i-1))/nb-prb(lprb+j))/(prb(lprb+j+1)-prb(lprb+j))                       !/*eb         63*/
      do 90 i=2,nb                                                              !/*eb         64*/
   90 sso(ks+i)=bbv(lbbv+i-1)                                                   !/*eb         65*/
  100 if(d.lt..001*prb(lprb+nb+1))go to 110                                     !/*eb         66*/
      call erprnt(1,1,2,je,k(1),0,0,0,                                          !/*eb         67*/
     & '30hbins did not converge in group,i3,16h of distribution,i4')           !/*eb         68*/
  110 sfb(lsfb+je)=prb(lprb+nb+1)                                               !/*eb         69*/
!                                                                               !/*eb         70*/
!        print the equiprobable bins.                                           !/*eb         71*/
      ea=0.                                                                     !/*eb         72*/
      do 120 i=1,nb                                                             !/*eb         73*/
!ccc     extra parenthesis required for convex compiler optimization.           !/*eb4b        1*/
  120 ea=(ea+.5*(sso(ks+i+1)+sso(ks+i)))                                        !/*eb4b        2*/
      eaa(leaa+irs)=eaa(leaa+irs)+prb(lprb+nb+1)*ea/nb                          !/*eb         75*/
  130 if(ink(10).ne.0)write(iuo,140)je,(sso(ks+i),i=1,nb+1)                     !/*eb         76*/
  140 format(/i7,1p9e12.4/(7x,1p9e12.4))                                        !/*eb         77*/
      if(sso(k(14)+ms*nb+1).gt.1.5*sso(k(14)+ms*nb))call erprnt                 !/*eb         78*/
     & (1,2,1,k(1),0,0,0,0,'25hvery poor top group on si,i3,6h card.')          !/*eb         79*/
!                                                                               !/*eb         80*/
!        calculate the fraction of the distribution above and below             !/*eb         81*/
!        the source range.                                                      !/*eb         82*/
      sm=1.                                                                     !/*eb         83*/
      t1=0.                                                                     !/*eb         84*/
      t2=0.                                                                     !/*eb         85*/
      spf(k(13)+3,1)=0.                                                         !/*eb         86*/
      do 150 j=1,ms                                                             !/*eb         87*/
      t1=t1+sfb(lsfb+j)                                                         !/*eb         88*/
  150 t2=t2+s(3,j+1)                                                            !/*eb         89*/
      eaa(leaa+irs)=eaa(leaa+irs)/t1                                            !/*eb         90*/
      if(k(12).ne.3)go to 190                                                   !/*eb         91*/
      s1=0.                                                                     !/*eb         92*/
      s2=0.                                                                     !/*eb         93*/
      ie=0                                                                      !/*eb         94*/
      je=0                                                                      !/*eb         95*/
      if(q(11).eq.0.)go to 160                                                  !/*eb         96*/
      s1=.5*q(11)*(1.+erf2((s(1,1)-q(3))/q(2)))                                 !/*eb         97*/
      s2=.5*q(11)*(1.-erf2((s(1,k(4))-q(3))/q(2)))                              !/*eb         98*/
  160 if(q(11).eq.1.)go to 170                                                  !/*eb         99*/
      e=s(1,1)                                                                  !/*eb        100*/
      if(e.gt.0.)s1=s1-(1.-q(11))*qnc7(spec,e,zero,er,1000,ie)                  !/*eb        101*/
      e=s(1,k(4))                                                               !/*eb        102*/
      if(e.lt.100.)s2=(1.-q(11))*qnc7(spec,e,zero+100.,er,1000,je)+s2           !/*eb        103*/
      if(ie.lt.0.or.je.lt.0)call erprnt(1,1,0,0,0,0,0,0,                        !/*eb        104*/
     & '45hqnc7 integration error in normalizing source.')                      !/*eb        105*/
  170 t3=1./(t1+s2+s1)                                                          !/*eb        106*/
      sm=t1*t3                                                                  !/*eb        107*/
      if(ink(10).ne.0)write(iuo,180)s1*t3,s2*t3,sm,t1+s2+s1                     !/*eb        108*/
  180 format(//5x,45hfraction of distribution below source range =,             !/*eb        109*/
     & 1pe12.5/5x,45hfraction of distribution above source range =,e12.5        !/*eb        110*/
     & /5x,27hsource weight multiplied by,e12.5,                                !/*eb        111*/
     & 41h = fraction of distribution within range./                            !/*eb        112*/
     & 5x,29htotal distribution integral =,0pf11.8)                             !/*eb        113*/
  190 if(ink(10).ne.0)write(iuo,200)s(1,1),0.,0.,0.,1.                          !/*eb        114*/
  200 format(//47x,10hcumulative,6x,6hweight/18x,11hprobability,                !/*eb        115*/
     & 6x,4hbias,7x,11hprobability,4x,10hmultiplier//1x,1p5e14.4)               !/*eb        116*/
!                                                                               !/*eb        117*/
!       normalize and adjust probability and bias arrays.                       !/*eb        118*/
      spf(k(13)+2,1)=0.                                                         !/*eb        119*/
      do 210 j=1,ms                                                             !/*eb        120*/
      t4=sfb(lsfb+j)/t1                                                         !/*eb        121*/
      t5=s(3,j+1)                                                               !/*eb        122*/
      t6=t4                                                                     !/*eb        123*/
      if(t2.ne.0.)t6=s(3,j+1)/t2                                                !/*eb        124*/
      spf(k(13)+2,j+1)=s(2,j)+t6                                                !/*eb        125*/
      spf(k(13)+3,j+1)=sm                                                       !/*eb        126*/
      if(t6.ne.0.)spf(k(13)+3,j+1)=sm*t4/t6                                     !/*eb        127*/
      swtx=min(swtx,s(3,j+1))                                                   !/*eb        128*/
  210 if(ink(10).ne.0)write(iuo,220)s(1,j+1),t4,t5,s(2,j+1),s(3,j+1)            !/*eb        129*/
  220 format(1x,1p5e14.4)                                                       !/*eb        130*/
      spf(k(13)+3,1)=sm                                                         !/*eb        131*/
      return                                                                    !/*eb        132*/
      end                                                                       !/*eb        133*/
