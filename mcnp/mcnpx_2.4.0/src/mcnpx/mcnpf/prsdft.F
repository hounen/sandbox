!#ident prsdft.F from deck pp                                                   !/*pp          1*/
      subroutine prsdft                                                         !/*pp          2*/
!        print source distribution frequency tables.                            !/*pp          3*/
      use global_data                                                           !/*ppx         1*/
!                                                                               !/*ppx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*ppx         3*/
      implicit integer(kindi) (i-n)                                             !/*ppx         4*/
!                                                                               !/*ppx         5*/
      dimension fs(20),fb(20),gs(20),gb(20),js(20),ms(20)                       !/*pp          5*/
      character hq*2,hx*75                                                      !/*pp4c        1*/
      k(i,j)=ksd(lksd+i,j)                                                      !/*pp          7*/
      s(i,j,m)=spf(k(13,i)+j,m)                                                 !/*pp          8*/
!                                                                               !/*pp          9*/
!        do all distributions for which frequency data exist.                   !/*pp         10*/
      nf=0                                                                      !/*pp         11*/
      do 220 md=1,msd                                                           !/*pp         12*/
      if(k(4,md)*k(12,md).eq.0.or.k(2,md)+k(7,md).ne.0)go to 220                !/*pp         13*/
      if(nf.eq.0)write(iuo,10)                                                  !/*pp         14*/
   10 format(37h1source distribution frequency tables,67x,                      !/*pp         15*/
     & 15hprint table 170//41h the expected values below do not include,        !/*pp         16*/
     & 57h the effect of the rejection loop which samples position.)            !/*pp         17*/
      nf=1                                                                      !/*pp         18*/
      write(iuo,20)k(1,md),hbln(k(12,md),1)                                     !/*pp         19*/
   20 format(//20h source distribution,i4,6h  for ,a3)                          !/*pp         20*/
      do 40 i=k(15,md)+1,k(15,md)+k(3,md),25                                    !/*pp         21*/
      do 30 j=i,i+24                                                            !/*pp         22*/
   30 hx((j-i)*3+1:(j-i)*3+3)=char(jscn(j)/65536)//                             !/*pp         23*/
     & char(mod(jscn(j),65536)/256)//char(mod(jscn(j),256))                     !/*pp         24*/
   40 write(iuo,'(10x,a75)')hx                                                  !/*pp         25*/
!                                                                               !/*pp         26*/
!        sort out the possible cases.                                           !/*pp         27*/
      f=1.                                                                      !/*pp         28*/
      g=1.                                                                      !/*pp         29*/
      jd=ivdis(k(12,md))                                                        !/*pp         30*/
      if(jd.eq.0.or.jd.eq.md)go to 110                                          !/*cor4-1    197*/
      if(k(8,jd).ne.0)go to 100                                                 !/*pp         32*/
!                                                                               !/*pp         33*/
!        case of s on si or ds card.                                            !/*pp         34*/
!        work recursively through the entire tree of s options.                 !/*pp         35*/
      l=1                                                                       !/*pp         36*/
   50 f=0.                                                                      !/*pp         37*/
      g=0.                                                                      !/*pp         38*/
      m=1                                                                       !/*pp         39*/
   60 if(s(jd,1,m).le.0.)go to 90                                               !/*pp         40*/
      j=jd                                                                      !/*pp         41*/
      if(k(7,jd).ne.0)j=ivdis(ivdd(k(12,md)))                                   !/*pp         42*/
      f1=s(j,2,m+1-k(5,j))                                                      !/*pp         43*/
      if(m.ne.1)f1=f1-s(j,2,m-k(5,j))                                           !/*pp         44*/
      g1=f1*s(j,3,m+1-k(5,j))                                                   !/*pp         45*/
      if(s(jd,1,m).ne.md)go to 70                                               !/*pp         46*/
      f=f+f1                                                                    !/*pp         47*/
      g=g+g1                                                                    !/*pp         48*/
      go to 90                                                                  !/*pp         49*/
   70 if(k(6,int(s(jd,1,m))).eq.0)go to 90                                      !/*pp         50*/
      fs(l)=f                                                                   !/*pp         51*/
      gs(l)=g                                                                   !/*pp         52*/
      fb(l)=f1                                                                  !/*pp         53*/
      gb(l)=g1                                                                  !/*pp         54*/
      js(l)=jd                                                                  !/*pp         55*/
      ms(l)=m                                                                   !/*pp         56*/
      l=l+1                                                                     !/*pp         57*/
      jd=s(jd,1,m)                                                              !/*pp         58*/
      go to 50                                                                  !/*pp         59*/
   80 l=l-1                                                                     !/*pp         60*/
      m=ms(l)                                                                   !/*pp         61*/
      jd=js(l)                                                                  !/*pp         62*/
      g=gs(l)+g*gb(l)                                                           !/*pp         63*/
      f=fs(l)+f*fb(l)                                                           !/*pp         64*/
   90 m=m+1                                                                     !/*pp         65*/
      if(m.le.k(4,jd))go to 60                                                  !/*pp         66*/
      if(l.gt.1)go to 80                                                        !/*pp         67*/
      go to 110                                                                 !/*pp         68*/
!                                                                               !/*pp         69*/
!        case of q on ds card.                                                  !/*pp         70*/
!        the algorithm for this has not been completed yet.                     !/*pp         71*/
  100 f=0.                                                                      !/*pp         72*/
      g=0.                                                                      !/*pp         73*/
!                                                                               !/*pp         74*/
!        print the table.                                                       !/*pp         75*/
  110 write(iuo,120)                                                            !/*pp         76*/
  120 format(/41x,6hnumber,39x,6hweight/13x,6hsource/3x,1hn,10x,5hvalue,        !/*pp         77*/
     & 3x,2(6x,7hsampled,6x,8hexpected,2x,16hsampled/expected)/)                !/*pp         78*/
      t1=0.                                                                     !/*pp         79*/
      t2=0.                                                                     !/*pp         80*/
      t3=0.                                                                     !/*pp         81*/
      t4=0.                                                                     !/*pp         82*/
      do 190 n=2-k(5,md),k(4,md)                                                !/*pp         83*/
      if(k(19,md).ne.0)go to 130                                                !/*pp         84*/
      en=s(md,2,n)                                                              !/*pp         85*/
      if(n.ne.1)en=en-s(md,2,n-1)                                               !/*pp         86*/
      ew=en*srv(1,17)*s(md,3,n)                                                 !/*pp         87*/
      go to 140                                                                 !/*pp         88*/
  130 en=s(md,4,n)-s(md,4,n-1)                                                  !/*pp         89*/
      ew=en*srv(1,17)*(1.-frc(lfrc+md))                                         !/*pp         90*/
      if(k(18,md).eq.0)go to 140                                                !/*pp         91*/
      b=(s(md,2,n)-s(md,2,n-1))/(s(md,1,n)-s(md,1,n-1))                         !/*pp         92*/
      ew=srv(1,17)*(1.-frc(lfrc+md))*((s(md,2,n-1)-b*s(md,1,n-1))*              !/*pp         93*/
     & (s(md,1,n)-s(md,1,n-1))+.5*b*(s(md,1,n)**2-s(md,1,n-1)**2))              !/*pp         94*/
  140 en=f*en                                                                   !/*pp         95*/
      ew=g*ew                                                                   !/*pp         96*/
      sn=wns(lwns+k(16,md)+1,n)*fpi                                             !/*pp         97*/
      r1=1.                                                                     !/*pp         98*/
      if(en.ne.0.)r1=sn/en                                                      !/*pp         99*/
      sw=wns(lwns+k(16,md)+2,n)*fpi                                             !/*pp        100*/
      r2=1.                                                                     !/*pp        101*/
      if(ew.ne.0.)r2=sw/ew                                                      !/*pp        102*/
      t1=t1+sn                                                                  !/*pp        103*/
      t2=t2+en                                                                  !/*pp        104*/
      t3=t3+sw                                                                  !/*pp        105*/
      t4=t4+ew                                                                  !/*pp        106*/
      j=k(21,md)*(n-1)+1                                                        !/*pp        107*/
      if(k(6,md).ne.0.or.k(12,md).le.2.or.k(12,md).eq.14)go to 170              !/*pp        108*/
      write(iuo,150)n-1+k(5,md),s(md,1,j),sn,en,r1,sw,ew,r2                     !/*pp        109*/
  150 format(i4,1pe17.5,3x,3e13.4,6x,3e13.4)                                    !/*pp        110*/
      if(k(21,md).ne.1)write(iuo,160)s(md,1,j+1),s(md,1,j+2)                    !/*pp        111*/
  160 format(1pe21.5)                                                           !/*pp        112*/
      go to 190                                                                 !/*pp        113*/
  170 m=0                                                                       !/*pp        114*/
      if(k(6,md).ne.0.and.s(md,1,j).ne.0)m=ksd(lksd+1,int(s(md,1,j)))           !/*pp        115*/
      if(k(6,md).eq.0.and.k(12,md).ne.2.and.s(md,1,j).gt.0.)                    !/*pp        116*/
     & m=ncl(lncl+int(s(md,1,j)))                                               !/*pp        117*/
      hq=' '                                                                    !/*pp4c        2*/
      if(k(6,md).ne.0.or.k(12,md).ne.2.or.s(md,1,j).eq.0.)go to 178             !/*pp4c        3*/
      mq=s(md,1,j)                                                              !/*pp4c        4*/
      m=nsf(lnsf+mq)                                                            !/*pp4c        5*/
      if(kfq.ne.0)write(hq,'(1h.,i1)')kfq                                       !/*pp4c        6*/
  178 write(iuo,180)n-1+k(5,md),m,hq,sn,en,r1,sw,ew,r2                          !/*pp4c        7*/
  180 format(i4,6x,i6,a2,6x,1p3e13.4,6x,3e13.4)                                 !/*pp4c        8*/
  190 continue                                                                  !/*pp        122*/
      r1=1.                                                                     !/*pp        123*/
      if(t2.ne.0.)r1=t1/t2                                                      !/*pp        124*/
      r2=1.                                                                     !/*pp        125*/
      if(t4.ne.0.)r2=t3/t4                                                      !/*pp        126*/
      write(iuo,200)t1,t2,r1,t3,t4,r2                                           !/*pp        127*/
  200 format(6h total,18x,1p3e13.4,6x,3e13.4)                                   !/*pp        128*/
      if(f*g.eq.0.)write(iuo,210)                                               !/*pp        129*/
  210 format(/43h prsdft does not yet do expected values for,                   !/*pp        130*/
     & 47h distributions with such complicated dependency,                      !/*pp        131*/
     & 17h as this one has.)                                                    !/*pp        132*/
  220 continue                                                                  !/*pp        133*/
      return                                                                    !/*pp        134*/
      end                                                                       !/*pp        135*/
