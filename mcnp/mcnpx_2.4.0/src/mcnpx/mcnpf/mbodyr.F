!#ident mbodyr.F from deck mbr4c2                                               !/*mbr4c2      1*/
      subroutine mbodyr(ia,n,sc)                                                !/*mbr4c2      2*/
!        special vector computations for rhp.                                   !/*mbr4c2      3*/
      use global_parameters                                                     !/*mbrx        1*/
      implicit real(kindr) (a-h,o-z)                                            !/*mbrx        2*/
      implicit integer(kindi) (i-n)                                             !/*mbrx        3*/
      dimension sc(33)                                                          !/*mbr4c2      6*/
      dimension ia(3)                                                           !/*mbr4c2      7*/
!                                                                               !/*mbr4c2      8*/
!        set facet for regular hexagon if default, and                          !/*mbr4c2      9*/
!        check that facet is orthogonal to axis.                                !/*mbr4c2     10*/
      do 110 kk=1,3                                                             !/*mbr4c2     11*/
      ia(kk)=0                                                                  !/*mbr4c2     12*/
      if(kk.ne.1.and.n.le.9)go to 110                                           !/*mbr4c2     13*/
!                                                                               !/*mbr4c2     14*/
!        second facet vector is set orthogonal to axis and                      !/*mbr4c2     15*/
!        60 degrees from first facet.                                           !/*mbr4c2     16*/
!        set ia(kk) to axis if facet kk along an axis and                       !/*mbr4c2     17*/
!        obtain normalized vector.                                              !/*mbr4c2     18*/
      if(sc(5+kk*3).eq.0..and.sc(6+kk*3).eq.0.)ia(kk)=1                         !/*mbr4c2     19*/
      if(sc(4+kk*3).eq.0..and.sc(6+kk*3).eq.0.)ia(kk)=2                         !/*mbr4c2     20*/
      if(sc(4+kk*3).eq.0..and.sc(5+kk*3).eq.0.)ia(kk)=3                         !/*mbr4c2     21*/
      r=sqrt(sc(4+kk*3)**2+sc(5+kk*3)**2+sc(6+kk*3)**2)                         !/*mbr4c2     22*/
      sc(16+kk*3)=sc(4+kk*3)/r                                                  !/*mbr4c2     23*/
      sc(17+kk*3)=sc(5+kk*3)/r                                                  !/*mbr4c2     24*/
      sc(18+kk*3)=sc(6+kk*3)/r                                                  !/*mbr4c2     25*/
      sc(30+kk)=r                                                               !/*mbr4c2     26*/
      if(kk.ne.1.and.n.le.9)go to 10                                            !/*mbr4c2     27*/
!                                                                               !/*mbr4c2     28*/
!        orthogonality check of facet vector and axis                           !/*mbr4c2     29*/
      call mbodyo(1,16,16+3*kk,0,0,ab,ac,sc)                                    !/*mbr4c2     30*/
   10 if(kk.ne.1.or.n.gt.9)go to 110                                            !/*mbr4c2     31*/
      sc(32)=r                                                                  !/*mbr4c2     32*/
      sc(33)=r                                                                  !/*mbr4c2     33*/
!                                                                               !/*mbr4c2     34*/
!        second facet vector is set orthogonal to axis and                      !/*mbr4c2     35*/
!        60 degrees from first facet in c-clockwise direction.                  !/*mbr4c2     36*/
      mq=0                                                                      !/*mbr4c2     37*/
      qc=0.                                                                     !/*mbr4c2     38*/
      qd=0.                                                                     !/*mbr4c2     39*/
      if(sc(16).eq.0.)go to 30                                                  !/*mbr4c2     40*/
      m=1                                                                       !/*mbr4c2     41*/
      d=sc(20)-sc(19)*sc(17)/sc(16)                                             !/*mbr4c2     42*/
      if(abs(d).gt.1.e-7)go to 20                                               !/*mbr4c2     43*/
      mq=1                                                                      !/*mbr4c2     44*/
      d=sc(21)-sc(19)*sc(18)/sc(16)                                             !/*mbr4c2     45*/
      dz=.5/d                                                                   !/*mbr4c2     46*/
      qa=-sc(18)*dz/sc(16)                                                      !/*mbr4c2     47*/
      qb=-sc(17)/sc(16)                                                         !/*mbr4c2     48*/
      go to 70                                                                  !/*mbr4c2     49*/
   20 qa=.5/d                                                                   !/*mbr4c2     50*/
      qb=((sc(19)*sc(18)/sc(16))-sc(21))/d                                      !/*mbr4c2     51*/
      qc=-qa*sc(17)/sc(16)                                                      !/*mbr4c2     52*/
      qd=(-qb*sc(17)/sc(16))-sc(18)/sc(16)                                      !/*mbr4c2     53*/
      go to 70                                                                  !/*mbr4c2     54*/
   30 if(sc(17).eq.0.)go to 50                                                  !/*mbr4c2     55*/
      m=2                                                                       !/*mbr4c2     56*/
      d=sc(21)-sc(20)*sc(18)/sc(17)                                             !/*mbr4c2     57*/
      if(abs(d).gt.1.e-7)go to 40                                               !/*mbr4c2     58*/
      mq=1                                                                      !/*mbr4c2     59*/
      d=sc(19)-sc(20)*sc(16)/sc(17)                                             !/*mbr4c2     60*/
      dz=.5/d                                                                   !/*mbr4c2     61*/
      qa=-sc(16)*dz/sc(17)                                                      !/*mbr4c2     62*/
      qb=-sc(18)/sc(17)                                                         !/*mbr4c2     63*/
      go to 70                                                                  !/*mbr4c2     64*/
   40 qa=.5/d                                                                   !/*mbr4c2     65*/
      qb=((sc(20)*sc(16)/sc(17))-sc(19))/d                                      !/*mbr4c2     66*/
      qc=-qa*sc(18)/sc(17)                                                      !/*mbr4c2     67*/
      qd=(-qb*sc(18)/sc(17))-sc(16)/sc(17)                                      !/*mbr4c2     68*/
      go to 70                                                                  !/*mbr4c2     69*/
   50 m=3                                                                       !/*mbr4c2     70*/
      d=sc(19)-sc(21)*sc(16)/sc(18)                                             !/*mbr4c2     71*/
      if(abs(d).gt.1.e-7)go to 60                                               !/*mbr4c2     72*/
      mq=1                                                                      !/*mbr4c2     73*/
      d=sc(20)-sc(21)*sc(17)/sc(18)                                             !/*mbr4c2     74*/
      dz=.5/d                                                                   !/*mbr4c2     75*/
      qa=-sc(17)*dz/sc(18)                                                      !/*mbr4c2     76*/
      qb=-sc(16)/sc(18)                                                         !/*mbr4c2     77*/
      go to 70                                                                  !/*mbr4c2     78*/
   60 qa=.5/d                                                                   !/*mbr4c2     79*/
      qb=((sc(21)*sc(17)/sc(18))-sc(20))/d                                      !/*mbr4c2     80*/
      qc=-qa*sc(16)/sc(18)                                                      !/*mbr4c2     81*/
      qd=(-qb*sc(16)/sc(18))-sc(17)/sc(18)                                      !/*mbr4c2     82*/
   70 a=qd**2+qb**2+1                                                           !/*mbr4c2     83*/
      b=2.*(qc*qd+qa*qb)                                                        !/*mbr4c2     84*/
      c=qc**2+qa**2-1.0                                                         !/*mbr4c2     85*/
      if(mq.eq.1)c=c+dz**2                                                      !/*mbr4c2     86*/
      q=b**2-4.*a*c                                                             !/*mbr4c2     87*/
      q=sqrt(q)                                                                 !/*mbr4c2     88*/
!                                                                               !/*mbr4c2     89*/
!        first select root with plus sign.                                      !/*mbr4c2     90*/
      call mbodys(1,m,mq,a,b,dz,q,qa,qb,sc)                                     !/*mbr4c2     91*/
!        check if right root?                                                   !/*mbr4c2     92*/
      call crspro(sc(22),sc(19),sc(28))                                         !/*mbr4c2     93*/
      ba=(sc(16)*sc(28)+sc(17)*sc(29)+sc(18)*sc(30))/(0.5*sqrt(3.))             !/*mbr4c2     94*/
      if(abs(ba).lt.0.99995.or.abs(ba).gt.1.00005)call erprnt                   !/*mbr4c2     95*/
     & (2,1,0,0,0,0,0,0,'37hfailed to create second facet vector.')             !/*mbr4c2     96*/
      if(ba.lt.0.)go to 90                                                      !/*mbr4c2     97*/
!                                                                               !/*mbr4c2     98*/
!        switch to other root.                                                  !/*mbr4c2     99*/
      q=-q                                                                      !/*mbr4c2    100*/
      call mbodys(1,m,mq,a,b,dz,q,qa,qb,sc)                                     !/*mbr4c2    101*/
      q=-q                                                                      !/*mbr4c2    102*/
!        third facet vector is set orthogonal to axis and                       !/*mbr4c2    103*/
!        120 degrees from first facet in c-clockwise direction.                 !/*mbr4c2    104*/
   90 qa=-qa                                                                    !/*mbr4c2    105*/
      qc=-qc                                                                    !/*mbr4c2    106*/
      b=-b                                                                      !/*mbr4c2    107*/
      if(mq.eq.1)dz=-dz                                                         !/*mbr4c2    108*/
!                                                                               !/*mbr4c2    109*/
!        first select root with plus sign.                                      !/*mbr4c2    110*/
      call mbodys(2,m,mq,a,b,dz,q,qa,qb,sc)                                     !/*mbr4c2    111*/
  100 call crspro(sc(25),sc(19),sc(28))                                         !/*mbr4c2    112*/
      ba=(sc(16)*sc(28)+sc(17)*sc(29)+sc(18)*sc(30))/(0.5*sqrt(3.))             !/*mbr4c2    113*/
      if(abs(ba).lt.0.99995.or.abs(ba).gt.1.00005)call erprnt                   !/*mbr4c2    114*/
     & (2,1,0,0,0,0,0,0,'36hfailed to create third facet vector.')              !/*mbr4c2    115*/
      if(ba.lt.0.)go to 110                                                     !/*mbr4c2    116*/
!                                                                               !/*mbr4c2    117*/
!        switch to other root.                                                  !/*mbr4c2    118*/
      q=-q                                                                      !/*mbr4c2    119*/
      call mbodys(2,m,mq,a,b,dz,q,qa,qb,sc)                                     !/*mbr4c2    120*/
      q=-q                                                                      !/*mbr4c2    121*/
  110 continue                                                                  !/*mbr4c2    122*/
      return                                                                    !/*mbr4c2    123*/
      end                                                                       !/*mbr4c2    124*/
