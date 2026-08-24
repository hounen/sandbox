!#ident mbody.F from deck xb4c                                                  !/*xb4c        1*/
      subroutine mbody(jf,ix)                                                   !/*xb4c        2*/
!        set up  and check additional surfaces for macrobodies.                 !/*xb4c        3*/
!        jf = 1/2 call from oldcrd/igeom.                                       !/*xb4c        4*/
!        ix = surface index if called from oldcrd.                              !/*xb4c        5*/
      use global_data                                                           !/*xbx         1*/
      use imcn_data                                                             !/*xbx         2*/
!                                                                               !/*xbx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*xbx         4*/
      implicit integer(kindi) (i-n)                                             !/*xbx         5*/
!                                                                               !/*xbx         6*/
      dimension sc(33)                                                          !/*xb4c        8*/
      dimension ia(3),mf(6,5)                                                   !/*xb4c2       1*/
!                                                                               !/*xb4c       10*/
      if(jf.eq.2)go to 710                                                      !/*xb4c2       2*/
!                                                                               !/*xb4c       12*/
!**************************** called from oldcrd ***********************        !/*xb4c       13*/
!                                                                      *        !/*xb4c       14*/
!        a new array, ksm, is created with an entry for each surface.           !/*xb4c       15*/
!        if the surface is not part of a macrobody then ksm(lksm+mxj)=0.        !/*xb4c       16*/
!        if the surface is a master surface of  the macrobody, then             !/*xb4c       17*/
!        ksm(lksm+mxj)=-macrobody surface type, that is incremented             !/*xb4c       18*/
!        by -1000 if it is infinite in one dimension.                           !/*xb4c       19*/
!        if the surface is one of the other surfaces of the macrobody,          !/*xb4c       20*/
!        then ksm(lksm+mxj) is set to the problem number of master              !/*xb4c       21*/
!        surface.                                                               !/*xb4c       22*/
      ia(1)=0                                                                   !/*xb4c2       3*/
      ia(2)=0                                                                   !/*xb4c2       4*/
      ia(3)=0                                                                   !/*xb4c2       5*/
      n=nwc-1-m2c                                                               !/*xb4c2       6*/
      do 10 k=1,33                                                              !/*xb4c2       7*/
      if(k.gt.n)sc(k)=0.                                                        !/*xb4c2       8*/
   10 if(k.le.n)sc(k)=scf(ix+k)                                                 !/*xb4c2       9*/
      lsc(llsc+mxj)=ix                                                          !/*xb4c2      10*/
      iy=ix                                                                     !/*xb4c2      11*/
      ksm(lksm+mxj)=-m1c                                                        !/*xb4c2      12*/
      kst(lkst+mxj)=m1c                                                         !/*xb4c2      13*/
      jn=6                                                                      !/*xb4c2      14*/
      if(m1c.eq.33.or.m1c.eq.34.or.m1c.eq.36)jn=3                               !/*xb4c2      15*/
      if(m1c.eq.39)jn=8                                                         !/*xb4c2      16*/
      if(m1c.eq.32.or.m1c.eq.35)jn=0                                            !/*xb4c2      17*/
      if(m1c.eq.37)jn=5                                                         !/*xb4c2      18*/
      go to(20,70,90,140,230,310,360,420,510,200)m1c-29                         !/*xb4c2      19*/
!                                                                               !/*xb4c2      20*/
! >>>>>  box: set coefficients for each j surface.                              !/*xb4c2      21*/
   20 if(n.eq.9)jn=4                                                            !/*xb4c2      22*/
      do 40 k=1,jn/2                                                            !/*xb4c2      23*/
      x=sc(3+3*(k-1)+1)                                                         !/*xb4c2      24*/
      y=sc(3+3*(k-1)+2)                                                         !/*xb4c2      25*/
      z=sc(3+3*(k-1)+3)                                                         !/*xb4c2      26*/
      r=x**2+y**2+z**2                                                          !/*xb4c2      27*/
      if(r.eq.0.)call erprnt(2,1,0,0,0,0,0,0,                                   !/*xb4c2      28*/
     & '29hzero dimension for macrobody.')                                      !/*xb4c2      29*/
      sc(21+k)=0.                                                               !/*xb4c2      30*/
      if(r.eq.0.)go to 40                                                       !/*xb4c2      31*/
      r=sqrt(r)                                                                 !/*xb4c2      32*/
      sc(13+3*(k-1))=x/r                                                        !/*xb4c2      33*/
      sc(14+3*(k-1))=y/r                                                        !/*xb4c2      34*/
      sc(15+3*(k-1))=z/r                                                        !/*xb4c2      35*/
      sc(21+k)=r                                                                !/*xb4c2      36*/
      if(k.eq.1)go to 40                                                        !/*xb4c2      37*/
      if(k.eq.3)go to 30                                                        !/*xb4c2      38*/
!                                                                               !/*xb4c2      39*/
!        check for orthogonal vectors.                                          !/*xb4c2      40*/
      call mbodyo(1,13,16,0,0,ab,ac,sc)                                         !/*xb4c2      41*/
      go to 40                                                                  !/*xb4c2      42*/
   30 call mbodyo(2,13,16,25,19,ab,ac,sc)                                       !/*xb4c2      43*/
   40 continue                                                                  !/*xb4c2      44*/
      x=-1.                                                                     !/*xb4c2      45*/
      if(jn.eq.4)call erprnt(2,2,0,0,0,0,0,0,                                   !/*xb4c2      46*/
     & '33hbox is infinite in one dimension.')                                  !/*xb4c2      47*/
!                                                                               !/*xb4c2      48*/
!        increment ksm if box is infinite in a dimension.                       !/*xb4c2      49*/
      if(jn.eq.4)ksm(lksm+mxj)=-m1c-1000                                        !/*xb4c2      50*/
      do 60 j=1,jn                                                              !/*xb4c2      51*/
      jtr(ljtr+mxj+j)=jtr(ljtr+mxj)                                             !/*xb4c2      52*/
      ksu(lksu+mxj+j)=ksu(lksu+mxj)                                             !/*xb4c2      53*/
      ksm(lksm+mxj+j)=mxj                                                       !/*xb4c2      54*/
      nsfm(lnsf+mxj+j)=0                                                        !/*xb4c2      55*/
      lsc(llsc+mxj+j)=iy                                                        !/*xb4c2      56*/
      kst(lkst+mxj+j)=1                                                         !/*xb4c2      57*/
      jj=(j+1)/2                                                                !/*xb4c2      58*/
      if(x.ge.0.)go to 50                                                       !/*xb4c2      59*/
      ij=0                                                                      !/*xb4c2      60*/
      if(sc(14+(jj-1)*3).eq.0..and.sc(15+(jj-1)*3).eq.0.)ij=1                   !/*xb4c2      61*/
      if(sc(13+(jj-1)*3).eq.0..and.sc(15+(jj-1)*3).eq.0.)ij=2                   !/*xb4c2      62*/
      if(sc(13+(jj-1)*3).eq.0..and.sc(14+(jj-1)*3).eq.0.)ij=3                   !/*xb4c2      63*/
   50 call mbodyp(5,ii,ij,ik,iy,j,jj,jn,ia,x,h,sc)                              !/*xb4c2      64*/
   60 continue                                                                  !/*xb4c2      65*/
      go to 700                                                                 !/*xb4c2      66*/
!                                                                               !/*xb4c2      67*/
! >>>>>  rpp: set coefficients for each j surface.                              !/*xb4c2      68*/
   70 x=-1.                                                                     !/*xb4c2      69*/
      ik=0                                                                      !/*xb4c2      70*/
!                                                                               !/*xb4c2      71*/
!        check for infinite in one dimension.                                   !/*xb4c2      72*/
      if(sc(1).eq.sc(2))ik=11                                                   !/*xb4c2      73*/
      if(sc(3).eq.sc(4))ik=ik+12                                                !/*xb4c2      74*/
      if(sc(5).eq.sc(6))ik=ik+13                                                !/*xb4c2      75*/
      if(sc(1).gt.sc(2).or.sc(3).gt.sc(4).or.sc(5).gt.sc(6).or.ik.gt.15)        !/*xb4c2      76*/
     & call erprnt(2,1,0,0,0,0,0,0,                                             !/*xb4c2      77*/
     & '32hinvalid dimension for macrobody.')                                   !/*xb4c2      78*/
      if(ik.gt.10.and.ik.lt.14)jn=4                                             !/*xb4c2      79*/
      if(jn.eq.4)call erprnt(2,2,0,0,0,0,0,0,                                   !/*xb4c2      80*/
     & '33hrpp is infinite in one dimension.')                                  !/*xb4c2      81*/
!                                                                               !/*xb4c2      82*/
!        increment ksm if rpp is infinite in a dimension.                       !/*xb4c2      83*/
      if(jn.eq.4)ksm(lksm+mxj)=-m1c-1000                                        !/*xb4c2      84*/
      do 80 j=1,jn                                                              !/*xb4c2      85*/
      jtr(ljtr+mxj+j)=jtr(ljtr+mxj)                                             !/*xb4c2      86*/
      ksu(lksu+mxj+j)=ksu(lksu+mxj)                                             !/*xb4c2      87*/
      ksm(lksm+mxj+j)=mxj                                                       !/*xb4c2      88*/
      nsfm(lnsf+mxj+j)=0                                                        !/*xb4c2      89*/
      kst(lkst+mxj+j)=1                                                         !/*xb4c2      90*/
      jj=(j+1)/2                                                                !/*xb4c2      91*/
      lsc(llsc+mxj+j)=iy                                                        !/*xb4c2      92*/
      ij=jj                                                                     !/*xb4c2      93*/
      call mbodyp(6,ii,ij,ik,iy,j,jj,jn,ia,x,h,sc)                              !/*xb4c2      94*/
   80 continue                                                                  !/*xb4c2      95*/
      go to 700                                                                 !/*xb4c2      96*/
!                                                                               !/*xb4c2      97*/
! >>>>>  sph: set coefficients for surface as s surface.                        !/*xb4c2      98*/
   90 if(sc(n).le.0.)call erprnt(2,1,0,0,0,0,0,0,                               !/*xb4c2      99*/
     & '32hinvalid dimension for macrobody.')                                   !/*xb4c2     100*/
      ksm(lksm+mxj)=0                                                           !/*xb4c2     101*/
      if(n.eq.1)go to 100                                                       !/*xb4c2     102*/
      if(sc(1).ne.0..or.sc(2).ne.0..or.sc(3).ne.0.)go to 110                    !/*xb4c2     103*/
  100 kst(lkst+mxj)=5                                                           !/*xb4c2     104*/
      r=sc(1)                                                                   !/*xb4c2     105*/
      if(n.ne.1)r=sc(4)                                                         !/*xb4c2     106*/
      scf(iy+1)=r**2                                                            !/*xb4c2     107*/
      go to 130                                                                 !/*xb4c2     108*/
  110 j=0                                                                       !/*xb4c2     109*/
      if(sc(2).eq.0..and.sc(3).eq.0.)j=1                                        !/*xb4c2     110*/
      if(sc(1).eq.0..and.sc(3).eq.0.)j=2                                        !/*xb4c2     111*/
      if(sc(1).eq.0..and.sc(2).eq.0.)j=3                                        !/*xb4c2     112*/
      kst(lkst+mxj)=6+j                                                         !/*xb4c2     113*/
      if(j.eq.0)go to 120                                                       !/*xb4c2     114*/
      scf(iy+1)=sc(j)                                                           !/*xb4c2     115*/
      scf(iy+2)=sc(4)**2                                                        !/*xb4c2     116*/
      iy=iy+1                                                                   !/*xb4c2     117*/
      go to 130                                                                 !/*xb4c2     118*/
  120 scf(iy+1)=sc(1)                                                           !/*xb4c2     119*/
      scf(iy+2)=sc(2)                                                           !/*xb4c2     120*/
      scf(iy+3)=sc(3)                                                           !/*xb4c2     121*/
      scf(iy+4)=sc(4)**2                                                        !/*xb4c2     122*/
      iy=iy+3                                                                   !/*xb4c2     123*/
  130 iy=iy+1                                                                   !/*xb4c2     124*/
      go to 700                                                                 !/*xb4c2     125*/
!                                                                               !/*xb4c2     126*/
! >>>>>  rcc: set coefficients for each j surface.                              !/*xb4c2     127*/
  140 x=-1.                                                                     !/*xb4c2     128*/
      if((sc(4).eq.0..and.sc(5).eq.0..and.sc(6).eq.0.).or.                      !/*xb4c2     129*/
     & sc(7).le.0.)call erprnt(2,1,0,0,0,0,0,0,                                 !/*xb4c2     130*/
     & '32hinvalid dimension for macrobody.')                                   !/*xb4c2     131*/
      jj=1                                                                      !/*xb4c2     132*/
      ij=0                                                                      !/*xb4c2     133*/
      if(sc(5).eq.0..and.sc(6).eq.0.)ij=1                                       !/*xb4c2     134*/
      if(sc(4).eq.0..and.sc(6).eq.0.)ij=2                                       !/*xb4c2     135*/
      if(sc(4).eq.0..and.sc(5).eq.0.)ij=3                                       !/*xb4c2     136*/
      if(ij.ne.0)go to 150                                                      !/*xb4c2     137*/
      r=sqrt(sc(4)**2+sc(5)**2+sc(6)**2)                                        !/*xb4c2     138*/
      sc(8)=sc(4)/r                                                             !/*xb4c2     139*/
      sc(9)=sc(5)/r                                                             !/*xb4c2     140*/
      sc(10)=sc(6)/r                                                            !/*xb4c2     141*/
  150 if(ij.eq.1)r=sc(2)**2+sc(3)**2                                            !/*xb4c2     142*/
      if(ij.eq.2)r=sc(1)**2+sc(3)**2                                            !/*xb4c2     143*/
      if(ij.eq.3)r=sc(1)**2+sc(2)**2                                            !/*xb4c2     144*/
      if(ij.eq.0)r=sc(1)**2+sc(2)**2+sc(3)**2                                   !/*xb4c2     145*/
      do 190 j=1,jn                                                             !/*xb4c2     146*/
      jtr(ljtr+mxj+j)=jtr(ljtr+mxj)                                             !/*xb4c2     147*/
      ksu(lksu+mxj+j)=ksu(lksu+mxj)                                             !/*xb4c2     148*/
      ksm(lksm+mxj+j)=mxj                                                       !/*xb4c2     149*/
      nsfm(lnsf+mxj+j)=0                                                        !/*xb4c2     150*/
      kst(lkst+mxj+j)=1                                                         !/*xb4c2     151*/
      lsc(llsc+mxj+j)=iy                                                        !/*xb4c2     152*/
      if(j.ne.1)go to 180                                                       !/*xb4c2     153*/
!                                                                               !/*xb4c2     154*/
!        set up cylinder as special surface or gq.                              !/*xb4c2     155*/
      if(ij.eq.0)go to 170                                                      !/*xb4c2     156*/
      if(r.ne.0.)go to 160                                                      !/*xb4c2     157*/
      kst(lkst+mxj+j)=12+ij                                                     !/*xb4c2     158*/
      scf(iy+1)=sc(7)**2                                                        !/*xb4c2     159*/
      iy=iy+1                                                                   !/*xb4c2     160*/
      go to 190                                                                 !/*xb4c2     161*/
  160 kst(lkst+mxj+j)=9+ij                                                      !/*xb4c2     162*/
      scf(iy+1)=sc(1)                                                           !/*xb4c2     163*/
      scf(iy+2)=sc(3)                                                           !/*xb4c2     164*/
      if(ij.eq.1)scf(iy+1)=sc(2)                                                !/*xb4c2     165*/
      if(ij.eq.3)scf(iy+2)=sc(2)                                                !/*xb4c2     166*/
      scf(iy+3)=sc(7)**2                                                        !/*xb4c2     167*/
      iy=iy+3                                                                   !/*xb4c2     168*/
      go to 190                                                                 !/*xb4c2     169*/
  170 kst(lkst+mxj+j)=23                                                        !/*xb4c2     170*/
      scf(iy+1)=1.-sc(8)**2                                                     !/*xb4c2     171*/
      scf(iy+2)=1.-sc(9)**2                                                     !/*xb4c2     172*/
      scf(iy+3)=1.-sc(10)**2                                                    !/*xb4c2     173*/
      scf(iy+4)=-2.*sc(8)*sc(9)                                                 !/*xb4c2     174*/
      scf(iy+5)=-2.*sc(9)*sc(10)                                                !/*xb4c2     175*/
      scf(iy+6)=-2.*sc(8)*sc(10)                                                !/*xb4c2     176*/
      scf(iy+7)=-sc(2)*scf(iy+4)-sc(3)*scf(iy+6)-2.*sc(1)*scf(iy+1)             !/*xb4c2     177*/
      scf(iy+8)=-sc(1)*scf(iy+4)-sc(3)*scf(iy+5)-2.*sc(2)*scf(iy+2)             !/*xb4c2     178*/
      scf(iy+9)=-sc(1)*scf(iy+6)-sc(2)*scf(iy+5)-2.*sc(3)*scf(iy+3)             !/*xb4c2     179*/
      scf(iy+10)=sc(1)*sc(2)*scf(iy+4)+sc(2)*sc(3)*scf(iy+5)+                   !/*xb4c2     180*/
     & sc(1)*sc(3)*scf(iy+6)+sc(1)**2*scf(iy+1)+sc(2)**2*scf(iy+2)+             !/*xb4c2     181*/
     & sc(3)**2*scf(iy+3)-sc(7)**2                                              !/*xb4c2     182*/
      iy=iy+10                                                                  !/*xb4c2     183*/
      go to 190                                                                 !/*xb4c2     184*/
  180 call mbodyp(3,ii,ij,ik,iy,j,jj,jn,ia,x,h,sc)                              !/*xb4c2     185*/
  190 continue                                                                  !/*xb4c2     186*/
      go to 700                                                                 !/*xb4c2     187*/
!                                                                               !/*xb4c2     188*/
! >>>>>  rhp: set coefficients for each j surface.                              !/*xb4c2     189*/
!        interpret defaults when data truncated.                                !/*xb4c2     190*/
  200 x=-1.                                                                     !/*xb4c2     191*/
      if((sc(4).eq.0..and.sc(5).eq.0..and.sc(6).eq.0.))                         !/*xb4c2     192*/
     & call erprnt(2,1,0,0,0,0,0,0,                                             !/*xb4c2     193*/
     & '32hinvalid dimension for macrobody.')                                   !/*xb4c2     194*/
      if(n.gt.9.and.n.le.12)call erprnt(2,1,0,0,0,0,0,0,                        !/*xb4c2     195*/
     & '35hno default allowed for third facet.')                                !/*xb4c2     196*/
      ij=0                                                                      !/*xb4c2     197*/
      if(sc(5).eq.0..and.sc(6).eq.0.)ij=1                                       !/*xb4c2     198*/
      if(sc(4).eq.0..and.sc(6).eq.0.)ij=2                                       !/*xb4c2     199*/
      if(sc(4).eq.0..and.sc(5).eq.0.)ij=3                                       !/*xb4c2     200*/
      h=sqrt(sc(4)**2+sc(5)**2+sc(6)**2)                                        !/*xb4c2     201*/
      sc(16)=sc(4)/h                                                            !/*xb4c2     202*/
      sc(17)=sc(5)/h                                                            !/*xb4c2     203*/
      sc(18)=sc(6)/h                                                            !/*xb4c2     204*/
!                                                                               !/*xb4c2     205*/
!        set facet for regular hexagon if default, and                          !/*xb4c2     206*/
!        check that facet is orthogonal to axis.                                !/*xb4c2     207*/
      call mbodyr(ia,n,sc)                                                      !/*xb4c2     208*/
      if(h.ge.1.e6)jn=6                                                         !/*xb4c2     209*/
      if(jn.eq.6)call erprnt(2,2,0,0,0,0,0,0,                                   !/*xb4c2     210*/
     & '35hrhp is infinite in axial dimension.')                                !/*xb4c2     211*/
      if(jn.eq.6)ksm(lksm+mxj)=-m1c-1000                                        !/*xb4c2     212*/
!                                                                               !/*xb4c2     213*/
!        create 6 or 8 surfaces.                                                !/*xb4c2     214*/
      do 220 j=1,jn                                                             !/*xb4c2     215*/
      jtr(ljtr+mxj+j)=jtr(ljtr+mxj)                                             !/*xb4c2     216*/
      ksu(lksu+mxj+j)=ksu(lksu+mxj)                                             !/*xb4c2     217*/
      ksm(lksm+mxj+j)=mxj                                                       !/*xb4c2     218*/
      nsfm(lnsf+mxj+j)=0                                                        !/*xb4c2     219*/
      lsc(llsc+mxj+j)=iy                                                        !/*xb4c2     220*/
      kst(lkst+mxj+j)=1                                                         !/*xb4c2     221*/
      jj=(j+1)/2                                                                !/*xb4c2     222*/
      if(j.gt.6)go to 210                                                       !/*xb4c2     223*/
      call mbodyp(7,ii,ij,ik,iy,j,jj,jn,ia,x,h,sc)                              !/*xb4c2     224*/
      go to 220                                                                 !/*xb4c2     225*/
  210 call mbodyp(8,ii,ij,ik,iy,j,jj,jn,ia,x,h,sc)                              !/*xb4c2     226*/
  220 continue                                                                  !/*xb4c2     227*/
      go to 700                                                                 !/*xb4c2     228*/
!                                                                               !/*xb4c2     229*/
! >>>>>  rec: set coefficients for each j surface.                              !/*xb4c2     230*/
  230 x=-1.                                                                     !/*xb4c2     231*/
      jj=1                                                                      !/*xb4c2     232*/
      if((sc(4).eq.0..and.sc(5).eq.0..and.sc(6).eq.0.).or.                      !/*xb4c2     233*/
     & (sc(7).eq.0..and.sc(8).eq.0..and.sc(9).eq.0.).or.                        !/*xb4c2     234*/
     & (sc(10).eq.0..and.sc(11).eq.0..and.sc(12).eq.0.))                        !/*xb4c2     235*/
     & call erprnt(2,1,0,0,0,0,0,0,'26hinvalid dimension for rec.')             !/*xb4c2     236*/
      if(n.eq.12.and.sc(10).eq.0..and.sc(11).eq.0..and.sc(12).eq.0.)            !/*xb4c2     237*/
     & call erprnt(2,1,0,0,0,0,0,0,'26hinvalid dimension for rec.')             !/*xb4c2     238*/
       if(n.eq.10.and.sc(10).le.0.)call erprnt(2,1,0,0,0,0,0,0,                 !/*xb4c2     239*/
     & '26hinvalid dimension for rec.')                                         !/*xb4c2     240*/
      if(n.eq.10)call erprnt(2,2,0,0,0,0,0,0,                                   !/*xb4c2     241*/
     & '40halternate format used for rec macrobody.')                           !/*xb4c2     242*/
!        determine square of major and minor radii, a2 and b2.                  !/*xb4c2     243*/
      a2=sc(7)**2+sc(8)**2+sc(9)**2                                             !/*xb4c2     244*/
      if(n.eq.12)b2=sc(10)**2+sc(11)**2+sc(12)**2                               !/*xb4c2     245*/
      if(n.eq.10)b2=sc(10)**2                                                   !/*xb4c2     246*/
      h2=sc(4)**2+sc(5)**2+sc(6)**2                                             !/*xb4c2     247*/
      a=sqrt(a2)                                                                !/*xb4c2     248*/
      b=sqrt(b2)                                                                !/*xb4c2     249*/
      h=sqrt(h2)                                                                !/*xb4c2     250*/
!        obtain r2 vector as cross product in alternate format.                 !/*xb4c2     251*/
      if(n.eq.10)call crspro(sc(4),sc(7),sc(10))                                !/*xb4c2     252*/
      if(n.eq.10)bq=sqrt(sc(10)**2+sc(11)**2+sc(12)**2)                         !/*xb4c2     253*/
!        first normalize.                                                       !/*xb4c2     254*/
      do 240 kk=1,3                                                             !/*xb4c2     255*/
      sc(3+kk)=sc(3+kk)/h                                                       !/*xb4c2     256*/
      sc(6+kk)=sc(6+kk)/a                                                       !/*xb4c2     257*/
      if(n.eq.12)sc(9+kk)=sc(9+kk)/b                                            !/*xb4c2     258*/
  240 if(n.eq.10)sc(9+kk)=sc(9+kk)/bq                                           !/*xb4c2     259*/
!                                                                               !/*xb4c2     260*/
!        check for orthogonal vectors.                                          !/*xb4c2     261*/
      call mbodyo(1,4,7,0,0,ab,ac,sc)                                           !/*xb4c2     262*/
      if(abs(ab).gt..01)go to 260                                               !/*xb4c2     263*/
  250 call mbodyo(2,4,7,25,10,ab,ac,sc)                                         !/*xb4c2     264*/
  260 af=sqrt(sc(7)**2+sc(8)**2+sc(9)**2)                                       !/*xb4c2     265*/
      bf=sqrt(sc(10)**2+sc(11)**2+sc(12)**2)                                    !/*xb4c2     266*/
      hf=sqrt(sc(4)**2+sc(5)**2+sc(6)**2)                                       !/*xb4c2     267*/
!        now restore lengths.                                                   !/*xb4c2     268*/
      do 270 kk=1,3                                                             !/*xb4c2     269*/
      sc(3+kk)=sc(3+kk)*h/hf                                                    !/*xb4c2     270*/
      sc(6+kk)=sc(6+kk)*a/af                                                    !/*xb4c2     271*/
  270 sc(9+kk)=sc(9+kk)*b/bf                                                    !/*xb4c2     272*/
      ij=0                                                                      !/*xb4c2     273*/
      if(sc(5).eq.0..and.sc(6).eq.0.)ij=1                                       !/*xb4c2     274*/
      if(sc(4).eq.0..and.sc(6).eq.0.)ij=2                                       !/*xb4c2     275*/
      if(sc(4).eq.0..and.sc(5).eq.0.)ij=3                                       !/*xb4c2     276*/
      if(ij.eq.0)r=sc(1)**2+sc(2)**2+sc(3)**2                                   !/*xb4c2     277*/
      if(ij.eq.1)r=sc(2)**2+sc(3)**2                                            !/*xb4c2     278*/
      if(ij.eq.2)r=sc(1)**2+sc(3)**2                                            !/*xb4c2     279*/
      if(ij.eq.3)r=sc(1)**2+sc(2)**2                                            !/*xb4c2     280*/
      do 300 j=1,jn                                                             !/*xb4c2     281*/
      jtr(ljtr+mxj+j)=jtr(ljtr+mxj)                                             !/*xb4c2     282*/
      ksu(lksu+mxj+j)=ksu(lksu+mxj)                                             !/*xb4c2     283*/
      ksm(lksm+mxj+j)=mxj                                                       !/*xb4c2     284*/
      nsfm(lnsf+mxj+j)=0                                                        !/*xb4c2     285*/
      kst(lkst+mxj+j)=1                                                         !/*xb4c2     286*/
      lsc(llsc+mxj+j)=iy                                                        !/*xb4c2     287*/
      if(j.ne.1)go to 290                                                       !/*xb4c2     288*/
!                                                                               !/*xb4c2     289*/
!        set up ellipse as special surface or gq.                               !/*xb4c2     290*/
!        sq must also have major axis along axis.                               !/*xb4c2     291*/
      if(ij.eq.0)go to 280                                                      !/*xb4c2     292*/
      if(sc(7).ne.0..and.sc(8).ne.0.)go to 280                                  !/*xb4c2     293*/
      if(sc(7).ne.0..and.sc(9).ne.0.)go to 280                                  !/*xb4c2     294*/
      if(sc(8).ne.0..and.sc(9).ne.0.)go to 280                                  !/*xb4c2     295*/
      kst(lkst+mxj+j)=22                                                        !/*xb4c2     296*/
      scf(iy+1)=a2*(1.-sc(4)**2/h2)-sc(7)**2*(1.-(b2/a2))                       !/*xb4c2     297*/
      scf(iy+2)=a2*(1.-sc(5)**2/h2)-sc(8)**2*(1.-(b2/a2))                       !/*xb4c2     298*/
      scf(iy+3)=a2*(1.-sc(6)**2/h2)-sc(9)**2*(1.-(b2/a2))                       !/*xb4c2     299*/
      if(ij.eq.1)scf(iy+1)=0.                                                   !/*xb4c2     300*/
      if(ij.eq.2)scf(iy+2)=0.                                                   !/*xb4c2     301*/
      if(ij.eq.3)scf(iy+3)=0.                                                   !/*xb4c2     302*/
      scf(iy+4)=0.                                                              !/*xb4c2     303*/
      scf(iy+5)=0.                                                              !/*xb4c2     304*/
      scf(iy+6)=0.                                                              !/*xb4c2     305*/
      if(ij.eq.1)scf(iy+7)=a2*r-(sc(2)**2*sc(8)**2+sc(3)**2*sc(9)**2)*          !/*xb4c2     306*/
     & (1.-(b2/a2))-a2*b2-scf(iy+2)*sc(2)**2-scf(iy+3)*sc(3)**2                 !/*xb4c2     307*/
      if(ij.eq.2)scf(iy+7)=a2*r-(sc(1)**2*sc(7)**2+sc(3)**2*sc(9)**2)*          !/*xb4c2     308*/
     & (1.-(b2/a2))-a2*b2-scf(iy+1)*sc(1)**2-scf(iy+3)*sc(3)**2                 !/*xb4c2     309*/
      if(ij.eq.3)scf(iy+7)=a2*r-(sc(1)**2*sc(7)**2+sc(2)**2*sc(8)**2)*          !/*xb4c2     310*/
     & (1.-(b2/a2))-a2*b2-scf(iy+1)*sc(1)**2-scf(iy+2)*sc(2)**2                 !/*xb4c2     311*/
      scf(iy+8)=sc(1)                                                           !/*xb4c2     312*/
      scf(iy+9)=sc(2)                                                           !/*xb4c2     313*/
      scf(iy+10)=sc(3)                                                          !/*xb4c2     314*/
      iy=iy+10                                                                  !/*xb4c2     315*/
      go to 300                                                                 !/*xb4c2     316*/
!        gq surface.                                                            !/*xb4c2     317*/
  280 kst(lkst+mxj+j)=23                                                        !/*xb4c2     318*/
      scf(iy+1)=a2*(1.-sc(4)**2/h2)-sc(7)**2*(1.-(b2/a2))                       !/*xb4c2     319*/
      scf(iy+2)=a2*(1.-sc(5)**2/h2)-sc(8)**2*(1.-(b2/a2))                       !/*xb4c2     320*/
      scf(iy+3)=a2*(1.-sc(6)**2/h2)-sc(9)**2*(1.-(b2/a2))                       !/*xb4c2     321*/
      scf(iy+4)=-2.*a2*sc(4)*sc(5)/h2 -2.*sc(7)*sc(8)*(1.-(b2/a2))              !/*xb4c2     322*/
      scf(iy+5)=-2.*a2*sc(5)*sc(6)/h2 -2.*sc(8)*sc(9)*(1.-(b2/a2))              !/*xb4c2     323*/
      scf(iy+6)=-2.*a2*sc(4)*sc(6)/h2 -2.*sc(7)*sc(9)*(1.-(b2/a2))              !/*xb4c2     324*/
      scf(iy+7)=a2*2.*((sc(4)/h2)*(sc(2)*sc(5)+sc(3)*sc(6))-                    !/*xb4c2     325*/
     & sc(1)*((1.-(sc(4)**2/h2))-((sc(7)**2/a2)*(1.-(b2/a2)))))                 !/*xb4c2     326*/
     & +2.*(1.-(b2/a2))*(sc(2)*sc(7)*sc(8)+sc(3)*sc(7)*sc(9))                   !/*xb4c2     327*/
      scf(iy+8)=a2*2.*((sc(5)/h2)*(sc(1)*sc(4)+sc(3)*sc(6)) -                   !/*xb4c2     328*/
     & sc(2)*((1.-(sc(5)**2/h2))-((sc(8)**2/a2)*(1.-(b2/a2)))))                 !/*xb4c2     329*/
     & +2.*(1.-(b2/a2))*(sc(1)*sc(7)*sc(8)+sc(3)*sc(8)*sc(9))                   !/*xb4c2     330*/
      scf(iy+9)=a2*2.*((sc(6)/h2)*(sc(1)*sc(4)+sc(2)*sc(5)) -                   !/*xb4c2     331*/
     & sc(3)*((1.-(sc(6)**2/h2))-((sc(9)**2/a2)*(1.-(b2/a2)))))                 !/*xb4c2     332*/
     & +2.*(1.-(b2/a2))*(sc(1)*sc(7)*sc(9)+sc(2)*sc(8)*sc(9))                   !/*xb4c2     333*/
      scf(iy+10)=a2*((-2./h2)*(sc(1)*sc(2)*sc(4)*sc(5)+sc(2)*sc(3)*             !/*xb4c2     334*/
     & sc(5)*sc(6)+sc(1)*sc(3)*sc(4)*sc(6))+sc(1)**2*(1.-sc(4)**2/h2)+          !/*xb4c2     335*/
     & sc(2)**2*(1.-sc(5)**2/h2)+sc(3)**2*(1.-sc(6)**2/h2))-                    !/*xb4c2     336*/
     & (sc(1)**2*sc(7)**2+sc(2)**2*sc(8)**2+sc(3)**2*sc(9)**2)*                 !/*xb4c2     337*/
     & (1.-b2/a2)-a2*b2                                                         !/*xb4c2     338*/
     & -2.*(sc(1)*sc(2)*sc(7)*sc(8)+sc(2)*sc(3)*sc(8)*sc(9)+                    !/*xb4c2     339*/
     & sc(1)*sc(3)*sc(7)*sc(9))*(1.-b2/a2)                                      !/*xb4c2     340*/
      iy=iy+10                                                                  !/*xb4c2     341*/
      go to 300                                                                 !/*xb4c2     342*/
  290 call mbodyp(1,ii,ij,ik,iy,j,jj,jn,ia,x,h,sc)                              !/*xb4c2     343*/
  300 continue                                                                  !/*xb4c2     344*/
      go to 700                                                                 !/*xb4c2     345*/
!                                                                               !/*xb4c2     346*/
! >>>>>  ell: set coefficients for surface as sq or gq surface.                 !/*xb4c2     347*/
  310 if(sc(7).eq.0.)call erprnt(2,1,0,0,0,0,0,0,                               !/*xb4c2     348*/
     & '23hinvalid radius for ell.')                                            !/*xb4c2     349*/
      if(sc(7).ge.0.)go to 320                                                  !/*xb4c2     350*/
!        using alternate format with center of ellipse, major axis              !/*xb4c2     351*/
!        radius vector, and minor radius (as negative).                         !/*xb4c2     352*/
      b2=sc(7)**2                                                               !/*xb4c2     353*/
      a2=sc(4)**2+sc(5)**2+sc(6)**2                                             !/*xb4c2     354*/
      if(a2.eq.0.)call erprnt(2,1,0,0,0,0,0,0,                                  !/*xb4c2     355*/
     & '23hinvalid radius for ell.')                                            !/*xb4c2     356*/
      a=sqrt(a2)                                                                !/*xb4c2     357*/
      u=sc(4)/a                                                                 !/*xb4c2     358*/
      v=sc(5)/a                                                                 !/*xb4c2     359*/
      w=sc(6)/a                                                                 !/*xb4c2     360*/
      x=sc(1)                                                                   !/*xb4c2     361*/
      y=sc(2)                                                                   !/*xb4c2     362*/
      z=sc(3)                                                                   !/*xb4c2     363*/
      go to 330                                                                 !/*xb4c2     364*/
!        using standard format for ell.                                         !/*xb4c2     365*/
  320 a2=sc(7)**2                                                               !/*xb4c2     366*/
      c=(sc(4)-sc(1))**2+(sc(5)-sc(2))**2+(sc(6)-sc(3))**2                      !/*xb4c2     367*/
      if(c.le.0.)call erprnt(2,1,0,0,0,0,0,0,'21hinvalid foci for ell.')        !/*xb4c2     368*/
      c=sqrt(c)                                                                 !/*xb4c2     369*/
      u=(sc(4)-sc(1))/c                                                         !/*xb4c2     370*/
      v=(sc(5)-sc(2))/c                                                         !/*xb4c2     371*/
      w=(sc(6)-sc(3))/c                                                         !/*xb4c2     372*/
      c=c*0.5                                                                   !/*xb4c2     373*/
      b2=2.*sc(7)*c-c**2                                                        !/*xb4c2     374*/
      if(b2.le.0.)call erprnt(2,1,0,0,0,0,0,0,                                  !/*xb4c2     375*/
     & '21hinvalid foci for ell.')                                              !/*xb4c2     376*/
      x=.5*(sc(1)+sc(4))                                                        !/*xb4c2     377*/
      y=.5*(sc(2)+sc(5))                                                        !/*xb4c2     378*/
      z=.5*(sc(3)+sc(6))                                                        !/*xb4c2     379*/
!        set up ellipse as sq or gq surface.                                    !/*xb4c2     380*/
  330 am=-a2+b2                                                                 !/*xb4c2     381*/
      du=a2+u**2*am                                                             !/*xb4c2     382*/
      dv=a2+v**2*am                                                             !/*xb4c2     383*/
      dw=a2+w**2*am                                                             !/*xb4c2     384*/
      ksm(lksm+mxj)=0                                                           !/*xb4c2     385*/
      if((u.ne.0..and.v.ne.0.).or.(u.ne.0..and.w.ne.0.).or.                     !/*xb4c2     386*/
     & (v.ne.0..and.w.ne.0.))go to 340                                          !/*xb4c2     387*/
!        sq.                                                                    !/*xb4c2     388*/
      kst(lkst+mxj)=22                                                          !/*xb4c2     389*/
      scf(iy+1)=du                                                              !/*xb4c2     390*/
      scf(iy+2)=dv                                                              !/*xb4c2     391*/
      scf(iy+3)=dw                                                              !/*xb4c2     392*/
      scf(iy+4)=0.                                                              !/*xb4c2     393*/
      scf(iy+5)=0.                                                              !/*xb4c2     394*/
      scf(iy+6)=0.                                                              !/*xb4c2     395*/
      scf(iy+7)=-a2*b2                                                          !/*xb4c2     396*/
      scf(iy+8)=x                                                               !/*xb4c2     397*/
      scf(iy+9)=y                                                               !/*xb4c2     398*/
      scf(iy+10)=z                                                              !/*xb4c2     399*/
      go to 350                                                                 !/*xb4c2     400*/
  340 kst(lkst+mxj)=23                                                          !/*xb4c2     401*/
!        gq.                                                                    !/*xb4c2     402*/
      scf(iy+1)=du                                                              !/*xb4c2     403*/
      scf(iy+2)=dv                                                              !/*xb4c2     404*/
      scf(iy+3)=dw                                                              !/*xb4c2     405*/
      scf(iy+4)=2.*u*v*am                                                       !/*xb4c2     406*/
      scf(iy+5)=2.*v*w*am                                                       !/*xb4c2     407*/
      scf(iy+6)=2.*u*w*am                                                       !/*xb4c2     408*/
      scf(iy+7)=-2.*x*du-2.*u*v*y*am-2.*u*w*z*am                                !/*xb4c2     409*/
      scf(iy+8)=-2.*y*dv-2.*u*v*x*am-2.*v*w*z*am                                !/*xb4c2     410*/
      scf(iy+9)=-2.*z*dw-2.*u*w*x*am-2.*v*w*y*am                                !/*xb4c2     411*/
      scf(iy+10)=x**2*du+y**2*dv+z**2*dw+x*y*2.*u*v*am+y*z*2.*v*w*am+           !/*xb4c2     412*/
     & x*z*2.*u*w*am-a2*b2                                                      !/*xb4c2     413*/
  350 iy=iy+10                                                                  !/*xb4c2     414*/
      go to 700                                                                 !/*xb4c2     415*/
!                                                                               !/*xb4c2     416*/
! >>>>>  trc: set coefficients for each j surface.                              !/*xb4c2     417*/
  360 x=-1.                                                                     !/*xb4c2     418*/
      jj=1                                                                      !/*xb4c2     419*/
      if((sc(4).eq.0..and.sc(5).eq.0..and.sc(6).eq.0.).or.                      !/*xb4c2     420*/
     & (sc(7).eq.0..and.sc(8).eq.0.).or.sc(7).lt.0..or.sc(8).lt.0.)             !/*xb4c2     421*/
     & call erprnt(2,1,0,0,0,0,0,0,'26hinvalid dimension for trc.')             !/*xb4c2     422*/
      ij=0                                                                      !/*xb4c2     423*/
      if(sc(5).eq.0..and.sc(6).eq.0.)ij=1                                       !/*xb4c2     424*/
      if(sc(4).eq.0..and.sc(6).eq.0.)ij=2                                       !/*xb4c2     425*/
      if(sc(4).eq.0..and.sc(5).eq.0.)ij=3                                       !/*xb4c2     426*/
      rh=sqrt(sc(4)**2+sc(5)**2+sc(6)**2)                                       !/*xb4c2     427*/
      sc(9)=sc(4)/rh                                                            !/*xb4c2     428*/
      sc(10)=sc(5)/rh                                                           !/*xb4c2     429*/
      sc(11)=sc(6)/rh                                                           !/*xb4c2     430*/
  370 if(ij.eq.1)r=sc(2)**2+sc(3)**2                                            !/*xb4c2     431*/
      if(ij.eq.2)r=sc(1)**2+sc(3)**2                                            !/*xb4c2     432*/
      if(ij.eq.3)r=sc(1)**2+sc(2)**2                                            !/*xb4c2     433*/
      if(ij.eq.0)r=sc(1)**2+sc(2)**2+sc(3)**2                                   !/*xb4c2     434*/
      do 410 j=1,jn                                                             !/*xb4c2     435*/
      jtr(ljtr+mxj+j)=jtr(ljtr+mxj)                                             !/*xb4c2     436*/
      ksu(lksu+mxj+j)=ksu(lksu+mxj)                                             !/*xb4c2     437*/
      ksm(lksm+mxj+j)=mxj                                                       !/*xb4c2     438*/
      nsfm(lnsf+mxj+j)=0                                                        !/*xb4c2     439*/
      kst(lkst+mxj+j)=1                                                         !/*xb4c2     440*/
      lsc(llsc+mxj+j)=iy                                                        !/*xb4c2     441*/
      if(j.ne.1)go to 400                                                       !/*xb4c2     442*/
!                                                                               !/*xb4c2     443*/
!        set up cone as special surface or gq.                                  !/*xb4c2     444*/
      if(ij.eq.0)go to 390                                                      !/*xb4c2     445*/
      if(r.ne.0.)go to 380                                                      !/*xb4c2     446*/
      kst(lkst+mxj+j)=18+ij                                                     !/*xb4c2     447*/
      scf(iy+2)=(sc(7)-sc(8))**2/rh**2                                          !/*xb4c2     448*/
      scf(iy+1)=sc(ij)+(sc(7)/sc(8))*sc(3+ij)/((sc(7)/sc(8))-1.)                !/*xb4c2     449*/
      scf(iy+3)=0                                                               !/*xb4c2     450*/
      iy=iy+3                                                                   !/*xb4c2     451*/
      go to 410                                                                 !/*xb4c2     452*/
  380 kst(lkst+mxj+j)=15+ij                                                     !/*xb4c2     453*/
      scf(iy+4)=(sc(7)-sc(8))**2/rh**2                                          !/*xb4c2     454*/
      scf(iy+ij)=sc(ij)+(sc(7)/sc(8))*sc(3+ij)/((sc(7)/sc(8))-1.)               !/*xb4c2     455*/
      scf(iy+5)=0                                                               !/*xb4c2     456*/
      if(ij.ne.1)scf(iy+1)=sc(1)                                                !/*xb4c2     457*/
      if(ij.ne.2)scf(iy+2)=sc(2)                                                !/*xb4c2     458*/
      if(ij.ne.3)scf(iy+3)=sc(3)                                                !/*xb4c2     459*/
      iy=iy+5                                                                   !/*xb4c2     460*/
      go to 410                                                                 !/*xb4c2     461*/
  390 kst(lkst+mxj+j)=23                                                        !/*xb4c2     462*/
      scf(iy+1)=1.-sc(9)**2                                                     !/*xb4c2     463*/
      scf(iy+2)=1.-sc(10)**2                                                    !/*xb4c2     464*/
      scf(iy+3)=1.-sc(11)**2                                                    !/*xb4c2     465*/
      scf(iy+4)=-2.*sc(9)*sc(10)                                                !/*xb4c2     466*/
      scf(iy+5)=-2.*sc(10)*sc(11)                                               !/*xb4c2     467*/
      scf(iy+6)=-2.*sc(9)*sc(11)                                                !/*xb4c2     468*/
      scf(iy+7)=-sc(2)*scf(iy+4)-sc(3)*scf(iy+6)-2.*sc(1)*scf(iy+1)             !/*xb4c2     469*/
      scf(iy+8)=-sc(1)*scf(iy+4)-sc(3)*scf(iy+5)-2.*sc(2)*scf(iy+2)             !/*xb4c2     470*/
      scf(iy+9)=-sc(1)*scf(iy+6)-sc(2)*scf(iy+5)-2.*sc(3)*scf(iy+3)             !/*xb4c2     471*/
      scf(iy+10)=sc(1)*sc(2)*scf(iy+4)+sc(2)*sc(3)*scf(iy+5)+                   !/*xb4c2     472*/
     & sc(1)*sc(3)*scf(iy+6)+sc(1)**2*scf(iy+1)+sc(2)**2*scf(iy+2)+             !/*xb4c2     473*/
     & sc(3)**2*scf(iy+3)-sc(7)**2                                              !/*xb4c2     474*/
!        add terms to change cylinder to cone.                                  !/*xb4c2     475*/
      t2=(sc(7)-sc(8))**2/rh**2                                                 !/*xb4c2     476*/
      dp=-(sc(7)/sc(8))*rh/((sc(7)/sc(8))-1.)                                   !/*xb4c2     477*/
      scf(iy+1)=scf(iy+1)-t2*sc(9)**2                                           !/*xb4c2     478*/
      scf(iy+2)=scf(iy+2)-t2*sc(10)**2                                          !/*xb4c2     479*/
      scf(iy+3)=scf(iy+3)-t2*sc(11)**2                                          !/*xb4c2     480*/
      scf(iy+7)=scf(iy+7)-t2*                                                   !/*xb4c2     481*/
     &(sc(2)*scf(iy+4)+sc(3)*scf(iy+6)-2.*sc(1)*sc(9)**2+2.*dp*sc(9))           !/*xb4c2     482*/
      scf(iy+8)=scf(iy+8)-t2*                                                   !/*xb4c2     483*/
     &(sc(1)*scf(iy+4)+sc(3)*scf(iy+5)-2.*sc(2)*sc(10)**2+2.*dp*sc(10))         !/*xb4c2     484*/
      scf(iy+9)=scf(iy+9)-t2*                                                   !/*xb4c2     485*/
     &(sc(1)*scf(iy+6)+sc(2)*scf(iy+5)-2.*sc(3)*sc(11)**2+2.*dp*sc(11))         !/*xb4c2     486*/
      scf(iy+10)=scf(iy+10)+sc(7)**2-t2*                                        !/*xb4c2     487*/
     & (-sc(1)*sc(2)*scf(iy+4)-sc(2)*sc(3)*scf(iy+5)-                           !/*xb4c2     488*/
     & sc(1)*sc(3)*scf(iy+6)+sc(1)**2*sc(9)**2+sc(2)**2*sc(10)**2+              !/*xb4c2     489*/
     & sc(3)**2*sc(11)**2+dp**2                                                 !/*xb4c2     490*/
     & -2.*dp*(sc(1)*sc(9)+sc(2)*sc(10)+sc(3)*sc(11)))                          !/*xb4c2     491*/
      scf(iy+4)=scf(iy+4)-2.*t2*sc(9)*sc(10)                                    !/*xb4c2     492*/
      scf(iy+5)=scf(iy+5)-2.*t2*sc(10)*sc(11)                                   !/*xb4c2     493*/
      scf(iy+6)=scf(iy+6)-2.*t2*sc(9)*sc(11)                                    !/*xb4c2     494*/
      iy=iy+10                                                                  !/*xb4c2     495*/
      go to 410                                                                 !/*xb4c2     496*/
  400 call mbodyp(2,ii,ij,ik,iy,j,jj,jn,ia,x,h,sc)                              !/*xb4c2     497*/
  410 continue                                                                  !/*xb4c2     498*/
      go to 700                                                                 !/*xb4c2     499*/
!                                                                               !/*xb4c2     500*/
! >>>>>  wed: set coefficients for each j surface.                              !/*xb4c2     501*/
  420 if(n.ne.12)call erprnt(2,1,0,0,0,0,0,0,                                   !/*xb4c2     502*/
     & '26hinvalid dimension for wed.')                                         !/*xb4c2     503*/
      do 440 k=1,3                                                              !/*xb4c2     504*/
      x=sc(3+3*(k-1)+1)                                                         !/*xb4c2     505*/
      y=sc(3+3*(k-1)+2)                                                         !/*xb4c2     506*/
      z=sc(3+3*(k-1)+3)                                                         !/*xb4c2     507*/
      r=x**2+y**2+z**2                                                          !/*xb4c2     508*/
      if(r.eq.0.)call erprnt(2,1,0,0,0,0,0,0,                                   !/*xb4c2     509*/
     & '23hzero dimension for wed.')                                            !/*xb4c2     510*/
      sc(21+k)=0.                                                               !/*xb4c2     511*/
      if(r.eq.0.)go to 440                                                      !/*xb4c2     512*/
      r=sqrt(r)                                                                 !/*xb4c2     513*/
      sc(13+3*(k-1))=x/r                                                        !/*xb4c2     514*/
      sc(14+3*(k-1))=y/r                                                        !/*xb4c2     515*/
      sc(15+3*(k-1))=z/r                                                        !/*xb4c2     516*/
      sc(21+k)=r                                                                !/*xb4c2     517*/
      if(k.eq.1)go to 440                                                       !/*xb4c2     518*/
      if(k.eq.3)go to 430                                                       !/*xb4c2     519*/
!                                                                               !/*xb4c2     520*/
!        check for orthogonal vectors.                                          !/*xb4c2     521*/
      call mbodyo(1,13,16,0,0,ab,ac,sc)                                         !/*xb4c2     522*/
      go to 440                                                                 !/*xb4c2     523*/
  430 call mbodyo(2,13,16,25,19,ab,ac,sc)                                       !/*xb4c2     524*/
  440 continue                                                                  !/*xb4c2     525*/
!        find vector to slanted plane.                                          !/*xb4c2     526*/
      d=0                                                                       !/*xb4c2     527*/
      do 450 k=1,3                                                              !/*xb4c2     528*/
      sc(30+k)=sc(12+k)*sc(23)+sc(15+k)*sc(22)                                  !/*xb4c2     529*/
  450 d=d+sc(30+k)**2                                                           !/*xb4c2     530*/
      d=sqrt(d)                                                                 !/*xb4c2     531*/
      do 460 k=1,3                                                              !/*xb4c2     532*/
  460 sc(30+k)=sc(30+k)/d                                                       !/*xb4c2     533*/
      d=sc(22)*sc(23)/sqrt(sc(22)**2+sc(23)**2)                                 !/*xb4c2     534*/
      x=-1.                                                                     !/*xb4c2     535*/
      do 500 j=1,jn                                                             !/*xb4c2     536*/
      jtr(ljtr+mxj+j)=jtr(ljtr+mxj)                                             !/*xb4c2     537*/
      ksu(lksu+mxj+j)=ksu(lksu+mxj)                                             !/*xb4c2     538*/
      ksm(lksm+mxj+j)=mxj                                                       !/*xb4c2     539*/
      nsfm(lnsf+mxj+j)=0                                                        !/*xb4c2     540*/
      lsc(llsc+mxj+j)=iy                                                        !/*xb4c2     541*/
      kst(lkst+mxj+j)=1                                                         !/*xb4c2     542*/
      jj=(j+1)/2                                                                !/*xb4c2     543*/
      if(j.gt.3)jj=(j+2)/2                                                      !/*xb4c2     544*/
      x=-x                                                                      !/*xb4c2     545*/
      if(x.lt.0.)go to 470                                                      !/*xb4c2     546*/
      ij=0                                                                      !/*xb4c2     547*/
      if(sc(14+(jj-1)*3).eq.0..and.sc(15+(jj-1)*3).eq.0.)ij=1                   !/*xb4c2     548*/
      if(sc(13+(jj-1)*3).eq.0..and.sc(15+(jj-1)*3).eq.0.)ij=2                   !/*xb4c2     549*/
      if(sc(13+(jj-1)*3).eq.0..and.sc(14+(jj-1)*3).eq.0.)ij=3                   !/*xb4c2     550*/
  470 if(j.eq.1)go to 480                                                       !/*xb4c2     551*/
      call mbodyp(4,ii,ij,ik,iy,j,jj,jn,ia,x,h,sc)                              !/*xb4c2     552*/
      go to 500                                                                 !/*xb4c2     553*/
!        do slanted plane.                                                      !/*xb4c2     554*/
  480 do 490 k=1,4                                                              !/*xb4c2     555*/
      scf(iy+k)=sc(30+k)                                                        !/*xb4c2     556*/
      if(k.ne.4)go to 490                                                       !/*xb4c2     557*/
      scf(iy+k)=scf(iy+1)*sc(1)+scf(iy+2)*sc(2)+scf(iy+3)*sc(3)                 !/*xb4c2     558*/
      scf(iy+k)=scf(iy+k)+(scf(iy+1)*sc(31)+                                    !/*xb4c2     559*/
     & scf(iy+2)*sc(32)+scf(iy+3)*sc(33))*d                                     !/*xb4c2     560*/
  490 continue                                                                  !/*xb4c2     561*/
      iy=iy+4                                                                   !/*xb4c2     562*/
  500 continue                                                                  !/*xb4c2     563*/
      go to 700                                                                 !/*xb4c2     564*/
!                                                                               !/*xb4c2     565*/
! >>>>>  arb: set coefficients for each j surface.                              !/*xb4c2     566*/
  510 if(n.lt.28.or.n.gt.30)call erprnt(2,1,0,0,0,0,0,0,                        !/*xb4c2     567*/
     & '31hinvalid number entries for arb.')                                    !/*xb4c2     568*/
      nc=0                                                                      !/*xb4c2     569*/
      im=0                                                                      !/*xb4c2     570*/
      do 520 ii=25,n                                                            !/*xb4c2     571*/
      i=sc(ii)                                                                  !/*xb4c2     572*/
      if(i.lt.0.or.(i.eq.0.and.ii.eq.25))                                       !/*xb4c2     573*/
     & call erprnt(2,1,0,0,0,0,0,0,                                             !/*xb4c2     574*/
     & '40hinvalid four digit face entries for arb.')                           !/*xb4c2     575*/
      if(i.le.0)go to 520                                                       !/*xb4c2     576*/
      if(i.lt.1230.or.i.gt.8765)call erprnt(2,1,0,0,0,0,0,0,                    !/*xb4c2     577*/
     & '40hinvalid four digit face entries for arb.')                           !/*xb4c2     578*/
      if(i.lt.1230.or.i.gt.8765)go to 520                                       !/*xb4c2     579*/
      nc=nc+1                                                                   !/*xb4c2     580*/
      i1=i/1000                                                                 !/*xb4c2     581*/
      if(i1.gt.8)call erprnt(2,1,0,0,0,0,0,0,                                   !/*xb4c2     582*/
     & '40hinvalid four digit face entries for arb.')                           !/*xb4c2     583*/
      mf(nc,1)=i1                                                               !/*xb4c2     584*/
      if(i1.gt.im)im=i1                                                         !/*xb4c2     585*/
      i2=i-i1*1000                                                              !/*xb4c2     586*/
      i2=i2/100                                                                 !/*xb4c2     587*/
      if(i2.gt.8.or.i2.lt.1.or.i2.eq.i1)call erprnt(2,1,0,0,0,0,0,0,            !/*xb4c2     588*/
     & '40hinvalid four digit face entries for arb.')                           !/*xb4c2     589*/
      mf(nc,2)=i2                                                               !/*xb4c2     590*/
      if(i2.gt.im)im=i2                                                         !/*xb4c2     591*/
      i3=i-i1*1000-i2*100                                                       !/*xb4c2     592*/
      i3=i3/10                                                                  !/*xb4c2     593*/
      if(i3.gt.8.or.i3.lt.1.or.i3.eq.i2.or.i3.eq.i1)                            !/*xb4c2     594*/
     & call erprnt(2,1,0,0,0,0,0,0,                                             !/*xb4c2     595*/
     & '40hinvalid four digit face entries for arb.')                           !/*xb4c2     596*/
      mf(nc,3)=i3                                                               !/*xb4c2     597*/
      mf(nc,4)=0                                                                !/*xb4c2     598*/
      mf(nc,5)=3                                                                !/*xb4c2     599*/
      if(i3.gt.im)im=i3                                                         !/*xb4c2     600*/
      i4=i-i1*1000-i2*100-i3*10                                                 !/*xb4c2     601*/
      if(i4.eq.0)go to 520                                                      !/*xb4c2     602*/
      if(i4.gt.8.or.i4.eq.i3.or.i4.eq.i2.or.i4.eq.i1)                           !/*xb4c2     603*/
     & call erprnt(2,1,0,0,0,0,0,0,                                             !/*xb4c2     604*/
     & '40hinvalid four digit face entries for arb.')                           !/*xb4c2     605*/
      mf(nc,4)=i4                                                               !/*xb4c2     606*/
      if(i4.gt.im)im=i4                                                         !/*xb4c2     607*/
      mf(nc,5)=4                                                                !/*xb4c2     608*/
  520 continue                                                                  !/*xb4c2     609*/
      if(nc.ge.4.and.nc.le.6)call erprnt(2,2,1,nc,0,0,0,0,                      !/*xb4c2     610*/
     & '33harb has specified number faces = ,i3,1h.')                           !/*xb4c2     611*/
      if(nc.lt.4.or.nc.gt.6)call erprnt(2,1,1,nc,0,0,0,0,                       !/*xb4c2     612*/
     & '33harb has specified number faces = ,i3,1h.')                           !/*xb4c2     613*/
      jn=nc                                                                     !/*xb4c2     614*/
!        find point inside to use for sense.                                    !/*xb4c2     615*/
      nt=0                                                                      !/*xb4c2     616*/
      do 550 i=1,nc                                                             !/*xb4c2     617*/
      nt=nt+mf(i,5)                                                             !/*xb4c2     618*/
      do 540 kk=1,4                                                             !/*xb4c2     619*/
      if(kk.gt.mf(i,5))go to 540                                                !/*xb4c2     620*/
      j=mf(i,kk)                                                                !/*xb4c2     621*/
      do 530 k=1,3                                                              !/*xb4c2     622*/
      if(i.eq.1.and.kk.eq.1)tpp(13+k)=0.                                        !/*xb4c2     623*/
  530 tpp(13+k)=tpp(13+k)+sc(k+(j-1)*3)                                         !/*xb4c2     624*/
  540 continue                                                                  !/*xb4c2     625*/
  550 continue                                                                  !/*xb4c2     626*/
      do 560 k=1,3                                                              !/*xb4c2     627*/
  560 tpp(13+k)=tpp(13+k)/nt                                                    !/*xb4c2     628*/
!        increment ksm by 1000*number faces.                                    !/*xb4c2     629*/
      ksm(lksm+mxj)=ksm(lksm+mxj)-1000*jn                                       !/*xb4c2     630*/
!        implement each plane surface of arb.                                   !/*xb4c2     631*/
      ie=0                                                                      !/*xb4c2     632*/
      do 660 j=1,jn                                                             !/*xb4c2     633*/
      jtr(ljtr+mxj+j)=jtr(ljtr+mxj)                                             !/*xb4c2     634*/
      ksu(lksu+mxj+j)=ksu(lksu+mxj)                                             !/*xb4c2     635*/
      ksm(lksm+mxj+j)=mxj                                                       !/*xb4c2     636*/
      nsfm(lnsf+mxj+j)=0                                                        !/*xb4c2     637*/
      lsc(llsc+mxj+j)=iy                                                        !/*xb4c2     638*/
      kst(lkst+mxj+j)=1                                                         !/*xb4c2     639*/
!          prepare to do cross product to obtain surface normal                 !/*xb4c2     640*/
      j1=mf(j,1)                                                                !/*xb4c2     641*/
      j2=mf(j,2)                                                                !/*xb4c2     642*/
      j3=mf(j,3)                                                                !/*xb4c2     643*/
      j4=mf(j,4)                                                                !/*xb4c2     644*/
      do 570 k=1,3                                                              !/*xb4c2     645*/
      tpp(k)=sc(k+(j2-1)*3)-sc(k+(j1-1)*3)                                      !/*xb4c2     646*/
  570 tpp(k+3)=sc(k+(j3-1)*3)-sc(k+(j2-1)*3)                                    !/*xb4c2     647*/
      call crspro(tpp(1),tpp(4),tpp(10))                                        !/*xb4c2     648*/
      s=0.                                                                      !/*xb4c2     649*/
      do 580 k=1,3                                                              !/*xb4c2     650*/
  580 s=s+tpp(9+k)**2                                                           !/*xb4c2     651*/
      if(s.eq.0.)call erprnt(2,1,0,0,0,0,0,0,                                   !/*xb4c2     652*/
     & '31hentries for arb not consistent.')                                    !/*xb4c2     653*/
      s=sqrt(s)                                                                 !/*xb4c2     654*/
!        a,b,c coefficients are the normal to plane.                            !/*xb4c2     655*/
!        obtain d coefficient from plane eq. at first point.                    !/*xb4c2     656*/
      scf(iy+4)=0.                                                              !/*xb4c2     657*/
      do 590 k=1,3                                                              !/*xb4c2     658*/
      tpp(6+k)=tpp(9+k)/s                                                       !/*xb4c2     659*/
      scf(iy+k)=tpp(6+k)                                                        !/*xb4c2     660*/
  590 scf(iy+4)=scf(iy+4)+scf(iy+k)*sc(k+(j1-1)*3)                              !/*xb4c2     661*/
!        check sense and switch signs if necessary.                             !/*xb4c2     662*/
      ss=-scf(iy+4)                                                             !/*xb4c2     663*/
      do 600 k=1,3                                                              !/*xb4c2     664*/
  600 ss=ss+scf(iy+k)*tpp(13+k)                                                 !/*xb4c2     665*/
      if(ss.lt.0.)go to 620                                                     !/*xb4c2     666*/
      do 610 k=1,4                                                              !/*xb4c2     667*/
  610 scf(iy+k)=-scf(iy+k)                                                      !/*xb4c2     668*/
!        if 4th point, determine how far from plane.                            !/*xb4c2     669*/
  620 if(j4.eq.0)go to 660                                                      !/*xb4c2     670*/
      ds=scf(iy+4)                                                              !/*xb4c2     671*/
      do 630 k=1,3                                                              !/*xb4c2     672*/
  630 ds=ds-scf(iy+k)*sc(k+(j4-1)*3)                                            !/*xb4c2     673*/
      if(abs(ds).lt.1.e-6)go to 660                                             !/*xb4c2     674*/
      ie=ie+1                                                                   !/*xb4c2     675*/
      if(ie.eq.1)call erprnt(2,2,0,0,0,0,0,0,                                   !/*xb4c2     676*/
     & '48hfourth point more than 1.e-6 from plane for arb.')                   !/*xb4c2     677*/
      if(ie.eq.1)write(iuo,640)                                                 !/*xb4c2     678*/
  640 format(5x,46h corner   face   distance    new xyz on plane  )             !/*xb4c2     679*/
      x=sc(1+(j4-1)*3)+scf(iy+1)*ds                                             !/*xb4c2     680*/
      y=sc(2+(j4-1)*3)+scf(iy+2)*ds                                             !/*xb4c2     681*/
      z=sc(3+(j4-1)*3)+scf(iy+3)*ds                                             !/*xb4c2     682*/
      write(iuo,650)j4,j,ds,x,y,z                                               !/*xb4c2     683*/
  650 format(i10,i9,e11.3,3e15.7)                                               !/*xb4c2     684*/
  660 iy=iy+4                                                                   !/*xb4c2     685*/
!        check for each surface of arb that mid-point of surface                !/*xb4c2     686*/
!        based on 3 corners has negative sense with respect to                  !/*xb4c2     687*/
!        each of the other surfaces.                                            !/*xb4c2     688*/
      do 690 j=1,jn                                                             !/*xb4c2     689*/
      x=0                                                                       !/*xb4c2     690*/
      y=0                                                                       !/*xb4c2     691*/
      z=0                                                                       !/*xb4c2     692*/
      do 670 k=1,3                                                              !/*xb4c2     693*/
      jk=mf(j,k)                                                                !/*xb4c2     694*/
      x=x+third*sc(1+(jk-1)*3)                                                  !/*xb4c2     695*/
      y=y+third*sc(2+(jk-1)*3)                                                  !/*xb4c2     696*/
  670 z=z+third*sc(3+(jk-1)*3)                                                  !/*xb4c2     697*/
      do 680 jj=1,jn                                                            !/*xb4c2     698*/
      if(jj.eq.j)go to 680                                                      !/*xb4c2     699*/
      iq=iy-(jn-jj+1)*4                                                         !/*xb4c2     700*/
      ss=-scf(iq+4)                                                             !/*xb4c2     701*/
      ss=ss+scf(iq+1)*x                                                         !/*xb4c2     702*/
      ss=ss+scf(iq+2)*y                                                         !/*xb4c2     703*/
      ss=ss+scf(iq+3)*z                                                         !/*xb4c2     704*/
      if(ss.lt.0.)go to 680                                                     !/*xb4c2     705*/
      call erprnt(2,1,1,j,0,0,0,0,                                              !/*xb4c2     706*/
     & '23hmidpoint of arb surface,i3,20h has positive sense.')                 !/*xb4c2     707*/
      go to 690                                                                 !/*xb4c2     708*/
  680 continue                                                                  !/*xb4c2     709*/
  690 continue                                                                  !/*xb4c2     710*/
      go to 700                                                                 !/*xb4c2     711*/
!                                                                               !/*xb4c2     712*/
!        finish macrocell.                                                      !/*xb4c2     713*/
  700 if(jn.ne.0)jtr(ljtr+mxj)=0                                                !/*xb4c2     714*/
      mxj=mxj+jn                                                                !/*xb4c2     715*/
      lsc(llsc+mxj+1)=iy                                                        !/*xb4c2     716*/
      return                                                                    !/*xb4c2     717*/
!                                                                               !/*xb4c2     718*/
!**************************** called from igeom ************************        !/*xb4c2     719*/
!                                                                      *        !/*xb4c2     720*/
!        expand any macrobody surfaces on cell cards to full surfaces.          !/*xb4c2     721*/
  710 do 800 ic=1,mxa                                                           !/*xb4c2     722*/
      m1=abs(lca(llca+ic))                                                      !/*xb4c2     723*/
  720 m2=abs(lca(llca+ic+1))-1                                                  !/*xb4c2     724*/
      do 740 la=m1,m2                                                           !/*xb4c2     725*/
      if(lja(llja+la).gt.1000000)go to 740                                      !/*xb4c2     726*/
      if(la.gt.m1.and.lja(llja+la-1).eq.1000004)go to 740                       !/*xb4c2     727*/
      if(idna(lida+la).lt.0)go to 740                                           !/*xb4c2     728*/
      jb=namchg(2,abs(lja(llja+la)))                                            !/*xb4c2     729*/
      if(jb.gt.0.and.jb.le.mxj)go to 730                                        !/*xb4c2     730*/
      if(abs(lja(llja+la)).gt.1000)go to 740                                    !/*xb4c2     731*/
      call erprnt(2,1,2,lja(llja+la),ncl(lncl+ic),0,0,1,                        !/*xb4c2     732*/
     & '8hsurface ,i6,20h not found for cell ,i5,6h card.')                     !/*xb4c2     733*/
      go to 740                                                                 !/*xb4c2     734*/
  730 if(ksm(lksm+jb).lt.0)go to 750                                            !/*xb4c2     735*/
  740 continue                                                                  !/*xb4c2     736*/
      go to 800                                                                 !/*xb4c2     737*/
  750 iz=0                                                                      !/*xb4c2     738*/
!                                                                               !/*xb4c2     739*/
!        deal with macrobody surface on cell ic -- expand to surfaces.          !/*xb4c2     740*/
      ix=lsc(llsc+jb)                                                           !/*xb4c2     741*/
      jz=6                                                                      !/*xb4c2     742*/
      ji=-ksm(lksm+jb)                                                          !/*xb4c2     743*/
      if(ji.eq.33.or.ji.eq.34.or.ji.eq.36)jz=3                                  !/*xb4c2     744*/
      if(ji.eq.39)jz=8                                                          !/*xb4c2     745*/
      if(ji.gt.4000)jz=ji/1000                                                  !/*xb4c2     746*/
      if(ji.eq.37)jz=5                                                          !/*xb4c2     747*/
      if(ji.eq.1030.or.ji.eq.1031)jz=4                                          !/*xb4c2     748*/
      if(idna(lida+la).ge.0)go to 760                                           !/*xb4c2     749*/
      if(abs(idna(lida+la)).gt.jz)                                              !/*xb4c2     750*/
     & call erprnt(2,1,2,lja(llja+la),ncl(lncl+ic),0,0,1,                       !/*xb4c2     751*/
     & '32hfacet does not exist for surface,i6,8h on cell,i5,6h card.')         !/*xb4c2     752*/
      m1=la+1                                                                   !/*xb4c2     753*/
      go to 720                                                                 !/*xb4c2     754*/
!                                                                               !/*xb4c2     755*/
!        slot is already occupied,   lh=total additional.                       !/*xb4c2     756*/
!        include unions if outside macrobody -- positive sense.                 !/*xb4c2     757*/
!        move data up to make room for additional surfaces.                     !/*xb4c2     758*/
  760 lh=jz-1                                                                   !/*xb4c2     759*/
      if(lja(llja+la).gt.0)lh=lh+(jz+1)                                         !/*xb4c2     760*/
      do 770 ii=1,nlja-la                                                       !/*xb4c2     761*/
      i=nlja-la+1-ii                                                            !/*xb4c2     762*/
      idna(lida+la+lh+i)=idna(lida+la+i)                                        !/*xb4c2     763*/
  770 lja(llja+la+lh+i)=lja(llja+la+i)                                          !/*xb4c2     764*/
      mq=lja(llja+la)                                                           !/*xb4c2     765*/
      idna(lida+la)=1                                                           !/*xb4c2     766*/
      if(mq.gt.0)lja(llja+la+1)=lja(llja+la)                                    !/*xb4c2     767*/
      if(mq.gt.0)lja(llja+la)=1000001                                           !/*xb4c2     768*/
      if(mq.gt.0)lja(llja+la+lh)=1000002                                        !/*xb4c2     769*/
      if(mq.gt.0)idna(lida+la+1)=idna(lida+la)                                  !/*xb4c2     770*/
      if(mq.gt.0)idna(lida+la)=0                                                !/*xb4c2     771*/
      if(mq.gt.0)idna(lida+la+lh)=0                                             !/*xb4c2     772*/
      nlja=nlja+lh                                                              !/*xb4c2     773*/
      do 780 i=ic,mxa                                                           !/*xb4c2     774*/
  780 lca(llca+i+1)=lca(llca+i+1)+sign(lh,lca(llca+i+1))                        !/*xb4c2     775*/
      if(mq.gt.0)lca(llca+ic)=-abs(lca(llca+ic))                                !/*xb4c2     776*/
!                                                                               !/*xb4c2     777*/
!        include surfaces for macrobody.                                        !/*xb4c2     778*/
      jj=la+1                                                                   !/*xb4c2     779*/
      if(mq.gt.0)jj=jj+1                                                        !/*xb4c2     780*/
      do 790 i=2,jz                                                             !/*xb4c2     781*/
      if(mq.gt.0)lja(llja+jj)=1000003                                           !/*xb4c2     782*/
      if(mq.gt.0)idna(lida+jj)=0                                                !/*xb4c2     783*/
      if(mq.gt.0)jj=jj+1                                                        !/*xb4c2     784*/
      lja(llja+jj)=mq                                                           !/*xb4c2     785*/
      idna(lida+jj)=i                                                           !/*xb4c2     786*/
      jj=jj+1                                                                   !/*xb4c2     787*/
  790 continue                                                                  !/*xb4c2     788*/
      if(mq.lt.0)jj=jj-1                                                        !/*xb4c2     789*/
      m1=jj+1                                                                   !/*xb4c2     790*/
      go to 720                                                                 !/*xb4c2     791*/
  800 continue                                                                  !/*xb4c2     792*/
      return                                                                    !/*xb4c      706*/
      end                                                                       !/*xb4c      707*/
