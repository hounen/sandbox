!#ident prlost.F from deck po                                                   !/*po          1*/
      subroutine prlost                                                         !/*po          2*/
!        print information about lost particles.                                !/*po          3*/
      use global_data                                                           !/*pox         1*/
!                                                                               !/*pox         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*pox         3*/
      implicit integer(kindi) (i-n)                                             !/*pox         4*/
!                                                                               !/*pox         5*/
      dimension ip(29),dv(3)                                                    !/*po          5*/
      character hf*8,ht(8)*16,hw(2)*8                                           !/*po4c        1*/
      data hw/'newcel','track'/,                                                !/*po          7*/
     & ip/0,1,2,3,4,0,1,2,3,0,0,0,1,2,3,0,0,0,1,2,3,0,0,1,2,3,1,2,3/            !/*po          8*/
#ifdef MULTT                                                                    /*!poxk        1*/
      call thread_lockon(jlock)                                                 !/*poxk        2*/
#endif                                                                          /*!poxk        3*/
!                                                                               !/*po         10*/
!        once only, send geometry error message to the tty.                     !/*po         11*/
      nerr=nerr+1                                                               !/*po         12*/
      if(nerr.eq.1.and.ntasks.le.1.and.ltasks.le.1)                             !/*po4c        2*/
     & write(jtty,10)hw(mod(kdb-1,2)+1)                                         !/*po4c        3*/
   10 format(19h geometry error in ,a7)                                         !/*po         14*/
!                                                                               !/*po         15*/
!        determine whether the particle really is in cell icl.                  !/*po         16*/
      if(kdb.eq.2.or.kdb.eq.4.or.kdb.eq.6)go to 20                              !/*po         17*/
      uuu=-uuu                                                                  !/*po         18*/
      vvv=-vvv                                                                  !/*po         19*/
      www=-www                                                                  !/*po         20*/
   20 call chkcel(icl,0,j1)                                                     !/*po         21*/
      if(j1.eq.0)go to 40                                                       !/*po         22*/
      do 30 iq=1,mxa                                                            !/*po         23*/
      call chkcel(iq,0,j2)                                                      !/*po         24*/
   30 if(j2.eq.0)go to 40                                                       !/*po         25*/
   40 if(kdb.eq.2.or.kdb.eq.4.or.kdb.eq.6)go to 230                             !/*po         26*/
      uuu=-uuu                                                                  !/*po         27*/
      vvv=-vvv                                                                  !/*po         28*/
      www=-www                                                                  !/*po         29*/
!                                                                               !/*po         30*/
!*****************  kdb=1, 3, or 5 -- no cell found  *******************        !/*po         31*/
!                                                                      *        !/*po         32*/
!        print page heading and general description of the incident.            !/*po         33*/
      jq=nsf(lnsf+jsu)                                                          !/*po4c        4*/
      write(hf,'(i8)')jq                                                        !/*po4c        5*/
      if(kfq.ne.0)write(hf,'(i6,1h.,i1)')jq,kfq                                 !/*po4c        6*/
      write(iuo,50)nerr,npstc,hnp(ipt),hf,ncl(lncl+icl)                         !/*po4c        7*/
   50 format(21h1   lost particle no.,i3,5x,16hno cell found in,                !/*po         35*/
     & 18h subroutine newcel,5x,11hhistory no.,i9//5h the ,a11,                 !/*pox         7*/
     & 44h currently being tracked has reached surface,a8,7h; there/            !/*po4c        8*/
     & 60h appears to be no cell on the other side of the surface from,         !/*po         38*/
     & 5h cell,i6/15h at that point.)                                           !/*po         39*/
      if(kdb.eq.3)write(iuo,60)hnp(ipt)                                         !/*po         40*/
   60 format(/5h the ,a11,35h got lost on its way to a detector.)               !/*pox         8*/
      if(kdb.eq.5)write(iuo,70)hnp(ipt)                                         !/*po         42*/
   70 format(/5h the ,a11,39h got lost on its way to a dxtran place.)           !/*pox         9*/
      if(j1.eq.0)write(iuo,80)hnp(ipt),ncl(lncl+icl)                            !/*po         44*/
   80 format(/5h the ,a11,11h is in cell,i6,1h.)                                !/*pox        10*/
      if(j1.ne.0.and.j2.eq.0)write(iuo,90)ncl(lncl+iq)                          !/*po         46*/
   90 format(/25h point (x,y,z) is in cell,i6)                                  !/*po         47*/
!                                                                               !/*po         48*/
!        print the basic particle data.                                         !/*po         49*/
      write(iuo,100)xxx,yyy,zzz,uuu,vvv,www,erg,wgt,tme                         !/*po         50*/
  100 format(/19h x,y,z coordinates:,1pe21.5,2e15.5/                            !/*po         51*/
     & 25h u,v,w direction cosines:,3e15.5/                                     !/*po         52*/
     & 9h energy =,e12.5,4x,8hweight =,e12.5,4x,6htime =,e12.5)                 !/*po         53*/
!                                                                               !/*po         54*/
!        print the radius if it is applicable.                                  !/*po         55*/
      k=ip(kst(lkst+jsu))                                                       !/*po4a        1*/
      if(kst(lkst+jsu).ne.22)go to 110                                          !/*po4a        2*/
      i=lsc(llsc+jsu)+1                                                         !/*po         58*/
      if(scf(i+8).eq.0..and.scf(i+9).eq.0..and.scf(i+4).eq.0..and.              !/*po         59*/
     & scf(i+5).eq.0..and.scf(i+1).eq.scf(i+2))k=1                              !/*po         60*/
      if(scf(i+9).eq.0..and.scf(i+7).eq.0..and.scf(i+5).eq.0..and.              !/*po         61*/
     & scf(i+3).eq.0..and.scf(i+2).eq.scf(i))k=2                                !/*po         62*/
      if(scf(i+7).eq.0..and.scf(i+8).eq.0..and.scf(i+3).eq.0..and.              !/*po         63*/
     & scf(i+4).eq.0..and.scf(i).eq.scf(i+1))k=3                                !/*po         64*/
  110 if(k.eq.1)write(iuo,120)sqrt(yyy**2+zzz**2)                               !/*po         65*/
  120 format(18h sqrt(y**2+z**2) =,1pe12.5)                                     !/*po         66*/
      if(k.eq.2)write(iuo,130)sqrt(zzz**2+xxx**2)                               !/*po         67*/
  130 format(18h sqrt(z**2+x**2) =,1pe12.5)                                     !/*po         68*/
      if(k.eq.3)write(iuo,140)sqrt(xxx**2+yyy**2)                               !/*po         69*/
  140 format(18h sqrt(x**2+y**2) =,1pe12.5)                                     !/*po         70*/
      if(k.eq.4)write(iuo,150)sqrt(xxx**2+yyy**2+zzz**2)                        !/*po         71*/
  150 format(23h sqrt(x**2+y**2+z**2) =,1pe12.5)                                !/*po         72*/
!                                                                               !/*po         73*/
!        print the rest of the general information.                             !/*po         74*/
      write(iuo,160)hf,dls,pmf,hnp(ipt),nch(ipt)                                !/*po4c        9*/
  160 format(24h the distance to surface,a8,23h from the last event is,         !/*po4c       10*/
     & 1pe12.5/49h the distance to collision from the last event is,            !/*po         77*/
     & e12.5/15h the number of ,a11,                                            !/*pox        11*/
     & 37h collisions so far in this history is,i6,1h.)                         !/*po         79*/
!                                                                               !/*po         80*/
!        print a list of the rejected cells.                                    !/*po         81*/
      do 170 i1=abs(lca(llca+icl)),nlja                                         !/*po         82*/
  170 if(abs(lja(llja+i1)).eq.jsu)go to 180                                     !/*po         83*/
  180 j2=lcaj(klcj+i1)                                                          !/*po         84*/
      j3=abs(lcaj(klcj+i1+1))-1                                                 !/*po         85*/
      if(j3.lt.j2)go to 340                                                     !/*po4c2       1*/
      write(iuo,190)hf,ncl(lncl+icl)                                            !/*po4c       11*/
  190 format(/52h the cells so far found on the other side of surface,          !/*po         88*/
     & a8,8h of cell,i5,                                                        !/*po4c       12*/
     & /55h (and the surface with respect to which the point x,y,z,             !/*po         90*/
     & 26h had the wrong sense) are:/                                           !/*po         91*/
     & 36h (see chapter 5 of the user manual.)/)                                !/*po4c       13*/
      m=0                                                                       !/*po         93*/
      do 220 j1=j2,j3                                                           !/*po         94*/
      if(i.eq.1000001.and.laj(j1).ne.1000002)go to 220                          !/*po         95*/
      i=abs(laj(j1))                                                            !/*po         96*/
      if(i.gt.1000000)go to 220                                                 !/*po         97*/
      call chkcel(i,0,j)                                                        !/*po         98*/
      m=m+1                                                                     !/*po         99*/
      ht(m)=' '                                                                 !/*po        100*/
      write(ht(m)(1:6),'(i6)')ncl(lncl+i)                                       !/*po        101*/
      if(lca(llca+i).lt.0)go to 200                                             !/*po        102*/
      ht(m)(7:7)='('                                                            !/*po        103*/
      r=nsf(lnsf+abs(lja(llja+j)))                                              !/*po        104*/
      jq=r                                                                      !/*po4c       14*/
      write(hf,'(i8)')jq                                                        !/*po4c       15*/
      if(kfq.ne.0)write(hf,'(i6,1h.,i1)')jq,kfq                                 !/*po4c       16*/
      i=log10(.5+r)                                                             !/*po        105*/
      if(kfq.ne.0)i=i+2                                                         !/*po4c       17*/
      ht(m)(8:8+i)=hf(8-i:8)                                                    !/*po4c       18*/
      ht(m)(9+i:9+i)=')'                                                        !/*po        108*/
  200 if(m.lt.7.and.j1.lt.j3)go to 220                                          !/*po4c       19*/
      write(iuo,210)(ht(j),j=1,m)                                               !/*po        110*/
  210 format(7(a16))                                                            !/*po4c       20*/
      m=0                                                                       !/*po        112*/
  220 continue                                                                  !/*po        113*/
      go to 290                                                                 !/*po        114*/
!                                                                               !/*po        115*/
!*************  kdb=2, 4, or 6 -- no intersection found  ***************        !/*po        116*/
!                                                                      *        !/*po        117*/
!        print page heading and general description of the incident.            !/*po        118*/
  230 write(iuo,240)nerr,npstc                                                  !/*po        119*/
  240 format(21h1   lost particle no.,i3,5x,24hno intersection found in,        !/*po        120*/
     & 17h subroutine track,5x,11hhistory no.,i9)                               !/*po        121*/
      if(j1.eq.0)write(iuo,250)hnp(ipt),ncl(lncl+icl),hnp(ipt)                  !/*po        122*/
  250 format(/5h the ,a11,35h currently being tracked is in cell,i5,1h.,        !/*pox        12*/
     & 15h  there appears/36h to be no surface of the cell in the,              !/*po        124*/
     & 15h direction the ,a11,10h is going.)                                    !/*pox        13*/
      if(kdb.eq.4)write(iuo,60)hnp(ipt)                                         !/*po        126*/
      if(kdb.eq.6)write(iuo,70)hnp(ipt)                                         !/*po        127*/
      if(j1.ne.0.and.j2.eq.0)write(iuo,260)hnp(ipt),                            !/*po        128*/
     & ncl(lncl+icl),ncl(lncl+iq)                                               !/*po        129*/
  260 format(/5h the ,a11,40h currently being tracked thinks it is in,          !/*pox        14*/
     & 5h cell,i5,1h./23h actually it is in cell,i5,1h.)                        !/*po        131*/
      if(j1.ne.0.and.j2.ne.0)write(iuo,270)hnp(ipt),ncl(lncl+icl)               !/*po        132*/
  270 format(/5h the ,a11,40h currently being tracked thinks it is in,          !/*pox        15*/
     & 5h cell,i5,1h./34h actually it is in no cell at all.)                    !/*po        134*/
!                                                                               !/*po        135*/
!        print the basic particle data.                                         !/*po        136*/
!     QUESTION: Is the nch(1)+nch(2) left over from only two particle           !/*pox        16*/
!               types?  Should the sum be over all particle types ???           !/*pox        17*/
      write(iuo,280)xxx,yyy,zzz,uuu,vvv,www,erg,wgt,tme,pmf,                    !/*po        137*/
     & nch(1)+nch(2)                                                            !/*po        138*/
  280 format(/19h x,y,z coordinates:,1pe21.5,2e15.5/                            !/*po        139*/
     & 25h u,v,w direction cosines:,3e15.5/                                     !/*po        140*/
     & 9h energy =,e12.5,4x,8hweight =,e12.5,4x,6htime =,e12.5/                 !/*po        141*/
     & 49h the distance to collision from the last event is,e12.5/              !/*po        142*/
     & 51h the number of collisions so far in this history is,i6,1h.)           !/*po        143*/
!                                                                               !/*po        144*/
!**********************  check for cone vertex.  ***********************        !/*po        145*/
!                                                                      *        !/*po        146*/
  290 do 310 j=1,mxj                                                            !/*po        147*/
      kk=kst(lkst+j)                                                            !/*po4a        3*/
      if(kk.lt.16.or.kk.gt.21)go to 310                                         !/*po        149*/
      ix=lsc(llsc+j)                                                            !/*po        150*/
      do 300 i=1,3                                                              !/*po        151*/
      dv(i)=gpblcm(i)                                                           !/*po        152*/
  300 if(kk.le.18)dv(i)=dv(i)-scf(ix+i)                                         !/*po        153*/
      if(kk.gt.18)dv(kk-18)=dv(kk-18)-scf(ix+1)                                 !/*po        154*/
      if((dv(1)*vvv-uuu*dv(2))**2+(dv(2)*www-vvv*dv(3))**2+(dv(3)*uuu-          !/*po        155*/
     & www*dv(1))**2.lt.1.e-8*(dv(1)**2+dv(2)**2+dv(3)**2))go to 320            !/*po        156*/
  310 continue                                                                  !/*po        157*/
      go to 340                                                                 !/*po        158*/
  320 write(iuo,330)nsf(lnsf+j)                                                 !/*po        159*/
  330 format(/50h the trajectory passes close to the vertex of cone,i7/         !/*po        160*/
     & 48h perhaps that is the cause of the lost particle.)                     !/*po        161*/
  340 write(iuo,'(/////)')                                                      !/*po        162*/
#ifdef MULTT                                                                    /*!poxk        4*/
      call thread_lockoff(jlock)                                                !/*poxk        5*/
#endif                                                                          /*!poxk        6*/
      return                                                                    !/*po        165*/
      end                                                                       !/*po        166*/
