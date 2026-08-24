!#ident chkcel.F from deck cc                                                   !/*cc          1*/
      subroutine chkcel(i1,m,j)                                                 !/*cc          2*/
!        check the sense of xyz with respect to the surfaces of cell i1.        !/*cc          3*/
!        m=0  simple cell:  return j=0 if xyz is in the cell.  otherwise        !/*cc          4*/
!             return j=first bad surface.  ignore surface jsu.                  !/*cc          5*/
!             complicated cell:  return j=0 if xyz is in the cell and,          !/*cc          6*/
!             if jsu is a surface of the cell, uvw is directed into the         !/*cc          7*/
!             cell.  otherwise return j=1.                                      !/*cc          8*/
!        m=1  for surface segmenting in tally.  return j=first good             !/*cc          9*/
!             surface.  if none, return j=0.                                    !/*cc         10*/
!        m=2  like m=0, but cell is treated as complicated regardless.          !/*cc         11*/
!        m=3  preprocessor for track in places where lgc is not already         !/*cc         12*/
!             loaded.  load logical elements into lgc.                          !/*cc         13*/
!        if jsu is a surface of the cell and either m>0 or the cell is          !/*cc         14*/
!        complicated, uvw must be appropriately defined.                        !/*cc         15*/
      use global_data                                                           !/*ccx         1*/
!                                                                               !/*ccx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*ccx         3*/
      implicit integer(kindi) (i-n)                                             !/*ccx         4*/
!                                                                               !/*ccx         5*/
!                                                                               !/*cc         17*/
!        do all surfaces of cell i1 or until mm is matched.                     !/*cc         18*/
      j2=abs(lca(llca+i1))                                                      !/*cc         19*/
      il=j2-lca(llca+i1)+m                                                      !/*cc         20*/
      mm=m                                                                      !/*cc         21*/
      if(il.ne.0.and.m.eq.0)mm=2                                                !/*cc         22*/
      do 340 j=j2,abs(lca(llca+i1+1))-1                                         !/*cc         23*/
      jk=abs(lja(llja+j))                                                       !/*cc         24*/
      if(il.eq.0)go to 30                                                       !/*cc         25*/
      if(jk.gt.1000000)go to 20                                                 !/*cc         26*/
      if(jk.ne.jsu)go to 40                                                     !/*cc         27*/
   10 lgc(j-j2+1)=1                                                             !/*cc         28*/
      if(jsu.eq.0)go to 340                                                     !/*ccxa        1*/
      if(lja(llja+j)*angl().lt.0.)lgc(j-j2+1)=0                                 !/*cc         29*/
      go to 340                                                                 !/*cc         30*/
   20 lgc(j-j2+1)=jk                                                            !/*cc         31*/
      go to 340                                                                 !/*cc         32*/
   30 if(jk.eq.jsu)go to 340                                                    !/*cc         33*/
   40 k=kst(lkst+jk)                                                            !/*cc4a        1*/
      i=lsc(llsc+jk)+1                                                          !/*cc         35*/
      go to(50,60,60,60,70,80,90,100,110,120,130,140,150,160,170,180,           !/*cc         36*/
     & 190,200,230,240,250,280,290,300,310,320)k                                !/*cc         37*/
!                                                                               !/*cc         38*/
! >>>>>  planes:  p, px, py, pz                                                 !/*cc         39*/
!        if jsu<0, a plane may be coincident with jsu at a higher level.        !/*cc4b        1*/
   50 t4=scf(i)*xxx+scf(i+1)*yyy+scf(i+2)*zzz-scf(i+3)                          !/*cc         40*/
      if(jsu.ge.0)go to 330                                                     !/*cc4b        2*/
      if(ksc(lksc+jk).ne.ksc(lksc-jsu))go to 335                                !/*cc4b        3*/
      t5=scf(i)*uuu+scf(i+1)*vvv+scf(i+2)*www                                   !/*cc4c        1*/
      if(abs(t4).le.coincd*abs(t5))t4=t5                                        !/*cc4c        2*/
      go to 335                                                                 !/*cc4b        5*/
   60 t4=gpblcm(k-1)-scf(i)                                                     !/*cc4b        6*/
      if(jsu.ge.0)go to 330                                                     !/*cc4b        7*/
      if(ksc(lksc+jk).ne.ksc(lksc-jsu))go to 335                                !/*cc4b        8*/
      if(abs(t4).le.coincd*abs(gpblcm(k+2)))t4=gpblcm(k+2)                      !/*cc4c        3*/
      go to 335                                                                 !/*cc4b       10*/
!                                                                               !/*cc         44*/
! >>>>>  spheres:  so, s, sx, sy, sz                                            !/*cc         45*/
   70 t4=xxx**2+yyy**2+zzz**2-scf(i)                                            !/*cc         46*/
      go to 330                                                                 !/*cc         47*/
   80 t4=(xxx-scf(i))**2+(yyy-scf(i+1))**2+(zzz-scf(i+2))**2-scf(i+3)           !/*cc         48*/
      go to 330                                                                 !/*cc         49*/
   90 t4=(xxx-scf(i))**2+yyy**2+zzz**2-scf(i+1)                                 !/*cc         50*/
      go to 330                                                                 !/*cc         51*/
  100 t4=xxx**2+(yyy-scf(i))**2+zzz**2-scf(i+1)                                 !/*cc         52*/
      go to 330                                                                 !/*cc         53*/
  110 t4=xxx**2+yyy**2+(zzz-scf(i))**2-scf(i+1)                                 !/*cc         54*/
      go to 330                                                                 !/*cc         55*/
!                                                                               !/*cc         56*/
! >>>>>  cylinders:  c/x, c/y, c/z, cx, cy, cz                                  !/*cc         57*/
  120 t4=(yyy-scf(i))**2+(zzz-scf(i+1))**2-scf(i+2)                             !/*cc         58*/
      go to 330                                                                 !/*cc         59*/
  130 t4=(xxx-scf(i))**2+(zzz-scf(i+1))**2-scf(i+2)                             !/*cc         60*/
      go to 330                                                                 !/*cc         61*/
  140 t4=(xxx-scf(i))**2+(yyy-scf(i+1))**2-scf(i+2)                             !/*cc         62*/
      go to 330                                                                 !/*cc         63*/
  150 t4=yyy**2+zzz**2-scf(i)                                                   !/*cc         64*/
      go to 330                                                                 !/*cc         65*/
  160 t4=xxx**2+zzz**2-scf(i)                                                   !/*cc         66*/
      go to 330                                                                 !/*cc         67*/
  170 t4=xxx**2+yyy**2-scf(i)                                                   !/*cc         68*/
      go to 330                                                                 !/*cc         69*/
!                                                                               !/*cc         70*/
! >>>>>  cones:  k/x, k/y, k/z, kx, ky, kz                                      !/*cc         71*/
  180 t1=(yyy-scf(i+1))**2+(zzz-scf(i+2))**2                                    !/*cc         72*/
      t2=xxx-scf(i)                                                             !/*cc         73*/
      go to 210                                                                 !/*cc         74*/
  190 t1=(xxx-scf(i))**2+(zzz-scf(i+2))**2                                      !/*cc         75*/
      t2=yyy-scf(i+1)                                                           !/*cc         76*/
      go to 210                                                                 !/*cc         77*/
  200 t1=(xxx-scf(i))**2+(yyy-scf(i+1))**2                                      !/*cc         78*/
      t2=zzz-scf(i+2)                                                           !/*cc         79*/
  210 if(scf(i+4).eq.0.)go to 220                                               !/*cc         80*/
      t4=t1-sign((scf(i+4)*t2)**2,scf(i+4)*t2)                                  !/*cc         81*/
      go to 330                                                                 !/*cc         82*/
  220 t4=t1-scf(i+3)*t2**2                                                      !/*cc         83*/
      go to 330                                                                 !/*cc         84*/
  230 t1=yyy**2+zzz**2                                                          !/*cc         85*/
      t2=xxx-scf(i)                                                             !/*cc         86*/
      go to 260                                                                 !/*cc         87*/
  240 t1=xxx**2+zzz**2                                                          !/*cc         88*/
      t2=yyy-scf(i)                                                             !/*cc         89*/
      go to 260                                                                 !/*cc         90*/
  250 t1=xxx**2+yyy**2                                                          !/*cc         91*/
      t2=zzz-scf(i)                                                             !/*cc         92*/
  260 if(scf(i+2).eq.0.)go to 270                                               !/*cc         93*/
      t4=t1-sign((scf(i+2)*t2)**2,scf(i+2)*t2)                                  !/*cc         94*/
      go to 330                                                                 !/*cc         95*/
  270 t4=t1-scf(i+1)*t2**2                                                      !/*cc         96*/
      go to 330                                                                 !/*cc         97*/
!                                                                               !/*cc         98*/
! >>>>>  special quadratic:  sq                                                 !/*cc         99*/
  280 t1=xxx-scf(i+7)                                                           !/*cc        100*/
      t2=yyy-scf(i+8)                                                           !/*cc        101*/
      t3=zzz-scf(i+9)                                                           !/*cc        102*/
      t4=scf(i)*t1**2+scf(i+1)*t2**2+scf(i+2)*t3**2+2.*(scf(i+3)*t1+            !/*cc        103*/
     & scf(i+4)*t2+scf(i+5)*t3)+scf(i+6)                                        !/*cc        104*/
      go to 330                                                                 !/*cc        105*/
!                                                                               !/*cc        106*/
! >>>>>  general quadratic:  gq                                                 !/*cc        107*/
  290 t4=(scf(i)*xxx+scf(i+3)*yyy+scf(i+6))*xxx+                                !/*cc        108*/
     & (scf(i+1)*yyy+scf(i+4)*zzz+scf(i+7))*yyy+                                !/*cc        109*/
     & (scf(i+2)*zzz+scf(i+5)*xxx+scf(i+8))*zzz+scf(i+9)                        !/*cc        110*/
      go to 330                                                                 !/*cc        111*/
!                                                                               !/*cc        112*/
! >>>>>  tori:  tx, ty, tz                                                      !/*cc        113*/
  300 t4=(sqrt((yyy-scf(i+1))**2+(zzz-scf(i+2))**2)-scf(i+3))**2+               !/*cc        114*/
     & scf(i+6)*(xxx-scf(i))**2-scf(i+5)**2                                     !/*cc        115*/
      go to 330                                                                 !/*cc        116*/
  310 t4=(sqrt((xxx-scf(i))**2+(zzz-scf(i+2))**2)-scf(i+3))**2+                 !/*cc        117*/
     & scf(i+6)*(yyy-scf(i+1))**2-scf(i+5)**2                                   !/*cc        118*/
      go to 330                                                                 !/*cc        119*/
  320 t4=(sqrt((xxx-scf(i))**2+(yyy-scf(i+1))**2)-scf(i+3))**2+                 !/*cc        120*/
     & scf(i+6)*(zzz-scf(i+2))**2-scf(i+5)**2                                   !/*cc        121*/
!                                                                               !/*cc        122*/
!        set logical value into lgc.  return if it matches mm.                  !/*cc        123*/
  330 if(t4.eq.0.)go to 10                                                      !/*cc        124*/
  335 lgc(j-j2+1)=1                                                             !/*cc4b       11*/
      if(lja(llja+j)*t4.lt.0.)lgc(j-j2+1)=0                                     !/*cc        126*/
      if(lgc(j-j2+1).eq.mm)return                                               !/*cc        127*/
  340 continue                                                                  !/*cc        128*/
      j=0                                                                       !/*cc        129*/
      if(mm.ne.2)return                                                         !/*cc        130*/
      j=1-lgeval(lgc,abs(lca(llca+i1+1))-j2)                                    !/*cc        131*/
      return                                                                    !/*cc        132*/
      end                                                                       !/*cc        133*/
