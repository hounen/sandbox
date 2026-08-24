!#ident angl.F from deck al                                                     !/*al          1*/
      function angl()                                                           !/*al          2*/
!        calculate the cosine of the angle between the direction of the         !/*al          3*/
!        particle and the positive normal, ang, to the surface jsu.             !/*al          4*/
      use global_data                                                           !/*alx         1*/
!                                                                               !/*alx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*alx         3*/
      implicit integer(kindi) (i-n)                                             !/*alx         4*/
!                                                                               !/*alx         5*/
!                                                                               !/*al          6*/
      k=kst(lkst+jsu)                                                           !/*al4a        1*/
      i=lsc(llsc+jsu)+1                                                         !/*al          8*/
      ang(1)=0.                                                                 !/*al          9*/
      ang(2)=0.                                                                 !/*al         10*/
      ang(3)=0.                                                                 !/*al         11*/
      go to(10,20,20,20,30,40,50,50,50,60,70,80,90,100,110,120,                 !/*al         12*/
     & 130,140,150,160,170,180,190,200,210,220)k                                !/*al         13*/
!                                                                               !/*al         14*/
! >>>>>  planes:  p, px, py, pz                                                 !/*al         15*/
   10 ang(1)=scf(i)                                                             !/*al         16*/
      ang(2)=scf(i+1)                                                           !/*al         17*/
      ang(3)=scf(i+2)                                                           !/*al         18*/
      go to 230                                                                 !/*al         19*/
   20 ang(k-1)=1.                                                               !/*al         20*/
      angl=gpblcm(k+2)                                                          !/*al         21*/
      return                                                                    !/*al         22*/
!                                                                               !/*al         23*/
! >>>>>  spheres:  so, s, sx, sy, sz                                            !/*al         24*/
   30 ang(1)=xxx                                                                !/*al         25*/
      ang(2)=yyy                                                                !/*al         26*/
      ang(3)=zzz                                                                !/*al         27*/
      go to 230                                                                 !/*al         28*/
   40 ang(1)=xxx-scf(i)                                                         !/*al         29*/
      ang(2)=yyy-scf(i+1)                                                       !/*al         30*/
      ang(3)=zzz-scf(i+2)                                                       !/*al         31*/
      go to 230                                                                 !/*al         32*/
   50 ang(1)=xxx                                                                !/*al         33*/
      ang(2)=yyy                                                                !/*al         34*/
      ang(3)=zzz                                                                !/*al         35*/
      ang(k-6)=ang(k-6)-scf(i)                                                  !/*al         36*/
      go to 230                                                                 !/*al         37*/
!                                                                               !/*al         38*/
! >>>>>  cylinders:  c/x, c/y, c/z, cx, cy, cz                                  !/*al         39*/
   60 ang(2)=yyy-scf(i)                                                         !/*al         40*/
      ang(3)=zzz-scf(i+1)                                                       !/*al         41*/
      go to 230                                                                 !/*al         42*/
   70 ang(1)=xxx-scf(i)                                                         !/*al         43*/
      ang(3)=zzz-scf(i+1)                                                       !/*al         44*/
      go to 230                                                                 !/*al         45*/
   80 ang(1)=xxx-scf(i)                                                         !/*al         46*/
      ang(2)=yyy-scf(i+1)                                                       !/*al         47*/
      go to 230                                                                 !/*al         48*/
   90 ang(2)=yyy                                                                !/*al         49*/
      ang(3)=zzz                                                                !/*al         50*/
      go to 230                                                                 !/*al         51*/
  100 ang(1)=xxx                                                                !/*al         52*/
      ang(3)=zzz                                                                !/*al         53*/
      go to 230                                                                 !/*al         54*/
  110 ang(1)=xxx                                                                !/*al         55*/
      ang(2)=yyy                                                                !/*al         56*/
      go to 230                                                                 !/*al         57*/
!                                                                               !/*al         58*/
! >>>>>  cones:  k/x, k/y, k/z, kx, ky, kz                                      !/*al         59*/
  120 ang(1)=(scf(i)-xxx)*scf(i+3)                                              !/*al         60*/
      ang(2)=yyy-scf(i+1)                                                       !/*al         61*/
      ang(3)=zzz-scf(i+2)                                                       !/*al         62*/
      go to 230                                                                 !/*al         63*/
  130 ang(1)=xxx-scf(i)                                                         !/*al         64*/
      ang(2)=(scf(i+1)-yyy)*scf(i+3)                                            !/*al         65*/
      ang(3)=zzz-scf(i+2)                                                       !/*al         66*/
      go to 230                                                                 !/*al         67*/
  140 ang(1)=xxx-scf(i)                                                         !/*al         68*/
      ang(2)=yyy-scf(i+1)                                                       !/*al         69*/
      ang(3)=(scf(i+2)-zzz)*scf(i+3)                                            !/*al         70*/
      go to 230                                                                 !/*al         71*/
  150 ang(1)=(scf(i)-xxx)*scf(i+1)                                              !/*al         72*/
      ang(2)=yyy                                                                !/*al         73*/
      ang(3)=zzz                                                                !/*al         74*/
      go to 230                                                                 !/*al         75*/
  160 ang(1)=xxx                                                                !/*al         76*/
      ang(2)=(scf(i)-yyy)*scf(i+1)                                              !/*al         77*/
      ang(3)=zzz                                                                !/*al         78*/
      go to 230                                                                 !/*al         79*/
  170 ang(1)=xxx                                                                !/*al         80*/
      ang(2)=yyy                                                                !/*al         81*/
      ang(3)=(scf(i)-zzz)*scf(i+1)                                              !/*al         82*/
      go to 230                                                                 !/*al         83*/
!                                                                               !/*al         84*/
! >>>>>  special quadratic:  sq                                                 !/*al         85*/
  180 ang(1)=scf(i)*(xxx-scf(i+7))+scf(i+3)                                     !/*al         86*/
      ang(2)=scf(i+1)*(yyy-scf(i+8))+scf(i+4)                                   !/*al         87*/
      ang(3)=scf(i+2)*(zzz-scf(i+9))+scf(i+5)                                   !/*al         88*/
      go to 230                                                                 !/*al         89*/
!                                                                               !/*al         90*/
! >>>>>  general quadratic:  gq                                                 !/*al         91*/
  190 ang(1)=2.*scf(i)*xxx+scf(i+3)*yyy+scf(i+5)*zzz+scf(i+6)                   !/*al         92*/
      ang(2)=2.*scf(i+1)*yyy+scf(i+3)*xxx+scf(i+4)*zzz+scf(i+7)                 !/*al         93*/
      ang(3)=2.*scf(i+2)*zzz+scf(i+4)*yyy+scf(i+5)*xxx+scf(i+8)                 !/*al         94*/
      go to 230                                                                 !/*al         95*/
!                                                                               !/*al         96*/
! >>>>>  tori:  tx, ty, tz                                                      !/*al         97*/
  200 t4=2.*(1.-scf(i+3)/sqrt((yyy-scf(i+1))**2+(zzz-scf(i+2))**2))             !/*al         98*/
      ang(1)=2.*scf(i+6)*(xxx-scf(i))                                           !/*al         99*/
      ang(2)=(yyy-scf(i+1))*t4                                                  !/*al        100*/
      ang(3)=(zzz-scf(i+2))*t4                                                  !/*al        101*/
      go to 230                                                                 !/*al        102*/
  210 t4=2.*(1.-scf(i+3)/sqrt((xxx-scf(i))**2+(zzz-scf(i+2))**2))               !/*al        103*/
      ang(1)=(xxx-scf(i))*t4                                                    !/*al        104*/
      ang(2)=2.*scf(i+6)*(yyy-scf(i+1))                                         !/*al        105*/
      ang(3)=(zzz-scf(i+2))*t4                                                  !/*al        106*/
      go to 230                                                                 !/*al        107*/
  220 t4=2.*(1.-scf(i+3)/sqrt((xxx-scf(i))**2+(yyy-scf(i+1))**2))               !/*al        108*/
      ang(1)=(xxx-scf(i))*t4                                                    !/*al        109*/
      ang(2)=(yyy-scf(i+1))*t4                                                  !/*al        110*/
      ang(3)=2.*scf(i+6)*(zzz-scf(i+2))                                         !/*al        111*/
!                                                                               !/*al        112*/
!        normalize ang and calculate the cosine.                                !/*al        113*/
  230 t4=sqrt(ang(1)**2+ang(2)**2+ang(3)**2)                                    !/*al        114*/
      if(t4.ne.0.)t4=1./t4                                                      !/*al        115*/
      ang(1)=ang(1)*t4                                                          !/*al        116*/
      ang(2)=ang(2)*t4                                                          !/*al        117*/
      ang(3)=ang(3)*t4                                                          !/*al        118*/
      angl=max(-one,min(one,ang(1)*uuu+ang(2)*vvv+ang(3)*www))                  !/*al        119*/
      return                                                                    !/*al        120*/
      end                                                                       !/*al        121*/
