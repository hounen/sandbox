!#ident amatrx.F from deck am                                                   !/*am          1*/
      subroutine amatrx(js,aa)                                                  !/*am          2*/
!        put surface js into the form  (1,x,y,z)*aa(4x4)*(1,x,y,z)t=0           !/*am4b        1*/
      use global_data                                                           !/*amx         1*/
!                                                                               !/*amx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*amx         3*/
      implicit integer(kindi) (i-n)                                             !/*amx         4*/
!                                                                               !/*amx         5*/
      dimension aa(4,4)                                                         !/*am          5*/
!                                                                               !/*am          6*/
      do 10 j=1,4                                                               !/*am          7*/
      do 10 i=1,4                                                               !/*am          8*/
   10 aa(i,j)=0.                                                                !/*am          9*/
      l=lsc(llsc+js)+1                                                          !/*am         10*/
      k=kst(lkst+js)                                                            !/*am4a        1*/
      do 20 i=2,4                                                               !/*am         12*/
   20 if(k.ge.5.and.k.le.21)aa(i,i)=1.                                          !/*am         13*/
      go to(30,40,40,40,50,60,70,70,70,80,80,80,90,90,90,                       !/*am         14*/
     & 100,100,100,110,110,110,120,130)k                                        !/*am         15*/
!                                                                               !/*am         16*/
! >>>>>  p                                                                      !/*am         17*/
   30 aa(1,1)=-scf(l+3)                                                         !/*am         18*/
      aa(1,2)=.5*scf(l)                                                         !/*am         19*/
      aa(1,3)=.5*scf(l+1)                                                       !/*am         20*/
      aa(1,4)=.5*scf(l+2)                                                       !/*am         21*/
      go to 140                                                                 !/*am         22*/
!                                                                               !/*am         23*/
! >>>>>  px py pz                                                               !/*am         24*/
   40 aa(1,1)=-scf(l)                                                           !/*am         25*/
      aa(1,k)=.5                                                                !/*am         26*/
      go to 140                                                                 !/*am         27*/
!                                                                               !/*am         28*/
! >>>>>  so                                                                     !/*am         29*/
   50 aa(1,1)=-scf(l)                                                           !/*am         30*/
      go to 140                                                                 !/*am         31*/
!                                                                               !/*am         32*/
! >>>>>  s                                                                      !/*am         33*/
   60 aa(1,1)=scf(l)**2+scf(l+1)**2+scf(l+2)**2-scf(l+3)                        !/*am         34*/
      aa(1,2)=-scf(l)                                                           !/*am         35*/
      aa(1,3)=-scf(l+1)                                                         !/*am         36*/
      aa(1,4)=-scf(l+2)                                                         !/*am         37*/
      go to 140                                                                 !/*am         38*/
!                                                                               !/*am         39*/
! >>>>>  sx sy sz                                                               !/*am         40*/
   70 aa(1,1)=scf(l)**2-scf(l+1)                                                !/*am         41*/
      aa(1,k-5)=-scf(l)                                                         !/*am         42*/
      go to 140                                                                 !/*am         43*/
!                                                                               !/*am         44*/
! >>>>>  c/x c/y c/z                                                            !/*am         45*/
   80 aa(1,1)=scf(l)**2+scf(l+1)**2-scf(l+2)                                    !/*am         46*/
      aa(1,2)=-scf(l)                                                           !/*am         47*/
      aa(1,3)=-scf(l+k/12)                                                      !/*am         48*/
      aa(1,4)=-scf(l+1)                                                         !/*am         49*/
      aa(1,k-8)=0.                                                              !/*am         50*/
      aa(k-8,k-8)=0.                                                            !/*am         51*/
      go to 140                                                                 !/*am         52*/
!                                                                               !/*am         53*/
! >>>>>  cx cy cz                                                               !/*am         54*/
   90 aa(1,1)=-scf(l)                                                           !/*am         55*/
      aa(k-11,k-11)=0.                                                          !/*am         56*/
      go to 140                                                                 !/*am         57*/
!                                                                               !/*am         58*/
! >>>>>  k/x k/y k/z                                                            !/*am         59*/
  100 aa(1,1)=-(1.+scf(l+3))*scf(l+k-16)**2+scf(l)**2+scf(l+1)**2+              !/*am         60*/
     & scf(l+2)**2                                                              !/*am         61*/
      aa(1,2)=-scf(l)                                                           !/*am         62*/
      aa(1,3)=-scf(l+1)                                                         !/*am         63*/
      aa(1,4)=-scf(l+2)                                                         !/*am         64*/
      aa(1,k-14)=scf(l+k-16)*scf(l+3)                                           !/*am         65*/
      aa(k-14,k-14)=-scf(l+3)                                                   !/*am         66*/
      go to 140                                                                 !/*am         67*/
!                                                                               !/*am         68*/
! >>>>>  kx ky kz                                                               !/*am         69*/
  110 aa(1,1)=-scf(l+1)*scf(l)**2                                               !/*am         70*/
      aa(1,k-17)=scf(l)*scf(l+1)                                                !/*am         71*/
      aa(k-17,k-17)=-scf(l+1)                                                   !/*am         72*/
      go to 140                                                                 !/*am         73*/
!                                                                               !/*am         74*/
! >>>>>  sq                                                                     !/*am         75*/
  120 aa(1,1)=scf(l)*scf(l+7)**2+scf(l+1)*scf(l+8)**2+scf(l+2)*                 !/*am         76*/
     & scf(l+9)**2-2.*(scf(l+3)*scf(l+7)+scf(l+4)*scf(l+8)+                     !/*am         77*/
     & scf(l+5)*scf(l+9))+scf(l+6)                                              !/*am         78*/
      aa(1,2)=-scf(l)*scf(l+7)+scf(l+3)                                         !/*am         79*/
      aa(1,3)=-scf(l+1)*scf(l+8)+scf(l+4)                                       !/*am         80*/
      aa(1,4)=-scf(l+2)*scf(l+9)+scf(l+5)                                       !/*am         81*/
      aa(2,2)=scf(l)                                                            !/*am         82*/
      aa(3,3)=scf(l+1)                                                          !/*am         83*/
      aa(4,4)=scf(l+2)                                                          !/*am         84*/
      go to 140                                                                 !/*am         85*/
!                                                                               !/*am         86*/
! >>>>>  gq                                                                     !/*am         87*/
  130 aa(1,1)=scf(l+9)                                                          !/*am         88*/
      aa(1,2)=.5*scf(l+6)                                                       !/*am         89*/
      aa(1,3)=.5*scf(l+7)                                                       !/*am         90*/
      aa(1,4)=.5*scf(l+8)                                                       !/*am         91*/
      aa(2,2)=scf(l)                                                            !/*am         92*/
      aa(2,3)=.5*scf(l+3)                                                       !/*am         93*/
      aa(2,4)=.5*scf(l+5)                                                       !/*am         94*/
      aa(3,3)=scf(l+1)                                                          !/*am         95*/
      aa(3,4)=.5*scf(l+4)                                                       !/*am         96*/
      aa(4,4)=scf(l+2)                                                          !/*am         97*/
!                                                                               !/*am         98*/
!        complete the symmetric aa matrix.                                      !/*am         99*/
  140 do 150 i=2,4                                                              !/*am        100*/
      do 150 j=2,i                                                              !/*am        101*/
  150 aa(i,j-1)=aa(j-1,i)                                                       !/*am        102*/
      return                                                                    !/*am        103*/
      end                                                                       !/*am        104*/
