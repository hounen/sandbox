!#ident action.F from deck az                                                   !/*az          1*/
      subroutine action                                                         !/*az          2*/
!        print cell activity.                                                   !/*az4c2       1*/
      use global_data                                                           !/*azx         1*/
!                                                                               !/*azx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*azx         3*/
      implicit integer(kindi) (i-n)                                             !/*azx         4*/
!                                                                               !/*azx         5*/
      character hc(2)*10                                                        !/*azx         6*/
      data hc/'collisions',' substeps'/                                         !/*azx         7*/
!                                                                               !/*az          9*/
!        print the table of activity in each cell of the problem.               !/*az         10*/
      do 70 ip=1,mipt                                                           !/*az         11*/
      if(kpt(ip).eq.0)go to 70                                                  !/*az         12*/
      if(charge(ip).eq.zero)write(iuo,10)hnp(ip),hc(1),hc(1)                    !/*azx         8*/
      if(charge(ip).ne.zero)write(iuo,10)hnp(ip),hc(2),hc(2)                    !/*azx         9*/
   10 format(1h1,a11,22h activity in each cell,70x,15hprint table 126//         !/*azx        10*/
     & 17x,6htracks,5x,10hpopulation,3x,a10,3x,                                 !/*az         15*/
     & a10,5x,6hnumber,8x,4hflux,8x,7haverage,6x,7haverage/                     !/*az         16*/
     & 8x,4hcell,4x,8hentering,31x,8h* weight,2(5x,8hweighted),                 !/*az         17*/
     & 3x,5htrack,19h weight   track mfp/52x,13h(per history),4x,               !/*az         18*/
     & 6henergy,7x,6henergy,5x,10h(relative),6x,4h(cm)/)                        !/*az         19*/
      do 20 j=1,8                                                               !/*az         20*/
   20 tpp(j)=0.                                                                 !/*az         21*/
      do 50 i=1,mxa                                                             !/*az         22*/
      if(fim(lfim+ip,i).eq.0.)go to 50                                          !/*az         23*/
      do 30 j=1,4                                                               !/*az         24*/
      tpp(j)=tpp(j)+pac(lpac+ip,j,i)                                            !/*az         25*/
   30 tpp(j+4)=0.                                                               !/*az         26*/
      if(pac(lpac+ip,9,i).ne.0.)tpp(5)=pac(lpac+ip,5,i)/pac(lpac+ip,9,i)        !/*az         27*/
      if(pac(lpac+ip,10,i).ne.0.)tpp(6)=pac(lpac+ip,6,i)/                       !/*az         28*/
     & pac(lpac+ip,10,i)                                                        !/*az         29*/
      if(pac(lpac+ip,7,i).ne.0.)tpp(7)=pac(lpac+ip,10,i)*fim(lfim+ip,i)         !/*az4a        3*/
     & /pac(lpac+ip,7,i)                                                        !/*az4a        4*/
      if(pac(lpac+ip,10,i).ne.0.)tpp(8)=pac(lpac+ip,8,i)/                       !/*az         32*/
     & pac(lpac+ip,10,i)                                                        !/*az         33*/
      write(iuo,40)i,ncl(lncl+i),(pac(lpac+ip,j,i),j=1,3),                      !/*az4a        5*/
     & pac(lpac+ip,4,i)*fpi,(tpp(j),j=5,8)                                      !/*az4a        6*/
   40 format(2i6,f13.0,tl1,f14.0,tl1,f14.0,tl1,1h ,1p5e13.4)                    !/*az4a        7*/
   50 continue                                                                  !/*az         37*/
      write(iuo,60)tpp(1),tpp(2),tpp(3),tpp(4)*fpi                              !/*az4a        8*/
   60 format(/5x,5htotal,f15.0,tl1,f14.0,tl1,f14.0,tl1,1pe14.4)                 !/*az4a        9*/
!                                                                               !/*az4c2       2*/
!        print the repeated structure/lattice table.                            !/*az4c2       3*/
      if(ink(128).ne.0)call mapmaz(ip)                                          !/*az4b        1*/
   70 continue                                                                  !/*az         40*/
      return                                                                    !/*az        212*/
      end                                                                       !/*az        213*/
