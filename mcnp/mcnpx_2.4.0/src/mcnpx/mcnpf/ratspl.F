!#ident ratspl.F from deck rl                                                   !/*rl          1*/
      subroutine ratspl                                                         !/*rl          4*/
#if defined(MCPLOT) && defined(DISSPLA)                                         /*!rlx         1*/
!        draw a rational spline line.  requires disspla.                        !/*rl          5*/
      use global_data                                                           !/*rlx         2*/
      use mcplot_data                                                           !/*rlx         3*/
!                                                                               !/*rlx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*rlx         5*/
      implicit integer(kindi) (i-n)                                             !/*rlx         6*/
!                                                                               !/*rlx         7*/
!                                                                               !/*rl          8*/
      call raspln(real(tensn))                                                  !/*rl          9*/
      j=0                                                                       !/*rl         10*/
      do 20 i=1,npt(1)                                                          !/*rl         11*/
      if(ab1(lab1+i).lt.plim(1).or.ab1(lab1+i).gt.plim(2).or.                   !/*rl         12*/
     & ord(lord+i).lt.plim(3).or.ord(lord+i).gt.plim(4))go to 10                !/*rl         13*/
      j=j+1                                                                     !/*rl         14*/
      xrr(lxrr+j)=ab1(lab1+i)                                                   !/*rl         15*/
      yrr(lyrr+j)=ord(lord+i)                                                   !/*rl         16*/
      if(i.lt.npt(1))go to 20                                                   !/*rl         17*/
   10 if(j.ge.2)call curve(xrr(lxrr+1),yrr(lyrr+1),j,0)                         !/*rl         18*/
      j=0                                                                       !/*rl         19*/
   20 continue                                                                  !/*rl         20*/
#endif                                                                          /*!rlx         8*/
      return                                                                    !/*rl         21*/
      end                                                                       !/*rl         22*/
