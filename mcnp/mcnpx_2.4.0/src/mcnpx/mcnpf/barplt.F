!#ident barplt.F from deck bp                                                   !/*bp          1*/
      subroutine barplt                                                         !/*bp          3*/
#ifdef MCPLOT                                                                   /*!bpx         1*/
!        plot the data as bars.                                                 !/*bp          4*/
      use global_data                                                           !/*bpx         2*/
      use mcplot_data                                                           !/*bpx         3*/
!                                                                               !/*bpx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*bpx         5*/
      implicit integer(kindi) (i-n)                                             !/*bpx         6*/
!                                                                               !/*bpx         7*/
      dimension x(4),y(4)                                                       !/*bp          7*/
!                                                                               !/*bp          8*/
      d=(plim(2)-plim(1))/140.                                                  !/*bp          9*/
      y(1)=plim(3)                                                              !/*bp         10*/
      y(4)=y(1)                                                                 !/*bp         11*/
      do 10 i=1,npt(1)                                                          !/*bp         12*/
      y(2)=ord(lord+i)                                                          !/*bp         13*/
      y(3)=y(2)                                                                 !/*bp         14*/
      x(1)=ab1(lab1+i)-d                                                        !/*bp         15*/
      x(2)=x(1)                                                                 !/*bp         16*/
      x(3)=x(2)+2.*d                                                            !/*bp         17*/
      x(4)=x(3)                                                                 !/*bp         18*/
   10 call plin(4,x,y)                                                          !/*bp         19*/
#endif                                                                          /*!bpx         8*/
      return                                                                    !/*bp         20*/
      end                                                                       !/*bp         21*/
