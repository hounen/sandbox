!#ident isheet.F from deck ih                                                   !/*ih          1*/
      function isheet(p,iq,js)                                                  !/*ih          3*/
#ifdef PLOT                                                                     /*!ihx         1*/
!        return 1 if point p on curve iq of surface js is on                    !/*ih          4*/
!        the wrong sheet of a one-sheet cone, zero otherwise.                   !/*ih          5*/
      use global_data                                                           !/*ihx         2*/
      use plot_data                                                             !/*ihx         3*/
!                                                                               !/*ihx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*ihx         5*/
      implicit integer(kindi) (i-n)                                             !/*ihx         6*/
!                                                                               !/*ihx         7*/
!                                                                               !/*ih          8*/
      isheet=0                                                                  !/*ih          9*/
      i=max(0,(kst(lkst+js)-13)/3-7*(kst(lkst+js)/22))                          !/*ih4a        1*/
      if(i.eq.0)return                                                          !/*ih         11*/
      d=scf(lsc(llsc+js)+7-2*i)                                                 !/*ih         12*/
      if(d.eq.0.)return                                                         !/*ih         13*/
      call ptost(p,iq,js,s,t)                                                   !/*ih         14*/
      k=kst(lkst+js)-11-3*i                                                     !/*ih4a        2*/
      if(d*(plmx(k,1)+s*extent(1)*plmx(k,2)+t*extent(2)*plmx(k,3)-              !/*ih         16*/
     & scf(lsc(llsc+js)+(2-i)*(k-1)+i-1)).lt.0.)isheet=1                        !/*ih4c        1*/
#else                                                                           /*!ihx         8*/
      isheet=0                                                                  !/*ihx         9*/
#endif                                                                          /*!ihx        10*/
      return                                                                    !/*ih         18*/
      end                                                                       !/*ih         19*/
