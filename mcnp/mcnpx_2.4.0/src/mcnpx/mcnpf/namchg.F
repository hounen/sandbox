!#ident namchg.F from deck cj                                                   !/*cj          1*/
      function namchg(mm,ji)                                                    !/*cj          2*/
!        return the array index corresponding to the name ji.                   !/*cj          3*/
      use global_data                                                           !/*cjx         1*/
!                                                                               !/*cjx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*cjx         3*/
      implicit integer(kindi) (i-n)                                             !/*cjx         4*/
!                                                                               !/*cjx         5*/
!                                                                               !/*cj          5*/
      go to(10,30,50)mm                                                         !/*cj          6*/
!                                                                               !/*cj          7*/
! >>>>>  mm=1 -- cell                                                           !/*cj          8*/
   10 do 20 namchg=1,mxa                                                        !/*cj          9*/
   20 if(ncl(lncl+namchg).eq.ji)return                                          !/*cj         10*/
      go to 70                                                                  !/*cj         11*/
!                                                                               !/*cj         12*/
! >>>>>  mm=2 -- surface                                                        !/*cj         13*/
   30 do 40 namchg=1,mxj                                                        !/*cj         14*/
   40 if(nsf(lnsf+namchg).eq.abs(ji))return                                     !/*cj         15*/
      go to 70                                                                  !/*cj         16*/
!                                                                               !/*cj         17*/
! >>>>>  mm=3 -- bounding surface                                               !/*cj         18*/
   50 do 60 kk=1,nlja                                                           !/*cj         19*/
      namchg=abs(lja(llja+kk))                                                  !/*cj         20*/
      if(namchg.ge.1000000)go to 60                                             !/*cj         21*/
      if(nsf(lnsf+namchg).eq.abs(ji))return                                     !/*cj         22*/
   60 continue                                                                  !/*cj         23*/
!                                                                               !/*cj         24*/
!        namchg=0 means that no valid name was found.                           !/*cj         25*/
   70 namchg=0                                                                  !/*cj         26*/
      return                                                                    !/*cj         27*/
      end                                                                       !/*cj         28*/
