!#ident arbobv.F from deck ob                                                   !/*ob          1*/
      subroutine arbobv(a,b,c,ie)                                               !/*ob          2*/
!        provide 2 arbitrary base vectors b and c perpendicular to a.           !/*ob          3*/
!        the length of a is assumed to be 1.                                    !/*ob          4*/
!        make abc a right-handed system.  return with ie=1 if a=0.              !/*ob          5*/
!        b is made catawampous to avoid singularities.                          !/*ob          6*/
      use global_parameters                                                     !/*obx         1*/
      implicit real(kindr) (a-h,o-z)                                            !/*obx         2*/
      implicit integer(kindi) (i-n)                                             !/*obx         3*/
      dimension a(3),b(3),c(3)                                                  !/*ob          9*/
!                                                                               !/*ob         10*/
      ie=1                                                                      !/*ob         11*/
      j=1                                                                       !/*ob         12*/
      if(abs(a(2)).gt.abs(a(1)))j=2                                             !/*ob         13*/
      if(abs(a(3)).gt.abs(a(j)))j=3                                             !/*ob         14*/
      k=mod(j,3)+1                                                              !/*ob         15*/
      l=6-j-k                                                                   !/*ob         16*/
      t=a(j)**2+a(k)**2                                                         !/*ob         17*/
      if(t.eq.0.)return                                                         !/*ob         18*/
      b(l)=.0637922                                                             !/*ob         19*/
      b(k)=(a(j)*sqrt(t-(t+a(l)**2)*b(l)**2)-a(k)*a(l)*b(l))/t                  !/*ob         20*/
      b(j)=-(b(k)*a(k)+b(l)*a(l))/a(j)                                          !/*ob         21*/
      call crspro(a,b,c)                                                        !/*ob         22*/
      ie=0                                                                      !/*ob         23*/
      return                                                                    !/*ob         24*/
      end                                                                       !/*ob         25*/
