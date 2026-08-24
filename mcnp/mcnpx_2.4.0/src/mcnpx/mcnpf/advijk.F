!#ident advijk.F from deck aj                                                   !/*aj          1*/
      subroutine advijk                                                         !/*aj          2*/
!        advance source random number, rijk, for the next history.              !/*aj          3*/
!        ranj is the lower 24 bits of rijk, rani is the upper 24 bits.          !/*aj          4*/
      use global_data                                                           !/*ajx         1*/
!                                                                               !/*ajx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*ajx         3*/
      implicit integer(kindi) (i-n)                                             !/*ajx         4*/
!                                                                               !/*ajx         5*/
      parameter (p=2d0**24,q=2d0**(-24))                                        !/*aj          8*/
!                                                                               !/*aj          9*/
      a=rnfs*ranj                                                               !/*aj         10*/
      b=(rnfb*ranj-aint(rnfb*ranj*q)*p)                                         !/*aj         11*/
     & +(rnfs*rani-aint(rnfs*rani*q)*p)+aint(a*q)                               !/*aj         12*/
      ranj=a-aint(a*q)*p                                                        !/*aj         13*/
      rani=b-aint(b*q)*p                                                        !/*aj         14*/
      rijk=rani*p+ranj                                                          !/*aj         15*/
      return                                                                    !/*aj         16*/
      end                                                                       !/*aj         17*/
