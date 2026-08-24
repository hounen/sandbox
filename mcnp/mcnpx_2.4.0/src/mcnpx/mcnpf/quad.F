!#ident quad.F from deck qu                                                     !/*qu          1*/
      subroutine quad(c,j,s1,s2)                                                !/*qu          2*/
!        solve the quadratic equation c.                                        !/*qu          3*/
!        return the j real roots in s1 and s2.                                  !/*qu          4*/
      use global_parameters                                                     !/*qux         1*/
      implicit real(kindr) (a-h,o-z)                                            !/*qux         2*/
      implicit integer(kindi) (i-n)                                             !/*qux         3*/
      dimension c(3)                                                            !/*qu          7*/
!                                                                               !/*qu          8*/
      s1=0.                                                                     !/*qu          9*/
      s2=0.                                                                     !/*qu         10*/
      j=1                                                                       !/*qu         11*/
      if(abs(c(1)).lt.1.e-37)go to 30                                           !/*qu4a        1*/
      q1=-.5*c(2)/c(1)                                                          !/*qu         13*/
      q2=c(3)/c(1)                                                              !/*qu         14*/
      t1=q1**2-q2                                                               !/*qu         15*/
      if(t1.gt.0.)go to 20                                                      !/*qu         16*/
      if(-t1.gt.1.e-6*abs(q1))go to 10                                          !/*qu         17*/
      s1=q1                                                                     !/*qu         18*/
      return                                                                    !/*qu         19*/
   10 if(abs(c(2))+abs(c(3)).gt.1.e-6*abs(c(1)))go to 40                        !/*qu         20*/
      return                                                                    !/*qu         21*/
   20 j=2                                                                       !/*qu         22*/
      if(abs(q2).le.1.e-6*t1)go to 30                                           !/*qu         23*/
      t2=sqrt(t1)                                                               !/*qu         24*/
      s2=q1+t2                                                                  !/*qu         25*/
      s1=q1-t2                                                                  !/*qu         26*/
      return                                                                    !/*qu         27*/
   30 if(abs(c(2)).lt.1.e-37)go to 40                                           !/*qu4a        2*/
      if(j.eq.2)s2=2.*q1                                                        !/*qu         29*/
      s1=-c(3)/c(2)                                                             !/*qu         30*/
      return                                                                    !/*qu         31*/
   40 j=0                                                                       !/*qu         32*/
      return                                                                    !/*qu         33*/
      end                                                                       !/*qu         34*/
