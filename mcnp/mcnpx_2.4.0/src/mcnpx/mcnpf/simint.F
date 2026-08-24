!#ident simint.F from deck si4b                                                 !/*si4b        1*/
      function simint(d,f,n)                                                    !/*si4b        2*/
!       integrate a tabular function f(x) from x1 to xn, where f(x) is          !/*si4b        3*/
!       given at n equally-spaced abscissae x1...xn. use a simpson rule         !/*si4b        4*/
!       dependent on the number of points n.  return the integral.              !/*si4b        5*/
!       d = xn - x1 is the distance between the limits of integration.          !/*si4b        6*/
      use global_parameters                                                     !/*six         1*/
      implicit real(kindr) (a-h,o-z)                                            !/*six         2*/
      implicit integer(kindi) (i-n)                                             !/*six         3*/
      dimension f(n)                                                            !/*si4b        9*/
!                                                                               !/*si4b       10*/
      if(n.gt.1)go to 10                                                        !/*si4b       11*/
      simint=0.                                                                 !/*si4b       12*/
      return                                                                    !/*si4b       13*/
   10 if(n.gt.2)go to 20                                                        !/*si4b       14*/
      simint=.5*(f(1)+f(2))*d                                                   !/*si4b       15*/
      return                                                                    !/*si4b       16*/
   20 if(n.gt.3)go to 30                                                        !/*si4b       17*/
      simint=(f(1)+4.*f(2)+f(3))*d/6.                                           !/*si4b       18*/
      return                                                                    !/*si4b       19*/
   30 if(n.gt.4)go to 40                                                        !/*si4b       20*/
      simint=.125*(f(1)+3.*(f(2)+f(3))+f(4))*d                                  !/*si4b       21*/
      return                                                                    !/*si4b       22*/
   40 if(n.eq.(n/2)*2)go to 70                                                  !/*si4b       23*/
      s2=0.                                                                     !/*si4b       24*/
      do 50 i=3,n-2,2                                                           !/*si4b       25*/
   50 s2=s2+f(i)                                                                !/*si4b       26*/
      s4=0.                                                                     !/*si4b       27*/
      do 60 i=2,n-1,2                                                           !/*si4b       28*/
   60 s4=s4+f(i)                                                                !/*si4b       29*/
      simint=(f(1)+4.*s4+2.*s2+f(n))*d/(3*(n-1))                                !/*si4b       30*/
      return                                                                    !/*si4b       31*/
   70 if(n.gt.6)go to 80                                                        !/*si4b       32*/
      simint=(f(1)+3.875*(f(2)+f(5))+2.625*(f(3)+f(4))+f(6))*d/(3*(n-1))        !/*si4b       33*/
      return                                                                    !/*si4b       34*/
   80 s2=0.                                                                     !/*si4b       35*/
      do 90 i=6,n-2,2                                                           !/*si4b       36*/
   90 s2=s2+f(i)                                                                !/*si4b       37*/
      s4=0.                                                                     !/*si4b       38*/
      do 100 i=7,n-1,2                                                          !/*si4b       39*/
  100 s4=s4+f(i)                                                                !/*si4b       40*/
      simint=(f(1)+3.875*(f(2)+f(5))+2.625*(f(3)+f(4))+4.*s4+2.*s2+             !/*si4b       41*/
     & f(n))*d/(3*(n-1))                                                        !/*si4b       42*/
      return                                                                    !/*si4b       43*/
      end                                                                       !/*si4b       44*/
