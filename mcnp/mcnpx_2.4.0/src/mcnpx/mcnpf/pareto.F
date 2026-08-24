!#ident pareto.F from deck pe4a                                                 !/*pe4a        1*/
      function pareto(ar,x,n)                                                   !/*pe4a        2*/
!        pareto pdf=(1/x(1))*(1.+x(2)*ar(i)/x(1))**(-1./x(2)-1.)                !/*pe4a        3*/
      use global_parameters                                                     !/*pex         1*/
      implicit real(kindr) (a-h,o-z)                                            !/*pex         2*/
      implicit integer(kindi) (i-n)                                             !/*pex         3*/
      dimension x(2),ar(n)                                                      !/*pe4a        6*/
!                                                                               !/*pe4a        7*/
      if(x(1).le.0..or.x(2).eq.0.)go to 20                                      !/*pe4a        8*/
      t1=0.                                                                     !/*pe4a        9*/
      t2=x(2)/x(1)                                                              !/*pe4a       10*/
      do 10 i=1,n                                                               !/*pe4a       11*/
      t3=1.+ar(i)*t2                                                            !/*pe4a       12*/
      if(t3.le.0.)go to 20                                                      !/*pe4a       13*/
   10 t1=t1+log(t3)                                                             !/*pe4a       14*/
      pareto=n*log(x(1))+(1.+1./x(2))*t1                                        !/*pe4a       15*/
      return                                                                    !/*pe4a       16*/
   20 pareto=1.e30                                                              !/*pe4a       17*/
      return                                                                    !/*pe4a       18*/
      end                                                                       !/*pe4a       19*/
