!#ident qpol.F from deck qp                                                     !/*qp          1*/
      function qpol(s,x,y,n)                                                    !/*qp          2*/
!        interpolate quadratically in tabular function y(x), whose              !/*qp          3*/
!        length is n, to get qpol, which is the value of y at x=s.              !/*qp          4*/
      use global_parameters                                                     !/*qpx         1*/
      implicit real(kindr) (a-h,o-z)                                            !/*qpx         2*/
      implicit integer(kindi) (i-n)                                             !/*qpx         3*/
      dimension x(n),y(n)                                                       !/*qp          7*/
!                                                                               !/*qp          8*/
      if(n.eq.2)go to 80                                                        !/*qp          9*/
      i=1                                                                       !/*qp         10*/
      if(x(1).gt.x(n))go to 30                                                  !/*qp         11*/
      if(s.le.x(2))go to 70                                                     !/*qp         12*/
      if(s.ge.x(n-1))go to 60                                                   !/*qp         13*/
      do 10 i=2,n-2                                                             !/*qp         14*/
   10 if(s.lt.x(i+1))go to 20                                                   !/*qp         15*/
   20 if(s.le..5*(x(i-1)+x(i+2)))i=i-1                                          !/*qp         16*/
      go to 70                                                                  !/*qp         17*/
   30 if(s.ge.x(2))go to 70                                                     !/*qp         18*/
      if(s.le.x(n-1))go to 60                                                   !/*qp         19*/
      do 40 i=2,n-2                                                             !/*qp         20*/
   40 if(s.gt.x(i+1))go to 50                                                   !/*qp         21*/
   50 if(s.ge..5*(x(i-1)+x(i+2)))i=i-1                                          !/*qp         22*/
      go to 70                                                                  !/*qp         23*/
   60 i=n-2                                                                     !/*qp         24*/
   70 qpol=(s-x(i+1))*(s-x(i+2))*y(i)/((x(i)-x(i+1))*(x(i)-x(i+2)))+            !/*qp         25*/
     & (s-x(i))*(s-x(i+2))*y(i+1)/((x(i)-x(i+1))*(x(i+2)-x(i+1)))+              !/*qp         26*/
     & (s-x(i))*(s-x(i+1))*y(i+2)/((x(i+2)-x(i))*(x(i+2)-x(i+1)))               !/*qp         27*/
      return                                                                    !/*qp         28*/
   80 qpol=y(1)+(s-x(1))*(y(2)-y(1))/(x(2)-x(1))                                !/*qp         29*/
      return                                                                    !/*qp         30*/
      end                                                                       !/*qp         31*/
