!#ident spol.F from deck sq4c                                                   !/*sq4c        1*/
      function spol(s,x,y,e,u,n,in,id,m1,m2)                                    !/*sq4c        2*/
!        cubic spline interpolation with parabolic runout.                      !/*sq4c        3*/
      use global_parameters                                                     !/*sqx         1*/
      implicit real(kindr) (a-h,o-z)                                            !/*sqx         2*/
      implicit integer(kindi) (i-n)                                             !/*sqx         3*/
      dimension  x(n),y(n),e(n),u(n)                                            !/*sq4c        7*/
      if(in.ne.1)go to 30                                                       !/*sq4c        8*/
      in=2                                                                      !/*sq4c        9*/
      n1=n-1                                                                    !/*sq4c       10*/
      e(1)=one                                                                  !/*sq4c       11*/
      u(1)=0.                                                                   !/*sq4c       12*/
      b1=x(2)-x(1)                                                              !/*sq4c       13*/
      c1=(y(2)-y(1))/b1                                                         !/*sq4c       14*/
      do 10 j=2,n1                                                              !/*sq4c       15*/
      b2=x(j+1)-x(j)                                                            !/*sq4c       16*/
      c2=(y(j+1)-y(j))/b2                                                       !/*sq4c       17*/
      b=x(j+1)-x(j-1)                                                           !/*sq4c       18*/
      d=(c2-c1)/b                                                               !/*sq4c       19*/
      c=b1/b                                                                    !/*sq4c       20*/
      b1=b2                                                                     !/*sq4c       21*/
      c1=c2                                                                     !/*sq4c       22*/
      p=c*e(j-1)+one+one                                                        !/*sq4c       23*/
      e(j)=(c-one)/p                                                            !/*sq4c       24*/
   10 u(j)=(d-c*u(j-1))/p                                                       !/*sq4c       25*/
      e(n)=u(n1)/(one-e(n1))                                                    !/*sq4c       26*/
      do 20 kk=1,n1                                                             !/*sq4c       27*/
      k=n-kk                                                                    !/*sq4c       28*/
   20 e(k)=e(k)*e(k+1)+u(k)                                                     !/*sq4c       29*/
      id=0                                                                      !/*sq4c       30*/
      m2=0                                                                      !/*sq4c       31*/
      m1=n                                                                      !/*sq4c       32*/
      if(x(1).le.x(n))go to 30                                                  !/*sq4c       33*/
      id=1                                                                      !/*sq4c       34*/
      m2=n                                                                      !/*sq4c       35*/
      m1=0                                                                      !/*sq4c       36*/
   30 if(s.lt.x(m1+id).and.s.gt.x(m2+1-id))go to 40                             !/*sq4c       37*/
      if(s.ge.x(m1+id))mu=m1+2*id                                               !/*sq4c       38*/
      if(s.le.x(m2+1-id))mu=m2+2*(1-id)                                         !/*sq4c       39*/
      go to 60                                                                  !/*sq4c       40*/
   40 ml=m2                                                                     !/*sq4c       41*/
      mu=m1                                                                     !/*sq4c       42*/
   50 m3=(ml+mu)/2                                                              !/*sq4c       43*/
      if(s.lt.x(m3))mu=m3                                                       !/*sq4c       44*/
      if(s.ge.x(m3))ml=m3                                                       !/*sq4c       45*/
      if(iabs(mu-ml).gt.1)go to 50                                              !/*sq4c       46*/
      mu=mu+id                                                                  !/*sq4c       47*/
!                                                                               !/*sq4c       48*/
   60 spol=(e(mu-1)*((x(mu)-s)**3)+e(mu)*((s-x(mu-1))**3)+                      !/*sq4c       49*/
     & (y(mu-1)-e(mu-1)*((x(mu)-x(mu-1))**2))*(x(mu)-s)+                        !/*sq4c       50*/
     & (y(mu)-e(mu)*((x(mu)-x(mu-1))**2))*(s-x(mu-1)))/                         !/*sq4c       51*/
     & (x(mu)-x(mu-1))                                                          !/*sq4c       52*/
      return                                                                    !/*sq4c       53*/
      end                                                                       !/*sq4c       54*/
