!#ident broadn.F from deck br                                                   !/*br          1*/
      subroutine broadn                                                         !/*br          2*/
!        adjust the temperature of neutron cross-section table iex.             !/*br          3*/
      use global_data                                                           !/*brx         1*/
!                                                                               !/*brx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*brx         3*/
      implicit integer(kindi) (i-n)                                             !/*brx         4*/
!                                                                               !/*brx         5*/
!                                                                               !/*br          5*/
!        determine the temperature the table should have.                       !/*br          6*/
      k=0                                                                       !/*br          7*/
      th=0.                                                                     !/*br          8*/
      do 40 ic=1,mxa                                                            !/*br          9*/
      j=mat(lmat+ic)                                                            !/*br         10*/
      do 10 m=jmd(ljmd+j),jmd(ljmd+j+1)-1                                       !/*br         11*/
   10 if(lme(llme+1,m).eq.iex)go to 20                                          !/*br         12*/
      go to 40                                                                  !/*br         13*/
   20 if(k.eq.0)t=tmp(ltmp+(ic-1)*mxt+1)                                        !/*br         14*/
      k=1                                                                       !/*br         15*/
      er=.01                                                                    !/*br4c        1*/
      do 30 l=1,mxt                                                             !/*br         16*/
   30 if(abs(tmp(ltmp+(ic-1)*mxt+1)-t).gt.er*t)go to 50                         !/*br4c        3*/
   40 continue                                                                  !/*br         18*/
      if(k.eq.0)return                                                          !/*br         19*/
      th=t                                                                      !/*br         20*/
!                                                                               !/*br         21*/
!        adjust elastic and total cross sections to correct temperature.        !/*br         22*/
   50 if(abs(tbt(ltbt+iex)-th).le.er*th)return                                  !/*br4c        4*/
      b=500.*abs(th-tbt(ltbt+iex))/awn(lawn+iex)                                !/*br         24*/
      do 80 j=1,nxs(lnxs+3,iex)                                                 !/*br         25*/
      l=jxs(ljxs+1,iex)+j-1                                                     !/*br         26*/
      if(xss(l).gt.b)go to 90                                                   !/*br         27*/
      f1=1.                                                                     !/*br         28*/
      if(tbt(ltbt+iex).eq.0.)go to 60                                           !/*br         29*/
      a=sqrt(awn(lawn+iex)*xss(l)/tbt(ltbt+iex))                                !/*br         30*/
      if(a.ge.2.)f1=1.+.5/a**2                                                  !/*br         31*/
      i=a/.04                                                                   !/*br         32*/
      if(a.lt.2.)f1=(thgf(i)+(a/.04-i)*(thgf(i+1)-thgf(i)))/a                   !/*br         33*/
   60 f2=1.                                                                     !/*br         34*/
      if(th.eq.0.)go to 70                                                      !/*br         35*/
      a=sqrt(awn(lawn+iex)*xss(l)/th)                                           !/*br         36*/
      if(a.ge.2.)f2=1.+.5/a**2                                                  !/*br         37*/
      i=a/.04                                                                   !/*br         38*/
      if(a.lt.2.)f2=(thgf(i)+(a/.04-i)*(thgf(i+1)-thgf(i)))/a                   !/*br         39*/
   70 a=xss(l+3*nxs(lnxs+3,iex))*(f2-f1)/f1                                     !/*br         40*/
      xss(l+nxs(lnxs+3,iex))=xss(l+nxs(lnxs+3,iex))+a                           !/*br         41*/
   80 xss(l+3*nxs(lnxs+3,iex))=xss(l+3*nxs(lnxs+3,iex))+a                       !/*br         42*/
   90 tbt(ltbt+iex)=th                                                          !/*br         43*/
      return                                                                    !/*br         44*/
      end                                                                       !/*br         45*/
