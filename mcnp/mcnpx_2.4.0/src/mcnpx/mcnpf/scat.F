!#ident scat.F from deck mt                                                     !/*mt          1*/
      function scat(ns)                                                         !/*mt          2*/
!        sample a multigroup scattering angle.                                  !/*mt          3*/
!        ns=index of secondary particle.                                        !/*mt          4*/
      use global_data                                                           !/*mtx         1*/
!                                                                               !/*mtx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*mtx         3*/
      implicit integer(kindi) (i-n)                                             !/*mtx         4*/
!                                                                               !/*mtx         5*/
!                                                                               !/*mt          6*/
      l=nint(xss(jxs(ljxs+16,iex)+ns))                                          !/*mtx         6*/
      if(l.eq.0)go to 60                                                        !/*mt          8*/
      l=nint(xss(jxs(ljxs+1,iex)+l+ixcos-2))                                    !/*mtx         7*/
      if(l.eq.0)go to 60                                                        !/*mt         10*/
      lx=jxs(ljxs+1,iex)+nint(xss(jxs(ljxs+17,iex)+ns))+l-2                     !/*mt4c        1*/
      if(ns.eq.0)i=nxs(lnxs+9,iex)                                              !/*mt         12*/
      if(ns.ne.0)i=nint(xss(jxs(ljxs+14,iex)+ns-1))                             !/*mtx         8*/
      if(ns.eq.0)n=nxs(lnxs+3,iex)                                              !/*mt         14*/
      if(ns.ne.0)n=nint(xss(jxs(ljxs+15,iex)+ns-1))                             !/*mtx         9*/
      if(i.ne.0)go to 30                                                        !/*mt         16*/
!                                                                               !/*mt         17*/
!        equi-probable cosine bins.                                             !/*mt         18*/
      if(n.eq.1)go to 10                                                        !/*mt         19*/
      t=rang()*(n-1)                                                            !/*mt         20*/
      j=t                                                                       !/*mt         21*/
      scat=xss(lx+j)+(t-j)*(xss(lx+1+j)-xss(lx+j))                              !/*mt         22*/
      return                                                                    !/*mt         23*/
!                                                                               !/*mt         24*/
!        special treatment for one moment.                                      !/*mt         25*/
   10 if(xss(lx).lt.0.)go to 20                                                 !/*mt         26*/
      scat=1.-2.*rang()*(1.-xss(lx))                                            !/*mt         27*/
      return                                                                    !/*mt         28*/
   20 scat=-1.+2.*rang()*(1.+xss(lx))                                           !/*mt         29*/
      return                                                                    !/*mt         30*/
!                                                                               !/*mt         31*/
!        discrete cosines.                                                      !/*mt         32*/
   30 n=n/2                                                                     !/*mt         33*/
      rn=rang()                                                                 !/*mt         34*/
      do 40 j=1,n                                                               !/*mt         35*/
   40 if(xss(lx-1+j).gt.rn)go to 50                                             !/*mt         36*/
   50 scat=xss(lx-1+j+n)                                                        !/*mt         37*/
      return                                                                    !/*mt         38*/
!                                                                               !/*mt         39*/
!        isotropic.                                                             !/*mt         40*/
   60 scat=-1.+2.*rang()                                                        !/*mt         41*/
      ipsc=104                                                                  !/*mt         42*/
      return                                                                    !/*mt         43*/
      end                                                                       !/*mt         44*/
