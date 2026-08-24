!#ident xsec.F from deck xs                                                     !/*xs          1*/
      function xsec(s,dk,wt,eo,po,es,z,ze,zf)                                   !/*xs          2*/
      use global_data                                                           !/*xsx         1*/
!                                                                               !/*xsx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*xsx         3*/
      implicit integer(kindi) (i-n)                                             !/*xsx         4*/
!                                                                               !/*xsx         5*/
      parameter (pf=2.*pie/fscon)                                               !/*xs          4*/
      dimension zf(3)                                                           !/*xs          5*/
!                                                                               !/*xs          6*/
      e=eo-dk                                                                   !/*xs          7*/
      p=sqrt((s-dk+2.)*(s-dk))                                                  !/*xs          8*/
      ey=log(e+p)                                                               !/*xs          9*/
      ee=eo*e                                                                   !/*xs         10*/
      pp=po*p                                                                   !/*xs         11*/
      f1=2.*third*pp**3-ee*(p**2+po**2)*pp+es*e*p**3+                           !/*xs         12*/
     & ey*(eo*po**3-2.*es*pp**2)                                                !/*xs         13*/
      f2=8.*third*ee*pp**3+dk**2*(ee+ee**2+pp**2)*pp+                           !/*xs         14*/
     & dk*((ee+po**2)*es*p**3-(ee+p**2)*ey*po**3)                               !/*xs         15*/
      s4=.5*p*(f1*pp+log((ee+pp-1.)/dk)*f2)/pp**4                               !/*xs         16*/
      g=100.*dk/(ee*zf(1))                                                      !/*xs         17*/
      h1=1.+(e/eo)**2                                                           !/*xs         18*/
      h2=2.*third*e/eo                                                          !/*xs         19*/
      if(g.gt..735)p1=(19.83-4.184*log(g+.7))*(h1-h2)                           !/*xs         20*/
      if(g.le..735)p1=(20.867-g*(4.409-1.156*g))*h1-                            !/*xs         21*/
     & (20.209-g*(2.625+.159*g))*h2                                             !/*xs         22*/
      xsec=(s4+po*(.25*p1-(zf(2)*third+wt*zf(3)+log(2.*ee/dk)-.5)*              !/*xs         23*/
     & (h1-h2)))*ze*e/((1.-exp(-pf*z*e/p))*p*eo)                                !/*xs         24*/
      return                                                                    !/*xs         25*/
      end                                                                       !/*xs         26*/
