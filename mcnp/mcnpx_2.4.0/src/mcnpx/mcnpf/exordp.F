!#ident exordp.F from deck ej4b                                                 !/*ej4b        1*/
      function exordp(ii,jj)                                                    !/*ej4b        3*/
#ifdef MCPLOT                                                                   /*!ejx         1*/
!        extract ordinate values for photon cross section plot at               !/*ej4b        4*/
!        abscissa value ii, interpolating as necessary for jjth nuclide.        !/*ej4b        5*/
      use global_data                                                           !/*ejx         2*/
      use mcplot_data                                                           !/*ejx         3*/
!                                                                               !/*ejx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*ejx         5*/
      implicit integer(kindi) (i-n)                                             !/*ejx         6*/
!                                                                               !/*ejx         7*/
!                                                                               !/*ej4b        8*/
      exordp=0.                                                                 !/*ej4b        9*/
      mt=-kxspmt                                                                !/*ej4b       10*/
      je=kxspen(lxen+jj)+ii                                                     !/*ej4b       11*/
      jx=kxspxs(lxxs+jj)+ii                                                     !/*ej4b       12*/
      ff=0.                                                                     !/*ej4b       13*/
      if(kxsplt.eq.1)go to 10                                                   !/*ej4b       14*/
      je=kxspen(lxen+jj)+kxspnx(lxnx+mxe1+jj)-1                                 !/*ej4b       15*/
      jx=kxspxs(lxxs+jj)+kxspnx(lxnx+mxe1+jj)-1                                 !/*ej4b       16*/
      ff=(log(ab1(lab1+ii))-xss(je))/(xss(je+1)-xss(je))                        !/*ej4b       17*/
!                                                                               !/*ej4b       18*/
!        mt=1,2,3,4 (incoherent, coherent, photoelectric, pair)                 !/*ej4c2       1*/
   10 go to(30,20,30,30,60,50)mt                                                !/*ej4b       20*/
   20 if(ab1(lab1+ii).gt.1.0001*emcf(2).or.nocoh.ne.0)return                    !/*ej4b       21*/
   30 if(xss(jx).eq.0.)return                                                   !/*ej4b       22*/
      if(ff.gt.0.)go to 40                                                      !/*ej4b       23*/
      exordp=exp(xss(jx))                                                       !/*ej4b       24*/
      return                                                                    !/*ej4b       25*/
   40 if(xss(jx+1).eq.0.)return                                                 !/*ej4b       26*/
      exordp=exp(xss(jx)+ff*(xss(jx+1)-xss(jx)))                                !/*ej4b       27*/
      return                                                                    !/*ej4b       28*/
!                                                                               !/*ej4b       29*/
!        photon heating (not in log format) with coherent scatter.              !/*ej4b       30*/
   50 if(ab1(lab1+ii).gt.1.0001*emcf(2).or.nocoh.ne.0)go to 60                  !/*ej4b       31*/
      exordp=xss(jx)                                                            !/*ej4b       32*/
      if(ff.gt.0.)exordp=xss(jx)*(xss(jx+1)/xss(jx))**ff                        !/*ej4b       33*/
      return                                                                    !/*ej4b       34*/
!                                                                               !/*ej4b       35*/
!        total cross section or heating without coherent scatter.               !/*ej4b       36*/
   60 xt=0.                                                                     !/*ej4b       37*/
      c=0.                                                                      !/*ej4b       38*/
      ne=kxspnx(lxnx+jj)                                                        !/*ej4b       39*/
      do 80 i=1,4                                                               !/*ej4b       40*/
      xe=0.                                                                     !/*ej4b       41*/
      xs=xss(je+i*ne)                                                           !/*ej4b       42*/
      if(xs.eq.0.)go to 80                                                      !/*ej4b       43*/
      if(ff.le.0.)go to 70                                                      !/*ej4b       44*/
      if(xss(je+i*ne+1).eq.0.)go to 80                                          !/*ej4b       45*/
      xs=xss(je+i*ne)+ff*(xss(je+i*ne+1)-xss(je+i*ne))                          !/*ej4b       46*/
   70 xe=exp(xs)                                                                !/*ej4b       47*/
      if(i.ne.2)go to 80                                                        !/*ej4b       48*/
      if(ab1(lab1+ii).le.1.0001*emcf(2).or.nocoh.eq.0)go to 80                  !/*ej4b       49*/
      c=xe                                                                      !/*ej4b       50*/
      xe=0.                                                                     !/*ej4b       51*/
   80 xt=xt+xe                                                                  !/*ej4b       52*/
      if(mt.eq.6)go to 90                                                       !/*ej4b       53*/
      exordp=xt                                                                 !/*ej4b       54*/
      return                                                                    !/*ej4b       55*/
!                                                                               !/*ej4b       56*/
!        adjust photon heating if no coherent scatter.                          !/*ej4b       57*/
   90 exordp=xss(jx)*(xt+c)/xt                                                  !/*ej4b       58*/
      if(ff.gt.0.)exordp=exordp*(xss(jx+1)/xss(jx))**ff                         !/*ej4b       59*/
#else                                                                           /*!ejx         8*/
      exordp=0.                                                                 !/*ejx         9*/
#endif                                                                          /*!ejx        10*/
      return                                                                    !/*ej4b       60*/
      end                                                                       !/*ej4b       61*/
