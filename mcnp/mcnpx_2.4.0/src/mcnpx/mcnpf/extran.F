!#ident extran.F from deck ey                                                   !/*ey          1*/
      subroutine extran                                                         !/*ey          2*/
!        adjust the mean free path by the exponential transform.                !/*ey          3*/
      use global_data                                                           !/*eyx         1*/
!                                                                               !/*eyx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*eyx         3*/
      implicit integer(kindi) (i-n)                                             !/*eyx         4*/
!                                                                               !/*eyx         5*/
!                                                                               !/*ey          5*/
      m=int(abs(qax(lqax+ipt,icl)))-4                                           !/*ey          6*/
      p=qax(lqax+ipt,icl)-aint(qax(lqax+ipt,icl))                               !/*ey          7*/
      if(p.eq.0.)p=sign(siga/totm,qax(lqax+ipt,icl))                            !/*ey          8*/
      c=1.                                                                      !/*ey          9*/
      if(m.lt.0)c=gpblcm(7+m)                                                   !/*ey         10*/
      if(m.le.0)go to 10                                                        !/*ey         11*/
      t=sqrt((vec(lvec+1,m)-xxx)**2+(vec(lvec+2,m)-yyy)**2+                     !/*ey         12*/
     & (vec(lvec+3,m)-zzz)**2)                                                  !/*ey         13*/
      if(t.ne.0.)c=(uuu*(vec(lvec+1,m)-xxx)+                                    !/*ey         14*/
     & vvv*(vec(lvec+2,m)-yyy)+www*(vec(lvec+3,m)-zzz))/t                       !/*ey         15*/
   10 qpl=ple*(1.-p*c)                                                          !/*ey         16*/
      return                                                                    !/*ey         17*/
      end                                                                       !/*ey         18*/
