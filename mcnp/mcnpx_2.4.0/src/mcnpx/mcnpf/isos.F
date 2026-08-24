!#ident isos.F from deck iz                                                     !/*iz          1*/
      subroutine isos(d,l)                                                      !/*iz4b        1*/
!        sample a direction d isotropically.                                    !/*iz          3*/
      use global_parameters                                                     !/*izx         1*/
      implicit real(kindr) (a-h,o-z)                                            !/*izx         2*/
      implicit integer(kindi) (i-n)                                             !/*izx         3*/
      dimension d(3)                                                            !/*iz          5*/
!                                                                               !/*iz          6*/
   10 t1=2.*rang()-1.                                                           !/*iz          7*/
      t2=2.*rang()-1.                                                           !/*iz          8*/
      r=t1**2+t2**2                                                             !/*iz          9*/
      if(r.gt.1.)go to 10                                                       !/*iz         10*/
      d(1)=2.*r-1.                                                              !/*iz         11*/
      t3=sqrt((1.-d(1)**2)/r)                                                   !/*iz         12*/
      d(2)=t1*t3                                                                !/*iz         13*/
      d(3)=t2*t3                                                                !/*iz         14*/
      if(l.ne.0)call uplev(d)                                                   !/*iz4b        2*/
      return                                                                    !/*iz         15*/
      end                                                                       !/*iz         16*/
