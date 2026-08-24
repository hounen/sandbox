!#ident rotas.F from deck rz                                                    !/*rz          1*/
      subroutine rotas(c,a,d,l,ir)                                              !/*rz4b        1*/
!        sample a direction d at an angle arccos(c) from axis a and             !/*rz          3*/
!        at an azimuthal angle sampled uniformly.                               !/*rz          4*/
      use global_parameters                                                     !/*rzx         1*/
      implicit real(kindr) (a-h,o-z)                                            !/*rzx         2*/
      implicit integer(kindi) (i-n)                                             !/*rzx         3*/
      character h*16                                                            !/*rz          6*/
      dimension a(3),d(3)                                                       !/*rz          7*/
!                                                                               !/*rz          8*/
      if(abs(c).ge.1.)go to 30                                                  !/*rz          9*/
   10 t1=2.*rang()-1.                                                           !/*rz         10*/
      t2=2.*rang()-1.                                                           !/*rz         11*/
      r=t1**2+t2**2                                                             !/*rz         12*/
      if(r.gt.1.)go to 10                                                       !/*rz         13*/
      r=sqrt((1.-c**2)/r)                                                       !/*rz         14*/
      t1=t1*r                                                                   !/*rz         15*/
      t2=t2*r                                                                   !/*rz         16*/
      if(abs(a(3)).gt..9)go to 20                                               !/*rz         17*/
      s=sqrt(a(1)**2+a(2)**2)                                                   !/*rz         18*/
      t=1./s                                                                    !/*rz         19*/
      d(1)=a(1)*c+(t1*a(1)*a(3)-t2*a(2))*t                                      !/*rz         20*/
      d(2)=a(2)*c+(t1*a(2)*a(3)+t2*a(1))*t                                      !/*rz         21*/
      d(3)=a(3)*c-t1*s                                                          !/*rz         22*/
      if(l.ne.0)call uplev(d)                                                   !/*rz4b        2*/
!                                                                               !/*rz         23*/
!        renormalize every 50 calls to prevent error buildup.                   !/*rz         24*/
      ir=ir-1                                                                   !/*rz         25*/
      if(ir.ne.0)return                                                         !/*rz         26*/
      ir=50                                                                     !/*rz         27*/
      s=1./sqrt(d(1)**2+d(2)**2+d(3)**2)                                        !/*rz         28*/
      d(1)=d(1)*s                                                               !/*rz         29*/
      d(2)=d(2)*s                                                               !/*rz         30*/
      d(3)=d(3)*s                                                               !/*rz         31*/
      if(l.ne.0)call uplev(d)                                                   !/*rz4b        3*/
      return                                                                    !/*rz         32*/
!                                                                               !/*rz         33*/
!        special handling for the case of exceptionally large a(3).             !/*rz         34*/
   20 s=sqrt(a(1)**2+a(3)**2)                                                   !/*rz         35*/
      t=1./s                                                                    !/*rz         36*/
      d(1)=a(1)*c+(t1*a(1)*a(2)+t2*a(3))*t                                      !/*rz         37*/
      d(2)=a(2)*c-t1*s                                                          !/*rz         38*/
      d(3)=a(3)*c+(t1*a(3)*a(2)-t2*a(1))*t                                      !/*rz         39*/
      if(l.ne.0)call uplev(d)                                                   !/*rz4b        4*/
      return                                                                    !/*rz         40*/
!                                                                               !/*rz         41*/
!        special handling for abs(c)=1. or more.                                !/*rz         42*/
   30 if(abs(c).gt.1.0001)go to 40                                              !/*rz         43*/
      d(1)=c*a(1)                                                               !/*rz         44*/
      d(2)=c*a(2)                                                               !/*rz         45*/
      d(3)=c*a(3)                                                               !/*rz         46*/
      if(l.ne.0)call uplev(d)                                                   !/*rz4b        5*/
      return                                                                    !/*rz         47*/
   40 write(h,'(f16.10)')c                                                      !/*rz         48*/
      call expirx(1,'rotas','cosine ='//h)                                      !/*rz4b        6*/
      return                                                                    !/*rz         50*/
      end                                                                       !/*rz         51*/
