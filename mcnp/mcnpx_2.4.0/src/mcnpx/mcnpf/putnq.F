!#ident putnq.F from deck pq                                                    !/*pq          1*/
      subroutine putnq(ie)                                                      !/*pq          2*/
!        put the surfaces of the current cell into q-form.                      !/*pq          3*/
!        returns ie=-2 if a non-conforming surface is found.                    !/*pq          4*/
!        a*r**2+b*r+c*s**2+d*s+e    a,b,c,d,e in scfq(lscq+i,j),i=1,5           !/*pq          5*/
      use global_data                                                           !/*pqx         1*/
      use imcn_data                                                             !/*pqx         2*/
!                                                                               !/*pqx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*pqx         4*/
      implicit integer(kindi) (i-n)                                             !/*pqx         5*/
!                                                                               !/*pqx         6*/
      dimension aa(4,4),cc(4,4),dd(4,4)                                         !/*pq          8*/
!                                                                               !/*pq          9*/
      if(dbcn(7).ne.0.)write(iuo,10)                                            !/*pq         10*/
   10 format(/36h q-form coefficients of the surfaces,                          !/*pq         11*/
     & 20h of the current cell)                                                 !/*pq         12*/
!                                                                               !/*pq         13*/
!        do all of the surfaces of the current cell.                            !/*pq         14*/
      do 90 ns=1,nsv                                                            !/*pq         15*/
      jt=ljsv(llsv+ns)                                                          !/*pq         16*/
      kk=kst(lkst+jt)                                                           !/*pq4a        1*/
      ix=lsc(llsc+jt)                                                           !/*pq         18*/
      do 20 i=1,5                                                               !/*pq         19*/
   20 scfq(lscq+i,jt)=0.                                                        !/*pq         20*/
!                                                                               !/*pq         21*/
!        tori                                                                   !/*pq         22*/
      if(kk.le.23)go to 40                                                      !/*pq         23*/
      if(abs(abs(bbb(kk-22,2))-1.).gt.1.e-8)go to 100                           !/*pq         24*/
      do 30 i=1,3                                                               !/*pq         25*/
   30 if(kk.ne.23+i.and.abs(scf(ix+i)-bbb(1+i,1)).gt.1.e-8)go to 100            !/*pq         26*/
      scfq(lscq+1,jt)=1.                                                        !/*pq         27*/
      scfq(lscq+2,jt)=-2.*scf(ix+4)                                             !/*pq         28*/
      scfq(lscq+3,jt)=scf(ix+7)                                                 !/*pq         29*/
      scfq(lscq+4,jt)=-2.*scf(ix+7)*(scf(ix+kk-23)-bbb(kk-22,1))                !/*pq         30*/
      scfq(lscq+5,jt)=scf(ix+7)*(scf(ix+kk-23)-bbb(kk-22,1))**2+                !/*pq         31*/
     & scf(ix+4)**2-scf(ix+6)**2                                                !/*pq         32*/
      go to 70                                                                  !/*pq         33*/
!                                                                               !/*pq         34*/
!        put quadratic surfaces and planes into the form q*aa*q=0.              !/*pq         35*/
   40 call amatrx(jt,aa)                                                        !/*pq         36*/
      do 50 i=2,4                                                               !/*pq         37*/
      aa(i,1)=2.*aa(i,1)                                                        !/*pq         38*/
   50 aa(1,i)=0.                                                                !/*pq         39*/
!                                                                               !/*pq         40*/
!        rotate and translate according to q=bbb*p                              !/*pq         41*/
!        in the p coordinate system the equation of the surface is              !/*pq         42*/
!          p*bbb(t)*aa*bbb*p=0 which becomes p*dd*p=0                           !/*pq         43*/
      call matmpy(4,4,4,aa,4,bbb,4,cc,4,0)                                      !/*pq         44*/
      call matmpy(4,4,4,bbb,4,cc,4,dd,4,4)                                      !/*pq         48*/
      z2=1.e-8*(abs(dd(1,2))+abs(dd(2,1))+abs(dd(2,2))+abs(dd(3,3)))            !/*cor4-2    142*/
      do 60 i=2,4                                                               !/*cor4-2    143*/
      do 60 j=2,4                                                               !/*cor4-2    144*/
   60 if(i.ne.j.and.abs(dd(i,j)+dd(j,i)).gt.z2)go to 100                        !/*cor4-2    145*/
      if(abs(dd(1,3)+dd(3,1))+abs(dd(1,4)+dd(4,1)).gt.z2)go to 100              !/*cor4-2    146*/
      if(abs(dd(3,3)-dd(4,4)).gt.1.e-4*abs(dd(3,3)+dd(4,4)))go to 100           !/*cor4-2    147*/
!                                                                               !/*pq         54*/
!        calculate the q-form coefficients.                                     !/*pq         55*/
      a=abs(dd(3,3))                                                            !/*pq         56*/
      if(a.le.z2)a=abs(dd(2,2))                                                 !/*pq         57*/
      if(a.le.z2.and.abs(dd(1,2)+dd(2,1)).le.1.e-8*abs(dd(1,1)))                !/*pq         58*/
     & go to 100                                                                !/*pq         59*/
      if(a.le.z2)a=abs(dd(1,2)+dd(2,1))                                         !/*pq         60*/
      if(abs(dd(3,3)).gt.z2)scfq(lscq+1,jt)=dd(3,3)/a                           !/*pq         61*/
      if(abs(dd(2,2)).gt.z2.or.abs(dd(3,3)).gt.z2)                              !/*pq         62*/
     & scfq(lscq+3,jt)=dd(2,2)/a                                                !/*pq         63*/
      scfq(lscq+4,jt)=(dd(1,2)+dd(2,1))/a                                       !/*pq         64*/
      scfq(lscq+5,jt)=dd(1,1)/a                                                 !/*pq         65*/
!                                                                               !/*pq         66*/
!        print the q-form coefficients if required.                             !/*pq         67*/
   70 if(dbcn(7).ne.0.)write(iuo,80)nsf(lnsf+jt),(scfq(lscq+i,jt),i=1,5)        !/*pq         68*/
   80 format(i7,1p5e14.5)                                                       !/*pq         69*/
   90 continue                                                                  !/*pq         70*/
      return                                                                    !/*pq         71*/
!                                                                               !/*pq         72*/
!        return with ie=-2 if a non-conforming surface is found.                !/*pq         73*/
  100 if(dbcn(7).ne.0.)write(iuo,110)nsf(lnsf+jt)                               !/*pq         74*/
  110 format(/10h   surface,i7,36h is not rotationally symmetric about,         !/*pq         75*/
     & 36h previously specified symmetry axis.)                                 !/*pq         76*/
      ie=-2                                                                     !/*pq         77*/
      return                                                                    !/*pq         78*/
      end                                                                       !/*pq         79*/
