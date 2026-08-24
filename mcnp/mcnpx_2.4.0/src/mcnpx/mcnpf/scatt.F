!#ident scatt.F from deck sa                                                    !/*sa          1*/
      subroutine scatt(az)                                                      !/*sa          2*/
!        calculate electron scattering angle distributions.                     !/*sa          3*/
!        az = average atomic number                                             !/*sa          4*/
      use global_data                                                           !/*sax         1*/
!                                                                               !/*sax         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*sax         3*/
      implicit integer(kindi) (i-n)                                             !/*sax         4*/
!                                                                               !/*sax         5*/
      dimension p(240),h(240)                                                   !/*sa          6*/
!                                                                               !/*sa          7*/
!        prepare zz/z(z+1) correction factor.                                   !/*sa          8*/
      cr=0.                                                                     !/*sa          9*/
      cd=0.                                                                     !/*sa         10*/
      do 10 j=jmd(ljmd+mkc),jmd(ljmd+mkc+1)-1                                   !/*sa         11*/
      z=iza(liza+j)/1000                                                        !/*sa         12*/
      cr=cr+z**2*fme(lfme+j)                                                    !/*sa         13*/
   10 cd=cd+(z+1.)*z*fme(lfme+j)                                                !/*sa         14*/
      cr=cr/cd                                                                  !/*sa         15*/
!                                                                               !/*sa         16*/
      do 110 n=1,nee(3)-1                                                       !/*sax         6*/
      cm=sqrt(1.-4./((eee(leee+3,n)+eee(leee+3,n+1))/gpt(3)+8.))                !/*sax         7*/
      f=0.                                                                      !/*sa         19*/
      v=1.                                                                      !/*sa         20*/
      e1=eee(leee+3,n)+eee(leee+3,n+1)                                          !/*sax         8*/
      e2=(eee(leee+3,n)-eee(leee+3,n+1))/nsb(lnsb+mkc)                          !/*sax         9*/
      r1=rng(lrng+3,n+nee_max*(mkc-1))+rng(lrng+3,n+1+nee_max*(mkc-1))          !/*sax        10*/
      r2=drs(ldrs+3,n+nee_max*(mkc-1))                                          !/*sax        11*/
      lx=0                                                                      !/*sa         25*/
      do 20 i=1,240                                                             !/*sa         26*/
   20 p(i)=0.                                                                   !/*sa         27*/
      call sing(1,.5*(e1+e2),.5*(r1+r2),p,lx)                                   !/*sa         28*/
      call sing(2,.5*(e1-e2),.5*(r1-r2),p,lx)                                   !/*sa         29*/
      ta=p(1)*(r1-r2)/(p(2)*(r1+r2))                                            !/*sa         30*/
      xf=2.*r2*p(1)/(p(2)*(r1+r2)-p(1)*(r1-r2))                                 !/*sa         31*/
      hm=ta**(xf*p(lx))                                                         !/*sa         32*/
      h(1)=.5*(1.-hm)                                                           !/*sa         33*/
      do 30 lm=2,lx                                                             !/*sa         34*/
      h(lm)=(ta**(xf*p(lm))-hm)*(lm-.5)                                         !/*sa         35*/
   30 if(h(lm).lt.1e-6*(lm-.5))go to 40                                         !/*sa         36*/
   40 continue                                                                  !/*sa4b        2*/
!                                                                               !/*sa         37*/
!        calculate the cumulative scattering distribution as the sum of         !/*sa         38*/
!        the goudsmit-saunderson legendre series.                               !/*sa         39*/
      do 80 m=2,maxi                                                            !/*sa4b        6*/
      b=1.-calph(m)                                                             !/*sa4b        7*/
      c=.5*(1.-calph(m)**2)                                                     !/*sa4b        8*/
      egg(legg+m,n+nee(3)*(mkc-1))=h(1)*b+h(2)*c                                !/*sax        12*/
      do 80 l=3,lm-1                                                            !/*sa4b       10*/
      a=b                                                                       !/*sa4b       11*/
      b=c                                                                       !/*sa4b       12*/
      c=((2*l-3)*calph(m)*b-(l-3)*a)/l                                          !/*sa4b       13*/
   80 egg(legg+m,n+nee(3)*(mkc-1))=egg(legg+m,n+nee(3)*(mkc-1))+h(l)*c          !/*sax        13*/
      gm=egg(legg+maxi,n+nee(3)*(mkc-1))                                        !/*sax        14*/
!                                                                               !/*sa4b       16*/
!        apply zz/z(z+1) correction.                                            !/*sa4b       17*/
      do 90 m=2,maxi                                                            !/*sa4b       18*/
      d=egg(legg+m,n+nee(3)*(mkc-1))-egg(legg+m-1,n+nee(3)*(mkc-1))             !/*sax        15*/
      egg(legg+m-1,n+nee(3)*(mkc-1))=f                                          !/*sax        16*/
      if(cm.ge.calph(m).and.cm.lt.calph(m-1))                                   !/*sa4b       21*/
     & v=1.-(1.-cr)*(cm-calph(m))/(calph(m-1)-calph(m))                         !/*sa4b       22*/
      if(cm.ge.calph(m-1))v=cr                                                  !/*sa4b       23*/
   90 f=f+d*v                                                                   !/*sa4b       24*/
      egg(legg+maxi,n+nee(3)*(mkc-1))=f                                         !/*sax        17*/
      do 100 m=2,maxi                                                           !/*sa4b       26*/
  100 egg(legg+m,n+nee(3)*(mkc-1))=gm*egg(legg+m,n+nee(3)*(mkc-1))/f            !/*sax        18*/
  110 continue                                                                  !/*sa4b       29*/
      return                                                                    !/*sa         60*/
      end                                                                       !/*sa         61*/
