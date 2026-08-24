!#ident trfsrf.F from deck ss                                                   !/*ss          1*/
      subroutine trfsrf(js)                                                     !/*ss          2*/
!        transform surface js into the xyz coordinate system.                   !/*ss          3*/
      use global_data                                                           !/*ssx         1*/
      use imcn_data                                                             !/*ssx         2*/
!                                                                               !/*ssx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*ssx         4*/
      implicit integer(kindi) (i-n)                                             !/*ssx         5*/
!                                                                               !/*ssx         6*/
      dimension tm(4,4),aa(4,4),nc(23)                                          !/*ss          6*/
      data tm/1.,15*0./,nc/4,4*1,4,3*2,3*3,3*1,3*5,3*3,2*10/                    !/*ss          7*/
!                                                                               !/*ss          8*/
      do 10 jt=1,mxtr                                                           !/*ss          9*/
   10 if(abs(trf(ltrf+1,jt)).eq.jtr(ljtr+js))go to 20                           !/*ss         10*/
      call erprnt(1,1,2,jtr(ljtr+js),nsf(lnsf+js),0,0,0,                        !/*ss         11*/
     & '14htransformation,i5,12h for surface,i7,11h is absent.')                !/*ss         12*/
      return                                                                    !/*ss         13*/
   20 if(trf(ltrf+14,jt).eq.-2.)return                                          !/*ss         14*/
      ks=kst(lkst+js)                                                           !/*ss4a        1*/
      if(abs(ks-25).le.1)go to 180                                              !/*ss         16*/
!                                                                               !/*ss         17*/
!        generate and transform the aa matrix.                                  !/*ss         18*/
      do 30 i=1,4                                                               !/*ss         19*/
      do 30 j=2,4                                                               !/*ss         20*/
   30 tm(j,i)=trf(ltrf+3*i+j-3,jt)                                              !/*ss         21*/
      call amatrx(js,aa)                                                        !/*ss         22*/
      tpp(18)=aa(2,2)                                                           !/*ss         23*/
      tpp(19)=aa(3,3)                                                           !/*ss         24*/
      tpp(20)=aa(4,4)                                                           !/*ss         25*/
      call matmpy(4,4,4,aa,4,tm,4,tpp,4,0)                                      !/*ss         26*/
      call matmpy(4,4,4,tm,4,tpp,4,aa,4,4)                                      !/*ss         27*/
!                                                                               !/*ss         28*/
!        set up to convert to the simplest possible surface.                    !/*ss         29*/
!        zero out diagonal terms where possible.                                !/*ss         30*/
      ii=3                                                                      !/*ss         31*/
      ic=0                                                                      !/*ss         32*/
      im=1                                                                      !/*ss         33*/
      do 40 i=1,3                                                               !/*ss         34*/
      if(tpp(i+17).eq.0.)ic=ic+1                                                !/*ss         35*/
      if(abs(aa(i+1,i+1)).gt.abs(aa(ii+1,ii+1)))ii=i                            !/*ss         36*/
   40 if(abs(aa(i+1,i+1)).lt.abs(aa(im+1,im+1)))im=i                            !/*ss         37*/
      if(trf(ltrf+1,jt).gt.0..and.ic.ne.3)ic=0                                  !/*ss         38*/
      if(ic.gt.0)aa(im+1,im+1)=0.                                               !/*ss         39*/
      if(ic.gt.1)aa(7-im-ii,7-im-ii)=0.                                         !/*ss         40*/
      if(ic.gt.2)aa(ii+1,ii+1)=0.                                               !/*ss         41*/
!                                                                               !/*ss         42*/
!        put gq coefficients into tpp(1-10) and attempt to put                  !/*ss         43*/
!        sq coefficients into tpp(11-20)                                        !/*ss         44*/
      tpp(17)=aa(1,1)                                                           !/*ss         45*/
      tpp(10)=aa(1,1)                                                           !/*ss         46*/
      do 50 i=1,3                                                               !/*ss         47*/
      tpp(i+13)=0.                                                              !/*ss         48*/
      tpp(i+17)=0.                                                              !/*ss         49*/
      tpp(i)=aa(i+1,i+1)                                                        !/*ss         50*/
      tpp(i+10)=aa(i+1,i+1)                                                     !/*ss         51*/
      tpp(i+6)=2.*aa(1,i+1)                                                     !/*ss         52*/
      if(aa(i+1,i+1).ne.0.)tpp(i+17)=-aa(1,i+1)/aa(i+1,i+1)                     !/*ss         53*/
      if(aa(i+1,i+1).eq.0.)tpp(i+13)=aa(1,i+1)                                  !/*ss         54*/
   50 tpp(17)=tpp(17)-aa(i+1,i+1)*tpp(i+17)**2                                  !/*ss         55*/
!                                                                               !/*ss         56*/
!        use gq surface if necessary.                                           !/*ss         57*/
      if(ks.lt.10.or.ks.lt.23.and.trf(ltrf+1,jt).lt.0.)go to 60                 !/*ss         58*/
      if(ks.ge.16.and.ks.le.21.and.scf(lsc(llsc+js)+nc(ks)).ne.0.)              !/*ss         59*/
     & call erprnt(1,1,1,nsf(lnsf+js),0,0,0,0,                                  !/*ss         60*/
     & '22hone-sheet cone surface,i7,27h has a skew transformation.')           !/*ss         61*/
      ks=23                                                                     !/*ss         62*/
      tpp(4)=2.*aa(2,3)                                                         !/*ss         63*/
      tpp(5)=2.*aa(3,4)                                                         !/*ss         64*/
      tpp(6)=2.*aa(2,4)                                                         !/*ss         65*/
      if(aa(2,3).ne.0..or.aa(3,4).ne.0..or.aa(2,4).ne.0.)go to 130              !/*ss         66*/
      ks=22                                                                     !/*ss         67*/
      go to 110                                                                 !/*ss         68*/
!                                                                               !/*ss         69*/
!        produce a planar or spherical surface.                                 !/*ss         70*/
   60 if(ks.ge.10)go to 80                                                      !/*ss         71*/
      kk=1                                                                      !/*ss         72*/
      do 70 i=1,3                                                               !/*ss         73*/
      if(ks.le.4)tpp(i)=2.*tpp(i+13)                                            !/*ss         74*/
      if(ks.ge.5)tpp(i)=tpp(i+17)                                               !/*ss         75*/
   70 if(abs(tpp(i)).gt.abs(tpp(kk)))kk=i                                       !/*ss         76*/
      if(abs(tpp(1))+abs(tpp(2))+abs(tpp(3))-abs(tpp(kk)).gt.1.e-10*            !/*ss         77*/
     & (abs(tpp(kk))+abs(tpp(17))).or.ks.le.4.and.tpp(kk).lt.0.)kk=0            !/*ss         78*/
      if(ks.ge.5)kk=kk+5                                                        !/*ss         79*/
      ks=kk+1                                                                   !/*ss         80*/
      if(ks.gt.6.and.abs(tpp(kk-5)).lt.-1.e-10*tpp(17))ks=5                     !/*ss         81*/
      if(abs(ks-3).le.1)tpp(17)=tpp(17)/tpp(kk)                                 !/*ss         82*/
      if(ks.gt.6)tpp(1)=tpp(kk-5)                                               !/*ss         83*/
      tpp(nc(ks))=-tpp(17)                                                      !/*ss         84*/
      go to 130                                                                 !/*ss         85*/
!                                                                               !/*ss         86*/
!        produce a cylindrical surface.                                         !/*ss         87*/
   80 if(ks.ge.16)go to 90                                                      !/*ss         88*/
      tpp(1)=tpp(18)                                                            !/*ss         89*/
      tpp(2)=tpp(20)                                                            !/*ss         90*/
      if(im.eq.1)tpp(1)=tpp(19)                                                 !/*ss         91*/
      if(im.eq.3)tpp(2)=tpp(19)                                                 !/*ss         92*/
      ks=im+9                                                                   !/*ss         93*/
      if(abs(tpp(1))+abs(tpp(2)).lt.-1.e-10*tpp(17))ks=im+12                    !/*ss         94*/
      tpp(nc(ks))=-tpp(17)                                                      !/*ss         95*/
      go to 130                                                                 !/*ss         96*/
!                                                                               !/*ss         97*/
!        produce a conical surface.                                             !/*ss         98*/
   90 if(ks.eq.22)go to 110                                                     !/*ss         99*/
      k=lsc(llsc+js)+nc(ks)                                                     !/*ss        100*/
      ic=1                                                                      !/*ss        101*/
      if(tpp(1)*tpp(2).gt.0.)ic=3                                               !/*ss        102*/
      if(tpp(1)*tpp(3).gt.0.)ic=2                                               !/*ss        103*/
      j=mod(ks-16,3)+3*ic+2                                                     !/*ss        104*/
      ks=ic+15                                                                  !/*ss        105*/
      do 100 i=1,3                                                              !/*ss        106*/
  100 tpp(i)=tpp(17+i)                                                          !/*ss        107*/
      if(abs(tpp(1))+abs(tpp(2))+abs(tpp(3))-abs(tpp(ic)).lt.                   !/*ss        108*/
     & 1.e-10*(abs(tpp(ic))+1.))ks=ic+18                                        !/*ss        109*/
      if(ks.gt.18)tpp(1)=tpp(ic)                                                !/*ss        110*/
      tpp(nc(ks)-1)=scf(k-1)                                                    !/*ss        111*/
      tpp(nc(ks))=scf(k)*sign(one,trf(ltrf+j,jt))                               !/*ss4a        2*/
      go to 130                                                                 !/*ss        113*/
!                                                                               !/*ss        114*/
!        produce a special quadratic.                                           !/*ss        115*/
  110 do 120 i=1,10                                                             !/*ss        116*/
  120 tpp(i)=tpp(10+i)                                                          !/*ss        117*/
!                                                                               !/*ss        118*/
!        load surface coefficients into the scf array.                          !/*ss        119*/
  130 kst(lkst+js)=ks                                                           !/*ss4a        3*/
      n=lsc(llsc+js)+nc(ks)-lsc(llsc+js+1)                                      !/*ss        121*/
      if(n.eq.0)go to 160                                                       !/*ss        122*/
      do 140 iz=lsc(llsc+js+1)+1,lsc(llsc+mxj+1),1                              !/*ss        123*/
      i=lsc(llsc+mxj+1)+lsc(llsc+js+1)+1-iz                                     !/*ss        124*/
      j=i                                                                       !/*ss        125*/
      if(n.lt.0)j=lsc(llsc+js+1)+1+lsc(llsc+mxj+1)-i                            !/*ss        126*/
  140 scf(j+n)=scf(j)                                                           !/*ss        127*/
      do 150 j=js,mxj                                                           !/*ss        128*/
  150 lsc(llsc+j+1)=lsc(llsc+j+1)+n                                             !/*ss        129*/
  160 do 170 i=1,nc(ks)                                                         !/*ss        130*/
  170 scf(lsc(llsc+js)+i)=tpp(i)                                                !/*ss        131*/
      return                                                                    !/*ss        132*/
!                                                                               !/*ss        133*/
!        transform torus if transformation is not skewed.                       !/*ss        134*/
  180 if(trf(ltrf+1,jt).lt.0.)go to 190                                         !/*ss        135*/
      call erprnt(1,1,2,jtr(ljtr+js),nsf(lnsf+js),0,0,0,                        !/*ss        136*/
     & '14htransformation,i3,20h of toroidal surface,i5,11h is skewed.')        !/*ss        137*/
      return                                                                    !/*ss        138*/
  190 do 200 i=ks-19,ks-13,3                                                    !/*ss        139*/
  200 if(abs(trf(ltrf+i,jt)).gt..9)kst(lkst+js)=23+(i-2)/3                      !/*ss4a        4*/
      l=lsc(llsc+js)                                                            !/*ss        142*/
      tpp(1)=scf(l+1)-trf(ltrf+2,jt)                                            !/*ss        143*/
      tpp(2)=scf(l+2)-trf(ltrf+3,jt)                                            !/*ss        144*/
      tpp(3)=scf(l+3)-trf(ltrf+4,jt)                                            !/*ss        145*/
      call matmpy(3,3,1,trf(ltrf+5,jt),3,tpp,1,scf(l+1),1,4)                    !/*ss4c2       1*/
      return                                                                    !/*ss        147*/
      end                                                                       !/*ss        148*/
