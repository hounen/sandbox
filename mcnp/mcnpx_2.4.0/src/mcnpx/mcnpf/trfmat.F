!#ident trfmat.F from deck tm                                                   !/*tm          1*/
      subroutine trfmat(jt)                                                     !/*tm          2*/
!        complete the surface transformation matrix trf(jt).                    !/*tm          3*/
      use global_data                                                           !/*tmx         1*/
      use imcn_data                                                             !/*tmx         2*/
!                                                                               !/*tmx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*tmx         4*/
      implicit integer(kindi) (i-n)                                             !/*tmx         5*/
!                                                                               !/*tmx         6*/
      dimension tr(17),iy(2,3)                                                  !/*tm          6*/
      data iy/6,8,7,11,10,12/                                                   !/*tm          7*/
      dp(i,j)=tr(i)*tr(j)+tr(i+1)*tr(j+1)+tr(i+2)*tr(j+2)                       !/*tm          8*/
!                                                                               !/*tm          9*/
      nb=0                                                                      !/*tm         10*/
      ki=0                                                                      !/*tm4c2       1*/
      do 10 i=1,14                                                              !/*tm         11*/
      tr(i)=trf(ltrf+i,jt)                                                      !/*tm         12*/
   10 if(tr(i).ne.huge.and.i.ge.5.and.i.le.13)nb=nb+1                           !/*tm         13*/
      if(tr(14).eq.0.)tr(14)=1.                                                 !/*tm         14*/
      go to(20,320,320,40,320,160,90,320,320,230)nb+1                           !/*tm         15*/
!                                                                               !/*tm         16*/
! >>>>>  nb=0 -- no vectors.  simple translation.                               !/*tm         17*/
   20 do 30 i=5,13                                                              !/*tm         18*/
   30 tr(i)=trf(ltrf+i,0)                                                       !/*tm         19*/
      go to 260                                                                 !/*tm         20*/
!                                                                               !/*tm         21*/
! >>>>>  nb=3 -- one vector.                                                    !/*tm         22*/
   40 do 60 k=1,2                                                               !/*tm         23*/
      do 50 j=1,3                                                               !/*tm         24*/
   50 if(tr(3*j+2).ne.huge.and.tr(3*j+3).ne.huge.and.                           !/*tm         25*/
     & tr(3*j+4).ne.huge)go to 70                                               !/*tm         26*/
      ki=1                                                                      !/*tm4c2       2*/
      do 60 i=1,3                                                               !/*tm         27*/
      t=tr(iy(1,i))                                                             !/*tm         28*/
      tr(iy(1,i))=tr(iy(2,i))                                                   !/*tm         29*/
   60 tr(iy(2,i))=t                                                             !/*tm         30*/
      go to 320                                                                 !/*tm         31*/
   70 r=sqrt(tr(3*j+2)**2+tr(3*j+3)**2+tr(3*j+4)**2)                            !/*tm         32*/
      if(r.eq.0.)go to 320                                                      !/*tm         33*/
      do 80 i=2,4                                                               !/*tm         34*/
   80 tr(3*j+i)=tr(3*j+i)/r                                                     !/*tm         35*/
      call arbobv(tr(3*j+2),tr(3*(mod(j,3)+1)+2),                               !/*tm         36*/
     & tr(3*(mod(j+1,3)+1)+2),i)                                                !/*tm         37*/
      go to 140                                                                 !/*tm         38*/
!                                                                               !/*tm         39*/
! >>>>>  nb=6 -- two vectors in the same system.                                !/*tm         40*/
   90 do 110 k=1,2                                                              !/*tm         41*/
      do 100 j=1,3                                                              !/*tm         42*/
  100 if(tr(3*j+2).eq.huge.and.tr(3*j+3).eq.huge.and.                           !/*tm         43*/
     & tr(3*j+4).eq.huge)go to 120                                              !/*tm         44*/
      ki=1                                                                      !/*tm4c2       3*/
      do 110 i=1,3                                                              !/*tm         45*/
      t=tr(iy(1,i))                                                             !/*tm         46*/
      tr(iy(1,i))=tr(iy(2,i))                                                   !/*tm         47*/
  110 tr(iy(2,i))=t                                                             !/*tm         48*/
      go to 320                                                                 !/*tm         49*/
  120 do 130 l=1,2                                                              !/*tm         50*/
      m=mod(j+l-1,3)+1                                                          !/*tm         51*/
      r=sqrt(tr(3*m+2)**2+tr(3*m+3)**2+tr(3*m+4)**2)                            !/*tm         52*/
      if(r.eq.0.)go to 320                                                      !/*tm         53*/
      do 130 i=2,4                                                              !/*tm         54*/
  130 tr(3*m+i)=tr(3*m+i)/r                                                     !/*tm         55*/
      call crspro(tr(3*(mod(j,3)+1)+2),tr(3*(mod(j+1,3)+1)+2),tr(3*j+2))        !/*tm         56*/
  140 if(ki.eq.0)go to 260                                                      !/*tm4c2       4*/
      do 150 i=1,3                                                              !/*tm         58*/
      t=tr(iy(1,i))                                                             !/*tm         59*/
      tr(iy(1,i))=tr(iy(2,i))                                                   !/*tm         60*/
  150 tr(iy(2,i))=t                                                             !/*tm         61*/
      go to 260                                                                 !/*tm         62*/
!                                                                               !/*tm         63*/
! >>>>>  nb=5 -- one vector in each system.                                     !/*tm         64*/
  160 do 170 i1=1,3                                                             !/*tm         65*/
  170 if(tr(3*i1+2).ne.huge.and.tr(3*i1+3).ne.huge.and.                         !/*tm         66*/
     & tr(3*i1+4).ne.huge)go to 180                                             !/*tm         67*/
      go to 320                                                                 !/*tm         68*/
  180 do 190 j1=1,3                                                             !/*tm         69*/
  190 if(tr(j1+4).ne.huge.and.tr(j1+7).ne.huge.and.                             !/*tm         70*/
     & tr(j1+10).ne.huge)go to 200                                              !/*tm         71*/
      go to 320                                                                 !/*tm         72*/
  200 r=tr(3*i1+j1+1)**2                                                        !/*tm         73*/
      do 210 i=5,13                                                             !/*tm         74*/
  210 if(tr(i).ne.huge)r=r+tr(i)**2                                             !/*tm         75*/
      r=sqrt(.5*r)                                                              !/*tm         76*/
      if(r.eq.0.)go to 320                                                      !/*tm         77*/
      do 220 i=5,13                                                             !/*tm         78*/
  220 if(tr(i).ne.huge)tr(i)=tr(i)/r                                            !/*tm         79*/
      if(tr(3*i1+j1+1)**2.eq.1.)go to 320                                       !/*tm         80*/
      i2=mod(i1,3)+1                                                            !/*tm         81*/
      i3=mod(i2,3)+1                                                            !/*tm         82*/
      j2=mod(j1,3)+1                                                            !/*tm         83*/
      j3=mod(j2,3)+1                                                            !/*tm         84*/
      r=1./(1.-tr(3*i1+j1+1)**2)                                                !/*tm         85*/
      a=tr(3*i1+j3+1)*tr(3*i3+j1+1)*r                                           !/*tm         86*/
      b=tr(3*i1+j2+1)*tr(3*i2+j1+1)*r                                           !/*tm         87*/
      c=tr(3*i2+j1+1)*tr(3*i1+j3+1)*r                                           !/*tm         88*/
      d=tr(3*i3+j1+1)*tr(3*i1+j2+1)*r                                           !/*tm         89*/
      tr(3*i2+j2+1)=-a-b*tr(3*i1+j1+1)                                          !/*tm         90*/
      tr(3*i2+j3+1)=d-c*tr(3*i1+j1+1)                                           !/*tm         91*/
      tr(3*i3+j2+1)=c-d*tr(3*i1+j1+1)                                           !/*tm         92*/
      tr(3*i3+j3+1)=-b-a*tr(3*i1+j1+1)                                          !/*tm         93*/
      go to 260                                                                 !/*tm         94*/
!                                                                               !/*tm         95*/
! >>>>>  nb=9 -- entire b matrix.                                               !/*tm         96*/
  230 do 250 j=1,3                                                              !/*tm4b        1*/
      r=0.                                                                      !/*tm4b        2*/
      do 235 i=1,3                                                              !/*tm4b        3*/
  235 r=r+tr(3*j+i+1)**2                                                        !/*tm4b        4*/
      if(r.eq.0.)go to 320                                                      !/*tm4b        5*/
      s=1./sqrt(r)                                                              !/*tm4b        6*/
      do 240 i=1,3                                                              !/*tm4b        7*/
  240 tr(3*j+i+1)=tr(3*j+i+1)*s                                                 !/*tm4b        8*/
  250 continue                                                                  !/*tm4b        9*/
!                                                                               !/*tm4b       10*/
!        set flag (trf(ltrf+1,jt) > 0 to indicate skew rotation.                !/*tm4b       11*/
  260 do 265 i=5,13                                                             !/*tm4b       12*/
  265 if(tr(i).gt.1.e-10.and.abs(1.-tr(i)).gt.1.e-10)tr(1)=abs(tr(1))           !/*tm4b       13*/
!                                                                               !/*tm4b       14*/
!        transpose matrix and check orthogonality.                              !/*tm4b       15*/
      r=0.                                                                      !/*tm4b       16*/
      do 270 i=1,3                                                              !/*tm        106*/
  270 r=max(r,abs(dp(3*i+2,3*(mod(i,3)+1)+2)),abs(dp(3*i+2,3*i+2)-1.))          !/*tm        107*/
      if(ki.ne.0)go to 290                                                      !/*tm4c2       5*/
      do 280 i=1,3                                                              !/*tm        109*/
      t=tr(iy(1,i))                                                             !/*tm        110*/
      tr(iy(1,i))=tr(iy(2,i))                                                   !/*tm        111*/
  280 tr(iy(2,i))=t                                                             !/*tm        112*/
  290 if(r.gt.2.e-6)call erprnt(1,2,1,int(abs(tr(1))),0,0,0,0,                  !/*tm4c2       6*/
     & '43hnon-orthogonality of surface transformation,i5,8h > 2.e-6')          !/*tm        115*/
      if(r.gt.2.e-6)tr(1)=abs(tr(1))                                            !/*tm        116*/
!                                                                               !/*tm4b       17*/
!        check for degenerate rotation matrix.                                  !/*tm4b       18*/
      rr=dp(5,5)                                                                !/*tm        117*/
      rs=dp(5,8)                                                                !/*tm        118*/
      ra=sqrt(rr)                                                               !/*tm        119*/
      rb=sqrt(max(zero,dp(8,8)*rr**2-rr*rs**2))                                 !/*tm        120*/
      if(ra.lt.1.e-10.or.rb.lt.1.e-10)tr(14)=-2.                                !/*tm        121*/
      if(tr(14).eq.-2.)call erprnt(1,1,1,int(tr(1)),                            !/*tm        122*/
     & 0,0,0,0,'22hsurface transformation,i5,15h is degenerate.')               !/*tm        123*/
      if(tr(14).eq.-2.)go to 330                                                !/*tm        124*/
!                                                                               !/*tm4b       19*/
!        ensure rotation matrix is orthonormal.                                 !/*tm4b       20*/
      do 300 i=1,3                                                              !/*tm        125*/
      tr(i+7)=(tr(i+7)*rr-tr(i+4)*rs)/rb                                        !/*tm        126*/
  300 tr(i+4)=tr(i+4)/ra                                                        !/*tm        127*/
      call crspro(tr(5),tr(8),tpp)                                              !/*tm        128*/
      a=sign(one,tpp(1)*tr(11)+tpp(2)*tr(12)+tpp(3)*tr(13))                     !/*tm        129*/
      tr(11)=a*tpp(1)                                                           !/*tm        130*/
      tr(12)=a*tpp(2)                                                           !/*tm        131*/
      tr(13)=a*tpp(3)                                                           !/*tm        132*/
      do 305 i=5,13                                                             !/*tm4b       21*/
  305 if(abs(tr(i)).lt.1.e-10)tr(i)=0.                                          !/*tm4b       22*/
!                                                                               !/*tm        133*/
!        generate the other inter-origin vector.                                !/*tm        134*/
      do 310 i=1,3                                                              !/*tm        135*/
      tr(14+i)=tr(1+i)                                                          !/*tm        136*/
  310 tpp(i)=-tr(1+i)                                                           !/*tm        137*/
      if(tr(14).ne.-1.)call matmpy(3,3,1,tr(5),3,tpp,3,tr(2),3,0)               !/*tm        138*/
      if(tr(14).eq.-1.)call matmpy(3,3,1,tr(5),3,tpp,3,tr(15),3,1)              !/*tm        139*/
      go to 330                                                                 !/*tm        140*/
!                                                                               !/*tm        141*/
! >>>>>  bad b matrix.                                                          !/*tm        142*/
  320 call erprnt(1,1,1,int(abs(tr(1))),0,0,0,0,                                !/*tm4b       23*/
     & '22hsurface transformation,i5,24h is incorrectly defined.')              !/*tm        144*/
      tr(14)=-2.                                                                !/*tm        145*/
!                                                                               !/*tm        146*/
  330 do 340 i=1,17                                                             !/*tm        147*/
  340 trf(ltrf+i,jt)=tr(i)                                                      !/*tm        148*/
      return                                                                    !/*tm        149*/
      end                                                                       !/*tm        150*/
