!#ident latcon.F from deck la                                                   !/*la          1*/
      subroutine latcon                                                         !/*la          2*/
!        calculate the lattice vectors and search constants.                    !/*la          3*/
      use global_data                                                           !/*lax         1*/
      use imcn_data                                                             !/*lax         2*/
!                                                                               !/*lax         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*lax         4*/
      implicit integer(kindi) (i-n)                                             !/*lax         5*/
!                                                                               !/*lax         6*/
      dimension sc(4,8),xp(3),tn(8)                                             !/*la          6*/
      character ha*5                                                            !/*la          7*/
!                                                                               !/*la          8*/
      do 190 ic=1,mxa                                                           !/*la          9*/
      if(lat(llat+1,ic).eq.0)go to 190                                          !/*la         10*/
!                                                                               !/*la         11*/
!        put the lattice cell number into mfl or laf.                           !/*la         12*/
      if(jun(ljun+ic).eq.0)go to 30                                             !/*la         13*/
      do 20 i=1,mxa                                                             !/*la         14*/
      if(i.eq.ic)go to 20                                                       !/*la         15*/
      if(mfl(lmfl+1,i).eq.abs(jun(ljun+ic)))mfl(lmfl+2,i)=ic                    !/*la         16*/
      if(mfl(lmfl+1,i).ge.0)go to 20                                            !/*la         17*/
      lp=-mfl(lmfl+1,i)                                                         !/*la         18*/
      do 10 j=3,2+laf(lp+1,2)*laf(lp+2,2)*laf(lp+3,2)                           !/*la         19*/
   10 if(laf(lp+1,j).eq.abs(jun(ljun+ic)))laf(lp+2,j)=ic                        !/*la4a        1*/
   20 continue                                                                  !/*la         21*/
!                                                                               !/*la         22*/
!        put the coefficients of the surfaces into the same form.               !/*la         23*/
   30 n=abs(lca(llca+ic+1))-lca(llca+ic)                                        !/*la         24*/
      ip=lat(llat+2,ic)                                                         !/*la         25*/
      do 90 j=1,n                                                               !/*la         26*/
      js=abs(lja(llja+lca(llca+ic)+j-1))                                        !/*la         27*/
      ks=abs(kst(lkst+js))                                                      !/*cor4-1     58*/
      ls=lsc(llsc+js)                                                           !/*la         29*/
      if(ks.ne.1)go to 50                                                       !/*la         30*/
      tn(j)=sqrt(scf(ls+1)**2+scf(ls+2)**2+scf(ls+3)**2)                        !/*la         31*/
      do 40 i=1,4                                                               !/*la         32*/
   40 sc(i,j)=scf(ls+i)                                                         !/*la         33*/
      go to 70                                                                  !/*la         34*/
   50 do 60 i=1,4                                                               !/*la         35*/
   60 sc(i,j)=0.                                                                !/*la         36*/
      tn(j)=1.                                                                  !/*la         37*/
      sc(ks-1,j)=1.                                                             !/*la         38*/
      sc(4,j)=scf(ls+1)                                                         !/*la         39*/
   70 if(mod(j,2).eq.1)go to 80                                                 !/*la         40*/
      a=sc(1,j)*sc(1,j-1)+sc(2,j)*sc(2,j-1)+sc(3,j)*sc(3,j-1)                   !/*la         41*/
      if(a.lt.0.)tn(j)=-tn(j)                                                   !/*la         42*/
      if(abs(a-tn(j)).gt..001)go to 200                                         !/*la         43*/
   80 do 90 i=1,4                                                               !/*la         44*/
   90 sc(i,j)=sc(i,j)/tn(j)                                                     !/*la         45*/
      if(lat(llat+1,ic).eq.2)go to 150                                          !/*la         46*/
!                                                                               !/*la         47*/
!        hexahedron case                                                        !/*la         48*/
      if(n.gt.2)go to 110                                                       !/*la         49*/
      do 100 i=1,3                                                              !/*la         50*/
      vcl(lvcl+i,5,ip)=sc(i,1)/(sc(4,1)-sc(4,2))                                !/*la         51*/
  100 vcl(lvcl+i,1,ip)=(sc(4,1)-sc(4,2))*sc(i,1)                                !/*la         52*/
      vcl(lvcl+1,4,ip)=.5*(sc(4,1)+sc(4,2))/(sc(4,1)-sc(4,2))                   !/*la         53*/
      go to 190                                                                 !/*la         54*/
  110 if(n.eq.4)call crspro(sc(1,1),sc(1,3),sc(1,5))                            !/*la         55*/
      call crspro(sc(1,3),sc(1,5),xp)                                           !/*la         56*/
      t1=xp(1)*sc(1,1)+xp(2)*sc(2,1)+xp(3)*sc(3,1)                              !/*la         57*/
      if(t1.eq.0.)go to 200                                                     !/*la         58*/
      t1=(sc(4,1)-sc(4,2))/t1                                                   !/*la         59*/
      do 120 i=1,3                                                              !/*la         60*/
      vcl(lvcl+i,5,ip)=sc(i,1)/(sc(4,1)-sc(4,2))                                !/*la         61*/
  120 vcl(lvcl+i,1,ip)=t1*xp(i)                                                 !/*la         62*/
      vcl(lvcl+1,4,ip)=.5*(sc(4,1)+sc(4,2))/(sc(4,1)-sc(4,2))                   !/*la         63*/
      call crspro(sc(1,5),sc(1,1),xp)                                           !/*la         64*/
      t1=xp(1)*sc(1,3)+xp(2)*sc(2,3)+xp(3)*sc(3,3)                              !/*la         65*/
      if(t1.eq.0.)go to 200                                                     !/*la         66*/
      t1=(sc(4,3)-sc(4,4))/t1                                                   !/*la         67*/
      do 130 i=1,3                                                              !/*la         68*/
      vcl(lvcl+i,6,ip)=sc(i,3)/(sc(4,3)-sc(4,4))                                !/*la         69*/
  130 vcl(lvcl+i,2,ip)=t1*xp(i)                                                 !/*la         70*/
      vcl(lvcl+2,4,ip)=.5*(sc(4,3)+sc(4,4))/(sc(4,3)-sc(4,4))                   !/*la         71*/
      if(n.eq.4)go to 190                                                       !/*la         72*/
      call crspro(sc(1,1),sc(1,3),xp)                                           !/*la         73*/
      t1=xp(1)*sc(1,5)+xp(2)*sc(2,5)+xp(3)*sc(3,5)                              !/*la         74*/
      if(t1.eq.0.)go to 200                                                     !/*la         75*/
      t1=(sc(4,5)-sc(4,6))/t1                                                   !/*la         76*/
      do 140 i=1,3                                                              !/*la         77*/
      vcl(lvcl+i,7,ip)=sc(i,5)/(sc(4,5)-sc(4,6))                                !/*la         78*/
  140 vcl(lvcl+i,3,ip)=t1*xp(i)                                                 !/*la         79*/
      vcl(lvcl+3,4,ip)=.5*(sc(4,5)+sc(4,6))/(sc(4,5)-sc(4,6))                   !/*la         80*/
      go to 190                                                                 !/*la         81*/
!                                                                               !/*la         82*/
!        hexagonal prism case.                                                  !/*la         83*/
  150 if(n.eq.6)call crspro(sc(1,1),sc(1,3),sc(1,7))                            !/*la         84*/
      do 170 j=1,2                                                              !/*la         85*/
      call crspro(sc(1,6-2*j),sc(1,5),xp)                                       !/*la         86*/
      t1=sc(1,7)*xp(1)+sc(2,7)*xp(2)+sc(3,7)*xp(3)                              !/*la         87*/
      if(t1.eq.0.)go to 200                                                     !/*la         88*/
      call crspro(sc(1,6-2*j),sc(1,2*j),xp)                                     !/*la         89*/
      s=(sc(1,7)*xp(1)+sc(2,7)*xp(2)+sc(3,7)*xp(3))/t1                          !/*la         90*/
      call crspro(sc(1,2*j),sc(1,5),xp)                                         !/*la         91*/
      t=(sc(1,7)*xp(1)+sc(2,7)*xp(2)+sc(3,7)*xp(3))/t1                          !/*la         92*/
      bp=(sc(4,2*j-1)-s*sc(4,7-j))/t                                            !/*la         93*/
      c=(sc(4,2*j)-t*sc(4,6-2*j))/s                                             !/*la         94*/
      r=(bp-sc(4,6-2*j))/(c-sc(4,7-j))                                          !/*la         95*/
      g=1./((sc(4,4+j)-c)*r+sc(4,5-2*j)-sc(4,6-2*j))                            !/*la         96*/
      f=g*r                                                                     !/*la         97*/
      do 160 i=1,3                                                              !/*la         98*/
  160 tpp(i)=f*sc(i,5)+g*sc(i,6-2*j)                                            !/*la         99*/
      call crspro(tpp,sc(1,7),xp)                                               !/*la        100*/
      t1=sc(1,2*j)*xp(1)+sc(2,2*j)*xp(2)+sc(3,2*j)*xp(3)                        !/*la        101*/
      if(t1.eq.0.)go to 200                                                     !/*la        102*/
      t1=(sc(4,2*j-1)-sc(4,2*j))/t1                                             !/*la        103*/
      vcl(lvcl+3-j,4,ip)=.5*(f*(sc(4,4+j)+sc(4,7-j))+g*(sc(4,5-2*j)+bp))        !/*la        104*/
      do 170 i=1,3                                                              !/*la        105*/
      vcl(lvcl+i,7-j,ip)=tpp(i)                                                 !/*la        106*/
  170 vcl(lvcl+i,j,ip)=t1*xp(i)                                                 !/*la        107*/
      if(n.eq.6)go to 190                                                       !/*la        108*/
      call crspro(sc(1,1),sc(1,3),xp)                                           !/*la        109*/
      t1=(sc(4,7)-sc(4,8))/(xp(1)*sc(1,7)+xp(2)*sc(2,7)+xp(3)*sc(3,7))          !/*la        110*/
      do 180 i=1,3                                                              !/*la        111*/
      vcl(lvcl+i,7,ip)=sc(i,7)/(sc(4,7)-sc(4,8))                                !/*la        112*/
  180 vcl(lvcl+i,3,ip)=t1*xp(i)                                                 !/*la        113*/
      vcl(lvcl+3,4,ip)=.5*(sc(4,7)+sc(4,8))/(sc(4,7)-sc(4,8))                   !/*la        114*/
  190 continue                                                                  !/*la        115*/
      return                                                                    !/*la        116*/
!                                                                               !/*la        117*/
  200 write(ha,'(i5)')ncl(lncl+ic)                                              !/*la        118*/
      call expire(0,'latcon',                                                   !/*la        119*/
     & 'wrong order of surfaces in lattice cell '//ha)                          !/*la        120*/
      end                                                                       !/*la        121*/
