!#ident findel.F from deck fn                                                   !/*fn          1*/
      subroutine findel(ic)                                                     !/*fn          2*/
!        find the element in lattice ic that contains xxx,yyy,zzz and           !/*fn          3*/
!        adjust xxx,yyy,zzz to be relative to that element.                     !/*fn          4*/
      use global_data                                                           !/*fnx         1*/
!                                                                               !/*fnx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*fnx         3*/
      implicit integer(kindi) (i-n)                                             !/*fnx         4*/
!                                                                               !/*fnx         5*/
      dimension jx(14,2)                                                        !/*fnx         6*/
      data jx/1,1,2,2,3,3,1,1,2,2,2,2,3,3,                                      !/*fn4c        2*/
     &  1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1/                                     !/*fn4c        3*/
!                                                                               !/*fn          8*/
!        find the element.                                                      !/*fn          9*/
      m=lat(llat+2,ic)                                                          !/*fn         11*/
      jjj=0                                                                     !/*fn         12*/
      kkk=0                                                                     !/*fn         13*/
      i1=(abs(lca(llca+ic+1))-lca(llca+ic))/2-lat(llat+1,ic)+1                  !/*fn4b        1*/
      do 10 i=1,i1                                                              !/*fn4b        2*/
      a=xxx*vcl(lvcl+1,i+4,m)+yyy*vcl(lvcl+2,i+4,m)+                            !/*fn4b        3*/
     & zzz*vcl(lvcl+3,i+4,m)-vcl(lvcl+i,4,m)                                    !/*fn4b        4*/
   10 ijk(i)=nint(a)                                                            !/*fnx         7*/
!                                                                               !/*fn         17*/
!        adjust xxx,yyy,zzz.                                                    !/*fn         18*/
      do 20 i=1,3                                                               !/*fn         19*/
   20 gpblcm(i)=gpblcm(i)-iii*vcl(lvcl+i,1,m)-jjj*vcl(lvcl+i,2,m)-              !/*fn         20*/
     & kkk*vcl(lvcl+i,3,m)                                                      !/*fn         21*/
!                                                                               !/*fn4c        5*/
!        correct for coincident surfaces and hexagonal prism lattices.          !/*fn4c        6*/
!        check if new location completely inside lattice element.               !/*fn4c        7*/
      jc=0                                                                      !/*fn4c        8*/
   30 jc=jc+1                                                                   !/*fn4c        9*/
      if(jc.gt.10)go to 60                                                      !/*fn4c       10*/
      do 50 jk=lca(llca+ic),abs(lca(llca+ic+1))-1                               !/*fn4c       11*/
      j=abs(lja(llja+jk))                                                       !/*fn4c       12*/
      k=kst(lkst+j)                                                             !/*fn4c       13*/
      l=lsc(llsc+j)                                                             !/*fn4c       14*/
      if(k.eq.1)t4=scf(l+1)*xxx+scf(l+2)*yyy+scf(l+3)*zzz-scf(l+4)              !/*fn4c       15*/
      if(k.ne.1)t4=gpblcm(k-1)-scf(l+1)                                         !/*fn4c       16*/
      if(jsu.ge.0)go to 40                                                      !/*fn4c       17*/
      if(ksc(lksc-jsu).ne.ksc(lksc+j))go to 40                                  !/*fn4c       18*/
      if(k.eq.1)t5=scf(l+1)*uuu+scf(l+2)*vvv+scf(l+3)*www                       !/*fn4c       19*/
      if(k.ne.1)t5=gpblcm(k+2)                                                  !/*fn4c       20*/
      if(abs(t4).le.coincd*abs(t5))t4=t5                                        !/*fn4c       21*/
   40 if(lja(llja+jk)*t4.gt.0.)go to 50                                         !/*fn4c       22*/
!                                                                               !/*fn4c       23*/
!        location coincident with or beyond surface j, index ix.                !/*fn4c       24*/
      if(iovr.eq.2.and.t4.eq.0.)go to 50                                        !/*fn4c       25*/
      j1=jk-lca(llca+ic)+6*lat(llat+1,ic)-5                                     !/*fn4c       26*/
      ix=jx(j1,1)                                                               !/*fn4c       27*/
      iy=jx(j1,2)                                                               !/*fn4c       28*/
      ijk(ix)=ijk(ix)+iy                                                        !/*fnx         8*/
      xxx=xxx-iy*vcl(lvcl+1,ix,m)                                               !/*fn4c       30*/
      yyy=yyy-iy*vcl(lvcl+2,ix,m)                                               !/*fn4c       31*/
      zzz=zzz-iy*vcl(lvcl+3,ix,m)                                               !/*fn4c       32*/
!                                                                               !/*fn4c       33*/
!        if on or beyond hex side 3, increment sides 1 and 2.                   !/*fn4c       34*/
      if(ix.ne.2.or.j1.lt.11)go to 30                                           !/*fn4c       35*/
      iii=iii-iy                                                                !/*fn4c       36*/
      xxx=xxx+iy*vcl(lvcl+1,1,m)                                                !/*fn4c       37*/
      yyy=yyy+iy*vcl(lvcl+2,1,m)                                                !/*fn4c       38*/
      zzz=zzz+iy*vcl(lvcl+3,1,m)                                                !/*fn4c       39*/
      go to 30                                                                  !/*fn4c       40*/
   50 continue                                                                  !/*fn4c       41*/
!                                                                               !/*fn4c       42*/
!        final check that hexagonal element selected exists.                    !/*fn4c       43*/
      if(mfl(lmfl+1,ic).ge.0)return                                             !/*fn4c       44*/
      j=-mfl(lmfl+1,ic)                                                         !/*fn4c       45*/
      if(ijk(1).lt.laf(j+1,1).or.ijk(1).ge.laf(j+1,1)+laf(j+1,2))               !/*fnx         9*/
     & go to 60                                                                 !/*fnx        10*/
      if(ijk(2).lt.laf(j+2,1).or.ijk(2).ge.laf(j+2,1)+laf(j+2,2))               !/*fnx        11*/
     & go to 60                                                                 !/*fnx        12*/
      if(ijk(3).lt.laf(j+3,1).or.ijk(3).ge.laf(j+3,1)+laf(j+3,2))               !/*fnx        13*/
     & go to 60                                                                 !/*fnx        14*/
      return                                                                    !/*fn4c       49*/
!                                                                               !/*fn4c       50*/
!        cannot find lattice location.                                          !/*fn4c       51*/
   60 kdb=1                                                                     !/*fn4c       52*/
      if(iovr.eq.4)call expirx(1,'findel','zero lattice element hit.')          !/*fn4c       53*/
      return                                                                    !/*fn         53*/
      end                                                                       !/*fn         54*/
