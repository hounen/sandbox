!#ident pathmz.F from deck zl4b                                                 !/*zl4b        1*/
      subroutine pathmz(n,lv,ic,ie,li,hs)                                       !/*zl4b        2*/
!        put description of repeated structures / lattice path from             !/*zl4b        3*/
!        level 0 to level lv into hs for universe maze.                         !/*zl4b        4*/
!                                                                               !/*zl4b        5*/
!               n = character length of patch description.                      !/*zl4b        6*/
!          ic(ll) = cell number at level ll.                                    !/*zl4b        7*/
!          ie(ll) = lattice index.                                              !/*zl4b        8*/
!        li(1,ll) = current value of iii lattice index.                         !/*zl4b        9*/
!        li(2,ll) = current value of jjj lattice index.                         !/*zl4b       10*/
!        li(3,ll) = current value of kkk lattice index.                         !/*zl4b       11*/
!        li(4,ll) = lowest iii index.                                           !/*zl4b       12*/
!        li(5,ll) = lowest jjj index.                                           !/*zl4b       13*/
!        li(6,ll) = lowest kkk index.                                           !/*zl4b       14*/
!        li(7,ll) = highest iii index.                                          !/*zl4b       15*/
!        li(8,ll) = highest jjj index.                                          !/*zl4b       16*/
!        li(9,ll) = highest kkk index.                                          !/*zl4b       17*/
!        li(10,ll)= character length of cell name.                              !/*zl4b       18*/
!        li(11,ll)= maximum character length of index iii.                      !/*zl4b       19*/
!        li(12,ll)= maximum character length of index jjj.                      !/*zl4b       20*/
!        li(13,ll)= maximum character length of index kkk.                      !/*zl4b       21*/
!        li(14,ll)= total character length of lattice element name.             !/*zl4b       22*/
!        li(15,ll)= last ic value for which li array set up.                    !/*zl4b       23*/
      use global_data                                                           !/*zlx         1*/
!                                                                               !/*zlx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*zlx         3*/
      implicit integer(kindi) (i-n)                                             !/*zlx         4*/
!                                                                               !/*zlx         5*/
      dimension ic(0:mxlv),ie(0:mxlv),li(15,0:mxlv)                             !/*zl4b       25*/
      character hs(95)*1,ht*95,hu*24                                            !/*zl4b       26*/
!                                                                               !/*zl4b       27*/
!        get the lattice element descriptions in the path.                      !/*zl4b       28*/
      n=2                                                                       !/*zl4b       29*/
      ht=' '                                                                    !/*zl4b       30*/
      do 40 lx=0,lv                                                             !/*zl4b       31*/
      if(n.gt.88)go to 40                                                       !/*zl4b       32*/
      ll=lv-lx                                                                  !/*zl4b       33*/
      write(ht(n:n+6),'(i5,2h <)')ncl(lncl+ic(ll))                              !/*zl4b       34*/
      n=n+7                                                                     !/*zl4b       35*/
!                                                                               !/*zl4b       36*/
!        setup for all finite lattice elements at level ll.                     !/*zl4b       37*/
      mf=-mfl(lmfl+1,ic(ll))                                                    !/*zl4b       38*/
      if(mf.le.0)go to 40                                                       !/*zl4b       39*/
      if(ic(ll).eq.li(15,ll))go to 20                                           !/*zl4b       40*/
      do 10 i=1,3                                                               !/*zl4b       41*/
      li(i+3,ll)=laf(mf+i,1)                                                    !/*zl4b       42*/
      li(i+6,ll)=laf(mf+i,1)+laf(mf+i,2)-1                                      !/*zl4b       43*/
   10 li(i+10,ll)=                                                              !/*zl4b       44*/
     & 1+int(log10(.5+float(max(1,-10*li(i+3,ll),li(i+6,ll)))))                 !/*zl4b       45*/
      li(10,ll)=1+int(log10(.5+float(ncl(lncl+ic(ll)))))                        !/*zl4b       46*/
      li(14,ll)=li(10,ll)+li(11,ll)+li(12,ll)+li(13,ll)                         !/*zl4b       47*/
      li(15,ll)=ic(ll)                                                          !/*zl4b       48*/
!                                                                               !/*zl4b       49*/
!        process each element of the finite lattice.                            !/*zl4b       50*/
   20 if(n+li(14,ll).gt.95)go to 40                                             !/*zl4b       51*/
      n=n-6                                                                     !/*zl4b       52*/
      li(3,ll)=laf(mf+3,1)+(ie(ll)-1)/(laf(mf+1,2)*laf(mf+2,2))                 !/*zl4b       53*/
      li(2,ll)=laf(mf+2,1)+(ie(ll)-1-(li(3,ll)-laf(mf+3,1))*                    !/*zl4b       54*/
     & laf(mf+1,2)*laf(mf+2,2))/laf(mf+1,2)                                     !/*zl4b       55*/
      li(1,ll)=laf(mf+1,1)+ie(ll)-1-laf(mf+1,2)*(li(2,ll)-laf(mf+2,1)+          !/*zl4b       56*/
     & (li(3,ll)-laf(mf+3,1))*laf(mf+2,2))                                      !/*zl4b       57*/
      write(hu,'(i5,1h[,i5,2i6,1h])')ncl(lncl+ic(ll)),(li(i,ll),i=1,3)          !/*zl4b       58*/
      do 30 i=1,4                                                               !/*zl4b       59*/
      ht(n:n+li(i+9,ll))=hu(6*i-li(i+9,ll):6*i)                                 !/*zl4b       60*/
   30 n=n+li(i+9,ll)+1                                                          !/*zl4b       61*/
      ht(n+1:n+1)='<'                                                           !/*zl4b       62*/
      n=n+2                                                                     !/*zl4b       63*/
   40 continue                                                                  !/*zl4b       64*/
!                                                                               !/*zl4b       65*/
!        copy final path description into hs.                                   !/*zl4b       66*/
      n=n-3                                                                     !/*zl4b       67*/
      do 50 i=1,n                                                               !/*zl4b       68*/
   50 hs(i)=ht(i:i)                                                             !/*zl4b       69*/
      return                                                                    !/*zl4b       70*/
      end                                                                       !/*zl4b       71*/
