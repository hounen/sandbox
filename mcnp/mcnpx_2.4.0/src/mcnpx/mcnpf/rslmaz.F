!#ident rslmaz.F from deck zr4b                                                 !/*zr4b        1*/
      subroutine rslmaz(ir,np)                                                  !/*zr4b        2*/
!        increment maze to count sources (ir=1), tracks entering (ir=2),        !/*zr4b        3*/
!        and collisions (ir=3) in repeated structure/lattice elements.          !/*zr4b        4*/
!        np is the number of particles.                                         !/*zr4b        5*/
      use global_data                                                           !/*zrx         1*/
!                                                                               !/*zrx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*zrx         3*/
      implicit integer(kindi) (i-n)                                             !/*zrx         4*/
!                                                                               !/*zrx         5*/
      if(kc8.gt.0.or.ipt.gt.3)return                                            !/*zrxa        1*/
      lz=nmaz*(3*(ipt-nmip)+ir-1)+1                                             !/*zr4b        9*/
      do 10 lv=0,lev-1                                                          !/*zr4b       10*/
      ic=nint(udt(7,lv))                                                        !/*zr4b       11*/
      mi=mazp(lmzp+1,ic)                                                        !/*zr4b       12*/
      if(mfl(lmfl+1,ic).ge.0)go to 10                                           !/*zr4b       13*/
      mf=-mfl(lmfl+1,ic)                                                        !/*zr4b       14*/
      mi=1+nint(udt(8,lv))-laf(mf+1,1)+                                         !/*zr4b       15*/
     & laf(mf+1,2)*(nint(udt(9,lv))-laf(mf+2,1)+                                !/*zr4b       16*/
     & laf(mf+2,2)*(nint(udt(10,lv))-laf(mf+3,1)))                              !/*zr4b       17*/
   10 lz=lz+mazu(lmzu+mazp(lmzp+2,ic)+mi)                                       !/*zr4b       18*/
      mi=mazp(lmzp+1,icl)                                                       !/*zr4b       19*/
      mf=-mfl(lmfl+1,icl)                                                       !/*zr4b       20*/
      if(mf.gt.0)mi=1+iii-laf(mf+1,1)+laf(mf+1,2)*(jjj-laf(mf+2,1)+             !/*zr4b       21*/
     & laf(mf+2,2)*(kkk-laf(mf+3,1)))                                           !/*zr4b       22*/
      lz=lz+mazu(lmzu+mazp(lmzp+2,icl)+mi)                                      !/*zr4b       23*/
      maze(kmaz+lz)=maze(kmaz+lz)+np                                            !/*zr4b       24*/
      return                                                                    !/*zr4b       25*/
      end                                                                       !/*zr4b       26*/
