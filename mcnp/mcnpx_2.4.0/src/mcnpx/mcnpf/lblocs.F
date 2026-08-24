      subroutine lblocs(a)                                                      !/*lb          2*/
!        check common block lengths and floating-point precision.               !/*lb          3*/
      use global_data                                                           !/*lbx         1*/
!                                                                               !/*lbx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*lbx         3*/
      implicit integer(kindi) (i-n)                                             !/*lbx         4*/
!                                                                               !/*lbx         5*/
!                                                                               !/*lb          5*/
!        check the common block length parameters.                              !/*lb          6*/
      if(gfixcm(nfixcm).ne.one)write(jtty,10)'nfixcm'                           !/*lb          7*/
      if(jfixcm(lfixcm).ne.1)write(jtty,10)'lfixcm'                             !/*lb          8*/
      if(gvarcm(nvarcm).ne.one)write(jtty,10)'nvarcm'                           !/*lb          9*/
      if(jvarcm(lvarcm).ne.1)write(jtty,10)'lvarcm'                             !/*lb         10*/
      if(gephcm(nephcm).ne.one)write(jtty,10)'nephcm'                           !/*lb         11*/
      if(jephcm(lephcm).ne.1)write(jtty,10)'lephcm'                             !/*lb         12*/
      if(gpblcm(npblcm+1).ne.one)write(jtty,10)'npblcm'                         !/*lb         13*/
      if(gpb9cm(1,npblcm+1).ne.one)write(jtty,10)'npblcm'                       !/*lb         14*/
      if(jpblcm(lpblcm+1).ne.1)write(jtty,10)'lpblcm'                           !/*lb         15*/
      if(jpb9cm(1,lpblcm+1).ne.1)write(jtty,10)'lpblcm'                         !/*lb         16*/
      if(gtskcm(ntskcm).ne.one)write(jtty,10)'ntskcm'                           !/*lb         17*/
      if(jtskcm(ltskcm).ne.1)write(jtty,10)'ltskcm'                             !/*lb         18*/
      if(ktskpt(ltskpt).ne.1)write(jtty,10)'ltskpt'                             !/*lb4b        1*/
   10 format(1x,a6,10h is wrong.)                                               !/*lb         19*/
      if(gfixcm(nfixcm).ne.one.or.jfixcm(lfixcm).ne.1.or.                       !/*lb         20*/
     & gvarcm(nvarcm).ne.one.or.jvarcm(lvarcm).ne.1.or.                         !/*lb         21*/
     & gephcm(nephcm).ne.one.or.jephcm(lephcm).ne.1.or.                         !/*lb         22*/
     & gpblcm(npblcm+1).ne.one.or.gpb9cm(1,npblcm+1).ne.one.or.                 !/*lb         23*/
     & jpblcm(lpblcm+1).ne.1.or.jpb9cm(1,lpblcm+1).ne.1.or.                     !/*lb         24*/
     & gtskcm(ntskcm).ne.one.or.jtskcm(ltskcm).ne.1.                            !/*lb4b        2*/
     & or.ktskpt(ltskpt).ne.1)                                                  !/*lb4b        3*/
     & call expire(0,'lblocs','bad common block declaration.')                  !/*lb         26*/
!                                                                               !/*lb         27*/
!        check for sufficient floating-point precision.                         !/*lb         28*/
      if((a+1e-11).ne.one)go to 30                                              !/*cor4-1      9*/
      write(jtty,20)                                                            !/*lb         30*/
   20 format(/51h it appears that this computer has 32-bit words and/           !/*lb         31*/
     & 52h that code was preprocessed without the name "cheap"/                 !/*lb4c        1*/
     & 26h on the *define directive.)                                           !/*lb         33*/
      call expire(0,'lblocs','insufficient floating-point precision')           !/*lb         34*/
   30 return                                                                    !/*lb4a        1*/
      end                                                                       !/*lb         35*/
