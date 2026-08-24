!#ident ktable.F from deck ke4a                                                 !/*ke4a        1*/
      subroutine ktable                                                         !/*ke4a        2*/
!        print the estimated keffs and standard deviations by cycle.            !/*ke4a        3*/
!        called from subroutine kprint.                                         !/*ke4a        4*/
      use global_data                                                           !/*kex         1*/
!                                                                               !/*kex         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*kex         3*/
      implicit integer(kindi) (i-n)                                             !/*kex         4*/
!                                                                               !/*kex         5*/
      character hf*8,hk(3)*6                                                    !/*ke4c        1*/
      data hk/'k(col)','k(abs)','k(trk)'/                                       !/*ke4c        2*/
      dimension ks(3)                                                           !/*ke4c        3*/
      data ks/6,8,10/                                                           !/*ke4c        4*/
!                                                                               !/*ke4a        7*/
!        print a table that summarizes the various keff estimators.             !/*ke4a        8*/
      write(iuo,10)                                                             !/*ke4a        9*/
   10 format(55h1individual and average keff estimator results by cycle         !/*ke4a       10*/
     & //18h  keff     neutron,3x,25h keff estimators by cycle,10x,             !/*ke4a       11*/
     & 39h average keff estimators and deviations,15x,                          !/*ke4a       12*/
     & 17h average k(c/a/t)/20h cycle   histories  ,                            !/*ke4a       13*/
     & 26h k(coll)  k(abs)  k(track),4x,27h k(coll)  st dev    k(abs) ,         !/*ke4a       14*/
     & 52h st dev   k(track) st dev    k(c/a/t)  st dev    fom/)                !/*ke4a       15*/
!                                                                               !/*ke4a       16*/
!        check to see if the table is truncated.                                !/*ke4a       17*/
      if(kcz.gt.mrkp)write(iuo,20)kcz,mrkp                                      !/*ke4a       18*/
   20 format(/27h the total number of cycles,i4,15h exceeds mrkp =,i4,          !/*ke4a       19*/
     & 60h allowed for cycle storage, so the table is truncated except,         !/*ke4a       20*/
     & 21h for the final value./)                                               !/*ke4a       21*/
!                                                                               !/*ke4a       22*/
!        print the three individual keffs for the inactive cycles.              !/*ke4a       23*/
      do 30 i=1,min(kcz,ikz)                                                    !/*ke4a       24*/
      write(iuo,60)i,nint(rkpl(lrkp+18,i)),(rkpl(lrkp+k,i),k=1,3)               !/*ke4a       25*/
   30 if(i.ne.ikz.and.mod(i,10).eq.0)write(iuo,'(1x,131(1h-))')                 !/*ke4a       26*/
      if(kcz.le.ikz)return                                                      !/*ke4a       27*/
!                                                                               !/*ke4a       28*/
!        print individual, average, and combined active cycle keffs.            !/*ke4a       29*/
      write(iuo,40)                                                             !/*ke4a       30*/
   40 format(1x,19(1h-),28h  begin active keff cycles  ,84(1h-))                !/*ke4a       31*/
      write(iuo,60)ikz+1,nint(rkpl(lrkp+18,ikz+1)),(rkpl(lrkp+k,ikz+1),         !/*ke4a       32*/
     & k=1,3)                                                                   !/*ke4a       33*/
      if(kcz.eq.ikz+1)return                                                    !/*ke4a       34*/
      do 50 i=ikz+2,min(kcz,ikz+3)                                              !/*ke4a       35*/
   50 write(iuo,60)i,nint(rkpl(lrkp+18,i)),(rkpl(lrkp+k,i),k=1,3),              !/*ke4a       36*/
     & (rkpl(lrkp+2*j+4,i),rkpl(lrkp+2*j+5,i),j=1,3)                            !/*ke4a       37*/
      if(kcz.le.ikz+3)return                                                    !/*ke4a       38*/
!                                                                               !/*ke4a       39*/
!        print all of the batch lines with all four estimators.                 !/*ke4a       40*/
      im=min(kcz,mrkp)                                                          !/*ke4a       41*/
      do 70 i=ikz+4,im                                                          !/*ke4a       42*/
      fm=rkpl(lrkp+19,i)                                                        !/*ke4a       43*/
      if(fm.le.9999999..and.fm.ge.9.95)write(hf,'(i8)')nint(fm)                 !/*ke4a       44*/
      if(fm.gt.9999999..or.fm.lt.9.95)write(hf,'(1pe8.1)')fm                    !/*ke4a       45*/
      if(mct.lt.0)hf=' '                                                        !/*ke4a       46*/
      ii=i                                                                      !/*ke4a       47*/
      if(i.eq.im)ii=kcz                                                         !/*ke4a       48*/
      write(iuo,60)ii,nint(rkpl(lrkp+18,i)),(rkpl(lrkp+k,i),k=1,3),             !/*ke4a       49*/
     & (rkpl(lrkp+2*j+4,i),rkpl(lrkp+2*j+5,i),j=1,4),hf                         !/*ke4a       50*/
   60 format(i6,i12,2h |,f8.5,2f9.5,4h  | ,3(2f8.5,2x),2h| ,                    !/*ke4a       51*/
     & 2f8.5,2x,a8)                                                             !/*ke4a       52*/
   70 if(mod(i,10).eq.0)write(iuo,'(1x,131(1h-))')                              !/*ke4a       53*/
      return                                                                    !/*ke4a       54*/
      end                                                                       !/*ke4a       55*/
