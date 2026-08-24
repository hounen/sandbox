!#ident ksrctp.F from deck ks                                                   !/*ks          1*/
      subroutine ksrctp(mm)                                                     !/*ks          2*/
!        do all i/o on srctp, the kcode source file.                            !/*ks          3*/
      use global_data                                                           !/*ksx         1*/
!                                                                               !/*ksx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*ksx         3*/
      implicit integer(kindi) (i-n)                                             !/*ksx         4*/
!                                                                               !/*ksx         5*/
!                                                                               !/*ks          5*/
      go to(10,20,30,40)mm                                                      !/*ks          6*/
!                                                                               !/*ks          7*/
! >>>>>  mm=1 -- open an existing srctp file.                                   !/*ks          8*/
   10 if(inqire(srctp).ne.0)go to 15                                            !/*cor4-2     77*/
      call erprnt(1,1,0,0,0,0,0,0,'26hfile '//srctp//' unavailable.')           !/*ks4c        1*/
      return                                                                    !/*cor4-2     80*/
   15 open(ius,file=srctp,form='unformatted',status='old')                      !/*cor4-2     81*/
!                                                                               !/*ks         12*/
! >>>>>  mm=2 -- read the source distribution.                                  !/*ks         13*/
   20 rewind ius                                                                !/*ks         14*/
      read(ius)k,ixak,nsa,ist,mrl                                               !/*ks         15*/
      if(k.ne.947830.or.nsa.le.0)call expire(0,'ksrctp',                        !/*ks         16*/
     & srctp//' has wrong format.')                                             !/*ks         17*/
      if(mm.eq.2)read(ius)((fso(lfso+6*j+i),i=1,5),j=0,mrl-1)                   !/*ks         18*/
      return                                                                    !/*ks         19*/
!                                                                               !/*ks         20*/
! >>>>>  mm=3 -- create the srctp file with a unique name.                      !/*ks         21*/
   30 call unique(srctp,jtty)                                                   !/*ks         22*/
      open(ius,file=srctp,form='unformatted',status='new')                      !/*ks         23*/
      return                                                                    !/*ks         24*/
!                                                                               !/*ks         25*/
! >>>>>  mm=4 -- write the source distribution.                                 !/*ks         26*/
   40 rewind ius                                                                !/*ks         27*/
      k=947830                                                                  !/*ks4c        2*/
      write(ius)k,ixak,nsa,ist,msrk                                             !/*ks4c        3*/
      do 50 i=4,6*msrk*min(1,mcal),6                                            !/*ks         29*/
   50 if(fso(lfso+i).ne.0.)                                                     !/*ks         30*/
     & fso(lfso+i)=xss(jxs(ljxs+1,mgegbt(1))-1+int(fso(lfso+i)))                !/*ks         31*/
      write(ius)((fso(lfso+6*j+i),i=1,5),j=0,msrk-1)                            !/*ks         32*/
      do 70 i=4,6*msrk*min(1,mcal),6                                            !/*ks         33*/
      do 60 j=1,jgm(1)                                                          !/*ks         34*/
   60 if(fso(lfso+i).eq.xss(jxs(ljxs+1,mgegbt(1))+j-1))go to 70                 !/*ks         35*/
   70 if(fso(lfso+i).ne.0)fso(lfso+i)=j                                         !/*ks         36*/
      write(iuo,80)srctp,kcy                                                    !/*ks4a        1*/
   80 format(/37h source distribution written to file ,a8,8x,                   !/*ks         38*/
     & 7hcycle =,i6)                                                            !/*ks         39*/
      write(jtty,90)srctp,kcy                                                   !/*ks4a        2*/
   90 format(29h source distribution to file ,a8,8x,7hcycle =,i6)               !/*ks         41*/
      endfile ius                                                               !/*ks4a        3*/
      backspace ius                                                             !/*ks4a        4*/
      return                                                                    !/*ks         42*/
      end                                                                       !/*ks         43*/
