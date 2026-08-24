!#ident hpsort.F from deck hp4a                                                 !/*hp4a        1*/
      subroutine hpsort(it,kn,ks,np,ns)                                         !/*hp4a        2*/
!        sort (heapsort) the np extreme tallies into ascending order.           !/*hp4a        3*/
!        it is the tally number, kn and ks are offsets in the nhsd and          !/*hp4a        4*/
!        shsd arrays, np is the number of extreme tallies in the shsd           !/*hp4a        5*/
!        array, and ns is the number of big,small tallies in array stt.         !/*hp4a        6*/
      use global_data                                                           !/*hpx         1*/
!                                                                               !/*hpx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*hpx         3*/
      implicit integer(kindi) (i-n)                                             !/*hpx         4*/
!                                                                               !/*hpx         5*/
      dimension ar(2*ntp)                                                       !/*hp4a        8*/
!                                                                               !/*hp4a        9*/
!        fill array ar with the history tally points to be sorted.              !/*hp4a       10*/
      do 10 j=1,ns                                                              !/*hp4a       11*/
   10 ar(j)=stt(kstt+j,it)                                                      !/*hp4a       12*/
      do 20 j=1,np                                                              !/*hp4a       13*/
   20 ar(ns+j)=shsd(ks+j,it)                                                    !/*hp4a       14*/
      nh=ns+np                                                                  !/*hp4a       15*/
      ir=nh                                                                     !/*hp4a       16*/
      l=ir/2+1                                                                  !/*hp4a       17*/
      go to 40                                                                  !/*hp4a       18*/
!                                                                               !/*hp4a       19*/
!        perform the sort using the heapsort (nlogn) algorithm.                 !/*hp4a       20*/
   30 ar(ii)=a                                                                  !/*hp4a       21*/
   40 if(l.le.1)go to 90                                                        !/*hp4a       22*/
      l=l-1                                                                     !/*hp4a       23*/
      a=ar(l)                                                                   !/*hp4a       24*/
   50 ii=l                                                                      !/*hp4a       25*/
      j=l+l                                                                     !/*hp4a       26*/
      if(j-ir)60,70,30                                                          !/*hp4a       27*/
   60 if(ar(j).lt.ar(j+1))j=j+1                                                 !/*hp4a       28*/
   70 if(a.ge.ar(j))go to 80                                                    !/*hp4a       29*/
      ar(ii)=ar(j)                                                              !/*hp4a       30*/
      ii=j                                                                      !/*hp4a       31*/
      j=j+j                                                                     !/*hp4a       32*/
      if(j-ir)60,70,30                                                          !/*hp4a       33*/
   80 j=ir+1                                                                    !/*hp4a       34*/
      if(j-ir)60,70,30                                                          !/*hp4a       35*/
   90 a=ar(ir)                                                                  !/*hp4a       36*/
      ar(ir)=ar(1)                                                              !/*hp4a       37*/
      ir=ir-1                                                                   !/*hp4a       38*/
      if(ir.ne.1)go to 50                                                       !/*hp4a       39*/
      ar(1)=a                                                                   !/*hp4a       40*/
!                                                                               !/*hp4a       41*/
!        fill shsd(kshs+nsp+6,it) with the ntp largest,smallest values.         !/*hp4a       42*/
      nt=min(ntp,nh)                                                            !/*hp4a       43*/
      do 100 j=1,nt                                                             !/*hp4a       44*/
  100 shsd(ks+j,it)=ar(nh-nt+j)                                                 !/*hp4a       45*/
      nhsd(kn+1,it)=nt                                                          !/*hp4a       46*/
      nhsd(kn+2,it)=0                                                           !/*hp4a       47*/
      if(nt.eq.ntp)shsd(ks+ntp+1,it)=shsd(ks+1,it)                              !/*hp4a       48*/
      return                                                                    !/*hp4a       49*/
      end                                                                       !/*hp4a       50*/
