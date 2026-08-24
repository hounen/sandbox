!#ident check_tr.F from deck cktr                                               !/*cktr        1*/
      subroutine check_tr                                                       !/*cktr        2*/
!                                                                               !/*cktr        3*/
      use global_data                                                           !/*cktr        4*/
      use imcn_data                                                             !/*cktr        5*/
!                                                                               !/*cktr        6*/
      implicit real(kindr) (a-h,o-z)                                            !/*cktr        7*/
      implicit integer(kindi) (i-n)                                             !/*cktr        8*/
!                                                                               !/*cktr        9*/
!        check for unused tr cards.                                             !/*cktr       10*/
      do 70 m=1,mxtr                                                            !/*cktr       11*/
      i=nint(abs(trf(ltrf+1,m)))                                                !/*cktr       12*/
      if(i.gt.1000)go to 70                                                     !/*cktr       13*/
!                                                                               !/*cktr       14*/
!        check for existence of sdef or ssr transformation.                     !/*cktr       15*/
      if (nsr.ne.6) then                                                        !/*cktr       16*/
         do 10 j=1,imt                                                          !/*cktr       17*/
         jt=srv(1,20)                                                           !/*cktr       18*/
         if(imt.gt.1)jt=spf(ksd(lksd+13,ivdis(20))+1,j)                         !/*cktr       19*/
         if(jt.eq.0)go to 10                                                    !/*cktr       20*/
         if(m.eq.jt)go to 70                                                    !/*cktr       21*/
   10    continue                                                               !/*cktr       22*/
      else                                                                      !/*cktr       23*/
         do 20 j=1,imt                                                          !/*cktr       24*/
         jt=srv(1,5)                                                            !/*cktr       25*/
         if(imt.gt.1)jt=spf(ksd(lksd+13,ivdis(5))+1,j)                          !/*cktr       26*/
         if(jt.eq.0)go to 20                                                    !/*cktr       27*/
         if(m.eq.jt)go to 70                                                    !/*cktr       28*/
   20    continue                                                               !/*cktr       29*/
      endif                                                                     !/*cktr       30*/
!                                                                               !/*cktr       31*/
      do 30 j=1,mxj                                                             !/*cktr       32*/
   30 if(jtr(ljtr+j).eq.i)go to 70                                              !/*cktr       33*/
      if(junf.eq.0)go to 60                                                     !/*cktr       34*/
      do 50 j=1,mxa                                                             !/*cktr       35*/
      k=-mfl(lmfl+1,j)                                                          !/*cktr       36*/
      if(k.le.0)go to 50                                                        !/*cktr       37*/
      do 40 n=3,2+laf(k+1,2)*laf(k+2,2)*laf(k+3,2)                              !/*cktr       38*/
   40 if(laf(k+3,n).eq.m)go to 70                                               !/*cktr       39*/
   50 if(mfl(lmfl+3,j).eq.m.or.ktr(lktr+j).eq.i)go to 70                        !/*cktr       40*/
   60 continue                                                                  !/*cktr       41*/
#ifdef MESHTAL                                                                  /*!cktr       42*/
      if(nmesh.gt.0) then                                                       !/*cktr       43*/
         call erprnt(1,2,1,i,0,0,0,0,                                           !/*cktr       44*/
     &    '2htr,i3,37h card could be for mesh tallies only.')                   !/*cktr       45*/
      else                                                                      !/*cktr       46*/
         call erprnt(1,2,1,i,0,0,0,0,'2htr,i3,13h card unused.')                !/*cktr       47*/
      endif                                                                     !/*cktr       48*/
#else                                                                           /*!cktr       49*/
      call erprnt(1,2,1,i,0,0,0,0,'2htr,i3,13h card unused.')                   !/*cktr       50*/
#endif                                                                          /*!cktr       51*/
   70 continue                                                                  !/*cktr       52*/
!                                                                               !/*cktr       53*/
      return                                                                    !/*cktr       54*/
      end                                                                       !/*cktr       55*/
