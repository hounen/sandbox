!#ident acefpt.F from deck ap                                                   !/*ap          1*/
      subroutine acefpt                                                         !/*ap          2*/
!        sum the type 19 fission cross sections, put the totals after           !/*ap          3*/
!        the rest of the cross sections, and recalculate the locators.          !/*ap          4*/
!        this table is required if the problem has any type 7 tallies;          !/*ap          5*/
!        has any cells where fission is treated as capture; is a                !/*ap          6*/
!        mode n p problem with some expanded photon-production tables;          !/*ap          7*/
!        has any fm cards with pseudo-reactions -6, -7, or -8; is a             !/*apxc        1*/
!        kcode problem; or is a tally (xs) plot problem.                        !/*apxc        2*/
      use allocate_memory                                                       !/*apx         1*/
      use global_data                                                           !/*apx         2*/
!                                                                               !/*apx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*apx         4*/
      implicit integer(kindi) (i-n)                                             !/*apx         5*/
!                                                                               !/*apx         6*/
!                                                                               !/*ap         11*/
!        find min and max energy indices over all fission reactions.            !/*ap         12*/
      mi=100000                                                                 !/*ap         13*/
      ma=0                                                                      !/*ap         14*/
      do 10 j=1,nxs(lnxs+5,iex)                                                 !/*ap         15*/
      if(nint(xss(jxs(ljxs+5,iex)+j-1)).ne.19)go to 10                          !/*ap4c        1*/
      is=jxs(ljxs+7,iex)+nint(xss(jxs(ljxs+6,iex)+j-1))                         !/*ap4c        2*/
      mi=min(mi,nint(xss(is-1)))                                                !/*ap4c        3*/
      ma=max(ma,nint(xss(is-1))+nint(xss(is))-1)                                !/*ap4c        4*/
   10 continue                                                                  !/*ap         20*/
      if(ma.eq.0)return                                                         !/*ap         21*/
!                                                                               !/*ap         22*/
!        form fsig list beginning at location fis=end+1                         !/*ap         23*/
      kf=jxs(ljxs+22,iex)+2                                                     !/*ap         24*/
      xss(kf-1)=mi                                                              !/*ap         25*/
      xss(kf)=ma-mi+1                                                           !/*ap         26*/
      mc=kf+ma-mi+5                                                             !/*ap4a        1*/
      mxs=size(xss)                                                             !/*apx         7*/
      if(mc.gt.mxs) then                                                        !/*apx         8*/
        xss => chg_mem (xss,mc,is)                                              !/*apx         9*/
        if(is.ne.0)call expire(0,'acefpt','chg_mem error')                      !/*apx        10*/
      endif                                                                     !/*apx        11*/
!                                                                               !/*ap         28*/
!        zero the fission cross section storage for accumulation.               !/*ap         29*/
      do 20 i=1,ma-mi+1                                                         !/*ap         30*/
   20 xss(kf+i)=0.                                                              !/*ap         31*/
!                                                                               !/*ap         32*/
!        accumulate fission cross sections for all fission reactions in         !/*ap         33*/
!        corresponding locations of fsig list.                                  !/*ap         34*/
      do 40 j=1,nxs(lnxs+5,iex)                                                 !/*ap         35*/
      if(nint(xss(jxs(ljxs+5,iex)+j-1)).ne.19)go to 40                          !/*ap4c        5*/
      is=jxs(ljxs+7,iex)+nint(xss(jxs(ljxs+6,iex)+j-1))                         !/*ap4c        6*/
      ir=kf+nint(xss(is-1))-mi                                                  !/*ap4c        7*/
      do 30 i=1,nint(xss(is))                                                   !/*ap4c        8*/
   30 xss(ir+i)=xss(ir+i)+xss(is+i)                                             !/*ap         40*/
   40 continue                                                                  !/*ap         41*/
!                                                                               !/*ap         42*/
!        assign new values to fis, end, and table length.                       !/*ap         43*/
      jxs(ljxs+21,iex)=jxs(ljxs+22,iex)+1                                       !/*ap         44*/
      jxs(ljxs+22,iex)=jxs(ljxs+22,iex)+ma-mi+3                                 !/*ap         45*/
      nxs(lnxs+1,iex)=nxs(lnxs+1,iex)+ma-mi+3                                   !/*ap         46*/
      return                                                                    !/*ap         47*/
      end                                                                       !/*ap         48*/
