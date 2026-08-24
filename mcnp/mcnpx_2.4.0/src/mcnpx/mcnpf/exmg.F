!#ident exmg.F from deck eg                                                     !/*eg          1*/
      subroutine exmg(l,n)                                                      !/*eg          2*/
!        remove n words of multigroup cross-section table iex,                  !/*eg          3*/
!        starting at location l, and adjust locators.                           !/*eg          4*/
      use global_data                                                           !/*egx         1*/
!                                                                               !/*egx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*egx         3*/
      implicit integer(kindi) (i-n)                                             !/*egx         4*/
!                                                                               !/*egx         5*/
!                                                                               !/*eg          6*/
      do 10 i=l,jxs(ljxs+1,iex)+nxs(lnxs+1,iex)-1-n                             !/*eg          7*/
   10 xss(i)=xss(i+n)                                                           !/*eg          8*/
      do 20 i=1,17                                                              !/*eg          9*/
   20 if(jxs(ljxs+i,iex).gt.l)jxs(ljxs+i,iex)=jxs(ljxs+i,iex)-n                 !/*eg         10*/
      do 30 i=jxs(ljxs+12,iex),jxs(ljxs+12,iex)+nxs(lnxs+8,iex)-1               !/*eg         11*/
   30 if(jxs(ljxs+1,iex)+nint(xss(i))-1.gt.l)xss(i)=xss(i)-n                    !/*egx         6*/
      do 40 i=jxs(ljxs+13,iex),jxs(ljxs+13,iex)+nxs(lnxs+8,iex)                 !/*eg         13*/
   40 if(jxs(ljxs+1,iex)+nint(xss(i))-1.gt.l)xss(i)=xss(i)-n                    !/*egx         7*/
      do 50 i=jxs(ljxs+16,iex),jxs(ljxs+16,iex)+nxs(lnxs+8,iex)                 !/*eg         15*/
   50 if(jxs(ljxs+1,iex)+nint(xss(i))-1.gt.l)xss(i)=xss(i)-n                    !/*egx         8*/
      do 60 i=jxs(ljxs+17,iex),jxs(ljxs+17,iex)+nxs(lnxs+8,iex)                 !/*eg         17*/
   60 if(jxs(ljxs+1,iex)+nint(xss(i))-1.gt.l)xss(i)=xss(i)-n                    !/*egx         9*/
      nxs(lnxs+1,iex)=nxs(lnxs+1,iex)-n                                         !/*eg         19*/
      return                                                                    !/*eg         20*/
      end                                                                       !/*eg         21*/
