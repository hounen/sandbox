!#ident hgram.F from deck hg                                                    !/*hg          1*/
      subroutine hgram                                                          !/*hg          3*/
#ifdef MCPLOT                                                                   /*!hgx         1*/
!        draw a histogram.                                                      !/*hg          4*/
      use global_data                                                           !/*hgx         2*/
      use mcplot_data                                                           !/*hgx         3*/
!                                                                               !/*hgx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*hgx         5*/
      implicit integer(kindi) (i-n)                                             !/*hgx         6*/
!                                                                               !/*hgx         7*/
!                                                                               !/*hg          7*/
      xcc(lxcc+1)=ablo(1)                                                       !/*hg          8*/
      ycc(lycc+1)=plim(3)                                                       !/*hg          9*/
      do 10 i=1,npt(1)                                                          !/*hg         10*/
      xcc(lxcc+2*i)=xcc(lxcc+2*i-1)                                             !/*hg         11*/
      ycc(lycc+2*i)=ord(lord+i)                                                 !/*hg         12*/
      if(lax.lt.3)xcc(lxcc+2*i+1)=2.*ab1(lab1+i)-xcc(lxcc+2*i)                  !/*hg         13*/
      if(lax.ge.3)                                                              !/*hg         14*/
     & xcc(lxcc+2*i+1)=log10(2.*10.**ab1(lab1+i)-10.**xcc(lxcc+2*i))            !/*hg         15*/
   10 ycc(lycc+2*i+1)=ycc(lycc+2*i)                                             !/*hg         16*/
      xcc(lxcc+2*npt(1)+2)=xcc(lxcc+2*npt(1)+1)                                 !/*hg         17*/
      ycc(lycc+2*npt(1)+2)=plim(3)                                              !/*hg         18*/
      call plin(2*npt(1)+2,xcc(lxcc+1),ycc(lycc+1))                             !/*hg         19*/
#endif                                                                          /*!hgx         8*/
      return                                                                    !/*hg         20*/
      end                                                                       !/*hg         21*/
