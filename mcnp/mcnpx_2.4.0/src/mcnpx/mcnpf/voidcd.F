!#ident voidcd.F from deck vd                                                   !/*vd          1*/
      subroutine voidcd                                                         !/*vd          2*/
!        modify the problem to run with all voids, for problem checkout.        !/*vd          3*/
      use global_data                                                           !/*vdx         1*/
      use imcn_data                                                             !/*vdx         2*/
!                                                                               !/*vdx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*vdx         4*/
      implicit integer(kindi) (i-n)                                             !/*vdx         5*/
!                                                                               !/*vdx         6*/
!                                                                               !/*vd          6*/
!        zero the cross-section table count and limit the histories.            !/*vd          7*/
      mxe=0                                                                     !/*vd          8*/
      mcal=0                                                                    !/*vd4b        1*/
      mnnm=0                                                                    !/*vd4b        2*/
      if(npp.eq.0)npp=100000                                                    !/*vd          9*/
!                                                                               !/*vd         10*/
!        eliminate tally multipliers.                                           !/*vd         11*/
!        change heating tallies into flux tallies.                              !/*vd         12*/
      m=0                                                                       !/*vd         13*/
      do 20 i=1,ntal                                                            !/*vd         14*/
      do 10 j=1,4                                                               !/*vd         15*/
   10 iptal(lipt+5,j,i)=max(0,j-2)                                              !/*vd         16*/
      if(iptal(lipt+3,1,i).ne.0)m=i                                             !/*vd4a        1*/
   20 if(jptal(ljpt+2,i).eq.6.or.jptal(ljpt+2,i).eq.7)jptal(ljpt+2,i)=4         !/*vd4b        3*/
      if(m.ne.0)call erprnt(1,2,1,jptal(ljpt+1,m),0,0,0,0,                      !/*vd4a        2*/
     & '52hvoid card dangerous if tallyx refers to xsecs. tally,i4')            !/*vd4a        3*/
!                                                                               !/*vd         26*/
!        make all of the cells voids.                                           !/*vd         27*/
      do 30 i=1,mxa                                                             !/*vd4a        4*/
   30 mat(lmat+i)=0                                                             !/*vd4a        5*/
      return                                                                    !/*vd         30*/
      end                                                                       !/*vd         31*/
