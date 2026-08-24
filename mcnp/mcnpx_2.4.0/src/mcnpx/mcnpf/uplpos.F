!#ident uplpos.F from deck ul4b                                                 !/*ul4b        1*/
      subroutine uplpos(x,u,l,d,v,jf)                                           !/*ul4b        2*/
!        calculate the position and direction vectors, x and u, at              !/*ul4b        3*/
!        levels 0 thru l-1 for a particle travelling a distance d at            !/*ul4b        4*/
!        velocity v.  if jf.ne.0 an additional call to uplev is needed.         !/*ul4b        5*/
      use global_data                                                           !/*ulx         1*/
!                                                                               !/*ulx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*ulx         3*/
      implicit integer(kindi) (i-n)                                             !/*ulx         4*/
!                                                                               !/*ulx         5*/
      dimension x(3),u(3)                                                       !/*ul4b        7*/
!                                                                               !/*ul4b        8*/
      do 10 i=1,3                                                               !/*ul4b        9*/
   10 x(i)=x(i)+d*u(i)                                                          !/*ul4b       10*/
      tme=tme+d/v                                                               !/*ul4b       11*/
      if(l.eq.0)return                                                          !/*ul4b       12*/
      call uplev(u)                                                             !/*ul4b       13*/
      do 20 j=0,l-1                                                             !/*ul4b       14*/
      do 20 i=1,3                                                               !/*ul4b       15*/
   20 udt(i,j)=udt(i,j)+d*udt(i+3,j)                                            !/*ul4b       16*/
!                                                                               !/*ul4b       17*/
!        sometimes uplev must be called again to update direction cosine        !/*ul4b       18*/
!        as when uuu is used by knock, kxray, and brems because uplev is        !/*ul4b       19*/
!        is previously called with uold.                                        !/*ul4b       20*/
      if(jf.ne.0)call uplev(uuu)                                                !/*ul4b       21*/
      return                                                                    !/*ul4b       22*/
      end                                                                       !/*ul4b       23*/
