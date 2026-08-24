!#ident matmpy.F from deck mm                                                   !/*mm          1*/
      subroutine matmpy(m,n,l,a,ia,b,ib,c,ic,jt)                                !/*mm          2*/
!        multiply a (or a(transpose)) and b to give c.                          !/*mm          3*/
!        m = number of rows of a (or a(t)) and number of rows of c.             !/*mm          4*/
!        n = number of columns of a (or a(t)) and number of rows of b.          !/*mm          5*/
!        l = number of columns of b and number of columns of c.                 !/*mm          6*/
!        jt = 0 for c=a*b    jt.ne.0 for c=a(t)*b                               !/*mm          7*/
      use global_parameters                                                     !/*mmx         1*/
      implicit real(kindr) (a-h,o-z)                                            !/*mmx         2*/
      implicit integer(kindi) (i-n)                                             !/*mmx         3*/
      dimension a(ia,*),b(ib,*),c(ic,*)                                         !/*mm         10*/
!                                                                               !/*mm         11*/
      if(jt.ne.0)go to 20                                                       !/*mm         12*/
      do 10 j=1,l                                                               !/*mm         13*/
      do 10 i=1,m                                                               !/*mm         14*/
      c(i,j)=0.                                                                 !/*mm         15*/
      do 10 k=1,n                                                               !/*mm         16*/
   10 c(i,j)=c(i,j)+a(i,k)*b(k,j)                                               !/*mm         17*/
      return                                                                    !/*mm         18*/
   20 do 30 j=1,l                                                               !/*mm         19*/
      do 30 i=1,m                                                               !/*mm         20*/
      c(i,j)=0.                                                                 !/*mm         21*/
      do 30 k=1,n                                                               !/*mm         22*/
   30 c(i,j)=c(i,j)+a(k,i)*b(k,j)                                               !/*mm         23*/
      return                                                                    !/*mm         24*/
      end                                                                       !/*mm         25*/
