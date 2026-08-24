!#ident chqcel.F from deck cq                                                   !/*cq          1*/
      subroutine chqcel(r,s)                                                    !/*cq          2*/
!        find the sense of point r,s with respect to all surfaces of the        !/*cq          3*/
!        current cell.  set the logical values into lgc.                        !/*cq          4*/
      use global_data                                                           !/*cqx         1*/
      use imcn_data                                                             !/*cqx         2*/
!                                                                               !/*cqx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*cqx         4*/
      implicit integer(kindi) (i-n)                                             !/*cqx         5*/
!                                                                               !/*cqx         6*/
!                                                                               !/*cq          7*/
      do 20 i=1,nsjv                                                            !/*cq          8*/
      lgc(i)=ljav(llav+i)                                                       !/*cq          9*/
      j=abs(lgc(i))                                                             !/*cq         10*/
      if(j.gt.1000000)go to 20                                                  !/*cq         11*/
      t=(scfq(lscq+1,j)*r+scfq(lscq+2,j))*r+(scfq(lscq+3,j)*s+                  !/*cq         12*/
     & scfq(lscq+4,j))*s+scfq(lscq+5,j)                                         !/*cq         13*/
      k=kst(lkst+j)                                                             !/*cq4a        1*/
      if(k.lt.16.or.k.gt.21)go to 10                                            !/*cq         15*/
      l=mod(k-1,3)+1                                                            !/*cq         16*/
      if((bbb(l+1,1)+bbb(l+1,2)*s-scf(lsc(llsc+j)+l+(1-l)*(k/19)))*             !/*cq         17*/
     & scf(lsc(llsc+j)+5-2*(k/19)).lt.0.)t=1.                                   !/*cq         18*/
   10 lgc(i)=0                                                                  !/*cq         19*/
      if(ljav(llav+i)*t.ge.0.)lgc(i)=1                                          !/*cq         20*/
   20 continue                                                                  !/*cq         21*/
      return                                                                    !/*cq         22*/
      end                                                                       !/*cq         23*/
