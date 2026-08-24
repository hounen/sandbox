!#ident midpnt.F from deck mi                                                   !/*mi          1*/
      subroutine midpnt(j1,rs,tu,jh,ll,m1)                                      !/*mi          2*/
!        check branch jh of surface j1 between intersections rs and tu.         !/*mi          3*/
!        j1=0:  ll indicates whether the section is in the cell.                !/*mi          4*/
!        j1>0:  ll indicates whether the section is on the boundary.            !/*mi          5*/
!        m1=1 or 2 indicates cell is on minus or plus side of j1.               !/*mi          6*/
      use global_data                                                           !/*mix         1*/
      use imcn_data                                                             !/*mix         2*/
!                                                                               !/*mix         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*mix         4*/
      implicit integer(kindi) (i-n)                                             !/*mix         5*/
!                                                                               !/*mix         6*/
      dimension c(3),rs(2),tu(2)                                                !/*mi          9*/
!                                                                               !/*mi         10*/
      ll=0                                                                      !/*mi         11*/
!                                                                               !/*mi         12*/
!        the midpoint is about halfway between if surface j1 is  linear.        !/*mi         13*/
      r=.51729*rs(1)+.48271*tu(1)                                               !/*mi         14*/
      s=.51729*rs(2)+.48271*tu(2)                                               !/*mi         15*/
      if(j1.eq.0)go to 10                                                       !/*mi         16*/
      if(scfq(lscq+1,j1).eq.0..and.rs(2).ne.tu(2))return                        !/*mi         17*/
      if(scfq(lscq+1,j1).eq.0.)go to 10                                         !/*mi         18*/
!                                                                               !/*mi         19*/
!        solve quadratic for the midpoint if surface j1 is not linear.          !/*mi         20*/
      c(1)=scfq(lscq+1,j1)                                                      !/*mi         21*/
      c(2)=scfq(lscq+2,j1)                                                      !/*mi         22*/
      c(3)=scfq(lscq+3,j1)*s**2+scfq(lscq+4,j1)*s+scfq(lscq+5,j1)               !/*mi         23*/
      call quad(c,jj,r,r2)                                                      !/*mi         24*/
      if(jj.eq.0)return                                                         !/*mi         25*/
      if(jj.eq.1)go to 10                                                       !/*mi         26*/
      if(jh.eq.1)r=max(r,r2)                                                    !/*mi         27*/
      if(jh.eq.2)r=min(r,r2)                                                    !/*mi         28*/
!                                                                               !/*mi         29*/
!        find where the midpoint is with respect to the cell.                   !/*mi         30*/
   10 if(r.lt.0.)return                                                         !/*mi         31*/
      call chqcel(r,s)                                                          !/*mi         32*/
      iv=0                                                                      !/*mi         33*/
      do 30 i1=1,2                                                              !/*mi         34*/
      do 20 k=1,nsjv                                                            !/*mi         35*/
      if(ljav(llav+k).eq.j1)lgc(k)=i1-1                                         !/*mi         36*/
   20 if(ljav(llav+k).eq.-j1)lgc(k)=2-i1                                        !/*mi         37*/
      l=lgeval(lgc,nsjv)                                                        !/*mi         38*/
      if(l.ne.0)m1=i1                                                           !/*mi         39*/
   30 iv=iv+l                                                                   !/*mi         40*/
      if(iv.eq.1.or.j1.eq.0.and.iv.eq.2)ll=1                                    !/*mi         41*/
      return                                                                    !/*mi         42*/
      end                                                                       !/*mi         43*/
