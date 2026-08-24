!#ident setcel.F from deck sc                                                   !/*sc          1*/
      subroutine setcel(ib,ie)                                                  !/*sc4b        1*/
!        set up segment ib as the current cell for volume or area.              !/*sc          3*/
!        set ie=-7 if lgc overflow occurs.                                      !/*sc4b        2*/
      use global_data                                                           !/*scx         1*/
      use imcn_data                                                             !/*scx         2*/
!                                                                               !/*scx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*scx         4*/
      implicit integer(kindi) (i-n)                                             !/*scx         5*/
!                                                                               !/*scx         6*/
!                                                                               !/*sc          6*/
      if(dbcn(7).ne.0.)write(iuo,10)jptal(ljpt+1,ital),ib                       !/*sc          7*/
   10 format(/6h tally,i4,3x,7hsegment,i3,5x,45(2h..))                          !/*sc          8*/
!                                                                               !/*sc          9*/
!        modify ljav and ljsv for segment ib.                                   !/*sc         10*/
      if(ib.gt.1)ljav(llav+nsjv)=-ljav(llav+nsjv)                               !/*sc         11*/
      if(ib.eq.iptal(lipt+4,4,ital)-1)return                                    !/*sc         12*/
      if(nsjv.ge.mlgc)ie=-7                                                     !/*sc4b        3*/
      if(ie.eq.-7)return                                                        !/*sc4b        4*/
      nsjv=nsjv+1                                                               !/*sc         13*/
      ljav(llav+nsjv)=lja(llja+abs(lca(llca+iptal(lipt+4,1,ital)))+ib-1)        !/*sc         14*/
      do 20 i=1,nsv                                                             !/*sc         15*/
   20 if(ljsv(llsv+i).eq.abs(ljav(llav+nsjv)))return                            !/*sc         16*/
      nsv=nsv+1                                                                 !/*sc         17*/
      ljsv(llsv+nsv)=abs(ljav(llav+nsjv))                                       !/*sc         18*/
      return                                                                    !/*sc         19*/
      end                                                                       !/*sc         20*/
