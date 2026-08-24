!#ident lgeval.F from deck lg                                                   !/*lg          1*/
      function lgeval(lg,n)                                                     !/*lg          2*/
!        evaluate logical expression lg of length n.                            !/*lg          3*/
!        representation of logical elements:                                    !/*lg          4*/
!           (      1000001       true            1                              !/*lg          5*/
!           )      1000002       false           0                              !/*lg          6*/
!           union  1000003       intersection   implicit                        !/*lg          7*/
      dimension lg(*)                                                           !/*lg          8*/
!                                                                               !/*lg          9*/
      lg(n+1)=1000002                                                           !/*lg         10*/
      i=0                                                                       !/*lg         11*/
      l=0                                                                       !/*lg         12*/
   10 lgeval=1                                                                  !/*lg         13*/
   20 i=i+1                                                                     !/*lg         14*/
      if(lg(i).gt.1)go to 40                                                    !/*lg         15*/
      lgeval=min(lgeval,lg(i))                                                  !/*lg         16*/
      if(lgeval.ne.0)go to 20                                                   !/*lg         17*/
   30 i=i+1                                                                     !/*lg         18*/
      if(lg(i).lt.2)go to 30                                                    !/*lg         19*/
   40 if(lg(i).ne.1000003)go to 50                                              !/*lg         20*/
      if(lgeval.eq.0)go to 10                                                   !/*lg         21*/
      if(l.eq.0)return                                                          !/*lg         22*/
      go to 60                                                                  !/*lg         23*/
   50 if(lg(i).ne.1000001)go to 80                                              !/*lg         24*/
      l=l+1                                                                     !/*lg         25*/
      if(lgeval.ne.0)go to 20                                                   !/*lg         26*/
   60 m=1                                                                       !/*lg         27*/
   70 i=i+1                                                                     !/*lg         28*/
      if(lg(i).eq.1000001)m=m+1                                                 !/*lg         29*/
      if(lg(i).eq.1000002)m=m-1                                                 !/*lg         30*/
      if(m.ne.0)go to 70                                                        !/*lg         31*/
   80 l=l-1                                                                     !/*lg         32*/
      if(i.ne.n+1)go to 20                                                      !/*lg         33*/
      return                                                                    !/*lg         34*/
      end                                                                       !/*lg         35*/
