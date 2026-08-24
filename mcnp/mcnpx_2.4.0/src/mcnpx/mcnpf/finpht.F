!#ident finpht.F from deck fp                                                   !/*fp          1*/
      subroutine finpht                                                         !/*fp          2*/
!        score pulse height tallies at the end of each history.                 !/*fp          3*/
      use global_data                                                           !/*fpx         1*/
!                                                                               !/*fpx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*fpx         3*/
      implicit integer(kindi) (i-n)                                             !/*fpx         4*/
!                                                                               !/*fpx         5*/
!                                                                               !/*fp          5*/
      li=lipt                                                                   !/*fp          6*/
      do 80 ital=1,ntal                                                         !/*fp          7*/
      if(jptal(ljpt+2,ital).ne.8)go to 80                                       !/*fp          8*/
      j1=ktal+jptal(ljpt+5,ital)+1                                              !/*cor4-1    183*/
      do 70 ibu=1,iptal(li+3,3,ital)                                            !/*fp         11*/
      j2=j1+iptal(li+4,5,ital)*(ibu-1)                                          !/*cor4-1    184*/
      do 60 ir=1,iptal(li+1,3,ital)                                             !/*fp         15*/
      j3=j2+iptal(li+2,5,ital)*(ir-1)                                           !/*fp         16*/
      t=tal(j3)/wgt9(1)                                                         !/*fp         17*/
      if(t.eq.0.)go to 60                                                       !/*fp         18*/
      tal(j3)=0.                                                                !/*fp         19*/
      ibe=1                                                                     !/*fp         20*/
      if(iptal(li+7,1,ital).eq.0)go to 30                                       !/*fp         21*/
      l=lft(llft+2,ital)                                                        !/*fp         22*/
      if(l.eq.0)go to 20                                                        !/*fp         23*/
      if(t.le.0.)go to 20                                                       !/*fp4a        1*/
   10 t1=2.*rang()-1                                                            !/*fp         24*/
      t2=t1**2+rang()**2                                                        !/*fp         25*/
      if(t2.gt.1.)go to 10                                                      !/*fp         26*/
      t=t+.60056120439322*(tds(l+1)+tds(l+2)*sqrt(max(zero,                     !/*fp4b        1*/
     & t+tds(l+3)*t**2)))*sqrt(-log(t2)/t2)*t1                                  !/*fp4b        2*/
   20 ibe=jbin(7,t)                                                             !/*fp         29*/
      if(ibe.eq.0)go to 60                                                      !/*fp         30*/
   30 s=wgt9(1)                                                                 !/*fp         31*/
      if(jptal(ljpt+4,ital).ne.0)s=s*t                                          !/*fp         32*/
      if(iptal(li+7,2,ital).ne.0)s=s*tds(iptal(li+7,2,ital)+ibe)                !/*fp         33*/
      ie=ibe                                                                    !/*fp         35*/
   40 j4=j3+ie-1                                                                !/*cor4-1    186*/
      if(tal(j4).ne.0.)go to 50                                                 !/*fp         37*/
      jtls=jtls+1                                                               !/*fp         38*/
      if(jtls.le.ktls)tal(ktal+nmxf*mxf+jtls)=j4                                !/*fp4a        3*/
   50 tal(j4)=tal(j4)+s                                                         !/*fp         40*/
      ie=ie+iptal(li+7,4,ital)-ibe                                              !/*cor4-1    187*/
      if(ie.le.iptal(li+7,3,ital))go to 40                                      !/*cor4-1    188*/
   60 continue                                                                  !/*fp         43*/
   70 continue                                                                  !/*fp         44*/
   80 continue                                                                  !/*fp         45*/
      return                                                                    !/*fp         46*/
      end                                                                       !/*fp         47*/
