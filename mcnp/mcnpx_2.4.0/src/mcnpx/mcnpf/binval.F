!#ident binval.F from deck bv                                                   !/*bv          1*/
      subroutine binval(ib,jb,hs,lf)                                            !/*bv          2*/
!        return hs, the short form value of bin ib of bin type jb.              !/*bv          3*/
!        lf=1 if cell or surface bin letter is returned, zero otherwise.        !/*bv          4*/
      use global_data                                                           !/*bvx         1*/
!                                                                               !/*bvx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*bvx         3*/
      implicit integer(kindi) (i-n)                                             !/*bvx         4*/
!                                                                               !/*bvx         5*/
      character hs*11,ht*11,hf*7                                                !/*bv          6*/
!                                                                               !/*bv          7*/
      lf=0                                                                      !/*bv          8*/
      write(hs,'(i7)')ib                                                        !/*bv          9*/
#ifdef RADIOG                                                                   /*!bvx         6*/
      if(jptal(ljpt+8,ital).gt.2.and.jb.eq.4)go to 50                           !/*bvx         7*/
#endif                                                                          /*!bvx         8*/
      go to(10,40,50,60,60,50,50,50)abs(jb)                                     !/*bv4a        1*/
!                                                                               !/*bv         11*/
! >>>>>  jb=1 -- cell, surface, or detector bin.                                !/*bv         12*/
   10 if(jptal(ljpt+2,ital).eq.5)return                                         !/*bv         13*/
      if(ib.eq.iptal(lipt+1,4,ital))go to 70                                    !/*bv4a        2*/
      i=itds(iptal(lipt+1,1,ital)+ib)                                           !/*bv         14*/
      n=itds(i+itds(i)+1)+1                                                     !/*bv4a        3*/
      if(jb.lt.0)n=1                                                            !/*bv4a        4*/
      l=1                                                                       !/*bv         16*/
      ht=' '                                                                    !/*bv         17*/
      if(n.gt.1)ht='('                                                          !/*bv         18*/
      do 25 il=1,n                                                              !/*bv4a        5*/
      j=i                                                                       !/*bv4a        6*/
      if(il.gt.1)j=itds(i+itds(i)+il)                                           !/*bv4a        7*/
      n2=itds(j)                                                                !/*bv4a        8*/
      if(n.eq.1.and.n2.gt.1)ht(l:l)='('                                         !/*bv4a        9*/
      do 20 in=1,n2                                                             !/*bv4a       10*/
      j=j+1                                                                     !/*bv4a       11*/
      kfq=0                                                                     !/*bv4c        1*/
      if(jptal(ljpt+2,ital).gt.3.or.il.ne.1)go to 13                            !/*bv4c        2*/
      r=nsf(lnsf+itds(j))                                                       !/*bv4c        3*/
   13 if(jptal(ljpt+2,ital).gt.3.or.il.gt.1)r=ncl(lncl+abs(itds(j)))            !/*bv4c        4*/
      l1=log10(.5+r)+1.                                                         !/*bv4a       14*/
      if(l+l1+1.gt.11)go to 30                                                  !/*bv4a       15*/
      if(kfq.ne.0.and.l+l1+3.gt.11)go to 30                                     !/*bv4c        5*/
      write(hf,'(i7)')nint(r)                                                   !/*bv4a       16*/
      ht(l+1:l+l1)=hf(8-l1:7)                                                   !/*bv4a       17*/
      l=l+l1+1                                                                  !/*bv4a       18*/
      if(kfq.ne.0)write(ht(l:l+1),'(1h.,i1)')kfq                                !/*bv4c        6*/
      if(kfq.ne.0)l=l+2                                                         !/*bv4c        7*/
      if(itds(j).gt.0)go to 20                                                  !/*bv4a       19*/
      ht(l:l)='['                                                               !/*bv4a       20*/
      do 15 m=1,3                                                               !/*bv4a       21*/
      r=abs(itds(j+m))                                                          !/*bv4a       22*/
      l1=log10(.5+r)+1.                                                         !/*bv4a       23*/
      if(itds(j+m).le.0)l1=l1+1                                                 !/*bv4a       24*/
      if(l+l1+1.gt.11)go to 30                                                  !/*bv4a       25*/
      write(hf,'(i7)')itds(j+m)                                                 !/*bv4a       26*/
      ht(l+1:l+l1)=hf(8-l1:7)                                                   !/*bv4a       27*/
   15 l=l+l1+1                                                                  !/*bv4a       28*/
      ht(l:l)=']'                                                               !/*bv4a       29*/
      if(l+1.gt.11)go to 30                                                     !/*bv4a       30*/
      l=l+1                                                                     !/*bv4a       31*/
      j=j+3                                                                     !/*bv4a       32*/
   20 continue                                                                  !/*bv4a       33*/
   25 if(n.gt.1)ht(l:l)='<'                                                     !/*bv4a       34*/
      if(n.gt.1.or.n2.gt.1)ht(l:l)=')'                                          !/*bv4a       35*/
      hs=' '                                                                    !/*bv         28*/
      hs(6-l/2:11)=ht                                                           !/*bv         29*/
      return                                                                    !/*bv         30*/
   30 hs='      '//char(ichar('a')-1+mod(ib,26))                                !/*bv         31*/
      if(ib.gt.26)hs(6:6)=char(ichar('a')-1+ib/26)                              !/*bv         32*/
      lf=1                                                                      !/*bv         33*/
      return                                                                    !/*bv         34*/
!                                                                               !/*bv         35*/
! >>>>>  jb=2 -- total vs. flagged or total vs. direct.                         !/*bv         36*/
   40 if(ib.eq.1)go to 70                                                       !/*bv         37*/
      hs='  flagged'                                                            !/*bv         38*/
      if(jptal(ljpt+2,ital).eq.5)hs='   direct'                                 !/*bv         39*/
      return                                                                    !/*bv         40*/
!                                                                               !/*bv         41*/
! >>>>>  jb=3, 6, 7, or 8                                                       !/*bv         42*/
   50 if(ib.eq.iptal(lipt+jb,4,ital))go to 70                                   !/*bv         43*/
      if(iptal(lipt+jb,1,ital).gt.0)                                            !/*bv         44*/
     & write(hs,'(1pe11.4)')tds(iptal(lipt+jb,1,ital)+ib)                       !/*bv         45*/
      return                                                                    !/*bv         46*/
!                                                                               !/*bv         47*/
! >>>>>  jb=4 or 5                                                              !/*bv         48*/
   60 if(ib.ne.iptal(lipt+jb,4,ital))return                                     !/*bv         49*/
!                                                                               !/*bv         50*/
!        various totals cases.                                                  !/*bv         51*/
   70 hs='   total'                                                             !/*bv         52*/
#ifdef RADIOG                                                                   /*!bvx         9*/
      if(jptal(ljpt+2,ital).eq.5)return                                         !/*bvx        10*/
#endif                                                                          /*!bvx        11*/
      if(jb.eq.4)hs='   whole'                                                  !/*bv         53*/
      return                                                                    !/*bv         54*/
      end                                                                       !/*bv         55*/
