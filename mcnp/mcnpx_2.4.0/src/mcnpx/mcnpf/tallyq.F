!#ident tallyq.F from deck tq                                                   !/*tq          1*/
      subroutine tallyq(ip)                                                     !/*tq          2*/
!        print descriptive information about tally ital.                        !/*tq          3*/
!        ip contains the order of printing.                                     !/*tq          4*/
      use global_data                                                           !/*tqx         1*/
!                                                                               !/*tqx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*tqx         3*/
      implicit integer(kindi) (i-n)                                             !/*tqx         4*/
!                                                                               !/*tqx         5*/
      dimension ip(8),iv(7)                                                     !/*tq4a        1*/
      character ht*91,hq(2,8)*8,hf(mkft)*3                                      !/*tq          7*/
      data hq/'divisors','surface:','areas','surface:',2*' ','volumes',         !/*tq          8*/
     & '   cell:',2*' ','masses','   cell:','masses','   cell:',                !/*tq          9*/
     & 'volumes','   cell:'/                                                    !/*tq         10*/
!                                                                               !/*tq         11*/
!        print the tally heading and modification notices.                      !/*tq         12*/
      call tallyh                                                               !/*tq         13*/
      if(nsr.eq.71.or.nsr.eq.6)write(iuo,10)1./fpi                              !/*tq         14*/
   10 format(51h number of histories used for normalizing tallies =,            !/*tq         15*/
     & f12.2)                                                                   !/*tq         16*/
      if(iptal(lipt+3,2,ital).ne.0)write(iuo,20)                                !/*tq         17*/
   20 format(/11x,42hthis tally is modified by a dose function.)                !/*tq         18*/
      if(iptal(lipt+5,1,ital).eq.0.and.iptal(lipt+5,2,ital).ne.0)               !/*tq         19*/
     & write(iuo,30)tds(iptal(lipt+5,2,ital)+1)                                 !/*tq         20*/
   30 format(/11x,31hthis tally is all multiplied by,1pe13.5)                   !/*tq         21*/
      do 40 i=6,8                                                               !/*tq         22*/
   40 if((ip(7).eq.i.or.ip(8).eq.i).and.iptal(lipt+i,2,ital).ne.0)              !/*tq         23*/
     & go to 50                                                                 !/*tq         24*/
      go to 70                                                                  !/*tq         25*/
   50 write(iuo,60)                                                             !/*tq         26*/
   60 format(/11x,45hthis tally is modified by cm, em or tm cards.)             !/*tq         27*/
   70 n=0                                                                       !/*tq         28*/
      do 80 i=1,mkft                                                            !/*tq         29*/
      if(lft(llft+i,ital).eq.0)go to 80                                         !/*tq         30*/
      n=n+1                                                                     !/*tq         31*/
      hf(n)=hft(i)                                                              !/*tq         32*/
   80 continue                                                                  !/*tq         33*/
      if(n.ne.0)write(iuo,90)(hf(i),i=1,n)                                      !/*tq         34*/
   90 format(11x,30hthis tally is modified by   ft,15a5)                        !/*tq         35*/
!                                                                               !/*tq         36*/
!        print the definitions of any abbreviated bin designators.              !/*tq         37*/
      if(ip(7).ne.1.and.ip(8).ne.1.and.iptal(lipt+4,2,ital).eq.0)               !/*tq         38*/
     & go to 110                                                                !/*tq         39*/
      jb=-1                                                                     !/*tq4a        2*/
      jd=0                                                                      !/*tq4a        3*/
      if(jptal(ljpt+2,ital).ne.5)jd=itds(itds(iptal(lipt+1,1,ital)+1)-1)        !/*tq4a        4*/
      if(ip(7).eq.1.or.ip(8).eq.1.or.jd.lt.0)jb=1                               !/*tq4a        5*/
      do 100 i=1,iptal(lipt+1,3,ital)                                           !/*tq4a        6*/
      if(jptal(ljpt+2,ital).ne.5)jd=itds(itds(iptal(lipt+1,1,ital)+i)-1)        !/*tq4a        7*/
      if(jb.lt.0.and.jd.gt.0)go to 100                                          !/*tq4a        8*/
      call binval(i,jb,ht,j)                                                    !/*tq4a        9*/
      if(j.ne.0)call binlin(i,jb,ht)                                            !/*tq4a       10*/
  100 continue                                                                  !/*tq4a       11*/
  110 do 130 j=4,5                                                              !/*tq         43*/
      if(iptal(lipt+j,1,ital).eq.0.or.ip(7).ne.j.and.ip(8).ne.j)                !/*tq         44*/
     & go to 130                                                                !/*tq         45*/
      do 120 i=1,iptal(lipt+j,3,ital)                                           !/*tq         46*/
  120 call binlin(i,j,'1')                                                      !/*tq         47*/
  130 continue                                                                  !/*tq         48*/
!                                                                               !/*tq         49*/
!        print table of divisors, areas, volumes, or masses.                    !/*tq         50*/
      if(iptal(lipt+4,2,ital).eq.0)return                                       !/*tq         51*/
      mk=iptal(lipt+1,3,ital)                                                   !/*tq         52*/
      ml=iptal(lipt+4,3,ital)                                                   !/*tq         53*/
      n=0                                                                       !/*tq4a       12*/
      do 150 i=1,mk                                                             !/*tq4a       13*/
      if(i.eq.1)write(iuo,'(/11x,a8)')hq(1,jptal(ljpt+2,ital))                  !/*tq4a       14*/
      if(jb.lt.0.and.itds(itds(iptal(lipt+1,1,ital)+i)-1).gt.0)go to 140        !/*tq4a       15*/
      if(n.eq.0)ht=' '                                                          !/*tq4a       16*/
      n=n+1                                                                     !/*tq4a       17*/
      iv(n)=i-1                                                                 !/*tq4a       18*/
      call binval(i,jb,ht(13*n-10:13*n),j)                                      !/*tq4a       19*/
  140 if(n.lt.7.and.i.lt.mk)go to 150                                           !/*tq4a       20*/
      write(iuo,'(16x,a8,a91)')hq(2,jptal(ljpt+2,ital)),ht                      !/*tq4a       21*/
      if(iptal(lipt+4,1,ital).ne.0)write(iuo,'(15x,7hsegment)')                 !/*tq4a       22*/
      do 145 il=1,ml                                                            !/*tq4a       23*/
      ht=' '                                                                    !/*tq4a       24*/
      if(iptal(lipt+4,1,ital).ne.0)call binval(il,4,ht,j)                       !/*tq4a       25*/
  145 write(iuo,160)ht(1:11),(tds(iptal(lipt+4,2,ital)+                         !/*tq4a       26*/
     & iv(j)*ml+il),j=1,n)                                                      !/*tq4a       27*/
      n=0                                                                       !/*tq4a       28*/
  150 continue                                                                  !/*tq4a       29*/
  160 format(11x,a11,1x,1p7e13.5)                                               !/*tq         67*/
      return                                                                    !/*tq         68*/
      end                                                                       !/*tq         69*/
