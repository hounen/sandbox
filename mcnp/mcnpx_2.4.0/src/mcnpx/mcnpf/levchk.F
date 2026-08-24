!#ident levchk.F from deck lc                                                   !/*lc          1*/
      function levchk(ji)                                                       !/*lc          2*/
!        return zero if xyz is in the multilevel cell pointed to                !/*lc          3*/
!        by -icl, otherwise not zero. return ji=rejected cell.                  !/*cor4-2    187*/
      use global_data                                                           !/*lcx         1*/
!                                                                               !/*lcx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*lcx         3*/
      implicit integer(kindi) (i-n)                                             !/*lcx         4*/
!                                                                               !/*lcx         5*/
!                                                                               !/*lc          6*/
      levchk=0                                                                  !/*lc          7*/
      ip=-icl                                                                   !/*lc          8*/
      ll=kcp(ip-2)                                                              !/*lc          9*/
!                                                                               !/*lc         10*/
!        check xyz level and levels above.                                      !/*lc         11*/
      xx=xxx                                                                    !/*lc         12*/
      yy=yyy                                                                    !/*lc         13*/
      zz=zzz                                                                    !/*lc         14*/
      ji=iclp(1,ll)                                                             !/*lc         15*/
      do 50 lz=0,ll                                                             !/*lc         16*/
      l=ll-lz                                                                   !/*lc         17*/
      if(ji.eq.0)go to 10                                                       !/*lc         18*/
      call chkcel(ji,0,levchk)                                                  !/*lc         19*/
      isef(kise+1,ji)=isef(kise+1,ji)+iclp(5,l)                                 !/*lc4b        1*/
      if(levchk.ne.0)return                                                     !/*lc         21*/
      if(jun(ljun+ji).lt.0)go to 60                                             !/*lc         22*/
   10 if(l.eq.0)go to 60                                                        !/*lc         23*/
      if(ji.eq.0)go to 30                                                       !/*cor4-2    188*/
      if(lat(llat+1,ji).eq.0)go to 30                                           !/*lc         24*/
      if(iclp(2,l).eq.98765432)go to 150                                        !/*lc4b        2*/
      n=lat(llat+2,ji)                                                          !/*lc         25*/
      do 20 i=1,3                                                               !/*lc         26*/
   20 gpblcm(i)=gpblcm(i)+iclp(2,l)*vcl(lvcl+i,1,n)+                            !/*lc         27*/
     & iclp(3,l)*vcl(lvcl+i,2,n)+iclp(4,l)*vcl(lvcl+i,3,n)                      !/*lc         28*/
   30 ji=iclp(1,l-1)                                                            !/*lc         29*/
      if(ji.eq.0)go to 150                                                      !/*lc4b        3*/
      j=-mfl(lmfl+1,ji)                                                         !/*lc4b        4*/
      ju=-j                                                                     !/*lc4b        5*/
      m=mfl(lmfl+3,ji)                                                          !/*lc4b        6*/
      if(j.le.0)go to 35                                                        !/*lc4b        7*/
      if(iclp(2,l-1).eq.98765432)go to 150                                      !/*lc4b        8*/
      k=3+iclp(2,l-1)-laf(j+1,1)+laf(j+1,2)*                                    !/*lc4b        9*/
     & (iclp(3,l-1)-laf(j+2,1)+laf(j+2,2)*(iclp(4,l-1)-laf(j+3,1)))             !/*lc4b       10*/
      ju=laf(j+1,k)                                                             !/*lc4b       11*/
      m=laf(j+3,k)                                                              !/*lc4b       12*/
   35 if(iclp(1,l).eq.0)go to 37                                                !/*lc4b       13*/
      if(abs(jun(ljun+iclp(1,l))).ne.ju)go to 160                               !/*lc4b       14*/
   37 if(m.eq.0)go to 50                                                        !/*lc4b       15*/
      x=xxx                                                                     !/*lc         35*/
      y=yyy                                                                     !/*lc         36*/
      z=zzz                                                                     !/*lc         37*/
      do 40 i=1,3                                                               !/*lc         38*/
   40 gpblcm(i)=x*trf(ltrf+3*i+2,m)+y*trf(ltrf+3*i+3,m)+                        !/*lc         39*/
     & z*trf(ltrf+3*i+4,m)+trf(ltrf+i+14,m)                                     !/*lc         40*/
   50 continue                                                                  !/*lc         41*/
   60 xxx=xx                                                                    !/*lc         42*/
      yyy=yy                                                                    !/*lc         43*/
      zzz=zz                                                                    !/*lc         44*/
      ji=iclp(1,ll)                                                             !/*lc4b       16*/
      iii=iclp(2,ll)                                                            !/*lc4b       17*/
      jjj=iclp(3,ll)                                                            !/*lc4b       18*/
      kkk=iclp(4,ll)                                                            !/*lc4b       19*/
!                                                                               !/*lc         45*/
!        check levels below the xyz level.                                      !/*lc         46*/
      ju=0                                                                      !/*lc         47*/
      if(ll.eq.0)go to 65                                                       !/*lc4b       20*/
      j=-mfl(lmfl+1,iclp(1,ll-1))                                               !/*lc4b       21*/
      ju=-j                                                                     !/*lc4b       22*/
      if(j.eq.0)go to 160                                                       !/*lc4b       23*/
      if(j.lt.0)go to 65                                                        !/*lc4b       24*/
      ju=laf(j+1,3+iclp(2,ll-1)-laf(j+1,1)+laf(j+1,2)*                          !/*lc4b       25*/
     & (iclp(3,ll-1)-laf(j+2,1)+laf(j+2,2)*(iclp(4,ll-1)-laf(j+3,1))))          !/*lc4b       26*/
      if(ju.ne.abs(jun(ljun+iclp(1,ll-1))))go to 65                             !/*lc4b       27*/
      do 62 l=ll,kcp(ip-1)                                                      !/*lc4b       28*/
   62 if(iclp(1,l).ne.0)go to 160                                               !/*lc4b       29*/
      return                                                                    !/*lc4b       30*/
   65 do 140 l=ll,kcp(ip-1)                                                     !/*lc4b       31*/
!                                                                               !/*lc         51*/
!        find the cell of the level above if it is not specified.               !/*lc         52*/
      if(ji.ne.0)go to 100                                                      !/*lc         53*/
      do 70 i=1,nlse                                                            !/*lc         54*/
      ji=lse(klse+i)                                                            !/*lc         55*/
      if(abs(jun(ljun+ji)).ne.ju)go to 70                                       !/*lc         56*/
      call chkcel(ji,0,j)                                                       !/*lc         57*/
      if(j.eq.0)go to 95                                                        !/*lc4b       32*/
   70 continue                                                                  !/*lc         59*/
      do 80 ji=1,mxa                                                            !/*lc         60*/
      if(abs(jun(ljun+ji)).ne.ju)go to 80                                       !/*lc         61*/
      call chkcel(ji,0,j)                                                       !/*lc         62*/
      if(j.eq.0)go to 90                                                        !/*lc         63*/
   80 continue                                                                  !/*lc         64*/
      go to 170                                                                 !/*lc4b       33*/
   90 nlse=nlse+1                                                               !/*lc         67*/
      lse(klse+nlse)=ji                                                         !/*lc         68*/
   95 isef(kise+1,ji)=isef(kise+1,ji)+iclp(5,l)                                 !/*lc4b       34*/
      if(lat(llat+1,ji).ne.0)go to 180                                          !/*lc4b       35*/
!                                                                               !/*lc         70*/
!        check the cell of the current level if it is specified.                !/*lc         71*/
  100 j=-mfl(lmfl+1,ji)                                                         !/*lc4b       36*/
      ju=-j                                                                     !/*lc4b       37*/
      m=mfl(lmfl+3,ji)                                                          !/*lc4b       38*/
      if(j.le.0)go to 105                                                       !/*lc4b       39*/
      k=3+iii-laf(j+1,1)+laf(j+1,2)*                                            !/*lc4b       40*/
     & (jjj-laf(j+2,1)+laf(j+2,2)*(kkk-laf(j+3,1)))                             !/*lc4b       41*/
      ju=laf(j+1,k)                                                             !/*lc4b       42*/
      m=laf(j+3,k)                                                              !/*lc4b       43*/
      if(ju.ne.abs(jun(ljun+ji)))go to 105                                      !/*lc4b       44*/
      do 102 j=l+1,kcp(ip-1)                                                    !/*lc4b       45*/
  102 if(iclp(1,j).ne.0)go to 160                                               !/*lc4b       46*/
      go to 145                                                                 !/*lc4b       47*/
  105 if(m.eq.0)go to 120                                                       !/*lc4b       48*/
      x=xxx                                                                     !/*lc         77*/
      y=yyy                                                                     !/*lc         78*/
      z=zzz                                                                     !/*lc         79*/
      do 110 i=1,3                                                              !/*lc         80*/
  110 gpblcm(i)=x*trf(ltrf+i+4,m)+y*trf(ltrf+i+7,m)+                            !/*lc         81*/
     & z*trf(ltrf+i+10,m)+trf(ltrf+i+1,m)                                       !/*lc         82*/
  120 if(l.eq.kcp(ip-1))go to 140                                               !/*lc4b       49*/
      if(ju.eq.0)go to 160                                                      !/*lc4b       50*/
      ji=iclp(1,l+1)                                                            !/*lc4b       51*/
      if(ji.eq.0)go to 140                                                      !/*lc         84*/
      if(abs(jun(ljun+ji)).ne.ju)go to 160                                      !/*lc4b       52*/
      if(lat(llat+1,ji).eq.0)go to 130                                          !/*lc         85*/
      call findel(ji)                                                           !/*lc         86*/
      if(iclp(2,l+1).eq.98765432)go to 130                                      !/*lc4b       53*/
      if(iclp(2,l+1).ne.iii.or.iclp(3,l+1).ne.jjj.or.iclp(4,l+1).ne.kkk)        !/*lc4b       54*/
     & go to 190                                                                !/*lc4b       55*/
  130 call chkcel(ji,0,levchk)                                                  !/*lc         92*/
      isef(kise+1,ji)=isef(kise+1,ji)+iclp(5,l+1)                               !/*lc4b       56*/
      if(levchk.ne.0)return                                                     !/*lc         94*/
  140 continue                                                                  !/*lc         95*/
  145 xxx=xx                                                                    !/*lc4b       57*/
      yyy=yy                                                                    !/*lc         97*/
      zzz=zz                                                                    !/*lc         98*/
      return                                                                    !/*lc         99*/
!                                                                               !/*lc4b       58*/
!        distinguish between various repeated structures source errors.         !/*lc4b       59*/
  150 levchk=-1                                                                 !/*lc4b       60*/
      return                                                                    !/*lc4b       61*/
  160 levchk=-2                                                                 !/*lc4b       62*/
      return                                                                    !/*lc4b       63*/
  170 levchk=-3                                                                 !/*lc4b       64*/
      return                                                                    !/*lc4b       65*/
  180 levchk=-4                                                                 !/*lc4b       66*/
      return                                                                    !/*lc4b       67*/
  190 levchk=-5                                                                 !/*lc4b       68*/
      return                                                                    !/*lc4b       69*/
      end                                                                       !/*lc        100*/
