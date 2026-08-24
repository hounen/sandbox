!#ident levcel.F from deck ll                                                   !/*ll          1*/
      subroutine levcel                                                         !/*ll          2*/
!        set up udt and icl for a particle at a multilevel location.            !/*ll          3*/
      use global_data                                                           !/*llx         1*/
!                                                                               !/*llx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*llx         3*/
      implicit integer(kindi) (i-n)                                             !/*llx         4*/
!                                                                               !/*llx         5*/
!                                                                               !/*ll          5*/
      n=0                                                                       !/*ll          6*/
      ll=0                                                                      !/*ll          7*/
      ji=icl                                                                    !/*ll          8*/
      if(ji.gt.0)go to 50                                                       !/*ll          9*/
      ip=-icl                                                                   !/*ll         10*/
      n=kcp(ip-1)                                                               !/*ll         11*/
      ll=kcp(ip-2)                                                              !/*ll         12*/
!                                                                               !/*ll         13*/
!        do the levels above the xyz level.                                     !/*ll         14*/
      udt(1,ll)=xxx                                                             !/*ll         15*/
      udt(2,ll)=yyy                                                             !/*ll         16*/
      udt(3,ll)=zzz                                                             !/*ll         17*/
      iii=iclp(2,ll)                                                            !/*ll         18*/
      jjj=iclp(3,ll)                                                            !/*ll         19*/
      kkk=iclp(4,ll)                                                            !/*ll         20*/
      do 40 lz=0,ll-1                                                           !/*ll         21*/
      l=ll-1-lz                                                                 !/*ll         22*/
      do 10 i=1,4                                                               !/*ll         23*/
   10 udt(6+i,l)=iclp(i,l)                                                      !/*ll         24*/
      j=-mfl(lmfl+1,iclp(1,l))                                                  !/*ll         25*/
      if(j.lt.0)m=mfl(lmfl+3,iclp(1,l))                                         !/*ll         26*/
      if(j.gt.0)m=laf(j+3,int(3+udt(8,l)-laf(j+1,1)+laf(j+1,2)*                 !/*ll         27*/
     & (udt(9,l)-laf(j+2,1)+laf(j+2,2)*(udt(10,l)-laf(j+3,1)))))                !/*ll         28*/
      x=udt(1,l+1)                                                              !/*ll         29*/
      y=udt(2,l+1)                                                              !/*ll         30*/
      z=udt(3,l+1)                                                              !/*ll         31*/
      if(iclp(1,l+1).eq.0)go to 20                                              !/*ll4b        1*/
      if(lat(llat+1,iclp(1,l+1)).eq.0)go to 20                                  !/*ll4b        2*/
      k=lat(llat+2,iclp(1,l+1))                                                 !/*ll4b        3*/
      x=x+iclp(2,l+1)*vcl(lvcl+1,1,k)+iclp(3,l+1)*vcl(lvcl+1,2,k)+              !/*ll4b        4*/
     & iclp(4,l+1)*vcl(lvcl+1,3,k)                                              !/*ll4b        5*/
      y=y+iclp(2,l+1)*vcl(lvcl+2,1,k)+iclp(3,l+1)*vcl(lvcl+2,2,k)+              !/*ll4b        6*/
     & iclp(4,l+1)*vcl(lvcl+2,3,k)                                              !/*ll4b        7*/
      z=z+iclp(2,l+1)*vcl(lvcl+3,1,k)+iclp(3,l+1)*vcl(lvcl+3,2,k)+              !/*ll4b        8*/
     & iclp(4,l+1)*vcl(lvcl+3,3,k)                                              !/*ll4b        9*/
   20 if(m.eq.0)go to 30                                                        !/*ll         40*/
      udt(1,l)=x*trf(ltrf+5,m)+y*trf(ltrf+6,m)+                                 !/*ll         41*/
     & z*trf(ltrf+7,m)+trf(ltrf+15,m)                                           !/*ll         42*/
      udt(2,l)=x*trf(ltrf+8,m)+y*trf(ltrf+9,m)+                                 !/*ll         43*/
     & z*trf(ltrf+10,m)+trf(ltrf+16,m)                                          !/*ll         44*/
      udt(3,l)=x*trf(ltrf+11,m)+y*trf(ltrf+12,m)+                               !/*ll         45*/
     & z*trf(ltrf+13,m)+trf(ltrf+17,m)                                          !/*ll         46*/
      go to 40                                                                  !/*ll         47*/
   30 udt(1,l)=x                                                                !/*ll         48*/
      udt(2,l)=y                                                                !/*ll         49*/
      udt(3,l)=z                                                                !/*ll         50*/
   40 continue                                                                  !/*ll         51*/
      lev=ll                                                                    !/*ll         52*/
      if(lev.ne.0)call uplev(uuu)                                               !/*ll4b       10*/
!                                                                               !/*ll         54*/
!        do the levels at and below the xyz level.                              !/*ll         55*/
      ji=iclp(1,ll)                                                             !/*ll         56*/
   50 ju=0                                                                      !/*ll         57*/
      l=ll+1                                                                    !/*ll4b       11*/
      if(ll.eq.0)go to 60                                                       !/*ll4b       12*/
      j=-mfl(lmfl+1,iclp(1,ll-1))                                               !/*ll4b       13*/
      ju=-j                                                                     !/*ll4b       14*/
      if(j.le.0)go to 60                                                        !/*ll4b       15*/
      ju=laf(j+1,3+iclp(2,ll-1)-laf(j+1,1)+laf(j+1,2)*                          !/*ll4b       16*/
     & (iclp(3,ll-1)-laf(j+2,1)+laf(j+2,2)*(iclp(4,ll-1)-laf(j+3,1))))          !/*ll4b       17*/
      if(ju.ne.abs(jun(ljun+iclp(1,ll-1))))go to 60                             !/*ll4b       18*/
      lev=ll-1                                                                  !/*ll4b       19*/
      icl=udt(7,lev)                                                            !/*ll4b       20*/
      iii=udt(8,lev)                                                            !/*ll4b       21*/
      jjj=udt(9,lev)                                                            !/*ll4b       22*/
      kkk=udt(10,lev)                                                           !/*ll4b       23*/
      return                                                                    !/*ll4b       24*/
   60 if(ji.ne.0)go to 100                                                      !/*ll         60*/
      do 70 i=1,nlse                                                            !/*ll         61*/
      ji=lse(klse+i)                                                            !/*ll         62*/
      if(abs(jun(ljun+ji)).ne.ju)go to 70                                       !/*ll         63*/
      call chkcel(ji,2,j)                                                       !/*ll         64*/
      if(j.eq.0)go to 100                                                       !/*ll         65*/
   70 continue                                                                  !/*ll         66*/
      do 80 ji=1,mxa                                                            !/*ll         67*/
      if(abs(jun(ljun+ji)).ne.ju)go to 80                                       !/*ll         68*/
      call chkcel(ji,2,j)                                                       !/*ll         69*/
      if(j.eq.0)go to 90                                                        !/*ll         70*/
   80 continue                                                                  !/*ll         71*/
      call expirx(1,'levcel','source point is not in any cell.')                !/*ll         72*/
      return                                                                    !/*ll         73*/
   90 nlse=nlse+1                                                               !/*ll         74*/
      lse(klse+nlse)=ji                                                         !/*ll         75*/
  100 icl=ji                                                                    !/*cor4-1    113*/
      jsu=-abs(jsu)                                                             !/*ll4c        1*/
      if(l.gt.ll+1.and.lat(llat+1,icl).ne.0)call findel(icl)                    !/*ll4c        2*/
      jsu=abs(jsu)                                                              !/*ll4c        3*/
      if(mfl(lmfl+1,icl).eq.0)return                                            !/*cor4-1    115*/
      j=-mfl(lmfl+1,icl)                                                        !/*cor4-1    116*/
      if(j.gt.0)go to 110                                                       !/*cor4-1    117*/
      ju=-j                                                                     !/*cor4-1    118*/
      call dunlev(icl)                                                          !/*ll         79*/
      ji=mfl(lmfl+2,icl)                                                        !/*ll         80*/
      if(l.le.n.and.ji.eq.0)ji=iclp(1,l)                                        !/*ll         82*/
      l=l+1                                                                     !/*ll         83*/
      go to 60                                                                  !/*ll         84*/
  110 m=3+iii-laf(j+1,1)+laf(j+1,2)*(jjj-laf(j+2,1)+laf(j+2,2)*                 !/*cor4-1    119*/
     & (kkk-laf(j+3,1)))                                                        !/*cor4-1    120*/
      if(laf(j+1,m).eq.abs(jun(ljun+icl)))return                                !/*cor4-1    121*/
      ju=laf(j+1,m)                                                             !/*cor4-1    122*/
      call dunlev(icl)                                                          !/*cor4-1    123*/
      ji=laf(j+2,m)                                                             !/*cor4-1    124*/
      if(l.le.n.and.ji.eq.0)ji=iclp(1,l)                                        !/*cor4-1    125*/
      l=l+1                                                                     !/*cor4-1    126*/
      go to 60                                                                  !/*cor4-1    127*/
      end                                                                       !/*ll         85*/
