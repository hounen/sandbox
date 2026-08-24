!#ident uplev.F from deck up                                                    !/*up          1*/
      subroutine uplev(u)                                                       !/*up4b        1*/
!        calculate direction cosines in all higher levels.                      !/*up          3*/
      use global_data                                                           !/*upx         1*/
!                                                                               !/*upx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*upx         3*/
      implicit integer(kindi) (i-n)                                             !/*upx         4*/
!                                                                               !/*upx         5*/
      dimension u(3)                                                            !/*up4b        2*/
!                                                                               !/*up          5*/
      udt(4,lev)=u(1)                                                           !/*up4b        3*/
      udt(5,lev)=u(2)                                                           !/*up4b        4*/
      udt(6,lev)=u(3)                                                           !/*up4b        5*/
      do 20 lz=0,lev-1                                                          !/*up          9*/
      l=lev-1-lz                                                                !/*up         10*/
      j=-mfl(lmfl+1,int(udt(7,l)))                                              !/*up         11*/
      if(j.lt.0)m=mfl(lmfl+3,int(udt(7,l)))                                     !/*up         12*/
      if(j.gt.0)m=laf(j+3,int(3+udt(8,l)-laf(j+1,1)+laf(j+1,2)*                 !/*up         13*/
     & (udt(9,l)-laf(j+2,1)+laf(j+2,2)*(udt(10,l)-laf(j+3,1)))))                !/*up         14*/
      if(m.eq.0)go to 10                                                        !/*up         15*/
      udt(4,l)=udt(4,l+1)*trf(ltrf+5,m)+udt(5,l+1)*trf(ltrf+6,m)+               !/*up         16*/
     & udt(6,l+1)*trf(ltrf+7,m)                                                 !/*up         17*/
      udt(5,l)=udt(4,l+1)*trf(ltrf+8,m)+udt(5,l+1)*trf(ltrf+9,m)+               !/*up         18*/
     & udt(6,l+1)*trf(ltrf+10,m)                                                !/*up         19*/
      udt(6,l)=udt(4,l+1)*trf(ltrf+11,m)+udt(5,l+1)*trf(ltrf+12,m)+             !/*up         20*/
     & udt(6,l+1)*trf(ltrf+13,m)                                                !/*up         21*/
      go to 20                                                                  !/*up         22*/
   10 udt(4,l)=udt(4,l+1)                                                       !/*up         23*/
      udt(5,l)=udt(5,l+1)                                                       !/*up         24*/
      udt(6,l)=udt(6,l+1)                                                       !/*up         25*/
   20 continue                                                                  !/*up         26*/
      return                                                                    !/*up         27*/
      end                                                                       !/*up         28*/
