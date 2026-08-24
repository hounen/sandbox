!#ident dddlev.F from deck dl                                                   !/*dl          1*/
      subroutine dddlev                                                         !/*dl          2*/
!        calculate direction cosines in all lower levels.                       !/*dl          3*/
      use global_data                                                           !/*dlx         1*/
!                                                                               !/*dlx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*dlx         3*/
      implicit integer(kindi) (i-n)                                             !/*dlx         4*/
!                                                                               !/*dlx         5*/
!                                                                               !/*dl          5*/
      do 10 l=0,lev-1                                                           !/*dl          6*/
      udt(4,l)=uuu                                                              !/*dl          7*/
      udt(5,l)=vvv                                                              !/*dl          8*/
      udt(6,l)=www                                                              !/*dl          9*/
      j=-mfl(lmfl+1,int(udt(7,l)))                                              !/*dl         10*/
      if(j.lt.0)m=mfl(lmfl+3,int(udt(7,l)))                                     !/*dl         11*/
      if(j.gt.0)m=laf(j+3,int(3+udt(8,l)-laf(j+1,1)+laf(j+1,2)*                 !/*dl         12*/
     & (udt(9,l)-laf(j+2,1)+laf(j+2,2)*(udt(10,l)-laf(j+3,1)))))                !/*dl         13*/
      if(m.eq.0)go to 10                                                        !/*dl         14*/
      uuu=udt(4,l)*trf(ltrf+5,m)+udt(5,l)*trf(ltrf+8,m)+                        !/*dl         15*/
     & udt(6,l)*trf(ltrf+11,m)                                                  !/*dl         16*/
      vvv=udt(4,l)*trf(ltrf+6,m)+udt(5,l)*trf(ltrf+9,m)+                        !/*dl         17*/
     & udt(6,l)*trf(ltrf+12,m)                                                  !/*dl         18*/
      www=udt(4,l)*trf(ltrf+7,m)+udt(5,l)*trf(ltrf+10,m)+                       !/*dl         19*/
     & udt(6,l)*trf(ltrf+13,m)                                                  !/*dl         20*/
   10 continue                                                                  !/*dl         21*/
      return                                                                    !/*dl         22*/
      end                                                                       !/*dl         23*/
