!#ident dunlev.F from deck du                                                   !/*du          1*/
      subroutine dunlev(ic)                                                     !/*du          2*/
!        transform coordinates to the next level down, into the universe        !/*du          3*/
!        filling cell ic.                                                       !/*du          4*/
      use global_data                                                           !/*dux         1*/
!                                                                               !/*dux         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*dux         3*/
      implicit integer(kindi) (i-n)                                             !/*dux         4*/
!                                                                               !/*dux         5*/
!                                                                               !/*du          6*/
      udt(1,lev)=xxx                                                            !/*du          7*/
      udt(2,lev)=yyy                                                            !/*du          8*/
      udt(3,lev)=zzz                                                            !/*du          9*/
      udt(4,lev)=uuu                                                            !/*du         10*/
      udt(5,lev)=vvv                                                            !/*du         11*/
      udt(6,lev)=www                                                            !/*du         12*/
      udt(7,lev)=ic                                                             !/*du         13*/
      udt(8,lev)=iii                                                            !/*du         14*/
      udt(9,lev)=jjj                                                            !/*du         15*/
      udt(10,lev)=kkk                                                           !/*du         16*/
      j=-mfl(lmfl+1,ic)                                                         !/*du         17*/
      if(j.lt.0)m=mfl(lmfl+3,ic)                                                !/*du         18*/
      if(j.gt.0)m=laf(j+3,3+iii-laf(j+1,1)+laf(j+1,2)*                          !/*du         19*/
     & (jjj-laf(j+2,1)+laf(j+2,2)*(kkk-laf(j+3,1))))                            !/*du         20*/
      if(m.eq.0)go to 10                                                        !/*du         21*/
      xxx=udt(1,lev)*trf(ltrf+5,m)+udt(2,lev)*trf(ltrf+8,m)+                    !/*du         22*/
     & udt(3,lev)*trf(ltrf+11,m)+trf(ltrf+2,m)                                  !/*du         23*/
      yyy=udt(1,lev)*trf(ltrf+6,m)+udt(2,lev)*trf(ltrf+9,m)+                    !/*du         24*/
     & udt(3,lev)*trf(ltrf+12,m)+trf(ltrf+3,m)                                  !/*du         25*/
      zzz=udt(1,lev)*trf(ltrf+7,m)+udt(2,lev)*trf(ltrf+10,m)+                   !/*du         26*/
     & udt(3,lev)*trf(ltrf+13,m)+trf(ltrf+4,m)                                  !/*du         27*/
      uuu=udt(4,lev)*trf(ltrf+5,m)+udt(5,lev)*trf(ltrf+8,m)+                    !/*du         28*/
     & udt(6,lev)*trf(ltrf+11,m)                                                !/*du         29*/
      vvv=udt(4,lev)*trf(ltrf+6,m)+udt(5,lev)*trf(ltrf+9,m)+                    !/*du         30*/
     & udt(6,lev)*trf(ltrf+12,m)                                                !/*du         31*/
      www=udt(4,lev)*trf(ltrf+7,m)+udt(5,lev)*trf(ltrf+10,m)+                   !/*du         32*/
     & udt(6,lev)*trf(ltrf+13,m)                                                !/*du         33*/
   10 lev=lev+1                                                                 !/*du         34*/
      return                                                                    !/*du         35*/
      end                                                                       !/*du         36*/
