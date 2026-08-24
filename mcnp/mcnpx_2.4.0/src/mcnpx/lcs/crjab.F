!     $Id: crjab.F,v 1.5 2002/03/27 18:14:59 gwm Exp $                        !/*cr          1*/
      subroutine crjab (k1,pp)                                                  !/*cr          2*/
#include <lcs/double.h>                                                         /*!cr          3*/
      dimension pp(380)                                                         !/*cr          4*/
#include <lcs/inout.h>                                                          /*!cr          5*/
#include <lcs/btcom.h>                                                          /*!cr          6*/
! subroutine crdet put inline                                                   !/*cr          7*/
      univ=0.05d0*value1                                                        !/*cr          8*/
      ie=abs(univ)                                                              !/*cr          9*/
!     energy interval                                                           !/*cr         10*/
      univ=univ-real(ie)                                                        !/*cr         11*/
!     inpt=0 if whole interval considered                                       !/*cr         12*/
!     k1=data per energy interval                                               !/*cr         13*/
      if (k1.ne.1) call errorf ('crjab.1')                                      !/*cr         14*/
!     answers stored in crdt                                                    !/*cr         15*/
      k=ie                                                                      !/*cr         16*/
      if (inpt) 50,20,10                                                        !/*cr         17*/
   10 k=inpt-1+k                                                                !/*cr         18*/
      crdt(1)=(pp(k+2)-pp(k+1))*univ+pp(k+1)                                    !/*cr         19*/
      crdt(2)=(pp(k+3)-pp(k+2))*univ+pp(k+2)                                    !/*cr         20*/
      go to 30                                                                  !/*cr         21*/
   20 continue                                                                  !/*cr         22*/
      crdt(1)=(pp(k+2)-pp(k+1))*univ+pp(k+1)                                    !/*cr         23*/
      crdt(2)=dp0                                                               !/*cr         24*/
   30 continue                                                                  !/*cr         25*/
      inpt=0                                                                    !/*cr         26*/
      value1=(pxyz(1)*pxyz(2)+pxyz(5)*pxyz(6)+pxyz(9)*pxyz(10))/e(1)            !/*cr         27*/
!     p1.p2/e(1)                                                                !/*cr         28*/
! p2 defined as square of original p2 from isom                                 !/*cr         29*/
      tem=dp1/dncms                                                             !/*cr         30*/
      value2=(value1/p2)*((e(2)*tem)-dp1)-tem                                   !/*cr         31*/
!     s=((p1.p2)/(e1*p2*p2))*((e2/m)-dp1)-1.0/m                                 !/*cr         32*/
      value2=dncms*crdt(1)*sqrt(p1oe1*p1oe1+dp2*value1*value2+p2*value2         !/*cr         33*/
     & *value2)/(e(2)*p1oe1*any)                                                !/*cr         34*/
!     (m)(c.s)(j**2+2s(p1.p2)/e1+(p2)(p2)(s)(s)                                 !/*cr         35*/
      if (value2-dp1) 40,40,60                                                  !/*cr         36*/
!     this tests sampling tech.to ensure fmaxs were selected so that            !/*cr         37*/
!     value2 lte one.                                                           !/*cr         38*/
   40 continue                                                                  !/*cr         39*/
      value1=rang()                                                             !/*cr         40*/
      return                                                                    !/*cr         41*/
   50 call errorf ('crjab.2')                                                   !/*cr         42*/
   60 continue                                                                  !/*cr         43*/
      write (io,70) value2,amasno,zee,einc,prtin                                !/*cr         44*/
      value2=dp1                                                                !/*cr         45*/
      go to 40                                                                  !/*cr         46*/
!                                                                               !/*cr         47*/
   70 format (' crjab: value2=',1pe12.4,' amasno=',f5.0,' zee=',f5.0,           !/*cr         48*/
     & ' einc=',1pe12.4,' prtin=',f5.0)                                         !/*cr         49*/
      end                                                                       !/*cr         50*/
