!     $Id: hadden.F,v 1.5 2002/03/27 18:14:44 gwm Exp $                       !/*ha          1*/
      subroutine hadden                                                         !/*ha          2*/
!                                                                     *         !/*ha          3*/
!---------------------------------------------------------------------*         !/*ha          4*/
!                                                                     *         !/*ha          5*/
#include <lcs/double.h>                                                         /*!ha          6*/
#include <fluka89/flprm1.h>                                                     /*!ha          7*/
#include <fluka89/flprm2.h>                                                     /*!ha          8*/
#include <lcs/inout.h>                                                          /*!ha          9*/
#include <fluka89/decyc2.h>                                                     /*!ha         10*/
#include <fluka89/part3.h>                                                      /*!ha         11*/
#include <fluka89/namsd.h>                                                      /*!ha         12*/
#include <fluka89/ablts1.h>                                                     /*!ha         13*/
#include <fluka89/split.h>                                                      /*!ha         14*/
      character*8 anamz, zknamz                                                 !/*ha         15*/
#include <fluka89/addhp2.h>                                                     /*!ha         16*/
#include <fluka89/addhn.h>                                                      /*!ha         17*/
      data iretur /0/                                                           !/*ha         18*/
      iretur=iretur+1                                                           !/*ha         19*/
      if (iretur.gt.1) return                                                   !/*ha         20*/
      do 10 i=1,94                                                              !/*ha         21*/
      aname(i)=anamc(i)                                                         !/*ha         22*/
      am(i)=amc(i)                                                              !/*ha         23*/
      ga(i)=gac(i)                                                              !/*ha         24*/
      tau(i)=tauc(i)                                                            !/*ha         25*/
      ich(i)=ichc(i)                                                            !/*ha         26*/
      ibar(i)=ibarc(i)                                                          !/*ha         27*/
      k1(i)=k1c(i)                                                              !/*ha         28*/
      k2(i)=k2c(i)                                                              !/*ha         29*/
   10 continue                                                                  !/*ha         30*/
      do 20 i=26,30                                                             !/*ha         31*/
      k1(i)=452                                                                 !/*ha         32*/
      k2(i)=452                                                                 !/*ha         33*/
   20 continue                                                                  !/*ha         34*/
      do 30 i=1,307                                                             !/*ha         35*/
      zkname(i)=zknamc(i)                                                       !/*ha         36*/
      wt(i)=wtc(i)                                                              !/*ha         37*/
      nzk(i,1)=nzkc(i,1)                                                        !/*ha         38*/
      nzk(i,2)=nzkc(i,2)                                                        !/*ha         39*/
      nzk(i,3)=nzkc(i,3)                                                        !/*ha         40*/
   30 continue                                                                  !/*ha         41*/
      do 40 i=1,16                                                              !/*ha         42*/
      l=i+94                                                                    !/*ha         43*/
      aname(l)=anamz(i)                                                         !/*ha         44*/
      am(l)=amz(i)                                                              !/*ha         45*/
      ga(l)=gaz(i)                                                              !/*ha         46*/
      tau(l)=tauz(i)                                                            !/*ha         47*/
      ich(l)=ichz(i)                                                            !/*ha         48*/
      ibar(l)=ibarz(i)                                                          !/*ha         49*/
      k1(l)=k1z(i)                                                              !/*ha         50*/
      k2(l)=k2z(i)                                                              !/*ha         51*/
   40 continue                                                                  !/*ha         52*/
      do 50 i=1,153                                                             !/*ha         53*/
      l=i+307                                                                   !/*ha         54*/
      zkname(l)=zknamz(i)                                                       !/*ha         55*/
      wt(l)=wtz(i)                                                              !/*ha         56*/
      nzk(l,3)=nzkz(i,3)                                                        !/*ha         57*/
      nzk(l,2)=nzkz(i,2)                                                        !/*ha         58*/
      nzk(l,1)=nzkz(i,1)                                                        !/*ha         59*/
   50 continue                                                                  !/*ha         60*/
      return                                                                    !/*ha         61*/
      end                                                                       !/*ha         62*/
