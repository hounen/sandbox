!     $Id: rchanw.F,v 1.4 2002/03/27 18:14:47 gwm Exp $                       !/*rc          1*/
      subroutine rchanw                                                         !/*rc          2*/
!                                                                     *         !/*rc          3*/
!---------------------------------------------------------------------*         !/*rc          4*/
!                                                                     *         !/*rc          5*/
#include <lcs/double.h>                                                         /*!rc          6*/
#include <fluka89/flprm1.h>                                                     /*!rc          7*/
#include <fluka89/flprm2.h>                                                     /*!rc          8*/
#include <lcs/inout.h>                                                          /*!rc          9*/
#include <fluka89/ablts1.h>                                                     /*!rc         10*/
#include <fluka89/split.h>                                                      /*!rc         11*/
      dimension hwt(460)                                                        !/*rc         12*/
#include <fluka89/redver.h>                                                     /*!rc         13*/
      dimension hwk(40)                                                         !/*rc         14*/
#include <fluka89/reac1.h>                                                      /*!rc         15*/
      dimension si(5184)                                                        !/*rc         16*/
      equivalence (wk(1),si(1))                                                 !/*rc         17*/
      ireg=16                                                                   !/*rc         18*/
      do 90 ire=1,ireg                                                          !/*rc         19*/
      iwko=irii(ire)                                                            !/*rc         20*/
      iee=ieii(ire+1)-ieii(ire)                                                 !/*rc         21*/
      ike=ikii(ire+1)-ikii(ire)                                                 !/*rc         22*/
      ieo=ieii(ire)+1                                                           !/*rc         23*/
      iika=ikii(ire)                                                            !/*rc         24*/
      do 80 ie=1,iee                                                            !/*rc         25*/
      sis=1.d-14                                                                !/*rc         26*/
      sinorc=0.1d0                                                              !/*rc         27*/
      do 10 ik=1,ike                                                            !/*rc         28*/
      iwk=iwko+iee*(ik-1)+ie                                                    !/*rc         29*/
      if (nrk(2,iika+ik).eq.0) sinorc=1.d0                                      !/*rc         30*/
      if (sinorc.lt.1.d0.and.ie.gt.3) si(iwk)=si(iwk)*0.1d0**(ie-3)             !/*rc         31*/
      if (sinorc.lt.1.d0.and.ie.gt.8) si(iwk)=1.d-5                             !/*rc         32*/
   10 sis=sis+si(iwk)*sinorc                                                    !/*rc         33*/
      siin(ieo+ie-1)=sis                                                        !/*rc         34*/
      sio=0.d0                                                                  !/*rc         35*/
      if (sis.ge.1.d-12) go to 20                                               !/*rc         36*/
      sis=1.d0                                                                  !/*rc         37*/
      sio=1.d0                                                                  !/*rc         38*/
   20 continue                                                                  !/*rc         39*/
      sinorc=0.1d0                                                              !/*rc         40*/
      do 30 ik=1,ike                                                            !/*rc         41*/
      if (nrk(2,iika+ik).eq.0) sinorc=1.d0                                      !/*rc         42*/
      iwk=iwko+iee*(ik-1)+ie                                                    !/*rc         43*/
      sio=sio+si(iwk)/sis*sinorc                                                !/*rc         44*/
   30 hwk(ik)=sio                                                               !/*rc         45*/
      do 40 ik=1,ike                                                            !/*rc         46*/
      iwk=iwko+iee*(ik-1)+ie                                                    !/*rc         47*/
   40 wk(iwk)=hwk(ik)                                                           !/*rc         48*/
      iiki=ikii(ire)                                                            !/*rc         49*/
      do 70 ik=1,ike                                                            !/*rc         50*/
      am111=0.d0                                                                !/*rc         51*/
      inrk1=nrk(1,iiki+ik)                                                      !/*rc         52*/
      if (inrk1.gt.0) am111=am(inrk1)                                           !/*rc         53*/
      am222=0.d0                                                                !/*rc         54*/
      inrk2=nrk(2,iiki+ik)                                                      !/*rc         55*/
      if (inrk2.gt.0) am222=am(inrk2)                                           !/*rc         56*/
      thresh(iiki+ik)=am111+am222                                               !/*rc         57*/
      if (inrk2-1.ge.0) go to 60                                                !/*rc         58*/
      inrkk=k1(inrk1)                                                           !/*rc         59*/
      amss=5.d0                                                                 !/*rc         60*/
      inrko=k2(inrk1)                                                           !/*rc         61*/
      do 50 inrk1=inrkk,inrko                                                   !/*rc         62*/
      inzk1=nzk(inrk1,1)                                                        !/*rc         63*/
      inzk2=nzk(inrk1,2)                                                        !/*rc         64*/
      inzk3=nzk(inrk1,3)                                                        !/*rc         65*/
      ams=am(inzk1)+am(inzk2)                                                   !/*rc         66*/
      if (inzk3-1.ge.0) ams=ams+am(inzk3)                                       !/*rc         67*/
      if (amss.gt.ams) amss=ams                                                 !/*rc         68*/
   50 continue                                                                  !/*rc         69*/
      ams=amss                                                                  !/*rc         70*/
      if (ams.lt.umo(ieo)) ams=umo(ieo)                                         !/*rc         71*/
      thresh(iiki+ik)=ams                                                       !/*rc         72*/
   60 continue                                                                  !/*rc         73*/
   70 continue                                                                  !/*rc         74*/
   80 continue                                                                  !/*rc         75*/
   90 continue                                                                  !/*rc         76*/
      do 100 j=1,460                                                            !/*rc         77*/
  100 hwt(j)=0.d0                                                               !/*rc         78*/
      do 120 i=1,110                                                            !/*rc         79*/
      ik1=k1(i)                                                                 !/*rc         80*/
      ik2=k2(i)                                                                 !/*rc         81*/
      hv=0.d0                                                                   !/*rc         82*/
      do 110 j=ik1,ik2                                                          !/*rc         83*/
      hv=hv+wt(j)                                                               !/*rc         84*/
      hwt(j)=hv                                                                 !/*rc         85*/
      ji=j                                                                      !/*rc         86*/
  110 continue                                                                  !/*rc         87*/
      if (abs(hv-1.d0).gt.1.d-4) write (io,140)                                 !/*rc         88*/
  120 continue                                                                  !/*rc         89*/
      do 130 j=1,460                                                            !/*rc         90*/
  130 wt(j)=hwt(j)                                                              !/*rc         91*/
      return                                                                    !/*rc         92*/
!                                                                               !/*rc         93*/
  140 format (44h error in hwt because of false use of rchanw)                  !/*rc         94*/
      end                                                                       !/*rc         95*/
