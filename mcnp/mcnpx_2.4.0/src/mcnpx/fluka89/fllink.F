!     $Id: fllink.F,v 1.4 2002/03/27 18:14:43 gwm Exp $                       !/*fl          1*/
      subroutine fllink (it,e,txi,tyi,tzi,we,cxj,cyj,czj,tkj,kpj,wej,np)        !/*fl          2*/
#include <lcs/double.h>                                                         /*!fl          3*/
      external f2bd                                                             !/*fl          4*/
#include <fluka89/paprop.h>                                                     /*!fl          5*/
#include <lcs/inout.h>                                                          /*!fl          6*/
#include <lcs/lptS.h>                                                           /*!fl          7*/
#include <lcs/fltolh.h>                                                         /*!fl          8*/
      parameter (mxm=30)                                                        !/*fl          9*/
      dimension cxi(mxm), cyi(mxm), czi(mxm), tki(mxm), wei(mxm), kpi           !/*fl         10*/
     & (mxm), cxj(mxm), cyj(mxm), czj(mxm), tkj(mxm), wej(mxm), kpj(mxm)        !/*fl         11*/
     & , cxk(mxm), cyk(mxm), czk(mxm), tkk(mxm), wek(mxm), kpk(mxm)             !/*fl         12*/
      kpk(1)=itypfl(it)                                                         !/*fl         13*/
      tkk(1)=1.0d-03*e                                                          !/*fl         14*/
      cxk(1)=txi                                                                !/*fl         15*/
      cyk(1)=tyi                                                                !/*fl         16*/
      czk(1)=tzi                                                                !/*fl         17*/
      wek(1)=we                                                                 !/*fl         18*/
      np=0                                                                      !/*fl         19*/
      mp1=1                                                                     !/*fl         20*/
      mp2=1                                                                     !/*fl         21*/
      mp=mp2                                                                    !/*fl         22*/
   10 continue                                                                  !/*fl         23*/
      do 40 j=mp1,mp2                                                           !/*fl         24*/
      ij=kpk(j)                                                                 !/*fl         25*/
      eke=tkk(j)                                                                !/*fl         26*/
      poo=sqrt(eke*(eke+dp2*am(ij)))                                            !/*fl         27*/
      call disint (ij,poo,eke,cxk(j),cyk(j),czk(j),wek(j),cxi,cyi,czi           !/*fl         28*/
     & ,tki,kpi,wei,npi)                                                        !/*fl         29*/
      do 30 i=1,npi                                                             !/*fl         30*/
      jp=ityplh(kpi(i))                                                         !/*fl         31*/
      if (jp.gt.0) go to 20                                                     !/*fl         32*/
      if (jp.le.6) then                                                         !/*fl         33*/
        jp=-jp                                                                  !/*fl         34*/
        go to 20                                                                !/*fl         35*/
      endif                                                                     !/*fl         36*/
      mp=mp+1                                                                   !/*fl         37*/
      kpk(mp)=kpi(i)                                                            !/*fl         38*/
      tkk(mp)=tki(i)                                                            !/*fl         39*/
      cxk(mp)=cxi(i)                                                            !/*fl         40*/
      cyk(mp)=cyi(i)                                                            !/*fl         41*/
      czk(mp)=czi(i)                                                            !/*fl         42*/
      wek(mp)=wei(i)                                                            !/*fl         43*/
      go to 30                                                                  !/*fl         44*/
   20 continue                                                                  !/*fl         45*/
      np=np+1                                                                   !/*fl         46*/
      kpj(np)=jp                                                                !/*fl         47*/
      tkj(np)=1.0d+03*tki(i)                                                    !/*fl         48*/
      cxj(np)=cxi(i)                                                            !/*fl         49*/
      cyj(np)=cyi(i)                                                            !/*fl         50*/
      czj(np)=czi(i)                                                            !/*fl         51*/
      wej(np)=wei(i)                                                            !/*fl         52*/
   30 continue                                                                  !/*fl         53*/
   40 continue                                                                  !/*fl         54*/
      if (mp.le.mp2) go to 50                                                   !/*fl         55*/
      mp1=mp2+1                                                                 !/*fl         56*/
      mp2=mp                                                                    !/*fl         57*/
      go to 10                                                                  !/*fl         58*/
   50 continue                                                                  !/*fl         59*/
      return                                                                    !/*fl         60*/
      end                                                                       !/*fl         61*/
