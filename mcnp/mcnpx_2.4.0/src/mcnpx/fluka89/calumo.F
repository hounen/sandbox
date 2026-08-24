!     $Id: calumo.F,v 1.4 2002/03/27 18:14:41 gwm Exp $                       !/*ca          1*/
      subroutine calumo (n,itta)                                                !/*ca          2*/
!                                                                     *         !/*ca          3*/
!---------------------------------------------------------------------*         !/*ca          4*/
!                                                                     *         !/*ca          5*/
#include <lcs/double.h>                                                         /*!ca          6*/
#include <fluka89/flprm1.h>                                                     /*!ca          7*/
#include <fluka89/flprm2.h>                                                     /*!ca          8*/
#include <lcs/inout.h>                                                          /*!ca          9*/
#include <fluka89/ablts1.h>                                                     /*!ca         10*/
#include <fluka89/redver.h>                                                     /*!ca         11*/
#include <fluka89/split.h>                                                      /*!ca         12*/
#include <fluka89/reac1.h>                                                      /*!ca         13*/
      ire=nure(n,itta/8+1)                                                      !/*ca         14*/
      ieo=ieii(ire)+1                                                           !/*ca         15*/
      iee=ieii(ire+1)                                                           !/*ca         16*/
      am1=am(n)                                                                 !/*ca         17*/
      am12=am1**2                                                               !/*ca         18*/
      am2=am(itta)                                                              !/*ca         19*/
      am22=am2**2                                                               !/*ca         20*/
      do 10 ie=ieo,iee                                                          !/*ca         21*/
      plab2=plabf(ie)**2                                                        !/*ca         22*/
      elab=sqrt(am12+am22+2.d0*sqrt(plab2+am12)*am2)                            !/*ca         23*/
      umo(ie)=elab                                                              !/*ca         24*/
   10 continue                                                                  !/*ca         25*/
      iko=ikii(ire)+1                                                           !/*ca         26*/
      ike=ikii(ire+1)                                                           !/*ca         27*/
      umoo=umo(ieo)                                                             !/*ca         28*/
      do 30 ik=iko,ike                                                          !/*ca         29*/
      if (nrk(2,ik).gt.0) go to 30                                              !/*ca         30*/
      iki=nrk(1,ik)                                                             !/*ca         31*/
      amss=5.d0                                                                 !/*ca         32*/
      k11=k1(iki)                                                               !/*ca         33*/
      k22=k2(iki)                                                               !/*ca         34*/
      do 20 ik1=k11,k22                                                         !/*ca         35*/
      in1=nzk(ik1,1)                                                            !/*ca         36*/
      ams=am(in1)                                                               !/*ca         37*/
      in1=nzk(ik1,2)                                                            !/*ca         38*/
      if (in1.gt.0) ams=ams+am(in1)                                             !/*ca         39*/
      in1=nzk(ik1,3)                                                            !/*ca         40*/
      if (in1.gt.0) ams=ams+am(in1)                                             !/*ca         41*/
      if (ams.lt.amss) amss=ams                                                 !/*ca         42*/
   20 continue                                                                  !/*ca         43*/
      if (umoo.lt.amss) umoo=amss                                               !/*ca         44*/
      thresh(ik)=umoo                                                           !/*ca         45*/
   30 continue                                                                  !/*ca         46*/
      return                                                                    !/*ca         47*/
      end                                                                       !/*ca         48*/
