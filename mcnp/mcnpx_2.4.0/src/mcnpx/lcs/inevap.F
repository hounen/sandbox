!     $Id: inevap.F,v 1.4 2002/03/27 18:15:08 gwm Exp $                       !/*in          1*/
      subroutine inevap (nbrtp)                                                 !/*in          2*/
#include <lcs/double.h>                                                         /*!in          3*/
#include <lcs/cacom.h>                                                          /*!in          4*/
#include <lcs/ercom.h>                                                          /*!in          5*/
      dimension exmm6(9), omega6(6)                                             !/*in          6*/
      do 10 k=1,250                                                             !/*in          7*/
      do 10 j=1,20                                                              !/*in          8*/
   10 waps(k,j)=dp0                                                             !/*in          9*/
      read (nbrtp) (pp0(j),pp1(j),pp2(j),j=1,1001)                              !/*in         10*/
      read (nbrtp) (ia6(j),j=1,6),(iz6(j),j=1,6)                                !/*in         11*/
      read (nbrtp) (rho9(j),j=1,6),(omega6(j),j=1,6)                            !/*in         12*/
      read (nbrtp) (exmm6(j),j=1,6)                                             !/*in         13*/
      read (nbrtp) (cam2(j),j=1,130)                                            !/*in         14*/
      read (nbrtp) (cam3(j),j=1,200)                                            !/*in         15*/
      read (nbrtp) (cam4(j),j=1,130)                                            !/*in         16*/
      read (nbrtp) (cam5(j),j=1,200)                                            !/*in         17*/
      read (nbrtp) ((tz0(i,j),j=1,7),i=1,3)                                     !/*in         18*/
      read (nbrtp) (rmass(j),j=1,297)                                           !/*in         19*/
      read (nbrtp) (alph(j),j=1,297)                                            !/*in         20*/
      read (nbrtp) (bet(j),j=1,297)                                             !/*in         21*/
      read (nbrtp) ((waps(i,j),i=1,250),j=1,20)                                 !/*in         22*/
      do 20 k=1,7                                                               !/*in         23*/
   20 tz0(4,k)=dp0                                                              !/*in         24*/
      waps(1,11)=exmass(1)                                                      !/*in         25*/
      waps(1,9)=exmass(2)                                                       !/*in         26*/
      waps(2,10)=exmass(3)                                                      !/*in         27*/
      waps(3,11)=exmass(4)                                                      !/*in         28*/
      waps(3,9)=exmass(5)                                                       !/*in         29*/
      waps(4,10)=exmass(6)                                                      !/*in         30*/
      waps(4,8)=exmass(5)+exmass(2)+2.91d0                                      !/*in         31*/
      waps(4,12)=exmass(4)+exmass(1)+2.9d0                                      !/*in         32*/
      waps(5,7)=exmass(5)+dp2*exmass(2)+2.7d0                                   !/*in         33*/
      waps(5,13)=exmass(4)+dp2*exmass(1)+2.69d0                                 !/*in         34*/
      do 30 i=1,6                                                               !/*in         35*/
   30 in6(6)=ia6(6)-iz6(6)                                                      !/*in         36*/
      return                                                                    !/*in         37*/
      end                                                                       !/*in         38*/
