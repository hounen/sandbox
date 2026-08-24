!     $Id: aiouta.F,v 1.4 2002/03/27 18:14:53 gwm Exp $                       !/*ai          1*/
      subroutine aiouta (y1)                                                    !/*ai          2*/
#include <lcs/double.h>                                                         /*!ai          3*/
!     ==================================================================        !/*ai          4*/
!     aiouta stores the decay particles from the escaping                       !/*ai          5*/
!     isobars into st.                                                          !/*ai          6*/
!                                                                               !/*ai          7*/
! AIOUTA called by AIOUT                                                        !/*ai          8*/
!     ==================================================================        !/*ai          9*/
!                                                                               !/*ai         10*/
#include <lcs/vegas.h>                                                          /*!ai         11*/
      dimension y1(32)                                                          !/*ai         12*/
      y1(1)=-y1(1)                                                              !/*ai         13*/
      y1(2)=-y1(2)                                                              !/*ai         14*/
      icsb(15)=icsb(15)+1                                                       !/*ai         15*/
      if (icsb(15).ge.i391) go to 20                                            !/*ai         16*/
      icsb8=i81*icsb(15)                                                        !/*ai         17*/
      do 10 idx=1,14                                                            !/*ai         18*/
      st(icsb8+idx)=y1(idx)                                                     !/*ai         19*/
   10 continue                                                                  !/*ai         20*/
      st(icsb8+13)=dp0                                                          !/*ai         21*/
      st(icsb8+15)=y1(24)                                                       !/*ai         22*/
      st(icsb8+16)=y1(25)                                                       !/*ai         23*/
      st(icsb8+17)=y1(27)                                                       !/*ai         24*/
      st(icsb8+18)=y1(10)                                                       !/*ai         25*/
      st(icsb8+19)=y1(11)                                                       !/*ai         26*/
      st(icsb8+20)=y1(12)                                                       !/*ai         27*/
      st(icsb8+21)=y1(5)                                                        !/*ai         28*/
      st(icsb8+i81)=y1(31)                                                      !/*ai         29*/
      return                                                                    !/*ai         30*/
   20 kcan=2                                                                    !/*ai         31*/
      lkcan='aiouta'                                                            !/*ai         32*/
      return                                                                    !/*ai         33*/
      end                                                                       !/*ai         34*/
