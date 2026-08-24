!     $Id: wssto.F,v 1.4 2002/03/27 18:15:26 gwm Exp $                        !/*ws          1*/
      subroutine wssto (icsb8)                                                  !/*ws          2*/
#include <lcs/double.h>                                                         /*!ws          3*/
!     ==================================================================        !/*ws          4*/
!     wssto stores some quantities from ws into st.  When an isobar             !/*ws          5*/
!     is first made, its characteristics are stored into st. Also,              !/*ws          6*/
!     when an isobar leaves the nucleus, its characteristics (before            !/*ws          7*/
!     decaying at the surface) are stored into st.                              !/*ws          8*/
!                                                                               !/*ws          9*/
! WSSTO  called by AIOUT  PSEL                                                  !/*ws         10*/
!     ==================================================================        !/*ws         11*/
!                                                                               !/*ws         12*/
#include <lcs/vegas.h>                                                          /*!ws         13*/
      if (ind(17)-1) 10,20,10                                                   !/*ws         14*/
   10 st(icsb8+3)=ws(3)                                                         !/*ws         15*/
      st(icsb8+4)=ws(4)                                                         !/*ws         16*/
      st(icsb8+5)=ws(5)                                                         !/*ws         17*/
   20 st(icsb8+1)=ws(1)                                                         !/*ws         18*/
      if (ind(4).eq.1) st(icsb8+1)=st(icsb8+1)-0.1d0                            !/*ws         19*/
      st(icsb8+2)=ws(2)                                                         !/*ws         20*/
      st(icsb8+6)=ws(6)                                                         !/*ws         21*/
      st(icsb8+7)=ws(7)                                                         !/*ws         22*/
      st(icsb8+8)=ws(8)                                                         !/*ws         23*/
      st(icsb8+9)=ws(9)                                                         !/*ws         24*/
      st(icsb8+10)=ws(10)                                                       !/*ws         25*/
      st(icsb8+11)=ws(11)                                                       !/*ws         26*/
      st(icsb8+12)=ws(12)                                                       !/*ws         27*/
      st(icsb8+13)=ws(13)                                                       !/*ws         28*/
      st(icsb8+14)=ws(14)                                                       !/*ws         29*/
      st(icsb8+15)=ws(25)                                                       !/*ws         30*/
      st(icsb8+16)=ws(24)                                                       !/*ws         31*/
      st(icsb8+i81)=ws(31)                                                      !/*ws         32*/
      return                                                                    !/*ws         33*/
      end                                                                       !/*ws         34*/
