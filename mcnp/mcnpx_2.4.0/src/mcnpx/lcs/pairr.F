!     $Id: pairr.F,v 1.4 2002/03/27 18:15:12 gwm Exp $                        !/*pa          1*/
      function pairr (a1,izz1,a2,izz2,corr1,corr2)                              !/*pa          2*/
#include <lcs/double.h>                                                         /*!pa          3*/
#include <lcs/ercom.h>                                                          /*!pa          4*/
#include <lcs/orcm.h>                                                           /*!pa          5*/
!ccc                                                                            !/*pa          6*/
!     choose nearest integers                                                   !/*pa          7*/
!ccc                                                                            !/*pa          8*/
      iaf=af+dph                                                                !/*pa          9*/
      ia2=a2+dph                                                                !/*pa         10*/
      ia1=iaf-ia2                                                               !/*pa         11*/
      inn1=ia1-izz1                                                             !/*pa         12*/
      inn2=ia2-izz2                                                             !/*pa         13*/
      corr1=cam4(izz1)+cam5(inn1)                                               !/*pa         14*/
      corr2=cam4(izz2)+cam5(inn2)                                               !/*pa         15*/
      pairr=corr1+corr2                                                         !/*pa         16*/
      return                                                                    !/*pa         17*/
      end                                                                       !/*pa         18*/
