!     $Id: vertex.F,v 1.4 2002/03/27 18:14:48 gwm Exp $                       !/*ve          1*/
      subroutine vertex (it,lt,ll,kfa,e0,iv,re,kfr1,kfr2,ame,iopt)              !/*ve          2*/
!                                                                     *         !/*ve          3*/
!---------------------------------------------------------------------*         !/*ve          4*/
!                                                                     *         !/*ve          5*/
#include <lcs/double.h>                                                         /*!ve          6*/
#include <fluka89/flprm1.h>                                                     /*!ve          7*/
#include <fluka89/flprm2.h>                                                     /*!ve          8*/
#include <lcs/inout.h>                                                          /*!ve          9*/
      dimension re(*), kfr1(*), kfr2(*), iv(*)                                  !/*ve         10*/
      if (lt.eq.1) write (io,70) it,lt,ll,kfa,e0,ame,iopt                       !/*ve         11*/
      pdb=0.8d0                                                                 !/*ve         12*/
      iva=1                                                                     !/*ve         13*/
      i=it                                                                      !/*ve         14*/
      j=it-1                                                                    !/*ve         15*/
      ivx=iva                                                                   !/*ve         16*/
      if (iopt.eq.2) ivx=2                                                      !/*ve         17*/
      if (iopt.eq.4.and.kfa.gt.6.and.ll.eq.0) ivx=2                             !/*ve         18*/
      if (iopt.eq.4.and.kfa.le.6.and.ll.eq.1) ivx=2                             !/*ve         19*/
      if (iopt.eq.5) ivx=2                                                      !/*ve         20*/
      if (i.gt.1) ivx=iv(j)                                                     !/*ve         21*/
      if (ll.eq.1.and.i.gt.1) ivx=iv(j)-5                                       !/*ve         22*/
      z=rang()                                                                  !/*ve         23*/
      y=rang()                                                                  !/*ve         24*/
      go to (10,20,10,20,20), ivx                                               !/*ve         25*/
   10 ivx=1                                                                     !/*ve         26*/
      if (z.ge.ame) ivx=2                                                       !/*ve         27*/
      go to 30                                                                  !/*ve         28*/
   20 ivx=3                                                                     !/*ve         29*/
      if (z.gt.pdb) ivx=4                                                       !/*ve         30*/
      if (z.gt.pdb.and.y.le.0.5d0) ivx=5                                        !/*ve         31*/
      go to 30                                                                  !/*ve         32*/
   30 continue                                                                  !/*ve         33*/
      if (ll.eq.1) ivx=ivx+5                                                    !/*ve         34*/
      iv(i)=ivx                                                                 !/*ve         35*/
      if (lt.eq.0) go to 60                                                     !/*ve         36*/
      if (i.eq.1) go to 40                                                      !/*ve         37*/
      write (io,80) iv(j),iv(i),ll                                              !/*ve         38*/
      go to 50                                                                  !/*ve         39*/
   40 write (io,80) iva,iv(i),ll                                                !/*ve         40*/
   50 continue                                                                  !/*ve         41*/
   60 continue                                                                  !/*ve         42*/
      return                                                                    !/*ve         43*/
!                                                                               !/*ve         44*/
   70 format (4i5,2e12.4,i5,' vertex it,lt,ll,kfa,e0,ame')                      !/*ve         45*/
   80 format (1h0,15hiv(j),iv(i),ll=,3i5)                                       !/*ve         46*/
      end                                                                       !/*ve         47*/
