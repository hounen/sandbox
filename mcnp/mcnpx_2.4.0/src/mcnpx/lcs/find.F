!     $Id: find.F,v 1.4 2002/03/27 18:15:03 gwm Exp $                         !/*fi          1*/
      subroutine find (arg,table,ii,ik)                                         !/*fi          2*/
#include <lcs/double.h>                                                         /*!fi          3*/
!     ==================================================================        !/*fi          4*/
!     find is a simple table look up routine.  ik is the number                 !/*fi          5*/
!     of entries in the table and ii is the desired entry.                      !/*fi          6*/
!                                                                               !/*fi          7*/
! FIND   called by AIANGL  AITOTA                                               !/*fi          8*/
!     ==================================================================        !/*fi          9*/
!                                                                               !/*fi         10*/
      dimension table(40)                                                       !/*fi         11*/
      do 10 i=1,ik                                                              !/*fi         12*/
      if (arg-table(i)) 20,20,10                                                !/*fi         13*/
   10 continue                                                                  !/*fi         14*/
      i=ik                                                                      !/*fi         15*/
   20 ii=i                                                                      !/*fi         16*/
      return                                                                    !/*fi         17*/
      end                                                                       !/*fi         18*/
