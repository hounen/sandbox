!     $Id: trfdat.F,v 1.4 2002/03/27 18:16:19 gwm Exp $                       !/*tr          1*/
      subroutine trfdat(m,tr)                                                   !/*tr          2*/
#ifdef MESHTAL                                                                  /*!tr          3*/
!       This subroutine is designed to load the proper transformation           !/*tr          4*/
!       numbers into tr(m), where m is the tr number.                           !/*tr          5*/
!                                                                               !/*tr          6*/
!       << Written by E. C. Snow  02/09/99 >>                                   !/*tr          7*/
!                                                                               !/*tr          8*/
      use global_data                                                           !/*trxc        1*/
!                                                                               !/*trxc        2*/
      implicit real(kindr) (a-h,o-z)                                            !/*trxc        3*/
      implicit integer(kindi) (i-n)                                             !/*trxc        4*/
!                                                                               !/*trxc        5*/
      dimension tr(*)                                                           !/*tr         10*/
!                                                                               !/*tr         11*/
      do 10 i=1,12                                                              !/*tr         12*/
      tr(i)=trf(ltrf+i+1,m)                                                     !/*tr         13*/
   10 continue                                                                  !/*tr         14*/
!                                                                               !/*tr         15*/
#endif                                                                          /*!tr         16*/
      return                                                                    !/*tr         17*/
      end                                                                       !/*tr         18*/
