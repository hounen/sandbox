!     $Id: z_hold.F,v 1.4 2002/03/27 18:15:27 gwm Exp $                       !/*z_          1*/
      subroutine z_hold                                                         !/*z_          2*/
!                                                                               !/*z_          3*/
!     This routine resets the rancom common block at the beginning              !/*z_          4*/
!     of each particle history.  This is to preserve tracking with              !/*z_          5*/
!     continue runs, and with the DBCN(8) debugging feature.                    !/*z_          6*/
!                                                                               !/*z_          7*/
#include <lcs/double.h>                                                         /*!z_          8*/
#include <lcs/rancom.h>                                                         /*!z_          9*/
!                                                                               !/*z_         10*/
      gauhold = dp0                                                             !/*z_         11*/
      bethold = dp0                                                             !/*z_         12*/
      return                                                                    !/*z_         13*/
      end                                                                       !/*z_         14*/
