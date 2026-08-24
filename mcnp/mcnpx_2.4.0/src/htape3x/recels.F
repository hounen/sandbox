!     $Id: recels.F,v 1.4 2002/03/27 18:14:02 gwm Exp $                       !/*re          1*/
      subroutine recels (ielstc)                                                !/*re          2*/
!                                                                               !/*re          3*/
!     detects (neutron) elastic collision                                       !/*re          4*/
!     IELSTC=0 for non-elastic                                                  !/*re          5*/
!     IELSTC=1 for elastic                                                      !/*re          6*/
!                                                                               !/*re          7*/
#include <htape3x/com32b.h>                                                     /*!re          8*/
#include <htape3x/label.h>                                                      /*!re          9*/
      ielstc=0                                                                  !/*re         10*/
      if (ex.ne.dp0) return                                                     !/*re         11*/
      if (nopart.ne.1) return                                                   !/*re         12*/
      if ((nbelo+nabov).ne.1) return                                            !/*re         13*/
      ielstc=1                                                                  !/*re         14*/
      return                                                                    !/*re         15*/
      end                                                                       !/*re         16*/
