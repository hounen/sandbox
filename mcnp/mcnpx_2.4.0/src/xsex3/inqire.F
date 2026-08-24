!     $Id: inqire.F,v 1.4 2002/03/27 18:16:25 gwm Exp $                       !/*in          1*/
      function inqire (hf)                                                      !/*in          2*/
#include <htape3x/com32b.h>                                                     /*!in          3*/
!        return 1 if file hf exists, 0 if it does not.                          !/*in          4*/
      character hf*(*)                                                          !/*in          5*/
      logical lv                                                                !/*in          6*/
!                                                                               !/*in          7*/
      inqire=1                                                                  !/*in          8*/
      inquire (file=hf,exist=lv)                                                !/*in          9*/
      if (.not.lv) inqire=0                                                     !/*in         10*/
      return                                                                    !/*in         11*/
      end                                                                       !/*in         12*/
