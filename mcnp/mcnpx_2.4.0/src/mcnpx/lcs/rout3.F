!     $Id: rout3.F,v 1.4 2002/03/27 18:15:22 gwm Exp $                        !/*ro          1*/
      subroutine rout3                                                          !/*ro          2*/
#include <lcs/double.h>                                                         /*!ro          3*/
#include <lcs/btcom.h>                                                          /*!ro          4*/
      if (no-4) 10,20,20                                                        !/*ro          5*/
!     bg6a in original                                                          !/*ro          6*/
   10 isw(11)=1                                                                 !/*ro          7*/
      go to 30                                                                  !/*ro          8*/
   20 isw(11)=0                                                                 !/*ro          9*/
   30 call undis                                                                !/*ro         10*/
      inc=1                                                                     !/*ro         11*/
      return                                                                    !/*ro         12*/
      end                                                                       !/*ro         13*/
