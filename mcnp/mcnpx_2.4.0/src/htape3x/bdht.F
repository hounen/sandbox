!     $Id: bdht.F,v 1.5 2002/03/27 18:13:56 gwm Exp $                         !/*bd          1*/
      block data bdht                                                           !/*bd          2*/
#include <htape3x/com32b.h>                                                     /*!bd          3*/
#include <htape3x/charht.h>                                                     /*!bd          4*/
#include <htape3x/comm7.h>                                                      /*!bd          5*/
      data msub /'int','outt','histp','histx','bertin','opt8a','opt15a',        !/*bd          6*/
     & 'phtlib'/                                                                !/*bd          7*/
      data isub /ndef*'        '/                                               !/*bd          8*/
      data balmax, nocasm /dp0,0/                                               !/*bd          9*/
      end                                                                       !/*bd         10*/
