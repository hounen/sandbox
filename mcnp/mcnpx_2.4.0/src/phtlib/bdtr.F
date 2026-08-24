!     $Id: bdtr.F,v 1.4 2002/03/27 18:16:21 gwm Exp $                         !/*bd          1*/
      block data bdtr                                                           !/*bd          2*/
#include <htape3x/com32b.h>                                                     /*!bd          3*/
#include <htape3x/big.h>                                                        /*!bd          4*/
#include <trx/gross.h>                                                          /*!bd          5*/
#include <htape3x/chartr.h>                                                     /*!bd          6*/
      data msub /'otrx','filin','bcdlib','binlib','error1','error2'/            !/*bd          7*/
      data isub /ndef*'        '/                                               !/*bda         1*/
      data lcl /len1*0/                                                         !/*bd          9*/
      end                                                                       !/*bd         10*/
