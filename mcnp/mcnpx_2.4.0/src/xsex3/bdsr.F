!     $Id: bdsr.F,v 1.4 2002/03/27 18:16:23 gwm Exp $                         !/*bd          1*/
      block data bdsr                                                           !/*bd          2*/
#include <htape3x/com32b.h>                                                     /*!bd          3*/
#include <htape3x/label.h>                                                      /*!bd          4*/
#include <htape3x/contrl.h>                                                     /*!bd          5*/
#include <htape3x/com10.h>                                                      /*!bd          6*/
#include <htape3x/com11.h>                                                      /*!bd          7*/
#include <htape3x/srcwt.h>                                                      /*!bd          8*/
      data nocasp /0/, fwti, fwtcum, fwscum /3*0./                              !/*bd          9*/
      data log10 /.true./                                                       !/*bd         10*/
      data log11 /.true./                                                       !/*bd         11*/
      data twopi /6.2831853/                                                    !/*bd         12*/
      data ibad10,ibad11,nobt10,nobt11,nocs10,nocs11 /6*0/                      !/*bd         13*/
      end                                                                       !/*bd         14*/
