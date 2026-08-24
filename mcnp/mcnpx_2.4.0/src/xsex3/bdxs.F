!     $Id: bdxs.F,v 1.7 2002/03/27 18:16:23 gwm Exp $                         !/*bd          1*/
      block data bdxs                                                           !/*bd          2*/
#include <htape3x/com32b.h>                                                     /*!bd          3*/
#include <xsex3/charxs.h>                                                       /*!bd          4*/
      data msub /'inxs','outxs','histp','xstal'/                                !/*bd          5*/
      data isub /ndef*'        '/                                               !/*bd          6*/
      data tlnon, tsnon, tlfis, tsfis /4*dp0/                                   !/*bd          7*/
      data tlels, tsels, tltot, tstot /4*dp0/                                   !/*bd          8*/
      data kpart /'proton','neutron','pion+','pion0','pion-',                   !/*bd          9*/
     & 'deutron','triton','he-3','alpha','photon','kaon+',                      !/*bd         10*/
     & 'kaon0','kaon-','p-bar','n-bar','elastic'/                               !/*bd         11*/
      end                                                                       !/*bd         12*/
