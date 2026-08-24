!     $Id: rotat.F,v 1.4 2002/03/27 18:14:47 gwm Exp $                        !/*ro          1*/
      subroutine rotat (px,py,pz,pxn,pyn,pzn,cote,site,cops,sips)               !/*ro          2*/
!                                                                     *         !/*ro          3*/
!---------------------------------------------------------------------*         !/*ro          4*/
!                                                                     *         !/*ro          5*/
#include <lcs/double.h>                                                         /*!ro          6*/
#include <fluka89/flprm1.h>                                                     /*!ro          7*/
#include <fluka89/flprm2.h>                                                     /*!ro          8*/
#include <lcs/inout.h>                                                          /*!ro          9*/
      pxn=-px*sips-py*cote*cops+pz*site*cops                                    !/*ro         10*/
      pyn=px*cops-py*cote*sips+pz*site*sips                                     !/*ro         11*/
      pzn=py*site+pz*cote                                                       !/*ro         12*/
      return                                                                    !/*ro         13*/
      end                                                                       !/*ro         14*/
