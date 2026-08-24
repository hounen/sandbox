!     $Id: poli.F,v 1.4 2002/03/27 18:14:47 gwm Exp $                         !/*po          1*/
      subroutine poli (cs,si)                                                   !/*po          2*/
!                                                                     *         !/*po          3*/
!---------------------------------------------------------------------*         !/*po          4*/
!                                                                     *         !/*po          5*/
#include <lcs/double.h>                                                         /*!po          6*/
#include <fluka89/flprm1.h>                                                     /*!po          7*/
#include <fluka89/flprm2.h>                                                     /*!po          8*/
#include <lcs/inout.h>                                                          /*!po          9*/
      u=rang()                                                                  !/*po         10*/
      cs=rang()                                                                 !/*po         11*/
      if (u.lt.0.5d0) cs=-cs                                                    !/*po         12*/
      si=sqrt(1.d0-cs*cs+1.d-10)                                                !/*po         13*/
      return                                                                    !/*po         14*/
      end                                                                       !/*po         15*/
