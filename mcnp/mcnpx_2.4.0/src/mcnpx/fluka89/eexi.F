!     $Id: eexi.F,v 1.4 2002/03/27 18:14:42 gwm Exp $                         !/*ee          1*/
      double precision function eexi (ij,e,a)                                   !/*ee          2*/
!                                                                     *         !/*ee          3*/
!---------------------------------------------------------------------*         !/*ee          4*/
!                                                                     *         !/*ee          5*/
#include <lcs/double.h>                                                         /*!ee          6*/
#include <fluka89/flprm1.h>                                                     /*!ee          7*/
#include <fluka89/flprm2.h>                                                     /*!ee          8*/
#include <lcs/inout.h>                                                          /*!ee          9*/
!********************************************************************           !/*ee         10*/
!     version by                     j. ranft                                   !/*ee         11*/
!                                    leipzig                                    !/*ee         12*/
!                                                                               !/*ee         13*/
!     nuclear excitation energy including all low energy secondaries            !/*ee         14*/
!     in collisions of particle ij of energy e gev on nuclei a                  !/*ee         15*/
!                                                                               !/*ee         16*/
!     note!!!!!!! non-standard particle numbering                               !/*ee         17*/
!                                                                               !/*ee         18*/
!********************************************************************           !/*ee         19*/
!                                                                               !/*ee         20*/
      if (e.le.0.125d0) go to 20                                                !/*ee         21*/
      b=sqrt(a)/9.d0-.2d0                                                       !/*ee         22*/
      if (b.gt.a*0.01d0) b=a*0.01d0                                             !/*ee         23*/
      if (e.ge.3.d0) go to 10                                                   !/*ee         24*/
      if (b.lt.0.125d0) b=0.125d0                                               !/*ee         25*/
      eexi=0.125d0+(e-0.125d0)*(b-0.125d0)/2.875d0                              !/*ee         26*/
      return                                                                    !/*ee         27*/
   10 continue                                                                  !/*ee         28*/
      eexi=b                                                                    !/*ee         29*/
      return                                                                    !/*ee         30*/
   20 continue                                                                  !/*ee         31*/
      eexi=e                                                                    !/*ee         32*/
      return                                                                    !/*ee         33*/
      end                                                                       !/*ee         34*/
