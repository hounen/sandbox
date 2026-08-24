!     $Id: beexi.F,v 1.4 2002/03/27 18:14:39 gwm Exp $                        !/*be          1*/
      double precision function beexi (ij,e,a)                                  !/*be          2*/
!                                                                     *         !/*be          3*/
!---------------------------------------------------------------------*         !/*be          4*/
!                                                                     *         !/*be          5*/
#include <lcs/double.h>                                                         /*!be          6*/
#include <fluka89/flprm1.h>                                                     /*!be          7*/
#include <fluka89/flprm2.h>                                                     /*!be          8*/
#include <lcs/inout.h>                                                          /*!be          9*/
!-----------------------------------------------                                !/*be         10*/
!     nuclear excitation energy including all low energy secondaries in         !/*be         11*/
!     collisions of particle ij of energy e gev on nuclei a                     !/*be         12*/
!-----------------------------------------------                                !/*be         13*/
      if (e.le.0.125d0) go to 20                                                !/*be         14*/
      b=sqrt(a)/9.d0-.2d0                                                       !/*be         15*/
      if (b.gt.a*0.01d0) b=a*0.01d0                                             !/*be         16*/
      if (e.ge.3.d0) go to 10                                                   !/*be         17*/
      if (b.lt.0.125d0) b=0.125d0                                               !/*be         18*/
      beexi=0.125d0+(e-0.125d0)*(b-0.125d0)/2.875d0                             !/*be         19*/
      return                                                                    !/*be         20*/
   10 continue                                                                  !/*be         21*/
      beexi=b                                                                   !/*be         22*/
      return                                                                    !/*be         23*/
   20 continue                                                                  !/*be         24*/
      beexi=e                                                                   !/*be         25*/
      return                                                                    !/*be         26*/
      end                                                                       !/*be         27*/
