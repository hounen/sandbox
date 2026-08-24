!     $Id: comcon.h,v 1.4 2002/03/27 18:14:13 gwm Exp $                       !/*co          1*/
!----------------------------------------------------------------------*        !/*co          2*/
!     include file: comcon copy                   created 26/11/86 by p*        !/*co          3*/
!     changes: none                                                    *        !/*co          4*/
!     included in the following subroutines or functions: not updated  *        !/*co          5*/
!                                                                      *        !/*co          6*/
!     description of the common block(s) and variable(s)               *        !/*co          7*/
!                                                                      *        !/*co          8*/
!                                                                      *        !/*co          9*/
!     /comcon/ contains information about compounds                    *        !/*co         10*/
!         icompl(imat) = number of materials in a compound             *        !/*co         11*/
!         matnum(i)    = material numbers (compounds consist of the    *        !/*co         12*/
!                        materials)                                    *        !/*co         13*/
!         contnt(i)    = partial densities                             *        !/*co         14*/
!         sumacn(i)    = cumulative atom contents of the compounds     *        !/*co         15*/
!         cabinx(i)    = cumulative at.cont.*inelastic cross section   *        !/*co         16*/
!         cabelx(i)    = cumulative at.cont.*  elastic cross section   *        !/*co         17*/
!         anxnor       = total inelastic cross section                 *        !/*co         18*/
!         elxnor       = total   elastic cross section                 *        !/*co         19*/
!----------------------------------------------------------------------*        !/*co         20*/
!                                                                               !/*co         21*/
      parameter (icomax=100)                                                    !/*co         22*/
      common /comcon/ contnt(icomax), sumacn(icomax), cabinx(icomax),           !/*co         23*/
     & cabelx(icomax), anxnor, elxnor, icompl(mxxmdf), matnum(icomax)           !/*co         24*/
