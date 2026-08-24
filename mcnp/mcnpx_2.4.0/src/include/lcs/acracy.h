!     $Id: acracy.h,v 1.4 2002/03/27 18:14:12 gwm Exp $                       !/*ac          1*/
!=== acracy ===========================================================*        !/*ac          2*/
!                                                                      *        !/*ac          3*/
!----------------------------------------------------------------------*        !/*ac          4*/
!     include file: acracy copy              created 26/11/86 by pa    *        !/*ac          5*/
!     changes: none                                                    *        !/*ac          6*/
!     included in the following subroutines or functions: not updated  *        !/*ac          7*/
!                                                                      *        !/*ac          8*/
!     description of the common block(s) and variable(s)               *        !/*ac          9*/
!                                                                      *        !/*ac         10*/
!                                                                      *        !/*ac         11*/
!     /acracy/ contains the accuracy requirements for ionisation       *        !/*ac         12*/
!              energy deposition and multiple coulomb scattering       *        !/*ac         13*/
!        estep  = distance between the ionisation energy               *        !/*ac         14*/
!                 deposition points                                    *        !/*ac         15*/
!        deferr = maximum permissible deflection error due to          *        !/*ac         16*/
!                 multiple coulomb scattering                          *        !/*ac         17*/
!        mulpri = approximation of multiple coulomb scattering         *        !/*ac         18*/
!                 used for primary particles                           *        !/*ac         19*/
!        mulsec = like mulpri but for secondaries                      *        !/*ac         20*/
!        iostep = number of ionisation energy deposition points        *        !/*ac         21*/
!                 between collisions                                   *        !/*ac         22*/
!        ishow  = number of energy deposition points/particle in       *        !/*ac         23*/
!                 electro-magnetic cascades                            *        !/*ac         24*/
!----------------------------------------------------------------------*        !/*ac         25*/
!                                                                               !/*ac         26*/
      common /acracy/ estep, deferr, mulpri, mulsec, iostep, ishow              !/*ac         27*/
