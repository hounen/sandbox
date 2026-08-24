!     $Id: paprop.h,v 1.4 2002/03/27 18:14:08 gwm Exp $                       !/*pa          1*/
!----------------------------------------------------------------------*        !/*pa          2*/
!     include file: paprop copy                   created 26/11/86 by p*        !/*pa          3*/
!     changes: none                                                    *        !/*pa          4*/
!     included in the following subroutines or functions: not updated  *        !/*pa          5*/
!                                                                      *        !/*pa          6*/
!     description of the common block(s) and variable(s)               *        !/*pa          7*/
!                                                                      *        !/*pa          8*/
!     /paprop/ contains particle properties                            *        !/*pa          9*/
!        btype  = literal name of the particle                         *        !/*pa         10*/
!        am     = particle mass in gev                                 *        !/*pa         11*/
!        ichrge = electric charge of the particle                      *        !/*pa         12*/
!        iscore = explanations for the scored distribution             *        !/*pa         13*/
!        genpar = names of the generalized particles                   *        !/*pa         14*/
!        ijdisc = list of the particle types to be discarded           *        !/*pa         15*/
!        thalf  = half life of the particle in sec                     *        !/*pa         16*/
!----------------------------------------------------------------------*        !/*pa         17*/
      character*8 btype, genpar                                                 !/*pa         18*/
      common /paprop/ btype(30), am(30), ichrge(30), iscore(10), genpar(        !/*pa         19*/
     & 30), ijdisc(30), thalf(30)                                               !/*pa         20*/
