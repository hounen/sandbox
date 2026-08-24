!     $Id: errgeo.h,v 1.4 2002/03/27 18:14:14 gwm Exp $                       !/*er          1*/
!----------------------------------------------------------------------*        !/*er          2*/
!     include file: errgeo copy                   created 27/11/86 by p*        !/*er          3*/
!     any change must be done also in module errge2 which differs only *        !/*er          4*/
!     for encur ---> ekin and which is included in kaskad              *        !/*er          5*/
!                                                   (a.ferrari 5/7/89) *        !/*er          6*/
!     changes: none                                                    *        !/*er          7*/
!     included in the following subroutines or functions: not updated  *        !/*er          8*/
!                                                                      *        !/*er          9*/
!     description of the common block(s) and variable(s)               *        !/*er         10*/
!                                                                      *        !/*er         11*/
!     /errgeo/ variables to handle geometry errors (only implemented   *        !/*er         12*/
!              in combinatorial geometry at present)                   *        !/*er         13*/
!        ljomes = if .false. switches off geometry error messages,     *        !/*er         14*/
!                 giving only a global summary (default: .true.)       *        !/*er         15*/
!        lmgnmv = if .true. the last step was performed using the      *        !/*er         16*/
!                 tracking inside the magnetic field                   *        !/*er         17*/
!        lephad = index to distinguish hadrons (value = 1) and         *        !/*er         18*/
!                 leptons/photons (value = 2)                          *        !/*er         19*/
!        encur  = current particle kinetic energy for transmission to  *        !/*er         20*/
!                 geometry subroutines                                 *        !/*er         21*/
!        jomerr = counters of geometry errors                          *        !/*er         22*/
!        endisc = accumulators of energy discarded because of geometry *        !/*er         23*/
!                 errors                                               *        !/*er         24*/
!                                                                      *        !/*er         25*/
!----------------------------------------------------------------------*        !/*er         26*/
!                                                                               !/*er         27*/
      logical ljomes, lmgnmv                                                    !/*er         28*/
      common /errgeo/ encur, endisc(2), ljomes, lmgnmv, lephad, jomerr(2        !/*er         29*/
     & )                                                                        !/*er         30*/
