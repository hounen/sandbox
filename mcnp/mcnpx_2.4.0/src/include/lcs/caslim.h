!     $Id: caslim.h,v 1.4 2002/03/27 18:14:13 gwm Exp $                       !/*ca          1*/
!=== caslim ===========================================================*        !/*ca          2*/
!                                                                               !/*ca          3*/
!----------------------------------------------------------------------*        !/*ca          4*/
!     include file: caslim copy                   created 26/11/86 by p*        !/*ca          5*/
!     changes: 20-sep-89 by a. ferrari                                 *        !/*ca          6*/
!     included in the following subroutines or functions: not updated  *        !/*ca          7*/
!                                                                      *        !/*ca          8*/
!     description of the common block(s) and variable(s)               *        !/*ca          9*/
!                                                                      *        !/*ca         10*/
!                                                                      *        !/*ca         11*/
!                                                                      *        !/*ca         12*/
!     /caslim/ is needed to decide when to stop the run                *        !/*ca         13*/
!        tlim   = if cpu-time-left<tlim the run will be ended          *        !/*ca         14*/
!        tmean  = is the average time needed for the following         *        !/*ca         15*/
!                 of one beam particle                                 *        !/*ca         16*/
!        tmax   = is the maximum time needed for the following         *        !/*ca         17*/
!                 of one beam particle                                 *        !/*ca         18*/
!        ttot   = the cumulative time needed to follow the beam        *        !/*ca         19*/
!                 particles                                            *        !/*ca         20*/
!        ncases = the maximum number of beam particles to be followed  *        !/*ca         21*/
!        nstars = the maximum number of stars to be generated          *        !/*ca         22*/
!        ncase  = the number of beam particles followed                *        !/*ca         23*/
!        mbatch = batch sizes                                          *        !/*ca         24*/
!        nbatch = number of batches                                    *        !/*ca         25*/
!        ibatch = current batch number                                 *        !/*ca         26*/
!        ncoinc = flag used by the detect option to know if the ncase  *        !/*ca         27*/
!                 particle has or has not to be considered in coinci-  *        !/*ca         28*/
!                 dence with the previous one (if they have the same   *        !/*ca         29*/
!                 ncoinc they belong to the same event)                *        !/*ca         30*/
!----------------------------------------------------------------------*        !/*ca         31*/
!                                                                               !/*ca         32*/
      common /caslim/ tlim, tmean, tmax, ttot, ncases, nstars, ncase,           !/*ca         33*/
     & mbatch(500), nbatch, ibatch, ncoinc                                      !/*ca         34*/
