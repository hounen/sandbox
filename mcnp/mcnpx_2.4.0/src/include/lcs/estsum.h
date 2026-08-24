!     $Id: estsum.h,v 1.4 2002/03/27 18:14:14 gwm Exp $                       !/*es          1*/
!----------------------------------------------------------------------*        !/*es          2*/
!     include file: estsum copy                   created 27/11/86 by p*        !/*es          3*/
!     changes: none                                                    *        !/*es          4*/
!     included in the following subroutines or functions: not updated  *        !/*es          5*/
!                                                                      *        !/*es          6*/
!     description of the common block(s) and variable(s)               *        !/*es          7*/
!                                                                      *        !/*es          8*/
!     /estsum/ accumulators for tracklength, bdryx and collision       *        !/*es          9*/
!              fluence estimators                                      *        !/*es         10*/
!        estadd = sum of estimates (by region, particle, energy and    *        !/*es         11*/
!                 estimator type)                                      *        !/*es         12*/
!        sx     = sum of estimates reset at the beginning of each batch*        !/*es         13*/
!                 (by region, particle and estimator type). it is      *        !/*es         14*/
!                 converted to standard deviation at the end of the job*        !/*es         15*/
!        sx2    = sum of squares of the above                          *        !/*es         16*/
!        lstdev = if .true. standard deviations are calculated         *        !/*es         17*/
!                                                                      *        !/*es         18*/
!----------------------------------------------------------------------*        !/*es         19*/
      parameter (idmebn=100, idmest=20)                                         !/*es         20*/
      logical lstdev                                                            !/*es         21*/
      double precision sx, sx2                                                  !/*es         22*/
      common /estsum/ sx(idmest,4,3), sx2(idmest,4,3), estadd(idmest,4          !/*es         23*/
     & ,idmebn,3), lstdev                                                       !/*es         24*/
