!     $Id: fxtmx.h,v 1.4 2002/03/27 18:14:15 gwm Exp $                        !/*fx          1*/
!----------------------------------------------------------------------*        !/*fx          2*/
!                                                                      *        !/*fx          3*/
!     common fxtmx                                                     *        !/*fx          4*/
!     common fxtmx is used for the fractional fixed step option        *        !/*fx          5*/
!     any change in common fxtmx must be done also in the module media *        !/*fx          6*/
!     of egsadd!!!!!!!!!!!!!!!!                                        *        !/*fx          7*/
!         mxxmde = maximum number of media in egs                      *        !/*fx          8*/
!         estepe = maximum fractional energy loss allowed for the gi-  *        !/*fx          9*/
!                  ven medium                                          *        !/*fx         10*/
!         ltmin  = logical array to flag whether or not has the step   *        !/*fx         11*/
!                  to be larger or the equal than the minimum allowed  *        !/*fx         12*/
!                  by moliere's theory, regardless of the energy loss: *        !/*fx         13*/
!                  default is .true.                                   *        !/*fx         14*/
!                                                                      *        !/*fx         15*/
!----------------------------------------------------------------------*        !/*fx         16*/
!                                                                               !/*fx         17*/
      logical ltmin, lfxtmx                                                     !/*fx         18*/
      common /fxtmx/ estepe(mxxmde), istpe(mxxmde), ltmin(mxxmde),              !/*fx         19*/
     & lfxtmx                                                                   !/*fx         20*/
