!     $Id: evesel.h,v 1.4 2002/03/27 18:14:15 gwm Exp $                       !/*ev          1*/
!=== evesel ===========================================================*        !/*ev          2*/
!                                                                      *        !/*ev          3*/
!----------------------------------------------------------------------*        !/*ev          4*/
!     include file: evesel copy                   created 26/11/86 by p*        !/*ev          5*/
!     changes: none                                                    *        !/*ev          6*/
!     included in the following subroutines or functions: not updated  *        !/*ev          7*/
!                                                                      *        !/*ev          8*/
!     description of the common block(s) and variable(s)               *        !/*ev          9*/
!                                                                      *        !/*ev         10*/
!                                                                      *        !/*ev         11*/
!     /evesel/ used to remember the event-creation method              *        !/*ev         12*/
!        evecod = literal descriptions of the event-generation         *        !/*ev         13*/
!        ieve   = number of the event subroutines used                 *        !/*ev         14*/
!----------------------------------------------------------------------*        !/*ev         15*/
!                                                                               !/*ev         16*/
      character*8 evecod                                                        !/*ev         17*/
      common /evesel/ evecod(11), ieve                                          !/*ev         18*/
