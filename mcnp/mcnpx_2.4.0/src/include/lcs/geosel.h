!     $Id: geosel.h,v 1.4 2002/03/27 18:14:15 gwm Exp $                       !/*ge          1*/
!----------------------------------------------------------------------*        !/*ge          2*/
!     include file: geosel copy                   created 26/11/86 by p*        !/*ge          3*/
!     changes: none                                                    *        !/*ge          4*/
!     included in the following subroutines or functions: not updated  *        !/*ge          5*/
!                                                                      *        !/*ge          6*/
!     description of the common block(s) and variable(s)               *        !/*ge          7*/
!                                                                      *        !/*ge          8*/
!     /geosel/ used to remember the geometry                           *        !/*ge          9*/
!        geocod = literal descriptions of the geometries               *        !/*ge         10*/
!        igeo   = number of the geometry type used                     *        !/*ge         11*/
!        ipri   = 1 print out the calls to geofar                      *        !/*ge         12*/
!----------------------------------------------------------------------*        !/*ge         13*/
      character*8 geocod                                                        !/*ge         14*/
      common /geosel/ geocod(10), igeo, ipri                                    !/*ge         15*/
