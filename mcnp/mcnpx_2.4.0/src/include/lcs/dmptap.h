!     $Id: dmptap.h,v 1.4 2002/03/27 18:14:14 gwm Exp $                       !/*dm          1*/
!----------------------------------------------------------------------*        !/*dm          2*/
!     include file: dmptap copy                   created 26/11/86 by p*        !/*dm          3*/
!     changes: none                                                    *        !/*dm          4*/
!     included in the following subroutines or functions: not updated  *        !/*dm          5*/
!                                                                      *        !/*dm          6*/
!     description of the common block(s) and variable(s)               *        !/*dm          7*/
!                                                                      *        !/*dm          8*/
!                                                                      *        !/*dm          9*/
!     /dmptap/ 'collision' types to be dumped into disk or tape        *        !/*dm         10*/
!     idump      = logical unit for the dump                           *        !/*dm         11*/
!     dump1...18 = if dumpx=.true. 'collision' type x will be dumped   *        !/*dm         12*/
!                   (for 'collision' types see subroutine dumpit.)     *        !/*dm         13*/
!----------------------------------------------------------------------*        !/*dm         14*/
      logical dump1, dump2, dump3, dump4, dump5, dump6                          !/*dm         15*/
      logical dump7, dump8, dump9, dump10, dump11, dump12                       !/*dm         16*/
      logical dump13, dump14, dump15, dump16, dump17, dump18                    !/*dm         17*/
      common /dmptap/ idump, dump1, dump2, dump3, dump4, dump5, dump6,          !/*dm         18*/
     & dump7, dump8, dump9, dump10, dump11, dump12, dump13, dump14,             !/*dm         19*/
     & dump15, dump16, dump17, dump18                                           !/*dm         20*/
