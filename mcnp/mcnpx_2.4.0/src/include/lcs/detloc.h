!     $Id: detloc.h,v 1.4 2002/03/27 18:14:14 gwm Exp $                       !/*de          1*/
!=== detloc ===========================================================*        !/*de          2*/
!                                                                      *        !/*de          3*/
!----------------------------------------------------------------------*        !/*de          4*/
!                                                                      *        !/*de          5*/
!     detloc                         created 20-sep-1989 by a. ferrari *        !/*de          6*/
!                                                                      *        !/*de          7*/
!            included in:                                              *        !/*de          8*/
!                         detect                                       *        !/*de          9*/
!                         bdnopt                                       *        !/*de         10*/
!                                                                      *        !/*de         11*/
!            w a r n i n g any change of the ndtcm2 parameter must be  *        !/*de         12*/
!            done also on the ndtcmx parameter of the detect module!   *        !/*de         13*/
!----------------------------------------------------------------------*        !/*de         14*/
!                                                                      *        !/*de         15*/
      parameter (ndtcm2=10)                                                     !/*de         16*/
      common /detloc/ accump(ndtcm2), accumn(ndtcm2), icoinc(ndtcm2),           !/*de         17*/
     & nclas                                                                    !/*de         18*/
