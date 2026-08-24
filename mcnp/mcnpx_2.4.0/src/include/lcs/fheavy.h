!     $Id: fheavy.h,v 1.4 2002/03/27 18:14:15 gwm Exp $                       !/*fh          1*/
!=== fheavy ===========================================================*        !/*fh          2*/
!                                                                               !/*fh          3*/
!----------------------------------------------------------------------*        !/*fh          4*/
!     last change on 5 april 1990     by alfredo ferrari, infn milan   *        !/*fh          5*/
!     include file: fheavy copy       created 5/4/90 by a. ferrari     *        !/*fh          6*/
!     changes: none                                                    *        !/*fh          7*/
!     included in the following subroutines or functions: not updated  *        !/*fh          8*/
!                                                                      *        !/*fh          9*/
!     description of the common block(s) and variable(s)               *        !/*fh         10*/
!                                                                      *        !/*fh         11*/
!     /fheavy/ is the storage for heavy secondaries created in the     *        !/*fh         12*/
!              nuclear evaporation                                     *        !/*fh         13*/
!        npheav     = number of secondaries                            *        !/*fh         14*/
!        kheavy(ip) = type of the secondary ip                         *        !/*fh         15*/
!        cxheav(ip) = direction cosine of the secondary ip             *        !/*fh         16*/
!                     with respect to x-axis                           *        !/*fh         17*/
!        cyheav(ip) = direction cosine of the secondary ip             *        !/*fh         18*/
!                     with respect to y-axis                           *        !/*fh         19*/
!        czheav(ip) = direction cosine of the secondary ip             *        !/*fh         20*/
!                     with respect to z-axis                           *        !/*fh         21*/
!        tkheav(ip) = kinetic energy of secondary ip                   *        !/*fh         22*/
!        pheavy(ip) = momentum of the secondary ip                     *        !/*fh         23*/
!        wheavy(ip) = weight of the secondary ip                       *        !/*fh         24*/
!        amheav(ip) = atomic masses of the six types of evaporated     *        !/*fh         25*/
!                     particles                                        *        !/*fh         26*/
!        anheav(kp) = name of the kp-type heavy particle               *        !/*fh         27*/
!        icheav(kp) = charge of the kp-type heavy particle             *        !/*fh         28*/
!        ibheav(kp) = mass number of the kp-type heavy particle        *        !/*fh         29*/
!----------------------------------------------------------------------*        !/*fh         30*/
!                                                                               !/*fh         31*/
      parameter (mxheav=100)                                                    !/*fh         32*/
      character*8 anheav                                                        !/*fh         33*/
      common /fheavy/ cxheav(mxheav), cyheav(mxheav), czheav(mxheav),           !/*fh         34*/
     & tkheav(mxheav), pheavy(mxheav), wheavy(mxheav), amheav(6), anheav        !/*fh         35*/
     & (6), kheavy(mxheav), icheav(6), ibheav(6), npheav                        !/*fh         36*/
