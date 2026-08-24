!     $Id: parevt.h,v 1.4 2002/03/27 18:14:08 gwm Exp $                       !/*pa          1*/
!=== parevt ==========================================================*         !/*pa          2*/
!                                                                     *         !/*pa          3*/
!---------------------------------------------------------------------*         !/*pa          4*/
!     created on 28-3-90 by a. ferrari, infn - milan                  *         !/*pa          5*/
!                                                                     *         !/*pa          6*/
!           included in:                                              *         !/*pa          7*/
!                          corevt                                     *         !/*pa          8*/
!                          ferevt (ferevv)                            *         !/*pa          9*/
!                          eventq (eventv)                            *         !/*pa         10*/
!                          evxtes                                     *         !/*pa         11*/
!                                                                     *         !/*pa         12*/
!           frdiff = fraction of the total number of events which are *         !/*pa         13*/
!                    diffractive ones                                 *         !/*pa         14*/
!           pthdff = momentum threshold for diffractive events        *         !/*pa         15*/
!                    diffractive ones                                 *         !/*pa         16*/
!           ldiffr = flag for particles undergoing diffractive events *         !/*pa         17*/
!           linctv = flag to switch on/off cascade nucleons and exci- *         !/*pa         18*/
!                    tation energy (it is important for testing the   *         !/*pa         19*/
!                    generator                                        *         !/*pa         20*/
!           levprt = flag for nuclear evaporation                     *         !/*pa         21*/
!                                                                     *         !/*pa         22*/
!---------------------------------------------------------------------*         !/*pa         23*/
!                                                                               !/*pa         24*/
!  30% of events, randomly choosen, are diffractive events!!                    !/*pa         25*/
!  now 20% (j. ranft & a. ferrari, 25-3-90                                      !/*pa         26*/
!                                                                               !/*pa         27*/
      parameter (frdiff=0.2d+00)                                                !/*pa         28*/
      parameter (pthdff=8.5d+00)                                                !/*pa         29*/
      parameter (ethsea=1.0d+00)                                                !/*pa         30*/
      logical ldiffr, linctv                                                    !/*pa         31*/
      common /parevt/ ldiffr(30), linctv                                        !/*pa         32*/
