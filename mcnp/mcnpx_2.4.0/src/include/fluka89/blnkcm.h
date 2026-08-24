!     $Id: blnkcm.h,v 1.4 2002/03/27 18:14:06 gwm Exp $                       !/*bl          1*/
!=== blnkcm ===========================================================*        !/*bl          2*/
!                                                                               !/*bl          3*/
!======================================================================*        !/*bl          4*/
!   a. ferrari 3-sept.-1989                                            *        !/*bl          5*/
!                                                                      *        !/*bl          6*/
!     blnkcm: this is the blank common for the vax version of fluka    *        !/*bl          7*/
!                                                                      *        !/*bl          8*/
!     w a r n i n g !!!! check also blnkdm module for any change!!!    *        !/*bl          9*/
!                                                                      *        !/*bl         10*/
!     nblnmx: blank common dimension in real*8 units!                  *        !/*bl         11*/
!                                                                      *        !/*bl         12*/
!     addrcm: this common contains all useful addresses for the blank  *        !/*bl         13*/
!             common  (in real*4 or i*4 numeration!!!!!!!!!!!!!!!!)    *        !/*bl         14*/
!                                                                      *        !/*bl         15*/
!             mblnmx = blank common dimension in i*4/real*4 units      *        !/*bl         16*/
!                                                                      *        !/*bl         17*/
!             kblnkl = last memory location used in the blank common   *        !/*bl         18*/
!                                                                      *        !/*bl         19*/
!             kgmbgn = beginning of geometry data                      *        !/*bl         20*/
!                                                                      *        !/*bl         21*/
!             kgmlst = last memory location for the geometry data      *        !/*bl         22*/
!                                                                      *        !/*bl         23*/
!             kdtbgn = beginning of detector data                      *        !/*bl         24*/
!                                                                      *        !/*bl         25*/
!             kdtlst = last memory location for the detector data      *        !/*bl         26*/
!                                                                      *        !/*bl         27*/
!             kubbgn = beginning of user defined binning storage       *        !/*bl         28*/
!                                                                      *        !/*bl         29*/
!             kublst = last memory location for user defined binnings  *        !/*bl         30*/
!                                                                      *        !/*bl         31*/
!             kuxbgn = beginning of user defined bdrx storage          *        !/*bl         32*/
!                                                                      *        !/*bl         33*/
!             kuxlst = last memory location for user defined bdrx      *        !/*bl         34*/
!                                                                      *        !/*bl         35*/
!       w a r n i n g the blank common is initialized to 0 as a i*4    *        !/*bl         36*/
!                       array!!!!!!!!!!!!!                             *        !/*bl         37*/
!                                                                      *        !/*bl         38*/
!----------------------------------------------------------------------*        !/*bl         39*/
      parameter (nblnmx=120000)                                                 !/*bl         40*/
      common /addrcm/ mblnmx                                                    !/*bl         41*/
