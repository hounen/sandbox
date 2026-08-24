!     $Id: scores.h,v 1.4 2002/03/27 18:14:18 gwm Exp $                       !/*sc          1*/
!----------------------------------------------------------------------*        !/*sc          2*/
!     include file: scores copy             created 27/11/86 by pa     *        !/*sc          3*/
!     changes: none                                                    *        !/*sc          4*/
!     included in the following subroutines or functions: not updated  *        !/*sc          5*/
!                                                                      *        !/*sc          6*/
!     description of the common block(s) and variable(s)               *        !/*sc          7*/
!                                                                      *        !/*sc          8*/
!     /scores/ contains data for various fluence estimators            *        !/*sc          9*/
!              (array index 1 = tracklength, 2 = boundary crossing,    *        !/*sc         10*/
!              3 = collision)                                          *        !/*sc         11*/
!        itbc   = indices of regions where fluence estimate is wanted  *        !/*sc         12*/
!                 (first regions if bdryx estimators)                  *        !/*sc         13*/
!        iboun2 = indices of second regions defining the boundaries    *        !/*sc         14*/
!                 (for bdryx estimator)                                *        !/*sc         15*/
!        kestim = if .eq.1 means corresponding estimator is requested  *        !/*sc         16*/
!        loga   = if .true. means logarithmic energy binning is        *        !/*sc         17*/
!                 requested for corresponding fluence estimator        *        !/*sc         18*/
!        ebin   = energy bin width for fluence estimators              *        !/*sc         19*/
!                 (in gev if linear, in decades if logarithmic)        *        !/*sc         20*/
!        nebin  = number of energy bins for fluence estimators         *        !/*sc         21*/
!                 (first bin is between zero and threshold esthr)      *        !/*sc         22*/
!        esthr  = threshold kinetic energy in gev for fluence          *        !/*sc         23*/
!                 estimators                                           *        !/*sc         24*/
!        lb1cur = if .true. bdryx estimator is for one-way current     *        !/*sc         25*/
!        lb2cur = if .true. bdryx estimator is for two-way current     *        !/*sc         26*/
!        lb1flu = if .true. bdryx est. is for one-way (cosine) fluence *        !/*sc         27*/
!        lb2flu = if .true. bdryx est. is for two-way (cosine) fluence *        !/*sc         28*/
!                 (default)                                            *        !/*sc         29*/
!        area   = area of boundary surfaces for bdryx estimator        *        !/*sc         30*/
!                                                                      *        !/*sc         31*/
!----------------------------------------------------------------------*        !/*sc         32*/
      logical loga, lb1cur, lb2cur, lb1flu, lb2flu                              !/*sc         33*/
      common /scores/ ebin(3), esthr(3), area(20), iboun2(20), nebin(3),        !/*sc         34*/
     & itbc(20,3), kestim(3), loga(3), lb1cur, lb2cur, lb1flu, lb2flu           !/*sc         35*/
