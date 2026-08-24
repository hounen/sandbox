!     $Id: bemit.h,v 1.4 2002/03/27 18:14:13 gwm Exp $                        !/*be          1*/
!----------------------------------------------------------------------*        !/*be          2*/
!     include file: bemit copy                    created 26/11/86 by p*        !/*be          3*/
!     changes: none                                                    *        !/*be          4*/
!     included in the following subroutines or functions: not updated  *        !/*be          5*/
!                                                                      *        !/*be          6*/
!     description of the common block(s) and variable(s)               *        !/*be          7*/
!                                                                      *        !/*be          8*/
!     /bemit/ contains beam properties when emittances specified       *        !/*be          9*/
!        verts  = s-parameter for the vertical focus, is distance of   *        !/*be         10*/
!                 v-focus from xina,yina,zina along the beam,          *        !/*be         11*/
!                 +ve if focus upstream of xina,yina,zina.             *        !/*be         12*/
!        vertl  = vertical l-parameter of the beam                     *        !/*be         13*/
!        verte  = vertical emittance                                   *        !/*be         14*/
!        hors   = s-parameter for horizontal focus, is distance of     *        !/*be         15*/
!                 h-focus from xina,yina,zina along the beam,          *        !/*be         16*/
!                 +ve if focus upstream of xina,yina,zina.             *        !/*be         17*/
!        horl   = horizontal l-parameter of the beam                   *        !/*be         18*/
!        hore   = horizontal emittance                                 *        !/*be         19*/
!        vsig   = sigma of spatial v-distribution                      *        !/*be         20*/
!        vpsig  = sigma of angular vprime-distribution                 *        !/*be         21*/
!        hsig   = sigma of spatial h-distribution                      *        !/*be         22*/
!        hpsig  = sigma of angular hprime-distribution                 *        !/*be         23*/
!        txv    = x-direction cosine of v-axis                         *        !/*be         24*/
!        tyv    = y-direction cosine of v-axis                         *        !/*be         25*/
!        tzv    = z-direction cosine of v-axis                         *        !/*be         26*/
!        txh    = x-direction cosine of h-axis                         *        !/*be         27*/
!        tyh    = y direction cosine of h-axis                         *        !/*be         28*/
!        tzh    = z-direction cosine of h-axis                         *        !/*be         29*/
!        ibemit = 1 if emittance option chosen, if not = 0             *        !/*be         30*/
!----------------------------------------------------------------------*        !/*be         31*/
      common /bemit/ verts, vertl, verte, hors, horl, hore, vsig, vpsig,        !/*be         32*/
     & hsig, hpsig, txv, tyv, tzv, txh, tyh, tzh, ibemit                        !/*be         33*/
