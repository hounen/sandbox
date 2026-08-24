!     $Id: plopri.h,v 1.4 2002/03/27 18:14:17 gwm Exp $                       !/*pl          1*/
!----------------------------------------------------------------------*        !/*pl          2*/
!     include file: plopri copy                   created 26/11/86 by p*        !/*pl          3*/
!     changes: none                                                    *        !/*pl          4*/
!     included in the following subroutines or functions: not updated  *        !/*pl          5*/
!                                                                      *        !/*pl          6*/
!     description of the common block(s) and variable(s)               *        !/*pl          7*/
!                                                                      *        !/*pl          8*/
!     /plopri/ contains x,y and z-distributions of the inelastic       *        !/*pl          9*/
!              interaction points of the beam particles                *        !/*pl         10*/
!        xpri   = projection on x-axis                                 *        !/*pl         11*/
!        xplow  = lower limit of the x-coordinates                     *        !/*pl         12*/
!        xphigh = upper limit of the x-coordinates                     *        !/*pl         13*/
!        xpbin  = bin size in x-direction                              *        !/*pl         14*/
!        xpbig  = number of interactions above the upper limit         *        !/*pl         15*/
!        xpsmal = number of interactions below the lower limit         *        !/*pl         16*/
!        nxpbin = number of bins in x-direction                        *        !/*pl         17*/
!        ...    = analogically for y- and z- directions                *        !/*pl         18*/
!        lpripl = if .eq. 1 first call: initialise for weight scoring  *        !/*pl         19*/
!               = if .eq. 2 first call: initialise for wee=1. scoring  *        !/*pl         20*/
!               = if .eq. 3 weight scoring                             *        !/*pl         21*/
!               = if .eq. 4 wee=1. scoring                             *        !/*pl         22*/
!                                                                      *        !/*pl         23*/
!----------------------------------------------------------------------*        !/*pl         24*/
!                                                                               !/*pl         25*/
      common /plopri/ xpri(50), xplow, xphigh, xpbin, xpbig, xpsmal,            !/*pl         26*/
     & ypri(50), yplow, yphigh, ypbin, ypbig, ypsmal, zpri(50), zplow,          !/*pl         27*/
     & zphigh, zpbin, zpbig, zpsmal, nxpbin, nypbin, nzpbin, lpripl             !/*pl         28*/
