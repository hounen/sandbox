!     $Id: beam.h,v 1.4 2002/03/27 18:14:12 gwm Exp $                         !/*be          1*/
!----------------------------------------------------------------------*        !/*be          2*/
!     include file: beam copy                    created 26/11/86 by pa*        !/*be          3*/
!     changes: none                                                    *        !/*be          4*/
!     included in the following subroutines or functions: not updated  *        !/*be          5*/
!                                                                      *        !/*be          6*/
!     description of the common block(s) and variable(s)               *        !/*be          7*/
!                                                                      *        !/*be          8*/
!                                                                      *        !/*be          9*/
!     /beam/ contains properties of the beam of primary particles      *        !/*be         10*/
!        pbeam  = average momentum of the beam particles in gev/c      *        !/*be         11*/
!        dpbeam = momentum spread of the beam in gev/c                 *        !/*be         12*/
!        divbm  = angular divergense of the beam in mrad               *        !/*be         13*/
!        xspot  = beam width in x-direction in cm                      *        !/*be         14*/
!        yspot  = beam width in y-direction in cm                      *        !/*be         15*/
!        xina   = x-coordinate of the centre of the beam spot          *        !/*be         16*/
!        yina   = y-coordinate of the centre of the beam spot          *        !/*be         17*/
!        zina   = z-coordinate of the centre of the beam spot          *        !/*be         18*/
!        tinx   = direction cosine of the beam with respect to         *        !/*be         19*/
!                 x-axis                                               *        !/*be         20*/
!        tiny   = direction cosine of the beam with respect to         *        !/*be         21*/
!                 y-axis                                               *        !/*be         22*/
!        tinz   = direction cosine of the beam with respect to         *        !/*be         23*/
!                 z-axis                                               *        !/*be         24*/
!        nforce = number of the region of forced interaction           *        !/*be         25*/
!        xfor   = x-coord. of the starting point of the region nforce  *        !/*be         26*/
!        yfor   = y-coord. of the starting point of the region nforce  *        !/*be         27*/
!        zfor   = z-coord. of the starting point of the region nforce  *        !/*be         28*/
!        disfor = thickness of the region nforce in cm                 *        !/*be         29*/
!        wfor   = relative weight of the particle due to forcing       *        !/*be         30*/
!        ijbeam = beam particle type (see btype in /paprop/)           *        !/*be         31*/
!        ipbite = flag describing the shape of the momentum            *        !/*be         32*/
!                 distribution of the beam                             *        !/*be         33*/
!                 0=rectangular, 1=gaussian                            *        !/*be         34*/
!        idiv   = flag describing the shape of the angular             *        !/*be         35*/
!                 divergence distribution of the beam                  *        !/*be         36*/
!                 0=rectangular, 1=gaussian                            *        !/*be         37*/
!        ixspot = flag describing the shape of the spatial             *        !/*be         38*/
!                 distribution of the beam spot in x-direction         *        !/*be         39*/
!                 0=rectangular, 1=gaussian                            *        !/*be         40*/
!        iyspot = flag describing the shape of the spatial             *        !/*be         41*/
!                 distribution of the beam spot in y-direction         *        !/*be         42*/
!                 0=rectangular, 1=gaussian                            *        !/*be         43*/
!        beawei = weight of the beam particles                         *        !/*be         44*/
!                                                                      *        !/*be         45*/
!----------------------------------------------------------------------*        !/*be         46*/
      common /beam/ pbeam, dpbeam, divbm, xspot, yspot, xina, yina, zina        !/*be         47*/
     & , tinx, tiny, tinz, beawei, xfor, yfor, zfor, disfor, wfor,              !/*be         48*/
     & ijbeam, ipbite, idiv, ixspot, iyspot, nforce                             !/*be         49*/
