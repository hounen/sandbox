!     $Id: mapa.h,v 1.4 2002/03/27 18:14:07 gwm Exp $                         !/*ma          1*/
!----------------------------------------------------------------------*        !/*ma          2*/
!     include file: mapa copy                    created 26/11/86 by pa*        !/*ma          3*/
!     note: change also mapa2 copy if you touch this file              *        !/*ma          4*/
!     changes: none                                                    *        !/*ma          5*/
!     included in the following subroutines or functions: not updated  *        !/*ma          6*/
!                                                                      *        !/*ma          7*/
!     description of the common block(s) and variable(s)               *        !/*ma          8*/
!                                                                      *        !/*ma          9*/
!                                                                      *        !/*ma         10*/
!     /mapa/ contains the parameters describing the target materials   *        !/*ma         11*/
!        matnam = matnam(i)=name of the material number i              *        !/*ma         12*/
!        medium = medium(nreg)=material number of the region nreg      *        !/*ma         13*/
!        amss   = the atomic weight of the materials                   *        !/*ma         14*/
!        ztar   = atomic numbers of the materials                      *        !/*ma         15*/
!        rho    = densities of the materials                           *        !/*ma         16*/
!        zlin   = inelastic scattering lengths of the materials        *        !/*ma         17*/
!                 for beam particles at the average beam energy in cm  *        !/*ma         18*/
!        zlel   = elastic scattering lengths of the materials for      *        !/*ma         19*/
!                 beam particles at average beam energy in cm          *        !/*ma         20*/
!        zlrad  = radiation lengths of the materials in cm             *        !/*ma         21*/
!        zul    = inelastic scattering lengths of the materials        *        !/*ma         22*/
!                 for neutrons at threshold energy in cm               *        !/*ma         23*/
!        icomp  = 0 if the material is not a compound                  *        !/*ma         24*/
!        nregs  = total number of the regions                          *        !/*ma         25*/
!        nmat   = total number of the materials                        *        !/*ma         26*/
!                                                                      *        !/*ma         27*/
!                        mxxrgn = maximum number of regions            *        !/*ma         28*/
!                                                                      *        !/*ma         29*/
!----------------------------------------------------------------------*        !/*ma         30*/
!                                                                               !/*ma         31*/
!     character*8 matnam                                                        !/*ma         32*/
!     common /mapa/ amss(mxxmdf), rho(mxxmdf)                                   !/*ma         33*/
