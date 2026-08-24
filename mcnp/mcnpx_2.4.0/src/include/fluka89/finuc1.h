!     $Id: finuc1.h,v 1.4 2002/03/27 18:14:07 gwm Exp $                       !/*fi          1*/
!=== finuc ============================================================*        !/*fi          2*/
!                                                                               !/*fi          3*/
!----------------------------------------------------------------------*        !/*fi          4*/
!     last change on 5 april 1990     by alfredo ferrari, infn milan   *        !/*fi          5*/
!     include file: finuc copy                   created 26/11/86 by pa*        !/*fi          6*/
!     note: if changed change finuc2 copy also                         *        !/*fi          7*/
!     changes: none                                                    *        !/*fi          8*/
!     included in the following subroutines or functions: not updated  *        !/*fi          9*/
!                                                                      *        !/*fi         10*/
!     description of the common block(s) and variable(s)               *        !/*fi         11*/
!                                                                      *        !/*fi         12*/
!     /finuc/ is the storage for secondaries created in event          *        !/*fi         13*/
!        np        = number of secondaries                             *        !/*fi         14*/
!        kpart(ip) = type of the secondary ip                          *        !/*fi         15*/
!        cxr(ip)   = direction cosine of the secondary ip              *        !/*fi         16*/
!                    with respect to x-axis                            *        !/*fi         17*/
!        cyr(ip)   = direction cosine of the secondary ip              *        !/*fi         18*/
!                    with respect to y-axis                            *        !/*fi         19*/
!        czr(ip)   = direction cosine of the secondary ip              *        !/*fi         20*/
!                    with respect to z-axis                            *        !/*fi         21*/
!        tki(ip)   = kinetic energy of secondary ip                    *        !/*fi         22*/
!        plr(ip)   = momentum of the secondary ip                      *        !/*fi         23*/
!        wei(ip)   = weight of the secondary ip                        *        !/*fi         24*/
!        tv        = excitation energy                                 *        !/*fi         25*/
!        tvcms     = actual excitation energy of the residual nucleus  *        !/*fi         26*/
!        tvrecl    = recoil kinetic energy of the residual nucleus     *        !/*fi         27*/
!        tvheav    = recoil kinetic energies of heavy (2-h, 3-h, 3-he, *        !/*fi         28*/
!                    4-he) fragments after evaporation                 *        !/*fi         29*/
!----------------------------------------------------------------------*        !/*fi         30*/
!                                                                               !/*fi         31*/
      parameter (mxp=999)                                                       !/*fi         32*/
      common /finuc/ cxr(mxp), cyr(mxp), czr(mxp), tki(mxp), plr(mxp),          !/*fi         33*/
     & wei(mxp), tv, tvcms, tvrecl, tvheav, np2, np, kpart(mxp)                 !/*fi         34*/
