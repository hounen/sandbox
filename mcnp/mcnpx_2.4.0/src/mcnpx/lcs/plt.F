!     $Id: plt.F,v 1.4 2002/03/27 18:15:16 gwm Exp $                          !/*pl          1*/
      subroutine plt (p,e,pt,et,beta,gama)                                      !/*pl          2*/
#include <lcs/double.h>                                                         /*!pl          3*/
!     ==================================================================        !/*pl          4*/
!     plt transforms the energy and momentum from the projectile                !/*pl          5*/
!     to the target frame. p,pt are four dimensional vectors                    !/*pl          6*/
!     containing the length and direction cosines of c*momentum                 !/*pl          7*/
!                                                                               !/*pl          8*/
! PLT    calls   SQRT                                                           !/*pl          9*/
! called by AINTER  REGB  REFL  RLAR  CEP  DELTA  GAMMA  POENEW  PSEL           !/*pl         10*/
!     ==================================================================        !/*pl         11*/
!                                                                               !/*pl         12*/
      dimension p(4), pt(4)                                                     !/*pl         13*/
      px=p(1)*p(2)                                                              !/*pl         14*/
      py=p(1)*p(3)                                                              !/*pl         15*/
      pz=p(1)*p(4)                                                              !/*pl         16*/
      pzt=(pz+beta*e)/gama                                                      !/*pl         17*/
      pp=sqrt(px*px+py*py+pzt*pzt)                                              !/*pl         18*/
      pt(1)=pp                                                                  !/*pl         19*/
      pt(2)=px/pp                                                               !/*pl         20*/
      pt(3)=py/pp                                                               !/*pl         21*/
      pt(4)=pzt/pp                                                              !/*pl         22*/
      et=(e+beta*pz)/gama                                                       !/*pl         23*/
      return                                                                    !/*pl         24*/
      end                                                                       !/*pl         25*/
