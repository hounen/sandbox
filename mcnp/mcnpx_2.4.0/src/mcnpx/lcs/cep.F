!     $Id: cep.F,v 1.4 2002/03/27 18:14:57 gwm Exp $                          !/*ce          1*/
      subroutine cep (delep,dele,w4,w)                                          !/*ce          2*/
#include <lcs/double.h>                                                         /*!ce          3*/
!     ==================================================================        !/*ce          4*/
!     delep is the energy change in the projectile rest frame                   !/*ce          5*/
!     dele is the corresponding energy change in the lab.                       !/*ce          6*/
!     w is the working vector                                                   !/*ce          7*/
!                                                                               !/*ce          8*/
! CEP    calls   SQRT  PLT                                                      !/*ce          9*/
! called by CASCAS  DECAY  GAMMA  PSEL                                          !/*ce         10*/
!     ==================================================================        !/*ce         11*/
!                                                                               !/*ce         12*/
      dimension p(4), pt(4), w(32)                                              !/*ce         13*/
#include <lcs/vegas.h>                                                          /*!ce         14*/
      bet=-voc                                                                  !/*ce         15*/
      p(1)=sqrt(w(5)*w(5)-w4*w4)                                                !/*ce         16*/
      p(2)=w(6)                                                                 !/*ce         17*/
      p(3)=w(7)                                                                 !/*ce         18*/
      p(4)=w(8)                                                                 !/*ce         19*/
!                                                                               !/*ce         20*/
!     lorentz transform from projectile to target frame                         !/*ce         21*/
!                                                                               !/*ce         22*/
      call plt (p,w(5),pt,et,bet,gama)                                          !/*ce         23*/
      et=et+delep                                                               !/*ce         24*/
!  particle energy is lower than its mass. is should                            !/*ce         25*/
!  be reflected in psel by subr refl                                            !/*ce         26*/
      if (et.gt.w4) go to 10                                                    !/*ce         27*/
      dele=-w(5)                                                                !/*ce         28*/
      return                                                                    !/*ce         29*/
   10 pt(1)=sqrt(et*et-w4*w4)                                                   !/*ce         30*/
!                                                                               !/*ce         31*/
!      lorentz boost back to projectile frame with correct                      !/*ce         32*/
!      (new) total energy                                                       !/*ce         33*/
!                                                                               !/*ce         34*/
      call plt (pt,et,p,el,voc,gama)                                            !/*ce         35*/
      dele=el-w(5)                                                              !/*ce         36*/
      return                                                                    !/*ce         37*/
      end                                                                       !/*ce         38*/
