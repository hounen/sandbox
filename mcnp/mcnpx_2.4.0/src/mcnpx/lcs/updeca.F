!     $Id: updeca.F,v 1.4 2002/03/27 18:15:26 gwm Exp $                       !/*up          1*/
      subroutine updeca                                                         !/*up          2*/
#include <lcs/double.h>                                                         /*!up          3*/
!     ==================================================================        !/*up          4*/
!     updeca calculates cos(delta),cos(phi),and sin(phi) for decay              !/*up          5*/
!     of isobars with respect to a specific z-axis in the isobar frame          !/*up          6*/
!                                                                               !/*up          7*/
! UPDECA calls   SQRT                                                           !/*up          8*/
! called by GAMMA  PSEL                                                         !/*up          9*/
!     ==================================================================        !/*up         10*/
!                                                                               !/*up         11*/
#include <lcs/inout.h>                                                          /*!up         12*/
#include <lcs/vegas.h>                                                          /*!up         13*/
      cosw=ws(6)*wws(6)+ws(7)*wws(7)+ws(8)*wws(8)                               !/*up         14*/
      p2=sqrt(wws(5)*wws(5)-wws(4)*wws(4))                                      !/*up         15*/
      alphac=ws(5)/ws(4)                                                        !/*up         16*/
      betac=sqrt(alphac*alphac-dp1)                                             !/*up         17*/
      p2cx=alphac*p2*cosw-betac*wws(5)                                          !/*up         18*/
! temporary test                                                                !/*up         19*/
      temp=dp1-cosw*cosw                                                        !/*up         20*/
      if (temp.lt.dp0) then                                                     !/*up         21*/
        write (io,30) temp,(ws(i),i=6,8),(wws(i),i=6,8)                         !/*up         22*/
        temp=dp0                                                                !/*up         23*/
      endif                                                                     !/*up         24*/
      sinw=sqrt(temp)                                                           !/*up         25*/
!  ws(28)=cos(delta)                                                            !/*up         26*/
      ws(28)=p2cx/sqrt(p2cx*p2cx+p2*p2*sinw*sinw)                               !/*up         27*/
      if (sinw.lt.(1.d-7)) go to 10                                             !/*up         28*/
      if (abs(ws(8)).ge.(.99999d0)) go to 20                                    !/*up         29*/
! normal calc. of sin(phi) and cos(phi)                                         !/*up         30*/
      denom=sqrt(dp1-ws(8)*ws(8))                                               !/*up         31*/
      beta=(-ws(7)*wws(6)+ws(6)*wws(7))/denom                                   !/*up         32*/
      gamma=(-ws(8)*(ws(6)*wws(6)+ws(7)*wws(7))+wws(8)*denom*denom)             !/*up         33*/
      gamma=gamma/denom                                                         !/*up         34*/
! ws(26)=cos(phi) ws(27)=sin(phi)                                               !/*up         35*/
      ws(26)=-gamma/sinw                                                        !/*up         36*/
      ws(27)=beta/sinw                                                          !/*up         37*/
      return                                                                    !/*up         38*/
   10 continue                                                                  !/*up         39*/
! sinw almost zero.hence phi=0                                                  !/*up         40*/
      ws(26)=dp1                                                                !/*up         41*/
      ws(27)=dp0                                                                !/*up         42*/
      return                                                                    !/*up         43*/
   20 continue                                                                  !/*up         44*/
! isobar moving almost parallel to lab.z axis                                   !/*up         45*/
      ws(26)=wws(6)/sinw                                                        !/*up         46*/
      ws(27)=wws(7)/sinw                                                        !/*up         47*/
      if (ws(8).lt.dp0) ws(26)=-ws(26)                                          !/*up         48*/
      return                                                                    !/*up         49*/
!                                                                               !/*up         50*/
   30 format (' updeca',1p7e13.5)                                               !/*up         51*/
      end                                                                       !/*up         52*/
