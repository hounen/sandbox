!     $Id: aptota.F,v 1.4 2002/03/27 18:14:54 gwm Exp $                       !/*ap          1*/
      subroutine aptota(e,apscat,aptot)                                         !/*ap          2*/
!                                                                               !/*ap          3*/
!       subr aptota calculates the ap cross section,                            !/*ap          4*/
!  that is the elastic scattering and total, the annihilation                   !/*ap          5*/
!  in our energy range beeing just the difference.                              !/*ap          6*/
!  tentatively we use an exponential fit to experimental                        !/*ap          7*/
!  data of v.flamino et al, cern-hera 79-03.                                    !/*ap          8*/
!  ap-p and ap-n cross sections are assumed equal.                              !/*ap          9*/
!                                                                               !/*ap         10*/
!*ca vegas                                                                      !/*ap         11*/
!                                                                               !/*ap         12*/
!     apscat = exp(4.6051702-1.036515387*exp(.38084751*log(                     !/*ap         13*/
!    &                 max(0.001,0.001*e))))                                    !/*ap         14*/
!     aptot = exp(4.54850633*exp(-.060102153*log(                               !/*ap         15*/
!    &                 max(0.001,0.001*e))))                                    !/*ap         16*/
!              sig(e) charge-exchange                                           !/*ap         17*/
!     aptac(e) = 4.4/sqrt(max(0.001,0.001*e))                                   !/*ap         18*/
      return                                                                    !/*ap         19*/
      end                                                                       !/*ap         20*/
