!     $Id: del1.F,v 1.4 2002/03/27 18:14:33 gwm Exp $                         !/*de          1*/
      function del1 (betasq,avz,at_den,bari_ln,jj)                              !/*de          2*/
!                                                                               !/*de          3*/
!     density effect correction Sternheimer                                     !/*de          4*/
!                                                                               !/*de          5*/
!         betasq = (v/c)**2 for the projectile                                  !/*de          6*/
!         avz = <Z> for the medium                                              !/*de          7*/
!             = SUM fme_(i)z_(i)                                                !/*de          8*/
!         at_den = atomic density (1/barn-cm)                                   !/*de          9*/
!         bari_ln = <ln I> with I in MeV                                        !/*de         10*/
!                 = SUM fme_(i)z_(i) ln I(i) /<Z>                               !/*de         11*/
!         jj:                                                                   !/*de         12*/
!            jj=1 gaseous element                                               !/*de         13*/
!            jj=2 condensed element                                             !/*de         14*/
!            jj=3 gaseous mixture                                               !/*de         15*/
!            jj=4 condensed mixture                                             !/*de         16*/
!                                                                               !/*de         17*/
#include <lcs/double.h>                                                         /*!de         18*/
      parameter (emass=0.5109990615d0)                                          !/*de         19*/
      parameter (el_rad=0.28179409238d0)                                        !/*de         20*/
      parameter (alpfsc=7.2973530791728595d-03)                                 !/*de         21*/
      parameter (del_con1=dp4*dppi*el_rad**3*(emass/alpfsc)**2*1.d-12)          !/*de         22*/
      parameter (ten_ln=2.3025851d0)                                            !/*de         23*/
      parameter (del_con2=dp1/ten_ln)                                           !/*de         24*/
      parameter (del_con3=dp2*ten_ln)                                           !/*de         25*/
      etasq=betasq/(dp1-betasq)                                                 !/*de         26*/
      bari_ev=1.d+6*exp(bari_ln)                                                !/*de         27*/
      cb=dp1-log(del_con1*avz*at_den)+dp2*bari_ln                               !/*de         28*/
      if (mod(jj,2).eq.0) then                                                  !/*de         29*/
        xb=dp2+min(1,int(bari_ev/100.d0))                                       !/*de         30*/
        xa=max(0.2d0,0.326d0*cb-dph*xb)                                         !/*de         31*/
      else                                                                      !/*de         32*/
        xb=dp4+min(1,int(cb/12.25d0))                                           !/*de         33*/
        xa=max(0.1d0*min(20,max(16,int(dp2*cb)-3)),0.326d0*cb-dph*xb)           !/*de         34*/
      endif                                                                     !/*de         35*/
      x=del_con2*dph*log(etasq)                                                 !/*de         36*/
      if (x.gt.xa) then                                                         !/*de         37*/
        del=del_con3*x-cb+(cb-del_con3*xa)*((xb-min(x,xb))/(xb-xa))**3          !/*de         38*/
        del=max(del,dp0)                                                        !/*de         39*/
      else                                                                      !/*de         40*/
        del=dp0                                                                 !/*de         41*/
      endif                                                                     !/*de         42*/
      del1=del                                                                  !/*de         43*/
      return                                                                    !/*de         44*/
      end                                                                       !/*de         45*/
