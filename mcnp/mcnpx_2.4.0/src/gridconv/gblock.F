!     $Id: gblock.F,v 1.4 2002/03/27 18:13:50 gwm Exp $                       !/*gb          1*/
      block data gblock                                                         !/*gb          2*/
!                                                                               !/*gb          3*/
#include <gridconv/gcdouble.h>                                                  /*!gb          4*/
#include <gridconv/gccom5.h>                                                    /*!gb          5*/
!                                                                               !/*gb          6*/
!       Define data description variables.                                      !/*gb          7*/
      data hdv/'flux      ','tracks    ','population','erg depost',             !/*gb          8*/
     & 'dose      ',5*' '/                                                      !/*gb          9*/
      data hdd/'total     ','de/dx     ','nuc recoil','heating # ',             !/*gb         10*/
     & 'non_track ',5*' '/                                                      !/*gb         11*/
      data hip/'neutron   ','photon    ','electron  ','muon_-    ',             !/*gb         12*/
     & 'tau_-     ','neutrino_e','neutrino_m','neutrino_t','proton    ',        !/*gb         13*/
     & 'lambda_0  ','sigma_+   ','sigma_-   ','xi_0      ','xi_-      ',        !/*gb         14*/
     & 'omega_-   ','lambda_c_+','xi_c_+    ','xi_c_0    ','lambda_b_0',        !/*gb         15*/
     & 'pi_+      ','pi_0      ','k_+       ','k_0_short ','k_0_long  ',        !/*gb         16*/
     & 'd_+       ','d_0       ','d_s_+     ','b_+       ','b_0       ',        !/*gb         17*/
     & 'b_s_0     ','deuteron  ','triton    ','helium_3  ','alpha     '/        !/*gb         18*/
      data hdb/'regular  ','irregular'/                                         !/*gb         19*/
!                                                                               !/*gb         20*/
      end                                                                       !/*gb         21*/
