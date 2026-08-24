!     $Id: mtran.F,v 1.4 2002/03/27 18:15:11 gwm Exp $                        !/*mt          1*/
      subroutine mtran(lp,ip,ja)                                                !/*mt          2*/
#include <lcs/lptS.h>                                                           /*!mt          3*/
      dimension laipt(lpt), lajan(lpt)                                          !/*mt          4*/
      data laipt/9,1,20,21,20,4,4,31,32,33,34,2,22,24,23,22,9,1,3,3,6,6/        !/*mt          5*/
      data lajan/1,1,1,0,-1,-1,1,1,1,1,1,0,1,1,1,-1,-1,-1,1,-1,1,-1/            !/*mt          6*/
!                                                                               !/*mt          7*/
!     IPT  JAN  LAHET    NAME                                                   !/*mt          8*/
!      1    1      2     neutron                                                !/*mt          9*/
!      1   -1     18     anti-neutron                                           !/*mt         10*/
!      2    0     12     photon                                                 !/*mt         11*/
!      3    -     19     electron                                               !/*mt         12*/
!      3    -     20     positron                                               !/*mt         13*/
!      4    1      7     muon_-                                                 !/*mt         14*/
!      4   -1      6     anti-muon_-                                            !/*mt         15*/
!      5    1      -     tau_-                                                  !/*mt         16*/
!      6    1     21     neutrino_e                                             !/*mt         17*/
!      6   -1     22     anti-neutrino_e                                        !/*mt         18*/
!      7    1      -     neutrino_m                                             !/*mt         19*/
!      8    1      -     neutrino_t                                             !/*mt         20*/
!      9    1      1     proton                                                 !/*mt         21*/
!      9   -1     17     anti-proton                                            !/*mt         22*/
!     10    1      -     lambda_0                                               !/*mt         23*/
!     11    1      -     sigma_+                                                !/*mt         24*/
!     12    1      -     sigma_-                                                !/*mt         25*/
!     13    1      -     xi_0                                                   !/*mt         26*/
!     14    1      -     xi_-                                                   !/*mt         27*/
!     15    1      -     omega_-                                                !/*mt         28*/
!     16    1      -     lambda_c_+                                             !/*mt         29*/
!     17    1      -     xi_c_+                                                 !/*mt         30*/
!     18    1      -     xi_c_0                                                 !/*mt         31*/
!     19    1      -     lambda_b_0                                             !/*mt         32*/
!     20    1      3     pi_+                                                   !/*mt         33*/
!     20   -1      5     anti-pi_+                                              !/*mt         34*/
!     21    0      4     pi_0                                                   !/*mt         35*/
!     22    1     13     k_+                                                    !/*mt         36*/
!     22   -1     16     anti-k_+                                               !/*mt         37*/
!     23    1     15     k_0_short                                              !/*mt         38*/
!     24    1     14     k_0_long                                               !/*mt         39*/
!     25    1      -     d_+                                                    !/*mt         40*/
!     26    1      -     d_0                                                    !/*mt         41*/
!     27    1      -     d_s_+                                                  !/*mt         42*/
!     28    1      -     b_+                                                    !/*mt         43*/
!     29    1      -     b_0                                                    !/*mt         44*/
!     30    1      -     b_s_0                                                  !/*mt         45*/
!     31    1      8     deuteron                                               !/*mt         46*/
!     32    1      9     triton                                                 !/*mt         47*/
!     33    1     10     helium_3                                               !/*mt         48*/
!     34    1     11     alpha                                                  !/*mt         49*/
!                                                                               !/*mt         50*/
      ip=laipt(lp)                                                              !/*mt         51*/
      ja=lajan(lp)                                                              !/*mt         52*/
      return                                                                    !/*mt         53*/
      end                                                                       !/*mt         54*/
