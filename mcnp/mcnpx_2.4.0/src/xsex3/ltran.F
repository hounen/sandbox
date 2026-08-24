!     $Id: ltran.F,v 1.4 2002/03/27 18:16:25 gwm Exp $                        !/*lt          1*/
      function ltran (ja)                                                       !/*lt          2*/
!                                                                               !/*lt          3*/
! Translates an MCNPX particle type JA into a LAHET ITYP > 0                    !/*lt          4*/
! Interim version, since MCNPX WSSA file does not yet contain                   !/*lt          5*/
! information to distinguish particle-antiparticle pairs.                       !/*lt          6*/
! Thus the (incorrect) mapping: pi-,pi+ > pi+, etc.                             !/*lt          7*/
!                                                                               !/*lt          8*/
!                                                                               !/*lt          9*/
!     IPT  JAN  LAHET    NAME                                                   !/*lt         10*/
!      1    1      2     neutron, anti-neutron                                  !/*lt         11*/
!      2    0     12     photon                                                 !/*lt         12*/
!      3    -     19     electron, positron                                     !/*lt         13*/
!      4    1      7     muon_-, anti-muon_- (muon_+)                           !/*lt         14*/
!      5    1      -     tau_-                                                  !/*lt         15*/
!      6    1     21     neutrino_e, anti-neutrino_e                            !/*lt         16*/
!      7    1      -     neutrino_m                                             !/*lt         17*/
!      8    1      -     neutrino_t                                             !/*lt         18*/
!      9    1      1     proton, anti-proton                                    !/*lt         19*/
!     10    1      -     lambda_0                                               !/*lt         20*/
!     11    1      -     sigma_+                                                !/*lt         21*/
!     12    1      -     sigma_-                                                !/*lt         22*/
!     13    1      -     xi_0                                                   !/*lt         23*/
!     14    1      -     xi_-                                                   !/*lt         24*/
!     15    1      -     omega_-                                                !/*lt         25*/
!     16    1      -     lambda_c_+                                             !/*lt         26*/
!     17    1      -     xi_c_+                                                 !/*lt         27*/
!     18    1      -     xi_c_0                                                 !/*lt         28*/
!     19    1      -     lambda_b_0                                             !/*lt         29*/
!     20    1      3     pi_+, anti-pi_+ (pi_-)                                 !/*lt         30*/
!     21    0      4     pi_0                                                   !/*lt         31*/
!     22    1     13     k_+, anti-k_+ (k_-)                                    !/*lt         32*/
!     23    1     15     k_0_short                                              !/*lt         33*/
!     24    1     14     k_0_long                                               !/*lt         34*/
!     25    1      -     d_+                                                    !/*lt         35*/
!     26    1      -     d_0                                                    !/*lt         36*/
!     27    1      -     d_s_+                                                  !/*lt         37*/
!     28    1      -     b_+                                                    !/*lt         38*/
!     29    1      -     b_0                                                    !/*lt         39*/
!     30    1      -     b_s_0                                                  !/*lt         40*/
!     31    1      8     deuteron                                               !/*lt         41*/
!     32    1      9     triton                                                 !/*lt         42*/
!     33    1     10     helium_3                                               !/*lt         43*/
!     34    1     11     alpha                                                  !/*lt         44*/
!     35    -     -      dummy particle                                         !/*lt         45*/
!                                                                               !/*lt         46*/
#include <htape3x/mipt.h>                                                       /*!lt         47*/
      dimension ipt(mipt)                                                       !/*lt         48*/
      data ipt/2,12,19,7,0,21,0,0,1,10*0,3,4,13,15,14,6*0,8,9,10,11,0/          !/*lt         49*/
      ltran=ipt(ja)                                                             !/*lt         50*/
      return                                                                    !/*lt         51*/
      end                                                                       !/*lt         52*/
