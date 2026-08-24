!#ident ltran.F from deck xlt                                                 /*xlt         1*/
      integer function ltran(ip,ja)                                             !/*xlt         2*/
!     Return LAHET particle designator for any MCNPX particle.                  !/*xlt         3*/
      use global_parameters                                                     !/*xlt         4*/
      dimension mcityp(-1:1,mipt)                                               !/*xlt         5*/
      data mcityp/18,0,2,0,12,0,20,0,19,6,0,7,0,0,0,22,0,21,0,0,0,0,0,0,        !/*xlt         6*/
     & 17,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,        !/*xlt         7*/
     & 0,5,0,3,0,4,0,16,0,13,0,0,15,0,0,14,0,0,0,0,0,0,0,0,0,0,0,0,0,0,         !/*xlt         8*/
     & 0,0,0,0,0,0,8,0,0,9,0,0,10,0,0,11/                                       !/*xlt         9*/
!                                                                               !/*xlt        10*/
!     IPT  JAN  LAHET    NAME                                                   !/*xlt        11*/
!      1    1      2     neutron                                                !/*xlt        12*/
!      1   -1     18     anti-neutron                                           !/*xlt        13*/
!      2    0     12     photon                                                 !/*xlt        14*/
!      3    -     19     electron                                               !/*xlt        15*/
!      3    -     20     positron                                               !/*xlt        16*/
!      4    1      7     muon_-                                                 !/*xlt        17*/
!      4   -1      6     anti-muon_-                                            !/*xlt        18*/
!      5    1      -     tau_-                                                  !/*xlt        19*/
!      6    1     21     neutrino_e                                             !/*xlt        20*/
!      6   -1     22     anti-neutrino_e                                        !/*xlt        21*/
!      7    1      -     neutrino_m                                             !/*xlt        22*/
!      8    1      -     neutrino_t                                             !/*xlt        23*/
!      9    1      1     proton                                                 !/*xlt        24*/
!      9   -1     17     anti-proton                                            !/*xlt        25*/
!     10    1      -     lambda_0                                               !/*xlt        26*/
!     11    1      -     sigma_+                                                !/*xlt        27*/
!     12    1      -     sigma_-                                                !/*xlt        28*/
!     13    1      -     xi_0                                                   !/*xlt        29*/
!     14    1      -     xi_-                                                   !/*xlt        30*/
!     15    1      -     omega_-                                                !/*xlt        31*/
!     16    1      -     lambda_c_+                                             !/*xlt        32*/
!     17    1      -     xi_c_+                                                 !/*xlt        33*/
!     18    1      -     xi_c_0                                                 !/*xlt        34*/
!     19    1      -     lambda_b_0                                             !/*xlt        35*/
!     20    1      3     pi_+                                                   !/*xlt        36*/
!     20   -1      5     anti-pi_+                                              !/*xlt        37*/
!     21    0      4     pi_0                                                   !/*xlt        38*/
!     22    1     13     k_+                                                    !/*xlt        39*/
!     22   -1     16     anti-k_+                                               !/*xlt        40*/
!     23    1     15     k_0_short                                              !/*xlt        41*/
!     24    1     14     k_0_long                                               !/*xlt        42*/
!     25    1      -     d_+                                                    !/*xlt        43*/
!     26    1      -     d_0                                                    !/*xlt        44*/
!     27    1      -     d_s_+                                                  !/*xlt        45*/
!     28    1      -     b_+                                                    !/*xlt        46*/
!     29    1      -     b_0                                                    !/*xlt        47*/
!     30    1      -     b_s_0                                                  !/*xlt        48*/
!     31    1      8     deuteron                                               !/*xlt        49*/
!     32    1      9     triton                                                 !/*xlt        50*/
!     33    1     10     helium_3                                               !/*xlt        51*/
!     34    1     11     alpha                                                  !/*xlt        52*/
!                                                                               !/*xlt        53*/
      ltran=mcityp(ja,ip)                                                       !/*xlt        54*/
      return                                                                    !/*xlt        55*/
      end                                                                       !/*xlt        56*/
