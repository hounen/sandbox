!#ident GLOBAL2_vv.F from deck mgt                                              !/*mgt         1*/
      module global_tables                                                      !/*mgt         2*/
!                                                                               !/*mgt         3*/
      use global_parameters                                                     !/*mgt         4*/
      implicit none                                                             !/*mgt         5*/
      save                                                                      !/*mgt         6*/
      public                                                                    !/*mgt         7*/
      integer, private :: k                                                     !/*mgt         8*/
!                                                                               !/*mgtc        1*/
!***********************************************************************        !/*mgtc        2*/
!        Particle dependent arrays.                                             !/*mgtc        3*/
!                                                                               !/*mgtc        4*/
!        Particle abbreviations (single-character designators).                 !/*mgtc        5*/
      character*(mipt) :: hip = 'npe|*uvwhl+-xyoc!?</zk%^g@f>bqdtsa'            !/*mgtl        1*/
!                                                                               !/*mgtc        7*/
!        Names, masses (in MeV), and charges of particles.                      !/*mgtc        8*/
      character*11 :: hnp(mipt)                                                 !/*mgtc        9*/
      real(kindr) :: gpt(mipt),charge(mipt)                                     !/*mgtc       10*/
      data hnp( 1),gpt( 1),charge( 1)/'neutron    ',     939.58d0, zero/        !/*mgtc       11*/
      data hnp( 2),gpt( 2),charge( 2)/'photon     ',         zero, zero/        !/*mgtc       12*/
      data hnp( 3),gpt( 3),charge( 3)/'electron   ',   0.511008d0,-1.d0/        !/*mgtc       13*/
      data hnp( 4),gpt( 4),charge( 4)/'muon_-     ', 105.658389d0,-1.d0/        !/*mgtc       14*/
      data hnp( 5),gpt( 5),charge( 5)/'tau_-      ',     1777.1d0,-1.d0/        !/*mgtc       15*/
      data hnp( 6),gpt( 6),charge( 6)/'neutrino_e ',         zero, zero/        !/*mgtc       16*/
      data hnp( 7),gpt( 7),charge( 7)/'neutrino_m ',         zero, zero/        !/*mgtc       17*/
      data hnp( 8),gpt( 8),charge( 8)/'neutrino_t ',         zero, zero/        !/*mgtc       18*/
      data hnp( 9),gpt( 9),charge( 9)/'proton     ',938.2723128d0,  one/        !/*mgtc       19*/
      data hnp(10),gpt(10),charge(10)/'lambda_0   ',   1115.684d0, zero/        !/*mgtc       20*/
      data hnp(11),gpt(11),charge(11)/'sigma_+    ',    1189.37d0,  one/        !/*mgtc       21*/
      data hnp(12),gpt(12),charge(12)/'sigma_-    ',   1197.436d0,-1.d0/        !/*mgtc       22*/
      data hnp(13),gpt(13),charge(13)/'xi_0       ',     1314.9d0, zero/        !/*mgtc       23*/
      data hnp(14),gpt(14),charge(14)/'xi_-       ',    1321.32d0,-1.d0/        !/*mgtc       24*/
      data hnp(15),gpt(15),charge(15)/'omega_-    ',    1672.45d0,-1.d0/        !/*mgtc       25*/
      data hnp(16),gpt(16),charge(16)/'lambda_c_+ ',     2285.0d0,  one/        !/*mgtc       26*/
      data hnp(17),gpt(17),charge(17)/'xi_c_+     ',     2465.1d0,  one/        !/*mgtc       27*/
      data hnp(18),gpt(18),charge(18)/'xi_c_0     ',     2470.3d0, zero/        !/*mgtc       28*/
      data hnp(19),gpt(19),charge(19)/'lambda_b_0 ',     5641.0d0, zero/        !/*mgtc       29*/
      data hnp(20),gpt(20),charge(20)/'pi_+       ',  139.56995d0,  one/        !/*mgtc       30*/
      data hnp(21),gpt(21),charge(21)/'pi_0       ',   134.9764d0, zero/        !/*mgtc       31*/
      data hnp(22),gpt(22),charge(22)/'k_+        ',    493.677d0,  one/        !/*mgtc       32*/
      data hnp(23),gpt(23),charge(23)/'k_0_short  ',    497.672d0, zero/        !/*mgtc       33*/
      data hnp(24),gpt(24),charge(24)/'k_0_long   ',    497.672d0, zero/        !/*mgtc       34*/
      data hnp(25),gpt(25),charge(25)/'d_+        ',     1869.3d0,  one/        !/*mgtc       35*/
      data hnp(26),gpt(26),charge(26)/'d_0        ',     1864.5d0, zero/        !/*mgtc       36*/
      data hnp(27),gpt(27),charge(27)/'d_s_+      ',     1968.5d0,  one/        !/*mgtc       37*/
      data hnp(28),gpt(28),charge(28)/'b_+        ',     5278.7d0,  one/        !/*mgtc       38*/
      data hnp(29),gpt(29),charge(29)/'b_0        ',     5279.0d0, zero/        !/*mgtc       39*/
      data hnp(30),gpt(30),charge(30)/'b_s_0      ',     5375.0d0, zero/        !/*mgtc       40*/
      data hnp(31),gpt(31),charge(31)/'deuteron   ',   1875.627d0,  one/        !/*mgtc       41*/
      data hnp(32),gpt(32),charge(32)/'triton     ',   2808.951d0,  one/        !/*mgtc       42*/
      data hnp(33),gpt(33),charge(33)/'helium_3   ',   2808.421d0,  two/        !/*mgtc       43*/
      data hnp(34),gpt(34),charge(34)/'alpha      ',   3727.418d0,  two/        !/*mgtc       44*/
!                                                                               !/*mgtc       45*/
!        Minimum and default energy cutoffs (in MeV).                           !/*mgtc       46*/
      real(kindr) :: ecut_min(mipt),ecut_def(mipt)                              !/*mgtc       47*/
      data ecut_min( 1), ecut_def( 1) /   zero,     zero/                       !/*mgtc       48*/
      data ecut_min( 2), ecut_def( 2) /1.0d-03,  1.0d-03/                       !/*mgtc       49*/
      data ecut_min( 3), ecut_def( 3) /1.0d-03,  1.0d-03/                       !/*mgtc       50*/
      data ecut_min( 4), ecut_def( 4) /1.0d-03,0.11261d0/                       !/*mgtc       51*/
      data ecut_min( 5), ecut_def( 5) /1.0d-03,  1.894d0/                       !/*mgtc       52*/
      data ecut_min( 6), ecut_def( 6) /   zero,     zero/                       !/*mgtc       53*/
      data ecut_min( 7), ecut_def( 7) /   zero,     zero/                       !/*mgtc       54*/
      data ecut_min( 8), ecut_def( 8) /   zero,     zero/                       !/*mgtc       55*/
      data ecut_min( 9), ecut_def( 9) /1.0d-03,      one/                       !/*mgtc       56*/
      data ecut_min(10), ecut_def(10) /1.0d-03,      one/                       !/*mgtc       57*/
      data ecut_min(11), ecut_def(11) /1.0d-03, 1.2676d0/                       !/*mgtc       58*/
      data ecut_min(12), ecut_def(12) /1.0d-03, 1.2762d0/                       !/*mgtc       59*/
      data ecut_min(13), ecut_def(13) /1.0d-03,      one/                       !/*mgtc       60*/
      data ecut_min(14), ecut_def(14) /1.0d-03, 1.4082d0/                       !/*mgtc       61*/
      data ecut_min(15), ecut_def(15) /1.0d-03, 1.7825d0/                       !/*mgtc       62*/
      data ecut_min(16), ecut_def(16) /1.0d-03, 2.4353d0/                       !/*mgtc       63*/
      data ecut_min(17), ecut_def(17) /1.0d-03, 2.6273d0/                       !/*mgtc       64*/
      data ecut_min(18), ecut_def(18) /1.0d-03,      one/                       !/*mgtc       65*/
      data ecut_min(19), ecut_def(19) /1.0d-03,      one/                       !/*mgtc       66*/
      data ecut_min(20), ecut_def(20) /1.0d-03,0.14875d0/                       !/*mgtc       67*/
      data ecut_min(21), ecut_def(21) /   zero,     zero/                       !/*mgtc       68*/
      data ecut_min(22), ecut_def(22) /1.0d-03,0.52614d0/                       !/*mgtc       69*/
      data ecut_min(23), ecut_def(23) /1.0d-06,  1.0d-06/                       !/*mgtc       70*/
      data ecut_min(24), ecut_def(24) /1.0d-06,  1.0d-06/                       !/*mgtc       71*/
      data ecut_min(25), ecut_def(25) /1.0d-03, 1.9923d0/                       !/*mgtc       72*/
      data ecut_min(26), ecut_def(26) /1.0d-03,      one/                       !/*mgtc       73*/
      data ecut_min(27), ecut_def(27) /1.0d-03,  2.098d0/                       !/*mgtc       74*/
      data ecut_min(28), ecut_def(28) /1.0d-03,  5.626d0/                       !/*mgtc       75*/
      data ecut_min(29), ecut_def(29) /1.0d-03,      one/                       !/*mgtc       76*/
      data ecut_min(30), ecut_def(30) /1.0d-03,      one/                       !/*mgtc       77*/
      data ecut_min(31), ecut_def(31) /1.0d-03,      two/                       !/*mgtc       78*/
      data ecut_min(32), ecut_def(32) /1.0d-03,    three/                       !/*mgtc       79*/
      data ecut_min(33), ecut_def(33) /1.0d-03,    three/                       !/*mgtc       80*/
      data ecut_min(34), ecut_def(34) /1.0d-03,    4.0d0/                       !/*mgtc       81*/
!                                                                               !/*mgtc       82*/
!        Decay constants (in shakes) and flag for (not)                         !/*mgtc       83*/
!        self-antiparticles.                                                    !/*mgtc       84*/
      real(kindr) :: decay(mipt)                                                !/*mgtc       85*/
      integer(kindi) :: not_self_anti(mipt)                                     !/*mgtc       86*/
      data decay( 1), not_self_anti( 1) /       huge, 1/                        !/*mgtc       87*/
      data decay( 2), not_self_anti( 2) /       huge, 0/                        !/*mgtc       88*/
      data decay( 3), not_self_anti( 3) /       huge, 1/                        !/*mgtc       89*/
      data decay( 4), not_self_anti( 4) /2.19703d+02, 1/                        !/*mgtc       90*/
      data decay( 5), not_self_anti( 5) /   2.92d-05, 1/                        !/*mgtc       91*/
      data decay( 6), not_self_anti( 6) /       huge, 1/                        !/*mgtc       92*/
      data decay( 7), not_self_anti( 7) /       huge, 1/                        !/*mgtc       93*/
      data decay( 8), not_self_anti( 8) /       huge, 1/                        !/*mgtc       94*/
      data decay( 9), not_self_anti( 9) /       huge, 1/                        !/*mgtc       95*/
      data decay(10), not_self_anti(10) /  2.632d-02, 1/                        !/*mgtc       96*/
      data decay(11), not_self_anti(11) /   7.99d-03, 1/                        !/*mgtc       97*/
      data decay(12), not_self_anti(12) /  1.479d-02, 1/                        !/*mgtc       98*/
      data decay(13), not_self_anti(13) /    2.9d-02, 1/                        !/*mgtc       99*/
      data decay(14), not_self_anti(14) /  1.639d-02, 1/                        !/*mgtc      100*/
      data decay(15), not_self_anti(15) /   8.22d-03, 1/                        !/*mgtc      101*/
      data decay(16), not_self_anti(16) /   2.06d-05, 1/                        !/*mgtc      102*/
      data decay(17), not_self_anti(17) /    3.5d-05, 1/                        !/*mgtc      103*/
      data decay(18), not_self_anti(18) /    9.8d-06, 1/                        !/*mgtc      104*/
      data decay(19), not_self_anti(19) /   1.07d-04, 1/                        !/*mgtc      105*/
      data decay(20), not_self_anti(20) /    2.603d0, 1/                        !/*mgtc      106*/
      data decay(21), not_self_anti(21) /    8.4d-09, 0/                        !/*mgtc      107*/
      data decay(22), not_self_anti(22) /   1.2371d0, 1/                        !/*mgtc      108*/
      data decay(23), not_self_anti(23) /  8.926d-03, 1/                        !/*mgtc      109*/
      data decay(24), not_self_anti(24) /     5.17d0, 1/                        !/*mgtc      110*/
      data decay(25), not_self_anti(25) /  1.057d-04, 1/                        !/*mgtc      111*/
      data decay(26), not_self_anti(26) /   4.15d-05, 1/                        !/*mgtc      112*/
      data decay(27), not_self_anti(27) /   4.67d-05, 1/                        !/*mgtc      113*/
      data decay(28), not_self_anti(28) /   1.54d-04, 1/                        !/*mgtc      114*/
      data decay(29), not_self_anti(29) /    1.5d-04, 1/                        !/*mgtc      115*/
      data decay(30), not_self_anti(30) /   1.34d-04, 1/                        !/*mgtc      116*/
      data decay(31), not_self_anti(31) /       huge, 1/                        !/*mgtc      117*/
      data decay(32), not_self_anti(32) /       huge, 1/                        !/*mgtc      118*/
      data decay(33), not_self_anti(33) /       huge, 1/                        !/*mgtc      119*/
      data decay(34), not_self_anti(34) /       huge, 1/                        !/*mgtc      120*/
!                                                                               !/*mgt         9*/
!***********************************************************************        !/*mgt        10*/
!        integer variables and arrays.                                          !/*mgt        11*/
!                                                                               !/*mgt        12*/
      integer :: itty                                                           !/*mgt        13*/
      integer :: jtty                                                           !/*mgt        14*/
!                                                                               !/*mgt        15*/
!        pwb columns corresponding to values of nter.                           !/*mgt        16*/
      integer(kindi) :: jrwb(items_pax_total,number_particle_categories)        !/*mgt        17*/
!     The link between various values of NTER and PWB categories.               !/*mgt        18*/
!     Allowed values of NTER must be among the loss categories of PAX,          !/*mgt        19*/
!     and their JRWB-values should not be 0.                                    !/*mgt        20*/
!        jrwb(nter,id_category_neutron):  neutrons;                             !/*mgt        21*/
!        jrwb(nter,id_category_photon):   photons;                              !/*mgt        22*/
!        jrwb(nter,id_category_electron): electrons;                            !/*mgt        23*/
!        jrwb(nter,id_category_gen_neu):  neutral particles;                    !/*mgt        24*/
!        jrwb(nter,id_category_gen_chg):  charged particles.                    !/*mgt        25*/
!                                                                               !/*mgt        26*/
!     JRWB: NTER-to-PWB link for neutrons.                                      !/*mgt        27*/
      data jrwb (loss_all_pls_escape,            id_category_neutron)           !/*mgt        28*/
     &                       /0/                                                !/*mgt        29*/
      data jrwb (loss_all_pls_energy_cutoff,     id_category_neutron)           !/*mgt        30*/
     &         /ipwb_external_energy_cutoff/                                    !/*mgt        31*/
      data jrwb (loss_all_pls_time_cutoff,       id_category_neutron)           !/*mgt        32*/
     &         /ipwb_external_time_cutoff/                                      !/*mgt        33*/
      data jrwb (loss_all_pls_weight_window,     id_category_neutron)           !/*mgt        34*/
     &         /ipwb_variance_weight_window/                                    !/*mgt        35*/
      data jrwb (loss_all_pls_cell_importance,   id_category_neutron)           !/*mgt        36*/
     &         /ipwb_variance_cell_importance/                                  !/*mgt        37*/
      data jrwb (loss_all_pls_weight_cutoff,     id_category_neutron)           !/*mgt        38*/
     &         /ipwb_variance_weight_cutoff/                                    !/*mgt        39*/
      data jrwb (loss_all_pls_energy_importance, id_category_neutron)           !/*mgt        40*/
     &         /ipwb_variance_energy_import/                                    !/*mgt        41*/
      data jrwb (loss_all_pls_dxtran,            id_category_neutron)           !/*mgt        42*/
     &         /ipwb_variance_dxtran/                                           !/*mgt        43*/
      data jrwb (loss_all_pls_forced_collisions, id_category_neutron)           !/*mgt        44*/
     &         /ipwb_variance_forced_coll/                                      !/*mgt        45*/
      data jrwb (loss_all_pls_exp_transform,     id_category_neutron)           !/*mgt        46*/
     &         /ipwb_variance_exp_transform/                                    !/*mgt        47*/
      data jrwb (loss_neutron_downscattering,    id_category_neutron)           !/*mgt        48*/
     &                       /0/                                                !/*mgt        49*/
      data jrwb (loss_neutron_capture,           id_category_neutron)           !/*mgt        50*/
     &          /ipwb_neutron_capture/                                          !/*mgt        51*/
      data jrwb (loss_neutron_loss_to_n_xn,      id_category_neutron)           !/*mgt        52*/
     &          /ipwb_neutron_loss_to_n_xn/                                     !/*mgt        53*/
      data jrwb (loss_neutron_loss_to_fission,   id_category_neutron)           !/*mgt        54*/
     &          /ipwb_neutron_loss_to_fission/                                  !/*mgt        55*/
      data jrwb (loss_neutron_nucl_interaction,  id_category_neutron)           !/*mgt        56*/
     &          /ipwb_neutron_nuc_interaction/                                  !/*mgt        57*/
      data jrwb (loss_neutron_particle_decay,    id_category_neutron)           !/*mgt        58*/
     &          /ipwb_neutron_particle_decay/                                   !/*mgt        59*/
      data jrwb (loss_neutron_tabular_boundary,  id_category_neutron)           !/*mgt        60*/
     &          /ipwb_neutron_tabular_bound/                                    !/*mgt        61*/
      data jrwb (loss_neutron_unused_18,         id_category_neutron)           !/*mgt        62*/
     &                       /0/                                                !/*mgt        63*/
      data jrwb (loss_neutron_unused_19,         id_category_neutron)           !/*mgt        64*/
     &                       /0/                                                !/*mgt        65*/
      data jrwb (loss_neutron_unused_20,         id_category_neutron)           !/*mgt        66*/
     &                       /0/                                                !/*mgt        67*/
      data jrwb (loss_neutron_unused_21,         id_category_neutron)           !/*mgt        71*/
     &                       /0/                                                !/*mgt        73*/
!                                                                               !/*mgt        74*/
!     JRWB: NTER-to-PWB link for photons.                                       !/*mgt        75*/
      data jrwb (loss_all_pls_escape,            id_category_photon)            !/*mgt        76*/
     &                       /0/                                                !/*mgt        77*/
      data jrwb (loss_all_pls_energy_cutoff,     id_category_photon)            !/*mgt        78*/
     &         /ipwb_external_energy_cutoff/                                    !/*mgt        79*/
      data jrwb (loss_all_pls_time_cutoff,       id_category_photon)            !/*mgt        80*/
     &         /ipwb_external_time_cutoff/                                      !/*mgt        81*/
      data jrwb (loss_all_pls_weight_window,     id_category_photon)            !/*mgt        82*/
     &         /ipwb_variance_weight_window/                                    !/*mgt        83*/
      data jrwb (loss_all_pls_cell_importance,   id_category_photon)            !/*mgt        84*/
     &         /ipwb_variance_cell_importance/                                  !/*mgt        85*/
      data jrwb (loss_all_pls_weight_cutoff,     id_category_photon)            !/*mgt        86*/
     &         /ipwb_variance_weight_cutoff/                                    !/*mgt        87*/
      data jrwb (loss_all_pls_energy_importance, id_category_photon)            !/*mgt        88*/
     &         /ipwb_variance_energy_import/                                    !/*mgt        89*/
      data jrwb (loss_all_pls_dxtran,            id_category_photon)            !/*mgt        90*/
     &         /ipwb_variance_dxtran/                                           !/*mgt        91*/
      data jrwb (loss_all_pls_forced_collisions, id_category_photon)            !/*mgt        92*/
     &         /ipwb_variance_forced_coll/                                      !/*mgt        93*/
      data jrwb (loss_all_pls_exp_transform,     id_category_photon)            !/*mgt        94*/
     &         /ipwb_variance_exp_transform/                                    !/*mgt        95*/
      data jrwb (loss_photon_compton_scatter,    id_category_photon)            !/*mgt        96*/
     &                      /0/                                                 !/*mgt        97*/
      data jrwb (loss_photon_capture,            id_category_photon)            !/*mgt        98*/
     &       /ipwb_photon_capture/                                              !/*mgt        99*/
      data jrwb (loss_photon_pair_production,    id_category_photon)            !/*mgt       100*/
     &          /ipwb_photon_pair_production/                                   !/*mgt       101*/
      data jrwb (loss_photon_photonuclear,       id_category_photon)            !/*mgt       102*/
     &          /ipwb_photon_photonuc_abs/                                      !/*mgt       103*/
      data jrwb (loss_photon_unused_15,          id_category_photon)            !/*mgt       104*/
     &                      /0/                                                 !/*mgt       105*/
      data jrwb (loss_photon_unused_16,          id_category_photon)            !/*mgt       106*/
     &                      /0/                                                 !/*mgt       107*/
      data jrwb (loss_photon_unused_17,          id_category_photon)            !/*mgt       108*/
     &                      /0/                                                 !/*mgt       109*/
      data jrwb (loss_photon_unused_18,          id_category_photon)            !/*mgt       110*/
     &                       /0/                                                !/*mgt       111*/
      data jrwb (loss_photon_unused_19,          id_category_photon)            !/*mgt       112*/
     &                       /0/                                                !/*mgt       113*/
      data jrwb (loss_photon_unused_20,          id_category_photon)            !/*mgt       114*/
     &                       /0/                                                !/*mgt       115*/
      data jrwb (loss_photon_unused_21,          id_category_photon)            !/*mgt       116*/
     &                       /0/                                                !/*mgt       117*/
!                                                                               !/*mgt       118*/
!     JRWB: NTER-to-PWB link for electrons.                                     !/*mgt       119*/
      data jrwb (loss_all_pls_escape,             id_category_electron)         !/*mgt       120*/
     &                       /0/                                                !/*mgt       121*/
      data jrwb (loss_all_pls_energy_cutoff,      id_category_electron)         !/*mgt       122*/
     &         /ipwb_external_energy_cutoff/                                    !/*mgt       123*/
      data jrwb (loss_all_pls_time_cutoff,        id_category_electron)         !/*mgt       124*/
     &         /ipwb_external_time_cutoff/                                      !/*mgt       125*/
      data jrwb (loss_all_pls_weight_window,      id_category_electron)         !/*mgt       126*/
     &         /ipwb_variance_weight_window/                                    !/*mgt       127*/
      data jrwb (loss_all_pls_cell_importance,    id_category_electron)         !/*mgt       128*/
     &         /ipwb_variance_cell_importance/                                  !/*mgt       129*/
      data jrwb (loss_all_pls_weight_cutoff,      id_category_electron)         !/*mgt       130*/
     &         /ipwb_variance_weight_cutoff/                                    !/*mgt       131*/
      data jrwb (loss_all_pls_energy_importance,  id_category_electron)         !/*mgt       132*/
     &         /ipwb_variance_energy_import/                                    !/*mgt       133*/
      data jrwb (loss_all_pls_dxtran,             id_category_electron)         !/*mgt       134*/
     &         /ipwb_variance_dxtran/                                           !/*mgt       135*/
      data jrwb (loss_all_pls_forced_collisions,  id_category_electron)         !/*mgt       136*/
     &         /ipwb_variance_forced_coll/                                      !/*mgt       137*/
      data jrwb (loss_all_pls_exp_transform,      id_category_electron)         !/*mgt       138*/
     &         /ipwb_variance_exp_transform/                                    !/*mgt       139*/
      data jrwb (loss_electron_scattering,        id_category_electron)         !/*mgt       140*/
     &                        /0/                                               !/*mgt       141*/
      data jrwb (loss_electron_bremsstrahlung,    id_category_electron)         !/*mgt       142*/
     &                        /0/                                               !/*mgt       143*/
      data jrwb (loss_electron_unused_13,         id_category_electron)         !/*mgt       144*/
     &                        /0/                                               !/*mgt       145*/
      data jrwb (loss_electron_unused_14,         id_category_electron)         !/*mgt       146*/
     &                        /0/                                               !/*mgt       147*/
      data jrwb (loss_electron_unused_15,         id_category_electron)         !/*mgt       148*/
     &                        /0/                                               !/*mgt       149*/
      data jrwb (loss_electron_interact_or_decay, id_category_electron)         !/*mgt       150*/
     &                        /0/                                               !/*mgt       151*/
      data jrwb (loss_electron_unused_17,         id_category_electron)         !/*mgt       152*/
     &                        /0/                                               !/*mgt       153*/
      data jrwb (loss_electron_unused_18,         id_category_electron)         !/*mgt       154*/
     &                       /0/                                                !/*mgt       155*/
      data jrwb (loss_electron_unused_19,         id_category_electron)         !/*mgt       156*/
     &                       /0/                                                !/*mgt       157*/
      data jrwb (loss_electron_unused_20,         id_category_electron)         !/*mgt       158*/
     &                       /0/                                                !/*mgt       159*/
      data jrwb (loss_electron_unused_21,         id_category_electron)         !/*mgt       160*/
     &                       /0/                                                !/*mgt       161*/
!                                                                               !/*mgt       162*/
!     JRWB: NTER-to-PWB link for generic neutral particles.                     !/*mgt       163*/
      data jrwb (loss_all_pls_escape,            id_category_gen_neu)           !/*mgt       164*/
     &                       /0/                                                !/*mgt       165*/
      data jrwb (loss_all_pls_energy_cutoff,     id_category_gen_neu)           !/*mgt       166*/
     &         /ipwb_external_energy_cutoff/                                    !/*mgt       167*/
      data jrwb (loss_all_pls_time_cutoff,       id_category_gen_neu)           !/*mgt       168*/
     &         /ipwb_external_time_cutoff/                                      !/*mgt       169*/
      data jrwb (loss_all_pls_weight_window,     id_category_gen_neu)           !/*mgt       170*/
     &         /ipwb_variance_weight_window/                                    !/*mgt       171*/
      data jrwb (loss_all_pls_cell_importance,   id_category_gen_neu)           !/*mgt       172*/
     &         /ipwb_variance_cell_importance/                                  !/*mgt       173*/
      data jrwb (loss_all_pls_weight_cutoff,     id_category_gen_neu)           !/*mgt       174*/
     &         /ipwb_variance_weight_cutoff/                                    !/*mgt       175*/
      data jrwb (loss_all_pls_energy_importance, id_category_gen_neu)           !/*mgt       176*/
     &         /ipwb_variance_energy_import/                                    !/*mgt       177*/
      data jrwb (loss_all_pls_dxtran,            id_category_gen_neu)           !/*mgt       178*/
     &         /ipwb_variance_dxtran/                                           !/*mgt       179*/
      data jrwb (loss_all_pls_forced_collisions, id_category_gen_neu)           !/*mgt       180*/
     &         /ipwb_variance_forced_coll/                                      !/*mgt       181*/
      data jrwb (loss_all_pls_exp_transform,     id_category_gen_neu)           !/*mgt       182*/
     &         /ipwb_variance_exp_transform/                                    !/*mgt       183*/
      data jrwb (loss_gen_neu_nucl_interaction,  id_category_gen_neu)           !/*mgt       184*/
     &           /ipwb_gen_neu_nuc_interaction/                                 !/*mgt       185*/
      data jrwb (loss_gen_neu_elastic_scatter,   id_category_gen_neu)           !/*mgt       186*/
     &                       /0/                                                !/*mgt       187*/
      data jrwb (loss_gen_neu_particle_decay,    id_category_gen_neu)           !/*mgt       188*/
     &          /ipwb_gen_neu_particle_decay/                                   !/*mgt       189*/
      data jrwb (loss_gen_neu_unused_14,         id_category_gen_neu)           !/*mgt       190*/
     &                       /0/                                                !/*mgt       191*/
      data jrwb (loss_gen_neu_unused_15,         id_category_gen_neu)           !/*mgt       192*/
     &                       /0/                                                !/*mgt       193*/
      data jrwb (loss_gen_neu_unused_16,         id_category_gen_neu)           !/*mgt       194*/
     &                       /0/                                                !/*mgt       195*/
      data jrwb (loss_gen_neu_unused_17,         id_category_gen_neu)           !/*mgt       196*/
     &                       /0/                                                !/*mgt       197*/
      data jrwb (loss_gen_neu_unused_18,         id_category_gen_neu)           !/*mgt       198*/
     &                       /0/                                                !/*mgt       199*/
      data jrwb (loss_gen_neu_unused_19,         id_category_gen_neu)           !/*mgt       200*/
     &                       /0/                                                !/*mgt       201*/
      data jrwb (loss_gen_neu_unused_20,         id_category_gen_neu)           !/*mgt       202*/
     &                       /0/                                                !/*mgt       203*/
      data jrwb (loss_gen_neu_unused_21,         id_category_gen_neu)           !/*mgt       204*/
     &                       /0/                                                !/*mgt       205*/
!                                                                               !/*mgt       206*/
!     JRWB: NTER-to-PWB link for generic charged particles.                     !/*mgt       207*/
      data jrwb (loss_all_pls_escape,            id_category_gen_chg)           !/*mgt       208*/
     &                       /0/                                                !/*mgt       209*/
      data jrwb (loss_all_pls_energy_cutoff,     id_category_gen_chg)           !/*mgt       210*/
     &         /ipwb_external_energy_cutoff/                                    !/*mgt       211*/
      data jrwb (loss_all_pls_time_cutoff,       id_category_gen_chg)           !/*mgt       212*/
     &         /ipwb_external_time_cutoff/                                      !/*mgt       213*/
      data jrwb (loss_all_pls_weight_window,     id_category_gen_chg)           !/*mgt       214*/
     &         /ipwb_variance_weight_window/                                    !/*mgt       215*/
      data jrwb (loss_all_pls_cell_importance,   id_category_gen_chg)           !/*mgt       216*/
     &         /ipwb_variance_cell_importance/                                  !/*mgt       217*/
      data jrwb (loss_all_pls_weight_cutoff,     id_category_gen_chg)           !/*mgt       218*/
     &         /ipwb_variance_weight_cutoff/                                    !/*mgt       219*/
      data jrwb (loss_all_pls_energy_importance, id_category_gen_chg)           !/*mgt       220*/
     &         /ipwb_variance_energy_import/                                    !/*mgt       221*/
      data jrwb (loss_all_pls_dxtran,            id_category_gen_chg)           !/*mgt       222*/
     &         /ipwb_variance_dxtran/                                           !/*mgt       223*/
      data jrwb (loss_all_pls_forced_collisions, id_category_gen_chg)           !/*mgt       224*/
     &         /ipwb_variance_forced_coll/                                      !/*mgt       225*/
      data jrwb (loss_all_pls_exp_transform,     id_category_gen_chg)           !/*mgt       226*/
     &         /ipwb_variance_exp_transform/                                    !/*mgt       227*/
      data jrwb (loss_gen_chg_multiple_scatter,  id_category_gen_chg)           !/*mgt       228*/
     &                       /0/                                                !/*mgt       229*/
      data jrwb (loss_gen_chg_bremsstrahlung,    id_category_gen_chg)           !/*mgt       230*/
     &                       /0/                                                !/*mgt       231*/
      data jrwb (loss_gen_chg_nucl_interaction,  id_category_gen_chg)           !/*mgt       232*/
     &           /ipwb_gen_chg_nuc_interaction/                                 !/*mgt       233*/
      data jrwb (loss_gen_chg_elastic_scatter,   id_category_gen_chg)           !/*mgt       234*/
     &                       /0/                                                !/*mgt       235*/
      data jrwb (loss_gen_chg_particle_decay,    id_category_gen_chg)           !/*mgt       236*/
     &          /ipwb_gen_chg_particle_decay/                                   !/*mgt       237*/
      data jrwb (loss_gen_chg_capture,           id_category_gen_chg)           !/*mgt       238*/
     &          /ipwb_gen_chg_capture/                                          !/*mgt       239*/
      data jrwb (loss_gen_chg_tabular_sampling,  id_category_gen_chg)           !/*mgt       240*/
     &          /ipwb_gen_chg_tabular_sampl/                                    !/*mgt       241*/
      data jrwb (loss_gen_chg_unused_18,         id_category_gen_chg)           !/*mgt       242*/
     &                       /0/                                                !/*mgt       243*/
      data jrwb (loss_gen_chg_unused_19,         id_category_gen_chg)           !/*mgt       244*/
     &                       /0/                                                !/*mgt       245*/
      data jrwb (loss_gen_chg_unused_20,         id_category_gen_chg)           !/*mgt       246*/
     &                       /0/                                                !/*mgt       247*/
      data jrwb (loss_gen_chg_unused_21,         id_category_gen_chg)           !/*mgt       248*/
     &                       /0/                                                !/*mgt       249*/
!                                                                               !/*mgt       250*/
!        numerical names of built-in source functions.                          !/*mgt       251*/
      integer(kindi) :: jsf(mjsf) = (/ 2,3,4,5,6,7,21,31,41 /)                  !/*mgt       252*/
!                                                                               !/*mgt       253*/
!        fission isotopes.                                                      !/*mgt       254*/
      integer(kindi) :: mfiss(22) = (/                                          !/*mgt       255*/
     & 90232,91233,92233,92234,92235,92236,92237,92238,                         !/*mgt       256*/
     & 92239,92240,93237,94238,94239,94240,94241,94242,94243,                   !/*mgt       257*/
     & 95241,95242,95243,96242,96244 /)                                         !/*mgt       258*/
!                                                                               !/*mgt       259*/
!        number of values required for each source variable.                    !/*mgt       260*/
      integer(kindi) :: nvs(maxv) = (/                                          !/*mgt       261*/
     & 1,1,1,1,1,3,1,1,1,1,1,3,3,1,1,1,1,1,1,1 /)                               !/*mgt       262*/
!                                                                               !/*mgt       263*/
!***********************************************************************        !/*mgt       264*/
!        real variables and arrays.                                             !/*mgt       265*/
!                                                                               !/*mgt       306*/
!        fission q-values in mev.                                               !/*mgt       307*/
      real(kindr) :: qfiss(23) = (/                                             !/*mgt       308*/
     & 171.91,175.57,180.84,179.45,180.88,179.50,180.40,                        !/*mgt       309*/
     & 181.31,180.40,180.40,183.67,186.65,189.44,186.36,188.99,                 !/*mgt       310*/
     & 185.98,187.48,190.83,190.54,190.25,190.49,190.49,180. /)                 !/*mgt       311*/
!                                                                               !/*mgt       312*/
!        bremsstrahlung photon/electron energy ratios.                          !/*mgt       313*/
      real(kindr) :: rktc(mtop) = (/                                            !/*mgt       314*/
     & .001,.00125,.0015,.00175,.002,.0025,.003,.0035,.004,                     !/*mgt       315*/
     & .0045,.005,.0055,.006,.007,.008,.009,.01,.0125,.015,.0175,.02,           !/*mgt       316*/
     & .025,.03,.035,.04,.045,.05,.055,.06,.07,.08,.09,.1,.125,.15,.175,        !/*mgt       317*/
     & .2,.25,.3,.35,.4,.45,.5,.55,.6,.7,.8,.9,.9999,(1.,k=1,40) /)             !/*mgt       318*/
!                                                                               !/*mgt       319*/
!        bins for detector and dxtran diagnostics.                              !/*mgt       320*/
      real(kindr) :: talb(8,2) = reshape (                                      !/*mgt       321*/
     & (/ .1,1.,2.,5.,10.,100.,1000.,1e38,                                      !/*mgt       322*/
     & 1.,2.,5.,10.,20.,50.,100.,1000. /), (/ 8,2 /) )                          !/*mgt       323*/
!                                                                               !/*mgt       324*/
!        form factor constants for photon scattering.                           !/*mgt       325*/
      real(kindr) :: vco(mcoh) = (/                                             !/*mgt       326*/
     & 0.,.01,.02,.03,.04,.05,.06,.08,.1,.12,.15,.18,.2,.25,                    !/*mgt       327*/
     & .3,.35,.4,.45,.5,.55,.6,.7,.8,.9,1.,1.1,1.2,1.3,1.4,1.5,1.6,1.7,         !/*mgt       328*/
     & 1.8,1.9,2.,2.2,2.4,2.6,2.8,3.,3.2,3.4,3.6,3.8,4.,4.2,4.4,4.6,            !/*mgt       329*/
     & 4.8,5.,5.2,5.4,5.6,5.8,6. /)                                             !/*mgt       330*/
      real(kindr) :: vic(minc) = (/                                             !/*mgt       331*/
     & 0.,.005,.01,.05,.1,.15,.2,.3,.4,.5,.6,.7,.8,.9,1.,1.5,                   !/*mgt       332*/
     & 2.,3.,4.,5.,8. /)                                                        !/*mgt       333*/
      real(kindr) :: wco(mcoh) = (/                                             !/*mgt       334*/
     & 0.,.0001,.0004,.0009,.0016,.0025,.0036,.0064,.01,                        !/*mgt       335*/
     & .0144,.0225,.0324,.04,.0625,.09,.1225,.16,.2025,.25,.3025,.36,           !/*mgt       336*/
     & .49,.64,.81,1.,1.21,1.44,1.69,1.96,2.25,2.56,2.89,3.24,3.61,             !/*mgt       337*/
     & 4.,4.84,5.76,6.76,7.84,9.,10.24,11.56,12.96,14.44,16.,17.64,             !/*mgt       338*/
     & 19.36,21.16,23.04,25.,27.04,29.16,31.36,33.64,36. /)                     !/*mgt       339*/
!                                                                               !/*mgt       340*/
!***********************************************************************        !/*mgt       341*/
!        character variables and arrays.                                        !/*mgt       342*/
!                                                                               !/*mgt       343*/
      character*80 :: aid                                                       !/*mgt       344*/
      character*80 :: aid1                                                      !/*mgt       345*/
      character*80 :: aids                                                      !/*mgt       346*/
      character*10 :: chcd = ' '                                                !/*mgt       347*/
      character*80 :: exms = ' '                                                !/*mgt       348*/
      character*80 :: hdpath                                                    !/*mgt       349*/
      character*80 :: hdpth                                                     !/*mgt       350*/
      character*69 :: hmes                                                      !/*mgt       352*/
      character*8 :: hovr = kod                                                 !/*mgt       353*/
      character*31 :: hsub                                                      !/*mgt       354*/
      character*10 :: htn = 'cdytpmghue'                                        !/*mgt       355*/
      character*9 :: ibin = 'fdusmcet&'                                         !/*mgt       356*/
      character*19 :: idtm                                                      !/*mgt       357*/
      character*19 :: idtms                                                     !/*mgt       358*/
      character*80 :: klin                                                      !/*mgt       359*/
      character*8 :: kods                                                       !/*mgt       360*/
      character*28 :: lods                                                      !/*mgt       361*/
      character*19 :: probid = ' '                                              !/*mgt       362*/
      character*19 :: probs                                                     !/*mgt       363*/
      character*5 :: vers                                                       !/*mgt       364*/
!                                                                               !/*mgt       365*/
      character*1 :: avrm(6) = (/ 'x','y','z','r','z','t' /)                    !/*mgt       366*/
      character*7 :: hcs(2) = (/ 'cell   ','surface' /)                         !/*mgt       367*/
      character*11 :: hfu(2) =  (/ 'formatted  ','unformatted' /)               !/*mgt       368*/
      character*10 :: hsd(2) =  (/ 'sequential','direct    ' /)                 !/*mgt       378*/
      character*11 :: ufil(3,6) =                                               !/*mgt       379*/
     & reshape ( (/ (' ',k=1,18) /), (/ 3,6 /) )                                !/*mgt       380*/
!                                                                               !/*mgt       381*/
!        names of source variables.                                             !/*mgt       382*/
      character*3 :: hbln(maxv,4) = reshape (                                   !/*mgt       383*/
     & (/ 'cel','sur','erg','tme','dir','pos','x  ','y  ','z  ','rad',          !/*mgt       384*/
     &    'ext','axs','vec','ccc','nrm','ara','wgt','eff','par','tr ',          !/*mgt       385*/
     &    'cel','old','new','col','tr ','poa','   ','   ','   ','bcw',          !/*mgt       386*/
     &    'ext','axs','   ','   ','   ','   ','wgt','psc','par','   ',          !/*mgt       387*/
     &    ('   ',k=1,40) /),                                                    !/*mgt       388*/
     & (/ maxv,4 /) )                                                           !/*mgt       389*/
      character*3 :: hblw(maxw) = (/ 'sym','pty','cel','   ' /)                 !/*mgt       390*/
!                                                                               !/*mgt       391*/
!        names of ft-card special treatments.                                   !/*mgt       392*/
      character*3 :: hft(mkft) = (/                                             !/*mgt       393*/
     & 'frv','geb','tmc','inc','icd','scx','scd','elc','ptt' /)                 !/*mgt       394*/
!                                                                               !/*mgt       395*/
!        names of the kinds of tally bins.                                      !/*mgt       396*/
      character*8 :: ilbl(9) = (/                                               !/*mgt       397*/
     & '        ','flag/dir','    user',' segment','    mult',                  !/*mgt       398*/
     & '  cosine','  energy','    time','        ' /)                           !/*mgt       399*/
!                                                                               !/*mgt       400*/
!        surface type symbols.                                                  !/*mgt       401*/
      character*3 :: ksf(39) = (/                                               !/*mgt       402*/
     & 'p  ','px ','py ','pz ','so ','s  ','sx ','sy ','sz ',                   !/*mgt       403*/
     & 'c/x','c/y','c/z','cx ','cy ','cz ','k/x','k/y','k/z','kx ',             !/*mgt       404*/
     & 'ky ','kz ','sq ','gq ','tx ','ty ','tz ','x  ','y  ','z  ',             !/*mgt       405*/
     & 'box','rpp','sph','rcc','rec','ell','trc','wed','arb','rhp' /)           !/*mgt       406*/
!                                                                               !/*mgt       407*/
!        file names.                                                            !/*mgt       408*/
      character*8 :: msub(ndef) = (/                                            !/*mgt       409*/
     & 'inp     ','outp    ','runtpe  ','mctal   ','wssa    ',                  !/*mgt       410*/
     & 'ptrac   ','comout  ','srctp   ','wwout   ','wwone   ',                  !/*mgt       411*/
     & 'plotm   ','rssa    ','xsdir   ','com     ','wwinp   ',                  !/*mgt       412*/
     & 'mdata   ','dumn1   ','dumn2   ', ('        ',k=1,14) /)                 !/*mgt       413*/
      character*8 :: iname = ' '                                                !/*mgt       414*/
!                                                                               !/*mgt       415*/
!        partial formats for the termination messages.                          !/*mgt       416*/
      character*57 :: rfq(15) = (/                                              !/*mgt       417*/
     & '35h     seconds before job time limit.                   ',             !/*mgt       418*/
     & '44hwhen           particle histories were done.          ',             !/*mgt       419*/
     & '31hbecause     particles got lost.                       ',             !/*mgt       420*/
     & '17hby tty interrupt.                                     ',             !/*mgt       421*/
     & '47hwhen it had used      minutes of computer time.       ',             !/*mgt       422*/
     & '18hby sense switch 1.                                    ',             !/*mgt       423*/
     & '34hwhen       kcode cycles were done.                    ',             !/*mgt       424*/
     & '54hwhen            events were written to the ptrac file.',             !/*mgt       425*/
     & '23hbecause of bad trouble.                               ',             !/*mgt       426*/
     & '36hat end of surface-source input file.                  ',             !/*mgt       427*/
     & '24hbecause of fatal errors.                              ',             !/*mgt       428*/
     & '                                                         ',             !/*mgt       429*/
     & '                                                         ',             !/*mgt       430*/
     & '                                                         ',             !/*mgt       431*/
     & '                                                         ' /)           !/*mgt       432*/
!                                                                               !/*mgt       433*/
!        labels for the printing of (PWB) weight balance tables.                !/*mgt       434*/
!                                                                               !/*mgt       435*/
!        PWB: External events, particle-independent.                            !/*mgt       436*/
      character*17 :: label_pwb_external                                        !/*mgt       437*/
     & (itm_pwb_external_begin:itm_pwb_external_end)                            !/*mgt       438*/
      data label_pwb_external (ipwb_external_entering)                          !/*mgt       439*/
     &     /'entering'/                                                         !/*mgt       440*/
      data label_pwb_external (ipwb_external_source)                            !/*mgt       441*/
     &     /'source'/                                                           !/*mgt       442*/
      data label_pwb_external (ipwb_external_energy_cutoff)                     !/*mgt       443*/
     &     /'energy cutoff'/                                                    !/*mgt       444*/
      data label_pwb_external (ipwb_external_time_cutoff)                       !/*mgt       445*/
     &     /'time cutoff'/                                                      !/*mgt       446*/
      data label_pwb_external (ipwb_external_exiting)                           !/*mgt       447*/
     &     /'exiting'/                                                          !/*mgt       448*/
!                                                                               !/*mgt       449*/
!        PWB: Variance-reduction events, particle-independent.                  !/*mgt       450*/
      character*17 :: label_pwb_variance                                        !/*mgt       451*/
     & (itm_pwb_variance_begin:itm_pwb_variance_end)                            !/*mgt       452*/
      data label_pwb_variance (ipwb_variance_weight_window)                     !/*mgt       453*/
     &     /'weight window'/                                                    !/*mgt       454*/
      data label_pwb_variance (ipwb_variance_cell_importance)                   !/*mgt       455*/
     &     /'cell importance'/                                                  !/*mgt       456*/
      data label_pwb_variance (ipwb_variance_weight_cutoff)                     !/*mgt       457*/
     &     /'weight cutoff'/                                                    !/*mgt       458*/
      data label_pwb_variance (ipwb_variance_energy_import)                     !/*mgt       459*/
     &     /'energy importance'/                                                !/*mgt       460*/
      data label_pwb_variance (ipwb_variance_dxtran)                            !/*mgt       461*/
     &     /'dxtran'/                                                           !/*mgt       462*/
      data label_pwb_variance (ipwb_variance_forced_coll)                       !/*mgt       463*/
     &     /'forced collisions'/                                                !/*mgt       464*/
      data label_pwb_variance (ipwb_variance_exp_transform)                     !/*mgt       465*/
     &     /'exp. transform'/                                                   !/*mgt       466*/
!                                                                               !/*mgt       467*/
!        PWB: Physical events, specific to neutrons.                            !/*mgt       468*/
      character*17 :: label_pwb_physical (itm_pwb_physical_begin:               !/*mgt       469*/
     &  itm_pwb_physical_end,number_particle_categories)                        !/*mgt       470*/
      data label_pwb_physical (ipwb_neutron_capture,                            !/*mgt       471*/
     &     id_category_neutron) /'capture'/                                     !/*mgt       472*/
      data label_pwb_physical (ipwb_neutron_n_xn,                               !/*mgt       473*/
     &     id_category_neutron) /'(n,xn)'/                                      !/*mgt       474*/
      data label_pwb_physical (ipwb_neutron_loss_to_n_xn,                       !/*mgt       475*/
     &     id_category_neutron) /'loss to (n,xn)'/                              !/*mgt       476*/
      data label_pwb_physical (ipwb_neutron_fission,                            !/*mgt       477*/
     &     id_category_neutron) /'fission'/                                     !/*mgt       478*/
      data label_pwb_physical (ipwb_neutron_loss_to_fission,                    !/*mgt       479*/
     &     id_category_neutron) /'loss to fission'/                             !/*mgt       480*/
      data label_pwb_physical (ipwb_neutron_photonuclear,                       !/*mgt       481*/
     &     id_category_neutron) /'photonuclear'/                                !/*mgt       482*/
      data label_pwb_physical (ipwb_neutron_nuc_interaction,                    !/*mgt       483*/
     &     id_category_neutron) /'nucl. interaction'/                           !/*mgt       484*/
      data label_pwb_physical (ipwb_neutron_tabular_bound,                      !/*mgt       485*/
     &     id_category_neutron) /'tabular boundary'/                            !/*mgt       486*/
      data label_pwb_physical (ipwb_neutron_particle_decay,                     !/*mgt       487*/
     &     id_category_neutron) /'particle decay'/                              !/*mgt       488*/
      data label_pwb_physical (ipwb_neutron_tabular_sampl,                      !/*mgt       489*/
     &     id_category_neutron) /'tabular sampling'/                            !/*mgt       490*/
      data label_pwb_physical (ipwb_neutron_unused_23,                          !/*mgt       491*/
     &     id_category_neutron) /' '/                                           !/*mgt       492*/
      data label_pwb_physical (ipwb_neutron_unused_24,                          !/*mgt       493*/
     &     id_category_neutron) /' '/                                           !/*mgt       494*/
      data label_pwb_physical (ipwb_neutron_unused_25,                          !/*mgt       499*/
     &     id_category_neutron) /' '/                                           !/*mgt       500*/
!                                                                               !/*mgt       502*/
!        PWB: Physical events, specific to photons.                             !/*mgt       503*/
      data label_pwb_physical (ipwb_photon_from_neutrons,                       !/*mgt       504*/
     &     id_category_photon) /'from neutrons'/                                !/*mgt       505*/
      data label_pwb_physical (ipwb_photon_bremsstrahlung,                      !/*mgt       506*/
     &     id_category_photon) /'bremsstrahlung'/                               !/*mgt       507*/
      data label_pwb_physical (ipwb_photon_capture,                             !/*mgt       508*/
     &     id_category_photon) /'capture'/                                      !/*mgt       509*/
      data label_pwb_physical (ipwb_photon_p_annihilation,                      !/*mgt       510*/
     &     id_category_photon) /'p-annihilation'/                               !/*mgt       511*/
      data label_pwb_physical (ipwb_photon_pair_production,                     !/*mgt       512*/
     &     id_category_photon) /'pair production'/                              !/*mgt       513*/
      data label_pwb_physical (ipwb_photon_photonuclear,                        !/*mgt       514*/
     &     id_category_photon) /'photonuclear'/                                 !/*mgt       515*/
      data label_pwb_physical (ipwb_photon_photonuc_abs,                        !/*mgt       516*/
     &     id_category_photon) /'photonuclear abs'/                             !/*mgt       517*/
      data label_pwb_physical (ipwb_photon_electron_x_rays,                     !/*mgt       518*/
     &     id_category_photon) /'electron x-rays'/                              !/*mgt       519*/
      data label_pwb_physical (ipwb_photon_flourescence,                        !/*mgt       520*/
     &     id_category_photon) /'flourescence'/                                 !/*mgt       521*/
      data label_pwb_physical (ipwb_photon_particle_decay,                      !/*mgt       522*/
     &     id_category_photon) /'particle decay'/                               !/*mgt       523*/
      data label_pwb_physical (ipwb_photon_nuc_interaction,                     !/*mgt       524*/
     &     id_category_photon) /'nucl. interaction'/                            !/*mgt       525*/
      data label_pwb_physical (ipwb_photon_tabular_sampl,                       !/*mgt       526*/
     &     id_category_photon) /'tabular sampling'/                             !/*mgt       527*/
      data label_pwb_physical (ipwb_photon_unused_25,                           !/*mgt       528*/
     &     id_category_photon) /' '/                                            !/*mgt       529*/
!                                                                               !/*mgt       530*/
!        PWB: Physical events, specific to electrons.                           !/*mgt       531*/
      data label_pwb_physical (ipwb_electron_pair_production,                   !/*mgt       532*/
     &     id_category_electron) /'pair production'/                            !/*mgt       533*/
      data label_pwb_physical (ipwb_electron_compton_recoil,                    !/*mgt       534*/
     &     id_category_electron) /'compton recoil'/                             !/*mgt       535*/
      data label_pwb_physical (ipwb_electron_photo_electric,                    !/*mgt       536*/
     &     id_category_electron) /'photo-electric'/                             !/*mgt       537*/
      data label_pwb_physical (ipwb_electron_photon_auger,                      !/*mgt       538*/
     &     id_category_electron) /'photon auger'/                               !/*mgt       539*/
      data label_pwb_physical (ipwb_electron_electron_auger,                    !/*mgt       540*/
     &     id_category_electron) /'electron auger'/                             !/*mgt       541*/
      data label_pwb_physical (ipwb_electron_knock_on,                          !/*mgt       542*/
     &     id_category_electron) /'knock-on'/                                   !/*mgt       543*/
      data label_pwb_physical (ipwb_electron_nuc_interaction,                   !/*mgt       544*/
     &     id_category_electron) /'nucl. interaction'/                          !/*mgt       545*/
      data label_pwb_physical (ipwb_electron_particle_decay,                    !/*mgt       546*/
     &     id_category_electron) /'particle decay'/                             !/*mgt       547*/
      data label_pwb_physical (ipwb_electron_unused_21,                         !/*mgt       548*/
     &     id_category_electron) /' '/                                          !/*mgt       549*/
      data label_pwb_physical (ipwb_electron_unused_22,                         !/*mgt       550*/
     &     id_category_electron) /' '/                                          !/*mgt       551*/
      data label_pwb_physical (ipwb_electron_unused_23,                         !/*mgt       552*/
     &     id_category_electron) /' '/                                          !/*mgt       553*/
      data label_pwb_physical (ipwb_electron_unused_24,                         !/*mgt       554*/
     &     id_category_electron) /' '/                                          !/*mgt       555*/
      data label_pwb_physical (ipwb_electron_unused_25,                         !/*mgt       556*/
     &     id_category_electron) /' '/                                          !/*mgt       557*/
!                                                                               !/*mgt       558*/
!        PWB: Physical events, for generic neutral particles.                   !/*mgt       559*/
      data label_pwb_physical (ipwb_gen_neu_unused_13,                          !/*mgt       560*/
     &     id_category_gen_neu) /' '/                                           !/*mgt       561*/
      data label_pwb_physical (ipwb_gen_neu_nuc_interaction,                    !/*mgt       562*/
     &     id_category_gen_neu) /'nucl. interaction'/                           !/*mgt       563*/
      data label_pwb_physical (ipwb_gen_neu_unused_15,                          !/*mgt       564*/
     &     id_category_gen_neu) /' '/                                           !/*mgt       565*/
      data label_pwb_physical (ipwb_gen_neu_particle_decay,                     !/*mgt       566*/
     &     id_category_gen_neu) /'particle decay'/                              !/*mgt       567*/
      data label_pwb_physical (ipwb_gen_neu_photonuclear,                       !/*mgt       568*/
     &     id_category_gen_neu) /'photonuclear'/                                !/*mgt       569*/
      data label_pwb_physical (ipwb_gen_neu_tabular_sampl,                      !/*mgt       570*/
     &     id_category_gen_neu) /'tabular sampling'/                            !/*mgt       571*/
      data label_pwb_physical (ipwb_gen_neu_gamma_xgen_neu,                     !/*mgt       572*/
     &     id_category_gen_neu) /'gamma,xgen_neu'/                              !/*mgt       573*/
      data label_pwb_physical (ipwb_gen_neu_unused_20,                          !/*mgt       574*/
     &     id_category_gen_neu) /' '/                                           !/*mgt       575*/
      data label_pwb_physical (ipwb_gen_neu_unused_21,                          !/*mgt       576*/
     &     id_category_gen_neu) /' '/                                           !/*mgt       577*/
      data label_pwb_physical (ipwb_gen_neu_unused_22,                          !/*mgt       578*/
     &     id_category_gen_neu) /' '/                                           !/*mgt       579*/
      data label_pwb_physical (ipwb_gen_neu_unused_23,                          !/*mgt       580*/
     &     id_category_gen_neu) /' '/                                           !/*mgt       581*/
      data label_pwb_physical (ipwb_gen_neu_unused_24,                          !/*mgt       582*/
     &     id_category_gen_neu) /' '/                                           !/*mgt       583*/
      data label_pwb_physical (ipwb_gen_neu_unused_25,                          !/*mgt       584*/
     &     id_category_gen_neu) /' '/                                           !/*mgt       585*/
!                                                                               !/*mgt       586*/
!        PWB: Physical events, for generic charged particles.                   !/*mgt       587*/
      data label_pwb_physical (ipwb_gen_chg_unused_13,                          !/*mgt       588*/
     &     id_category_gen_chg) /' '/                                           !/*mgt       589*/
      data label_pwb_physical (ipwb_gen_chg_nuc_interaction,                    !/*mgt       590*/
     &     id_category_gen_chg) /'nucl. interaction'/                           !/*mgt       591*/
      data label_pwb_physical (ipwb_gen_chg_unused_15,                          !/*mgt       592*/
     &     id_category_gen_chg) /' '/                                           !/*mgt       593*/
      data label_pwb_physical (ipwb_gen_chg_particle_decay,                     !/*mgt       594*/
     &     id_category_gen_chg) /'particle decay'/                              !/*mgt       595*/
      data label_pwb_physical (ipwb_gen_chg_photonuclear,                       !/*mgt       596*/
     &     id_category_gen_chg) /'photonuclear'/                                !/*mgt       597*/
      data label_pwb_physical (ipwb_gen_chg_tabular_sampl,                      !/*mgt       598*/
     &     id_category_gen_chg) /'tabular sampling'/                            !/*mgt       599*/
      data label_pwb_physical (ipwb_gen_chg_capture,                            !/*mgt       600*/
     &     id_category_gen_chg) /'capture'/                                     !/*mgt       601*/
      data label_pwb_physical (ipwb_gen_chg_gamma_xgen_chg,                     !/*mgt       602*/
     &     id_category_gen_chg) /'gamma,xgen_chg'/                              !/*mgt       603*/
      data label_pwb_physical (ipwb_gen_chg_unused_21,                          !/*mgt       604*/
     &     id_category_gen_chg) /' '/                                           !/*mgt       605*/
      data label_pwb_physical (ipwb_gen_chg_unused_22,                          !/*mgt       606*/
     &     id_category_gen_chg) /' '/                                           !/*mgt       607*/
      data label_pwb_physical (ipwb_gen_chg_unused_23,                          !/*mgt       608*/
     &     id_category_gen_chg) /' '/                                           !/*mgt       609*/
      data label_pwb_physical (ipwb_gen_chg_unused_24,                          !/*mgt       610*/
     &     id_category_gen_chg) /' '/                                           !/*mgt       611*/
      data label_pwb_physical (ipwb_gen_chg_unused_25,                          !/*mgt       612*/
     &     id_category_gen_chg) /' '/                                           !/*mgt       613*/
!                                                                               !/*mgt       614*/
!        PAX: problem summary table.                                            !/*mgt       615*/
      character*17 :: label_pax_gain_all_pls                                    !/*mgt       616*/
     & (itm_pax_all_pls_begin:itm_pax_all_pls_end)                              !/*mgt       617*/
!                                                                               !/*mgt       618*/
!        PAX: Track; weight; weight*energy gain. All particle types.            !/*mgt       619*/
      data label_pax_gain_all_pls(igain_all_pls_source)                         !/*mgt       620*/
     &     /'source'/                                                           !/*mgt       621*/
      data label_pax_gain_all_pls(igain_all_pls_nucl_interaction)               !/*mgt       622*/
     &     /'nucl. interaction'/                                                !/*mgt       623*/
      data label_pax_gain_all_pls(igain_all_pls_particle_decay)                 !/*mgt       624*/
     &     /'particle decay'/                                                   !/*mgt       625*/
      data label_pax_gain_all_pls(igain_all_pls_weight_window)                  !/*mgt       626*/
     &     /'weight window'/                                                    !/*mgt       627*/
      data label_pax_gain_all_pls(igain_all_pls_cell_importance)                !/*mgt       628*/
     &     /'cell importance'/                                                  !/*mgt       629*/
      data label_pax_gain_all_pls(igain_all_pls_weight_cutoff)                  !/*mgt       630*/
     &     /'weight cutoff'/                                                    !/*mgt       631*/
      data label_pax_gain_all_pls(igain_all_pls_energy_importance)              !/*mgt       632*/
     &     /'energy importance'/                                                !/*mgt       633*/
      data label_pax_gain_all_pls(igain_all_pls_dxtran)                         !/*mgt       634*/
     &     /'dxtran'/                                                           !/*mgt       635*/
      data label_pax_gain_all_pls(igain_all_pls_forced_collisions)              !/*mgt       636*/
     &     /'forced collisions'/                                                !/*mgt       637*/
      data label_pax_gain_all_pls(igain_all_pls_exp_transform)                  !/*mgt       638*/
     &     /'exp. transform'/                                                   !/*mgt       639*/
!                                                                               !/*mgt       640*/
!        PAX: Track; weight; weight*energy loss. All particle types.            !/*mgt       641*/
      character*17 :: label_pax_loss_all_pls                                    !/*mgt       642*/
     & (itm_pax_all_pls_begin:itm_pax_all_pls_end)                              !/*mgt       643*/
      data label_pax_loss_all_pls(loss_all_pls_escape)                          !/*mgt       644*/
     &     /'escape'/                                                           !/*mgt       645*/
      data label_pax_loss_all_pls(loss_all_pls_energy_cutoff)                   !/*mgt       646*/
     &     /'energy cutoff'/                                                    !/*mgt       647*/
      data label_pax_loss_all_pls(loss_all_pls_time_cutoff)                     !/*mgt       648*/
     &     /'time cutoff'/                                                      !/*mgt       649*/
      data label_pax_loss_all_pls(loss_all_pls_weight_window)                   !/*mgt       650*/
     &     /'weight window'/                                                    !/*mgt       651*/
      data label_pax_loss_all_pls(loss_all_pls_cell_importance)                 !/*mgt       652*/
     &     /'cell importance'/                                                  !/*mgt       653*/
      data label_pax_loss_all_pls(loss_all_pls_weight_cutoff)                   !/*mgt       654*/
     &     /'weight cutoff'/                                                    !/*mgt       655*/
      data label_pax_loss_all_pls(loss_all_pls_energy_importance)               !/*mgt       656*/
     &     /'energy importance'/                                                !/*mgt       657*/
      data label_pax_loss_all_pls(loss_all_pls_dxtran)                          !/*mgt       658*/
     &     /'dxtran'/                                                           !/*mgt       659*/
      data label_pax_loss_all_pls(loss_all_pls_forced_collisions)               !/*mgt       660*/
     &     /'forced collisions'/                                                !/*mgt       661*/
      data label_pax_loss_all_pls(loss_all_pls_exp_transform)                   !/*mgt       662*/
     &     /'exp. transform'/                                                   !/*mgt       663*/
!                                                                               !/*mgt       664*/
!        PAX: Track; weight; weight*energy gain. Specific to neutrons.          !/*mgt       665*/
      character*17 :: label_pax_gain_specific (itm_pax_specific_begin:          !/*mgt       666*/
     &  itm_pax_specific_end, number_particle_categories)                       !/*mgt       667*/
      data label_pax_gain_specific(igain_neutron_upscattering,                  !/*mgt       668*/
     &     id_category_neutron) /'upscattering'/                                !/*mgt       669*/
      data label_pax_gain_specific(igain_neutron_photonuclear,                  !/*mgt       670*/
     &     id_category_neutron) /'photonuclear'/                                !/*mgt       671*/
      data label_pax_gain_specific(igain_neutron_n_xn,                          !/*mgt       672*/
     &     id_category_neutron) /'(n,xn)'/                                      !/*mgt       673*/
      data label_pax_gain_specific(igain_neutron_prompt_fission,                !/*mgt       674*/
     &     id_category_neutron) /'prompt fission'/                              !/*mgt       675*/
      data label_pax_gain_specific(igain_neutron_delayed_fission,               !/*mgt       676*/
     &     id_category_neutron) /'delayed fission'/                             !/*mgt       677*/
      data label_pax_gain_specific(igain_neutron_unused_16,                     !/*mgt       678*/
     &     id_category_neutron) /' '/                                           !/*mgt       679*/
      data label_pax_gain_specific(igain_neutron_tabular_boundary,              !/*mgt       680*/
     &     id_category_neutron) /'tabular boundary'/                            !/*mgt       681*/
      data label_pax_gain_specific(igain_neutron_tabular_sampling,              !/*mgt       682*/
     &     id_category_neutron) /'tabular sampling'/                            !/*mgt       683*/
      data label_pax_gain_specific(igain_neutron_unused_19,                     !/*mgt       684*/
     &     id_category_neutron) /' '/                                           !/*mgt       685*/
      data label_pax_gain_specific(igain_neutron_unused_20,                     !/*mgt       686*/
     &     id_category_neutron) /' '/                                           !/*mgt       687*/
      data label_pax_gain_specific(igain_neutron_unused_21,                     !/*mgt       692*/
     &     id_category_neutron) /' '/                                           !/*mgt       693*/
!                                                                               !/*mgt       695*/
!        PAX: Track; weight; weight*energy loss. Specific to neutrons.          !/*mgt       696*/
      character*17 :: label_pax_loss_specific (itm_pax_specific_begin:          !/*mgt       697*/
     &  itm_pax_specific_end, number_particle_categories)                       !/*mgt       698*/
      data label_pax_loss_specific(loss_neutron_downscattering,                 !/*mgt       699*/
     &     id_category_neutron) /'downscattering'/                              !/*mgt       700*/
      data label_pax_loss_specific(loss_neutron_capture,                        !/*mgt       701*/
     &     id_category_neutron) /'capture'/                                     !/*mgt       702*/
      data label_pax_loss_specific(loss_neutron_loss_to_n_xn,                   !/*mgt       703*/
     &     id_category_neutron) /'loss to (n,xn)'/                              !/*mgt       704*/
      data label_pax_loss_specific(loss_neutron_loss_to_fission,                !/*mgt       705*/
     &     id_category_neutron) /'loss to fission'/                             !/*mgt       706*/
      data label_pax_loss_specific(loss_neutron_nucl_interaction,               !/*mgt       707*/
     &     id_category_neutron) /'nucl. interaction'/                           !/*mgt       708*/
      data label_pax_loss_specific(loss_neutron_particle_decay,                 !/*mgt       709*/
     &     id_category_neutron) /'particle decay'/                              !/*mgt       710*/
      data label_pax_loss_specific(loss_neutron_tabular_boundary,               !/*mgt       711*/
     &     id_category_neutron) /'tabular boundary'/                            !/*mgt       712*/
      data label_pax_loss_specific(loss_neutron_unused_18,                      !/*mgt       713*/
     &     id_category_neutron) /' '/                                           !/*mgt       714*/
      data label_pax_loss_specific(loss_neutron_unused_19,                      !/*mgt       715*/
     &     id_category_neutron) /' '/                                           !/*mgt       716*/
      data label_pax_loss_specific(loss_neutron_unused_20,                      !/*mgt       717*/
     &     id_category_neutron) /' '/                                           !/*mgt       718*/
      data label_pax_loss_specific(loss_neutron_unused_21,                      !/*mgt       723*/
     &     id_category_neutron) /' '/                                           !/*mgt       724*/
!                                                                               !/*mgt       726*/
!        PAX: Track; weight; weight*energy gain. Specific to photons.           !/*mgt       727*/
      data label_pax_gain_specific(igain_photon_from_neutrons,                  !/*mgt       728*/
     &     id_category_photon) /'from neutrons'/                                !/*mgt       729*/
      data label_pax_gain_specific(igain_photon_bremsstrahlung,                 !/*mgt       730*/
     &     id_category_photon) /'bremsstrahlung'/                               !/*mgt       731*/
      data label_pax_gain_specific(igain_photon_p_annihilation,                 !/*mgt       732*/
     &     id_category_photon) /'p-annihilation'/                               !/*mgt       733*/
      data label_pax_gain_specific(igain_photon_photonuclear,                   !/*mgt       734*/
     &     id_category_photon) /'photonuclear'/                                 !/*mgt       735*/
      data label_pax_gain_specific(igain_photon_electron_x_rays,                !/*mgt       736*/
     &     id_category_photon) /'electron x-rays'/                              !/*mgt       737*/
      data label_pax_gain_specific(igain_photon_1st_fluorescence,               !/*mgt       738*/
     &     id_category_photon) /'1st fluorescence'/                             !/*mgt       739*/
      data label_pax_gain_specific(igain_photon_2nd_fluorescence,               !/*mgt       740*/
     &     id_category_photon) /'2nd fluorescence'/                             !/*mgt       741*/
      data label_pax_gain_specific(igain_photon_gamma_xgamma,                   !/*mgt       742*/
     &     id_category_photon) /'(gamma,xgamma)'/                               !/*mgt       743*/
      data label_pax_gain_specific(igain_photon_tabular_sampling,               !/*mgt       744*/
     &     id_category_photon) /'tabular sampling'/                             !/*mgt       745*/
      data label_pax_gain_specific(igain_photon_unused_20,                      !/*mgt       746*/
     &     id_category_photon) /' '/                                            !/*mgt       747*/
      data label_pax_gain_specific(igain_photon_unused_21,                      !/*mgt       748*/
     &     id_category_photon) /' '/                                            !/*mgt       749*/
!                                                                               !/*mgt       750*/
!        PAX: Track; weight; weight*energy loss. Specific to photons.           !/*mgt       751*/
      data label_pax_loss_specific(loss_photon_compton_scatter,                 !/*mgt       752*/
     &     id_category_photon) /'compton scatter'/                              !/*mgt       753*/
      data label_pax_loss_specific(loss_photon_capture,                         !/*mgt       754*/
     &     id_category_photon) /'capture'/                                      !/*mgt       755*/
      data label_pax_loss_specific(loss_photon_pair_production,                 !/*mgt       756*/
     &     id_category_photon) /'pair production'/                              !/*mgt       757*/
      data label_pax_loss_specific(loss_photon_photonuclear,                    !/*mgt       758*/
     &     id_category_photon) /'photonuclear abs'/                             !/*mgt       759*/
      data label_pax_loss_specific(loss_photon_unused_15,                       !/*mgt       760*/
     &     id_category_photon) /' '/                                            !/*mgt       761*/
      data label_pax_loss_specific(loss_photon_unused_16,                       !/*mgt       762*/
     &     id_category_photon) /' '/                                            !/*mgt       763*/
      data label_pax_loss_specific(loss_photon_unused_17,                       !/*mgt       764*/
     &     id_category_photon) /' '/                                            !/*mgt       765*/
      data label_pax_loss_specific(loss_photon_unused_18,                       !/*mgt       766*/
     &     id_category_photon) /' '/                                            !/*mgt       767*/
      data label_pax_loss_specific(loss_photon_unused_19,                       !/*mgt       768*/
     &     id_category_photon) /' '/                                            !/*mgt       769*/
      data label_pax_loss_specific(loss_photon_unused_20,                       !/*mgt       770*/
     &     id_category_photon) /' '/                                            !/*mgt       771*/
      data label_pax_loss_specific(loss_photon_unused_21,                       !/*mgt       772*/
     &     id_category_photon) /' '/                                            !/*mgt       773*/
!                                                                               !/*mgt       774*/
!        PAX: Track; weight; weight*energy gain. Specific to electrons.         !/*mgt       775*/
      data label_pax_gain_specific(igain_electron_pair_production,              !/*mgt       776*/
     &     id_category_electron) /'pair production'/                            !/*mgt       777*/
      data label_pax_gain_specific(igain_electron_compton_recoil,               !/*mgt       778*/
     &     id_category_electron) /'compton recoil'/                             !/*mgt       779*/
      data label_pax_gain_specific(igain_electron_photo_electric,               !/*mgt       780*/
     &     id_category_electron) /'photo-electric'/                             !/*mgt       781*/
      data label_pax_gain_specific(igain_electron_photon_auger,                 !/*mgt       782*/
     &     id_category_electron) /'photon auger'/                               !/*mgt       783*/
      data label_pax_gain_specific(igain_electron_electron_auger,               !/*mgt       784*/
     &     id_category_electron) /'electron auger'/                             !/*mgt       785*/
      data label_pax_gain_specific(igain_electron_knock_on,                     !/*mgt       786*/
     &     id_category_electron) /'knock-on'/                                   !/*mgt       787*/
      data label_pax_gain_specific(igain_electron_gamma_xelectron,              !/*mgt       788*/
     &     id_category_electron) /'(gamma,xelectron)'/                          !/*mgt       789*/
      data label_pax_gain_specific(igain_electron_unused_18,                    !/*mgt       790*/
     &     id_category_electron) /' '/                                          !/*mgt       791*/
      data label_pax_gain_specific(igain_electron_unused_19,                    !/*mgt       792*/
     &     id_category_electron) /' '/                                          !/*mgt       793*/
      data label_pax_gain_specific(igain_electron_unused_20,                    !/*mgt       794*/
     &     id_category_electron) /' '/                                          !/*mgt       795*/
      data label_pax_gain_specific(igain_electron_unused_21,                    !/*mgt       796*/
     &     id_category_electron) /' '/                                          !/*mgt       797*/
!                                                                               !/*mgt       798*/
!        PAX: Track; weight; weight*energy loss. Specific to electrons.         !/*mgt       799*/
      data label_pax_loss_specific(loss_electron_scattering,                    !/*mgt       800*/
     &     id_category_electron) /'scattering'/                                 !/*mgt       801*/
      data label_pax_loss_specific(loss_electron_bremsstrahlung,                !/*mgt       802*/
     &     id_category_electron) /'bremsstrahlung'/                             !/*mgt       803*/
      data label_pax_loss_specific(loss_electron_unused_13,                     !/*mgt       804*/
     &     id_category_electron) /' '/                                          !/*mgt       805*/
      data label_pax_loss_specific(loss_electron_unused_14,                     !/*mgt       806*/
     &     id_category_electron) /' '/                                          !/*mgt       807*/
      data label_pax_loss_specific(loss_electron_unused_15,                     !/*mgt       808*/
     &     id_category_electron) /' '/                                          !/*mgt       809*/
      data label_pax_loss_specific(loss_electron_interact_or_decay,             !/*mgt       810*/
     &     id_category_electron) /'interact or decay'/                          !/*mgt       811*/
      data label_pax_loss_specific(loss_electron_unused_17,                     !/*mgt       812*/
     &     id_category_electron) /' '/                                          !/*mgt       813*/
      data label_pax_loss_specific(loss_electron_unused_18,                     !/*mgt       814*/
     &     id_category_electron) /' '/                                          !/*mgt       815*/
      data label_pax_loss_specific(loss_electron_unused_19,                     !/*mgt       816*/
     &     id_category_electron) /' '/                                          !/*mgt       817*/
      data label_pax_loss_specific(loss_electron_unused_20,                     !/*mgt       818*/
     &     id_category_electron) /' '/                                          !/*mgt       819*/
      data label_pax_loss_specific(loss_electron_unused_21,                     !/*mgt       820*/
     &     id_category_electron) /' '/                                          !/*mgt       821*/
!                                                                               !/*mgt       822*/
!        PAX: Track; weight; weight*energy gain. Generic neutral                !/*mgt       823*/
!             particles.                                                        !/*mgt       824*/
      data label_pax_gain_specific(igain_gen_neu_unused_11,                     !/*mgt       825*/
     &     id_category_gen_neu) /' '/                                           !/*mgt       826*/
      data label_pax_gain_specific(igain_gen_neu_tabular_sampling,              !/*mgt       827*/
     &     id_category_gen_neu) /'tabular sampling'/                            !/*mgt       828*/
      data label_pax_gain_specific(igain_gen_neu_photonuclear,                  !/*mgt       829*/
     &     id_category_gen_neu) /'photonuclear'/                                !/*mgt       830*/
      data label_pax_gain_specific(igain_gen_neu_unused_14,                     !/*mgt       831*/
     &     id_category_gen_neu) /' '/                                           !/*mgt       832*/
      data label_pax_gain_specific(igain_gen_neu_unused_15,                     !/*mgt       833*/
     &     id_category_gen_neu) /' '/                                           !/*mgt       834*/
      data label_pax_gain_specific(igain_gen_neu_unused_16,                     !/*mgt       835*/
     &     id_category_gen_neu) /' '/                                           !/*mgt       836*/
      data label_pax_gain_specific(igain_gen_neu_gamma_xgen_neu,                !/*mgt       837*/
     &     id_category_gen_neu) /'(gamma,xgen_neu)'/                            !/*mgt       838*/
      data label_pax_gain_specific(igain_gen_neu_unused_18,                     !/*mgt       839*/
     &     id_category_gen_neu) /' '/                                           !/*mgt       840*/
      data label_pax_gain_specific(igain_gen_neu_unused_19,                     !/*mgt       841*/
     &     id_category_gen_neu) /' '/                                           !/*mgt       842*/
      data label_pax_gain_specific(igain_gen_neu_unused_20,                     !/*mgt       843*/
     &     id_category_gen_neu) /' '/                                           !/*mgt       844*/
      data label_pax_gain_specific(igain_gen_neu_unused_21,                     !/*mgt       845*/
     &     id_category_gen_neu) /' '/                                           !/*mgt       846*/
!                                                                               !/*mgt       847*/
!        PAX: Track; weight; weight*energy loss. Generic neutral                !/*mgt       848*/
!             particles.                                                        !/*mgt       849*/
      data label_pax_loss_specific(loss_gen_neu_nucl_interaction,               !/*mgt       850*/
     &     id_category_gen_neu) /'nucl. interaction'/                           !/*mgt       851*/
      data label_pax_loss_specific(loss_gen_neu_elastic_scatter,                !/*mgt       852*/
     &     id_category_gen_neu) /'elastic scatter'/                             !/*mgt       853*/
      data label_pax_loss_specific(loss_gen_neu_particle_decay,                 !/*mgt       854*/
     &     id_category_gen_neu) /'particle decay'/                              !/*mgt       855*/
      data label_pax_loss_specific(loss_gen_neu_unused_14,                      !/*mgt       856*/
     &     id_category_gen_neu) /' '/                                           !/*mgt       857*/
      data label_pax_loss_specific(loss_gen_neu_unused_15,                      !/*mgt       858*/
     &     id_category_gen_neu) /' '/                                           !/*mgt       859*/
      data label_pax_loss_specific(loss_gen_neu_unused_16,                      !/*mgt       860*/
     &     id_category_gen_neu) /' '/                                           !/*mgt       861*/
      data label_pax_loss_specific(loss_gen_neu_unused_17,                      !/*mgt       862*/
     &     id_category_gen_neu) /' '/                                           !/*mgt       863*/
      data label_pax_loss_specific(loss_gen_neu_unused_18,                      !/*mgt       864*/
     &     id_category_gen_neu) /' '/                                           !/*mgt       865*/
      data label_pax_loss_specific(loss_gen_neu_unused_19,                      !/*mgt       866*/
     &     id_category_gen_neu) /' '/                                           !/*mgt       867*/
      data label_pax_loss_specific(loss_gen_neu_unused_20,                      !/*mgt       868*/
     &     id_category_gen_neu) /' '/                                           !/*mgt       869*/
      data label_pax_loss_specific(loss_gen_neu_unused_21,                      !/*mgt       870*/
     &     id_category_gen_neu) /' '/                                           !/*mgt       871*/
!                                                                               !/*mgt       872*/
!        PAX: Track; weight; weight*energy gain. Generic charged                !/*mgt       873*/
!             particles.                                                        !/*mgt       874*/
      data label_pax_gain_specific(igain_gen_chg_tabular_sampling,              !/*mgt       875*/
     &     id_category_gen_chg) /'tabular sampling'/                            !/*mgt       876*/
      data label_pax_gain_specific(igain_gen_chg_unused_12,                     !/*mgt       877*/
     &     id_category_gen_chg) /' '/                                           !/*mgt       878*/
      data label_pax_gain_specific(igain_gen_chg_photonuclear,                  !/*mgt       879*/
     &     id_category_gen_chg) /'photonuclear'/                                !/*mgt       880*/
      data label_pax_gain_specific(igain_gen_chg_elastic_recoil,                !/*mgt       881*/
     &     id_category_gen_chg) /'elastic recoil'/                              !/*mgt       882*/
      data label_pax_gain_specific(igain_gen_chg_unused_15,                     !/*mgt       883*/
     &     id_category_gen_chg) /' '/                                           !/*mgt       884*/
      data label_pax_gain_specific(igain_gen_chg_unused_16,                     !/*mgt       885*/
     &     id_category_gen_chg) /' '/                                           !/*mgt       886*/
      data label_pax_gain_specific(igain_gen_chg_gamma_xgen_chg,                !/*mgt       887*/
     &     id_category_gen_chg) /'(gamma,xgen_chg)'/                            !/*mgt       888*/
      data label_pax_gain_specific(igain_gen_chg_unused_18,                     !/*mgt       889*/
     &     id_category_gen_chg) /' '/                                           !/*mgt       890*/
      data label_pax_gain_specific(igain_gen_chg_unused_19,                     !/*mgt       891*/
     &     id_category_gen_chg) /' '/                                           !/*mgt       892*/
      data label_pax_gain_specific(igain_gen_chg_unused_20,                     !/*mgt       893*/
     &     id_category_gen_chg) /' '/                                           !/*mgt       894*/
      data label_pax_gain_specific(igain_gen_chg_unused_21,                     !/*mgt       895*/
     &     id_category_gen_chg) /' '/                                           !/*mgt       896*/
!                                                                               !/*mgt       897*/
!        PAX: Track; weight; weight*energy loss. Generic charged                !/*mgt       898*/
!        particles.                                                             !/*mgt       899*/
      data label_pax_loss_specific(loss_gen_chg_multiple_scatter,               !/*mgt       900*/
     &     id_category_gen_chg) /'multiple scatter'/                            !/*mgt       901*/
      data label_pax_loss_specific(loss_gen_chg_bremsstrahlung,                 !/*mgt       902*/
     &     id_category_gen_chg) /'bremsstrahlung'/                              !/*mgt       903*/
      data label_pax_loss_specific(loss_gen_chg_nucl_interaction,               !/*mgt       904*/
     &     id_category_gen_chg) /'nucl. interaction'/                           !/*mgt       905*/
      data label_pax_loss_specific(loss_gen_chg_elastic_scatter,                !/*mgt       906*/
     &     id_category_gen_chg) /'elastic scatter'/                             !/*mgt       907*/
      data label_pax_loss_specific(loss_gen_chg_particle_decay,                 !/*mgt       908*/
     &     id_category_gen_chg) /'particle decay'/                              !/*mgt       909*/
      data label_pax_loss_specific(loss_gen_chg_capture,                        !/*mgt       910*/
     &     id_category_gen_chg) /'capture'/                                     !/*mgt       911*/
      data label_pax_loss_specific(loss_gen_chg_tabular_sampling,               !/*mgt       912*/
     &     id_category_gen_chg) /'tabular sampling'/                            !/*mgt       913*/
      data label_pax_loss_specific(loss_gen_chg_unused_18,                      !/*mgt       914*/
     &     id_category_gen_chg) /' '/                                           !/*mgt       915*/
      data label_pax_loss_specific(loss_gen_chg_unused_19,                      !/*mgt       916*/
     &     id_category_gen_chg) /' '/                                           !/*mgt       917*/
      data label_pax_loss_specific(loss_gen_chg_unused_20,                      !/*mgt       918*/
     &     id_category_gen_chg) /' '/                                           !/*mgt       919*/
      data label_pax_loss_specific(loss_gen_chg_unused_21,                      !/*mgt       920*/
     &     id_category_gen_chg) /' '/                                           !/*mgt       921*/
!                                                                               !/*mgt       922*/
!***********************************************************************        !/*mgt       923*/
!        character common block.                                                !/*mgt       924*/
!                                                                               !/*mgt       925*/
!        isub:  names of files                                                  !/*mgt       926*/
      character*8 inp,outp,runtpe,mctal,wssa,ptrac,comout,srctp,                !/*mgt       927*/
     & wwout,wwone,plotm,rssa,xsdir,com,wwinp,mdata,dumn1,dumn2,                !/*mgt       928*/
     & isub(ndef)                                                               !/*mgt       929*/
      common /charcm/ inp,outp,runtpe,mctal,wssa,ptrac,comout,srctp,            !/*mgt       930*/
     & wwout,wwone,plotm,rssa,xsdir,com,wwinp,mdata,dumn1,dumn2                 !/*mgt       931*/
      character*8 dumn(14)                                                      !/*mgt       932*/
      common /charcm/ dumn                                                      !/*mgt       933*/
      equivalence (isub,inp)                                                    !/*mgt       934*/
!                                                                               !/*mgt       935*/
      end module global_tables                                                  !/*mgt       936*/
