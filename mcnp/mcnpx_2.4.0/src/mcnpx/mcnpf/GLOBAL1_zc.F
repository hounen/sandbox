!#ident GLOBAL1_zc.F from deck mgp                                              !/*mgp         1*/
      module global_parameters                                                  !/*mgp         2*/
!                                                                               !/*mgp         3*/
      implicit none                                                             !/*mgp         4*/
      save                                                                      !/*mgp         5*/
      public                                                                    !/*mgp         6*/
!                                                                               !/*mgp         7*/
!***********************************************************************        !/*mgp         8*/
!        integer parameters.                                                    !/*mgp         9*/
!                                                                               !/*mgp        10*/
!        parameters for real and integer kinds.                                 !/*mgp        11*/
      integer, parameter :: kindr = 8                                           !/*mgp        12*/
#ifdef CHEAP                                                                    /*!mgp        13*/
      integer, parameter :: kindi = 4                                           !/*mgp        14*/
#else                                                                           /*!mgp        15*/
      integer, parameter :: kindi = 8                                           !/*mgp        16*/
#endif                                                                          /*!mgp        17*/
!                                                                               !/*mgp        18*/
!        array dimensions.                                                      !/*mgp        19*/
      integer, parameter :: iemaxn = 31                                         !/*mgp        20*/
      integer, parameter :: iemaxp = 26                                         !/*mgp        21*/
      integer, parameter :: m_lca = 9                                           !/*mgp        22*/
      integer, parameter :: m_lcb = 8                                           !/*mgp        23*/
      integer, parameter :: m_lea = 8                                           !/*mgp        24*/
      integer, parameter :: m_leb = 4                                           !/*mgp        25*/
#ifdef MESHTAL                                                                  /*!mgp        26*/
      integer, parameter :: maxg = 20                                           !/*mgp        27*/
      integer, parameter :: maxgv = 10                                          !/*mgp        28*/
#endif                                                                          /*!mgp        29*/
      integer, parameter :: maxi = 34                                           !/*mgp        30*/
      integer, parameter :: maxv = 20                                           !/*mgp        31*/
      integer, parameter :: maxw = 4                                            !/*mgp        32*/
      integer, parameter :: mbng = 51                                           !/*mgp        33*/
      integer, parameter :: mcoh = 55                                           !/*mgp        34*/
      integer, parameter :: maxsec = 8                                          !/*mgp        35*/
      integer, parameter :: mcol = 11                                           !/*mgpc        1*/
      integer, parameter :: mcpu = 515                                          !/*mgp        36*/
      integer, parameter :: mdum = 50                                           !/*mgp        37*/
      integer, parameter :: minc = 21                                           !/*mgp        38*/
      integer, parameter :: mink = 200                                          !/*mgp        39*/
      integer, parameter :: mipt = 34                                           !/*mgp        40*/
      integer, parameter :: mixs = 12                                           !/*mgp        41*/
      integer, parameter :: mjsf = 9                                            !/*mgp        42*/
      integer, parameter :: mkft = 9                                            !/*mgp        43*/
      integer, parameter :: mkpl = 35                                           !/*mgp        44*/
      integer, parameter :: mktc = 26                                           !/*mgp        45*/
      integer, parameter :: mlpt = 22                                           !/*mgp        46*/
      integer, parameter :: mlgc = 1000                                         !/*mgp        47*/
      integer, parameter :: mpb = 5                                             !/*mgp        48*/
      integer, parameter :: mpng = 21                                           !/*mgp        49*/
      integer, parameter :: mseb = 301                                          !/*mgp        50*/
      integer, parameter :: msigg = 6                                           !/*mgp        51*/
      integer, parameter :: mspare = 3                                          !/*mgp        52*/
      integer, parameter :: mstp = 4                                            !/*mgp        53*/
      integer, parameter :: mtop = 89                                           !/*mgp        54*/
      integer, parameter :: mwng = (mtop+1)/2                                   !/*mgp        55*/
      integer, parameter :: mxdt = 20                                           !/*mgp        56*/
      integer, parameter :: mxdx = 10                                           !/*mgp        57*/
      integer, parameter :: mxlv = 10                                           !/*mgp        58*/
      integer, parameter :: mxso = 2                                            !/*mgp        59*/
      integer, parameter :: mxss = 6                                            !/*mgp        60*/
      integer, parameter :: nbmx = 640                                          !/*mgp        61*/
      integer, parameter :: ncolor = 64                                         !/*mgpk        1*/
      integer, parameter :: ndef = 32                                           !/*mgp        63*/
      integer, parameter :: nmkey = 11                                          !/*mgp        64*/
      integer, parameter :: nmrkp = 6500                                        !/*mgp        65*/
      integer, parameter :: novr = 5                                            !/*mgp        66*/
      integer, parameter :: nptr = 13                                           !/*mgp        67*/
      integer, parameter :: nsp = 602                                           !/*mgp        68*/
      integer, parameter :: nsp12 = nsp+12                                      !/*mgp        69*/
      integer, parameter :: npkey = 6                                           !/*mgp        70*/
      integer, parameter :: ntp = 201                                           !/*mgp        71*/
      integer, parameter :: nspt = nsp+ntp+7                                    !/*mgp        72*/
!                                                                               !/*mgp        73*/
!        in various accounting tables, we distinguish among categories          !/*mgp        74*/
!        of particles:  currently neutrons, photons, electrons, generic         !/*mgp        75*/
!        neutral particles, and generic charged particles.                      !/*mgp        76*/
!        named constants help to locate instances of this distinction.          !/*mgp        77*/
      integer, parameter :: id_category_neutron  = 1                            !/*mgp        78*/
      integer, parameter :: id_category_photon   = 2                            !/*mgp        79*/
      integer, parameter :: id_category_electron = 3                            !/*mgp        80*/
      integer, parameter :: id_category_gen_neu  = 4                            !/*mgp        81*/
      integer, parameter :: id_category_gen_chg  = 5                            !/*mgp        82*/
      integer, parameter :: number_particle_categories = 5                      !/*mgp        83*/
!                                                                               !/*mgp        84*/
!        named constants to clarify PWB contributions during transport.         !/*mgp        85*/
!        for a category of particle (of number_particle_categories),            !/*mgp        86*/
!        the constants should be contiguous integers starting with 1.           !/*mgp        87*/
!        the order must be: external events, then variance-reduction            !/*mgp        88*/
!        events, then physical events.                                          !/*mgp        89*/
!                                                                               !/*mgp        90*/
!        the parameters items_pwb_... that appear after each list (after        !/*mgp        91*/
!        neutrons in the particle-dependent case) must count the named          !/*mgp        92*/
!        constants in each category.                                            !/*mgp        93*/
!                                                                               !/*mgp        94*/
!        the named constants should be in one-to-one correspondence with        !/*mgp        95*/
!        elements of label_pwb_...                                              !/*mgp        96*/
!                                                                               !/*mgp        97*/
!        external events, particle-independent.                                 !/*mgp        98*/
      integer, parameter :: ipwb_external_entering      = 1                     !/*mgp        99*/
      integer, parameter :: ipwb_external_source        = 2                     !/*mgp       100*/
      integer, parameter :: ipwb_external_energy_cutoff = 3                     !/*mgp       101*/
      integer, parameter :: ipwb_external_time_cutoff   = 4                     !/*mgp       102*/
      integer, parameter :: ipwb_external_exiting       = 5                     !/*mgp       103*/
      integer, parameter :: items_pwb_external = 5                              !/*mgp       104*/
!                                                                               !/*mgp       105*/
!        variance-reduction events, particle-independent.                       !/*mgp       106*/
      integer, parameter :: ipwb_variance_weight_window   = 6                   !/*mgp       107*/
      integer, parameter :: ipwb_variance_cell_importance = 7                   !/*mgp       108*/
      integer, parameter :: ipwb_variance_weight_cutoff   = 8                   !/*mgp       109*/
      integer, parameter :: ipwb_variance_energy_import   = 9                   !/*mgp       110*/
      integer, parameter :: ipwb_variance_dxtran          = 10                  !/*mgp       111*/
      integer, parameter :: ipwb_variance_forced_coll     = 11                  !/*mgp       112*/
      integer, parameter :: ipwb_variance_exp_transform   = 12                  !/*mgp       113*/
      integer, parameter :: items_pwb_variance = 7                              !/*mgp       114*/
!                                                                               !/*mgp       115*/
!        physical events, specific to neutrons.                                 !/*mgp       116*/
      integer, parameter :: ipwb_neutron_capture         = 13                   !/*mgp       117*/
      integer, parameter :: ipwb_neutron_n_xn            = 14                   !/*mgp       118*/
      integer, parameter :: ipwb_neutron_loss_to_n_xn    = 15                   !/*mgp       119*/
      integer, parameter :: ipwb_neutron_fission         = 16                   !/*mgp       120*/
      integer, parameter :: ipwb_neutron_loss_to_fission = 17                   !/*mgp       121*/
      integer, parameter :: ipwb_neutron_photonuclear    = 18                   !/*mgp       122*/
      integer, parameter :: ipwb_neutron_nuc_interaction = 19                   !/*mgp       123*/
      integer, parameter :: ipwb_neutron_tabular_bound   = 20                   !/*mgp       124*/
      integer, parameter :: ipwb_neutron_particle_decay  = 21                   !/*mgp       125*/
      integer, parameter :: ipwb_neutron_tabular_sampl   = 22                   !/*mgp       126*/
      integer, parameter :: ipwb_neutron_unused_23       = 23                   !/*mgp       127*/
      integer, parameter :: ipwb_neutron_unused_24       = 24                   !/*mgp       128*/
      integer, parameter :: ipwb_neutron_unused_25       = 25                   !/*mgp       132*/
      integer, parameter :: items_pwb_physical = 13                             !/*mgp       134*/
!                                                                               !/*mgp       135*/
!        physical events, specific to photons.                                  !/*mgp       136*/
      integer, parameter :: ipwb_photon_from_neutrons   = 13                    !/*mgp       137*/
      integer, parameter :: ipwb_photon_bremsstrahlung  = 14                    !/*mgp       138*/
      integer, parameter :: ipwb_photon_capture         = 15                    !/*mgp       139*/
      integer, parameter :: ipwb_photon_p_annihilation  = 16                    !/*mgp       140*/
      integer, parameter :: ipwb_photon_pair_production = 17                    !/*mgp       141*/
      integer, parameter :: ipwb_photon_photonuclear    = 18                    !/*mgp       142*/
      integer, parameter :: ipwb_photon_photonuc_abs    = 19                    !/*mgp       143*/
      integer, parameter :: ipwb_photon_electron_x_rays = 20                    !/*mgp       144*/
      integer, parameter :: ipwb_photon_flourescence    = 21                    !/*mgp       145*/
      integer, parameter :: ipwb_photon_particle_decay  = 22                    !/*mgp       146*/
      integer, parameter :: ipwb_photon_nuc_interaction = 23                    !/*mgp       147*/
      integer, parameter :: ipwb_photon_tabular_sampl   = 24                    !/*mgp       148*/
      integer, parameter :: ipwb_photon_unused_25       = 25                    !/*mgp       149*/
!        the counter items_pwb_physical appears above.                          !/*mgp       150*/
!                                                                               !/*mgp       151*/
!        physical events, specific to electrons.                                !/*mgp       152*/
      integer, parameter :: ipwb_electron_pair_production = 13                  !/*mgp       153*/
      integer, parameter :: ipwb_electron_compton_recoil  = 14                  !/*mgp       154*/
      integer, parameter :: ipwb_electron_photo_electric  = 15                  !/*mgp       155*/
      integer, parameter :: ipwb_electron_photon_auger    = 16                  !/*mgp       156*/
      integer, parameter :: ipwb_electron_electron_auger  = 17                  !/*mgp       157*/
      integer, parameter :: ipwb_electron_knock_on        = 18                  !/*mgp       158*/
      integer, parameter :: ipwb_electron_nuc_interaction = 19                  !/*mgp       159*/
      integer, parameter :: ipwb_electron_particle_decay  = 20                  !/*mgp       160*/
      integer, parameter :: ipwb_electron_unused_21       = 21                  !/*mgp       161*/
      integer, parameter :: ipwb_electron_unused_22       = 22                  !/*mgp       162*/
      integer, parameter :: ipwb_electron_unused_23       = 23                  !/*mgp       163*/
      integer, parameter :: ipwb_electron_unused_24       = 24                  !/*mgp       164*/
      integer, parameter :: ipwb_electron_unused_25       = 25                  !/*mgp       165*/
!        the counter items_pwb_physical appears above.                          !/*mgp       166*/
!                                                                               !/*mgp       167*/
!        physical events, for generic neutral particles.                        !/*mgp       168*/
      integer, parameter :: ipwb_gen_neu_unused_13       = 13                   !/*mgp       169*/
      integer, parameter :: ipwb_gen_neu_nuc_interaction = 14                   !/*mgp       170*/
      integer, parameter :: ipwb_gen_neu_unused_15       = 15                   !/*mgp       171*/
      integer, parameter :: ipwb_gen_neu_particle_decay  = 16                   !/*mgp       172*/
      integer, parameter :: ipwb_gen_neu_photonuclear    = 17                   !/*mgp       173*/
      integer, parameter :: ipwb_gen_neu_tabular_sampl   = 18                   !/*mgp       174*/
      integer, parameter :: ipwb_gen_neu_gamma_xgen_neu  = 19                   !/*mgp       175*/
      integer, parameter :: ipwb_gen_neu_unused_20       = 20                   !/*mgp       176*/
      integer, parameter :: ipwb_gen_neu_unused_21       = 21                   !/*mgp       177*/
      integer, parameter :: ipwb_gen_neu_unused_22       = 22                   !/*mgp       178*/
      integer, parameter :: ipwb_gen_neu_unused_23       = 23                   !/*mgp       179*/
      integer, parameter :: ipwb_gen_neu_unused_24       = 24                   !/*mgp       180*/
      integer, parameter :: ipwb_gen_neu_unused_25       = 25                   !/*mgp       181*/
!        the counter items_pwb_physical appears above.                          !/*mgp       182*/
!                                                                               !/*mgp       183*/
!        physical events, for generic charged particles.                        !/*mgp       184*/
      integer, parameter :: ipwb_gen_chg_unused_13       = 13                   !/*mgp       185*/
      integer, parameter :: ipwb_gen_chg_nuc_interaction = 14                   !/*mgp       186*/
      integer, parameter :: ipwb_gen_chg_unused_15       = 15                   !/*mgp       187*/
      integer, parameter :: ipwb_gen_chg_particle_decay  = 16                   !/*mgp       188*/
      integer, parameter :: ipwb_gen_chg_photonuclear    = 17                   !/*mgp       189*/
      integer, parameter :: ipwb_gen_chg_tabular_sampl   = 18                   !/*mgp       190*/
      integer, parameter :: ipwb_gen_chg_capture         = 19                   !/*mgp       191*/
      integer, parameter :: ipwb_gen_chg_gamma_xgen_chg  = 20                   !/*mgp       192*/
      integer, parameter :: ipwb_gen_chg_unused_21       = 21                   !/*mgp       193*/
      integer, parameter :: ipwb_gen_chg_unused_22       = 22                   !/*mgp       194*/
      integer, parameter :: ipwb_gen_chg_unused_23       = 23                   !/*mgp       195*/
      integer, parameter :: ipwb_gen_chg_unused_24       = 24                   !/*mgp       196*/
      integer, parameter :: ipwb_gen_chg_unused_25       = 25                   !/*mgp       197*/
!        the counter items_pwb_physical appears above.                          !/*mgp       198*/
!                                                                               !/*mgp       199*/
!        supplementary parameters for PWB-related array dimensions.             !/*mgp       200*/
      integer, parameter :: items_pwb_total =                                   !/*mgp       201*/
     &   items_pwb_external + items_pwb_variance + items_pwb_physical           !/*mgp       202*/
!                                                                               !/*mgp       203*/
      integer, parameter :: itm_pwb_external_begin = 1,                         !/*mgp       204*/
     & itm_pwb_external_end = items_pwb_external                                !/*mgp       205*/
!                                                                               !/*mgp       206*/
      integer, parameter ::                                                     !/*mgp       207*/
     & itm_pwb_variance_begin = itm_pwb_external_end + 1,                       !/*mgp       208*/
     & itm_pwb_variance_end = itm_pwb_external_end + items_pwb_variance         !/*mgp       209*/
!                                                                               !/*mgp       210*/
      integer, parameter ::                                                     !/*mgp       211*/
     & itm_pwb_physical_begin = itm_pwb_variance_end + 1,                       !/*mgp       212*/
     & itm_pwb_physical_end = itm_pwb_variance_end + items_pwb_physical         !/*mgp       213*/
!                                                                               !/*mgp       214*/
!        named constants to clarify PAX contributions during transport.         !/*mgp       215*/
!        for a category of particle (of number_particle_categories),            !/*mgp       216*/
!        the constants should be contiguous integers starting with 1.           !/*mgp       217*/
!        the order must be: "all particles" then "specific" with the            !/*mgp       218*/
!        latter meaning specific to the category of particle type.              !/*mgp       219*/
!                                                                               !/*mgp       220*/
!        the parameters items_pax_... appearing after "gain_all_pls" for        !/*mgp       221*/
!        "all particles" and after "gain_neutron" for particle-specific         !/*mgp       222*/
!        case must count the named constants in each category.                  !/*mgp       223*/
!                                                                               !/*mgp       224*/
!        the named constants should be in one-to-one correspondence with        !/*mgp       225*/
!        elements of label_pax_...                                              !/*mgp       226*/
!                                                                               !/*mgp       227*/
!        each item in the loss_... sets, for each particle category,            !/*mgp       228*/
!        should also be given an appropriate JRWB-value (either 0 or one        !/*mgp       229*/
!        of the c        ipwb_... named constants).  If the termination         !/*mgp       230*/
!        variable NTER is ever set to a given loss_... item, then that          !/*mgp       231*/
!        item's JRWB-value should not be 0.                                     !/*mgp       232*/
!                                                                               !/*mgp       233*/
!        track; weight; weight*energy gain. For all particle types.             !/*mgp       234*/
      integer, parameter :: igain_all_pls_source            = 1                 !/*mgp       235*/
      integer, parameter :: igain_all_pls_nucl_interaction  = 2                 !/*mgp       236*/
      integer, parameter :: igain_all_pls_particle_decay    = 3                 !/*mgp       237*/
      integer, parameter :: igain_all_pls_weight_window     = 4                 !/*mgp       238*/
      integer, parameter :: igain_all_pls_cell_importance   = 5                 !/*mgp       239*/
      integer, parameter :: igain_all_pls_weight_cutoff     = 6                 !/*mgp       240*/
      integer, parameter :: igain_all_pls_energy_importance = 7                 !/*mgp       241*/
      integer, parameter :: igain_all_pls_dxtran            = 8                 !/*mgp       242*/
      integer, parameter :: igain_all_pls_forced_collisions = 9                 !/*mgp       243*/
      integer, parameter :: igain_all_pls_exp_transform     = 10                !/*mgp       244*/
      integer, parameter :: items_pax_all_pls=10                                !/*mgp       245*/
!                                                                               !/*mgp       246*/
!        track; weight; weight*energy loss. For all particle types.             !/*mgp       247*/
      integer, parameter :: loss_all_pls_escape            = 1                  !/*mgp       248*/
      integer, parameter :: loss_all_pls_energy_cutoff     = 2                  !/*mgp       249*/
      integer, parameter :: loss_all_pls_time_cutoff       = 3                  !/*mgp       250*/
      integer, parameter :: loss_all_pls_weight_window     = 4                  !/*mgp       251*/
      integer, parameter :: loss_all_pls_cell_importance   = 5                  !/*mgp       252*/
      integer, parameter :: loss_all_pls_weight_cutoff     = 6                  !/*mgp       253*/
      integer, parameter :: loss_all_pls_energy_importance = 7                  !/*mgp       254*/
      integer, parameter :: loss_all_pls_dxtran            = 8                  !/*mgp       255*/
      integer, parameter :: loss_all_pls_forced_collisions = 9                  !/*mgp       256*/
      integer, parameter :: loss_all_pls_exp_transform     = 10                 !/*mgp       257*/
!        the counter items_pax_all_pls appears above.                           !/*mgp       258*/
!                                                                               !/*mgp       259*/
!        track; weight; weight*energy gain. Specific to neutrons.               !/*mgp       260*/
      integer, parameter :: igain_neutron_upscattering      = 11                !/*mgp       261*/
      integer, parameter :: igain_neutron_photonuclear      = 12                !/*mgp       262*/
      integer, parameter :: igain_neutron_n_xn              = 13                !/*mgp       263*/
      integer, parameter :: igain_neutron_prompt_fission    = 14                !/*mgp       264*/
      integer, parameter :: igain_neutron_delayed_fission   = 15                !/*mgp       265*/
      integer, parameter :: igain_neutron_unused_16         = 16                !/*mgp       266*/
      integer, parameter :: igain_neutron_tabular_boundary  = 17                !/*mgp       267*/
      integer, parameter :: igain_neutron_tabular_sampling  = 18                !/*mgp       268*/
      integer, parameter :: igain_neutron_unused_19         = 19                !/*mgp       269*/
      integer, parameter :: igain_neutron_unused_20         = 20                !/*mgp       270*/
      integer, parameter :: igain_neutron_unused_21         = 21                !/*mgp       274*/
      integer, parameter :: items_pax_specific=11                               !/*mgp       276*/
!                                                                               !/*mgp       277*/
!        track; weight; weight*energy loss. Specific to neutrons.               !/*mgp       278*/
      integer, parameter :: loss_neutron_downscattering   = 11                  !/*mgp       279*/
      integer, parameter :: loss_neutron_capture          = 12                  !/*mgp       280*/
      integer, parameter :: loss_neutron_loss_to_n_xn     = 13                  !/*mgp       281*/
      integer, parameter :: loss_neutron_loss_to_fission  = 14                  !/*mgp       282*/
      integer, parameter :: loss_neutron_nucl_interaction = 15                  !/*mgp       283*/
      integer, parameter :: loss_neutron_particle_decay   = 16                  !/*mgp       284*/
      integer, parameter :: loss_neutron_tabular_boundary = 17                  !/*mgp       285*/
      integer, parameter :: loss_neutron_unused_18        = 18                  !/*mgp       286*/
      integer, parameter :: loss_neutron_unused_19        = 19                  !/*mgp       287*/
      integer, parameter :: loss_neutron_unused_20        = 20                  !/*mgp       288*/
      integer, parameter :: loss_neutron_unused_21        = 21                  !/*mgp       292*/
!        the counter items_pax_specific appears above.                          !/*mgp       294*/
!                                                                               !/*mgp       295*/
!        track; weight; weight*energy gain. Specific to photons.                !/*mgp       296*/
      integer, parameter :: igain_photon_from_neutrons     = 11                 !/*mgp       297*/
      integer, parameter :: igain_photon_bremsstrahlung    = 12                 !/*mgp       298*/
      integer, parameter :: igain_photon_p_annihilation    = 13                 !/*mgp       299*/
      integer, parameter :: igain_photon_photonuclear      = 14                 !/*mgp       300*/
      integer, parameter :: igain_photon_electron_x_rays   = 15                 !/*mgp       301*/
      integer, parameter :: igain_photon_1st_fluorescence  = 16                 !/*mgp       302*/
      integer, parameter :: igain_photon_2nd_fluorescence  = 17                 !/*mgp       303*/
      integer, parameter :: igain_photon_gamma_xgamma      = 18                 !/*mgp       304*/
      integer, parameter :: igain_photon_tabular_sampling  = 19                 !/*mgp       305*/
      integer, parameter :: igain_photon_unused_20         = 20                 !/*mgp       306*/
      integer, parameter :: igain_photon_unused_21         = 21                 !/*mgp       307*/
!        the counter items_pax_specific appears above.                          !/*mgp       308*/
!                                                                               !/*mgp       309*/
!        track; weight; weight*energy loss. Specific to photons.                !/*mgp       310*/
      integer, parameter :: loss_photon_compton_scatter = 11                    !/*mgp       311*/
      integer, parameter :: loss_photon_capture         = 12                    !/*mgp       312*/
      integer, parameter :: loss_photon_pair_production = 13                    !/*mgp       313*/
      integer, parameter :: loss_photon_photonuclear    = 14                    !/*mgp       314*/
      integer, parameter :: loss_photon_unused_15       = 15                    !/*mgp       315*/
      integer, parameter :: loss_photon_unused_16       = 16                    !/*mgp       316*/
      integer, parameter :: loss_photon_unused_17       = 17                    !/*mgp       317*/
      integer, parameter :: loss_photon_unused_18       = 18                    !/*mgp       318*/
      integer, parameter :: loss_photon_unused_19       = 19                    !/*mgp       319*/
      integer, parameter :: loss_photon_unused_20       = 20                    !/*mgp       320*/
      integer, parameter :: loss_photon_unused_21       = 21                    !/*mgp       321*/
!        the counter items_pax_specific appears above.                          !/*mgp       322*/
!                                                                               !/*mgp       323*/
!        track; weight; weight*energy gain. Specific to electrons.              !/*mgp       324*/
      integer, parameter :: igain_electron_pair_production = 11                 !/*mgp       325*/
      integer, parameter :: igain_electron_compton_recoil  = 12                 !/*mgp       326*/
      integer, parameter :: igain_electron_photo_electric  = 13                 !/*mgp       327*/
      integer, parameter :: igain_electron_photon_auger    = 14                 !/*mgp       328*/
      integer, parameter :: igain_electron_electron_auger  = 15                 !/*mgp       329*/
      integer, parameter :: igain_electron_knock_on        = 16                 !/*mgp       330*/
      integer, parameter :: igain_electron_gamma_xelectron = 17                 !/*mgp       331*/
      integer, parameter :: igain_electron_unused_18       = 18                 !/*mgp       332*/
      integer, parameter :: igain_electron_unused_19       = 19                 !/*mgp       333*/
      integer, parameter :: igain_electron_unused_20       = 20                 !/*mgp       334*/
      integer, parameter :: igain_electron_unused_21       = 21                 !/*mgp       335*/
!        the counter items_pax_specific appears above.                          !/*mgp       336*/
!                                                                               !/*mgp       337*/
!        track; weight; weight*energy loss. Specific to electrons.              !/*mgp       338*/
      integer, parameter :: loss_electron_scattering        = 11                !/*mgp       339*/
      integer, parameter :: loss_electron_bremsstrahlung    = 12                !/*mgp       340*/
      integer, parameter :: loss_electron_unused_13         = 13                !/*mgp       341*/
      integer, parameter :: loss_electron_unused_14         = 14                !/*mgp       342*/
      integer, parameter :: loss_electron_unused_15         = 15                !/*mgp       343*/
      integer, parameter :: loss_electron_interact_or_decay = 16                !/*mgp       344*/
      integer, parameter :: loss_electron_unused_17         = 17                !/*mgp       345*/
      integer, parameter :: loss_electron_unused_18         = 18                !/*mgp       346*/
      integer, parameter :: loss_electron_unused_19         = 19                !/*mgp       347*/
      integer, parameter :: loss_electron_unused_20         = 20                !/*mgp       348*/
      integer, parameter :: loss_electron_unused_21         = 21                !/*mgp       349*/
!        the counter items_pax_specific appears above.                          !/*mgp       350*/
!                                                                               !/*mgp       351*/
!        track; weight; weight*energy gain. Generic neutral particles.          !/*mgp       352*/
      integer, parameter :: igain_gen_neu_unused_11        = 11                 !/*mgp       353*/
      integer, parameter :: igain_gen_neu_tabular_sampling = 12                 !/*mgp       354*/
      integer, parameter :: igain_gen_neu_photonuclear     = 13                 !/*mgp       355*/
      integer, parameter :: igain_gen_neu_unused_14        = 14                 !/*mgp       356*/
      integer, parameter :: igain_gen_neu_unused_15        = 15                 !/*mgp       357*/
      integer, parameter :: igain_gen_neu_unused_16        = 16                 !/*mgp       358*/
      integer, parameter :: igain_gen_neu_gamma_xgen_neu   = 17                 !/*mgp       359*/
      integer, parameter :: igain_gen_neu_unused_18        = 18                 !/*mgp       360*/
      integer, parameter :: igain_gen_neu_unused_19        = 19                 !/*mgp       361*/
      integer, parameter :: igain_gen_neu_unused_20        = 20                 !/*mgp       362*/
      integer, parameter :: igain_gen_neu_unused_21        = 21                 !/*mgp       363*/
!        the counter items_pax_specific appears above.                          !/*mgp       364*/
!                                                                               !/*mgp       365*/
!        track; weight; weight*energy loss. Generic neutral particles.          !/*mgp       366*/
      integer, parameter :: loss_gen_neu_nucl_interaction = 11                  !/*mgp       367*/
      integer, parameter :: loss_gen_neu_elastic_scatter  = 12                  !/*mgp       368*/
      integer, parameter :: loss_gen_neu_particle_decay   = 13                  !/*mgp       369*/
      integer, parameter :: loss_gen_neu_unused_14        = 14                  !/*mgp       370*/
      integer, parameter :: loss_gen_neu_unused_15        = 15                  !/*mgp       371*/
      integer, parameter :: loss_gen_neu_unused_16        = 16                  !/*mgp       372*/
      integer, parameter :: loss_gen_neu_unused_17        = 17                  !/*mgp       373*/
      integer, parameter :: loss_gen_neu_unused_18        = 18                  !/*mgp       374*/
      integer, parameter :: loss_gen_neu_unused_19        = 19                  !/*mgp       375*/
      integer, parameter :: loss_gen_neu_unused_20        = 20                  !/*mgp       376*/
      integer, parameter :: loss_gen_neu_unused_21        = 21                  !/*mgp       377*/
!        the counter items_pax_specific appears above.                          !/*mgp       378*/
!                                                                               !/*mgp       379*/
!        track; weight; weight*energy gain. Generic charged particles.          !/*mgp       380*/
      integer, parameter :: igain_gen_chg_tabular_sampling = 11                 !/*mgp       381*/
      integer, parameter :: igain_gen_chg_unused_12        = 12                 !/*mgp       382*/
      integer, parameter :: igain_gen_chg_photonuclear     = 13                 !/*mgp       383*/
      integer, parameter :: igain_gen_chg_elastic_recoil   = 14                 !/*mgp       384*/
      integer, parameter :: igain_gen_chg_unused_15        = 15                 !/*mgp       385*/
      integer, parameter :: igain_gen_chg_unused_16        = 16                 !/*mgp       386*/
      integer, parameter :: igain_gen_chg_gamma_xgen_chg   = 17                 !/*mgp       387*/
      integer, parameter :: igain_gen_chg_unused_18        = 18                 !/*mgp       388*/
      integer, parameter :: igain_gen_chg_unused_19        = 19                 !/*mgp       389*/
      integer, parameter :: igain_gen_chg_unused_20        = 20                 !/*mgp       390*/
      integer, parameter :: igain_gen_chg_unused_21        = 21                 !/*mgp       391*/
!        the counter items_pax_specific appears above.                          !/*mgp       392*/
!                                                                               !/*mgp       393*/
!        track; weight; weight*energy loss. Generic charged particles.          !/*mgp       394*/
      integer, parameter :: loss_gen_chg_multiple_scatter = 11                  !/*mgp       395*/
      integer, parameter :: loss_gen_chg_bremsstrahlung   = 12                  !/*mgp       396*/
      integer, parameter :: loss_gen_chg_nucl_interaction = 13                  !/*mgp       397*/
      integer, parameter :: loss_gen_chg_elastic_scatter  = 14                  !/*mgp       398*/
      integer, parameter :: loss_gen_chg_particle_decay   = 15                  !/*mgp       399*/
      integer, parameter :: loss_gen_chg_capture          = 16                  !/*mgp       400*/
      integer, parameter :: loss_gen_chg_tabular_sampling = 17                  !/*mgp       401*/
      integer, parameter :: loss_gen_chg_unused_18        = 18                  !/*mgp       402*/
      integer, parameter :: loss_gen_chg_unused_19        = 19                  !/*mgp       403*/
      integer, parameter :: loss_gen_chg_unused_20        = 20                  !/*mgp       404*/
      integer, parameter :: loss_gen_chg_unused_21        = 21                  !/*mgp       405*/
!        the counter items_pax_specific appears above.                          !/*mgp       406*/
!                                                                               !/*mgp       407*/
!        supplementary parameters for PAX-related array dimensions.             !/*mgp       408*/
      integer, parameter :: items_pax_total =                                   !/*mgp       409*/
     &   items_pax_all_pls + items_pax_specific                                 !/*mgp       410*/
!                                                                               !/*mgp       411*/
      integer, parameter :: itm_pax_all_pls_begin = 1,                          !/*mgp       412*/
     &   itm_pax_all_pls_end = items_pax_all_pls                                !/*mgp       413*/
!                                                                               !/*mgp       414*/
      integer, parameter ::                                                     !/*mgp       415*/
     & itm_pax_specific_begin = itm_pax_all_pls_end + 1,                        !/*mgp       416*/
     & itm_pax_specific_end = itm_pax_all_pls_end + items_pax_specific          !/*mgp       417*/
!                                                                               !/*mgp       418*/
!         i/o unit numbers.                                                     !/*mgp       419*/
      integer, parameter :: iui = 31                                            !/*mgp       420*/
      integer, parameter :: iuo = 32                                            !/*mgp       421*/
      integer, parameter :: iur = 33                                            !/*mgp       422*/
      integer, parameter :: iux = 34                                            !/*mgp       423*/
      integer, parameter :: iud = 35                                            !/*mgp       424*/
      integer, parameter :: iup = 37                                            !/*mgp       425*/
      integer, parameter :: ius = 38                                            !/*mgp       426*/
      integer, parameter :: iu1 = 39                                            !/*mgp       427*/
      integer, parameter :: iu2 = 40                                            !/*mgp       428*/
      integer, parameter :: iusw = 41                                           !/*mgp       429*/
      integer, parameter :: iusr = 42                                           !/*mgp       430*/
      integer, parameter :: iusc = 43                                           !/*mgp       431*/
      integer, parameter :: iuc = 44                                            !/*mgp       432*/
      integer, parameter :: iut = 45                                            !/*mgp       433*/
      integer, parameter :: iuz = 46                                            !/*mgp       434*/
      integer, parameter :: iuk = 47                                            !/*mgp       435*/
      integer, parameter :: iu3 = 48                                            !/*mgp       436*/
      integer, parameter :: iu4 = 49                                            !/*mgp       437*/
      integer, parameter :: iupw = 50                                           !/*mgp       438*/
      integer, parameter :: iupc = 51                                           !/*mgp       439*/
      integer, parameter :: iupx = 52                                           !/*mgp       440*/
      integer, parameter :: iuw = 53                                            !/*mgp       441*/
#ifdef MESHTAL                                                                  /*!mgp       442*/
      integer, parameter :: ims = 54                                            !/*mgp       443*/
#endif                                                                          /*!mgp       444*/
!     integer, parameter :: nhstp = 55 defined elsewhere for HISTP              !/*mgp       445*/
      integer, parameter :: iub = 60                                            !/*mgp       446*/
      integer, parameter :: iuwe = 61                                           !/*mgp       447*/
      integer, parameter :: iuw1 = 62                                           !/*mgp       448*/
!                                                                               !/*mgp       449*/
!***********************************************************************        !/*mgp       450*/
!        real parameters: numbers                                               !/*mgp       451*/
      real(kindr), parameter :: zero = 0.0d0                                    !/*mgp       452*/
      real(kindr), parameter :: vsmall = 1.0d-37                                !/*mgp       453*/
      real(kindr), parameter :: p_1m12 = 1.0d-12                                !/*mgp       454*/
      real(kindr), parameter :: p_1m6 = 1.0d-06                                 !/*mgp       455*/
      real(kindr), parameter :: tenth = 1.0d-01                                 !/*mgp       456*/
      real(kindr), parameter :: third = 1.0d0/3.0d0                             !/*mgp       457*/
      real(kindr), parameter :: half = 0.5d0                                    !/*mgp       458*/
      real(kindr), parameter :: one = 1.0d0                                     !/*mgp       459*/
      real(kindr), parameter :: two = 2.0d0                                     !/*mgp       460*/
      real(kindr), parameter :: three = 3.0d0                                   !/*mgp       461*/
      real(kindr), parameter :: ten = 10.d0                                     !/*mgp       462*/
      real(kindr), parameter :: hundred=1.0d+02                                 !/*mgp       463*/
      real(kindr), parameter :: huge = 1.0d37                                   !/*mgp       464*/
!                                                                               !/*mgp       465*/
!        real parameters: physical                                              !/*mgp       466*/
      real(kindr), parameter :: aneut = 1.008664967d0                           !/*mgp       467*/
      real(kindr), parameter :: avogad = 6.022043446928244d23                   !/*mgp       468*/
      real(kindr), parameter :: avgdn = 1.0d-24*avogad/aneut                    !/*mgp       469*/
      real(kindr), parameter :: dfdmp = -15.0d0                                 !/*mgp       470*/
      real(kindr), parameter :: dftint = 100.0d0                                !/*mgp       471*/
      real(kindr), parameter :: euler = 0.577215664901532861d0                  !/*mgp       472*/
      real(kindr), parameter :: fscon = 137.0393d0                              !/*mgp       473*/
      real(kindr), parameter :: hsll = 1.0d-30                                  !/*mgp       474*/
      real(kindr), parameter :: pie = 3.1415926535898d0                         !/*mgp       475*/
      real(kindr), parameter :: planck = 4.135732d-13                           !/*mgp       476*/
      real(kindr), parameter :: slite = 299.7925d0                              !/*mgp       477*/
!                                                                               !/*mgp       478*/
!        real parameters: defaults                                              !/*mgp       479*/
      real(kindr), parameter :: emax_def = 1.0d+02                              !/*mgp       480*/
      real(kindr), parameter :: emax_top = 1.0d+03                              !/*mgp       481*/
!                                                                               !/*mgp       482*/
!***********************************************************************        !/*mgp       483*/
!        character parameters.                                                  !/*mgp       484*/
!                                                                               !/*mgp       485*/
      character*8, parameter :: kod = 'mcnpx'                                   !/*mgp       486*/
      character*5, parameter :: ver = '2.4.0'                                   !/*mgpl        1*/
      character*28, parameter :: loddat = 'Thu Aug 01 08:00:01 MST 2002'        !/*mgpl        2*/
#if defined(DEC) && !defined(UNIX)                                              /*!mgp       489*/
      character*80, parameter :: hdpth0 = 'c:\mcnpx\xs'                         !/*mgp       490*/
#else                                                                           /*!mgp       491*/
!     character*80, parameter :: hdpth0 = LIBPREFIX                             !/*mgpc        8*/
      character*80, parameter :: hdpth0 = '/usr/local/lib'                      !/*mgpk        4*/
#endif                                                                          /*!mgp       494*/
!                                                                               !/*mgp       495*/
      end module global_parameters                                              !/*mgp       496*/
