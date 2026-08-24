!     $Id: interact_outcomes.h,v 1.3 2002/03/27 18:14:09 gwm Exp $            !/*in          1*/
!     Possible returned values of LTER, the last argument of SUBROUTINE         !/*in          2*/
!     INTERACT.  The value identifies the type of event sampled.                !/*in          3*/
!                                                                               !/*in          4*/
!     Real nonelastic collision (terminal event).                               !/*in          5*/
      parameter (id_nonelastic_collisn_terminal  = 1)                           !/*in          6*/
!                                                                               !/*in          7*/
!     Real elastic collision (nonterminal event).                               !/*in          8*/
      parameter (id_elastic_collisn_nonterminal  = 2)                           !/*in          9*/
!                                                                               !/*in         10*/
!     True pseudocollision (nonterminal event).                                 !/*in         11*/
      parameter (id_true_pseudo_nonterminal      = 3)                           !/*in         12*/
!                                                                               !/*in         13*/
!     Full decay pseudocollision (terminal event).                              !/*in         14*/
      parameter (id_fulldecay_pseudo_terminal    = 4)                           !/*in         15*/
!                                                                               !/*in         16*/
!     Partial decay pseudocollision (nonterminal event).                        !/*in         17*/
      parameter (id_partdecay_pseudo_nonterminal = 5)                           !/*in         18*/
!                                                                               !/*in         19*/
!     Simple stopped particle (terminal event).                                 !/*in         20*/
      parameter (id_stopped_particle_terminal    = 6)                           !/*in         21*/
