!     $Id: zero_spb_length.F,v 1.4 2002/03/27 18:16:20 gwm Exp $              !/*ze          1*/
      subroutine zero_spb_length                                                !/*ze          2*/
!     Zero the variable LEN_DYNAMIC_SPB_DATA, the length of dynamically         !/*ze          3*/
!     allocated storage for secondary-particle biasing, prior to                !/*ze          4*/
!     counting the necessary storage locations.                                 !/*ze          5*/
!                                                                               !/*ze          6*/
#ifdef SPABI                                                                    /*!ze          7*/
      use global_data                                                           !/*zexc        1*/
!                                                                               !/*zexc        2*/
      implicit real(kindr) (a-h,o-z)                                            !/*zexc        3*/
      implicit integer(kindi) (i-n)                                             !/*zexc        4*/
!                                                                               !/*zexc        5*/
!                                                                               !/*ze          9*/
      len_dynamic_spb_data = 0                                                  !/*ze         10*/
#endif                                                                          /*!ze         11*/
      return                                                                    !/*ze         12*/
      end                                                                       !/*ze         13*/
