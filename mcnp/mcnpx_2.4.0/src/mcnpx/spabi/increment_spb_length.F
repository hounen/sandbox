!     $Id: increment_spb_length.F,v 1.4 2002/03/27 18:16:20 gwm Exp $         !/*in          1*/
      subroutine increment_spb_length(i)                                        !/*in          2*/
!     Add the integer argument I to the variable LEN_DYNAMIC_SPB_DATA,          !/*in          3*/
!     the length of dynamically allocated storage being reserved for            !/*in          4*/
!     secondary-particle biasing.                                               !/*in          5*/
!                                                                               !/*in          6*/
#ifdef SPABI                                                                    /*!in          7*/
      use global_data                                                           !/*inxc        1*/
!                                                                               !/*inxc        2*/
      implicit real(kindr) (a-h,o-z)                                            !/*inxc        3*/
      implicit integer(kindi) (i-n)                                             !/*inxc        4*/
!                                                                               !/*inxc        5*/
!                                                                               !/*in          9*/
      len_dynamic_spb_data = len_dynamic_spb_data + i                           !/*in         10*/
#endif                                                                          /*!in         11*/
      return                                                                    !/*in         12*/
      end                                                                       !/*in         13*/
