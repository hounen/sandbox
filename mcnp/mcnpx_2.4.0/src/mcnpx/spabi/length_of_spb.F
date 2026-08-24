!     $Id: length_of_spb.F,v 1.4 2002/03/27 18:16:20 gwm Exp $                !/*le          1*/
      function length_of_spb()                                                  !/*le          2*/
!     Return the variable LEN_DYNAMIC_SPB_DATA, the length of                   !/*le          3*/
!     dynamically allocated storage for secondary-particle biasing.             !/*le          4*/
!                                                                               !/*le          5*/
#ifdef SPABI                                                                    /*!le          6*/
      use global_data                                                           !/*lexc        1*/
!                                                                               !/*lexc        2*/
      implicit real(kindr) (a-h,o-z)                                            !/*lexc        3*/
      implicit integer(kindi) (i-n)                                             !/*lexc        4*/
!                                                                               !/*lexc        5*/
!                                                                               !/*le          8*/
      length_of_spb = len_dynamic_spb_data                                      !/*le          9*/
#else                                                                           /*!le         10*/
      length_of_spb = 0                                                         !/*le         11*/
#endif                                                                          /*!le         12*/
      return                                                                    !/*le         13*/
      end                                                                       !/*le         14*/
