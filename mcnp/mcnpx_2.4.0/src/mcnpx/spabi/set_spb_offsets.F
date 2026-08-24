!     $Id: set_spb_offsets.F,v 1.4 2002/03/27 18:16:20 gwm Exp $              !/*se          1*/
      subroutine set_spb_offsets                                                !/*se          2*/
!     Set (by secondary particle type) the individual offsets of the            !/*se          3*/
!     energy-band/biasing-parameter data within the dynamic allocated           !/*se          4*/
!     storage.                                                                  !/*se          5*/
!                                                                               !/*se          6*/
#ifdef SPABI                                                                    /*!se          7*/
      use global_data                                                           !/*sea         1*/
      use imcn_data                                                             !/*sea         2*/
!                                                                               !/*sea         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*sea         4*/
      implicit integer(kindi) (i-n)                                             !/*sea         5*/
!                                                                               !/*se         10*/
      do id_sec=1,mipt                                                          !/*se         11*/
         if (nqp(id_sec).ne.0) then                                             !/*se         12*/
            interior_offset(id_sec) = len_dynamic_spb_data                      !/*se         13*/
            return                                                              !/*se         14*/
         endif                                                                  !/*se         15*/
      enddo                                                                     !/*se         16*/
!     (The error message should be given elsewhere.)                            !/*se         17*/
!                                                                               !/*se         18*/
#endif                                                                          /*!se         19*/
      return                                                                    !/*se         20*/
      end                                                                       !/*se         21*/
