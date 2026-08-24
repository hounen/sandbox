!#ident getjdt.F from deck gj                                                   !/*gj          1*/
      subroutine getjdt(ha,hb)                                                  !/*gj          3*/
#ifdef AIX                                                                      /*!gjx         1*/
!        required to make ibm-aix data and time call work.                      !/*gj          4*/
      character ha*36,hb*36                                                     !/*gj          5*/
      hb=ha                                                                     !/*gj          6*/
#endif                                                                          /*!gjx         2*/
      return                                                                    !/*gj          7*/
      end                                                                       !/*gj          8*/
