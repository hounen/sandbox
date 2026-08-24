!     $Id: get_names.F,v 1.4 2002/03/27 18:14:51 gwm Exp $                    !/*ge          1*/
      subroutine get_names (namec,namem,idowrite)                               !/*ge          2*/
#ifdef HISTP                                                                    /*!ge          3*/
!         Returns MCNP names (problem numbers) and one flag                     !/*ge          4*/
!         for use in HISTP writing.                                             !/*ge          5*/
!         R E Prael 3/25/99                                                     !/*ge          6*/
      use global_data                                                           !/*gexc        1*/
!                                                                               !/*gexc        2*/
      implicit real(kindr) (a-h,o-z)                                            !/*gexc        3*/
      implicit integer(kindi) (i-n)                                             !/*gexc        4*/
!                                                                               !/*gexc        5*/
!                                                                               !/*ge          8*/
! Problem number (name) of current cell ICL.                                    !/*ge          9*/
      namec = ncl(lncl+icl)                                                     !/*ge         10*/
! Name of current material MKC.                                                 !/*ge         11*/
      namem = nmt(lnmt+mkc)                                                     !/*ge         12*/
! Flag to write for this cell.                                                  !/*ge         13*/
      idowrite = nhw(lnhw+icl)                                                  !/*ge         14*/
#endif                                                                          /*!ge         15*/
      return                                                                    !/*ge         16*/
      end                                                                       !/*ge         17*/
