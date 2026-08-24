!#ident runtpw.F from deck rw                                                   !/*rw          1*/
      subroutine runtpw(iu)                                                     !/*rw          2*/
!        write variable commons and tallies to runtpe.                          !/*rw          3*/
      use global_data                                                           !/*rwx         1*/
!                                                                               !/*rwx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*rwx         3*/
      implicit integer(kindi) (i-n)                                             !/*rwx         4*/
!                                                                               !/*rwx         5*/
      integer :: iu                                                             !/*rwx         6*/
!                                                                               !/*rw          5*/
      write(iu)kod,ver,loddat,idtm,chcd,probid                                  !/*rw          6*/
      write(iu)gvarcm,jvarcm                                                    !/*rw          7*/
      call vdac_write (iu)                                                      !/*rwx         7*/
      return                                                                    !/*rw         12*/
      end                                                                       !/*rw         13*/
