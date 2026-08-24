!#ident runtpr.F from deck rr                                                   !/*rr          1*/
      subroutine runtpr(iu,ie)                                                  !/*rr          2*/
!        read variable commons and tallies from runtpe.                         !/*rr          3*/
      use global_data                                                           !/*rrx         1*/
!                                                                               !/*rrx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*rrx         3*/
      implicit integer(kindi) (i-n)                                             !/*rrx         4*/
!                                                                               !/*rrx         5*/
      integer :: iu                                                             !/*rrx         6*/
      character hc*10,hi*19,hk*8,hl*8,hp*19,hv*5                                !/*rr4a        1*/
!                                                                               !/*rr          5*/
      ie=0                                                                      !/*rr          6*/
      read(iu,end=10)hk,hv,hl,hi,hc,hp                                          !/*rr          7*/
      read(iu)gvarcm,jvarcm                                                     !/*rr          8*/
      call vdac_read (iu)                                                       !/*rrx         7*/
      return                                                                    !/*rr         13*/
!                                                                               !/*rr         14*/
!        set error flag for end of file.                                        !/*rr         15*/
   10 ie=1                                                                      !/*rr         16*/
      return                                                                    !/*rr         17*/
      end                                                                       !/*rr         18*/
