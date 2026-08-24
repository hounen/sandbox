!#ident GKSSIM_gs.F from deck gks                                               !/*gks         1*/
      module gkssim_data                                                        !/*gks         2*/
!        variables and arrays for gks simulation subroutines.                   !/*gks         3*/
!                                                                               !/*gks         4*/
      use global_parameters, only : kindi,kindr                                 !/*gks         5*/
      implicit none                                                             !/*gks         6*/
      save                                                                      !/*gks         7*/
      public                                                                    !/*gks         8*/
!                                                                               !/*gks         9*/
!                                                                               !/*gks        10*/
!***********************************************************************        !/*gks        11*/
!        integer variables and arrays for gks simulation subroutines.           !/*gks        12*/
!                                                                               !/*gks        13*/
      integer(kindi) :: icol                                                    !/*gks        14*/
      integer(kindi) :: jta(2)                                                  !/*gks        15*/
      integer(kindi) :: kls                                                     !/*gks        16*/
      integer(kindi) :: lvd                                                     !/*gks        17*/
      integer(kindi) :: ltype                                                   !/*gks        18*/
      integer(kindi) :: munit                                                   !/*gks        19*/
      integer(kindi) :: nctext                                                  !/*gks        20*/
      integer(kindi) :: nltext                                                  !/*gks        21*/
      integer(kindi) :: npages                                                  !/*gks        22*/
      integer(kindi) :: nxnorm                                                  !/*gks        23*/
      integer(kindi) :: nynorm                                                  !/*gks        24*/
!                                                                               !/*gks        25*/
!***********************************************************************        !/*gks        26*/
!        real variables and arrays for gks simulation subroutines.              !/*gks        27*/
!                                                                               !/*gks        28*/
      real(kindr) :: als                                                        !/*gks        29*/
      real(kindr) :: chite(5)                                                   !/*gks        30*/
      real(kindr) :: chup(2)                                                    !/*gks        31*/
      real(kindr) :: psize(4)                                                   !/*gks        32*/
      real(kindr) :: rgb(3000)                                                  !/*gksk        1*/
      real(kindr) :: wsf                                                        !/*gks        34*/
      real(kindr) :: xlf                                                        !/*gks        35*/
      real(kindr) :: xrt                                                        !/*gks        36*/
      real(kindr) :: ybt                                                        !/*gks        37*/
      real(kindr) :: ytp                                                        !/*gks        38*/
!                                                                               !/*gks        39*/
      end module gkssim_data                                                    !/*gks        40*/
