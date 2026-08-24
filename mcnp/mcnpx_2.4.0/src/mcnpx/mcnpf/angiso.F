!#ident angiso.F from deck xai                                                /*xai         1*/
      subroutine angiso(sa)                                                     !/*xai         2*/
!        returns the scattering angle from an isotropic dist.                   !/*xai         3*/
!                                                                               !/*xai         4*/
!        Preconditions:                                                         !/*xai         5*/
!          None.                                                                !/*xai         6*/
!                                                                               !/*xai         7*/
!        Postconditions:                                                        !/*xai         8*/
!          return value sa is scattering angle mu in radians                    !/*xai         9*/
!          mu value is always between -1.0 and 1.0                              !/*xai        10*/
!                                                                               !/*xai        11*/
!          sa is a modified return value                                        !/*xai        12*/
!                                                                               !/*xai        13*/
      use global_data                                                           !/*xai        14*/
!                                                                               !/*xai        15*/
      implicit real(kindr) (a-h,o-z)                                            !/*xai        16*/
      implicit integer(kindi) (i-n)                                             !/*xai        17*/
!                                                                               !/*xai        18*/
!                                                                               !/*xai        19*/
! >>>>>  sample from isotropic cosine scattering angular distribution           !/*xai        20*/
      sa=two*rang()-one                                                         !/*xai        21*/
      return                                                                    !/*xai        22*/
      end                                                                       !/*xai        23*/
