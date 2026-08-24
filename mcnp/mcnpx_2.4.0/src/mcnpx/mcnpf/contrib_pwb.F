!#ident contrib_pwb.F from deck pwb                                             !/*pwb         1*/
      subroutine contrib_pwb(ip,nb,ic,wt)                                       !/*pwb         2*/
!        ip = identifier for particle type.                                     !/*pwb         3*/
!        nb = identifier for particle weight balance category.                  !/*pwb         4*/
!        ic = geometry cell.                                                    !/*pwb         5*/
!        wt = weight gained or lost.                                            !/*pwb         6*/
      use global_data                                                           !/*pwb         7*/
!                                                                               !/*pwb         8*/
      implicit real(kindr) (a-h,o-z)                                            !/*pwb         9*/
      implicit integer(kindi) (i-n)                                             !/*pwb        10*/
!                                                                               !/*pwb        11*/
      pwb(kpwb+ip,nb,ic) = pwb(kpwb+ip,nb,ic) + wt                              !/*pwb        15*/
      return                                                                    !/*pwb        16*/
      end                                                                       !/*pwb        17*/
