!#ident lose_pax.F from deck lpx                                                !/*lpx         1*/
      subroutine lose_pax (nx,ip,nt,wt,eg)                                      !/*lpx         2*/
!        nx = identifier for summary table loss category.                       !/*lpx         3*/
!        ip = identifier for particle type.                                     !/*lpx         4*/
!        nt = number of tracks lost.                                            !/*lpx         5*/
!        wt = weight lost.                                                      !/*lpx         6*/
!        eg = energy*weight lost.                                               !/*lpx         7*/
      use global_data                                                           !/*lpx         8*/
!                                                                               !/*lpx         9*/
      implicit real(kindr) (a-h,o-z)                                            !/*lpx        10*/
      implicit integer(kindi) (i-n)                                             !/*lpx        11*/
!                                                                               !/*lpx        12*/
      paxtc(4,nx,ip) = paxtc(4,nx,ip) + nt                                      !/*lpx        17*/
      paxtc(5,nx,ip) = paxtc(5,nx,ip) + wt                                      !/*lpx        18*/
      paxtc(6,nx,ip) = paxtc(6,nx,ip) + eg                                      !/*lpx        19*/
      return                                                                    !/*lpx        20*/
      end                                                                       !/*lpx        21*/
