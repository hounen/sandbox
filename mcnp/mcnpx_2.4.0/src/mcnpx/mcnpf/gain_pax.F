!#ident gain_pax.F from deck gpx                                                !/*gpx         1*/
      subroutine gain_pax (nx,ip,nt,wt,eg)                                      !/*gpx         2*/
!        nx = identifier for summary table gain category.                       !/*gpx         3*/
!        ip = identifier for particle type.                                     !/*gpx         4*/
!        nt = number of tracks gained.                                          !/*gpx         5*/
!        wt = weight gained.                                                    !/*gpx         6*/
!        eg = energy*weight gained.                                             !/*gpx         7*/
      use global_data                                                           !/*gpx         8*/
!                                                                               !/*gpx         9*/
      implicit real(kindr) (a-h,o-z)                                            !/*gpx        10*/
      implicit integer(kindi) (i-n)                                             !/*gpx        11*/
!                                                                               !/*gpx        12*/
      paxtc(1,nx,ip) = paxtc(1,nx,ip) + nt                                      !/*gpx        17*/
      paxtc(2,nx,ip) = paxtc(2,nx,ip) + wt                                      !/*gpx        18*/
      paxtc(3,nx,ip) = paxtc(3,nx,ip) + eg                                      !/*gpx        19*/
      return                                                                    !/*gpx        20*/
      end                                                                       !/*gpx        21*/
