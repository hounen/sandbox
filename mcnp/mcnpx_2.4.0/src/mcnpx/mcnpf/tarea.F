!#ident tarea.F from deck ta                                                    !/*ta          1*/
      function tarea(t0)                                                        !/*ta          2*/
!        this routine provides the function for the integration scheme          !/*ta          3*/
!        qnc7 which is used to compute the surface area of a torus.             !/*ta          4*/
      use global_data                                                           !/*tax         1*/
      use imcn_data                                                             !/*tax         2*/
!                                                                               !/*tax         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*tax         4*/
      implicit integer(kindi) (i-n)                                             !/*tax         5*/
!                                                                               !/*tax         6*/
!                                                                               !/*ta          7*/
      tarea=sqrt(1.+ajsh*cos(t0)**2)                                            !/*ta          8*/
      return                                                                    !/*ta          9*/
      end                                                                       !/*ta         10*/
