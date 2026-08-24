!#ident dotpro.F from deck dr4a                                                 !/*dr4a        1*/
      function dotpro(v1,v2)                                                    !/*dr4a        2*/
!        calculate the dot product = v1 . v2                                    !/*dr4a        3*/
      use global_parameters                                                     !/*drx         1*/
      implicit real(kindr) (a-h,o-z)                                            !/*drx         2*/
      implicit integer(kindi) (i-n)                                             !/*drx         3*/
      dimension v1(3),v2(3)                                                     !/*dr4a        6*/
!                                                                               !/*dr4a        7*/
      dotpro=v1(1)*v2(1)+v1(2)*v2(2)+v1(3)*v2(3)                                !/*dr4a        8*/
      return                                                                    !/*dr4a        9*/
      end                                                                       !/*dr4a       10*/
