!     $Id: cut_mc.F,v 1.4 2002/03/27 18:14:59 gwm Exp $                       !/*cu          1*/
      function cut_mc(lp)                                                       !/*cu          2*/
      use global_data                                                           !/*cuxc        1*/
!                                                                               !/*cuxc        2*/
      implicit real(kindr) (a-h,o-z)                                            !/*cuxc        3*/
      implicit integer(kindi) (i-n)                                             !/*cuxc        4*/
!                                                                               !/*cuxc        5*/
      call mtran(lp,ip,ja)                                                      !/*cu          4*/
      cut_mc=ecf(ip)                                                            !/*cu          5*/
      return                                                                    !/*cu          6*/
      end                                                                       !/*cu          7*/
