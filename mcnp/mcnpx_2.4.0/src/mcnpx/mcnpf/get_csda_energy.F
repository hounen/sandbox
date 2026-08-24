!#ident get_csda_energy.F from deck ge                                          !/*ge          1*/
      function get_csda_energy (e0, d)                                          !/*ge          2*/
!                                                                               !/*ge          3*/
!     Starting at energy E0, travel a distance D, assuming the                  !/*ge          4*/
!     Continuous-Slowing-Down-Approximation in histogram energy                 !/*ge          5*/
!     bins, and report the final energy, or the energy cutoff,                  !/*ge          6*/
!     if the final energy would have been lower than the cutoff.                !/*ge          7*/
!                                                                               !/*ge          8*/
!     Assume that E0 lies within the energy group NG.                           !/*ge          9*/
!                                                                               !/*ge         10*/
!     Assume also that NGP, IPT, MKC, and ICL have been properly                !/*ge         11*/
!     set be the calling routine.                                               !/*ge         12*/
!                                                                               !/*ge         13*/
!     The input arguments are not changed by the function.                      !/*ge         14*/
!                                                                               !/*ge         15*/
      use global_data                                                           !/*gexc        1*/
!                                                                               !/*gexc        2*/
      implicit real(kindr) (a-h,o-z)                                            !/*gexc        3*/
      implicit integer(kindi) (i-n)                                             !/*gexc        4*/
!                                                                               !/*ge         17*/
      e = e0                                                                    !/*ge         18*/
      d_left_local = d                                                          !/*ge         19*/
      qs_csda_loop_local =                                                      !/*ge         20*/
     &       qav(lqav+ipt,ngp+nee_max*(mkc-1))*rho(lrho+icl)                    !/*ge         21*/
      do nx = ngp+1, nee(ipt)-2                                                 !/*ge         22*/
         d_next_ngp_local =                                                     !/*ge         23*/
     &          (e-eee(leee+ipt,nx))/qs_csda_loop_local                         !/*ge         24*/
         if (d_left_local.le.d_next_ngp_local) then                             !/*ge         25*/
            e = e - qs_csda_loop_local*d_left_local                             !/*ge         26*/
            go to 99                                                            !/*ge         27*/
         else                                                                   !/*ge         28*/
            d_left_local = d_left_local - d_next_ngp_local                      !/*ge         29*/
            e = eee(leee+ipt,nx)                                                !/*ge         30*/
            qs_csda_loop_local =                                                !/*ge         31*/
     &             qav(lqav+ipt,nx+nee_max*(mkc-1))*rho(lrho+icl)               !/*ge         32*/
         endif                                                                  !/*ge         33*/
      enddo                                                                     !/*ge         34*/
!                                                                               !/*ge         35*/
!     Special logic for group containing energy cutoff.                         !/*ge         36*/
      d_ecut_local = (e-elc(ipt))/qs_csda_loop_local                            !/*ge         37*/
      if (d_left_local.le.d_ecut_local) then                                    !/*ge         38*/
         e = e - qs_csda_loop_local*d_left_local                                !/*ge         39*/
      else                                                                      !/*ge         40*/
         e = elc(ipt)                                                           !/*ge         41*/
      endif                                                                     !/*ge         42*/
!                                                                               !/*ge         43*/
   99 get_csda_energy = e                                                       !/*ge         44*/
      return                                                                    !/*ge         45*/
      end                                                                       !/*ge         46*/
