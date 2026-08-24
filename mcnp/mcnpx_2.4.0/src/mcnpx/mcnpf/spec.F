!#ident spec.F from deck fs                                                     !/*fs          1*/
      function spec(e)                                                          !/*fs          2*/
!        source spectral functions for equiprobable bin generation.             !/*fs          3*/
!        e is the independent variable of source distribution irs.              !/*fs          4*/
!        spec is not called for pure gaussians (4 and 41).                      !/*fs          5*/
      use global_data                                                           !/*fsx         1*/
      use imcn_data                                                             !/*fsx         2*/
!                                                                               !/*fsx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*fsx         4*/
      implicit integer(kindi) (i-n)                                             !/*fsx         5*/
!                                                                               !/*fsx         6*/
      q(i)=sqq(lsqq+i,irs)                                                      !/*fs          8*/
!                                                                               !/*fs          9*/
      go to(10,20,80,30,40,50,60,70,80)ksd(lksd+2,irs)                          !/*fs         10*/
!                                                                               !/*fs         11*/
! >>>>>  spectrum 2 -- maxwell (fission) spectrum (endf law 7).                 !/*fs         12*/
   10 spec=q(3)*sqrt(e)*exp(-e/q(2))                                            !/*fs         13*/
      return                                                                    !/*fs         14*/
!                                                                               !/*fs         15*/
! >>>>>  spectrum 3 -- watt (fission) spectrum (endf law 10).                   !/*fs         16*/
   20 spec=q(7)*exp(-e/q(9))*sinh(sqrt(q(8)*e))                                 !/*fs         17*/
      return                                                                    !/*fs         18*/
!                                                                               !/*fs         19*/
! >>>>>  spectrum 5 -- evaporation spectrum (endf law 9)                        !/*fs         20*/
   30 spec=q(3)*e*exp(-e/q(2))                                                  !/*fs         21*/
      return                                                                    !/*fs         22*/
!                                                                               !/*fs         23*/
! >>>>>  spectrum 6 -- modified gaussian (fusion) spectrum.                     !/*fs         24*/
   40 spec=q(4)*exp(-min(((sqrt(e)-q(3))/q(2))**2,zero+25.))                    !/*fs         25*/
      return                                                                    !/*fs         26*/
!                                                                               !/*fs         27*/
! >>>>>  spectrum 7 -- spare.                                                   !/*fs         28*/
   50 spec=0.                                                                   !/*fs4b        1*/
      return                                                                    !/*fs4b        2*/
!                                                                               !/*fs         30*/
! >>>>>  function 21 -- power law.                                              !/*fs         31*/
   60 spec=(q(5)+1.)*abs(e)**q(5)/q(9)                                          !/*fs         32*/
      return                                                                    !/*fs         33*/
!                                                                               !/*fs         34*/
! >>>>>  function 31 -- exponential.                                            !/*fs         35*/
   70 spec=1./q(9)                                                              !/*fs         36*/
      if(q(5).ne.0.)spec=spec*q(5)*exp(q(5)*e)                                  !/*fs         37*/
      return                                                                    !/*fs         38*/
!                                                                               !/*fs         39*/
   80 spec=0.                                                                   !/*fs4b        3*/
      return                                                                    !/*fs4b        4*/
      end                                                                       !/*fs         41*/
