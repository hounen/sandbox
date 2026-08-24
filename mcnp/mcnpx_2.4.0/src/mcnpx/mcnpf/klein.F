!#ident klein.F from deck kl                                                    !/*kl          1*/
      subroutine klein(e1,e2,cs)                                                !/*kl          2*/
!        sample the klein-nishina distribution.                                 !/*kl          3*/
!        e1 = energy in.  e2 = energy out.  cs = scattering cosine.             !/*kl          4*/
!        the unit of energy is the rest mass of the electron.                   !/*kl          5*/
!        reference: r. n. blomquist and e. m. gelbard, nuclear science          !/*kl          6*/
!        and engineering, 83, 380-384 (1983)                                    !/*kl          7*/
      use global_parameters                                                     !/*klx         1*/
      implicit real(kindr) (a-h,o-z)                                            !/*klx         2*/
      implicit integer(kindi) (i-n)                                             !/*klx         3*/
!                                                                               !/*kl         10*/
      a=1./e1                                                                   !/*kl         11*/
      b=1./(1.+2.*e1)                                                           !/*kl         12*/
!                                                                               !/*kl         13*/
!        kahn's method (e < 1.5 mev) aecu-3259                                  !/*kl         14*/
      if(e1.ge.3.)go to 30                                                      !/*kl         15*/
      t=1./(1.+8.*b)                                                            !/*kl         16*/
   10 if(rang().le.t)go to 20                                                   !/*kl         17*/
      s=(1.+2.*e1*rang())*b                                                     !/*kl         18*/
      cs=1.+a*(1.-1./s)                                                         !/*kl         19*/
      if(rang().gt..5*(cs**2+s))go to 10                                        !/*kl         20*/
      e2=e1*s                                                                   !/*kl         21*/
      return                                                                    !/*kl         22*/
   20 r=2.*rang()                                                               !/*kl         23*/
      s=1./(1.+e1*r)                                                            !/*kl         24*/
      if(rang().gt.4.*s*(1.-s))go to 10                                         !/*kl         25*/
      e2=e1*s                                                                   !/*kl         26*/
      cs=1.-r                                                                   !/*kl         27*/
      return                                                                    !/*kl         28*/
!                                                                               !/*kl         29*/
!        koblinger's method (e > 1.5 mev) nse 56,218(1975)                      !/*kl         30*/
   30 t=rang()*(4.*a+.5*(1.-b**2)-(1.-2.*(1.+e1)*a**2)*log(b))                  !/*kl         31*/
      if(t.gt.2.*a)go to 40                                                     !/*kl         32*/
      r=2.*rang()                                                               !/*kl         33*/
      cs=1.-r                                                                   !/*kl         34*/
      e2=1./(a+r)                                                               !/*kl         35*/
      return                                                                    !/*kl         36*/
   40 if(t.gt.4.*a)go to 50                                                     !/*kl         37*/
      e2=e1*(1.+rang()*(b-1.))                                                  !/*kl         38*/
      cs=1.+a-1./e2                                                             !/*kl         39*/
      return                                                                    !/*kl         40*/
   50 if(t.gt.4.*a+.5*(1.-b**2))go to 60                                        !/*kl         41*/
      e2=e1*sqrt(1.-rang()*(1.-b**2))                                           !/*kl         42*/
      cs=1.+a-1./e2                                                             !/*kl         43*/
      return                                                                    !/*kl         44*/
   60 e2=e1*b**rang()                                                           !/*kl         45*/
      cs=1.+a-1./e2                                                             !/*kl         46*/
      return                                                                    !/*kl         47*/
      end                                                                       !/*kl         48*/
