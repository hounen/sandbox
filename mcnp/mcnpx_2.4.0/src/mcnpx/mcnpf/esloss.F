!#ident esloss.F from deck ls                                                   !/*ls          1*/
      function esloss(nq)                                                       !/*ls4b        1*/
!        sample the electron energy loss rate, with optional straggling.        !/*ls          3*/
      use global_data                                                           !/*lsx         1*/
      use lambda_bins                                                           !/*lsx         2*/
!                                                                               !/*lsx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*lsx         4*/
      implicit integer(kindi) (i-n)                                             !/*lsx         5*/
!                                                                               !/*lsx         6*/
      parameter (bs=euler-1.5d+0)                                               !/*ls4b        3*/
      parameter (bc=.989571788d+0,h2=2.08643410365460141d-2)                    !/*ls4b        4*/
!       bc = probability that lambda.lt.100., sampling landau function.         !/*ls4b        5*/
!       h2 =  2/f, with f = integral of borsch-supan asymptotic form.           !/*ls4b        6*/
!                                                                               !/*ls4b        7*/
      if(istrg(3).ne.1)go to 10                                                 !/*lsxa        1*/
      esloss=qav(lqav+3,nq+nee_max*(mkc-1))*rho(lrho+icl)                       !/*lsx         8*/
      return                                                                    !/*ls4b       11*/
   10 if(rang().le.bc)go to 20                                                  !/*ls4b       12*/
      w=1./(sqrt(1.+h2*rang())-1.)                                              !/*ls4b       13*/
      fl=w+log(w)+bs                                                            !/*ls4b       14*/
      go to 30                                                                  !/*ls4b       15*/
   20 f=(mlam-1)*rang()                                                         !/*ls4b       16*/
      j=f                                                                       !/*ls4b       17*/
      fl=eqlm(j+1)+(f-j)*(eqlm(j+2)-eqlm(j+1))                                  !/*ls4b       18*/
   30 if(fl.gt.flc(lflc+nq+nee(3)*(mkc-1)))go to 10                             !/*lsx         9*/
   40 t1=2.*rang()-1.                                                           !/*ls4b       20*/
      t2=t1**2+rang()**2                                                        !/*ls4b       21*/
      if(t2.gt.1.)go to 40                                                      !/*ls4b       22*/
      q=qav(lqav+3,nq+nee_max*(mkc-1))+                                         !/*lsx        10*/
     & asp(lasp+nq+nee(3)*(mkc-1))*(fl-ear(lear+nq+nee(3)*(mkc-1)))+            !/*lsx        11*/
     & qcn(lqcn+nq+nee(3)*(mkc-1))*sqrt(-2.*log(t2)/t2)*t1                      !/*lsx        12*/
      if(q*drs(ldrs+3,nq+nee_max*(mkc-1))*nsb(lnsb+mkc).ge.erg)go to 10         !/*lsx        13*/
      if(q.lt.0.)q=0.                                                           !/*ls4b       27*/
      esloss=q*rho(lrho+icl)                                                    !/*ls         96*/
      return                                                                    !/*ls         97*/
      end                                                                       !/*ls         98*/
