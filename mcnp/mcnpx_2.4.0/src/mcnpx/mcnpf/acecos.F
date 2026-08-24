!#ident acecos.F from deck ao                                                   !/*ao          1*/
      function acecos(ia,ka)                                                    !/*ao4c        1*/
!        sample a cosine from the distribution at ia,ka.  return the            !/*ao4c        2*/
!        cosine (acecos) and the index in xss of the first word of the          !/*ao4c        3*/
!        cosine table used (ixcos). ka=0: isotropic. ka<0: tabular only.        !/*ao4c        4*/
!        32-equiprobable bin cosine data:                                       !/*ao4c        5*/
!           ne,(e(i),i=1,ne),(lmu(i),i=1,ne),(mu(lmu+j),j=1,33)                 !/*ao4c        6*/
!        tabular probability angular distribution data:                         !/*ao4c        7*/
!           ne,(e(i),i=1,ne),(lmu(i),i=1,ne),jj,np,                             !/*ao4c        8*/
!           (mu(lmu+j),j=1,np),(pdf(lmu+j),j=1,np),(cdf(lmu+j),j=1,np)          !/*ao4c        9*/
!           where jj is the interpolation flag at lmu(i).                       !/*ao4c       10*/
      use global_data                                                           !/*aox         1*/
!                                                                               !/*aox         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*aox         3*/
      implicit integer(kindi) (i-n)                                             !/*aox         4*/
!                                                                               !/*aox         5*/
!                                                                               !/*ao         12*/
!        find the cosine table by binary search on the energy table.            !/*ao         13*/
      if(ka.eq.0)go to 40                                                       !/*ao4c       12*/
      lm=ka                                                                     !/*ao4c       13*/
      if(ka.lt.0)go to 50                                                       !/*ao4c       14*/
      ic=ia+ka                                                                  !/*ao4c       15*/
      n=nint(xss(ic-1))                                                         !/*ao4c       16*/
      ib=ic-1+n                                                                 !/*ao         16*/
   10 if(ib-ic.eq.1)go to 30                                                    !/*ao         17*/
      ih=(ic+ib)/2                                                              !/*ao         18*/
      if(erg.lt.xss(ih))go to 20                                                !/*ao         19*/
      ic=ih                                                                     !/*ao         20*/
      go to 10                                                                  !/*ao         21*/
   20 ib=ih                                                                     !/*ao         22*/
      go to 10                                                                  !/*ao         23*/
!                                                                               !/*ao         24*/
!        sample between adjoining tables by interpolation fraction.             !/*ao         25*/
   30 if(rang()*(xss(ib)-xss(ic)).lt.erg-xss(ic))ic=ib                          !/*ao         26*/
      lm=nint(xss(ic+n))                                                        !/*ao4c       17*/
      if(lm.eq.0)go to 40                                                       !/*ao4c       18*/
      if(lm.lt.0)go to 50                                                       !/*ao4c       19*/
!                                                                               !/*ao         28*/
!        sample from table of 32 equiprobable cosine groups.                    !/*ao         29*/
      t1=rang()*32.                                                             !/*ao         30*/
      kr=t1                                                                     !/*ao         31*/
      ixcos=ia-1+lm                                                             !/*ao4c       20*/
      acecos=xss(ixcos+kr)+(t1-kr)*(xss(ixcos+kr+1)-xss(ixcos+kr))              !/*ao4c       21*/
      return                                                                    !/*ao         34*/
!                                                                               !/*ao         35*/
!        isotropic case                                                         !/*ao         36*/
   40 acecos=2.*rang()-1.                                                       !/*ao         37*/
      ixcos=0                                                                   !/*ao4c       22*/
      return                                                                    !/*ao4c       23*/
!                                                                               !/*ao4c       24*/
!        tabular probability angular distribution.                              !/*ao4c       25*/
   50 k=ia-1-lm                                                                 !/*ao4c       26*/
      ixcos=-k                                                                  !/*ao4c       27*/
      jj=nint(xss(k))                                                           !/*ao4c       28*/
      np=nint(xss(k+1))                                                         !/*ao4c       29*/
      rn=rang()                                                                 !/*ao4c       30*/
!                                                                               !/*ao4c       31*/
!        binary search of cumulative density function.                          !/*ao4c       32*/
      ic=k+2*np+2                                                               !/*ao4c       33*/
      ib=k+3*np+1                                                               !/*ao4c       34*/
   60 if(ib-ic.eq.1)go to 80                                                    !/*ao4c       35*/
      ih=(ic+ib)/2                                                              !/*ao4c       36*/
      if(rn.lt.xss(ih))go to 70                                                 !/*ao4c       37*/
      ic=ih                                                                     !/*ao4c       38*/
      go to 60                                                                  !/*ao4c       39*/
   70 ib=ih                                                                     !/*ao4c       40*/
      go to 60                                                                  !/*ao4c       41*/
!                                                                               !/*ao4c       42*/
   80 fa=xss(ic-np)                                                             !/*ao4c       43*/
      ca=xss(ic-2*np)                                                           !/*ao4c       44*/
      acecos=ca+(rn-xss(ic))/fa                                                 !/*ao4c       45*/
      if(jj.eq.1)return                                                         !/*ao4c       46*/
      bb=(xss(ic-np+1)-fa)/(xss(ic-2*np+1)-ca)                                  !/*ao4c       47*/
      if(bb.ne.0.)                                                              !/*ao4c       48*/
     & acecos=ca+(sqrt(max(zero,fa**2+2.*bb*(rn-xss(ic))))-fa)/bb               !/*ao4c       49*/
      return                                                                    !/*ao         39*/
      end                                                                       !/*ao         40*/
