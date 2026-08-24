!#ident brem.F from deck bm                                                     !/*bm          1*/
      subroutine brem                                                           !/*bm          2*/
!        calculate bremsstrahlung probabilities and spectra.                    !/*bm          3*/
      use global_data                                                           !/*bmx         1*/
!                                                                               !/*bmx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*bmx         3*/
      implicit integer(kindi) (i-n)                                             !/*bmx         4*/
!                                                                               !/*bmx         5*/
      real(kindr), allocatable :: t_array(:)                                    !/*bmx         6*/
      parameter (pf=2.*pie/fscon)                                               !/*bm          5*/
      dimension ek(11),g(11),sw(13),tq(13),sv(2),dv(2)                          !/*bm          6*/
      data tq/50.,40.,30.,20.,15.,12.5,10.,8.,6.,5.,4.,3.,2./                   !/*bm          7*/
      data sw/1.,.98,.94,.855,.77,.71,.625,.52,.383,.298,.205,.103,0./          !/*bm          8*/
!                                                                               !/*bm          9*/
!        compute the unbiased cumulative probability, eba, for energy           !/*bm         10*/
!        loss fractions (photon/electron) in single bremsstrahlung              !/*bm         11*/
!        events, and the bremsstrahlung cross section, pbr.                     !/*bm         12*/
!        set up dynamic memory scratch arrays in t_array.                       !/*bmx         7*/
!                                                                               !/*bm4c        1*/
!        integrated tiger series (its) method before its3.0.                    !/*bm4c        2*/
      if(nxs(lnxs+16,lme(llme+3,jmd(ljmd+mkc))).eq.3)go to 120                  !/*bm4c        3*/
      do 110 n=1,nee(3)                                                         !/*bmx         8*/
      e=eee(leee+3,n)                                                           !/*bmx         9*/
      wt=1.                                                                     !/*bm         15*/
      if(e.le.tq(13))wt=0.                                                      !/*bm         16*/
      if(e.gt.tq(13).and.e.lt.tq(1))wt=qpol(e,tq,sw,13)                         !/*bm         17*/
      s=e/gpt(3)                                                                !/*bm         18*/
      tl=log(s)                                                                 !/*bm         19*/
      eo=s+1.                                                                   !/*bm         20*/
      po=sqrt(s*(s+2.))                                                         !/*bm         21*/
      es=log(eo+po)                                                             !/*bm         22*/
      do 100 k=2,ntop                                                           !/*bm4c        4*/
      do 10 i=1,11                                                              !/*bm         24*/
      ek(i)=rkt(k)-(i-1)*(rkt(k)-rkt(k-1))*.1                                   !/*bm         25*/
   10 g(i)=0.                                                                   !/*bm         26*/
      do 80 j=jmd(ljmd+mkc),jmd(ljmd+mkc+1)-1                                   !/*bm         27*/
      ix=lme(llme+3,j)                                                          !/*bm         28*/
      z=nxs(lnxs+2,ix)                                                          !/*bm         29*/
      ze=.00232376*(1.-exp(-pf*z*eo/po))*z*(z+1.)                               !/*bm         30*/
      cr=qpol(tl,exs(jxs(ljxs+5,ix)),exs(jxs(ljxs+5,ix)+                        !/*bm         31*/
     & nxs(lnxs+5,ix)),nxs(lnxs+5,ix))*fme(lfme+j)                              !/*bm         32*/
      if(e.ge.1.)go to 40                                                       !/*bm         33*/
      dv(1)=2.*exs(jxs(ljxs+1,ix)+2)-1.                                         !/*bm         34*/
      dv(2)=exs(jxs(ljxs+1,ix)+2)                                               !/*bm         35*/
      do 30 i=1,11                                                              !/*bm         36*/
      if(ek(i).le.dv(2))go to 60                                                !/*bm         37*/
      if(i.ge.2)go to 30                                                        !/*bm         38*/
      do 20 l=1,2                                                               !/*bm         39*/
   20 sv(l)=cr*xsec(s,dv(l)*s,wt,eo,po,es,z,ze,exs(jxs(ljxs+8,ix)))             !/*bm         40*/
   30 g(i)=g(i)+((sv(1)-sv(2))*ek(i)+sv(2)*dv(1)-sv(1)*dv(2))/                  !/*bm         41*/
     & (dv(1)-dv(2))                                                            !/*bm         42*/
      go to 80                                                                  !/*bm         43*/
   40 d=exs(jxs(ljxs+1,ix)+3)                                                   !/*bm         44*/
      do 50 i=1,11                                                              !/*bm         45*/
      if(ek(i).le.d)go to 60                                                    !/*bm         46*/
      if(i.ge.2)go to 50                                                        !/*bm         47*/
      s1=cr*xsec(s,d*s,wt,eo,po,es,z,ze,exs(jxs(ljxs+8,ix)))                    !/*bm         48*/
      a=exp(qpol(tl,exs(jxs(ljxs+6,ix)),exs(jxs(ljxs+6,ix)+                     !/*bm         49*/
     & nxs(lnxs+6,ix)),nxs(lnxs+6,ix)))*fme(lfme+j)                             !/*bm         50*/
   50 g(i)=g(i)+((s1-a)*ek(i)+a*d-s1)/(d-1.)                                    !/*bm         51*/
      go to 80                                                                  !/*bm         52*/
   60 do 70 i=i,11                                                              !/*bm         53*/
   70 g(i)=g(i)+cr*xsec(s,ek(i)*s,wt,eo,po,es,z,ze,exs(jxs(ljxs+8,ix)))         !/*bm         54*/
   80 continue                                                                  !/*bm         55*/
      do 90 i=1,11                                                              !/*bm         56*/
   90 g(i)=g(i)/ek(i)                                                           !/*bm         57*/
  100 eba(leba+k,n+nee(3)*(mkc-1))=(g(1)+4.*(g(2)+g(4)+g(6)+g(8)+g(10))+        !/*bmx        10*/
     & 2.*(g(3)+g(5)+g(7)+g(9))+g(11))*(rkt(k)-rkt(k-1))+                       !/*bm         59*/
     & eba(leba+k-1,n+nee(3)*(mkc-1))                                           !/*bmx        11*/
      pbr(lpbr+n+nee(3)*(mkc-1))=eba(leba+ntop,n+nee(3)*(mkc-1))/30.            !/*bmx        12*/
!                                                                               !/*bm         62*/
!        normalize the cumulative unbiased probability, eba.                    !/*bm         63*/
      do 110 k=2,ntop                                                           !/*bm4c        6*/
  110 eba(leba+k,n+nee(3)*(mkc-1))=eba(leba+k,n+nee(3)*(mkc-1))/                !/*bmx        13*/
     & eba(leba+ntop,n+nee(3)*(mkc-1))                                          !/*bmx        14*/
      go to 210                                                                 !/*bm4c        8*/
!                                                                               !/*bm4c        9*/
!        integrated tiger series (its) method after its3.0.                     !/*bm4c       10*/
!                                                                               !/*bm4c       11*/
!        set up dynamic memory scratch arrays in tal.                           !/*bm4c       12*/
!        use the values for the first zaid since they are equal.                !/*bm4c       13*/
  120 ix=lme(llme+3,jmd(ljmd+mkc))                                              !/*bm4c       14*/
      nm=nxs(lnxs+5,ix)                                                         !/*bm4c       15*/
      km=nxs(lnxs+6,ix)                                                         !/*bm4c       16*/
      l0=0                                                                      !/*bmx        15*/
      l1=l0+km                                                                  !/*bm4c       18*/
      l2=l1+nm                                                                  !/*bm4c       19*/
      l3=l2+nm*km                                                               !/*bm4c       20*/
      l5=l0+km+nm+(nm+nee(3))*km                                                !/*bmx        16*/
      m8=max(nm,km)                                                             !/*bm4c       22*/
      m9=km+nm+(nm+nee(3))*km+2*m8                                              !/*bmx        17*/
      allocate (t_array(m9), stat=is)                                           !/*bmx        18*/
      if(is.ne.0)call expire(0,'brem','error getting memory')                   !/*bmx        19*/
      t_array=0.                                                                !/*bmx        20*/
!                                                                               !/*bm4c       28*/
!        bremsstrahlung production cross sections based upon the                !/*bm4c       29*/
!        evaluation described in:                                               !/*bm4c       30*/
!        "cross sections for bremsstrahlung production and electron-            !/*bm4c       31*/
!        impact ionization," s. m. selzter, in monte carlo transport            !/*bm4c       32*/
!        of electrons and photons, t. m. jenkins, w. r. nelson, and             !/*bm4c       33*/
!        a. rindi, eds.  algorithm implementation based upon its3.0             !/*bm4c       34*/
!        and fit values from its3.0 database.                                   !/*bm4c       35*/
      do 140 j=jmd(ljmd+mkc),jmd(ljmd+mkc+1)-1                                  !/*bm4c       36*/
      ix=lme(llme+3,j)                                                          !/*bm4c       37*/
      z=nxs(lnxs+2,ix)                                                          !/*bm4c       38*/
      do 140 n=1,nm                                                             !/*bm4c       39*/
      t_array(l1+n)=log(exs(jxs(ljxs+5,ix)+n-1))                                !/*bmx        21*/
      do 140 k=1,km                                                             !/*bm4c       41*/
  140 t_array(l2+n+(k-1)*nm)=t_array(l2+n+(k-1)*nm)+                            !/*bmx        22*/
     & z*z*exs(jxs(ljxs+5,ix)+km+n-1+k*nm)*fme(lfme+j)                          !/*bm4c       43*/
      oe=one+1.d-6                                                              !/*bm4c       44*/
      do 150 k=1,km                                                             !/*bm4c       45*/
      t_array(l0+k)=log(oe-exs(jxs(ljxs+5,ix)+nm+k-1))                          !/*bmx        23*/
      do 150 n=1,nm                                                             !/*bm4c       47*/
  150 t_array(l2+n+(k-1)*nm)=log(t_array(l2+n+(k-1)*nm))                        !/*bmx        24*/
      do 160 k=1,km                                                             !/*bm4c       49*/
      in=1                                                                      !/*bm4c       50*/
      do 160 n=1,nee(3)                                                         !/*bmx        25*/
      tl=log(eee(leee+3,n))                                                     !/*bmx        26*/
      bl=log(one-(gpt(3)/(eee(leee+3,n)+gpt(3)))**2)                            !/*bmx        27*/
  160 t_array(l3+k+(n-1)*km)=spol(tl,t_array(l1+1),t_array(l2+1+(k-1)           !/*bmx        28*/
     & *nm),t_array(l5+1),t_array(l5+m8+1),nm,in,id,m1,m2)-bl                   !/*bmx        29*/
      do 200 n=1,nee(3)                                                         !/*bmx        30*/
      eba(leba+1,n+nee(3)*(mkc-1))=0.                                           !/*bmx        31*/
      in=1                                                                      !/*bm4c       58*/
      do 190 k=2,ntop                                                           !/*bm4c       59*/
      de=rkt(k)-rkt(k-1)                                                        !/*bm4c       60*/
      do 170 np=1,11                                                            !/*bm4c       61*/
      ek(np)=rkt(k)-.1d0*(np-1)*de                                              !/*bm4c       62*/
      xp=log(oe-ek(np))                                                         !/*bm4c       63*/
  170 g(np)=exp(spol(xp,t_array(l0+1),t_array(l3+1+(n-1)*km),                   !/*bmx        32*/
     & t_array(l5+1),t_array(l5+m8+1),km,in,id,m1,m2))                          !/*bmx        33*/
      do 180 i=1,11                                                             !/*bm4c       66*/
  180 g(i)=g(i)/ek(i)                                                           !/*bm4c       67*/
  190 eba(leba+k,n+nee(3)*(mkc-1))=                                             !/*bmx        34*/
     & (g(1)+4.d0*(g(2)+g(4)+g(6)+g(8)+g(10))+2.d0*                             !/*bmx        35*/
     & (g(3)+g(5)+g(7)+g(9))+g(11))*de+eba(leba+k-1,n+nee(3)*(mkc-1))           !/*bmx        36*/
!                                                                               !/*bm4c       70*/
!        30=3*(number of integration points -1)                                 !/*bm4c       71*/
      pbr(lpbr+n+nee(3)*(mkc-1))=eba(leba+ntop,n+nee(3)*(mkc-1))/30.d0          !/*bmx        37*/
!                                                                               !/*bm4c       73*/
!        normalize the cumulative unbiased probability, eba.                    !/*bm4c       74*/
      do 200 k=2,ntop                                                           !/*bm4c       75*/
  200 eba(leba+k,n+nee(3)*(mkc-1))=eba(leba+k,n+nee(3)*(mkc-1))/                !/*bmx        38*/
     & eba(leba+ntop,n+nee(3)*(mkc-1))                                          !/*bmx        39*/
!                                                                               !/*bm         67*/
!        integrate bremsstrahlung probabilities and spectra for the             !/*bm         68*/
!        thick-target bremsstrahlung approximation.                             !/*bm         69*/
  210 do 230 nz=1,nee(3)-1                                                      !/*bmx        40*/
      n=nee(3)-nz                                                               !/*bmx        41*/
      pbt(lpbt+n+nee(3)*(mkc-1))=pbt(lpbt+n+1+nee(3)*(mkc-1))+                  !/*bmx        42*/
     & .5*(pbr(lpbr+n+nee(3)*(mkc-1))+pbr(lpbr+n+1+nee(3)*(mkc-1)))*            !/*bmx        43*/
     & (rng(lrng+3,n+nee_max*(mkc-1))-rng(lrng+3,n+1+nee_max*(mkc-1)))          !/*bmx        44*/
      do 220 k=2,ntop                                                           !/*bm4c       79*/
      e=rkt(k)*eee(leee+3,n)                                                    !/*bmx        45*/
      do 220 j=n,nee(3)-1                                                       !/*bmx        46*/
      q=1.                                                                      !/*bm         78*/
      if(e.lt.eee(leee+3,j))                                                    !/*bmx        47*/
     & q=qpol(e/eee(leee+3,j),rkt,eba(leba+1,j+nee(3)*(mkc-1)),ntop)            !/*bmx        48*/
  220 ebt(lebt+k,n+nee(3)*(mkc-1))=ebt(lebt+k,n+nee(3)*(mkc-1))+                !/*bmx        49*/
     & q*.5*(pbr(lpbr+j+nee(3)*(mkc-1))+pbr(lpbr+j+1+nee(3)*(mkc-1)))*          !/*bmx        50*/
     & (rng(lrng+3,j+nee_max*(mkc-1))-rng(lrng+3,j+1+nee_max*(mkc-1)))          !/*bmx        51*/
!                                                                               !/*bm         84*/
!        normalize the ttb cumulative unbiased probability, ebt.                !/*bm         85*/
      do 230 k=2,ntop                                                           !/*bm4c       83*/
  230 ebt(lebt+k,n+nee(3)*(mkc-1))=ebt(lebt+k,n+nee(3)*(mkc-1))/                !/*bmx        52*/
     & ebt(lebt+ntop,n+nee(3)*(mkc-1))                                          !/*bmx        53*/
      do 240 k=1,ntop                                                           !/*bm4c       86*/
  240 ebt(lebt+k,nee(3)*mkc)=ebt(lebt+k,nee(3)*mkc-1)                           !/*bmx        54*/
!                                                                               !/*bm         91*/
!        set up the bbrem card bremsstrahlung energy biasing scheme.            !/*bm         92*/
      if (allocated( t_array)) deallocate (t_array)                             !/*bmx        55*/
      if(mbi(lmbi+mkc).eq.0)return                                              !/*bm         93*/
      do 280 m=1,nee(3)                                                         !/*bmx        56*/
!                                                                               !/*bm         95*/
!        first, bias and normalize the cumulative probability, ebd.             !/*bm         96*/
      f=0.                                                                      !/*bm         97*/
      do 250 k=2,ntop                                                           !/*bm4c       89*/
      d=eba(leba+k,m+nee(3)*(mkc-1))-eba(leba+k-1,m+nee(3)*(mkc-1))             !/*bmx        57*/
      ebd(lebd+k-1,m+nee(3)*(mkc-1))=f                                          !/*bmx        58*/
  250 f=f+d/bbrem(k)                                                            !/*bm4c       90*/
      fst(lfst+m+nee(3)*(mkc-1))=f                                              !/*bmx        59*/
      do 260 k=1,ntop-1                                                         !/*bm4c       91*/
  260 ebd(lebd+k,m+nee(3)*(mkc-1))=ebd(lebd+k,m+nee(3)*(mkc-1))/f               !/*bmx        60*/
!                                                                               !/*bm        105*/
!        then, bias and normalize the ttb cumulative probability, ebt.          !/*bm        106*/
      f=0.                                                                      !/*bm        107*/
      do 270 k=2,ntop                                                           !/*bm4c       93*/
      d=ebt(lebt+k,m+nee(3)*(mkc-1))-ebt(lebt+k-1,m+nee(3)*(mkc-1))             !/*bmx        61*/
      ebt(lebt+k-1,m+nee(3)*(mkc-1))=f                                          !/*bmx        62*/
  270 f=f+d/bbrem(k)                                                            !/*bm4c       94*/
      ftt(lftt+m+nee(3)*(mkc-1))=f                                              !/*bmx        63*/
      do 280 k=1,ntop-1                                                         !/*bm4c       95*/
  280 ebt(lebt+k,m+nee(3)*(mkc-1))=ebt(lebt+k,m+nee(3)*(mkc-1))/f               !/*bmx        64*/
      return                                                                    !/*bm        115*/
      end                                                                       !/*bm        116*/
