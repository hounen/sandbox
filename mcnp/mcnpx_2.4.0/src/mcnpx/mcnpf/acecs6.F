!#ident acecs6.F from deck ae4a                                                 !/*ae4a        1*/
      function acecs6(ii,id,iw,jc,r,cs)                                         !/*ae4c        1*/
!        sample energy acecs6 given angle cs, law 67 (endf/b-vi law 7).         !/*ae4a        3*/
!        ii=0 acecs6 called from acecas (transport): save id,iw,jc,r            !/*ae4c        2*/
!             and random numbers generated in sampling energy;                  !/*ae4a        5*/
!        ii=1 acecs6 called from calcps (next-event estimator):                 !/*ae4a        6*/
!             use id,iw,jc,r, and random numbers saved from transport.          !/*ae4c        3*/
      use global_data                                                           !/*aex         1*/
!                                                                               !/*aex         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*aex         3*/
      implicit integer(kindi) (i-n)                                             !/*aex         4*/
!                                                                               !/*aex         5*/
      dimension el(2),eh(2),lx(2)                                               !/*ae4c        4*/
      t=0.                                                                      !/*ae4c        5*/
      if(ii.ne.0)go to 10                                                       !/*ae4a       11*/
      id0=id                                                                    !/*ae4c        6*/
      iw0=iw                                                                    !/*ae4a       12*/
      ic0=jc                                                                    !/*ae4a       13*/
      rr0=r                                                                     !/*ae4a       14*/
!                                                                               !/*ae4a       15*/
!        go to appropriate table for incident energy                            !/*ae4a       16*/
   10 jw=iw0+2*nint(xss(iw0))+1                                                 !/*ae4c        7*/
      ne=nint(xss(jw))                                                          !/*ae4c        8*/
      lx(1)=id0+nint(xss(jw+ne+ic0))-1                                          !/*ae4c        9*/
      lx(2)=0                                                                   !/*ae4a       20*/
      ix=1                                                                      !/*ae4a       21*/
      ir=0                                                                      !/*ae4a       22*/
      if(rr0.eq.0.)go to 20                                                     !/*ae4a       23*/
      lx(2)=id0+nint(xss(jw+ne+ic0+1))-1                                        !/*ae4c       10*/
      if(ii.eq.0)rnb(1)=rang()                                                  !/*ae4a       25*/
      ir=1                                                                      !/*ae4a       26*/
      if(rnb(1).le.rr0)ix=2                                                     !/*ae4a       27*/
   20 do 110 m=1,2                                                              !/*ae4a       28*/
      if(lx(m).eq.0)go to 110                                                   !/*ae4a       29*/
      le=lx(m)                                                                  !/*ae4a       30*/
      mu=nint(xss(le))                                                          !/*ae4c       11*/
      nm=nint(xss(le+1))                                                        !/*ae4c       12*/
!                                                                               !/*ae4a       33*/
!        find appropriate table for sampled cosine                              !/*ae4a       34*/
      do 30 iq=1,nm-1                                                           !/*ae4a       35*/
   30 if(xss(le+iq+2).ge.cs)go to 40                                            !/*ae4a       36*/
   40 if(mu.eq.1)go to 50                                                       !/*ae4a       37*/
      ir=ir+1                                                                   !/*ae4a       38*/
      if(ii.eq.0)rnb(ir)=rang()                                                 !/*ae4a       39*/
      if(rnb(ir).le.(cs-xss(le+iq+1))/(xss(le+iq+2)-                            !/*ae4a       40*/
     & xss(le+iq+1)))iq=iq+1                                                    !/*ae4a       41*/
!                                                                               !/*ae4a       42*/
!        sample from tabulated energy distribution                              !/*ae4a       43*/
   50 lb=id0+nint(xss(le+nm+iq+1))                                              !/*ae4c       13*/
      jj=nint(xss(lb-1))                                                        !/*ae4c       14*/
      np=nint(xss(lb))                                                          !/*ae4c       15*/
      el(m)=xss(lb+1)                                                           !/*ae4a       47*/
      eh(m)=xss(lb+np)                                                          !/*ae4a       48*/
      if(m.ne.ix)go to 110                                                      !/*ae4a       49*/
      ir=ir+1                                                                   !/*ae4a       50*/
      if(ii.eq.0)rnb(ir)=rang()                                                 !/*ae4a       51*/
      ic=lb+2*np+1                                                              !/*ae4a       52*/
      ib=lb+3*np                                                                !/*ae4a       53*/
      call bnsrch(rnb(ir),ic,ib,ig)                                             !/*aex         6*/
   80 l=ic-2*np                                                                 !/*ae4a       61*/
      fa=xss(l+np)                                                              !/*ae4a       62*/
      ea=xss(l)                                                                 !/*ae4a       63*/
      if(jj.eq.1)go to 90                                                       !/*ae4a       64*/
      bb=(xss(l+np+1)-fa)/(xss(l+1)-ea)                                         !/*ae4a       65*/
      if(bb.eq.0.)go to 90                                                      !/*ae4a       66*/
      t=ea+(sqrt(max(zero,fa**2+2.*bb*(rnb(ir)-xss(ic))))-fa)/bb                !/*ae4b        1*/
      go to 100                                                                 !/*ae4a       68*/
   90 t=ea+(rnb(ir)-xss(ic))/fa                                                 !/*ae4a       69*/
  100 acecs6=t                                                                  !/*ae4a       70*/
  110 continue                                                                  !/*ae4a       71*/
!                                                                               !/*ae4a       72*/
!        use scaled interpolation between energies                              !/*ae4a       73*/
      if(rr0.eq.0.)return                                                       !/*ae4a       74*/
      t1=el(1)+rr0*(el(2)-el(1))                                                !/*ae4a       75*/
      t2=eh(1)+rr0*(eh(2)-eh(1))                                                !/*ae4a       76*/
      acecs6=t1+(t-el(ix))*(t2-t1)/(eh(ix)-el(ix))                              !/*ae4c       16*/
      return                                                                    !/*ae4a       78*/
      end                                                                       !/*ae4a       79*/
