!#ident abvals.F from deck av                                                   !/*av          1*/
      subroutine abvals(la,iv)                                                  !/*av          3*/
#ifdef MCPLOT                                                                   /*!avx         1*/
!        extract npt(iv) abscissa values of free variable ifree(iv) from        !/*av          4*/
!        tds and put them in ab1(la).  la may be lab1 or lab2.                  !/*av          5*/
      use global_data                                                           !/*avx         2*/
      use mcplot_data                                                           !/*avx         3*/
!                                                                               !/*avx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*avx         5*/
      implicit integer(kindi) (i-n)                                             !/*avx         6*/
!                                                                               !/*avx         7*/
!                                                                               !/*av4b        1*/
!        branch for an xs plot.                                                 !/*av4b        2*/
      if(kxsplt.ne.0)go to 120                                                  !/*av4b        3*/
!                                                                               !/*av          8*/
!        use bin numbers if the variable is not c, e, or t.                     !/*av          9*/
      if(ifree(iv).ge.6)go to 20                                                !/*av         10*/
#ifdef RADIOG                                                                   /*!avx         8*/
      if(ifree(iv).eq.4.and.jptal(ljpt+8,mtal).gt.2)go to 20                    !/*avx         9*/
#endif                                                                          /*!avx        10*/
      do 10 i=1,npt(iv)                                                         !/*av         11*/
   10 ab1(la+i)=i                                                               !/*av         12*/
      icut(iv)=1                                                                !/*av         13*/
      abhi(iv)=npt(iv)+1                                                        !/*av         14*/
      ablo(iv)=0.                                                               !/*av         15*/
      return                                                                    !/*av         16*/
!                                                                               !/*av         17*/
!        use upper bin bounds for c, e, and t.                                  !/*av         18*/
   20 do 30 i=1,npt(iv)                                                         !/*av         19*/
   30 ab1(la+i)=tds(iptal(lipt+ifree(iv),1,mtal)+i)                             !/*av         20*/
      abhi(iv)=ab1(la+npt(iv))                                                  !/*av         21*/
!                                                                               !/*av         22*/
!        define left insertion point.                                           !/*av         23*/
      ablo(iv)=0.                                                               !/*av         24*/
      if(ifree(iv).eq.6)ablo(iv)=-1.                                            !/*av         25*/
      if(ifree(iv).ne.7.or.ktfile.ne.1)go to 50                                 !/*av         26*/
      ablo(iv)=huge                                                             !/*av         27*/
      do 40 i=1,mipt                                                            !/*av         29*/
   40 if(ktp(lktp+i,mtal).ne.0)ablo(iv)=min(ablo(iv),ecf(i))                    !/*avx        11*/
   50 if(ifree(iv).eq.8)ablo(iv)=min(zero,ab1(la+1))                            !/*av         32*/
#ifdef RADIOG                                                                   /*!avx        12*/
      if((ifree(iv).eq.4.or.ifree(iv).eq.6).and.jptal(ljpt+8,mtal).gt.2)        !/*avx        13*/
     & ablo(iv)=tds(iptal(lipt+ifree(iv),1,mtal))                               !/*avx        14*/
#endif                                                                          /*!avx        15*/
      if(nfree.eq.1.and.lax.ge.3.and.ablo(iv).le.0.)                            !/*av         33*/
     & ablo(iv)=max(zero+1e-14,.00001*ab1(la+1))                                !/*cor4-2    222*/
!                                                                               !/*av         35*/
!        determine position of cutoff.                                          !/*av         36*/
      do 60 i=1,npt(iv)                                                         !/*av         37*/
      icut(iv)=i                                                                !/*av         38*/
   60 if(ablo(iv).lt.ab1(la+i))go to 80                                         !/*av         39*/
      write(jtty,70)                                                            !/*av         40*/
   70 format(38h cutoff is larger than all bin values.)                         !/*av         41*/
      inform=1                                                                  !/*av         42*/
      return                                                                    !/*av         43*/
!                                                                               !/*av         44*/
!        normalize 2d ordinate values by bin width.                             !/*av         45*/
   80 if(ktfile.ne.1.and.iptal(lipt+ifree(iv),2,mtal).ne.0)return               !/*av         46*/
      if(nfree.eq.2)go to 100                                                   !/*avx        16*/
#ifdef RADIOG                                                                   /*!avx        17*/
      if(jptal(ljpt+2,mtal).eq.5.and.(ifree(iv).eq.4.or.ifree(iv).eq.6))        !/*avx        18*/
     & go to 92                                                                 !/*avx        19*/
#endif                                                                          /*!avx        20*/
      if(nonorm.ne.0)go to 100                                                  !/*avx        21*/
      do 90 i=icut(iv),npt(iv)                                                  !/*av         48*/
      if(i.eq.icut(iv))d=ab1(la+i)-ablo(iv)                                     !/*av         49*/
      if(i.ne.icut(iv))d=ab1(la+i)-ab1(la+i-1)                                  !/*av         50*/
      if(ifree(iv).eq.6)d=2.*pie*d                                              !/*av         51*/
   90 if(d.ne.0.)ord(lord+i)=ord(lord+i)/d                                      !/*av         52*/
#ifdef RADIOG                                                                   /*!avx        22*/
      go to 100                                                                 !/*avx        23*/
!                                                                               !/*avx        24*/
!       if nonorm.ne.0 multiply ordinate by bin area of grid.                   !/*avx        25*/
   92 if(nonorm.eq.0.or.jptal(ljpt+8,mtal).eq.5)go to 100                       !/*avx        26*/
      ib=6                                                                      !/*avx        27*/
      if(ifree(iv).eq.6)ib=4                                                    !/*avx        28*/
      if(kbin(ib,1).eq.iptal(lipt+ib,4,mtal))go to 94                           !/*avx        29*/
      d1=tds(iptal(lipt+ib,1,mtal)+kbin(ib,1))-                                 !/*avx        30*/
     & tds(iptal(lipt+ib,1,mtal)+kbin(ib,1)-1)                                  !/*avx        31*/
      go to 96                                                                  !/*avx        32*/
   94 d1=tds(iptal(lipt+ib,1,mtal)+iptal(lipt+ib,3,mtal))-                      !/*avx        33*/
     & tds(iptal(lipt+ib,1,mtal))                                               !/*avx        34*/
   96 do 98 i=icut(iv),npt(iv)                                                  !/*avx        35*/
      if(i.eq.icut(iv))d=d1*(ab1(la+i)-ablo(iv))                                !/*avx        36*/
      if(i.ne.icut(iv))d=d1*(ab1(la+i)-ab1(la+i-1))                             !/*avx        37*/
   98 ord(lord+i)=d*ord(lord+i)                                                 !/*avx        38*/
#endif                                                                          /*!avx        39*/
!                                                                               !/*av         53*/
!        bin center the abscissa values.                                        !/*av         54*/
  100 do 110 iz=icut(iv)+1,npt(iv)                                              !/*av         55*/
      i=npt(iv)+icut(iv)+1-iz                                                   !/*av         56*/
  110 ab1(la+i)=.5*(ab1(la+i)+ab1(la+i-1))                                      !/*av         57*/
      ab1(la+icut(iv))=.5*(ab1(la+icut(iv))+ablo(iv))                           !/*av         58*/
      return                                                                    !/*av         59*/
!                                                                               !/*av4b        4*/
!        multigroup plots.  npt(iv)=2*ne.                                       !/*av4b        5*/
  120 if(kxsptp.ne.6.and.kxsptp.ne.7)go to 140                                  !/*av4b        6*/
      ne=nxs(lnxs+5,kxspie(lxie+1))                                             !/*av4b        7*/
      do 130 i=1,ne                                                             !/*av4b        8*/
      kp=kxspen(lxen+1)+i                                                       !/*av4b        9*/
      ab1(la+npt(iv)-2*i+1)=xss(kp)-.5*xss(kp+ne)                               !/*av4b       10*/
  130 ab1(la+npt(iv)-2*i+2)=xss(kp)+.5*xss(kp+ne)                               !/*av4b       11*/
      go to 250                                                                 !/*av4b       12*/
!                                                                               !/*av4b       13*/
!        create energy grid for non-multigroup xs plots.                        !/*av4b       14*/
  140 in=0                                                                      !/*av4b       15*/
      if(kxspar.lt.3.or.kxsptp.ne.10)go to 160                                  !/*avx        40*/
!                                                                               !/*av4b       17*/
!        set up grid for electrons.                                             !/*av4b       18*/
      in=nee(3)-1                                                               !/*avx        41*/
      do 150 j=1,in                                                             !/*av4b       20*/
      k=in-j+1                                                                  !/*av4b       21*/
  150 ab1(la+j)=eee(leee+3,k)                                                   !/*avx        42*/
      go to 250                                                                 !/*av4b       23*/
!                                                                               !/*av4b       24*/
!        get energy grid for single nuclide case.                               !/*av4b       25*/
  160 kf=0                                                                      !/*av4b       26*/
      if(kxsptp.eq.4.and.nxs(lnxs+5,kxspie(lxie+1)).eq.4.and.                   !/*av4b       27*/
     & kxspmt.le.2)kf=2                                                         !/*av4b       28*/
      if(kxsplt.gt.1)go to 200                                                  !/*av4b       29*/
      if(kxsptp.eq.2.or.kf.ne.0)go to 180                                       !/*av4b       30*/
      npt(iv)=kxspnx(lxnx+1)                                                    !/*av4b       31*/
      do 170 i=1,npt(iv)                                                        !/*av4b       32*/
      ab1(la+i)=xss(kxspen(lxen+1)+i)                                           !/*av4b       33*/
  170 if(kxspar.eq.2.and.kxsptp.ne.9)                                           !/*avxl        1*/
     & ab1(la+i)=exp(xss(kxspen(lxen+1)+i))                                     !/*avxl        2*/
      go to 250                                                                 !/*av4b       35*/
!        drxs or s(a,b) elastic / energy.                                       !/*av4b       36*/
  180 npt(iv)=2*kxspnx(lxnx+1)                                                  !/*av4b       37*/
      do 190 i=1,npt(iv)                                                        !/*av4b       38*/
  190 ab1(la+i)=xss(kxspen(lxen+1)+(i+1)/2)                                     !/*av4b       39*/
      go to 250                                                                 !/*av4b       40*/
!                                                                               !/*av4b       41*/
!        multiple nuclides on different grids.                                  !/*av4b       42*/
!        loop through nuclides then through energies.                           !/*av4b       43*/
!        set up dummy pointer in upper half of kxspnx array.                    !/*av4b       44*/
!        lower half of kxspnx array has number of points in xss array.          !/*av4b       45*/
  200 do 210 j=1,kxsplt                                                         !/*av4b       46*/
  210 kxspnx(lxnx+mxe1+j)=1                                                     !/*av4b       47*/
!                                                                               !/*av4b       48*/
!        find next minimum energy of given isotopes.                            !/*av4b       49*/
!        eo=-huge for photons.                                                  !/*av4b       50*/
      eo=-huge                                                                  !/*av4b       51*/
  220 en=huge                                                                   !/*av4b       52*/
      do 230 j=1,kxsplt                                                         !/*av4b       53*/
      if(kxspen(lxen+j).le.0.or.kxspxs(lxxs+j).le.0)go to 230                   !/*av4b       54*/
      if(kxspnx(lxnx+mxe1+j).gt.kxspnx(lxnx+j))go to 230                        !/*av4b       55*/
      e=xss(kxspen(lxen+j)+kxspnx(lxnx+mxe1+j))                                 !/*av4b       56*/
      if(e.gt.en)go to 230                                                      !/*av4b       57*/
      if(e.eq.en)kxspnx(lxnx+mxe1+k)=kxspnx(lxnx+mxe1+k)+1                      !/*av4b       58*/
      k=j                                                                       !/*av4b       59*/
      en=e                                                                      !/*av4b       60*/
  230 continue                                                                  !/*av4b       61*/
      if(en.eq.huge)go to 240                                                   !/*av4b       62*/
      kxspnx(lxnx+mxe1+k)=kxspnx(lxnx+mxe1+k)+1                                 !/*av4b       63*/
      if(en.le.eo)go to 220                                                     !/*av4b       64*/
      eo=en                                                                     !/*av4b       65*/
      in=in+1                                                                   !/*av4b       66*/
      ab1(la+in)=en                                                             !/*av4b       67*/
      if(kxspar.eq.2.and.kxsptp.ne.9)ab1(la+in)=exp(en)                         !/*avxl        3*/
      kxsptp=nty(lnty+kxspie(lxie+k))                                           !/*av4b       69*/
!        drxs or s(a,b) elastic / energy.                                       !/*av4b       70*/
      if(kxsptp.ne.2.and.kf.ne.k)go to 220                                      !/*av4b       71*/
      ab1(la+in)=en-1.e-5*en                                                    !/*av4b       72*/
      in=in+1                                                                   !/*av4b       73*/
      ab1(la+in)=en                                                             !/*av4b       74*/
      go to 220                                                                 !/*av4b       75*/
  240 npt(iv)=in                                                                !/*av4b       76*/
!                                                                               !/*av4b       77*/
!        determine left insertion point and set upper energy.                   !/*av4b       78*/
  250 abhi(iv)=ab1(la+npt(iv))                                                  !/*av4b       79*/
      do 260 i=1,npt(iv)                                                        !/*av4b       80*/
  260 if(lax.lt.3.or.ab1(la+i).gt.0.)go to 270                                  !/*av4b       81*/
  270 icut(iv)=i                                                                !/*av4b       82*/
      ablo(iv)=ab1(la+i)                                                        !/*av4b       83*/
#endif                                                                          /*!avx        43*/
      return                                                                    !/*av4b       84*/
      end                                                                       !/*av         60*/
