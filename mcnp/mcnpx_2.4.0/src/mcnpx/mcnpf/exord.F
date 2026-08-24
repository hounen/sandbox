!#ident exord.F from deck eo                                                    !/*eo          1*/
      subroutine exord(m)                                                       !/*eo          3*/
#ifdef MCPLOT                                                                   /*!eox         1*/
!        extract ordinate values and errors from array tal.                     !/*eo          4*/
!        m=1 to get extreme ordinates only, m=2 to load ord array.              !/*eo          5*/
      use global_data                                                           !/*eox         2*/
      use mcplot_data                                                           !/*eox         3*/
!                                                                               !/*eox         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*eox         5*/
      implicit integer(kindi) (i-n)                                             !/*eox         6*/
!                                                                               !/*eox         7*/
!                                                                               !/*eo4b        2*/
      if(kxsplt.ne.0)go to 70                                                   !/*eo4b        3*/
!                                                                               !/*eo          8*/
      i=0                                                                       !/*eo          9*/
      if(ktfile.ne.1)go to 20                                                   !/*eo         10*/
      t=max(1,nps)                                                              !/*eo         11*/
      if(nsr.eq.71.and.kcz.ge.ikz)t=nsrck*(kcz-ikz)                             !/*eo4c        1*/
      if(kc8.lt.0)t=t+nsrck-wt0*nsa                                             !/*eo4c        3*/
      if(knrm.ne.0)t=pax(1,1,1)                                                 !/*eox        12*/
      if(nsr.eq.6.and.nrrs.ge.nrss)t=max(1,np1)                                 !/*eo         12*/
      if(nsr.eq.6.and.nrrs.lt.nrss)t=max(1,npsr)                                !/*eo         13*/
      fp=1./max(t,one)                                                          !/*eo4b        7*/
   20 j=ltal+jptal(ljpt+5,mtal)                                                 !/*eo         18*/
      n=iptal(lipt+1,5,mtal)                                                    !/*eo4b        8*/
      do 60 ir=ibl(1,1),ibl(1,2)                                                !/*eo         19*/
      j1=(ir-1)*iptal(lipt+2,5,mtal)                                            !/*eo         20*/
      do 60 if=ibl(2,1),ibl(2,2)                                                !/*eo         21*/
      j2=j1+(if-1)*iptal(lipt+3,5,mtal)                                         !/*eo         22*/
      do 60 iu=ibl(3,1),ibl(3,2)                                                !/*eo         23*/
      j3=j2+(iu-1)*iptal(lipt+4,5,mtal)                                         !/*eo         24*/
      do 60 is=ibl(4,1),ibl(4,2)                                                !/*eo         25*/
      j4=j3+iptal(lipt+5,5,mtal)*(is-1)                                         !/*eo         26*/
      do 60 im=ibl(5,1),ibl(5,2)                                                !/*eo         27*/
      j5=j4+(im-1)*iptal(lipt+6,5,mtal)                                         !/*eo         28*/
      do 60 ic=ibl(6,1),ibl(6,2)                                                !/*eo         29*/
      j6=j5+(ic-1)*iptal(lipt+7,5,mtal)                                         !/*eo         30*/
      do 60 ie=ibl(7,1),ibl(7,2)                                                !/*eo         31*/
      j7=j6+(ie-1)*iptal(lipt+8,5,mtal)                                         !/*eo         32*/
      do 60 it=ibl(8,1),ibl(8,2)                                                !/*eo         33*/
      k=j7+it                                                                   !/*eo         34*/
      i=i+1                                                                     !/*eo         35*/
      if(ktfile.ne.1)go to 40                                                   !/*eo         36*/
      k=k+j+mxf+lpert*mxfp                                                      !/*eo4b        9*/
      y=tal(k)                                                                  !/*eo         38*/
      if(nfree.eq.2)go to 30                                                    !/*eo         39*/
      erb(lerb+i)=tal(k+mxf)                                                    !/*eo         40*/
      if(erb(lerb+i).le.0.)y=0.                                                 !/*eo         41*/
      if(y.ne.0.)erb(lerb+i)=min(erb(lerb+i)/y**2-fp,one)                       !/*eo         42*/
      if(erb(lerb+i).gt.0.)erb(lerb+i)=sqrt(erb(lerb+i))                        !/*eo         43*/
   30 if(iptal(lipt+4,2,mtal).ne.0)y=y/                                         !/*eo         44*/
     & tds(iptal(lipt+4,2,mtal)+iptal(lipt+4,3,mtal)*(ir-1)+is)                 !/*eo         45*/
      if((jptal(ljpt+2,mtal).eq.6.or.jptal(ljpt+2,mtal).eq.7).and.              !/*eo         46*/
     & jptal(ljpt+4,mtal).eq.1)y=y*1.60219e-22                                  !/*eox        14*/
      y=y*fp                                                                    !/*eo         48*/
      go to 50                                                                  !/*eo         49*/
   40 k=j+2*(k+lpert*n)-1                                                       !/*eo4b       10*/
      y=tal(k)                                                                  !/*eo         51*/
      if(nfree.eq.1)erb(lerb+i)=tal(k+1)                                        !/*eo         52*/
   50 if(m.eq.2)go to 60                                                        !/*eo         53*/
      xyzmn(3)=min(y,xyzmn(3))                                                  !/*eo         54*/
      xyzmx(3)=max(y,xyzmx(3))                                                  !/*eo         55*/
   60 if(m.eq.2)ord(lord+i)=y                                                   !/*eo         56*/
      return                                                                    !/*eo         57*/
!                                                                               !/*eo4b       11*/
!        loop through cross section library, zeroing out                        !/*eo4b       12*/
!        error bound values.  assign cross section                              !/*eo4b       13*/
!        values to ordinate array if m=2, otherwise search                      !/*eo4b       14*/
!        for min and max ord values (if m=1).                                   !/*eo4b       15*/
!                                                                               !/*eo4b       16*/
!        set error bars to zero.                                                !/*eo4b       17*/
   70 do 80 i=1,npt(1)                                                          !/*eo4b       18*/
   80 erb(lerb+i)=0.                                                            !/*eo4c        5*/
!                                                                               !/*eo4b       20*/
!        multigroup libraries.                                                  !/*eo4b       21*/
      if(kxsptp.ne.6.and.kxsptp.ne.7)go to 130                                  !/*eo4b       22*/
      if(m.eq.2)go to 100                                                       !/*eo4b       23*/
      do 90 i=1,npt(1)                                                          !/*eo4b       24*/
      xyzmn(3)=min(ord(lord+i),xyzmn(3))                                        !/*eo4b       25*/
   90 xyzmx(3)=max(ord(lord+i),xyzmx(3))                                        !/*eo4b       26*/
      return                                                                    !/*eo4b       27*/
  100 do 120 i=1,npt(1)                                                         !/*eo4b       28*/
      ord(lord+npt(1)-i+1)=0.                                                   !/*eo4c        6*/
      f1=one                                                                    !/*eo4b       30*/
      do 110 j=1,kxsplt                                                         !/*eo4b       31*/
      if(kxspxs(lxxs+j).le.0)go to 110                                          !/*eo4b       32*/
      if(kxspkm.ne.0)f1=fme(lfme+kxspkm+j)                                      !/*eo4b       33*/
      ord(lord+npt(1)-i+1)=ord(lord+npt(1)-i+1)+f1*                             !/*eo4b       34*/
     & xss(kxspxs(lxxs+j)+(i+1)/2)                                              !/*eo4b       35*/
  110 continue                                                                  !/*eo4b       36*/
  120 continue                                                                  !/*eo4b       37*/
      return                                                                    !/*eo4b       38*/
!                                                                               !/*eo4b       39*/
!        continuous and discrete libraries.                                     !/*eo4b       40*/
  130 if(kxspar.eq.3)go to 260                                                  !/*eo4b       41*/
      if(m.eq.2)go to 150                                                       !/*eo4b       42*/
      do 140 i=1,npt(1)                                                         !/*eo4b       43*/
      xyzmn(3)=min(ord(lord+i),xyzmn(3))                                        !/*eo4b       44*/
  140 xyzmx(3)=max(ord(lord+i),xyzmx(3))                                        !/*eo4b       45*/
      return                                                                    !/*eo4b       46*/
!                                                                               !/*eo4b       47*/
!        set flag for s(a,b) elastic to be divided by energy.                   !/*eo4b       48*/
  150 kf=0                                                                      !/*eo4b       49*/
      if(kxsptp.eq.4.and.nxs(lnxs+5,kxspie(lxie+1)).eq.4.and.                   !/*eo4b       50*/
     & kxspmt.le.2)kf=2                                                         !/*eo4b       51*/
!                                                                               !/*eo4b       52*/
!        single nuclide continuous case.                                        !/*eo4b       53*/
      if(kxsplt.gt.1)go to 190                                                  !/*eo4b       54*/
      if(kxsptp.eq.2.or.kf.ne.0)go to 170                                       !/*eo4b       55*/
      do 160 i=1,npt(1)                                                         !/*eo4b       56*/
      ord(lord+i)=max(zero,zero+xss(kxspxs(lxxs+1)+i))                          !/*eo4b       57*/
  160 if(kxsptp.eq.5)ord(lord+i)=exordp(i,1)                                    !/*eo4b       58*/
      return                                                                    !/*eo4b       59*/
!                                                                               !/*eo4b       60*/
!        single nuclide drxs case, or s(a,b) elastic / energy.                  !/*eo4b       61*/
  170 ord(lord+1)=max(zero,zero+xss(kxspxs(lxxs+1)+1))                          !/*eo4b       62*/
      if(kf.ne.0)ord(lord+1)=.1*ord(lord+1)/xss(kxspen(lxen+1)+1)               !/*eo4b       63*/
      do 180 i=2,npt(1)                                                         !/*eo4b       64*/
      ord(lord+i)=max(zero,zero+xss(kxspxs(lxxs+1)+i/2))                        !/*eo4b       65*/
  180 if(kf.ne.0)ord(lord+i)=ord(lord+i)/ab1(lab1+i)                            !/*eo4b       66*/
      return                                                                    !/*eo4b       67*/
!                                                                               !/*eo4b       68*/
!        multiple nuclide case.                                                 !/*eo4b       69*/
!        set up dummy pointer in upper half of kxspnx array.                    !/*eo4b       70*/
!        lower half of kxspnx array has number of points in xss array.          !/*eo4b       71*/
  190 do 200 j=1,kxsplt                                                         !/*eo4b       72*/
  200 kxspnx(lxnx+mxe1+j)=2                                                     !/*eo4b       73*/
!                                                                               !/*eo4b       74*/
!        begin loop over energies (abscissae).                                  !/*eo4b       75*/
!        search in each nuclide for an energy point just above                  !/*eo4b       76*/
!        abscissa value.  linearly interpolate for ordinate value.              !/*eo4b       77*/
      f1=one                                                                    !/*eo4b       78*/
      do 250 i=1,npt(1)                                                         !/*eo4b       79*/
      ord(lord+i)=0.                                                            !/*eo4c        7*/
      do 240 j=1,kxsplt                                                         !/*eo4b       81*/
      y=0.                                                                      !/*eo4c        8*/
      if(kxspen(lxen+j).le.0.or.kxspxs(lxxs+j).le.0)go to 240                   !/*eo4b       83*/
      kxsptp=nty(lnty+kxspie(lxie+j))                                           !/*eo4b       84*/
  210 kj=kxspnx(lxnx+mxe1+j)                                                    !/*eo4b       85*/
      e=xss(kxspen(lxen+j)+kj)                                                  !/*eo4b       86*/
      if(kxsptp.eq.5)e=exp(e)                                                   !/*eo4b       87*/
      if(e.gt.ab1(lab1+i))go to 220                                             !/*eo4b       88*/
      if(kj.ge.kxspnx(lxnx+j))go to 220                                         !/*eo4b       89*/
      kxspnx(lxnx+mxe1+j)=kj+1                                                  !/*eo4b       90*/
      go to 210                                                                 !/*eo4b       91*/
  220 if(kxsptp.ne.4)f1=fme(lfme+kxspkm+j)                                      !/*eo4b       92*/
      if(kxsptp.ne.2.and.kxsptp.ne.5.and.kf.ne.j)                               !/*eo4b       93*/
     & y=f1*(xss(kxspxs(lxxs+j)+kj-1)+                                          !/*eo4b       94*/
     & (xss(kxspxs(lxxs+j)+kj)-xss(kxspxs(lxxs+j)+kj-1))*                       !/*eo4b       95*/
     & max(zero,min(one,(ab1(lab1+i)-xss(kxspen(lxen+j)+kj-1))/                 !/*eo4b       96*/
     & (e-xss(kxspen(lxen+j)+kj-1)))))                                          !/*eo4b       97*/
      if(kxsptp.eq.2)y=f1*xss(kxspxs(lxxs+j)+kj-1)                              !/*eo4b       98*/
      if(kxsptp.eq.5)y=f1*exordp(i,j)                                           !/*eo4b       99*/
      if(kf.eq.j.and.ab1(lab1+i).ge.xss(kxspen(lxen+j)+1))                      !/*eo4b      100*/
     & y=xss(kxspxs(lxxs+j)+kj-1)/ab1(lab1+i)                                   !/*eo4b      101*/
      if(kxsptp.eq.9.and.ab1(lab1+i).lt.xss(kxspen(lxen+j)+1))y=zero            !/*eoxl        1*/
  240 ord(lord+i)=ord(lord+i)+y                                                 !/*eo4b      102*/
  250 continue                                                                  !/*eo4b      103*/
      if(kxsptp.eq.4)kxsplt=1                                                   !/*eo4b      104*/
      return                                                                    !/*eo4b      105*/
!                                                                               !/*eo4b      106*/
!        electron library.                                                      !/*eo4b      107*/
  260 if(m.eq.2)go to 280                                                       !/*eo4b      108*/
      do 270 i=1,npt(1)                                                         !/*eo4b      109*/
      xyzmn(3)=min(ord(lord+i),xyzmn(3))                                        !/*eo4b      110*/
  270 xyzmx(3)=max(ord(lord+i),xyzmx(3))                                        !/*eo4b      111*/
      return                                                                    !/*eo4b      112*/
!                                                                               !/*eo4b      113*/
  280 do 290 i=1,npt(1)                                                         !/*eo4b      114*/
      ord(lord+i)=zero                                                          !/*eo4b      115*/
      ni=nee(3)*kxsmat-i                                                        !/*eox        15*/
      nj=nee_max*(kxsmat-1)+nee(3)-i                                            !/*eox        16*/
      if(kxspmt.le.10)y=xse85(lx85+kxspmt,ni)                                   !/*eo4b      117*/
      if(kxspmt.eq.11)y=rng(lrng+3,nj)                                          !/*eox        17*/
      if(kxspmt.eq.12)y=qav(lqav+3,nj)                                          !/*eox        18*/
      if(kxspmt.eq.13)y=ear(lear+ni)                                            !/*eo4b      120*/
  290 ord(lord+i)=y                                                             !/*eo4b      121*/
#endif                                                                          /*!eox        19*/
      return                                                                    !/*eo4b      122*/
      end                                                                       !/*eo         58*/
