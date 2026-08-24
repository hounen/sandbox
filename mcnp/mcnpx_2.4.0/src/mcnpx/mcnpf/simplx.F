!#ident simplx.F from deck sm4a                                                 !/*sm4a        1*/
      subroutine simplx(l,lk,np,ts)                                             !/*sm4a        2*/
!        perform nelder-mead downhill simplex search to minimize pareto.        !/*sm4a        3*/
!        pareto(ar,x,n) evaluates negative log of pareto pdf                    !/*sm4a        4*/
!        l            number of the bin in the tfc array                        !/*sm4a        5*/
!        lk           lshs+nsp+5+tail points skipped for discrete values        !/*sm4a        6*/
!        np           number of tail points to be fit                           !/*sm4a        7*/
!        ts           history tally value where the fit begins                  !/*sm4a        8*/
!        xx(n,1)      initial guesses of function parameters                    !/*sm4a        9*/
!        f            value to norm largest point to 100 for better fit         !/*sm4a       10*/
!        z(il)        minimum value of pareto at convergence                    !/*sm4b        1*/
!        fi           minimum initial value of pareto                           !/*sm4a       12*/
!        a            initial length of simplex (default=0.1)                   !/*sm4a       13*/
      use global_data                                                           !/*smx         1*/
!                                                                               !/*smx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*smx         3*/
      implicit integer(kindi) (i-n)                                             !/*smx         4*/
!                                                                               !/*smx         5*/
      parameter (a=.1,s=1.732050807569,t=2.828427124746,                        !/*sm4a       15*/
     & t1=1.+a*(s+1.)/t,t2=1.+a*(s-1.)/t)                                       !/*sm4a       16*/
      dimension ar(ntp),xc(2),xe(2),xm(2),xr(2),xx(2,3),z(3)                    !/*sm4a       17*/
!                                                                               !/*sm4a       18*/
!        subtract starting tail value ts from history tally tail data.          !/*sm4a       19*/
      it=ital+iper*ntal                                                         !/*sm4b        2*/
      f=100./(shsd(lk+np,it)-ts)                                                !/*sm4b        3*/
      do 10 i=1,np                                                              !/*sm4a       21*/
   10 ar(i)=f*(shsd(lk+i,it)-ts)                                                !/*sm4b        4*/
!                                                                               !/*sm4a       23*/
!        set starting simplex points.                                           !/*sm4a       24*/
      ip=1                                                                      !/*sm4a       25*/
      xx(1,1)=1.                                                                !/*sm4a       26*/
      xx(2,1)=1.                                                                !/*sm4a       27*/
      xx(1,2)=t1                                                                !/*sm4a       28*/
      xx(2,2)=t2                                                                !/*sm4a       29*/
      xx(1,3)=t2                                                                !/*sm4a       30*/
      xx(2,3)=t1                                                                !/*sm4a       31*/
!                                                                               !/*sm4a       32*/
!        calculate pareto at each of the 3 points of the simplex.               !/*sm4a       33*/
      do 20 i=1,3                                                               !/*sm4a       34*/
   20 z(i)=pareto(ar,xx(1,i),np)                                                !/*sm4a       35*/
      fi=min(z(1),min(z(2),z(3)))                                               !/*sm4a       36*/
      ic=0                                                                      !/*sm4b        5*/
      go to 130                                                                 !/*sm4a       38*/
!                                                                               !/*sm4a       39*/
!        iterate to converge two pareto paramters xx(1,i) and xx(2,i).          !/*sm4a       40*/
   30 ic=ic+1                                                                   !/*sm4b        6*/
      if(ic.gt.1000)return                                                      !/*sm4b        7*/
!                                                                               !/*sm4a       43*/
!        compute centroid point.                                                !/*sm4a       44*/
   40 do 50 i=1,2                                                               !/*sm4a       45*/
      xm(i)=0.                                                                  !/*sm4a       46*/
      do 50 j=1,3                                                               !/*sm4a       47*/
   50 if(j.ne.ih)xm(i)=xm(i)+.5*xx(i,j)                                         !/*sm4b        8*/
!                                                                               !/*sm4a       49*/
!        calculate and check reflected point.                                   !/*sm4a       50*/
      xr(1)=2.*xm(1)-xx(1,ih)                                                   !/*sm4b        9*/
      xr(2)=2.*xm(2)-xx(2,ih)                                                   !/*sm4b       10*/
      zr=pareto(ar,xr,np)                                                       !/*sm4b       11*/
      if(zr.lt.z(il))go to 100                                                  !/*sm4b       12*/
      do 60 i=1,3                                                               !/*sm4b       13*/
   60 if(i.ne.ih.and.zr.lt.z(i))go to 110                                       !/*sm4b       14*/
      if(zr.ge.z(ih))go to 70                                                   !/*sm4b       15*/
      xx(1,ih)=xr(1)                                                            !/*sm4b       16*/
      xx(2,ih)=xr(2)                                                            !/*sm4b       17*/
      z(ih)=zr                                                                  !/*sm4b       18*/
!                                                                               !/*sm4a       61*/
!        calculate and check contracted point.                                  !/*sm4a       62*/
   70 xc(1)=.5*(xx(1,ih)+xm(1))                                                 !/*sm4b       19*/
      xc(2)=.5*(xx(2,ih)+xm(2))                                                 !/*sm4b       20*/
      zc=pareto(ar,xc,np)                                                       !/*sm4a       65*/
      if(zc.gt.z(ih))go to 80                                                   !/*sm4b       21*/
      xx(1,ih)=xc(1)                                                            !/*sm4b       22*/
      xx(2,ih)=xc(2)                                                            !/*sm4b       23*/
      z(ih)=zc                                                                  !/*sm4b       24*/
      go to 130                                                                 !/*sm4a       70*/
!                                                                               !/*sm4a       71*/
!        shrink simplex.                                                        !/*sm4a       72*/
   80 do 90 j=1,3                                                               !/*sm4a       73*/
      if(j.eq.il)go to 90                                                       !/*sm4b       25*/
      xx(1,j)=.5*(xx(1,j)+xx(1,il))                                             !/*sm4b       26*/
      xx(2,j)=.5*(xx(2,j)+xx(2,il))                                             !/*sm4b       27*/
      z(j)=pareto(ar,xx(1,j),np)                                                !/*sm4a       77*/
   90 continue                                                                  !/*sm4a       78*/
      go to 130                                                                 !/*sm4a       79*/
!                                                                               !/*sm4a       80*/
!        calculate and check expansion point.                                   !/*sm4a       81*/
  100 xe(1)=2.*xr(1)-xm(1)                                                      !/*sm4a       82*/
      xe(2)=2.*xr(2)-xm(2)                                                      !/*sm4a       83*/
      zx=pareto(ar,xe,np)                                                       !/*sm4b       28*/
      if(zx.lt.z(il))go to 120                                                  !/*sm4b       29*/
  110 xx(1,ih)=xr(1)                                                            !/*sm4b       30*/
      xx(2,ih)=xr(2)                                                            !/*sm4b       31*/
      z(ih)=zr                                                                  !/*sm4b       32*/
      go to 130                                                                 !/*sm4a       89*/
!                                                                               !/*sm4a       90*/
!        replace worst point by expansion point.                                !/*sm4a       91*/
  120 xx(1,ih)=xe(1)                                                            !/*sm4b       33*/
      xx(2,ih)=xe(2)                                                            !/*sm4b       34*/
      z(ih)=zx                                                                  !/*sm4b       35*/
!                                                                               !/*sm4a       95*/
!        check convergence of the two pareto parameters xx.                     !/*sm4a       96*/
  130 il=1                                                                      !/*sm4b       36*/
      if(z(2).lt.z(1))il=2                                                      !/*sm4b       37*/
      if(z(3).lt.z(il))il=3                                                     !/*sm4b       38*/
      ih=1                                                                      !/*sm4b       39*/
      if(z(2).gt.z(1))ih=2                                                      !/*sm4b       40*/
      if(z(3).gt.z(ih))ih=3                                                     !/*sm4b       41*/
      if(ih.eq.il)go to 150                                                     !/*sm4b       42*/
      do 140 i=1,2                                                              !/*sm4a      104*/
  140 if(abs(xx(i,ih)-xx(i,il))/(abs(xx(i,ih))+abs(xx(i,il))).gt.               !/*sm4b       43*/
     & 1.e-5)go to 30                                                           !/*sm4a      106*/
!                                                                               !/*sm4a      107*/
!        convergence assumed valid if a minimum was found.                      !/*sm4a      108*/
!        a second pass could be done with different initial conditions.         !/*sm4a      109*/
  150 if(z(il).ge.fi.or.xx(1,il).eq.0..or.xx(2,il).eq.0.)return                 !/*sm4b       44*/
      tfc(ltfc+5,l,it)=1.+1./xx(2,il)                                           !/*sm4b       45*/
      tfc(ltfc+6,l,it)=f/xx(1,il)                                               !/*sm4b       46*/
      return                                                                    !/*sm4a      113*/
      end                                                                       !/*sm4a      114*/
