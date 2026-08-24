!#ident wwval.F from deck we4b                                                  !/*we4b        1*/
      function wwval(ny,nc,nb,na,ix)                                            !/*we4c2       1*/
!        calculate the weight-window value for the particle type ny             !/*we4b        3*/
!        in cell nc and energy group nb. na is used only when a                 !/*we4b        4*/
!        weight-window mesh is input: 0 indicates to omit angular               !/*we4b        5*/
!        weighting and 1 to include it.  ix is the mesh index.                  !/*we4c2       2*/
      use global_data                                                           !/*wex         1*/
!                                                                               !/*wex         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*wex         3*/
      implicit integer(kindi) (i-n)                                             !/*wex         4*/
!                                                                               !/*wex         5*/
      dimension q(3),rc(3),x(3)                                                 !/*we4b        8*/
!                                                                               !/*we4b        9*/
!        if wwp(5).ge.0, return the cell-based weight-window value.             !/*we4c2       3*/
      if(wwp(ny,5).lt.0.)go to 10                                               !/*we4b       11*/
      ix=0                                                                      !/*we4c2       4*/
      wwval=wwf(lwwf+nc+mxa*(nb-1+mww(ny)))                                     !/*we4b       12*/
      return                                                                    !/*we4b       13*/
!                                                                               !/*we4b       14*/
!        get the weight-window value from the weight-window mesh.               !/*we4b       15*/
   10 wwval=0.                                                                  !/*we4c        1*/
      do 20 i=1,3                                                               !/*we4b       17*/
      rc(i)=gpblcm(i)                                                           !/*we4b       18*/
   20 if(lev.ne.0)rc(i)=udt(i,0)                                                !/*we4b       19*/
      if(nwgeom.ne.2)go to 50                                                   !/*we4b       20*/
!                                                                               !/*we4b       21*/
!        for cylindrical geometry, find r,z,theta for given x,y,z.              !/*we4b       22*/
!        get vector x() from cylinder base to x,y,z.                            !/*we4b       23*/
      do 30 i=1,3                                                               !/*we4b       24*/
      x(i)=rc(i)-wwm(3+i)                                                       !/*we4b       25*/
   30 rc(i)=0.                                                                  !/*we4c        2*/
!                                                                               !/*we4b       27*/
!        dot product with unit cylinder height vector gives z.                  !/*we4b       28*/
      rc(2)=dotpro(x,wwm(20))                                                   !/*we4b       29*/
      if(rc(2).lt.0..or.rc(2).gt.wwm(23))go to 200                              !/*we4c        3*/
!                                                                               !/*we4b       31*/
!        use x and wwm(20) to form radius vector q - a radius of zero           !/*we4b       32*/
!        defaults to theta (t) of zero.                                         !/*we4b       33*/
      do 40 i=1,3                                                               !/*we4b       34*/
      q(i)=x(i)-wwm(19+i)*rc(2)                                                 !/*we4b       35*/
   40 rc(1)=rc(1)+q(i)*q(i)                                                     !/*we4b       36*/
      rc(1)=sqrt(rc(1))                                                         !/*we4b       37*/
      if(rc(1).lt.1.d-37)go to 50                                               !/*we4b       38*/
!                                                                               !/*we4b       39*/
!        dot product of q with unit cylinder radius vector gives theta.         !/*we4b       40*/
      t=dotpro(q,wwm(16))/rc(1)                                                 !/*we4b       41*/
      if(abs(t).gt.one)t=sign(one,t)                                            !/*we4b       42*/
      rc(3)=acos(t)                                                             !/*we4b       43*/
      if(dotpro(q,wwm(24)).gt.0.)rc(3)=2.d0*pie-rc(3)                           !/*we4c        4*/
!                                                                               !/*we4b       45*/
!        loop over coordinates - variable ia is a dimensional offset.           !/*we4b       46*/
   50 ia=0                                                                      !/*we4b       47*/
      m=1                                                                       !/*we4b       48*/
      ix=1                                                                      !/*we4c2       5*/
      do 160 i=1,3                                                              !/*we4b       50*/
      r=rc(i)                                                                   !/*we4b       51*/
      iw=3*nint(wwm(i+6))                                                       !/*we4b       52*/
      if(r.lt.wgm(lwgm+ia+2).or.r.gt.wgm(lwgm+ia+2+iw))go to 200                !/*we4b       53*/
!                                                                               !/*we4b       54*/
!        binary search for coarse mesh interval.                                !/*we4b       55*/
      ic=0                                                                      !/*we4b       56*/
      ib=nint(wwm(i+6))                                                         !/*we4b       57*/
   60 if(ib-ic.eq.1)go to 80                                                    !/*we4b       58*/
      ih=(ic+ib)/2                                                              !/*we4b       59*/
      if(r.lt.wgm(lwgm+ia+2+3*ih))go to 70                                      !/*we4b       60*/
      ic=ih                                                                     !/*we4b       61*/
      go to 60                                                                  !/*we4b       62*/
   70 ib=ih                                                                     !/*we4b       63*/
      go to 60                                                                  !/*we4b       64*/
!                                                                               !/*we4b       65*/
!        search for fine mesh interval, from largest to smallest.               !/*we4b       66*/
   80 rr=wgm(lwgm+ia+3*(ib+1))                                                  !/*we4b       67*/
      im=lwgm+ia+3*ib+1                                                         !/*we4b       68*/
      np=nint(wgm(im))-nint(wgm(im-3))                                          !/*we4b       69*/
      wp=wgm(im+1)-wgm(im-2)                                                    !/*we4b       70*/
      if(abs(1.-rr).gt..0001)go to 90                                           !/*we4c2       6*/
!                                                                               !/*we4b       72*/
!        uniform fine mesh intervals.                                           !/*we4b       73*/
      k=int(np*(r-wgm(im-2))/wp)+1+nint(wgm(im-3))                              !/*we4b       74*/
      go to 150                                                                 !/*we4b       75*/
!                                                                               !/*we4b       76*/
!        increasing fine mesh size.                                             !/*we4b       77*/
   90 if(rr.lt.1.)go to 120                                                     !/*we4b       78*/
      rr=one/rr                                                                 !/*we4b       79*/
      dx=wp/(one-rr**np)*(one-rr)                                               !/*we4b       80*/
      nf=np                                                                     !/*we4b       81*/
      d=wgm(im+1)-dx                                                            !/*we4b       82*/
  100 if(r.ge.d)go to 110                                                       !/*we4b       83*/
      nf=nf-1                                                                   !/*we4b       84*/
      dx=dx*rr                                                                  !/*we4b       85*/
      d=d-dx                                                                    !/*we4b       86*/
      go to 100                                                                 !/*we4b       87*/
  110 k=nint(wgm(im-3))+nf                                                      !/*we4b       88*/
      go to 150                                                                 !/*we4b       89*/
!                                                                               !/*we4b       90*/
!        decreasing fine mesh size.                                             !/*we4b       91*/
  120 dx=wp/(one-rr**np)*(one-rr)                                               !/*we4b       92*/
      nf=1                                                                      !/*we4b       93*/
      d=wgm(im-2)+dx                                                            !/*we4b       94*/
  130 if(r.le.d)go to 140                                                       !/*we4b       95*/
      nf=nf+1                                                                   !/*we4b       96*/
      dx=dx*rr                                                                  !/*we4b       97*/
      d=d+dx                                                                    !/*we4b       98*/
      go to 130                                                                 !/*we4b       99*/
  140 k=nint(wgm(im-3))+nf                                                      !/*we4b      100*/
!                                                                               !/*we4b      101*/
!        determine the mesh index ix.                                           !/*we4c2       7*/
  150 ix=ix+m*(k-1)                                                             !/*we4c2       8*/
      m=m*nint(wwm(i))                                                          !/*we4b      104*/
  160 ia=ia+3*(nint(wwm(i+6))+1)                                                !/*we4b      105*/
!                                                                               !/*we4b      106*/
!        adjust the mesh index and get the weight-window value.                 !/*we4b      107*/
      if(nwang.eq.1)ix=4*ix-3                                                   !/*we4c2       9*/
      ip=lwwf+ix+nwwm*(nb-1+mww(ny))                                            !/*we4c2      10*/
      wwval=wwf(ip)                                                             !/*we4b      110*/
      if(nwang.ne.1.or.na.eq.0)return                                           !/*we4b      111*/
!                                                                               !/*we4b      112*/
!        use the adjoint current to calculate angular weighting.                !/*we4b      113*/
      wa=2.d0                                                                   !/*we4b      114*/
      s=0.                                                                      !/*we4c        5*/
      do 170 i=1,3                                                              !/*we4b      116*/
  170 s=s+wwf(ip+i)*wwf(ip+i)                                                   !/*we4b      117*/
      dj=sqrt(s)                                                                !/*we4b      118*/
      if(dj.eq.0.)go to 190                                                     !/*we4b      119*/
      do 180 i=1,3                                                              !/*we4b      120*/
      q(i)=wwf(ip+i)/dj                                                         !/*we4b      121*/
      x(i)=gpblcm(3+i)                                                          !/*we4b      122*/
  180 if(lev.ne.0)x(i)=udt(3+i,0)                                               !/*we4b      123*/
!                                                                               !/*we4b      124*/
!        find dot product of particle direction and adjoint current.            !/*we4b      125*/
      d=dotpro(q,x)                                                             !/*we4b      126*/
!                                                                               !/*we4b      127*/
!        calculate the average cosine (adjoint current/adjoint flux).           !/*we4b      128*/
!        provide an upper bound to ensure reasonable weight-windows.            !/*we4b      129*/
!        a value of zero implies an isotropic adjoint flux.                     !/*we4b      130*/
      ac=min(dj*wwval,zero+.99)                                                 !/*we4b      131*/
      if(ac.lt.1.e-6)go to 190                                                  !/*we4b      132*/
!                                                                               !/*we4b      133*/
!        set the maximum entropy parameter using the average cosine.            !/*we4b      134*/
      if(ac.lt.0.8001)u=ac*(2.99821d0-2.2669248d0*ac)/                          !/*we4b      135*/
     & (one-ac*(.769332d0+ac*(.519928d0-.2691594d0*ac)))                        !/*we4b      136*/
      if(ac.ge.0.8001)u=one/(one-ac)                                            !/*we4b      137*/
!                                                                               !/*we4b      138*/
!        calculate the angular weighting factor.                                !/*we4b      139*/
      wa=(exp(u*(one-d))-exp(-u*(one+d)))/u                                     !/*we4b      140*/
  190 wwval=wwval*wa                                                            !/*we4b      141*/
      return                                                                    !/*we4b      142*/
!                                                                               !/*we4b      143*/
!        print a warning if outside the weight-window mesh.                     !/*we4b      144*/
  200 ix=0                                                                      !/*we4c2      11*/
      if(iovr.eq.4)call errprn(1,nwsg(3),1,zero+ncl(lncl+icl),zero              !/*we4c2      12*/
     & ,'icl',' ','particle exited weight-window mesh.')                        !/*we4c2      13*/
      return                                                                    !/*we4b      147*/
      end                                                                       !/*we4b      148*/
