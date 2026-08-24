!#ident acetbl.F from deck ab                                                   !/*ab          1*/
      subroutine acetbl(l,ii,r,ln)                                              !/*ab          2*/
!        get interpolation parameters r and ii of the energy in the             !/*ab          3*/
!        table at xss(l).                                                       !/*ab          4*/
!        data:  nr,nbttc(i=1,nr),int(i=1,nr),nf,e(i=1,nf)                       !/*ab          5*/
      use global_data                                                           !/*abx         1*/
!                                                                               !/*abx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*abx         3*/
      implicit integer(kindi) (i-n)                                             !/*abx         4*/
!                                                                               !/*abx         5*/
!                                                                               !/*ab          7*/
      nr=nint(xss(l))                                                           !/*ab4c        1*/
      ie=l+2*nr+1                                                               !/*ab          9*/
      nf=nint(xss(ie))                                                          !/*ab4c        2*/
      ln=2*(nr+1)+nf                                                            !/*ab         11*/
      r=0.                                                                      !/*ab         12*/
!                                                                               !/*ab         13*/
!        use extreme value if energy is off either end of the table.            !/*ab         14*/
      if(erg.ge.xss(ie+nf))go to 70                                             !/*ab         15*/
      if(erg.le.xss(ie+1))go to 80                                              !/*ab         16*/
!                                                                               !/*ab         17*/
!        binary search for the location of the energy in the table.             !/*ab         18*/
      ic=ie+1                                                                   !/*ab         19*/
      ib=ie+nf                                                                  !/*ab         20*/
   10 if(ib-ic.eq.1)go to 30                                                    !/*ab         21*/
      ih=(ic+ib)/2                                                              !/*ab         22*/
      if(erg.lt.xss(ih))go to 20                                                !/*ab         23*/
      ic=ih                                                                     !/*ab         24*/
      go to 10                                                                  !/*ab         25*/
   20 ib=ih                                                                     !/*ab         26*/
      go to 10                                                                  !/*ab         27*/
   30 ii=ic-ie                                                                  !/*ab         28*/
!                                                                               !/*ab         29*/
!        calculate interpolation fraction r unless int=1 (histogram).           !/*ab         30*/
      if(nr.eq.0)go to 60                                                       !/*ab         31*/
      do 40 n=1,nr                                                              !/*ab         32*/
   40 if(ib-ie.le.nint(xss(l+n)))go to 50                                       !/*ab4c        3*/
      n=nr                                                                      !/*ab         34*/
   50 if(nint(xss(l+nr+n)).eq.1)return                                          !/*ab4c        4*/
   60 if(erg-xss(ic).lt.1.e-6*(xss(ib)-xss(ic)))return                          !/*ab4b        1*/
      r=(erg-xss(ic))/(xss(ib)-xss(ic))                                         !/*ab4a        2*/
      return                                                                    !/*ab         37*/
!                                                                               !/*ab         38*/
   70 ii=nf                                                                     !/*ab         39*/
      return                                                                    !/*ab         40*/
   80 ii=1                                                                      !/*ab         41*/
      return                                                                    !/*ab         42*/
      end                                                                       !/*ab         43*/
