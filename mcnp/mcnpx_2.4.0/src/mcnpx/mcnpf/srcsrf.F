!#ident srcsrf.F from deck sf                                                   !/*sf          1*/
      subroutine srcsrf(m)                                                      !/*sf          2*/
!        check that surface m is an acceptable source surface.                  !/*sf          3*/
!        convert it to simplest form if it is an unskewed spheroid.             !/*sf          4*/
      use global_data                                                           !/*sfx         1*/
      use imcn_data                                                             !/*sfx         2*/
!                                                                               !/*sfx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*sfx         4*/
      implicit integer(kindi) (i-n)                                             !/*sfx         5*/
!                                                                               !/*sfx         6*/
!                                                                               !/*sf          7*/
      k=kst(lkst+m)                                                             !/*sf4a        1*/
      if(k.le.9)return                                                          !/*sf          9*/
      if(k.ne.22)go to 10                                                       !/*sf         10*/
      l=lsc(llsc+m)                                                             !/*sf         11*/
      if((scf(l+1).eq.scf(l+2).or.scf(l+2).eq.scf(l+3).or.                      !/*sf         12*/
     & scf(l+3).eq.scf(l+1)).and.scf(l+1)*scf(l+2).gt.0..and.                   !/*sf         13*/
     & scf(l+2)*scf(l+3).gt.0.)go to 20                                         !/*sf         14*/
   10 call erprnt(1,1,1,nsf(lnsf+m),0,0,0,0,                                    !/*sf         15*/
     & '14hsource surface,i7,34h is not plane, sphere or spheroid.')            !/*sf         16*/
      return                                                                    !/*sf         17*/
!                                                                               !/*sf         18*/
!        convert coefficients of source spheroid to simplest form.              !/*sf         19*/
   20 do 30 j=1,3                                                               !/*sf         20*/
      scf(l+7)=scf(l+7)-scf(l+3+j)**2/scf(l+j)                                  !/*sf         21*/
      scf(l+7+j)=scf(l+7+j)-scf(l+3+j)/scf(l+j)                                 !/*sf         22*/
   30 scf(l+3+j)=0.                                                             !/*sf         23*/
      a=scf(l+1)                                                                !/*sf         24*/
      if(scf(l+2).eq.scf(l+3))a=scf(l+2)                                        !/*sf         25*/
      do 40 j=1,7                                                               !/*sf         26*/
   40 scf(l+j)=scf(l+j)/a                                                       !/*sf         27*/
      return                                                                    !/*sf         28*/
      end                                                                       !/*sf         29*/
