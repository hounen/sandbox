!#ident acefcn.F from deck ai                                                   !/*ai          1*/
      function acefcn(l,eg,ln)                                                  !/*ai          2*/
!        evaluate a function of eg from the table at xss(l).                    !/*ai          3*/
!        also return ln, the length of the table.                               !/*ai          4*/
!        data:  nr,nbttc(i=1,nr),int(i=1,nr),nf,e(i=1,nf),f(i=1,nf)             !/*ai          5*/
      use global_data                                                           !/*aix         1*/
!                                                                               !/*aix         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*aix         3*/
      implicit integer(kindi) (i-n)                                             !/*aix         4*/
!                                                                               !/*aix         5*/
!                                                                               !/*ai          7*/
      nr=nint(xss(l))                                                           !/*ai4c        1*/
      ie=l+2*nr+1                                                               !/*ai          9*/
      nf=nint(xss(ie))                                                          !/*ai4c        2*/
      ln=2*(nr+nf+1)                                                            !/*ai         11*/
!                                                                               !/*ai         12*/
!        use the extreme value if eg is off either end of the table.            !/*ai         13*/
      if(eg.ge.xss(ie+nf))go to 110                                             !/*ai         14*/
      if(eg.le.xss(ie+1))go to 120                                              !/*ai         15*/
!                                                                               !/*ai         16*/
!        binary search for the location of eg in the table.                     !/*ai         17*/
      ic=ie+1                                                                   !/*ai         18*/
      ib=ie+nf                                                                  !/*ai         19*/
   10 if(ib-ic.eq.1)go to 30                                                    !/*ai         20*/
      ih=(ic+ib)/2                                                              !/*ai         21*/
      if(eg.lt.xss(ih))go to 20                                                 !/*ai         22*/
      ic=ih                                                                     !/*ai         23*/
      go to 10                                                                  !/*ai         24*/
   20 ib=ih                                                                     !/*ai         25*/
      go to 10                                                                  !/*ai         26*/
   30 ea=xss(ic)                                                                !/*ai         27*/
      eb=xss(ib)                                                                !/*ai         28*/
      fa=xss(ic+nf)                                                             !/*ai         29*/
      fb=xss(ib+nf)                                                             !/*ai         30*/
!                                                                               !/*ai         31*/
!        find out which kind of interpolation should be used.                   !/*ai         32*/
      if(nr.eq.0)go to 70                                                       !/*ai         33*/
      do 40 n=1,nr                                                              !/*ai         34*/
   40 if(ib-ie.le.nint(xss(l+n)))go to 50                                       !/*ai4c        3*/
      n=nr                                                                      !/*ai         36*/
!                                                                               !/*ai         37*/
!        interpolate between table entries.                                     !/*ai         38*/
   50 go to(60,70,80,90,100)nint(xss(l+nr+n))                                   !/*ai4c        4*/
   60 acefcn=fa                                                                 !/*ai         40*/
      return                                                                    !/*ai         41*/
   70 acefcn=fa+(fb-fa)*(eg-ea)/(eb-ea)                                         !/*ai         42*/
      return                                                                    !/*ai         43*/
   80 acefcn=fa+(fb-fa)*log(eg/ea)/log(eb/ea)                                   !/*ai         44*/
      return                                                                    !/*ai         45*/
   90 acefcn=fa*(fb/fa)**((eg-ea)/(eb-ea))                                      !/*ai         46*/
      return                                                                    !/*ai         47*/
  100 acefcn=fa*(fb/fa)**(log(eg/ea)/log(eb/ea))                                !/*ai         48*/
      return                                                                    !/*ai         49*/
!                                                                               !/*ai         50*/
  110 acefcn=xss(ie+2*nf)                                                       !/*ai         51*/
      return                                                                    !/*ai         52*/
  120 acefcn=xss(ie+nf+1)                                                       !/*ai         53*/
      return                                                                    !/*ai         54*/
      end                                                                       !/*ai         55*/
