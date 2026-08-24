!#ident acenu.F from deck an                                                    !/*an          1*/
      function acenu(l)                                                         !/*an4c        1*/
!        calculate the average number of neutrons from fission.                 !/*an          3*/
!        the data for all real nuclides are such that acenu will always         !/*an          4*/
!        be greater than 1 and almost always greater than 2.                    !/*an          5*/
!        data (for polynomial function):  lnu,nc,(c(i),i=1,nc)                  !/*an          6*/
!        l is the location of the nubar table.                                  !/*an4c        2*/
      use global_data                                                           !/*anx         1*/
!                                                                               !/*anx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*anx         3*/
      implicit integer(kindi) (i-n)                                             !/*anx         4*/
!                                                                               !/*anx         5*/
!                                                                               !/*an          8*/
!        find out which kind of calculation to do.                              !/*an          9*/
      if(nint(xss(l)).ne.1)go to 20                                             !/*anx         6*/
!                                                                               !/*an         12*/
!        calculate acenu by evaluating a polynomial in energy.                  !/*an         13*/
      n=nint(xss(l+1))-1                                                        !/*anx         7*/
      acenu=xss(l+n+2)                                                          !/*an         15*/
      do 10 i=1,n                                                               !/*an         16*/
   10 acenu=acenu*erg+xss(l+n+2-i)                                              !/*an         17*/
      return                                                                    !/*an         18*/
!                                                                               !/*an         19*/
!        calculate acenu by interpolation in a table.                           !/*an         20*/
   20 acenu=acefcn(l+1,erg,i)                                                   !/*an         21*/
      return                                                                    !/*an         22*/
      end                                                                       !/*an         23*/
