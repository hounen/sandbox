!#ident dosef.F from deck do                                                    !/*do          1*/
      function dosef(t)                                                         !/*do          2*/
!        convert tally score t from flux to dose.                               !/*do          3*/
      use global_data                                                           !/*dox         1*/
!                                                                               !/*dox         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*dox         3*/
      implicit integer(kindi) (i-n)                                             !/*dox         4*/
!                                                                               !/*dox         5*/
!                                                                               !/*do          5*/
      l=iptal(lipt+3,2,ital)                                                    !/*do          6*/
      n=tds(l)                                                                  !/*do          7*/
!                                                                               !/*doxc        1*/
!        use built-in dose function.                                            !/*doxc        2*/
      if(n.lt.0)dosef=t*dfact(ipt,nint(tds(l+1)),erg,nint(tds(l-1)),            !/*doxk        1*/
     & nint(tds(l+2)),tds(l+3))                                                 !/*doxc        4*/
      if(n.lt.0)return                                                          !/*doxc        5*/
!                                                                               !/*do          8*/
!        use extreme value if energy is off either end of the table.            !/*do          9*/
      if(erg.ge.tds(l+n))go to 80                                               !/*do         10*/
      if(erg.le.tds(l+1))go to 90                                               !/*do         11*/
!                                                                               !/*do         12*/
!        binary search for the location of the energy in the table.             !/*do         13*/
      ic=l+1                                                                    !/*do         14*/
      ib=l+n                                                                    !/*do         15*/
   10 if(ib-ic.eq.1)go to 30                                                    !/*do         16*/
      ih=(ic+ib)/2                                                              !/*do         17*/
      if(erg.lt.tds(ih))go to 20                                                !/*do         18*/
      ic=ih                                                                     !/*do         19*/
      go to 10                                                                  !/*do         20*/
   20 ib=ih                                                                     !/*do         21*/
      go to 10                                                                  !/*do         22*/
!                                                                               !/*do         23*/
!        interpolate for the value of the conversion factor.                    !/*do         24*/
   30 go to(40,50,60,70)nint(tds(l-1))+1                                        !/*doxc        6*/
   40 dosef=t*tds(n+ic)*(tds(n+ic+1)/tds(n+ic))**                               !/*do         26*/
     & (log(erg/tds(ic))/log(tds(ic+1)/tds(ic)))                                !/*do         27*/
      return                                                                    !/*do         28*/
   50 dosef=t*tds(n+ic)*(tds(n+ic+1)/tds(n+ic))**                               !/*do         29*/
     & ((erg-tds(ic))/(tds(ic+1)-tds(ic)))                                      !/*do         30*/
      return                                                                    !/*do         31*/
   60 dosef=t*(tds(n+ic)+(tds(n+ic+1)-tds(n+ic))*log(erg/tds(ic))/              !/*do         32*/
     & log(tds(ic+1)/tds(ic)))                                                  !/*do         33*/
      return                                                                    !/*do         34*/
   70 dosef=t*(tds(n+ic)+(tds(n+ic+1)-tds(n+ic))*(erg-tds(ic))/                 !/*do         35*/
     & (tds(ic+1)-tds(ic)))                                                     !/*do         36*/
      return                                                                    !/*do         37*/
!                                                                               !/*do         38*/
   80 dosef=t*tds(l+n+n)                                                        !/*do         39*/
      return                                                                    !/*do         40*/
   90 dosef=t*tds(l+n+1)                                                        !/*do         41*/
      return                                                                    !/*do         42*/
      end                                                                       !/*do         43*/
