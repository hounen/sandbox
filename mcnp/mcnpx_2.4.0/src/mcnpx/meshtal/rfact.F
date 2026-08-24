!     $Id: rfact.F,v 1.4 2002/03/27 18:16:19 gwm Exp $                        !/*rf          1*/
      function rfact(n1,n2,n3,rm,eg,n)                                          !/*rf          2*/
#ifndef MESHTAL                                                                 /*!rf          3*/
      rfact=0.                                                                  !/*rf          4*/
#else                                                                           /*!rf          5*/
!        Multiply by special dose function.                                     !/*rf          6*/
!        Written by E. C. Snow 11/20/97.  <revised 02/09/99>                    !/*rf          7*/
      use global_data                                                           !/*rfxc        1*/
!                                                                               !/*rfxc        2*/
      implicit real(kindr) (a-h,o-z)                                            !/*rfxc        3*/
      implicit integer(kindi) (i-n)                                             !/*rfxc        4*/
!                                                                               !/*rfxc        5*/
!                                                                               !/*rf          9*/
      l=lemsh(n1)                                                               !/*rf         10*/
!                                                                               !/*rf         11*/
!        use extreme value if enegy is off either end of the table.             !/*rf         12*/
      if(eg.le.emsh(l+1))go to 80                                               !/*rf         13*/
      if(eg.ge.emsh(l+n))go to 90                                               !/*rf         14*/
!                                                                               !/*rf         15*/
!        binary search for the location of the energy in the table.             !/*rf         16*/
      ic=l+1                                                                    !/*rf         17*/
      ib=l+n                                                                    !/*rf         18*/
   10 if(ib-ic.eq.1)go to 30                                                    !/*rf         19*/
      ih=(ic+ib)/2                                                              !/*rf         20*/
      if(eg.lt.emsh(ih))go to 20                                                !/*rf         21*/
      ic=ih                                                                     !/*rf         22*/
      go to 10                                                                  !/*rf         23*/
   20 ib=ih                                                                     !/*rf         24*/
      go to 10                                                                  !/*rf         25*/
!                                                                               !/*rf         26*/
!        interpolate for the value of the conversion factor.                    !/*rf         27*/
   30 go to (40,50)n2                                                           !/*rf         28*/
   40 rfact=rm*(emsh(n+ic)+(emsh(n+ic+1)-emsh(n+ic))*(eg-emsh(ic))/             !/*rf         29*/
     & (emsh(ic+1)-emsh(ic)))                                                   !/*rf         30*/
      return                                                                    !/*rf         31*/
   50 rfact=rm*(emsh(n+ic)+(emsh(n+ic+1)-emsh(n+ic))*log(eg/emsh(ic))/          !/*rf         32*/
     & log(emsh(ic+1)/emsh(ic)))                                                !/*rf         33*/
      return                                                                    !/*rf         34*/
!                                                                               !/*rf         35*/
   80 rfact=rm*emsh(l+n+1)                                                      !/*rf         36*/
      return                                                                    !/*rf         37*/
   90 rfact=rm*emsh(l+n+n)                                                      !/*rf         38*/
#endif                                                                          /*!rf         39*/
      return                                                                    !/*rf         40*/
      end                                                                       !/*rf         41*/
