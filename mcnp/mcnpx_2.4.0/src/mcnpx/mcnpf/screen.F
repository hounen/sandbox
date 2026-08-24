!#ident screen.F from deck ee                                                   !/*ee          1*/
      function screen(ar)                                                       !/*ee          2*/
!        calculate the screening factor for the thomas-fermi model.             !/*ee          3*/
      use global_parameters                                                     !/*eex         1*/
      implicit real(kindr) (a-h,o-z)                                            !/*eex         2*/
      implicit integer(kindi) (i-n)                                             !/*eex         3*/
      dimension a(3),b(3)                                                       !/*ee          4*/
      data a/.1,.55,.35/,b/1.,.2,.05/                                           !/*ee          5*/
!                                                                               !/*ee          6*/
      screen=0.                                                                 !/*ee          7*/
      do 40 i=1,3                                                               !/*ee          8*/
      do 40 j=1,3                                                               !/*ee          9*/
      s=(ar*b(j))**2                                                            !/*ee         10*/
      if(s.gt..1)go to 20                                                       !/*ee         11*/
      tl=0.                                                                     !/*ee         12*/
      t=-1.                                                                     !/*ee         13*/
      do 10 n=1,9                                                               !/*ee         14*/
      if(abs(t).lt.n*1e-9)go to 30                                              !/*ee         15*/
      t=-t*s                                                                    !/*ee         16*/
   10 tl=tl+t/n                                                                 !/*ee         17*/
      go to 30                                                                  !/*ee         18*/
   20 tl=log(1.+s)                                                              !/*ee         19*/
   30 if(i.eq.j)screen=screen-.5*tl*a(i)**2                                     !/*ee         20*/
   40 if(i.ne.j)screen=screen+a(i)*a(j)*((1.+s)*tl/((ar*b(i))**2-s)+.5)         !/*ee         21*/
      return                                                                    !/*ee         22*/
      end                                                                       !/*ee         23*/
