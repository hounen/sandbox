!#ident prssrj.F from deck rj                                                   !/*rj          1*/
      subroutine prssrj                                                         !/*rj          2*/
!        print summary of the performance of the surface source.                !/*rj          3*/
      use global_data                                                           !/*rjx         1*/
!                                                                               !/*rjx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*rjx         3*/
      implicit integer(kindi) (i-n)                                             !/*rjx         4*/
!                                                                               !/*rjx         5*/
!                                                                               !/*rj          5*/
!        print summary information.                                             !/*rj          6*/
      write(iuo,10)rssa,np1,nrrs,nqss,ntss                                      !/*rj          7*/
   10 format(52h1summary information from this use of surface-source,           !/*rj          8*/
     & 6h file ,a8,38x,15hprint table 170//                                     !/*rj          9*/
     & 5x,28hnumber of original histories,i14/                                  !/*rj         10*/
     & 5x,22hnumber of records read,i20/                                        !/*rj         11*/
     & 5x,28hnumber of recorded histories,i14/                                  !/*rj         12*/
     & 5x,24hnumber of histories read,i18)                                      !/*rj         13*/
!                                                                               !/*rj         14*/
!        print the rejected particles, weights and fractions.                   !/*rj         15*/
      tb=0.                                                                     !/*rj         16*/
      do 20 i=1,7                                                               !/*rj         17*/
   20 tb=tb+wssi(i)                                                             !/*rj         18*/
      write(iuo,30)snit                                                         !/*rj         19*/
   30 format(/5x,28hsource multiplication factor,f13.5)                         !/*rj         20*/
      write(iuo,40)twss,tb,tb/twss                                              !/*rj         21*/
   40 format(/5x,19htotal source weight,1pe35.5/                                !/*rj         22*/
     & 5x,21htotal rejected weight,e33.5/                                       !/*rj         23*/
     & 5x,24hfraction weight rejected,0pe30.5)                                  !/*rj         24*/
      write(iuo,50)nssi(1),wssi(1),wssi(1)/twss                                 !/*rj         25*/
   50 format(/5x,35hparticles rejected on particle type,i19/                    !/*rj         26*/
     & 5x,29hparticle type rejected weight,1pe25.5/                             !/*rj         27*/
     & 5x,38hfraction weight particle type rejected,0pe16.5)                    !/*rj         28*/
      write(iuo,60)nssi(2),wssi(2),wssi(2)/twss                                 !/*rj         29*/
   60 format(/5x,25hparticles rejected on ncp,i29/                              !/*rj         30*/
     & 5x,19hncp rejected weight,1pe35.5/                                       !/*rj         31*/
     & 5x,28hfraction weight ncp rejected,0pe26.5)                              !/*rj         32*/
      write(iuo,70)nssi(3),wssi(3),wssi(3)/twss                                 !/*rj         33*/
   70 format(/5x,37hparticles rejected on surface or cell,i17/                  !/*rj         34*/
     & 5x,23hsurface rejected weight,1pe31.5/                                   !/*rj         35*/
     & 5x,32hfraction weight surface rejected,0pe22.5)                          !/*rj         36*/
      write(iuo,80)nssi(5),wssi(5),wssi(5)/twss                                 !/*rj         37*/
   80 format(/5x,28hparticles rejected on energy,i26/                           !/*rj         38*/
     & 5x,22henergy rejected weight,1pe32.5/                                    !/*rj         39*/
     & 5x,31hfraction weight energy rejected,0pe23.5)                           !/*rj         40*/
      write(iuo,90)nssi(6),wssi(6),wssi(6)/twss                                 !/*rj         41*/
   90 format(/5x,26hparticles rejected on time,i28/                             !/*rj         42*/
     & 5x,20htime rejected weight,1pe34.5/                                      !/*rj         43*/
     & 5x,29hfraction weight time rejected,0pe25.5)                             !/*rj         44*/
      write(iuo,100)nssi(8)                                                     !/*rj         45*/
  100 format(/5x,32hparticles rejected by nps limits,i22/                       !/*rj         46*/
     & 5x,51hno weight loss associated with this rejection mode.)               !/*rj         47*/
      if(nrcd.eq.6)write(iuo,110)nssi(7),wssi(7),wssi(7)/twss                   !/*rj         48*/
  110 format(/5x,27hparticles rejected on angle,i27/                            !/*rj         49*/
     & 5x,21hangle rejected weight,1pe33.5/                                     !/*rj         50*/
     & 5x,30hfraction weight angle rejected,0pe24.5)                            !/*rj         51*/
      nj=0                                                                      !/*rj         52*/
      do 120 i=1,8                                                              !/*rj         53*/
  120 nj=nj+nssi(i)                                                             !/*rj         54*/
      write(iuo,130)nj                                                          !/*rj         55*/
  130 format(//5x,34htotal number of particles rejected,i20)                    !/*rj         56*/
      write(iuo,140)nrrs-nj,twac,twac/twss                                      !/*rj         57*/
  140 format(/5x,34htotal number of particles accepted,i20/                     !/*rj         58*/
     & 5x,21htotal weight accepted,1pe33.5/                                     !/*rj         59*/
     & 5x,33hfraction of total weight accepted,0pe21.5)                         !/*rj         60*/
      return                                                                    !/*rj         61*/
      end                                                                       !/*rj         62*/
