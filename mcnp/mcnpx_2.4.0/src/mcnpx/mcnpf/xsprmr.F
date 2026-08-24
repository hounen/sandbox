!#ident xsprmr.F from deck xp4b                                                 !/*xp4b        1*/
      subroutine xsprmr(j)                                                      !/*xp4b        3*/
#ifdef MCPLOT                                                                   /*!xpx         1*/
!        xsprmr prints to unit j a primer on xs plotting.                       !/*xp4b        4*/
      use global_data                                                           !/*xpx         2*/
      use mcplot_data                                                           !/*xpx         3*/
!                                                                               !/*xpx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*xpx         5*/
      implicit integer(kindi) (i-n)                                             !/*xpx         6*/
!                                                                               !/*xpx         7*/
      write(j,10)                                                               !/*xp4b        7*/
   10 format(' primer on cross section (xs) plotting'//                         !/*xp4c        1*/
     & ' there are three directives:'//                                         !/*xp4b        9*/
     & '     xs  =  material or zaid (required)'/                               !/*xp4b       10*/
     & '     mt  =  reaction number'/                                           !/*xp4b       11*/
     & '     par =  particle type'//                                            !/*xp4b       12*/
     & ' the xs directive performs a cross section plot.'/                      !/*xp4c        2*/
     & ' examples of the xs directive are:'/                                    !/*xp4c        3*/
     & '     xs=m1           ; plot xs for material 1'/                         !/*xp4b       15*/
     & '     xs=13027.50c    ; plot xs for nuclide 13027.50c'/)                 !/*xp4b       16*/
      write(j,20)(klbl(i),hlbl(i),i=1,6)                                        !/*xp4b       17*/
   20 format(' the reaction number directive, mt, selects a particular'/        !/*xp4b       18*/
     & ' xs reaction to plot. (mt=1 default)'/                                  !/*xp4b       19*/
     & ' mt=0 lists available mts.'/                                            !/*xp4b       20*/
     & ' for neutrons, mt are the reactions given in user manual',              !/*xp4c        4*/
     & ' appendix g.'/3x,'note that some mts may have been expunged.'/          !/*xp4b       22*/
     & 3x,'the following special values are also available:'/                   !/*xp4b       23*/
     & (5x,4hmt =,i3,5x,a40))                                                   !/*xp4b       24*/
      write(j,30)(klbl(i),hlbl(i),i=11,16)                                      !/*xp4b       25*/
   30 format(' for photons the supported mts are:'/                             !/*xp4b       26*/
     & (5x,4hmt =,i3,5x,a40))                                                   !/*xp4b       27*/
      write(j,40)(klbl(i),hlbl(i),i=31,43)                                      !/*xp4b       28*/
   40 format(' for electrons the supported mts are columns from print',         !/*xp4b       29*/
     & ' table 85:'/(5x,4hmt =,i3,5x,a40))                                      !/*xp4b       30*/
      write(j,50)(klbl(i),hlbl(i),i=8,10)                                       !/*xp4b       31*/
   50 format(' for s(a,b) the supported mts are:'/(5x,4hmt =,i3,5x,a40))        !/*xp4b       32*/
      write(j,60)(klbl(i),hlbl(i),i=21,27)                                      !/*xp4b       33*/
   60 format(' for multigroup data the supported mts are edits (mt>0)',         !/*xp4b       34*/
     & ' and:'/(5x,4hmt =,i3,5x,a40))                                           !/*xp4b       35*/
      write(j,70)                                                               !/*xp4b       36*/
   70 format(/' allowed particle types are:'/                                   !/*xp4b       37*/
     & '    par=n   neutron data (default)'/'    par=p   photon data'/          !/*xp4b       38*/
     & '    par=e   electron data (only specfied for materials)')               !/*xp4b       39*/
      write(j,80)                                                               !/*xp4b       40*/
   80 format(/' only cross section data loaded from the inp file',              !/*xp4b       41*/
     & ' can be plotted.'/3x,'some data may have been expunged or',             !/*xp4b       42*/
     & ' may not be loaded in the'/3x,'particular mode requested.'/)            !/*xp4b       43*/
      write(j,90)                                                               !/*xp4b       44*/
   90 format(' for postscript plotting:'/                                       !/*xp4b       45*/
     & 3x,4hfile,8x,26hsend next plot to plotm.ps,/                             !/*xp4b       46*/
     & 3x,8hfile all,4x,32hsend remaining plots to plotm.ps,/                   !/*xp4b       47*/
     & 3x,9hfile none,3x,30hsend no more plots to plotm.ps,/)                   !/*xp4b       48*/
#endif                                                                          /*!xpx         8*/
      return                                                                    !/*xp4b       49*/
      end                                                                       !/*xp4b       50*/
