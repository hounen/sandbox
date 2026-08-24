      subroutine fisstat(iuo)                                                   !/*fi          1*/
#ifdef CEM                                                                      /*!fi          2*/
! ======================================================================        !/*fi          3*/
!                                                                               !/*fi          4*/
!   CEM95 written by S. G. Mashnik                                              !/*fi          5*/
!                                                                               !/*fi          6*/
!   Prints fission fragment statistics                                          !/*fi          7*/
!                                                                               !/*fi          8*/
! ======================================================================        !/*fi          9*/
!                                                                               !/*fi         10*/
!   Written by F.Gallmeier, ORNL  CPED  March 1998                              !/*fi         11*/
!   Changed to double by F.Gallmeier, ORNL  CPED  October 1998                  !/*fi         12*/
!                                                                               !/*fi         13*/
! ======================================================================        !/*fi         14*/
#include <cem/cdouble.h>                                                        /*!fi         15*/
!                                                                               !/*fi         16*/
!     arrays for fragment statistics                                            !/*fi         17*/
      dimension afrag(200), zfrag(100)                                          !/*fi         18*/
      common /fragdt/ afrag, zfrag, afrtt, afrsq, afrmin, afrmax,               !/*fi         19*/
     &                zfrtt, zfrsq, zfrmin, zfrmax, ekfrtt, ekfrsq,             !/*fi         20*/
     &                ekfrmn, ekfrmx, eufrtt, eufrsq, eufrmn, eufrmx,           !/*fi         21*/
     &                nfrag                                                     !/*fi         22*/
      if (nfrag.gt.0) then                                                      !/*fi         23*/
         fnfrag = nfrag                                                         !/*fi         24*/
         do 10 i=1,200                                                          !/*fi         25*/
            afrag(i) = afrag(i)/fnfrag                                          !/*fi         26*/
   10    continue                                                               !/*fi         27*/
         do 20 i=1,100                                                          !/*fi         28*/
            zfrag(i) = zfrag(i)/fnfrag                                          !/*fi         29*/
   20    continue                                                               !/*fi         30*/
         write (iuo, 1250)                                                      !/*fi         31*/
         do 30 i=1,100                                                          !/*fi         32*/
            write(iuo, 1251) i, afrag(i), zfrag(i)                              !/*fi         33*/
   30    continue                                                               !/*fi         34*/
         do 40 i=101,200                                                        !/*fi         35*/
            write(iuo, 1252) i, afrag(i)                                        !/*fi         36*/
   40    continue                                                               !/*fi         37*/
         afrtav = afrtt/fnfrag                                                  !/*fi         38*/
         afrtsq = sqrt(abs(afrsq/fnfrag) - afrtav*afrtav)                       !/*fi         39*/
         zfrtav = zfrtt/fnfrag                                                  !/*fi         40*/
         zfrtsq = sqrt(abs(zfrsq/fnfrag) - zfrtav*zfrtav)                       !/*fi         41*/
         ekfrtav = ekfrtt/fnfrag                                                !/*fi         42*/
         ekfrtsq = sqrt( abs(ekfrsq/fnfrag) - ekfrtav*ekfrtav)                  !/*fi         43*/
         eufrtav = eufrtt/fnfrag                                                !/*fi         44*/
         eufrtsq = sqrt(  abs(eufrsq/fnfrag) - eufrtav*eufrtav)                 !/*fi         45*/
         write (iuo, 1260) nfrag, zfrtav, zfrtsq, zfrmin, zfrmax,               !/*fi         46*/
     &                        afrtav, afrtsq, afrmin, afrmax,                   !/*fi         47*/
     &                        ekfrtav, ekfrtsq, ekfrmn, ekfrmx,                 !/*fi         48*/
     &                        eufrtav, eufrtsq, eufrmn, eufrmx                  !/*fi         49*/
      else                                                                      !/*fi         50*/
         write (iuo, 1300)                                                      !/*fi         51*/
      endif                                                                     !/*fi         52*/
 1250 format (/2x,'Mass and Charge distribution of fission fragments'/          !/*fi         53*/
     &        'Mass/Charge    Mass-       Charge- distrubution  ')              !/*fi         54*/
 1251 format (2x,i4,2(2x,f10.6))                                                !/*fi         55*/
 1252 format (2x,i4,2x,f10.6)                                                   !/*fi         56*/
 1260 format (/2x,'Mean mass, charge, kinetic energy, excitation',              !/*fi         57*/
     &         'energy of the ', i7,' fission fragments:',                      !/*fi         58*/
     &       /2x,' Zav = ',f5.1,' +/-',f5.1,';       Zmin = ',f4.0,             !/*fi         59*/
     &       ';   Zmax = ',f4.0/2x,' Aav = ',f5.1,' +/-',f5.1,';',7x,           !/*fi         60*/
     &       'Amin = ',f4.0,';   Amax = ',f4.0                                  !/*fi         61*/
     &         /2x,'Ekav = ',f5.1,                                              !/*fi         62*/
     &       ' +/-',f5.1,' MeV;  Ekmin = ',f4.1,';  Ekmax = ',f5.1,             !/*fi         63*/
     &         /2x,'Euav = ',f5.1,                                              !/*fi         64*/
     &       ' +/-',f5.1,' MeV;  Eumin = ',f4.1,';  Eumax = ',f5.1)             !/*fi         65*/
 1300 format (//'  No fission fragment statistics available:',                  !/*fi         66*/
     &        ' no fission event occured.')                                     !/*fi         67*/
#endif                                                                          /*!fi         68*/
      return                                                                    !/*fi         69*/
      end                                                                       !/*fi         70*/
