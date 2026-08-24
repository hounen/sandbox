!     $Id: en.F,v 1.4 2002/03/27 18:15:02 gwm Exp $                           !/*en          1*/
      function en (dmass,pmass,ws4,beta)                                        !/*en          2*/
#include <lcs/double.h>                                                         /*!en          3*/
!     ==================================================================        !/*en          4*/
!     en is called by aitota to check if the isobar has enough                  !/*en          5*/
!     energy to decay, that is to have enough energy to stay                    !/*en          6*/
!     above the fermi level.                                                    !/*en          7*/
!     assume parallel decay because this always gives more energy than          !/*en          8*/
!     when the particles come off opposite each other. for more details         !/*en          9*/
!     see notes and jackson book on classical electrodynamics,page 395.         !/*en         10*/
!     dmass and pmass are the masses of the decaying nucleon and pion.          !/*en         11*/
!                                                                               !/*en         12*/
! EN     calls   SQRT                                                           !/*en         13*/
! called by AITOTA                                                              !/*en         14*/
!     ==================================================================        !/*en         15*/
!                                                                               !/*en         16*/
      dmass2=dmass*dmass                                                        !/*en         17*/
      pmass2=pmass*pmass                                                        !/*en         18*/
! ws4 is the mass of the isobar.                                                !/*en         19*/
      amass2=ws4*ws4                                                            !/*en         20*/
! betapn is the beta of the nucleon in the center of mass when the              !/*en         21*/
!  isobar is at rest.                                                           !/*en         22*/
! then do lorentz transformation (see jackson p.361 and notes) to bring         !/*en         23*/
!  particle into the moving frame of isobar(or the lab frame).                  !/*en         24*/
      betapn=amass2+dmass2-pmass2                                               !/*en         25*/
      betapn=(betapn*betapn-dp4*amass2*dmass2)/(betapn*betapn)                  !/*en         26*/
      if (betapn.lt.dp0) betapn=dp0                                             !/*en         27*/
      betapn=sqrt(betapn)                                                       !/*en         28*/
! betan and en are the beta and energy of the decay nucleon in the lab          !/*en         29*/
!  frame.                                                                       !/*en         30*/
      betan=(betapn+beta)/(dp1+betapn*beta)                                     !/*en         31*/
      en=dmass/(sqrt(dp1-betan*betan))                                          !/*en         32*/
      return                                                                    !/*en         33*/
      end                                                                       !/*en         34*/
