      subroutine runpht(z, a, ue, ecv_, eph_, elv_, tph_,                       !/*ru          1*/
     &                  nph_, ipht_, icc_, maxph)                               !/*ru          2*/
#ifdef CEM                                                                      /*!ru          3*/
! ======================================================================        !/*ru          4*/
!                                                                               !/*ru          5*/
!   Decouples the PHT call from CEM                                             !/*ru          6*/
!                                                                               !/*ru          7*/
!   Written by F. Gallmeier, ORNL CPED, June 1998.                              !/*ru          8*/
!                                                                               !/*ru          9*/
! ======================================================================        !/*ru         10*/
!                                                                               !/*ru         11*/
!  Input:                                                                       !/*ru         12*/
!         z     - charge of excited nucleus                                     !/*ru         13*/
!         a     - atomic mass of excited nucleus                                !/*ru         14*/
!         ue    - extitation energy                                             !/*ru         15*/
!         nph_  - start to store photons at the                                 !/*ru         16*/
!                 array element npht+1 of the PHT photon bank                   !/*ru         17*/
!         ipht_ - flag indicating to envoke PHT photon production               !/*ru         18*/
!         icc_  - PHT option flag                                               !/*ru         19*/
!         maxph - maximum of PHT photon to be created                           !/*ru         20*/
!                                                                               !/*ru         21*/
!  Output:                                                                      !/*ru         22*/
!         ecv_  - ?                                                             !/*ru         23*/
!         ue    - rest excitation energy                                        !/*ru         24*/
!         nph_  - pointer to last arrayelement into PHT photon bank             !/*ru         25*/
!         eph_  - array of PHT photon energies                                  !/*ru         26*/
!         elv_  - not used in MCNPX                                             !/*ru         27*/
!         tph_  - delaytime of photon production                                !/*ru         28*/
!                                                                               !/*ru         29*/
#include <cem/cdouble.h>                                                        /*!ru         30*/
      dimension eph_(1), elv_(1), tph_(1)                                       !/*ru         31*/
!                                                                               !/*ru         32*/
!  decouple variables                                                           !/*ru         33*/
        zz=z                                                                    !/*ru         34*/
        aa=a                                                                    !/*ru         35*/
        uu=ue                                                                   !/*ru         36*/
        ecv_=dp0                                                                !/*ru         37*/
        if (aa.gt.dp0.and.uu.gt.dp0.and.ipht_.ne.0) then                        !/*ru         38*/
           call pht(aa,zz,uu,ecv_,eph_,elv_,tph_,nph_,maxph,icc_)               !/*ru         39*/
           ue=uu                                                                !/*ru         40*/
        endif                                                                   !/*ru         41*/
!                                                                               !/*ru         42*/
#endif                                                                          /*!ru         43*/
        return                                                                  !/*ru         44*/
        end                                                                     !/*ru         45*/
