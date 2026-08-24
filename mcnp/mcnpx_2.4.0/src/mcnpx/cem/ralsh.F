      subroutine ralsh(a,z,ue,v,n,uglmom,nfiss,fision)                          !/*ra          1*/
#ifdef CEM                                                                      /*!ra          2*/
! ======================================================================        !/*ra          3*/
!                                                                               !/*ra          4*/
!   Interface from CEM into the LCS-RAL fission fragment routines               !/*ra          5*/
!                                                                               !/*ra          6*/
!   Called by: PRECOF                                                           !/*ra          7*/
!                                                                               !/*ra          8*/
!   Calls: FISSED                                                               !/*ra          9*/
!                                                                               !/*ra         10*/
!   CEM95 written by S. G. Mashnik                                              !/*ra         11*/
!   FISSED written by F. Achison                                                !/*ra         12*/
!                                                                               !/*ra         13*/
!   Written by F. Gallmeier,  ORNL CPED  July 1998                              !/*ra         14*/
!   Changed to double by F. Gallmeier,  ORNL CPED  July 1998                    !/*ra         15*/
!   edited  by F. Gallmeier,  ORNL CPED  December 1998                          !/*ra         16*/
!                                                                               !/*ra         17*/
! ======================================================================        !/*ra         18*/
#include <lcs/double.h>                                                         /*!ra         19*/
#include <lcs/cacom.h>                                                          /*!ra         20*/
#include <lcs/ercom.h>                                                          /*!ra         21*/
      dimension aafis(10), zzfis(10), uufis(10), vvfis(10,3)                    !/*ra         22*/
      dimension ffisk(10), amfis(10,3), uglmom(3), v(3)                         !/*ra         23*/
      dimension nnfis(10)                                                       !/*ra         24*/
      logical fising, fision                                                    !/*ra         25*/
      common /fiscom/ aafis, zzfis, uufis, vvfis, ffisk, amfis, nnfis           !/*ra         26*/
!                                                                               !/*ra         27*/
!     arrays for fragment statistics                                            !/*ra         28*/
      dimension afrag(200), zfrag(100)                                          !/*ra         29*/
      common /fragdt/ afrag, zfrag, afrtt, afrsq, afrmin, afrmax,               !/*ra         30*/
     &                zfrtt, zfrsq, zfrmin, zfrmax, ekfrtt, ekfrsq,             !/*ra         31*/
     &                ekfrmn, ekfrmx, eufrtt, eufrsq, eufrmn, eufrmx,           !/*ra         32*/
     &                nfrag                                                     !/*ra         33*/
!                                                                               !/*ra         34*/
!        initialize variables                                                   !/*ra         35*/
      ja = a+dp1m5                                                              !/*ra         36*/
      jz = z+dp1m5                                                              !/*ra         37*/
      uu = ue                                                                   !/*ra         38*/
      do 10 i=1,3                                                               !/*ra         39*/
         coslbr(i) = v(i)                                                       !/*ra         40*/
         coslf0(i) = v(i)                                                       !/*ra         41*/
   10 continue                                                                  !/*ra         42*/
      fising = .true.                                                           !/*ra         43*/
      fisinh = .true.                                                           !/*ra         44*/
      fision = fisinh                                                           !/*ra         45*/
!                                                                               !/*ra         46*/
!        no fission for Z<71                                                    !/*ra         47*/
      if (jz.ge.71) then                                                        !/*ra         48*/
         call fissed (ja,jz,uu)                                                 !/*ra         49*/
         if (.not.fisinh) then                                                  !/*ra         50*/
            fising = .false.                                                    !/*ra         51*/
            fision = fisinh                                                     !/*ra         52*/
            return                                                              !/*ra         53*/
         endif                                                                  !/*ra         54*/
      else                                                                      !/*ra         55*/
         fising = .false.                                                       !/*ra         56*/
         fisinh = .false.                                                       !/*ra         57*/
         fision = fisinh                                                        !/*ra         58*/
         return                                                                 !/*ra         59*/
      endif                                                                     !/*ra         60*/
!                                                                               !/*ra         61*/
!        return variables                                                       !/*ra         62*/
      do 30 i=1,2                                                               !/*ra         63*/
         nfiss = nfiss+1                                                        !/*ra         64*/
         if(nfiss.ge.10) call expirx(1,'RALSH',                                 !/*ra         65*/
     &      'fission fragment bank full')                                       !/*ra         66*/
         aafis(nfiss) = afis(i)                                                 !/*ra         67*/
         zzfis(nfiss) = zfis(i)                                                 !/*ra         68*/
         uufis(nfiss) = fisu(i)                                                 !/*ra         69*/
         ffisk(nfiss) = fiske(i)                                                !/*ra         70*/
         nnfis(nfiss) = n                                                       !/*ra         71*/
         do 20 j=1,3                                                            !/*ra         72*/
            if(i.eq.1) vvfis(nfiss,j) = coslf1(j)                               !/*ra         73*/
            if(i.eq.2) vvfis(nfiss,j) = coslf2(j)                               !/*ra         74*/
            amfis(nfiss,j) = afis(i)/a*uglmom(j)                                !/*ra         75*/
   20    continue                                                               !/*ra         76*/
!                                                                               !/*ra         77*/
!       update fission fragment statistics                                      !/*ra         78*/
        ia = afis(i) + dp1m4                                                    !/*ra         79*/
        afrag(ia) = afrag(ia) + dp1                                             !/*ra         80*/
        iz = zfis(i) + dp1m4                                                    !/*ra         81*/
        zfrag(iz) = zfrag(iz) + dp1                                             !/*ra         82*/
        nfrag = nfrag + 1                                                       !/*ra         83*/
        arg = afis(i)                                                           !/*ra         84*/
        afrtt = afrtt + arg                                                     !/*ra         85*/
        afrsq = afrsq + arg*arg                                                 !/*ra         86*/
        afrmin = min(afrmin,arg)                                                !/*ra         87*/
        afrmax = max(afrmax,arg)                                                !/*ra         88*/
        arg = zfis(i)                                                           !/*ra         89*/
        zfrtt = zfrtt + arg                                                     !/*ra         90*/
        zfrsq = zfrsq + arg*arg                                                 !/*ra         91*/
        zfrmin = min(zfrmin,arg)                                                !/*ra         92*/
        zfrmax = max(zfrmax,arg)                                                !/*ra         93*/
        arg = fiske(i)                                                          !/*ra         94*/
        ekfrtt = ekfrtt + arg                                                   !/*ra         95*/
        ekfrsq = ekfrsq + arg*arg                                               !/*ra         96*/
        ekfrmn = min(ekfrmn,arg)                                                !/*ra         97*/
        ekfrmx = max(ekfrmx,arg)                                                !/*ra         98*/
        arg = fisu(i)                                                           !/*ra         99*/
        eufrtt = eufrtt + arg                                                   !/*ra        100*/
        eufrsq = eufrsq + arg*arg                                               !/*ra        101*/
        eufrmn = min(eufrmn,arg)                                                !/*ra        102*/
        eufrmx = max(eufrmx,arg)                                                !/*ra        103*/
   30 continue                                                                  !/*ra        104*/
!                                                                               !/*ra        105*/
#endif                                                                          /*!ra        106*/
      return                                                                    !/*ra        107*/
      end                                                                       !/*ra        108*/
