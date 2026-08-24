      subroutine fiscem(a,z,u,                                                  !/*fi          1*/
     &           tres,e,pnx,pny,pnz,v,uglmom,n,nfiss,emnucb)                    !/*fi          2*/
#ifdef CEM                                                                      /*!fi          3*/
! ======================================================================        !/*fi          4*/
!                                                                               !/*fi          5*/
!   Hands over a fission fragment from the bank to CEM                          !/*fi          6*/
!                                                                               !/*fi          7*/
!   Called by: PRECOF                                                           !/*fi          8*/
!                                                                               !/*fi          9*/
!   CEM95 written by S. G. Mashnik                                              !/*fi         10*/
!                                                                               !/*fi         11*/
!   Written by F. Gallmeier,  ORNL CPED  July 1998                              !/*fi         12*/
!   Changed to double by F. Gallmeier,  ORNL CPED  October 1998                 !/*fi         13*/
!                                                                               !/*fi         14*/
! ======================================================================        !/*fi         15*/
#include <cem/cdouble.h>                                                        /*!fi         16*/
      dimension uglmom(3), v(3)                                                 !/*fi         17*/
      dimension aafis(10), zzfis(10), uufis(10), vvfis(10,3)                    !/*fi         18*/
      dimension ffisk(10), amfis(10,3)                                          !/*fi         19*/
      dimension nnfis(10)                                                       !/*fi         20*/
      common /fiscom/ aafis, zzfis, uufis, vvfis, ffisk, amfis, nnfis           !/*fi         21*/
      if(nfiss.le.0)call expirx(1,'fiscem',                                     !/*fi         22*/
     &             'fission fragment bank is empty')                            !/*fi         23*/
      n = 1000                                                                  !/*fixk        1*/
      u = uufis(nfiss)                                                          !/*fi         25*/
      a = aafis(nfiss)                                                          !/*fi         26*/
      z = zzfis(nfiss)                                                          !/*fi         27*/
      iz = z + dp1m6                                                            !/*fi         28*/
      iz = max(iz,1)                                                            !/*fi         29*/
      un = a-z                                                                  !/*fi         30*/
      in = un + dp1m6                                                           !/*fi         31*/
      in = max(in,1)                                                            !/*fi         32*/
      emx = fmolnix(iz,in,2)                                                    !/*fi         33*/
      remn = a*emnucb + emx/dp1000                                              !/*fi         34*/
      tres = ffisk(nfiss)                                                       !/*fi         35*/
      e = tres/dp1000 + remn                                                    !/*fi         36*/
      pn = sqrt(e*e -remn*remn)                                                 !/*fi         37*/
      do 10 j=1,3                                                               !/*fi         38*/
         v(j) = vvfis(nfiss,j)                                                  !/*fi         39*/
         uglmom(j) = amfis(nfiss,j)                                             !/*fi         40*/
   10 continue                                                                  !/*fi         41*/
      pnx = v(1) *pn                                                            !/*fi         42*/
      pny = v(2) *pn                                                            !/*fi         43*/
      pnz = v(3) *pn                                                            !/*fi         44*/
      do 20 j=1,3                                                               !/*fi         45*/
         v(j) = v(j) *pn/e                                                      !/*fi         46*/
   20 continue                                                                  !/*fi         47*/
      nfiss = nfiss -1                                                          !/*fi         48*/
#endif                                                                          /*!fi         49*/
      return                                                                    !/*fi         50*/
      end                                                                       !/*fi         51*/
