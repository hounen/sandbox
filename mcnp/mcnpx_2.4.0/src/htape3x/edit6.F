!     $Id: edit6.F,v 1.5 2002/03/27 18:13:58 gwm Exp $                        !/*ed          1*/
      subroutine edit6 (tlly,tlsq,ntim,ntim1,m)                                 !/*ed          2*/
#include <htape3x/com32b.h>                                                     /*!ed          3*/
#include <htape3x/lpt.h>                                                        /*!ed          4*/
#include <htape3x/contrl.h>                                                     /*!ed          5*/
#include <htape3x/bndy.h>                                                       /*!ed          6*/
#include <htape3x/ctconv.h>                                                     /*!ed          7*/
      dimension tlly(ntim1,18,1), tlsq(ntim1,18,1)                              !/*ed          8*/
      character time*8, line*18                                                 !/*ed          9*/
      dimension line(18)                                                        !/*ed         10*/
      data nerg, nerg1 /17,18/                                                  !/*ed         11*/
      data time /'time='/                                                       !/*ed         12*/
      data line /'coulomb loss h-1','coulomb loss pi+-',                        !/*ed         13*/
     & 'coulomb loss mu+-','coulomb loss K+-','coulomb loss pbar',              !/*ed         14*/
     & 'coulomb loss h-2','coulomb loss h-3','coulomb loss he-3',               !/*ed         15*/
     & 'coulomb loss he-4','nuclear recoil','excitation',                       !/*ed         16*/
     & 'pi0 decay gammas','kin. eng. of e+,e-','positron mass',                 !/*ed         17*/
     & 'kin. eng. of muons','mu+ mass','mu- mass','total deposition'/           !/*ed         18*/
      if (lbunch) then                                                          !/*ed         19*/
        if (jmat.eq.0) write (io,110) (lparm(i),i=1,nparm)                      !/*ed         20*/
        if (jmat.ne.0) write (io,120) (lparm(i),i=1,nparm)                      !/*ed         21*/
      elseif (m.lt.nbin) then                                                   !/*ed         22*/
        if (jmat.eq.0) write (io,60) lparm(m)                                   !/*ed         23*/
        if (jmat.ne.0) write (io,90) lparm(m)                                   !/*ed         24*/
      else                                                                      !/*ed         25*/
        write (io,100)                                                          !/*ed         26*/
      endif                                                                     !/*ed         27*/
      fac=1.0/dnparm(m)                                                         !/*ed         28*/
      if (lsnprm) write (io,130) dnparm(m)                                      !/*ed         29*/
      if (ntim.le.1) go to 30                                                   !/*ed         30*/
      ilo=1                                                                     !/*ed         31*/
   10 continue                                                                  !/*ed         32*/
      ihi=min(ilo+4,ntim)                                                       !/*ed         33*/
      write (io,70) (time,timb(i),i=ilo,ihi)                                    !/*ed         34*/
      do 20 j=1,nerg1                                                           !/*ed         35*/
      write (io,50) line(j),(fac*dntim(i)*tlly(i,j,m),tlsq(i,j,m),i=ilo         !/*ed         36*/
     & ,ihi)                                                                    !/*ed         37*/
   20 continue                                                                  !/*ed         38*/
      if (ihi.ge.ntim) go to 30                                                 !/*ed         39*/
      ilo=ilo+5                                                                 !/*ed         40*/
      go to 10                                                                  !/*ed         41*/
   30 continue                                                                  !/*ed         42*/
      write (io,80)                                                             !/*ed         43*/
      i=ntim1                                                                   !/*ed         44*/
      do 40 j=1,nerg1                                                           !/*ed         45*/
      write (io,50) line(j),fac*tlly(i,j,m),tlsq(i,j,m)                         !/*ed         46*/
   40 continue                                                                  !/*ed         47*/
      return                                                                    !/*ed         48*/
!                                                                               !/*ed         49*/
   50 format (1x,a18,5(1pd14.5,0pf6.3))                                         !/*ed         50*/
   60 format (/21x,'energy deposition in cell',i4)                              !/*ed         51*/
   70 format (/18x,5(3x,a5,1pd12.3))                                            !/*ed         52*/
   80 format (/24x,'time total')                                                !/*ed         53*/
   90 format (/21x,'energy deposition in material',i4)                          !/*ed         54*/
  100 format (/25x,'total energy deposition')                                   !/*ed         55*/
  110 format (/10x,'energy deposition in cells',15i4/(36x,15i4))                !/*ed         56*/
  120 format (/10x,'energy deposition in materials',15i4/(40x,15i4))            !/*ed         57*/
  130 format (/35x,'tally divided by',1pd14.5)                                  !/*ed         58*/
      end                                                                       !/*ed         59*/
