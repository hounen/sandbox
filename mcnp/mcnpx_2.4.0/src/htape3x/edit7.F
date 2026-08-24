!     $Id: edit7.F,v 1.5 2002/03/27 18:13:58 gwm Exp $                        !/*ed          1*/
      subroutine edit7 (tlly,tlsq,ntim,ntim1,m)                                 !/*ed          2*/
#include <htape3x/com32b.h>                                                     /*!ed          3*/
#include <htape3x/lpt.h>                                                        /*!ed          4*/
#include <htape3x/contrl.h>                                                     /*!ed          5*/
#include <htape3x/bndy.h>                                                       /*!ed          6*/
#include <htape3x/ctconv.h>                                                     /*!ed          7*/
      dimension tlly(ntim1,21,1), tlsq(ntim1,21,1)                              !/*ed          8*/
      character time*8, line*16                                                 !/*ed          9*/
      dimension line(21)                                                        !/*ed         10*/
      data nerg, nerg1 /20,21/                                                  !/*ed         11*/
      data time /'time='/                                                       !/*ed         12*/
      data line /'source k.e.  ','source mass ','leakage k.e.',                 !/*ed         13*/
     & 'leakage mass','coulomb losses','mass stopped','nuclear recoil',         !/*ed         14*/
     & 'excitation','pi0 decay gammas','neutrino energy ',                      !/*ed         15*/
     & 'neutrn cutoff ke','neutrn ctff mass','positron mass',                   !/*ed         16*/
     & 'electron mass','k.e. of e+, e-','mu+ mass','mu- mass',                  !/*ed         17*/
     & 'k.e. of mu+, mu-','target mass ','int conv energy',                     !/*ed         18*/
     & 'mass-energy bal '/                                                      !/*ed         19*/
      if (m.lt.nbin.and.jmat.eq.0) write (io,60) lparm(m)                       !/*ed         20*/
      if (m.lt.nbin.and.jmat.ne.0) write (io,90) lparm(m)                       !/*ed         21*/
      if (m.eq.nbin) write (io,100)                                             !/*ed         22*/
      fac=1.0/dnparm(m)                                                         !/*ed         23*/
      if (lsnprm) write (io,110) dnparm(m)                                      !/*ed         24*/
      if (ntim.le.1) go to 30                                                   !/*ed         25*/
      ilo=1                                                                     !/*ed         26*/
   10 continue                                                                  !/*ed         27*/
      ihi=min(ilo+4,ntim)                                                       !/*ed         28*/
      write (io,70) (time,timb(i),i=ilo,ihi)                                    !/*ed         29*/
      do 20 j=1,nerg1                                                           !/*ed         30*/
      write (io,50) line(j),(fac*tlly(i,j,m),fac*tlsq(i,j,m),i=ilo,ihi)         !/*ed         31*/
   20 continue                                                                  !/*ed         32*/
      if (ihi.ge.ntim) go to 30                                                 !/*ed         33*/
      ilo=ilo+5                                                                 !/*ed         34*/
      go to 10                                                                  !/*ed         35*/
   30 continue                                                                  !/*ed         36*/
      write (io,80)                                                             !/*ed         37*/
      i=ntim1                                                                   !/*ed         38*/
      do 40 j=1,nerg1                                                           !/*ed         39*/
      write (io,50) line(j),fac*tlly(i,j,m),fac*tlsq(i,j,m)                     !/*ed         40*/
   40 continue                                                                  !/*ed         41*/
      return                                                                    !/*ed         42*/
!                                                                               !/*ed         43*/
   50 format (1x,a16,3x,5(1pd12.4,1pd8.1))                                      !/*ed         44*/
   60 format (/21x,'mass-energy balance in cell',i4)                            !/*ed         45*/
   70 format (/18x,5(3x,a5,1pd12.3))                                            !/*ed         46*/
   80 format (/24x,'time total')                                                !/*ed         47*/
   90 format (/21x,'mass-energy balance in material',i4)                        !/*ed         48*/
  100 format (/25x,'total mass-energy balance')                                 !/*ed         49*/
  110 format (/35x,'tally divided by',1pd14.5)                                  !/*ed         50*/
      end                                                                       !/*ed         51*/
