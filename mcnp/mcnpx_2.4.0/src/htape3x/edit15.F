!     $Id: edit15.F,v 1.4 2002/03/27 18:13:57 gwm Exp $                       !/*ed          1*/
      subroutine edit15 (tlly,tlsq,nerg,nerg1,ntim,ntim1,k)                     !/*ed          2*/
#include <htape3x/com32b.h>                                                     /*!ed          3*/
#include <htape3x/lpt.h>                                                        /*!ed          4*/
#include <htape3x/contrl.h>                                                     /*!ed          5*/
#include <htape3x/bndy.h>                                                       /*!ed          6*/
#include <htape3x/ctconv.h>                                                     /*!ed          7*/
      dimension tlly(ntim1,nerg1,1), tlsq(ntim1,nerg1,1)                        !/*ed          8*/
      data lori /23/                                                            !/*ed          9*/
      if (lbunch) then                                                          !/*ed         10*/
        if (jmat.eq.0) write (io,40) (lparm(i),i=1,nparm)                       !/*ed         11*/
        if (jmat.ne.0) write (io,60) (lparm(i),i=1,nparm)                       !/*ed         12*/
        if (ixout.gt.0) then                                                    !/*ed         13*/
          if (jmat.eq.0) write (lori,50) (lparm(i),i=1,nparm)                   !/*ed         14*/
          if (jmat.ne.0) write (lori,70) (lparm(i),i=1,nparm)                   !/*ed         15*/
        endif                                                                   !/*ed         16*/
      elseif (nparm.gt.0) then                                                  !/*ed         17*/
        if (jmat.eq.0) write (io,80) lparm(k)                                   !/*ed         18*/
        if (jmat.ne.0) write (io,100) lparm(k)                                  !/*ed         19*/
        if (ixout.gt.0) then                                                    !/*ed         20*/
          if (jmat.eq.0) write (lori,90) lparm(k)                               !/*ed         21*/
          if (jmat.ne.0) write (lori,110) lparm(k)                              !/*ed         22*/
        endif                                                                   !/*ed         23*/
      else                                                                      !/*ed         24*/
        write (io,120)                                                          !/*ed         25*/
        if (ixout.gt.0) write (lori,120)                                        !/*ed         26*/
      endif                                                                     !/*ed         27*/
      if (kopt.eq.0) write (io,190)                                             !/*ed         28*/
      if (kopt.eq.1) write (io,200)                                             !/*ed         29*/
      if (kopt.eq.2) write (io,210)                                             !/*ed         30*/
      fac=1.0/dnparm(k)                                                         !/*ed         31*/
      if (lsnprm) write (io,130) dnparm(k)                                      !/*ed         32*/
      do 30 iz=1,nerg                                                           !/*ed         33*/
      rr=tlly(ntim1,iz,k)                                                       !/*ed         34*/
      if (rr.eq.0.) go to 30                                                    !/*ed         35*/
      iflag=0                                                                   !/*ed         36*/
      izm1=iz-1                                                                 !/*ed         37*/
      do 20 inn=1,ntim                                                          !/*ed         38*/
      r=tlly(inn,iz,k)                                                          !/*ed         39*/
      if (r.eq.0.) go to 20                                                     !/*ed         40*/
      inm1=inn-1                                                                !/*ed         41*/
      if (iflag.eq.0) go to 10                                                  !/*ed         42*/
      write (io,140) inm1,r*fac,tlsq(inn,iz,k)                                  !/*ed         43*/
      if (ixout.gt.0) write (lori,140) inm1,r*fac,tlsq(inn,iz,k)                !/*ed         44*/
      go to 20                                                                  !/*ed         45*/
   10 continue                                                                  !/*ed         46*/
      iflag=1                                                                   !/*ed         47*/
      write (io,150) izm1,inm1,r*fac,tlsq(inn,iz,k)                             !/*ed         48*/
      if (ixout.gt.0) write (lori,160) izm1,inm1,r*fac,tlsq(inn,iz,k)           !/*ed         49*/
   20 continue                                                                  !/*ed         50*/
   30 continue                                                                  !/*ed         51*/
      write (io,170) fac*tlly(ntim1,nerg1,k),tlsq(ntim1,nerg1,k)                !/*ed         52*/
      if (ihiaz.gt.0) write (io,180) ihiaz                                      !/*ed         53*/
      return                                                                    !/*ed         54*/
!                                                                               !/*ed         55*/
   40 format (10x,'isotopic reaction rate in cells ',13i4/(50x,13i4))           !/*ed         56*/
   50 format (10x,'distribution of removed nuclei in cells ',13i4/(50x,1        !/*ed         57*/
     & 3i4))                                                                    !/*ed         58*/
   60 format (10x,'isotopic reaction rate in materials ',13i4/(54x,13i4)        !/*ed         59*/
     & )                                                                        !/*ed         60*/
   70 format (10x,'distribution of removed nuclei in materials ',13i4/(5        !/*ed         61*/
     & 4x,13i4))                                                                !/*ed         62*/
   80 format (21x,'isotopic reaction rate in cell ',i4)                         !/*ed         63*/
   90 format (21x,'distribution of removed nuclei in cell ',i4)                 !/*ed         64*/
  100 format (21x,'isotopic reaction rate in material ',i4)                     !/*ed         65*/
  110 format (21x,'distribution of removed nuclei in material ',i4)             !/*ed         66*/
  120 format (21x,'isotopic reaction rate nuclei')                              !/*ed         67*/
  130 format (/35x,'tally divided by',1pd14.5)                                  !/*ed         68*/
  140 format (13x,'n =',i4,1pd14.5,0pf7.4)                                      !/*ed         69*/
  150 format (/5x,'z =',i3,'  n =',i4,1pd14.5,0pf7.4)                           !/*ed         70*/
  160 format (5x,'z =',i3,'  n =',i4,1pd14.5,0pf7.4)                            !/*ed         71*/
  170 format (/5x,'mean weight of removed nuclei per cascade',1pd14.5,0p        !/*ed         72*/
     & f7.4)                                                                    !/*ed         73*/
  180 format (/5x,'    high a/z =',i5)                                          !/*ed         74*/
  190 format (/26x,'(all collisions)')                                          !/*ed         75*/
  200 format (/26x,'(elastic scattering only)')                                 !/*ed         76*/
  210 format (/26x,'(nonelastic collisions only)')                              !/*ed         77*/
      end                                                                       !/*ed         78*/
