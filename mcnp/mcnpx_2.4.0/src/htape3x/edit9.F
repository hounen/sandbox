!     $Id: edit9.F,v 1.5 2002/03/27 18:13:58 gwm Exp $                        !/*ed          1*/
      subroutine edit9 (tlly,tlsq,nerg,nerg1,ntim,ntim1,l,k)                    !/*ed          2*/
#include <htape3x/com32b.h>                                                     /*!ed          3*/
      character time*8                                                          !/*ed          4*/
#include <htape3x/label.h>                                                      /*!ed          5*/
#include <htape3x/contrl.h>                                                     /*!ed          6*/
#include <htape3x/box.h>                                                        /*!ed          7*/
#include <htape3x/bndy.h>                                                       /*!ed          8*/
#include <htape3x/ctconv.h>                                                     /*!ed          9*/
      dimension tlly(ntim1,nerg1,1), tlsq(ntim1,nerg1,1)                        !/*ed         10*/
#include <htape3x/kpt.h>                                                        /*!ed         11*/
      data time /'time='/                                                       !/*ed         12*/
      entry edit10(tlly,tlsq,nerg,nerg1,ntim,ntim1,l,k)                         !/*ed         13*/
      ityp=itip(l)+1                                                            !/*ed         14*/
      nlo=1                                                                     !/*ed         15*/
      if (nfprm1.eq.2) nlo=2                                                    !/*ed         16*/
      klo=1                                                                     !/*ed         17*/
      if (nerg1.eq.2) klo=2                                                     !/*ed         18*/
      do 60 iseg=1,nseg1                                                        !/*ed         19*/
      do 50 ia=nlo,nfprm1                                                       !/*ed         20*/
      fmult=dfparm(ia)/dnparm(k)                                                !/*ed         21*/
      if (lbunch) then                                                          !/*ed         22*/
        m=ia+nfprm1*(iseg-1+nseg1*(l-1))                                        !/*ed         23*/
      else                                                                      !/*ed         24*/
        m=ia+nfprm1*(iseg-1+nseg1*(k-1+nparm*(l-1)))                            !/*ed         25*/
      endif                                                                     !/*ed         26*/
      if (tlly(ntim1,nerg1,m).eq.0) go to 50                                    !/*ed         27*/
      if (iopt.eq.10) then                                                      !/*ed         28*/
        if (.not.lbunch) write (io,150) kpart(ityp),lparm(k)                    !/*ed         29*/
        if (lbunch) write (io,190) kpart(ityp),(lparm(i),i=1,nparm)             !/*ed         30*/
      elseif (ia.lt.nfprm1) then                                                !/*ed         31*/
        if (.not.lbunch) write (io,130) kpart(ityp),lparm(k),ia                 !/*ed         32*/
        if (lbunch) write (io,170) kpart(ityp),ia,(lparm(i),i=1,nparm)          !/*ed         33*/
      else                                                                      !/*ed         34*/
        if (.not.lbunch) write (io,140) kpart(ityp),lparm(k)                    !/*ed         35*/
        if (lbunch) write (io,180) kpart(ityp),(lparm(i),i=1,nparm)             !/*ed         36*/
      endif                                                                     !/*ed         37*/
      if (iseg.eq.1) write (io,70)                                              !/*ed         38*/
      if (iseg.eq.2) write (io,80)                                              !/*ed         39*/
      if (lsnprm) write (io,200) dnparm(k)                                      !/*ed         40*/
      if (ifeng) write (io,160)                                                 !/*ed         41*/
      if (itmult.gt.0) write (io,'(/35x,24htally multiplied by time)')          !/*ed         42*/
      if (irsp.gt.0) write (io,                                                 !/*ed         43*/
     & '(/35x,37htally multiplied by response function)')                       !/*ed         44*/
      if (irsp.lt.0) write (io,                                                 !/*ed         45*/
     & '(/35x,34htally divided by response function)')                          !/*ed         46*/
      if (ntim.le.1) go to 30                                                   !/*ed         47*/
      ilo=1                                                                     !/*ed         48*/
   10 continue                                                                  !/*ed         49*/
      ihi=min(ilo+4,ntim)                                                       !/*ed         50*/
      write (io,110) (time,timb(i),i=ilo,ihi)                                   !/*ed         51*/
      do 20 j=klo,nerg1                                                         !/*ed         52*/
      fact=fmult*dnerg(j)                                                       !/*ed         53*/
      if (j.ne.nerg1) write (io,90) ergb(j),(fact*dntim(i)*tlly(i,j,m)          !/*ed         54*/
     & ,tlsq(i,j,m),i=ilo,ihi)                                                  !/*ed         55*/
      if (j.eq.nerg1) write (io,100) (fact*dntim(i)*tlly(i,j,m),tlsq(i,j        !/*ed         56*/
     & ,m),i=ilo,ihi)                                                           !/*ed         57*/
   20 continue                                                                  !/*ed         58*/
      if (ihi.ge.ntim) go to 30                                                 !/*ed         59*/
      ilo=ilo+5                                                                 !/*ed         60*/
      go to 10                                                                  !/*ed         61*/
   30 continue                                                                  !/*ed         62*/
      write (io,120)                                                            !/*ed         63*/
      i=ntim1                                                                   !/*ed         64*/
      do 40 j=klo,nerg1                                                         !/*ed         65*/
      fact=fmult*dnerg(j)                                                       !/*ed         66*/
      if (j.lt.nerg1) write (io,90) ergb(j),fact*tlly(i,j,m),tlsq(i,j,m)        !/*ed         67*/
      if (j.eq.nerg1) write (io,100) fact*tlly(i,j,m),tlsq(i,j,m)               !/*ed         68*/
   40 continue                                                                  !/*ed         69*/
   50 continue                                                                  !/*ed         70*/
   60 continue                                                                  !/*ed         71*/
      return                                                                    !/*ed         72*/
!                                                                               !/*ed         73*/
   70 format (20x,'inside window')                                              !/*ed         74*/
   80 format (20x,'outside window')                                             !/*ed         75*/
   90 format (1pd12.4,5(1pd14.5,0pf7.4))                                        !/*ed         76*/
  100 format (7x,'total',5(1pd14.5,0pf7.4))                                     !/*ed         77*/
  110 format (/6x,'energy',5(3x,a6,1pd12.3))                                    !/*ed         78*/
  120 format (/6x,'energy',6x,'time total')                                     !/*ed         79*/
  130 format (/21x,a8,1x,'current across surface',i5,' for angular bin'         !/*ed         80*/
     & ,i3)                                                                     !/*ed         81*/
  140 format (/21x,a8,1x,'current across surface',i5,' for all angles')         !/*ed         82*/
  150 format (/21x,a8,1x,'flux across surface',i4)                              !/*ed         83*/
  160 format (/35x,'tally multiplied by energy')                                !/*ed         84*/
  170 format (/10x,a8,' current in angle bin',i4,' across surfaces',11i4        !/*ed         85*/
     & /(59x,11i4))                                                             !/*ed         86*/
  180 format (/10x,a8,' current for all angles across surfaces',11i4/(61        !/*ed         87*/
     & x,11i4))                                                                 !/*ed         88*/
  190 format (/10x,a8,' flux across surfaces',15i4/(39x,15i4))                  !/*ed         89*/
  200 format (/35x,'tally divided by',1pd14.5)                                  !/*ed         90*/
      end                                                                       !/*ed         91*/
