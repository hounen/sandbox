!     $Id: edit1.F,v 1.5 2002/03/27 18:13:57 gwm Exp $                        !/*ed          1*/
      subroutine edit1 (tlly,tlsq,nerg,nerg1,ntim,ntim1,l,k)                    !/*ed          2*/
#include <htape3x/com32b.h>                                                     /*!ed          3*/
      character time*8                                                          !/*ed          4*/
#include <htape3x/label.h>                                                      /*!ed          5*/
#include <htape3x/contrl.h>                                                     /*!ed          6*/
#include <htape3x/bndy.h>                                                       /*!ed          7*/
#include <htape3x/ctconv.h>                                                     /*!ed          8*/
      dimension tlly(ntim1,nerg1,1), tlsq(ntim1,nerg1,1)                        !/*ed          9*/
#include <htape3x/kpt.h>                                                        /*!ed         10*/
      data time /'time='/                                                       !/*ed         11*/
      entry edit2(tlly,tlsq,nerg,nerg1,ntim,ntim1,l,k)                          !/*ed         12*/
      ityp=itip(l)+1                                                            !/*ed         13*/
      nlo=1                                                                     !/*ed         14*/
      if (nfprm1.eq.2) nlo=2                                                    !/*ed         15*/
      klo=1                                                                     !/*ed         16*/
      if (nerg1.eq.2) klo=2                                                     !/*ed         17*/
      do 80 iseg=1,nseg1                                                        !/*ed         18*/
      do 70 ia=nlo,nfprm1                                                       !/*ed         19*/
      fmult=dfparm(ia)                                                          !/*ed         20*/
      if (lbunch) then                                                          !/*ed         21*/
        fmult=fmult/dnparm(k)                                                   !/*ed         22*/
        m=ia+nfprm1*(iseg-1+nseg1*(l-1))                                        !/*ed         23*/
      elseif (nparm.gt.0) then                                                  !/*ed         24*/
        fmult=fmult/dnparm(k)                                                   !/*ed         25*/
        m=ia+nfprm1*(iseg-1+nseg1*(k-1+nparm*(l-1)))                            !/*ed         26*/
      else                                                                      !/*ed         27*/
        m=ia+nfprm1*(iseg-1+nseg1*(l-1))                                        !/*ed         28*/
      endif                                                                     !/*ed         29*/
      if (tlly(ntim1,nerg1,m).eq.dp0) go to 70                                  !/*ed         30*/
      if (iopt.eq.2) go to 10                                                   !/*ed         31*/
      if (lbunch) then                                                          !/*ed         32*/
        if (ia.lt.nfprm1) write (io,220) kpart(ityp),ia,(lparm(i),i=1           !/*ed         33*/
     &  ,nparm)                                                                 !/*ed         34*/
        if (ia.ge.nfprm1) write (io,230) kpart(ityp),(lparm(i),i=1,nparm        !/*ed         35*/
     &  )                                                                       !/*ed         36*/
      elseif (nparm.gt.0) then                                                  !/*ed         37*/
        if (ia.lt.nfprm1) write (io,150) kpart(ityp),lparm(k),ia                !/*ed         38*/
        if (ia.ge.nfprm1) write (io,160) kpart(ityp),lparm(k)                   !/*ed         39*/
      else                                                                      !/*ed         40*/
        if (ia.lt.nfprm1) write (io,170) kpart(ityp),ia                         !/*ed         41*/
        if (ia.ge.nfprm1) write (io,240) kpart(ityp)                            !/*ed         42*/
      endif                                                                     !/*ed         43*/
      if (nseg.gt.0) write (io,100) iseg                                        !/*ed         44*/
      go to 20                                                                  !/*ed         45*/
   10 continue                                                                  !/*ed         46*/
      if (lbunch) then                                                          !/*ed         47*/
        if (nseg.eq.0) write (io,250) kpart(ityp),(lparm(i),i=1,nparm)          !/*ed         48*/
        if (nseg.gt.0) write (io,260) kpart(ityp),iseg,(lparm(i),i=1            !/*ed         49*/
     &  ,nparm)                                                                 !/*ed         50*/
      else                                                                      !/*ed         51*/
        if (nseg.eq.0) write (io,180) kpart(ityp),lparm(k)                      !/*ed         52*/
        if (nseg.gt.0) write (io,190) kpart(ityp),lparm(k),iseg                 !/*ed         53*/
      endif                                                                     !/*ed         54*/
   20 continue                                                                  !/*ed         55*/
      if (lsnprm) write (io,280) dnparm(k)                                      !/*ed         56*/
      if (ifeng) write (io,210)                                                 !/*ed         57*/
      if (itmult.gt.0) write (io,'(/35x,24htally multiplied by time)')          !/*ed         58*/
      if (irsp.gt.0) write (io,                                                 !/*ed         59*/
     & '(/35x,37htally multiplied by response function)')                       !/*ed         60*/
      if (irsp.lt.0) write (io,                                                 !/*ed         61*/
     & '(/35x,34htally divided by response function)')                          !/*ed         62*/
      klo=1                                                                     !/*ed         63*/
      if (nerg1.eq.2) klo=2                                                     !/*ed         64*/
      if (ntim.le.1) go to 50                                                   !/*ed         65*/
      ilo=1                                                                     !/*ed         66*/
   30 continue                                                                  !/*ed         67*/
      ihi=min(ilo+4,ntim)                                                       !/*ed         68*/
      write (io,130) (time,timb(i),i=ilo,ihi)                                   !/*ed         69*/
      do 40 j=klo,nerg1                                                         !/*ed         70*/
      fact=fmult*dnerg(j)                                                       !/*ed         71*/
      if (j.ne.nerg1) write (io,110) ergb(j),(fact*dntim(i)*tlly(i,j,m)         !/*ed         72*/
     & ,tlsq(i,j,m),i=ilo,ihi)                                                  !/*ed         73*/
      if (j.eq.nerg1) write (io,120) (fact*dntim(i)*tlly(i,j,m),tlsq(i,j        !/*ed         74*/
     & ,m),i=ilo,ihi)                                                           !/*ed         75*/
   40 continue                                                                  !/*ed         76*/
      if (ihi.ge.ntim) go to 50                                                 !/*ed         77*/
      ilo=ilo+5                                                                 !/*ed         78*/
      go to 30                                                                  !/*ed         79*/
   50 continue                                                                  !/*ed         80*/
      write (io,140)                                                            !/*ed         81*/
      i=ntim1                                                                   !/*ed         82*/
      do 60 j=klo,nerg1                                                         !/*ed         83*/
      fact=fmult*dnerg(j)                                                       !/*ed         84*/
      if (j.ne.nerg1) write (io,110) ergb(j),fact*tlly(i,j,m),tlsq(i,j,m        !/*ed         85*/
     & )                                                                        !/*ed         86*/
      if (j.eq.nerg1) write (io,120) fact*tlly(i,j,m),tlsq(i,j,m)               !/*ed         87*/
   60 continue                                                                  !/*ed         88*/
   70 continue                                                                  !/*ed         89*/
   80 continue                                                                  !/*ed         90*/
      return                                                                    !/*ed         91*/
      entry edit4(tlly,tlsq,nerg,nerg1,ntim,ntim1,l,k)                          !/*ed         92*/
      if (.not.lbunch) write (io,200) lparm(k)                                  !/*ed         93*/
      if (lbunch) write (io,270) (lparm(i),i=1,nparm)                           !/*ed         94*/
      if (lsnprm) write (io,280) dnparm(k)                                      !/*ed         95*/
      if (ifeng) write (io,210)                                                 !/*ed         96*/
      if (itmult.gt.0) write (io,'(/35x,24htally multiplied by time)')          !/*ed         97*/
      if (irsp.gt.0) write (io,                                                 !/*ed         98*/
     & '(/35x,37htally multiplied by response function)')                       !/*ed         99*/
      if (irsp.lt.0) write (io,                                                 !/*ed        100*/
     & '(/35x,34htally divided by response function)')                          !/*ed        101*/
      write (io,140)                                                            !/*ed        102*/
      fmult=1.0/dnparm(k)                                                       !/*ed        103*/
      klo=1                                                                     !/*ed        104*/
      if (nerg1.eq.2) klo=2                                                     !/*ed        105*/
      do 90 j=klo,nerg1                                                         !/*ed        106*/
      fact=fmult*dnerg(j)                                                       !/*ed        107*/
      if (j.ne.nerg1) write (io,110) ergb(j),fact*tlly(1,j,k),tlsq(1,j,k        !/*ed        108*/
     & )                                                                        !/*ed        109*/
      if (j.eq.nerg1) write (io,120) fact*tlly(1,j,k),tlsq(1,j,k)               !/*ed        110*/
   90 continue                                                                  !/*ed        111*/
      return                                                                    !/*ed        112*/
!                                                                               !/*ed        113*/
  100 format (10x,'for surface segment',i4)                                     !/*ed        114*/
  110 format (1pd12.4,5(1pd14.5,0pf7.4))                                        !/*ed        115*/
  120 format (7x,'total',5(1pd14.5,0pf7.4))                                     !/*ed        116*/
  130 format (/6x,'energy',5(3x,a6,1pd12.3))                                    !/*ed        117*/
  140 format (/6x,'energy',6x,'time total')                                     !/*ed        118*/
  150 format (/21x,a8,1x,'current across surface',i5,' for angular bin'         !/*ed        119*/
     & ,i3)                                                                     !/*ed        120*/
  160 format (/21x,a8,1x,'current across surface',i5,' for all angles')         !/*ed        121*/
  170 format (/21x,a8,1x,'total escape from system in angle bin',i3)            !/*ed        122*/
  180 format (/21x,a8,1x,'flux across surface',i4)                              !/*ed        123*/
  190 format (/21x,a8,1x,'flux across surface',i4,' in segment',i4)             !/*ed        124*/
  200 format (/30x,'track length neutron flux in cell',i5)                      !/*ed        125*/
  210 format (/35x,'tally multiplied by energy')                                !/*ed        126*/
  220 format (/10x,a8,' current in angular bin',i4,' across surfaces',13        !/*ed        127*/
     & i4/(61x,13i4))                                                           !/*ed        128*/
  230 format (/10x,a8,' current for all angles across surfaces',13i4/(57        !/*ed        129*/
     & x,13i4))                                                                 !/*ed        130*/
  240 format (/21x,a8,' total escape from system for all angles')               !/*ed        131*/
  250 format (/10x,a8,' flux across surfaces',15i4/(23x,15i4))                  !/*ed        132*/
  260 format (/10x,a8,' flux in segment',i4,' across surfaces',13i4/(56x        !/*ed        133*/
     & ,13i4))                                                                  !/*ed        134*/
  270 format (/10x,'track length neutron flux in cells',15i4/(44x,15i4))        !/*ed        135*/
  280 format (/35x,'tally divided by',1pd14.5)                                  !/*ed        136*/
      end                                                                       !/*ed        137*/
