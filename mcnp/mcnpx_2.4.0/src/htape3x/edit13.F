!     $Id: edit13.F,v 1.5 2002/03/27 18:13:57 gwm Exp $                       !/*ed          1*/
      subroutine edit13 (tlly,tlsq,nerg,nerg1,ntim,ntim1,l)                     !/*ed          2*/
#include <htape3x/com32b.h>                                                     /*!ed          3*/
#include <htape3x/label.h>                                                      /*!ed          4*/
#include <htape3x/contrl.h>                                                     /*!ed          5*/
#include <htape3x/bndy.h>                                                       /*!ed          6*/
#include <htape3x/ctconv.h>                                                     /*!ed          7*/
#include <htape3x/kpt.h>                                                        /*!ed          8*/
      dimension tlly(ntim1,nerg1,1), tlsq(ntim1,nerg1,1)                        !/*ed          9*/
#ifndef HT13                                                                    /*!ed         10*/
      character label*15                                                        !/*ed         11*/
      data label /'azimuthal bin ='/                                            !/*ed         12*/
      ityp=itip(l)+1                                                            !/*ed         13*/
      nlo=1                                                                     !/*ed         14*/
      if (nfprm1.eq.2) nlo=2                                                    !/*ed         15*/
      klo=1                                                                     !/*ed         16*/
      if (nerg1.eq.2) klo=2                                                     !/*ed         17*/
      mlo=1                                                                     !/*ed         18*/
      if (ntim1.eq.2) mlo=2                                                     !/*ed         19*/
      do 70 it=mlo,ntim1                                                        !/*ed         20*/
      if (it.le.ntim) then                                                      !/*ed         21*/
        fmt0=dntim(it)                                                          !/*ed         22*/
      else                                                                      !/*ed         23*/
        fmt0=1.0                                                                !/*ed         24*/
      endif                                                                     !/*ed         25*/
      do 60 ia=nlo,nfprm1                                                       !/*ed         26*/
      fmult=fmt0*dfparm(ia)                                                     !/*ed         27*/
      do 10 iphi=1,nparm1                                                       !/*ed         28*/
      do 10 ie=1,nerg1                                                          !/*ed         29*/
      k=iphi+nparm1*(ia-1+nfprm1*(l-1))                                         !/*ed         30*/
      tlly(it,ie,k)=tlly(it,ie,k)*fmult                                         !/*ed         31*/
      if (iphi.lt.nparm1) tlly(it,ie,k)=tlly(it,ie,k)*nparm                     !/*ed         32*/
   10 continue                                                                  !/*ed         33*/
      if (nparm.gt.0.and.ia.lt.nfprm1) write (io,150) kpart(ityp),it,ia         !/*ed         34*/
      if (nparm.gt.0.and.ia.ge.nfprm1) write (io,160) kpart(ityp),it            !/*ed         35*/
      if (ifeng) write (io,'(/35x,26htally multiplied by energy)')              !/*ed         36*/
      if (itmult.gt.0) write (io,'(/35x,24htally multiplied by time)')          !/*ed         37*/
      if (irsp.gt.0) write (io,                                                 !/*ed         38*/
     & '(/35x,37htally multiplied by response function)')                       !/*ed         39*/
      if (irsp.lt.0) write (io,                                                 !/*ed         40*/
     & '(/35x,34htally divided by response function)')                          !/*ed         41*/
      klo=1                                                                     !/*ed         42*/
      if (nerg1.eq.2) klo=2                                                     !/*ed         43*/
      if (nparm1.le.2) go to 40                                                 !/*ed         44*/
      ilo=1+nparm1*(ia-1+nfprm1*(l-1))                                          !/*ed         45*/
      il0=ilo                                                                   !/*ed         46*/
      ilmax=ilo+nparm-1                                                         !/*ed         47*/
   20 continue                                                                  !/*ed         48*/
      ihi=min(ilo+4,ilmax)                                                      !/*ed         49*/
      write (io,170) (label,m-il0+1,m=ilo,ihi)                                  !/*ed         50*/
      do 30 j=klo,nerg1                                                         !/*ed         51*/
      fact=dnerg(j)                                                             !/*ed         52*/
      if (j.ne.nerg1) write (io,180) ergb(j),(fact*tlly(it,j,m),tlsq(it         !/*ed         53*/
     & ,j,m),m=ilo,ihi)                                                         !/*ed         54*/
      if (j.eq.nerg1) write (io,190) (fact*tlly(it,j,m),tlsq(it,j,m),m          !/*ed         55*/
     & =ilo,ihi)                                                                !/*ed         56*/
   30 continue                                                                  !/*ed         57*/
      if (ihi.eq.ilmax) go to 40                                                !/*ed         58*/
      ilo=ilo+5                                                                 !/*ed         59*/
      go to 20                                                                  !/*ed         60*/
   40 continue                                                                  !/*ed         61*/
      write (io,'(/6x,6henergy,6x,33haverage over all azimuthal angles)'        !/*ed         62*/
     & )                                                                        !/*ed         63*/
      do 50 j=klo,nerg1                                                         !/*ed         64*/
      fact=dnerg(j)                                                             !/*ed         65*/
      m=nparm1*(ia+nfprm1*(l-1))                                                !/*ed         66*/
      if (j.ne.nerg1) write (io,180) ergb(j),fact*tlly(it,j,m),tlsq(it,j        !/*ed         67*/
     & ,m)                                                                      !/*ed         68*/
      if (j.eq.nerg1) write (io,190) fact*tlly(it,j,m),tlsq(it,j,m)             !/*ed         69*/
   50 continue                                                                  !/*ed         70*/
   60 continue                                                                  !/*ed         71*/
   70 continue                                                                  !/*ed         72*/
#endif                                                                          /*!ed         73*/
#ifdef HT13                                                                     /*!ed         74*/
      character label*11                                                        !/*ed         75*/
      data label /'polar bin  '/                                                !/*ed         76*/
      data zero /dp0/                                                           !/*ed         77*/
      data convf /57.29577/                                                     !/*ed         78*/
      ityp=itip(l)+1                                                            !/*ed         79*/
      nlo=1                                                                     !/*ed         80*/
      if (ntim1.eq.2) nlo=2                                                     !/*ed         81*/
      do 140 it=nlo,ntim1                                                       !/*ed         82*/
      if (it.le.ntim) then                                                      !/*ed         83*/
        fmt0=dntim(it)                                                          !/*ed         84*/
      else                                                                      !/*ed         85*/
        fmt0=1.0                                                                !/*ed         86*/
      endif                                                                     !/*ed         87*/
      do 130 ie=1,nerg1                                                         !/*ed         88*/
      do 80 ia=1,nfprm1                                                         !/*ed         89*/
      do 80 iphi=1,nparm1                                                       !/*ed         90*/
      fmult=fmt0*dnerg(ie)*dfparm(ia)                                           !/*ed         91*/
      k=iphi+nparm1*(ia-1+nfprm1*(l-1))                                         !/*ed         92*/
      tlly(it,ie,k)=tlly(it,ie,k)*fmult                                         !/*ed         93*/
      if (iphi.lt.nparm1) tlly(it,ie,k)=tlly(it,ie,k)*nparm                     !/*ed         94*/
   80 continue                                                                  !/*ed         95*/
      write (io,200) titl2                                                      !/*ed         96*/
      eless=dp0                                                                 !/*ed         97*/
      if (ie.ge.2) eless=ergb(ie-1)                                             !/*ed         98*/
      if (ie.lt.nerg1) write (io,210) kpart(ityp),eless,ergb(ie)                !/*ed         99*/
      if (ie.ge.nerg1) write (io,210) kpart(ityp),zero,ergb(nerg)               !/*ed        100*/
      if (itmult.gt.0) write (io,'(/35x,24htally multiplied by time)')          !/*ed        101*/
      if (ifeng) write (io,'(/35x,26htally multiplied by energy)')              !/*ed        102*/
      if (nparm1.le.2) go to 110                                                !/*ed        103*/
      ialo=1                                                                    !/*ed        104*/
   90 continue                                                                  !/*ed        105*/
      iahi=min(ialo+4,nfprm)                                                    !/*ed        106*/
      write (io,220) (label,ia,ia=ialo,iahi)                                    !/*ed        107*/
      do 100 iphi=1,nparm                                                       !/*ed        108*/
      mlo=iphi+nparm1*(ialo-1+nfprm1*(l-1))                                     !/*ed        109*/
      mhi=iphi+nparm1*(iahi-1+nfprm1*(l-1))                                     !/*ed        110*/
      write (io,230) iphi,(tlly(it,ie,m),tlsq(it,ie,m),m=mlo,mhi,nparm1)        !/*ed        111*/
  100 continue                                                                  !/*ed        112*/
      if (iahi.eq.nfprm) go to 110                                              !/*ed        113*/
      ialo=iahi+1                                                               !/*ed        114*/
      go to 90                                                                  !/*ed        115*/
  110 continue                                                                  !/*ed        116*/
      write (io,                                                                !/*ed        117*/
     & '(//25x,10h polar bin,11x,29haverage over azimuthal angles)')            !/*ed        118*/
      do 120 ia=1,nfprm                                                         !/*ed        119*/
      m=nparm1*(ia+nfprm1*(l-1))                                                !/*ed        120*/
      if (ia.gt.1) then                                                         !/*ed        121*/
        alo=convf*acos(fparm(ia-1))                                             !/*ed        122*/
      else                                                                      !/*ed        123*/
        alo=180.                                                                !/*ed        124*/
      endif                                                                     !/*ed        125*/
      ahi=convf*acos(fparm(ia))                                                 !/*ed        126*/
      write (io,240) ia,alo,ahi,tlly(it,ie,m),tlsq(it,ie,m)                     !/*ed        127*/
  120 continue                                                                  !/*ed        128*/
      k=nparm1*nfprm1*l                                                         !/*ed        129*/
      temp=12.56637*tlly(it,ie,k)                                               !/*ed        130*/
      write (io,250) temp,tlsq(it,ie,k),tlly(it,ie,k)                           !/*ed        131*/
  130 continue                                                                  !/*ed        132*/
  140 continue                                                                  !/*ed        133*/
#endif                                                                          /*!ed        134*/
      return                                                                    !/*ed        135*/
!                                                                               !/*ed        136*/
  150 format (/21x,a8,1x,'emission per steradian for time bin',i3,              !/*ed        137*/
     & ' in polar angle bin',i3)                                                !/*ed        138*/
  160 format (/21x,a8,1x,'emission per steradian for time bin',i3,              !/*ed        139*/
     & ' for all polar angles')                                                 !/*ed        140*/
  170 format (/6x,'energy',5(3x,a15,i3))                                        !/*ed        141*/
  180 format (1pd12.4,5(1pd14.5,0pf7.4))                                        !/*ed        142*/
  190 format (7x,'total',5(1pd14.5,0pf7.4))                                     !/*ed        143*/
  200 format ('1',9x,10a8/)                                                     !/*ed        144*/
  210 format (/17x,a8,1x,'emission per steradian from',f7.2,' mev to',          !/*ed        145*/
     & f7.2,' mev')                                                             !/*ed        146*/
  220 format (/'    azmth',5(3x,a11,i2,1x))                                     !/*ed        147*/
  230 format (i7,2x,5(1pd11.3,0pf6.3))                                          !/*ed        148*/
  240 format (15x,i6,2x,f7.2,' to',f7.2,1pd23.4,0pf6.3)                         !/*ed        149*/
  250 format (/17x,'total emission all angles',1pd13.4,0pf6.3,' (',             !/*ed        150*/
     & 1pd11.4,' per steradian)')                                               !/*ed        151*/
      end                                                                       !/*ed        152*/
