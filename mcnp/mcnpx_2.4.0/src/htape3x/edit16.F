!     $Id: edit16.F,v 1.5 2002/03/27 18:13:57 gwm Exp $                       !/*ed          1*/
      subroutine edit16 (tlly,tlsq,nerg,nerg1,ntim,m)                           !/*ed          2*/
#include <htape3x/com32b.h>                                                     /*!ed          3*/
#include <htape3x/lpt.h>                                                        /*!ed          4*/
#include <htape3x/contrl.h>                                                     /*!ed          5*/
#include <htape3x/bndy.h>                                                       /*!ed          6*/
#include <htape3x/ctconv.h>                                                     /*!ed          7*/
      dimension tlly(ntim,nerg1,1), tlsq(ntim,nerg1,1)                          !/*ed          8*/
      dimension tarry(4)                                                        !/*ed          9*/
#ifdef HTPLOT                                                                   /*!ed         10*/
      dimension char(4)                                                         !/*ed         11*/
      character char*30                                                         !/*ed         12*/
      character labx*19, laby1*23, laby2*19, bin*3, cell*5, mat*4               !/*ed         13*/
     & , title*38, laby*32, labx1*19, labx2*19, laby3*30, laby4*26              !/*ed         14*/
      data char /'total recoil energy spectrum',                                !/*ed         15*/
     & 'elastic recoil energy spectrum','total damage energy spectrum',         !/*ed         16*/
     & 'elastic damage energy spectrum'/                                        !/*ed         17*/
      data cell /' cell'/, mat /' mat'/                                         !/*ed         18*/
      data labx1 /'recoil energy (mev)'/, labx2 /'damage energy (mev)'/,        !/*ed         19*/
     & laby1 /'recoils per cascade*MeV'/, laby2 /'recoils per cascade'/,        !/*ed         20*/
     & laby3 /'recoils*energy per cascade*MeV'/,                                !/*ed         21*/
     & laby4 /'recoils*energy per cascade'/                                     !/*ed         22*/
#endif                                                                          /*!ed         23*/
      if (lbunch) then                                                          !/*ed         24*/
        if (jmat.ne.0) write (io,200) (lparm(i),i=1,nparm)                      !/*ed         25*/
        if (jmat.eq.0) write (io,210) (lparm(i),i=1,nparm)                      !/*ed         26*/
      elseif (m.lt.nbin) then                                                   !/*ed         27*/
        if (jmat.ne.0) write (io,170) lparm(m)                                  !/*ed         28*/
        if (jmat.eq.0) write (io,180) lparm(m)                                  !/*ed         29*/
      else                                                                      !/*ed         30*/
        write (io,190)                                                          !/*ed         31*/
      endif                                                                     !/*ed         32*/
      if (ifeng) write (io,'(/35x,26htally multiplied by energy)')              !/*ed         33*/
      if (lsnprm) write (io,220) dnparm(m)                                      !/*ed         34*/
      write (io,150)                                                            !/*ed         35*/
      do 10 k=1,nerg                                                            !/*ed         36*/
      fact=dnerg(k)/dnparm(m)                                                   !/*ed         37*/
      write (io,110) ergb(k),(fact*tlly(j,k,m),tlsq(j,k,m),j=1,4)               !/*ed         38*/
   10 continue                                                                  !/*ed         39*/
      write (io,120) (fact*tlly(j,nerg1-1,m),tlsq(j,nerg1-1,m),j=1,4)           !/*ed         40*/
      write (io,130) (fact*tlly(j,nerg1,m),tlsq(j,nerg1,m),j=1,4)               !/*ed         41*/
      do 20 j=1,4                                                               !/*ed         42*/
      tarry(j)=dp0                                                              !/*ed         43*/
      if (tlly(j,nerg1-1,m).gt.dp0) tarry(j)=tlly(j,nerg1,m)/tlly(j             !/*ed         44*/
     & ,nerg1-1,m)                                                              !/*ed         45*/
   20 continue                                                                  !/*ed         46*/
      write (io,140) (tarry(j),j=1,4)                                           !/*ed         47*/
#ifdef HTPLOT                                                                   /*!ed         48*/
      if (kplot.lt.1) go to 100                                                 !/*ed         49*/
      nerg=nerg1-2                                                              !/*ed         50*/
      if (nerg.le.1) go to 100                                                  !/*ed         51*/
      if (lsnerg.and..not.ifeng) laby=laby1                                     !/*ed         52*/
      if (.not.lsnerg.and..not.ifeng) laby=laby2                                !/*ed         53*/
      if (lsnerg.and.ifeng) laby=laby3                                          !/*ed         54*/
      if (lsnerg.and.ifeng) laby=laby4                                          !/*ed         55*/
      nup=2*nerg+2                                                              !/*ed         56*/
      xx(1)=0.3*ergb(1)                                                         !/*ed         57*/
      xx(2)=xx(1)                                                               !/*ed         58*/
      do 30 i=1,nerg                                                            !/*ed         59*/
      j=2*i+1                                                                   !/*ed         60*/
      xx(j)=ergb(i)                                                             !/*ed         61*/
   30 xx(j+1)=ergb(i)                                                           !/*ed         62*/
      if (m.lt.nbin) write (bin,'(i3)') lparm(m)                                !/*ed         63*/
      do 90 j=1,4                                                               !/*ed         64*/
      if (tlly(j,nerg1,m).le.dp0) go to 90                                      !/*ed         65*/
      do 40 k=1,nerg                                                            !/*ed         66*/
      fact=dnerg(k)/dnparm(m)                                                   !/*ed         67*/
   40 y0(k)=fact*tlly(j,k,m)                                                    !/*ed         68*/
      do 50 i=1,nerg                                                            !/*ed         69*/
      nlo=i                                                                     !/*ed         70*/
      if (y0(i).gt.dp0) go to 60                                                !/*ed         71*/
   50 continue                                                                  !/*ed         72*/
      go to 90                                                                  !/*ed         73*/
   60 continue                                                                  !/*ed         74*/
      nlo=max((nlo-1),1)                                                        !/*ed         75*/
      do 70 i=1,nerg                                                            !/*ed         76*/
      nhi=nerg+1-i                                                              !/*ed         77*/
      if (y0(nhi).gt.dp0) go to 80                                              !/*ed         78*/
   70 continue                                                                  !/*ed         79*/
      go to 90                                                                  !/*ed         80*/
   80 continue                                                                  !/*ed         81*/
      nup=2*nhi+2                                                               !/*ed         82*/
      call fix (base,y0,yy,nerg)                                                !/*ed         83*/
      if (m.eq.nbin) title=char(j)                                              !/*ed         84*/
      if (m.lt.nbin.and.jmat.eq.0) title=char(j)//cell//bin                     !/*ed         85*/
      if (m.lt.nbin.and.jmat.ne.0) title=char(j)//mat//bin                      !/*ed         86*/
      n1=2*nlo-1                                                                !/*ed         87*/
      n2=nup-n1+1                                                               !/*ed         88*/
      if (j.le.2) labx=labx1                                                    !/*ed         89*/
      if (j.gt.2) labx=labx2                                                    !/*ed         90*/
      call hlogxy (xx(n1),yy(n1),n2,labx,laby,title)                            !/*ed         91*/
      call gpage                                                                !/*ed         92*/
      nplot=nplot+1                                                             !/*ed         93*/
      write (io,160) nplot,icase,title                                          !/*ed         94*/
   90 continue                                                                  !/*ed         95*/
  100 continue                                                                  !/*ed         96*/
#endif                                                                          /*!ed         97*/
      return                                                                    !/*ed         98*/
!                                                                               !/*ed         99*/
  110 format (1x,1pd12.3,4(1pd18.3,1x,0pf5.3))                                  !/*ed        100*/
  120 format ('0mean recoils',4(1pd18.3,1x,0pf5.3)/' per history')              !/*ed        101*/
  130 format ('0mean energy ',4(1pd18.3,1x,0pf5.3)/' per history')              !/*ed        102*/
  140 format ('0mean energy ',4(1pd18.3,6x)/' per recoil')                      !/*ed        103*/
  150 format (/5x,'energy',12x,'total recoil',12x,'elstc recoil',12x,           !/*ed        104*/
     & 'total damage',12x,'elstc damage')                                       !/*ed        105*/
  160 format ('0plot no.',i3,' case no.',i3,5x,a)                               !/*ed        106*/
  170 format (/35x,' recoil and damage energy spectra in material',i4)          !/*ed        107*/
  180 format (/35x,' recoil and damage energy spectra in cell',i4)              !/*ed        108*/
  190 format (/35x,'total recoil and damage energy spectra')                    !/*ed        109*/
  200 format (/10x,'recoil and damage energy spectra in materials',15i4/        !/*ed        110*/
     & (41x,15i4))                                                              !/*ed        111*/
  210 format (/10x,'recoil and damage energy spectra in cells',15i4/(37x        !/*ed        112*/
     & ,15i4))                                                                  !/*ed        113*/
  220 format (/35x,'tally divided by',1pd14.5)                                  !/*ed        114*/
      end                                                                       !/*ed        115*/
