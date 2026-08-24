!     $Id: end_histp.F,v 1.5 2002/03/27 18:14:51 gwm Exp $                    !/*en          1*/
      subroutine end_histp (nps)                                                !/*en          2*/
!     HISTP Implementation: This routine needed only for histp writing.         !/*en          3*/
#ifdef HISTP                                                                    /*!en          4*/
#include <lcs/double.h>                                                         /*!en          5*/
#include <lcs/lptS.h>                                                           /*!en          6*/
#include <lcs/rucom.h>                                                          /*!en          7*/
#include <lcs/concom.h>                                                         /*!en          8*/
#include <lcs/inout.h>                                                          /*!en          9*/
      ncol=-4                                                                   !/*en         10*/
      maxcas=nps                                                                !/*en         11*/
      call wrt_histp                                                            !/*en         12*/
      write (io,200) (napart(i),i=1,11)                                         !/*en         13*/
      write (io,220) (j,(nosum(i,j),i=1,11),j=1,8)                              !/*en         14*/
      write (io,210) (napart(i),i=13,18)                                        !/*en         15*/
      write (io,230) (j,(nosum(i,j),i=13,18),ncolx(j),j=1,8)                    !/*en         16*/
      write (io,270) (napart(i),i=1,7)                                          !/*en         17*/
      write (io,280) (j,(wtsum(i,j),i=1,7),j=1,8)                               !/*en         18*/
      if (icpt.ne.0) then                                                       !/*en         19*/
        write (io,240) (napart(i),i=8,11)                                       !/*en         20*/
        write (io,260) (j,(wtsum(i,j),i=8,11),j=1,8)                            !/*en         21*/
      endif                                                                     !/*en         22*/
      write (io,250) (napart(i),i=13,18)                                        !/*en         23*/
      write (io,290) (j,(wtsum(i,j),i=13,18),j=1,8)                             !/*en         24*/
      fn=maxcas*maxbch                                                          !/*en         25*/
      fs1=fiswt1/fn                                                             !/*en         26*/
      fs2=fiswt2-fs1*fiswt1                                                     !/*en         27*/
      if (fn.le.dp1) fs2=dp0                                                    !/*en         28*/
      if (fn.gt.dp1.and.fs1.ne.dp0) fs2=sqrt(fs2/(fn*(fn-dp1)))/fs1             !/*en         29*/
      write (io,350) ifisct,fs1,fs2                                             !/*en         30*/
      if (nrnorm(1).gt.0) exlow(5)=exlow(5)/nrnorm(1)                           !/*en         31*/
      fr=dp0                                                                    !/*en         32*/
      if (nrnorm(1).gt.0) then                                                  !/*en         33*/
        f3=nrnorm(3)                                                            !/*en         34*/
        f1=nrnorm(1)                                                            !/*en         35*/
        fr=f3/f1                                                                !/*en         36*/
      endif                                                                     !/*en         37*/
      write (io,370) negex,exlow(3),nrnorm(1),exlow(1),fr,exlow(5)              !/*en         38*/
     & ,nrnorm(2),negaz,lowaz,nrnorm(4)                                         !/*en         39*/
      call hadcnt (0,nout)                                                      !/*en         40*/
      write (io,80) nbov,nbhw                                                   !/*en         41*/
      write (io,90)                                                             !/*en         42*/
      do 70 i=1,lpt                                                             !/*en         43*/
      if (nbt(i).gt.0) write (io,100) napart(i),nbt(i)                          !/*en         44*/
   70 continue                                                                  !/*en         45*/
      close (nhstp)                                                             !/*en         46*/
!                                                                               !/*en         47*/
   80 format (/' bank overflows to backup file ',i6/                            !/*en         48*/
     & ' maximum ever in bank ',i8)                                             !/*en         49*/
   90 format (' particles of each type banked:')                                !/*en         50*/
  100 format (5x,a8,i10)                                                        !/*en         51*/
  110 format (1h1,' ---- collision information, all energies ----')             !/*en         52*/
  120 format (1h0,9x,2(5x,4hreal,10x,6hpseudo,5x),4x,6hpseudo,10x,              !/*en         53*/
     & 4hreal,10x,6hpseudo)                                                     !/*en         54*/
  130 format (1h0,8x,2(3x,9hnonhydrog,3x),2(4x,8hhydrogen,3x),5x,               !/*en         55*/
     & 5hdecay,5x,2(4x,7helastic,4x))                                           !/*en         56*/
  140 format (1h0,9x,7(2x,10hcollisions,3x))                                    !/*en         57*/
  150 format (1h0,6hmedium,i3,7(2x,e11.5,2x))                                   !/*en         58*/
  160 format (///38x,                                                           !/*en         59*/
     & ' pseudo collisions within internal void by particles of '/)             !/*en         60*/
  170 format (7(8x,5h type,i2)/4x,7e15.5/)                                      !/*en         61*/
  180 format (4(8x,5h type,i2)/4x,4e15.5/)                                      !/*en         62*/
  190 format (1h0/1h0,' ---- collision information for nucleons above '         !/*en         63*/
     & ,f10.1,'   mev and chg. pions above ',f10.1,'   mev.----')               !/*en         64*/
  200 format (1h1,20x,42hnumber of history tape records by particle,            !/*en         65*/
     & 19h (actually written)/1h0,11x,a6,4x,a7,5(6x,a3),2x,a8,2x,a6,6x,         !/*en         66*/
     & a3,4x,a5)                                                                !/*en         67*/
  210 format (1h0,15x,a3,3x,a7,2x,a8,4x,a3,5x,a5,5x,a5,2x,5htotal/)             !/*en         68*/
  220 format (7h ncol =,i2,11i9/)                                               !/*en         69*/
  230 format (7h ncol =,i2,7i9/)                                                !/*en         70*/
  240 format (1h0,11x,4(a9,3x)/)                                                !/*en         71*/
  250 format (1h0,11x,6(a9,3x)/)                                                !/*en         72*/
  260 format (7h ncol =,i2,1p4e12.4/)                                           !/*en         73*/
  270 format (1h0,10x,42hweight of history tape records by particle,            !/*en         74*/
     & 30h (includes suppressed records)/1h0,11x,7(a9,3x)/)                     !/*en         75*/
  280 format (7h ncol =,i2,1p7e12.4/)                                           !/*en         76*/
  290 format (7h ncol =,i2,1p6e12.4/)                                           !/*en         77*/
  300 format ('0number of n, p, d, ph written to NEUTP=',4i8/                   !/*en         78*/
     & ' total weight of n, p, d, ph written=',1p4e12.4)                        !/*en         79*/
  310 format ('0mean weight of NEUTP neutrons per cascade =',1pe12.4,           !/*en         80*/
     & ' (',0pf6.4,')')                                                         !/*en         81*/
  320 format ('0mean weight of NEUTP protons per cascade =',1pe12.4,            !/*en         82*/
     & ' (',0pf6.4,')')                                                         !/*en         83*/
  330 format ('0mean weight of NEUTP deuterons per cascade =',1pe12.4,          !/*en         84*/
     & ' (',0pf6.4,')')                                                         !/*en         85*/
  340 format ('0mean weight of NEUTP photons per cascade =',1pe12.4,            !/*en         86*/
     & ' (',0pf6.4,')')                                                         !/*en         87*/
  350 format (27h0number of fission events =,i7,5x,                             !/*en         88*/
     & 31hweight of fission per cascade =,1pe12.4,2h (,0pf5.3,1h))              !/*en         89*/
  370 format (/' rejections for ex.lt.0 =',i6,' worst case =',f8.2,             !/*en         90*/
     & ' mev'/' renormalizations for ex.lt.0 =',i6,' worst case =',f8.3,        !/*en         91*/
     & ' mev'/' mean number of renormalization iterations =',f7.2,              !/*en         92*/
     & ' mean final ex =',f8.5,' mev'/                                          !/*en         93*/
     & ' number of renormalizations rejected =',i6/                             !/*en         94*/
     & ' number of rejections for z.lt.0 or n.lt.0 =',i6/                       !/*en         95*/
     & ' number of rejections for z.eq.0,n.gt.0 or n.eq.0,z.gt.0 =',i6/         !/*en         96*/
     & ' number of total disintegrations in cascad =',i6)                       !/*en         97*/
  380 format (///' end of run: next random number (rijk) = ',2z6/10x,           !/*en         98*/
     & 36hnumber of random numbers generated =,i12)                             !/*en         99*/
  390 format (///' end of run: next random number = ',2z6/10x,                  !/*en        100*/
     & 36hnumber of random numbers generated =,i12)                             !/*en        101*/
!                                                                               !/*en        102*/
#endif                                                                          /*!en        103*/
      return                                                                    !/*en        104*/
      end                                                                       !/*en        105*/
