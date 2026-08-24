!     $Id: forec.F,v 1.5 2002/03/27 18:13:59 gwm Exp $                        !/*fo          1*/
      subroutine forec (*)                                                      !/*fo          2*/
#include <htape3x/com32b.h>                                                     /*!fo          3*/
#include <htape3x/label.h>                                                      /*!fo          4*/
#include <htape3x/contrl.h>                                                     /*!fo          5*/
      ibad=0                                                                    !/*fo          6*/
      if (mnuc.le.0) go to 20                                                   !/*fo          7*/
! residual nucleus records                                                      !/*fo          8*/
      if (nocasx.gt.0.and.nocas.eq.nocasx) then                                 !/*fo          9*/
        write (io,*) ' mnuc = ',mnuc                                            !/*fo         10*/
        write (io,*) '    APRR  ZPRR    ERECR        UUR'                       !/*fo         11*/
      endif                                                                     !/*fo         12*/
      do 10 i=1,mnuc                                                            !/*fo         13*/
      irec=1                                                                    !/*fo         14*/
      read (nhst,end=70,iostat=ibad,err=70) aprr(i),zprr(i),erecr(i),           !/*fo         15*/
     & uur(i),ecvr(i),ur(i),vr(i),wr(i)                                         !/*fo         16*/
      if (nocasx.gt.0.and.nocas.eq.nocasx) write (io,1001) i,aprr(i),           !/*fo         17*/
     & zprr(i), erecr(i), uur(i)                                                !/*fo         18*/
 1001 format(1x,i1,0p2f6.0,1p2e12.4)                                            !/*fo         19*/
      if (ibad.ne.0) go to 70                                                   !/*fo         20*/
   10 continue                                                                  !/*fo         21*/
   20 continue                                                                  !/*fo         22*/
! above-cutoff records                                                          !/*fo         23*/
      if (nabov.le.0) go to 40                                                  !/*fo         24*/
      irec=2                                                                    !/*fo         25*/
      read (nhst,end=70,iostat=ibad,err=70) (namea(i),tipa(i),ea(i),            !/*fo         26*/
     & ua(i),va(i),wa(i),wta(i),tca(i),i=1,nabov)                               !/*fo         27*/
      if (ibad.ne.0) go to 70                                                   !/*fo         28*/
      if (nocasx.gt.0.and.nocas.eq.nocasx) then                                 !/*fo         29*/
        write (io,*) ' nabov = ',nabov                                          !/*fo         30*/
        write (io,*) '      TIPA    JTIPA    EA'                                !/*fo         31*/
      endif                                                                     !/*fo         32*/
      do 30 i=1,nabov                                                           !/*fo         33*/
      jtipa(i)=tipa(i)                                                          !/*fo         34*/
      jtipa(i)=jtipa(i)/100                                                     !/*fo         35*/
      tipa(i)=mod(tipa(i),100.e0)                                               !/*fo         36*/
      if (nocasx.gt.0.and.nocas.eq.nocasx) write (io,1002) i,tipa(i),           !/*fo         37*/
     & jtipa(i),ea(i)                                                           !/*fo         38*/
 1002 format(1x,i4,f6.0,i6,1pe12.5)                                             !/*fo         39*/
   30 continue                                                                  !/*fo         40*/
   40 continue                                                                  !/*fo         41*/
! below-cutoff records                                                          !/*fo         42*/
      if (nbelo.le.0) go to 60                                                  !/*fo         43*/
      irec=3                                                                    !/*fo         44*/
      read (nhst,end=70,iostat=ibad,err=70) (tipb(i),eb(i),ub(i),vb(i),         !/*fo         45*/
     & wb(i),wtb(i),tcb(i),i=1,nbelo)                                           !/*fo         46*/
      if (ibad.ne.0) go to 70                                                   !/*fo         47*/
      if (nocasx.gt.0.and.nocas.eq.nocasx) then                                 !/*fo         48*/
        write (io,*) ' nbelo = ',nbelo                                          !/*fo         49*/
        write (io,*) '      TIPB    JTIPB    EB'                                !/*fo         50*/
      endif                                                                     !/*fo         51*/
      do 50 i=1,nbelo                                                           !/*fo         52*/
      jtipb(i)=tipb(i)                                                          !/*fo         53*/
      jtipb(i)=jtipb(i)/100                                                     !/*fo         54*/
      tipb(i)=mod(tipb(i),100.e0)                                               !/*fo         55*/
      if (nocasx.gt.0.and.nocas.eq.nocasx) write (io,1002) i,tipb(i),           !/*fo         56*/
     & jtipb(i),eb(i)                                                           !/*fo         57*/
   50 continue                                                                  !/*fo         58*/
   60 continue                                                                  !/*fo         59*/
      return                                                                    !/*fo         60*/
   70 continue                                                                  !/*fo         61*/
      write (io,*)                                                              !/*fo         62*/
     & ' irec, ncol, nocas, name, mat, nmed, nabov, nbelo, mnuc  ',             !/*fo         63*/
     & irec, ncol, nocas, name, mat, nmed, nabov, nbelo, mnuc                   !/*fo         64*/
      return 1                                                                  !/*fo         65*/
      end                                                                       !/*fo         66*/
