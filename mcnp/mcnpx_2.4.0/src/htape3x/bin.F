!     $Id: bin.F,v 1.4 2002/03/27 18:13:56 gwm Exp $                          !/*bi          1*/
      subroutine bin (tlly0,hold0,tlsq0,cov,tlfsq,nerg,nerg1,ntim,ntim1)        !/*bi          2*/
#include <htape3x/com32b.h>                                                     /*!bi          3*/
#include <htape3x/label.h>                                                      /*!bi          4*/
#include <htape3x/srcwt.h>                                                      /*!bi          5*/
#include <htape3x/contrl.h>                                                     /*!bi          6*/
#include <htape3x/bndy.h>                                                       /*!bi          7*/
#include <htape3x/ctconv.h>                                                     /*!bi          8*/
#include <htape3x/com11.h>                                                      /*!bi          9*/
#include <htape3x/com10.h>                                                      /*!bi         10*/
      dimension tlly0(1), tlsq0(1), hold0(1)                                    !/*bi         11*/
      dimension cov(1), tlfsq(1)                                                !/*bi         12*/
      log10=.true.                                                              !/*bi         13*/
      log11=.true.                                                              !/*bi         14*/
      noca11=0                                                                  !/*bi         15*/
      ir10=0                                                                    !/*bi         16*/
      assign 110 to jbr                                                         !/*bi         17*/
   10 continue                                                                  !/*bi         18*/
      call rednmt                                                               !/*bi         19*/
      if (ncol.gt.0) go to 100                                                  !/*bi         20*/
   20 continue                                                                  !/*bi         21*/
      mgo=ncol+5                                                                !/*bi         22*/
      go to (30,50,60,70,60), mgo                                               !/*bi         23*/
   30 continue                                                                  !/*bi         24*/
! end of run                                                                    !/*bi         25*/
      if (imerge.eq.0) assign 40 to jbr                                         !/*bi         26*/
      go to 80                                                                  !/*bi         27*/
   40 continue                                                                  !/*bi         28*/
      return                                                                    !/*bi         29*/
   50 continue                                                                  !/*bi         30*/
! end of batch                                                                  !/*bi         31*/
      if (imerge.eq.0) nocas0=0                                                 !/*bi         32*/
      go to 10                                                                  !/*bi         33*/
   60 continue                                                                  !/*bi         34*/
! ncol not used                                                                 !/*bi         35*/
      call errorf ('bin.1   ')                                                  !/*bi         36*/
   70 continue                                                                  !/*bi         37*/
! start of run                                                                  !/*bi         38*/
      call rednmt                                                               !/*bi         39*/
      nocas0=nocas                                                              !/*bi         40*/
   80 if (imerge.ne.0) then                                                     !/*bi         41*/
        if (nhst.eq.nh11) then                                                  !/*bi         42*/
          if (ir10.eq.0.or.ncol.eq.-4) then                                     !/*bi         43*/
            if (ncol.eq.-4) then                                                !/*bi         44*/
              imerge=0                                                          !/*bi         45*/
              ir10=0                                                            !/*bi         46*/
              assign 110 to jbr                                                 !/*bi         47*/
            endif                                                               !/*bi         48*/
            noca11=nocas                                                        !/*bi         49*/
            log11=.false.                                                       !/*bi         50*/
            nhst=nh10                                                           !/*bi         51*/
            go to 10                                                            !/*bi         52*/
          else                                                                  !/*bi         53*/
            ir10=0                                                              !/*bi         54*/
          endif                                                                 !/*bi         55*/
        elseif (ncol.le.-3.or.nocas.gt.noca11) then                             !/*bi         56*/
          ir10=1                                                                !/*bi         57*/
          log10=.false.                                                         !/*bi         58*/
          nhst=nh11                                                             !/*bi         59*/
          go to 10                                                              !/*bi         60*/
        endif                                                                   !/*bi         61*/
      endif                                                                     !/*bi         62*/
      if (ncol.gt.0.and.nocas.eq.nocas0) go to 110                              !/*bi         63*/
! update tally arrays                                                           !/*bi         64*/
      if (iopt.eq.11.or.iopt.eq.12) call cov11 (cov,hold0,lsize)                !/*bi         65*/
      fwtd=1.0                                                                  !/*bi         66*/
      if (irs.gt.0) fwtd=fwti                                                   !/*bi         67*/
      do 90 k=1,lsize                                                           !/*bi         68*/
      if (hold0(k).eq.dp0) go to 90                                             !/*bi         69*/
      fwth=fwti*hold0(k)                                                        !/*bi         70*/
      tlly0(k)=tlly0(k)+fwth                                                    !/*bi         71*/
      tlsq0(k)=tlsq0(k)+fwth**2                                                 !/*bi         72*/
      tlfsq(k)=tlfsq(k)+fwtd*fwth                                               !/*bi         73*/
      hold0(k)=dp0                                                              !/*bi         74*/
   90 continue                                                                  !/*bi         75*/
      if (ir10.eq.1) go to 10                                                   !/*bi         76*/
      nocas0=nocas                                                              !/*bi         77*/
      go to jbr, (40,110)                                                       !/*bi         78*/
  100 continue                                                                  !/*bi         79*/
! event record read                                                             !/*bi         80*/
      if (nocas.ne.nocas0) go to 80                                             !/*bi         81*/
  110 continue                                                                  !/*bi         82*/
      go to (120,130,140,150,160,170,180,190,200,210,220,230,240,250,260        !/*bi         83*/
     & ,270,280,290,300), iopt                                                  !/*bi         84*/
  120 continue                                                                  !/*bi         85*/
! surface current                                                               !/*bi         86*/
      call opt1 (hold0,nerg,nerg1,ntim,ntim1)                                   !/*bi         87*/
      if (ncol) 20,20,80                                                        !/*bi         88*/
  130 continue                                                                  !/*bi         89*/
! surface flux                                                                  !/*bi         90*/
      call opt2 (hold0,nerg,nerg1,ntim,ntim1)                                   !/*bi         91*/
      if (ncol) 20,20,80                                                        !/*bi         92*/
  140 continue                                                                  !/*bi         93*/
! particle production                                                           !/*bi         94*/
      call opt3 (hold0,nerg,nerg1,nbin)                                         !/*bi         95*/
      if (ncol) 20,20,80                                                        !/*bi         96*/
  150 continue                                                                  !/*bi         97*/
! track length neutron flux                                                     !/*bi         98*/
      call opt4 (hold0,nerg,nerg1)                                              !/*bi         99*/
      if (ncol) 20,20,80                                                        !/*bi        100*/
  160 continue                                                                  !/*bi        101*/
! nuclear excitation                                                            !/*bi        102*/
      call opt5 (tlly0,hold0,tlsq0,cov,nerg1)                                   !/*bi        103*/
      if (ncol) 20,20,80                                                        !/*bi        104*/
  170 continue                                                                  !/*bi        105*/
! energy deposition                                                             !/*bi        106*/
      call opt6 (hold0,ntim,ntim1)                                              !/*bi        107*/
      if (ncol) 20,20,80                                                        !/*bi        108*/
  180 continue                                                                  !/*bi        109*/
      call opt7 (hold0,ntim,ntim1)                                              !/*bi        110*/
      if (ncol) 20,20,80                                                        !/*bi        111*/
  190 continue                                                                  !/*bi        112*/
      call opt8 (tlly0,hold0,tlsq0,nerg1,ntim1)                                 !/*bi        113*/
      if (ncol) 20,20,80                                                        !/*bi        114*/
  200 continue                                                                  !/*bi        115*/
! surface current with collimating window                                       !/*bi        116*/
      call opt9 (hold0,nerg,nerg1,ntim,ntim1)                                   !/*bi        117*/
      if (ncol) 20,20,80                                                        !/*bi        118*/
  210 continue                                                                  !/*bi        119*/
! surface flux with collimating window                                          !/*bi        120*/
      call opt10 (hold0,nerg,nerg1,ntim,ntim1)                                  !/*bi        121*/
      if (ncol) 20,20,80                                                        !/*bi        122*/
  220 continue                                                                  !/*bi        123*/
! pulse shape analysis                                                          !/*bi        124*/
      call opt11 (hold0,nerg,nerg1)                                             !/*bi        125*/
      if (ncol) 20,20,80                                                        !/*bi        126*/
  230 continue                                                                  !/*bi        127*/
! pulse shape analysis with window                                              !/*bi        128*/
      call opt12 (hold0,nerg,nerg1)                                             !/*bi        129*/
      if (ncol) 20,20,80                                                        !/*bi        130*/
  240 continue                                                                  !/*bi        131*/
! emission distribution                                                         !/*bi        132*/
      call opt13 (hold0,nerg,nerg1,ntim,ntim1)                                  !/*bi        133*/
      if (ncol) 20,20,80                                                        !/*bi        134*/
  250 continue                                                                  !/*bi        135*/
! gas production                                                                !/*bi        136*/
      call opt14 (hold0)                                                        !/*bi        137*/
      if (ncol) 20,20,80                                                        !/*bi        138*/
  260 continue                                                                  !/*bi        139*/
!*****                                                                          !/*bi        140*/
! option 15 - isotopic collision rates                                          !/*bi        141*/
      call opt15 (tlly0,hold0,tlsq0,nerg1,ntim1)                                !/*bi        142*/
      if (ncol) 20,20,80                                                        !/*bi        143*/
!>>>>>                                                                          !/*bi        144*/
  270 continue                                                                  !/*bi        145*/
!*****                                                                          !/*bi        146*/
! option 16 - recoil energy spectra                                             !/*bi        147*/
      call opt16 (hold0,nerg,nerg1,ntim)                                        !/*bi        148*/
      if (ncol) 20,20,80                                                        !/*bi        149*/
!>>>>>                                                                          !/*bi        150*/
  280 continue                                                                  !/*bi        151*/
  290 continue                                                                  !/*bi        152*/
  300 continue                                                                  !/*bi        153*/
      write (io,310) iopt                                                       !/*bi        154*/
      write (jtty,310) iopt                                                     !/*bi        155*/
      call errorf ('bin.2   ')                                                  !/*bi        156*/
      return                                                                    !/*bi        157*/
!                                                                               !/*bi        158*/
  310 format (5x,'error - user option',i3,' not defined')                       !/*bi        159*/
      end                                                                       !/*bi        160*/
