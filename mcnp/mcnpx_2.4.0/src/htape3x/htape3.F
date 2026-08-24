!     $Id: htape3.F,v 1.9 2002/03/27 18:13:59 gwm Exp $                       !/*ht          1*/
#ifdef LCS                                                                      /*!ht          2*/
      program htape3                                                            !/*ht          3*/
#else                                                                           /*!ht          4*/
      program htape3x                                                           !/*ht          5*/
#endif                                                                          /*!ht          6*/
#include <htape3x/com32b.h>                                                     /*!ht          7*/
      external blkd                                                             !/*ht          8*/
      external bdht, bdsr                                                       !/*ht          9*/
! main routine of the HTAPE3 code                                               !/*ht         10*/
      character *8 min, mout, mh10, mh11, mhist                                 !/*ht         11*/
      character *8 new, old                                                     !/*ht         12*/
      logical ifl10, ifl11                                                      !/*ht         13*/
#include <htape3x/bulk.h>                                                       /*!ht         14*/
#include <htape3x/label.h>                                                      /*!ht         15*/
#include <htape3x/contrl.h>                                                     /*!ht         16*/
#include <htape3x/com11.h>                                                      /*!ht         17*/
#include <htape3x/bndy.h>                                                       /*!ht         18*/
#include <htape3x/ctconv.h>                                                     /*!ht         19*/
#include <htape3x/exm.h>                                                        /*!ht         20*/
#include <htape3x/srcwt.h>                                                      /*!ht         21*/
#include <htape3x/charht.h>                                                     /*!ht         22*/
      character*8 kodvar                                                        !/*ht         23*/
      data kodvar/'........'/                                                   !/*ht         24*/
      data c0 /0./, s0 /0./, a0 /0./                                            !/*ht         25*/
      data jplot /0/                                                            !/*ht         26*/
      data new /'new'/, old /'old'/                                             !/*ht         27*/
!        open the file or files for the user terminal.                          !/*ht         28*/
      itty=5                                                                    !/*ht         29*/
      jtty=6                                                                    !/*ht         30*/
      ltesting = .false.                                                        !/*ht         31*/
!                                                                               !/*ht         32*/
      call exemes (isub(1),msub(1),ndef)                                        !/*ht         33*/
      in=8                                                                      !/*ht         34*/
      io=9                                                                      !/*ht         35*/
      open (in,file=int,status=old)                                             !/*ht         36*/
      if (inqire(outt).ne.0.and.outt.eq.'outt') outt='outta'                    !/*ht         37*/
      call uopen (jtty,io,outt,'f')                                             !/*ht         38*/
      nplot=0                                                                   !/*ht         39*/
      ifirst=0                                                                  !/*ht         40*/
      iscrt=4                                                                   !/*ht         41*/
      lfll=ndas                                                                 !/*ht         42*/
      icase=0                                                                   !/*ht         43*/
      nexlo=0                                                                   !/*ht         44*/
      engnu=(pmass(3)**2-pmass(6)**2)/(2.0*pmass(3))                            !/*ht         45*/
      min=int                                                                   !/*ht         46*/
      mout=outt                                                                 !/*ht         47*/
      nhst=0                                                                    !/*ht         48*/
      nh10=10                                                                   !/*ht         49*/
      nh11=11                                                                   !/*ht         50*/
      mh10=histp                                                                !/*ht         51*/
      mh11=histx                                                                !/*ht         52*/
      inquire (file=mh10,exist=ifl10)                                           !/*ht         53*/
      if (ifl10) then                                                           !/*ht         54*/
        mhist=mh10                                                              !/*ht         55*/
        nhst=nh10                                                               !/*ht         56*/
        open (nh10,file=histp,status=old,access='sequential',                   !/*ht         57*/
     &   form='unformatted')                                                    !/*ht         58*/
      endif                                                                     !/*ht         59*/
      inquire (file=mh11,exist=ifl11)                                           !/*ht         60*/
      if (ifl11) then                                                           !/*ht         61*/
        mhist=mh11                                                              !/*ht         62*/
        nhst=nh11                                                               !/*ht         63*/
        open (nh11,file=histx,status=old,access='sequential',                   !/*ht         64*/
     &   form='unformatted')                                                    !/*ht         65*/
        read (nh11) kodvar                                                      !/*ht         66*/
        lf11a=.false.                                                           !/*ht         67*/
        if (kodvar(1:4).eq.'mcnp') lf11a=.true.                                 !/*ht         68*/
        rewind nh11                                                             !/*ht         69*/
!       write (jtty,*) ' mh11,nh11,kodvar,lf11a ',mh11,nh11,kodvar,lf11a        !/*ht         70*/
      endif                                                                     !/*ht         71*/
      if (nhst.ne.0) go to 10                                                   !/*ht         72*/
      write (jtty,60)                                                           !/*ht         73*/
      write (io,60)                                                             !/*ht         74*/
      call errorf ('htp.1   ')                                                  !/*ht         75*/
   10 continue                                                                  !/*ht         76*/
      write (jtty,70) min,mout,mhist                                            !/*ht         77*/
      call krack                                                                !/*hta         1*/
   20 if (ifl10) rewind nh10                                                    !/*ht         79*/
      if (ifl11) rewind nh11                                                    !/*ht         80*/
      lf11b=.false.                                                             !/*ht         81*/
      ncol10=0                                                                  !/*ht         82*/
      ncol11=0                                                                  !/*ht         83*/
      ibad10=0                                                                  !/*ht         84*/
      nocasp=0                                                                  !/*ht         85*/
      call secnd (cpa)                                                          !/*ht         86*/
      icase=icase+1                                                             !/*ht         87*/
      call input (nerg,nerg1,ntim,ntim1,*40)                                    !/*ht         88*/
#ifdef HTPLOT                                                                   /*!ht         89*/
      if (kplot.eq.0.or.jplot.ne.0) go to 30                                    !/*ht         90*/
      call gplot (1hu,5hhtape,6)                                                !/*ht         91*/
      call libhi                                                                !/*ht         92*/
      jplot=1                                                                   !/*ht         93*/
   30 continue                                                                  !/*ht         94*/
#endif                                                                          /*!ht         95*/
      call bin (blk(iptll),blk(iphld),blk(iptls),blk(ipcov),blk(ipfsq)          !/*ht         96*/
     & ,nerg,nerg1,ntim,ntim1)                                                  !/*ht         97*/
      nn=maxcas*maxbch                                                          !/*ht         98*/
      write (io,150) nn                                                         !/*ht         99*/
      write (jtty,150) nn                                                       !/*ht        100*/
  150 format (' statistical degrees of freedom (number of histories) =',        !/*ht        101*/
     & i9)                                                                      !/*ht        102*/
      call edit (blk(iptll),blk(iptls),blk(ipcov),blk(ipfsq),nerg,nerg1         !/*ht        103*/
     & ,ntim,ntim1)                                                             !/*ht        104*/
      write (jtty,80) icase                                                     !/*ht        105*/
      write (io,80) icase                                                       !/*ht        106*/
#ifdef LCS                                                                      /*!ht        107*/
      call timeit                                                               !/*ht        108*/
#endif                                                                          /*!ht        109*/
      if (ifl10) call newfil (mh10,nh10)                                        !/*ht        110*/
      if (ifl11) then                                                           !/*ht        111*/
        call newfil (mh11,nh11)                                                 !/*ht        112*/
        nhst=nh11                                                               !/*ht        113*/
      endif                                                                     !/*ht        114*/
      go to 20                                                                  !/*ht        115*/
   40 continue                                                                  !/*ht        116*/
#ifdef HTPLOT                                                                   /*!ht        117*/
      if (jplot.ne.0) call gdone                                                !/*ht        118*/
#endif                                                                          /*!ht        119*/
      close (in)                                                                !/*ht        120*/
      close (io)                                                                !/*ht        121*/
      stop                                                                      !/*ht        122*/
!                                                                               !/*ht        123*/
   60 format (5x,'unable to locate a valid histp file')                         !/*ht        124*/
   70 format (5x,'input file =',a8,5x,'output file =',a8,                       !/*ht        125*/
     & 'history tape =',a8)                                                     !/*ht        126*/
   80 format (/5x,'case no.',i3,' completed')                                   !/*ht        127*/
      end                                                                       !/*ht        128*/
