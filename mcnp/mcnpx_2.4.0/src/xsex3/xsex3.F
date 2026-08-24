!     $Id: xsex3.F,v 1.10 2002/03/27 18:16:27 gwm Exp $                        !/*xs          1*/
      program xsex3                                                             !/*xs          2*/
#include <htape3x/com32b.h>                                                     /*!xs          3*/
      external blkd                                                             !/*xs          4*/
      external bdxs, bdsr                                                       !/*xs          5*/
      logical liut                                                              !/*xs          6*/
      character*8 new, old                                                      !/*xs          7*/
#include <htape3x/label.h>                                                      /*!xs          8*/
#include <htape3x/contrl.h>                                                     /*!xs          9*/
#include <htape3x/srcwt.h>                                                      /*!xs         10*/
#include <xsex3/charxs.h>                                                       /*!xs         11*/
      data c0 /0./, s0 /0./, a0 /0./, jplot /0/                                 !/*xs         12*/
      data new /'new'/, old /'old'/                                             !/*xs         13*/
!        open the file or files for the user terminal.                          !/*xs         14*/
      itty=5                                                                    !/*xs         15*/
      jtty=6                                                                    !/*xs         16*/
      ltesting = .false.                                                        !/*xs         17*/
!                                                                               !/*xs         18*/
      call exemes (isub(1),msub(1),ndef)                                        !/*xs         19*/
      in=8                                                                      !/*xs         20*/
      io=9                                                                      !/*xs         21*/
      iut=11                                                                    !/*xs         22*/
      lfll=mdas                                                                 !/*xs         23*/
#ifdef XSEXPOINTERS                                                             /*!xs         24*/
      call mallof(kdy,8/ndp2*lfll)                                              !/*xs         25*/
      if(kdy.le.0) call errorf ('xsx.2   ')                                     !/*xs         26*/
#endif                                                                          /*!xs         27*/
      open (in,file=inxs,status=old)                                            !/*xs         28*/
      if (inqire(outxs).ne.0.and.outxs.eq.'outxs') outxs='outxsa'               !/*xs         29*/
      call uopen (jtty,io,outxs,'f')                                            !/*xs         30*/
      open (10,file=histp,status=old,access='sequential',                       !/*xs         31*/
     & form='unformatted')                                                      !/*xs         32*/
      imerge=0                                                                  !/*xs         33*/
      fwton=.false.                                                             !/*xs         34*/
      nh10=10                                                                   !/*xs         35*/
      nocasx=0                                                                  !/*xs         36*/
      nocol10=0                                                                 !/*xs         37*/
      ibad10=0                                                                  !/*xs         38*/
      ifirst=0                                                                  !/*xs         39*/
      iscrt=4                                                                   !/*xs         40*/
      nplot=0                                                                   !/*xs         41*/
      nhst=10                                                                   !/*xs         42*/
      icase=0                                                                   !/*xs         43*/
      nexlo=0                                                                   !/*xs         44*/
      call krack                                                                !/*xs         45*/
      call scanx                                                                !/*xs         46*/
      call secnd (cpa)                                                          !/*xs         47*/
      do 10 nc=1,ncase                                                          !/*xs         48*/
      call inx (iblk(iperg+lerg(nc)),iblk(ipang+lang(nc)),iblk(iptyp            !/*xs         49*/
     & +ltyp(nc)),nc)                                                           !/*xs         50*/
!     subroutine inx (ergb,xmu,itype,nc)                                        !/*xs         51*/
   10 continue                                                                  !/*xs         52*/
      call bx (iblk(iptll),iblk(iptls),iblk(iphld),iblk(iperg),iblk             !/*xs         53*/
     & (ipang),iblk(iptyp))                                                     !/*xs         54*/
      nn=maxcas*maxbch                                                          !/*xs         55*/
      write (io,150) nn                                                         !/*xs         56*/
      write (jtty,150) nn                                                       !/*xs         57*/
  150 format (' statistical degrees of freedom (number of histories) =',        !/*xs         58*/
     & i9)                                                                      !/*xs         59*/
#ifdef XSPLOT                                                                   /*!xs         60*/
      if (kplot.ne.0.and.jplot.eq.0) then                                       !/*xs         61*/
        call gplot (1hu,4hxsex,6)                                               !/*xs         62*/
        call libhi                                                              !/*xs         63*/
        jplot=1                                                                 !/*xs         64*/
      endif                                                                     !/*xs         65*/
#endif                                                                          /*!xs         66*/
      call edx1                                                                 !/*xs         67*/
      do 20 nc=1,ncase                                                          !/*xs         68*/
      icase=nc                                                                  !/*xs         69*/
      iyield=iyeld0(nc)                                                         !/*xs         70*/
      imom=imom0(nc)                                                            !/*xs         71*/
      nerg=nnerg(nc)                                                            !/*xs         72*/
      nang=nnang(nc)                                                            !/*xs         73*/
      ntype=nntype(nc)                                                          !/*xs         74*/
      kplot=kplot0(nc)                                                          !/*xs         75*/
      call edx (iblk(iptll+nsize(nc)),iblk(iptls+nsize(nc)),nang1(nc)           !/*xs         76*/
     & ,nerg1(nc),iblk(iphld),iblk(iphld+ndp2*nerg1(nc)),iblk(iperg+lerg        !/*xs         77*/
     & (nc)),iblk(ipang+lang(nc)),iblk(iptyp+ltyp(nc)),nc)                      !/*xs         78*/
      if (kplot.ne.0) then                                                      !/*xs         79*/
#ifdef XSPLOT                                                                   /*!xs         80*/
        i1=iphld+ndp2*max(nerg1(nc),nang1(nc))                                  !/*xs         81*/
        i2=i1+2*ndp2*nang1(nc)                                                  !/*xs         82*/
        i3=i2+2*ndp2*nerg1(nc)                                                  !/*xs         83*/
        call plx (iblk(iptll+nsize(nc)),iblk(iptls+nsize(nc)),nang1(nc)         !/*xs         84*/
     &  ,nerg1(nc),iblk(iphld),iblk(i1),iblk(i2),iblk(i3),iblk(iperg            !/*xs         85*/
     &  +lerg(nc)),iblk(ipang+lang(nc)),iblk(iptyp+ltyp(nc)))                   !/*xs         86*/
#endif                                                                          /*!xs         87*/
        inquire (unit=iut, opened=liut)                                         !/*xs         88*/
        if (.not.liut) then                                                     !/*xs         89*/
          if (inqire(xstal).ne.0.and.xstal.eq.'xstal') xstal='xstala'           !/*xs         90*/
          call uopen (jtty,iut,xstal,'f')                                       !/*xs         91*/
        endif                                                                   !/*xs         92*/
        call xstalw (iut,iblk(iptll+nsize(nc)),iblk(iptls+nsize(nc)),           !/*xs         93*/
     &  nang1(nc),nerg1(nc),iblk(iperg+lerg(nc)),iblk(ipang+lang(nc)),          !/*xs         94*/
     &  nc)                                                                     !/*xs         95*/
      endif                                                                     !/*xs         96*/
   20 continue                                                                  !/*xs         97*/
      if (kplot.ne.0) then                                                      !/*xs         98*/
        if (ltesting) then                                                      !/*xs         99*/
          write (io,*)'MCTAL-format file written. Use NONORM.'                  !/*xs        100*/
        else                                                                    !/*xs        101*/
          write (io,*)'MCTAL-format file ',xstal,' written. Use NONORM.'        !/*xs        102*/
        end if                                                                  !/*xs        103*/
        write (jtty,*)'MCTAL-format file ',xstal,' written. Use NONORM.'        !/*xs        104*/
        close (iut)                                                             !/*xs        105*/
      endif                                                                     !/*xs        106*/
!                                                                               !/*xs        107*/
      call timeit                                                               !/*xs        108*/
#ifdef XSPLOT                                                                   /*!xs        109*/
      if (jplot.ne.0) call gdone                                                !/*xs        110*/
#endif                                                                          /*!xs        111*/
      stop                                                                      !/*xs        112*/
      end                                                                       !/*xs        113*/
