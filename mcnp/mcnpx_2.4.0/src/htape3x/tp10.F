!     $Id: tp10.F,v 1.4 2002/03/27 18:14:03 gwm Exp $                         !/*tp          1*/
      subroutine tp10 (iflag,nc)                                                !/*tp          2*/
#include <htape3x/com32b.h>                                                     /*!tp          3*/
#include <htape3x/srcwt.h>                                                      /*!tp          4*/
#include <htape3x/label.h>                                                      /*!tp          5*/
#include <htape3x/contrl.h>                                                     /*!tp          6*/
#include <htape3x/com10.h>                                                      /*!tp          7*/
      parameter (mx=29)                                                         !/*tp          8*/
      dimension iqq(mx)                                                         !/*tp          9*/
      equivalence (iqq(1),ncol)                                                 !/*tp         10*/
      dimension nc(8)                                                           !/*tp         11*/
   10 continue                                                                  !/*tp         12*/
      if (log10) then                                                           !/*tp         13*/
        ibad=0                                                                  !/*tp         14*/
        read (nhst,end=60,iostat=ibad,err=60) iqq10                             !/*tp         15*/
        if (ibad.ne.0) go to 60                                                 !/*tp         16*/
      endif                                                                     !/*tp         17*/
      do 20 i=1,mx                                                              !/*tp         18*/
   20 iqq(i)=iqq10(i)                                                           !/*tp         19*/
      if (fwton.and.ncol.eq.1) call weight                                      !/*tp         20*/
      if (ncol.le.0) go to 50                                                   !/*tp         21*/
      nocs10=nocas                                                              !/*tp         22*/
      if (.not.log10) then                                                      !/*tp         23*/
        log10=.true.                                                            !/*tp         24*/
        go to 110                                                               !/*tp         25*/
      endif                                                                     !/*tp         26*/
      if (maxcas.gt.0.and.nocas.gt.maxcas) go to 65                             !/*tp         27*/
      if (nocasx.gt.0.and.nocas.ne.nocasx) go to 40                             !/*tp         28*/
      if (nocasx.gt.0.and.nocas.eq.nocasx) then                                 !/*tp         29*/
        write (io,*) ' NCOL  NOCAS  NAME LELEM   MAT  NMED  BOLD   ',           !/*tp         30*/
     &  'BLZ NOPART '                                                           !/*tp         31*/
        write (io,1003) ncol,nocas,name,lelem,mat,nmed,bold,blz,nopart          !/*tp         32*/
        write (io,*) '   TIP     OLDWT            WT             TC'            !/*tp         33*/
        write (io,1002) tip,oldwt,wt,tc                                         !/*tp         34*/
        write (io,*) '       E              EC             EX'                  !/*tp         35*/
        write (io,1001) e,ec,ex                                                 !/*tp         36*/
        write (io,*) '       X              Y              Z'                   !/*tp         37*/
        write (io,1001) x,y,z                                                   !/*tp         38*/
        write (io,*) '       XC             YC             XC'                  !/*tp         39*/
        write (io,1001) xc,yc,zc                                                !/*tp         40*/
        write (io,*) '       U              V              W'                   !/*tp         41*/
        write (io,1001) u,v,w                                                   !/*tp         42*/
      endif                                                                     !/*tp         43*/
 1001 format(1p4e15.4)                                                          !/*tp         44*/
 1002 format(0pf6.0,1p4e15.4)                                                   !/*tp         45*/
 1003 format(i3,i9,7i6)                                                         !/*tp         46*/
      if (nc(ncol).eq.0) go to 40                                               !/*tp         47*/
      if (iflag.le.0) go to 30                                                  !/*tp         48*/
      ityp=tip+1.001                                                            !/*tp         49*/
      if (ncp(ityp).eq.0) go to 40                                              !/*tp         50*/
   30 go to (110,100,100,110,100,100,110,110), ncol                             !/*tp         51*/
   40 go to (10,90,90,10,90,90,10,10), ncol                                     !/*tp         52*/
   50 ngo=ncol+5                                                                !/*tp         53*/
      go to (120,110,70,80,60), ngo                                             !/*tp         54*/
!------ unexpected end of file                                                  !/*tp         55*/
   60 continue                                                                  !/*tp         56*/
      maxcas=nocas                                                              !/*tp         57*/
!     backspace nhst                                                            !/*tp         58*/
!     read (nhst) (iqq10(i),i=1,3)                                              !/*tp         59*/
      write (io,130) ibad,iqq(1),iqq(2),iqq(3),iqq10(1),iqq10(2),               !/*tp         60*/
     & iqq10(3)                                                                 !/*tp         61*/
      write (jtty,130) ibad,iqq(1),iqq(2),iqq(3),iqq10(1),iqq10(2),             !/*tp         62*/
     & iqq10(3)                                                                 !/*tp         63*/
      write (io,140) maxcas                                                     !/*tp         64*/
      write (jtty,140) maxcas                                                   !/*tp         65*/
   65 continue                                                                  !/*tp         66*/
      nocas=nocs10                                                              !/*tp         67*/
      ncol=-4                                                                   !/*tp         68*/
      maxbch=1                                                                  !/*tp         69*/
      name=0                                                                    !/*tp         70*/
      go to 110                                                                 !/*tp         71*/
!------ end of tape                                                             !/*tp         72*/
   70 continue                                                                  !/*tp         73*/
      call eotap                                                                !/*tp         74*/
      go to 10                                                                  !/*tp         75*/
!------ start of run                                                            !/*tp         76*/
   80 call sorun                                                                !/*tp         77*/
      if (iopt.eq.16) call filden                                               !/*tp         78*/
      if (iflag.eq.-1) go to 110                                                !/*tp         79*/
      go to 10                                                                  !/*tp         80*/
!------ skip follow-on records                                                  !/*tp         81*/
   90 continue                                                                  !/*tp         82*/
      call skipper (*60)                                                        !/*tp         83*/
      go to 10                                                                  !/*tp         84*/
!------ read follow-on records                                                  !/*tp         85*/
  100 call forec (*60)                                                          !/*tp         86*/
  110 return                                                                    !/*tp         87*/
  120 continue                                                                  !/*tp         88*/
!------ end of run                                                              !/*tp         89*/
      if (iqq10(mx).gt.0) then                                                  !/*tp         90*/
        maxcas=iqq10(mx)                                                        !/*tp         91*/
        iqq(mx)=0                                                               !/*tp         92*/
      endif                                                                     !/*tp         93*/
      read (nhst) wtsum,nosum                                                   !/*tp         94*/
      backspace nhst                                                            !/*tp         95*/
      go to 110                                                                 !/*tp         96*/
!                                                                               !/*tp         97*/
  130 format (' unexpected EOF in TP10 - error code =',i5/                      !/*tp         98*/
     & ' last good record NCOL =',i2,' NOCAS =',i9,' NAME =',i7/                !/*tp         99*/
     & ' record in error NCOL =',i2,' NOCAS =',i9,' NAME =',i7)                 !/*tp        100*/
  140 format(' norm reset to MAXCAS =',i8)                                      !/*tp        101*/
      end                                                                       !/*tp        102*/
