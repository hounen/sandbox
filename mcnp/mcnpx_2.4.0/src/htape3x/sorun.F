!     $Id: sorun.F,v 1.12 2002/03/27 18:14:03 gwm Exp $                       !/*so          1*/
      subroutine sorun                                                          !/*so          2*/
#include <htape3x/com32b.h>                                                     /*!so          3*/
#include <htape3x/label.h>                                                      /*!so          4*/
#include <htape3x/contrl.h>                                                     /*!so          5*/
      character *8 nn1, nn2, info, wkod                                         !/*so          6*/
#ifdef CHEAP                                                                    /*!so          7*/
      equivalence (nn1,iqq(2)), (nn2,iqq(4)), (info,iqq(6)),                    !/*so          8*/
     & (wkod,iqq(8))                                                            !/*so          9*/
#endif                                                                          /*!so         10*/
#ifndef CHEAP                                                                   /*!so         11*/
      equivalence (nn1,iqq(2)), (nn2,iqq(3)), (info,iqq(4)),                    !/*so         12*/
     & (wkod,iqq(5))                                                            !/*so         13*/
#endif                                                                          /*!so         14*/
      dimension iqq(29)                                                         !/*so         15*/
      equivalence (iqq(1),ncol)                                                 !/*so         16*/
      parameter (lpt2=lpt+2)                                                    !/*so         17*/
      character *9 kip0, lip0(lpt2)                                             !/*so         18*/
      data lip0 /'proton','neutron','pi+','pi0','pi-','mu+','mu-',              !/*so         19*/
     & 'deutron','triton','he-3','alpha','photon','K+','K0-long',               !/*so         20*/
     & 'K0-shrt','K-','p-bar','n-bar','e-','e+','nu','nu-bar',                  !/*so         21*/
     & 'pi decay','surface'/                                                    !/*so         22*/
      data iflag /0/                                                            !/*so         23*/
#ifdef CHEAP                                                                    /*!so         24*/
!     histp written with jip0 stored in iqq(10)                                 !/*so         25*/
      jip0=iqq(10)                                                              !/*so         26*/
#endif                                                                          /*!so         27*/
#ifndef CHEAP                                                                   /*!so         28*/
!     histp written with jip0 stored in iqq(6)                                  !/*so         29*/
      jip0=iqq(6)                                                               !/*so         30*/
#endif                                                                          /*!so         31*/
      if (wkod.eq.nn1) wkod=' '                                                 !/*so         32*/
      kip0=lip0(jip0+1)                                                         !/*so         33*/
   10 continue                                                                  !/*so         34*/
      read (nhst) maxbch,maxcas,mxmat,lneutp,npidk,n1col,nobalc,emin            !/*so         35*/
     & ,emax,nexite,nspred,nstrag,nseudo,idummy,ibertp,icpt,ipht                !/*so         36*/
      read (nhst) (nel(m),m=1,mxmat)                                            !/*so         37*/
      mel(1)=1                                                                  !/*so         38*/
      do 20 m=1,mxmat                                                           !/*so         39*/
      nelm=nel(m)                                                               !/*so         40*/
      mel(m+1)=mel(m)+nelm                                                      !/*so         41*/
      read (nhst) (zz(mel(m)+l-1),a(mel(m)+l-1),(sigg(ll,mel(m)+l-1),ll=        !/*so         42*/
     & 1,6),l=1,nelm)                                                           !/*so         43*/
      read (nhst) (sigmx(i,m),i=1,lpt)                                          !/*so         44*/
      read (nhst) (hsigg(i,m),i=1,lpt)                                          !/*so         45*/
   20 continue                                                                  !/*so         46*/
      mx=mxmat+1                                                                !/*so         47*/
      read (nhst) (sigmx(i,mx),i=1,lpt)                                         !/*so         48*/
      if (iflag.eq.0) then                                                      !/*so         49*/
        iflag=1                                                                 !/*so         50*/
        if (.not.ltesting) then                                                 !/*so         51*/
          write (io,30) info,wkod,nn1,nn2                                       !/*so         52*/
        else                                                                    !/*so         53*/
          write (io,40) info                                                    !/*so         54*/
        endif                                                                   !/*so         55*/
        write (io,70) maxbch,maxcas,mxmat,lneutp,npidk,n1col,nobalc,            !/*so         56*/
     & nexite,nspred,nstrag,nseudo,ibertp                                       !/*so         57*/
     &   ,icpt,ipht,emax,(emin(l),l=1,lpt)                                      !/*so         58*/
        write (jtty,50) info,kip0                                               !/*so         59*/
        write (io,60) info,kip0                                                 !/*so         60*/
      endif                                                                     !/*so         61*/
      return                                                                    !/*so         62*/
!                                                                               !/*so         63*/
   50 format (' ',a8,' was generated by a ',a8,' source')                       !/*so         64*/
   60 format ('0',a8,' was generated by a ',a8,' source')                       !/*so         65*/
   30 format (1h1,20x,' reading ',a8,' as history tape written by ',            !/*so         66*/
     & a8,2a10)                                                                 !/*so         67*/
   40 format (1h1,20x,' reading ',a8,' as history tape written ',               !/*so         68*/
     & 'with parameters:')                                                      !/*so         69*/
   70 format (/' maxbch    maxcas  mxmat lneutp  npidk  n1col  ',               !/*so         70*/
     & 'nbalc nexite nspred nstrag nseudo ibertp   icpt   ipht'/                !/*so         71*/
     & i6,i10,12i7//' emax ',e12.4//' emin:'/(6x,7e12.4))                       !/*so         72*/
      end                                                                       !/*so         73*/
