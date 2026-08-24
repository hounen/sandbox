!     $Id: wrt_histp.F,v 1.5 2002/06/14 17:05:42 jxh Exp $                      !/*wr          1*/
      subroutine wrt_histp                                                      !/*wr          2*/
!     HISTP Implementation: This routine needed only for histp writing.         !/*wr          3*/
#ifdef HISTP                                                                    /*!wr          4*/
!                                                                               !/*wr          5*/
!       collision tape record types (some unused):                              !/*wr          6*/
!       ncol ngo                                                                !/*wr          7*/
!        -4   1   end of run                                                    !/*wr          8*/
!        -3   2   end of batch (not used)                                       !/*wr          9*/
!        -2   3   end of tape, another will be written (not used)               !/*wr         10*/
!        -1   4   start of run                                                  !/*wr         11*/
!         0   5   not used                                                      !/*wr         12*/
!         1   6   source-particle data                                          !/*wr         13*/
!         2   7   nuclear interaction (nonabsorption)                           !/*wr         14*/
!         3   8   charged particle slowed to cutoff                             !/*wr         15*/
!         4   9   escape                                                        !/*wr         16*/
!         5  10   pseudo-collision                                              !/*wr         17*/
!         6  11   nuclear absorption                                            !/*wr         18*/
!         7  12   internal medium boundary crossing                             !/*wr         19*/
!         8  13   multiple scattering event                                     !/*wr         20*/
!         9  14   not used                                                      !/*wr         21*/
!                                                                               !/*wr         22*/
#include <lcs/double.h>                                                         /*!wr         23*/
#include <lcs/lptS.h>                                                           /*!wr         24*/
#include <lcs/rucom.h>                                                          /*!wr         25*/
#include <lcs/bankb.h>                                                          /*!wr         26*/
#include <lcs/charla.h>                                                         /*!wr         27*/
#include <lcs/concom.h>                                                         /*!wr         28*/
#include <lcs/komon.h>                                                          /*!wr         29*/
#include <lcs/inout.h>                                                          /*!wr         30*/
#ifdef CHEAP                                                                    /*!wr         31*/
      parameter (mx=16)                                                         !/*wr         32*/
#else                                                                           /*!wr         33*/
      parameter (mx=18)                                                         !/*wr         34*/
#endif                                                                          /*!wr         35*/
      character nn1*8, nn2*8                                                    !/*wr         36*/
      real bq                                                                   !/*wr         37*/
      dimension bq(7), ibq(7), iqq(28)                                          !/*wr         38*/
      equivalence (bq,ibq)                                                      !/*wr         39*/
      data iqq /28*0/                                                           !/*wrxk        1*/
      data bq /7*0.0/                                                           !/*wr         41*/
#ifdef MHISTP                                                                   /*!wrxk        2*/
      character hx*8                                                            !/*wrxk        3*/
      save hx                                                                   !/*wrxk        4*/
      data margin /4096/                                                        !/*wrxk        5*/
#endif                                                                          /*!wrxk        6*/
      data llq /29/, llpt /lpt/                                                 !/*wr         43*/
!                                                                               !/*wr         44*/
      if (nhstp.eq.0) return                                                    !/*wr         45*/
#ifdef MHISTP                                                                   /*!wrxk        7*/
      if (ntapes.eq.0) then                                                     !/*wrxk        8*/
         hx=histp                                                               !/*wrxk        9*/
         ntapes=1                                                               !/*wrxk       10*/
         nwrds=0                                                                !/*wrxk       11*/
      endif                                                                     !/*wrxk       12*/
!                                                                               !/*wrxk       13*/
!       check for file size.                                                    !/*wrxk       14*/
      if (nwrds.ge.(lhist-margin)) then                                         !/*wrxk       15*/
         if (hx.eq.'histp') hx='histpa'                                         !/*wrxk       16*/
         call unique (hx,itty)                                                  !/*wrxk       17*/
         mmcol=-2                                                               !/*wrxk       18*/
#ifdef CHEAP                                                                    /*!wrxk       19*/
         write (nhstp) mmcol,hx,(qq(i),i=1,26)                                  !/*wrxk       20*/
#else                                                                           /*!wrxk       21*/
         write (nhstp) mmcol,hx,(qq(i),i=1,27)                                  !/*wrxk       22*/
#endif                                                                          /*!wrxk       23*/
         nwrds=1+nwrds+llq                                                      !/*wrxk       24*/
         close (nhstp)                                                          !/*wrxk       25*/
         open(nhstp,file=hx,status='new',form='unformatted')                    !/*wrxk       26*/
         write (itty,20) hx                                                     !/*wrxk       27*/
         write (io,20) hx                                                       !/*wrxk       28*/
 20      format (/5x,'history tape is file',1x,a10/)                            !/*wrxk       29*/
         ntapes=ntapes+1                                                        !/*wrxk       30*/
         if (ntapes.ge.mxhist) call expire (0,'wrt_hstp',                       !/*wrxk       31*/
     &   'no. of histp files exceeds mxhist.')                                  !/*wrxk       32*/
         nwrds=0                                                                !/*wrxk       33*/
      endif                                                                     !/*wrxk       34*/
#endif                                                                          /*!wrxk       35*/
!                                                                               !/*wr         46*/
      if (ncol.eq.-4) then                                                      !/*wr         47*/
!       end of run                                                              !/*wr         48*/
        call getidt (idtm)                                                      !/*wr         49*/
        nn1(1:8)=idtm(3:10)                                                     !/*wr         50*/
        nn2(1:8)=idtm(12:19)                                                    !/*wr         51*/
        iqq(mx)=maxcas                                                          !/*wr         52*/
        write(nhstp) ncol,nn1,nn2,(ncolc(i),i=6,13),(iqq(i),i=1,mx)             !/*wr         53*/
#ifdef MHISTP                                                                   /*!wrxk       36*/
        nwrds=1+nwrds+llq                                                       !/*wrxk       37*/
#endif                                                                          /*!wrxk       38*/
        write(nhstp) wtsum,nosum                                                !/*wr         54*/
#ifdef MHISTP                                                                   /*!wrxk       39*/
#ifdef CHEAP                                                                    /*!wrxk       40*/
        nwrds=1+nwrds+24*llpt                                                   !/*wrxk       41*/
#else                                                                           /*!wrxk       42*/
        nwrds=1+nwrds+16*llpt                                                   !/*wrxk       43*/
#endif                                                                          /*!wrxk       44*/
#endif                                                                          /*!wrxk       45*/
        return                                                                  !/*wr         55*/
      else if(ncol.ne.2.and.ncol.ne.3.and.ncol.ne.5.and.ncol.ne.6) then         !/*wr         56*/
        call expirx (1,'wrt_histp',                                             !/*wr         57*/
     &    'improper value of ncol in wrt_histp.')                               !/*wr         58*/
        return                                                                  !/*wr         59*/
      endif                                                                     !/*wr         60*/
!                                                                               !/*wrxk       46*/
!                                                                               !/*wrxk       47*/
      call get_names (ncl_,nmt_,iwr_)                                           !/*wrxk       48*/
      if (iwr_.eq.0) return                                                     !/*wrxk       49*/
!                                                                               !/*wr         61*/
!                                                                               !/*wr         62*/
      ip=tip7+1.01d0                                                            !/*wr         63*/
      wtsum(ip,ncol)=wtsum(ip,ncol)+oldwt                                       !/*wr         64*/
      nosum(ip,ncol)=nosum(ip,ncol)+1                                           !/*wr         65*/
      ngo=ncol+5                                                                !/*wr         66*/
      ncolc(ngo)=ncolc(ngo)+1                                                   !/*wr         67*/
!                                                                               !/*wr         69*/
      if (iwr_.eq.0) return                                                     !/*wr         70*/
!                                                                               !/*wr         71*/
      if (ncol.eq.3 .and. (nabov+nbelo).le.0) then                              !/*wr         72*/
!       charged particle slowed to cutoff                                       !/*wr         73*/
        ibq(1)=0                                                                !/*wr         74*/
        bq(6)=0.0                                                               !/*wr         75*/
        bq(7)=0.0                                                               !/*wr         76*/
        write(nhstp)ncol,nocas,name7,nmt_,nmed7,ncl_,ncl_,real(tip7),           !/*wr         77*/
     &   real(x7),real(y7),real(z7),real(xc7),real(yc7),real(zc7),              !/*wr         78*/
     &   real(oldwt),real(wt7),real(e7),real(ec7),real(u7),real(v7),            !/*wr         79*/
     &   real(w7),real(tc7),ibq                                                 !/*wr         80*/
#ifdef MHISTP                                                                   /*!wrxk       50*/
        nwrds=nwrds+llq                                                         !/*wrxk       51*/
#endif                                                                          /*!wrxk       52*/
        return                                                                  !/*wr         81*/
!                                                                               !/*wr         82*/
      else if (ncol.eq.3 .or. ncol.eq.5) then                                   !/*wr         83*/
!       charged particle slowed to cutoff or pseudo-collision                   !/*wr         84*/
        mnuc=0                                                                  !/*wr         85*/
        exc0=dp0                                                                !/*wr         86*/
      endif                                                                     !/*wr         87*/
!                                                                               !/*wr         88*/
      write(nhstp)ncol,nocas,name7,nmt_,nmed7,ncl_,ncl_,real(tip7),             !/*wr         89*/
     & real(x7),real(y7),real(z7),real(xc7),real(yc7),real(zc7),                !/*wr         90*/
     & real(oldwt),real(wt7),real(e7),real(ec7),real(u7),real(v7),              !/*wr         91*/
     & real(w7),real(tc7),lelem,nopart,nabov,nbelo,mnuc,real(exc0),             !/*wr         92*/
     & real(geomxs)                                                             !/*wr         93*/
#ifdef MHISTP                                                                   /*!wrxk       53*/
      nwrds=1+nwrds+llq                                                         !/*wrxk       54*/
#endif                                                                          /*!wrxk       55*/
      if (mnuc.gt.0) then                                                       !/*wr         94*/
!                                                                               !/*wr         95*/
!       residual nucleus records                                                !/*wr         96*/
        do 160 i=1,mnuc                                                         !/*wr         97*/
        write (nhstp) real(aprr(i)),real(zprr(i)),real(erecr(i)),               !/*wr         98*/
     &   real(uur(i)),real(ecvr(i)),real(ur(i)),real(vr(i)),real(wr(i))         !/*wr         99*/
  160   continue                                                                !/*wr        100*/
#ifdef MHISTP                                                                   /*!wrxk       56*/
        nwrds=1+nwrds+8*mnuc                                                    !/*wrxk       57*/
#endif                                                                          /*!wrxk       58*/
!                                                                               !/*wr        101*/
      endif                                                                     !/*wr        102*/
!                                                                               !/*wr        103*/
      if (nabov.gt.0) write(nhstp) (namea(i),real(tipa(i)),real(ea(i)),         !/*wr        104*/
     & real(ua(i)),real(va(i)),real(wa(i)),real(wta(i)),real(tca(i)),           !/*wr        105*/
     & i=1,nabov)                                                               !/*wr        106*/
#ifdef MHISTP                                                                   /*!wrxk       59*/
      nwrds=1+nwrds+8*nabov                                                     !/*wrxk       60*/
#endif                                                                          /*!wrxk       61*/
      if (nbelo.gt.0) write(nhstp) (real(tipb(i)),real(eb(i)),                  !/*wr        108*/
     & real(ub(i)),real(vb(i)),real(wb(i)),real(wtb(i)),real(tcb(i)),           !/*wr        109*/
     & i=1,nbelo)                                                               !/*wr        110*/
#ifdef MHISTP                                                                   /*!wrxk       62*/
      nwrds=1+nwrds+7*nbelo                                                     !/*wrxk       63*/
#endif                                                                          /*!wrxk       64*/
#endif                                                                          /*!wr        111*/
      return                                                                    !/*wr        112*/
      end                                                                       !/*wr        113*/
