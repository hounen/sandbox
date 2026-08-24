!     $Id: ace_tot.F,v 1.6 2002/03/27 18:14:52 gwm Exp $                      !/*ac          1*/
      subroutine ace_tot (e,jtyp,med,nel,mel,a9,z9,fmf,sigg,hsigg,              !/*ac          2*/
     & xsiso,esxmp,esxln,rr,di)                                                 !/*ac          3*/
!     E     : particle energy (MeV) [LAHET: e7]                                 !/*ac          4*/
!     JTYP  : LAHET Particle type   [LAHET: ityp]                               !/*ac          5*/
!     MED   : material number       [LAHET: mtl]                                !/*ac          6*/
#include <lcs/double.h>                                                         /*!ac          7*/
#include <lcs/inout.h>                                                          /*!ac          8*/
#include <lcs/lptS.h>                                                           /*!ac          9*/
#include <lcs/rucom.h>                                                          /*!ac         10*/
#include <lcs/elstic.h>                                                         /*!ac         11*/
#include <lcs/concom.h>                                                         /*!ac         12*/
      dimension nel(1), mel(1), a9(1), z9(1), fmf(1), sigg(6,1),                !/*ac         13*/
     & hsigg(lpt,1), xsiso(2,1), esxmp(1), esxln(iemaxn,1)                      !/*ac         14*/
!=======================================================================        !/*ac         15*/
      if (jtyp.gt.2) then                                                       !/*ac         16*/
         write(io,*) 'JTYP: ', jtyp                                             !/*ac         17*/
         write(itty,*) 'JTYP: ', jtyp                                           !/*ac         18*/
         call expirx(1,'ACE_TOT','jtyp.gt.2')                                   !/*ac         19*/
      endif                                                                     !/*ac         20*/
      totm=dp0                                                                  !/*ac         21*/
      totels=dp0                                                                !/*ac         22*/
      sigdk=dp0                                                                 !/*ac         23*/
      if (med.eq.0.or.noact.eq.0.and.ielas.eq.0) go to 20                       !/*ac         24*/
      nel1=nel(med)                                                             !/*ac         25*/
      if (ielas.gt.0.and.jtyp.eq.2.and.lmxs(jtyp).ne.0) ie=ieidxn(e)            !/*ac         26*/
      do 10 i=1,nel1                                                            !/*ac         27*/
      mel1=mel(med)-1+i                                                         !/*ac         28*/
      xsiso(2,i)=dp0                                                            !/*ac         29*/
      if (lmxs(jtyp).eq.0) then                                                 !/*ac         30*/
!        NEW CROSS SECTION METHOD                                               !/*ac         31*/
         if (charge(jtyp).eq.dp0.or.n1col.eq.-1) then                           !/*ac         32*/
            call xs_real (jtyp,e,a9(mel1),z9(mel1),fmf(mel1),xs_abs,            !/*ac         33*/
     &       xsiso(2,i),xsiso(1,i))                                             !/*ac         34*/
         else                                                                   !/*ac         35*/
            call xs_max (ielas,jtyp,a9(mel1),fmf(mel1),xs_abs,                  !/*ac         36*/
     &       xsiso(2,i),xsiso(1,i))                                             !/*ac         37*/
         endif                                                                  !/*ac         38*/
      else                                                                      !/*ac         39*/
!        OLD CROSS SECTION METHOD                                               !/*ac         40*/
         if (a9(mel1).eq.dp1) then                                              !/*ac         41*/
            xsiso(1,i)=hsigg(jtyp,med)                                          !/*ac         42*/
            xsiso(2,i)=dp0                                                      !/*ac         43*/
         else                                                                   !/*ac         44*/
            xsiso(1,i)=sigg(1,mel1)                                             !/*ac         45*/
            if (ielas.eq.0.or.jtyp.eq.1.and.ielas.lt.2) then                    !/*ac         46*/
               xsiso(2,i)=dp0                                                   !/*ac         47*/
            elseif (ielas.gt.0.and.jtyp.eq.2) then                              !/*ac         48*/
               xsiso(2,i)=fsgels(e,mel1,esxln(1,1),esn,iemaxn,ie)               !/*ac         49*/
            elseif (ielas.eq.2.and.jtyp.eq.1.and.e.gt.esminp) then              !/*ac         50*/
               xsiso(2,i)=esxmp(mel1)                                           !/*ac         51*/
            endif                                                               !/*ac         52*/
         endif                                                                  !/*ac         53*/
         xsiso(1,i)=xsiso(1,i)+xsiso(2,i)                                       !/*ac         54*/
      endif                                                                     !/*ac         55*/
      if (noact.eq.0) xsiso(1,i)=xsiso(2,i)                                     !/*ac         56*/
!                                                                               !/*ac         57*/
      totm=totm+xsiso(1,i)                                                      !/*ac         58*/
      totels=totels+xsiso(2,i)                                                  !/*ac         59*/
   10 continue                                                                  !/*ac         60*/
   20 continue                                                                  !/*ac         61*/
      siga = totm - totels                                                      !/*ac         62*/
      if (n1col.eq.-1) then                                                     !/*ac         63*/
         di = tiny_float                                                        !/*ac         64*/
      else                                                                      !/*ac         65*/
         temp = totm*rr                                                         !/*ac         66*/
         if (temp.gt.dp0) then                                                  !/*ac         67*/
            di = -log(rang())/temp                                              !/*ac         68*/
         else                                                                   !/*ac         69*/
            di = huge_float                                                     !/*ac         70*/
         endif                                                                  !/*ac         71*/
      endif                                                                     !/*ac         72*/
      return                                                                    !/*ac         73*/
      end                                                                       !/*ac         74*/
