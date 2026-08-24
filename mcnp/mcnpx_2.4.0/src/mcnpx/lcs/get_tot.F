!     $Id: get_tot.F,v 1.8 2002/03/27 18:15:05 gwm Exp $                      !/*ge          1*/
      subroutine get_tot (e,jtyp,med,nel,mel,                                   !/*ge          2*/
#ifdef CEM                                                                      /*!ge          3*/
     & ipnf,                                                                    !/*ge          4*/
#endif                                                                          /*!ge          5*/
     & a9,                                                                      !/*ge          6*/
     & z9,fmf,                                                                  !/*ge          7*/
     & sigg,hsigg,xsiso,                                                        !/*ge          8*/
#ifdef CEM                                                                      /*!ge          9*/
     & epnmin,                                                                  !/*ge         10*/
#endif                                                                          /*!ge         11*/
     & rr,di)                                                                   !/*ge         12*/
!     E     : particle energy (MeV) [LAHET: e7]                                 !/*ge         13*/
!     JTYP  : LAHET Particle type   [LAHET: ityp]                               !/*ge         14*/
!     MED   : material number       [LAHET: mtl]                                !/*ge         15*/
#include <lcs/double.h>                                                         /*!ge         16*/
#include <lcs/lptS.h>                                                           /*!ge         17*/
#include <lcs/rucom.h>                                                          /*!ge         18*/
#include <lcs/concom.h>                                                         /*!ge         19*/
      dimension nel(1), mel(1), a9(1), z9(1), fmf(1),                           !/*ge         20*/
     & sigg(6,1), hsigg(lpt,1), xsiso(2,1)                                      !/*ge         21*/
#ifdef CEM                                                                      /*!ge         22*/
      dimension ipnf(1), epnmin(1)                                              !/*ge         23*/
#endif                                                                          /*!ge         24*/
      dimension itt(lpt)                                                        !/*ge         25*/
#ifdef CEM                                                                      /*!ge         26*/
! FCG TEMPORARY expedient:  this is unchanged.                                  !/*ge         27*/
      data itt /5*1,0,0,3,4,5,6,0,4*1,2,2,4*0/                                  !/*ge         28*/
! FCG                                                                           !/*ge         29*/
#else                                                                           /*!ge         30*/
      data itt /5*1,0,0,3,4,5,6,0,4*1,2,2,4*0/                                  !/*ge         31*/
#endif                                                                          /*!ge         32*/
!=======================================================================        !/*ge         33*/
      if (jtyp.le.2) call expirx(1,'GET_TOT','jtyp.le.2')                       !/*ge         34*/
      totels=dp0                                                                !/*ge         35*/
      totm=dp0                                                                  !/*ge         36*/
      sigdk=dp0                                                                 !/*ge         37*/
      if (med.eq.0.or.noact.eq.0.and.ielas.eq.0) go to 20                       !/*ge         38*/
      ii=itt(jtyp)                                                              !/*ge         39*/
      if (ii.le.0) go to 20                                                     !/*ge         40*/
      nel1=nel(med)                                                             !/*ge         41*/
      do 10 i=1,nel1                                                            !/*ge         42*/
      mel1=mel(med)-1+i                                                         !/*ge         43*/
      if (lmxs(jtyp).ne.0) then                                                 !/*ge         44*/
!        OLD CROSS SECTION METHOD                                               !/*ge         45*/
         if (a9(mel1).eq.dp1) then                                              !/*ge         46*/
            xsiso(1,i)=hsigg(jtyp,med)                                          !/*ge         47*/
         else                                                                   !/*ge         48*/
            xsiso(1,i)=sigg(ii,mel1)                                            !/*ge         49*/
         endif                                                                  !/*ge         50*/
         xsiso(2,i)=dp0                                                         !/*ge         51*/
      else                                                                      !/*ge         52*/
!        NEW CROSS SECTION METHOD                                               !/*ge         53*/
         if (charge(jtyp).eq.dp0.or.n1col.eq.-1) then                           !/*ge         54*/
            call xs_real (jtyp,e,a9(mel1),z9(mel1),fmf(mel1),xs_abs,            !/*ge         55*/
     &       xsiso(2,i),xsiso(1,i))                                             !/*ge         56*/
         else                                                                   !/*ge         57*/
            call xs_max (ielas,jtyp,a9(mel1),fmf(mel1),xs_abs,                  !/*ge         58*/
     &       xsiso(2,i),xsiso(1,i))                                             !/*ge         59*/
         endif                                                                  !/*ge         60*/
      endif                                                                     !/*ge         61*/
!=======================================================================        !/*ge         62*/
      if (noact.eq.0) xsiso(1,i)=xsiso(2,i)                                     !/*ge         63*/
      totm=totm+xsiso(1,i)                                                      !/*ge         64*/
      totels=totels+xsiso(2,i)                                                  !/*ge         65*/
   10 continue                                                                  !/*ge         66*/
   20 continue                                                                  !/*ge         67*/
!----- unstable neutral particle                                                !/*ge         68*/
      if (instab(jtyp).ne.0) then                                               !/*ge         69*/
         if (charge(jtyp).eq.dp0.or.med.eq.0) then                              !/*ge         70*/
            edkmin = e                                                          !/*ge         71*/
         else                                                                   !/*ge         72*/
            edkmin = emin(jtyp)                                                 !/*ge         73*/
         endif                                                                  !/*ge         74*/
         sigdk = dkcons(jtyp)/sqrt((edkmin/pmass(jtyp)+dp1)**2-dp1)             !/*ge         75*/
      endif                                                                     !/*ge         76*/
      if (n1col.eq.-1) then                                                     !/*ge         77*/
         di = tiny_float                                                        !/*ge         78*/
      else                                                                      !/*ge         79*/
         temp = totm*rr + sigdk                                                 !/*ge         80*/
         if (temp.gt.dp0) then                                                  !/*ge         81*/
            di = -log(rang())/temp                                              !/*ge         82*/
         else                                                                   !/*ge         83*/
            di = huge_float                                                     !/*ge         84*/
         endif                                                                  !/*ge         85*/
      endif                                                                     !/*ge         86*/
      return                                                                    !/*ge         87*/
      end                                                                       !/*ge         88*/
