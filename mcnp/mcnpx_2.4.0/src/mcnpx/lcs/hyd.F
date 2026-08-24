!     $Id: hyd.F,v 1.11 2002/03/27 18:15:06 gwm Exp $                         !/*hy          1*/
      subroutine hyd (ecl,den_h,hsigg_,lmxs_)                                   !/*hy          2*/
!     ==================================================================        !/*hy          3*/
!     hyd calls: CRSEC   HEH   SCATTL   PCOL   SGM2                             !/*hy          4*/
!     ==================================================================        !/*hy          5*/
#include <lcs/double.h>                                                         /*!hy          6*/
#include <lcs/lptS.h>                                                           /*!hy          7*/
#include <lcs/rucom.h>                                                          /*!hy          8*/
#include <lcs/cacom.h>                                                          /*!hy          9*/
#include <lcs/banka.h>                                                          /*!hy         10*/
      ect=ecl                                                                   !/*hy         11*/
      if (ihie.ne.0) then                                                       !/*hy         12*/
#ifndef OLDM                                                                    /*!hy         13*/
         call expirx(1,'HYD','ihie.ne.0')                                       !/*hy         14*/
#else                                                                           /*!hy         15*/
         inc=ityp-1                                                             !/*hy         16*/
!        Get H cross section at ec.                                             !/*hy         17*/
         call crsec (ect,inc,heht,hehel,helnel)                                 !/*hy         18*/
         if (lmxs_.ne.0) then                                                   !/*hy         19*/
            r=rang()                                                            !/*hy         20*/
            if (heht*den_h*1.d+24/hsigg_-r.lt.dp0) then                         !/*hy         21*/
!____          LCS: call revnt (4)                                              !/*hy         22*/
               lelem=0                                                          !/*hy         23*/
               nopart=-1                                                        !/*hy         24*/
               go to 60                                                         !/*hy         25*/
            endif                                                               !/*hy         26*/
         endif                                                                  !/*hy         27*/
!        Real event                                                             !/*hy         28*/
         r=rang()                                                               !/*hy         29*/
         if (hehel/heht.gt.r) then                                              !/*hy         30*/
!           Elastic                                                             !/*hy         31*/
            call scattl (inc,ect,kind,ep,alpha,beta,gam)                        !/*hy         32*/
            nopart=2                                                            !/*hy         33*/
            wtfas(1)=dp1                                                        !/*hy         34*/
            wtfas(2)=dp1                                                        !/*hy         35*/
         else                                                                   !/*hy         36*/
!           Nonelastic                                                          !/*hy         37*/
            call heh (inc,ect,kind,ep,alpha,beta,gam,wtfas,nopart)              !/*hy         38*/
         endif                                                                  !/*hy         39*/
#endif                                                                          /*!hy         40*/
      else                                                                      !/*hy         41*/
         it=ityp-ityp/5                                                         !/*hy         42*/
         call sgm2 (it,ecl,i,ei,hsig)                                           !/*hy         43*/
         if (lmxs_.ne.0) then                                                   !/*hy         44*/
            r=rang()                                                            !/*hy         45*/
            if (hsig*den_h*1.d+24/hsigg_-r.lt.dp0) then                         !/*hy         46*/
!____          LCS: call revnt (4)                                              !/*hy         47*/
#ifdef OLDM                                                                     /*!hy         48*/
! +++          QUESTION: Is the setting "lelem=0" needed?                       !/*hy         49*/
! +++          Latest Prael work omits the following line.                      !/*hy         50*/
               lelem=0                                                          !/*hy         51*/
! +++          However, instances of lelem .ne. 0 occur --with-OLDXS            !/*hy         52*/
#endif                                                                          /*!hy         53*/
               nopart=-1                                                        !/*hy         54*/
               go to 60                                                         !/*hy         55*/
            endif                                                               !/*hy         56*/
         endif                                                                  !/*hy         57*/
         call pcol (ityp,hsig,ect,nopart,kind,ep,alpha,beta,gam,andit9)         !/*hy         58*/
      endif                                                                     !/*hy         59*/
!____ LCS: call revnt (3)                                                       !/*hy         60*/
   60 apr=dp0                                                                   !/*hy         61*/
      zpr=dp0                                                                   !/*hy         62*/
      ex=dp0                                                                    !/*hy         63*/
      exc0=dp0                                                                  !/*hy         64*/
      erec=dp0                                                                  !/*hy         65*/
      do 70 i=1,6                                                               !/*hy         66*/
         npart(i)=0                                                             !/*hy         67*/
   70 continue                                                                  !/*hy         68*/
      uu=dp0                                                                    !/*hy         69*/
      return                                                                    !/*hy         70*/
      end                                                                       !/*hy         71*/
