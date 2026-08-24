!     $Id: nobal.F,v 1.4 2002/03/27 18:15:12 gwm Exp $                        !/*no          1*/
      subroutine nobal (ecl,a1,z1,nrj,negzpr)                                   !/*no          2*/
#include <lcs/double.h>                                                         /*!no          3*/
#include <lcs/lptS.h>                                                           /*!no          4*/
#include <lcs/rucom.h>                                                          /*!no          5*/
#include <lcs/cacom.h>                                                          /*!no          6*/
#include <lcs/banka.h>                                                          /*!no          7*/
      dimension parts(lpt)                                                      !/*no          8*/
      data bindng, pimas, pi0mas /7.d0,139.9d0,135.1d0/                         !/*no          9*/
!.......................................................................        !/*no         10*/
! no energy balancing                                                           !/*no         11*/
!.......................................................................        !/*no         12*/
! section for ISABEL                                                            !/*no         13*/
!.......................................................................        !/*no         14*/
      if (nnuc.eq.0) then                                                       !/*no         15*/
        apr=dp0                                                                 !/*no         16*/
        zpr=dp0                                                                 !/*no         17*/
        go to 30                                                                !/*no         18*/
      elseif (nnuc.gt.0) then                                                   !/*no         19*/
        apr=ian(nnuc)                                                           !/*no         20*/
        zpr=iqn(nnuc)                                                           !/*no         21*/
        ex=exn(nnuc)                                                            !/*no         22*/
        go to 30                                                                !/*no         23*/
      endif                                                                     !/*no         24*/
!.......................................................................        !/*no         25*/
! section for Bertini model                                                     !/*no         26*/
!.......................................................................        !/*no         27*/
      zpr=z1+charge(ityp)                                                       !/*no         28*/
      do 10 i=1,5                                                               !/*no         29*/
   10 parts(i)=dp0                                                              !/*no         30*/
      if (ityp.le.2) then                                                       !/*no         31*/
        apr=a1+dp1                                                              !/*no         32*/
        ex=ecl+bindng                                                           !/*no         33*/
      else                                                                      !/*no         34*/
        apr=a1                                                                  !/*no         35*/
        ex=ecl+pimas                                                            !/*no         36*/
      endif                                                                     !/*no         37*/
      if (nopart.eq.0) go to 30                                                 !/*no         38*/
      sume=dp0                                                                  !/*no         39*/
      do 20 i=1,nopart                                                          !/*no         40*/
      lk=kind(i)+1                                                              !/*no         41*/
      zpr=zpr-charge(lk)                                                        !/*no         42*/
      parts(lk)=parts(lk)+dp1                                                   !/*no         43*/
      sume=sume+ep(i)                                                           !/*no         44*/
   20 continue                                                                  !/*no         45*/
      chgpis=parts(3)+parts(5)                                                  !/*no         46*/
      fpt=parts(1)+parts(2)                                                     !/*no         47*/
      apr=apr-fpt                                                               !/*no         48*/
      ex=ex-fpt*bindng-sume-chgpis*pimas-parts(4)*pi0mas                        !/*no         49*/
!.......................................................................        !/*no         50*/
   30 continue                                                                  !/*no         51*/
      if (zpr.lt.dp0.or.apr.lt.zpr) then                                        !/*no         52*/
        negzpr=negzpr+1                                                         !/*no         53*/
        if (negzpr.gt.5) call expirx(1,'NOBAL','negzpr.gt.5')                   !/*no         54*/
        negaz=negaz+1                                                           !/*no         55*/
        nrj=1                                                                   !/*no         56*/
      elseif ((apr.eq.zpr.or.zpr.eq.dp0).and.apr.gt.dp3) then                   !/*no         57*/
        lowaz=lowaz+1                                                           !/*no         58*/
        nrj=1                                                                   !/*no         59*/
      else                                                                      !/*no         60*/
        if (apr.le.dp1) nrnorm(4)=nrnorm(4)+1                                   !/*no         61*/
!L    ==================================================================        !/*no         62*/
!L        if (lelem.eq.-1) call revnt (3)                                       !/*no         63*/
!L        if (lelem.ne.-1) call revnt (1)                                       !/*no         64*/
!L    ==================================================================        !/*no         65*/
        if (apr.gt.dp0) then                                                    !/*no         66*/
          waz=931.162d0*apr                                                     !/*no         67*/
          call recoil (ecl,waz,0)                                               !/*no         68*/
          if (nexite.ne.0) then                                                 !/*no         69*/
            ex=max(ex-erec,dp0)                                                 !/*no         70*/
          else                                                                  !/*no         71*/
            ex=max(ex,dp0)                                                      !/*no         72*/
          endif                                                                 !/*no         73*/
        else                                                                    !/*no         74*/
          ex=dp0                                                                !/*no         75*/
          erec=dp0                                                              !/*no         76*/
        endif                                                                   !/*no         77*/
      endif                                                                     !/*no         78*/
      return                                                                    !/*no         79*/
      end                                                                       !/*no         80*/
