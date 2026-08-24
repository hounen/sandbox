!     $Id: recoil.F,v 1.4 2002/03/27 18:15:19 gwm Exp $                       !/*re          1*/
      subroutine recoil (ecl,aa,iflag)                                          !/*re          2*/
#include <lcs/double.h>                                                         /*!re          3*/
! for nobalc=0, aa is total energy of recoil nucleus:                           !/*re          4*/
!     mass + excitation + recoil kinetic energy                                 !/*re          5*/
#include <lcs/inout.h>                                                          /*!re          6*/
#include <lcs/lptS.h>                                                           /*!re          7*/
#include <lcs/rucom.h>                                                          /*!re          8*/
#include <lcs/cacom.h>                                                          /*!re          9*/
#include <lcs/banka.h>                                                          /*!re         10*/
      if (apr.le.dp0) call expirx(1,'RECOIL','apr.le.dp0')                      !/*re         11*/
      px=dp0                                                                    !/*re         12*/
      py=dp0                                                                    !/*re         13*/
      pz=dp0                                                                    !/*re         14*/
      if (nopart.eq.0) go to 20                                                 !/*re         15*/
      do 10 i=1,nopart                                                          !/*re         16*/
      kd=kind(i)+1                                                              !/*re         17*/
      tm=pmass(kd)                                                              !/*re         18*/
      if (ep(i).le.dp0) then                                                    !/*re         19*/
        write (io,*) ' nocas,i,kind(i),kd,pmass(kd),ep(i) ',nocas,i,kind        !/*re         20*/
     &  (i),kd,pmass(kd),ep(i)                                                  !/*re         21*/
        write (itty,*) ' nocas,i,kind(i),kd,pmass(kd),ep(i) ',nocas,i           !/*re         22*/
     &  ,kind(i),kd,pmass(kd),ep(i)                                             !/*re         23*/
        ep(i)=dp0                                                               !/*re         24*/
      endif                                                                     !/*re         25*/
      pi=ep(i)*sqrt(dp1+dp2*tm/ep(i))                                           !/*re         26*/
      px=pi*alpha(i)+px                                                         !/*re         27*/
      py=pi*beta(i)+py                                                          !/*re         28*/
   10 pz=pi*gam(i)+pz                                                           !/*re         29*/
   20 continue                                                                  !/*re         30*/
      tm=pmass(ityp)                                                            !/*re         31*/
!     pz=ecl*sqrt(dp1+dp2*tm/ecl)-pz                                            !/*re         32*/
      pz=sqrt(ecl*(ecl+dp2*tm))-pz                                              !/*re         33*/
      pi=px*px+py*py+pz*pz                                                      !/*re         34*/
      if (iflag.eq.0) then                                                      !/*re         35*/
!        AA = MASS + EXCITATION                                                 !/*re         36*/
        erec=sqrt(aa*aa+pi)-aa                                                  !/*re         37*/
      else                                                                      !/*re         38*/
!        AA = MASS + EXCITATION + RECOIL                                        !/*re         39*/
        temp=aa*aa-pi                                                           !/*re         40*/
        if (temp.lt.dp0) then                                                   !/*re         41*/
          erec=-1.0e10                                                          !/*re         42*/
          return                                                                !/*re         43*/
        endif                                                                   !/*re         44*/
        erec=pi/(aa+sqrt(temp))                                                 !/*re         45*/
      endif                                                                     !/*re         46*/
      if (pi.gt.0) then                                                         !/*re         47*/
        pi=sqrt(pi)                                                             !/*re         48*/
        coslbr(1)=-px/pi                                                        !/*re         49*/
        coslbr(2)=-py/pi                                                        !/*re         50*/
        coslbr(3)=pz/pi                                                         !/*re         51*/
      else                                                                      !/*re         52*/
        coslbr(1)=dp0                                                           !/*re         53*/
        coslbr(2)=dp0                                                           !/*re         54*/
        coslbr(3)=dp1                                                           !/*re         55*/
      endif                                                                     !/*re         56*/
      return                                                                    !/*re         57*/
      end                                                                       !/*re         58*/
