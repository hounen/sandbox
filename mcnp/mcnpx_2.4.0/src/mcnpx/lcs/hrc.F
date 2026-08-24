!     $Id: hrc.F,v 1.4 2002/03/27 18:15:06 gwm Exp $                          !/*hr          1*/
      subroutine hrc (ecl,aa,iflag,erek)                                        !/*hr          2*/
#include <lcs/double.h>                                                         /*!hr          3*/
#include <lcs/lptS.h>                                                           /*!hr          4*/
#include <lcs/rucom.h>                                                          /*!hr          5*/
#include <lcs/cacom.h>                                                          /*!hr          6*/
#include <lcs/banka.h>                                                          /*!hr          7*/
      if (apr.le.dp0) call expirx(1,'HRC','apr.le.dp0')                         !/*hr          8*/
      px=dp0                                                                    !/*hr          9*/
      py=dp0                                                                    !/*hr         10*/
      pz=dp0                                                                    !/*hr         11*/
      if (nopart.eq.0) go to 20                                                 !/*hr         12*/
      do 10 i=1,nopart                                                          !/*hr         13*/
      kd=kind(i)+1                                                              !/*hr         14*/
      tm=pmass(kd)                                                              !/*hr         15*/
      pi=ep(i)*sqrt(dp1+dp2*tm/ep(i))                                           !/*hr         16*/
      px=pi*alpha(i)*wtfas(i)+px                                                !/*hr         17*/
      py=pi*beta(i)*wtfas(i)+py                                                 !/*hr         18*/
   10 pz=pi*gam(i)*wtfas(i)+pz                                                  !/*hr         19*/
   20 continue                                                                  !/*hr         20*/
      tm=pmass(ityp)                                                            !/*hr         21*/
      pz=ecl*sqrt(dp1+dp2*tm/ecl)-pz                                            !/*hr         22*/
      pi=px*px+py*py+pz*pz                                                      !/*hr         23*/
      if (iflag.eq.0) then                                                      !/*hr         24*/
!        AA = MASS + EXCITATION                                                 !/*hr         25*/
        erek=sqrt(aa*aa+pi)-aa                                                  !/*hr         26*/
      else                                                                      !/*hr         27*/
!        AA = MASS + EXCITATION + RECOIL                                        !/*hr         28*/
        temp=aa*aa-pi                                                           !/*hr         29*/
        if (temp.lt.dp0) then                                                   !/*hr         30*/
          erek=-1.d10                                                           !/*hr         31*/
          return                                                                !/*hr         32*/
        endif                                                                   !/*hr         33*/
        erek=pi/(aa+sqrt(temp))                                                 !/*hr         34*/
      endif                                                                     !/*hr         35*/
      pi=sqrt(pi)                                                               !/*hr         36*/
      coslbr(1)=-px/pi                                                          !/*hr         37*/
      coslbr(2)=-py/pi                                                          !/*hr         38*/
      coslbr(3)=pz/pi                                                           !/*hr         39*/
      return                                                                    !/*hr         40*/
      end                                                                       !/*hr         41*/
