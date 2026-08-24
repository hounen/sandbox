!     $Id: fissed.F,v 1.6 2002/03/27 18:15:04 gwm Exp $                       !/*fi          1*/
      subroutine fissed (ja,jz,u)                                               !/*fi          2*/
!                                                                               !/*fi          3*/
!      subroutine to pick post fission parameters for nucleus                   !/*fi          4*/
!      jz,ja excited to u and recoiling with erec.                              !/*fi          5*/
!                                                                               !/*fi          6*/
#include <lcs/double.h>                                                         /*!fi          7*/
#include <lcs/cacom.h>                                                          /*!fi          8*/
#include <lcs/ercom.h>                                                          /*!fi          9*/
#include <lcs/inout.h>                                                          /*!fi         10*/
      dimension evodba(4), evodbs(4)                                            !/*fi         11*/
!     following data for assymetric vs symetric picking                         !/*fi         12*/
      data evodba, evodbs /18.8d0,18.1d0,18.1d0,18.5d0,20.43d0,20.43d0,2        !/*fi         13*/
     & 0.43d0,20.712d0/                                                         !/*fi         14*/
      data sigmaa, y00, aamean /6.5d0,0.1176d0,140.d0/                          !/*fi         15*/
!     asym gauss width,1.0/level density parameter and high mass mean.          !/*fi         16*/
      data afact, zfact, enmass /8.3674d0,0.7826d0,939.5124d0/                  !/*fi         17*/
!     mass diff from 1 amu for neutron,diff p & n masses and n mass             !/*fi         18*/
      if (.not.fisinh) then                                                     !/*fi         19*/
!***********************************************************************        !/*fi         20*/
!                                                                               !/*fi         21*/
!  logic error fissed called with fisinh unset.                                 !/*fi         22*/
!                                                                               !/*fi         23*/
!***********************************************************************        !/*fi         24*/
        write (io,60) ja,jz,u,erec                                              !/*fi         25*/
        call expirx(1,'FISSED','fissed called with fisinh unset')               !/*fi         26*/
      endif                                                                     !/*fi         27*/
!***********************************************************************        !/*fi         28*/
!                                                                               !/*fi         29*/
!       pick the masses                                                         !/*fi         30*/
!                                                                               !/*fi         31*/
!***********************************************************************        !/*fi         32*/
      z=real(jz)                                                                !/*fi         33*/
      a=real(ja)                                                                !/*fi         34*/
      nck2=0                                                                    !/*fi         35*/
   10 continue                                                                  !/*fi         36*/
      nck2=nck2+1                                                               !/*fi         37*/
      if (nck2.gt.10) then                                                      !/*fi         38*/
!***********************************************************************        !/*fi         39*/
!                                                                               !/*fi         40*/
!     fission failure                                                           !/*fi         41*/
!                                                                               !/*fi         42*/
!***********************************************************************        !/*fi         43*/
        write (io,50) ja,jz,u,erec,zfis(1),afis(1),zfis(2),afis(2)              !/*fi         44*/
        fisinh=.false.                                                          !/*fi         45*/
        return                                                                  !/*fi         46*/
      endif                                                                     !/*fi         47*/
      temp=z*z/a                                                                !/*fi         48*/
      if (temp.le.35.d0) then                                                   !/*fi         49*/
        if (jz.le.88) go to 20                                                  !/*fi         50*/
      elseif (u.le.62.d0) then                                                  !/*fi         51*/
!***********************************************************************        !/*fi         52*/
!                                                                               !/*fi         53*/
!   high z fission mass distribution. competition for sym vs assym              !/*fi         54*/
!  simple symmetric to assymetric data fit                                      !/*fi         55*/
!                                                                               !/*fi         56*/
!***********************************************************************        !/*fi         57*/
        arg=-0.36d0*u                                                           !/*fi         58*/
        arg=4.87d+03*exp(arg)                                                   !/*fi         59*/
        proba=arg/(dp1+arg)                                                     !/*fi         60*/
        if (rang().le.proba) then                                               !/*fi         61*/
!***********************************************************************        !/*fi         62*/
!                                                                               !/*fi         63*/
!      assymetric fission                                                       !/*fi         64*/
!                                                                               !/*fi         65*/
!***********************************************************************        !/*fi         66*/
          a1=gaussn(aamean,sigmaa)                                              !/*fi         67*/
          go to 30                                                              !/*fi         68*/
        endif                                                                   !/*fi         69*/
      endif                                                                     !/*fi         70*/
!***********************************************************************        !/*fi         71*/
!                                                                               !/*fi         72*/
!     find assymetric barrier for width computation                             !/*fi         73*/
!     assymetric barrier from seaborg and vandenbosch                           !/*fi         74*/
!     phys.rev. 88,507 (1952) phys.rev. 110,507 (1958)                          !/*fi         75*/
!     find if ee eo oe or oo nucleus                                            !/*fi         76*/
!     na=1 odd-odd,2 even-odd,3 odd-even,4 even-even                            !/*fi         77*/
!                                                                               !/*fi         78*/
!***********************************************************************        !/*fi         79*/
      jn=ja-jz                                                                  !/*fi         80*/
      na=1                                                                      !/*fi         81*/
      if (jz.eq.2*(jz/2)) na=na+1                                               !/*fi         82*/
      if (jn.eq.2*(jn/2)) na=na+2                                               !/*fi         83*/
      temp=z*z/a                                                                !/*fi         84*/
      eff=evodba(na)-0.36d0*temp                                                !/*fi         85*/
   20 continue                                                                  !/*fi         86*/
      upr=u-eff                                                                 !/*fi         87*/
      if (upr.gt.1.d+02) upr=1.d+02                                             !/*fi         88*/
      sigmas=0.425d0*(upr*(dp1-0.005d0*upr)+9.35d0)                             !/*fi         89*/
!***********************************************************************        !/*fi         90*/
!                                                                               !/*fi         91*/
!     sigmas is symmetrin fission mass width.taken from systematic of           !/*fi         92*/
!     neuzil & fairhall phys.rev. 129,2705,(1963)                               !/*fi         93*/
!                                                                               !/*fi         94*/
!      low z fission is always symmetric                                        !/*fi         95*/
!                                                                               !/*fi         96*/
!      high z fission is sometimes.loop back here from 1 loop if                !/*fi         97*/
!      symmetric fission predicted.                                             !/*fi         98*/
!                                                                               !/*fi         99*/
!***********************************************************************        !/*fi        100*/
      amean=dph*a                                                               !/*fi        101*/
      a1=gaussn(amean,sigmas)                                                   !/*fi        102*/
   30 continue                                                                  !/*fi        103*/
!***********************************************************************        !/*fi        104*/
!                                                                               !/*fi        105*/
!      1 loop for assymmetrin fission returns to here.                          !/*fi        106*/
!                                                                               !/*fi        107*/
!***********************************************************************        !/*fi        108*/
      afis(1)=aint(a1)                                                          !/*fi        109*/
!***********************************************************************        !/*fi        110*/
!                                                                               !/*fi        111*/
!       check for low final a                                                   !/*fi        112*/
!                                                                               !/*fi        113*/
!***********************************************************************        !/*fi        114*/
      if (afis(1).lt.dp5) afis(1)=dp5                                           !/*fi        115*/
      if (a-afis(1).lt.dp5) afis(1)=a-dp5                                       !/*fi        116*/
      afis(2)=a-afis(1)                                                         !/*fi        117*/
!***********************************************************************        !/*fi        118*/
!                                                                               !/*fi        119*/
!        pick the charge                                                        !/*fi        120*/
!                                                                               !/*fi        121*/
!***********************************************************************        !/*fi        122*/
      z1=65.5d0*afis(1)/(131.d0+afis(1)**0.666667d0)                            !/*fi        123*/
      z2=65.5d0*afis(2)/(131.d0+afis(2)**0.666667d0)                            !/*fi        124*/
      z1=z1+dph*(z-z1-z2)                                                       !/*fi        125*/
!***********************************************************************        !/*fi        126*/
!                                                                               !/*fi        127*/
!       we use constant charge density with a 2 unit gaussian smearing          !/*fi        128*/
!                                                                               !/*fi        129*/
!***********************************************************************        !/*fi        130*/
      z1=gaussn(z1,dp2)                                                         !/*fi        131*/
      zfis(1)=aint(z1)                                                          !/*fi        132*/
      zfis(2)=z-zfis(1)                                                         !/*fi        133*/
!***********************************************************************        !/*fi        134*/
!                                                                               !/*fi        135*/
!   check for reasonable z a combinations.                                      !/*fi        136*/
!                                                                               !/*fi        137*/
!***********************************************************************        !/*fi        138*/
      if (zfis(1).ge.afis(1)) go to 10                                          !/*fi        139*/
      if (zfis(2).ge.afis(2)) go to 10                                          !/*fi        140*/
      if (zfis(1).lt.dp1) go to 10                                              !/*fi        141*/
      if (zfis(2).lt.dp1) go to 10                                              !/*fi        142*/
!***********************************************************************        !/*fi        143*/
!                                                                               !/*fi        144*/
!      compute binding energy and actual masses of fragments                    !/*fi        145*/
!                                                                               !/*fi        146*/
!***********************************************************************        !/*fi        147*/
      be0=afact*a-zfact*z-energy(a,z)                                           !/*fi        148*/
      rm0=enmass*a-zfact*z-be0                                                  !/*fi        149*/
      be1=afact*afis(1)-zfact*zfis(1)-energy(afis(1),zfis(1))                   !/*fi        150*/
      rm1=enmass*afis(1)-zfact*zfis(1)-be1                                      !/*fi        151*/
      be2=afact*afis(2)-zfact*zfis(2)-energy(afis(2),zfis(2))                   !/*fi        152*/
      rm2=enmass*afis(2)-zfact*zfis(2)-be2                                      !/*fi        153*/
!***********************************************************************        !/*fi        154*/
!                                                                               !/*fi        155*/
!      pick recoil kinetic energy.use systematic of ...........                 !/*fi        156*/
!      unik et.al. proc.3rd iaea symp.on phy.& chem. fision,rochester.vo        !/*fi        157*/
!                                                                               !/*fi        158*/
!***********************************************************************        !/*fi        159*/
      totkm=0.13323d0*z*z/a**0.33333333d0-11.4d0                                !/*fi        160*/
!***********************************************************************        !/*fi        161*/
!                                                                               !/*fi        162*/
!      use a width of 15% value at half height.                                 !/*fi        163*/
!                                                                               !/*fi        164*/
!***********************************************************************        !/*fi        165*/
      sigmak=0.084d0*totkm                                                      !/*fi        166*/
!***********************************************************************        !/*fi        167*/
!                                                                               !/*fi        168*/
!   check event is energetically possible                                       !/*fi        169*/
!                                                                               !/*fi        170*/
!***********************************************************************        !/*fi        171*/
      temp2=u+be1+be2-be0                                                       !/*fi        172*/
      nck=0                                                                     !/*fi        173*/
   40 continue                                                                  !/*fi        174*/
      totke=gaussn(totkm,sigmak)                                                !/*fi        175*/
      if (nck.gt.10) go to 10                                                   !/*fi        176*/
      nck=nck+1                                                                 !/*fi        177*/
      if (totke.gt.temp2) go to 40                                              !/*fi        178*/
!***********************************************************************        !/*fi        179*/
!                                                                               !/*fi        180*/
!      pick excitation from equidistribution of original plus energy bal        !/*fi        181*/
!                                                                               !/*fi        182*/
!***********************************************************************        !/*fi        183*/
      temp=(temp2-totke)/a                                                      !/*fi        184*/
      fisu(1)=afis(1)*temp                                                      !/*fi        185*/
      fisu(2)=afis(2)*temp                                                      !/*fi        186*/
!ccc                                                                            !/*fi        187*/
!     find total masses, including excitation energies, at evap time            !/*fi        188*/
!ccc                                                                            !/*fi        189*/
      amcf=rm0+u                                                                !/*fi        190*/
      amc1=rm1+fisu(1)                                                          !/*fi        191*/
      amc2=rm2+fisu(2)                                                          !/*fi        192*/
      amdiff=amcf-amc1-amc2                                                     !/*fi        193*/
      if (amdiff.lt.dp0) go to 40                                               !/*fi        194*/
!***********************************************************************        !/*fi        195*/
!                                                                               !/*fi        196*/
!     amdiff= ekin should be satisfied                                          !/*fi        197*/
!                                                                               !/*fi        198*/
!***********************************************************************        !/*fi        199*/
      ernff=erec                                                                !/*fi        200*/
      call fisdis                                                               !/*fi        201*/
      return                                                                    !/*fi        202*/
!                                                                               !/*fi        203*/
   50 format ('---> fission failed: ja=',i5,'  jz=',i5/'-       u=',            !/*fi        204*/
     & 1pe10.3,'      erec=',e10.3/'    zfis1=',e10.3,'     afis1=',            !/*fi        205*/
     & e10.3/'    zfis2=',e10.3,'     afis2=',e10.3)                            !/*fi        206*/
   60 format (//'  logic error in fissed.called with fisinh flag',              !/*fi        207*/
     & ' unset.',2i10,2f10.5)                                                   !/*fi        208*/
      end                                                                       !/*fi        209*/
