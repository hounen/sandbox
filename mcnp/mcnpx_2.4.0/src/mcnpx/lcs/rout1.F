!     $Id: rout1.F,v 1.4 2002/03/27 18:15:21 gwm Exp $                        !/*ro          1*/
      subroutine rout1                                                          !/*ro          2*/
#include <lcs/double.h>                                                         /*!ro          3*/
#include <lcs/btcom.h>                                                          /*!ro          4*/
      data parm /3.09366772628d10/                                              !/*ro          5*/
      out(11)=zee                                                               !/*ro          6*/
      value2=zee**6.6666667d-1                                                  !/*ro          7*/
      do 10 i=5,7                                                               !/*ro          8*/
      space(i+2)=out(i)*out(11)                                                 !/*ro          9*/
   10 space(i+5)=(out(i+3)*value2)+7.d0                                         !/*ro         10*/
!     scaled protons per cc and potential proton well depth                     !/*ro         11*/
!     (mev) in each region                                                      !/*ro         12*/
      out(12)=amasno-out(11)                                                    !/*ro         13*/
!     no. of neutrons n, stored                                                 !/*ro         14*/
      value2=out(12)**6.6666667d-1                                              !/*ro         15*/
!     n 2/3                                                                     !/*ro         16*/
      do 20 i=5,7                                                               !/*ro         17*/
      space(i-4)=out(i)*out(12)                                                 !/*ro         18*/
   20 space(i-1)=(out(i+3)*value2)+7.d0                                         !/*ro         19*/
!     scaled neuts. per cc and pot. neut. well depth (mev)                      !/*ro         20*/
!     in each region                                                            !/*ro         21*/
      do 30 i=1,3                                                               !/*ro         22*/
      hvn(i)=dph*space(i+3)                                                     !/*ro         23*/
      hvp(i)=dph*space(i+9)                                                     !/*ro         24*/
      awd(i)=hvn(i)+hvp(i)                                                      !/*ro         25*/
      fvnp(i)=dph*awd(i)                                                        !/*ro         26*/
      vnvp(i)=space(i+3)-space(i+9)                                             !/*ro         27*/
      pmac(i)=vnvp(i)-hvn(i)                                                    !/*ro         28*/
      ppan(i)=-vnvp(i)-hvp(i)                                                   !/*ro         29*/
      thpn(i)=hvp(i)-vnvp(i)                                                    !/*ro         30*/
      ffptfn(i)=-vnvp(i)+fvnp(i)                                                !/*ro         31*/
      tffn(i)=space(i+9)-fvnp(i)                                                !/*ro         32*/
   30 tffp(i)=vnvp(i)+tffn(i)                                                   !/*ro         33*/
      pppda=(dp2*zee)/(zee+amasno-dp1)                                          !/*ro         34*/
      ppmda=(dp2*out(12))/(amasno+out(12)-dp1)                                  !/*ro         35*/
      ppnda=(dp2*zee*out(12))/(amasno*amasno-amasno)                            !/*ro         36*/
      if (zee.eq.out(12)) go to 40                                              !/*ro         37*/
      ppnna=(out(12)*out(12)-out(12))/(out(12)*out(12)+zee*zee-amasno)          !/*ro         38*/
      go to 50                                                                  !/*ro         39*/
   40 continue                                                                  !/*ro         40*/
      ppnna=dph                                                                 !/*ro         41*/
   50 continue                                                                  !/*ro         42*/
!     pion absorption calc. for each reg.  1/2 nwd, (-pnan, -ppac)              !/*ro         43*/
!     1/2 pwd, (-pnap, -pnac) av. well depth, 1/4 av. well depth,               !/*ro         44*/
!     (n-p)well depth, (-vpvn), 1/2nwd -pwd, (pmap, -vphn), 1/2pwd -nwd,        !/*ro         45*/
!     (-vnhp), 3/2pwd -nwd, 5/4pwd -3/4nwd, 3/4pwd -1/4nwd,                     !/*ro         46*/
!     3/4nwd -1/4pwd, prob. pip deut abs, prob pim deut abs                     !/*ro         47*/
!     prob pin deut abs, prob pin nn abs rather than pp                         !/*ro         48*/
      k=15                                                                      !/*ro         49*/
      do 60 i=4,6                                                               !/*ro         50*/
      out(k)=space(i+6)+einc                                                    !/*ro         51*/
      out(k+3)=space(i)+einc                                                    !/*ro         52*/
   60 k=k-1                                                                     !/*ro         53*/
!     total k.e. in mev incident proton(neutron)particle in each region         !/*ro         54*/
      out(30)=(zee/out(4))*1.4412d-13                                           !/*ro         55*/
!     coulomb potential at surface in mev.  conversion                          !/*ro         56*/
!     factor=mev-cm per proton    (bg33)                                        !/*ro         57*/
      if (ctofe) 80,70,80                                                       !/*ro         58*/
   70 ctofe=out(30)                                                             !/*ro         59*/
!     if ctofe=0,then equate it to1/2 potential energy at surface               !/*ro         60*/
   80 do 90 i=1,3                                                               !/*ro         61*/
      cfepn(i+3)=space(i+3)+ctofen                                              !/*ro         62*/
   90 cfepn(i)=space(i+9)+ctofe                                                 !/*ro         63*/
!     bg33p--cutoff energies in each region for neutrons(protons)               !/*ro         64*/
      in1=0                                                                     !/*ro         65*/
      value1=parm                                                               !/*ro         66*/
!     calc. of fermi momenta per cm.  pf equ. 2pi*((3/8pi)to 1/3)*e10           !/*ro         67*/
      do 100 i=1,3                                                              !/*ro         68*/
      fmpn(i)=value1*(space(i+6))**3.3333333d-1                                 !/*ro         69*/
  100 fmpn(i+3)=value1*(space(i))**3.3333333d-1                                 !/*ro         70*/
!     fermi momenta per cm. of protons(neutrons)                                !/*ro         71*/
      return                                                                    !/*ro         72*/
      end                                                                       !/*ro         73*/
