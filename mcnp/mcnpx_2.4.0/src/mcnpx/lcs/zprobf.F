!     $Id: zprobf.F,v 1.5 2002/03/27 18:15:27 gwm Exp $                       !/*zp          1*/
      function zprobf (af9,zf9,a2)                                              !/*zp          2*/
#include <lcs/double.h>                                                         /*!zp          3*/
#include <lcs/orcm.h>                                                           /*!zp          4*/
      data cnver /931.143d0/                                                    !/*zp          5*/
!ccc                                                                            !/*zp          6*/
!     table lookup of fong dma(in mev) and dza for mass corrections             !/*zp          7*/
!ccc                                                                            !/*zp          8*/
      ro=rocc                                                                   !/*zp          9*/
      a1=af9-a2                                                                 !/*zp         10*/
      ia1=a1+dph                                                                !/*zp         11*/
      ia2=a2+dph                                                                !/*zp         12*/
      iaa=ia1                                                                   !/*zp         13*/
      k=iaa-59                                                                  !/*zp         14*/
      if (k.lt.1) return                                                        !/*zp         15*/
      kdel=ia2-iaa                                                              !/*zp         16*/
      k2=k+kdel                                                                 !/*zp         17*/
      if (k2.lt.1) return                                                       !/*zp         18*/
      baz=dp0                                                                   !/*zp         19*/
      c12=dp0                                                                   !/*zp         20*/
      c12s=dp1/(a1**0.333333333d0+a2**0.333333333d0)                            !/*zp         21*/
      c12s=c12s*1.44d0/ro                                                       !/*zp         22*/
      c12=c12s                                                                  !/*zp         23*/
!ccc                                                                            !/*zp         24*/
!     change ba and za                                                          !/*zp         25*/
!     from fong,wing,1964,p.151, fong book                                      !/*zp         26*/
!ccc                                                                            !/*zp         27*/
      za1=(a1+3.d-03*a1**2)/(dp2+1.d-02*a1)                                     !/*zp         28*/
      za2=(a2+3.d-03*a2**2)/(dp2+1.d-02*a2)                                     !/*zp         29*/
!ccc                                                                            !/*zp         30*/
!     ba below is already in mev                                                !/*zp         31*/
!ccc                                                                            !/*zp         32*/
      ba1=3.258d0-(60.22d0/a1**dph)+(431.6d0/a1)                                !/*zp         33*/
      ba2=3.258d0-(60.22d0/a2**dph)+(431.6d0/a2)                                !/*zp         34*/
      pac=11.51d0*(dp1/a1**dph+dp1/a2**dph)                                     !/*zp         35*/
      ama1=0.0089794d0*a1**2-2.20717d0*a1+33.448d0+a1*(931.481d0-cnver)         !/*zp         36*/
      ama2=0.0089794d0*a2**2-2.20717d0*a2+33.448d0+a2*(931.481d0-cnver)         !/*zp         37*/
      cz=-ba1*(zf9-za1)**2/dp2-ba2*za2**2/dp2                                   !/*zp         38*/
      cz=cz-pac-ama1-ama2                                                       !/*zp         39*/
      az=-(ba1+ba2)/dp2                                                         !/*zp         40*/
      bz=(ba1*(zf9-za1)+ba2*za2)                                                !/*zp         41*/
      itrmax=2                                                                  !/*zp         42*/
      do 10 itr=1,itrmax                                                        !/*zp         43*/
      z2p=(bz-c12*zf9)/(-dp2*(az+c12))                                          !/*zp         44*/
      z1p=zf9-z2p                                                               !/*zp         45*/
      baz=-az-c12                                                               !/*zp         46*/
      c12=ekinr4/(z1p*z2p)                                                      !/*zp         47*/
   10 continue                                                                  !/*zp         48*/
      zprobf=z2p                                                                !/*zp         49*/
      return                                                                    !/*zp         50*/
      end                                                                       !/*zp         51*/
