!     $Id: fisdis.F,v 1.4 2002/03/27 18:15:03 gwm Exp $                       !/*fi          1*/
      subroutine fisdis                                                         !/*fi          2*/
#include <lcs/double.h>                                                         /*!fi          3*/
#include <lcs/cacom.h>                                                          /*!fi          4*/
#include <lcs/ercom.h>                                                          /*!fi          5*/
      dimension g(3)                                                            !/*fi          6*/
!ccc                                                                            !/*fi          7*/
!     select angles in center of momentum system isotropically                  !/*fi          8*/
!ccc                                                                            !/*fi          9*/
      call gtiso (g(1),g(2),g(3))                                               !/*fi         10*/
      coscm=coslf0(1)*g(1)+coslf0(2)*g(2)+coslf0(3)*g(3)                        !/*fi         11*/
!ccc                                                                            !/*fi         12*/
!     compute fragment kinetic energies and velocities relativistically         !/*fi         13*/
!ccc                                                                            !/*fi         14*/
      psq=dp2*amdiff*amc1*amc2/amcf                                             !/*fi         15*/
      pscorr=(dp1-amdiff/(dp2*amcf))*(dp1+amdiff/(dp2*amc2))*(dp1+amdiff        !/*fi         16*/
     & /(dp2*amc1))                                                             !/*fi         17*/
      psq=psq*pscorr                                                            !/*fi         18*/
      gamma=dp1+ernff/amcf                                                      !/*fi         19*/
      bbb=sqrt(psq*(gamma-dp1)*(gamma+dp1))                                     !/*fi         20*/
      ecm1=(amcf**2+amc1**2-amc2**2)/(dp2*amcf)                                 !/*fi         21*/
      ecm2=(amcf**2-amc1**2+amc2**2)/(dp2*amcf)                                 !/*fi         22*/
      fiske(1)=gamma*ecm1+bbb*coscm-amc1                                        !/*fi         23*/
      fiske(2)=gamma*ecm2-bbb*coscm-amc2                                        !/*fi         24*/
      aa0=psq**2*(dp1-coscm**2)                                                 !/*fi         25*/
      dn1=sqrt(aa0+(bbb*ecm1+gamma*psq*coscm)**2)                               !/*fi         26*/
      dn2=sqrt(aa0+(bbb*ecm2-gamma*psq*coscm)**2)                               !/*fi         27*/
      c1=bbb*ecm1+psq*(gamma-dp1)*coscm                                         !/*fi         28*/
      c2=bbb*ecm2-psq*(gamma-dp1)*coscm                                         !/*fi         29*/
      do 10 i=1,3                                                               !/*fi         30*/
      coslf1(i)=(c1*coslf0(i)+psq*g(i))/dn1                                     !/*fi         31*/
      coslf2(i)=(c2*coslbr(i)-psq*g(i))/dn2                                     !/*fi         32*/
   10 continue                                                                  !/*fi         33*/
      return                                                                    !/*fi         34*/
      end                                                                       !/*fi         35*/
