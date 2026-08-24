!     $Id: keave.F,v 1.4 2002/03/27 18:15:09 gwm Exp $                        !/*ke          1*/
      subroutine keave (a2np,a2,smasum)                                         !/*ke          2*/
#include <lcs/double.h>                                                         /*!ke          3*/
#include <lcs/ercom.h>                                                          /*!ke          4*/
#include <lcs/orcm.h>                                                           /*!ke          5*/
!ccc                                                                            !/*ke          6*/
!     double interpolate in ef and a for neptunium                              !/*ke          7*/
!ccc                                                                            !/*ke          8*/
      ia2=a2np+dph                                                              !/*ke          9*/
      ekinh=dp0                                                                 !/*ke         10*/
      k2=ke+1                                                                   !/*ke         11*/
      ekinl=epkin(ia2-100,ke)*(dp1-dela)+epkin(ia2-99,ke)*dela                  !/*ke         12*/
      if (ke.ne.kemax) ekinh=epkin(ia2-100,k2)*(dp1-dela)+epkin(ia2-99          !/*ke         13*/
     & ,k2)*dela                                                                !/*ke         14*/
      ekinr4=ekinl*(dp1-delef)+ekinh*delef-ern                                  !/*ke         15*/
      z2pnp=zprobf(afnp,zfnp,a2np)                                              !/*ke         16*/
      z1pnp=zfnp-z2pnp                                                          !/*ke         17*/
      a1np=afnp-a2np                                                            !/*ke         18*/
      c=(a1np-z1pnp*dp2)/a1np                                                   !/*ke         19*/
      smala1=a1np*(dp1+yzero*c*c)/bzero                                         !/*ke         20*/
      c=(a2np-z2pnp*dp2)/a2np                                                   !/*ke         21*/
      smala2=a2np*(dp1+yzero*c*c)/bzero                                         !/*ke         22*/
      smasum=smala1+smala2                                                      !/*ke         23*/
      z1p=z1pnp                                                                 !/*ke         24*/
      z2p=z2pnp                                                                 !/*ke         25*/
      return                                                                    !/*ke         26*/
      end                                                                       !/*ke         27*/
