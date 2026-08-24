!     $Id: interp.F,v 1.5 2002/03/27 18:15:09 gwm Exp $                       !/*in          1*/
      subroutine interp (a1)                                                    !/*in          2*/
#include <lcs/double.h>                                                         /*!in          3*/
!     the purpose of this subroutine is to interpolate in epperson              !/*in          4*/
!     data tables for relative kinetic energy and total excitation energ        !/*in          5*/
!     scission time - excitation energy of fissioning nucleus                   !/*in          6*/
!     the latter are supplied by  r.g.alsmiller fits                            !/*in          7*/
!                                                                               !/*in          8*/
!     specifically, delef and ke are computed for a given ef                    !/*in          9*/
!                                                                               !/*in         10*/
#include <lcs/ercom.h>                                                          /*!in         11*/
#include <lcs/orcm.h>                                                           /*!in         12*/
!ccc                                                                            !/*in         13*/
!     if kemax.gt.6,change dimension s in exitt                                 !/*in         14*/
!ccc                                                                            !/*in         15*/
      a2=afnp-a1                                                                !/*in         16*/
      ia1=a1+dph                                                                !/*in         17*/
      iaf=afnp+dph                                                              !/*in         18*/
      ia2=iaf-ia1                                                               !/*in         19*/
      iahm=afnp/dp2                                                             !/*in         20*/
      iahm=iaf-iahm                                                             !/*in         21*/
      if (ia1.ge.iahm) then                                                     !/*in         22*/
        iatm=ia2                                                                !/*in         23*/
        ia2=ia1                                                                 !/*in         24*/
        ia1=iatm                                                                !/*in         25*/
      endif                                                                     !/*in         26*/
      iaa=ia2-100                                                               !/*in         27*/
      delef=dp0                                                                 !/*in         28*/
      j1=kemax+4                                                                !/*in         29*/
      j2=j1+kemax-2                                                             !/*in         30*/
      do 10 j=j1,j2                                                             !/*in         31*/
      if (ef.gt.ainfo(j)) go to 10                                              !/*in         32*/
      ke=j-kemax-3                                                              !/*in         33*/
      go to 20                                                                  !/*in         34*/
   10 continue                                                                  !/*in         35*/
      ke=kemax                                                                  !/*in         36*/
      delef=dp0                                                                 !/*in         37*/
      delkex=sigke(iaa,ke)                                                      !/*in         38*/
      epkinx=epkin(iaa,ke)                                                      !/*in         39*/
      e1maxp=delexc(iaa,ke,ismal)                                               !/*in         40*/
      ern=ainfo(kemax+3)                                                        !/*in         41*/
      go to 30                                                                  !/*in         42*/
   20 continue                                                                  !/*in         43*/
      delef=ainfo(j+1)-ainfo(j)                                                 !/*in         44*/
      delef=(ef-ainfo(j))/delef                                                 !/*in         45*/
      if (delef.lt.dp0) delef=dp0                                               !/*in         46*/
      delkex=(dp1-delef)*sigke(iaa,ke)+delef*sigke(iaa,ke+1)                    !/*in         47*/
      epkinx=(dp1-delef)*epkin(iaa,ke)+delef*epkin(iaa,ke+1)                    !/*in         48*/
      e1maxp=delexc(iaa,ke,ismal)*(dp1-delef)+delexc(iaa,ke+1,ismal)            !/*in         49*/
     & *delef                                                                   !/*in         50*/
!ccc                                                                            !/*in         51*/
!     put kinetic energies of neptunium(236,93) compound nucleus in ainf        !/*in         52*/
!     ainfo(kemax+3)                                                            !/*in         53*/
!ccc                                                                            !/*in         54*/
      ern=ainfo(ke+3)*(dp1-delef)+ainfo(ke+4)*delef                             !/*in         55*/
   30 continue                                                                  !/*in         56*/
      delken=delkex                                                             !/*in         57*/
      e1maxp=e1maxp+ef                                                          !/*in         58*/
      emaxn=e1maxp                                                              !/*in         59*/
      delke=scalke*delken                                                       !/*in         60*/
      ekinr4=epkinx-ern                                                         !/*in         61*/
      z2p=zprobf(afnp,zfnp,a2)                                                  !/*in         62*/
      z1p=zfnp-z2p                                                              !/*in         63*/
      c=(a1-z1p*dp2)/a1                                                         !/*in         64*/
      smala1=a1*(dp1+yzero*c*c)/bzero                                           !/*in         65*/
      c=(a2-z2p*dp2)/a2                                                         !/*in         66*/
      smala2=a2*(dp1+yzero*c*c)/bzero                                           !/*in         67*/
      smasum=smala1+smala2                                                      !/*in         68*/
      arg=smasum*emaxn                                                          !/*in         69*/
!ccc                                                                            !/*in         70*/
!     return delken and arg in common to qmaxx later                            !/*in         71*/
!ccc                                                                            !/*in         72*/
      return                                                                    !/*in         73*/
      end                                                                       !/*in         74*/
