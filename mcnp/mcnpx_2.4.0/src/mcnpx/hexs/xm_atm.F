!     $Id: xm_atm.F,v 1.4 2002/03/27 18:14:50 gwm Exp $                       !/*xm          1*/
      function xm_atm (a,z,c)                                                   !/*xm          2*/
#include <lcs/double.h>                                                         /*!xm          3*/
#include <lcs/phprm0.h>                                                         /*!xm          4*/
#include <lcs/wapcom.h>                                                         /*!xm          5*/
      character*1 c                                                             !/*xm          6*/
      ia=a                                                                      !/*xm          7*/
      iz=z                                                                      !/*xm          8*/
      j=iz-iz0(ia)+lmid+1                                                       !/*xm          9*/
      if (j.lt.1.or.j.gt.lwidth) then                                           !/*xm         10*/
        c=' '                                                                   !/*xm         11*/
        in=ia-iz                                                                !/*xm         12*/
        xm_atm=frdm(ia,iz)                                                      !/*xm         13*/
        if (iz.gt.0.and.in.gt.0) then                                           !/*xm         14*/
          if (iz.lt.izmin(ia)) then                                             !/*xm         15*/
            iz1=iz                                                              !/*xm         16*/
            ia1=iz1+maxn(iz)                                                    !/*xm         17*/
          elseif (iz.gt.izmax(ia)) then                                         !/*xm         18*/
            iz1=maxz(in)                                                        !/*xm         19*/
            ia1=iz1+in                                                          !/*xm         20*/
          else                                                                  !/*xm         21*/
            write (*,*) ' 2:ia,iz,in,j,maxn(iz),maxz(in) ',ia,iz,in,j           !/*xm         22*/
     &      ,maxn(iz),maxz(in)                                                  !/*xm         23*/
            stop                                                                !/*xm         24*/
          endif                                                                 !/*xm         25*/
          a1=ia1                                                                !/*xm         26*/
          z1=iz1                                                                !/*xm         27*/
          xm_atm=xm_atm-frdm(ia1,iz1)+ewaps(a1,z1,c,iwaps)                      !/*xm         28*/
        endif                                                                   !/*xm         29*/
      else                                                                      !/*xm         30*/
        xm_atm=wapz(j,ia)                                                       !/*xm         31*/
        c=cwaps(j,ia)                                                           !/*xm         32*/
      endif                                                                     !/*xm         33*/
      return                                                                    !/*xm         34*/
      end                                                                       !/*xm         35*/
