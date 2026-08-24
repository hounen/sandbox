!     $Id: nopart.F,v 1.4 2002/03/27 18:15:12 gwm Exp $                       !/*no          1*/
      function nopart (type,dens,pos,hole,nhole,gm)                             !/*no          2*/
#include <lcs/double.h>                                                         /*!no          3*/
!     ==================================================================        !/*no          4*/
!     checks whether there exists a nucleon in the target                       !/*no          5*/
!     at the suggested interaction spot according to different                  !/*no          6*/
!     prescriptions (frozen target picture).                                    !/*no          7*/
!     hole contains the coordinates of the nhole particles                      !/*no          8*/
!     hit until now.                                                            !/*no          9*/
!                                                                               !/*no         10*/
! NOPART called by AINTER  PSEL                                                 !/*no         11*/
!     ==================================================================        !/*no         12*/
!                                                                               !/*no         13*/
#include <lcs/vegas.h>                                                          /*!no         14*/
      dimension pos(15), hole(4,250), dd(3)                                     !/*no         15*/
!                                                                               !/*no         16*/
      nopart=0                                                                  !/*no         17*/
!                                                                               !/*no         18*/
!      nopart .eq. 0 ==> no restrictions occured                                !/*no         19*/
!             .gt. 1 ==>  a restriction was encountered                         !/*no         20*/
!                                                                               !/*no         21*/
      if (nhole.le.0) return                                                    !/*no         22*/
      th=.666667d0                                                              !/*no         23*/
      iknp=abs(knp)                                                             !/*no         24*/
      if (iknp-2) 10,20,30                                                      !/*no         25*/
!     the restriction is fixed gaplength (1.1 fm, default)                      !/*no         26*/
   10 rest=gapl**2                                                              !/*no         27*/
      go to 40                                                                  !/*no         28*/
!     the restriction is v*dens=1                                               !/*no         29*/
   20 rest=(0.2387324d0/dens)**th                                               !/*no         30*/
      go to 40                                                                  !/*no         31*/
!     the restriction is 1/cubert(dens)                                         !/*no         32*/
   30 rest=dp1/dens**th                                                         !/*no         33*/
   40 do 60 i=1,nhole                                                           !/*no         34*/
      if (knp.lt.0) go to 50                                                    !/*no         35*/
      if (type.ne.hole(4,i)) go to 60                                           !/*no         36*/
   50 dd(1)=pos(10)-hole(1,i)                                                   !/*no         37*/
      dd(2)=pos(11)-hole(2,i)                                                   !/*no         38*/
      dd(3)=(pos(12)-hole(3,i))/gm                                              !/*no         39*/
      d=dd(1)*dd(1)+dd(2)*dd(2)+dd(3)*dd(3)                                     !/*no         40*/
      if (d.lt.rest) go to 70                                                   !/*no         41*/
   60 continue                                                                  !/*no         42*/
      return                                                                    !/*no         43*/
   70 nopart=2                                                                  !/*no         44*/
      return                                                                    !/*no         45*/
      end                                                                       !/*no         46*/
