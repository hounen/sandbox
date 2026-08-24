!     $Id: amass.F,v 1.5 2002/03/27 18:14:53 gwm Exp $                        !/*am          1*/
      function amass (iz,ia,iok)                                                !/*am          2*/
#include <lcs/double.h>                                                         /*!am          3*/
!                                                                               !/*am          4*/
!  AMASS returns the total mass as the function value for arguements            !/*am          5*/
!  IZ and IA.  IOK is the index of the total mass in the AXM array and          !/*am          6*/
!  of the mass excess in the XM array. IOK is returned as 0 when the            !/*am          7*/
!  mass is not in the library (in which case the breakup routines do            !/*am          8*/
!  not apply to the mass.)                                                      !/*am          9*/
!                                                                               !/*am         10*/
!     REFERENCES.......  LEVBRK    CBREAK    FBRK                               !/*am         11*/
!                                                                               !/*am         12*/
#include <lcs/parms.h>                                                          /*!am         13*/
#include <lcs/fbrcom.h>                                                         /*!am         14*/
      iz1=iz+1                                                                  !/*am         15*/
      if (iz1.le.0.or.iz1.gt.11) go to 10                                       !/*am         16*/
      in1=ia-iz+1                                                               !/*am         17*/
      if (in1.le.0.or.in1.gt.13) go to 10                                       !/*am         18*/
      ipt=ip(iz1,in1)                                                           !/*am         19*/
      if (ipt.le.0.or.ipt.gt.lmas) go to 10                                     !/*am         20*/
      iok=ipt                                                                   !/*am         21*/
      amass=axm(ipt)                                                            !/*am         22*/
      return                                                                    !/*am         23*/
   10 iok=0                                                                     !/*am         24*/
      amass=dp0                                                                 !/*am         25*/
      return                                                                    !/*am         26*/
      end                                                                       !/*am         27*/
