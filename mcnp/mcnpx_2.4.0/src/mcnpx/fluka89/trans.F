!     $Id: trans.F,v 1.4 2002/03/27 18:14:48 gwm Exp $                        !/*tr          1*/
      subroutine trans (xo,yo,zo,de,sfe,cfe,x,y,z)                              !/*tr          2*/
!                                                                     *         !/*tr          3*/
!---------------------------------------------------------------------*         !/*tr          4*/
!                                                                     *         !/*tr          5*/
#include <lcs/double.h>                                                         /*!tr          6*/
#include <fluka89/flprm1.h>                                                     /*!tr          7*/
#include <fluka89/flprm2.h>                                                     /*!tr          8*/
#include <lcs/inout.h>                                                          /*!tr          9*/
!********************************************************************           !/*tr         10*/
!     version by                     j. ranft                                   !/*tr         11*/
!                                    leipzig                                    !/*tr         12*/
!     last change 12. april 83    by pertti aarnio                              !/*tr         13*/
!                                    helsinki university of                     !/*tr         14*/
!                                    technology, finland                        !/*tr         15*/
!                                                                               !/*tr         16*/
!     this is a subroutine of fluka to give new direction cosines               !/*tr         17*/
!                                                                               !/*tr         18*/
!     input variables:                                                          !/*tr         19*/
!        xo,yo,zo = original direction cosines                                  !/*tr         20*/
!        de       = polar (theta) angle of "scattering"                         !/*tr         21*/
!        sfe,cfe  = sine and cosine of the azimuthal (phi) angle                !/*tr         22*/
!                   of "scattering"                                             !/*tr         23*/
!                                                                               !/*tr         24*/
!     output variables:                                                         !/*tr         25*/
!        x,y,z     = new direction cosines                                      !/*tr         26*/
!                                                                               !/*tr         27*/
!     rotation of coordinate system (see cern 64-47)                            !/*tr         28*/
!                                                                               !/*tr         29*/
!     due to the implementation of the multiple coulomb scattering              !/*tr         30*/
!     also very small angles have to be treated correctly.                      !/*tr         31*/
!     now small angle approximation is used practically only                    !/*tr         32*/
!     when xo and yo are exactly 0.                                             !/*tr         33*/
!                                                                               !/*tr         34*/
!********************************************************************           !/*tr         35*/
!                                                                               !/*tr         36*/
!  changed by a. ferrari 4-9-89                                                 !/*tr         37*/
!                                                                               !/*tr         38*/
!     if (de.gt.0.3d0)go to 20                                                  !/*tr         39*/
!     if (de.gt.1.d-8)go to 20                                                  !/*tr         40*/
!     dede=de*de                                                                !/*tr         41*/
!     cde=1.d0-dede*0.5d0*(1.d0-dede/12.d0)                                     !/*tr         42*/
!     sde=de*(1.d0-(dede/6.d0)*(1.d0-dede/20.d0))                               !/*tr         43*/
!     go to 30                                                                  !/*tr         44*/
      cde=cos(de)                                                               !/*tr         45*/
      sde=sin(de)                                                               !/*tr         46*/
      a=xo**2+yo**2                                                             !/*tr         47*/
      if (a.lt.anglgb) then                                                     !/*tr         48*/
        x=sde*cfe                                                               !/*tr         49*/
        y=sde*sfe                                                               !/*tr         50*/
!   z=cde corrected august 88 pa                                                !/*tr         51*/
        z=cde*zo                                                                !/*tr         52*/
      else                                                                      !/*tr         53*/
        xi=sde*cfe                                                              !/*tr         54*/
        yi=sde*sfe                                                              !/*tr         55*/
        zi=cde                                                                  !/*tr         56*/
        a=sqrt(a)                                                               !/*tr         57*/
        x=-yo*xi/a-zo*xo*yi/a+xo*zi                                             !/*tr         58*/
        y=xo*xi/a-zo*yo*yi/a+yo*zi                                              !/*tr         59*/
        z=a*yi+zo*zi                                                            !/*tr         60*/
      endif                                                                     !/*tr         61*/
      return                                                                    !/*tr         62*/
      end                                                                       !/*tr         63*/
