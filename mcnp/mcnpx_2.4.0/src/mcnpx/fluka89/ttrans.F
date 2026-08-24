!     $Id: ttrans.F,v 1.4 2002/03/27 18:14:48 gwm Exp $                       !/*tt          1*/
      subroutine ttrans (xo,yo,zo,cde,sde,sfe,cfe,x,y,z)                        !/*tt          2*/
!                                                                     *         !/*tt          3*/
!---------------------------------------------------------------------*         !/*tt          4*/
!                                                                     *         !/*tt          5*/
#include <lcs/double.h>                                                         /*!tt          6*/
#include <fluka89/flprm1.h>                                                     /*!tt          7*/
#include <fluka89/flprm2.h>                                                     /*!tt          8*/
#include <lcs/inout.h>                                                          /*!tt          9*/
!********************************************************************           !/*tt         10*/
!     version by                     j. ranft                                   !/*tt         11*/
!                                    leipzig                                    !/*tt         12*/
!                                                                               !/*tt         13*/
!     this is a subroutine of fluka to give new direction cosines               !/*tt         14*/
!                                                                               !/*tt         15*/
!     input variables:                                                          !/*tt         16*/
!        xo,yo,zo = original direction cosines                                  !/*tt         17*/
!        cde,sde  = cosine and sine of the polar (theta)                        !/*tt         18*/
!                   angle of "scattering"                                       !/*tt         19*/
!        sde      = sine of the polar (theta) angle of "scattering"             !/*tt         20*/
!        sfe,cfe  = sine and cosine of the azimuthal (phi) angle                !/*tt         21*/
!                   of "scattering"                                             !/*tt         22*/
!                                                                               !/*tt         23*/
!     output variables:                                                         !/*tt         24*/
!        x,y,z     = new direction cosines                                      !/*tt         25*/
!                                                                               !/*tt         26*/
!     rotation of coordinate system (see cern 64-47 )                           !/*tt         27*/
!********************************************************************           !/*tt         28*/
!                                                                               !/*tt         29*/
!  changed by a. ferrari                                                        !/*tt         30*/
!                                                                               !/*tt         31*/
!     if (abs(xo)-0.0001d0) 1,1,2                                               !/*tt         32*/
!   1 if (abs(yo)-0.0001d0) 3,3,2                                               !/*tt         33*/
!   3 continue                                                                  !/*tt         34*/
      a=xo**2+yo**2                                                             !/*tt         35*/
      if (a.lt.anglgb) then                                                     !/*tt         36*/
        x=sde*cfe                                                               !/*tt         37*/
        y=sde*sfe                                                               !/*tt         38*/
!     z=cde corrected august 88 pa                                              !/*tt         39*/
        z=cde*zo                                                                !/*tt         40*/
      else                                                                      !/*tt         41*/
        xi=sde*cfe                                                              !/*tt         42*/
        yi=sde*sfe                                                              !/*tt         43*/
        zi=cde                                                                  !/*tt         44*/
        a=sqrt(a)                                                               !/*tt         45*/
        x=-yo*xi/a-zo*xo*yi/a+xo*zi                                             !/*tt         46*/
        y=xo*xi/a-zo*yo*yi/a+yo*zi                                              !/*tt         47*/
        z=a*yi+zo*zi                                                            !/*tt         48*/
      endif                                                                     !/*tt         49*/
      return                                                                    !/*tt         50*/
      end                                                                       !/*tt         51*/
