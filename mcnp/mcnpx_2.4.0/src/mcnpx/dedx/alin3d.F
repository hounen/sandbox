!     $Id: alin3d.F,v 1.5 2002/03/27 18:14:32 gwm Exp $                       !/*al          1*/
      function alin3d (xo,yo,x,y,z,nx,ny,intflg)                                !/*al          2*/
#include <lcs/double.h>                                                         /*!al          3*/
!                                                                               !/*al          4*/
!     linear 3-dimensional interpolation.  interpolates a table, z(i,j),        !/*al          5*/
!     as a function of x(i) and y(j) at entry points, xo and yo.                !/*al          6*/
!     the surface is assumed to be defined by the following function -          !/*al          7*/
!                   z(x,y) = a + b*x + c*y + d*x*y                              !/*al          8*/
!     if intflg = 'old', the binary search section will be by-passed.           !/*al          9*/
!                                                                               !/*al         10*/
!     n o t e -   both x and y arrays must be in ascending order.               !/*al         11*/
!                                                                               !/*al         12*/
!     routine by harry m. murphy,  afwl/satr,  30 january 1976.                 !/*al         13*/
!                                                                               !/*al         14*/
      dimension x(nx), y(ny), z(nx,ny)                                          !/*al         15*/
      character*3 intflg                                                        !/*al         16*/
!                                                                               !/*al         17*/
!                   start.  check array dimensions.                             !/*al         18*/
      alin3d=0.0                                                                !/*al         19*/
      nerr=1                                                                    !/*al         20*/
      if (intflg.eq.'old') go to 50                                             !/*al         21*/
      if ((nx.lt.2).or.(ny.lt.2)) go to 60                                      !/*al         22*/
!                   search x-array for entry point.                             !/*al         23*/
      do 10 i=2,nx                                                              !/*al         24*/
      if (x(i).ge.xo) go to 20                                                  !/*al         25*/
   10 continue                                                                  !/*al         26*/
      i=nx                                                                      !/*al         27*/
   20 dx=x(i)-x(i-1)                                                            !/*al         28*/
!                   check for duplicate values in the x-array.                  !/*al         29*/
      nerr=2                                                                    !/*al         30*/
      if (dx.eq.0.0) go to 60                                                   !/*al         31*/
!                   search y-array for entry point.                             !/*al         32*/
      do 30 j=2,ny                                                              !/*al         33*/
      if (y(j).ge.yo) go to 40                                                  !/*al         34*/
   30 continue                                                                  !/*al         35*/
      j=ny                                                                      !/*al         36*/
   40 dy=y(j)-y(j-1)                                                            !/*al         37*/
!                   check for duplicate values in the y-array.                  !/*al         38*/
      nerr=3                                                                    !/*al         39*/
      if (dy.eq.0.0) go to 60                                                   !/*al         40*/
!                   ready to interpolate.  compute frx and fry.                 !/*al         41*/
      frx=(xo-x(i-1))/dx                                                        !/*al         42*/
      fry=(yo-y(j-1))/dy                                                        !/*al         43*/
!                   interpolate in z,y(j-1) plane.                              !/*al         44*/
   50 z1=z(i-1,j-1)+frx*(z(i,j-1)-z(i-1,j-1))                                   !/*al         45*/
!                   interpolate in z,y(j) plane.                                !/*al         46*/
      z2=z(i-1,j)+frx*(z(i,j)-z(i-1,j))                                         !/*al         47*/
!                   interpolate in z,xo plane.                                  !/*al         48*/
      alin3d=z1+fry*(z2-z1)                                                     !/*al         49*/
      go to 70                                                                  !/*al         50*/
!                   error message.                                              !/*al         51*/
   60 write (16,80) nerr,nx,ny,xo,yo                                            !/*al         52*/
!                   return.                                                     !/*al         53*/
   70 return                                                                    !/*al         54*/
!                                                                               !/*al         55*/
   80 format (1x/' function alin3d error.  error number ',i2,'.'/               !/*al         56*/
     & ' 1 - length of x- or y-arrays (or both) less than 2.'/                  !/*al         57*/
     & ' 2 - duplicate values in the x-array.'/                                 !/*al         58*/
     & ' 3 - duplicate values in the y-array.'/1x/                              !/*al         59*/
     &' nx =',i4,'    ny =',i4,'    xo =',1pe12.4,'    yo =',1pe12.4/1x)        !/*al         60*/
      end                                                                       !/*al         61*/
