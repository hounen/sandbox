!     $Id: alin.F,v 1.5 2002/03/27 18:14:32 gwm Exp $                         !/*al          1*/
      function alin (xv,x,y,nxy,intflg)                                         !/*al          2*/
#include <lcs/double.h>                                                         /*!al          3*/
!                                                                               !/*al          4*/
!     linear interpolation routine.  interpolates a table of y as a             !/*al          5*/
!     function of x at entry point xv.                                          !/*al          6*/
!     the x array may be in ascending or descending order, but it must          !/*al          7*/
!     be monotonic. if intflg = 'old', the routine will skip the binary         !/*al          8*/
!     search section.                                                           !/*al          9*/
!                                                                               !/*al         10*/
!        called from -                                                          !/*al         11*/
!             safer                                                             !/*al         12*/
!                                                                               !/*al         13*/
!     routine by harry m. murphy,  afwl/dyvm,  23 july 1978.                    !/*al         14*/
!     updated by henry j. happ iii, tetra corp, 7 october 1981 to               !/*al         15*/
!     allow for both ascending and descending order.                            !/*al         16*/
!     updated by hjh dec82 to add the intflg parameter.                         !/*al         17*/
!                                                                               !/*al         18*/
      dimension x(nxy), y(nxy)                                                  !/*al         19*/
!                                                                               !/*al         20*/
!     original code had:     integer i, l, m, nxy                               !/*al         21*/
      character*3 intflg                                                        !/*al         22*/
!                                                                               !/*al         23*/
!     original code had:     real delta, xo                                     !/*al         24*/
!                                                                               !/*al         25*/
      logical error                                                             !/*al         26*/
!                   start.  make xv and nxy local.                              !/*al         27*/
      alin=0.0                                                                  !/*al         28*/
      xo=xv                                                                     !/*al         29*/
      if (intflg.eq.'old') go to 50                                             !/*al         30*/
!                                                                               !/*al         31*/
      l=1                                                                       !/*al         32*/
      m=nxy                                                                     !/*al         33*/
      error=m.lt.2                                                              !/*al         34*/
      if (error) write (16,70)                                                  !/*al         35*/
      if (error) go to 60                                                       !/*al         36*/
!                   check for descending order.                                 !/*al         37*/
      if (x(1).gt.x(nxy)) go to 20                                              !/*al         38*/
!                   ascending order.                                            !/*al         39*/
!                   do binary search of array x for the entry point.            !/*al         40*/
      i=(l+m)/2                                                                 !/*al         41*/
   10 if (xo.gt.x(i)) l=i                                                       !/*al         42*/
      if (xo.le.x(i)) m=i                                                       !/*al         43*/
      i=(l+m)/2                                                                 !/*al         44*/
      if (l.lt.i) go to 10                                                      !/*al         45*/
      go to 40                                                                  !/*al         46*/
!                   descending order.                                           !/*al         47*/
!                   do binary search of array x for the entry point.            !/*al         48*/
   20 i=(l+m)/2                                                                 !/*al         49*/
   30 if (xo.lt.x(i)) l=i                                                       !/*al         50*/
      if (xo.ge.x(i)) m=i                                                       !/*al         51*/
      i=(l+m)/2                                                                 !/*al         52*/
      if (l.lt.i) go to 30                                                      !/*al         53*/
!                   we have the entry point.  calculate delta.                  !/*al         54*/
   40 delta=x(i+1)-x(i)                                                         !/*al         55*/
      error=delta.eq.0.0                                                        !/*al         56*/
      if (error) write (16,80) i                                                !/*al         57*/
      if (error) go to 60                                                       !/*al         58*/
!                   do linear interpolation.                                    !/*al         59*/
   50 alin=y(i)+(y(i+1)-y(i))*(xo-x(i))/delta                                   !/*al         60*/
!                   return.                                                     !/*al         61*/
   60 return                                                                    !/*al         62*/
!                                                                               !/*al         63*/
   70 format ('0function alin error.  table length less than 2.')               !/*al         64*/
   80 format ('0function alin error.  ',                                        !/*al         65*/
     & 'independent variable array is bi-valued at i =',i5,'.')                 !/*al         66*/
      end                                                                       !/*al         67*/
