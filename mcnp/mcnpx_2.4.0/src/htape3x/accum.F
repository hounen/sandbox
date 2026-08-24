!     $Id: accum.F,v 1.4 2002/03/27 18:13:56 gwm Exp $                        !/*ac          1*/
      subroutine accum (hold,nbin,ipt,jb,wt)                                    !/*ac          2*/
#include <htape3x/com32b.h>                                                     /*!ac          3*/
      real wt                                                                   !/*ac          4*/
      dimension hold(8,nbin)                                                    !/*ac          5*/
      ib=jb                                                                     !/*ac          6*/
!------ ipt: 1=h1; 2=h2; 3=h3; 4=he3; 5=he4                                     !/*ac          7*/
      do 10 i=1,2                                                               !/*ac          8*/
      if (ib.eq.0) go to 10                                                     !/*ac          9*/
      hold(ipt,ib)=hold(ipt,ib)+wt                                              !/*ac         10*/
      if (ipt.le.3) hold(6,ib)=hold(6,ib)+wt                                    !/*ac         11*/
      if (ipt.gt.3) hold(7,ib)=hold(7,ib)+wt                                    !/*ac         12*/
      hold(8,ib)=hold(8,ib)+wt                                                  !/*ac         13*/
   10 ib=nbin                                                                   !/*ac         14*/
      return                                                                    !/*ac         15*/
      end                                                                       !/*ac         16*/
