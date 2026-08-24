!     $Id: levbrk.F,v 1.4 2002/03/27 18:15:10 gwm Exp $                       !/*le          1*/
      subroutine levbrk (ipt,llev,n,kip,ilev,t)                                 !/*le          2*/
#include <lcs/double.h>                                                         /*!le          3*/
!///////////////////////////////////////////////////////////////////////        !/*le          4*/
!  FBRLEV selects a breakup channel for a specific level.                       !/*le          5*/
!///////////////////////////////////////////////////////////////////////        !/*le          6*/
#include <lcs/parms.h>                                                          /*!le          7*/
#include <lcs/fbrcom.h>                                                         /*!le          8*/
      dimension kip(nmx), ilev(nmx)                                             !/*le          9*/
      l=il0(jns(ipt)+llev)                                                      !/*le         10*/
      ki=nklev(jns(ipt)+llev)                                                   !/*le         11*/
      if (ki.le.0) then                                                         !/*le         12*/
        n=1                                                                     !/*le         13*/
        kip(1)=ipt                                                              !/*le         14*/
        ilev(1)=-1                                                              !/*le         15*/
        t=dp0                                                                   !/*le         16*/
        return                                                                  !/*le         17*/
      endif                                                                     !/*le         18*/
      nlo=l+1                                                                   !/*le         19*/
      nup=nlo+ki-1                                                              !/*le         20*/
      wtot=wf(nup)                                                              !/*le         21*/
      fran=wtot*rang()                                                          !/*le         22*/
      do 10 kk=nlo,nup                                                          !/*le         23*/
      if (wf(kk).gt.fran) go to 20                                              !/*le         24*/
   10 continue                                                                  !/*le         25*/
      kk=nup                                                                    !/*le         26*/
!                                                                               !/*le         27*/
!       decay channel selected                                                  !/*le         28*/
!                                                                               !/*le         29*/
   20 continue                                                                  !/*le         30*/
      t=q1(kk)                                                                  !/*le         31*/
      n=n1(kk)                                                                  !/*le         32*/
      do 30 j=1,n                                                               !/*le         33*/
      kip(j)=izap1(j,kk)                                                        !/*le         34*/
      ilev(j)=1                                                                 !/*le         35*/
   30 continue                                                                  !/*le         36*/
      jjix=klev1(kk)                                                            !/*le         37*/
      ix=mod(jjix,3)+1                                                          !/*le         38*/
      ilev(ix)=jjix/3+1                                                         !/*le         39*/
      return                                                                    !/*le         40*/
      end                                                                       !/*le         41*/
