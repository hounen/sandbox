!     $Id: ch3.F,v 1.4 2002/03/27 18:14:57 gwm Exp $                          !/*ch          1*/
      subroutine ch3 (l)                                                        !/*ch          2*/
#include <lcs/double.h>                                                         /*!ch          3*/
!                                                                               !/*ch          4*/
!     CH3 determines all particle combinations allowed for 3-body               !/*ch          5*/
!     breakup subject to the condition that the 1st and 2nd particle            !/*ch          6*/
!     must be one of the first LPART in the list IALL.                          !/*ch          7*/
!                                                                               !/*ch          8*/
!     REFERENCES.......  LEVBRK    CBREAK                                       !/*ch          9*/
!                                                                               !/*ch         10*/
#include <lcs/parms.h>                                                          /*!ch         11*/
#include <lcs/fbrcom.h>                                                         /*!ch         12*/
!  allowed light particle breakup products                                      !/*ch         13*/
      dimension iall(10)                                                        !/*ch         14*/
      data iall /1,2,3,4,5,6,7,8,9,10/                                          !/*ch         15*/
!               n,H1,H2,H3,He3,H4,He4,Li4,He5,Li5                               !/*ch         16*/
      data n /3/, nm1 /2/, fn /dp3/                                             !/*ch         17*/
      i=0                                                                       !/*ch         18*/
      fizr=irz                                                                  !/*ch         19*/
      ztest=max(fizr,dp1)                                                       !/*ch         20*/
      fiar=ira                                                                  !/*ch         21*/
      do 50 j1=1,nrm                                                            !/*ch         22*/
      i1=iall(j1)                                                               !/*ch         23*/
      if (fn*a(i1).gt.fiar) go to 60                                            !/*ch         24*/
      if (z(i1).ge.ztest) go to 50                                              !/*ch         25*/
      fa(1)=a(i1)                                                               !/*ch         26*/
      fla(1)=fa(1)                                                              !/*ch         27*/
      fz(1)=z(i1)                                                               !/*ch         28*/
      flz(1)=fz(1)                                                              !/*ch         29*/
      do 40 j2=j1,nrm                                                           !/*ch         30*/
      i2=iall(j2)                                                               !/*ch         31*/
      if (fla(1)+(fn-dp1)*a(i2).gt.fiar) go to 50                               !/*ch         32*/
      flz(2)=flz(1)+z(i2)                                                       !/*ch         33*/
      if (flz(2).ge.ztest) go to 40                                             !/*ch         34*/
      fa(2)=a(i2)                                                               !/*ch         35*/
      fla(2)=fla(1)+fa(2)                                                       !/*ch         36*/
      fz(2)=z(i2)                                                               !/*ch         37*/
!                                                                               !/*ch         38*/
!       check to see if this is a valid breakup mode                            !/*ch         39*/
!                                                                               !/*ch         40*/
      fz(n)=fizr-flz(nm1)                                                       !/*ch         41*/
      fa(n)=fiar-fla(nm1)                                                       !/*ch         42*/
      do 20 i3=i2,nr                                                            !/*ch         43*/
      if (a(i3)-fa(n)) 20,10,40                                                 !/*ch         44*/
   10 continue                                                                  !/*ch         45*/
      if (z(i3).eq.fz(n)) go to 30                                              !/*ch         46*/
   20 continue                                                                  !/*ch         47*/
      go to 40                                                                  !/*ch         48*/
   30 i=i+1                                                                     !/*ch         49*/
      izap(1,i)=i1                                                              !/*ch         50*/
      izap(2,i)=i2                                                              !/*ch         51*/
      izap(3,i)=i3                                                              !/*ch         52*/
   40 continue                                                                  !/*ch         53*/
   50 continue                                                                  !/*ch         54*/
   60 continue                                                                  !/*ch         55*/
      l=i                                                                       !/*ch         56*/
      return                                                                    !/*ch         57*/
      end                                                                       !/*ch         58*/
