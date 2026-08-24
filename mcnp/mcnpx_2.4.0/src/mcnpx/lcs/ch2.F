!     $Id: ch2.F,v 1.4 2002/03/27 18:14:57 gwm Exp $                          !/*ch          1*/
      subroutine ch2 (l)                                                        !/*ch          2*/
#include <lcs/double.h>                                                         /*!ch          3*/
!                                                                               !/*ch          4*/
!  CH2 determines all particle combinations allowed for 2-body breakup          !/*ch          5*/
!  subject to the condition that the first particle must be one                 !/*ch          6*/
!  of the first LPART in the list IALL.                                         !/*ch          7*/
!                                                                               !/*ch          8*/
!     REFERENCES.......  LEVBRK    CBREAK                                       !/*ch          9*/
!                                                                               !/*ch         10*/
#include <lcs/parms.h>                                                          /*!ch         11*/
#include <lcs/fbrcom.h>                                                         /*!ch         12*/
!  allowed light particle breakup products                                      !/*ch         13*/
      dimension iall(10)                                                        !/*ch         14*/
      data iall /1,2,3,4,5,6,7,8,9,10/                                          !/*ch         15*/
!               n,H1,H2,H3,He3,H4,He4,Li4,He5,Li5                               !/*ch         16*/
      data n /2/, nm1 /1/, fn /dp2/                                             !/*ch         17*/
      i=0                                                                       !/*ch         18*/
      fizr=irz                                                                  !/*ch         19*/
      ztest=max(fizr,dp1)                                                       !/*ch         20*/
      fiar=ira                                                                  !/*ch         21*/
      do 40 j1=1,nrm                                                            !/*ch         22*/
      i1=iall(j1)                                                               !/*ch         23*/
      if (fn*a(i1).gt.fiar) go to 50                                            !/*ch         24*/
      if (z(i1).ge.ztest) go to 40                                              !/*ch         25*/
      fa(1)=a(i1)                                                               !/*ch         26*/
      fla(1)=fa(1)                                                              !/*ch         27*/
      fz(1)=z(i1)                                                               !/*ch         28*/
      flz(1)=fz(1)                                                              !/*ch         29*/
!                                                                               !/*ch         30*/
!       check to see if this is a valid breakup mode                            !/*ch         31*/
!                                                                               !/*ch         32*/
      fz(n)=fizr-flz(nm1)                                                       !/*ch         33*/
      fa(n)=fiar-fla(nm1)                                                       !/*ch         34*/
      do 20 i2=i1,nr                                                            !/*ch         35*/
      if (a(i2)-fa(n)) 20,10,40                                                 !/*ch         36*/
   10 continue                                                                  !/*ch         37*/
      if (z(i2).eq.fz(n)) go to 30                                              !/*ch         38*/
   20 continue                                                                  !/*ch         39*/
      go to 40                                                                  !/*ch         40*/
   30 i=i+1                                                                     !/*ch         41*/
      izap(1,i)=i1                                                              !/*ch         42*/
      izap(2,i)=i2                                                              !/*ch         43*/
   40 continue                                                                  !/*ch         44*/
   50 continue                                                                  !/*ch         45*/
      l=i                                                                       !/*ch         46*/
      return                                                                    !/*ch         47*/
      end                                                                       !/*ch         48*/
