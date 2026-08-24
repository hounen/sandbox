!     $Id: kinels.F,v 1.4 2002/03/27 18:15:10 gwm Exp $                       !/*ki          1*/
      subroutine kinels (csthcm,am1,am1sq,am2,einc,eout,aenrgy,xalp,xbet        !/*ki          2*/
     & ,xgam)                                                                   !/*ki          3*/
!--------------------------------------------------------                       !/*ki          4*/
! subname= kinels                       b.colborn, r.e.prael                    !/*ki          5*/
! KINELS calls...  SQRT  AZIRN                                                  !/*ki          6*/
! KINELS called by   ELSCAT   ELSC2                                             !/*ki          7*/
!--------------------------------------------------------                       !/*ki          8*/
#include <lcs/double.h>                                                         /*!ki          9*/
      e1=einc+am1                                                               !/*ki         10*/
      e2=am2                                                                    !/*ki         11*/
      wlab=e1+e2                                                                !/*ki         12*/
!  am1,e1 and am2,e2 are rest mass,total energy for part. 1(inc. neut.)         !/*ki         13*/
! and part. 2(struck nucleus) respectively,wlab is total energy in lab i        !/*ki         14*/
      p1=sqrt(e1*e1-am1sq)                                                      !/*ki         15*/
!     p1 is total momentum of part. 1 in lab, p2 = 0.                           !/*ki         16*/
      bta=p1/wlab                                                               !/*ki         17*/
      zta=sqrt(dp1-bta*bta)                                                     !/*ki         18*/
      gma=dp1/zta                                                               !/*ki         19*/
      wcm=zta*wlab                                                              !/*ki         20*/
!  the cm suffix denotes center of mass, part. 4 is secondary neut.             !/*ki         21*/
!  part. 3 is recoiling nucleus.                                                !/*ki         22*/
      e4cm=am2*gma                                                              !/*ki         23*/
!  this expression for e4cm is obtained from eqn. 29b,rev. mod. phys.,vo        !/*ki         24*/
!  ,no. 3,july 1962,page 433, kinematics of high energy particles,k. g.         !/*ki         25*/
      e3cm=wcm-e4cm                                                             !/*ki         26*/
      p3cm=sqrt(e3cm*e3cm-am1sq)                                                !/*ki         27*/
      pz3cm=p3cm*csthcm                                                         !/*ki         28*/
      pz3=gma*(pz3cm+bta*e3cm)                                                  !/*ki         29*/
      e3=gma*(e3cm+bta*pz3cm)                                                   !/*ki         30*/
      p3=sqrt(e3*e3-am1sq)                                                      !/*ki         31*/
      eout=e3-am1                                                               !/*ki         32*/
      csth=pz3/p3                                                               !/*ki         33*/
      e4=wlab-e3                                                                !/*ki         34*/
      aenrgy=e4-am2                                                             !/*ki         35*/
      if (aenrgy.lt.dp0) aenrgy=dp0                                             !/*ki         36*/
      snth=dp1-csth*csth                                                        !/*ki         37*/
      if (snth.ge.dp0) go to 10                                                 !/*ki         38*/
      csth=dp1                                                                  !/*ki         39*/
      snth=dp0                                                                  !/*ki         40*/
      go to 20                                                                  !/*ki         41*/
   10 snth=sqrt(snth)                                                           !/*ki         42*/
   20 call azirn (snph,csph)                                                    !/*ki         43*/
      xalp=snth*csph                                                            !/*ki         44*/
      xbet=snth*snph                                                            !/*ki         45*/
      xgam=csth                                                                 !/*ki         46*/
      return                                                                    !/*ki         47*/
      end                                                                       !/*ki         48*/
