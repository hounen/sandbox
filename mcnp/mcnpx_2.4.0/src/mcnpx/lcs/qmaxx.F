!     $Id: qmaxx.F,v 1.4 2002/03/27 18:15:18 gwm Exp $                        !/*qm          1*/
      subroutine qmaxx (a,qa,q)                                                 !/*qm          2*/
#include <lcs/double.h>                                                         /*!qm          3*/
#include <lcs/ercom.h>                                                          /*!qm          4*/
#include <lcs/orcm.h>                                                           /*!qm          5*/
#include <lcs/inout.h>                                                          /*!qm          6*/
!ccc                                                                            !/*qm          7*/
!     omegao = sqrt(dp4*ef*af/bz0)                                              !/*qm          8*/
!ccc                                                                            !/*qm          9*/
      delke=delken                                                              !/*qm         10*/
      qa=dp0                                                                    !/*qm         11*/
      q=dp1                                                                     !/*qm         12*/
      a2=a                                                                      !/*qm         13*/
!ccc                                                                            !/*qm         14*/
!     a1=af-a2                                                                  !/*qm         15*/
!ccc                                                                            !/*qm         16*/
      a1=afnp-a2                                                                !/*qm         17*/
      qnorm=dp1                                                                 !/*qm         18*/
      fact=sqrt(arg)                                                            !/*qm         19*/
      smasum=smala1+smala2                                                      !/*qm         20*/
      if (iversn.ne.2) then                                                     !/*qm         21*/
        fn=2.375d0                                                              !/*qm         22*/
        delke=sqrt(z1p*z2p/smasum)                                              !/*qm         23*/
        delke=delke*sqrt(fact)                                                  !/*qm         24*/
        q=q/((a1+a2)*(a1**1.66666667d0+a2**1.66666667d0)**1.5d0)                !/*qm         25*/
      else                                                                      !/*qm         26*/
        fn=3.625d0                                                              !/*qm         27*/
!ccc                                                                            !/*qm         28*/
!        this change was put in to give new  relativistic wke                   !/*qm         29*/
!ccc                                                                            !/*qm         30*/
        fn=fm                                                                   !/*qm         31*/
        q=q*sqrt(ekinr4*arg)/smasum                                             !/*qm         32*/
        q=q/(a1+a2)                                                             !/*qm         33*/
      endif                                                                     !/*qm         34*/
      q3=exp(dp2*fact-omegao)                                                   !/*qm         35*/
      crit=dp1-fn/fact                                                          !/*qm         36*/
      if (crit.le.dp0) then                                                     !/*qm         37*/
        write (io,10) crit,fn,fact,iversn,arg,qnorm,delke                       !/*qm         38*/
        return                                                                  !/*qm         39*/
      endif                                                                     !/*qm         40*/
      q2=crit*arg**2.5d0                                                        !/*qm         41*/
      q=q/sqrt(baz*(a1+a2))                                                     !/*qm         42*/
      q5=qnorm*(a1*a2)**4*sqrt(smala1*smala2)/smasum**3                         !/*qm         43*/
      q=q*q5                                                                    !/*qm         44*/
      q=q/smasum**2.5d0                                                         !/*qm         45*/
      q5=q*q2*q3                                                                !/*qm         46*/
      qa=q5*delke                                                               !/*qm         47*/
      if (qa.le.dp0) write (io,20) qa,q5,delke,q,q2,q3,qnorm,baz,a1,a2          !/*qm         48*/
     & ,smala1,smala2,arg,crit,fact,fn,omegao,ekinr4,delken,z1p,z2p,af          !/*qm         49*/
     & ,zf,ef,afnp,zfnp,yz0,bz0,e1maxp                                          !/*qm         50*/
      return                                                                    !/*qm         51*/
!                                                                               !/*qm         52*/
   10 format (1h ,'crit=',1pe15.6,2x,1p2e15.6,2x,i10,3e15.6)                    !/*qm         53*/
   20 format (1h1,'qmaxx has qamax le. 0.0',2x//(1x,1p8e15.6,2x))               !/*qm         54*/
      end                                                                       !/*qm         55*/
