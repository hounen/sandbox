!     $Id: qsubb.F,v 1.4 2002/03/27 18:15:19 gwm Exp $                        !/*qs          1*/
      subroutine qsubb (qb,mode,a,z,ipr0,nh)                                    !/*qs          2*/
#include <lcs/double.h>                                                         /*!qs          3*/
      dimension qb(6), sum(6), binom2(6), ia7(6), iz7(6), in7(6)                !/*qs          4*/
      data ia7 /1,1,2,3,3,4/, iz7 /0,1,1,1,2,2/                                 !/*qs          5*/
      data in7 /1,0,1,2,1,2/, binom2 /dp1,dp1,dp2,dp3,dp3,6.d0/                 !/*qs          6*/
      if (mode.gt.0) go to 20                                                   !/*qs          7*/
      do 10 j=1,6                                                               !/*qs          8*/
   10 qb(j)=dp1                                                                 !/*qs          9*/
      go to 70                                                                  !/*qs         10*/
   20 continue                                                                  !/*qs         11*/
      p1=z/a                                                                    !/*qs         12*/
      p2=dp1-z/a                                                                !/*qs         13*/
      np=nh+ia7(ipr0)                                                           !/*qs         14*/
      do 30 i=1,6                                                               !/*qs         15*/
   30 sum(i)=dp0                                                                !/*qs         16*/
      sum0=dp0                                                                  !/*qs         17*/
      nup=nh+1                                                                  !/*qs         18*/
      do 50 n=1,nup                                                             !/*qs         19*/
      i=n-1                                                                     !/*qs         20*/
      fi=i                                                                      !/*qs         21*/
      if (mode.eq.1) then                                                       !/*qs         22*/
        wt=binom1(nh,i)*p1**i*p2**(nh-i)                                        !/*qs         23*/
      else                                                                      !/*qs         24*/
        wt=binom1(np,i+iz7(ipr0))*binom1(nh,i)                                  !/*qs         25*/
        ind=i+i+ia7(ipr0)                                                       !/*qs         26*/
        wt=wt*p1**ind*p2**(nh+nh-ind)                                           !/*qs         27*/
      endif                                                                     !/*qs         28*/
      sum0=sum0+wt                                                              !/*qs         29*/
      item=iz7(ipr0)+i                                                          !/*qs         30*/
      do 40 j=1,6                                                               !/*qs         31*/
      arg=binom1(item,iz7(j))*binom1(np-item,in7(j))                            !/*qs         32*/
      sum(j)=sum(j)+wt*arg                                                      !/*qs         33*/
   40 continue                                                                  !/*qs         34*/
   50 continue                                                                  !/*qs         35*/
      do 60 j=1,6                                                               !/*qs         36*/
      if (sum(j).gt.dp0) then                                                   !/*qs         37*/
        denom=binom1(np,ia7(j))*binom2(j)*sum0                                  !/*qs         38*/
        denom=p1**iz7(j)*p2**in7(j)*denom                                       !/*qs         39*/
        qb(j)=sum(j)/denom                                                      !/*qs         40*/
      else                                                                      !/*qs         41*/
        qb(j)=dp0                                                               !/*qs         42*/
      endif                                                                     !/*qs         43*/
   60 continue                                                                  !/*qs         44*/
   70 continue                                                                  !/*qs         45*/
      return                                                                    !/*qs         46*/
      end                                                                       !/*qs         47*/
