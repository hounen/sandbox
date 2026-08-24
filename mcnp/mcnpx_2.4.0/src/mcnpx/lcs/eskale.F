!     $Id: eskale.F,v 1.4 2002/03/27 18:15:02 gwm Exp $                       !/*es          1*/
      subroutine eskale (ie,ehicut,fin,nofas,itslo,efas,alpfas,betfas           !/*es          2*/
     & ,gamfas,wtfas,rmfas,exfas,refas,noslo,eslo,alpslo,betslo,gamslo)         !/*es          3*/
! ESKALE       calls...  MCMOSC  BERT  $WFI  $WFA$  $WFF                        !/*es          4*/
! ESKALE                 called by...   SKALE                                   !/*es          5*/
#include <lcs/double.h>                                                         /*!es          6*/
#include <lcs/btcom.h>                                                          /*!es          7*/
      dimension fin(8), itslo(1), efas(1), alpfas(1), betfas(1), gamfas(        !/*es          8*/
     & 1), wtfas(1), eslo(1), alpslo(1), betslo(1), gamslo(1)                   !/*es          9*/
#include <lcs/l05S.h>                                                           /*!es         10*/
      dimension itslo2(l05), efas2(l05), wtfas2(l05), alpfa2(l05),              !/*es         11*/
     & betfa2(l05), gamfa2(l05)                                                 !/*es         12*/
#include <lcs/inout.h>                                                          /*!es         13*/
      data f /.95d0/                                                            !/*es         14*/
      data w /.2d0/                                                             !/*es         15*/
      ind=1                                                                     !/*es         16*/
      e1nc=fin(3)                                                               !/*es         17*/
      fin(3)=ehicut                                                             !/*es         18*/
      feinc=ehicut*f                                                            !/*es         19*/
      prt1n=fin(7)+dp1                                                          !/*es         20*/
!**                                                                             !/*es         21*/
!**        nep = no. of escaping particles                                      !/*es         22*/
!**                                                                             !/*es         23*/
   10 nep=int(esps(1)+1.0d-2)                                                   !/*es         24*/
!                                                                               !/*es         25*/
!**      check for pions                                                        !/*es         26*/
!                                                                               !/*es         27*/
      j=2                                                                       !/*es         28*/
      do 20 i=1,nep                                                             !/*es         29*/
      if (esps(j).ge.dp3) go to 50                                              !/*es         30*/
      j=j+8                                                                     !/*es         31*/
   20 continue                                                                  !/*es         32*/
      j=2                                                                       !/*es         33*/
      do 40 i=1,nep                                                             !/*es         34*/
!                                                                               !/*es         35*/
!**      check for escaping particle with x and y  =  incident x and y          !/*es         36*/
!                                                                               !/*es         37*/
      if (esps(j+5).ne.xinc.or.esps(j+6).ne.yinc) go to 30                      !/*es         38*/
!                                                                               !/*es         39*/
      if (esps(j).ne.prt1n) go to 30                                            !/*es         40*/
!                                                                               !/*es         41*/
!**      check for energy of escaping particle greater than f * energy          !/*es         42*/
!**      of incident particle                                                   !/*es         43*/
!                                                                               !/*es         44*/
      if (esps(j+1).gt.feinc) go to 60                                          !/*es         45*/
   30 j=j+8                                                                     !/*es         46*/
   40 continue                                                                  !/*es         47*/
!                                                                               !/*es         48*/
!**      a 'non-elastic' collision has occurred                                 !/*es         49*/
!                                                                               !/*es         50*/
   50 if (ind-1) 140,140,100                                                    !/*es         51*/
!                                                                               !/*es         52*/
!**      an 'elastic' collision has occurred                                    !/*es         53*/
!                                                                               !/*es         54*/
   60 continue                                                                  !/*es         55*/
      if (ind.gt.1) go to 90                                                    !/*es         56*/
      atar=fin(1)                                                               !/*es         57*/
      itinc=fin(7)                                                              !/*es         58*/
      call mcmosc (atar,itinc,e1nc,ehicut,noslo,itslo,eslo,alpslo,betslo        !/*es         59*/
     & ,gamslo,efas,wtfas,alpfas,betfas,gamfas,rmfas,nofas,exfas,refas          !/*es         60*/
     & ,why)                                                                    !/*es         61*/
      nofas1=nofas                                                              !/*es         62*/
      if (nofas1.gt.0) go to 70                                                 !/*es         63*/
      ie=2                                                                      !/*es         64*/
      go to 150                                                                 !/*es         65*/
!                                                                               !/*es         66*/
!**      multiply all particle weights for this collision by w                  !/*es         67*/
!                                                                               !/*es         68*/
   70 do 80 i=1,nofas1                                                          !/*es         69*/
      wtfas(i)=wtfas(i)*w                                                       !/*es         70*/
   80 continue                                                                  !/*es         71*/
   90 continue                                                                  !/*es         72*/
      call bert (fin,noslo,itslo2,eslo,alpslo,betslo,gamslo)                    !/*es         73*/
      if (noslo.le.0) go to 90                                                  !/*es         74*/
      ind=2                                                                     !/*es         75*/
      go to 10                                                                  !/*es         76*/
  100 call mcmosc (atar,itinc,e1nc,ehicut,noslo,itslo2,eslo,alpslo              !/*es         77*/
     & ,betslo,gamslo,efas2,wtfas2,alpfa2,betfa2,gamfa2,rmfas2,nofas2           !/*es         78*/
     & ,exfas2,refas2,why)                                                      !/*es         79*/
      if (nofas2.le.0) go to 90                                                 !/*es         80*/
!                                                                               !/*es         81*/
!**      multiply all particle weights by 1.-w                                  !/*es         82*/
      w2=dp1-w                                                                  !/*es         83*/
      do 110 i=1,nofas2                                                         !/*es         84*/
      wtfas2(i)=wtfas2(i)*w2                                                    !/*es         85*/
  110 continue                                                                  !/*es         86*/
!                                                                               !/*es         87*/
!**      combine two collisions                                                 !/*es         88*/
!                                                                               !/*es         89*/
      if (nofas1+nofas2.le.60) go to 120                                        !/*es         90*/
      write (io,160) nofas1,nofas2                                              !/*es         91*/
      if (nofas1.eq.60) nofas1=59                                               !/*es         92*/
      nofas2=60-nofas1                                                          !/*es         93*/
  120 nofas=nofas1+nofas2                                                       !/*es         94*/
      do 130 i=1,nofas2                                                         !/*es         95*/
      j=nofas1+i                                                                !/*es         96*/
      itslo(j)=itslo2(i)                                                        !/*es         97*/
      efas(j)=efas2(i)                                                          !/*es         98*/
      wtfas(j)=wtfas2(i)                                                        !/*es         99*/
      alpfas(j)=alpfa2(i)                                                       !/*es        100*/
      betfas(j)=betfa2(i)                                                       !/*es        101*/
      gamfas(j)=gamfa2(i)                                                       !/*es        102*/
  130 continue                                                                  !/*es        103*/
      exfas=exfas*w+exfas2*(dp1-w)                                              !/*es        104*/
      rmfas=rmfas*w+rmfas2*(dp1-w)                                              !/*es        105*/
      refas=refas*w+refas2*(dp1-w)                                              !/*es        106*/
      ie=3                                                                      !/*es        107*/
      go to 150                                                                 !/*es        108*/
  140 ie=1                                                                      !/*es        109*/
  150 fin(3)=e1nc                                                               !/*es        110*/
      return                                                                    !/*es        111*/
!                                                                               !/*es        112*/
  160 format ('1',10x,'dimension s exceeded in escale',8x,'nofas1 = ',i2        !/*es        113*/
     & ,6x,'nofas2 = ',i2)                                                      !/*es        114*/
      end                                                                       !/*es        115*/
