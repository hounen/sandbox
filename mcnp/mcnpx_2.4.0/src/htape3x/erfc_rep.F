!     $Id: erfc_rep.F,v 1.4 2002/03/27 18:13:58 gwm Exp $                     !/*er          1*/
      function erfc_rep (x)                                                     !/*er          2*/
#include <htape3x/com32b.h>                                                     /*!er          3*/
!***library=slatec                                                              !/*er          4*/
!***author  fullerton, w., (lanl)                                               !/*er          5*/
!***purpose  computes the complementary error function (ERFC).                  !/*er          6*/
!***routines called  csevl                                                      !/*er          7*/
!***end prologue  ERFC                                                          !/*er          8*/
      dimension erfcs(13), erfccs(24), erc2cs(23)                               !/*er          9*/
      data erfcs(1) /-.049046121234691808d0/                                    !/*er         10*/
      data erfcs(2) /-.14226120510371364d0/                                     !/*er         11*/
      data erfcs(3) /.010035582187599796d0/                                     !/*er         12*/
      data erfcs(4) /-.000576876469976748d0/                                    !/*er         13*/
      data erfcs(5) /.000027419931252196d0/                                     !/*er         14*/
      data erfcs(6) /-.000001104317550734d0/                                    !/*er         15*/
      data erfcs(7) /.000000038488755420d0/                                     !/*er         16*/
      data erfcs(8) /-.000000001180858253d0/                                    !/*er         17*/
      data erfcs(9) /.000000000032334215d0/                                     !/*er         18*/
      data erfcs(10) /-.000000000000799101d0/                                   !/*er         19*/
      data erfcs(11) /.000000000000017990d0/                                    !/*er         20*/
      data erfcs(12) /-.000000000000000371d0/                                   !/*er         21*/
      data erfcs(13) /.000000000000000007d0/                                    !/*er         22*/
      data erc2cs(1) /-.069601346602309501d0/                                   !/*er         23*/
      data erc2cs(2) /-.041101339362620893d0/                                   !/*er         24*/
      data erc2cs(3) /.003914495866689626d0/                                    !/*er         25*/
      data erc2cs(4) /-.000490639565054897d0/                                   !/*er         26*/
      data erc2cs(5) /.000071574790013770d0/                                    !/*er         27*/
      data erc2cs(6) /-.000011530716341312d0/                                   !/*er         28*/
      data erc2cs(7) /.000001994670590201d0/                                    !/*er         29*/
      data erc2cs(8) /-.000000364266647159d0/                                   !/*er         30*/
      data erc2cs(9) /.000000069443726100d0/                                    !/*er         31*/
      data erc2cs(10) /-.000000013712209021d0/                                  !/*er         32*/
      data erc2cs(11) /.000000002788389661d0/                                   !/*er         33*/
      data erc2cs(12) /-.000000000581416472d0/                                  !/*er         34*/
      data erc2cs(13) /.000000000123892049d0/                                   !/*er         35*/
      data erc2cs(14) /-.000000000026906391d0/                                  !/*er         36*/
      data erc2cs(15) /.000000000005942614d0/                                   !/*er         37*/
      data erc2cs(16) /-.000000000001332386d0/                                  !/*er         38*/
      data erc2cs(17) /.000000000000302804d0/                                   !/*er         39*/
      data erc2cs(18) /-.000000000000069666d0/                                  !/*er         40*/
      data erc2cs(19) /.000000000000016208d0/                                   !/*er         41*/
      data erc2cs(20) /-.000000000000003809d0/                                  !/*er         42*/
      data erc2cs(21) /.000000000000000904d0/                                   !/*er         43*/
      data erc2cs(22) /-.000000000000000216d0/                                  !/*er         44*/
      data erc2cs(23) /.000000000000000052d0/                                   !/*er         45*/
      data erfccs(1) /0.0715179310202925d0/                                     !/*er         46*/
      data erfccs(2) /-.026532434337606719d0/                                   !/*er         47*/
      data erfccs(3) /.001711153977920853d0/                                    !/*er         48*/
      data erfccs(4) /-.000163751663458512d0/                                   !/*er         49*/
      data erfccs(5) /.000019871293500549d0/                                    !/*er         50*/
      data erfccs(6) /-.000002843712412769d0/                                   !/*er         51*/
      data erfccs(7) /.000000460616130901d0/                                    !/*er         52*/
      data erfccs(8) /-.000000082277530261d0/                                   !/*er         53*/
      data erfccs(9) /.000000015921418724d0/                                    !/*er         54*/
      data erfccs(10) /-.000000003295071356d0/                                  !/*er         55*/
      data erfccs(11) /.000000000722343973d0/                                   !/*er         56*/
      data erfccs(12) /-.000000000166485584d0/                                  !/*er         57*/
      data erfccs(13) /.000000000040103931d0/                                   !/*er         58*/
      data erfccs(14) /-.000000000010048164d0/                                  !/*er         59*/
      data erfccs(15) /.000000000002608272d0/                                   !/*er         60*/
      data erfccs(16) /-.000000000000699105d0/                                  !/*er         61*/
      data erfccs(17) /.000000000000192946d0/                                   !/*er         62*/
      data erfccs(18) /-.000000000000054704d0/                                  !/*er         63*/
      data erfccs(19) /.000000000000015901d0/                                   !/*er         64*/
      data erfccs(20) /-.000000000000004729d0/                                  !/*er         65*/
      data erfccs(21) /.000000000000001432d0/                                   !/*er         66*/
      data erfccs(22) /-.000000000000000439d0/                                  !/*er         67*/
      data erfccs(23) /.000000000000000138d0/                                   !/*er         68*/
      data erfccs(24) /-.000000000000000048d0/                                  !/*er         69*/
      data sqrtpi /1.7724538509055160d0/                                        !/*er         70*/
      data nterf, nterfc, nterc2 /11,2*21/                                      !/*er         71*/
      data xsml /-.56573450083402577d+01/                                       !/*er         72*/
      data xmax /.9d+01/                                                        !/*er         73*/
      data sqeps /.11920928955078125d-06/                                       !/*er         74*/
!                                                                               !/*er         75*/
      if (x.gt.xsml) go to 10                                                   !/*er         76*/
!                                                                               !/*er         77*/
! ERFC(x) = 1.0 - ERF(x) for x .lt. xsml                                        !/*er         78*/
!                                                                               !/*er         79*/
      erfc_rep=dp2                                                              !/*er         80*/
      return                                                                    !/*er         81*/
!                                                                               !/*er         82*/
   10 if (x.gt.xmax) go to 30                                                   !/*er         83*/
      y=abs(x)                                                                  !/*er         84*/
      if (y.gt.dp1) go to 20                                                    !/*er         85*/
!                                                                               !/*er         86*/
! ERFC(x) = 1.0 - ERF(x) for -1.0 .le. x .le. 1.0                               !/*er         87*/
!                                                                               !/*er         88*/
      if(y.lt.sqeps)erfc_rep=dp1-dp2*x/sqrtpi                                   !/*er         89*/
      if(y.ge.sqeps)erfc_rep=dp1-x*(dp1+csevl(dp2*x*x-dp1,erfcs,nterf))         !/*er         90*/
      return                                                                    !/*er         91*/
!                                                                               !/*er         92*/
! ERFC(x) = 1.0 - ERF(x) for 1.0 .lt. abs(x) .le. xmax                          !/*er         93*/
!                                                                               !/*er         94*/
   20 y=y*y                                                                     !/*er         95*/
      if (y.le.dp4) erfc_rep=exp(-y)/abs(x)*(dph+csevl((8.d0/y-dp5)/dp3         !/*er         96*/
     & ,erc2cs,nterc2))                                                         !/*er         97*/
      if (y.gt.dp4) erfc_rep=exp(-y)/abs(x)*(dph+csevl(8.d0/y-dp1,erfccs        !/*er         98*/
     & ,nterfc))                                                                !/*er         99*/
      if (x.lt.dp0) erfc_rep=dp2-erfc_rep                                       !/*er        100*/
      return                                                                    !/*er        101*/
!                                                                               !/*er        102*/
   30 continue                                                                  !/*er        103*/
      erfc_rep=dp0                                                              !/*er        104*/
      return                                                                    !/*er        105*/
!                                                                               !/*er        106*/
      end                                                                       !/*er        107*/
