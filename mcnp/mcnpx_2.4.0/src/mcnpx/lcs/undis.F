!     $Id: undis.F,v 1.4 2002/03/27 18:15:25 gwm Exp $                        !/*un          1*/
      subroutine undis                                                          !/*un          2*/
#include <lcs/double.h>                                                         /*!un          3*/
#include <lcs/btcom.h>                                                          /*!un          4*/
      if (begru) 90,110,80                                                      !/*un          5*/
   10 begru=dp1                                                                 !/*un          6*/
   20 continue                                                                  !/*un          7*/
      ran1=dp2*rang()-dp1                                                       !/*un          8*/
      ran2=dp2*rang()-dp1                                                       !/*un          9*/
      if (ran1**2+ran2**2.ge.dp1) go to 20                                      !/*un         10*/
      xi(1)=ran1*out(4)                                                         !/*un         11*/
      xi(2)=ran2*out(4)                                                         !/*un         12*/
      xinc=xi(1)                                                                !/*un         13*/
      yinc=xi(2)                                                                !/*un         14*/
      curr(4)=xi(1)                                                             !/*un         15*/
      curr(5)=xi(2)                                                             !/*un         16*/
      xi(3)=-out(4)                                                             !/*un         17*/
      dcos(1)=dp0                                                               !/*un         18*/
      dcos(2)=dp0                                                               !/*un         19*/
      dcos(3)=dp1                                                               !/*un         20*/
      med=4                                                                     !/*un         21*/
      curr(6)=xi(3)                                                             !/*un         22*/
      curr(7)=dp0                                                               !/*un         23*/
      curr(8)=dp0                                                               !/*un         24*/
      curr(9)=dp1                                                               !/*un         25*/
      curr(10)=med                                                              !/*un         26*/
!     x, y and z coordinates, also alpha, beta and gamma                        !/*un         27*/
!     direction cosines.  med=4, (no. of geom)                                  !/*un         28*/
   30 return                                                                    !/*un         29*/
   40 begru=begru+dp1                                                           !/*un         30*/
   50 begru=begru+dp1                                                           !/*un         31*/
      if (casesn-begru) 70,60,60                                                !/*un         32*/
   60 continue                                                                  !/*un         33*/
      frand=rang()                                                              !/*un         34*/
      go to 20                                                                  !/*un         35*/
   70 begru=dp0                                                                 !/*un         36*/
      go to 30                                                                  !/*un         37*/
!     final random in erand.  run completed                                     !/*un         38*/
   80 if (count(6).eq.dp0) go to 110                                            !/*un         39*/
   90 count(6)=dp0                                                              !/*un         40*/
  100 if (begru) 40,10,50                                                       !/*un         41*/
  110 do 120 i=1,12                                                             !/*un         42*/
      pcc(i)=cc(i)                                                              !/*un         43*/
  120 nip(i)=ipec(i)                                                            !/*un         44*/
      do 130 i=1,5                                                              !/*un         45*/
  130 ppnb(i)=pnbc(i)                                                           !/*un         46*/
      go to 100                                                                 !/*un         47*/
!*** when begru = 0, munpu common is zeroed                                     !/*un         48*/
      end                                                                       !/*un         49*/
