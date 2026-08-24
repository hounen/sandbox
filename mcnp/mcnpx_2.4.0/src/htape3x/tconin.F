!     $Id: tconin.F,v 1.5 2002/03/27 18:14:03 gwm Exp $                       !/*tc          1*/
      subroutine tconin                                                         !/*tc          2*/
#include <htape3x/com32b.h>                                                     /*!tc          3*/
!------ time convolution input                                                  !/*tc          4*/
#include <htape3x/label.h>                                                      /*!tc          5*/
#include <htape3x/contrl.h>                                                     /*!tc          6*/
#include <htape3x/ctcon.h>                                                      /*!tc          7*/
      character *6 isub                                                         !/*tc          8*/
      data isub /'tconin'/                                                      !/*tc          9*/
      read (in,*) itopt,twit,tpeak,tsig                                         !/*tc         10*/
      if (itopt.eq.2.and.tsig.le.dp0) tsig=twit/4.0                             !/*tc         11*/
      write (io,50) itopt,twit,tpeak,tsig                                       !/*tc         12*/
      itopt2=itopt+2                                                            !/*tc         13*/
      if (itopt2.lt.1.or.itopt2.gt.6) call expire (0,isub,                      !/*tc         14*/
     & 'itopt not implemented')                                                 !/*tc         15*/
      if (itopt2.ne.5) then                                                     !/*tc         16*/
        aa=tpeak-0.5*twit                                                       !/*tc         17*/
        bb=tpeak+0.5*twit                                                       !/*tc         18*/
      else                                                                      !/*tc         19*/
        call tsrcin                                                             !/*tc         20*/
        aa=tsrc(1)                                                              !/*tc         21*/
        bb=tsrc(ntprb)                                                          !/*tc         22*/
      endif                                                                     !/*tc         23*/
      go to (40,10,20,30,40,20), itopt2                                         !/*tc         24*/
   10 if (twit.le.dp0) call expire (0,isub,'twit must be > 0')                  !/*tc         25*/
      twit1=1.0/twit                                                            !/*tc         26*/
      go to 40                                                                  !/*tc         27*/
   20 if (twit.le.dp0) call expire (0,isub,'twit must be > 0')                  !/*tc         28*/
      twit1=2.0/twit                                                            !/*tc         29*/
      go to 40                                                                  !/*tc         30*/
   30 if (twit.le.dp0) call expire (0,isub,'twit must be > 0')                  !/*tc         31*/
      twit1=1.0/(sqrt(2.0)*tsig)                                                !/*tc         32*/
      twit2=0.5/erf_rep(0.5*twit1*twit)                                         !/*tc         33*/
   40 return                                                                    !/*tc         34*/
!                                                                               !/*tc         35*/
   50 format (1h0,'time convolution:'/5x,'itopt =',i3/5x,'twit =',1pd13.        !/*tc         36*/
     & 5/5x,'tpeak =',1pd13.5/5x,'tsig =',1pd13.5)                              !/*tc         37*/
      end                                                                       !/*tc         38*/
