!     $Id: set_pcap.F,v 1.4 2002/03/27 18:15:23 gwm Exp $                     !/*se          1*/
      subroutine set_pcap (ecl,med,nel,mel,a9,pcap)                             !/*se          2*/
#include <lcs/double.h>                                                         /*!se          3*/
#include <lcs/lptS.h>                                                           /*!se          4*/
#include <lcs/rucom.h>                                                          /*!se          5*/
      dimension nel(1), mel(1), a9(1), pcap(1)                                  !/*se          6*/
!     ************************************************************              !/*se          7*/
!                                                                               !/*se          8*/
!*      pi- capture                                                             !/*se          9*/
!                                                                               !/*se         10*/
      mel1=mel(med)-1                                                           !/*se         11*/
      nel1=nel(med)                                                             !/*se         12*/
      r=rang()                                                                  !/*se         13*/
      do 10 l=1,nel1                                                            !/*se         14*/
      r=r-pcap(mel1+l)                                                          !/*se         15*/
      if (r.gt.dp0) go to 10                                                    !/*se         16*/
      lelem=l                                                                   !/*se         17*/
      iexp=l                                                                    !/*se         18*/
      go to 20                                                                  !/*se         19*/
   10 continue                                                                  !/*se         20*/
      call errorf ('set_pcap.2')                                                !/*se         21*/
   20 continue                                                                  !/*se         22*/
      a1=a9(mel1+iexp)                                                          !/*se         23*/
      if (a1.eq.dp1) then                                                       !/*se         24*/
        lelem=-1                                                                !/*se         25*/
        nopart=-1                                                               !/*se         26*/
      endif                                                                     !/*se         27*/
      return                                                                    !/*se         28*/
      end                                                                       !/*se         29*/
