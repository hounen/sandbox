!     $Id: nudist.F,v 1.4 2002/03/27 18:14:46 gwm Exp $                       !/*nu          1*/
      integer function nudist (anu,kb,explam)                                   !/*nu          2*/
!                                                                     *         !/*nu          3*/
!---------------------------------------------------------------------*         !/*nu          4*/
!                                                                     *         !/*nu          5*/
#include <lcs/double.h>                                                         /*!nu          6*/
#include <fluka89/flprm1.h>                                                     /*!nu          7*/
#include <fluka89/flprm2.h>                                                     /*!nu          8*/
#include <lcs/inout.h>                                                          /*!nu          9*/
      dimension ab(13,9), anub(9), anum(9)                                      !/*nu         10*/
      data ab /1.d0,12*0.d0,0.5771d0,0.2565d0,0.1159d0,0.0397d0,                !/*nu         11*/
     & 0.0095d0,0.0013d0,0.0001d0,6*0.d0,0.5457d0,0.2548d0,0.1303d0,            !/*nu         12*/
     & 0.0515d0,0.0146d0,0.0028d0,0.0003d0,6*0.d0,0.4462d0,0.2445d0,            !/*nu         13*/
     & 0.1599d0,0.0919d0,0.0406d0,0.0132d0,0.0032d0,0.0005d0,0.0001d0,          !/*nu         14*/
     & 4*0.d0,0.3387d0,0.2105d0,0.1670d0,0.1291d0,0.0849d0,0.0441d0,            !/*nu         15*/
     & 0.0180d0,0.0059d0,0.0014d0,0.0003d0,0.0001d0,2*0.d0,0.2959d0,            !/*nu         16*/
     & 0.1885d0,0.1673d0,0.1344d0,0.1044d0,0.0653d0,0.0336d0,0.0144d0,          !/*nu         17*/
     & 0.0046d0,0.0012d0,0.0003d0,2*0.d0,0.2835d0,0.1818d0,0.1572d0,            !/*nu         18*/
     & 0.1357d0,0.1082d0,0.0715d0,0.0380d0,0.0161d0,0.0057d0,0.0018d0,          !/*nu         19*/
     & 0.0004d0,0.0001d0,0.d0,0.2247d0,0.1481d0,0.1342d0,0.1328d0,              !/*nu         20*/
     & 0.1211d0,0.0984d0,0.0700d0,0.0394d0,0.0198d0,0.0077d0,0.0028d0,          !/*nu         21*/
     & 0.0008d0,0.0002d0,13*0.d0/                                               !/*nu         22*/
      data anub /1.d0,1.65d0,1.74d0,2.09d0,2.66d0,3.0d0,3.1d0,3.75d0,           !/*nu         23*/
     & 8.d0/                                                                    !/*nu         24*/
      data anum /1.d0,1.45d0,1.52d0,1.77d0,2.19d0,2.42d0,2.47d0,2.90d0,         !/*nu         25*/
     & 5.d0/                                                                    !/*nu         26*/
      data init /0/                                                             !/*nu         27*/
      dimension aab(9), dist(13)                                                !/*nu         28*/
      if (init.eq.1) go to 50                                                   !/*nu         29*/
      ia=9                                                                      !/*nu         30*/
      ir=8                                                                      !/*nu         31*/
      do 10 in1=1,13                                                            !/*nu         32*/
      ain=in1*3.75d0/8.d0                                                       !/*nu         33*/
      nain=ain                                                                  !/*nu         34*/
      if (nain.lt.1) nain=1                                                     !/*nu         35*/
      nainp=nain+1                                                              !/*nu         36*/
      ab(in1,ia)=(ain-nain)*ab(nainp,ir)+(nainp-ain)*ab(nain,ir)                !/*nu         37*/
      if (nainp.gt.13) ab(in1,ia)=0.d0                                          !/*nu         38*/
      if (ab(in1,ia).lt.0.d0) ab(in1,ia)=0.d0                                   !/*nu         39*/
   10 continue                                                                  !/*nu         40*/
      do 40 ia=1,9                                                              !/*nu         41*/
      aab(ia)=0.d0                                                              !/*nu         42*/
      do 20 in1=1,13                                                            !/*nu         43*/
      aab(ia)=aab(ia)+ab(in1,ia)                                                !/*nu         44*/
   20 continue                                                                  !/*nu         45*/
      do 30 in1=2,13                                                            !/*nu         46*/
      ab(1,ia)=ab(1,ia)/aab(ia)                                                 !/*nu         47*/
      ab(in1,ia)=ab(in1,ia)/aab(ia)+ab(in1-1,ia)                                !/*nu         48*/
   30 continue                                                                  !/*nu         49*/
   40 continue                                                                  !/*nu         50*/
      init=1                                                                    !/*nu         51*/
   50 continue                                                                  !/*nu         52*/
      do 60 i=1,9                                                               !/*nu         53*/
      if (anu.lt.anub(i)) go to 70                                              !/*nu         54*/
   60 continue                                                                  !/*nu         55*/
      i=9                                                                       !/*nu         56*/
   70 continue                                                                  !/*nu         57*/
      nanu=i-1                                                                  !/*nu         58*/
      if (nanu.le.0) nanu=1                                                     !/*nu         59*/
      if (nanu.ge.8) go to 110                                                  !/*nu         60*/
      do 80 in1=1,13                                                            !/*nu         61*/
      nanun=nanu+1                                                              !/*nu         62*/
      dist(in1)=((anu-anub(nanu))*ab(in1,nanun)+(anub(nanun)-anu)*ab(in1        !/*nu         63*/
     & ,nanu))/(anub(nanun)-anub(nanu))                                         !/*nu         64*/
   80 continue                                                                  !/*nu         65*/
      x=rang()                                                                  !/*nu         66*/
      do 90 in1=1,13                                                            !/*nu         67*/
      if (x.le.dist(in1)) go to 100                                             !/*nu         68*/
   90 continue                                                                  !/*nu         69*/
      in1=13                                                                    !/*nu         70*/
  100 continue                                                                  !/*nu         71*/
      nudist=in1                                                                !/*nu         72*/
      return                                                                    !/*nu         73*/
  110 continue                                                                  !/*nu         74*/
      nud=kpois(explam)                                                         !/*nu         75*/
      nudist=nud+1                                                              !/*nu         76*/
      return                                                                    !/*nu         77*/
      end                                                                       !/*nu         78*/
