!     $Id: resp.F,v 1.4 2002/03/27 18:14:02 gwm Exp $                         !/*re          1*/
      function resp (ec)                                                        !/*re          2*/
#include <htape3x/com32b.h>                                                     /*!re          3*/
      real ec                                                                   !/*re          4*/
!------ get response(ec)                                                        !/*re          5*/
#include <htape3x/cresp.h>                                                      /*!re          6*/
      temp=ec                                                                   !/*re          7*/
      e=min(max(eresp(1),temp),eresp(nresp))                                    !/*re          8*/
      do 10 ie=2,nresp                                                          !/*re          9*/
      if (e.le.eresp(ie)) go to 20                                              !/*re         10*/
   10 continue                                                                  !/*re         11*/
      ie=nresp                                                                  !/*re         12*/
      e=eresp(nresp)                                                            !/*re         13*/
   20 ir=iresp(ie-1)                                                            !/*re         14*/
      e1=eresp(ie-1)                                                            !/*re         15*/
      e2=eresp(ie)                                                              !/*re         16*/
      f1=fresp(ie-1)                                                            !/*re         17*/
      f2=fresp(ie)                                                              !/*re         18*/
      go to (30,40,50,60,70), ir                                                !/*re         19*/
!------ constant                                                                !/*re         20*/
   30 resp=f1                                                                   !/*re         21*/
      go to 80                                                                  !/*re         22*/
!------ lin-lin                                                                 !/*re         23*/
   40 resp=f1+(f2-f1)*(e-e1)/(e2-e1)                                            !/*re         24*/
      go to 80                                                                  !/*re         25*/
!------ lin-log                                                                 !/*re         26*/
   50 resp=f1+(f2-f1)*log(e/e1)/log(e2/e1)                                      !/*re         27*/
      go to 80                                                                  !/*re         28*/
!------ log-lin                                                                 !/*re         29*/
   60 resp=f1*(f2/f1)**((e-e1)/(e2-e1))                                         !/*re         30*/
      go to 80                                                                  !/*re         31*/
!------ log-log                                                                 !/*re         32*/
   70 resp=f1*(f2/f1)**(log(e/e1)/log(e2/e1))                                   !/*re         33*/
   80 return                                                                    !/*re         34*/
      end                                                                       !/*re         35*/
