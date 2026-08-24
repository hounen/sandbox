!     $Id: wtin.F,v 1.5 2002/03/27 18:14:04 gwm Exp $                         !/*wt          1*/
      subroutine wtin                                                           !/*wt          2*/
!------ process hetc-like source descriptions for re-source                     !/*wt          3*/
#include <htape3x/com32b.h>                                                     /*!wt          4*/
#include <htape3x/label.h>                                                      /*!wt          5*/
#include <htape3x/contrl.h>                                                     /*!wt          6*/
#include <htape3x/wtpars.h>                                                     /*!wt          7*/
      parameter (nps=21)                                                        !/*wt          8*/
      dimension clear(nps), e0(2), tip0(2), z0(2)                               !/*wt          9*/
      equivalence (clear(1),ap(1))                                              !/*wt         10*/
      character *8 spec(2)                                                      !/*wt         11*/
      character *4 isub                                                         !/*wt         12*/
      data isub /'wtin'/                                                        !/*wt         13*/
      data spec /'original','modified'/                                         !/*wt         14*/
!------ clear out wtpars                                                        !/*wt         15*/
      do 10 i=1,nps                                                             !/*wt         16*/
   10 clear(i)=0                                                                !/*wt         17*/
      isopt(1)=0                                                                !/*wt         18*/
      isopt(2)=0                                                                !/*wt         19*/
!------ read and process source specs                                           !/*wt         20*/
      do 20 i=1,2                                                               !/*wt         21*/
      read (in,*) isopt(i),e0(i),tip0(i),z0(i),ap(i),bp(i),cp(i),co(i)          !/*wt         22*/
     & ,x0p(i),y0p(i)                                                           !/*wt         23*/
      if (i.eq.2.and.(e0(2).ne.e0(1).or.tip0(2).ne.tip0(1).or.                  !/*wt         24*/
     & z0(2).ne.z0(1))) call expire (0,isub,                                    !/*wt         25*/
     & 'e0,tip0,z0 must be same for both specs')                                !/*wt         26*/
      if (isopt(i).lt.0) call expire (0,isub,'isopt must be >=0')               !/*wt         27*/
      if (ap(i).le.dp0) call expire (0,isub,'a0 must be >0')                    !/*wt         28*/
      if (bp(i).le.dp0) bp(i)=ap(i)                                             !/*wt         29*/
      if (cp(i).le.dp0) then                                                    !/*wt         30*/
        if (isopt(i).eq.1) cp(i)=1.0                                            !/*wt         31*/
        if (isopt(i).eq.2) cp(i)=3.717                                          !/*wt         32*/
      endif                                                                     !/*wt         33*/
      if (isopt(i).eq.1.and.cp(i).gt.1.0) call expire (0,isub,                  !/*wt         34*/
     & 'cuts must be <=1')                                                      !/*wt         35*/
      if (abs(co(i)).gt.1.0) call expire (0,isub,'improper cosine')             !/*wt         36*/
      write (io,30) spec(i),isopt(i),e0(i),tip0(i),z0(i),ap(i),bp(i),cp         !/*wt         37*/
     & (i),co(i),x0p(i),y0p(i)                                                  !/*wt         38*/
      if (isopt(i).eq.3) call wtin4                                             !/*wt         39*/
   20 continue                                                                  !/*wt         40*/
!------ initialize remaining wtpars                                             !/*wt         41*/
      call winit                                                                !/*wt         42*/
      return                                                                    !/*wt         43*/
!                                                                               !/*wt         44*/
   30 format (1h0,a8,' source parameters:'/5x,'isopt =',i4/5x,'e0 =',1pd        !/*wt         45*/
     & 13.5/5x,'tip0 =',1pd13.5/5x,'z0 =',1pd13.5/5x,'a0 =',1pd13.5/5x,         !/*wt         46*/
     & 'b0 =',1pd13.5/5x,'cuts =',1pd13.5/5x,'smu =',1pd13.5/5x,'x0 =',         !/*wt         47*/
     * 1pd13.5/5x,'y0 =',1pd13.5)                                               !/*wt         48*/
      end                                                                       !/*wt         49*/
