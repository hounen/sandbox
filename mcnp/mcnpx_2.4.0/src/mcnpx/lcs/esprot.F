!     $Id: esprot.F,v 1.4 2002/03/27 18:15:03 gwm Exp $                       !/*es          1*/
      subroutine esprot (mxmt,nel,mel,a9,fmf,esxlp,esxmp)                       !/*es          2*/
!--------------------------------------------------------                       !/*es          3*/
! subname= esprot                       b.colborn                               !/*es          4*/
!  ESPROT calls  NASAPX  ERPRNT                                                 !/*es          5*/
!  ESPROT called by  XIN                                                        !/*es          6*/
!--------------------------------------------------------                       !/*es          7*/
!           called by XIN to set up problem dependent  elastic x-sec            !/*es          8*/
!           arrays for protrons                                                 !/*es          9*/
!                                                    apr 83                     !/*es         10*/
#include <lcs/double.h>                                                         /*!es         11*/
#include <lcs/inout.h>                                                          /*!es         12*/
#include <lcs/elstic.h>                                                         /*!es         13*/
      parameter (ieused=5)                                                      !/*es         14*/
      dimension nel(1), mel(1), a9(1), fmf(1), esxlp(iemaxp,1), esxmp(1)        !/*es         15*/
!        compute cross sections for problem dependent media                     !/*es         16*/
      ierr=0                                                                    !/*es         17*/
      do 20 m=1,mxmt                                                            !/*es         18*/
      nel1=nel(m)                                                               !/*es         19*/
      do 20 l=1,nel1                                                            !/*es         20*/
      kmat2=mel(m)+l-1                                                          !/*es         21*/
      if (a9(kmat2).le.dp1) go to 20                                            !/*es         22*/
      temp=dp0                                                                  !/*es         23*/
      do 10 ie=1,iemaxp                                                         !/*es         24*/
      call nasapx (a9(kmat2),esp(ie),xsig)                                      !/*es         25*/
      esxlp(ie,kmat2)=xsig*fmf(kmat2)                                           !/*es         26*/
      temp=max(temp,esxlp(ie,kmat2))                                            !/*es         27*/
   10 continue                                                                  !/*es         28*/
      esxmp(kmat2)=temp                                                         !/*es         29*/
   20 continue                                                                  !/*es         30*/
      if (nledit.ne.0) then                                                     !/*es         31*/
        write (io,40)                                                           !/*es         32*/
        do 30 m=1,mxmt                                                          !/*es         33*/
        nel1=nel(m)                                                             !/*es         34*/
        write (io,50) m                                                         !/*es         35*/
        do 30 ie=1,iemaxp                                                       !/*es         36*/
        write (io,60) (esxlp(ie,mel(m)+l-1),l=1,nel1)                           !/*es         37*/
   30   continue                                                                !/*es         38*/
      endif                                                                     !/*es         39*/
      return                                                                    !/*es         40*/
!                                                                               !/*es         41*/
   40 format('1LAHET elastic cross sections computed by psneut...esxlp',        !/*es         42*/
     & 48x,'print table 43')                                                    !/*es         43*/
   50 format (10x,'medium=',i5)                                                 !/*es         44*/
   60 format (10e12.4)                                                          !/*es         45*/
      end                                                                       !/*es         46*/
