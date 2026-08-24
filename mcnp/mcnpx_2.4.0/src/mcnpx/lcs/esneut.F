!     $Id: esneut.F,v 1.4 2002/03/27 18:15:03 gwm Exp $                       !/*es          1*/
      subroutine esneut (mxmt,nel,mel,a9,fmf,esxln)                             !/*es          2*/
!--------------------------------------------------------                       !/*es          3*/
! subname= esneut                       b.colborn                               !/*es          4*/
!  ESNEUT calls  HILONX  NASANX                                                 !/*es          5*/
!  ESNEUT called by  XIN                                                        !/*es          6*/
!--------------------------------------------------------                       !/*es          7*/
!           called by XIN to set up problem dependent  elastic x-sec            !/*es          8*/
!           arrays for neutrons                                                 !/*es          9*/
!                                                    apr 83                     !/*es         10*/
#include <lcs/double.h>                                                         /*!es         11*/
#include <lcs/inout.h>                                                          /*!es         12*/
#include <lcs/elstic.h>                                                         /*!es         13*/
      parameter (ieused=5)                                                      !/*es         14*/
      dimension nel(1), mel(1), a9(1), fmf(1), esxln(iemaxn,1)                  !/*es         15*/
!        compute cross sections for problem dependent media                     !/*es         16*/
      do 30 m=1,mxmt                                                            !/*es         17*/
      nel1=nel(m)                                                               !/*es         18*/
      do 30 l=1,nel1                                                            !/*es         19*/
      kmat2=mel(m)+l-1                                                          !/*es         20*/
      if (a9(kmat2).le.dp1) go to 30                                            !/*es         21*/
      do 10 ie=1,ieused                                                         !/*es         22*/
      call hilonx (a9(kmat2),esn(ie),xsig)                                      !/*es         23*/
   10 esxln(ie,kmat2)=xsig*fmf(kmat2)                                           !/*es         24*/
      do 20 ie=ieused+1,iemaxn                                                  !/*es         25*/
      call nasanx (a9(kmat2),esn(ie),xsig)                                      !/*es         26*/
   20 esxln(ie,kmat2)=xsig*fmf(kmat2)                                           !/*es         27*/
   30 continue                                                                  !/*es         28*/
      if (nledit.ne.0) then                                                     !/*es         29*/
        write (io,50)                                                           !/*es         30*/
        do 40 m=1,mxmt                                                          !/*es         31*/
        nel1=nel(m)                                                             !/*es         32*/
        write (io,60) m                                                         !/*es         33*/
        do 40 ie=1,iemaxn                                                       !/*es         34*/
        write (io,70) (esxln(ie,mel(m)+l-1),l=1,nel1)                           !/*es         35*/
   40   continue                                                                !/*es         36*/
      endif                                                                     !/*es         37*/
      return                                                                    !/*es         38*/
!                                                                               !/*es         39*/
   50 format('1LAHET elastic cross sections computed by esneut...esxln',        !/*es         40*/
     & 48x,'print table 43')                                                    !/*es         41*/
   60 format (10x,'medium=',i5)                                                 !/*es         42*/
   70 format (10e12.4)                                                          !/*es         43*/
      end                                                                       !/*es         44*/
