!     $Id: ins_histp.F,v 1.8 2002/06/14 17:05:42 jxh Exp $                      !/*in          1*/
      subroutine ins_histp(nel,mel,z9,a9,sigg,hsigg,sigmx,m_prob,m_max)         !/*in          2*/
!     HISTP Implementation: This routine needed only for histp writing.         !/*in          3*/
#ifdef HISTP                                                                    /*!in          4*/
      use global_parameters, only : kod,ver,loddat                              !/*ina         1*/
#include <lcs/double.h>                                                         /*!in          5*/
#include <lcs/lptS.h>                                                           /*!in          7*/
#include <lcs/rucom.h>                                                          /*!in          8*/
#include <lcs/concom.h>                                                         /*!in          9*/
#include <lcs/inout.h>                                                          /*!in         10*/
#include <lcs/charla.h>                                                         /*!in         11*/
      dimension nel(1), mel(1), z9(1), a9(1), sigg(6,1), hsigg(lpt,1),          !/*in         12*/
     & sigmx(lpt,1)                                                             !/*in         13*/
      character nn1*8, nn2*8, m_prob*19                                         !/*in         14*/
      data llq /28/, llpt /lpt/                                                 !/*in         15*/
      data idum /0/                                                             !/*in         16*/
!                                                                               !/*in         17*/
!  Initialize HISTP                                                             !/*in         18*/
      probid=m_prob                                                             !/*in         19*/
      maxcas=m_max                                                              !/*in         20*/
      iqhist=0                                                                  !/*in         21*/
      nhstp=55                                                                  !/*in         22*/
      histp='histp'                                                             !/*in         23*/
      if (inqire(histp).ne.0) then                                              !/*in         24*/
        write (itty,10)                                                         !/*in         25*/
   10   format(' histp already exists.  histpa is created instead.')            !/*in         26*/
        histp='histpa'                                                          !/*in         27*/
        call unique(histp,itty)                                                 !/*in         28*/
      endif                                                                     !/*in         29*/
      open(nhstp,file=histp,status='new',form='unformatted')                    !/*inxk        1*/
      write (itty,20) histp                                                     !/*in         32*/
      write (io,20) histp                                                       !/*in         33*/
   20 format (/5x,'history tape is file',1x,a10/)                               !/*in         34*/
!                                                                               !/*in         35*/
      ncol=-1                                                                   !/*in         36*/
      do 30 j=1,8                                                               !/*in         37*/
      do 30 i=1,lpt                                                             !/*in         38*/
      nosum(i,j)=0                                                              !/*in         39*/
   30 wtsum(i,j)=dp0                                                            !/*in         40*/
!                                                                               !/*in         41*/
      ncolc(4)=ncolc(4)+1                                                       !/*in         42*/
!                                                                               !/*in         43*/
!     start of run                                                              !/*in         44*/
!                                                                               !/*in         45*/
      nn1(1:8)=probid(3:10)                                                     !/*in         46*/
      nn2(1:8)=probid(12:19)                                                    !/*in         47*/
#ifdef CHEAP                                                                    /*!in         48*/
      write (nhstp) ncol,nn1,nn2,histp,kod,jip0,(idum,i=1,19)                   !/*in         49*/
#else                                                                           /*!in         50*/
      write (nhstp) ncol,nn1,nn2,histp,kod,jip0,(idum,i=1,23)                   !/*in         51*/
#endif                                                                          /*!in         52*/
      write (nhstp) maxbch,maxcas,mxmat,lneutp,npidk,n1col,nobalc,emin          !/*in         53*/
     & ,emax,nexite,nspred,nstrag,nseudo,nbertp,ibertp,icpt,ipht,               !/*in         54*/
     & (idum,i=1,5)                                                             !/*in         55*/
      write (nhstp) (nel(m),m=1,mxmat)                                          !/*in         56*/
      do 40 m=1,mxmat                                                           !/*in         57*/
      nelm=nel(m)                                                               !/*in         58*/
      write (nhstp) (z9(mel(m)+l-1),a9(mel(m)+l-1),                             !/*in         59*/
     & (sigg(ll,mel(m)+l-1),ll=1,6),l=1,nelm)                                   !/*in         60*/
      write (nhstp) (sigmx(i,m),i=1,lpt)                                        !/*in         61*/
      write (nhstp) (hsigg(i,m),i=1,lpt)                                        !/*in         62*/
   40 continue                                                                  !/*in         63*/
      mx=mxmat+1                                                                !/*in         64*/
      write (nhstp) (sigmx(i,mx),i=1,lpt)                                       !/*in         65*/
#endif                                                                          /*!in         66*/
      return                                                                    !/*in         67*/
      end                                                                       !/*in         68*/
