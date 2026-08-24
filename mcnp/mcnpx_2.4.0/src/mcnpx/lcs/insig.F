!     $Id: insig.F,v 1.4 2002/03/27 18:15:08 gwm Exp $                        !/*in          1*/
      subroutine insig (nbrtp,geosig)                                           !/*in          2*/
#include <lcs/double.h>                                                         /*!in          3*/
#include <lcs/tpcrs.h>                                                          /*!in          4*/
#include <lcs/btcom.h>                                                          /*!in          5*/
      dimension xsl(10,260)                                                     !/*in          6*/
      equivalence (xsl,crsc)                                                    !/*in          7*/
      dimension geosig(260)                                                     !/*in          8*/
      nrt=0                                                                     !/*in          9*/
      sf=dp1                                                                    !/*in         10*/
!     sf=1.0 at present                                                         !/*in         11*/
      i1i=1                                                                     !/*in         12*/
      i2i=600                                                                   !/*in         13*/
      do 10 j=1,4                                                               !/*in         14*/
      read (nbrtp) (crsc(i),i=i1i,i2i)                                          !/*in         15*/
      i1i=i1i+600                                                               !/*in         16*/
      i2i=i2i+600                                                               !/*in         17*/
   10 continue                                                                  !/*in         18*/
      k=1                                                                       !/*in         19*/
      do 20 i=4,2394,10                                                         !/*in         20*/
      radfm=1.d+13*crsc(i)                                                      !/*in         21*/
      geosig(k)=.031416d0*radfm**2                                              !/*in         22*/
   20 k=k+1                                                                     !/*in         23*/
      do 30 k=240,260                                                           !/*in         24*/
      geosig(k)=geosig(238)*(239-k)+geosig(239)*(k-238)                         !/*in         25*/
   30 continue                                                                  !/*in         26*/
      call setcoul                                                              !/*in         27*/
!    fix to extend range of masses for bert from a=239 to a=260                 !/*in         28*/
      do 50 k=240,260                                                           !/*in         29*/
      xsl(1,k)=k                                                                !/*in         30*/
      do 40 j=2,10                                                              !/*in         31*/
      xsl(j,k)=xsl(j,238)*(239-k)+xsl(j,239)*(k-238)                            !/*in         32*/
   40 continue                                                                  !/*in         33*/
   50 continue                                                                  !/*in         34*/
      read (nbrtp) (cs(i),i=1,29849)                                            !/*in         35*/
      ln=2                                                                      !/*in         36*/
!     nor=record number                                                         !/*in         37*/
!     nrt=number of files                                                       !/*in         38*/
      pnms=.708d+13                                                             !/*in         39*/
!     pi+ or - mass in recip. cm                                                !/*in         40*/
      dncms=4.758d+13                                                           !/*in         41*/
      sqnm=dncms*dncms                                                          !/*in         42*/
!     nucleon mass squared                                                      !/*in         43*/
!     nucleon mass in recip. cm                                                 !/*in         44*/
      rcpmv=.50613d+11                                                          !/*in         45*/
!     reciprocal cm per mv                                                      !/*in         46*/
      poms=.684d+13                                                             !/*in         47*/
!     pi0 mass in recip. cm                                                     !/*in         48*/
      do 60 i=1,19                                                              !/*in         49*/
      poac(i)=poac(i)+poac(i)                                                   !/*in         50*/
   60 ppac(i)=ppac(i)/sf                                                        !/*in         51*/
      return                                                                    !/*in         52*/
      end                                                                       !/*in         53*/
