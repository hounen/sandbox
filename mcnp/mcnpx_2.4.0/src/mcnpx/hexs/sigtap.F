!     $Id: sigtap.F,v 1.5 2002/03/27 18:14:50 gwm Exp $                       !/*si          1*/
      subroutine sigtap (itype,lu,io)                                           !/*si          2*/
!  B&P ROUTINE                                                                  !/*si          3*/
!     read cross sections from barpol.dat                                       !/*si          4*/
!                                                                               !/*si          5*/
#include <lcs/double.h>                                                         /*!si          6*/
#include <hexs/barpo.h>                                                         /*!si          7*/
      external xsbd                                                             !/*si          8*/
      character*1 ccc, a1, com(10)                                              !/*si          9*/
      dimension isig(51), mm(2), na(2), izz(19), ne(2)                          !/*si         10*/
      data ccc /'c'/, a1 /'*'/, com /',',',',',',',',',',',',',',',',','        !/*si         11*/
     & ,','/                                                                    !/*si         12*/
      if (itype.ge.0) return                                                    !/*si         13*/
!     ns=4                                                                      !/*si         14*/
!     ne(in) - number of energy points (51 for nucleons,43 for pions)           !/*si         15*/
!     nel - number of targets (19 for nucleons and for pions)                   !/*si         16*/
!     iener(l+in1) the energy beams in mev                                      !/*si         17*/
!     in=1 for nucleons                                                         !/*si         18*/
!     in=2 for pions                                                            !/*si         19*/
!     isig1,isig2,isig3,isig4- cross sections                                   !/*si         20*/
!     for n=1                                                                   !/*si         21*/
!     (j=1) cross sections for neutrons                                         !/*si         22*/
!     (j=2) cross sections for protons                                          !/*si         23*/
!     for n=2                                                                   !/*si         24*/
!     (j=3) cross sections for pi-                                              !/*si         25*/
!     (j=4) cross sections for pi+                                              !/*si         26*/
!     k- vs. target mass numbers                                                !/*si         27*/
!     l- vs. energy                                                            !/*si         28*/
!     izz(k)- the charge numbers of the  targets                               !/*si         29*/
!                                                                              !/*si         30*/
      write (*,*) ' **** cross sections taken from barpol.dat ****'             !/*si         31*/
      call openlcsdata (lu, "sigtap", "barpol.dat", 2)                          !/*si         32*/
!     open (lu,file='barpol.dat',status='old',form='formatted')                 !/*si         33*/
      rewind lu                                                                 !/*si         34*/
      read (lu,70) n,ns,ne(1),ne(2),nel                                         !/*si         35*/
      do 10 in=1,2                                                              !/*si         36*/
      in1=(in-1)*ne(1)                                                          !/*si         37*/
   10 read (lu,80) (iener(l+in1),l=1,ne(in))                                    !/*si         38*/
      do 40 in=1,2                                                              !/*si         39*/
      do 40 i=1,2                                                               !/*si         40*/
      j=i+(in-1)*2                                                              !/*si         41*/
      do 40 k=1,nel                                                             !/*si         42*/
      kn=(k-1)*ne(in)                                                           !/*si         43*/
      if (j.eq.1) read (lu,80) (isig1(kn+l),l=1,ne(in)),izz(k),j                !/*si         44*/
      if (j.eq.2) read (lu,80) (isig2(kn+l),l=1,ne(in)),izz(k),j                !/*si         45*/
      if (j.eq.3) read (lu,80) (isig3(kn+l),l=1,ne(in)),izz(k),j                !/*si         46*/
      if (j.eq.4) read (lu,80) (isig4(kn+l),l=1,ne(in)),izz(k),j                !/*si         47*/
      if (itype.ne.-2) go to 40                                                 !/*si         48*/
      do 20 l=1,ne(in)                                                          !/*si         49*/
      if (j.eq.1) isig(l)=isig1(kn+l)                                           !/*si         50*/
      if (j.eq.2) isig(l)=isig2(kn+l)                                           !/*si         51*/
      if (j.eq.3) isig(l)=isig3(kn+l)                                           !/*si         52*/
      if (j.eq.4) isig(l)=isig4(kn+l)                                           !/*si         53*/
   20 continue                                                                  !/*si         54*/
      mm(1)=8                                                                   !/*si         55*/
      na(1)=2                                                                   !/*si         56*/
      mm(2)=7                                                                   !/*si         57*/
      na(2)=1                                                                   !/*si         58*/
      naa=7                                                                     !/*si         59*/
      do 30 m=1,mm(in)                                                          !/*si         60*/
      im=(m-1)*7                                                                !/*si         61*/
      if (m.eq.mm(in)) naa=na(in)                                               !/*si         62*/
   30 write (io,60) a1,(isig(im+n),com(n),n=1,naa)                              !/*si         63*/
      write (io,50) ccc,izz(k),j                                                !/*si         64*/
   40 continue                                                                  !/*si         65*/
      close (lu)                                                                !/*si         66*/
      return                                                                    !/*si         67*/
!                                                                               !/*si         68*/
   50 format (a1,2i8)                                                           !/*si         69*/
   60 format (5x,a1,7(i8,a1))                                                   !/*si         70*/
   70 format (5i5)                                                              !/*si         71*/
   80 format (8i9)                                                              !/*si         72*/
      end                                                                       !/*si         73*/
