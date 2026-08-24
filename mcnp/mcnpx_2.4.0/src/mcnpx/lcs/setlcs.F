!     $Id: setlcs.F,v 1.4 2002/03/27 18:15:23 gwm Exp $                       !/*se          1*/
      subroutine setlcs(m_jtty,m_iuo,m_ink43,m_nmat,m_kpt,m_mipt,               !/*se          2*/
     & lcaopt,m_lca,optlcb,m_lcb,leaopt,m_lea,optleb,m_leb)                     !/*se          3*/
#include <lcs/double.h>                                                         /*!se          4*/
#include <lcs/lptS.h>                                                           /*!se          5*/
#include <lcs/charla.h>                                                         /*!se          6*/
#include <lcs/concom.h>                                                         /*!se          7*/
#include <lcs/elstic.h>                                                         /*!se          8*/
#include <lcs/inout.h>                                                          /*!se          9*/
#include <lcs/rucom.h>                                                          /*!se         10*/
#include <lcs/cacom.h>                                                          /*!se         11*/
#include <lcs/ercom.h>                                                          /*!se         12*/
#include <lcs/phtcom.h>                                                         /*!se         13*/
      dimension m_kpt(m_mipt), lcaopt(m_lca), leaopt(m_lea)                     !/*se         14*/
      dimension optlcb(m_lcb), optleb(m_leb)                                    !/*se         15*/
!        Set LCA options                                                        !/*se         16*/
      ielas=lcaopt(1)                                                           !/*se         17*/
      ipreq=lcaopt(2)                                                           !/*se         18*/
      iexisa=lcaopt(3)                                                          !/*se         19*/
      ichoic=lcaopt(4)                                                          !/*se         20*/
      jcoul=lcaopt(5)                                                           !/*se         21*/
      nexite=lcaopt(6)                                                          !/*se         22*/
      npidk=lcaopt(7)                                                           !/*se         23*/
      noact=lcaopt(8)                                                           !/*se         24*/
#ifdef CEM                                                                      /*!se         25*/
      icem=lcaopt(9)                                                            !/*se         26*/
#endif                                                                          /*!se         27*/
!        Set LCB options                                                        !/*se         28*/
      flenb(1)=optlcb(1)                                                        !/*se         29*/
      flenb(2)=optlcb(2)                                                        !/*se         30*/
      flenb(3)=optlcb(3)                                                        !/*se         31*/
      flenb(4)=optlcb(4)                                                        !/*se         32*/
      flenb(5)=optlcb(5)                                                        !/*se         33*/
      flenb(6)=optlcb(6)                                                        !/*se         34*/
      ctofe9=optlcb(7)                                                          !/*se         35*/
      flim0=optlcb(8)                                                           !/*se         36*/
!        Set LEA options                                                        !/*se         37*/
      ipht=leaopt(1)                                                            !/*se         38*/
      icc=leaopt(2)                                                             !/*se         39*/
      nobalc=leaopt(3)                                                          !/*se         40*/
      nobale=leaopt(4)                                                          !/*se         41*/
      ifbrk=leaopt(5)                                                           !/*se         42*/
      ilvden=leaopt(6)                                                          !/*se         43*/
      ievap=leaopt(7)                                                           !/*se         44*/
      nofis=leaopt(8)                                                           !/*se         45*/
!        Set LEB options                                                        !/*se         46*/
      yzere=optleb(1)                                                           !/*se         47*/
      bzere=optleb(2)                                                           !/*se         48*/
      yzero=optleb(3)                                                           !/*se         49*/
      bzero=optleb(4)                                                           !/*se         50*/
!        Set other options.                                                     !/*se         51*/
      itty=m_jtty                                                               !/*se         52*/
      io=m_iuo                                                                  !/*se         53*/
#ifdef LCS_PRINT                                                                /*!se         54*/
      nledit=m_ink43                                                            !/*se         55*/
#else                                                                           /*!se         56*/
      nledit=0                                                                  !/*se         57*/
#endif                                                                          /*!se         58*/
      mxmat = m_nmat                                                            !/*se         59*/
      do 10 j=1,ndef                                                            !/*se         60*/
   10 isub(j) = msub(j)                                                         !/*se         61*/
      do 20 i=1,lpt                                                             !/*se         62*/
   20 emin(i)=cut_mc(i)                                                         !/*se         63*/
!                                                                               !/*se         64*/
      do 30 i=1,lpt                                                             !/*se         65*/
      call mtran(i,ip,ja)                                                       !/*se         66*/
      if (m_kpt(ip).eq.0) emin(i)=1.d+30                                        !/*se         67*/
   30 continue                                                                  !/*se         68*/
      if (m_kpt(4).eq.1) then                                                   !/*se         69*/
        ibertp=-1                                                               !/*se         70*/
      else                                                                      !/*se         71*/
        ibertp=1                                                                !/*se         72*/
      endif                                                                     !/*se         73*/
      icpt=m_kpt(31)+m_kpt(32)+m_kpt(33)+m_kpt(34)                              !/*se         74*/
!                                                                               !/*se         84*/
      ilvdn=ilvden                                                              !/*se         85*/
      ctofn9=ctofe9                                                             !/*se         86*/
      write (io,40)                                                             !/*se         87*/
      write (io,50) ielas,ipreq,iexisa,ichoic,jcoul,nexite,npidk,noact          !/*se         88*/
#ifdef CEM                                                                      /*!se         89*/
     & ,icem                                                                    !/*se         90*/
#endif                                                                          /*!se         91*/
      write (io,60) (flenb(i),i=1,6),ctofe9,flim0                               !/*se         92*/
      write (io,70) ipht,icc,nobalc,nobale,ifbrk,ilvden,ievap,nofis             !/*se         93*/
      write (io,80) yzere,bzere,yzero,bzero                                     !/*se         94*/
#ifndef OLDM                                                                    /*!se         95*/
      if (noact.eq.-1) then                                                     !/*se         96*/
         noact=1                                                                !/*se         97*/
         n1col=-1                                                               !/*se         98*/
         write (io,*) ' XSEX3 CROSS SECTION CALCULATION: n1col = -1 '           !/*se         99*/
      endif                                                                     !/*se        100*/
!     MODEXS=0 is new cross section method                                      !/*se        101*/
      modexs=0                                                                  !/*se        102*/
#else                                                                           /*!se        103*/
!     MODEXS=1 reverts to old cross section methods                             !/*se        104*/
      modexs=1                                                                  !/*se        105*/
#endif                                                                          /*!se        106*/
!     write (io,*) ' CROSS SECTION MODE = ',modexs                              !/*se        107*/
!                                                                               !/*se        108*/
   40 format(//'1LAHET physics options:',81x,'print table 41')                  !/*sea         1*/
   50 format (/' lca',4x,'ielas',3x,'ipreq',2x,'iexisa',2x,'ichoic',3x,         !/*se        110*/
     & 'jcoul',2x,'nexite',3x,'npidk',3x,'noact'                                !/*se        111*/
#ifdef CEM                                                                      /*!se        112*/
     & ,4x,'icem'                                                               !/*se        113*/
#endif                                                                          /*!se        114*/
     & /' lca',1x,9i8)                                                          !/*se        115*/
   60 format (/' lcb',4x,'flenb(i),i=1,6',58x,'ctofe',7x,'flim0'/               !/*se        116*/
     & ' lcb',2x,1p8e12.4)                                                      !/*se        117*/
   70 format (/' lea',5x,'ipht',5x,'icc',2x,'nobalc',2x,'nobale',3x,            !/*se        118*/
     & 'ifbrk',2x,'ilvden',3x,'ievap',3x,'nofis'/' lea',1x,8i8)                 !/*se        119*/
   80 format (/' leb',4x,'yzere',7x,'bzere',7x,'yzero',7x,'bzero'/              !/*se        120*/
     & ' leb',2x,1p4e12.4/)                                                     !/*se        121*/
!                                                                               !/*se        122*/
      return                                                                    !/*se        123*/
      end                                                                       !/*se        124*/
