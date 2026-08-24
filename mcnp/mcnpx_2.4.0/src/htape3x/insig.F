!     $Id: insig.F,v 1.5 2002/03/27 18:14:00 gwm Exp $                        !/*in          1*/
      subroutine insig                                                          !/*in          2*/
      use global_parameters, only : hdpth0                                      !/*inyc        1*/
#include <htape3x/com32b.h>                                                     /*!in          3*/
#include <htape3x/label.h>                                                      /*!in          4*/
#include <htape3x/contrl.h>                                                     /*!in          5*/
#include <htape3x/charht.h>                                                     /*!in          6*/
#include <htape3x/exm.h>                                                        /*!in          7*/
#include <htape3x/wapcom.h>                                                     /*!in          8*/
      common /label2/ geosig(260), crsc(600), den(lmel), tden(lmt)              !/*in          9*/
      character hp*80,hq*89                                                     !/*inyc        2*/
      data nbertn /20/, ijk /0/                                                 !/*in         14*/
      if (ijk.gt.0) return                                                      !/*in         15*/
      ijk=1                                                                     !/*in         16*/
!                                                                               !/*inyc        3*/
!        get path to and open bertin file.                                      !/*inyc        4*/
      hp=' '                                                                    !/*inyc        5*/
#ifdef UNICOS                                                                   /*!inyc        6*/
      call cgtenv('DATAPATH'//char(0),hp)                                       !/*inyc        7*/
#endif                                                                          /*!inyc        8*/
#if defined(UNIX) || defined(DEC)                                               /*!inyc        9*/
#ifdef GENERIC_ENV                                                              /*!inyc       10*/
      call generic_getenv('DATAPATH',hp)                                        !/*inyc       11*/
#else                                                                           /*!inyc       12*/
      call getenv('DATAPATH',hp)                                                !/*inyc       13*/
#endif                                                                          /*!inyc       14*/
#endif                                                                          /*!inyc       15*/
      l=leng(hp)                                                                !/*inyc       16*/
      if(l.le.0)hp=hdpth0                                                       !/*inyc       17*/
      if(l.le.0)l=leng(hp)                                                      !/*inyc       18*/
      hq=hp(1:l)//'/'//bertin                                                   !/*inyc       19*/
#ifdef PCDOS                                                                    /*!inyc       20*/
      hq=hp(1:l)//'\'//bertin                                                   !/*inyc       21*/
#endif                                                                          /*!inyc       22*/
      if(inqire(bertin).ne.0)hq=bertin                                          !/*inyc       23*/
      if(inqire(hq).eq.0)call expire(0,'insig','cannot open '//bertin)          !/*inyc       24*/
      open (nbertn,file=hq,status='old',form='unformatted',                     !/*inyc       25*/
     & access='sequential')                                                     !/*inyc       26*/
!                                                                               !/*inyc       27*/
      k=1                                                                       !/*in         32*/
      do 20 j=1,4                                                               !/*in         33*/
      read (nbertn) (crsc(i),i=1,600)                                           !/*in         34*/
      do 20 i=4,594,10                                                          !/*in         35*/
      radfm=1.d+13*crsc(i)                                                      !/*in         36*/
      geosig(k)=.031416d0*radfm**2                                              !/*in         37*/
   20 k=k+1                                                                     !/*in         38*/
      do 30 k=240,260                                                           !/*in         39*/
      geosig(k)=geosig(238)*(239-k)+geosig(239)*(k-238)                         !/*in         40*/
   30 continue                                                                  !/*in         41*/
#ifdef LCS                                                                      /*!in         42*/
      read (nbertn) idmm                                                        !/*in         43*/
!*****read evaporation data                                                     !/*in         44*/
      read (nbertn) idmm                                                        !/*in         45*/
      read (nbertn) idmm                                                        !/*in         46*/
      read (nbertn) idmm                                                        !/*in         47*/
      read (nbertn) idmm                                                        !/*in         48*/
      read (nbertn) idmm                                                        !/*in         49*/
      read (nbertn) idmm                                                        !/*in         50*/
      read (nbertn) idmm                                                        !/*in         51*/
      read (nbertn) idmm                                                        !/*in         52*/
      read (nbertn) idmm                                                        !/*in         53*/
      read (nbertn) idmm                                                        !/*in         54*/
      read (nbertn) idmm                                                        !/*in         55*/
      read (nbertn) idmm                                                        !/*in         56*/
      read (nbertn) idmm                                                        !/*in         57*/
!*****read fission data                                                         !/*in         58*/
      read (nbertn) idmm                                                        !/*in         59*/
      read (nbertn) idmm                                                        !/*in         60*/
      read (nbertn) idmm                                                        !/*in         61*/
!*****read coulomb barrier data                                                 !/*in         62*/
      read (nbertn) idmm                                                        !/*in         63*/
      read (nbertn) idmm                                                        !/*in         64*/
      read (nbertn) idmm                                                        !/*in         65*/
      read (nbertn) idmm                                                        !/*in         66*/
      read (nbertn) idmm                                                        !/*in         67*/
      read (nbertn) idmm                                                        !/*in         68*/
      read (nbertn) idmm                                                        !/*in         69*/
      read (nbertn) idmm                                                        !/*in         70*/
      read (nbertn) idmm                                                        !/*in         71*/
      read (nbertn) idmm                                                        !/*in         72*/
!*****read Fermi breakup data                                                   !/*in         73*/
      read (nbertn) nr,nr1,nr2                                                  !/*in         74*/
      do 40 i1=1,nr                                                             !/*in         75*/
      read (nbertn) idmm                                                        !/*in         76*/
      read (nbertn) idmm                                                        !/*in         77*/
   40 continue                                                                  !/*in         78*/
      nrx=nr1+nr2                                                               !/*in         79*/
      do 50 i=1,nrx                                                             !/*in         80*/
      read (nbertn) idmm                                                        !/*in         81*/
   50 continue                                                                  !/*in         82*/
!*****read new mass data                                                        !/*in         83*/
      read (nbertn) waps                                                        !/*in         84*/
      read (nbertn) elbnde                                                      !/*in         85*/
      read (nbertn) deln                                                        !/*in         86*/
      read (nbertn) third                                                       !/*in         87*/
      read (nbertn) iz0                                                         !/*in         88*/
      read (nbertn) maxz                                                        !/*in         89*/
      read (nbertn) maxn                                                        !/*in         90*/
      read (nbertn) izmin                                                       !/*in         91*/
      read (nbertn) izmax                                                       !/*in         92*/
      read (nbertn) cwaps                                                       !/*in         93*/
!*****read cross section maximum data                                           !/*in         94*/
      read (nbertn) idmm                                                        !/*in         95*/
      read (nbertn) idmm                                                        !/*in         96*/
      read (nbertn) idmm                                                        !/*in         97*/
!*****                                                                          !/*in         98*/
      close (nbertn)                                                            !/*in         99*/
      return                                                                    !/*in        100*/
!                                                                               !/*in        101*/
#endif                                                                          /*!in        102*/
      end                                                                       !/*in        103*/
