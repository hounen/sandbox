!     $Id: input.F,v 1.12 2002/03/27 18:13:59 gwm Exp $                       !/*in          1*/
      subroutine input (nerg,nerg1,ntim,ntim1,*)                                !/*in          2*/
      use global_parameters, only : hdpth0,kod,ver,loddat                       !/*inxc        1*/
#include <htape3x/com32b.h>                                                     /*!in          3*/
#include <htape3x/charht.h>                                                     /*!in          5*/
#include <htape3x/label.h>                                                      /*!in          6*/
#include <htape3x/srcwt.h>                                                      /*!in          7*/
#include <htape3x/contrl.h>                                                     /*!in          8*/
#include <htape3x/box.h>                                                        /*!in          9*/
#include <htape3x/bndy.h>                                                       /*!in         10*/
#include <htape3x/ctconv.h>                                                     /*!in         11*/
#include <htape3x/bulk.h>                                                       /*!in         12*/
#include <htape3x/big.h>                                                        /*!in         13*/
#include <htape3x/big4.h>                                                       /*!in         14*/
      character idtm*19                                                         !/*in         15*/
      data iflag /0/, i_once /0/                                                !/*in         16*/
      character hp*80,hq*89                                                     !/*inxc        2*/
!                                                                               !/*ina         3*/
!        get path to phtlib file.                                               !/*ina         4*/
      hp=' '                                                                    !/*inxc        3*/
#ifdef UNICOS                                                                   /*!ina         5*/
      call cgtenv('DATAPATH'//char(0),hp)                                       !/*ina         6*/
#endif                                                                          /*!ina         7*/
#if defined(UNIX) || defined(DEC)                                               /*!ina         8*/
#ifdef GENERIC_ENV                                                              /*!ina         9*/
      call generic_getenv('DATAPATH',hp)                                        !/*ina        10*/
#else                                                                           /*!ina        11*/
      call getenv('DATAPATH',hp)                                                !/*ina        12*/
#endif                                                                          /*!ina        13*/
#endif                                                                          /*!in         20*/
      l=leng(hp)                                                                !/*ina        14*/
      if(l.le.0)hp=hdpth0                                                       !/*inxc        4*/
      if(l.le.0)l=leng(hp)                                                      !/*inxc        5*/
      hq=hp(1:l)//'/'//phtlib                                                   !/*inxc        6*/
#ifdef PCDOS                                                                    /*!inxc        7*/
      hq=hp(1:l)//'\'//phtlib                                                   !/*inxc        8*/
#endif                                                                          /*!inxc        9*/
      if(inqire(phtlib).ne.0)hq=phtlib                                          !/*inxc       10*/
      if(inqire(hq).eq.0)call expire(0,'input','cannot open '//phtlib)          !/*inxc       11*/
!                                                                               !/*ina        17*/
      do 10 i=1,lb2                                                             !/*in         21*/
   10 ergb(i)=dp0                                                               !/*in         22*/
      do 15 i=1,lb3                                                             !/*in         23*/
   15 timb(i)=dp0                                                               !/*in         24*/
      twopi=4.0*acos(0.)                                                        !/*in         25*/
      read (in,470,end=420) titl1                                               !/*in         26*/
      read (in,470) titl2                                                       !/*in         27*/
      iopt=0                                                                    !/*in         28*/
      nerg=0                                                                    !/*in         29*/
      ntim=0                                                                    !/*in         30*/
      kplot=0                                                                   !/*in         31*/
      ntim1=0                                                                   !/*in         32*/
      nerg1=0                                                                   !/*in         33*/
      ntype=0                                                                   !/*in         34*/
      ntyp1=0                                                                   !/*in         35*/
      kopt=0                                                                    !/*in         36*/
      nseg=0                                                                    !/*in         37*/
      nparm=0                                                                   !/*in         38*/
      nfprm=0                                                                   !/*in         39*/
      fnorm=1.0                                                                 !/*in         40*/
      ltest=0                                                                   !/*in         41*/
      ixout=0                                                                   !/*in         42*/
      irs=0                                                                     !/*in         43*/
      imerge=0                                                                  !/*in         44*/
      itconv=0                                                                  !/*in         45*/
      irsp=0                                                                    !/*in         46*/
      itmult=0                                                                  !/*in         47*/
      do 20 i=1,lpx1                                                            !/*in         48*/
      dfparm(i)=1.0                                                             !/*in         49*/
      dnparm(i)=dp0                                                             !/*in         50*/
   20 continue                                                                  !/*in         51*/
      do 30 i=1,lpx                                                             !/*in         52*/
      lparm(i)=i                                                                !/*in         53*/
      fparm(i)=1.0                                                              !/*in         54*/
   30 continue                                                                  !/*in         55*/
      do 40 i=1,lb31                                                            !/*in         56*/
   40 dntim(i)=1.0                                                              !/*in         57*/
      do 50 i=1,lb21                                                            !/*in         58*/
   50 dnerg(i)=1.0                                                              !/*in         59*/
      lbunch=.false.                                                            !/*in         60*/
      lsfprm=.false.                                                            !/*in         61*/
      lsntyp=.false.                                                            !/*in         62*/
      lsnprm=.false.                                                            !/*in         63*/
      lsntim=.false.                                                            !/*in         64*/
      lsnerg=.false.                                                            !/*in         65*/
      ifeng=.false.                                                             !/*in         66*/
      nocasx=0                                                                  !/*in         67*/
      read (in,*) iopt,nerg,ntim,ntyp1,kopt,nparm,nfprm,fnorm,kplot,            !/*in         68*/
     & ixout,irs,imerge,itconv,irsp,itmult,nocasx,ltest                         !/*in         69*/
      if ((.not.ltesting).and.(ltest.ne.0)) ltesting = .true.                   !/*in         70*/
      if (i_once.eq.0) then                                                     !/*in         71*/
        i_once=1                                                                !/*in         72*/
        call getidt (idtm)                                                      !/*in         73*/
        if(.not.ltesting) write (io,640) loddat,idtm                            !/*in         74*/
        write (jtty,640) loddat,idtm                                            !/*in         75*/
      endif                                                                     !/*in         76*/
      write (io,480) icase,titl1,titl2                                          !/*in         77*/
      write (io,490) iopt,nerg,ntim,ntyp1,kopt,nparm,nfprm,fnorm,kplot,         !/*in         78*/
     & ixout,irs,imerge,itconv,irsp,itmult,nocasx,ltest                         !/*in         79*/
      if (nocasx.gt.0) then                                                     !/*in         80*/
        write (jtty,*) ' Only history ',nocasx,' will be tallied!'              !/*in         81*/
        write (io,*) ' Only history ',nocasx,' will be tallied!'                !/*in         82*/
      endif                                                                     !/*in         83*/
      if (iopt.lt.0) ifeng=.true.                                               !/*in         84*/
      iopt=abs(iopt)                                                            !/*in         85*/
      jopt=iopt/100                                                             !/*in         86*/
      iopt=mod(iopt,100)                                                        !/*in         87*/
      if (jopt.ge.2) call expire (0,'input','iopt value not allowed')           !/*in         88*/
#ifndef LCS                                                                     /*!in         89*/
      if (iopt.eq.7) call expire (0,'input','IOPT=7 not allowed')               !/*in         90*/
      if (iopt.eq.6) call expire (0,'input','IOPT=6 not allowed')               !/*in         91*/
      if (iopt.eq.4) call expire (0,'input','IOPT=4 not allowed')               !/*in         92*/
#endif                                                                          /*!in         93*/
      if (jopt.eq.1) lbunch=.true.                                              !/*in         94*/
      if ((2.lt.iopt.and.iopt.lt.9).or.13.lt.iopt) then                         !/*in         95*/
        imerge=0                                                                !/*in         96*/
        if (imerge.gt.0) write (io,630)                                         !/*in         97*/
        nhst=nh10                                                               !/*in         98*/
      endif                                                                     !/*in         99*/
      if (ntyp1.lt.0) lsntyp=.true.                                             !/*in        100*/
      ntyp1=abs(ntyp1)                                                          !/*in        101*/
      if (nerg.lt.0) lsnerg=.true.                                              !/*in        102*/
      nerg=abs(nerg)                                                            !/*in        103*/
      if (nparm.lt.0) lsnprm=.true.                                             !/*in        104*/
      if (lsnprm.and.iopt.eq.13) then                                           !/*in        105*/
        call expire (0,'input','nparm.lt.0 not allowed for iopt=13')            !/*in        106*/
      endif                                                                     !/*in        107*/
      nparm=abs(nparm)                                                          !/*in        108*/
      if (nfprm.lt.0) lsfprm=.true.                                             !/*in        109*/
      nfprm=abs(nfprm)                                                          !/*in        110*/
      if (ntim.lt.0) lsntim=.true.                                              !/*in        111*/
      ntim=abs(ntim)                                                            !/*in        112*/
      if (fnorm.eq.dp0) fnorm=1.0                                               !/*in        113*/
      do 60 i=1,11                                                              !/*in        114*/
   60 ncp(i)=0                                                                  !/*in        115*/
      if (nerg.gt.0) go to 70                                                   !/*in        116*/
      ergb(1)=1.0e+30                                                           !/*in        117*/
      go to 80                                                                  !/*in        118*/
   70 continue                                                                  !/*in        119*/
      if (nerg.gt.lb2) call expire (0,'input','too many energy bins')           !/*in        120*/
      read (in,*) (ergb(i),i=1,nerg)                                            !/*in        121*/
      call fixe (ergb,nerg)                                                     !/*in        122*/
   80 continue                                                                  !/*in        123*/
      if (ntim.gt.0) go to 90                                                   !/*in        124*/
      timb(1)=1.0e+30                                                           !/*in        125*/
      go to 100                                                                 !/*in        126*/
   90 continue                                                                  !/*in        127*/
      if (ntim.gt.lb3) call expire (0,'input','too many time bins')             !/*in        128*/
      read (in,*) (timb(i),i=1,ntim)                                            !/*in        129*/
      call fixe (timb,ntim)                                                     !/*in        130*/
  100 continue                                                                  !/*in        131*/
      if (ntim.eq.1) ntim=0                                                     !/*in        132*/
      if (ntyp1.gt.0) read (in,*) (itip(i),i=1,ntyp1)                           !/*in        133*/
      if (nparm.gt.0) read (in,*) (lparm(i),i=1,nparm)                          !/*in        134*/
      if (nfprm.gt.0) read (in,*) (fparm(i),i=1,nfprm)                          !/*in        135*/
      if (nfprm.gt.1.and.fparm(1).eq.fparm(nfprm)) then                         !/*in        136*/
        delf=2.0/nfprm                                                          !/*in        137*/
        nup=nfprm-1                                                             !/*in        138*/
        fparm(nfprm)=1.0                                                        !/*in        139*/
        do 110 i=1,nup                                                          !/*in        140*/
  110   fparm(nfprm-i)=fparm(nfprm-i+1)-delf                                    !/*in        141*/
      endif                                                                     !/*in        142*/
      if (nerg.gt.0) write (io,500) (ergb(i),i=1,nerg)                          !/*in        143*/
      if (ntim.gt.0) write (io,510) (timb(i),i=1,ntim)                          !/*in        144*/
      if (ntyp1.gt.0) write (io,520) (itip(j),j=1,ntyp1)                        !/*in        145*/
      if (nparm.gt.0) write (io,530) (lparm(i),i=1,nparm)                       !/*in        146*/
      if (lbunch.and.nparm.gt.0) write (io,570)                                 !/*in        147*/
      if (nfprm.gt.0) write (io,540) (fparm(i),i=1,nfprm)                       !/*in        148*/
      np1=nparm+1                                                               !/*in        149*/
      if (.not.lsnprm) go to 130                                                !/*in        150*/
      read (in,*) (dnparm(i),i=1,np1)                                           !/*in        151*/
      write (io,430) (dnparm(i),i=1,np1)                                        !/*in        152*/
      if (lbunch) then                                                          !/*in        153*/
        temp=dp0                                                                !/*in        154*/
        do 120 i=1,nparm                                                        !/*in        155*/
        temp=temp+dnparm(i)                                                     !/*in        156*/
  120   continue                                                                !/*in        157*/
        dnparm(1)=temp                                                          !/*in        158*/
        write (io,580) dnparm(1)                                                !/*in        159*/
      else                                                                      !/*in        160*/
        write (io,590)                                                          !/*in        161*/
      endif                                                                     !/*in        162*/
      write (io,600)                                                            !/*in        163*/
      write (jtty,600)                                                          !/*in        164*/
  130 continue                                                                  !/*in        165*/
      do 140 i=1,np1                                                            !/*in        166*/
      if (dnparm(i).eq.dp0) dnparm(i)=1.0                                       !/*in        167*/
  140 continue                                                                  !/*in        168*/
      if (irs.ne.0) then                                                        !/*in        169*/
        fwton=.true.                                                            !/*in        170*/
        call wtin                                                               !/*in        171*/
      else                                                                      !/*in        172*/
        fwton=.false.                                                           !/*in        173*/
      endif                                                                     !/*in        174*/
      if (itconv.ne.0) call tconin                                              !/*in        175*/
      if (irsp.ne.0) call respin                                                !/*in        176*/
!*****                                                                          !/*in        177*/
      if (abs(iopt).gt.16) go to 340                                            !/*in        178*/
      go to (150,180,190,200,210,220,230,240,250,290,300,310,320,330,           !/*in        179*/
     & 335,337), iopt                                                           !/*in        180*/
!>>>>>                                                                          !/*in        181*/
  150 continue                                                                  !/*in        182*/
! option 1 - surface current                                                    !/*in        183*/
      ntype=ntyp1                                                               !/*in        184*/
      if (nfprm.le.0) fparm(1)=1.0                                              !/*in        185*/
      if (kopt.le.4) go to 160                                                  !/*in        186*/
      read (in,*) ifseg,nseg,(fseg(i),i=1,nseg)                                 !/*in        187*/
      write (io,440) ifseg,nseg,(fseg(i),i=1,nseg)                              !/*in        188*/
      kopt=kopt-5                                                               !/*in        189*/
  160 continue                                                                  !/*in        190*/
      if (nfprm.eq.0) kopt=0                                                    !/*in        191*/
      if (.not.lbunch) nbin=(nseg+1)*max(nparm,1)*(nfprm+1)                     !/*in        192*/
      if (lbunch) nbin=(nseg+1)*(nfprm+1)                                       !/*in        193*/
      if (kopt.ne.4) go to 350                                                  !/*in        194*/
      read (in,*) cv                                                            !/*in        195*/
      temp=sqrt(cv(1)**2+cv(2)**2+cv(3)**2)                                     !/*in        196*/
      do 170 i=1,3                                                              !/*in        197*/
      cv(i)=cv(i)/temp                                                          !/*in        198*/
  170 continue                                                                  !/*in        199*/
      write (io,450) cv                                                         !/*in        200*/
      go to 350                                                                 !/*in        201*/
  180 continue                                                                  !/*in        202*/
! option 2 - surface flux                                                       !/*in        203*/
      ntype=ntyp1                                                               !/*in        204*/
      if (nparm.le.0) call expire (0,'input','nparm needed for iopt=2')         !/*in        205*/
      if (nfprm.ne.0) call expire (0,'input',                                   !/*in        206*/
     & 'nfprm must be 0 for iopt=2')                                            !/*in        207*/
      if (kopt.eq.1) read (in,*) ifseg,nseg,(fseg(i),i=1,nseg)                  !/*in        208*/
      if (kopt.eq.1) write (io,440) ifseg,nseg,(fseg(i),i=1,nseg)               !/*in        209*/
      if (.not.lbunch) nbin=(nseg+1)*(nfprm+1)*nparm                            !/*in        210*/
      if (lbunch) nbin=(nseg+1)*(nfprm+1)                                       !/*in        211*/
      go to 350                                                                 !/*in        212*/
  190 continue                                                                  !/*in        213*/
! option 3 - particle production                                                !/*in        214*/
      ntype=lpt+22                                                              !/*in        215*/
      ntim=0                                                                    !/*in        216*/
      nerg1=nerg+3                                                              !/*in        217*/
      if (.not.lbunch) nbin=nparm+1                                             !/*in        218*/
      if (lbunch) nbin=1                                                        !/*in        219*/
      jmat=mod(kopt,2)                                                          !/*in        220*/
      kopt=(kopt-jmat)/2                                                        !/*in        221*/
      go to 370                                                                 !/*in        222*/
  200 continue                                                                  !/*in        223*/
! option 4 - neutron track length flux                                          !/*in        224*/
      ntype=1                                                                   !/*in        225*/
      ncp(2)=1                                                                  !/*in        226*/
      ntim=0                                                                    !/*in        227*/
      if (.not.lbunch) nbin=nparm                                               !/*in        228*/
      if (lbunch) nbin=1                                                        !/*in        229*/
      if (nparm.le.0) call expire (0,'input','nparm needed - iopt=4')           !/*in        230*/
      go to 370                                                                 !/*in        231*/
  210 continue                                                                  !/*in        232*/
! option 5 - nuclear excitation                                                 !/*in        233*/
      call peek (maxa,maxz)                                                     !/*in        234*/
      nerg=maxa+4                                                               !/*in        235*/
      ntim=4                                                                    !/*in        236*/
      ntim1=4                                                                   !/*in        237*/
      if (.not.lbunch) nbin=max(nparm,1)                                        !/*in        238*/
      if (lbunch) nbin=1                                                        !/*in        239*/
      lsize2=2*(nerg+1)*nbin                                                    !/*in        240*/
      jmat=mod(kopt,2)                                                          !/*in        241*/
      kopt=(kopt-jmat)/2                                                        !/*in        242*/
      ntype=1                                                                   !/*in        243*/
      go to 370                                                                 !/*in        244*/
  220 continue                                                                  !/*in        245*/
! option 6 - energy deposition                                                  !/*in        246*/
      nerg=17                                                                   !/*in        247*/
      if (.not.lbunch) nbin=nparm+1                                             !/*in        248*/
      if (lbunch) nbin=1                                                        !/*in        249*/
      ntype=1                                                                   !/*in        250*/
      jmat=mod(kopt,2)                                                          !/*in        251*/
      kopt=(kopt-jmat)/2                                                        !/*in        252*/
      go to 370                                                                 !/*in        253*/
  230 continue                                                                  !/*in        254*/
! option 7 - energy balance                                                     !/*in        255*/
      if (lbunch) call expire (0,'input','iopt = 107 not allowed')              !/*in        256*/
      nerg=20                                                                   !/*in        257*/
      nbin=nparm+1                                                              !/*in        258*/
      ntype=1                                                                   !/*in        259*/
      jmat=mod(kopt,2)                                                          !/*in        260*/
      kopt=(kopt-jmat)/2                                                        !/*in        261*/
      call insig                                                                !/*in        262*/
      go to 370                                                                 !/*in        263*/
  240 continue                                                                  !/*in        264*/
! option 8 - residual masses                                                    !/*in        265*/
      call peek (maxa,maxz)                                                     !/*in        266*/
      maxn=maxa-maxz                                                            !/*in        267*/
      ntim=maxn+10                                                              !/*in        268*/
      nerg=maxz+10                                                              !/*in        269*/
      nbin=1                                                                    !/*in        270*/
      ntype=1                                                                   !/*in        271*/
      if (.not.lbunch.and.nparm.gt.0) nbin=nparm                                !/*in        272*/
      jmat=mod(kopt,2)                                                          !/*in        273*/
      kopt=(kopt-jmat)/2                                                        !/*in        274*/
      lowaz=0                                                                   !/*in        275*/
      ihiaz=0                                                                   !/*in        276*/
      if (ixout.gt.0) write (io,611)                                            !/*in        277*/
      wtnone=dp0                                                                !/*in        278*/
      do 245 i=1,len2                                                           !/*in        279*/
  245 wtfin0(i)=dp0                                                             !/*in        280*/
      do 255 i=1,len1                                                           !/*in        281*/
  255 wtfin(i)=dp0                                                              !/*in        282*/
      if (iflag.ne.0) go to 370                                                 !/*in        283*/
      iflag=1                                                                   !/*in        284*/
      open (30,file=hq,status='old',form='unformatted',                         !/*inxc       12*/
     & access='sequential')                                                     !/*inxc       13*/
      call bininu (k,l)                                                         !/*in        300*/
      go to 370                                                                 !/*in        301*/
  250 continue                                                                  !/*in        302*/
! option 9 - surface current with window                                        !/*in        303*/
      if (nparm.le.0) call expire (0,'input','nparm needed - iopt=9')           !/*in        304*/
  260 continue                                                                  !/*in        305*/
      ifseg=0                                                                   !/*in        306*/
      if (kopt.lt.5) go to 270                                                  !/*in        307*/
      kopt=kopt-5                                                               !/*in        308*/
      ifseg=1                                                                   !/*in        309*/
  270 continue                                                                  !/*in        310*/
      ntype=ntyp1                                                               !/*in        311*/
      nseg=1                                                                    !/*in        312*/
      if (.not.lbunch) nbin=(nseg+1)*(nfprm+1)*nparm                            !/*in        313*/
      if (lbunch) nbin=(nseg+1)*(nfprm+1)                                       !/*in        314*/
      nup=4                                                                     !/*in        315*/
      if (ifseg.eq.1) nup=3                                                     !/*in        316*/
      read (in,*) iwind,(xyz(i),i=1,nup)                                        !/*in        317*/
      write (io,460) iwind,(xyz(i),i=1,nup)                                     !/*in        318*/
      if (ifseg.eq.1) xyz(3)=xyz(3)**2                                          !/*in        319*/
      if (iopt.eq.10) go to 350                                                 !/*in        320*/
      if (kopt.ne.4) go to 350                                                  !/*in        321*/
      read (in,*) cv                                                            !/*in        322*/
      temp=sqrt(cv(1)**2+cv(2)**2+cv(3)**2)                                     !/*in        323*/
      do 280 i=1,3                                                              !/*in        324*/
      cv(i)=cv(i)/temp                                                          !/*in        325*/
  280 continue                                                                  !/*in        326*/
      write (io,450) cv                                                         !/*in        327*/
      go to 350                                                                 !/*in        328*/
  290 continue                                                                  !/*in        329*/
! option 10 - surface flux with window                                          !/*in        330*/
      if (nparm.le.0) call expire (0,'input','nparm needed - iopt=10')          !/*in        331*/
      if (nfprm.ne.0) call expire (0,'input',                                   !/*in        332*/
     & 'nfprm must be 0 for iopt=10')                                           !/*in        333*/
      ifseg=kopt                                                                !/*in        334*/
      go to 270                                                                 !/*in        335*/
  300 continue                                                                  !/*in        336*/
! option 11 - pulse shape analysis                                              !/*in        337*/
      if (nparm.le.0) call expire (0,'input','nparm needed - iopt=11')          !/*in        338*/
      ntim=3                                                                    !/*in        339*/
      ntim1=3                                                                   !/*in        340*/
      go to 150                                                                 !/*in        341*/
  310 continue                                                                  !/*in        342*/
! option 12 - pulse shape analysis with window                                  !/*in        343*/
      if (nparm.le.0) call expire (0,'input','nparm needed - iopt=12')          !/*in        344*/
      ntim=3                                                                    !/*in        345*/
      ntim1=3                                                                   !/*in        346*/
      go to 260                                                                 !/*in        347*/
  320 continue                                                                  !/*in        348*/
! option 13 - emission distribution                                             !/*in        349*/
      if (lbunch) call expire (0,'input','iopt = 113 not allowed')              !/*in        350*/
      kopt=max(kopt,1)                                                          !/*in        351*/
      ntype=ntyp1                                                               !/*in        352*/
      lsfprm=.true.                                                             !/*in        353*/
      if (nparm.le.0) nparm=1                                                   !/*in        354*/
      if (nfprm.le.0) nfprm=1                                                   !/*in        355*/
      fparm(nfprm)=1.0                                                          !/*in        356*/
      dfparm(nfprm+1)=1.0/(2.0*twopi)                                           !/*in        357*/
      nbin=(nparm+1)*(nfprm+1)                                                  !/*in        358*/
      go to 350                                                                 !/*in        359*/
! option 14 - gas production                                                    !/*in        360*/
  330 ntype=8                                                                   !/*in        361*/
! ntype(1-8):h1,h2,h3,htot,he3,he4,hetot,gas tot                                !/*in        362*/
      if (.not.lbunch) nbin=nparm+1                                             !/*in        363*/
      if (lbunch) nbin=1                                                        !/*in        364*/
      jmat=mod(kopt,2)                                                          !/*in        365*/
      go to 370                                                                 !/*in        366*/
!*****                                                                          !/*in        367*/
  335 continue                                                                  !/*in        368*/
! option 15 - isotopic collision rates                                          !/*in        369*/
      call peek (maxa,maxz)                                                     !/*in        370*/
      maxn=maxa-maxz                                                            !/*in        371*/
      ntim=maxn+10                                                              !/*in        372*/
      nerg=maxz+10                                                              !/*in        373*/
      nbin=1                                                                    !/*in        374*/
      ntype=1                                                                   !/*in        375*/
      if (.not.lbunch.and.nparm.gt.0) nbin=nparm                                !/*in        376*/
      jmat=mod(kopt,2)                                                          !/*in        377*/
      kopt=(kopt-jmat)/2                                                        !/*in        378*/
      lowaz=0                                                                   !/*in        379*/
      ihiaz=0                                                                   !/*in        380*/
      if (ixout.gt.0) write (io,615)                                            !/*in        381*/
      go to 370                                                                 !/*in        382*/
!>>>>>                                                                          !/*in        383*/
!*****                                                                          !/*in        384*/
  337 continue                                                                  !/*in        385*/
! option 16 - recoil energy spectra                                             !/*in        386*/
      ntype=1                                                                   !/*in        387*/
      ntim=4                                                                    !/*in        388*/
      ntim1=4                                                                   !/*in        389*/
      nerg1=nerg+2                                                              !/*in        390*/
      nbin=nparm+1                                                              !/*in        391*/
      if (.not.lbunch) nbin=nparm+1                                             !/*in        392*/
      if (lbunch) nbin=1                                                        !/*in        393*/
      jmat=mod(kopt,2)                                                          !/*in        394*/
      kopt=(kopt-jmat)/2                                                        !/*in        395*/
      call insig                                                                !/*in        396*/
      go to 370                                                                 !/*in        397*/
!>>>>>                                                                          !/*in        398*/
  340 continue                                                                  !/*in        399*/
      call expire (0,'input','unimplemented option requested')                  !/*in        400*/
  350 continue                                                                  !/*in        401*/
      do 360 i=1,ntype                                                          !/*in        402*/
      ncp(itip(i)+1)=1                                                          !/*in        403*/
  360 continue                                                                  !/*in        404*/
  370 continue                                                                  !/*in        405*/
      if (.not.lsfprm) go to 390                                                !/*in        406*/
      angold=-1.0                                                               !/*in        407*/
      do 380 i=1,nfprm                                                          !/*in        408*/
      dfparm(i)=1.0/((fparm(i)-angold)*twopi)                                   !/*in        409*/
      angold=fparm(i)                                                           !/*in        410*/
  380 continue                                                                  !/*in        411*/
      write (io,550) (dfparm(i),i=1,nfprm)                                      !/*in        412*/
  390 continue                                                                  !/*in        413*/
      if (lsnerg) then                                                          !/*in        414*/
        engold=dp0                                                              !/*in        415*/
        do 400 i=1,nerg                                                         !/*in        416*/
        dnerg(i)=1.0/(ergb(i)-engold)                                           !/*in        417*/
        engold=ergb(i)                                                          !/*in        418*/
  400   continue                                                                !/*in        419*/
        write (io,560) (dnerg(i),i=1,nerg)                                      !/*in        420*/
      endif                                                                     !/*in        421*/
      if (lsntim) then                                                          !/*in        422*/
        timold=dp0                                                              !/*in        423*/
        do 410 i=1,ntim                                                         !/*in        424*/
        dntim(i)=1.0/(timb(i)-timold)                                           !/*in        425*/
        timold=timb(i)                                                          !/*in        426*/
  410   continue                                                                !/*in        427*/
        write (io,620) (dntim(i),i=1,ntim)                                      !/*in        428*/
      endif                                                                     !/*in        429*/
      if (nerg1.eq.0) nerg1=nerg+1                                              !/*in        430*/
      if (ntim1.eq.0) ntim1=ntim+1                                              !/*in        431*/
      if (nerg.eq.0) nerg=1                                                     !/*in        432*/
      if (ntim.eq.0) ntim=1                                                     !/*in        433*/
      nseg1=nseg+1                                                              !/*in        434*/
      if (.not.lbunch) nparm1=nparm+1                                           !/*in        435*/
      if (lbunch) nparm1=1                                                      !/*in        436*/
      nfprm1=nfprm+1                                                            !/*in        437*/
      lfe=.true.                                                                !/*in        438*/
      lft=.true.                                                                !/*in        439*/
      if (nerg1.le.nerg) lfe=.false.                                            !/*in        440*/
      if (iopt.eq.3.and.nerg1.le.nerg+2) lfe=.false.                            !/*in        441*/
      if (ntim1.le.ntim) lft=.false.                                            !/*in        442*/
      lfte=lft.and.lfe                                                          !/*in        443*/
      lsize=nerg1*ntim1*ntype*nbin                                              !/*in        444*/
      i3=3                                                                      !/*in        445*/
      i4=4                                                                      !/*in        446*/
      maxblk=4*lsize                                                            !/*in        447*/
      if (iopt.eq.11.or.iopt.eq.12) maxblk=maxblk+lsize                         !/*in        448*/
      if (iopt.eq.5) maxblk=maxblk+lsize2                                       !/*in        449*/
      maxblk=maxblk+1                                                           !/*in        450*/
      call pointr (blk,maxblk,i3)                                               !/*in        451*/
      if (ipterr(dum).ne.0) call errorf ('input.1 ')                            !/*in        452*/
      call putm ('tlly  ',lsize,i4,iptll)                                       !/*in        453*/
      call putm ('tlsq  ',lsize,i4,iptls)                                       !/*in        454*/
      call putm ('hold  ',lsize,i4,iphld)                                       !/*in        455*/
      call putm ('tlfsq ',lsize,i4,ipfsq)                                       !/*in        456*/
      if (iopt.eq.11.or.iopt.eq.12) call putm ('cov   ',lsize,i4,ipcov)         !/*in        457*/
      if (iopt.eq.5) call putm ('cov   ',lsize2,i4,ipcov)                       !/*in        458*/
      if (ipterr(dum).ne.0) call errorf ('input.2 ')                            !/*in        459*/
      fwti=1                                                                    !/*in        460*/
      fwtcum=0                                                                  !/*in        461*/
      fwscum=0                                                                  !/*in        462*/
      if (ipterr(dum).ne.0) call errorf ('input.3 ')                            !/*in        463*/
      call status (io)                                                          !/*in        464*/
      return                                                                    !/*in        465*/
  420 continue                                                                  !/*in        466*/
      return 1                                                                  !/*in        467*/
!                                                                               !/*in        468*/
  430 format (/10x,'tally divisors i=1,nparm+1:'/(10x,1p7d14.5))                !/*in        469*/
  440 format (5x,'ifseg =',i2,'   nseg =',i4,'   fseg ='/(15x,1p8d14.4))        !/*in        470*/
  450 format (5x,'angular definition vector:',1p3d20.7)                         !/*in        471*/
  460 format ('0 window definition: iwind=',i2,' parameters',1p4d15.4)          !/*in        472*/
  470 format (10a8)                                                             !/*in        473*/
  480 format ('1',19x,'case',i3//10x,10a8//10x,10a8)                            !/*in        474*/
  490 format (//10x,'tally option =',i4/                                        !/*in        475*/
     & 10x,'number of energy bins =',i4/                                        !/*in        476*/
     & 10x,'number of time bins =',i4/10x,'number of particle types =',         !/*in        477*/
     & i4/10x,'secondary option =',i4/                                          !/*in        478*/
     & 10x,'number of integer parameters =',i4/10x,                             !/*in        479*/
     & 'number of real parameters =',i4/10x,'normalization constant =',         !/*in        480*/
     & 1pd14.6/10x,'plot control =',i4/10x,'auxiliary output flag =',           !/*in        481*/
     & i4/10x,'source modification flag =',i4/10x,                              !/*in        482*/
     & 'tape combination flag =',i4/10x,'time convolution flag =',i4/           !/*in        483*/
     & 10x,'response function flag=',i4,                                        !/*in        484*/
     & '; >0 means multiply; <0 means divide'/                                  !/*in        485*/
     & 10x,'time multiplication flag =',i4/                                     !/*in        486*/
     & 10x,'single cascade edit number =',i10/                                  !/*in        487*/
     & 10x,'testing flag =',i4)                                                 !/*in        488*/
  500 format (/10x,'energy bin boundaries :'/(10x,1p8d13.4))                    !/*in        489*/
  510 format (/10x,'time bin boundaries :'/(10x,1p8d13.4))                      !/*in        490*/
  520 format (/10x,'particle types :',30i3)                                     !/*in        491*/
  530 format (/10x,'integer parameters :'/(10x,10i6))                           !/*in        492*/
  540 format (/10x,'real parameters:'/(10x,1p4d15.6))                           !/*in        493*/
  550 format (/10x,                                                             !/*in        494*/
     & 'multiplicative factors for angle normalization i=1,nfprm:'/             !/*in        495*/
     & (10x,1p7d14.5))                                                          !/*in        496*/
  560 format (/10x,                                                             !/*in        497*/
     & 'multiplicative factors for energy normalization i=1,nerg:'/             !/*in        498*/
     & (10x,1p7d14.5))                                                          !/*in        499*/
  570 format (10x,'note: the tallies for the above integer parameters',         !/*in        500*/
     & ' will be combined into a single tally edit')                            !/*in        501*/
  580 format (/10x,'tally divisor for combined tally set to',1pd14.5)           !/*in        502*/
  590 format (/10x,'any dp0 values will be set to 1.0')                         !/*in        503*/
  600 format (10x,'WARNING: in this version of HTAPE, the DNPARM',              !/*in        504*/
     & ' parameters are divisors of the tallies')                               !/*in        505*/
  610 format (/10x,'an ORIHET input tape will be written')                      !/*in        506*/
!*****                                                                          !/*in        507*/
  611 format (/10x,'a CINDER input tape OPT8A will be written')                 !/*in        508*/
  615 format (/10x,'a CINDER input tape OPT15A will be written')                !/*in        509*/
!>>>>>                                                                          !/*in        510*/
  620 format (/10x,                                                             !/*in        511*/
     & 'multiplicative factors for time normalization i=1,ntim:'/               !/*in        512*/
     & (10x,1p7d14.5))                                                          !/*in        513*/
  630 format (/10x,'NOTE: for this edit option, merge is disallowed,',          !/*in        514*/
     & ' and the history tape is HISTP')                                        !/*in        515*/
#ifdef LCS                                                                      /*!in        516*/
  640   format (' Running HTAPE3 of ',a28,4x,a19)                               !/*in        517*/
#else                                                                           /*!in        518*/
  640   format (' Running HTAPE3X of ',a28,4x,a19)                              !/*in        519*/
#endif                                                                          /*!in        520*/
      end                                                                       !/*in        521*/
