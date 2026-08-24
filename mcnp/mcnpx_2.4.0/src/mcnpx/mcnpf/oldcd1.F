!#ident oldcd1.F from deck ol                                                   !/*ol          1*/
      subroutine oldcd1                                                         !/*ol          2*/
!        complete the processing of the previous card.                          !/*ol          3*/
      use allocate_memory                                                       !/*olx         1*/
      use global_data                                                           !/*olx         2*/
      use imcn_data                                                             !/*olx         3*/
!                                                                               !/*olx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*olx         5*/
      implicit integer(kindi) (i-n)                                             !/*olx         6*/
!                                                                               !/*olx         7*/
      dimension ns(39)                                                          !/*ol4c        1*/
!        space needed for input coefficients for each surface type.             !/*ol          7*/
      data ns/4,4*1,4,3*2,3*3,3*1,3*5,3*3,2*10,3*7,3*0,24,15,4,18,18,           !/*ol4c        2*/
     & 10,18,20,24,32/                                                          !/*ol4c        3*/
!                                                                               !/*ol          9*/
      go to( 20, 40, 10, 50, 10, 10, 10, 10, 10, 10, 10, 10, 60, 10,210,        !/*ol         10*/
     &       10, 10, 80, 10, 10, 80, 10, 90,100,110,110,110,150,210,150,        !/*ol         11*/
     &      150,150, 10,160,170,170,150,150,150,180,180,190,210,150, 10,        !/*ol         12*/
     &      210,220,240, 10, 10,280, 10, 10, 10, 10)ica                         !/*ol         13*/
      go to( 10,290,300, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,310,        !/*ol         14*/
     &      315,320, 10, 10, 10, 10, 10, 10, 10,340, 10, 10, 10,350, 10,        !/*olx         8*/
     &      360, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,460,460,460,480,        !/*olx         9*/
     &       10,10,9910,9920,9930)ica-55                                        !/*olxc        1*/
   10 return                                                                    !/*ol         17*/
!                                                                               !/*ol         18*/
! >>>>>  cell descriptions                                                      !/*ol         19*/
!        m1c=count of lja items.                                                !/*ol         20*/
   20 if(likef.ne.0)go to 30                                                    !/*ol         21*/
!        tighter checks would waste less memory.  see comment in nxtit1.        !/*ol4c        6*/
      if(mxa.gt.50.and.mlja.gt.3250)mlja=mlja+7*m1c                             !/*ol4c        7*/
      if(mxa.le.50.or.mlja.le.3250)mlja=mlja+17*m1c                             !/*ol4c        8*/
      mxit=max(mxit,17*m1c)                                                     !/*ol4c        9*/
      return                                                                    !/*ol         24*/
   30 call likebt(2)                                                            !/*ol         25*/
      return                                                                    !/*ol         26*/
!                                                                               !/*ol         27*/
! >>>>>  surface descriptions                                                   !/*ol         28*/
!        m1c=surface type index.                                                !/*ol         29*/
!        m2c=1 if surface type symbol is the second item.                       !/*ol         30*/
   40 n=10                                                                      !/*ol         31*/
      if(m2c.eq.0.and.m1c.le.26.and.m1c.ge.1)n=ns(m1c)                          !/*ol         32*/
      if(m1c.eq.1.and.nwc.gt.8)n=10                                             !/*ol         33*/
!                                                                               !/*ol4c       10*/
!        allow for creation of additional surfaces for macrobodies.             !/*ol4c       11*/
      if(m1c.ge.30)n=ns(m1c)                                                    !/*ol4c       12*/
      nsc=nsc+n                                                                 !/*ol         34*/
      if(m1c.lt.30.or.m1c.gt.39)return                                          !/*ol4c       13*/
      jn=6                                                                      !/*ol4c       14*/
      if(m1c.eq.33.or.m1c.eq.34.or.m1c.eq.36)jn=3                               !/*ol4c       15*/
      if(m1c.eq.39)jn=8                                                         !/*ol4c       16*/
      if(m1c.eq.32.or.m1c.eq.35)jn=0                                            !/*ol4c       17*/
      if(m1c.eq.37)jn=5                                                         !/*ol4c       18*/
      if(mxj+jn.gt.size(nsfm))then                                              !/*olx        11*/
        nsfm => chg_mem (nsfm,mxj+jn+100,is)                                    !/*olx        12*/
        if(is.ne.0)call expire(0,'oldcd1','chg_mem error')                      !/*olx        13*/
      endif                                                                     !/*olx        14*/
      do 45 j=1,jn                                                              !/*ol4c       21*/
      mxj=mxj+1                                                                 !/*ol4c       22*/
   45 nsfm(lnsf+mxj)=0                                                          !/*ol4c       23*/
      return                                                                    !/*ol         35*/
!                                                                               !/*ol         36*/
! >>>>>  vectors                                                    vect        !/*ol         37*/
   50 nvec=nwc/4                                                                !/*ol         38*/
      return                                                                    !/*ol         39*/
!                                                                               !/*ol         40*/
! >>>>>  weight-window energies                                      wwe        !/*ol         41*/
   60 do 70 i=1,mipt                                                            !/*ol         42*/
   70 if(nqp(i).ne.0)nww(i)=max(nww(i),nwc)                                     !/*ol         43*/
      return                                                                    !/*ol         44*/
!                                                                               !/*ol         45*/
! >>>>>  source distributions                                      si,ds        !/*ol         46*/
!        m1c=count of colons and parens.                                        !/*ol         47*/
   80 mxxs=mxxs+4*(nwc-m1c)                                                     !/*ol         48*/
      mkcp=mkcp+4*m1c                                                           !/*ol         49*/
      i=1                                                                       !/*ol         50*/
      if(nwc.gt.2)i=2                                                           !/*ol         51*/
      if(mod(jsd(i,1+icn/31)/2**mod(icn,31),2).eq.0)                            !/*ol         52*/
     & jsd(i,1+icn/31)=jsd(i,1+icn/31)+2**mod(icn,31)                           !/*ol         53*/
      return                                                                    !/*ol         54*/
!                                                                               !/*ol         55*/
! >>>>>  source definition                                          sdef        !/*ol         56*/
!        m2c=count of colons and parens.                                        !/*ol         57*/
   90 if(m2c.ne.0)mkcp=mkcp+m2c*4                                               !/*ol         58*/
      return                                                                    !/*ol         59*/
!                                                                               !/*ol         60*/
! >>>>>  tally                                                         f        !/*ol         61*/
!        m7c=count of total number of bins.                                     !/*ol4a        2*/
!        m9c=itds() storage for talloc.                                         !/*ol4a        3*/
  100 if(mod(icn,10).eq.5)go to 130                                             !/*ol         62*/
      lit=lit+max(nqw,1)*(m9c+5)                                                !/*olx        15*/
!     lit=lit+max(nqw,1)*(m7c+m9c+10) ??? 6/25/01                               !/*olx        16*/
      ktl(ital,1)=m7c+1                                                         !/*ol4a        6*/
      go to 210                                                                 !/*ol         66*/
!                                                                               !/*ol         67*/
! >>>>>  ring detectors                                         fx,fy,fz        !/*ol         68*/
  110 ltd=ltd+5*(nwc/3)                                                         !/*ol         69*/
      do 120 i=1,mipt                                                           !/*ol         70*/
  120 if(nqp(i).ne.0)ndet(i)=ndet(i)+nwc/3                                      !/*ol         71*/
      go to 210                                                                 !/*ol         72*/
!                                                                               !/*ol         73*/
! >>>>>  point detectors                                               f        !/*ol         74*/
  130 ltd=ltd+5*(nwc/4)                                                         !/*ol         75*/
      do 140 i=1,mipt                                                           !/*ol         76*/
  140 if(nqp(i).ne.0)ndet(i)=ndet(i)+nwc/4                                      !/*ol         77*/
      go to 210                                                                 !/*ol         78*/
!                                                                               !/*ol         79*/
! >>>>>  various tally cards                        e,t,c,em,tm,cm,fu,ft        !/*ol         80*/
  150 ltd=ltd+nwc                                                               !/*ol         81*/
      if((ich.eq.'c'.or.ich.eq.'e'.or.ich.eq.'t').and.icn.eq.0.or.              !/*ol         82*/
     & ich.eq.'fu')ltd=ltd+nwc                                                  !/*ol         83*/
      go to 210                                                                 !/*ol         84*/
!                                                                               !/*ol         85*/
! >>>>>  tally multiplier bins                                        fm        !/*ol         86*/
  160 ltd=ltd+4+2*nwc                                                           !/*ol         87*/
      go to 210                                                                 !/*ol         88*/
!                                                                               !/*ol         89*/
! >>>>>  dose function                                             de,df        !/*olxc        2*/
  170 ltd=ltd+max(2*nwc+4,5)                                                    !/*olxc        3*/
      go to 210                                                                 !/*ol         92*/
!                                                                               !/*ol         93*/
! >>>>>  tally flagging                                        cf and sf        !/*ol         94*/
  180 lit=lit+nwc+1                                                             !/*ol         95*/
      go to 210                                                                 !/*ol         96*/
!                                                                               !/*ol         97*/
! >>>>>  tally segments                                               fs        !/*ol         98*/
  190 if (mod(icn,10).eq.5) then                                                !/*olx        22*/
#ifdef RADIOG                                                                   /*!olx        23*/
      go to 150                                                                 !/*olx        24*/
#else                                                                           /*!olx        25*/
        call erprnt(2,2,0,0,0,0,0,0,                                            !/*olx        26*/
     & '59hThis version of MCNPX does not include radiography tallies.')        !/*olx        27*/
        return                                                                  !/*olx        28*/
#endif                                                                          /*!olx        29*/
      endif                                                                     !/*olx        30*/
      mlja=mlja+nwc                                                             !/*olx        31*/
      if(icn.ne.0)ktl(ital,2)=nwc+2                                             !/*ol        100*/
      if(icn.ne.0)go to 210                                                     !/*ol        101*/
      do 200 i=1,ntalmx                                                         !/*ol        102*/
  200 if(ktl(i,2).eq.1)ktl(i,2)=nwc+2                                           !/*ol        103*/
!                                                                               !/*ol        104*/
! >>>>>  tally cards                                 above + pd,fc,sd,dd        !/*ol        105*/
  210 lrt=lrt+nwc+2                                                             !/*ol        106*/
      if(ich.eq.'pd')lrt=lrt+max(0,mxa-nwc)                                     !/*ol        107*/
      if(ich.eq.'fc')lrt=lrt+24*nwc                                             !/*ol        108*/
      if(ich.eq.'fu')lrt=lrt+nwc                                                !/*ol        109*/
      return                                                                    !/*ol        110*/
!                                                                               !/*ol        111*/
! >>>>>  parameters for dxtran                                       dxt        !/*ol        112*/
  220 do 230 i=1,mipt                                                           !/*ol        113*/
  230 if(nqp(i).ne.0)ndx(i)=nwc/5                                               !/*ol        114*/
      return                                                                    !/*ol        115*/
!                                                                               !/*ol        116*/
! >>>>>  material specifications                                       m        !/*ol        117*/
!        for each nonvoid cell, make space for nuclide summary.                 !/*ol4c2       3*/
  240 n=0                                                                       !/*ol4c2       4*/
      do 250 i=1,mxa                                                            !/*ol4c2       5*/
  250 if(mat(lmat+i).eq.icn)n=n+1                                               !/*ol4c2       6*/
      npn=npn+n*nwc/2                                                           !/*ol4c2       7*/
!        increment the total number of isotopes seen so far.                    !/*ol4c2       8*/
      mix=mix+nwc/2                                                             !/*ol4c2       9*/
!        update the maximum number of nuclides seen for any material.           !/*ol4c2      10*/
      mnnm=max(mnnm,nwc/2)                                                      !/*ol4c2      11*/
      return                                                                    !/*ol4c2      12*/
!                                                                               !/*ol        130*/
! >>>>>  atomic weights                                            awtab        !/*ol        131*/
  280 naw=(nwc+1)/2                                                             !/*ol        132*/
      return                                                                    !/*ol        133*/
!                                                                               !/*ol        134*/
! >>>>>  thermal times                                             thtme        !/*ol        135*/
  290 mxt=max(mxt,nwc)                                                          !/*ol        136*/
      return                                                                    !/*ol        137*/
!                                                                               !/*ol        138*/
! >>>>>  thermal s(a,b) data specifications                           mt        !/*ol        139*/
  300 indt=indt+nwc                                                             !/*ol        140*/
      return                                                                    !/*ol        141*/
!                                                                               !/*ol        142*/
! >>>>>  locations of kcode source points                           ksrc        !/*ol        143*/
  310 nsrc=nwc                                                                  !/*ol        144*/
      return                                                                    !/*ol        145*/
!                                                                               !/*ol4c       24*/
! >>>>>  weight window generator parameters                          wwg        !/*ol4c       25*/
  315 if(wwg(2).eq.0.)return                                                    !/*ol4c       26*/
      if(nwwma.eq.0)nwwma=mxa                                                   !/*ol4c       27*/
      if(nwgma.eq.0)nwgma=1                                                     !/*ol4c       28*/
      if(imesh(1).ne.0)nwgma=3*nint(wwma(7)+wwma(8)+wwma(9))+9                  !/*ol4c       29*/
      nwgeoa=3                                                                  !/*ol4c       30*/
      if(iwwg.ne.-1)iwwg=1                                                      !/*ol4c       31*/
      return                                                                    !/*ol4c       32*/
!                                                                               !/*ol        146*/
! >>>>>  energy bins for weight-window generator                    wwge        !/*ol        147*/
  320 do 330 i=1,mipt                                                           !/*ol        148*/
      if(nqp(i).ne.0)ngww(i)=10                                                 !/*ol        149*/
  330 if(nqp(i).ne.0.and.nwc.ne.0)ngww(i)=nwc                                   !/*ol        150*/
      return                                                                    !/*ol        151*/
!                                                                               !/*ol        152*/
! >>>>>  photon-production bias                                    pikmt        !/*ol        153*/
  340 npikmt=nwc                                                                !/*ol        154*/
      return                                                                    !/*ol        155*/
!                                                                               !/*ol4a        9*/
! >>>>>  ptrac specifications                                      ptrac        !/*ol4a       10*/
!        m2c=storage needed for ptr().                                          !/*ol4a       11*/
!        m4c=track storage portion of pts().                                    !/*ol4a       12*/
!        m5c=flag for particle data storage portion of pts().                   !/*ol4a       13*/
  350 iptra(1)=m2c                                                              !/*ol4a       14*/
      if(m5c.eq.0)m5c=1                                                         !/*ol4a       15*/
      iptra(2)=6+m5c+3**m5c                                                     !/*ol4a       16*/
      if(m4c.eq.0)m4c=100                                                       !/*ol4a       17*/
      iptra(2)=iptra(2)*(m4c+2)                                                 !/*ol4a       18*/
      return                                                                    !/*ol4a       19*/
!                                                                               !/*ol4c       33*/
! >>>>>  superimposed weight window generator mesh                  mesh        !/*ol4c       34*/
  360 j=0                                                                       !/*ol4c       35*/
      if(nwgeoa.eq.3)nwgma=3                                                    !/*ol4c       36*/
      do 370 i=2,nmkey                                                          !/*ol4c       37*/
  370 if(imesh(i).ne.0)j=1                                                      !/*ol4c       38*/
      iwwg=2                                                                    !/*ol4c       39*/
      if(j.eq.0)return                                                          !/*ol4c       40*/
      iwwg=1                                                                    !/*ol4c       41*/
      if(nwgeoa.eq.0)nwgeoa=1                                                   !/*ol4c       42*/
      do 380 i=1,3                                                              !/*ol4c       43*/
  380 if(wwma(i).eq.0.)wwma(i)=wwma(i+6)                                        !/*ol4c2      13*/
      nwwma=nint(wwma(1)*wwma(2)*wwma(3))                                       !/*ol4c       45*/
      if(nwgeoa.eq.3)nwwma=mxa                                                  !/*ol4c       46*/
      nwgma=3*nint(wwma(7)+wwma(8)+wwma(9))+9                                   !/*ol4c       47*/
      return                                                                    !/*ol4c       48*/
!                                                                               !/*olx        32*/
! >>>>>  Image detectors.                                     pi,tir,tic        !/*olx        33*/
  460 continue                                                                  !/*olx        34*/
#ifdef RADIOG                                                                   /*!olx        35*/
      ltd=ltd+19*(nwc/10)                                                       !/*olx        36*/
      if(nqw.eq.0)ndet(mod(icn/10,2)+1)=ndet(mod(icn/10,2)+1)+nwc/10            !/*olx        37*/
      do 465 i=1,mipt                                                           !/*olx        38*/
      if(nqp(i).eq.0)go to 465                                                  !/*olx        39*/
      ndet(i)=ndet(i)+nwc/10                                                    !/*olx        40*/
  465 continue                                                                  !/*olx        41*/
      go to 210                                                                 !/*olx        42*/
#else                                                                           /*!olx        43*/
      return                                                                    !/*olx        44*/
#endif                                                                          /*!olx        45*/
!                                                                               !/*olx        46*/
! >>>>> Secondary Particle Biasing.                                spabi        !/*olx        47*/
  480 continue                                                                  !/*olx        48*/
#ifdef SPABI                                                                    /*!olx        49*/
      is_spabi = is_spabi + count_spb_data()                                    !/*olxa        1*/
#endif                                                                          /*!olx        51*/
      return                                                                    !/*olx        52*/
!                                                                               !/*ol        156*/
! >>>>>  first spare card type                                        za        !/*ol        157*/
 9910 continue                                                                  !/*ol        158*/
      return                                                                    !/*ol        159*/
!                                                                               !/*ol        160*/
! >>>>>  second spare card type                                       zb        !/*ol        161*/
 9920 continue                                                                  !/*ol        162*/
      return                                                                    !/*ol        163*/
!                                                                               !/*ol        164*/
! >>>>>  third spare card type                                        zc        !/*ol        165*/
 9930 continue                                                                  !/*ol        166*/
      return                                                                    !/*ol        167*/
!                                                                               !/*ol        168*/
      end                                                                       !/*ol        169*/
