!#ident setdas.F from deck sd                                                   !/*sd          1*/
      subroutine setdas                                                         !/*sd          2*/
!        define offsets for the arrays in dynamically allocated storage.        !/*sd          3*/
!        do both the general storage and storage for imcn.                      !/*sd          4*/
      use global_data                                                           !/*sdx         1*/
      use imcn_data                                                             !/*sdx         2*/
!                                                                               !/*sdx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*sdx         4*/
      implicit integer(kindi) (i-n)                                             !/*sdx         5*/
!                                                                               !/*sdx         6*/
      dimension ks(4)                                                           !/*sd          7*/
#ifdef MESHTAL                                                                  /*!sdx         7*/
      dimension zd(10),jz(5)                                                    !/*sdx         8*/
#endif                                                                          /*!sdx         9*/
!                                                                               !/*sd          8*/
      msd=ksdef                                                                 !/*sd         10*/
      do 20 i=0,999                                                             !/*sd         11*/
      k=2**mod(i,31)                                                            !/*sd         12*/
      l=1+i/31                                                                  !/*sd         13*/
      do 10 j=1,4                                                               !/*sd         14*/
   10 ks(j)=mod(jsd(j,l)/k,2)                                                   !/*sd         15*/
      if(ks(1)+ks(2)+ks(3)+ks(4).ne.0)msd=msd+1                                 !/*sd         16*/
   20 if(ks(1)+ks(4).eq.2.or.ks(2)*(ks(3)+ks(4)).ne.0)msso1=msso1+mseb          !/*sdx        10*/
      do 30 i=1,ntal                                                            !/*sdx        11*/
   30 ktl1=ktl1+ktl(i,1)*ktl(i,2)                                               !/*sdx        12*/
      ltd=ltd+ktl1+10                                                           !/*sdx        13*/
      lit=lit+10                                                                !/*sd         22*/
      do 40 i=1,mipt                                                            !/*sd         23*/
      ndnd=ndnd+ndet(i)                                                         !/*sd         24*/
   40 nxnx=nxnx+ndx(i)                                                          !/*sd         25*/
      do 50 i=2,mipt+1                                                          !/*sd         26*/
      mww(i)=mww(i-1)+nww(i-1)                                                  !/*sd         27*/
   50 mgww(i)=mgww(i-1)+ngww(i-1)                                               !/*sd         28*/
!                                                                               !/*sdx        14*/
!        save all storage parameters into fixcom variables.                     !/*sdx        15*/
!        some may be eliminated if unchanged in the second pass.                !/*sdx        16*/
      ntal1=ntal                                                                !/*sdx        17*/
#ifdef MESHTAL                                                                  /*!sdx        18*/
      ntal1=ntal+nmesh                                                          !/*sdx        19*/
      if(nmesh.gt.0) then                                                       !/*sdx        20*/
        call gdtal(10,zero,zd,jz)                                               !/*sdx        21*/
        call gdtal(11,zero,zd,jz)                                               !/*sdx        22*/
        do 60 i=ntal+1,ntal1                                                    !/*sdx        23*/
   60   ktl1=ktl1+ktl(i,1)*ktl(i,2)                                             !/*sdx        24*/
        ltd=ltd+ktl1                                                            !/*sdx        25*/
      endif                                                                     !/*sdx        26*/
#endif                                                                          /*!sdx        27*/
#ifdef SPABI                                                                    /*!sdx        28*/
      main_spb_offset=1                                                         !/*sdx        29*/
      nspabi=length_of_spb()                                                    !/*sdx        30*/
#endif                                                                          /*!sdx        31*/
      nee3=nee(3)                                                               !/*sdx        32*/
      istrg3=0                                                                  !/*sdxa        1*/
      if(istrg(3).eq.1)istrg3=1                                                 !/*sdxa        2*/
      mix1=mix                                                                  !/*sdx        34*/
      iplt1=iplt                                                                !/*sdx        35*/
      mxa1=mxa                                                                  !/*sdx        36*/
      igm1=igm                                                                  !/*sdx        37*/
      npert1=npert                                                              !/*sdx        38*/
      mnnm1=mnnm                                                                !/*sdx        39*/
      nxnx1=nxnx                                                                !/*sdx        40*/
      msd1=msd                                                                  !/*sdx        41*/
      mbbm1=nint(bbrem(1))                                                      !/*sdx        42*/
      mgww1=mgww(mipt+1)                                                        !/*sdx        43*/
      mcal1=mcal                                                                !/*sdx        44*/
      npn1=npn                                                                  !/*sdx        45*/
      kpt1=kpt                                                                  !/*sdx        46*/
      ipert1=ipert                                                              !/*sdx        47*/
      nsc1=nsc                                                                  !/*sdx        48*/
      mxxs1=mxxs                                                                !/*sdx        49*/
      ltd1=ltd                                                                  !/*sdx        50*/
      mxt1=mxt                                                                  !/*sdx        51*/
      mxtr1=mxtr                                                                !/*sdx        52*/
      nlat1=nlat                                                                !/*sdx        53*/
      nvec1=nvec                                                                !/*sdx        54*/
      nwgm1=nwgm                                                                !/*sdx        55*/
      nwwm1=nwwm                                                                !/*sdx        56*/
      nwgma1=nwgma                                                              !/*sdx        57*/
      nwma1(1:3)=wwma(1:3)                                                      !/*sdx        58*/
      mlja1=mlja                                                                !/*sdx        59*/
      mxj1=mxj                                                                  !/*sdx        60*/
      mww1=mww(mipt+1)                                                          !/*sdx        61*/
      njsx1=njsx(1)                                                             !/*sdx        62*/
      lit1=lit                                                                  !/*sdx        63*/
      njsr1=njsr(1)                                                             !/*sdx        64*/
      nilr1=nilr(1)                                                             !/*sdx        65*/
      njsw1=njsw                                                                !/*sdx        66*/
      niwr1=niwr                                                                !/*sdx        67*/
      mssc1=mssc                                                                !/*sdx        68*/
      mjss1=mjss                                                                !/*sdx        69*/
      nilw1=nilw                                                                !/*sdx        70*/
      junf1=junf                                                                !/*sdx        71*/
      mkcp1=mkcp                                                                !/*sdx        72*/
      mlaf1=mlaf                                                                !/*sdx        73*/
      mxafs1=mxafs                                                              !/*sdx        74*/
      mlja1=mlja                                                                !/*sdx        75*/
      nmzu1=nmzu                                                                !/*sdx        76*/
      mipts1=mipts                                                              !/*sdx        77*/
      mlaj1=mlaj                                                                !/*sdx        78*/
      ndnd1=ndnd                                                                !/*sdx        79*/
      icw1=icw                                                                  !/*sdx        80*/
      msrk1=msrk                                                                !/*sdx        81*/
      nwwma1=nwwma                                                              !/*sdx        82*/
      nmaz1=nmaz                                                                !/*sdx        83*/
      njss1=njss                                                                !/*sdx        84*/
      npikmt1=npikmt                                                            !/*sdx        85*/
      iptra1=iptra(1)                                                           !/*sdx        86*/
      iptra2=iptra(2)                                                           !/*sdx        87*/
      kfl1=kfl                                                                  !/*sdx        88*/
      indt1=indt                                                                !/*sdx        89*/
      ndup1=ndup                                                                !/*sdx        90*/
      lrt1=lrt                                                                  !/*sdx        91*/
      naw1=naw                                                                  !/*sdx        92*/
      nmfm1=nmfm                                                                !/*sdx        93*/
      ides1=ides                                                                !/*sdx        94*/
      mrkp1=mrkp                                                                !/*sdx        95*/
      kjaq1=kjaq                                                                !/*sdx        96*/
!                                                                               !/*sd4c       87*/
!        set multitasking pointers to sequential pointers for sequential        !/*sd4c       88*/
!        problems and multitasking problems not calling trnspt/utask            !/*sd4c       89*/
!        such as pvm plotting problems.                                         !/*sd4c       90*/
      do 110 i=1,ltskpt-1                                                       !/*sd4c       91*/
  110 ktskpt(i)=lpntcm(i)                                                       !/*sd4c       92*/
      return                                                                    !/*sd        228*/
      end                                                                       !/*sd        229*/
