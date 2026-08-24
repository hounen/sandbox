!#ident pconst.F from deck ot4a                                                 !/*ot4a        1*/
      subroutine pconst                                                         !/*ot4a        2*/
!        print physical constants and compilation options.                      !/*ot4a        3*/
      use global_data                                                           !/*otx         1*/
!                                                                               !/*otx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*otx         3*/
      implicit integer(kindi) (i-n)                                             !/*otx         4*/
!                                                                               !/*otx         5*/
!                                                                               !/*ot4a        5*/
      write(iuo,'(19h1physical constants,85x,14hprint table 98//)')             !/*ot4a        6*/
      write(iuo,'(3x,4hname,20x,5hvalue,5x,11hdescription/)')                   !/*ot4a        7*/
      write(iuo,'(3x,4hhuge,1p1e25.13,5x,8hinfinity)')huge                      !/*ot4a        8*/
      write(iuo,'(4x,3hpie,1p1e25.13,5x,2hpi)')pie                              !/*ot4a        9*/
      write(iuo,'(2x,5heuler,1p1e25.13,5x,14heuler constant)')euler             !/*ot4b        1*/
      write(iuo,'(1x,6havogad,1p1e25.13,5x,                                     !/*ot4a       10*/
     & 32havogadro number (molecules/mole))')avogad                             !/*ot4a       11*/
      write(iuo,'(2x,5haneut,1p1e25.13,5x,18hneutron mass (amu))')aneut         !/*ot4a       12*/
      write(iuo,'(2x,5havgdn,1p1e25.13,5x,9havogadro ,'//                       !/*ot4a       13*/
     & '47hnumber/neutron mass (1.e-24*molecules/mole/amu))')avgdn              !/*ot4a       14*/
      write(iuo,'(2x,5hslite,1p1e25.13,5x,'//                                   !/*ot4a       15*/
     & '25hspeed of light (cm/shake))')slite                                    !/*ot4a       16*/
      write(iuo,'(1x,6hplanck,1p1e25.13,5x,'//                                  !/*ot4a       17*/
     & '27hplanck constant (mev shake))')planck                                 !/*ot4a       18*/
      write(iuo,'(2x,5hfscon,1p1e25.13,5x,'//                                   !/*ot4a       19*/
     & '47hinverse fine structure constant h*c/(2*pi*e**2))')fscon              !/*ot4a       20*/
      write(iuo,'(7h gpt(1),1p1e25.13,5x,18hneutron mass (mev))')gpt(1)         !/*ot4a       21*/
      write(iuo,'(7h gpt(3),1p1e25.13,5x,19helectron mass (mev))')gpt(3)        !/*ot4a       22*/
!                                                                               !/*ot4a       23*/
      write(iuo,'(//18h fission q-values:,5x,7hnuclide,4x,6hq(mev),'//          !/*ot4a       24*/
     & '3x,7hnuclide,4x,6hq(mev))')                                             !/*ot4a       25*/
      write(iuo,'(20x,i10,f10.2,i10,f10.2)')(mfiss(i),qfiss(i),i=1,22)          !/*ot4a       26*/
      write(iuo,'(25x,5hother,f10.2)')qfiss(23)                                 !/*ot4a       27*/
!                                                                               !/*ot4a       28*/
!        write compilation options and datapath.                                !/*otx         6*/
      if(mct.lt.0)return                                                        !/*ot4a       30*/
      write(iuo,'(//45h the following compilation options were used:/)')        !/*ot4a       31*/
#ifdef CHEAP                                                                    /*!ot4a       36*/
      write(iuo,'(5x,5hCHEAP)')                                                 !/*otx         7*/
#endif                                                                          /*!cpp       198*/
#ifdef UNICOS                                                                   /*!ot4a       40*/
      write(iuo,'(5x,6hUNICOS)')                                                !/*otx         8*/
#endif                                                                          /*!cpp       199*/
#ifdef UNIX                                                                     /*!ot4a       44*/
      write(iuo,'(5x,4hUNIX)')                                                  !/*otx         9*/
#endif                                                                          /*!cpp       200*/
#ifdef AIX                                                                      /*!ot4a       46*/
      write(iuo,'(5x,3hAIX)')                                                   !/*otx        10*/
#endif                                                                          /*!cpp       201*/
#ifdef HPUX                                                                     /*!ot4a       48*/
      write(iuo,'(5x,4hHPUX)')                                                  !/*otx        11*/
#endif                                                                          /*!cpp       202*/
#ifdef SUN                                                                      /*!ot4a       50*/
      write(iuo,'(5x,3hSUN)')                                                   !/*otx        12*/
#endif                                                                          /*!cpp       203*/
#ifdef DEC                                                                      /*!ot4a       52*/
      write(iuo,'(5x,3hDEC)')                                                   !/*otx        13*/
#endif                                                                          /*!cpp       204*/
#ifdef SGI                                                                      /*!otx        14*/
      write(iuo,'(5x,3hSGI)')                                                   !/*otx        15*/
#endif                                                                          /*!otx        16*/
#ifdef PLOT                                                                     /*!ot4a       76*/
      write(iuo,'(5x,4hPLOT)')                                                  !/*otx        17*/
#endif                                                                          /*!cpp       206*/
#ifdef MCPLOT                                                                   /*!ot4a       78*/
      write(iuo,'(5x,6hMCPLOT)')                                                !/*otx        18*/
#endif                                                                          /*!cpp       207*/
#ifdef GKSSIM                                                                   /*!ot4a       80*/
      write(iuo,'(5x,6hGKSSIM)')                                                !/*otx        19*/
#endif                                                                          /*!cpp       208*/
#ifdef CGS                                                                      /*!ot4a       84*/
      write(iuo,'(5x,3hCGS)')                                                   !/*otx        20*/
#endif                                                                          /*!cpp       209*/
#ifdef XLIB                                                                     /*!ot4a       86*/
      write(iuo,'(5x,4hXLIB)')                                                  !/*otx        21*/
#endif                                                                          /*!cpp       210*/
#ifdef SUNCGS                                                                   /*!ot4a       88*/
      write(iuo,'(5x,6hSUNCGS)')                                                !/*otx        22*/
#endif                                                                          /*!cpp       211*/
#ifdef DISSPLA                                                                  /*!ot4a       92*/
      write(iuo,'(5x,7hDISSPLA)')                                               !/*otx        23*/
#endif                                                                          /*!cpp       212*/
#ifdef DISSCGS                                                                  /*!ot4a       94*/
      write(iuo,'(5x,7hDISSCGS)')                                               !/*otx        24*/
#endif                                                                          /*!cpp       213*/
#ifdef PCDOS                                                                    /*!ot4b        2*/
      write(iuo,'(5x,5hPCDOS)')                                                 !/*otx        25*/
#endif                                                                          /*!cpp       214*/
#ifdef LAHEY                                                                    /*!ot4b        4*/
      write(iuo,'(5x,5hLAHEY)')                                                 !/*otx        26*/
#endif                                                                          /*!cpp       215*/
#ifdef QWIN                                                                     /*!ot4c        1*/
      write(iuo,'(4hQWIN)')                                                     !/*otx        27*/
#endif                                                                          /*!cpp       216*/
#ifdef LP64                                                                     /*!ot4b        8*/
      write(iuo,'(5x,4hLP64)')                                                  !/*otx        28*/
#endif                                                                          /*!cpp       217*/
#ifdef LINUX                                                                    /*!ot4b       10*/
      write(iuo,'(5x,5hLINUX)')                                                 !/*otx        29*/
#endif                                                                          /*!cpp       218*/
#ifdef MULTT                                                                    /*!ot4a      102*/
      write(iuo,'(5x,5hMULTT)')                                                 !/*otx        30*/
#endif                                                                          /*!cpp       219*/
#ifdef MULTP                                                                    /*!ot4a      104*/
      write(iuo,'(5x,5hMULTP)')                                                 !/*otx        31*/
#endif                                                                          /*!cpp       220*/
#ifdef PVM                                                                      /*!ot4a      106*/
      write(iuo,'(5x,3hPVM)')                                                   !/*otx        32*/
#endif                                                                          /*!cpp       221*/
#ifdef XS64                                                                     /*!ot4a      108*/
      write(iuo,'(5x,4hXS64)')                                                  !/*otx        33*/
#endif                                                                          /*!cpp       222*/
#ifdef OLDM                                                                     /*!otx        34*/
      write(iuo,'(5x,4hOLDM)')                                                  !/*otx        35*/
#endif                                                                          /*!otx        36*/
#ifdef XSPLOT                                                                   /*!otx        37*/
      write(iuo,'(5x,6hXSPLOT)')                                                !/*otx        38*/
#endif                                                                          /*!otx        39*/
#ifdef BIG                                                                      /*!otx        40*/
      write(iuo,'(5x,3hBIG)')                                                   !/*otx        41*/
#endif                                                                          /*!otx        42*/
#ifdef CEM                                                                      /*!otx        43*/
      write(iuo,'(5x,3hCEM)')                                                   !/*otx        44*/
#endif                                                                          /*!otx        45*/
#ifdef G77                                                                      /*!otx        46*/
      write(iuo,'(5x,3hG77)')                                                   !/*otx        47*/
#endif                                                                          /*!otx        48*/
#ifdef HISTP                                                                    /*!otx        49*/
      write(iuo,'(5x,5hHISTP)')                                                 !/*otx        50*/
#endif                                                                          /*!otx        51*/
#ifdef HT13                                                                     /*!otx        52*/
      write(iuo,'(5x,4hHT13)')                                                  !/*otx        53*/
#endif                                                                          /*!otx        54*/
#ifdef HTPLOT                                                                   /*!otx        55*/
      write(iuo,'(5x,6hHTPLOT)')                                                !/*otx        56*/
#endif                                                                          /*!otx        57*/
#ifdef LCS                                                                      /*!otx        58*/
      write(iuo,'(5x,3hLCS)')                                                   !/*otx        59*/
#endif                                                                          /*!otx        60*/
#ifdef LCS_PRINT                                                                /*!otx        61*/
      write(iuo,'(5x,9hLCS_PRINT)')                                             !/*otx        62*/
#endif                                                                          /*!otx        63*/
#ifdef MESHTAL                                                                  /*!otx        64*/
      write(iuo,'(5x,7hMESHTAL)')                                               !/*otx        65*/
#endif                                                                          /*!otx        66*/
#ifdef NO_PAW                                                                   /*!otx        67*/
      write(iuo,'(5x,6hNO_PAW)')                                                !/*otx        68*/
#endif                                                                          /*!otx        69*/
#ifdef NPM                                                                      /*!otx        70*/
      write(iuo,'(5x,3hNPM)')                                                   !/*otx        71*/
#endif                                                                          /*!otx        72*/
#ifdef ORIHET                                                                   /*!otx        73*/
      write(iuo,'(5x,6hORIHET)')                                                !/*otx        74*/
#endif                                                                          /*!otx        75*/
#ifdef RADIOG                                                                   /*!otx        76*/
      write(iuo,'(5x,6hRADIOG)')                                                !/*otx        77*/
#endif                                                                          /*!otx        78*/
#ifdef SPABI                                                                    /*!otx        79*/
      write(iuo,'(5x,5hSPABI)')                                                 !/*otx        80*/
#endif                                                                          /*!otx        81*/
      write(iuo,'(5x,18hdefault datapath: ,a80)')hdpath                         !/*ot4a      110*/
      if(hdpath.ne.hdpth)write(iuo,'(23x,a80)')hdpth                            !/*ot4a      111*/
      return                                                                    !/*ot4a      112*/
      end                                                                       !/*ot4a      113*/
