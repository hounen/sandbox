!     $Id: tpcrs.h,v 1.4 2002/03/27 18:14:18 gwm Exp $                        !/*tp          1*/
      parameter (lcrscl=2600)                                                   !/*tp          2*/
      common /tpcrs/ cs(29850), crsc(lcrscl), hsigmx(7), eth(4,4), dexs,        !/*tp          3*/
     & fnpsg(2,176), pipsg(2,126), locx(4,4)                                    !/*tp          4*/
      dimension dcln(80), dcin(115), ppac(19), poac(19), fmxsn(161),            !/*tp          5*/
     & fmxdn(130), fmxsp(117), pdci(60), pdch(55), dchn(143), dchna(36),        !/*tp          6*/
     & dchnb(60), pspcl(158), pdpcl(130), spcln(158), dpcln(130), fsln(1        !/*tp          7*/
     & 76), frinn(161), dmin(101), ppscl(117), pnscl(117), pmscl(117),          !/*tp          8*/
     & pnnsl(117), pcfsl(234), fripn(117), pnmi(101), pnfsl(234), pnec(1        !/*tp          9*/
     & 26), pnnec(126), pmxc(126), pmec(126), ppec(126), pec(176), ecn(1        !/*tp         10*/
     & 76), ppdc(6426), pmdd(6426), pmdx(6426), pndd(6426)                      !/*tp         11*/
      equivalence (cs(1),dcln(1)), (cs(81),dcin(1)), (cs(196),ppac(1)),         !/*tp         12*/
     & (cs(215),poac(1)), (cs(234),fmxsn(1)), (cs(395),fmxdn(1)), (cs(52        !/*tp         13*/
     & 5),fmxsp(1)), (cs(642),pdci(1)), (cs(702),pdch(1)), (cs(757),dchn        !/*tp         14*/
     & (1)), (cs(900),dchna(1)), (cs(936),dchnb(1)), (cs(996),pspcl(1)),        !/*tp         15*/
     & (cs(1154),pdpcl(1)), (cs(1284),spcln(1)), (cs(1442),dpcln(1)),           !/*tp         16*/
     & (cs(1572),fsln(1)), (cs(1748),frinn(1)), (cs(1909),dmin(1)), (cs(        !/*tp         17*/
     & 2010),ppscl(1)), (cs(2127),pnscl(1)), (cs(2244),pmscl(1)), (cs(23        !/*tp         18*/
     & 61),pnnsl(1)), (cs(2478),pcfsl(1)), (cs(2712),fripn(1)), (cs(2829        !/*tp         19*/
     & ),pnmi(1))                                                               !/*tp         20*/
      equivalence (cs(2930),pnfsl(1)), (cs(3164),pnec(1)), (cs(3290)            !/*tp         21*/
     & ,pnnec(1)), (cs(3416),pmxc(1)), (cs(3542),pmec(1)), (cs(3668)            !/*tp         22*/
     & ,ppec(1)), (cs(3794),pec(1)), (cs(3970),ecn(1)), (cs(4146),ppdc(1        !/*tp         23*/
     & )), (cs(10572),pmdd(1)), (cs(16998),pmdx(1)), (cs(23424),pndd(1))        !/*tp         24*/
