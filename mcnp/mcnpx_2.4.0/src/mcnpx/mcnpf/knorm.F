!#ident knorm.F from deck km4a                                                  !/*km4a        1*/
      subroutine knorm(nt,ik,hz,zk,je)                                          !/*km4a        2*/
!        check for normality of keff values at 95% and 99% confidence.          !/*km4a        3*/
!        called from kprint(if >4 active cycles exist), kbatch, kskcyc.         !/*km4a        4*/
!                                                                               !/*km4a        5*/
!        use w test in ref. 1 for less than 51 keff values.                     !/*km4a        6*/
!        use d test in ref. 2 for more than 50 keff values.                     !/*km4a        7*/
! ref. 1: shapiro and wilk, biometrika, 52, p. 591 (1965).                      !/*kmx         1*/
!   "an analysis of variance test for normality,"                               !/*kmx         2*/
! ref. 2: d'agostino, biometrika, 58, p. 341 (1971).                            !/*kmx         3*/
!   "an omnibus test of normality for moderate and large size samples,"         !/*kmx         4*/
!                                                                               !/*km4a       12*/
!        ik is the number cycles skipped (nt<3) or values in zk (nt=3).         !/*km4a       13*/
!        ns is the amount of storage in xx for keff cycle values.               !/*km4a       15*/
!        nt is the type normalization test required:                            !/*km4c        1*/
!          = 1 is the test and printout for keff results summary page;          !/*km4a       17*/
!          = 2 is test on different cycles skipped data without prints;         !/*km4a       18*/
!          = 3 is test on different batched keff data without prints.           !/*km4a       19*/
!        zk contains the batched keff data from subroutine kbatch, nt=3.        !/*km4a       20*/
!        je is number of estimators that do not appear normal for nt=1.         !/*km4a       21*/
!        hz = xx/xx/xx col,abs,tl where xx=95,99,or no (for not normal).        !/*km4a       22*/
!                                                                               !/*km4a       23*/
      use global_data                                                           !/*kmx         7*/
!                                                                               !/*kmx         8*/
      implicit real(kindr) (a-h,o-z)                                            !/*kmx         9*/
      implicit integer(kindi) (i-n)                                             !/*kmx        10*/
!                                                                               !/*kmx        11*/
      parameter (ns=nmrkp)                                                      !/*km4c        4*/
      dimension a(25,48),c(50,2),f(2,2,24),fn(24),xx(ns),zk(3,*)                !/*km4a       26*/
      save xx                                                                   !/*km4c        5*/
      character hk(3)*13,hp(2)*2,hy(2)*23,hz*8                                  !/*km4a       27*/
      data hk/'k( collision)','k(absorption)','k(trk length)'/,                 !/*km4a       28*/
     & hp/'95','99'/,hy/' ',', but not at 95 percent'/                          !/*km4a       29*/
!                                                                               !/*km4a       30*/
!        coefficients for the w test from table 5, p. 603 in ref. 1.            !/*km4a       31*/
!        the number of samples n is shifted down two: n is in a(i,n-2).         !/*km4a       32*/
      data (a(i,1),i=1,25)/.7071,24*0./                                         !/*km4a       33*/
      data (a(i,2),i=1,25)/.6872,.1667,23*0./                                   !/*km4a       34*/
      data (a(i,3),i=1,25)/.6646,.2413,23*0./                                   !/*km4a       35*/
      data (a(i,4),i=1,25)/.6431,.2806,.0875,22*0./                             !/*km4a       36*/
      data (a(i,5),i=1,25)/.6233,.3031,.1401,22*0./                             !/*km4a       37*/
      data (a(i,6),i=1,25)/.6052,.3164,.1743,.0561,21*0./                       !/*km4a       38*/
      data (a(i,7),i=1,25)/.5888,.3244,.1976,.0947,21*0./                       !/*km4a       39*/
      data (a(i,8),i=1,25)/.5739,.3291,.2141,.1224,.0399,20*0./                 !/*km4a       40*/
      data (a(i,9),i=1,25)/.5601,.3315,.2260,.1429,.0695,20*0./                 !/*km4a       41*/
      data (a(i,10),i=1,25)/.5475,.3325,.2347,.1586,.0922,.0303,19*0./          !/*km4a       42*/
      data (a(i,11),i=1,25)/.5359,.3325,.2412,.1707,.1099,.0539,19*0./          !/*km4a       43*/
      data (a(i,12),i=1,25)/.5251,.3318,.2460,.1802,.1240,.0727,.0240,          !/*km4a       44*/
     & 18*0./                                                                   !/*km4a       45*/
      data (a(i,13),i=1,25)/.5150,.3306,.2495,.1878,.1353,.0880,.0433,          !/*km4a       46*/
     & 18*0./                                                                   !/*km4a       47*/
      data (a(i,14),i=1,25)/.5056,.3290,.2521,.1939,.1447,.1005,.0593,          !/*km4a       48*/
     & .0196,17*0./                                                             !/*km4a       49*/
      data (a(i,15),i=1,25)/.4968,.3273,.2540,.1988,.1524,.1109,.0725,          !/*km4a       50*/
     & .0359,17*0./                                                             !/*km4a       51*/
      data (a(i,16),i=1,25)/.4886,.3253,.2553,.2027,.1587,.1197,.0837,          !/*km4a       52*/
     & .0496,.0163,16*0./                                                       !/*km4a       53*/
      data (a(i,17),i=1,25)/.4808,.3232,.2561,.2059,.1641,.1271,.0932,          !/*km4a       54*/
     & .0612,.0303,16*0./                                                       !/*km4a       55*/
      data (a(i,18),i=1,25)/.4734,.3211,.2565,.2085,.1686,.1334,.1013,          !/*km4a       56*/
     & .0711,.0422,.0140,15*0./                                                 !/*km4a       57*/
      data (a(i,19),i=1,25)/.4643,.3185,.2578,.2119,.1736,.1399,.1092,          !/*km4a       58*/
     & .0804,.0530,.0263,15*0./                                                 !/*km4a       59*/
      data (a(i,20),i=1,25)/.4590,.3156,.2571,.2131,.1764,.1443,.1150,          !/*km4a       60*/
     & .0878,.0618,.0368,.0122,14*0./                                           !/*km4a       61*/
      data (a(i,21),i=1,25)/.4542,.3126,.2563,.2139,.1787,.1480,.1201,          !/*km4a       62*/
     & .0941,.0696,.0459,.0228,14*0./                                           !/*km4a       63*/
      data (a(i,22),i=1,25)/.4493,.3098,.2554,.2145,.1807,.1512,.1245,          !/*km4a       64*/
     & .0997,.0764,.0539,.0321,.0107,13*0./                                     !/*km4a       65*/
      data (a(i,23),i=1,25)/.4450,.3069,.2543,.2148,.1822,.1539,.1283,          !/*km4a       66*/
     & .1046,.0823,.0610,.0403,.0200,13*0./                                     !/*km4a       67*/
      data (a(i,24),i=1,25)/.4407,.3043,.2533,.2151,.1836,.1563,.1316,          !/*km4a       68*/
     & .1089,.0876,.0672,.0476,.0284,.0094,12*0./                               !/*km4a       69*/
      data (a(i,25),i=1,25)/.4366,.3018,.2522,.2152,.1848,.1584,.1346,          !/*km4a       70*/
     & .1128,.0923,.0728,.0540,.0358,.0178,12*0./                               !/*km4a       71*/
      data (a(i,26),i=1,25)/.4328,.2992,.2510,.2151,.1857,.1601,.1372,          !/*km4a       72*/
     & .1162,.0965,.0778,.0598,.0424,.0253,.0084,11*0./                         !/*km4a       73*/
      data (a(i,27),i=1,25)/.4291,.2968,.2499,.2150,.1864,.1616,.1395,          !/*km4a       74*/
     & .1192,.1002,.0822,.0650,.0483,.0320,.0159,11*0./                         !/*km4a       75*/
      data (a(i,28),i=1,25)/.4254,.2944,.2487,.2148,.1870,.1630,.1415,          !/*km4a       76*/
     & .1219,.1036,.0862,.0697,.0537,.0381,.0227,.0076,10*0./                   !/*km4a       77*/
      data (a(i,29),i=1,25)/.4220,.2921,.2475,.2145,.1875,.1641,.1433,          !/*km4a       78*/
     & .1243,.1066,.0899,.0739,.0585,.0435,.0289,.0144,10*0./                   !/*km4a       79*/
      data (a(i,30),i=1,25)/.4188,.2898,.2463,.2141,.1878,.1651,.1449,          !/*km4a       80*/
     & .1265,.1093,.0931,.0777,.0629,.0485,.0344,.0206,.0068,9*0./              !/*km4a       81*/
      data (a(i,31),i=1,25)/.4156,.2876,.2451,.2137,.1880,.1660,.1463,          !/*km4a       82*/
     & .1284,.1118,.0961,.0812,.0669,.0530,.0395,.0262,.0131,9*0./              !/*km4a       83*/
      data (a(i,32),i=1,25)/.4127,.2854,.2439,.2132,.1882,.1667,.1475,          !/*km4a       84*/
     & .1301,.1140,.0988,.0844,.0706,.0572,.0441,.0314,.0187,.0062,8*0./        !/*km4a       85*/
      data (a(i,33),i=1,25)/.4096,.2834,.2427,.2127,.1883,.1673,.1487,          !/*km4a       86*/
     & .1317,.1160,.1013,.0873,.0739,.0610,.0484,.0361,.0239,.0119,8*0./        !/*km4a       87*/
      data (a(i,34),i=1,25)/.4068,.2813,.2415,.2121,.1883,.1678,.1496,          !/*km4a       88*/
     & .1331,.1179,.1036,.0900,.0770,.0645,.0523,.0404,.0287,.0172,             !/*km4a       89*/
     & .0057,7*0./                                                              !/*km4a       90*/
      data (a(i,35),i=1,25)/.4040,.2794,.2403,.2116,.1883,.1683,.1505,          !/*km4a       91*/
     & .1344,.1196,.1056,.0924,.0798,.0677,.0559,.0444,.0331,.0220,             !/*km4a       92*/
     & .0110,7*0./                                                              !/*km4a       93*/
      data (a(i,36),i=1,25)/.4015,.2774,.2391,.2110,.1881,.1686,.1513,          !/*km4a       94*/
     & .1356,.1211,.1075,.0947,.0824,.0706,.0592,.0481,.0372,.0264,             !/*km4a       95*/
     & .0158,.0053,6*0./                                                        !/*km4a       96*/
      data (a(i,37),i=1,25)/.3989,.2755,.2380,.2104,.1880,.1689,.1520,          !/*km4a       97*/
     & .1366,.1225,.1092,.0967,.0848,.0733,.0622,.0515,.0409,.0305,             !/*km4a       98*/
     & .0203,.0101,6*0./                                                        !/*km4a       99*/
      data (a(i,38),i=1,25)/.3964,.2737,.2368,.2098,.1878,.1691,.1526,          !/*km4a      100*/
     & .1376,.1237,.1108,.0986,.0870,.0759,.0651,.0546,.0444,.0343,             !/*km4a      101*/
     & .0244,.0146,.0049,5*0./                                                  !/*km4a      102*/
      data (a(i,39),i=1,25)/.3940,.2719,.2357,.2091,.1876,.1693,.1531,          !/*km4a      103*/
     & .1384,.1249,.1123,.1004,.0891,.0782,.0677,.0575,.0476,.0379,             !/*km4a      104*/
     & .0283,.0188,.0094,5*0./                                                  !/*km4a      105*/
      data (a(i,40),i=1,25)/.3917,.2701,.2345,.2085,.1874,.1694,.1535,          !/*km4a      106*/
     & .1392,.1259,.1136,.1020,.0909,.0804,.0701,.0602,.0506,.0411,             !/*km4a      107*/
     & .0318,.0227,.0136,.0045,4*0./                                            !/*km4a      108*/
      data (a(i,41),i=1,25)/.3894,.2684,.2334,.2078,.1871,.1695,.1539,          !/*km4a      109*/
     & .1398,.1269,.1149,.1035,.0927,.0824,.0724,.0628,.0534,.0442,             !/*km4a      110*/
     & .0352,.0263,.0175,.0087,4*0./                                            !/*km4a      111*/
      data (a(i,42),i=1,25)/.3872,.2667,.2323,.2072,.1868,.1695,.1542,          !/*km4a      112*/
     & .1405,.1278,.1160,.1049,.0943,.0842,.0745,.0651,.0560,.0471,             !/*km4a      113*/
     & .0383,.0296,.0211,.0216,.0042,3*0./                                      !/*km4a      114*/
      data (a(i,43),i=1,25)/.3850,.2651,.2313,.2065,.1865,.1695,.1545,          !/*km4a      115*/
     & .1410,.1286,.1170,.1062,.0959,.0860,.0765,.0673,.0584,.0497,             !/*km4a      116*/
     & .0412,.0328,.0245,.0163,.0081,3*0./                                      !/*km4a      117*/
      data (a(i,44),i=1,25)/.3830,.2635,.2302,.2058,.1862,.1695,.1548,          !/*km4a      118*/
     & .1415,.1293,.1180,.1073,.0972,.0876,.0783,.0694,.0607,.0522,             !/*km4a      119*/
     & .0439,.0357,.0277,.0197,.0118,.0039,2*0./                                !/*km4a      120*/
      data (a(i,45),i=1,25)/.3808,.2620,.2291,.2052,.1859,.1695,.1550,          !/*km4a      121*/
     & .1420,.1300,.1189,.1085,.0986,.0892,.0801,.0713,.0628,.0546,             !/*km4a      122*/
     & .0465,.0385,.0307,.0229,.0153,.0076,2*0./                                !/*km4a      123*/
      data (a(i,46),i=1,25)/.3789,.2604,.2281,.2045,.1855,.1693,.1551,          !/*km4a      124*/
     & .1423,.1306,.1197,.1095,.0998,.0906,.0817,.0731,.0648,.0568,             !/*km4a      125*/
     & .0489,.0411,.0335,.0259,.0185,.0111,.0037,.0000/                         !/*km4a      126*/
      data (a(i,47),i=1,25)/.3770,.2589,.2271,.2038,.1851,.1692,.1553,          !/*km4a      127*/
     & .1427,.1312,.1205,.1105,.1010,.0919,.0832,.0748,.0667,.0588,             !/*km4a      128*/
     & .0511,.0436,.0361,.0288,.0215,.0143,.0071,0./                            !/*km4a      129*/
      data (a(i,48),i=1,25)/.3751,.2574,.2260,.2032,.1847,.1691,.1554,          !/*km4a      130*/
     &.1430,.1317,.1212,.1113,.1020,.0932,.0846,.0764,.0685,.0608,.0532,        !/*km4a      131*/
     &.0459,.0386,.0314,.0244,.0174,.0104,.0035/                                !/*km4a      132*/
!                                                                               !/*km4a      133*/
!        95 and 99 percentile levels for w test from table 6,p 605,ref 1        !/*km4a      134*/
      data (c(i,1),i=3,50)/.767,.748,.762,.788,.803,.818,.829,.842,.850,        !/*km4a      135*/
     & .859,.866,.874,.881,.887,.892,.897,.901,.905,.908,.911,.914,.916,        !/*km4a      136*/
     & .918,.920,.923,.924,.926,.927,.929,.930,.931,.933,.934,.935,.936,        !/*km4a      137*/
     & .938,.939,.940,.941,.942,.943,.944,.945,.945,.946,.947,.947,.947/        !/*km4a      138*/
      data (c(i,2),i=3,50)/.753,.687,.686,.713,.730,.749,.764,.781,.792,        !/*km4a      139*/
     & .805,.814,.825,.835,.844,.851,.858,.863,.868,.873,.878,.881,.884,        !/*km4a      140*/
     & .888,.891,.894,.896,.898,.900,.902,.904,.906,.908,.910,.912,.914,        !/*km4a      141*/
     & .916,.917,.919,.920,.922,.923,.924,.926,.927,.928,.929,.929,.930/        !/*km4a      142*/
!                                                                               !/*km4a      143*/
!        95 and 99 percentile ranges for d test from table 1,p 343,ref 2        !/*km4a      144*/
      data (f(1,1,i),i=1,24)/-2.757,-2.699,-2.652,-2.613,-2.580,-2.552,         !/*km4a      145*/
     &  -2.452,-2.391,-2.348,-2.316,-2.291,-2.270,-2.253,-2.239,-2.226,         !/*km4a      146*/
     &  -2.215,-2.206,-2.197,-2.189,-2.182,-2.176,-2.170,-2.164,-2.159/         !/*km4a      147*/
      data (f(2,1,i),i=1,24)/ 1.038, 1.115, 1.176, 1.226, 1.268, 1.303,         !/*km4a      148*/
     &   1.423, 1.496, 1.545, 1.528, 1.610, 1.633, 1.652, 1.668, 1.682,         !/*km4a      149*/
     &   1.694, 1.704, 1.714, 1.722, 1.730, 1.737, 1.743, 1.749, 1.754/         !/*km4a      150*/
      data (f(1,2,i),i=1,24)/-3.949,-3.846,-3.762,-3.693,-3.635,-3.584,         !/*km4a      151*/
     &  -3.409,-3.302,-3.227,-3.172,-3.129,-3.094,-3.064,-3.040,-3.019,         !/*km4a      152*/
     &  -3.000,-2.984,-2.969,-2.956,-2.944,-2.933,-2.923,-2.914,-2.906/         !/*km4a      153*/
      data (f(2,2,i),i=1,24)/ 1.192, 1.301, 1.388, 1.459, 1.518, 1.569,         !/*km4a      154*/
     &   1.746, 1.853, 1.927, 1.983, 2.026, 2.061, 2.090, 2.144, 2.136,         !/*km4a      155*/
     &   2.154, 2.171, 2.185, 2.199, 2.211, 2.221, 2.231, 2.241, 2.249/         !/*km4a      156*/
!                                                                               !/*km4a      157*/
!        24 values of sample sizes that accompany the d test data above.        !/*km4a      158*/
      data fn/50.,60.,70.,80.,90.,100.,150.,200.,250.,300.,350.,400.,           !/*km4a      159*/
     & 450.,500.,550.,600.,650.,700.,750.,800.,850.,900.,950.,1000./            !/*km4a      160*/
!                                                                               !/*km4a      161*/
!        define number of active cycles n with keff values stored.              !/*km4a      162*/
      mn=min(kcz,mrkp)                                                          !/*km4a      163*/
      n=mn-ik                                                                   !/*km4a      164*/
      if(nt.eq.3)n=ik                                                           !/*km4a      165*/
      hz='no/no/no'                                                             !/*km4a      166*/
      je=0                                                                      !/*km4a      167*/
      if(nt.gt.1)go to 40                                                       !/*km4c        6*/
!                                                                               !/*km4a      169*/
!        check for storage limitations on the normality check.                  !/*km4a      170*/
      if(mn.ne.kcz.and.n.le.ns)write(iuo,10)mrkp-ikz,mrkp                       !/*km4a      171*/
   10 format(45h the w normality test is limited to the first,i5,               !/*km4a      172*/
     & 52h active keff cycles because the mrkp storage is only,i5)              !/*km4a      173*/
      if(n.gt.ns)write(iuo,20)ns                                                !/*km4a      174*/
   20 format(45h the w normality test is limited to the first,i5,               !/*km4a      175*/
     & 61h active keff cycles, increase parameter ns in knorm for more.)        !/*km4a      176*/
!                                                                               !/*km4a      177*/
!        print a title line for the normality checks.                           !/*km4a      178*/
      if(nt.eq.1)write(iuo,30)                                                  !/*km4c        7*/
   30 format(55h the results of the w test for normality applied to the,        !/*km4a      180*/
     & 56h individual collision, absorption, and track-length keff,             !/*km4a      181*/
     & 18h cycle values are:/)                                                  !/*km4a      182*/
!                                                                               !/*km4a      183*/
!        loop over the three keff estimators to test for normality.             !/*km4a      184*/
   40 np=min(n,ns)                                                              !/*km4a      185*/
      io=0                                                                      !/*km4c        8*/
      if(nt.le.0)io=19                                                          !/*km4c        9*/
      do 280 ke=1,3                                                             !/*km4a      186*/
      if(nt.lt.3)go to 60                                                       !/*km4a      187*/
      do 50 i=1,ik                                                              !/*km4a      188*/
   50 xx(i)=zk(ke,i)                                                            !/*km4a      189*/
      go to 80                                                                  !/*km4a      190*/
   60 do 70 i=1,np                                                              !/*km4a      191*/
   70 xx(i)=rkpl(lrkp+ke+io,i+ik)                                               !/*km4c       10*/
   80 ir=np                                                                     !/*km4a      193*/
      l=ir/2+1                                                                  !/*km4a      194*/
      go to 100                                                                 !/*km4a      195*/
!                                                                               !/*km4a      196*/
!        perform the sort using the heapsort (nlogn) algorithm.                 !/*km4a      197*/
   90 xx(ii)=x                                                                  !/*km4a      198*/
  100 if(l.le.1)go to 150                                                       !/*km4a      199*/
      l=l-1                                                                     !/*km4a      200*/
      x=xx(l)                                                                   !/*km4a      201*/
  110 ii=l                                                                      !/*km4a      202*/
      j=l+l                                                                     !/*km4a      203*/
      if(j-ir)120,130,90                                                        !/*km4a      204*/
  120 if(xx(j).lt.xx(j+1))j=j+1                                                 !/*km4a      205*/
  130 if(x.ge.xx(j))go to 140                                                   !/*km4a      206*/
      xx(ii)=xx(j)                                                              !/*km4a      207*/
      ii=j                                                                      !/*km4a      208*/
      j=j+j                                                                     !/*km4a      209*/
      if(j-ir)120,130,90                                                        !/*km4a      210*/
  140 j=ir+1                                                                    !/*km4a      211*/
      if(j-ir)120,130,90                                                        !/*km4a      212*/
  150 x=xx(ir)                                                                  !/*km4a      213*/
      xx(ir)=xx(1)                                                              !/*km4a      214*/
      ir=ir-1                                                                   !/*km4a      215*/
      if(ir.ne.1)go to 110                                                      !/*km4a      216*/
      xx(1)=x                                                                   !/*km4a      217*/
!                                                                               !/*km4a      218*/
!        find the average of the keffs.                                         !/*km4a      219*/
      wa=0.                                                                     !/*km4a      220*/
      do 160 j=1,np                                                             !/*km4a      221*/
  160 wa=wa+xx(j)                                                               !/*km4a      222*/
      wa=wa/np                                                                  !/*km4a      223*/
!                                                                               !/*km4a      224*/
!        compute the square of the differences from the avg keff.               !/*km4a      225*/
      ws=0.                                                                     !/*km4a      226*/
      do 170 j=1,np                                                             !/*km4a      227*/
  170 ws=ws+(xx(j)-wa)**2                                                       !/*km4a      228*/
      i=1                                                                       !/*km4c       11*/
      if(ws.le.0.)go to 250                                                     !/*km4c       12*/
      if(np.gt.50)go to 200                                                     !/*km4a      229*/
!                                                                               !/*km4a      230*/
!        perform the w test for np<51 as described on p. 602 of ref. 1.         !/*km4a      231*/
      wb=0.                                                                     !/*km4a      232*/
      k=np/2                                                                    !/*km4a      233*/
      do 180 i=1,k                                                              !/*km4a      234*/
  180 wb=wb+a(i,np-2)*(xx(np-i+1)-xx(i))                                        !/*km4a      235*/
      w=wb**2/ws                                                                !/*km4a      236*/
!                                                                               !/*km4a      237*/
!        check w normality test at 95 and 99 percent confidence levels.         !/*km4a      238*/
      do 190 i=1,2                                                              !/*km4a      239*/
  190 if(w.ge.c(np,i))go to 250                                                 !/*km4a      240*/
      go to 270                                                                 !/*km4a      241*/
!                                                                               !/*km4a      242*/
!        use d test described in ref. 2 for more than 50 keff cycles.           !/*km4a      243*/
!        t is given by eqn. 2.2 on p. 342 in ref. 2.                            !/*km4a      244*/
!        d is given by eqn. 2.7 on p. 343 in ref. 2.                            !/*km4a      245*/
  200 t=0.0                                                                     !/*km4a      246*/
      f0=np                                                                     !/*km4a      247*/
      xn=0.5*(f0+1.)                                                            !/*km4a      248*/
      do 210 i=1,np                                                             !/*km4a      249*/
  210 t=t+(i-xn)*xx(i)                                                          !/*km4a      250*/
      d=t/(sqrt(ws)*f0**1.5)                                                    !/*km4a      251*/
!                                                                               !/*km4a      252*/
!        convert d statistic to standard normal and test for normality.         !/*km4a      253*/
!        standard normal y is given by eqns. 2.5 and 2.6, p 342, ref. 2.        !/*km4a      254*/
      y=(d-0.5/sqrt(pie))*sqrt(f0)/0.02998598                                   !/*km4a      255*/
      do 220 j=2,24                                                             !/*km4a      256*/
  220 if(f0.lt.fn(j))go to 230                                                  !/*km4a      257*/
      j=24                                                                      !/*km4a      258*/
  230 do 240 i=1,2                                                              !/*km4a      259*/
      fy=max(zero,(fn(j)-f0)/(fn(j)-fn(j-1)))                                   !/*km4a      260*/
      ys=f(1,i,j)-(f(1,i,j)-f(1,i,j-1))*fy                                      !/*km4a      261*/
      yl=f(2,i,j)-(f(2,i,j)-f(2,i,j-1))*fy                                      !/*km4a      262*/
  240 if(y.ge.ys.and.y.le.yl)go to 250                                          !/*km4a      263*/
      go to 270                                                                 !/*km4a      264*/
!                                                                               !/*km4a      265*/
!        print results of normality tests at 95 and 99% conf levels.            !/*km4a      266*/
  250 hz(3*ke-2:3*ke-1)=hp(i)                                                   !/*km4a      267*/
      if(nt.eq.1)write(iuo,260)hk(ke),hp(i),hy(i)                               !/*km4a      268*/
  260 format(11x,4hthe ,a13,29h cycle values appear normally,                   !/*km4a      269*/
     & 20h distributed at the ,a2,25h percent confidence level,a23)             !/*km4a      270*/
      if(nt.gt.0)go to 280                                                      !/*km4c       16*/
      return                                                                    !/*km4c       17*/
!                                                                               !/*km4a      272*/
!        print a warning for keff estimator that does not appear normal.        !/*km4a      273*/
  270 if(nt.gt.1)go to 280                                                      !/*km4c       18*/
      je=je+1                                                                   !/*km4a      275*/
      if(nt.eq.1)call erprnt(1,2,0,0,0,0,0,-1,                                  !/*kmx        16*/
     & '57h'//hk(ke)//' not normally distributed at 99% confidence.')           !/*kmx        17*/
      if(nt.eq.1)write(iuo,275)hk(ke),hp(2)                                     !/*km4c       28*/
  275 format(15h warning.  the ,a13,28h cycle values do not appear ,            !/*km4a      279*/
     & 28hnormally distributed at the ,a2,25h percent confidence level)         !/*km4a      280*/
!                                                                               !/*km4a      281*/
!        end of the normality test do loop for three keff estimators.           !/*km4a      282*/
  280 continue                                                                  !/*km4a      283*/
      if(nt.eq.1)write(iuo,'(/)')                                               !/*km4a      284*/
      return                                                                    !/*km4a      285*/
      end                                                                       !/*km4a      286*/
