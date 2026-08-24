!     $Id: minihb.F,v 1.6 2002/03/27 18:13:52 gwm Exp $                       !/*mi          1*/
      SUBROUTINE HLIMIT (LIM)                                                   !/*mi          2*/
#ifndef NO_PAW                                                                  /*!mi          3*/
!     ******************************************************************        !/*mi          4*/
!     *                                                                *        !/*mi          5*/
!     *        SETS THE BIGGEST ADDRESS THAT CAN BE USED IN BLANK      *        !/*mi          6*/
!     *        COMMON TO LIM. ON CDC IF THE LENGTH OF BLANK COMMON     *        !/*mi          7*/
!     *        IS LESS THAN LIM CALLS THE EXTENSION OF MEMOR           *        !/*mi          8*/
!     *                                                                *        !/*mi          9*/
!     ******************************************************************        !/*mi         10*/
!                                                                               !/*mi         11*/
      COMMON/HFLAG /ID    ,IDBADD,LID   ,IDLAST,IDHOLD,NBIT  ,NBITCH,           !/*mi         12*/
     &       NCHAR ,NX0   ,NX1   ,NX2   ,INTER ,INDEX ,LAST  ,LIMIT ,           !/*mi         13*/
     &       LFIEL ,NEWHIS,NRLENG,NWLIB ,NWFLAG,NBFLAG,NWSTAT,NRHIST,           !/*mi         14*/
     &       IDISC ,LFHIST,LLHIST,NWHIST,IERR  ,NV    ,NRDIS ,IA2               !/*mi         15*/
!                                                                               !/*mi         16*/
      COMMON /MIBOOK/B(512000)                                                  !/*mi         17*/
      DIMENSION IB(1)                                                           !/*mi         18*/
      EQUIVALENCE (B(1),IB(1))                                                  !/*mi         19*/
!                                                                               !/*mi         20*/
!                                                                               !/*mi         21*/
!     ------------------------------------------------------------------        !/*mi         22*/
!                                                                               !/*mi         23*/
      CALL HMACHI                                                               !/*mi         24*/
!                                                                               !/*mi         25*/
      IERR  =0                                                                  !/*mi         26*/
      IF (IDISC .EQ. 0)                          GO TO 20                       !/*mi         27*/
      CALL HBUFF (812,'HLIMIT',0)                                               !/*mi         28*/
      IERR  =1                                                                  !/*mi         29*/
      GO TO 99                                                                  !/*mi         30*/
!                                                                               !/*mi         31*/
  20  LIMIT =LIM                                                                !/*mi         32*/
  30  INDEX =LIMIT+1                                                            !/*mi         33*/
      INTER=INDEX                                                               !/*mi         34*/
      LAST=INTER                                                                !/*mi         35*/
      LFOLD =LFIEL                                                              !/*mi         36*/
!                                                                               !/*mi         37*/
      CALL HDELET(0)                                                            !/*mi         38*/
   99 RETURN                                                                    !/*mi         39*/
      END                                                                       !/*mi         40*/
      SUBROUTINE HBOOK1(IDD,KTITL,NNNX,XXX0,XXX1,VALMAX)                        !/*mi         41*/
!                                                                               !/*mi         42*/
!     ******************************************************************        !/*mi         43*/
!     *                                                                *        !/*mi         44*/
!     *       BOOKING OF A 1-DIM HISTOGRAM                             *        !/*mi         45*/
!     *                                                                *        !/*mi         46*/
!     ******************************************************************        !/*mi         47*/
!                                                                               !/*mi         48*/
      COMMON/HFLAG /ID    ,IDBADD,LID   ,IDLAST,IDHOLD,NBIT  ,NBITCH,           !/*mi         49*/
     &       NCHAR ,NX0   ,NX1   ,NX2   ,INTER ,INDEX ,LAST  ,LIMIT ,           !/*mi         50*/
     &       LFIEL ,NEWHIS,NRLENG,NWLIB ,NWFLAG,NBFLAG,NWSTAT,NRHIST,           !/*mi         51*/
     &       IDISC ,LFHIST,LLHIST,NWHIST,IERR  ,NV    ,NRDIS ,IA2               !/*mi         52*/
!                                                                               !/*mi         53*/
      COMMON /MIBOOK/B(512000)                                                  !/*mi         54*/
      DIMENSION IB(1)                                                           !/*mi         55*/
      EQUIVALENCE (B(1),IB(1))                                                  !/*mi         56*/
!                                                                               !/*mi         57*/
      COMMON/HPPRIN/IFW   ,NW    ,NB    ,IH    ,NHT   ,ICN   ,IPONCE,           !/*mi         58*/
     &       NH    ,MSTEP ,NOENT ,NOLD  ,IDOLAR,IBLANC,KBINSZ,INO   ,           !/*mi         59*/
     &       KSQUEZ,NCOLMA,NCOLPA,NLINPA,BIGP  ,ICBLAC,ICSTAR,ICFUNC,           !/*mi         60*/
     &       IDG(42),MAX(30),IDENT(9)                                           !/*mi         61*/
!                                                                               !/*mi         62*/
      COMMON/HCTIT/LTIT(40)                                                     !/*mi         63*/
!                                                                               !/*mi         64*/
      COMMON/HGARB/IT1,NBPROX,IDCONT,ITLAST,JNEXT,NWH,NWTITL,                   !/*mi         65*/
     &             NX,X0,X1,BWID,XVALMA,GA(5)                                   !/*mi         66*/
      CHARACTER*(*) KTITL                                                       !/*mi         67*/
!      DIMENSION KTITL(1)                                                       !/*mi         68*/
!                                                                               !/*mi         69*/
!     ------------------------------------------------------------------        !/*mi         70*/
!                                                                               !/*mi         71*/
!                                                                               !/*mi         72*/
      CALL HMACHI                                                               !/*mi         73*/
      IERR=0                                                                    !/*mi         74*/
!                                                                               !/*mi         75*/
      IF(IDD.NE.0)GO TO 10                                                      !/*mi         76*/
      CALL HBUFF(113,'HBOOK1',IDD)                                              !/*mi         77*/
      RETURN                                                                    !/*mi         78*/
!                                                                               !/*mi         79*/
  10  NOENT=0                                                                   !/*mi         80*/
      IFW=6                                                                     !/*mi         81*/
      CALL NOARG(IFW)                                                           !/*mi         82*/
      XVALMA=0.                                                                 !/*mi         83*/
      X1=0.                                                                     !/*mi         84*/
      X0=0.                                                                     !/*mi         85*/
      NX=100                                                                    !/*mi         86*/
      IF(IFW.EQ.6)XVALMA=FLOARG(VALMAX)                                         !/*mi         87*/
      IF(IFW.GE.5)X1=FLOARG(XXX1)                                               !/*mi         88*/
      IF(IFW.GE.5)X0=FLOARG(XXX0)                                               !/*mi         89*/
      IF(IFW.GE.3)NX=INTARG(NNNX)                                               !/*mi         90*/
      IF(NX.EQ.0)NX=100                                                         !/*mi         91*/
      IF(IFW.EQ.1)NOENT=1                                                       !/*mi         92*/
      IF(X1.LE.X0)NOENT=1                                                       !/*mi         93*/
!                                                                               !/*mi         94*/
      ID=IDD                                                                    !/*mi         95*/
      IF(KBINSZ.EQ.0)GO TO 20                                                   !/*mi         96*/
      CALL HBIN(X0,X1,NX,X0,X1,NX,BWID)                                         !/*mi         97*/
      NX=100                                                                    !/*mi         98*/
      IF(IFW.GE.3)NX=INTARG(NNNX)                                               !/*mi         99*/
      X1=X0+BWID*FLOAT(NX)                                                      !/*mi        100*/
  20  IF(NX.EQ.0)NX=100                                                         !/*mi        101*/
      CALL HSEARC(IDCONT)                                                       !/*mi        102*/
      IF(IDCONT.NE.1)GO TO 50                                                   !/*mi        103*/
!                                                                               !/*mi        104*/
!             ID IS ALREADY REFERENCED                                          !/*mi        105*/
!                                                                               !/*mi        106*/
      CALL HBUFF(112,'HBOOK1',IDD)                                              !/*mi        107*/
      CALL HDELET(IDD)                                                          !/*mi        108*/
!                                                                               !/*mi        109*/
!             COMPUTE PACKING FACTOR                                            !/*mi        110*/
!                                                                               !/*mi        111*/
 50   NBPROX=NBIT                                                               !/*mi        112*/
      IF(IFW.LT.6)GO TO 60                                                      !/*mi        113*/
      IF(XVALMA.LT.1.)                 GO TO 60                                 !/*mi        114*/
      IF(X1.LE.X0)CALL HBUFF(215,'HBOOK1',IDD)                                  !/*mi        115*/
      IF(X1.LE.X0)GO TO 60                                                      !/*mi        116*/
!                                                                               !/*mi        117*/
      NBPROX=ALOG(XVALMA)/ALOG(2.)+1.                                           !/*mi        118*/
      IF(NBPROX.GT.NBIT/2)NBPROX=NBIT                                           !/*mi        119*/
!                                                                               !/*mi        120*/
 60   NBPROX=MIN0(NBPROX,NBIT)                                                  !/*mi        121*/
!                                                                               !/*mi        122*/
!             COMPUTE NUMBER OF WORDS NECESSARY                                 !/*mi        123*/
!                                                                               !/*mi        124*/
      NWTITL=0                                                                  !/*mi        125*/
      IF(IFW.GT.1)THEN                                                          !/*mi        126*/
!         IF(KTITL(1).EQ.0)THEN                                                 !/*mi        127*/
!            LTIT(1)=0                                                          !/*mi        128*/
!         ELSE                                                                  !/*mi        129*/
!?????????????????????????????????????????                                      !/*mi        130*/
            CALL UCTOH(KTITL,LTIT,10,80)                                        !/*mi        131*/
!         ENDIF                                                                 !/*mi        132*/
         IF(LTIT(1).NE.0)CALL HBTIT(LTIT,ITLAST,NWTITL,IT1)                     !/*mi        133*/
      ENDIF                                                                     !/*mi        134*/
      NB=NBIT/NBPROX                                                            !/*mi        135*/
      NWH=(NX+2)/NB + 1                                                         !/*mi        136*/
      NWHIST=NWH+NWTITL+12                                                      !/*mi        137*/
!                                                                               !/*mi        138*/
!             BUILT HISTOGRAM IN //                                             !/*mi        139*/
!                                                                               !/*mi        140*/
      CALL HREDUC(NWHIST+NWLIB,'HBOOK1')                                        !/*mi        141*/
      IF(IERR.NE.0)                    GO TO 99                                 !/*mi        142*/
      CALL HINLIB                                                               !/*mi        143*/
      IB(LFHIST)=NWTITL+5                                                       !/*mi        144*/
      JNEXT=LFHIST+IB(LFHIST)                                                   !/*mi        145*/
      CALL HSIFLA(1,1)                                                          !/*mi        146*/
      IF(NOENT.EQ.1)CALL HSIFLA(5,1)                                            !/*mi        147*/
!                                                                               !/*mi        148*/
      IF(NWTITL.EQ.0)GO TO 70                                                   !/*mi        149*/
      CALL UCOPY(LTIT(IT1),B(LFHIST+5),NWTITL)                                  !/*mi        150*/
      IB(JNEXT-1)=ITLAST                                                        !/*mi        151*/
!                                                                               !/*mi        152*/
  70  IB(LFHIST+1)=NX                                                           !/*mi        153*/
      B(LFHIST+4)=1.                                                            !/*mi        154*/
      IF(NOENT.NE.1)B(LFHIST+4)=FLOAT(NX)/(X1-X0)                               !/*mi        155*/
      B(LFHIST+3)=X1                                                            !/*mi        156*/
      B(LFHIST+2)=X0-0.000001/B(LFHIST+4)                                       !/*mi        157*/
!                                                                               !/*mi        158*/
      IB(JNEXT)=NWH+5                                                           !/*mi        159*/
      IB(JNEXT+1)=1                                                             !/*mi        160*/
      IB(JNEXT+2)=4                                                             !/*mi        161*/
      IB(JNEXT+3)=NBPROX                                                        !/*mi        162*/
      CALL VZERO(B(JNEXT+4),NWH+2)                                              !/*mi        163*/
!                                                                               !/*mi        164*/
  99  RETURN                                                                    !/*mi        165*/
      END                                                                       !/*mi        166*/
      SUBROUTINE HBOOK2(IDD,KTITL,NNX,XX0,XX1,NNY,YY0,YY1,VALMAX)               !/*mi        167*/
!                                                                               !/*mi        168*/
!     ******************************************************************        !/*mi        169*/
!     *                                                                *        !/*mi        170*/
!     *       BOOKING OF A 2-DIM HISTOGRAM                             *        !/*mi        171*/
!     *                                                                *        !/*mi        172*/
!     ******************************************************************        !/*mi        173*/
!                                                                               !/*mi        174*/
      COMMON/HFLAG /ID    ,IDBADD,LID   ,IDLAST,IDHOLD,NBIT  ,NBITCH,           !/*mi        175*/
     &       NCHAR ,NX0   ,NX1   ,NX2   ,INTER ,INDEX ,LAST  ,LIMIT ,           !/*mi        176*/
     &       LFIEL ,NEWHIS,NRLENG,NWLIB ,NWFLAG,NBFLAG,NWSTAT,NRHIST,           !/*mi        177*/
     &       IDISC ,LFHIST,LLHIST,NWHIST,IERR  ,NV    ,NRDIS ,IA2               !/*mi        178*/
!                                                                               !/*mi        179*/
      COMMON /MIBOOK/B(512000)                                                  !/*mi        180*/
      DIMENSION IB(1)                                                           !/*mi        181*/
      EQUIVALENCE (B(1),IB(1))                                                  !/*mi        182*/
!                                                                               !/*mi        183*/
      COMMON/HPPRIN/IFW   ,NW    ,NB    ,IH    ,NHT   ,ICN   ,IPONCE,           !/*mi        184*/
     &       NH    ,MSTEP ,NOENT ,NOLD  ,IDOLAR,IBLANC,KBINSZ,INO   ,           !/*mi        185*/
     &       KSQUEZ,NCOLMA,NCOLPA,NLINPA,BIGP  ,ICBLAC,ICSTAR,ICFUNC,           !/*mi        186*/
     &       IDG(42),MAX(30),IDENT(9)                                           !/*mi        187*/
!                                                                               !/*mi        188*/
      COMMON/HCTIT/LTIT(40)                                                     !/*mi        189*/
!                                                                               !/*mi        190*/
      COMMON/HGARB/IDCONT,ITLAST,JNEXT,NBSCAT,IT1,NWH,NWTITL,BWIDY,NX,          !/*mi        191*/
     &             NY,XVALMA,BWIDX,X0,X1,Y0,Y1,GA                               !/*mi        192*/
      CHARACTER*(*) KTITL                                                       !/*mi        193*/
!      DIMENSION KTITL(1)                                                       !/*mi        194*/
!                                                                               !/*mi        195*/
!     ------------------------------------------------------------------        !/*mi        196*/
!                                                                               !/*mi        197*/
!                                                                               !/*mi        198*/
      CALL HMACHI                                                               !/*mi        199*/
      IERR=0                                                                    !/*mi        200*/
!                                                                               !/*mi        201*/
!                                                                               !/*mi        202*/
      IF(IDD.NE.0)GO TO 10                                                      !/*mi        203*/
      CALL HBUFF(113,'HBOOK2',IDD)                                              !/*mi        204*/
      RETURN                                                                    !/*mi        205*/
!                                                                               !/*mi        206*/
  10  NX=INTARG(NNX)                                                            !/*mi        207*/
      X0=FLOARG(XX0)                                                            !/*mi        208*/
      X1=FLOARG(XX1)                                                            !/*mi        209*/
      NY=INTARG(NNY)                                                            !/*mi        210*/
      Y0=FLOARG(YY0)                                                            !/*mi        211*/
      Y1=FLOARG(YY1)                                                            !/*mi        212*/
!                                                                               !/*mi        213*/
!                                                                               !/*mi        214*/
      IFW=9                                                                     !/*mi        215*/
      CALL NOARG(IFW)                                                           !/*mi        216*/
      IF(X0.LT.X1)GO TO 20                                                      !/*mi        217*/
      CALL HBUFF(214,'HBOOK2',IDD)                                              !/*mi        218*/
      GO TO 99                                                                  !/*mi        219*/
  20  IF(Y0.LT.Y1)GO TO 30                                                      !/*mi        220*/
      CALL HBUFF(214,'HBOOK2',IDD)                                              !/*mi        221*/
      GO TO 99                                                                  !/*mi        222*/
  30  IF(NX.GT.0.AND.NY.GT.0)GO TO 40                                           !/*mi        223*/
      CALL HBUFF(211,'HBOOK2',IDD)                                              !/*mi        224*/
      GO TO 99                                                                  !/*mi        225*/
!                                                                               !/*mi        226*/
!                                                                               !/*mi        227*/
 40   ID=IDD                                                                    !/*mi        228*/
      IF(KBINSZ.NE.0)   CALL HBIN(X0,X1,NX,X0,X1,NX,BWIDX)                      !/*mi        229*/
      IF(KBINSZ.NE.0)   CALL HBIN(Y0,Y1,NY,Y0,Y1,NY,BWIDY)                      !/*mi        230*/
      NX=INTARG(NNX)                                                            !/*mi        231*/
      NY=INTARG(NNY)                                                            !/*mi        232*/
      CALL HSEARC(IDCONT)                                                       !/*mi        233*/
      IF(IDCONT.NE.1)GO TO 50                                                   !/*mi        234*/
!                                                                               !/*mi        235*/
!             ID IS ALREADY REFERENCED                                          !/*mi        236*/
!                                                                               !/*mi        237*/
      CALL HBUFF(112,'HBOOK2',IDD)                                              !/*mi        238*/
      CALL HDELET(IDD)                                                          !/*mi        239*/
!                                                                               !/*mi        240*/
!             COMPUTE PACKING FACTOR                                            !/*mi        241*/
!                                                                               !/*mi        242*/
 50   NBSCAT=10/NV                                                              !/*mi        243*/
      IF(IFW.LT.9)GO TO 60                                                      !/*mi        244*/
      XVALMA=FLOARG(VALMAX)                                                     !/*mi        245*/
      IF(XVALMA.LT.1.)NBSCAT=NBIT                                               !/*mi        246*/
      IF(XVALMA.LT.1.)                 GO TO 60                                 !/*mi        247*/
      NBSCAT=ALOG(XVALMA)/ALOG(2.)+1.                                           !/*mi        248*/
      IF(NBSCAT.GT.NBIT/2)NBSCAT=NBIT                                           !/*mi        249*/
!                                                                               !/*mi        250*/
 60   NBSCAT=MIN0(NBSCAT,NBIT)                                                  !/*mi        251*/
!                                                                               !/*mi        252*/
!             COMPUTES NUMBER OF WORDS TO BOOK HISTOGRAM                        !/*mi        253*/
!                                                                               !/*mi        254*/
!      IF(KTITL(1).EQ.0)THEN                                                    !/*mi        255*/
!         LTIT(1)=0                                                             !/*mi        256*/
!      ELSE                                                                     !/*mi        257*/
!?????????????????????????????????????????                                      !/*mi        258*/
         CALL UCTOH(KTITL,LTIT,10,80)                                           !/*mi        259*/
!      ENDIF                                                                    !/*mi        260*/
      NWTITL=0                                                                  !/*mi        261*/
      IF(LTIT(1).NE.0)CALL HBTIT(LTIT,ITLAST,NWTITL,IT1)                        !/*mi        262*/
      NB=NBIT/NBSCAT                                                            !/*mi        263*/
      NWH=((NX+3)*(NY+3)-1)/NB+1                                                !/*mi        264*/
      NWHIST=NWH+NWTITL+16                                                      !/*mi        265*/
!                                                                               !/*mi        266*/
!             RESERVES SPACE                                                    !/*mi        267*/
!                                                                               !/*mi        268*/
      CALL HREDUC(NWHIST+NWLIB,'HBOOK2')                                        !/*mi        269*/
      IF(IERR.NE.0)GO TO 99                                                     !/*mi        270*/
!                                                                               !/*mi        271*/
!             INSERTS ID IN THE LIBRARY                                         !/*mi        272*/
!                                                                               !/*mi        273*/
      CALL HINLIB                                                               !/*mi        274*/
      IB(LFHIST)=NWTITL+9                                                       !/*mi        275*/
      JNEXT=LFHIST+IB(LFHIST)                                                   !/*mi        276*/
!                                                                               !/*mi        277*/
      CALL HSIFLA(4-NV,1)                                                       !/*mi        278*/
!                                                                               !/*mi        279*/
!             COPY TITLE                                                        !/*mi        280*/
!                                                                               !/*mi        281*/
      IF(NWTITL.EQ.0)GO TO 70                                                   !/*mi        282*/
      CALL UCOPY(LTIT(IT1),B(LFHIST+9),NWTITL)                                  !/*mi        283*/
      IB(JNEXT-1)=ITLAST                                                        !/*mi        284*/
!                                                                               !/*mi        285*/
!             KEEP HISTOGRAM INFORMATION                                        !/*mi        286*/
!                                                                               !/*mi        287*/
  70  IB(LFHIST+1)=NX                                                           !/*mi        288*/
      B(LFHIST+3)=X1                                                            !/*mi        289*/
      B(LFHIST+4)=FLOAT(NX)/(X1-X0)                                             !/*mi        290*/
      B(LFHIST+2)=X0-0.000001/B(LFHIST+4)                                       !/*mi        291*/
!                                                                               !/*mi        292*/
      IB(LFHIST+5)=NY                                                           !/*mi        293*/
      B(LFHIST+7)=Y1                                                            !/*mi        294*/
      B(LFHIST+8)=FLOAT(NY)/(Y1-Y0)                                             !/*mi        295*/
      B(LFHIST+6)=Y0-0.000001/B(LFHIST+8)                                       !/*mi        296*/
!                                                                               !/*mi        297*/
      IB(JNEXT)=NWH+5                                                           !/*mi        298*/
      IB(JNEXT+1)=2                                                             !/*mi        299*/
      IB(JNEXT+2)=4                                                             !/*mi        300*/
      IB(JNEXT+3)=NBSCAT                                                        !/*mi        301*/
      CALL VZERO(B(JNEXT+4),NWH+2)                                              !/*mi        302*/
!                                                                               !/*mi        303*/
  99  RETURN                                                                    !/*mi        304*/
      END                                                                       !/*mi        305*/
      SUBROUTINE HMACHI                                                         !/*mi        306*/
!                                                                               !/*mi        307*/
!     ******************************************************************        !/*mi        308*/
!     *                                                                *        !/*mi        309*/
!     *       INITIALISATION SUBROUTINE                                *        !/*mi        310*/
!     *       ROUTINE EXECUTED ONCE FOR ALL AT THE 1ST CALL            *        !/*mi        311*/
!     *       TO  HBOOK                                                *        !/*mi        312*/
!     *       DEFINE CONTANTS FOR ONE MACHINE                          *        !/*mi        313*/
!     *                                                                *        !/*mi        314*/
!     ******************************************************************        !/*mi        315*/
!     *                                                                *        !/*mi        316*/
!     *       DESCRIPTION OF VARIABLES IN COMMONS                      *        !/*mi        317*/
!     *                                                                *        !/*mi        318*/
!     *              A, COMMON/HFLAG/                                  *        !/*mi        319*/
!     *                                                                *        !/*mi        320*/
!     *       ID     CURRENT IDENTIFIER                                *        !/*mi        321*/
!     *       IDLAST LAST IDENTIFIER FILLED                            *        !/*mi        322*/
!     *       IDHOLD LAST ID ACCESSED BY HI,HIJ,ETC.                   *        !/*mi        323*/
!     *       LID    LAST IDENTIFIER SEARCHED IN ADDRESS TABLE         *        !/*mi        324*/
!     *       IDBADD ADDRESS OF ID IN ADDRESS TABLE (LIBRARY)          *        !/*mi        325*/
!     *       NBIT   NUMBER OF BITS IN A MACHINE WORD                  *        !/*mi        326*/
!     *       NBITCH NUMBER OF BITS PER CHARACTER                      *        !/*mi        327*/
!     *       NX0    POINTER TO 1ST WORD USABLE BY HBOOK (IN//)        *        !/*mi        328*/
!     *       NX1    POINTER TO THE BEGINNING OF THE LIBRARY           *        !/*mi        329*/
!     *       NX2    POINTER TO THE END OF LIBRARY+1                   *        !/*mi        330*/
!     *       INTER  POINTER TO INTERMEDIATE FILLING BUFFER            *        !/*mi        331*/
!     *       INDEX  POINTER TO DISK DIRECTORY                         *        !/*mi        332*/
!     *       LAST   CURRENT POINTER IN INT.FIL.BUFFER                 *        !/*mi        333*/
!     *       LIMIT  MAXIMUM NUMBER OF WORDS USABLE IN //              *        !/*mi        334*/
!     *       LFIEL  CURRENT USED LENGTH OF //                         *        !/*mi        335*/
!     *       NRLENG STEP FOR MEMORY EXTENSION                         *        !/*mi        336*/
!     *       NEWHIS POINTER TO 1ST WORD FREE                          *        !/*mi        337*/
!     *       NWLIB  NUMBER OF WORDS PER ID IN LIBRARY                 *        !/*mi        338*/
!     *       NWFLAG NUMBER OF WORDS FOR FLAGS IN LIBRARY              *        !/*mi        339*/
!     *       NBFLAG NUMBER OF FLAGS                                   *        !/*mi        340*/
!     *       NWSTAT NUMBER OF WORDS FOR STATISTICS                    *        !/*mi        341*/
!     *       NCHAR  NUMBER OF CHARACTERS IN A MACHINE WORD            *        !/*mi        342*/
!     *       IDISC  = 0  NO MASS STORAGE                              *        !/*mi        343*/
!     *              = 1  DISC                                         *        !/*mi        344*/
!     *              = 2  LCM                                          *        !/*mi        345*/
!     *       NRHIST NUMBER OF IDS CURRUNTLY EXISTING                  *        !/*mi        346*/
!     *       NRDIS  MAXIMUN NUMBER OF IDS FORESEEN ON DISC            *        !/*mi        347*/
!     *       IERR   0,1 NO ERROR,ERROR AFTER ANY MEMORY ACTION        *        !/*mi        348*/
!     *       NV     1 FOR HTABLE     2 FOR HBOOK2                     *        !/*mi        349*/
!     *       LFHIST FIRST LOCATION OF HIST ID  IN //                  *        !/*mi        350*/
!     *       LLHIST LAST LOCATION                                     *        !/*mi        351*/
!     *       NWHIST NUMBER OF WORDS FOR ID                            *        !/*mi        352*/
!     *                                                                *        !/*mi        353*/
!     *                                                                *        !/*mi        354*/
!     *              B, COMMON/HPPRIN/                                 *        !/*mi        355*/
!     *                                                                *        !/*mi        356*/
!     *       IFW    POINTER TO NUMBER OF ENTRIES FOR CURRENT ID       *        !/*mi        357*/
!     *       NW     NUMBER OF WORDS FOR SUM OF WEIGHTS                *        !/*mi        358*/
!     *       NB     NUMBER OF BITS PER CHANNEL                        *        !/*mi        359*/
!     *       IH     CURRENT NUMBER OF HISTOGRAM PRINTED               *        !/*mi        360*/
!     *       NHT    NUMBER OF PIECES IN WHICH ID IS SPLITTED          *        !/*mi        361*/
!     *       ICN    NUMBER OF CHANNELS                                *        !/*mi        362*/
!     *       NH     NUMBER OF HISTOGRAMS PRINTED                      *        !/*mi        363*/
!     *       MSTEP  NUMBER OF PRINTER COLUMNS (HBIGBI)                *        !/*mi        364*/
!     *       NOENT  SEVERAL MEANINGS                                  *        !/*mi        365*/
!     *       NOLD   USED TO PRINT TITLE                               *        !/*mi        366*/
!     *       IDOLAR 1H$ INTEGER VALUE                                 *        !/*mi        367*/
!     *       IBLANC 1H                                                *        !/*mi        368*/
!     *       KBINSZ =1 IF BIN ADJUSTEMENT REQUIRED AT BOOKING         *        !/*mi        369*/
!     *       KSQUEZ =1 IF NO PAGE EJECT AT PRINTING                   *        !/*mi        370*/
!     *       NCOLMA MAXIMUM NUMBER OF COLUMNS FOR PRINTING            *        !/*mi        371*/
!     *       NCOLPA NUMBER OF COLUMNS IN ONE PAGE                     *        !/*mi        372*/
!     *       NLINPA NUMBER OF LINES IN ONE PAGE                       *        !/*mi        373*/
!     *       MAX    POWERS OF 2                                       *        !/*mi        374*/
!     *       INO    2HNO                                              *        !/*mi        375*/
!     *       BIGP   VERY BIG NUMBER                                   *        !/*mi        376*/
!     *                                                                *        !/*mi        377*/
!     *                                                                *        !/*mi        378*/
!     *              C, COMMON/HBITS/                                  *        !/*mi        379*/
!     *                                                                *        !/*mi        380*/
!     *       I1     HBOOK1                                            *        !/*mi        381*/
!     *       I2     HBOOK2                                            *        !/*mi        382*/
!     *       I3     HTABLE                                            *        !/*mi        383*/
!     *       I4     HCORE                                             *        !/*mi        384*/
!     *       I5     AUTOMATIC BINNING                                 *        !/*mi        385*/
!     *       I6     HLOK/HUNLOK                                       *        !/*mi        386*/
!     *       I7     HBSTAT                                            *        !/*mi        387*/
!     *       I8     SET BY ANY OPTIONS (PROJECTIONS)                  *        !/*mi        388*/
!     *       I9     HBARX                                             *        !/*mi        389*/
!     *       I10    HBARY                                             *        !/*mi        390*/
!     *       I11    HERROR                                            *        !/*mi        391*/
!     *       I12    HFUNC                                             *        !/*mi        392*/
!     *       I13    HROTAT                                            *        !/*mi        393*/
!     *       I14    HPRFUN                                            *        !/*mi        394*/
!     *       I15    HPRLOW                                            *        !/*mi        395*/
!     *       I16    HPRHIS                                            *        !/*mi        396*/
!     *       I17    HBIGBI                                            *        !/*mi        397*/
!     *       I18    HNORMA                                            *        !/*mi        398*/
!     *       I19    HSCALE                                            *        !/*mi        399*/
!     *       I20    HMAXIM                                            *        !/*mi        400*/
!     *       I21    HMINIM                                            *        !/*mi        401*/
!     *       I22    HINTEG                                            *        !/*mi        402*/
!     *       I23    H2PAGE                                            *        !/*mi        403*/
!     *       I24    H1EVLI                                            *        !/*mi        404*/
!     *       I25    HPRSTA                                            *        !/*mi        405*/
!     *       I26    HLOGAR                                            *        !/*mi        406*/
!     *       I27    HBLACK                                            *        !/*mi        407*/
!     *       I28    HSTAR                                             *        !/*mi        408*/
!     *       I29    HPRCHA                                            *        !/*mi        409*/
!     *       I30    HPRCON                                            *        !/*mi        410*/
!     *       I31    HPRERR                                            *        !/*mi        411*/
!     *                                                                *        !/*mi        412*/
!     *       I32,I33,I35  NOT YET USED                                *        !/*mi        413*/
!     *       I34 USED NOT AS A BIT BUT A FLAG AT PRINTING             *        !/*mi        414*/
!     *       I230=I2+I3                                               *        !/*mi        415*/
!     *       I123=I1+I230                                             *        !/*mi        416*/
!     *                                                                *        !/*mi        417*/
!     *                                                                *        !/*mi        418*/
!     *              D, COMMON/HTAPIO/                                 *        !/*mi        419*/
!     *                                                                *        !/*mi        420*/
!     *       IOUT   LOGICAL UNIT FOR PRINTING OUTPUT                  *        !/*mi        421*/
!     *       IERMES LOGICAL UNIT FOR ERROR MESSAGES                   *        !/*mi        422*/
!     *       LDISC  LOGICAL UNIT FOR DISC MASS STORAGE                *        !/*mi        423*/
!     *                                                                *        !/*mi        424*/
!     *                                                                *        !/*mi        425*/
!     *                                                                *        !/*mi        426*/
!     ******************************************************************        !/*mi        427*/
!                                                                               !/*mi        428*/
      COMMON/HFLAG /ID    ,IDBADD,LID   ,IDLAST,IDHOLD,NBIT  ,NBITCH,           !/*mi        429*/
     &       NCHAR ,NX0   ,NX1   ,NX2   ,INTER ,INDEX ,LAST  ,LIMIT ,           !/*mi        430*/
     &       LFIEL ,NEWHIS,NRLENG,NWLIB ,NWFLAG,NBFLAG,NWSTAT,NRHIST,           !/*mi        431*/
     &       IDISC ,LFHIST,LLHIST,NWHIST,IERR  ,NV    ,NRDIS ,IA2               !/*mi        432*/
!                                                                               !/*mi        433*/
      COMMON / HBITS  / I1,   I2,   I3,   I4,   I5,   I6,   I7,   I8,           !/*mi        434*/
     &                  I9,   I10,  I11,  I12,  I13,  I14,  I15,  I16,          !/*mi        435*/
     &I17,  I18,  I19,  I20,  I21,  I22,  I23,  I24,  I25,  I26,  I27,          !/*mi        436*/
     &I28,  I29,  I30,  I31,  I32,  I33,  I34,  I35,  I123, I230                !/*mi        437*/
!                                                                               !/*mi        438*/
      COMMON /MIBOOK/B(512000)                                                  !/*mi        439*/
      DIMENSION IB(1)                                                           !/*mi        440*/
      EQUIVALENCE (B(1),IB(1))                                                  !/*mi        441*/
!                                                                               !/*mi        442*/
      COMMON /HPAR/ PNEF,PNBF,PSEL,PFLV,PLUN,PNBX,PNCX,FNIX,FEPS                !/*mi        443*/
!                                                                               !/*mi        444*/
      COMMON/HPPRIN/IFW   ,NW    ,NB    ,IH    ,NHT   ,ICN   ,IPONCE,           !/*mi        445*/
     &       NH    ,MSTEP ,NOENT ,NOLD  ,IDOLAR,IBLANC,KBINSZ,INO   ,           !/*mi        446*/
     &       KSQUEZ,NCOLMA,NCOLPA,NLINPA,BIGP  ,ICBLAC,ICSTAR,ICFUNC,           !/*mi        447*/
     &       IDG(42),MAX(30),IDENT(9)                                           !/*mi        448*/
!                                                                               !/*mi        449*/
      COMMON/HTAPIO/IOUT,IERMES,LDISC                                           !/*mi        450*/
!                                                                               !/*mi        451*/
      CHARACTER*4 IDGTDA(42),IPROJ(9)                                           !/*mi        452*/
      COMMON/HPFILL/IHV1(5)                                                     !/*mi        453*/
      COMMON/HVERS/INSTAL(20),IVERS(5),IDATE(8)                                 !/*mi        454*/
!                                                                               !/*mi        455*/
      DATA IDGTDA/'0','1','2','3','4','5','6','7','8','9',                      !/*mi        456*/
     &            'A','B','C','D','E','F','G','H','I','J',                      !/*mi        457*/
     &            'K','L','M','N','O','P','Q','R','S','T',                      !/*mi        458*/
     &            'U','V','W','X','Y','Z','*','.','-','+',                      !/*mi        459*/
     &            ' ','/'/                                                      !/*mi        460*/
!                                                                               !/*mi        461*/
      DATA IPROJ/'HIST','HIST','PROX','PROY','SLIX',                            !/*mi        462*/
     &           'SLIY','BANX','BANY','FUNC'/                                   !/*mi        463*/
!                                                                               !/*mi        464*/
      DATA IAPPEL/0/                                                            !/*mi        465*/
!                                                                               !/*mi        466*/
!     ------------------------------------------------------------------        !/*mi        467*/
!                                                                               !/*mi        468*/
      IDHOLD=0                                                                  !/*mi        469*/
      IDLAST=0                                                                  !/*mi        470*/
      IF(IAPPEL.NE.0)RETURN                                                     !/*mi        471*/
      IAPPEL=1                                                                  !/*mi        472*/
!                                                                               !/*mi        473*/
!             GET DATE OF DAY                                                   !/*mi        474*/
!                                                                               !/*mi        475*/
!      CALL DATIMH(IVERS,INSTAL)                                                !/*mi        476*/
!      CALL UBLOW(IVERS,IDATE,8)                                                !/*mi        477*/
!                                                                               !/*mi        478*/
!             SET VERSION NUMBER                                                !/*mi        479*/
!                                                                               !/*mi        480*/
!?????????????????????????????????????????                                      !/*mi        481*/
      CALL UCTOH1('3.46 ',IVERS,5)                                              !/*mi        482*/
!                                                                               !/*mi        483*/
!                                                                               !/*mi        484*/
      NBIT=32                                                                   !/*mi        485*/
      NBITCH=8                                                                  !/*mi        486*/
      IOUT=6                                                                    !/*mi        487*/
      BIGP=1.E+38                                                               !/*mi        488*/
      NX0=1                                                                     !/*mi        489*/
      LIMIT=1000000                                                             !/*mi        490*/
      LFIEL=0                                                                   !/*mi        491*/
!      CALL UCTOH1('VAX  CERN            ',INSTAL,20)                           !/*mi        492*/
      IERMES=IOUT                                                               !/*mi        493*/
      NHT=1                                                                     !/*mi        494*/
      MSTEP=1                                                                   !/*mi        495*/
      NOLD=4                                                                    !/*mi        496*/
      NCHAR=NBIT/NBITCH                                                         !/*mi        497*/
      NCOLPA=128                                                                !/*mi        498*/
      NCOLMA=100                                                                !/*mi        499*/
      NLINPA=61                                                                 !/*mi        500*/
      NV=2                                                                      !/*mi        501*/
      NBFLAG=32                                                                 !/*mi        502*/
      KBINSZ=0                                                                  !/*mi        503*/
      KSQUEZ=0                                                                  !/*mi        504*/
      LID=0                                                                     !/*mi        505*/
      NRHIST=0                                                                  !/*mi        506*/
      IERR=0                                                                    !/*mi        507*/
      IH=0                                                                      !/*mi        508*/
      NWFLAG=(NBFLAG-1)/NBIT +1                                                 !/*mi        509*/
      NWLIB=NWFLAG+4                                                            !/*mi        510*/
      NX1=NX0                                                                   !/*mi        511*/
      NX2=NX0                                                                   !/*mi        512*/
      NEWHIS=NX0                                                                !/*mi        513*/
      INDEX=LIMIT+1                                                             !/*mi        514*/
      LAST=INDEX                                                                !/*mi        515*/
      INTER=INDEX                                                               !/*mi        516*/
      NH=0                                                                      !/*mi        517*/
      NRDIS=0                                                                   !/*mi        518*/
      IDISC=0                                                                   !/*mi        519*/
      NWSTAT=6                                                                  !/*mi        520*/
      NRLENG=100                                                                !/*mi        521*/
      IPONCE=0                                                                  !/*mi        522*/
      PNEF=0.                                                                   !/*mi        523*/
      PNBF=0.                                                                   !/*mi        524*/
      PSEL=-1.                                                                  !/*mi        525*/
      PFLV=-1.                                                                  !/*mi        526*/
      PLUN=-1.                                                                  !/*mi        527*/
      PNBX=-1.                                                                  !/*mi        528*/
      PNCX=-1.                                                                  !/*mi        529*/
      FNIX=-1.                                                                  !/*mi        530*/
      FEPS=-1.                                                                  !/*mi        531*/
!                                                                               !/*mi        532*/
      IA2=1                                                                     !/*mi        533*/
      IF(NCHAR.LT.4)IA2=2                                                       !/*mi        534*/
      CALL VZERO(I1,37)                                                         !/*mi        535*/
      K=(NBIT+1)/2                                                              !/*mi        536*/
      MAX(1)  =2                                                                !/*mi        537*/
      DO 10 I=2,K                                                               !/*mi        538*/
      MAX(I)  =MAX(I-1)*2                                                       !/*mi        539*/
      MAX(I-1)=MAX(I-1)-1                                                       !/*mi        540*/
 10   CONTINUE                                                                  !/*mi        541*/
      MAX(K)  =MAX(K)-1                                                         !/*mi        542*/
!?????????????????????????????????????????                                      !/*mi        543*/
      CALL UCTOH(IDGTDA,IDG,4,168)                                              !/*mi        544*/
      ICSTAR=IDG(37)                                                            !/*mi        545*/
      ICBLAC=IDG(34)                                                            !/*mi        546*/
      ICFUNC=IDG(37)                                                            !/*mi        547*/
!?????????????????????????????????????????                                      !/*mi        548*/
      CALL UCTOH(IPROJ,IDENT,4,36)                                              !/*mi        549*/
!?????????????????????????????????????????                                      !/*mi        550*/
      CALL UCTOH('NO  ',INO,4,4)                                                !/*mi        551*/
!                                                                               !/*mi        552*/
      L2=1                                                                      !/*mi        553*/
!?????????????????????????????????????????                                      !/*mi        554*/
      CALL UCTOH('$   ',IDOL,4,4)                                               !/*mi        555*/
      IDOLAR=JBYT(IDOL,L2,NBITCH)                                               !/*mi        556*/
      IBLANC=JBYT(IDG(41),L2,NBITCH)                                            !/*mi        557*/
!                                                                               !/*mi        558*/
      RETURN                                                                    !/*mi        559*/
      END                                                                       !/*mi        560*/
      SUBROUTINE HBUFF(IERR,ILABEL,ID)                                          !/*mi        561*/
!                                                                               !/*mi        562*/
!     ******************************************************************        !/*mi        563*/
!     *                                                                *        !/*mi        564*/
!     *       THIS ROUTINE PRINTS ERROR MESSAGES ON FILE IERMES        *        !/*mi        565*/
!     *                                                                *        !/*mi        566*/
!     *          111   HISTOGRAM NOT REFERENCED                        *        !/*mi        567*/
!     *          212   NUMBER OF CHANNELS IN X .GT.NCLIM               *        !/*mi        568*/
!     *          213   NUMBER OF CHANNELS IN Y .GT.NCLIM               *        !/*mi        569*/
!     *          214   ABSCISSA OF ORIGIN .GT.ABSCISSA OF EXTREMITY    *        !/*mi        570*/
!     *          323   COMMAND ALREADY EXECUTED                        *        !/*mi        571*/
!     *          324   THIS COMMAND MUST APPEAR BEFORE FILLING         *        !/*mi        572*/
!     *          112   ID IS ALREADY REFERENCED                        *        !/*mi        573*/
!     *          321   COMMAND NOT AVAILABLE FOR A 1-DIM               *        !/*mi        574*/
!     *          322   ...........                 2-DIM               *        !/*mi        575*/
!     *          811   BUFFER AREA TOO SHORT                           *        !/*mi        576*/
!     *          711   OPERATION BETWEEN HIST WITH DIFFERENT SPECIFICA *        !/*mi        577*/
!     *          541   COMPARISON .........................            *        !/*mi        578*/
!     *          542   NORMALISATION FACTOR=0.                         *        !/*mi        579*/
!     *          543   BAD SCALE FACTOR                                *        !/*mi        580*/
!     *          721   DEGREE OF POLYNOMIAL .LE.0  IN HFITPO           *        !/*mi        581*/
!     *          722   NUMBER OF CHANNELS .LT.DEGREE OF POLYNOMIAL     *        !/*mi        582*/
!     *          311   FIRST LOCATION OF BUFFER SET .LE.O BY HISTGO    *        !/*mi        583*/
!     *          211   NUMBER OF CHANNELS IN X OR Y .LE.O              *        !/*mi        584*/
!     *          113   ID=0 ILLEGAL BOOKING IDENTIFIER                 *        !/*mi        585*/
!     *          215   PACKING NOT PERMITTED WITH AUTOMATIC BINNING    *        !/*mi        586*/
!     *          712   OPERAND HISTOGRAMS MUST RESID IN MEMORY         *        !/*mi        587*/
!     *          114   ILLEGAL PARAMETER                               *        !/*mi        588*/
!     *          115   THIS BAND OR SLICE NUMBER DOES NOT EXIST        *        !/*mi        589*/
!     *          325   OPTION NOT EXISTING                             *        !/*mi        590*/
!     *                                                                *        !/*mi        591*/
!     ******************************************************************        !/*mi        592*/
!                                                                               !/*mi        593*/
      COMMON/HFORM/IA(127),IDU                                                  !/*mi        594*/
      CHARACTER*(*) ILABEL                                                      !/*mi        595*/
!                                                                               !/*mi        596*/
!     ------------------------------------------------------------------        !/*mi        597*/
!                                                                               !/*mi        598*/
      CALL HFORMA(5)                                                            !/*mi        599*/
      CALL VBLANK(IA,127)                                                       !/*mi        600*/
!?????????????????????????????????????????                                      !/*mi        601*/
      CALL UCTOH1('****** HBOOK ERROR NO =',IA,23)                              !/*mi        602*/
      CALL HBCDI(IERR,3,IA(24))                                                 !/*mi        603*/
!?????????????????????????????????????????                                      !/*mi        604*/
      CALL UCTOH1('   CALLED FROM ',IA(27),15)                                  !/*mi        605*/
      CALL UCTOH1(ILABEL,IA(42),6)                                              !/*mi        606*/
!?????????????????????????????????????????                                      !/*mi        607*/
      IF(ID.NE.0)CALL UCTOH1('ID =',IA(52),4)                                   !/*mi        608*/
      CALL HBCDI(ID,8,IA(56))                                                   !/*mi        609*/
      CALL HFORMA(4)                                                            !/*mi        610*/
      RETURN                                                                    !/*mi        611*/
      END                                                                       !/*mi        612*/
      SUBROUTINE HBIN(A1,A2,NAA,BL,BH,NB,BWID)                                  !/*mi        613*/
      AL = AMIN1(A1,A2)                                                         !/*mi        614*/
      AH = AMAX1(A1,A2)                                                         !/*mi        615*/
      IF (AL.EQ.AH)  AH = AL + 1.                                               !/*mi        616*/
!         IF NAA .EQ. -1 , PROGRAM USES BWID INPUT FROM CALLING ROUTINE         !/*mi        617*/
      IF (NAA .EQ. -1)  GO TO 150                                               !/*mi        618*/
   10 NA = NAA - 1                                                              !/*mi        619*/
      IF (NA .LT. 1)  NA = 1                                                    !/*mi        620*/
!          GET NOMINAL BIN WIDTH IN EXPON FORM                                  !/*mi        621*/
   20 AWID = (AH-AL)/FLOAT(NA)                                                  !/*mi        622*/
      LOG = ALOG10(AWID)                                                        !/*mi        623*/
      IF (AWID .LE. 1.0)  LOG=LOG-1                                             !/*mi        624*/
      SIGFIG = AWID * (10.00 **(-LOG))                                          !/*mi        625*/
!         ROUND MANTISSA UP TO 2, 2.5, 5, OR 10                                 !/*mi        626*/
      IF(SIGFIG .GT. 2.0)  GO TO 40                                             !/*mi        627*/
      SIGRND = 2.0                                                              !/*mi        628*/
      GO TO 100                                                                 !/*mi        629*/
   40 IF (SIGFIG .GT. 2.5)  GO TO 45                                            !/*mi        630*/
      SIGRND = 2.5                                                              !/*mi        631*/
      GO TO 100                                                                 !/*mi        632*/
  45  IF(SIGFIG.GT.4.)GO TO 50                                                  !/*mi        633*/
      SIGRND=4.                                                                 !/*mi        634*/
      GO TO 100                                                                 !/*mi        635*/
   50 IF(SIGFIG .GT. 5.0)  GO TO 60                                             !/*mi        636*/
      SIGRND =5.0                                                               !/*mi        637*/
      GO TO 100                                                                 !/*mi        638*/
   60 SIGRND = 1.0                                                              !/*mi        639*/
      LOG = LOG + 1                                                             !/*mi        640*/
  100 CONTINUE                                                                  !/*mi        641*/
      BWID = SIGRND*10.0**LOG                                                   !/*mi        642*/
      GO TO 200                                                                 !/*mi        643*/
!         GET NEW BOUNDS FROM NEW WIDTH BWID                                    !/*mi        644*/
  150 IF (BWID .LE. 0.)  GO TO 10                                               !/*mi        645*/
  200 CONTINUE                                                                  !/*mi        646*/
      ALB = AL/BWID                                                             !/*mi        647*/
      LWID=ALB                                                                  !/*mi        648*/
      IF (ALB .LT. 0.0)  LWID=LWID-1                                            !/*mi        649*/
      BL=SIGRND*FLOAT(LWID)*10.**LOG                                            !/*mi        650*/
      ALB = AH/BWID + 1.0                                                       !/*mi        651*/
      KWID = ALB                                                                !/*mi        652*/
      IF (ALB .LT. 0.0)  KWID=KWID-1                                            !/*mi        653*/
      BH=SIGRND*FLOAT(KWID)*10.**LOG                                            !/*mi        654*/
      NB = KWID-LWID                                                            !/*mi        655*/
      IF (NAA .GT. 5)  GO TO 240                                                !/*mi        656*/
      IF (NAA .EQ. -1)  RETURN                                                  !/*mi        657*/
!          REQUEST FOR ONE BIN IS DIFFICULT CASE                                !/*mi        658*/
      IF (NAA .GT. 1 .OR. NB .EQ. 1)  RETURN                                    !/*mi        659*/
      BWID =  BWID*2.0                                                          !/*mi        660*/
       NB  = 1                                                                  !/*mi        661*/
       RETURN                                                                   !/*mi        662*/
  240 IF (2*NB .NE. NAA)  RETURN                                                !/*mi        663*/
      NA = NA + 1                                                               !/*mi        664*/
      GO TO 20                                                                  !/*mi        665*/
      END                                                                       !/*mi        666*/
      SUBROUTINE HSEARC(IDCONT)                                                 !/*mi        667*/
!                                                                               !/*mi        668*/
!     ******************************************************************        !/*mi        669*/
!     *                                                                *        !/*mi        670*/
!     *       BINARY SEARCH IN THE ADDRESS TABLE OF THE IDENT  ID      *        !/*mi        671*/
!     *       IDBADD CONTAINS THE ADRESS OF ID IF ID EXIST             *        !/*mi        672*/
!     *       IF ID DOES NOT EXIST IDBADD IS THE ADRESS OF THE ID      *        !/*mi        673*/
!     *       IMMEDIATLY GREATER THAN ID                               *        !/*mi        674*/
!     *       IF(IDCONT=2 ) ID INEXISTING                              *        !/*mi        675*/
!     *                                                                *        !/*mi        676*/
!     ******************************************************************        !/*mi        677*/
!                                                                               !/*mi        678*/
      COMMON /MIBOOK/B(512000)                                                  !/*mi        679*/
      DIMENSION IB(1)                                                           !/*mi        680*/
      EQUIVALENCE (B(1),IB(1))                                                  !/*mi        681*/
!                                                                               !/*mi        682*/
      COMMON/HFLAG /ID    ,IDBADD,LID   ,IDLAST,IDHOLD,NBIT  ,NBITCH,           !/*mi        683*/
     &       NCHAR ,NX0   ,NX1   ,NX2   ,INTER ,INDEX ,LAST  ,LIMIT ,           !/*mi        684*/
     &       LFIEL ,NEWHIS,NRLENG,NWLIB ,NWFLAG,NBFLAG,NWSTAT,NRHIST,           !/*mi        685*/
     &       IDISC ,LFHIST,LLHIST,NWHIST,IERR  ,NV    ,NRDIS ,IA2               !/*mi        686*/
!                                                                               !/*mi        687*/
!                                                                               !/*mi        688*/
!     ------------------------------------------------------------------        !/*mi        689*/
!                                                                               !/*mi        690*/
      IDCONT=1                                                                  !/*mi        691*/
      IF(ID.EQ.LID)RETURN                                                       !/*mi        692*/
      LID=ID                                                                    !/*mi        693*/
      IDBADD=NX1                                                                !/*mi        694*/
      LUP=NRHIST+1                                                              !/*mi        695*/
      LDOWN=0                                                                   !/*mi        696*/
!                                                                               !/*mi        697*/
  10  IF(LUP-LDOWN.LE.1)GO TO 40                                                !/*mi        698*/
      LMEAN=(LUP+LDOWN)/2                                                       !/*mi        699*/
      IDBADD=NX1+NWLIB*(LMEAN-1)                                                !/*mi        700*/
      IF(ID-IB(IDBADD))20,99,30                                                 !/*mi        701*/
  20  LUP=LMEAN                                                                 !/*mi        702*/
      GO TO 10                                                                  !/*mi        703*/
!                                                                               !/*mi        704*/
  30  LDOWN=LMEAN                                                               !/*mi        705*/
      IDBADD=IDBADD+NWLIB                                                       !/*mi        706*/
      GO TO 10                                                                  !/*mi        707*/
!                                                                               !/*mi        708*/
  40  IDCONT=2                                                                  !/*mi        709*/
      IDLAST=0                                                                  !/*mi        710*/
      LID=0                                                                     !/*mi        711*/
  99  RETURN                                                                    !/*mi        712*/
      END                                                                       !/*mi        713*/
      SUBROUTINE HDELET(ID1)                                                    !/*mi        714*/
!                                                                               !/*mi        715*/
!     ******************************************************************        !/*mi        716*/
!     *                                                                *        !/*mi        717*/
!     *       DELETES ID FROM MEMORY OR MASS STORAGE                   *        !/*mi        718*/
!     *                                                                *        !/*mi        719*/
!     ******************************************************************        !/*mi        720*/
!                                                                               !/*mi        721*/
      COMMON/HFLAG /ID    ,IDBADD,LID   ,IDLAST,IDHOLD,NBIT  ,NBITCH,           !/*mi        722*/
     &       NCHAR ,NX0   ,NX1   ,NX2   ,INTER ,INDEX ,LAST  ,LIMIT ,           !/*mi        723*/
     &       LFIEL ,NEWHIS,NRLENG,NWLIB ,NWFLAG,NBFLAG,NWSTAT,NRHIST,           !/*mi        724*/
     &       IDISC ,LFHIST,LLHIST,NWHIST,IERR  ,NV    ,NRDIS ,IA2               !/*mi        725*/
!                                                                               !/*mi        726*/
      COMMON /MIBOOK/B(512000)                                                  !/*mi        727*/
      DIMENSION IB(1)                                                           !/*mi        728*/
      EQUIVALENCE (B(1),IB(1))                                                  !/*mi        729*/
!                                                                               !/*mi        730*/
      COMMON/HTAPIO/IOUT,IERMES,LDISC                                           !/*mi        731*/
!                                                                               !/*mi        732*/
!                                                                               !/*mi        733*/
!     ------------------------------------------------------------------        !/*mi        734*/
!                                                                               !/*mi        735*/
      CALL HMACHI                                                               !/*mi        736*/
!      CALL HDEB                                                                !/*mi        737*/
      IF(ID1.EQ.0)GO TO 120                                                     !/*mi        738*/
      ID=ID1                                                                    !/*mi        739*/
      CALL HSEARC(IDCONT)                                                       !/*mi        740*/
      IF(IDCONT.EQ.1)GO TO 10                                                   !/*mi        741*/
      CALL HBUFF(111,'HDELET',ID1)                                              !/*mi        742*/
      GO TO 999                                                                 !/*mi        743*/
!                                                                               !/*mi        744*/
  10  IF(NRHIST.LT.2)GO TO 120                                                  !/*mi        745*/
!      IF(IB(IDBADD+1).LT.0)CALL HGET('HDELET')                                 !/*mi        746*/
      LFHIST=IB(IDBADD+1)                                                       !/*mi        747*/
      NLESS=IB(IDBADD+2)                                                        !/*mi        748*/
!                                                                               !/*mi        749*/
!             DELETES ID1 FROM HISTOGRAM BUFFER                                 !/*mi        750*/
!                                                                               !/*mi        751*/
      NEWHIS=NEWHIS-NLESS                                                       !/*mi        752*/
      CALL UCOPY2(IB(LFHIST+NLESS),IB(LFHIST),NEWHIS-LFHIST)                    !/*mi        753*/
      NX=NX2-NWLIB                                                              !/*mi        754*/
!                                                                               !/*mi        755*/
!             COMPUTES TEMPORARY POINTERS FOR HISTOGRAMS RESIDENT IN            !/*mi        756*/
!             MEMORY                                                            !/*mi        757*/
!                                                                               !/*mi        758*/
      DO 20 I=NX1,NX,NWLIB                                                      !/*mi        759*/
      IF(IB(I+NWLIB-1).EQ.ID)IPR=I+NWLIB-1                                      !/*mi        760*/
  20  IF(IB(I+1).GT.LFHIST)IB(I+1)=IB(I+1)-NLESS                                !/*mi        761*/
!                                                                               !/*mi        762*/
!             SHIFT BY 1 LAST WORD OF HIST FOR WHICH ID>ID1                     !/*mi        763*/
!                                                                               !/*mi        764*/
      JDBADD=IDBADD                                                             !/*mi        765*/
      I1=IDBADD+NWLIB                                                           !/*mi        766*/
      IF(I1.GT.NX)GO TO 55                                                      !/*mi        767*/
      DO 40 IDBADD=I1,NX,NWLIB                                                  !/*mi        768*/
      IF(IB(IDBADD+1).GT.0)GO TO 30                                             !/*mi        769*/
!      CALL HGETT                                                               !/*mi        770*/
      IT=NEWHIS+IB(IDBADD+2)-1                                                  !/*mi        771*/
      IB(IT)=IB(IT)-1                                                           !/*mi        772*/
!      CALL HPUTT                                                               !/*mi        773*/
      GO TO 40                                                                  !/*mi        774*/
  30  IT=IB(IDBADD+1)+IB(IDBADD+2)-1                                            !/*mi        775*/
      IB(IT)=IB(IT)-1                                                           !/*mi        776*/
  40  CONTINUE                                                                  !/*mi        777*/
      IDBADD=JDBADD                                                             !/*mi        778*/
!                                                                               !/*mi        779*/
!             SHIFT LIBRARY                                                     !/*mi        780*/
!                                                                               !/*mi        781*/
      DO 50 I=I1,NX,NWLIB                                                       !/*mi        782*/
  50  CALL UCOPY(IB(I),IB(I-NWLIB),NWLIB-1)                                     !/*mi        783*/
!                                                                               !/*mi        784*/
!             SHIFT PRINTING ORDER                                              !/*mi        785*/
!                                                                               !/*mi        786*/
  55  IT=NX2-NWLIB-1                                                            !/*mi        787*/
      IF(IPR.GT.IT)GO TO 65                                                     !/*mi        788*/
      DO 60 I=IPR,IT,NWLIB                                                      !/*mi        789*/
  60  IB(I)=IB(I+NWLIB)                                                         !/*mi        790*/
!                                                                               !/*mi        791*/
!             SQUEEZES HIST BUFFER BY NWLIB WORDS                               !/*mi        792*/
!                                                                               !/*mi        793*/
  65  CALL UCOPY2(IB(NX2),IB(NX2-NWLIB),NEWHIS-NX2)                             !/*mi        794*/
      NX=NX-NWLIB                                                               !/*mi        795*/
      NEWHIS=NEWHIS-NWLIB                                                       !/*mi        796*/
!                                                                               !/*mi        797*/
      DO 70 I=NX1,NX,NWLIB                                                      !/*mi        798*/
  70  IF(IB(I+1).GT.0)IB(I+1)=IB(I+1)-NWLIB                                     !/*mi        799*/
      ID=ID1                                                                    !/*mi        800*/
      NX2=NX2-NWLIB                                                             !/*mi        801*/
      NRHIST=NRHIST-1                                                           !/*mi        802*/
      GO TO 999                                                                 !/*mi        803*/
!                                                                               !/*mi        804*/
 120  CONTINUE                                                                  !/*mi        805*/
!                                                                               !/*mi        806*/
      NX2=NX1                                                                   !/*mi        807*/
      NEWHIS=NX1                                                                !/*mi        808*/
      NRHIST=0                                                                  !/*mi        809*/
!                                                                               !/*mi        810*/
  999 IDLAST=0                                                                  !/*mi        811*/
      IDHOLD=0                                                                  !/*mi        812*/
      LID   =0                                                                  !/*mi        813*/
      RETURN                                                                    !/*mi        814*/
      END                                                                       !/*mi        815*/
      SUBROUTINE HBTIT(KTITL,ITLAST,NWTITL,IT1)                                 !/*mi        816*/
!                                                                               !/*mi        817*/
!     ******************************************************************        !/*mi        818*/
!     *                                                                *        !/*mi        819*/
!     *       COMPUTES NUMBER OF WORDS GIVEN IN ARRAY KTITL            *        !/*mi        820*/
!     *                                                                *        !/*mi        821*/
!     ******************************************************************        !/*mi        822*/
!                                                                               !/*mi        823*/
      COMMON/HFLAG /ID    ,IDBADD,LID   ,IDLAST,IDHOLD,NBIT  ,NBITCH,           !/*mi        824*/
     &       NCHAR ,NX0   ,NX1   ,NX2   ,INTER ,INDEX ,LAST  ,LIMIT ,           !/*mi        825*/
     &       LFIEL ,NEWHIS,NRLENG,NWLIB ,NWFLAG,NBFLAG,NWSTAT,NRHIST,           !/*mi        826*/
     &       IDISC ,LFHIST,LLHIST,NWHIST,IERR  ,NV    ,NRDIS ,IA2               !/*mi        827*/
!                                                                               !/*mi        828*/
      COMMON/HPPRIN/IFW   ,NW    ,NB    ,IH    ,NHT   ,ICN   ,IPONCE,           !/*mi        829*/
     &       NH    ,MSTEP ,NOENT ,NOLD  ,IDOLAR,IBLANC,KBINSZ,INO   ,           !/*mi        830*/
     &       KSQUEZ,NCOLMA,NCOLPA,NLINPA,BIGP  ,ICBLAC,ICSTAR,ICFUNC,           !/*mi        831*/
     &       IDG(42),MAX(30),IDENT(9)                                           !/*mi        832*/
!                                                                               !/*mi        833*/
      DIMENSION KTITL(1)                                                        !/*mi        834*/
!                                                                               !/*mi        835*/
!     ------------------------------------------------------------------        !/*mi        836*/
!                                                                               !/*mi        837*/
      NWTITL=0                                                                  !/*mi        838*/
      IT1=1                                                                     !/*mi        839*/
      NWT=100                                                                   !/*mi        840*/
      NWTI=IABS(KTITL(1))                                                       !/*mi        841*/
      IF(NWTI.GT.NWT)GO TO 10                                                   !/*mi        842*/
      NWTITL=NWTI                                                               !/*mi        843*/
      IT1=2                                                                     !/*mi        844*/
      ITLAST=KTITL(NWTITL+1)                                                    !/*mi        845*/
      GO TO 99                                                                  !/*mi        846*/
  10  CONTINUE                                                                  !/*mi        847*/
!                                                                               !/*mi        848*/
      DO 40 I=1,NWT                                                             !/*mi        849*/
      IF(KTITL(I).EQ.0)GO TO 99                                                 !/*mi        850*/
      ITLAST=KTITL(I)                                                           !/*mi        851*/
      NWTITL=NWTITL+1                                                           !/*mi        852*/
      LBIT=1-NBITCH                                                             !/*mi        853*/
      DO 30 J=1,NCHAR                                                           !/*mi        854*/
      LBIT=LBIT+NBITCH                                                          !/*mi        855*/
      ICHAR=JBYT(ITLAST,LBIT,NBITCH)                                            !/*mi        856*/
      IF(ICHAR.NE.IDOLAR)GO TO 30                                               !/*mi        857*/
!                                                                               !/*mi        858*/
      DO 20 K=J,NCHAR                                                           !/*mi        859*/
      CALL SBYT(IBLANC,ITLAST,LBIT,NBITCH)                                      !/*mi        860*/
  20  LBIT=LBIT+NBITCH                                                          !/*mi        861*/
      GO TO 99                                                                  !/*mi        862*/
!                                                                               !/*mi        863*/
  30  CONTINUE                                                                  !/*mi        864*/
  40  CONTINUE                                                                  !/*mi        865*/
      NWTITL=8                                                                  !/*mi        866*/
!                                                                               !/*mi        867*/
  99  RETURN                                                                    !/*mi        868*/
      END                                                                       !/*mi        869*/
      SUBROUTINE HREDUC (N,LABEL)                                               !/*mi        870*/
!                                                                               !/*mi        871*/
!     ******************************************************************        !/*mi        872*/
!     *                                                                *        !/*mi        873*/
!     *       ROUTINE CALLED BY SUBROUTINE LABEL                       *        !/*mi        874*/
!     *       IERR=0  N WORDS CAN BE USED FOR STORAGE STARTING         *        !/*mi        875*/
!     *               AT ADRESS NEWHIS                                 *        !/*mi        876*/
!     *           =1  NOT ENOUGH SPACE                                 *        !/*mi        877*/
!     *                                                                *        !/*mi        878*/
!     ******************************************************************        !/*mi        879*/
!                                                                               !/*mi        880*/
      COMMON/HFLAG /ID    ,IDBADD,LID   ,IDLAST,IDHOLD,NBIT  ,NBITCH,           !/*mi        881*/
     &       NCHAR ,NX0   ,NX1   ,NX2   ,INTER ,INDEX ,LAST  ,LIMIT ,           !/*mi        882*/
     &       LFIEL ,NEWHIS,NRLENG,NWLIB ,NWFLAG,NBFLAG,NWSTAT,NRHIST,           !/*mi        883*/
     &       IDISC ,LFHIST,LLHIST,NWHIST,IERR  ,NV    ,NRDIS ,IA2               !/*mi        884*/
!                                                                               !/*mi        885*/
      COMMON /MIBOOK/B(512000)                                                  !/*mi        886*/
      DIMENSION IB(1)                                                           !/*mi        887*/
      EQUIVALENCE (B(1),IB(1))                                                  !/*mi        888*/
!                                                                               !/*mi        889*/
      CHARACTER*(*) LABEL                                                       !/*mi        890*/
!                                                                               !/*mi        891*/
!     ------------------------------------------------------------------        !/*mi        892*/
!                                                                               !/*mi        893*/
      IERR=0                                                                    !/*mi        894*/
    5 IF (N .LE. 0)              GO TO 999                                      !/*mi        895*/
      IF(NEWHIS+N.LT.INTER)GO TO 50                                             !/*mi        896*/
      CALL HBUFF (811,LABEL,0)                                                  !/*mi        897*/
      IERR  =1                                                                  !/*mi        898*/
      GO TO 999                                                                 !/*mi        899*/
   50 NW    =NEWHIS+N-LFIEL                                                     !/*mi        900*/
      IF(NW.GT.0)CALL HMEMEX(NW)                                                !/*mi        901*/
  999 RETURN                                                                    !/*mi        902*/
      END                                                                       !/*mi        903*/
      SUBROUTINE HSIFLA(KFLA,I)                                                 !/*mi        904*/
!                                                                               !/*mi        905*/
!     ******************************************************************        !/*mi        906*/
!     *                                                                *        !/*mi        907*/
!     *       SETS BIT NO KFLA TO I                                    *        !/*mi        908*/
!     *                                                                *        !/*mi        909*/
!     ******************************************************************        !/*mi        910*/
!                                                                               !/*mi        911*/
      COMMON/HFLAG /ID    ,IDBADD,LID   ,IDLAST,IDHOLD,NBIT  ,NBITCH,           !/*mi        912*/
     &       NCHAR ,NX0   ,NX1   ,NX2   ,INTER ,INDEX ,LAST  ,LIMIT ,           !/*mi        913*/
     &       LFIEL ,NEWHIS,NRLENG,NWLIB ,NWFLAG,NBFLAG,NWSTAT,NRHIST,           !/*mi        914*/
     &       IDISC ,LFHIST,LLHIST,NWHIST,IERR  ,NV    ,NRDIS ,IA2               !/*mi        915*/
!                                                                               !/*mi        916*/
      COMMON / HBITS  / I1,   I2,   I3,   I4,   I5,   I6,   I7,   I8,           !/*mi        917*/
     &                  I9,   I10,  I11,  I12,  I13,  I14,  I15,  I16,          !/*mi        918*/
     &I17,  I18,  I19,  I20,  I21,  I22,  I23,  I24,  I25,  I26,  I27,          !/*mi        919*/
     &I28,  I29,  I30,  I31,  I32,  I33,  I34,  I35,  I123, I230                !/*mi        920*/
!                                                                               !/*mi        921*/
      COMMON /MIBOOK/B(512000)                                                  !/*mi        922*/
      DIMENSION IB(1)                                                           !/*mi        923*/
      EQUIVALENCE (B(1),IB(1))                                                  !/*mi        924*/
!                                                                               !/*mi        925*/
      DIMENSION IFLAG(32)                                                       !/*mi        926*/
      EQUIVALENCE       (I1,IFLAG(1))                                           !/*mi        927*/
!                                                                               !/*mi        928*/
!     ------------------------------------------------------------------        !/*mi        929*/
!                                                                               !/*mi        930*/
      L=KFLA/(NBIT+1)                                                           !/*mi        931*/
      IWFLAG=IDBADD+L+3                                                         !/*mi        932*/
      L=KFLA-NBIT*L                                                             !/*mi        933*/
      IF(I.EQ.0)GO TO 10                                                        !/*mi        934*/
      CALL SBIT1(B(IWFLAG),L)                                                   !/*mi        935*/
      GO TO 20                                                                  !/*mi        936*/
  10  CALL SBIT0(B(IWFLAG),L)                                                   !/*mi        937*/
  20  CONTINUE                                                                  !/*mi        938*/
!                                                                               !/*mi        939*/
      IFLAG(KFLA)=I                                                             !/*mi        940*/
      I230=I2+I3                                                                !/*mi        941*/
      I123=I1+I230                                                              !/*mi        942*/
      RETURN                                                                    !/*mi        943*/
      END                                                                       !/*mi        944*/
      SUBROUTINE HINLIB                                                         !/*mi        945*/
!                                                                               !/*mi        946*/
!     ******************************************************************        !/*mi        947*/
!     *                                                                *        !/*mi        948*/
!     *       INSERT THE HISTOGRAM IN THE COMMON BLANK                 *        !/*mi        949*/
!     *       CHANGE ALL THE POINTERS IN THE LIBRARY                   *        !/*mi        950*/
!     *       INSERT IN THE LIBRARY THE FEATURES OF THE                *        !/*mi        951*/
!     *       HISTOGRAM                                                *        !/*mi        952*/
!     *                                                                *        !/*mi        953*/
!     ******************************************************************        !/*mi        954*/
!                                                                               !/*mi        955*/
      COMMON /MIBOOK/B(512000)                                                  !/*mi        956*/
      DIMENSION IB(1)                                                           !/*mi        957*/
      EQUIVALENCE (B(1),IB(1))                                                  !/*mi        958*/
!                                                                               !/*mi        959*/
      COMMON/HFLAG /ID    ,IDBADD,LID   ,IDLAST,IDHOLD,NBIT  ,NBITCH,           !/*mi        960*/
     &       NCHAR ,NX0   ,NX1   ,NX2   ,INTER ,INDEX ,LAST  ,LIMIT ,           !/*mi        961*/
     &       LFIEL ,NEWHIS,NRLENG,NWLIB ,NWFLAG,NBFLAG,NWSTAT,NRHIST,           !/*mi        962*/
     &       IDISC ,LFHIST,LLHIST,NWHIST,IERR  ,NV    ,NRDIS ,IA2               !/*mi        963*/
!                                                                               !/*mi        964*/
!                                                                               !/*mi        965*/
!     ------------------------------------------------------------------        !/*mi        966*/
!                                                                               !/*mi        967*/
!                                                                               !/*mi        968*/
!             UPDATE OF POINTERS                                                !/*mi        969*/
!                                                                               !/*mi        970*/
      NRHIST=NRHIST+1                                                           !/*mi        971*/
      NX    =NX2-NWLIB+1                                                        !/*mi        972*/
      NI    = NX1+1                                                             !/*mi        973*/
      IF (NX    .LT. NI)                    GO TO  30                           !/*mi        974*/
      DO  10 I=NI,NX,NWLIB                                                      !/*mi        975*/
      IF (IB(I) .LT. 0 )                    GO TO  10                           !/*mi        976*/
      IB(I) =IB(I)+NWLIB                                                        !/*mi        977*/
   10 CONTINUE                                                                  !/*mi        978*/
!                                                                               !/*mi        979*/
!             SHIFT HISTOGRAMS                                                  !/*mi        980*/
!                                                                               !/*mi        981*/
      NX   =NEWHIS-NX2                                                          !/*mi        982*/
      CALL UCOPY2 (B(NX2),B(NX2+NWLIB),NX)                                      !/*mi        983*/
      IF (IDBADD .EQ. NX2)                  GO TO  30                           !/*mi        984*/
!                                                                               !/*mi        985*/
!             SHIFT OF LIBRARY                                                  !/*mi        986*/
!             UPDATE OF LIBRARY ORDER                                           !/*mi        987*/
!                                                                               !/*mi        988*/
      NX     =NX2-IDBADD                                                        !/*mi        989*/
      CALL UCOPY2 (B(IDBADD),B(IDBADD+NWLIB),NX)                                !/*mi        990*/
      NX     =NX2-NWLIB                                                         !/*mi        991*/
      DO  20 I=IDBADD,NX,NWLIB                                                  !/*mi        992*/
      J      =I+NWLIB-1                                                         !/*mi        993*/
      IF (IB(I+NWLIB+1) .LT. 0)             GO TO  20                           !/*mi        994*/
      K      =IB(I+NWLIB+1)+IB(I+NWLIB+2)-1                                     !/*mi        995*/
      IB(K)  =IB(K)+1                                                           !/*mi        996*/
   20 IB(  J)=IB(J+NWLIB)                                                       !/*mi        997*/
!                                                                               !/*mi        998*/
!              UPDATE LIBRARY                                                   !/*mi        999*/
!                                                                               !/*mi       1000*/
   30 NEWHIS      =NEWHIS+NWLIB                                                 !/*mi       1001*/
      NX2         =NX2   +NWLIB                                                 !/*mi       1002*/
      IB(NX2-1)   =ID                                                           !/*mi       1003*/
      IB(  IDBADD)=ID                                                           !/*mi       1004*/
      IB(IDBADD+1)=NEWHIS                                                       !/*mi       1005*/
      IB(IDBADD+2)=NWHIST                                                       !/*mi       1006*/
      DO 40 I=1,NWFLAG                                                          !/*mi       1007*/
   40 IB(IDBADD+I+2)=0                                                          !/*mi       1008*/
      LFHIST=NEWHIS                                                             !/*mi       1009*/
      NEWHIS      =NEWHIS+NWHIST                                                !/*mi       1010*/
      IB(NEWHIS-1)=(IDBADD-NX1+1)/NWLIB +1                                      !/*mi       1011*/
!                                                                               !/*mi       1012*/
      IDHOLD=0                                                                  !/*mi       1013*/
      IDLAST=0                                                                  !/*mi       1014*/
      LID   =0                                                                  !/*mi       1015*/
      RETURN                                                                    !/*mi       1016*/
      END                                                                       !/*mi       1017*/
      SUBROUTINE HF1(ID1,X,W)                                                   !/*mi       1018*/
!                                                                               !/*mi       1019*/
!     ******************************************************************        !/*mi       1020*/
!     *                                                                *        !/*mi       1021*/
!     *       FAST FILLING ENTRY FOR A 1-DIM HISTOGRAM                 *        !/*mi       1022*/
!     *                                                                *        !/*mi       1023*/
!     ******************************************************************        !/*mi       1024*/
!                                                                               !/*mi       1025*/
      COMMON /MIBOOK/B(512000)                                                  !/*mi       1026*/
      DIMENSION IB(1)                                                           !/*mi       1027*/
      EQUIVALENCE (B(1),IB(1))                                                  !/*mi       1028*/
!                                                                               !/*mi       1029*/
      COMMON/HFLAG /ID    ,IDBADD,LID   ,IDLAST,IDHOLD,NBIT  ,NBITCH,           !/*mi       1030*/
     &       NCHAR ,NX0   ,NX1   ,NX2   ,INTER ,INDEX ,LAST  ,LIMIT ,           !/*mi       1031*/
     &       LFIEL ,NEWHIS,NRLENG,NWLIB ,NWFLAG,NBFLAG,NWSTAT,NRHIST,           !/*mi       1032*/
     &       IDISC ,LFHIST,LLHIST,NWHIST,IERR  ,NV    ,NRDIS ,IA2               !/*mi       1033*/
!                                                                               !/*mi       1034*/
      COMMON / HBITS  / I1,   I2,   I3,   I4,   I5,   I6,   I7,   I8,           !/*mi       1035*/
     &                  I9,   I10,  I11,  I12,  I13,  I14,  I15,  I16,          !/*mi       1036*/
     &I17,  I18,  I19,  I20,  I21,  I22,  I23,  I24,  I25,  I26,  I27,          !/*mi       1037*/
     &I28,  I29,  I30,  I31,  I32,  I33,  I34,  I35,  I123, I230                !/*mi       1038*/
!                                                                               !/*mi       1039*/
      COMMON/HPFILL/IFPROX,IFWPRX,IFWPRY,NBPROX,ICHAN                           !/*mi       1040*/
!                                                                               !/*mi       1041*/
!     ------------------------------------------------------------------        !/*mi       1042*/
!                                                                               !/*mi       1043*/
      IF(ID1.EQ.IDLAST)GO TO 10                                                 !/*mi       1044*/
      ID=ID1                                                                    !/*mi       1045*/
      CALL HSEARC(IDCONT)                                                       !/*mi       1046*/
      IF(IDCONT.NE.1)RETURN                                                     !/*mi       1047*/
      IDLAST=ID1                                                                !/*mi       1048*/
      IFWPRX=IB(IDBADD+1)+2                                                     !/*mi       1049*/
      I9=0                                                                      !/*mi       1050*/
      I5=JBIT(B(IDBADD+3),5)                                                    !/*mi       1051*/
      IF=IFWPRX+IB(IFWPRX-2)                                                    !/*mi       1052*/
      IFPROX=IF+IB(IF)-2                                                        !/*mi       1053*/
      NBPROX=IB(IFPROX-1)                                                       !/*mi       1054*/
!                                                                               !/*mi       1055*/
  10  IB(IFPROX)=IB(IFPROX)+1                                                   !/*mi       1056*/
      IF(I5.NE.0)CALL HAUTO(X)                                                  !/*mi       1057*/
      IF(X.LT.B(IFWPRX))GO TO 40                                                !/*mi       1058*/
      IF(X.GT.B(IFWPRX+1))GO TO 50                                              !/*mi       1059*/
      ICHAN=(X-B(IFWPRX))*B(IFWPRX+2) + 1                                       !/*mi       1060*/
!                                                                               !/*mi       1061*/
  20  IF(NBPROX.NE.NBIT)GO TO 30                                                !/*mi       1062*/
      LWORD=IFPROX+ICHAN+1                                                      !/*mi       1063*/
      B(LWORD)=B(LWORD)+W                                                       !/*mi       1064*/
      RETURN                                                                    !/*mi       1065*/
!                                                                               !/*mi       1066*/
  30  NB=NBIT/NBPROX                                                            !/*mi       1067*/
      LWORD=ICHAN/NB                                                            !/*mi       1068*/
      LBIT=(ICHAN-NB*LWORD)*NBPROX+1                                            !/*mi       1069*/
      CALL HBYTE(W+0.5,IB(LWORD+IFPROX+1),NBPROX,LBIT)                          !/*mi       1070*/
      RETURN                                                                    !/*mi       1071*/
!                                                                               !/*mi       1072*/
  40  ICHAN=0                                                                   !/*mi       1073*/
      GO TO 20                                                                  !/*mi       1074*/
  50  ICHAN=IB(IFWPRX-1)+1                                                      !/*mi       1075*/
      GO TO 20                                                                  !/*mi       1076*/
!                                                                               !/*mi       1077*/
      END                                                                       !/*mi       1078*/
!DECK  ID>, HF2.                                                                !/*mi       1079*/
      SUBROUTINE HF2(ID1,X,Y,W)                                                 !/*mi       1080*/
!                                                                               !/*mi       1081*/
!     ******************************************************************        !/*mi       1082*/
!     *                                                                *        !/*mi       1083*/
!     *       FAST FILLING ENTRY FOR A 2-DIM HISTOGRAM                 *        !/*mi       1084*/
!     *                                                                *        !/*mi       1085*/
!     ******************************************************************        !/*mi       1086*/
!                                                                               !/*mi       1087*/
      COMMON /MIBOOK/B(512000)                                                  !/*mi       1088*/
      DIMENSION IB(1)                                                           !/*mi       1089*/
      EQUIVALENCE (B(1),IB(1))                                                  !/*mi       1090*/
!                                                                               !/*mi       1091*/
      COMMON/HFLAG /ID    ,IDBADD,LID   ,IDLAST,IDHOLD,NBIT  ,NBITCH,           !/*mi       1092*/
     &       NCHAR ,NX0   ,NX1   ,NX2   ,INTER ,INDEX ,LAST  ,LIMIT ,           !/*mi       1093*/
     &       LFIEL ,NEWHIS,NRLENG,NWLIB ,NWFLAG,NBFLAG,NWSTAT,NRHIST,           !/*mi       1094*/
     &       IDISC ,LFHIST,LLHIST,NWHIST,IERR  ,NV    ,NRDIS ,IA2               !/*mi       1095*/
!                                                                               !/*mi       1096*/
      COMMON/HPFILL/IFSCAT,IFWPRX,IFWPRY,NBSCAT,ICHAN                           !/*mi       1097*/
!                                                                               !/*mi       1098*/
!     ------------------------------------------------------------------        !/*mi       1099*/
!                                                                               !/*mi       1100*/
      IF(ID1.EQ.IDLAST)GO TO 10                                                 !/*mi       1101*/
      ID=ID1                                                                    !/*mi       1102*/
      CALL HSEARC(IDCONT)                                                       !/*mi       1103*/
      IF(IDCONT.NE.1)RETURN                                                     !/*mi       1104*/
      IDLAST=ID1                                                                !/*mi       1105*/
!                                                                               !/*mi       1106*/
      IFWPRX=IB(IDBADD+1)+2                                                     !/*mi       1107*/
      IFWPRY=IFWPRX+4                                                           !/*mi       1108*/
      IF=IFWPRX+IB(IFWPRX-2)                                                    !/*mi       1109*/
      IFSCAT=IF+IB(IF)-2                                                        !/*mi       1110*/
      NBSCAT=IB(IFSCAT-1)                                                       !/*mi       1111*/
!                                                                               !/*mi       1112*/
  10  IB(IFSCAT)=IB(IFSCAT)+1                                                   !/*mi       1113*/
      IF(X.LT.B(IFWPRX))GO TO 50                                                !/*mi       1114*/
      IF(X.GT.B(IFWPRX+1))GO TO 60                                              !/*mi       1115*/
      I=(X-B(IFWPRX))*B(IFWPRX+2) + 1                                           !/*mi       1116*/
  20  IF(Y.LT.B(IFWPRY))GO TO 70                                                !/*mi       1117*/
      IF(Y.GT.B(IFWPRY+1))GO TO 80                                              !/*mi       1118*/
      J=(Y-B(IFWPRY))*B(IFWPRY+2) + 1                                           !/*mi       1119*/
!                                                                               !/*mi       1120*/
  30  ICHAN=(IB(IFWPRY-1)-J+2)*(IB(IFWPRX-1)+3)+I                               !/*mi       1121*/
      IF(NBSCAT.EQ.NBIT)GO TO 40                                                !/*mi       1122*/
      NB=NBIT/NBSCAT                                                            !/*mi       1123*/
      LWORD=ICHAN/NB                                                            !/*mi       1124*/
      LBIT=(NB-1-MOD(ICHAN,NB))*NBSCAT +1                                       !/*mi       1125*/
      CALL HBYTE(W+0.5,IB(LWORD+IFSCAT+1),NBSCAT,LBIT)                          !/*mi       1126*/
      RETURN                                                                    !/*mi       1127*/
!                                                                               !/*mi       1128*/
  40  LWORD=IFSCAT+ICHAN+1                                                      !/*mi       1129*/
      B(LWORD)=B(LWORD)+W                                                       !/*mi       1130*/
      RETURN                                                                    !/*mi       1131*/
!                                                                               !/*mi       1132*/
  50  I=0                                                                       !/*mi       1133*/
      GO TO 20                                                                  !/*mi       1134*/
  60  I=IB(IFWPRX-1)+1                                                          !/*mi       1135*/
      GO TO 20                                                                  !/*mi       1136*/
  70  J=0                                                                       !/*mi       1137*/
      GO TO 30                                                                  !/*mi       1138*/
  80  J=IB(IFWPRY-1)+1                                                          !/*mi       1139*/
      GO TO 30                                                                  !/*mi       1140*/
!                                                                               !/*mi       1141*/
      END                                                                       !/*mi       1142*/
      SUBROUTINE HFORMA(I)                                                      !/*mi       1143*/
!                                                                               !/*mi       1144*/
!     ******************************************************************        !/*mi       1145*/
!     *                                                                *        !/*mi       1146*/
!     *        THIS ROUTINE PRINTS DIFFERENT FORMATS OF HBOOK          *        !/*mi       1147*/
!     *                                                                *        !/*mi       1148*/
!     ******************************************************************        !/*mi       1149*/
!                                                                               !/*mi       1150*/
      COMMON/HTAPIO/IOUT,IERMES,LDISC                                           !/*mi       1151*/
!                                                                               !/*mi       1152*/
      COMMON/HFORM/IA(127),IDU                                                  !/*mi       1153*/
      COMMON/HPPRIN/IFW   ,NW    ,NB    ,IH    ,NHT   ,ICN   ,IPONCE,           !/*mi       1154*/
     &       NH    ,MSTEP ,NOENT ,NOLD  ,IDOLAR,IBLANC,KBINSZ,INO   ,           !/*mi       1155*/
     &       KSQUEZ,NCOLMA,NCOLPA,NLINPA,BIGP  ,ICBLAC,ICSTAR,ICFUNC,           !/*mi       1156*/
     &       IDG(42),MAX(30),IDENT(9)                                           !/*mi       1157*/
!                                                                               !/*mi       1158*/
!                                                                               !/*mi       1159*/
!     ------------------------------------------------------------------        !/*mi       1160*/
!                                                                               !/*mi       1161*/
      GO TO (10,20,30,40,50),I                                                  !/*mi       1162*/
  10  CONTINUE                                                                  !/*mi       1163*/
      DO 12 II=1,127                                                            !/*mi       1164*/
      KK=128-II                                                                 !/*mi       1165*/
      IF(IA(KK).NE.IDG(41))GO TO 15                                             !/*mi       1166*/
  12  CONTINUE                                                                  !/*mi       1167*/
  15  WRITE(IOUT,1000)(IA(II),II=1,KK)                                          !/*mi       1168*/
      GO TO 999                                                                 !/*mi       1169*/
  20  WRITE(IOUT,2000)                                                          !/*mi       1170*/
      GO TO 999                                                                 !/*mi       1171*/
  30  WRITE(IOUT,3000)                                                          !/*mi       1172*/
      GO TO 999                                                                 !/*mi       1173*/
  40  WRITE(IERMES,1000)IA                                                      !/*mi       1174*/
      GO TO 999                                                                 !/*mi       1175*/
  50  WRITE(IERMES,2000)                                                        !/*mi       1176*/
 999  RETURN                                                                    !/*mi       1177*/
!                                                                               !/*mi       1178*/
 1000 FORMAT(' ',127A1)                                                         !/*mi       1179*/
 2000 FORMAT(' ')                                                               !/*mi       1180*/
 3000 FORMAT('1')                                                               !/*mi       1181*/
      END                                                                       !/*mi       1182*/
      SUBROUTINE HBCDI(L,N,IA)                                                  !/*mi       1183*/
!                                                                               !/*mi       1184*/
!     ******************************************************************        !/*mi       1185*/
!     *                                                                *        !/*mi       1186*/
!     *        THIS ROUTINE CONVERTS INTEGER IN BCD                    *        !/*mi       1187*/
!     *                                                                *        !/*mi       1188*/
!     ******************************************************************        !/*mi       1189*/
!                                                                               !/*mi       1190*/
      DIMENSION IA(1)                                                           !/*mi       1191*/
      COMMON/HPPRIN/IFW   ,NW    ,NB    ,IH    ,NHT   ,ICN   ,IPONCE,           !/*mi       1192*/
     &       NH    ,MSTEP ,NOENT ,NOLD  ,IDOLAR,IBLANC,KBINSZ,INO   ,           !/*mi       1193*/
     &       KSQUEZ,NCOLMA,NCOLPA,NLINPA,BIGP  ,ICBLAC,ICSTAR,ICFUNC,           !/*mi       1194*/
     &       IDG(42),MAX(30),IDENT(9)                                           !/*mi       1195*/
!                                                                               !/*mi       1196*/
!                                                                               !/*mi       1197*/
!     ------------------------------------------------------------------        !/*mi       1198*/
!                                                                               !/*mi       1199*/
      IF(L.EQ.0)GO TO 50                                                        !/*mi       1200*/
      M=L                                                                       !/*mi       1201*/
      NN=N                                                                      !/*mi       1202*/
      IA(1)=IDG(41)                                                             !/*mi       1203*/
      IF(L.GT.0)GO TO 10                                                        !/*mi       1204*/
      M=-L                                                                      !/*mi       1205*/
      IA(1)=IDG(39)                                                             !/*mi       1206*/
      NN=N-1                                                                    !/*mi       1207*/
  10  N1=1                                                                      !/*mi       1208*/
      J=N+1                                                                     !/*mi       1209*/
      K1=0                                                                      !/*mi       1210*/
      DO 40 I=1,NN                                                              !/*mi       1211*/
      J=J-1                                                                     !/*mi       1212*/
      IF(M.LT.N1)GO TO 20                                                       !/*mi       1213*/
      N2=10*N1                                                                  !/*mi       1214*/
      K2=MOD(M,N2)                                                              !/*mi       1215*/
      K=(K2-K1)/N1                                                              !/*mi       1216*/
      IA(J)=IDG(K+1)                                                            !/*mi       1217*/
      GO TO 30                                                                  !/*mi       1218*/
  20  IA(J)=IDG(41)                                                             !/*mi       1219*/
  30  K1=K2                                                                     !/*mi       1220*/
  40  N1=N2                                                                     !/*mi       1221*/
      RETURN                                                                    !/*mi       1222*/
!                                                                               !/*mi       1223*/
  50  CONTINUE                                                                  !/*mi       1224*/
      DO 60 I=1,N                                                               !/*mi       1225*/
  60  IA(I)=IDG(41)                                                             !/*mi       1226*/
      RETURN                                                                    !/*mi       1227*/
      END                                                                       !/*mi       1228*/
      SUBROUTINE HMEMEX(N)                                                      !/*mi       1229*/
!                                                                               !/*mi       1230*/
!     ******************************************************************        !/*mi       1231*/
!     *                                                                *        !/*mi       1232*/
!     *       DYNAMIC MEMORY EXTENSION BY N LOCATIONS                  *        !/*mi       1233*/
!     *                                                                *        !/*mi       1234*/
!     ******************************************************************        !/*mi       1235*/
!                                                                               !/*mi       1236*/
      COMMON/HFLAG /ID    ,IDBADD,LID   ,IDLAST,IDHOLD,NBIT  ,NBITCH,           !/*mi       1237*/
     &       NCHAR ,NX0   ,NX1   ,NX2   ,INTER ,INDEX ,LAST  ,LIMIT ,           !/*mi       1238*/
     &       LFIEL ,NEWHIS,NRLENG,NWLIB ,NWFLAG,NBFLAG,NWSTAT,NRHIST,           !/*mi       1239*/
     &       IDISC ,LFHIST,LLHIST,NWHIST,IERR  ,NV    ,NRDIS ,IA2               !/*mi       1240*/
!                                                                               !/*mi       1241*/
      COMMON /MIBOOK/B(512000)                                                  !/*mi       1242*/
      DIMENSION IB(1)                                                           !/*mi       1243*/
      EQUIVALENCE (B(1),IB(1))                                                  !/*mi       1244*/
!                                                                               !/*mi       1245*/
!                                                                               !/*mi       1246*/
!     ------------------------------------------------------------------        !/*mi       1247*/
!                                                                               !/*mi       1248*/
      LFIEL=LFIEL+N                                                             !/*mi       1249*/
      LFIEL=(LFIEL/NRLENG+1)*NRLENG                                             !/*mi       1250*/
      IF(LFIEL.GE.INTER)LFIEL=INTER-1                                           !/*mi       1251*/
      RETURN                                                                    !/*mi       1252*/
      END                                                                       !/*mi       1253*/
      SUBROUTINE HBYTE(W,IAD,NBB,IBIT)                                          !/*mi       1254*/
!                                                                               !/*mi       1255*/
!     ******************************************************************        !/*mi       1256*/
!     *                                                                *        !/*mi       1257*/
!     *       INCREMENTS BYTE STARTING AT BIT N0=IBIT BY W             *        !/*mi       1258*/
!     *       NB=NUMBER OF BITS OF THE BYTE                            *        !/*mi       1259*/
!     *       IF(OLD CONTENT +W).GT.MAXIMUM  TRUNCATION                *        !/*mi       1260*/
!     *                                                                *        !/*mi       1261*/
!     ******************************************************************        !/*mi       1262*/
!                                                                               !/*mi       1263*/
      COMMON/HPPRIN/IFW   ,NW    ,NB    ,IH    ,NHT   ,ICN   ,IPONCE,           !/*mi       1264*/
     &       NH    ,MSTEP ,NOENT ,NOLD  ,IDOLAR,IBLANC,KBINSZ,INO   ,           !/*mi       1265*/
     &       KSQUEZ,NCOLMA,NCOLPA,NLINPA,BIGP  ,ICBLAC,ICSTAR,ICFUNC,           !/*mi       1266*/
     &       IDG(42),MAX(30),IDENT(9)                                           !/*mi       1267*/
!                                                                               !/*mi       1268*/
      DIMENSION IAD(1)                                                          !/*mi       1269*/
!                                                                               !/*mi       1270*/
!     ------------------------------------------------------------------        !/*mi       1271*/
!                                                                               !/*mi       1272*/
      INT=JBYT(IAD,IBIT,NBB)                                                    !/*mi       1273*/
      INT=INT+W                                                                 !/*mi       1274*/
      IF(INT.GT.MAX(NBB))INT=MAX(NBB)                                           !/*mi       1275*/
      CALL SBYT(INT,IAD,IBIT,NBB)                                               !/*mi       1276*/
      RETURN                                                                    !/*mi       1277*/
      END                                                                       !/*mi       1278*/
      SUBROUTINE HSTORE(IDD,LUN)                                                !/*mi       1279*/
!                                                                               !/*mi       1280*/
!     ******************************************************************        !/*mi       1281*/
!     *                                                                *        !/*mi       1282*/
!     *       STORES ON DISK I THE CONTENTS OF HISTOGRAM IDD           *        !/*mi       1283*/
!     *                                                                *        !/*mi       1284*/
!     ******************************************************************        !/*mi       1285*/
!                                                                               !/*mi       1286*/
      COMMON/HFLAG /ID    ,IDBADD,LID   ,IDLAST,IDHOLD,NBIT  ,NBITCH,           !/*mi       1287*/
     &       NCHAR ,NX0   ,NX1   ,NX2   ,INTER ,INDEX ,LAST  ,LIMIT ,           !/*mi       1288*/
     &       LFIEL ,NEWHIS,NRLENG,NWLIB ,NWFLAG,NBFLAG,NWSTAT,NRHIST,           !/*mi       1289*/
     &       IDISC ,LFHIST,LLHIST,NWHIST,IERR  ,NV    ,NRDIS ,IA2               !/*mi       1290*/
!                                                                               !/*mi       1291*/
      COMMON /MIBOOK/B(512000)                                                  !/*mi       1292*/
      DIMENSION IB(1)                                                           !/*mi       1293*/
      EQUIVALENCE (B(1),IB(1))                                                  !/*mi       1294*/
!                                                                               !/*mi       1295*/
!                                                                               !/*mi       1296*/
!     ------------------------------------------------------------------        !/*mi       1297*/
!                                                                               !/*mi       1298*/
      CALL HMACHI                                                               !/*mi       1299*/
!      CALL HDEB                                                                !/*mi       1300*/
      IRET=3                                                                    !/*mi       1301*/
      I=IABS(LUN)                                                               !/*mi       1302*/
!                                                                               !/*mi       1303*/
   5  CALL HGEN(IDD,'HSTORE',-1,IRET)                                           !/*mi       1304*/
      IF(IRET.EQ.0)GO TO 99                                                     !/*mi       1305*/
      LFHIST=IB(IDBADD+1)                                                       !/*mi       1306*/
      IF(LFHIST.LT.0)LFHIST=NEWHIS                                              !/*mi       1307*/
      NWHIST=IB(IDBADD+2)                                                       !/*mi       1308*/
!                                                                               !/*mi       1309*/
      J=IDBADD+3                                                                !/*mi       1310*/
      N=IDBADD+4                                                                !/*mi       1311*/
      WRITE(I)ID,NWHIST,(B(K),K=J,N)                                            !/*mi       1312*/
      N=LFHIST+NWHIST-1                                                         !/*mi       1313*/
      WRITE(I)(B(K),K=LFHIST,N)                                                 !/*mi       1314*/
      IRET=2                                                                    !/*mi       1315*/
      GO TO 5                                                                   !/*mi       1316*/
!                                                                               !/*mi       1317*/
  99  ID=0                                                                      !/*mi       1318*/
      IF(IDD.EQ.0)WRITE(I)ID,ID,ID,ID                                           !/*mi       1319*/
      RETURN                                                                    !/*mi       1320*/
      END                                                                       !/*mi       1321*/
      SUBROUTINE HGEN(ID1,ILABEL,IFL,IRET)                                      !/*mi       1322*/
!                                                                               !/*mi       1323*/
!     ******************************************************************        !/*mi       1324*/
!     *                                                                *        !/*mi       1325*/
!     *       DOES ANY TYPE OF OPERATION FOR 1 OR ALL HISTOGRAMS       *        !/*mi       1326*/
!     *       IRET=2  CONTINUATION OF IRET=3                           *        !/*mi       1327*/
!     *           =3  RETURN CONTROL TO ROUTINE ILABEL ONCE ENOUGH     *        !/*mi       1328*/
!     *               INFORMATION IS GOT                               *        !/*mi       1329*/
!     *                                                                *        !/*mi       1330*/
!     *       IFL>0   COPY DEFINITIVELY ID IN MEMORY IF NOT ALREADY    *        !/*mi       1331*/
!     *       IFL<0   COPY TEMPORARELY ID IN MEMORY IF NOT ALREADY     *        !/*mi       1332*/
!     *           =0  NO DECODING                                      *        !/*mi       1333*/
!     *                                                                *        !/*mi       1334*/
!     ******************************************************************        !/*mi       1335*/
!                                                                               !/*mi       1336*/
      COMMON /MIBOOK/B(512000)                                                  !/*mi       1337*/
      DIMENSION IB(1)                                                           !/*mi       1338*/
      EQUIVALENCE (B(1),IB(1))                                                  !/*mi       1339*/
!                                                                               !/*mi       1340*/
      COMMON/HFLAG /ID    ,IDBADD,LID   ,IDLAST,IDHOLD,NBIT  ,NBITCH,           !/*mi       1341*/
     &       NCHAR ,NX0   ,NX1   ,NX2   ,INTER ,INDEX ,LAST  ,LIMIT ,           !/*mi       1342*/
     &       LFIEL ,NEWHIS,NRLENG,NWLIB ,NWFLAG,NBFLAG,NWSTAT,NRHIST,           !/*mi       1343*/
     &       IDISC ,LFHIST,LLHIST,NWHIST,IERR  ,NV    ,NRDIS ,IA2               !/*mi       1344*/
!                                                                               !/*mi       1345*/
      COMMON / HBITS  / I1,   I2,   I3,   I4,   I5,   I6,   I7,   I8,           !/*mi       1346*/
     &                  I9,   I10,  I11,  I12,  I13,  I14,  I15,  I16,          !/*mi       1347*/
     &I17,  I18,  I19,  I20,  I21,  I22,  I23,  I24,  I25,  I26,  I27,          !/*mi       1348*/
     &I28,  I29,  I30,  I31,  I32,  I33,  I34,  I35,  I123, I230                !/*mi       1349*/
!                                                                               !/*mi       1350*/
      CHARACTER*(*) ILABEL                                                      !/*mi       1351*/
!                                                                               !/*mi       1352*/
!     ------------------------------------------------------------------        !/*mi       1353*/
!                                                                               !/*mi       1354*/
      IF(IRET.EQ.2)GO TO 50                                                     !/*mi       1355*/
!                                                                               !/*mi       1356*/
      CALL HMACHI                                                               !/*mi       1357*/
!                                                                               !/*mi       1358*/
      IF(ID1.EQ.0)GO TO 10                                                      !/*mi       1359*/
      ID=ID1                                                                    !/*mi       1360*/
      CALL HSEARC(IDCONT)                                                       !/*mi       1361*/
      IF(IDCONT.EQ.1)GO TO 99                                                   !/*mi       1362*/
      CALL HBUFF(111,ILABEL,ID1)                                                !/*mi       1363*/
      GO TO 60                                                                  !/*mi       1364*/
!                                                                               !/*mi       1365*/
  10  JD=0                                                                      !/*mi       1366*/
  20  JD=JD+1                                                                   !/*mi       1367*/
      IF(JD.GT.NRHIST)GO TO 60                                                  !/*mi       1368*/
      ID=IB(NX1+JD*NWLIB-1)                                                     !/*mi       1369*/
      CALL HSEARC(IDCONT)                                                       !/*mi       1370*/
!                                                                               !/*mi       1371*/
      GO TO 99                                                                  !/*mi       1372*/
  50  IF(ID1.NE.0)GO TO 60                                                      !/*mi       1373*/
!                                                                               !/*mi       1374*/
      IF(IRET.EQ.2)IRET=3                                                       !/*mi       1375*/
      GO TO 20                                                                  !/*mi       1376*/
!                                                                               !/*mi       1377*/
  60  IRET=0                                                                    !/*mi       1378*/
  99  RETURN                                                                    !/*mi       1379*/
      END                                                                       !/*mi       1380*/
      SUBROUTINE HAUTO(Y)                                                       !/*mi       1381*/
!                                                                               !/*mi       1382*/
!     ******************************************************************        !/*mi       1383*/
!     *                                                                *        !/*mi       1384*/
!     *       COMPUTES AUTOMATICLY THE BIN WIDTH DURING FILLING        *        !/*mi       1385*/
!     *       BY A BYNARY METHOD                                       *        !/*mi       1386*/
!     *                                                                *        !/*mi       1387*/
!     ******************************************************************        !/*mi       1388*/
!                                                                               !/*mi       1389*/
      COMMON /MIBOOK/B(512000)                                                  !/*mi       1390*/
      DIMENSION IB(1)                                                           !/*mi       1391*/
      EQUIVALENCE (B(1),IB(1))                                                  !/*mi       1392*/
!                                                                               !/*mi       1393*/
      COMMON/HFLAG /ID    ,IDBADD,LID   ,IDLAST,IDHOLD,NBIT  ,NBITCH,           !/*mi       1394*/
     &       NCHAR ,NX0   ,NX1   ,NX2   ,INTER ,INDEX ,LAST  ,LIMIT ,           !/*mi       1395*/
     &       LFIEL ,NEWHIS,NRLENG,NWLIB ,NWFLAG,NBFLAG,NWSTAT,NRHIST,           !/*mi       1396*/
     &       IDISC ,LFHIST,LLHIST,NWHIST,IERR  ,NV    ,NRDIS ,IA2               !/*mi       1397*/
!                                                                               !/*mi       1398*/
      COMMON / HBITS  / I1,   I2,   I3,   I4,   I5,   I6,   I7,   I8,           !/*mi       1399*/
     &                  I9,   I10,  I11,  I12,  I13,  I14,  I15,  I16,          !/*mi       1400*/
     &I17,  I18,  I19,  I20,  I21,  I22,  I23,  I24,  I25,  I26,  I27,          !/*mi       1401*/
     &I28,  I29,  I30,  I31,  I32,  I33,  I34,  I35,  I123, I230                !/*mi       1402*/
!                                                                               !/*mi       1403*/
      COMMON/HPFILL/IFPROX,IFWPRX,IFWPRY,NBPROX,ICHAN                           !/*mi       1404*/
      COMMON/HGARB/DB,ICN,ITAMP,J,NOENT,BFW,IDL,IFW,K,M                         !/*mi       1405*/
     &            ,NWPROX,X0,X1,XCN,GA(3)                                       !/*mi       1406*/
!                                                                               !/*mi       1407*/
!     ------------------------------------------------------------------        !/*mi       1408*/
!                                                                               !/*mi       1409*/
      ICN=IB(IFWPRX-1)                                                          !/*mi       1410*/
      XCN=ICN                                                                   !/*mi       1411*/
      NWPROX=ICN+3                                                              !/*mi       1412*/
      ITAMP=0                                                                   !/*mi       1413*/
      M=1+I9                                                                    !/*mi       1414*/
      X=Y                                                                       !/*mi       1415*/
      NOENT=IB(IFPROX)                                                          !/*mi       1416*/
      IF(NOENT.GE.3)GO TO 50                                                    !/*mi       1417*/
      IF(NOENT.EQ.2)GO TO 10                                                    !/*mi       1418*/
      B(IFWPRX)=X-0.001*ABS(X)                                                  !/*mi       1419*/
      IF(X.EQ.0.)B(IFWPRX)=-1.E-20                                              !/*mi       1420*/
      B(IFWPRX+1)=X+XCN                                                         !/*mi       1421*/
      B(IFWPRX+2)=1.                                                            !/*mi       1422*/
      GO TO 999                                                                 !/*mi       1423*/
!                                                                               !/*mi       1424*/
  10  IDL=0                                                                     !/*mi       1425*/
      IF(X.NE.B(IFWPRX))GO TO 20                                                !/*mi       1426*/
      X=1.001*X                                                                 !/*mi       1427*/
  20  IF(X.LT.B(IFWPRX))GO TO 30                                                !/*mi       1428*/
      B(IFWPRX+1)=X+0.001*ABS(X)                                                !/*mi       1429*/
      IF(X.EQ.0.)B(IFWPRX+1)=1.E+20                                             !/*mi       1430*/
      GO TO 40                                                                  !/*mi       1431*/
!                                                                               !/*mi       1432*/
  30  BFW=B(IFWPRX)                                                             !/*mi       1433*/
      IDL=1                                                                     !/*mi       1434*/
      B(IFWPRX)=X-0.001*ABS(X)                                                  !/*mi       1435*/
      IF(X.EQ.0.)B(IFWPRX)=-1.E-20                                              !/*mi       1436*/
      B(IFWPRX+1)=BFW+0.001*ABS(BFW)                                            !/*mi       1437*/
      IF(BFW.EQ.0.)B(IFWPRX+1)=1.E+20                                           !/*mi       1438*/
  40  X0=B(IFWPRX)                                                              !/*mi       1439*/
      X1=B(IFWPRX+1)                                                            !/*mi       1440*/
      CALL HBIN(X0,X1,IB(IFWPRX-1),B(IFWPRX),X1,ICN,DB)                         !/*mi       1441*/
      B(IFWPRX+2)=1./DB                                                         !/*mi       1442*/
      B(IFWPRX+1)=B(IFWPRX)+XCN*DB                                              !/*mi       1443*/
      IF(IDL.EQ.0)GO TO 999                                                     !/*mi       1444*/
      I=(BFW-B(IFWPRX))*B(IFWPRX+2) + 2.                                        !/*mi       1445*/
      B(IFPROX+I)=B(IFPROX+2)                                                   !/*mi       1446*/
      B(IFPROX+2)=0.                                                            !/*mi       1447*/
      IF(I9.EQ.0)GO TO 999                                                      !/*mi       1448*/
      B(IFPROX+NWPROX+I)=B(IFPROX+NWPROX+2)                                     !/*mi       1449*/
      B(IFPROX+NWPROX+2)=0.                                                     !/*mi       1450*/
      GO TO 999                                                                 !/*mi       1451*/
!                                                                               !/*mi       1452*/
  50  IFW=IFPROX+ICN+3                                                          !/*mi       1453*/
  60  X0=B(IFWPRX)                                                              !/*mi       1454*/
      X1=B(IFWPRX+1)                                                            !/*mi       1455*/
      IF(X.LT.X0)GO TO 70                                                       !/*mi       1456*/
      IF(X.GE.X1)GO TO 90                                                       !/*mi       1457*/
      GO TO 200                                                                 !/*mi       1458*/
!                                                                               !/*mi       1459*/
  70  B(IFWPRX)=2.*X0-X1                                                        !/*mi       1460*/
      B(IFWPRX+2)=B(IFWPRX+2)/2.                                                !/*mi       1461*/
      IF(ITAMP.EQ.1)GO TO 75                                                    !/*mi       1462*/
      ITAMP=1                                                                   !/*mi       1463*/
      IB(IFW)=IB(IFW)+1                                                         !/*mi       1464*/
  75  J=IFPROX+ICN+2                                                            !/*mi       1465*/
      DO 80 I=1,ICN,2                                                           !/*mi       1466*/
      J=J-1                                                                     !/*mi       1467*/
      K=IFPROX+ICN-I+1                                                          !/*mi       1468*/
      B(J)=B(K)+B(K+1)                                                          !/*mi       1469*/
      IF(I9.NE.0)B(J+NWPROX)=B(K+NWPROX)+B(K+NWPROX+1)                          !/*mi       1470*/
  80  CONTINUE                                                                  !/*mi       1471*/
!                                                                               !/*mi       1472*/
      CALL VZERO(B(IFPROX+2),ICN/2)                                             !/*mi       1473*/
      IF(I9.NE.0)CALL VZERO(B(IFPROX+NWPROX+2),ICN/2)                           !/*mi       1474*/
      GO TO 60                                                                  !/*mi       1475*/
!                                                                               !/*mi       1476*/
  90  B(IFWPRX+2)=B(IFWPRX+2)/2.                                                !/*mi       1477*/
      B(IFWPRX+1)=B(IFWPRX)+XCN/B(IFWPRX+2)                                     !/*mi       1478*/
      IF(ITAMP.EQ.1)GO TO 95                                                    !/*mi       1479*/
      ITAMP=1                                                                   !/*mi       1480*/
      IB(IFW)=IB(IFW)+1                                                         !/*mi       1481*/
  95  J=IFPROX+1                                                                !/*mi       1482*/
      DO 100 I=1,ICN,2                                                          !/*mi       1483*/
      J=J+1                                                                     !/*mi       1484*/
      K=I+IFPROX+1                                                              !/*mi       1485*/
      B(J)=B(K)+B(K+1)                                                          !/*mi       1486*/
      IF(I9.NE.0)B(J+NWPROX)=B(K+NWPROX)+B(K+NWPROX+1)                          !/*mi       1487*/
 100  CONTINUE                                                                  !/*mi       1488*/
      CALL VZERO(B(J+1),ICN/2)                                                  !/*mi       1489*/
      IF(I9.NE.0)CALL VZERO(B(J+NWPROX+1),ICN/2)                                !/*mi       1490*/
!                                                                               !/*mi       1491*/
      GO TO 60                                                                  !/*mi       1492*/
!                                                                               !/*mi       1493*/
 200  IF(NOENT/(IB(IFW)+1).LT.20)GO TO 999                                      !/*mi       1494*/
      B(IFWPRX)=B(IFWPRX)-0.000001/B(IFWPRX+2)                                  !/*mi       1495*/
      IB(IFW)=0                                                                 !/*mi       1496*/
      CALL HSIFLA(5,0)                                                          !/*mi       1497*/
!                                                                               !/*mi       1498*/
 999  RETURN                                                                    !/*mi       1499*/
      END                                                                       !/*mi       1500*/
      FUNCTION FLOARG(A)                                                        !/*mi       1501*/
      FLOARG=A                                                                  !/*mi       1502*/
      RETURN                                                                    !/*mi       1503*/
      END                                                                       !/*mi       1504*/
      FUNCTION INTARG(I)                                                        !/*mi       1505*/
      INTARG=I                                                                  !/*mi       1506*/
      RETURN                                                                    !/*mi       1507*/
      END                                                                       !/*mi       1508*/
      SUBROUTINE NOARG(I)                                                       !/*mi       1509*/
      RETURN                                                                    !/*mi       1510*/
      END                                                                       !/*mi       1511*/
      FUNCTION JBIT (IZW,IZP)                                                   !/*mi       1512*/
      JBIT = IBITS (IZW,IZP-1,1)                                                !/*mi       1513*/
      RETURN                                                                    !/*mi       1514*/
      END                                                                       !/*mi       1515*/
      FUNCTION JBYT (IZW,IZP,NZB)                                               !/*mi       1516*/
      PARAMETER   (NBITPW=32)                                                   !/*mi       1517*/
      JBYT = IBITS (IZW,IZP-1,NZB)                                              !/*mi       1518*/
      RETURN                                                                    !/*mi       1519*/
      END                                                                       !/*mi       1520*/
      SUBROUTINE SBIT0 (IZW,IZP)                                                !/*mi       1521*/
      IZW = IBCLR (IZW, IZP-1)                                                  !/*mi       1522*/
      RETURN                                                                    !/*mi       1523*/
      END                                                                       !/*mi       1524*/
      SUBROUTINE SBIT1 (IZW,IZP)                                                !/*mi       1525*/
      IZW = IBSET (IZW, IZP-1)                                                  !/*mi       1526*/
      RETURN                                                                    !/*mi       1527*/
      END                                                                       !/*mi       1528*/
      SUBROUTINE SBYT (IT,IZW,IZP,NZB)                                          !/*mi       1529*/
      PARAMETER   (NBITPW=32)                                                   !/*mi       1530*/
      PARAMETER   (IALL11 = -1)                                                 !/*mi       1531*/
      MSK = ISHFT  (IALL11,-NBITPW+NZB)                                         !/*mi       1532*/
      IZW = IOR ( IAND (IZW, NOT(ISHFT (MSK,IZP-1)))                            !/*mi       1533*/
     &,           ISHFT (IAND(IT,MSK),IZP-1))                                   !/*mi       1534*/
      RETURN                                                                    !/*mi       1535*/
      END                                                                       !/*mi       1536*/
      SUBROUTINE UBLOW (MS,MT,NCHP)                                             !/*mi       1537*/
      DIMENSION    MS(99), MT(99), NCHP(9)                                      !/*mi       1538*/
      PARAMETER    (IALLBL =  538976288 )                                       !/*mi       1539*/
      PARAMETER    (IBLAN1 =    2105376 )                                       !/*mi       1540*/
      PARAMETER    (MASK1  =  -16777216 )                                       !/*mi       1541*/
      NCH = NCHP(1)                                                             !/*mi       1542*/
      IF   (NCH)             91, 29, 11                                         !/*mi       1543*/
   11 NWS    = ISHFT (NCH,-2)                                                   !/*mi       1544*/
      NTRAIL = IAND (NCH,3)                                                     !/*mi       1545*/
      JT     = 0                                                                !/*mi       1546*/
      IF (NWS.EQ.0)          GO TO 26                                           !/*mi       1547*/
!--                Unpack the initial complete words                            !/*mi       1548*/
      DO 24 JS=1,NWS                                                            !/*mi       1549*/
      MWD      = MS(JS)                                                         !/*mi       1550*/
      MT(JT+1) = IOR (IBLAN1,IAND(MASK1,MWD))                                   !/*mi       1551*/
      MT(JT+2) = IOR (IBLAN1,IAND(MASK1,ISHFT(MWD, 8)))                         !/*mi       1552*/
      MT(JT+3) = IOR (IBLAN1,IAND(MASK1,ISHFT(MWD,16)))                         !/*mi       1553*/
      MT(JT+4) = IOR (IBLAN1,           ISHFT(MWD,24) )                         !/*mi       1554*/
   24 JT = JT + 4                                                               !/*mi       1555*/
      IF (NTRAIL.EQ.0)       RETURN                                             !/*mi       1556*/
!--                Unpack the trailing word                                     !/*mi       1557*/
   26 MWD = MS(NWS+1)                                                           !/*mi       1558*/
      DO 28 JS=1,NTRAIL                                                         !/*mi       1559*/
      MT(JT+1) = IOR (IBLAN1,IAND(MASK1,MWD))                                   !/*mi       1560*/
      MWD = ISHFT (MWD,8)                                                       !/*mi       1561*/
   28 JT = JT + 1                                                               !/*mi       1562*/
   29 RETURN                                                                    !/*mi       1563*/
   91 write(*,*) ' !!!!!!!  ERROR INDICATED IN UBLOW   !!!!!!!!'                !/*mi       1564*/
      END                                                                       !/*mi       1565*/
      SUBROUTINE UCOPY (A,B,N)                                                  !/*mi       1566*/
      DIMENSION A(*),B(*)                                                       !/*mi       1567*/
!--                NO OVERLAP  OR  BEGINNING OF A ON END OF B                   !/*mi       1568*/
      IF (N.EQ.0) RETURN                                                        !/*mi       1569*/
         DO 21 I=1,N                                                            !/*mi       1570*/
   21 B(I)=A(I)                                                                 !/*mi       1571*/
      RETURN                                                                    !/*mi       1572*/
      END                                                                       !/*mi       1573*/
      SUBROUTINE UCOPY2 (A,B,N)                                                 !/*mi       1574*/
      DIMENSION A(*),B(*)                                                       !/*mi       1575*/
      IF (N.LT.2)            GO TO 41                                           !/*mi       1576*/
      IA = LOC (A)                                                              !/*mi       1577*/
      IB = LOC (B)                                                              !/*mi       1578*/
      IF   (IA-IB)           20,99,10                                           !/*mi       1579*/
!--                End of B may overlap beginning of A                          !/*mi       1580*/
   10 DO 15 I=1,N                                                               !/*mi       1581*/
   15 B(I) = A(I)                                                               !/*mi       1582*/
      RETURN                                                                    !/*mi       1583*/
!--                End of A may overlap with beginning of B                     !/*mi       1584*/
   20 DO 25 I=N,1,-1                                                            !/*mi       1585*/
   25 B(I) = A(I)                                                               !/*mi       1586*/
      RETURN                                                                    !/*mi       1587*/
!--                Case N=1, gives trouble on SGI                               !/*mi       1588*/
   41 IF (N.LE.0)            RETURN                                             !/*mi       1589*/
      B(1) = A(1)                                                               !/*mi       1590*/
   99 RETURN                                                                    !/*mi       1591*/
      END                                                                       !/*mi       1592*/
      SUBROUTINE UCTOH (MS,MT,NPW,NCH)                                          !/*mi       1593*/
!?????????????????????????????????????????                                      !/*mi       1594*/
      CHARACTER    MS*99                                                        !/*mi       1595*/
      DIMENSION    MT(99)                                                       !/*mi       1596*/
      PARAMETER   (NCHAPW=4)                                                    !/*mi       1597*/
      CHARACTER    CHWORD*(NCHAPW)                                              !/*mi       1598*/
      CHARACTER    BLANK *(NCHAPW)                                              !/*mi       1599*/
      PARAMETER   (BLANK = ' ')                                                 !/*mi       1600*/
      INTEGER      IWORD                                                        !/*mi       1601*/
      EQUIVALENCE (IWORD,CHWORD)                                                !/*mi       1602*/
      IF   (NCH)             91, 29, 11                                         !/*mi       1603*/
   11 IF (NPW.LE.0)          GO TO 91                                           !/*mi       1604*/
      IF (NPW.EQ.1)          GO TO 21                                           !/*mi       1605*/
      IF (NPW.LT.NCHAPW)     GO TO 31                                           !/*mi       1606*/
!--------          NPW = maximum                                                !/*mi       1607*/
      JS     = 0                                                                !/*mi       1608*/
      NWT    = NCH / NCHAPW                                                     !/*mi       1609*/
      NTRAIL = NCH - NWT*NCHAPW                                                 !/*mi       1610*/
      DO 14  JT=1,NWT                                                           !/*mi       1611*/
      jsb=JS+1                                                                  !/*mi       1612*/
      jse=JS+nchapw                                                             !/*mi       1613*/
      CHWORD = MS(jsb:jse)                                                      !/*mi       1614*/
      MT(JT) = IWORD                                                            !/*mi       1615*/
   14 JS     = JS + NCHAPW                                                      !/*mi       1616*/
      IF (NTRAIL.EQ.0)       RETURN                                             !/*mi       1617*/
      CHWORD    = MS(JS+1:JS+NTRAIL)                                            !/*mi       1618*/
      MT(NWT+1) = IWORD                                                         !/*mi       1619*/
      RETURN                                                                    !/*mi       1620*/
!--------          NPW = 1                                                      !/*mi       1621*/
!--                equivalent to 'CALL UCTOH1(MS,MT,NCH)'                       !/*mi       1622*/
   21 CHWORD = BLANK                                                            !/*mi       1623*/
      DO 24 JS=1,NCH                                                            !/*mi       1624*/
      CHWORD(1:1) = MS(JS:JS)                                                   !/*mi       1625*/
      MT(JS)      = IWORD                                                       !/*mi       1626*/
   24 CONTINUE                                                                  !/*mi       1627*/
   29 RETURN                                                                    !/*mi       1628*/
!--------          NPW = 2 ...                                                  !/*mi       1629*/
   31 CHWORD = BLANK                                                            !/*mi       1630*/
      JS     = 0                                                                !/*mi       1631*/
      NWT    = NCH / NPW                                                        !/*mi       1632*/
      NTRAIL = NCH - NWT*NPW                                                    !/*mi       1633*/
      DO 34  JT=1,NWT                                                           !/*mi       1634*/
      CHWORD(1:NPW) = MS(JS+1:JS+NPW)                                           !/*mi       1635*/
      MT(JT) = IWORD                                                            !/*mi       1636*/
   34 JS     = JS + NPW                                                         !/*mi       1637*/
      IF (NTRAIL.EQ.0)       RETURN                                             !/*mi       1638*/
      CHWORD    = MS(JS+1:JS+NTRAIL)                                            !/*mi       1639*/
      MT(NWT+1) = IWORD                                                         !/*mi       1640*/
      RETURN                                                                    !/*mi       1641*/
   91 write(*,*) ' !!!!!!!  ERROR INDICATED IN UCTOH   !!!!!!!!'                !/*mi       1642*/
      END                                                                       !/*mi       1643*/
      SUBROUTINE UCTOH1 (MS,MT,NCH)                                             !/*mi       1644*/
      CHARACTER    MS*99                                                        !/*mi       1645*/
      DIMENSION    MT(99)                                                       !/*mi       1646*/
      PARAMETER   (NCHAPW=4)                                                    !/*mi       1647*/
      CHARACTER    CHWORD*(NCHAPW)                                              !/*mi       1648*/
      CHARACTER    BLANK *(NCHAPW)                                              !/*mi       1649*/
      PARAMETER   (BLANK = ' ')                                                 !/*mi       1650*/
      INTEGER      IWORD                                                        !/*mi       1651*/
      EQUIVALENCE (IWORD,CHWORD)                                                !/*mi       1652*/
      IF   (NCH)             91, 29, 11                                         !/*mi       1653*/
   11 CHWORD = BLANK                                                            !/*mi       1654*/
      DO 24 JS=1,NCH                                                            !/*mi       1655*/
      CHWORD(1:1) = MS(JS:JS)                                                   !/*mi       1656*/
      MT(JS)      = IWORD                                                       !/*mi       1657*/
   24 CONTINUE                                                                  !/*mi       1658*/
   29 RETURN                                                                    !/*mi       1659*/
   91 write(*,*) ' !!!!!!!  ERROR INDICATED IN UCTOH1   !!!!!!!!'               !/*mi       1660*/
      END                                                                       !/*mi       1661*/
      SUBROUTINE VBLANK (A,N)                                                   !/*mi       1662*/
      INTEGER      A(*), BLANK                                                  !/*mi       1663*/
      DATA  BLANK  / 4H     /                                                   !/*mi       1664*/
      IF (N.LE.0)  RETURN                                                       !/*mi       1665*/
      DO 9 I= 1,N                                                               !/*mi       1666*/
    9 A(I)= BLANK                                                               !/*mi       1667*/
      RETURN                                                                    !/*mi       1668*/
      END                                                                       !/*mi       1669*/
      SUBROUTINE VZERO (IA,N)                                                   !/*mi       1670*/
      DIMENSION IA(*)                                                           !/*mi       1671*/
      IF (N.LE.0)  RETURN                                                       !/*mi       1672*/
      DO 9 I= 1,N                                                               !/*mi       1673*/
    9 IA(I)= 0                                                                  !/*mi       1674*/
#endif                                                                          /*!mi       1675*/
      RETURN                                                                    !/*mi       1676*/
      END                                                                       !/*mi       1677*/
