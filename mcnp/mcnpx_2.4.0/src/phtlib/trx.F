!     $Id: trx.F,v 1.4 2002/03/27 18:16:23 gwm Exp $                          !/*tr          1*/
      program trx                                                               !/*tr          2*/
#include <htape3x/com32b.h>                                                     /*!tr          3*/
      external bdtr                                                             !/*tr          4*/
#include <htape3x/big.h>                                                        /*!tr          5*/
#include <trx/ioput.h>                                                          /*!tr          6*/
#include <htape3x/chartr.h>                                                     /*!tr          7*/
      character *8 new                                                          !/*tr          8*/
      data new /'new'/                                                          !/*tr          9*/
      data lin, lout /0,1/                                                      !/*tr         10*/
!                                                                               !/*tr         11*/
      call exemes (isub(1),msub(1),ndef)                                        !/*tr         12*/
      nin2=5                                                                    !/*tr         13*/
      nout2=6                                                                   !/*tr         14*/
      nout=9                                                                    !/*tr         15*/
      if (inqire(otrx).ne.0.and.otrx.eq.'otrx') otrx='otrxa'                    !/*tr         16*/
      call uopen (nout2,nout,otrx,'f')                                          !/*tr         17*/
      write (nout2,*) 'input mode?'                                             !/*tr         18*/
      write (nout2,*) 'enter +1 for binary input from BINLIB'                   !/*tr         19*/
      write (nout2,*) 'enter 0 for bcd input from BCDLIB'                       !/*tr         20*/
#ifdef LCS                                                                      /*!tr         21*/
      write (nout2,*) 'enter -1 to generate new library from LLL data'          !/*tr         22*/
#endif                                                                          /*!tr         23*/
      read (nin2,*) lin                                                         !/*tr         24*/
      write (nout2,*) 'output mode?'                                            !/*tr         25*/
      write (nout2,*) 'enter +1 for binary output on BINLIB'                    !/*tr         26*/
      write (nout2,*) 'enter 0 for bcd output on BCDLIB'                        !/*tr         27*/
      write (nout2,*) 'enter -1 to print out specific isotopic data'            !/*tr         28*/
      read (nin2,*) lout                                                        !/*tr         29*/
      if (lin) 10,20,30                                                         !/*tr         30*/
   10 continue                                                                  !/*tr         31*/
#ifdef LCS                                                                      /*!tr         32*/
      open (10,file=filin,status='old')                                         !/*tr         33*/
      open (98,file=error1,status=new)                                          !/*tr         34*/
      open (99,file=error2,status=new)                                          !/*tr         35*/
      call llib (k,l)                                                           !/*tr         36*/
      go to 40                                                                  !/*tr         37*/
#endif                                                                          /*!tr         38*/
#ifndef LCS                                                                     /*!tr         39*/
      write (nout2,*) 'input option -1 not allowed'                             !/*tr         40*/
      write (nout,*) 'input option -1 not allowed'                              !/*tr         41*/
      stop                                                                      !/*tr         42*/
#endif                                                                          /*!tr         43*/
   20 continue                                                                  !/*tr         44*/
      open (20,file=bcdlib,status='old')                                        !/*tr         45*/
      call bcdin (k,l)                                                          !/*tr         46*/
      go to 40                                                                  !/*tr         47*/
   30 continue                                                                  !/*tr         48*/
      open (30,file=binlib,status='old',access='sequential',form=               !/*tr         49*/
     & 'unformatted')                                                           !/*tr         50*/
      call binin (k,l)                                                          !/*tr         51*/
   40 continue                                                                  !/*tr         52*/
      write (nout2,100) k,l,idk(l)                                              !/*tr         53*/
      write (nout,100) l,k,idk(l)                                               !/*tr         54*/
      if (lout.ge.0) call pout                                                  !/*tr         55*/
      if (lout) 70,50,60                                                        !/*tr         56*/
   50 continue                                                                  !/*tr         57*/
      if (inqire(bcdlib).ne.0.and.bcdlib.eq.'bcdlib') bcdlib='bcdliba'          !/*tr         58*/
      call uopen (nout2,20,bcdlib,'f')                                          !/*tr         59*/
      call bcdout(l)                                                            !/*tr         60*/
      go to 80                                                                  !/*tr         61*/
   60 continue                                                                  !/*tr         62*/
      if (inqire(binlib).ne.0.and.binlib.eq.'binlib') binlib='binliba'          !/*tr         63*/
!     call uopen (nout2,30,binlib,'u')                                          !/*tr         64*/
      if (inqire(binlib).ne.0) call unique (binlib,nout2)                       !/*tr         65*/
      open (30,file=binlib,status=new,access='sequential',form=                 !/*tr         66*/
     & 'unformatted')                                                           !/*tr         67*/
      call binout (k,l)                                                         !/*tr         68*/
      go to 80                                                                  !/*tr         69*/
   70 continue                                                                  !/*tr         70*/
      write (nout2,90)                                                          !/*tr         71*/
      read (nin2,*) iz,ia                                                       !/*tr         72*/
      if (iz.le.0.or.ia.le.0) go to 80                                          !/*tr         73*/
      call list (iz,ia)                                                         !/*tr         74*/
      go to 70                                                                  !/*tr         75*/
   80 continue                                                                  !/*tr         76*/
      stop                                                                      !/*tr         77*/
!                                                                               !/*tr         78*/
   90 format (' iz,ia?')                                                        !/*tr         79*/
  100 format (' len1=',i8/' len2=',i8/,' len3=',i8//)                           !/*tr         80*/
      end                                                                       !/*tr         81*/
