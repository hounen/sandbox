!#ident mreset.F from deck mq                                                   !/*mq          1*/
      subroutine mreset(ks)                                                     !/*mq          3*/
#ifdef MCPLOT                                                                   /*!mqx         1*/
!        execute the reset aa command, where ks is the index of aa.             !/*mq          4*/
      use global_data                                                           !/*mqx         2*/
      use mcplot_data                                                           !/*mqx         3*/
!                                                                               !/*mqx         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*mqx         5*/
      implicit integer(kindi) (i-n)                                             !/*mqx         6*/
!                                                                               !/*mqx         7*/
      logical lv                                                                !/*mq          7*/
!                                                                               !/*mq          8*/
      lv=.not.(keys(ks).eq.'reset')                                             !/*mq          9*/
      go to(900,900,900, 10,900,900,900,900,900,900,900,900,900,900,900,        !/*mq4b        1*/
     &      900,900, 20, 30, 40, 10, 70, 80, 70, 70, 70, 70, 70, 90,100,        !/*mq4b        2*/
     &       90,120,120,130,130,130,130,140,140,150,160,160,160,160,170,        !/*mq4b        3*/
     &      180,180,190,200,210,220,900,900,230,240,260,270)ks                  !/*mq4b        4*/
   10 koplot=0                                                                  !/*mq         14*/
      if(jgf.eq.1)jgf=0                                                         !/*mq         15*/
      if(lv)return                                                              !/*mq         16*/
   20 mtal=min(1,ntal)                                                          !/*mq         17*/
      if(lv)return                                                              !/*mq         18*/
   30 nonorm=0                                                                  !/*mq         19*/
      if(lv)return                                                              !/*mq         20*/
   40 do 50 j=1,3                                                               !/*mq         21*/
      scalf(1,j)=1.                                                             !/*mq         22*/
   50 scalf(2,j)=0.                                                             !/*mq         23*/
      if(lv)return                                                              !/*mq         24*/
      do 60 i=1,7                                                               !/*mq         25*/
      titles(i)=' '                                                             !/*mq         26*/
   60 ititle(i)=0                                                               !/*mq         27*/
      go to 80                                                                  !/*mq         28*/
   70 i=index('xyzltqs',keys(ks)(1:1))                                          !/*mq         29*/
      ititle(i)=0                                                               !/*mq         30*/
      titles(i)=' '                                                             !/*mq         31*/
      if(i.ne.5)return                                                          !/*mq         32*/
      ititle(6)=0                                                               !/*mq         33*/
      titles(6)=' '                                                             !/*mq         34*/
      return                                                                    !/*mq         35*/
   80 lput=0                                                                    !/*mq         36*/
      if(lv)return                                                              !/*mq         37*/
   90 nfree=1                                                                   !/*mq         38*/
      ifree(1)=7                                                                !/*mq         39*/
      if(lv.and.keys(ks).ne.'set')return                                        !/*mq         40*/
  100 do 110 i=1,8                                                              !/*mq         41*/
  110 kbin(i,1)=0                                                               !/*mq         42*/
      if(lv)return                                                              !/*mq         43*/
  120 itfc=0                                                                    !/*mq         44*/
      if(lv)return                                                              !/*mq         45*/
  130 lax=0                                                                     !/*mq         46*/
      if(lv)return                                                              !/*mq         47*/
      jlim(1)=0                                                                 !/*mq         48*/
      jlim(2)=0                                                                 !/*mq         49*/
      go to 150                                                                 !/*mq         50*/
  140 jlim(index('xy',keys(ks)(1:1)))=0                                         !/*mq         51*/
      return                                                                    !/*mq         52*/
  150 mscal=1                                                                   !/*mq         53*/
      if(lv)return                                                              !/*mq         54*/
  160 kurv=0                                                                    !/*mq         55*/
      if(lv)return                                                              !/*mq         56*/
  170 noerbr=0                                                                  !/*mq         57*/
      if(lv)return                                                              !/*mq         58*/
  180 cthick=.02                                                                !/*mq         59*/
      if(lv)return                                                              !/*mq         60*/
  190 legend=1                                                                  !/*mq         61*/
      if(lv)return                                                              !/*mq         62*/
  200 contur(1)=5.                                                              !/*mq         63*/
      contur(2)=95.                                                             !/*mq         64*/
      contur(3)=10.                                                             !/*mq         65*/
      ipct=1                                                                    !/*mq         66*/
      if(lv)return                                                              !/*mq         67*/
  210 j3d=0                                                                     !/*mq         68*/
      if(lv)return                                                              !/*mq         69*/
  220 pht(1)=225.                                                               !/*mq         70*/
      pht(2)=35.                                                                !/*mq         71*/
      if(lv)return                                                              !/*mq4b        5*/
  230 lpert=0                                                                   !/*mq4b        6*/
      if(lv)return                                                              !/*mq4b        7*/
  240 if(kxsplt.eq.0)return                                                     !/*mq4b        8*/
      kxsplt=1                                                                  !/*mq4b        9*/
      kxspma=nmt(lnmt+1)                                                        !/*mq4b       10*/
      write(xspttl,'(5x,1hm,i4)')kxspma                                         !/*mq4b       11*/
      lax=4                                                                     !/*mq4b       12*/
      kurv=2                                                                    !/*mq4b       13*/
      cthick=0.01                                                               !/*mq4b       14*/
      noerbr=1                                                                  !/*mq4b       15*/
      do 250 j=1,mxe1                                                           !/*mq4b       16*/
      kxspnx(lxnx+j)=0                                                          !/*mq4b       17*/
      kxspen(lxen+j)=0                                                          !/*mq4b       18*/
      kxspie(lxie+j)=0                                                          !/*mq4b       19*/
  250 kxspxs(lxxs+j)=0                                                          !/*mq4b       20*/
      if(lv)return                                                              !/*mq4b       21*/
  260 kxspmt=-1                                                                 !/*mq4b       22*/
      if(kxspar.eq.2.and.mcal.eq.0)kxspmt=-5                                    !/*mq4b       23*/
      if(kxspar.eq.3.and.kxsptp.eq.10)kxspmt=3                                  !/*mqx         8*/
      if(lv)return                                                              !/*mq4b       25*/
  270 kxspar=1                                                                  !/*mq4b       26*/
      if(lv)return                                                              !/*mq4b       27*/
      kxspmt=-1                                                                 !/*mq4b       28*/
!                                                                               !/*mq4b       29*/
  900 continue                                                                  !/*mqx         9*/
#else                                                                           /*!mqx        10*/
      koplot=0                                                                  !/*mqx        11*/
#endif                                                                          /*!mqx        12*/
      return                                                                    !/*mqx        13*/
      end                                                                       !/*mq         73*/
