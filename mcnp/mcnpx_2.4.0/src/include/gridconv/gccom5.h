!     $Id: gccom5.h,v 1.6 2002/03/27 18:14:10 gwm Exp $                       !/*gc          1*/
!                                                                               !/*gc          2*/
      parameter (mdas=4000000)                                                  !/*gc          3*/
      parameter (mxg=20,mxv=10)                                                 !/*gc          4*/
!                                                                               !/*gc          5*/
      character kodvar*8,vervar*5,loddatvar*28,aid*80,hfd*30,hfp*30,            !/*gc          6*/
     & hdv(mxv)*10,hdd(mxv)*10,hdx(mxg,mxv)*10,hip(34)*10,hdb(2)*9              !/*gc          7*/
      common /charcm/ kodvar,vervar,loddatvar,aid,hfd,hfp,hdv,hdd,hdx,          !/*gc          8*/
     & hip,hdb                                                                  !/*gc          9*/
!                                                                               !/*gc         10*/
      common /datin/ ngtyp(mxg),igpt(mxg),mxgv(mxg),mxgc(mxg),                  !/*gc         11*/
     & nugd(mxg),mxgt(mxg),ngv(mxg,mxv),nmv(mxg,mxv),njt(mxg),                  !/*gc         12*/
     & ng1(mxg),ng2(mxg),ng3(mxg),nps                                           !/*gc         13*/
      common /datft/ ergl(mxg),ergh(mxg)                                        !/*gc         14*/
      common /local/ lgdat,lpdat,ngd,nxv,mfltyp,                                !/*gc         15*/
     & lgdx,lgdy,lgdz,lpdx,lpdy,lpdz,ixs,iys,izs,                               !/*gc         16*/
     & lgdr,lgdt,lpdr,lpdt,lgdp,lgda,lpdp,lpda,irs,its                          !/*gc         17*/
!                                                                               !/*gc         18*/
!       Dynamically allocated common                                            !/*gc         19*/
      common /dac/ das(mdas)                                                    !/*gc         20*/
!                                                                               !/*gc         21*/
