!#ident utask.F from deck ut                                                    !/*ut          1*/
      subroutine utask                                                          !/*ut          2*/
!        initialize for the current task.                                       !/*ut          3*/
      use global_data                                                           !/*utx         1*/
!                                                                               !/*utx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*utx         3*/
      implicit integer(kindi) (i-n)                                             !/*utx         4*/
!                                                                               !/*utx         5*/
!                                                                               !/*ut          5*/
!        initialize /tskcom/ and /pblcom/.                                      !/*ut          6*/
      do 10 i=1,ntskcm                                                          !/*ut          7*/
   10 gtskcm(i)=0.                                                              !/*ut          8*/
      do 20 i=1,ltskcm                                                          !/*ut          9*/
   20 jtskcm(i)=0                                                               !/*ut         10*/
      do 30 i=1,npblcm+1                                                        !/*ut         11*/
      gpblcm(i)=0.                                                              !/*ut         12*/
      do 30 j=1,mpb                                                             !/*ut         13*/
   30 gpb9cm(j,i)=0.                                                            !/*ut         14*/
      do 40 i=1,lpblcm+1                                                        !/*ut         15*/
      jpblcm(i)=0                                                               !/*ut         16*/
      do 40 j=1,mpb                                                             !/*ut         17*/
   40 jpb9cm(j,i)=0                                                             !/*ut         18*/
      do 50 i=1,nipt                                                            !/*utx         6*/
      wcs1tc(i)=wcs1(i)                                                         !/*ut         20*/
   50 wcs2tc(i)=wcs2(i)                                                         !/*ut         21*/
      wgtstc(1)=huge                                                            !/*ut         22*/
#if defined(MULTT) || defined(MULTP)                                            /*!ut4a        1*/
      if(ntasks.le.1)then                                                       !/*ut4c        1*/
         jlock=0                                                                !/*ut4c        2*/
      else                                                                      !/*ut4c        3*/
         jlock=-1                                                               !/*ut4c        4*/
      endif                                                                     !/*ut4c        5*/
!                                                                               !/*ut         24*/
!        define the task number.                                                !/*ut         25*/
      ktask=0                                                                   !/*ut4c        6*/
#ifdef MULTT                                                                    /*!ut4c        7*/
      call thread_id(ktask)                                                     !/*utxk        1*/
#endif                                                                          /*!cpp       238*/
!                                                                               !/*ut         29*/
!        set vdac variable k pointers.                                          !/*utx        10*/
      kddm=(ktask+1)*2*ntal1*(npert1+1)                                         !/*utxk        2*/
      kddn=(ktask+1)*24*ndnd1                                                   !/*utxk        3*/
      kdec=(ktask+1)*3*mxa1*ndnd1                                               !/*utxk        4*/
      kdxc=(ktask+1)*3*mxa1*nxnx1                                               !/*utxk        5*/
      kdxd=(ktask+1)*mipt*24*mxdx*min(1,nxnx1)                                  !/*utxk        6*/
      kfeb=(ktask+1)*2*max(16,igm1)                                             !/*utxk       13*/
      kflx=(ktask+1)*min(1,icw1)*mxa1*igm1                                      !/*utxk       14*/
      kwfa=(ktask+1)*(mgww1+mipt)*nwwma1                                        !/*utxk       15*/
      ksww=(ktask+1)*(mgww1+mipt)*nwwma1                                        !/*utxk       16*/
      kpac=(ktask+2)*mipt*10*mxa1                                               !/*utxk       17*/
      kpan=(ktask+2)*3*8*npn1                                                   !/*utxk       18*/
      kpwb=(ktask+2)*mipt*items_pwb_total*mxa1                                  !/*utxk       19*/
      kpcc=(ktask+1)*3*mxa1*kpt1(2)                                             !/*utxk       20*/
      ksum=(ktask+3)*npert1                                                     !/*utxk       21*/
      kwns=(ktask+1)*2*mxxs1/4                                                  !/*utxk       22*/
      kise=(ktask+1)*2*mxa1                                                     !/*utxk       23*/
      kmaz=(ktask+1)*3*nmaz1*(kpt1(1)+kpt1(2)+kpt1(3))                          !/*utxk       24*/
      kndp=(ktask+1)*6*ndnd1                                                    !/*utxk       25*/
      kndr=(ktask+1)*mxe1                                                       !/*utxk       26*/
      klaj=ktask*(mlaj1+mxa1)                                                   !/*utxk       27*/
      klcj=ktask*(mlja1+1)                                                      !/*utxk       28*/
      klse=ktask*mxa1                                                           !/*utxk       29*/
!                                                                               !/*utx        33*/
!        set edac variable k pointers.                                          !/*utx        34*/
#ifdef RADIOG                                                                   /*!utxk       30*/
      kdrc=ktask*20*ndnd1                                                       !/*utxk       31*/
#else                                                                           /*!utxk       32*/
      kdrc=ktask*18*ndnd1                                                       !/*utxk       33*/
#endif                                                                          /*!utxk       34*/
      kfdd=ktask*2*msd1                                                         !/*utxk       35*/
      kpik=ktask*npikmt1                                                        !/*utxk       36*/
      kptb=ktask*5*npert1*mnnm1                                                 !/*utxk       37*/
      krtc=ktask*15*mxe1                                                        !/*utxk       38*/
      ktgp=ktask*mxe1                                                           !/*utxk       39*/
      kifg=ktask*(mgww1+mipt)*nwwma1                                            !/*utxk       40*/
      kifl=ktask*mxa1*min(1,max(0,kfl1-1))                                      !/*utxk       41*/
      kpc2=ktask*mxa1                                                           !/*utxk       42*/
      kjfl=ktask*mxj1*mod(kfl1,2)                                               !/*utxk       43*/
      kjft=ktask*ntal1                                                          !/*utxk       44*/
      kjpb=ktask*npert1                                                         !/*utxk       45*/
      kktc=ktask*2*mxe1                                                         !/*utxk       46*/
      knmc=ktask*4*mgww1*nwwma1                                                 !/*utxk       47*/
#ifdef MESHTAL                                                                  /*!utxk       48*/
      if(nmesh.gt.0) call gdtal(13,zero,zc,jz)                                  !/*utxk       49*/
#endif                                                                          /*!utxk       50*/
!                                                                               !/*utx        49*/
!        set tally and bank variable k pointers.                                !/*utx        50*/
      kshs=(ktask+1)*nspt*ntal1*(npert1+1)                                      !/*utxk       51*/
      kstt=(ktask+1)*ntp*ntal1*(npert1+1)                                       !/*utxk       52*/
      knhs=(ktask+1)*nsp12*ntal1*(npert1+1)                                     !/*utxk       53*/
      ktal=(ktask+1)*(nmxf*mxf+ktls)                                            !/*utxk       54*/
      kgbn=ktask*mbnk                                                           !/*utxk       55*/
      kbnk=ktask*(nbmx*(lpblcm+2*abs(iunr))+1)                                  !/*utxk       56*/
!                                                                               !/*ut4a        6*/
!        clear and reset history score and tally blocks for this task.          !/*ut4a        7*/
      if(itask.lt.0)go to 59                                                    !/*ut4c       29*/
      do 54 it=1,ntal*(npert+1)                                                 !/*ut4b       11*/
      do 52 j=1,nsp12                                                           !/*ut4a        9*/
   52 nhsd(knhs+j,it)=0                                                         !/*ut4c       30*/
      do 53 j=1,nspt                                                            !/*ut4c       31*/
   53 shsd(kshs+j,it)=0.                                                        !/*ut4c       32*/
      ddm(kddm+1,it)=ddm(lddm+1,it)                                             !/*ut4a       12*/
   54 shsd(kshs+nspt-1,it)=shsd(lshs+nspt-1,it)                                 !/*ut4a       13*/
      do 55 i=1,nmxf*mxf                                                        !/*ut4a       14*/
   55 tal(ktal+i)=0.                                                            !/*ut4a       15*/
#ifdef MESHTAL                                                                  /*!utxk       57*/
      if(nmesh.gt.0) call gdtal(14,zero,zc,jz)                                  !/*utxk       58*/
#endif                                                                          /*!utxk       59*/
!                                                                               !/*utx        57*/
!        backup l-pointer values for use in msgtsk.                             !/*utx        58*/
      do 56 j=1,ndnd                                                            !/*utx        59*/
   56 ddn(kddn+22,j)=ddn(lddn+22,j)                                             !/*utx        60*/
      do 57 k=1,mxdx*min(1,nxnx)                                                !/*utx        61*/
      do 57 j=1,nipt                                                            !/*utx        62*/
   57 dxd(kdxd+j,22,k)=dxd(ldxd+j,22,k)                                         !/*utx        63*/
#endif                                                                          /*!cpp       237*/
!                                                                               !/*ut         65*/
!        count the other-side cells and the source cells.                       !/*ut         66*/
   59 do 60 nlaj=0,mlaj+mxa-1                                                   !/*ut4c       33*/
   60 if(laj(klaj+nlaj+1).eq.0)go to 70                                         !/*ut         68*/
   70 do 80 nlse=0,mxa-1                                                        !/*ut         69*/
   80 if(lse(klse+nlse+1).eq.0)go to 90                                         !/*ut         70*/
!                                                                               !/*ut4c       34*/
!        initialize weight window generator nmc scoring loop.                   !/*ut4c       35*/
   90 if(iwwg.eq.0)return                                                       !/*ut4c       36*/
      do 100 i=1,(mgww(mipt+1)+nipt)*nwwma                                      !/*utx        64*/
  100 iafg(kifg+i)=0                                                            !/*ut4c       38*/
      return                                                                    !/*ut4c       39*/
      end                                                                       !/*ut         76*/
