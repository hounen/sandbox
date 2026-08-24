!#ident output.F from deck ou                                                   !/*ou          1*/
      subroutine output                                                         !/*ou          2*/
!        do output for mcrun.                                                   !/*ou          3*/
      use global_data                                                           !/*oux         1*/
!                                                                               !/*oux         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*oux         3*/
      implicit integer(kindi) (i-n)                                             !/*oux         4*/
!                                                                               !/*oux         5*/
#ifdef MESHTAL                                                                  /*!oux         6*/
      dimension zd(10),jz(5)                                                    !/*oux         7*/
#endif                                                                          /*!oux         8*/
      character hf*8                                                            !/*ou          5*/
!                                                                               !/*ou          6*/
!        quit if trnspt ran into trouble.                                       !/*ou          7*/
#if defined(MULTT) || defined(MULTP)                                            /*!ou4a        1*/
      if(kdbnps.ne.0)call expire(mmkdb,hsub,hmes)                               !/*ou          9*/
#endif                                                                          /*!cpp       274*/
#ifdef PCDOS                                                                    /*!ou4a        2*/
      if(lockl)call pttyin                                                      !/*ou4a        3*/
#endif                                                                          /*!cpp       275*/
      if(irup.ne.0)call qttyin(nps,'24houtput rendezvous, nps =,i9')            !/*ou4a        4*/
!                                                                               !/*ouxk        1*/
!       quit if the stop file is present (like an interrupt quit).              !/*ouxk        2*/
      if(nst.eq.0.and.inqire('stop'//inp).ne.0)nst=nst+16                       !/*ouxk        3*/
      if(ntal.eq.0.or.npp.lt.0)go to 38                                         !/*ou4a        5*/
!                                                                               !/*ou         10*/
!        update detector and dxtran russian roulette criteria.                  !/*ou         11*/
      if(nps.ne.200.and.jtfc.eq.0)go to 30                                      !/*ou         12*/
      do 10 i=1,ndnd                                                            !/*ou         13*/
   10 ddn(lddn+24,i)=ddn(lddn+20,i)/nps                                         !/*ou         14*/
      do 20 i=1,mxdx*min(1,nxnx)                                                !/*ou         15*/
      do 20 j=1,nipt                                                            !/*oux        11*/
   20 dxd(ldxd+j,24,i)=dxd(ldxd+j,20,i)/nps                                     !/*ou         17*/
!                                                                               !/*ou         18*/
!        add a line to tally fluctuation chart if required.                     !/*ou         19*/
!                                                                               !/*ou4a        6*/
!        update the smallest of the extreme ntp history tally points.           !/*ou4a        7*/
   30 if(jtfc.ne.0)call addtfc(lnhs+nsp+5,lshs+nsp+5,lshs+nsp+ntp+5)            !/*ou4a        8*/
      do 32 it=1,ntal*(npert+1)                                                 !/*ou4b        1*/
   32 if(nhsd(lnhs+nsp+6,it).eq.ntp)shsd(lshs+nsp+ntp+6,it)=                    !/*ou4a       10*/
     & shsd(lshs+nsp+6,it)                                                      !/*ou4a       11*/
      if(mdc+mpc+nst+mrm.eq.0)go to 38                                          !/*ou4b        2*/
!                                                                               !/*ou4a       13*/
!        find and store min and max history tally bins if required.             !/*ou4a       14*/
      do 36 it=1,ntal*(npert+1)                                                 !/*ou4b        3*/
      nl=0                                                                      !/*ou4a       16*/
      nu=0                                                                      !/*ou4a       17*/
      do 34 j=2,nsp                                                             !/*ou4a       18*/
      if(nhsd(lnhs+j,it).gt.0)nu=j                                              !/*ou4a       19*/
   34 if(nl.eq.0)nl=nu                                                          !/*ou4a       20*/
      nhsd(lnhs+nsp+3,it)=nl                                                    !/*ou4a       21*/
   36 nhsd(lnhs+nsp+4,it)=nu                                                    !/*ou4a       22*/
!                                                                               !/*ou4a       23*/
!        add a tfc line if not done already for the print and/or dump.          !/*ou4a       24*/
      if(mdc+mpc+nst.ne.0.and.jtfc.eq.0)                                        !/*ou4b        4*/
     & call addtfc(lnhs+nsp+5,lshs+nsp+5,lshs+nsp+ntp+5)                        !/*ou4b        5*/
   38 npsout=nps                                                                !/*ou4a       26*/
!                                                                               !/*ou         22*/
!        at end of kcode cycle, calculate k and write out the source.           !/*ou         23*/
      if(nsr.ne.71.or.nsa.ne.0.or.npp.lt.0)go to 40                             !/*ou         24*/
      if(kcy.gt.kct.and.kct.gt.0)go to 40                                       !/*ou4c        7*/
      call kcalc                                                                !/*ou4c        8*/
      if(mdc+nst.ne.0.and.kcy.ge.ikz)call ksrctp(4)                             !/*oux        14*/
!                                                                               !/*ou         29*/
!        mcplot if required.                                                    !/*ou         30*/
   40 if(mrm.eq.0)go to 60                                                      !/*ou         31*/
#ifdef MCPLOT                                                                   /*!ou         32*/
      ifile=itty                                                                !/*ou         33*/
      if(kmplot.ne.0)go to 50                                                   !/*ou         34*/
      ifile=iu4                                                                 !/*ou         35*/
      rewind iu4                                                                !/*ou         36*/
   50 call mcplot                                                               !/*ou         37*/
      hovr='mcrun'                                                              !/*ou         38*/
      trm=cp1                                                                   !/*ou         39*/
      kmplot=0                                                                  !/*ou         40*/
#endif                                                                          /*!cpp       277*/
!                                                                               !/*ou         42*/
!        print if required.                                                     !/*ou         43*/
   60 if(mpc+nst.eq.0)go to 70                                                  !/*ou         44*/
      call sumary                                                               !/*ou         45*/
#ifdef MESHTAL                                                                  /*!oux        16*/
      if(nmesh.gt.0.and.mct.eq.-1)call gdtal(4,zero,zd,jz)                      !/*oux        17*/
#endif                                                                          /*!oux        18*/
      tlc=cp1                                                                   !/*ou         46*/
!                                                                               !/*ou         47*/
!        dump if required.                                                      !/*ou         48*/
   70 if(mdc+nst.eq.0.or.npp.lt.0)go to 80                                      !/*ou         49*/
      call tpefil(4)                                                            !/*ou         50*/
#ifdef MESHTAL                                                                  /*!oux        19*/
      if(nmesh.gt.0.and.mct.ne.-1)call gdtal(4,zero,zd,jz)                      !/*oux        20*/
#endif                                                                          /*!oux        21*/
      tdc=cp1                                                                   !/*ou         51*/
      if(issw.ne.0.and.nst.ne.0)call wrwssa                                     !/*ou         52*/
!                                                                               !/*ou         53*/
!        write mctal file if required.                                          !/*ou         54*/
   80 if(mct.eq.0.or.nst.eq.0.and.npp.ge.0)go to 100                            !/*ou4a       28*/
      hf=mctal                                                                  !/*ou         56*/
      call mctalw(hf)                                                           !/*ou         57*/
      mctal=hf                                                                  !/*ou         58*/
      write(jtty,90)hf                                                          !/*ou         59*/
      write(iuo,90)hf                                                           !/*ou         60*/
   90 format(28h tally data written to file ,a8)                                !/*ou         61*/
!                                                                               !/*ou4a       29*/
!        write the ptrac summary statement if required.                         !/*ou4a       30*/
  100 if(iptr.ne.0.and.nst.ne.0)call ptrak(8)                                   !/*ou4a       31*/
  110 if(jtfc.ne.0.or.mdc+mpc.eq.0.or.npp.lt.0)return                           !/*ou4c       17*/
!                                                                               !/*ou4a       33*/
!        delete tfc line for nps if it was added for a print or dump.           !/*ou4a       34*/
      nn=1                                                                      !/*ou4a       35*/
      do 120 i=1,20                                                             !/*ou4c       18*/
  120 if(npc(i).ne.0)nn=i                                                       !/*ou4c       19*/
      npc(nn)=0                                                                 !/*ou4a       38*/
      return                                                                    !/*ou         62*/
      end                                                                       !/*ou         63*/
