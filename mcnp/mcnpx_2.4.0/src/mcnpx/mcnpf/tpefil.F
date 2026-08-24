!#ident tpefil.F from deck tf                                                   !/*tf          1*/
      subroutine tpefil(mm)                                                     !/*tf          2*/
!        do all i/o on runtpe, the file of restart dumps.                       !/*tf          3*/
      use allocate_memory                                                       !/*tfx         1*/
      use global_data                                                           !/*tfx         2*/
!                                                                               !/*tfx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*tfx         4*/
      implicit integer(kindi) (i-n)                                             !/*tfx         5*/
!                                                                               !/*tfx         6*/
      character hc*10,hi*19,hk*8,hl*28,hp*32,hv*5                               !/*tfxc        1*/
      real(kindr), allocatable :: x(:)                                          !/*tfxc        2*/
!                                                                               !/*tf          6*/
      if (lcaopt(8).eq.-1) return                                               !/*tfx         8*/
      go to(10,20,30,50,80,170,180,250)mm                                       !/*tf          7*/
!                                                                               !/*tf          8*/
! >>>>>  mm=1 -- create runtpe with a unique name.  write first record.         !/*tf          9*/
   10 call unique(runtpe,jtty)                                                  !/*tf         10*/
      open(iur,file=runtpe,form='unformatted',status='new')                     !/*tf         11*/
      call getidt(idtm)                                                         !/*tf         12*/
      write(iur)kod,ver,loddat,idtm,chcd,probid,probs,aid,ufil,mxe              !/*tf         13*/
      return                                                                    !/*tf         14*/
!                                                                               !/*tf         15*/
! >>>>>  mm=2 -- write a cross section table on runtpe.                         !/*tf         16*/
   20 allocate (x(nxs(lnxs+1,iex)), stat=is)                                    !/*tfxc        3*/
      if(is.ne.0)call expire(0,'tpefil','allocate error')                       !/*tfxc        4*/
      x(:)=xss(1:nxs(lnxs+1,iex))                                               !/*tfxc        5*/
      write(iur)x                                                               !/*tfxc        6*/
      deallocate(x)                                                             !/*tfxc        7*/
      nkxs=nkxs+1                                                               !/*tf         18*/
      kxs(lkxs+nkxs)=iex                                                        !/*tf         19*/
      return                                                                    !/*tf         20*/
!                                                                               !/*tf         21*/
! >>>>>  mm=3 -- write the fixed data and the first dump.                       !/*tf         22*/
   30 write(iur)gfixcm,jfixcm                                                   !/*tf         23*/
      call fdac_write (iur)                                                     !/*tfx        10*/
!                                                                               !/*tf         28*/
! >>>>>  mm=4 -- write a restart dump.                                          !/*tf         29*/
   50 call getidt(idtm)                                                         !/*tf         30*/
      knod=knod+1                                                               !/*tf         31*/
      if(ndmp.gt.0)call runtpq                                                  !/*tf         32*/
      call runtpw(iur)                                                          !/*tf         33*/
      endfile iur                                                               !/*tf         34*/
      backspace iur                                                             !/*tf         35*/
!                                                                               !/*tf         38*/
!        print comments about the dump.                                         !/*tf         39*/
!     QUESTION: Should the sum below coll(1)+...+coll(3) actually               !/*tfx        11*/
!               be taken over all particle types ???                            !/*tfx        12*/
      write(hp,'(2f16.0)')coll(1)+coll(2)+coll(3),rnr                           !/*tf4a       14*/
      if(mct.ge.0)write(iuo,60)knod,runtpe,nps,hp(1:15),cts/60,hp(17:31)        !/*tf4a       15*/
   60 format(/1x,119('*')/9h dump no.,i5,9h on file ,a8,5x,5hnps =,i10,         !/*tf4a       16*/
     & 5x,6hcoll =,a15,5x,5hctm =,f8.2,5x,5hnrn =,a15/)                         !/*tf4a       17*/
      write(jtty,70)knod,runtpe,nps,hp(1:15),cts/60.,hp(17:31)                  !/*tf4a       18*/
   70 format(5h dump,i5,9h on file ,a8,3x,5hnps =,i10,4x,6hcoll =,a15/          !/*tf4a       19*/
     & 30x,5hctm =,f10.2,5x,5hnrn =,a15)                                        !/*tf4a       20*/
      return                                                                    !/*tf4b        2*/
!                                                                               !/*tf         47*/
! >>>>>  mm=5 -- open existing runtpe and read it for continue run.             !/*tf         48*/
   80 if(inqire(runtpe).eq.0)call expire(0,'tpefil',                            !/*tf         49*/
     & 'file '//runtpe//' does not exist.')                                     !/*tf         50*/
      open(iur,file=runtpe,form='unformatted',status='old')                     !/*tf         51*/
      rewind iur                                                                !/*tf         52*/
!                                                                               !/*tf         53*/
!        read and check the first record.                                       !/*tf         54*/
      read(iur)hk,hv,hl,hi,hc,probid,probs,aid,ufil,mxe                         !/*tf         55*/
      if(hk//hv.ne.kod//ver)call expire(0,'tpefil',                             !/*tfxc        8*/
     & 'file '//runtpe//' was written by '//hk//' version '//hv)                !/*cor4-2     61*/
      if(hl.ne.loddat.and.mct.ge.0)write(iuo,90)runtpe,hk,hv,hl                 !/*tf4a       22*/
   90 format(/6h file ,a8,16h was written by ,a8,9h version ,a5,                !/*tf         59*/
     & 11h loaded on ,a28)                                                      !/*tfx        13*/
!                                                                               !/*tf         61*/
!        skip over the cross-section tables and read fixed data.                !/*tf         62*/
      do 100 i=1,mxe                                                            !/*tf         63*/
  100 read(iur)id                                                               !/*tf4a       23*/
      read(iur)gfixcm,jfixcm                                                    !/*tf         65*/
      call dynamic_allocate                                                     !/*tfx        14*/
#if defined(MULTT) || defined(MULTP)                                            /*!tfxl        1*/
      tal => chg_mem (tal,(nmxf*mxf+ktls)*(mtasks+1),is)                        !/*tfxl        2*/
#else                                                                           /*!tfxl        3*/
      tal => chg_mem (tal,nmxf*mxf+ktls,is)                                     !/*tfxl        4*/
#endif                                                                          /*!tfxl        5*/
      if(is.ne.0)call expire(0,'tpefil','chg_mem error')                        !/*tfx        16*/
!        some of tal is read from runtpe - may not need to zero rest.           !/*tfx        17*/
      tal=0.                                                                    !/*tfx        18*/
      xss => chg_mem (xss,lxs+4,is)                                             !/*tfx        19*/
      if(is.ne.0)call expire(0,'tpefil','chg_mem error')                        !/*tfx        20*/
      call fdac_read (iur)                                                      !/*tfx        21*/
!                                                                               !/*tf         69*/
!        read in the cross-section tables.                                      !/*tf         70*/
      if(mxe.eq.0)go to 120                                                     !/*tf         71*/
      rewind iur                                                                !/*tf         72*/
      read(iur)id                                                               !/*tf4a       26*/
      do 110 k=1,mxe                                                            !/*tf         74*/
      l0=jxs(ljxs+1,kxs(lkxs+k))                                                !/*tfx        22*/
      l1=l0+nxs(lnxs+1,kxs(lkxs+k))-1                                           !/*tfx        23*/
      allocate (x(nxs(lnxs+1,kxs(lkxs+k))), stat=is)                            !/*tfxc        9*/
      if (is.ne.0) call expire(0,'tpefil','allocate error')                     !/*tfxc       10*/
      read(iur)x                                                                !/*tfxc       11*/
      xss(l0:l1)=x(:)                                                           !/*tfxc       12*/
      deallocate(x)                                                             !/*tfxc       13*/
  110 continue                                                                  !/*tfxc       14*/
      read(iur)id                                                               !/*tf4a       29*/
      read(iur)id                                                               !/*tf4a       30*/
      read(iur)id                                                               !/*tf4a       31*/
!                                                                               !/*tf         80*/
!        read the desired restart dump.                                         !/*tf         81*/
  120 call runtpr(iur,ie)                                                       !/*tf         82*/
      if((knod.ne.idmp.or.idmp.eq.0).and.ie.eq.0)go to 120                      !/*tf         83*/
      if(ie.ne.0)backspace iur                                                  !/*tf         84*/
      write(hc,'(i6)')idmp                                                      !/*tf         85*/
      if(idmp.ne.0.and.knod.ne.idmp)call expire(0,'tpefil',                     !/*tf         86*/
     & 'unable to read dump no. '//hc(1:6)//' on '//runtpe)                     !/*tf         87*/
!                                                                               !/*tf         88*/
!        reposition after the fixed data if the execute item was cn.            !/*tf         89*/
      if(jfcn.eq.0)go to 140                                                    !/*tf         90*/
      rewind iur                                                                !/*tf         91*/
      do 130 i=1,mxe+4                                                          !/*tf         92*/
  130 read(iur)id                                                               !/*tf4a       32*/
!                                                                               !/*tf         94*/
!        print comments about starting from the restart dump.                   !/*tf         95*/
  140 write(iuo,150)knod,runtpe,nps,cts/60.,probid                              !/*tf         96*/
  150 format(/23h starting from dump no.,i5,11h from file ,a8,5x,               !/*tf4a       33*/
     & 5hnps =,i10,5x,5hctm =,f8.2,5x,9hprobid = ,a19/)                         !/*tf4a       34*/
      write(jtty,160)knod,runtpe,nps,cts/60.,probid                             !/*tf         99*/
  160 format(19h starting from dump,i5,7h  file ,a8,6h nps =,i10,               !/*tf4a       35*/
     & 6h ctm =,f8.2/10h probid = ,a19)                                         !/*tf4a       36*/
      return                                                                    !/*tf4a       48*/
!                                                                               !/*tf        103*/
! >>>>>  mm=6 -- make sure of write access to runtpe.                           !/*tf        104*/
  170 endfile iur                                                               !/*tf        105*/
      backspace iur                                                             !/*tf        106*/
      return                                                                    !/*tf        107*/
!                                                                               !/*tf        108*/
! >>>>>  mm=7 -- open existing runtpe and read it for mcplot or plot.           !/*tf        109*/
  180 open(iur,file=runtpe,form='unformatted',status='old')                     !/*tf        110*/
      rewind iur                                                                !/*tf        111*/
!                                                                               !/*tf        113*/
!        read and check the first record.                                       !/*tf        114*/
      read(iur)hk,hv,hl,hi,hc,probid,probs,aid,ufil,mxe                         !/*tf        115*/
      if(hk//hv.eq.kod//ver)go to 200                                           !/*tfxc       15*/
      write(jtty,190)runtpe,hk,hv                                               !/*tf        117*/
  190 format(6h file ,a8,16h was written by ,a8,9h version ,a5)                 !/*tf        118*/
      close(iur)                                                                !/*tf        119*/
      return                                                                    !/*tf        120*/
  200 if(hl.ne.loddat)write(jtty,210)runtpe,hk,hv,hl                            !/*tf        121*/
  210 format(6h file ,a8,16h was written by ,a8,9h version ,a5/                 !/*tfx        25*/
     & 11h loaded on ,a28)                                                      !/*tfx        26*/
!                                                                               !/*tf        124*/
!        skip over the cross-section tables and read fixed data.                !/*tf        125*/
      do 220 i=1,mxe                                                            !/*tf        126*/
  220 read(iur)id                                                               !/*tf4a       49*/
      read(iur)gfixcm,jfixcm                                                    !/*tf        128*/
      call dynamic_allocate                                                     !/*tfx        27*/
      call fdac_read (iur)                                                      !/*tfx        28*/
      tal => chg_mem (tal,nmxf*mxf+ktls,is)                                     !/*tfx        29*/
      if(is.ne.0) call expire(0,'tpefil','chg_mem error')                       !/*tfx        30*/
!                                                                               !/*tf        132*/
!        read the desired dump.                                                 !/*tf        133*/
  230 call runtpr(iur,ie)                                                       !/*tf        134*/
      if((knod.ne.idmp.or.idmp.eq.0).and.ie.eq.0)go to 230                      !/*tf        135*/
      close(iur)                                                                !/*tf        136*/
      if(idmp.eq.0.or.knod.eq.idmp)ktfile=17                                    !/*tf        137*/
      if(ktfile.ne.17)write(jtty,240)idmp,runtpe                                !/*tf        138*/
  240 format(24h unable to read dump no.,i6,4h on ,a8)                          !/*tf        139*/
      return                                                                    !/*tf        140*/
!                                                                               !/*tf        141*/
! >>>>>  mm=8 -- set field length and read in the cross-section tables.         !/*tf        142*/
  250 if(mxe.eq.0)return                                                        !/*tfx        31*/
      xss => chg_mem (xss,lxs+4,is)                                             !/*tfx        32*/
      if(is.ne.0)call expire(0,'tpefil','chg_mem error')                        !/*tfx        33*/
      rewind iur                                                                !/*tf        145*/
      read(iur)id                                                               !/*tf4a       52*/
      do 260 k=1,mxe                                                            !/*tf        147*/
      l=jxs(ljxs+1,kxs(lkxs+k))+nxs(lnxs+1,kxs(lkxs+k))-1                       !/*tfx        34*/
      allocate (x(nxs(lnxs+1,kxs(lkxs+k))), stat=is)                            !/*tfxc       16*/
      if (is.ne.0) call expire(0,'tpefil','allocate error')                     !/*tfxc       17*/
      read(iur)x                                                                !/*tfxc       18*/
      xss(jxs(ljxs+1,kxs(lkxs+k)):l)=x(:)                                       !/*tfxc       19*/
      deallocate(x)                                                             !/*tfxc       20*/
  260 continue                                                                  !/*tfxc       21*/
      return                                                                    !/*tf        150*/
      end                                                                       !/*tf        151*/
