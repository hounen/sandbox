!#ident vtask.F from deck vt                                                    !/*vt          1*/
      subroutine vtask                                                          !/*vt          2*/
!        sweep data accumulated by current task into permanent arrays.          !/*vt          3*/
      use global_data                                                           !/*vtx         1*/
!                                                                               !/*vtx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*vtx         3*/
      implicit integer(kindi) (i-n)                                             !/*vtx         4*/
!                                                                               !/*vtx         5*/
#if defined(MULTT) || defined(MULTP)                                            /*!vtx         6*/
      dimension ar(ntp)                                                         !/*vt4a        1*/
#endif                                                                          /*!vtx         7*/
#ifdef MULTP                                                                    /*!vt4c        1*/
!                                                                               !/*vt4c        2*/
!        multiprocessing - use mynum and itask to determine action.             !/*vt4c        3*/
      if(mynum.gt.0.and.itask.eq.-2)go to 109                                   !/*vt4c        4*/
      if(mynum.gt.0.and.ntasks.le.1)go to 20                                    !/*vt4c        5*/
#endif                                                                          /*!cpp       246*/
!                                                                               !/*vt4c        7*/
!        sort task tallies with lock off for improved multitasking.             !/*vt4c        8*/
#ifdef MULTT                                                                    /*!vtxk        1*/
      call thread_lockoff(jlock)                                                !/*vtxk        2*/
#endif                                                                          /*!vtxk        3*/
      do 10 it=1,ntal*(npert+1)                                                 !/*vt4c       10*/
   10 if(nhsd(knhs+nsp+6,it).gt.1)call hpsort(it,knhs+nsp+5,kshs+nsp+5,         !/*vt4c       11*/
     & nhsd(knhs+nsp+6,it),nhsd(knhs+nsp+7,it))                                 !/*vt4c       12*/
#if defined(MULTP) && defined(MULTT)                                            /*!vt4c       13*/
!                                                                               !/*vt4c       14*/
!        multiprocessing with multitasking - use barriers.                      !/*vt4c       15*/
      if(mynum.gt.0)then                                                        !/*vtxk        4*/
        call thread_barrier()                                                   !/*vtxk        5*/
!        master thread moves his k pointers to l pointers.                      !/*vtxk        6*/
        if(ktask.eq.0)then                                                      !/*vtxk        7*/
          do i=1,ltskpt-1                                                       !/*vtxk        8*/
          lpntcm(i)=ktskpt(i)                                                   !/*vtxk        9*/
          enddo                                                                 !/*vtxk       10*/
        endif                                                                   !/*vtxk       11*/
        call thread_barrier()                                                   !/*vtxk       12*/
      endif                                                                     !/*vtxk       13*/
#endif                                                                          /*!cpp       247*/
#ifdef MULTT                                                                    /*!vtxk       14*/
      call thread_lockon(jlock)                                                 !/*vtxk       15*/
#endif                                                                          /*!vtxk       16*/
!                                                                               !/*vt4c       27*/
!        sweep data from task common to variable common.                        !/*vt4c       28*/
   20 if(kc8.eq.2)go to 60                                                      !/*vt4c       32*/
      do 25 j=1,nipt                                                            !/*vtx        12*/
      eacc(j,1)=eacc(j,1)+eacctc(j,1)                                           !/*vtx        13*/
      eacc(j,2)=eacc(j,2)+eacctc(j,2)                                           !/*vtx        14*/
   25 eacc(j,3)=max(eacc(j,3),eacctc(j,3))                                      !/*vtx        15*/
      do 27 j=1,nipt                                                            !/*vtx        16*/
      racc(j,1)=racc(j,1)+racctc(j,1)                                           !/*vtx        17*/
   27 racc(j,2)=racc(j,2)+racctc(j,2)                                           !/*vtx        18*/
      do 30 i=1,nipt                                                            !/*vtx        19*/
      do 30 j=1,items_pax_total                                                 !/*vtx        20*/
      do 30 k=1,6                                                               !/*vt         14*/
   30 pax(k,j,i)=pax(k,j,i)+paxtc(k,j,i)                                        !/*vt         15*/
      do 35 i=1,4                                                               !/*vt4c       36*/
      do 35 j=1,2                                                               !/*vt4c       37*/
   35 rlt(i,j)=rlt(i,j)+rlttc(i,j)                                              !/*vt4c       38*/
      do 40 i=1,7                                                               !/*vt4c2       2*/
   40 smul(i)=smul(i)+smultc(i)                                                 !/*vt4c       40*/
      do 45 i=1,3                                                               !/*vt4c       41*/
      do 45 j=1,nipt                                                            !/*vtx        21*/
   45 tmav(j,i)=tmav(j,i)+tmavtc(j,i)                                           !/*vt4c       43*/
      do 50 i=1,nipt                                                            !/*vtx        22*/
      nbt(i)=nbt(i)+nbttc(i)                                                    !/*vt4c       45*/
   50 coll(i)=coll(i)+colltc(i)                                                 !/*vt4c       46*/
      do 55 i=1,nipt                                                            !/*vtx        23*/
      do 55 j=1,mxdx                                                            !/*vt4c       48*/
      do 55 k=1,8                                                               !/*vt4c       49*/
   55 nziy(k,j,i)=nziy(k,j,i)+nziytc(k,j,i)                                     !/*vt4c       50*/
!                                                                               !/*vt4c       51*/
!        always sweep this data from task common to variable common.            !/*vt4c       52*/
   60 nbhwm=max(nbhwm,nbhwtc)                                                   !/*vt4c       53*/
      nrnh(1)=nrnh(1)+nrnhtc(1)                                                 !/*vt         30*/
      if(nrnhtc(2).gt.nrnh(2))nrnh(3)=nrnhtc(3)                                 !/*vt         31*/
      if(nrnhtc(2).gt.nrnh(2))nrnh(2)=nrnhtc(2)                                 !/*vt         32*/
      rnr=rnr+rnrtc                                                             !/*vt         33*/
      wgts(1)=min(wgts(1),wgtstc(1))                                            !/*vt         34*/
      wgts(2)=max(wgts(2),wgtstc(2))                                            !/*vt         35*/
      do 65 i=1,3                                                               !/*vt4c       54*/
   65 sumk(i)=sumk(i)+sumktc(i)                                                 !/*vt4c       56*/
#if defined(MULTT) || defined(MULTP)                                            /*!vt4a       15*/
!                                                                               !/*vt4c       57*/
!        return if master thread of subtask.                                    !/*vt4c       58*/
      if(mynum.gt.0.and.ktask.eq.0)return                                       !/*vt4c       59*/
!                                                                               !/*vt         40*/
!        combine global and task largest tally points into global array.        !/*vt4a       16*/
      if(ntal.eq.0)go to 109                                                    !/*vt4c       60*/
      do 108 it=1,ntal*(npert+1)                                                !/*vt4b        5*/
      if(nhsd(knhs+nsp+2,it).eq.0)go to 108                                     !/*vt4a       18*/
      do 102 j=1,nsp+5                                                          !/*vt4a       19*/
      nhsd(lnhs+j,it)=nhsd(lnhs+j,it)+nhsd(knhs+j,it)                           !/*vt4a       20*/
  102 shsd(lshs+j,it)=shsd(lshs+j,it)+shsd(kshs+j,it)                           !/*vt4a       21*/
      if(nhsd(knhs+nsp+6,it).eq.0)go to 108                                     !/*vt4a       22*/
!                                                                               !/*vt4a       23*/
!        do extreme tally merge only if this task has large points.             !/*vt4a       24*/
      nk=nhsd(knhs+nsp+6,it)+kshs+nsp+5                                         !/*vt4a       25*/
      nl=nhsd(lnhs+nsp+6,it)+lshs+nsp+5                                         !/*vt4a       26*/
      np=min(ntp,nhsd(knhs+nsp+6,it)+nhsd(lnhs+nsp+6,it))                       !/*vt4a       27*/
!                                                                               !/*vt4a       28*/
!        merge global and task extreme tallies and keep sorted.                 !/*vt4a       29*/
      do 104 k=np,1,-1                                                          !/*vt4a       30*/
      a=shsd(nl,it)                                                             !/*vt4a       31*/
      ar(k)=max(a,shsd(nk,it))                                                  !/*vt4a       32*/
      if(ar(k).eq.a)nl=nl-1                                                     !/*vt4a       33*/
  104 if(ar(k).gt.a)nk=nk-1                                                     !/*vt4a       34*/
      do 106 k=1,np                                                             !/*vt4a       35*/
  106 shsd(lshs+nsp+5+k,it)=ar(k)                                               !/*vt4a       36*/
      nhsd(lnhs+nsp+6,it)=np                                                    !/*vt4a       37*/
  108 continue                                                                  !/*vt4a       38*/
!                                                                               !/*vt4a       39*/
!        sweep data in dynamically allocated storage.                           !/*vt         41*/
  109 do 110 i=1,ntal*(npert+1)                                                 !/*vt4c       61*/
      if(ddm(kddm+1,i).gt.ddm(lddm+1,i))ddm(lddm+2,i)=ddm(kddm+2,i)             !/*vt         43*/
  110 ddm(lddm+1,i)=max(ddm(lddm+1,i),ddm(kddm+1,i))                            !/*vt         44*/
      do 130 i=1,ndnd                                                           !/*vt         45*/
      do 120 j=1,20                                                             !/*vt         46*/
      ddn(lddn+j,i)=ddn(lddn+j,i)+ddn(kddn+j,i)                                 !/*vt         47*/
  120 ddn(kddn+j,i)=0.                                                          !/*vt         48*/
      if(ddn(kddn+21,i).gt.ddn(lddn+21,i))ddn(lddn+23,i)=ddn(kddn+23,i)         !/*vt         49*/
  130 ddn(lddn+21,i)=max(ddn(lddn+21,i),ddn(kddn+21,i))                         !/*vt         50*/
      do 140 i=1,mxa*ndnd                                                       !/*vt         51*/
      do 140 j=1,3                                                              !/*vt         52*/
      dec(ldec+j,i)=dec(ldec+j,i)+dec(kdec+j,i)                                 !/*vt         53*/
  140 dec(kdec+j,i)=0.                                                          !/*vt         54*/
      do 150 i=1,mxa*nxnx                                                       !/*vt         55*/
      do 150 j=1,3                                                              !/*vt         56*/
      dxc(ldxc+j,i)=dxc(ldxc+j,i)+dxc(kdxc+j,i)                                 !/*vt         57*/
  150 dxc(kdxc+j,i)=0.                                                          !/*vt         58*/
      do 170 i=1,mxdx*min(1,nxnx)                                               !/*vt         59*/
      do 170 j=1,nipt                                                           !/*vtx        26*/
      do 160 k=1,20                                                             !/*vt         61*/
      dxd(ldxd+j,k,i)=dxd(ldxd+j,k,i)+dxd(kdxd+j,k,i)                           !/*vt         62*/
  160 dxd(kdxd+j,k,i)=0.                                                        !/*vt         63*/
      if(dxd(kdxd+j,21,i).gt.dxd(ldxd+j,21,i))                                  !/*vt         64*/
     & dxd(ldxd+j,23,i)=dxd(kdxd+j,23,i)                                        !/*vt         65*/
  170 dxd(ldxd+j,21,i)=max(dxd(ldxd+j,21,i),dxd(kdxd+j,21,i))                   !/*vt         66*/
      do 175 i=1,2                                                              !/*vt4b        7*/
      do 175 j=1,max(16,igm)                                                    !/*vt4b        8*/
      febl(lfeb+i,j)=febl(lfeb+i,j)+febl(kfeb+i,j)                              !/*vt4b        9*/
  175 febl(kfeb+i,j)=0.                                                         !/*vt4b       10*/
      do 180 i=1,min(1,icw)*mxa*igm                                             !/*vt         67*/
      flx(lflx+i)=flx(lflx+i)+flx(kflx+i)                                       !/*vt         68*/
  180 flx(kflx+i)=0.                                                            !/*vt         69*/
      do 190 i=1,(mgww(mipt+1)+nipt)*nwwma                                      !/*vtx        27*/
      wwfa(lwfa+i)=wwfa(lwfa+i)+wwfa(kwfa+i)                                    !/*vt4c       63*/
      swwfa(lsww+i)=swwfa(lsww+i)+swwfa(ksww+i)                                 !/*vt4c       64*/
      wwfa(kwfa+i)=0.                                                           !/*vt4c       65*/
  190 swwfa(ksww+i)=0.                                                          !/*vt4c       66*/
      do 200 i=1,mxa                                                            !/*vt         75*/
      do 200 j=1,10                                                             !/*vt         76*/
      do 200 k=1,nipt                                                           !/*vtx        28*/
      pac(lpac+k,j,i)=pac(lpac+k,j,i)+pac(kpac+k,j,i)                           !/*vt         78*/
  200 pac(kpac+k,j,i)=0.                                                        !/*vt         79*/
      do 210 i=1,npn                                                            !/*vt         80*/
      do 210 j=1,8                                                              !/*vt4c2       3*/
      do 210 k=1,3                                                              !/*vt4c2       4*/
      pan(lpan+k,j,i)=pan(lpan+k,j,i)+pan(kpan+k,j,i)                           !/*vt         83*/
  210 pan(kpan+k,j,i)=0.                                                        !/*vt         84*/
      do 220 i=1,mxa*kpt(2)                                                     !/*vt         85*/
      do 220 j=1,3                                                              !/*vt         86*/
      pcc(lpcc+j,i)=pcc(lpcc+j,i)+pcc(kpcc+j,i)                                 !/*vt         87*/
  220 pcc(kpcc+j,i)=0.                                                          !/*vt         88*/
      do 230 i=1,mxa                                                            !/*vt         89*/
      do 230 j=1,items_pwb_total                                                !/*vtx        29*/
      do 230 k=1,nipt                                                           !/*vtx        30*/
      pwb(lpwb+k,j,i)=pwb(lpwb+k,j,i)+pwb(kpwb+k,j,i)                           !/*vt         92*/
  230 pwb(kpwb+k,j,i)=0.                                                        !/*vt         93*/
      if(nsr.ne.71)go to 238                                                    !/*vt4c       68*/
      do 236 i=1,npert                                                          !/*vt4c       69*/
      sump(lsum+i)=sump(lsum+i)+sump(ksum+i)                                    !/*vt4c       70*/
  236 sump(ksum+i)=0.                                                           !/*vt4c       71*/
  238 do 240 i=1,mxxs/4                                                         !/*vt4c       72*/
      do 240 j=1,2                                                              !/*vt         95*/
      wns(lwns+j,i)=wns(lwns+j,i)+wns(kwns+j,i)                                 !/*vt         96*/
  240 wns(kwns+j,i)=0.                                                          !/*vt         97*/
      do 250 i=1,mxa                                                            !/*vt         98*/
      do 250 j=1,2                                                              !/*vt         99*/
      isef(lise+j,i)=isef(lise+j,i)+isef(kise+j,i)                              !/*vt        100*/
  250 isef(kise+j,i)=0                                                          !/*vt        101*/
      do 255 i=1,3*nmaz*(kpt(1)+kpt(2)+kpt(3))                                  !/*vt4b       11*/
      maze(lmaz+i)=maze(lmaz+i)+maze(kmaz+i)                                    !/*vt4b       12*/
  255 maze(kmaz+i)=0                                                            !/*vt4b       13*/
      do 260 i=1,ndnd                                                           !/*vt        102*/
      do 260 j=1,6                                                              !/*vt        103*/
      ndpf(lndp+j,i)=ndpf(lndp+j,i)+ndpf(kndp+j,i)                              !/*vt        104*/
  260 ndpf(kndp+j,i)=0                                                          !/*vt        105*/
      do 270 i=1,mxe                                                            !/*vt        106*/
      ndr(lndr+i)=ndr(lndr+i)+ndr(kndr+i)                                       !/*vt        107*/
  270 ndr(kndr+i)=0                                                             !/*vt        108*/
      do 280 i=1,(nmxf-1)*mxf                                                   !/*vt4a       40*/
      tal(ltal+mxf+i)=tal(ltal+mxf+i)+tal(ktal+mxf+i)                           !/*vt        110*/
  280 tal(ktal+mxf+i)=0.                                                        !/*vt        111*/
#ifdef MESHTAL                                                                  /*!vtxk       17*/
      if(nmesh.gt.0) call gdtal(15,zero,zd,jz)                                  !/*vtxk       18*/
#endif                                                                          /*!vtxk       19*/
#endif                                                                          /*!cpp       248*/
      return                                                                    !/*vt        112*/
      end                                                                       !/*vt        113*/
