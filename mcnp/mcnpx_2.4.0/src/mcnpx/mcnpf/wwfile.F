!#ident wwfile.F from deck zw4b                                                 !/*zw4b        1*/
      subroutine wwfile(ip)                                                     !/*zw4b        2*/
!        read in the weight-window mesh description and bounds.                 !/*zw4b        3*/
      use global_data                                                           !/*zwx         1*/
!                                                                               !/*zwx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*zwx         3*/
      implicit integer(kindi) (i-n)                                             !/*zwx         4*/
!                                                                               !/*zwx         5*/
      character ha*80,hb*16                                                     !/*zw4c        1*/
!                                                                               !/*zw4b        7*/
      if(ip.gt.0)go to 90                                                       !/*zw4b        8*/
!                                                                               !/*zw4b        9*/
! ********************* ip=0 : called before setdas ********************        !/*zw4c        2*/
!                                                                               !/*zw4c        3*/
!        open the wwinp file.                                                   !/*zw4c        4*/
      if(inqire(wwinp).eq.0)go to 260                                           !/*zw4c        5*/
      open(iuw,file=wwinp,status='old')                                         !/*zw4b       12*/
!                                                                               !/*zw4c        6*/
!        check if wwinp file is mesh-based or cell-based.                       !/*zw4c        7*/
!        if cell-based, count energy groups, nww(jp), on wwe cards.             !/*zw4c        8*/
      nwgeom=0                                                                  !/*zw4c        9*/
      jk=0                                                                      !/*zw4c       10*/
   10 read(iuw,'(a80)',end=25)ha                                                !/*zw4c       11*/
      jc=1                                                                      !/*zw4c       12*/
      kh=kdarg(ha,jc,0,hb,r)                                                    !/*zwx         6*/
      if(kh.gt.0.and.jk.ne.0)go to 20                                           !/*zwx         7*/
      jk=0                                                                      !/*zw4c       14*/
      if(hb(1:3).ne.'wwe'.and.hb(1:3).ne.'wwn')go to 10                         !/*zw4c       15*/
      nwgeom=3                                                                  !/*zw4c       16*/
      jc=index(ha,':')+2                                                        !/*zw4c       17*/
      if(jc.lt.6.or.jc.gt.50)go to 290                                          !/*zw4c       18*/
      jp=index('npe',ha(jc-1:jc-1))                                             !/*zw4c       19*/
      if(jp.eq.0)go to 290                                                      !/*zw4c       20*/
      if(hb(1:3).eq.'wwn')nww(jp)=max(nww(jp),1)                                !/*zw4c       21*/
      if(hb(1:3).ne.'wwe')go to 10                                              !/*zw4c       22*/
      jk=1                                                                      !/*zw4c       23*/
      nww(jp)=-1                                                                !/*zw4c       24*/
   20 nww(jp)=nww(jp)+1                                                         !/*zw4c       25*/
      if(kdarg(ha,jc,0,hb,r).gt.0)go to 20                                      !/*zw4c       26*/
      go to 10                                                                  !/*zw4c       27*/
   25 rewind iuw                                                                !/*zw4c       28*/
      if(nwgeom.eq.3)return                                                     !/*zw4c       29*/
!                                                                               !/*zw4c       30*/
!        read information for mesh-based weight windows.                        !/*zw4c       31*/
      nwang=0                                                                   !/*zw4b       14*/
      read(iuw,'(4i10)',end=300)if,iv,ni,nr                                     !/*zw4c       32*/
      if(if.eq.2)nwang=1                                                        !/*zw4b       16*/
      if(if.eq.3)nwang=2                                                        !/*zw4b       17*/
      read(iuw,'(7i10)',end=300)(nww(j),j=1,min(ni,mipt))                       !/*zw4c       33*/
      read(iuw,170,end=300)(wwm(j),j=1,min(nr,26))                              !/*zw4c       34*/
      nwgeom=max(1,min(2,nint(wwm(nr))))                                        !/*zw4c       35*/
      nwwm=nint(wwm(1)*wwm(2)*wwm(3))                                           !/*zw4b       25*/
      if(nwang.eq.1)nwwm=4*nwwm                                                 !/*zw4b       26*/
      nwgm=3*nint(wwm(7)+wwm(8)+wwm(9))+9                                       !/*zw4b       27*/
!                                                                               !/*zw4b       28*/
!        set cylindrical geometry radial and axial direction cosines.           !/*zw4b       29*/
      if(nwgeom.eq.1)return                                                     !/*zw4b       30*/
      do 40 j=1,2                                                               !/*zw4b       31*/
      s=0.                                                                      !/*zw4c       36*/
      iq=15+(j-1)*4                                                             !/*zw4b       33*/
      do 30 i=1,3                                                               !/*zw4b       34*/
      it=9                                                                      !/*zw4b       35*/
      if(j.eq.1)it=12                                                           !/*zw4b       36*/
      wwm(iq+i)=wwm(it+i)-wwm(3+i)                                              !/*zw4b       37*/
   30 s=s+wwm(iq+i)*wwm(iq+i)                                                   !/*zw4b       38*/
      if(s.le.0.)go to 270                                                      !/*zw4c       37*/
      s=sqrt(s)                                                                 !/*zw4b       40*/
      wwm(iq+4)=s                                                               !/*zw4b       41*/
      do 40 i=1,3                                                               !/*zw4b       42*/
   40 wwm(iq+i)=wwm(iq+i)/s                                                     !/*zw4b       43*/
!                                                                               !/*zw4b       44*/
!        check that radial and axial direction cosines are orthogonal.          !/*zw4b       45*/
      iq=15                                                                     !/*zw4b       46*/
      if(abs(dotpro(wwm(iq+1),wwm(iq+5))).gt.1e-4)go to 280                     !/*zw4c       38*/
      call crspro(wwm(iq+1),wwm(iq+5),wwm(iq+9))                                !/*zw4b       48*/
      return                                                                    !/*zw4b       49*/
!                                                                               !/*zw4c       39*/
! ******************** ip=ipt : called after setdas ********************        !/*zw4c       40*/
!                                                                               !/*zw4b       61*/
!        read the geometric mesh data.                                          !/*zw4b       62*/
   90 if(nwgeom.eq.3)go to 190                                                  !/*zw4c       41*/
      if(mww(ip).ne.0)go to 150                                                 !/*zw4c       42*/
      rewind iuw                                                                !/*zw4c       43*/
      read(iuw,'(a80)')ha                                                       !/*zw4c       44*/
      write(iuo,92)wwinp,ha(61:79)                                              !/*zw4c       45*/
   92 format(/45h reading mesh-based weight windows from file ,a8,              !/*zw4c       46*/
     & 15h with probid = ,a19//)                                                !/*zw4c       47*/
      read(iuw,'(a80)')(ha,i=1,nwgeom+2)                                        !/*zw4c       48*/
      ir=0                                                                      !/*zw4b       64*/
      do 100 i=1,3                                                              !/*zw4b       65*/
      wgm(lwgm+ir+1)=0.                                                         !/*zw4b       66*/
      is=ir+3                                                                   !/*zw4b       67*/
      ir=is+3*nint(wwm(i+6))                                                    !/*zw4b       68*/
      read(iuw,170,end=300)(wgm(lwgm+j),j=is,ir)                                !/*zw4c       49*/
  100 wgm(lwgm+is-1)=wgm(lwgm+is)                                               !/*zw4b       70*/
!                                                                               !/*zw4b       71*/
!        make number of fine meshes cumulative.                                 !/*zw4b       72*/
      ir=0                                                                      !/*zw4b       73*/
      do 120 i=1,3                                                              !/*zw4b       74*/
      do 110 j=1,nint(wwm(i+6))                                                 !/*zw4c2       1*/
      is=lwgm+ir+3*j+1                                                          !/*zw4b       76*/
  110 wgm(is)=wgm(is)+wgm(is-3)                                                 !/*zw4b       77*/
  120 ir=is-lwgm+2                                                              !/*zw4b       78*/
!                                                                               !/*zw4b       79*/
!        renormalize axial bins and convert revolutions to radians.             !/*zw4b       80*/
      if(nwgeom.ne.2)go to 150                                                  !/*zw4b       81*/
      ir=lwgm+3+3*nint(wwm(7))+4                                                !/*zw4b       82*/
      do 130 i=1,3*nint(wwm(8)),3                                               !/*zw4b       83*/
  130 wgm(ir+i)=wgm(ir+i)-wgm(ir-2)                                             !/*zw4b       84*/
      wgm(ir-1)=0.                                                              !/*zw4c       50*/
      wgm(ir-2)=0.                                                              !/*zw4c       51*/
      ir=lwgm+3*nint(wwm(7))+3*nint(wwm(8))+6+2                                 !/*zw4b       87*/
      do 140 i=1,nint(wwm(9))+1                                                 !/*zw4b       88*/
      is=3*(i-1)                                                                !/*zw4b       89*/
  140 wgm(ir+is)=wgm(ir+is)*2.*pie                                              !/*zw4b       90*/
!                                                                               !/*zw4b       91*/
!        read the mesh weight-window energies and bounds.                       !/*zw4b       92*/
  150 do 160 k=1,nww(ip),6                                                      !/*zw4b       93*/
  160 read(iuw,170,end=300)(wwe(lwwe+mww(ip)+j),j=k,min(k+5,nww(ip)))           !/*zw4c       52*/
  170 format(6g13.5)                                                            !/*zw4b       95*/
      nw=nwwm*mww(ip)                                                           !/*zw4b       96*/
      do 180 i=1,nww(ip)                                                        !/*zw4b       97*/
      kl=1+(i-1)*nwwm                                                           !/*zw4b       98*/
      ku=i*nwwm                                                                 !/*zw4b       99*/
      if(nwang.ne.2)read(iuw,170,end=300)(wwf(lwwf+nw+j),j=kl,ku)               !/*zw4c       53*/
      if(nwang.eq.2)read(iuw,170,end=300)((wwf(lwwf+nw+j+m),m=0,3),             !/*zw4c       54*/
     & j=kl,ku)                                                                 !/*zw4b      102*/
  180 continue                                                                  !/*zw4b      103*/
      return                                                                    !/*zw4b      104*/
!                                                                               !/*zw4c       55*/
!        read cell-based weight windows for particle type ip.                   !/*zw4c       56*/
  190 write(iuo,                                                                !/*zw4c       57*/
     & '(/45h reading cell-based weight windows from file ,a8//)')wwinp         !/*zw4c       58*/
  200 nw=-1                                                                     !/*zw4c       59*/
  210 jc=1                                                                      !/*zw4c       60*/
      read(iuw,'(a80)',end=250)ha                                               !/*zw4c       61*/
  220 k=kdarg(ha,jc,0,hb,r)                                                     !/*zw4c       62*/
      if(k.gt.0.and.nw.ge.0)go to 240                                           !/*zw4c       63*/
      if(k.ne.0)go to 210                                                       !/*zw4c       64*/
      if(hb(1:3).ne.'wwe'.and.hb(1:3).ne.'wwn')go to 200                        !/*zw4c       65*/
      jc=index(ha,':')+2                                                        !/*zw4c       66*/
      if(jc.lt.6.or.jc.gt.50)go to 290                                          !/*zw4c       67*/
      jp=index('npe',ha(jc-1:jc-1))                                             !/*zw4c       68*/
      if(jp.eq.0)go to 290                                                      !/*zw4c       69*/
      if(jp.ne.ip)go to 200                                                     !/*zw4c       70*/
      nw=0                                                                      !/*zw4c       71*/
      i=0                                                                       !/*zw4c       72*/
      if(hb(1:3).eq.'wwe')go to 220                                             !/*zw4c       73*/
      do 230 j=1,jc                                                             !/*zw4c       74*/
      m=index('1234567890',ha(j:j))                                             !/*zw4c       75*/
  230 if(m.ne.0)nw=10*nw+mod(m,10)                                              !/*zw4c       76*/
      if(nw.eq.0.or.nw.gt.nww(jp))go to 290                                     !/*zw4c       77*/
      go to 220                                                                 !/*zw4c       78*/
  240 i=i+1                                                                     !/*zw4c       79*/
      if((nw.eq.0.and.i.gt.nww(jp)).or.(nw.gt.0.and.i.gt.mxa))go to 290         !/*zw4c       80*/
      if(nw.gt.0)wwf(lwwf+(nw-1+mww(jp))*mxa+i)=r                               !/*zw4c       81*/
      if(nw.eq.0)wwe(lwwe+mww(jp)+i)=r                                          !/*zw4c       82*/
      go to 220                                                                 !/*zw4c       83*/
  250 rewind iuw                                                                !/*zw4c       84*/
      return                                                                    !/*zw4c       85*/
!                                                                               !/*zw4c       86*/
!        process various input errors.                                          !/*zw4c       87*/
  260 call erprnt(1,1,0,0,0,0,0,0,                                              !/*zw4c       88*/
     & '48hweight window input file '//wwinp//' was not found.')                !/*zw4c       89*/
      return                                                                    !/*zw4c       90*/
  270 call erprnt(1,1,0,0,0,0,0,0,                                              !/*zw4c       91*/
     & '53hwwinp file contains a zero size cylinder in the grid.')              !/*zw4c       92*/
      return                                                                    !/*zw4c       93*/
  280 call erprnt(1,1,0,0,0,0,0,0,                                              !/*zw4c       94*/
     & '54hwwinp file radial and axial directions not orthogonal.')             !/*zw4c       95*/
      return                                                                    !/*zw4c       96*/
  290 call erprnt(1,1,0,0,0,0,0,0,                                              !/*zw4c       97*/
     & '39hcell-based windows wrong in wwinp file.')                            !/*zw4c       98*/
      return                                                                    !/*zw4c       99*/
  300 call expire(0,'wwfile','unexpected eof on '//wwinp//' file.')             !/*zw4c      100*/
      end                                                                       !/*zw4b      106*/
