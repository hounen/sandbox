!#ident avrwgi.F from deck vi4c                                                 !/*vi4c        1*/
      subroutine avrwgi(jq)                                                     !/*vi4c        2*/
!        initialize mesh-based weight window generator.                         !/*vi4c        3*/
      use global_data                                                           !/*vix         1*/
!                                                                               !/*vix         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*vix         3*/
      implicit integer(kindi) (i-n)                                             !/*vix         4*/
!                                                                               !/*vix         5*/
      dimension mi(3),ng(mipt)                                                  !/*vi4c        5*/
      dimension cc(3),dc(3)                                                     !/*vi4c        6*/
      character du*80                                                           !/*vi4c        7*/
!                                                                               !/*vi4c        8*/
      if(jq.gt.0)go to 50                                                       !/*vi4c        9*/
!                                                                               !/*vi4c       10*/
! ********************** jq=0 : called before setdas *******************        !/*vi4c       11*/
!                                                                               !/*vi4c       12*/
!        open the wwinp file and read the header information.                   !/*vi4c       13*/
!        mesh-based wwinp only; cell-based treated in wwfile.                   !/*vi4c       14*/
      if(inqire(wwinp).eq.0)go to 410                                           !/*vi4c       15*/
      open(iuw,file=wwinp,status='old')                                         !/*vi4c       16*/
      rewind iuw                                                                !/*vi4c       17*/
      read(iuw,'(4i10)',end=400)if,iv,ni,nr                                     !/*vi4c       18*/
      if(nr.ne.16)nr=10                                                         !/*vi4c       19*/
      ni=min(ni,mipt)                                                           !/*vi4c       20*/
      read(iuw,'(7i10)',end=400)(ng(j),j=1,ni)                                  !/*vi4c       21*/
      do 10 j=1,ni                                                              !/*vi4c       22*/
   10 ngww(j)=max(ngww(j),ng(j))                                                !/*vi4c       23*/
      read(iuw,'(6g13.5)',end=400)(wwma(j),j=1,nr)                              !/*vi4c       24*/
      nwgeoa=nint(wwma(nr))                                                     !/*vi4c       25*/
      if(nwgeoa.lt.1.or.nwgeoa.gt.2)call erprnt(1,1,0,0,0,0,0,0,                !/*vi4c       26*/
     & '36hinvalid geometry type on wwinp file.')                               !/*vi4c       27*/
      nwwma=nint(wwma(1)*wwma(2)*wwma(3))                                       !/*vi4c       28*/
      nwgma=3*nint(wwma(7)+wwma(8)+wwma(9))+9                                   !/*vi4c       29*/
!                                                                               !/*vi4c       30*/
!        set cylindrical geometry radial and axial direction cosines.           !/*vi4c       31*/
      if(nwgeoa.ne.2)return                                                     !/*vi4c       32*/
      do 30 j=1,2                                                               !/*vi4c       33*/
      s=0.                                                                      !/*vi4c       34*/
      iq=15+(j-1)*4                                                             !/*vi4c       35*/
      it=9                                                                      !/*vi4c       36*/
      if(j.eq.1)it=12                                                           !/*vi4c       37*/
      do 20 i=1,3                                                               !/*vi4c       38*/
      wwma(iq+i)=wwma(it+i)-wwma(3+i)                                           !/*vi4c       39*/
   20 s=s+wwma(iq+i)*wwma(iq+i)                                                 !/*vi4c       40*/
      if(s.le.0.)go to 420                                                      !/*vi4c       41*/
      s=sqrt(s)                                                                 !/*vi4c       42*/
      wwma(iq+4)=s                                                              !/*vi4c       43*/
      do 30 i=1,3                                                               !/*vi4c       44*/
   30 wwma(iq+i)=wwma(iq+i)/s                                                   !/*vi4c       45*/
!                                                                               !/*vi4c       46*/
!        check that radial and axial direction cosines are orthogonal.          !/*vi4c       47*/
      if(abs(dotpro(wwma(16),wwma(20))).gt.1e-4)go to 430                       !/*vi4c       48*/
      call crspro(wwma(20),wwma(16),wwma(24))                                   !/*vi4c       49*/
      return                                                                    !/*vi4c       50*/
!                                                                               !/*vi4c       51*/
! ********************** jq=1 : called after setdas ********************        !/*vi4c       52*/
!                                                                               !/*vi4c       53*/
!        jq=1 : load mesh data, initialize wwout, wwone files.                  !/*vi4c       54*/
!        switch iwwg from 1/2 mesh card/wwinp file flag to being the            !/*vi4c       55*/
!           tfc tally index for wwg optimization.                               !/*vi4c       56*/
!                                                                               !/*vi4c       57*/
!        reset iwwg = weight window generator tally bin to optimize.            !/*vi4c       58*/
   50 iw=iwwg                                                                   !/*vi4c       59*/
      iwwg=-1                                                                   !/*vi4c       60*/
      do 60 it=1,ntal                                                           !/*vi4c       61*/
   60 if(jptal(ljpt+1,it).eq.nint(wwg(1)))iwwg=jptal(ljpt+7,it)                 !/*vi4c       62*/
      if(iwwg.le.0)call erprnt(1,1,1,nint(wwg(1)),0,0,0,0,                      !/*vi4c       63*/
     & '19hwwg reference tally,i4,11h not found.')                              !/*vi4c       64*/
!                                                                               !/*vi4c       65*/
!        do multiple particle initialization of energy groups.                  !/*vi4c       66*/
      ig=0                                                                      !/*vi4c       68*/
      do 70 i=1,mipt                                                            !/*vi4c       69*/
      if(ngww(i).ne.0)ni=i                                                      !/*vi4c       70*/
      if(ngww(i).gt.1)ig=1                                                      !/*vi4c       71*/
      if(ngww(i).eq.1)ewwg(lewg+1+mgww(i))=100.                                 !/*vi4c       72*/
      if(ngww(i).eq.1.and.emx(i).lt.huge.and.emx(i).gt.100.)                    !/*vi4c       73*/
     & ewwg(lewg+1+mgww(i))=emx(i)                                              !/*vi4c       74*/
      if(ngww(i).eq.1.and.wwg(8).ne.0.)ewwg(lewg+1+mgww(i))=tco(i)              !/*vi4c       75*/
   70 continue                                                                  !/*vi4c2       2*/
!                                                                               !/*vi4c       77*/
!        open wwout file.                                                       !/*vi4c       78*/
      call unique(wwout,jtty)                                                   !/*vi4c       79*/
      open(iuwe,file=wwout,status='new')                                        !/*vi4c       80*/
!                                                                               !/*vi4c       81*/
!        open wwone file if more than one energy group is defined.              !/*vi4c       82*/
      if(ig.eq.0)go to 80                                                       !/*vi4c       83*/
      call unique(wwone,jtty)                                                   !/*vi4c       84*/
      call erprnt(1,2,0,0,0,0,0,0,                                              !/*vi4c       85*/
     & '55henergy groups > 1, writing single group file '//wwone//' .')         !/*vi4c       86*/
      open(iuw1,file=wwone,status='new')                                        !/*vi4c       87*/
!                                                                               !/*vi4c       88*/
!        finished with cell-based geometries.                                   !/*vi4c       89*/
   80 if(nwgeoa.eq.3)return                                                     !/*vi4c       90*/
      if(konrun.ne.0)go to 190                                                  !/*vi4c       91*/
!                                                                               !/*vi4c2       3*/
!        transfer reference point to array dc.                                  !/*vi4c2       4*/
      do 85 i=1,3                                                               !/*vi4c2       5*/
   85 dc(i)=wgma(lwga+i)                                                        !/*vi4c2       6*/
!                                                                               !/*vi4c       92*/
!        *** case 1: meshes from wwinp file. ***                                !/*vi4c       93*/
!                                                                               !/*vi4c       94*/
!        read the geometric mesh data from the wwinp file.                      !/*vi4c       95*/
      if(iw.ne.2)go to 100                                                      !/*vi4c       96*/
      rewind iuw                                                                !/*vi4c       97*/
      ir=3+nwgeoa                                                               !/*vi4c       98*/
      read(iuw,'(a80)')(du,i=1,ir)                                              !/*vi4c       99*/
      ir=0                                                                      !/*vi4c      100*/
      do 90 i=1,3                                                               !/*vi4c      101*/
      wgma(lwga+ir+1)=0.                                                        !/*vi4c      103*/
      is=ir+3                                                                   !/*vi4c      104*/
      ir=is+3*nint(wwma(i+6))                                                   !/*vi4c      105*/
      read(iuw,'(6g13.5)',end=400)(wgma(lwga+j),j=is,ir)                        !/*vi4c      106*/
   90 wgma(lwga+is-1)=wgma(lwga+is)                                             !/*vi4c2       7*/
      go to (250,230)nwgeoa                                                     !/*vi4c      109*/
!                                                                               !/*vi4c      110*/
!        *** case 2: meshes from mesh card ***                                  !/*vi4c      111*/
!                                                                               !/*vi4c      117*/
!        if no wwinp file, initialize default or mesh card mesh.                !/*vi4c      118*/
  100 if(nwgeoa.ne.1)go to 130                                                  !/*vi4c2       8*/
      ir=1                                                                      !/*vi4c      120*/
      do 120 i=1,3                                                              !/*vi4c      121*/
      wgma(lwga+ir)=0.                                                          !/*vi4c      122*/
      wgma(lwga+ir+1)=wwma(i+3)                                                 !/*vi4c      123*/
      wgma(lwga+ir+2)=wwma(i+3)                                                 !/*vi4c      124*/
  120 ir=ir+3*nint(wwma(i+6))+3                                                 !/*vi4c      125*/
      go to 160                                                                 !/*vi4c      126*/
  130 ir=1                                                                      !/*vi4c      127*/
      do 140 i=1,3                                                              !/*vi4c      128*/
      wgma(lwga+ir)=0.                                                          !/*vi4c      129*/
      wgma(lwga+ir+1)=0.                                                        !/*vi4c      130*/
      wgma(lwga+ir+2)=0.                                                        !/*vi4c      131*/
  140 ir=ir+3*nint(wwma(i+6))+3                                                 !/*vi4c      132*/
!                                                                               !/*vi4c      133*/
!        get top center point and point on bottom plane defining theta=0        !/*vi4c      134*/
!        for cylindrical mesh these pts are origin+height*axs and               !/*vi4c      135*/
!        origin+rmax*vec.                                                       !/*vi4c      136*/
      do 150 i=1,3                                                              !/*vi4c      137*/
      wwma(i+9)=wwma(i+3)+wwma(i+19)*wwma(23)                                   !/*vi4c      138*/
  150 wwma(i+12)=wwma(i+3)+wwma(i+15)*wwma(19)                                  !/*vi4c      139*/
!                                                                               !/*vi4c      140*/
!        set fine mesh ratios to 1.                                             !/*vi4c      141*/
  160 ir=0                                                                      !/*vi4c      142*/
      is=0                                                                      !/*vi4c      143*/
      do 180 i=1,3                                                              !/*vi4c      144*/
      is=3*nint(wwma(i+6))+3                                                    !/*vi4c      145*/
      do 170 j=ir+4,ir+is,3                                                     !/*vi4c      146*/
  170 wgma(lwga+j+2)=1.                                                         !/*vi4c      147*/
  180 ir=ir+is                                                                  !/*vi4c      148*/
      go to (250,230)nwgeoa                                                     !/*vi4c      149*/
!                                                                               !/*vi4c      150*/
!        *** case 3: meshes from runtpe (continue run) ***                      !/*vi4c      151*/
!                                                                               !/*vi4c      152*/
!        unconvert fine meshes from cumulative and convert radians              !/*vi4c      153*/
!        back to revolutions for wwout, wwone files.                            !/*vi4c      154*/
  190 ir=0                                                                      !/*vi4c      155*/
      do 210 i=1,3                                                              !/*vi4c      156*/
      s=0.                                                                      !/*vi4c2       9*/
      do 200 j=1,nint(wwma(i+6))                                                !/*vi4c2      10*/
      is=lwga+ir+3*j+1                                                          !/*vi4c      159*/
      t=nint(wgma(is)-s)                                                        !/*vi4c      160*/
      s=wgma(is)                                                                !/*vi4c      161*/
  200 wgma(is)=t                                                                !/*vi4c      162*/
  210 ir=is-lwga+2                                                              !/*vi4c      163*/
      if(nwgeoa.ne.2)go to 250                                                  !/*vi4c      164*/
      c=.5/pie                                                                  !/*vi4c      165*/
      ir=lwga+3*nint(wwma(7)+wwma(8))+6+5                                       !/*vi4c      166*/
      do 220 i=ir,ir+3*nint(wwma(9))-1,3                                        !/*vi4c      167*/
  220 wgma(i)=wgma(i)*c                                                         !/*vi4c      168*/
!                                                                               !/*vi4c      169*/
!        *** cases 1,2,3: all meshes ***                                        !/*vi4c      170*/
!                                                                               !/*vi4c      171*/
!        renormalize axial meshes (nwgeoa = 2).                                 !/*vi4c      172*/
  230 ir=lwga+3+3*nint(wwma(7))+4                                               !/*vi4c      173*/
      do 240 i=1,3*nint(wwma(8)),3                                              !/*vi4c      174*/
  240 wgma(ir+i)=wgma(ir+i)-wgma(ir-2)                                          !/*vi4c      175*/
      wgma(ir-1)=0.                                                             !/*vi4c      176*/
      wgma(ir-2)=0.                                                             !/*vi4c      177*/
!                                                                               !/*vi4c      178*/
!        write header and geometry information to wwout file.                   !/*vi4c      179*/
  250 w=nwgeoa                                                                  !/*vi4c      180*/
      if=1                                                                      !/*vi4c      181*/
      iv=1                                                                      !/*vi4c      182*/
      nr=10                                                                     !/*vi4c      183*/
      if(nwgeoa.eq.2)nr=16                                                      !/*vi4c      184*/
      if(mct.ge.0)write(iuwe,'(4i10,20x,a19)')if,iv,ni,nr,probid                !/*vi4c      185*/
      if(mct.lt.0)write(iuwe,'(4i10)')if,iv,ni,nr                               !/*vi4c      186*/
      write(iuwe,'(7i10)')(ngww(i),i=1,ni)                                      !/*vi4c      187*/
      write(iuwe,'(6g13.5)')(wwma(i),i=1,nr-1),w                                !/*vi4c      188*/
      ir=0                                                                      !/*vi4c      189*/
      do 260 i=1,3                                                              !/*vi4c      190*/
      is=ir+3                                                                   !/*vi4c      191*/
      ir=is+3*nint(wwma(i+6))                                                   !/*vi4c      192*/
  260 write(iuwe,'(6g13.5)')(wgma(lwga+j),j=is,ir)                              !/*vi4c      193*/
!                                                                               !/*vi4c      194*/
!        write wwone header and geometry information if more than one           !/*vi4c      195*/
!        energy group is defined.                                               !/*vi4c      196*/
      if(ig.eq.0)go to 280                                                      !/*vi4c      197*/
      if(mct.ge.0)write(iuw1,'(4i10,20x,a19)')if,iv,ni,nr,probid                !/*vi4c      198*/
      if(mct.lt.0)write(iuw1,'(4i10)')if,iv,ni,nr                               !/*vi4c      199*/
      write(iuw1,'(7i10)')(min(ngww(i),1),i=1,ni)                               !/*vi4c      200*/
      write(iuw1,'(6g13.5)')(wwma(i),i=1,nr-1),w                                !/*vi4c      201*/
      ir=0                                                                      !/*vi4c      202*/
      do 270 i=1,3                                                              !/*vi4c      203*/
      is=ir+3                                                                   !/*vi4c      204*/
      ir=is+3*nint(wwma(i+6))                                                   !/*vi4c      205*/
  270 write(iuw1,'(6g13.5)')(wgma(lwga+j),j=is,ir)                              !/*vi4c      206*/
!                                                                               !/*vi4c      207*/
!        convert revolutions to radians.                                        !/*vi4c      208*/
  280 if(nwgeoa.ne.2)go to 300                                                  !/*vi4c      209*/
      ir=lwga+3*nint(wwma(7)+wwma(8))+6+5                                       !/*vi4c      210*/
      do 290 i=ir,ir+3*nint(wwma(9))-1,3                                        !/*vi4c      211*/
  290 wgma(i)=wgma(i)*2.*pie                                                    !/*vi4c      212*/
!                                                                               !/*vi4c      213*/
!        make fine meshes cumulative to ease search burden later.               !/*vi4c      214*/
  300 ir=0                                                                      !/*vi4c      215*/
      do 320 i=1,3                                                              !/*vi4c      216*/
      do 310 j=1,nint(wwma(i+6))                                                !/*vi4c2      11*/
      is=lwga+ir+3*j+1                                                          !/*vi4c      218*/
  310 wgma(is)=wgma(is)+wgma(is-3)                                              !/*vi4c      219*/
  320 ir=is-lwga+2                                                              !/*vi4c      220*/
!                                                                               !/*vi4c      221*/
      if(konrun.ne.0)return                                                     !/*vi4c      222*/
!                                                                               !/*vi4c      223*/
!        add surface arrays:                                                    !/*vi4c      224*/
!           rr=fine mesh ratio in coarse mesh iu                                !/*vi4c      225*/
!           im=index for number of fine meshes in coarse mesh iu                !/*vi4c      226*/
!           np=number of fine meshes in coarse mesh iu                          !/*vi4c      227*/
!           wp=width of coarse mesh interval delta(iu-il)                       !/*vi4c      228*/
!           wf=width of fine mesh cells in course mesh iu                       !/*vi4c      229*/
      ib=0                                                                      !/*vi4c      230*/
      do 340 i=1,3                                                              !/*vi4c      231*/
      ic=1                                                                      !/*vi4c      232*/
      asm(lasm+i,ic)=wgma(lwga+ib+3)                                            !/*vi4c      233*/
      do 330 j=1,nint(wwma(i+6))                                                !/*vi4c      234*/
      rr=wgma(lwga+ib+3*(j+1))                                                  !/*vi4c      235*/
      im=lwga+ib+3*j+1                                                          !/*vi4c      236*/
      wn=wgma(im)-wgma(im-3)                                                    !/*vi4c      237*/
      wp=wgma(im+1)-wgma(im-2)                                                  !/*vi4c      238*/
      wf=wp/wn                                                                  !/*vi4c      239*/
      np=nint(wn)                                                               !/*vi4c      240*/
      do 330 k=1,np                                                             !/*vi4c      241*/
      ic=ic+1                                                                   !/*vi4c      242*/
  330 asm(lasm+i,ic)=asm(lasm+i,ic-1)+wf                                        !/*vi4c      243*/
      ism(i)=ic                                                                 !/*vi4c      244*/
  340 ib=ib+3*(nint(wwma(i+6))+1)                                               !/*vi4c      245*/
!                                                                               !/*vi4c      246*/
!        calculate reference mesh index number.                                 !/*vi4c      247*/
      if(nwgeoa.eq.2)go to 350                                                  !/*vi4c      248*/
      call avrxyz(dc,mi,-one,ie)                                                !/*vix         6*/
      go to 370                                                                 !/*vi4c      250*/
!                                                                               !/*vi4c      251*/
!        put reference point in cylindrical mesh coordinate system.             !/*vi4c      252*/
!        calculate r; if r=0 let theta=0.                                       !/*vi4c      253*/
  350 do 360 i=1,3                                                              !/*vi4c      254*/
  360 cc(i)=dc(i)-wwma(i+3)                                                     !/*vi4c      255*/
      dc(1)=dotpro(wwma(16),cc)                                                 !/*vi4c      256*/
      dc(3)=dotpro(wwma(20),cc)                                                 !/*vi4c      257*/
      dc(2)=dotpro(wwma(24),cc)                                                 !/*vi4c      258*/
      cc(1)=sqrt(dc(1)**2+dc(2)**2)                                             !/*vi4c      259*/
      cc(2)=dc(3)                                                               !/*vi4c      260*/
      cc(3)=0.                                                                  !/*vi4c      261*/
      if(cc(1).ne.0.)cc(3)=acos(max(-one,min(one,dc(1)/cc(1))))                 !/*vi4c      262*/
      if(dc(2).lt.0.)cc(3)=pie+pie-cc(3)                                        !/*vi4c2      12*/
      call avrxyz(cc,mi,-one,ie)                                                !/*vix         7*/
!                                                                               !/*vi4c      265*/
!        set mai = reference mesh cell index.                                   !/*vi4c      266*/
  370 mai=1+(mi(1)-2)+nint(wwma(1))*(mi(2)-2)                                   !/*vi4c      267*/
     & +nint(wwma(1)*wwma(2))*(mi(3)-2)                                         !/*vi4c      268*/
      write(iuo,380)mai,(mi(i)-1,i=1,3)                                         !/*vi4c      269*/
  380 format(//50h mesh-based weight window generator reference mesh,           !/*vi4c      270*/
     & 7h index:,i9/5x,26hfine mesh indices,  i,j,k:,3i5)                       !/*vi4c      271*/
      return                                                                    !/*vi4c      272*/
!                                                                               !/*vi4c      273*/
!        process various input errors.                                          !/*vi4c      274*/
  400 call erprnt(1,1,0,0,0,0,0,0,                                              !/*vi4c      275*/
     & '32hunexpected eof on '//wwinp//' file.')                                !/*vi4c      276*/
      go to 450                                                                 !/*vi4c      277*/
  410 call erprnt(1,1,0,0,0,0,0,0,                                              !/*vi4c      278*/
     & '53hweight window mesh input file '//wwinp//' was not found.')           !/*vi4c      279*/
      go to 450                                                                 !/*vi4c      280*/
  420 call erprnt(1,1,0,0,0,0,0,0,                                              !/*vi4c      281*/
     & '53hwwinp file contains a zero size cylinder in the grid.')              !/*vi4c      282*/
      go to 450                                                                 !/*vi4c      283*/
  430 call erprnt(1,1,0,0,0,0,0,0,                                              !/*vi4c      284*/
     & '54hwwinp file radial and axial directions not orthogonal.')             !/*vi4c      285*/
  450 iwwg=-1                                                                   !/*vi4c      289*/
      return                                                                    !/*vi4c      290*/
      end                                                                       !/*vi4c      291*/
