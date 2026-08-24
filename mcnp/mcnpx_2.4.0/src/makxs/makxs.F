!     $Id: makxs.F,v 1.5 2002/03/27 18:14:21 gwm Exp $                        !/*ma          1*/
      program makxs                                                             !/*ma          2*/
!                                                                               !/*ma          3*/
!    copyright notice for mcnp version 4b                                       !/*ma          4*/
! this program was prepared by the regents of the university of                 !/*ma          5*/
! california at los alamos national laboratory (the university) under           !/*ma          6*/
! contract number w-7405-eng-36 with the u.s. department of energy              !/*ma          7*/
! (doe).  the university has certain rights in the program pursuant to          !/*ma          8*/
! the contract and the program should not be copied or distributed              !/*ma          9*/
! outside your organization.  all rights in the program are reserved by         !/*ma         10*/
! the doe and the university.  neither the u.s. government nor the              !/*ma         11*/
! university makes any warranty, express or implied, or assumes any             !/*ma         12*/
! liability or responsibility for the use of this software.                     !/*ma         13*/
!                                                                               !/*ma         14*/
! ----------------------------------------------------------------------        !/*ma         15*/
!                                                                               !/*ma         16*/
!        makxs takes tables from old cross-section files and puts them          !/*ma         17*/
!        into new cross-section files in any arrangement specified by           !/*ma         18*/
!        the specs file.  a new xsdir file is written.  the cross-              !/*ma         19*/
!        section files may be any combination of types 1 and 2.                 !/*ma         20*/
!        supported define symbols: CHEAP, XS64, DEC, SGI                        !/*ma         21*/
!                                                                               !/*ma         22*/
!        john s. hendricks 2/5/97, andrew k. hess 6/25/98                       !/*ma         23*/
!                                                                               !/*ma         24*/
!        files used:  unit, name, contents, access/form, and use modes.         !/*ma         25*/
!         ius specs   task specifications               s/f  input only         !/*ma         26*/
!         iup tprint  output for printing               s/f  output only        !/*ma         27*/
!         iud         old xsdir                         s/f  input only         !/*ma         28*/
!         iue         new xsdir                         s/f  output only        !/*ma         29*/
!         iux         old cross-section files                input only         !/*ma         30*/
!         iuy         new cross-section files                output only        !/*ma         31*/
!                                                                               !/*ma         32*/
!        specs file:  free format, blank delimited, 80-character records        !/*ma         33*/
!        for type 1, omit the record length and entries per record.             !/*ma         34*/
!        1   old xsdir      new xsdir                                           !/*ma         35*/
!        2   oldxslib*  newxslib  type  record length*  entries/record*         !/*ma         36*/
!        3   access route (may be blank)                                        !/*ma         37*/
!        4+  list of tables for newxslib (if oldxslib blank)                    !/*ma         38*/
!            blank record (if oldxslib blank)                                   !/*ma         39*/
!        records 2 thru the blank record may be repeated with data for          !/*ma         40*/
!        additional new cross-section files. *=optional.                        !/*ma         41*/
!                                                                               !/*ma         42*/
#ifdef CHEAP                                                                    /*!ma         43*/
!        floating-point entities on 32-bit computers.                           !/*ma         44*/
      implicit double precision (a-h,o-z)                                       !/*ma         45*/
#endif                                                                          /*!ma         46*/
!                                                                               !/*ma         47*/
!        mxt > maximum number of tables in any output xs file.                  !/*ma         48*/
!        mxs > maximum record size of any input or output xs file.              !/*ma         49*/
      parameter (mxt=1000,mxs=9000)                                             !/*ma         50*/
      dimension ex(mxs),lb(mxs)                                                 !/*ma         51*/
#ifndef XS64                                                                    /*!ma         52*/
      real xs(mxs)                                                              !/*ma         53*/
#else                                                                           /*!ma         54*/
      dimension xs(mxs)                                                         !/*ma         55*/
#endif                                                                          /*!ma         56*/
      equivalence (ex,xs,lb)                                                    !/*ma         57*/
!                                                                               !/*ma         58*/
      parameter (ius=31,iup=32,iud=33,iue=34,iux=35,iuy=36)                     !/*ma         59*/
      logical lv                                                                !/*ma         60*/
      character gc*64,gf(mxt)*64,gr(mxt)*70,gz(mxt)*10,ha*1,hd*10,hf*64,        !/*ma         61*/
     & hk*70,hl*80,hm*10,hn*8,ho*8,hp*12,hr*71,ht*10,hx(7)*80,hy*1,hz*10        !/*ma         62*/
      dimension aw(16),tt(mxt),iz(16),ly(6,mxt),nx(16),jx(32)                   !/*ma         63*/
      data xs/mxs*0./                                                           !/*ma         64*/
!                                                                               !/*ma         65*/
!        open and read the specs and tprint files.                              !/*ma         66*/
      open(iup,file='tprint',status='new')                                      !/*ma         67*/
      open(ius,file='specs',status='old',err=620,iostat=ie)                     !/*ma         68*/
      rewind ius                                                                !/*ma         69*/
      write(iup,10)                                                             !/*ma         70*/
   10 format(18h1output from makxs//11h specs file/)                            !/*ma         71*/
!                                                                               !/*ma         72*/
!        get ho, hn = the names of the old, new xsdir files.                    !/*ma         73*/
      read(ius,'(a80)')hl                                                       !/*ma         74*/
      il=1                                                                      !/*ma         75*/
      call nxtsym(hl,' ',1,it,iu)                                               !/*ma         76*/
      if(iu.eq.0)go to 660                                                      !/*ma         77*/
      ho=hl(it:iu)                                                              !/*ma         78*/
      call nxtsym(hl,' ',iu+1,it,iu)                                            !/*ma         79*/
      if(iu.eq.0)go to 660                                                      !/*ma         80*/
      hn=hl(it:iu)                                                              !/*ma         81*/
      call nxtsym(hl,' ',iu+1,it,iu)                                            !/*ma         82*/
      if(iu.ne.0)go to 660                                                      !/*ma         83*/
!                                                                               !/*ma         84*/
!        open the old and new xsdir files.                                      !/*ma         85*/
      open(iud,file=ho,status='old',err=640,iostat=ie)                          !/*ma         86*/
      open(iue,file=hn,status='new')                                            !/*ma         87*/
      gc=' '                                                                    !/*ma         88*/
!                                                                               !/*ma         89*/
!        copy the atomic weights table.                                         !/*ma         90*/
   20 read(iud,'(a80)',end=40)hl                                                !/*ma         91*/
      if(hl.ne.'atomic weight ratios')go to 20                                  !/*ma         92*/
   30 write(iue,'(a80)')hl                                                      !/*ma         93*/
      if(hl.eq.'directory')go to 40                                             !/*ma         94*/
      read(iud,'(a80)',end=40)hl                                                !/*ma         95*/
      go to 30                                                                  !/*ma         96*/
!                                                                               !/*ma         97*/
!        read the specs file for the next output cross-section file.            !/*ma         98*/
!        gf(1)=old library file, hf=new library file.                           !/*ma         99*/
   40 read(ius,'(a80)',end=790)hl                                               !/*ma        100*/
      il=il+1                                                                   !/*ma        101*/
      lr=80                                                                     !/*ma        102*/
      nn=4                                                                      !/*ma        103*/
      call nxtsym(hl,' ',1,it,iu)                                               !/*ma        104*/
      if(iu.eq.0)go to 660                                                      !/*ma        105*/
      hf=hl(it:iu)                                                              !/*ma        106*/
      call nxtsym(hl,' ',iu+1,it,iu)                                            !/*ma        107*/
      if(iu.eq.0)go to 660                                                      !/*ma        108*/
      js=ius                                                                    !/*ma        109*/
      if(it.eq.iu.and.(hl(it:iu).eq.'1'.or.hl(it:iu).eq.'2'))go to 50           !/*ma        110*/
      js=iud                                                                    !/*ma        111*/
      gf(1)=hf                                                                  !/*ma        112*/
      hf=hl(it:iu)                                                              !/*ma        113*/
      call nxtsym(hl,' ',iu+1,it,iu)                                            !/*ma        114*/
      if(it.ne.iu.or.(hl(it:iu).ne.'1'.and.hl(it:iu).ne.'2'))go to 660          !/*ma        115*/
!                                                                               !/*ma        116*/
!        get new library type, ny.                                              !/*ma        117*/
!        for type 2 get record length, lr, and entries per record, nn.          !/*ma        118*/
   50 hy=hl(it:iu)                                                              !/*ma        119*/
      read(hy,'(bn,i1)')ny                                                      !/*ma        120*/
      if(ny.eq.1)go to 80                                                       !/*ma        121*/
      nn=512                                                                    !/*ma        122*/
      lr=0                                                                      !/*ma        123*/
      call nxtsym(hl,' ',iu+1,it,iu)                                            !/*ma        124*/
      if(iu.eq.0)go to 90                                                       !/*ma        125*/
      ht=hl(it:iu)                                                              !/*ma        126*/
      do 60 i=it,iu                                                             !/*ma        127*/
   60 if(index('0123456789',hl(i:i)).eq.0)go to 660                             !/*ma        128*/
      read(ht,'(bn,i10)')lr                                                     !/*ma        129*/
      call nxtsym(hl,' ',iu+1,it,iu)                                            !/*ma        130*/
      if(iu.eq.0)go to 660                                                      !/*ma        131*/
      ht=hl(it:iu)                                                              !/*ma        132*/
      do 70 i=it,iu                                                             !/*ma        133*/
   70 if(index('0123456789',hl(i:i)).eq.0)go to 660                             !/*ma        134*/
      read(ht,'(bn,i10)')nn                                                     !/*ma        135*/
!        check for too many entries on line in specs file.                      !/*ma        136*/
   80 call nxtsym(hl,' ',iu+1,it,iu)                                            !/*ma        137*/
      if(iu.ne.0)go to 660                                                      !/*ma        138*/
!                                                                               !/*ma        139*/
!        read new library data path, hr (3rd line on specs file).               !/*ma        140*/
   90 nz=0                                                                      !/*ma        141*/
      read(ius,'(a80)',end=680)hl                                               !/*ma        142*/
      il=il+1                                                                   !/*ma        143*/
      if(hl.eq.' ')hl='0'                                                       !/*ma        144*/
      call nxtsym(hl,' ',1,it,iu)                                               !/*ma        145*/
      hr=hl(it:iu)                                                              !/*ma        146*/
      call nxtsym(hl,' ',iu+1,it,iu)                                            !/*ma        147*/
      if(iu.ne.0)go to 660                                                      !/*ma        148*/
!                                                                               !/*ma        149*/
!        read zaid from specs file (js=ius) or old xsdir (js=iud).              !/*ma        150*/
  100 read(js,'(a80)',end=130)hl                                                !/*ma        151*/
      if(js.eq.ius)il=il+1                                                      !/*ma        152*/
      if(hl.eq.' ')go to 130                                                    !/*ma        153*/
      iu=0                                                                      !/*ma        154*/
  110 call nxtsym(hl,' ',iu+1,it,iu)                                            !/*ma        155*/
      if(iu.eq.0)go to 100                                                      !/*ma        156*/
      nz=nz+1                                                                   !/*ma        157*/
      gz(nz)=' '                                                                !/*ma        158*/
      gz(nz)(10+it-iu:10)=hl(it:iu)                                             !/*ma        159*/
      if(js.eq.ius)go to 110                                                    !/*ma        160*/
      call nxtsym(hl,' ',iu+1,it,iu)                                            !/*ma        161*/
      if(iu.ne.0)call nxtsym(hl,' ',iu+1,it,iu)                                 !/*ma        162*/
      if(iu.eq.0)go to 120                                                      !/*ma        163*/
      if(hl(it:iu).eq.gf(1))go to 100                                           !/*ma        164*/
  120 nz=nz-1                                                                   !/*ma        165*/
      go to 100                                                                 !/*ma        166*/
!                                                                               !/*ma        167*/
!        read old directory to set up locators for getting xsecs                !/*ma        168*/
!        from the old library.                                                  !/*ma        169*/
  130 rewind iud                                                                !/*ma        170*/
      ha=gz(1)(10:10)                                                           !/*ma        171*/
      if(ny.ne.2)go to 140                                                      !/*ma        172*/
      ex(mxs)=12345.                                                            !/*ma        173*/
!        lm=default record length.  lr=true record length.                      !/*ma        174*/
#if defined(DEC) || defined(SGI)                                                /*!ma        175*/
      lm=nn                                                                     !/*ma        176*/
#else                                                                           /*!ma        177*/
      lm=4*nn                                                                   !/*ma        178*/
#endif                                                                          /*!ma        179*/
      if(ex(mxs).eq.xs(mxs).or.ha.eq.'e')lm=2*lm                                !/*ma        180*/
      if(lr.eq.0)lr=lm                                                          !/*ma        181*/
      if(lr.ne.lm)write(iup,135)hf(1:10),lr,lm                                  !/*ma        182*/
  135 format(/16h *** warning ***//23h record length of file ,a10,5x,           !/*ma        183*/
     & 10hspecified:,i8,5x,9hexpected:,i8/)                                     !/*ma        184*/
!        skip atomic weight ratios to old xsdir directory beginning.            !/*ma        185*/
  140 read(iud,'(a80)',end=720)hl                                               !/*ma        186*/
      if(hl.eq.'directory')go to 150                                            !/*ma        187*/
      go to 140                                                                 !/*ma        188*/
  150 do 160 i=1,nz                                                             !/*ma        189*/
      tt(i)=0.                                                                  !/*ma        190*/
      ly(6,i)=0                                                                 !/*ma        191*/
  160 ly(1,i)=0                                                                 !/*ma        192*/
!                                                                               !/*ma        193*/
!        match requested nuclides, gz(i), with old xsdir nuclides, ht.          !/*ma        194*/
      do 220 ni=1,nz                                                            !/*ma        195*/
  170 read(iud,'(a80)',end=720)hl                                               !/*ma        196*/
      call nxtsym(hl,' ',1,it,iu)                                               !/*ma        197*/
      ht=' '                                                                    !/*ma        198*/
      ht(10+it-iu:10)=hl(it:iu)                                                 !/*ma        199*/
      do 180 i=1,nz                                                             !/*ma        200*/
  180 if(gz(i).eq.ht)go to 190                                                  !/*ma        201*/
      go to 170                                                                 !/*ma        202*/
  190 if(ly(1,i).ne.0)go to 700                                                 !/*ma        203*/
      j=2                                                                       !/*ma        204*/
!        handle continuations (+) in xsdir file.                                !/*ma        205*/
  200 call nxtsym(hl,' ',iu+1,it,iu)                                            !/*ma        206*/
      if(iu.eq.0)go to 220                                                      !/*ma        207*/
      ht=hl(it:iu)                                                              !/*ma        208*/
      if(ht.ne.'+')go to 210                                                    !/*ma        209*/
      read(iud,'(a80)',end=720)hl                                               !/*ma        210*/
      iu=0                                                                      !/*ma        211*/
      go to 200                                                                 !/*ma        212*/
!                                                                               !/*ma        213*/
!        get:  library name gf(i), path gr(i), type ly(1,i),                    !/*ma        214*/
!              address ly(2,i), length ly(3,i), record length ly(4,i),          !/*ma        215*/
!              words per record ly(5,i), ptable flag ly(6,i).                   !/*ma        216*/
  210 if(j.eq.3)gf(i)=hl(it:iu)                                                 !/*ma        217*/
      if(j.eq.4)gr(i)=hl(it:iu)                                                 !/*ma        218*/
      if(j.ge.5.and.j.le.9)read(ht,'(bn,i10)')ly(j-4,i)                         !/*ma        219*/
      if(hl(it:iu).eq.'ptable')ly(6,i)=1                                        !/*ma        220*/
      j=j+1                                                                     !/*ma        221*/
      if(j.ne.11)go to 200                                                      !/*ma        222*/
      hp=' '                                                                    !/*ma        223*/
      hp(12+it-iu:12)=hl(it:iu)                                                 !/*ma        224*/
      read(hp,'(e12.0)')tt(i)                                                   !/*ma        225*/
      go to 200                                                                 !/*ma        226*/
  220 if(ly(1,i).eq.1)ly(5,i)=4                                                 !/*ma        227*/
!                                                                               !/*ma        228*/
!        open the new cross-section file.                                       !/*ma        229*/
      if(ny.eq.1)open(iuy,file=hf,status='new')                                 !/*ma        230*/
      if(ny.ne.2)go to 250                                                      !/*ma        231*/
      jr=0                                                                      !/*ma        232*/
      do 230 je=1,nz                                                            !/*ma        233*/
  230 jr=jr+1+(ly(3,je)+nn-1)/nn                                                !/*ma        234*/
      write(iup,240)hf,jr                                                       !/*ma        235*/
  240 format(/47h new type 2 (direct access) cross-section file ,a64/           !/*ma        236*/
     & 13h will contain,i9,9h records.)                                         !/*ma        237*/
!rep                                                                            !/*ma        238*/
      open(iuy,file=hf,access='direct',recl=lr,status='new')                    !/*ma        239*/
!                                                                               !/*ma        240*/
!        do all of the required tables for this output file.                    !/*ma        241*/
  250 mr=1                                                                      !/*ma        242*/
      do 600 je=1,nz                                                            !/*ma        243*/
      ms=mr                                                                     !/*ma        244*/
!                                                                               !/*ma        245*/
!        open the old file if it is not already open.                           !/*ma        246*/
      if(gf(je).eq.gc)go to 260                                                 !/*ma        247*/
      if(gc.ne.' ')close(iux)                                                   !/*ma        248*/
      gc=gf(je)                                                                 !/*ma        249*/
      inquire(file=gc,exist=lv)                                                 !/*ma        250*/
      if(.not.lv)go to 750                                                      !/*ma        251*/
      if(ly(1,je).eq.1)open(iux,file=gc,status='old')                           !/*ma        252*/
      if(ly(1,je).eq.2)open(iux,file=gc,status='old',access='direct',           !/*ma        253*/
     & recl=ly(4,je))                                                           !/*ma        254*/
      if(ly(1,je).eq.1)rewind iux                                               !/*ma        255*/
      nr=1                                                                      !/*ma        256*/
!                                                                               !/*ma        257*/
!        copy the first block from the old file to the new file.                !/*ma        258*/
  260 nb=0                                                                      !/*ma        259*/
      nw=0                                                                      !/*ma        260*/
      if(ly(1,je).eq.2)go to 400                                                !/*ma        261*/
      if(ly(2,je).ge.nr)go to 370                                               !/*ma        262*/
      rewind iux                                                                !/*ma        263*/
      nr=1                                                                      !/*ma        264*/
  370 do 380 i=1,ly(2,je)-nr                                                    !/*ma        265*/
  380 read(iux,'(a1)',end=770)                                                  !/*ma        266*/
      read(iux,390,end=770)hz,at,tz,hd,hk,hm,(iz(i),aw(i),i=1,16),nx,jx         !/*ma        267*/
  390 format(a10,2e12.0,1x,a10/a70,a10/4(i7,f11.0)/4(i7,f11.0)/                 !/*ma        268*/
     & 4(i7,f11.0)/4(i7,f11.0)/8i9/8i9/8i9/8i9/8i9/8i9)                         !/*ma        269*/
      nr=ly(2,je)+12+(nx(1)+3)/4                                                !/*ma        270*/
      go to 410                                                                 !/*ma        271*/
  400 read(iux,rec=ly(2,je))hz,at,tz,hd,hk,hm,(iz(i),aw(i),i=1,16),nx,jx        !/*ma        272*/
      kr=ly(2,je)+1                                                             !/*ma        273*/
  410 if(ny.ne.1)go to 430                                                      !/*ma        274*/
      write(iuy,420)hz,at,tz,hd,hk,hm,(iz(i),aw(i),i=1,16),nx,jx                !/*ma        275*/
  420 format(a10,f12.6,1pe12.5,1x,a10/a70,a10/4(i7,0pf11.6)/4(i7,f11.6)/        !/*ma        276*/
     & 4(i7,f11.6)/4(i7,f11.6)/8i9/8i9/8i9/8i9/8i9/8i9)                         !/*ma        277*/
      mr=mr+11                                                                  !/*ma        278*/
      go to 440                                                                 !/*ma        279*/
  430 write(iuy,rec=mr)hz,at,tz,hd,hk,hm,(iz(i),aw(i),i=1,16),nx,jx             !/*ma        280*/
  440 ha=hz(10:10)                                                              !/*ma        281*/
!                                                                               !/*ma        282*/
!        copy the second block from the old file to the new file.               !/*ma        283*/
!        special treatment if both are type 1.                                  !/*ma        284*/
      if(ly(1,je).ne.1.or.ny.ne.1)go to 460                                     !/*ma        285*/
      do 450 ir=1,(nx(1)+3)/4                                                   !/*ma        286*/
      read(iux,'(a80)',end=770)hl                                               !/*ma        287*/
  450 write(iuy,'(a80)')hl                                                      !/*ma        288*/
      go to 560                                                                 !/*ma        289*/
!                                                                               !/*ma        290*/
!        general treatment if either is type 2.                                 !/*ma        291*/
  460 do 550 ir=1,(nx(1)+nn-1)/nn                                               !/*ma        292*/
      if(ly(1,je).eq.3)go to 480                                                !/*ma        293*/
      do 470 i=1,(nn-nb+ly(5,je)-1)/ly(5,je)                                    !/*ma        294*/
      n=min(ly(5,je),nx(1)-nw)                                                  !/*ma        295*/
      if(n.le.0)go to 480                                                       !/*ma        296*/
      if(ha.eq.'e'.and.ly(1,je).eq.1)read(iux,'(4e20.0)',end=770)               !/*ma        297*/
     & (ex(j),j=nb+1,nb+n)                                                      !/*ma        298*/
      if(ha.eq.'e'.and.ly(1,je).eq.2)read(iux,rec=kr)(ex(j),j=nb+1,nb+n)        !/*ma        299*/
      if(ha.ne.'e'.and.ly(1,je).eq.1)read(iux,'(4e20.0)',end=770)               !/*ma        300*/
     & (xs(j),j=nb+1,nb+n)                                                      !/*ma        301*/
      if(ha.ne.'e'.and.ly(1,je).eq.2)read(iux,rec=kr)(xs(j),j=nb+1,nb+n)        !/*ma        302*/
      kr=kr+1                                                                   !/*ma        303*/
      nw=nw+n                                                                   !/*ma        304*/
  470 nb=nb+n                                                                   !/*ma        305*/
  480 if(ny.eq.2)go to 500                                                      !/*ma        306*/
      hl=' '                                                                    !/*ma        307*/
      do 490 i=1,min(4,nb)                                                      !/*ma        308*/
      if(ha.ne.'e')go to 485                                                    !/*ma        309*/
      if(aint(ex(i)).eq.ex(i))write(hl(20*i-19:20*i),'(i20)')int(ex(i))         !/*ma        310*/
      if(aint(ex(i)).ne.ex(i))write(hl(20*i-19:20*i),'(1pe20.12)')ex(i)         !/*ma        311*/
      go to 490                                                                 !/*ma        312*/
  485 if(aint(xs(i)).eq.xs(i))write(hl(20*i-19:20*i),'(i20)')int(xs(i))         !/*ma        313*/
      if(aint(xs(i)).ne.xs(i))write(hl(20*i-19:20*i),'(1pe20.12)')xs(i)         !/*ma        314*/
  490 continue                                                                  !/*ma        315*/
      write(iuy,'(a80)')hl                                                      !/*ma        316*/
      go to 510                                                                 !/*ma        317*/
  500 if(ha.ne.'e')write(iuy,rec=mr+ir)(xs(j),j=1,min(nn,nb))                   !/*ma        318*/
      if(ha.eq.'e')write(iuy,rec=mr+ir)(ex(j),j=1,min(nn,nb))                   !/*ma        319*/
  510 if(ha.ne.'e')go to 530                                                    !/*ma        320*/
      do 520 i=1,nb-nn                                                          !/*ma        321*/
  520 ex(i)=ex(nn+i)                                                            !/*ma        322*/
      go to 550                                                                 !/*ma        323*/
  530 do 540 i=1,nb-nn                                                          !/*ma        324*/
  540 xs(i)=xs(nn+i)                                                            !/*ma        325*/
  550 nb=nb-nn                                                                  !/*ma        326*/
!                                                                               !/*ma        327*/
!        write the new xsdir entry.                                             !/*ma        328*/
  560 do 570 i=1,7                                                              !/*ma        329*/
  570 hx(i)=' '                                                                 !/*ma        330*/
      call addsym(hz,hx,7,ix,ie)                                                !/*ma        331*/
      write(hp,'(f12.6)')at                                                     !/*ma        332*/
      call addsym(hp,hx,7,ix,ie)                                                !/*ma        333*/
      call addsym(hf,hx,7,ix,ie)                                                !/*ma        334*/
      call addsym(hr,hx,7,ix,ie)                                                !/*ma        335*/
      call addsym(hy,hx,7,ix,ie)                                                !/*ma        336*/
      write(ht,'(i10)')ms                                                       !/*ma        337*/
      call addsym(ht,hx,7,ix,ie)                                                !/*ma        338*/
      write(ht,'(i10)')nx(1)                                                    !/*ma        339*/
      call addsym(ht,hx,7,ix,ie)                                                !/*ma        340*/
      if(ny.eq.1.and.ha.ne.'c'.and.ha.ne.'d'.and.ha.ne.'p')go to 590            !/*ma        341*/
      write(ht,'(i10)')lr                                                       !/*ma        342*/
      if(ny.eq.1)ht='0'                                                         !/*ma        343*/
      call addsym(ht,hx,7,ix,ie)                                                !/*ma        344*/
      write(ht,'(i10)')nn                                                       !/*ma        345*/
      if(ny.eq.1)ht='0'                                                         !/*ma        346*/
      call addsym(ht,hx,7,ix,ie)                                                !/*ma        347*/
      if(ha.ne.'c'.and.ha.ne.'d'.and.ha.ne.'p')go to 590                        !/*ma        348*/
      write(hp,'(1pe12.4)')tz                                                   !/*ma        349*/
      call addsym(hp,hx,7,ix,ie)                                                !/*ma        350*/
      if(ly(6,je).ne.0)call addsym('ptable',hx,7,ix,ie)                         !/*ma        351*/
      i=index(hd,'/')                                                           !/*ma        352*/
      j=index(hd(min(10,i+1):10),'/')                                           !/*ma        353*/
      if(i.ge.2.and.j.ge.i+2.and.j.le.i+3.and.j.le.8)                           !/*ma        354*/
     & call addsym(hd,hx,7,ix,ie)                                               !/*ma        355*/
      do 580 i=1,16                                                             !/*ma        356*/
      if(iz(i).eq.0)go to 590                                                   !/*ma        357*/
      write(ht,'(i10)')iz(i)                                                    !/*ma        358*/
      write(hp,'(f12.6)')aw(i)                                                  !/*ma        359*/
      call addsym(ht,hx,7,ix,ie)                                                !/*ma        360*/
  580 call addsym(hp,hx,7,ix,ie)                                                !/*ma        361*/
  590 write(iue,'(a80)')(hx(i),i=1,ix)                                          !/*ma        362*/
  600 mr=mr+ir                                                                  !/*ma        363*/
      write(iup,'(/23h finished writing file ,a64)')hf                          !/*ma        364*/
      close(iuy)                                                                !/*ma        365*/
      if(js.eq.ius)go to 40                                                     !/*ma        366*/
      rewind iud                                                                !/*ma        367*/
  610 read(iud,'(a80)',end=720)hl                                               !/*ma        368*/
      if(hl.eq.'directory')go to 40                                             !/*ma        369*/
      go to 610                                                                 !/*ma        370*/
!                                                                               !/*ma        371*/
!        print a message and quit in case of an error.                          !/*ma        372*/
  620 write(iup,630)ie                                                          !/*ma        373*/
  630 format(/41h trouble opening the specs file.  iostat=,i6)                  !/*ma        374*/
      stop                                                                      !/*ma        375*/
!                                                                               !/*ma        376*/
  640 write(iup,650)ho,ie                                                       !/*ma        377*/
  650 format(/28h trouble opening xsdir file ,a8,6x,7hiostat=,i6)               !/*ma        378*/
      stop                                                                      !/*ma        379*/
!                                                                               !/*ma        380*/
  660 write(iup,670)il,hl                                                       !/*ma        381*/
  670 format(/25h bad record in specs file/1x,i5,7x,a80)                        !/*ma        382*/
      stop                                                                      !/*ma        383*/
!                                                                               !/*ma        384*/
  680 write(iup,690)                                                            !/*ma        385*/
  690 format(/38h unexpected end of file in specs file.)                        !/*ma        386*/
      stop                                                                      !/*ma        387*/
!                                                                               !/*ma        388*/
  700 write(iup,710)ht,ho                                                       !/*ma        389*/
  710 format(/1x,a10,38h appears at least twice in xsdir file ,a8)              !/*ma        390*/
      stop                                                                      !/*ma        391*/
!                                                                               !/*ma        392*/
  720 write(iup,730)ho                                                          !/*ma        393*/
  730 format(/44h the following tables are not in xsdir file ,a8)               !/*ma        394*/
      do 740 i=1,nz                                                             !/*ma        395*/
  740 if(ly(1,i).eq.0)write(iup,'(4x,a10)')gz(i)                                !/*ma        396*/
      stop                                                                      !/*ma        397*/
!                                                                               !/*ma        398*/
  750 write(iup,760)gc                                                          !/*ma        399*/
  760 format(/34h a required file does not exist:  ,a64)                        !/*ma        400*/
      stop                                                                      !/*ma        401*/
!                                                                               !/*ma        402*/
  770 write(iup,780)gc                                                          !/*ma        403*/
  780 format(/32h unexpected end of file on file ,a64)                          !/*ma        404*/
  790 stop                                                                      !/*ma        405*/
      end                                                                       !/*ma        406*/
