!#ident rgdtal.F from deck xrg                                                  !/*xrg         1*/
      subroutine rgdtal(mm)                                                     !/*xrg         2*/
#ifdef MESHTAL                                                                  /*!xrg         3*/
!                                                                               !/*xrg         4*/
!       This subroutine is designed to read the data off of the INP             !/*xrg         5*/
!       file between the mesh and endms cards.  This is the data to             !/*xrg         6*/
!       set up all mesh tallies.                                                !/*xrg         7*/
!                                                                               !/*xrg         8*/
!       mm = 1   First pass through "INP".                                      !/*xrg         9*/
!       mm = 2   Second pass through "INP" fill data arrays.                    !/*xrg        10*/
!                                                                               !/*xrg        11*/
      use global_data                                                           !/*xrg        12*/
!                                                                               !/*xrg        13*/
      implicit real(kindr) (a-h,o-z)                                            !/*xrg        14*/
      implicit integer(kindi) (i-n)                                             !/*xrg        15*/
!                                                                               !/*xrg        16*/
!                                                                               !/*xrg        17*/
      common /meshtal/ ergl(maxg),ergh(maxg),rdos(5,maxg),                      !/*xrg        18*/
     & rmult(maxg),mdos(3,5,maxg),mmult(3,maxg),nemsh(maxg),                    !/*xrg        19*/
     & mshtr(maxg),mstyp(maxg),mshpt(maxg),ng1(maxg),ng2(maxg),                 !/*xrg        20*/
     & ng3(maxg),mxgt(maxg),ngv(maxg,maxgv),mxgc(maxg),mxgv(maxg)               !/*xrg        21*/
!                                                                               !/*xrg        22*/
      character ha*80,hd*80,hc(10)*5,hk(10)*5,hy(10)*5,hp*34                    !/*xrg        23*/
      character hx*25,hf*36                                                     !/*xrg        24*/
      data hc/'rmesh','cmesh','smesh','cora','corb','corc','ergsh',             !/*xrg        25*/
     & 'mshmf',2*' '/,hy/'flux ','traks','popul','pedep','dose ',               !/*xrg        26*/
     &  3*' ','mfact','trans'/,hk/'total','de/dx','recol','tlest',              !/*xrg        27*/
     &  'edlct',3*' ','mfact','trans'/,                                         !/*xrg        28*/
     &  hp/'npe|*uvwhl+-xyoc!?</zk%^g@f>bqdtsa'/                                !/*xrg        29*/
!                                                                               !/*xrg        30*/
!       Read through the mesh data input.                                       !/*xrg        31*/
!                                                                               !/*xrg        32*/
!       If this is the 1st pass, initialize all common.                         !/*xrg        33*/
      if(mm.eq.1) then                                                          !/*xrg        34*/
         do 4 i=1,maxg                                                          !/*xrg        35*/
            ergl(i)=zero                                                        !/*xrg        36*/
            ergh(i)=huge                                                        !/*xrg        37*/
            rmult(i)=zero                                                       !/*xrg        38*/
            nemsh(i)=0                                                          !/*xrg        39*/
            mshtr(i)=0                                                          !/*xrg        40*/
            mstyp(i)=0                                                          !/*xrg        41*/
            mshpt(i)=0                                                          !/*xrg        42*/
            ng1(i)=0                                                            !/*xrg        43*/
            ng2(i)=0                                                            !/*xrg        44*/
            ng3(i)=0                                                            !/*xrg        45*/
            mxgt(i)=0                                                           !/*xrg        46*/
            mxgc(i)=0                                                           !/*xrg        47*/
            mxgv(i)=0                                                           !/*xrg        48*/
            nugd(i)=0                                                           !/*xrg        49*/
!                                                                               !/*xrg        50*/
            do 1 j=1,5                                                          !/*xrg        51*/
               rdos(j,i)=zero                                                   !/*xrg        52*/
    1          continue                                                         !/*xrg        53*/
!                                                                               !/*xrg        54*/
            do 2 j=1,3                                                          !/*xrg        55*/
               mmult(j,i)=0                                                     !/*xrg        56*/
               do 2 k=1,5                                                       !/*xrg        57*/
                  mdos(j,k,i)=0                                                 !/*xrg        58*/
    2             continue                                                      !/*xrg        59*/
!                                                                               !/*xrg        60*/
            do 3 j=1,maxgv                                                      !/*xrg        61*/
               ngv(i,j)=0                                                       !/*xrg        62*/
    3          continue                                                         !/*xrg        63*/
!                                                                               !/*xrg        64*/
    4       continue                                                            !/*xrg        65*/
!                                                                               !/*xrg        66*/
      endif                                                                     !/*xrg        67*/
!                                                                               !/*xrg        68*/
    5 ni=0                                                                      !/*xrg        69*/
      nj=0                                                                      !/*xrg        70*/
      nc=0                                                                      !/*xrg        71*/
      nd=0                                                                      !/*xrg        72*/
   10 read(iui,'(a80)',end=990)ha                                               !/*xrg        73*/
      if(ha.eq.' ')go to 999                                                    !/*xrg        74*/
      if(mm.eq.2) then                                                          !/*xrg        75*/
         iln=iln+1                                                              !/*xrg        76*/
         write(iuo,'(i5,1h-,7x,a80)')iln,ha                                     !/*xrg        77*/
      endif                                                                     !/*xrg        78*/
      call nxtsym(ha,' ',1,ia,ib,1)                                             !/*xrg        79*/
      if(ha(ia:ib).eq.'c'.and.ha(1:5).ne.' ')go to 10                           !/*xrg        80*/
      do 14 ik=1,80                                                             !/*xrg        81*/
         if(ha(ik:ik).ne.' ') then                                              !/*xrg        82*/
            ii=ik                                                               !/*xrg        83*/
            go to 15                                                            !/*xrg        84*/
         endif                                                                  !/*xrg        85*/
   14    continue                                                               !/*xrg        86*/
!                                                                               !/*xrg        87*/
   15 if(ii.gt.5.and.nc.ne.0) then                                              !/*xrg        88*/
         hd=ha                                                                  !/*xrg        89*/
         it=6                                                                   !/*xrg        90*/
         go to 40                                                               !/*xrg        91*/
      else if (ii.le.5.and.nc.ne.0) then                                        !/*xrg        92*/
         nd=1                                                                   !/*xrg        93*/
         go to 40                                                               !/*xrg        94*/
      else if (ii.gt.5.and.nc.eq.0) then                                        !/*xrg        95*/
         go to 970                                                              !/*xrg        96*/
      endif                                                                     !/*xrg        97*/
!                                                                               !/*xrg        98*/
   20 hd=ha                                                                     !/*xrg        99*/
      it=ii                                                                     !/*xrg       100*/
      if(hd(it:it+4).eq.'endmd') return                                         !/*xrg       101*/
!                                                                               !/*xrg       102*/
   25 ix=index('+*',hd(it:it))                                                  !/*xrg       103*/
      if(ix.ne.0)it=it+1                                                        !/*xrg       104*/
      do 27 ik=it,80                                                            !/*xrg       105*/
         n=index(' 0123456789',hd(ik:ik))                                       !/*xrg       106*/
         if(n.ne.0) then                                                        !/*xrg       107*/
            iu=ik-1                                                             !/*xrg       108*/
            go to 28                                                            !/*xrg       109*/
         endif                                                                  !/*xrg       110*/
   27    continue                                                               !/*xrg       111*/
!                                                                               !/*xrg       112*/
   28 do 30 i=1,8                                                               !/*xrg       113*/
         if(hd(it:iu).eq.hc(i)(1:iu-it+1)) then                                 !/*xrg       114*/
            nc=i                                                                !/*xrg       115*/
            it=iu+1                                                             !/*xrg       116*/
            go to 40                                                            !/*xrg       117*/
         endif                                                                  !/*xrg       118*/
   30    continue                                                               !/*xrg       119*/
      if(mm.eq.1)go to 980                                                      !/*xrg       120*/
      go to 10                                                                  !/*xrg       121*/
!                                                                               !/*xrg       122*/
!       Transferr according to mm.                                              !/*xrg       123*/
   40 go to (50,500)mm                                                          !/*xrg       124*/
!                                                                               !/*xrg       125*/
!       Go through the input data and set the all of the parameters             !/*xrg       126*/
!       that you can.                                                           !/*xrg       127*/
!                                                                               !/*xrg       128*/
!       Transferr to handle each type of card.                                  !/*xrg       129*/
   50 go to (60,60,60,200,200,200,200,400)nc                                    !/*xrg       130*/
!                                                                               !/*xrg       131*/
!       <<  Mesh cards. >>                rmesh,cmesh,smesh                     !/*xrg       132*/
   60 if(nd.ne.0)go to 190                                                      !/*xrg       133*/
      if(ni.gt.0.or.nj.gt.0)go to 90                                            !/*xrg       134*/
      kx=0                                                                      !/*xrg       135*/
      do 70 ik=it,80                                                            !/*xrg       136*/
         n=index('0123456789 ',hd(ik:ik))                                       !/*xrg       137*/
         if(n.eq.11.and.kx.eq.0)go to 960                                       !/*xrg       138*/
         if(n.ne.11.and.n.ne.0) then                                            !/*xrg       139*/
            kx=kx*10+n-1                                                        !/*xrg       140*/
         else if ((n.eq.11.or.n.eq.0).and.kx.gt.0) then                         !/*xrg       141*/
            it=ik                                                               !/*xrg       142*/
            nmesh=nmesh+1                                                       !/*xrg       143*/
            go to 80                                                            !/*xrg       144*/
         endif                                                                  !/*xrg       145*/
   70    continue                                                               !/*xrg       146*/
!                                                                               !/*xrg       147*/
   80 nugd(nmesh)=kx                                                            !/*xrg       148*/
      nt=mod(kx,10)                                                             !/*xrg       149*/
      mstyp(nmesh)=10*nc+nt-1                                                   !/*xrg       150*/
      if(nt.eq.4.and.ix.eq.2) then                                              !/*xrg       151*/
         mstyp(nmesh)=mstyp(nmesh)+1                                            !/*xrg       152*/
         nt=5                                                                   !/*xrg       153*/
      endif                                                                     !/*xrg       154*/
      if(nt.eq.1)mtlflg=1                                                       !/*xrg       155*/
      if(nt.eq.2)mspflg=1                                                       !/*xrg       156*/
      if(nt.eq.3)medflg=1                                                       !/*xrg       157*/
      if(nt.eq.4)mpdflg=1                                                       !/*xrg       158*/
      if(nt.eq.5)mdxflg=1                                                       !/*xrg       159*/
      if(nt.eq.1.or.nt.ge.4) then                                               !/*xrg       160*/
         if(hd(it:it).ne.':') then                                              !/*xrg       161*/
            call erprnt(1,1,1,nt,0,0,0,0,                                       !/*xrg       162*/
     &       '10h Mesh type,i2,26h requires a particle type.')                  !/*xrg       163*/
            go to 5                                                             !/*xrg       164*/
         endif                                                                  !/*xrg       165*/
         it=it+1                                                                !/*xrg       166*/
         n=index(hp,hd(it:it))                                                  !/*xrg       167*/
         if(n.eq.0) then                                                        !/*xrg       168*/
            call erprnt(1,1,0,0,0,0,0,0,                                        !/*xrg       169*/
     &       '31h '//hd(it:it)//' is an illegal particle type.')                !/*xrg       170*/
            go to 5                                                             !/*xrg       171*/
         endif                                                                  !/*xrg       172*/
         it=it+1                                                                !/*xrg       173*/
         mshpt(nmesh)=n                                                         !/*xrg       174*/
      endif                                                                     !/*xrg       175*/
!                                                                               !/*xrg       176*/
   90 do 100 ik=it,80                                                           !/*xrg       177*/
         if(hd(ik:ik).ne.' ') then                                              !/*xrg       178*/
            it=ik                                                               !/*xrg       179*/
            go to 110                                                           !/*xrg       180*/
         endif                                                                  !/*xrg       181*/
  100    continue                                                               !/*xrg       182*/
      go to 10                                                                  !/*xrg       183*/
!                                                                               !/*xrg       184*/
  110 do 120 ik=it,80                                                           !/*xrg       185*/
         if(hd(ik:ik).eq.' ') then                                              !/*xrg       186*/
            iu=ik-1                                                             !/*xrg       187*/
            go to 130                                                           !/*xrg       188*/
         endif                                                                  !/*xrg       189*/
  120    continue                                                               !/*xrg       190*/
!                                                                               !/*xrg       191*/
  130 if (nt.eq.1.or.nt.eq.2.and.iu.ne.it) then                                 !/*xrg       192*/
         do 140 ij=1,10                                                         !/*xrg       193*/
            if(hd(it:iu).eq.hy(ij)) then                                        !/*xrg       194*/
               if(ij.ge.9) then                                                 !/*xrg       195*/
                  nj=nj+1                                                       !/*xrg       196*/
                  go to 142                                                     !/*xrg       197*/
               else                                                             !/*xrg       198*/
                  ni=ni+1                                                       !/*xrg       199*/
                  ngv(nmesh,ni)=ij                                              !/*xrg       200*/
                  go to 142                                                     !/*xrg       201*/
               endif                                                            !/*xrg       202*/
            endif                                                               !/*xrg       203*/
  140       continue                                                            !/*xrg       204*/
         hf=hd(it:iu)                                                           !/*xrg       205*/
         call erprnt(1,1,0,0,0,0,0,0,'55h illegal keyword:  '//hf)              !/*xrg       206*/
         go to 5                                                                !/*xrg       207*/
!                                                                               !/*xrg       208*/
  142    it=iu+1                                                                !/*xrg       209*/
         do 145 ik=it,80                                                        !/*xrg       210*/
            n=index('abcdefghijklmnopqrstuvwxyz',hd(ik:ik))                     !/*xrg       211*/
            m=index('0123456789',hd(ik:ik))                                     !/*xrg       212*/
            if(n.ne.0) then                                                     !/*xrg       213*/
               it=ik                                                            !/*xrg       214*/
               go to 110                                                        !/*xrg       215*/
            endif                                                               !/*xrg       216*/
            if(m.ne.0) then                                                     !/*xrg       217*/
               it=ik                                                            !/*xrg       218*/
               if(ij.eq.5.or.ij.ge.9)go to 146                                  !/*xrg       219*/
               call erprnt(1,1,0,0,0,0,0,0,                                     !/*xrg       220*/
     &         '44hKeyword '//hy(ij)//' cannot have numerical entries.')        !/*xrg       221*/
               go to 10                                                         !/*xrg       222*/
            endif                                                               !/*xrg       223*/
  145       continue                                                            !/*xrg       224*/
         go to 10                                                               !/*xrg       225*/
!                                                                               !/*xrg       226*/
  146    do 147 ik=it,80                                                        !/*xrg       227*/
            if (hd(ik:ik).eq.' ') then                                          !/*xrg       228*/
               iu=ik-1                                                          !/*xrg       229*/
               go to 142                                                        !/*xrg       230*/
            endif                                                               !/*xrg       231*/
  147       continue                                                            !/*xrg       232*/
!                                                                               !/*xrg       233*/
      else if (nt.eq.2) then                                                    !/*xrg       234*/
         n=index(hp,hd(it:iu))                                                  !/*xrg       235*/
         if(n.ne.0) then                                                        !/*xrg       236*/
            ni=ni+1                                                             !/*xrg       237*/
            it=iu+1                                                             !/*xrg       238*/
            ngv(nmesh,ni)=n                                                     !/*xrg       239*/
            go to 90                                                            !/*xrg       240*/
         endif                                                                  !/*xrg       241*/
         call erprnt(1,1,0,0,0,0,0,0,                                           !/*xrg       242*/
     &    '31h '//hd(it:it)//' is an illegal particle type.')                   !/*xrg       243*/
         go to 5                                                                !/*xrg       244*/
!                                                                               !/*xrg       245*/
      else if (nt.eq.3)then                                                     !/*xrg       246*/
         do 150 ij=1,10                                                         !/*xrg       247*/
            if(hd(it:iu).eq.hk(ij)) then                                        !/*xrg       248*/
               if(ij.ge.9) then                                                 !/*xrg       249*/
                  nj=nj+1                                                       !/*xrg       250*/
                  go to 152                                                     !/*xrg       251*/
               else                                                             !/*xrg       252*/
                  ni=ni+1                                                       !/*xrg       253*/
                  ngv(nmesh,ni)=ij                                              !/*xrg       254*/
                  go to 152                                                     !/*xrg       255*/
               endif                                                            !/*xrg       256*/
            endif                                                               !/*xrg       257*/
  150       continue                                                            !/*xrg       258*/
         hf=hd(it:iu)                                                           !/*xrg       259*/
         call erprnt(1,1,0,0,0,0,0,0,'55h illegal keyword:  '//hf)              !/*xrg       260*/
         go to 5                                                                !/*xrg       261*/
!                                                                               !/*xrg       262*/
  152    it=iu+1                                                                !/*xrg       263*/
         do 155 ik=it,80                                                        !/*xrg       264*/
            n=index('abcdefghijklmnopqrstuvwxyz',hd(ik:ik))                     !/*xrg       265*/
            m=index('0123456789',hd(ik:ik))                                     !/*xrg       266*/
            if(n.ne.0) then                                                     !/*xrg       267*/
               it=ik                                                            !/*xrg       268*/
               go to 90                                                         !/*xrg       269*/
            endif                                                               !/*xrg       270*/
            if(m.ne.0) then                                                     !/*xrg       271*/
               if(ij.ge.9)go to 155                                             !/*xrg       272*/
               call erprnt(1,1,0,0,0,0,0,0,                                     !/*xrg       273*/
     &         '44hKeyword '//hy(ij)//' cannot have numerical entries.')        !/*xrg       274*/
               go to 90                                                         !/*xrg       275*/
            endif                                                               !/*xrg       276*/
  155       continue                                                            !/*xrg       277*/
         go to 10                                                               !/*xrg       278*/
!                                                                               !/*xrg       279*/
      else                                                                      !/*xrg       280*/
         do 160 ij=9,10                                                         !/*xrg       281*/
            if(hd(it:iu).eq.hy(ij)) then                                        !/*xrg       282*/
               nj=nj+1                                                          !/*xrg       283*/
               go to 170                                                        !/*xrg       284*/
            endif                                                               !/*xrg       285*/
  160       continue                                                            !/*xrg       286*/
         hf=hd(it:iu)                                                           !/*xrg       287*/
         call erprnt(1,1,0,0,0,0,0,0,'55h illegal keyword:  '//hf)              !/*xrg       288*/
         go to 5                                                                !/*xrg       289*/
      endif                                                                     !/*xrg       290*/
!                                                                               !/*xrg       291*/
  170 it=iu+1                                                                   !/*xrg       292*/
      do 180 ik=it,80                                                           !/*xrg       293*/
         n=index('abcdefghijklmnopqrstuvwxyz',hd(ik:ik))                        !/*xrg       294*/
         if(n.ne.0) then                                                        !/*xrg       295*/
            it=ik                                                               !/*xrg       296*/
            go to 90                                                            !/*xrg       297*/
         endif                                                                  !/*xrg       298*/
  180    continue                                                               !/*xrg       299*/
      go to 10                                                                  !/*xrg       300*/
!                                                                               !/*xrg       301*/
!       Do the rapup at end of data on this card.                               !/*xrg       302*/
  190 if(ni.gt.0) then                                                          !/*xrg       303*/
         mxgv(nmesh)=ni                                                         !/*xrg       304*/
      else                                                                      !/*xrg       305*/
         mxgv(nmesh)=1                                                          !/*xrg       306*/
         nt=mod(nugd(nmesh),10)                                                 !/*xrg       307*/
         if(nt.le.3) then                                                       !/*xrg       308*/
            ngv(nmesh,1)=1                                                      !/*xrg       309*/
         endif                                                                  !/*xrg       310*/
      endif                                                                     !/*xrg       311*/
      ni=0                                                                      !/*xrg       312*/
      nj=0                                                                      !/*xrg       313*/
      nc=0                                                                      !/*xrg       314*/
      nd=0                                                                      !/*xrg       315*/
      go to 20                                                                  !/*xrg       316*/
!                                                                               !/*xrg       317*/
!       << Coordinate cards. >>          cora,corb,corc,ergsh                   !/*xrg       318*/
  200 if(nd.ne.0)go to 300                                                      !/*xrg       319*/
      ic=nc-3                                                                   !/*xrg       320*/
      if(ni.gt.0)go to 240                                                      !/*xrg       321*/
      i1=0                                                                      !/*xrg       322*/
      kx=0                                                                      !/*xrg       323*/
      do 210 ik=it,80                                                           !/*xrg       324*/
         n=index('0123456789',hd(ik:ik))                                        !/*xrg       325*/
         if(n.ne.0) then                                                        !/*xrg       326*/
            kx=kx*10+n-1                                                        !/*xrg       327*/
         else if (n.eq.0.and.kx.gt.0) then                                      !/*xrg       328*/
            it=ik                                                               !/*xrg       329*/
            iu=it                                                               !/*xrg       330*/
            go to 220                                                           !/*xrg       331*/
         else                                                                   !/*xrg       332*/
            call erprnt(1,1,0,0,0,0,0,0,                                        !/*xrg       333*/
     &       '38h This control must have a mesh number.')                       !/*xrg       334*/
         go to 5                                                                !/*xrg       335*/
         endif                                                                  !/*xrg       336*/
  210    continue                                                               !/*xrg       337*/
!                                                                               !/*xrg       338*/
  220 do 230 i=1,nmesh                                                          !/*xrg       339*/
         if(kx.eq.nugd(i)) then                                                 !/*xrg       340*/
            mn=i                                                                !/*xrg       341*/
            mt=mstyp(mn)/10                                                     !/*xrg       342*/
            go to 240                                                           !/*xrg       343*/
         endif                                                                  !/*xrg       344*/
  230    continue                                                               !/*xrg       345*/
      call erprnt(1,1,1,kx,0,0,0,0,                                             !/*xrg       346*/
     & '15h Mesh card no. ,i3,19h has not been read.')                          !/*xrg       347*/
      go to 5                                                                   !/*xrg       348*/
!                                                                               !/*xrg       349*/
  240 nk=0                                                                      !/*xrg       350*/
  250 do 260 ik=it,80                                                           !/*xrg       351*/
         if(hd(ik:ik).ne.' ') then                                              !/*xrg       352*/
            it=ik                                                               !/*xrg       353*/
            go to 270                                                           !/*xrg       354*/
         endif                                                                  !/*xrg       355*/
  260    continue                                                               !/*xrg       356*/
      go to 10                                                                  !/*xrg       357*/
!                                                                               !/*xrg       358*/
  270 if(ic.eq.4)go to 310                                                      !/*xrg       359*/
      do 280 ik=it,80                                                           !/*xrg       360*/
         if(hd(ik:ik).eq.' '.or.hd(ik:ik).eq.'i'.or.                            !/*xrga        1*/
     &                          hd(ik:ik).eq.'I') then                          !/*xrga        2*/
            iu=ik-1                                                             !/*xrg       362*/
            k=kdata(hd(it:iu))                                                  !/*xrg       363*/
            hx=' '                                                              !/*xrg       364*/
            hx(25-iu+it:25)=hd(it:iu)                                           !/*xrg       365*/
            if(k.ge.1)read(hx,'(e25.0)')ri                                      !/*xrg       366*/
            if(k.eq.2)read(hx,'(i25)')ir                                        !/*xrg       367*/
            it=ik                                                               !/*xrg       368*/
            go to 290                                                           !/*xrg       369*/
         endif                                                                  !/*xrg       370*/
  280    continue                                                               !/*xrg       371*/
!                                                                               !/*xrg       372*/
  290 if(nk.eq.0) then                                                          !/*xrg       373*/
         if(i1.ne.0) then                                                       !/*xrg       374*/
            i2=ir+1                                                             !/*xrg       375*/
         else                                                                   !/*xrg       376*/
            i1=1                                                                !/*xrg       377*/
            if(mt.eq.1.or.ic.eq.1) then                                         !/*xrg       378*/
               ni=ni+1                                                          !/*xrg       379*/
               go to 250                                                        !/*xrg       380*/
            elseif(mt.eq.2) then                                                !/*xrg       381*/
               if((hd(it:it).eq.'i'.or.hd(it:it).eq.'I').and.                   !/*xrga        3*/
     &                                          ic.eq.3) then                   !/*xrga        4*/
                  nk=1                                                          !/*xrg       383*/
                  i2=ir+1                                                       !/*xrg       384*/
                  it=it+1                                                       !/*xrg       385*/
                  go to 250                                                     !/*xrg       386*/
               else                                                             !/*xrg       387*/
                  ni=ni+1                                                       !/*xrg       388*/
                  go to 250                                                     !/*xrg       389*/
               endif                                                            !/*xrg       390*/
            elseif(mt.eq.3) then                                                !/*xrg       391*/
               if((hd(it:it).eq.'i'.or.hd(it:it).eq.'I').and.                   !/*xrga        5*/
     &                             (ic.eq.2.or.ic.eq.3)) then                   !/*xrga        6*/
                  nk=1                                                          !/*xrg       393*/
                  i2=ir+1                                                       !/*xrg       394*/
                  it=it+1                                                       !/*xrg       395*/
                  go to 250                                                     !/*xrg       396*/
               else                                                             !/*xrg       397*/
                  ni=ni+1                                                       !/*xrg       398*/
                  go to 250                                                     !/*xrg       399*/
               endif                                                            !/*xrg       400*/
            endif                                                               !/*xrg       401*/
         endif                                                                  !/*xrg       402*/
         if(hd(it:it).eq.'i'.or.hd(it:it).eq.'I') then                          !/*xrga        7*/
            nk=1                                                                !/*xrg       404*/
            it=it+1                                                             !/*xrg       405*/
            go to 250                                                           !/*xrg       406*/
         endif                                                                  !/*xrg       407*/
         ni=ni+1                                                                !/*xrg       408*/
         go to 250                                                              !/*xrg       409*/
      else                                                                      !/*xrg       410*/
         ni=ni+i2                                                               !/*xrg       411*/
         go to 240                                                              !/*xrg       412*/
      endif                                                                     !/*xrg       413*/
!                                                                               !/*xrg       414*/
!       Do rapup on corrdinate cards.                                           !/*xrg       415*/
  300 if(ic.eq.1) then                                                          !/*xrg       416*/
         ng1(mn)=ni                                                             !/*xrg       417*/
      elseif (ic.eq.2) then                                                     !/*xrg       418*/
         ng2(mn)=ni                                                             !/*xrg       419*/
      else                                                                      !/*xrg       420*/
         ng3(mn)=ni                                                             !/*xrg       421*/
      endif                                                                     !/*xrg       422*/
      ni=0                                                                      !/*xrg       423*/
      nc=0                                                                      !/*xrg       424*/
      nd=0                                                                      !/*xrg       425*/
      go to 20                                                                  !/*xrg       426*/
!                                                                               !/*xrg       427*/
!       << Energy range card. >>                        ergsh                   !/*xrg       428*/
  310 do 320 ik=it,80                                                           !/*xrg       429*/
         if(hd(ik:ik).eq.' ') then                                              !/*xrg       430*/
            iu=ik-1                                                             !/*xrg       431*/
            hx=' '                                                              !/*xrg       432*/
            hx(25-iu+it:25)=hd(it:iu)                                           !/*xrg       433*/
            read(hx,'(e25.0)')eg                                                !/*xrg       434*/
            ergl(mn)=eg                                                         !/*xrg       435*/
            it=ik                                                               !/*xrg       436*/
            go to 330                                                           !/*xrg       437*/
         endif                                                                  !/*xrg       438*/
  320    continue                                                               !/*xrg       439*/
!                                                                               !/*xrg       440*/
  330 do 340 ik=it,80                                                           !/*xrg       441*/
         if(hd(ik:ik).ne.' ') then                                              !/*xrg       442*/
            it=ik                                                               !/*xrg       443*/
            go to 350                                                           !/*xrg       444*/
         endif                                                                  !/*xrg       445*/
  340    continue                                                               !/*xrg       446*/
!                                                                               !/*xrg       447*/
  350 do 355 ik=it,80                                                           !/*xrg       448*/
         if(hd(ik:ik).eq.' ') then                                              !/*xrg       449*/
            iu=ik-1                                                             !/*xrg       450*/
            hx=' '                                                              !/*xrg       451*/
            hx(25-iu+it:25)=hd(it:iu)                                           !/*xrg       452*/
            read(hx,'(e25.0)')eg                                                !/*xrg       453*/
            ergh(mn)=eg                                                         !/*xrg       454*/
            go to 5                                                             !/*xrg       455*/
         endif                                                                  !/*xrg       456*/
  355    continue                                                               !/*xrg       457*/
!                                                                               !/*xrg       458*/
!       << Mesh tally multiplier function card. >>      mshmf                   !/*xrg       459*/
  400 if(nd.ne.0)go to 490                                                      !/*xrg       460*/
      if(ni.gt.0)go to 420                                                      !/*xrg       461*/
      kx=0                                                                      !/*xrg       462*/
      do 410 ik=it,80                                                           !/*xrg       463*/
         n=index('0123456789',hd(ik:ik))                                        !/*xrg       464*/
         if(n.ne.0) then                                                        !/*xrg       465*/
            kx=kx*10+n-1                                                        !/*xrg       466*/
         else if (n.eq.0.and.kx.gt.0) then                                      !/*xrg       467*/
            it=ik+1                                                             !/*xrg       468*/
            go to 420                                                           !/*xrg       469*/
         else                                                                   !/*xrg       470*/
            call erprnt(1,1,0,0,0,0,0,0,                                        !/*xrg       471*/
     &       '40h This mshmf card does not have a number.')                     !/*xrg       472*/
            go to 5                                                             !/*xrg       473*/
         endif                                                                  !/*xrg       474*/
  410    continue                                                               !/*xrg       475*/
!                                                                               !/*xrg       476*/
  420 do 430 ik=it,80                                                           !/*xrg       477*/
         if(hd(ik:ik).ne.' ') then                                              !/*xrg       478*/
            it=ik                                                               !/*xrg       479*/
            go to 440                                                           !/*xrg       480*/
         endif                                                                  !/*xrg       481*/
  430    continue                                                               !/*xrg       482*/
      go to 10                                                                  !/*xrg       483*/
!                                                                               !/*xrg       484*/
  440 do 450 ik=it,80                                                           !/*xrg       485*/
         if(hd(ik:ik).eq.' ') then                                              !/*xrg       486*/
            ni=ni+1                                                             !/*xrg       487*/
            it=ik                                                               !/*xrg       488*/
            go to 420                                                           !/*xrg       489*/
         endif                                                                  !/*xrg       490*/
  450    continue                                                               !/*xrg       491*/
!                                                                               !/*xrg       492*/
  490 if(mod(ni,2).ne.0) then                                                   !/*xrg       493*/
         call erprnt(1,1,0,0,0,0,0,0,                                           !/*xrg       494*/
     &    '40h Entries on mshmf card must be in pairs.')                        !/*xrg       495*/
         go to 5                                                                !/*xrg       496*/
      endif                                                                     !/*xrg       497*/
      nemsh(kx)=ni/2                                                            !/*xrg       498*/
      ni=0                                                                      !/*xrg       499*/
      nc=0                                                                      !/*xrg       500*/
      nd=0                                                                      !/*xrg       501*/
      go to 20                                                                  !/*xrg       502*/
!                                                                               !/*xrg       503*/
!       Go through the input data and set the all of the parameters             !/*xrg       504*/
!       that you did not set in the first pass.                                 !/*xrg       505*/
  500 go to (510,510,510,700,700,700,5,850)nc                                   !/*xrg       506*/
!                                                                               !/*xrg       507*/
  510 if(nd.ne.0)go to 630                                                      !/*xrg       508*/
      if(ni.gt.0.or.nj.gt.0)go to 530                                           !/*xrg       509*/
      nm=0                                                                      !/*xrg       510*/
      nb=0                                                                      !/*xrg       511*/
      kx=0                                                                      !/*xrg       512*/
      do 520 ik=it,80                                                           !/*xrg       513*/
         n=index('0123456789',hd(ik:ik))                                        !/*xrg       514*/
         if(n.ne.0) then                                                        !/*xrg       515*/
            kx=kx*10+n-1                                                        !/*xrg       516*/
         else if (n.eq.0.and.kx.gt.0) then                                      !/*xrg       517*/
            nmesh=nmesh+1                                                       !/*xrg       518*/
            it=ik+1                                                             !/*xrgl        1*/
            go to 530                                                           !/*xrg       520*/
         endif                                                                  !/*xrg       521*/
  520    continue                                                               !/*xrg       522*/
      go to 5                                                                   !/*xrg       523*/
!                                                                               !/*xrg       524*/
  530 nt=mod(kx,10)                                                             !/*xrg       525*/
      do 540 ik=it,80                                                           !/*xrg       526*/
         if(hd(ik:ik).ne.' ') then                                              !/*xrg       527*/
            it=ik                                                               !/*xrg       528*/
            go to 550                                                           !/*xrg       529*/
         endif                                                                  !/*xrg       530*/
  540    continue                                                               !/*xrg       531*/
      go to 10                                                                  !/*xrg       532*/
!                                                                               !/*xrg       533*/
  550 do 560 ik=it,80                                                           !/*xrg       534*/
         if(hd(ik:ik).eq.' ') then                                              !/*xrg       535*/
            iu=ik-1                                                             !/*xrg       536*/
            go to 570                                                           !/*xrg       537*/
         endif                                                                  !/*xrg       538*/
  560    continue                                                               !/*xrg       539*/
!                                                                               !/*xrg       540*/
  570 if(nt.ne.3) then                                                          !/*xrg       541*/
!     Correction as of 5/25/2000 <<Add>>                                        !/*xrg       542*/
         if (nt.eq.2.and.it.eq.iu) then                                         !/*xrg       543*/
            it=it+1                                                             !/*xrg       544*/
            go to 530                                                           !/*xrg       545*/
         endif                                                                  !/*xrg       546*/
!                                                                               !/*xrg       547*/
         do 580 ik=1,10                                                         !/*xrg       548*/
            if(hd(it:iu).eq.hy(ik)) then                                        !/*xrg       549*/
               kt=ik                                                            !/*xrg       550*/
               ni=ni+1                                                          !/*xrg       551*/
               it=iu+1                                                          !/*xrg       552*/
               go to 590                                                        !/*xrg       553*/
            endif                                                               !/*xrg       554*/
  580       continue                                                            !/*xrg       555*/
         go to 5                                                                !/*xrg       556*/
      else                                                                      !/*xrg       557*/
         do 582 ik=1,10                                                         !/*xrg       558*/
            if(hd(it:iu).eq.hk(ik)) then                                        !/*xrg       559*/
               kt=ik                                                            !/*xrg       560*/
               ni=ni+1                                                          !/*xrg       561*/
               it=iu+1                                                          !/*xrg       562*/
               go to 590                                                        !/*xrg       563*/
            endif                                                               !/*xrg       564*/
  582       continue                                                            !/*xrg       565*/
         go to 5                                                                !/*xrg       566*/
      endif                                                                     !/*xrg       567*/
      go to 5                                                                   !/*xrg       568*/
!                                                                               !/*xrg       569*/
!     Correction as of 5/25/2000 <<Change>>                                     !/*xrg       570*/
  590 if((nt.eq.1.and.kt.le.4).or.(nt.ge.2.and.kt.lt.9))go to 530               !/*xrg       571*/
!                                                                               !/*xrg       572*/
      m1=0                                                                      !/*xrg       573*/
  595 do 600 ik=it,80                                                           !/*xrg       574*/
         if(hd(ik:ik).ne.' ') then                                              !/*xrg       575*/
            n=index('0123456789+-',hd(ik:ik))                                   !/*xrg       576*/
            it=ik                                                               !/*xrg       577*/
            if(n.ne.0) then                                                     !/*xrg       578*/
               m1=m1+1                                                          !/*xrg       579*/
               go to 610                                                        !/*xrg       580*/
            endif                                                               !/*xrg       581*/
            go to 550                                                           !/*xrg       582*/
         endif                                                                  !/*xrg       583*/
  600    continue                                                               !/*xrg       584*/
      go to 10                                                                  !/*xrg       585*/
!                                                                               !/*xrg       586*/
  610 do 620 ik=it,80                                                           !/*xrg       587*/
         if(hd(ik:ik).eq.' ') then                                              !/*xrg       588*/
            iu=ik-1                                                             !/*xrg       589*/
            k=kdata(hd(it:iu))                                                  !/*xrg       590*/
            hx=' '                                                              !/*xrg       591*/
            hx(25-iu+it:25)=hd(it:iu)                                           !/*xrg       592*/
            if(k.ge.1)read(hx,'(e25.0)')ri                                      !/*xrg       593*/
            if(k.eq.2)read(hx,'(i25)')ir                                        !/*xrg       594*/
            if(kt.eq.9.and.m1.eq.2.and.mmult(1,nmesh).eq.-1)go to 615           !/*xrg       595*/
            if(m1.lt.4) then                                                    !/*xrg       596*/
               if(kt.eq.5) then                                                 !/*xrg       597*/
                  if(m1.eq.1)nb=nb+1                                            !/*xrg       598*/
                  mdos(m1,nb,nmesh)=ir                                          !/*xrg       599*/
               else if (kt.eq.9) then                                           !/*xrg       600*/
                  mmult(m1,nmesh)=ir                                            !/*xrg       601*/
               else if (kt.eq.10) then                                          !/*xrg       602*/
                  mshtr(nmesh)=ir                                               !/*xrg       603*/
               endif                                                            !/*xrg       604*/
               go to 618                                                        !/*xrg       605*/
            endif                                                               !/*xrg       606*/
  615       if(kt.eq.5) then                                                    !/*xrg       607*/
               rdos(nb,nmesh)=ri                                                !/*xrg       608*/
            else if (kt.eq.9) then                                              !/*xrg       609*/
               rmult(nmesh)=ri                                                  !/*xrg       610*/
            endif                                                               !/*xrg       611*/
  618       it=ik                                                               !/*xrg       612*/
            go to 595                                                           !/*xrg       613*/
         endif                                                                  !/*xrg       614*/
  620    continue                                                               !/*xrg       615*/
!                                                                               !/*xrg       616*/
  630 ni=0                                                                      !/*xrg       617*/
      nj=0                                                                      !/*xrg       618*/
      nc=0                                                                      !/*xrg       619*/
      nd=0                                                                      !/*xrg       620*/
      go to 20                                                                  !/*xrg       621*/
!                                                                               !/*xrg       622*/
!       << Coordinate cards. >>                cora,corb,corc                   !/*xrg       623*/
  700 if(nd.ne.0)go to 830                                                      !/*xrg       624*/
      ic=nc-3                                                                   !/*xrg       625*/
      if(ni.gt.0)go to 740                                                      !/*xrg       626*/
      i1=0                                                                      !/*xrg       627*/
      kx=0                                                                      !/*xrg       628*/
      do 710 ik=it,80                                                           !/*xrg       629*/
         n=index('0123456789',hd(ik:ik))                                        !/*xrg       630*/
         if(n.ne.0) then                                                        !/*xrg       631*/
            kx=kx*10+n-1                                                        !/*xrg       632*/
         else if (n.eq.0.and.kx.gt.0) then                                      !/*xrg       633*/
            it=ik+1                                                             !/*xrg       634*/
            go to 720                                                           !/*xrg       635*/
         endif                                                                  !/*xrg       636*/
  710    continue                                                               !/*xrg       637*/
      go to 5                                                                   !/*xrg       638*/
!                                                                               !/*xrg       639*/
  720 do 730 i=1,nmesh                                                          !/*xrg       640*/
         if(kx.eq.nugd(i)) then                                                 !/*xrg       641*/
            mn=i                                                                !/*xrg       642*/
            mt=mstyp(mn)/10                                                     !/*xrg       643*/
            go to 740                                                           !/*xrg       644*/
         endif                                                                  !/*xrg       645*/
  730    continue                                                               !/*xrg       646*/
      go to 5                                                                   !/*xrg       647*/
!                                                                               !/*xrg       648*/
  740 nk=0                                                                      !/*xrg       649*/
  750 do 760 ik=it,80                                                           !/*xrg       650*/
         if(hd(ik:ik).ne.' ') then                                              !/*xrg       651*/
            it=ik                                                               !/*xrg       652*/
            go to 770                                                           !/*xrg       653*/
         endif                                                                  !/*xrg       654*/
  760    continue                                                               !/*xrg       655*/
      go to 10                                                                  !/*xrg       656*/
!                                                                               !/*xrg       657*/
  770 do 780 ik=it,80                                                           !/*xrg       658*/
         if(hd(ik:ik).eq.' '.or.hd(ik:ik).eq.'i'.or.                            !/*xrga        8*/
     &                          hd(ik:ik).eq.'I') then                          !/*xrga        9*/
            iu=ik-1                                                             !/*xrg       660*/
            k=kdata(hd(it:iu))                                                  !/*xrg       661*/
            hx=' '                                                              !/*xrg       662*/
            hx(25-iu+it:25)=hd(it:iu)                                           !/*xrg       663*/
            if(k.ge.1)read(hx,'(e25.0)')ri                                      !/*xrg       664*/
            if(k.eq.2)read(hx,'(i25)')ir                                        !/*xrg       665*/
            it=ik                                                               !/*xrg       666*/
            go to 790                                                           !/*xrg       667*/
         endif                                                                  !/*xrg       668*/
  780    continue                                                               !/*xrg       669*/
!                                                                               !/*xrg       670*/
  790 if(nk.eq.0) then                                                          !/*xrg       671*/
         if(i1.ne.0) then                                                       !/*xrg       672*/
            i2=ir+1                                                             !/*xrg       673*/
         else                                                                   !/*xrg       674*/
            i1=1                                                                !/*xrg       675*/
            if(mt.eq.1.or.hd(it:it).ne.'i'.and.hd(it:it).ne.'I') then           !/*xrga       10*/
               ni=ni+1                                                          !/*xrg       677*/
               if(ic.eq.1) then                                                 !/*xrg       678*/
                  gd1(lgd1(mn)+ni)=ri                                           !/*xrg       679*/
               elseif (ic.eq.2) then                                            !/*xrg       680*/
                  gd2(lgd2(mn)+ni)=ri                                           !/*xrg       681*/
               else                                                             !/*xrg       682*/
                  gd3(lgd3(mn)+ni)=ri                                           !/*xrg       683*/
               endif                                                            !/*xrg       684*/
               go to 750                                                        !/*xrg       685*/
            elseif(mt.eq.2.and.ic.eq.3) then                                    !/*xrg       686*/
               nk=1                                                             !/*xrg       687*/
               i2=ir+1                                                          !/*xrg       688*/
               it=it+1                                                          !/*xrg       689*/
               go to 750                                                        !/*xrg       690*/
            elseif(mt.eq.3.and.(ic.eq.2.or.ic.eq.3)) then                       !/*xrg       691*/
               nk=1                                                             !/*xrg       692*/
               i2=ir+1                                                          !/*xrg       693*/
               it=it+1                                                          !/*xrg       694*/
               go to 750                                                        !/*xrg       695*/
           endif                                                                !/*xrg       696*/
         endif                                                                  !/*xrg       697*/
         if(hd(it:it).eq.'i'.or.hd(it:it).eq.'I') then                          !/*xrga       11*/
            nk=1                                                                !/*xrg       699*/
            it=it+1                                                             !/*xrg       700*/
            go to 750                                                           !/*xrg       701*/
         endif                                                                  !/*xrg       702*/
         ni=ni+1                                                                !/*xrg       703*/
         if(ic.eq.1) then                                                       !/*xrg       704*/
            gd1(lgd1(mn)+ni)=ri                                                 !/*xrg       705*/
         else if (ic.eq.2) then                                                 !/*xrg       706*/
            gd2(lgd2(mn)+ni)=ri                                                 !/*xrg       707*/
         else                                                                   !/*xrg       708*/
            gd3(lgd3(mn)+ni)=ri                                                 !/*xrg       709*/
         endif                                                                  !/*xrg       710*/
         go to 750                                                              !/*xrg       711*/
      else                                                                      !/*xrg       712*/
         if(ic.eq.1) then                                                       !/*xrg       713*/
            dx=(ri-gd1(lgd1(mn)+ni))/i2                                         !/*xrg       714*/
            do 820 i=1,i2                                                       !/*xrg       715*/
               gd1(lgd1(mn)+ni+i)=gd1(lgd1(mn)+ni+i-1)+dx                       !/*xrg       716*/
               if(abs(gd1(lgd1(mn)+ni+i)).lt.1e-16) then                        !/*xrg       717*/
                  gd1(lgd1(mn)+ni+i)=zero                                       !/*xrg       718*/
               endif                                                            !/*xrg       719*/
  820          continue                                                         !/*xrg       720*/
            ni=ni+i2                                                            !/*xrg       721*/
         elseif(ic.eq.2) then                                                   !/*xrg       722*/
            if(mt.eq.3.and.ni.eq.0) then                                        !/*xrg       723*/
               dx=(ri-zero)/i2                                                  !/*xrg       724*/
               gd2(lgd2(mn)+1)=dx                                               !/*xrg       725*/
            else                                                                !/*xrg       726*/
               dx=(ri-gd2(lgd2(mn)+ni))/i2                                      !/*xrg       727*/
               gd2(lgd2(mn)+ni+1)=gd2(lgd2(mn)+ni)+dx                           !/*xrg       728*/
            endif                                                               !/*xrg       729*/
            do 822 i=2,i2                                                       !/*xrg       730*/
               gd2(lgd2(mn)+ni+i)=gd2(lgd2(mn)+ni+i-1)+dx                       !/*xrg       731*/
               if(abs(gd2(lgd2(mn)+ni+i)).lt.1e-16) then                        !/*xrg       732*/
                  gd2(lgd2(mn)+ni+i)=zero                                       !/*xrg       733*/
               endif                                                            !/*xrg       734*/
  822          continue                                                         !/*xrg       735*/
            ni=ni+i2                                                            !/*xrg       736*/
         else                                                                   !/*xrg       737*/
            if((mt.eq.2.or.mt.eq.3).and.ni.eq.0) then                           !/*xrg       738*/
               dx=(ri-zero)/i2                                                  !/*xrg       739*/
               gd3(lgd3(mn)+1)=dx                                               !/*xrg       740*/
            else                                                                !/*xrg       741*/
               dx=(ri-gd3(lgd3(mn)+ni))/i2                                      !/*xrg       742*/
               gd3(lgd3(mn)+ni+1)=gd3(lgd3(mn)+ni)+dx                           !/*xrg       743*/
            endif                                                               !/*xrg       744*/
            do 825 i=2,i2                                                       !/*xrg       745*/
               gd3(lgd3(mn)+ni+i)=gd3(lgd3(mn)+ni+i-1)+dx                       !/*xrg       746*/
               if(abs(gd3(lgd3(mn)+ni+i)).lt.1e-16) then                        !/*xrg       747*/
                  gd3(lgd3(mn)+ni+i)=zero                                       !/*xrg       748*/
               endif                                                            !/*xrg       749*/
  825          continue                                                         !/*xrg       750*/
            ni=ni+i2                                                            !/*xrg       751*/
         endif                                                                  !/*xrg       752*/
         go to 740                                                              !/*xrg       753*/
      endif                                                                     !/*xrg       754*/
!                                                                               !/*xrg       755*/
!       Do rapup on coordinate cards.                                           !/*xrg       756*/
  830 ni=0                                                                      !/*xrg       757*/
      nc=0                                                                      !/*xrg       758*/
      nd=0                                                                      !/*xrg       759*/
      go to 20                                                                  !/*xrg       760*/
!                                                                               !/*xrg       761*/
!       << Mesh tally multiplier function card. >>      mshmf                   !/*xrg       762*/
  850 if(nd.ne.0)go to 910                                                      !/*xrg       763*/
      if(ni.gt.0)go to 870                                                      !/*xrg       764*/
      kx=0                                                                      !/*xrg       765*/
      do 860 ik=it,80                                                           !/*xrg       766*/
         n=index('0123456789',hd(ik:ik))                                        !/*xrg       767*/
         if(n.ne.0) then                                                        !/*xrg       768*/
            kx=kx*10+n-1                                                        !/*xrg       769*/
         else if (n.eq.0.and.kx.gt.0) then                                      !/*xrg       770*/
            it=ik                                                               !/*xrg       771*/
            nx=nemsh(kx)                                                        !/*xrg       772*/
            go to 870                                                           !/*xrg       773*/
         endif                                                                  !/*xrg       774*/
  860    continue                                                               !/*xrg       775*/
      go to 5                                                                   !/*xrg       776*/
!                                                                               !/*xrg       777*/
  870 do 880 ik=it,80                                                           !/*xrg       778*/
         if(hd(ik:ik).ne.' ') then                                              !/*xrg       779*/
            it=ik                                                               !/*xrg       780*/
            go to 890                                                           !/*xrg       781*/
         endif                                                                  !/*xrg       782*/
  880    continue                                                               !/*xrg       783*/
      go to 10                                                                  !/*xrg       784*/
!                                                                               !/*xrg       785*/
  890 do 900 ik=it,80                                                           !/*xrg       786*/
         if(hd(ik:ik).eq.' ') then                                              !/*xrg       787*/
            iu=ik-1                                                             !/*xrg       788*/
            hx=' '                                                              !/*xrg       789*/
            hx(25-iu+it:25)=hd(it:iu)                                           !/*xrg       790*/
            read(hx,'(e25.0)')ri                                                !/*xrg       791*/
            ni=ni+1                                                             !/*xrg       792*/
            nk=int((ni+1)/2)                                                    !/*xrg       793*/
            if(mod(ni,2).ne.0) then                                             !/*xrg       794*/
               emsh(lemsh(kx)+nk)=ri                                            !/*xrg       795*/
            else                                                                !/*xrg       796*/
               emsh(lemsh(kx)+nx+nk)=ri                                         !/*xrg       797*/
            endif                                                               !/*xrg       798*/
            it=ik                                                               !/*xrg       799*/
            go to 870                                                           !/*xrg       800*/
         endif                                                                  !/*xrg       801*/
  900    continue                                                               !/*xrg       802*/
!                                                                               !/*xrg       803*/
  910 ni=0                                                                      !/*xrg       804*/
      nc=0                                                                      !/*xrg       805*/
      nd=0                                                                      !/*xrg       806*/
      go to 20                                                                  !/*xrg       807*/
!                                                                               !/*xrg       808*/
!       Error messages for common problems.                                     !/*xrg       809*/
  960 call erprnt(1,1,0,0,0,0,0,0,                                              !/*xrg       810*/
     & '40h The control number seems to be missing.')                           !/*xrg       811*/
      go to 5                                                                   !/*xrg       812*/
!                                                                               !/*xrg       813*/
!       Error messages for common problems.                                     !/*xrg       814*/
  970 call erprnt(1,1,0,0,0,0,0,0,                                              !/*xrg       815*/
     & '45h This continue card seems to be out of place.')                      !/*xrg       816*/
      go to 5                                                                   !/*xrg       817*/
!                                                                               !/*xrg       818*/
  980 call erprnt(1,1,0,0,0,0,0,0,                                              !/*xrg       819*/
     & '30h Incorrect card for mesh data.')                                     !/*xrg       820*/
      go to 5                                                                   !/*xrg       821*/
!                                                                               !/*xrg       822*/
  990 call expire(0,'rgdtal','unexpected EOF on INP.')                          !/*xrg       823*/
      return                                                                    !/*xrg       824*/
!                                                                               !/*xrg       825*/
  999 call expire(0,'rgdtal','inappropriate blank line.')                       !/*xrg       826*/
#endif                                                                          /*!xrg       827*/
      return                                                                    !/*xrg       828*/
      end                                                                       !/*xrg       829*/
