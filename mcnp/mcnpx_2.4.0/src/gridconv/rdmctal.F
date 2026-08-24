!     $Id: rdmctal.F,v 1.9 2002/06/14 17:05:40 jxh Exp $                        !/*rd          1*/
      subroutine rdmctal                                                        !/*rd          2*/
!                                                                               !/*rd          3*/
!       This program is designed to convert a rectangular grid                  !/*rd          4*/
!       tally data output from MCNPX to input files for use                     !/*rd          5*/
!       with one of several graphics packages.                                  !/*rd          6*/
!                                                                               !/*rd          7*/
!       Written by E. C. Snow  11/20/97   (revised: 10/10/2000)                 !/*rd          8*/
!                                                                               !/*rd          9*/
!       The current package selections are:                                     !/*rd         10*/
!          ng=1   IDL <default>                                                 !/*rd         11*/
!          ng=2   PAW (provided that NO_PAW is not set)                         !/*rd         12*/
!          ng=3   Tecplot                                                       !/*rd         13*/
!                                                                               !/*rd         14*/
#include <gridconv/gcdouble.h>                                                  /*!rd         15*/
#include <gridconv/gccom5.h>                                                    /*!rd         16*/
!                                                                               !/*rd         17*/
      dimension gdx(1),gdy(1),gdz(1),gdata(1),pdx(1),pdy(1),                    !/*rd         18*/
     & pdz(1),pdata(1)                                                          !/*rd         19*/
      equivalence (das,gdx,gdy,gdz,gdata,pdx,pdy,pdz,pdata)                     !/*rd         20*/
!                                                                               !/*rd         21*/
      dimension ntl(100),ibt(8),nb(8),jb(8),kb(8),nt(8),                        !/*rd         22*/
     & ip(34),jp(34)                                                            !/*rd         23*/
      character hf*30,hb*80,hn*4,hj*2,hi(8)*1,hk(8)*10,ht*10,cm*80              !/*rd         24*/
      data hi/'f','d','u','s','m','c','e','t'/                                  !/*rd         25*/
      data hk/'Detectors ','Direct    ','Users     ','Segment   ',              !/*rd         26*/
     & 'Multiplier','Cosine    ','Energy    ','Time      '/                     !/*rd         27*/
!                                                                               !/*rd         28*/
!       Open data input file and read header information.                       !/*rd         29*/
!                                                                               !/*rd         30*/
      open(30,file=hfd,form='formatted',status='old')                           !/*rd         31*/
      read(30,'(a80)')hb                                                        !/*rd         32*/
!                                                                               !/*rd         33*/
      read(30,'(a80)')aid                                                       !/*rd         34*/
      read(30,'(a80)')hb                                                        !/*rd         35*/
      if(hb(11:16).eq.' npert') then                                            !/*rd         36*/
         write(6,'(''I can not handle perturbations tallies.'')')               !/*rd         37*/
         go to 900                                                              !/*rd         38*/
      endif                                                                     !/*rd         39*/
      backspace 30                                                              !/*rd         40*/
      read(30,'(a4,i6)')hn,nj                                                   !/*rd         41*/
      ni=nj/16                                                                  !/*rd         42*/
      nk=mod(nj,16)                                                             !/*rd         43*/
      jj=0                                                                      !/*rd         44*/
      do 10 i=1,ni                                                              !/*rd         45*/
      jj=(i-1)*16                                                               !/*rd         46*/
   10 read(30,'(16i5)')(ntl(jj+j),j=1,16)                                       !/*rd         47*/
      jj=ni*16                                                                  !/*rd         48*/
      if(nk.gt.0)read(30,'(16i5)')(ntl(jj+j),j=1,nk)                            !/*rd         49*/
!                                                                               !/*rd         50*/
   20 ng=0                                                                      !/*rd         51*/
      write(6,'(''The following tallies are avaliable.'')')                     !/*rd         52*/
      write(6,'(10i5,//)')(ntl(i),i=1,nj)                                       !/*rd         53*/
      write(6,'(''Enter the number of the one you want.'')')                    !/*rd         54*/
      write(6,'('' <default='',i3,''>: '')')ntl(1)                              !/*rd         55*/
      write(6,'(''#'')')                                                        !/*rd         56*/
      read(5,'(a30)')hf                                                         !/*rd         57*/
      kx=0                                                                      !/*rd         58*/
      do 30 j=1,30                                                              !/*rd         59*/
      n=index('0123456789',hf(j:j))                                             !/*rd         60*/
      if(n.ne.0) then                                                           !/*rd         61*/
         kx=10*kx+n-1                                                           !/*rd         62*/
      elseif(n.eq.0.and.kx.gt.0) then                                           !/*rd         63*/
         ng=kx                                                                  !/*rd         64*/
         go to 40                                                               !/*rd         65*/
      endif                                                                     !/*rd         66*/
   30 continue                                                                  !/*rd         67*/
!                                                                               !/*rd         68*/
   40 if(ng.eq.0) then                                                          !/*rd         69*/
         ng=ntl(1)                                                              !/*rd         70*/
         go to 60                                                               !/*rd         71*/
      endif                                                                     !/*rd         72*/
      do 50 i=1,nj                                                              !/*rd         73*/
      if(ng.eq.ntl(i))go to 60                                                  !/*rd         74*/
   50 continue                                                                  !/*rd         75*/
      write(6,                                                                  !/*rd         76*/
     & '(''Not a tally no. '',i3,'' on this file, try again.'')')ng             !/*rd         77*/
      go to 20                                                                  !/*rd         78*/
!                                                                               !/*rd         79*/
!       Find the tally information requested and read it into                   !/*rd         80*/
!       the gdata array.                                                        !/*rd         81*/
   60 read(30,'(a80)')hb                                                        !/*rd         82*/
      if(hb(1:5).ne.'tally')go to 60                                            !/*rd         83*/
      backspace 30                                                              !/*rd         84*/
      read(30,'(a5,3i5)')hb,mi,il,mt                                            !/*rd         85*/
      if(mi.ne.ng)go to 60                                                      !/*rd         86*/
      read(30,'(34i2)')(ip(i),i=1,34)                                           !/*rd         87*/
      il=0                                                                      !/*rd         88*/
      do 70 i=1,34                                                              !/*rd         89*/
      if(ip(i).eq.1) then                                                       !/*rd         90*/
         il=il+1                                                                !/*rd         91*/
         jp(il)=i                                                               !/*rd         92*/
      endif                                                                     !/*rd         93*/
   70 continue                                                                  !/*rd         94*/
!                                                                               !/*rd         95*/
      if(il.gt.1) then                                                          !/*rd         96*/
         write(6,'(''This is a multiple '',i2,'' particle tally.'')')il         !/*rd         97*/
      else                                                                      !/*rd         98*/
         write(6,'(''This is a '',a10,'' tally.'')')hip(jp(1))                  !/*rd         99*/
      endif                                                                     !/*rd        100*/
!                                                                               !/*rd        101*/
!       Call for dimensions of plot info and free variables                     !/*rd        102*/
!       to be used.  Assume the user knows what kind of                         !/*rd        103*/
!       binning was used, if not read the MCTAL file.                           !/*rd        104*/
      write(6,'(''Do you what 1 or 2 independent variables.'')')                !/*rd        105*/
      write(6,'('' < Default = 1 > '')')                                        !/*rd        106*/
      read(5,'(a30)')hf                                                         !/*rd        107*/
      kx=0                                                                      !/*rd        108*/
      do 80 j=1,30                                                              !/*rd        109*/
      n=index('0123456789',hf(j:j))                                             !/*rd        110*/
      if(n.ne.0) then                                                           !/*rd        111*/
         kx=10*kx+n-1                                                           !/*rd        112*/
      elseif(n.eq.0.and.kx.gt.0) then                                           !/*rd        113*/
         md=kx                                                                  !/*rd        114*/
         go to 90                                                               !/*rd        115*/
      endif                                                                     !/*rd        116*/
   80 continue                                                                  !/*rd        117*/
      md=1                                                                      !/*rd        118*/
!                                                                               !/*rd        119*/
   90 n1=0                                                                      !/*rd        120*/
      n2=0                                                                      !/*rd        121*/
      if(md.eq.1) then                                                          !/*rd        122*/
         write(6,'(''Which tally bin do you what free?'')')                     !/*rd        123*/
         write(6,'('' < Default = energy > '')')                                !/*rd        124*/
         read(5,'(a30)')hf                                                      !/*rd        125*/
         do 100 j=1,30                                                          !/*rd        126*/
         if(hf(j:j).eq.' ')go to 100                                            !/*rd        127*/
         n1=index('fdusmcet',hf(j:j))                                           !/*rd        128*/
         if(n1.eq.0)go to 110                                                   !/*rd        129*/
         go to 140                                                              !/*rd        130*/
  100    continue                                                               !/*rd        131*/
         n1=7                                                                   !/*rd        132*/
         go to 140                                                              !/*rd        133*/
!                                                                               !/*rd        134*/
  110    write(6,'(a1,'' is not a proper bin notation. '')')hf(j:j)             !/*rd        135*/
         go to 900                                                              !/*rd        136*/
      elseif(md.eq.2) then                                                      !/*rd        137*/
         write(6,'(''Which two tally bins do you what free?'')')                !/*rd        138*/
         write(6,'('' < No Default > '')')                                      !/*rd        139*/
         read(5,'(a30)')hf                                                      !/*rd        140*/
         do 120 j=1,30                                                          !/*rd        141*/
         if(hf(j:j).eq.' ')go to 120                                            !/*rd        142*/
         if(n1.eq.0) then                                                       !/*rd        143*/
            n1=index('fdusmcet',hf(j:j))                                        !/*rd        144*/
            if(n1.eq.0)go to 130                                                !/*rd        145*/
            go to 120                                                           !/*rd        146*/
         else                                                                   !/*rd        147*/
            n2=index('fdusmcet',hf(j:j))                                        !/*rd        148*/
            if(n2.eq.0.or.n1.eq.n2)go to 130                                    !/*rd        149*/
            go to 140                                                           !/*rd        150*/
         endif                                                                  !/*rd        151*/
  120    continue                                                               !/*rd        152*/
  130    write(6,'(a1,'' is not a proper bin notation. '')')hf(j:j)             !/*rd        153*/
         go to 900                                                              !/*rd        154*/
      else                                                                      !/*rd        155*/
         write(6,'(''Dimension of plot must be 1 or 2.'')')                     !/*rd        156*/
         go to 900                                                              !/*rd        157*/
      endif                                                                     !/*rd        158*/
!                                                                               !/*rd        159*/
  140 if(md.eq.2.and.n1.gt.n2) then                                             !/*rd        160*/
         n=n1                                                                   !/*rd        161*/
         n1=n2                                                                  !/*rd        162*/
         n2=n                                                                   !/*rd        163*/
      endif                                                                     !/*rd        164*/
!                                                                               !/*rd        165*/
!      See what type of bin information is available.                           !/*rd        166*/
!      nb(i)=  Number of bins in the Ith set.                                   !/*rd        167*/
!                                                                               !/*rd        168*/
      do 150 i=1,8                                                              !/*rd        169*/
      nt(i)=1                                                                   !/*rd        170*/
      nb(i)=0                                                                   !/*rd        171*/
  150 ibt(i)=0                                                                  !/*rd        172*/
!                                                                               !/*rd        173*/
      write(6,'(''Tally comment lines follow, if any.'')')                      !/*rd        174*/
  160 read(30,'(a80)')hb                                                        !/*rd        175*/
      if(hb(1:5).eq.'     ') then                                               !/*rd        176*/
         write(6,'(a80)')hb                                                     !/*rd        177*/
         go to 160                                                              !/*rd        178*/
      endif                                                                     !/*rd        179*/
!                                                                               !/*rd        180*/
      backspace 30                                                              !/*rd        181*/
      kj=0                                                                      !/*rd        182*/
  170 read(30,'(a2,i8)')hj,n                                                    !/*rd        183*/
      do 180 ib=1,8                                                             !/*rd        184*/
      if(hj(1:1).eq.hi(ib))go to 190                                            !/*rd        185*/
  180 continue                                                                  !/*rd        186*/
!                                                                               !/*rd        187*/
  190 ht=hk(ib)                                                                 !/*rd        188*/
      mm=mod(ntl(ib),10)                                                        !/*rd        189*/
      if(ib.eq.1) then                                                          !/*rd        190*/
         if(mm.le.2) ht='Surface   '                                            !/*rd        191*/
         if(mm.eq.4.or.mm.gt.5) ht='Cell      '                                 !/*rd        192*/
      elseif(ib.eq.2) then                                                      !/*rd        193*/
         if(mm.ne.5) ht='Flagged   '                                            !/*rd        194*/
      endif                                                                     !/*rd        195*/
      write(6,'(''Tally contains '',i5,2x,a10,'' bins.'')')n,ht                 !/*rd        196*/
      if(hj(2:2).eq.'t') then                                                   !/*rd        197*/
         ibt(ib)=1                                                              !/*rd        198*/
         write(6,'(''Including a total bin!'')')                                !/*rd        199*/
      endif                                                                     !/*rd        200*/
      kj=kj+1                                                                   !/*rd        201*/
      jb(kj)=ib                                                                 !/*rd        202*/
      nb(ib)=n                                                                  !/*rd        203*/
      if(ib.eq.1.and.mod(mi,10).eq.5)then                                       !/*rd        204*/
         if(kj.lt.8)go to 170                                                   !/*rd        205*/
         go to 225                                                              !/*rd        206*/
      endif                                                                     !/*rd        207*/
      if(n.eq.0.or.ib.eq.2) then                                                !/*rd        208*/
         if(ib.ne.2)nb(ib)=1                                                    !/*rd        209*/
         if(kj.lt.8)go to 170                                                   !/*rd        210*/
         go to 225                                                              !/*rd        211*/
      endif                                                                     !/*rd        212*/
      if(ib.ne.n1.and.ib.ne.n2) then                                            !/*rd        213*/
         if(nb(ib).lt.1) then                                                   !/*rd        214*/
            nb(ib)=1                                                            !/*rd        215*/
            if(kj.ge.8)go to 225                                                !/*rd        216*/
         else                                                                   !/*rd        217*/
            nn=nb(ib)                                                           !/*rd        218*/
            if(ibt(ib).eq.1)nn=nb(ib)-1                                         !/*rd        219*/
            ni=nn/6                                                             !/*rd        220*/
            na=mod(nn,6)                                                        !/*rd        221*/
            if(na.gt.0)ni=ni+1                                                  !/*rd        222*/
            do 200 k=1,ni                                                       !/*rd        223*/
            read(30,'(e13.5)')du                                                !/*rd        224*/
  200       continue                                                            !/*rd        225*/
            if(kj.ge.8)go to 225                                                !/*rd        226*/
         endif                                                                  !/*rd        227*/
      endif                                                                     !/*rd        228*/
!                                                                               !/*rd        229*/
      if(ib.eq.n1) then                                                         !/*rd        230*/
         lgdx=1                                                                 !/*rd        231*/
         ng1(1)=nb(ib)                                                          !/*rd        232*/
         if(ibt(ib).eq.1)ng1(1)=nb(ib)-1                                        !/*rd        233*/
         if(mt.gt.2.and.(n1.eq.4.or.n1.eq.6))ng1(1)=nb(ib)+1                    !/*rd        234*/
         ni=ng1(1)/6                                                            !/*rd        235*/
         na=mod(ng1(1),6)                                                       !/*rd        236*/
         do 210 k=1,ni                                                          !/*rd        237*/
         jj=(k-1)*6                                                             !/*rd        238*/
         read(30,'(1p6e13.5)')(gdx(lgdx+jj+j),j=1,6)                            !/*rd        239*/
  210    continue                                                               !/*rd        240*/
         jj=ni*6                                                                !/*rd        241*/
         if(na.gt.0)read(30,'(1p6e13.5)')(gdx(lgdx+jj+j),j=1,na)                !/*rd        242*/
      elseif(ib.eq.n2) then                                                     !/*rd        243*/
         lgdy=lgdx+ng1(1)                                                       !/*rd        244*/
         ng2(1)=nb(ib)                                                          !/*rd        245*/
         if(ibt(ib).eq.1)ng2(1)=nb(ib)-1                                        !/*rd        246*/
         if(mt.gt.2.and.(n2.eq.4.or.n2.eq.6))ng2(1)=nb(ib)+1                    !/*rd        247*/
         ni=ng2(1)/6                                                            !/*rd        248*/
         na=mod(ng2(1),6)                                                       !/*rd        249*/
         do 220 k=1,ni                                                          !/*rd        250*/
         jj=(k-1)*6                                                             !/*rd        251*/
         read(30,'(1p6e13.5)')(gdy(lgdy+jj+j),j=1,6)                            !/*rd        252*/
  220    continue                                                               !/*rd        253*/
         jj=ni*6                                                                !/*rd        254*/
         if(na.gt.0)read(30,'(1p6e13.5)')(gdy(lgdy+jj+j),j=1,na)                !/*rd        255*/
      endif                                                                     !/*rd        256*/
      if(kj.lt.8)go to 170                                                      !/*rd        257*/
!                                                                               !/*rd        258*/
  225 if(md.eq.1) then                                                          !/*rd        259*/
         lgdt=lgdx+ng1(1)                                                       !/*rd        260*/
      else                                                                      !/*rd        261*/
         lgdy=lgdx+ng1(1)                                                       !/*rd        262*/
         lgdt=lgdy+ng2(1)                                                       !/*rd        263*/
      endif                                                                     !/*rd        264*/
!                                                                               !/*rd        265*/
!          Read in the values of tally and error over the n1 and n2             !/*rd        266*/
!          coordinates for the other bins selected by the user.                 !/*rd        267*/
!                                                                               !/*rd        268*/
!      nt(i)=  Bin number of the i set. (fixed)                                 !/*rd        269*/
!                                                                               !/*rd        270*/
      do 260 ib=1,8                                                             !/*rd        271*/
      if(jb(ib).eq.1) then                                                      !/*rd        272*/
         if(nb(ib).gt.1.and.n1.ne.ib.and.n2.ne.ib) then                         !/*rd        273*/
            write(6,'(''Enter the "F" bin # do you wish to use?'')')            !/*rd        274*/
            write(6,'('' <default=1> '')')                                      !/*rd        275*/
            write(6,'(''#'')')                                                  !/*rd        276*/
            go to 230                                                           !/*rd        277*/
         endif                                                                  !/*rd        278*/
         go to 260                                                              !/*rd        279*/
      elseif(jb(ib).eq.2) then                                                  !/*rd        280*/
         if(nb(ib).gt.1.and.n1.ne.ib.and.n2.ne.ib) then                         !/*rd        281*/
            write(6,'(''Enter the "D" bin # do you wish to use?'')')            !/*rd        282*/
            write(6,'('' <default=1> '')')                                      !/*rd        283*/
            write(6,'(''#'')')                                                  !/*rd        284*/
            go to 230                                                           !/*rd        285*/
         endif                                                                  !/*rd        286*/
         go to 260                                                              !/*rd        287*/
      elseif(jb(ib).eq.3) then                                                  !/*rd        288*/
         if(nb(ib).gt.1.and.n1.ne.ib.and.n2.ne.ib) then                         !/*rd        289*/
            write(6,'(''Enter the "U" bin # do you wish to use?'')')            !/*rd        290*/
            write(6,'('' <default=1> '')')                                      !/*rd        291*/
            write(6,'(''#'')')                                                  !/*rd        292*/
            go to 230                                                           !/*rd        293*/
         endif                                                                  !/*rd        294*/
         go to 260                                                              !/*rd        295*/
      elseif(jb(ib).eq.4) then                                                  !/*rd        296*/
         if(nb(ib).gt.1.and.n1.ne.ib.and.n2.ne.ib) then                         !/*rd        297*/
            write(6,'(''Enter the "S" bin # do you wish to use?'')')            !/*rd        298*/
            write(6,'('' <default=1> '')')                                      !/*rd        299*/
            write(6,'(''#'')')                                                  !/*rd        300*/
            go to 230                                                           !/*rd        301*/
         endif                                                                  !/*rd        302*/
         go to 260                                                              !/*rd        303*/
      elseif(jb(ib).eq.5) then                                                  !/*rd        304*/
         if(nb(ib).gt.1.and.n1.ne.ib.and.n2.ne.ib) then                         !/*rd        305*/
            write(6,'(''Enter the "M" bin # do you wish to use?'')')            !/*rd        306*/
            write(6,'('' <default=1> '')')                                      !/*rd        307*/
            write(6,'(''#'')')                                                  !/*rd        308*/
            go to 230                                                           !/*rd        309*/
         endif                                                                  !/*rd        310*/
         go to 260                                                              !/*rd        311*/
      elseif(jb(ib).eq.6) then                                                  !/*rd        312*/
         if(nb(ib).gt.1.and.n1.ne.ib.and.n2.ne.ib) then                         !/*rd        313*/
            if(ibt(ib).eq.1)nt(ib)=nb(ib)                                       !/*rd        314*/
            write(6,'(''Enter the "C" bin # do you wish to use?'')')            !/*rd        315*/
            if(ibt(ib).eq.0) then                                               !/*rd        316*/
               write(6,'('' <default=1> '')')                                   !/*rd        317*/
            else                                                                !/*rd        318*/
               write(6,'('' <default=total bin> '')')                           !/*rd        319*/
            endif                                                               !/*rd        320*/
            write(6,'(''#'')')                                                  !/*rd        321*/
            go to 230                                                           !/*rd        322*/
         endif                                                                  !/*rd        323*/
         go to 260                                                              !/*rd        324*/
      elseif(jb(ib).eq.7) then                                                  !/*rd        325*/
         if(nb(ib).gt.1.and.n1.ne.ib.and.n2.ne.ib) then                         !/*rd        326*/
            if(ibt(ib).eq.1)nt(ib)=nb(ib)                                       !/*rd        327*/
            write(6,'(''Enter the "E" bin # do you wish to use?'')')            !/*rd        328*/
            if(ibt(ib).eq.0) then                                               !/*rd        329*/
               write(6,'('' <default=1> '')')                                   !/*rd        330*/
            else                                                                !/*rd        331*/
               write(6,'('' <default=total bin> '')')                           !/*rd        332*/
            endif                                                               !/*rd        333*/
            write(6,'(''#'')')                                                  !/*rd        334*/
            go to 230                                                           !/*rd        335*/
         endif                                                                  !/*rd        336*/
         go to 260                                                              !/*rd        337*/
      elseif(jb(ib).eq.8) then                                                  !/*rd        338*/
         if(nb(ib).gt.1.and.n1.ne.ib.and.n2.ne.ib) then                         !/*rd        339*/
            if(ibt(ib).eq.1)nt(ib)=nb(ib)                                       !/*rd        340*/
            write(6,'(''Enter the "T" bin # do you wish to use?'')')            !/*rd        341*/
            if(ibt(ib).eq.0) then                                               !/*rd        342*/
               write(6,'('' <default=1> '')')                                   !/*rd        343*/
            else                                                                !/*rd        344*/
               write(6,'('' <default=total bin> '')')                           !/*rd        345*/
            endif                                                               !/*rd        346*/
            write(6,'(''#'')')                                                  !/*rd        347*/
            go to 230                                                           !/*rd        348*/
         endif                                                                  !/*rd        349*/
         go to 260                                                              !/*rd        350*/
      endif                                                                     !/*rd        351*/
!                                                                               !/*rd        352*/
  230 read(5,'(a30)')hf                                                         !/*rd        353*/
      kx=0                                                                      !/*rd        354*/
      do 240 j=1,30                                                             !/*rd        355*/
      n=index('0123456789',hf(j:j))                                             !/*rd        356*/
      if(n.ne.0) then                                                           !/*rd        357*/
         kx=10*kx+n-1                                                           !/*rd        358*/
      elseif(n.eq.0.and.kx.gt.0) then                                           !/*rd        359*/
         nt(ib)=kx                                                              !/*rd        360*/
      endif                                                                     !/*rd        361*/
  240 continue                                                                  !/*rd        362*/
!                                                                               !/*rd        363*/
  260 continue                                                                  !/*rd        364*/
!                                                                               !/*rd        365*/
!      Read in the data into gdata array!                                       !/*rd        366*/
!                                                                               !/*rd        367*/
      read(30,'(a80)')hb                                                        !/*rd        368*/
      if(hb(1:4).ne.'vals') then                                                !/*rd        369*/
         write(6,'(''Data values are not in the correct place.'')')             !/*rd        370*/
         go to 900                                                              !/*rd        371*/
      endif                                                                     !/*rd        372*/
!                                                                               !/*rd        373*/
      mmx=1                                                                     !/*rd        374*/
      do 280 j=1,8                                                              !/*rd        375*/
  280 mmx=mmx*nb(j)                                                             !/*rd        376*/
!                                                                               !/*rd        377*/
      ni=mmx/4                                                                  !/*rd        378*/
      nr=mod(mmx,4)                                                             !/*rd        379*/
      do 290 ii=1,ni                                                            !/*rd        380*/
      jj=(ii-1)*4                                                               !/*rd        381*/
      read(30,'(4(1pe13.5,0pf7.4))')                                            !/*rd        382*/
     & gdata(lgdt+jj+1),gdata(lgdt+mmx+jj+1),                                   !/*rd        383*/
     & gdata(lgdt+jj+2),gdata(lgdt+mmx+jj+2),                                   !/*rd        384*/
     & gdata(lgdt+jj+3),gdata(lgdt+mmx+jj+3),                                   !/*rd        385*/
     & gdata(lgdt+jj+4),gdata(lgdt+mmx+jj+4)                                    !/*rd        386*/
  290 continue                                                                  !/*rd        387*/
      jj=ni*4                                                                   !/*rd        388*/
      if(nr.eq.1) then                                                          !/*rd        389*/
         read(30,'(1pe13.5,0pf7.4)')                                            !/*rd        390*/
     &    gdata(lgdt+jj+1),gdata(lgdt+mmx+jj+1)                                 !/*rd        391*/
      elseif(nr.eq.2) then                                                      !/*rd        392*/
         read(30,'(2(1pe13.5,0pf7.4))')                                         !/*rd        393*/
     &    gdata(lgdt+jj+1),gdata(lgdt+mmx+jj+1),                                !/*rd        394*/
     &    gdata(lgdt+jj+2),gdata(lgdt+mmx+jj+2)                                 !/*rd        395*/
      elseif(nr.eq.3) then                                                      !/*rd        396*/
         read(30,'(3(1pe13.5,0pf7.4))')                                         !/*rd        397*/
     &    gdata(lgdt+jj+1),gdata(lgdt+mmx+jj+1),                                !/*rd        398*/
     &    gdata(lgdt+jj+2),gdata(lgdt+mmx+jj+2),                                !/*rd        399*/
     &    gdata(lgdt+jj+3),gdata(lgdt+mmx+jj+3)                                 !/*rd        400*/
      endif                                                                     !/*rd        401*/
!                                                                               !/*rd        402*/
!       Set up the offsets for the output pdata array.                          !/*rd        403*/
      lpdx=lgdt+2*mmx                                                           !/*rd        404*/
      if(md.eq.1) then                                                          !/*rd        405*/
         lpdt=lpdx+ng1(1)                                                       !/*rd        406*/
      else                                                                      !/*rd        407*/
         lpdy=lpdx+ng1(1)                                                       !/*rd        408*/
         lpdt=lpdy+ng2(1)                                                       !/*rd        409*/
      endif                                                                     !/*rd        410*/
!                                                                               !/*rd        411*/
!        Transferr to 1 or 2 dimensional output requirements.                   !/*rd        412*/
      go to (300,600)md                                                         !/*rd        413*/
!                                                                               !/*rd        414*/
!       Generate input file for one(1) dimensional plots.                       !/*rd        415*/
!                                                                               !/*rd        416*/
!       First determine if the free variable is regular or irregular.           !/*rd        417*/
  300 nnb1=nb(n1)                                                               !/*rd        418*/
      if(ibt(n1).eq.1)nnb1=nnb1-1                                               !/*rd        419*/
!                                                                               !/*rd        420*/
      dx1=abs(gdx(lgdx+2)-gdx(lgdx+1))                                          !/*rd        421*/
      do 310 i=3,nnb1                                                           !/*rd        422*/
      dx2=abs(gdx(lgdx+i)-gdx(lgdx+i-1))                                        !/*rd        423*/
      dx3=abs(dx1-dx2)                                                          !/*rd        424*/
      if(dx3.gt.0.0001*dx1)go to 320                                            !/*rd        425*/
  310 continue                                                                  !/*rd        426*/
      ixs=1                                                                     !/*rd        427*/
      go to 330                                                                 !/*rd        428*/
!                                                                               !/*rd        429*/
  320 ixs=2                                                                     !/*rd        430*/
!                                                                               !/*rd        431*/
  330 jj=1                                                                      !/*rd        432*/
      do 340 i=jj,nnb1                                                          !/*rd        434*/
      if(mt.gt.2) then                                                          !/*rd        435*/
         pdx(lpdx+i)=(gdx(lgdx+i)+gdx(lgdx+i+1))/2.0                            !/*rdxk        1*/
      else                                                                      !/*rd        437*/
         pdx(lpdx+i)=gdx(lgdx+i)                                                !/*rd        438*/
      endif                                                                     !/*rd        439*/
  340 continue                                                                  !/*rd        440*/
!                                                                               !/*rd        441*/
!                                                                               !/*rd        442*/
      write(6,'(''Spacings in the one dimension.'')')                           !/*rd        443*/
      write(6,'(a1,'' is '',a9)')hi(n1),hdb(ixs)                                !/*rdxk        2*/
!                                                                               !/*rd        445*/
!       Get the required values out of the gdata array and                      !/*rd        446*/
!       store them in the pdata array to write to an input                      !/*rd        447*/
!       file for the graphics package requested.                                !/*rd        448*/
!                                                                               !/*rd        449*/
      kb(8)=1                                                                   !/*rd        450*/
      do 350 i=1,7                                                              !/*rd        451*/
      j=8-i                                                                     !/*rd        452*/
      kb(j)=kb(j+1)*nb(j+1)                                                     !/*rd        453*/
  350 continue                                                                  !/*rd        454*/
!                                                                               !/*rd        455*/
      nx=1                                                                      !/*rdxk        3*/
      do 360 k=1,8                                                              !/*rdxk        4*/
  360 nx=nx+(nt(k)-1)*kb(k)                                                     !/*rd        458*/
!                                                                               !/*rd        462*/
      do 370 i=1,nnb1                                                           !/*rd        463*/
      l=(i-1)*kb(n1)                                                            !/*rd        464*/
      mx=nx+l                                                                   !/*rd        466*/
!                                                                               !/*rd        467*/
      pdata(lpdt+i)=gdata(lgdt+mx)                                              !/*rd        468*/
      pdata(lpdt+nb(n1)+i)=gdata(lgdt+mmx+mx)                                   !/*rd        469*/
  370 continue                                                                  !/*rd        470*/
!                                                                               !/*rd        471*/
!       Find out what graphics package is to be used                            !/*rd        472*/
!       and set the default to IDL. (ng=1)                                      !/*rd        473*/
!                                                                               !/*rd        474*/
      write(6,'(''Enter number for graphics package to be used.'')')            !/*rd        475*/
  380 write(6,'(''Your choices are as follows:'')')                             !/*rd        476*/
      write(6,'(''   1.    IDL <default>'')')                                   !/*rd        477*/
      write(6,'(''   2.    Tecplot'')')                                         !/*rd        478*/
#ifndef NO_PAW                                                                  /*!rd        479*/
      write(6,'('' PAW is not been programed for 1D plots.'')')                 !/*rd        480*/
#endif                                                                          /*!rd        481*/
      write(6,'(''#'')')                                                        !/*rd        482*/
      read(5,'(a30)')hfp                                                        !/*rd        483*/
      kx=0                                                                      !/*rd        484*/
      ng=0                                                                      !/*rd        485*/
      do 390 j=1,30                                                             !/*rd        486*/
      n=index('0123456789',hfp(j:j))                                            !/*rd        487*/
      if(n.ne.0) then                                                           !/*rd        488*/
         kx=kx*10+n-1                                                           !/*rd        489*/
      elseif(n.eq.0.and.kx.gt.0) then                                           !/*rd        490*/
         ng=kx                                                                  !/*rd        491*/
         go to 400                                                              !/*rd        492*/
      elseif(n.eq.0.and.j.eq.30) then                                           !/*rd        493*/
         ng=1                                                                   !/*rd        494*/
         go to 400                                                              !/*rd        495*/
      endif                                                                     !/*rd        496*/
  390 continue                                                                  !/*rd        497*/
!                                                                               !/*rd        498*/
  400 if(ng.gt.3) then                                                          !/*rd        499*/
         write(6,'(''This graphics package number is unacceptable.'')')         !/*rd        500*/
         write(6,'(''Please try again.'')')                                     !/*rd        501*/
         go to 380                                                              !/*rd        502*/
      endif                                                                     !/*rd        503*/
!                                                                               !/*rd        504*/
      go to (410,420)ng                                                         !/*rd        505*/
!                                                                               !/*rd        506*/
  410 write(6,'(''What IDL filename do you want.'')')                           !/*rd        507*/
      write(6,'(''%'')')                                                        !/*rd        508*/
      read(5,'(a30)')hfp                                                        !/*rd        509*/
      j=leng(hfp)                                                               !/*rd        510*/
      if(j.eq.0) then                                                           !/*rd        511*/
         j=8                                                                    !/*rd        512*/
         hfp(1:8)='data.idl'                                                    !/*rd        513*/
         write(6,'(''Using default output file name: data.idl'')')              !/*rd        514*/
      endif                                                                     !/*rd        515*/
!                                                                               !/*rd        516*/
      call unique(hfp,6)                                                        !/*rd        517*/
      open(31,file=hfp,form='formatted',status='new')                           !/*rd        518*/
         write(31,'(a80)')aid                                                   !/*rd        519*/
      if(il.eq.1) then                                                          !/*rd        520*/
         write(31,                                                              !/*rd        521*/
     &  '(''This file contains one(1) dimensional '',a10,''data.'')')           !/*rd        522*/
     &    hip(ip(1))                                                            !/*rd        523*/
      else                                                                      !/*rd        524*/
         write(31,                                                              !/*rd        525*/
     &  '(''This file contains 1D data for the '',i2,'' particles.'')')         !/*rd        526*/
     &   il                                                                     !/*rd        527*/
         write(31,'(1x,6a10)')(hip(jp(i)),i=1,il)                               !/*rd        528*/
      endif                                                                     !/*rd        529*/
         write(31,                                                              !/*rd        530*/
     &  '(''There are three(3) data arrays that follow this header.'')')        !/*rd        531*/
         write(31,'(''The 1st array contains the values of '','//               !/*rd        532*/
     &    'a10,''('',i3,''). <format=1x,g12.6>'')')hk(n1),nnb1                  !/*rd        533*/
         write(31,'(''The 2nd array contains '',i3,'' tally values.'//          !/*rd        534*/
     &    ' <format=1x,g12.6>'')')nnb1                                          !/*rd        535*/
         write(31,'(''The 3rd array contains '',i3,'' error values.'//          !/*rd        536*/
     &    ' <format=1x,g12.6>'')')nnb1                                          !/*rd        537*/
         write(31,'(''***** End of header data. ***** '')')                     !/*rd        538*/
         write(31,'(10000(1x,g12.6))')(pdx(lpdx+i),i=1,nnb1)                    !/*rd        539*/
         write(31,'(10000(1x,g12.6))')(pdata(lpdt+i),i=1,nnb1)                  !/*rd        540*/
         write(31,'(10000(1x,g12.6))')(pdata(lpdt+nb(n1)+i),i=1,nnb1)           !/*rd        541*/
         close(31)                                                              !/*rd        542*/
         go to 860                                                              !/*rd        543*/
!                                                                               !/*rd        544*/
  420 write(6,'(''What Tecplot filename do you want.'')')                       !/*rd        545*/
      write(6,'(''$'')')                                                        !/*rd        546*/
      read(5,'(a30)')hfp                                                        !/*rd        547*/
      j=leng(hfp)                                                               !/*rd        548*/
      if(j.eq.0)then                                                            !/*rd        549*/
         j=8                                                                    !/*rd        550*/
         hfp(1:8)='data.tek'                                                    !/*rd        551*/
         write(6,'(''Using default output file name: data.tek'')')              !/*rd        552*/
      endif                                                                     !/*rd        553*/
!                                                                               !/*rd        554*/
      call unique(hfp,6)                                                        !/*rd        555*/
      open(31,file=hfp,form='formatted',status='new')                           !/*rd        556*/
!                                                                               !/*rd        557*/
!       Ask user what comments and title are to be written to this file.        !/*rd        558*/
!                                                                               !/*rd        559*/
      write(31,'(1h#,a80)')aid                                                  !/*rd        560*/
      write(6,'(''Do you want comment lines on this file. (y/n)'')')            !/*rd        561*/
      write(6,'(''  <default = n or CR = 1st line of INP file.> '')')           !/*rd        562*/
      write(6,'(''$'')')                                                        !/*rd        563*/
      read(5,'(a3)')hn                                                          !/*rd        564*/
      if(hn(1:1).eq.'y'.or.hn(1:3).eq.'yes') then                               !/*rd        565*/
  430    write(6,'(''Enter up to 80 characters:'')')                            !/*rd        566*/
         write(6,'(''$'')')                                                     !/*rd        567*/
         read(5,'(a80)')cm                                                      !/*rd        568*/
         write(31,'(1h#,a80)')cm                                                !/*rd        569*/
         write(6,'(''Do you want to add another line: (y/n)'')')                !/*rd        570*/
         write(6,'(''  <default = n or CR > '')')                               !/*rd        571*/
         write(6,'(''$'')')                                                     !/*rd        572*/
         read(5,'(a3)')hn                                                       !/*rd        573*/
         if(hn(1:1).eq.'y'.or.hn(1:3).eq.'yes')go to 430                        !/*rd        574*/
      endif                                                                     !/*rd        575*/
!                                                                               !/*rd        576*/
      write(6,'(''Enter the Title of your plot, up to 50 chars.:'')')           !/*rd        577*/
      write(6,                                                                  !/*rd        578*/
     & '(''  <default = n or CR = 1-50 char. of title on INP>'')')              !/*rd        579*/
      write(6,'(''$'')')                                                        !/*rd        580*/
      read(5,'(a80)')cm                                                         !/*rd        581*/
      do 440 i=1,50                                                             !/*rd        582*/
  440 if(cm(i:i).ne.' ')go to 830                                               !/*rd        583*/
      write(31,'(8h Title='',a50,1h'')')aid                                     !/*rd        584*/
      write(31,                                                                 !/*rd        585*/
     & '(14h Varaibles = '',a10,4h'', '',a10,10h'', ''error'')')                !/*rd        586*/
     &  hk(n1),hdv(2)                                                           !/*rd        587*/
      write(31,'('' zone i='',i3,1h,,'' f=point'')')nb(n1)                      !/*rdxk        5*/
      do 470 ix=1,nnb1                                                          !/*rd        589*/
      write(31,'(3(1x,g13.6))')pdx(lpdx+ix),                                    !/*rd        590*/
     & pdata(lpdt+ix),pdata(lpdt+nb(n1)+ix)                                     !/*rd        591*/
  470 continue                                                                  !/*rd        592*/
      close(31)                                                                 !/*rd        593*/
      go to 860                                                                 !/*rd        594*/
!                                                                               !/*rd        595*/
!       Generate input files for two(2) dimensional plots.                      !/*rd        596*/
!                                                                               !/*rd        597*/
!       First determine if the grid is regular or irregular.                    !/*rd        598*/
  600 nnb1=nb(n1)                                                               !/*rd        599*/
      if(ibt(n1).eq.1)nnb1=nb(n1)-1                                             !/*rd        600*/
      nnb2=nb(n2)                                                               !/*rd        601*/
      if(ibt(n2).eq.1)nnb2=nb(n2)-1                                             !/*rd        602*/
!                                                                               !/*rd        603*/
      dx1=abs(gdx(lgdx+2)-gdx(lgdx+1))                                          !/*rd        604*/
      do 610 i=3,nnb1                                                           !/*rd        605*/
      dx2=abs(gdx(lgdx+i)-gdx(lgdx+i-1))                                        !/*rd        606*/
      dx3=abs(dx1-dx2)                                                          !/*rd        607*/
      if(dx3.gt.0.0001*dx1)go to 620                                            !/*rd        608*/
  610 continue                                                                  !/*rd        609*/
      ixs=1                                                                     !/*rd        610*/
      go to 630                                                                 !/*rd        611*/
!                                                                               !/*rd        612*/
  620 ixs=2                                                                     !/*rd        613*/
!                                                                               !/*rd        614*/
  630 do 640 i=1,nnb1                                                           !/*rd        615*/
      if(mt.gt.2) then                                                          !/*rd        616*/
         pdx(lpdx+i)=(gdx(lgdx+i)+gdx(lgdx+i+1))/2.0                            !/*rd        617*/
      else                                                                      !/*rd        618*/
         pdx(lpdx+i)=gdx(lgdx+i)                                                !/*rd        619*/
      endif                                                                     !/*rd        620*/
  640 continue                                                                  !/*rd        621*/
!                                                                               !/*rd        622*/
      dy1=abs(gdy(lgdy+2)-gdx(lgdy+1))                                          !/*rd        623*/
      do 650 i=3,nnb2                                                           !/*rd        624*/
      dy2=abs(gdy(lgdy+i)-gdx(lgdy+i-1))                                        !/*rd        625*/
      dy3=abs(dy1-dy2)                                                          !/*rd        626*/
      if(dy3.gt.0.0001*dy1)go to 660                                            !/*rd        627*/
  650 continue                                                                  !/*rd        628*/
      iys=1                                                                     !/*rd        629*/
      go to 670                                                                 !/*rd        630*/
!                                                                               !/*rd        631*/
  660 iys=2                                                                     !/*rd        632*/
!                                                                               !/*rd        633*/
  670 do 680 i=1,nnb2                                                           !/*rd        634*/
      if(mt.gt.2) then                                                          !/*rd        635*/
         pdy(lpdy+i)=(gdy(lgdy+i)+gdy(lgdy+i+1))/2.0                            !/*rd        636*/
      else                                                                      !/*rd        637*/
         pdy(lpdy+i)=gdy(lgdy+i)                                                !/*rd        638*/
      endif                                                                     !/*rd        639*/
  680 continue                                                                  !/*rd        640*/
!                                                                               !/*rd        641*/
      write(6,'(''Spacings in the two dimensions.'')')                          !/*rd        642*/
      write(6,'(a1,'' is '',a9)')hi(n1),hdb(ixs)                                !/*rd        643*/
      write(6,'(a1,'' is '',a9)')hi(n2),hdb(iys)                                !/*rd        644*/
!                                                                               !/*rd        645*/
!       Get the required values out of the gdata array and                      !/*rd        646*/
!       store them in the pdata array to write to an input                      !/*rd        647*/
!       file for the graphics package requested.                                !/*rd        648*/
      kb(7)=nb(8)                                                               !/*rd        649*/
      do 690 i=1,6                                                              !/*rd        650*/
      j=7-i                                                                     !/*rd        651*/
      kb(j)=kb(j+1)*nb(j+1)                                                     !/*rd        652*/
  690 continue                                                                  !/*rd        653*/
!                                                                               !/*rd        654*/
      mmz=nnb1*nnb2                                                             !/*rd        655*/
      nx=1                                                                      !/*rd        656*/
      do 700 k=1,8                                                              !/*rdxk        6*/
  700 nx=nx+(nt(k)-1)*kb(k)                                                     !/*rd        658*/
!                                                                               !/*rd        662*/
      do 710 j=1,nnb2                                                           !/*rd        663*/
      do 710 i=1,nnb1                                                           !/*rd        664*/
      l=(i-1)*kb(n1)                                                            !/*rd        665*/
      m=(j-1)*kb(n2)                                                            !/*rd        666*/
      mx=nx+l+m                                                                 !/*rd        667*/
      mz=(j-1)*nnb1+i                                                           !/*rd        668*/
      pdata(lpdt+mz)=gdata(lgdt+mx)                                             !/*rd        669*/
      pdata(lpdt+mmz+mz)=gdata(lgdt+mmx+mx)                                     !/*rd        670*/
  710 continue                                                                  !/*rd        671*/
!                                                                               !/*rd        672*/
!       Find out what graphics package is to be used                            !/*rd        673*/
!       and set the default to IDL. (ng=1)                                      !/*rd        674*/
!                                                                               !/*rd        675*/
      write(6,'(''Enter number for graphics package to be used.'')')            !/*rd        676*/
  720 write(6,'(''Your choices are as follows:'')')                             !/*rd        677*/
      write(6,'(''   1.    IDL <default>'')')                                   !/*rd        678*/
#ifndef NO_PAW                                                                  /*!rd        679*/
      write(6,'(''   2.    PAW'')')                                             !/*rd        680*/
#endif                                                                          /*!rd        681*/
      write(6,'(''   3.    Tecplot'')')                                         !/*rd        682*/
      write(6,'(''#'')')                                                        !/*rd        683*/
      read(5,'(a30)')hfp                                                        !/*rd        684*/
      kx=0                                                                      !/*rd        685*/
      ng=0                                                                      !/*rd        686*/
      do 730 j=1,30                                                             !/*rd        687*/
      n=index('0123456789',hfp(j:j))                                            !/*rd        688*/
      if(n.ne.0) then                                                           !/*rd        689*/
         kx=kx*10+n-1                                                           !/*rd        690*/
      elseif(n.eq.0.and.kx.gt.0) then                                           !/*rd        691*/
         ng=kx                                                                  !/*rd        692*/
         go to 740                                                              !/*rd        693*/
      elseif(n.eq.0.and.j.eq.30) then                                           !/*rd        694*/
         ng=1                                                                   !/*rd        695*/
         go to 740                                                              !/*rd        696*/
      endif                                                                     !/*rd        697*/
  730 continue                                                                  !/*rd        698*/
!                                                                               !/*rd        699*/
  740 if(ng.gt.3) then                                                          !/*rd        700*/
         write(6,'(''This graphics package number is unacceptable.'')')         !/*rd        701*/
         write(6,'(''Please try again.'')')                                     !/*rd        702*/
         go to 720                                                              !/*rd        703*/
#ifdef NO_PAW                                                                   /*!rd        704*/
      else if (ng.eq.2) then                                                    !/*rd        705*/
         write(6,'(''This graphics package number is unacceptable.'')')         !/*rd        706*/
         write(6,'(''Please try again.'')')                                     !/*rd        707*/
         go to 720                                                              !/*rd        708*/
#endif                                                                          /*!rd        709*/
      endif                                                                     !/*rd        710*/
!                                                                               !/*rd        711*/
      go to (750,780,800)ng                                                     !/*rd        712*/
!                                                                               !/*rd        713*/
  750 write(6,'(''What IDL filename do you want.'')')                           !/*rd        714*/
      write(6,'(''%'')')                                                        !/*rd        715*/
      read(5,'(a30)')hfp                                                        !/*rd        716*/
      j=leng(hfp)                                                               !/*rd        717*/
      if(j.eq.0) then                                                           !/*rd        718*/
         j=8                                                                    !/*rd        719*/
         hfp(1:8)='data.idl'                                                    !/*rd        720*/
         write(6,'(''Using default output file name: data.idl'')')              !/*rd        721*/
      endif                                                                     !/*rd        722*/
!                                                                               !/*rd        723*/
      call unique(hfp,6)                                                        !/*rd        724*/
      open(31,file=hfp,form='formatted',status='new')                           !/*rd        725*/
      write(31,'(a80)')aid                                                      !/*rd        726*/
      if(il.eq.1) then                                                          !/*rd        727*/
         write(31,                                                              !/*rd        728*/
     &  '(''This file contains two(2) dimensional '',a10,''data.'')')           !/*rd        729*/
     &   hip(ip(1))                                                             !/*rd        730*/
      else                                                                      !/*rd        731*/
         write(31,                                                              !/*rd        732*/
     &  '(''This file contains 2D data for the '',i2,'' particles.'')')         !/*rd        733*/
     &   il                                                                     !/*rd        734*/
         write(31,'(1x,6a10)')(hip(jp(i)),i=1,il)                               !/*rd        735*/
      endif                                                                     !/*rd        736*/
         write(31,                                                              !/*rd        737*/
     &  '(''There are four(4) data arrays that follow this header.'')')         !/*rd        738*/
         write(31,'(''The 1st array contains the values of '','//               !/*rd        739*/
     &    'a10,''('',i3,''). <format=1x,g12.6>'')')hk(n1),nnb1                  !/*rd        740*/
         write(31,'(''The 2nd array contains the values of '','//               !/*rd        741*/
     &    'a10,''('',i3,''). <format=1x,g12.6>'')')hk(n2),nnb2                  !/*rd        742*/
         write(31,'(''The 3rd array contains '','//                             !/*rd        743*/
     &    'i3,''x'',i3,'' tally values. <format=1x,g12.6>'')')nnb1,nnb2         !/*rd        744*/
         write(31,'(''The 4th array contains '','//                             !/*rd        745*/
     &    'i3,''x'',i3,'' error values. <format=1x,g12.6>'')')nnb1,nnb2         !/*rd        746*/
         write(31,'(''***** End of header data. ***** '')')                     !/*rd        747*/
         write(31,'(10000(1x,g12.6))')(pdx(lpdx+i),i=1,nnb1)                    !/*rd        748*/
         write(31,'(10000(1x,g12.6))')(pdy(lpdy+i),i=1,nnb2)                    !/*rd        749*/
         do 760 j=1,nnb2                                                        !/*rd        750*/
  760    write(31,'(10000(1x,g12.6))')                                          !/*rd        751*/
     &   (pdata(lpdt+(j-1)*nnb1+i),i=1,nnb1)                                    !/*rd        752*/
         do 770 j=1,nnb2                                                        !/*rd        753*/
  770    write(31,'(10000(1x,g12.6))')                                          !/*rd        754*/
     &   (pdata(lpdt+mmz+(j-1)*nnb1+i),i=1,nnb1)                                !/*rd        755*/
         close(31)                                                              !/*rd        756*/
         go to 860                                                              !/*rd        757*/
!                                                                               !/*rd        758*/
  780 continue                                                                  !/*rd        759*/
#ifndef NO_PAW                                                                  /*!rd        760*/
      write(6,'(''What PAW filename do you want.'')')                           !/*rd        761*/
      write(6,'(''&'')')                                                        !/*rd        762*/
      read(5,'(a30)')hfp                                                        !/*rd        763*/
      j=leng(hfp)                                                               !/*rd        764*/
      if(j.eq.0) then                                                           !/*rd        765*/
         j=8                                                                    !/*rd        766*/
         hfp(1:8)='data.paw'                                                    !/*rd        767*/
         write(6,'(''Using default output file name: data.paw'')')              !/*rd        768*/
      endif                                                                     !/*rd        769*/
!                                                                               !/*rd        770*/
      call unique(hfp,6)                                                        !/*rd        771*/
!                                                                               !/*rd        772*/
      xmin=gdx(lgdx+1)                                                          !/*rd        773*/
      xmax=gdx(lgdx+nnb1)                                                       !/*rd        774*/
      ymin=gdy(lgdy+1)                                                          !/*rd        775*/
      ymax=gdy(lgdy+nnb2)                                                       !/*rd        776*/
!                                                                               !/*rd        777*/
      call hlimit(512000)                                                       !/*rd        778*/
      call hbook2(100,hdx(ngd,nxv),nnb1,sngl(xmin),sngl(xmax),                  !/*rd        779*/
     & nnb2,sngl(ymin),sngl(ymax),0.0)                                          !/*rd        780*/
!       <<<< write pdata and xy to output file here>>>                          !/*rd        781*/
!                                                                               !/*rd        782*/
      do 790 iy=1,nnb2                                                          !/*rd        783*/
      do 790 ix=1,nnb1                                                          !/*rd        784*/
      mx=(iy-1)*nnb1+ix                                                         !/*rd        785*/
      f=pdata(lpdt+mx)                                                          !/*rd        786*/
      x=pdx(lpdx+ix)                                                            !/*rd        787*/
      y=pdy(lpdy+iy)                                                            !/*rd        788*/
      if(x.ge.xmin.and.x.le.xmax.and.y.ge.ymin.and.y.le.ymax) then              !/*rd        789*/
         call hf2(100,sngl(x),sngl(y),sngl(f))                                  !/*rd        790*/
      endif                                                                     !/*rd        791*/
  790 continue                                                                  !/*rd        792*/
!                                                                               !/*rd        793*/
      write(6,'(''Storing final histograms to '',a30)')hfp                      !/*rd        794*/
      open(unit=43,file=hfp,form='unformatted',status='new')                    !/*rd        795*/
      call hstore(0,43)                                                         !/*rd        796*/
      close(43)                                                                 !/*rd        797*/
      go to 860                                                                 !/*rd        798*/
#else                                                                           /*!rd        799*/
      write(6,'(''Logic error!'')')                                             !/*rd        800*/
      stop                                                                      !/*rd        801*/
#endif                                                                          /*!rd        802*/
!                                                                               !/*rd        803*/
  800 write(6,'(''What Tecplot filename do you want.'')')                       !/*rd        804*/
      write(6,'(''$'')')                                                        !/*rd        805*/
      read(5,'(a30)')hfp                                                        !/*rd        806*/
      j=leng(hfp)                                                               !/*rd        807*/
      if(j.eq.0)then                                                            !/*rd        808*/
         j=8                                                                    !/*rd        809*/
         hfp(1:8)='data.tek'                                                    !/*rd        810*/
         write(6,'(''Using default output file name: data.tek'')')              !/*rd        811*/
      endif                                                                     !/*rd        812*/
!                                                                               !/*rd        813*/
      call unique(hfp,6)                                                        !/*rd        814*/
      open(31,file=hfp,form='formatted',status='new')                           !/*rd        815*/
!                                                                               !/*rd        816*/
!       Ask user what comments and title are to be written to this file.        !/*rd        817*/
!                                                                               !/*rd        818*/
      write(31,'(1h#,a80)')aid                                                  !/*rd        819*/
      write(6,'(''Do you want comment lines on this file. (y/n)'')')            !/*rd        820*/
      write(6,'(''  <default = n or CR = 1st line of INP file.> '')')           !/*rd        821*/
      write(6,'(''$'')')                                                        !/*rd        822*/
      read(5,'(a3)')hn                                                          !/*rd        823*/
      if(hn(1:1).eq.'y'.or.hn(1:3).eq.'yes') then                               !/*rd        824*/
  810    write(6,'(''Enter up to 80 characters:'')')                            !/*rd        825*/
         write(6,'(''$'')')                                                     !/*rd        826*/
         read(5,'(a80)')cm                                                      !/*rd        827*/
         write(31,'(1h#,a80)')cm                                                !/*rd        828*/
         write(6,'(''Do you want to add another line: (y/n)'')')                !/*rd        829*/
         write(6,'(''  <default = n or CR > '')')                               !/*rd        830*/
         write(6,'(''$'')')                                                     !/*rd        831*/
         read(5,'(a3)')hn                                                       !/*rd        832*/
         if(hn(1:1).eq.'y'.or.hn(1:3).eq.'yes')go to 810                        !/*rd        833*/
      endif                                                                     !/*rd        834*/
!                                                                               !/*rd        835*/
      write(6,'(''Enter the Title of your plot, up to 50 chars.:'')')           !/*rd        836*/
      write(6,                                                                  !/*rd        837*/
     & '(''  <default = n or CR = 1-50 char. of title on INP>'')')              !/*rd        838*/
      write(6,'(''$'')')                                                        !/*rd        839*/
      read(5,'(a80)')cm                                                         !/*rd        840*/
      do 820 i=1,50                                                             !/*rd        841*/
  820 if(cm(i:i).ne.' ')go to 830                                               !/*rd        842*/
      write(31,'(8h Title='',a50,1h'')')aid                                     !/*rd        843*/
      go to 840                                                                 !/*rd        844*/
!                                                                               !/*rd        845*/
  830 write(31,'(8h Title='',a50,1h'')')cm                                      !/*rd        846*/
!                                                                               !/*rd        847*/
  840 write(31,                                                                 !/*rd        848*/
     & '(14h Varaibles = '',a10,4h'', '',a10,4h'', '',a10,'//                   !/*rd        849*/
     & '10h'', ''error'')')hk(n1),hk(n2),hdv(2)                                 !/*rd        850*/
      write(31,'('' zone i='',i3,1h,,'' j='',i3,1h,,'' f=point'')')             !/*rd        851*/
     & nb(n1),nb(n2)                                                            !/*rd        852*/
      do 850 iy=1,nnb2                                                          !/*rd        853*/
      do 850 ix=1,nnb1                                                          !/*rd        854*/
      mx=(iy-1)*nnb1+ix                                                         !/*rd        855*/
      write(31,'(4(1x,g13.6))')pdx(lpdx+ix),pdy(lpdy+iy),                       !/*rd        856*/
     & pdata(lpdt+mx),pdata(lpdt+mmz+mx)                                        !/*rd        857*/
  850 continue                                                                  !/*rd        858*/
      close(31)                                                                 !/*rd        859*/
!                                                                               !/*rd        860*/
  860 write(6,'(''Do you wish to create another plot file. y/n ?'')')           !/*rd        861*/
      write(6,'(''Type yes(y) or no(n) or hit return to exit.'')')              !/*rd        862*/
      write(6,'(''#'')')                                                        !/*rd        863*/
      read(5,'(a3)')hn                                                          !/*rd        864*/
      if(hn(1:1).eq.'y'.or.hn(1:3).eq.'yes') then                               !/*rd        865*/
        rewind 30                                                               !/*rd        866*/
        go to 20                                                                !/*rd        867*/
      endif                                                                     !/*rd        868*/
!                                                                               !/*rd        869*/
  900 rewind 30                                                                 !/*rd        870*/
      return                                                                    !/*rd        871*/
!                                                                               !/*rd        872*/
      end                                                                       !/*rd        873*/
