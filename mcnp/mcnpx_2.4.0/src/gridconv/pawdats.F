!     $Id: pawdats.F,v 1.6 2002/03/27 18:13:54 gwm Exp $                      !/*pa          1*/
      subroutine pawdats                                                        !/*pa          2*/
#ifndef NO_PAW                                                                  /*!pa          3*/
!       This subroutine is designed to convert input data from a                !/*pa          4*/
!       mesh tally set to a data set compatible with the input                  !/*pa          5*/
!       requirements of the PAW graphics package.                               !/*pa          6*/
!                                                                               !/*pa          7*/
!       Written by E. C. Snow  11/22/98.   (revised: 11/15/2001)                !/*pa          8*/
!                                                                               !/*pa          9*/
#include <gridconv/gcdouble.h>                                                  /*!pa         10*/
#include <gridconv/gccom5.h>                                                    /*!pa         11*/
!                                                                               !/*pa         12*/
      dimension gdr(1),gdp(1),gda(1),gdata(1),pdr(1),pdp(1),                    !/*pa         13*/
     & pda(1),pdata(1)                                                          !/*pa         14*/
      equivalence (das,gdr,gdp,gda,gdata,pdr,pdp,pda,pdata)                     !/*pa         15*/
!                                                                               !/*pa         16*/
      character hj*1,hi*3,hv(3)*1,hf*10,hb*30                                   !/*pa         17*/
      data hv/'R','Z','T'/                                                      !/*pa         18*/
!                                                                               !/*pa         19*/
      integer b(512000)                                                         !/*pa         20*/
      common /mibook/b                                                          !/*pa         21*/
!                                                                               !/*pa         22*/
      write(6,'("What PAW filename do you want.")')                             !/*pa         23*/
      write(6,'("&")')                                                          !/*pa         24*/
      read(5,'(a30)')hfp                                                        !/*pa         25*/
      j=leng(hfp)                                                               !/*pa         26*/
      if(j.eq.0) then                                                           !/*pa         27*/
         j=10                                                                   !/*pa         28*/
         hfp(1:10)='datars.paw'                                                 !/*pa         29*/
         write(6,'("Using default output file name: datars.paw")')              !/*pa         30*/
      endif                                                                     !/*pa         31*/
!                                                                               !/*pa         32*/
   20 call unique(hfp,6)                                                        !/*pa         33*/
!                                                                               !/*pa         34*/
   30 write(6,'("Enter plot dimensionality, 1,2, or 9 to exit")')               !/*pa         35*/
      write(6,'(" < No default! >")')                                           !/*pa         36*/
      write(6,'("&")')                                                          !/*pa         37*/
      read(5,'(a30)')hb                                                         !/*pa         38*/
      kx=0                                                                      !/*pa         39*/
      np=0                                                                      !/*pa         40*/
      do 35 j=1,30                                                              !/*pa         41*/
      n=index('0123456789',hb(j:j))                                             !/*pa         42*/
      if(n.ne.0) then                                                           !/*pa         43*/
         kx=kx*10+n-1                                                           !/*pa         44*/
      elseif(n.eq.0.and.kx.gt.0) then                                           !/*pa         45*/
         np=kx                                                                  !/*pa         46*/
         go to 38                                                               !/*pa         47*/
      elseif(np.eq.0.and.j.eq.30) then                                          !/*pa         48*/
         write(6,'(" There is no default value! ")')                            !/*pa         49*/
         go to 30                                                               !/*pa         50*/
      endif                                                                     !/*pa         51*/
   35 continue                                                                  !/*pa         52*/
!                                                                               !/*pa         53*/
   38 if(np.eq.9)go to 500                                                      !/*pa         54*/
      if(np.ge.1.and.np.le.2)go to 40                                           !/*pa         55*/
      go to 30                                                                  !/*pa         56*/
!                                                                               !/*pa         57*/
   40 n1=ng1(ngd)-1                                                             !/*pa         58*/
      n2=ng2(ngd)                                                               !/*pa         59*/
      n3=ng3(ngd)                                                               !/*pa         60*/
      nt=mod(ngtyp(ngd),10)                                                     !/*pa         61*/
      if(nt.eq.0) then                                                          !/*pa         62*/
         hf=hdx(ngd,nxv)                                                        !/*pa         63*/
      elseif(nt.eq.1) then                                                      !/*pa         64*/
         hf='source pts'                                                        !/*pa         65*/
      elseif(nt.eq.2) then                                                      !/*pa         66*/
         hf='ergdeposit'                                                        !/*pa         67*/
      elseif(nt.eq.3) then                                                      !/*pa         68*/
         hf='PD tracks.'                                                        !/*pa         69*/
      elseif(nt.eq.4) then                                                      !/*pa         70*/
         hf='DX tracks.'                                                        !/*pa         71*/
      endif                                                                     !/*pa         72*/
      go to (100,200)np                                                         !/*pa         73*/
!                                                                               !/*pa         74*/
!       One dinemsional plot data.                                              !/*pa         75*/
!       Get dependent variable and index of other two.                          !/*pa         76*/
  100 nv=0                                                                      !/*pa         77*/
      write(6,'("Plot data as fcn. of r, p, a, or q to exit?")')                !/*pa         78*/
      write(6,'(" < No default! >")')                                           !/*pa         79*/
      write(6,'("&")')                                                          !/*pa         80*/
      read(5,'(a3)')hi                                                          !/*pa         81*/
      if(hi(1:1).eq.'q')go to 500                                               !/*pa         82*/
      if(hi(1:1).eq.'r')nv=1                                                    !/*pa         83*/
      if(hi(1:1).eq.'p')nv=2                                                    !/*pa         84*/
      if(hi(1:1).eq.'a')nv=3                                                    !/*pa         85*/
!                                                                               !/*pa         86*/
      if(nv.eq.1) then                                                          !/*pa         87*/
         if(irs.ne.1) then                                                      !/*pa         88*/
            write(6,'("R spacing must be constant for PAW")')                   !/*pa         89*/
            go to 500                                                           !/*pa         90*/
         endif                                                                  !/*pa         91*/
  110    write(6,'("What indicies do you what for p and a?")')                  !/*pa         92*/
         write(6,'(" <default = 1,1>")')                                        !/*pa         93*/
         write(6,'("&")')                                                       !/*pa         94*/
         read(5,'(a30)')hb                                                      !/*pa         95*/
         do 112 j=1,30                                                          !/*pa         96*/
         n=index('0123456789',hb(j:j))                                          !/*pa         97*/
         if(n.ne.0) go to 113                                                   !/*pa         98*/
  112    continue                                                               !/*pa         99*/
         i2=1                                                                   !/*pa        100*/
         i3=1                                                                   !/*pa        101*/
         go to 118                                                              !/*pa        102*/
!                                                                               !/*pa        103*/
  113    kk=1                                                                   !/*pa        104*/
         do 116 i=1,2                                                           !/*pa        105*/
         kx=0                                                                   !/*pa        106*/
         do 114 j=kk,30                                                         !/*pa        107*/
         n=index('0123456789',hb(j:j))                                          !/*pa        108*/
         if(n.ne.0) then                                                        !/*pa        109*/
            kx=kx*10+n-1                                                        !/*pa        110*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*pa        111*/
            kk=j+1                                                              !/*pa        112*/
            go to 115                                                           !/*pa        113*/
         endif                                                                  !/*pa        114*/
  114    continue                                                               !/*pa        115*/
!                                                                               !/*pa        116*/
  115    if(i.eq.1) then                                                        !/*pa        117*/
            i2=kx                                                               !/*pa        118*/
         else                                                                   !/*pa        119*/
            i3=kx                                                               !/*pa        120*/
         endif                                                                  !/*pa        121*/
  116    continue                                                               !/*pa        122*/
!                                                                               !/*pa        123*/
  118    if(i2.gt.ng2(ngd).or.i3.gt.ng3(ngd)) then                              !/*pa        124*/
            write(6,'("One of these indices is too large!")')                   !/*pa        125*/
            go to 110                                                           !/*pa        126*/
         endif                                                                  !/*pa        127*/
      elseif(nv.eq.2) then                                                      !/*pa        128*/
         if(izs.ne.1) then                                                      !/*pa        129*/
            write(6,'("P spacing must be constant for PAW")')                   !/*pa        130*/
            go to 500                                                           !/*pa        131*/
         endif                                                                  !/*pa        132*/
  120    write(6,'("What indicies do you what for r and a?")')                  !/*pa        133*/
         write(6,'(" <default = 1,1>")')                                        !/*pa        134*/
         write(6,'("&")')                                                       !/*pa        135*/
         read(5,'(a30)')hb                                                      !/*pa        136*/
         do 122 j=1,30                                                          !/*pa        137*/
         n=index('0123456789',hb(j:j))                                          !/*pa        138*/
         if(n.ne.0) go to 123                                                   !/*pa        139*/
  122    continue                                                               !/*pa        140*/
         i1=1                                                                   !/*pa        141*/
         i3=1                                                                   !/*pa        142*/
         go to 128                                                              !/*pa        143*/
!                                                                               !/*pa        144*/
  123    kk=1                                                                   !/*pa        145*/
         do 126 i=1,2                                                           !/*pa        146*/
         kx=0                                                                   !/*pa        147*/
         do 124 j=kk,30                                                         !/*pa        148*/
         n=index('0123456789',hb(j:j))                                          !/*pa        149*/
         if(n.ne.0) then                                                        !/*pa        150*/
            kx=kx*10+n-1                                                        !/*pa        151*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*pa        152*/
            kk=j+1                                                              !/*pa        153*/
            go to 125                                                           !/*pa        154*/
         endif                                                                  !/*pa        155*/
  124    continue                                                               !/*pa        156*/
!                                                                               !/*pa        157*/
  125    if(i.eq.1) then                                                        !/*pa        158*/
            i1=kx                                                               !/*pa        159*/
         else                                                                   !/*pa        160*/
            i3=kx                                                               !/*pa        161*/
         endif                                                                  !/*pa        162*/
  126    continue                                                               !/*pa        163*/
!                                                                               !/*pa        164*/
  128    if(i1.gt.ng1(ngd)-1.or.i3.gt.ng3(ngd)) then                            !/*pa        165*/
            write(6,'("One of these indices is too large!")')                   !/*pa        166*/
            go to 120                                                           !/*pa        167*/
         endif                                                                  !/*pa        168*/
!                                                                               !/*pa        169*/
      elseif(nv.eq.3) then                                                      !/*pa        170*/
  130    write(6,'("What indicies do you what for r and p?")')                  !/*pa        171*/
         write(6,'(" <default = 1,1>")')                                        !/*pa        172*/
         write(6,'("&")')                                                       !/*pa        173*/
         read(5,'(a30)')hb                                                      !/*pa        174*/
         do 132 j=1,30                                                          !/*pa        175*/
         n=index('0123456789',hb(j:j))                                          !/*pa        176*/
         if(n.ne.0) go to 133                                                   !/*pa        177*/
  132    continue                                                               !/*pa        178*/
         i1=1                                                                   !/*pa        179*/
         i2=1                                                                   !/*pa        180*/
         go to 138                                                              !/*pa        181*/
!                                                                               !/*pa        182*/
  133    kk=1                                                                   !/*pa        183*/
         do 136 i=1,2                                                           !/*pa        184*/
         kx=0                                                                   !/*pa        185*/
         do 134 j=kk,30                                                         !/*pa        186*/
         n=index('0123456789',hb(j:j))                                          !/*pa        187*/
         if(n.ne.0) then                                                        !/*pa        188*/
            kx=kx*10+n-1                                                        !/*pa        189*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*pa        190*/
            kk=j+1                                                              !/*pa        191*/
            go to 135                                                           !/*pa        192*/
         endif                                                                  !/*pa        193*/
  134    continue                                                               !/*pa        194*/
!                                                                               !/*pa        195*/
  135    if(i.eq.1) then                                                        !/*pa        196*/
            i1=kx                                                               !/*pa        197*/
         else                                                                   !/*pa        198*/
            i2=kx                                                               !/*pa        199*/
         endif                                                                  !/*pa        200*/
  136    continue                                                               !/*pa        201*/
!                                                                               !/*pa        202*/
  138    if(i1.gt.ng1(ngd)-1.or.i2.gt.ng2(ngd)) then                            !/*pa        203*/
            write(6,'("One of these indices is too large!")')                   !/*pa        204*/
            go to 130                                                           !/*pa        205*/
         endif                                                                  !/*pa        206*/
      else                                                                      !/*pa        207*/
         go to 100                                                              !/*pa        208*/
      endif                                                                     !/*pa        209*/
!                                                                               !/*pa        210*/
!       nv is the number of the coordinate varibile that will                   !/*pa        211*/
!       looped over.   1,2,3 - r, p, a.                                         !/*pa        212*/
!       The other two indices will be fixed.                                    !/*pa        213*/
!                                                                               !/*pa        214*/
      if(nv.eq.1) then                                                          !/*pa        215*/
         cmn=gdr(lgdr+1)                                                        !/*pa        216*/
         cmx=gdr(lgdr+ng1(ngd))                                                 !/*pa        217*/
!                                                                               !/*pa        218*/
         do 140 i1=1,n1                                                         !/*pa        219*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*pa        220*/
         pdata(lpdat+i1)=gdata(lgdat+mx)                                        !/*pa        221*/
         pdata(lpdat+n1+i1)=gdata(lgdat+mxgc(ngd)+mx)                           !/*pa        222*/
  140    continue                                                               !/*pa        223*/
!                                                                               !/*pa        224*/
!       Write data the an unformatted file for input                            !/*pa        225*/
!       into the PAW utility for plotting.                                      !/*pa        226*/
!                                                                               !/*pa        227*/
         call hlimit(512000)                                                    !/*pa        228*/
         call hbook1(10,hf,n1,sngl(cmn),sngl(cmx),0.0)                          !/*pa        229*/
         call hbook1(11,'error',n1,sngl(cmn),sngl(cmx),0.0)                     !/*pa        230*/
         do 145 i=1,n1                                                          !/*pa        231*/
         r=gdr(lgdr+i)                                                          !/*pa        232*/
         f=pdata(lpdat+i)                                                       !/*pa        233*/
         g=pdata(lpdat+n1+i)                                                    !/*pa        234*/
         call hf1(10,sngl(r),sngl(f))                                           !/*pa        235*/
         call hf1(11,sngl(r),sngl(g))                                           !/*pa        236*/
  145    continue                                                               !/*pa        237*/
         go to 180                                                              !/*pa        238*/
!                                                                               !/*pa        239*/
      elseif(nv.eq.2) then                                                      !/*pa        240*/
         cmn=0.                                                                 !/*pa        241*/
         cmx=180.                                                               !/*pa        242*/
!                                                                               !/*pa        243*/
         do 150 i2=1,n2                                                         !/*pa        244*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*pa        245*/
         pdata(lpdat+i2)=gdata(lgdat+mx)                                        !/*pa        246*/
         pdata(lpdat+n2+i2)=gdata(lgdat+mxgc(ngd)+mx)                           !/*pa        247*/
  150    continue                                                               !/*pa        248*/
!                                                                               !/*pa        249*/
!       Write data the an unformatted file for input                            !/*pa        250*/
!       into the PAW utility for plotting.                                      !/*pa        251*/
!                                                                               !/*pa        252*/
         call hlimit(512000)                                                    !/*pa        253*/
         call hbook1(20,hf,n2,sngl(cmn),sngl(cmx),0.0)                          !/*pa        254*/
         call hbook1(21,'error',n2,sngl(cmn),sngl(cmx),0.0)                     !/*pa        255*/
         p=0                                                                    !/*pa        256*/
         f=pdata(lpdat+1)                                                       !/*pa        257*/
         g=pdata(lpdat+n2+1)                                                    !/*pa        258*/
         call hf1(20,sngl(t),sngl(f))                                           !/*pa        259*/
         call hf1(21,sngl(t),sngl(g))                                           !/*pa        260*/
c                                                                               !/*pa        261*/
         do 155 i=2,n2                                                          !/*pa        262*/
         p=gdp(lgdp+i-1)                                                        !/*pa        263*/
         f=pdata(lpdat+i)                                                       !/*pa        264*/
         g=pdata(lpdat+n2+i)                                                    !/*pa        265*/
         call hf1(20,sngl(p),sngl(f))                                           !/*pa        266*/
         call hf1(21,sngl(p),sngl(g))                                           !/*pa        267*/
  155    continue                                                               !/*pa        268*/
         go to 180                                                              !/*pa        269*/
!                                                                               !/*pa        270*/
      elseif(nv.eq.3) then                                                      !/*pa        271*/
         cmn=0.                                                                 !/*pa        272*/
         cmx=360.                                                               !/*pa        273*/
!                                                                               !/*pa        274*/
         do 160 i3=1,n3                                                         !/*pa        275*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*pa        276*/
         pdata(lpdat+i3)=gdata(lgdat+mx)                                        !/*pa        277*/
         pdata(lpdat+n3+i3)=gdata(lgdat+mxgc(ngd)+mx)                           !/*pa        278*/
  160    continue                                                               !/*pa        279*/
!                                                                               !/*pa        280*/
!       Write data the an unformatted file for input                            !/*pa        281*/
!       into the PAW utility for plotting.                                      !/*pa        282*/
!                                                                               !/*pa        283*/
         call hlimit(512000)                                                    !/*pa        284*/
         call hbook1(30,hf,n3,sngl(cmn),sngl(cmx),0.0)                          !/*pa        285*/
         call hbook1(31,'error',n3,sngl(cmn),sngl(cmx),0.0)                     !/*pa        286*/
         a=0                                                                    !/*pa        287*/
         f=pdata(lpdat+1)                                                       !/*pa        288*/
         g=pdata(lpdat+n3+1)                                                    !/*pa        289*/
         call hf1(30,sngl(a),sngl(f))                                           !/*pa        290*/
         call hf1(31,sngl(a),sngl(g))                                           !/*pa        291*/
c                                                                               !/*pa        292*/
         do 190 i=2,n3                                                          !/*pa        293*/
         a=gda(lgda+i-1)                                                        !/*pa        294*/
         f=pdata(lpdat+i)                                                       !/*pa        295*/
         g=pdata(lpdat+n3+i)                                                    !/*pa        296*/
         call hf1(30,sngl(a),sngl(f))                                           !/*pa        297*/
         call hf1(31,sngl(a),sngl(g))                                           !/*pa        298*/
  190    continue                                                               !/*pa        299*/
         go to 180                                                              !/*pa        300*/
      else                                                                      !/*pa        301*/
         go to 100                                                              !/*pa        302*/
      endif                                                                     !/*pa        303*/
!                                                                               !/*pa        304*/
  180 write(6,'("Storing final histograms to ",a30)')hfp                        !/*pa        305*/
      open(unit=43,file=hfp,form='unformatted',status='new')                    !/*pa        306*/
      call hstore(0,43)                                                         !/*pa        307*/
      close(43)                                                                 !/*pa        308*/
      return                                                                    !/*pa        309*/
!                                                                               !/*pa        310*/
!       Two dinemsional plot data.                                              !/*pa        311*/
!       Get dependent variables and index of other one.                         !/*pa        312*/
  200 ni=0                                                                      !/*pa        313*/
      write(6,'("Plot data as fcn. of rp, pa, ar or q to exit ?")')             !/*pa        314*/
      write(6,'(" <default = rp >")')                                           !/*pa        315*/
      write(6,'("&")')                                                          !/*pa        316*/
      read(5,'(a3)')hi                                                          !/*pa        317*/
      if(hi(1:1).eq.'q')go to 500                                               !/*pa        318*/
      if(hi(1:2).eq.'  ')hi(1:2)='rp'                                           !/*pa        319*/
      if(hi(1:2).eq.'rp') then                                                  !/*pa        320*/
         if(irs*izs.ne.1) then                                                  !/*pa        321*/
            write(6,'("R and P spacing must be constant for PAW")')             !/*pa        322*/
            go to 500                                                           !/*pa        323*/
         endif                                                                  !/*pa        324*/
         ni=1                                                                   !/*pa        325*/
  210    write(6,'("What indicies do you what for a?")')                        !/*pa        326*/
         write(6,'(" <default = 1>")')                                          !/*pa        327*/
         write(6,'("&")')                                                       !/*pa        328*/
         read(5,'(a30)')hb                                                      !/*pa        329*/
         kx=0                                                                   !/*pa        330*/
         do 215 j=1,30                                                          !/*pa        331*/
         n=index('0123456789',hb(j:j))                                          !/*pa        332*/
         if(n.ne.0) then                                                        !/*pa        333*/
            kx=kx*10+n-1                                                        !/*pa        334*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*pa        335*/
            i3=kx                                                               !/*pa        336*/
            go to 218                                                           !/*pa        337*/
         endif                                                                  !/*pa        338*/
  215    continue                                                               !/*pa        339*/
         i3=1                                                                   !/*pa        340*/
!                                                                               !/*pa        341*/
  218    if(i3.gt.ng3(ngd)) then                                                !/*pa        342*/
            write(6,'("This index is too large!")')                             !/*pa        343*/
            go to 210                                                           !/*pa        344*/
         endif                                                                  !/*pa        345*/
      elseif(hi(1:2).eq.'pa') then                                              !/*pa        346*/
         if(its*izs.ne.1) then                                                  !/*pa        347*/
            write(6,'("P and A spacing must be constant for PAW")')             !/*pa        348*/
            go to 500                                                           !/*pa        349*/
         endif                                                                  !/*pa        350*/
         ni=2                                                                   !/*pa        351*/
  220    write(6,'("What indicies do you what for r?")')                        !/*pa        352*/
         write(6,'(" <default = 1>")')                                          !/*pa        353*/
         write(6,'("&")')                                                       !/*pa        354*/
         read(5,'(a30)')hb                                                      !/*pa        355*/
         kx=0                                                                   !/*pa        356*/
         do 225 j=1,30                                                          !/*pa        357*/
         n=index('0123456789',hb(j:j))                                          !/*pa        358*/
         if(n.ne.0) then                                                        !/*pa        359*/
            kx=kx*10+n-1                                                        !/*pa        360*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*pa        361*/
            i1=kx                                                               !/*pa        362*/
            go to 228                                                           !/*pa        363*/
         endif                                                                  !/*pa        364*/
  225    continue                                                               !/*pa        365*/
         i1=1                                                                   !/*pa        366*/
!                                                                               !/*pa        367*/
  228    if(i1.gt.ng1(ngd)-1) then                                              !/*pa        368*/
            write(6,'("This index is too large!")')                             !/*pa        369*/
            go to 220                                                           !/*pa        370*/
         endif                                                                  !/*pa        371*/
      elseif(hi(1:2).eq.'ar') then                                              !/*pa        372*/
         if(its*irs.ne.1) then                                                  !/*pa        373*/
            write(6,'("A and R spacing must be constant for PAW")')             !/*pa        374*/
            go to 500                                                           !/*pa        375*/
         endif                                                                  !/*pa        376*/
         ni=3                                                                   !/*pa        377*/
  230    write(6,'("What indicies do you what for p?")')                        !/*pa        378*/
         write(6,'(" <default = 1>")')                                          !/*pa        379*/
         write(6,'("&")')                                                       !/*pa        380*/
         read(5,'(a30)')hb                                                      !/*pa        381*/
         kx=0                                                                   !/*pa        382*/
         do 235 j=1,30                                                          !/*pa        383*/
         n=index('0123456789',hb(j:j))                                          !/*pa        384*/
         if(n.ne.0) then                                                        !/*pa        385*/
            kx=kx*10+n-1                                                        !/*pa        386*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*pa        387*/
            i2=kx                                                               !/*pa        388*/
            go to 238                                                           !/*pa        389*/
         endif                                                                  !/*pa        390*/
  235    continue                                                               !/*pa        391*/
         i2=1                                                                   !/*pa        392*/
!                                                                               !/*pa        393*/
  238    if(i2.gt.ng2(ngd)) then                                                !/*pa        394*/
            write(6,'("This index is too large!")')                             !/*pa        395*/
            go to 230                                                           !/*pa        396*/
         endif                                                                  !/*pa        397*/
      else                                                                      !/*pa        398*/
         go to 200                                                              !/*pa        399*/
      endif                                                                     !/*pa        400*/
!                                                                               !/*pa        401*/
!      ni is an indicator as to which two coordinate varibles will be           !/*pa        402*/
!      looped over.    1,2,3  --  rp,pa,ar                                      !/*pa        403*/
!      The remaining index will be fixed.                                       !/*pa        404*/
      if(ni.eq.1) then                                                          !/*pa        405*/
         rmn=gdr(lgdr+1)                                                        !/*pa        406*/
         rmx=gdr(lgdr+ng1(ngd))                                                 !/*pa        407*/
         pmn=0.                                                                 !/*pa        408*/
         pmx=gdp(lgdp+ng2(ngd))                                                 !/*pa        409*/
!                                                                               !/*pa        410*/
         call hlimit(512000)                                                    !/*pa        411*/
         call hbook2(100,hf,n1,sngl(rmn),sngl(rmx),                             !/*pa        412*/
     &    n2,sngl(pmn),sngl(pmx),0.0)                                           !/*pa        413*/
         call hbook2(101,'error',n1,sngl(rmn),sngl(rmx),                        !/*pa        414*/
     &    n2,sngl(pmn),sngl(pmx),0.0)                                           !/*pa        415*/
!          <<<< write pdata and rp to output file here>>>                       !/*pa        416*/
!                                                                               !/*pa        417*/
         do 240 i1=1,n1                                                         !/*pa        418*/
         p=0                                                                    !/*pa        419*/
         r=gdr(lgdr+i1)                                                         !/*pa        420*/
         mx=(i3-1)*n1*n2+i1                                                     !/*pa        421*/
         f=gdata(lgdat+mx)                                                      !/*pa        422*/
         g=gdata(lgdat+mxgc(ngd)+mx)                                            !/*pa        423*/
         call hf2(100,sngl(r),sngl(p),sngl(f))                                  !/*pa        424*/
         call hf2(101,sngl(r),sngl(p),sngl(g))                                  !/*pa        425*/
!                                                                               !/*pa        426*/
         do 240 i2=2,n2                                                         !/*pa        427*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*pa        428*/
         f=gdata(lgdat+mx)                                                      !/*pa        429*/
         g=gdata(lgdat+mxgc(ngd)+mx)                                            !/*pa        430*/
         r=gdr(lgdr+i1)                                                         !/*pa        431*/
         p=gdp(lgdp+i2-1)                                                       !/*pa        432*/
         call hf2(100,sngl(r),sngl(p),sngl(f))                                  !/*pa        433*/
         call hf2(101,sngl(r),sngl(p),sngl(g))                                  !/*pa        434*/
  240    continue                                                               !/*pa        435*/
!                                                                               !/*pa        436*/
         write(6,'("Storing final histograms to ",a30)')hfp                     !/*pa        437*/
         open(unit=43,file=hfp,form='unformatted',status='new')                 !/*pa        438*/
         call hstore(0,43)                                                      !/*pa        439*/
         close(43)                                                              !/*pa        440*/
         return                                                                 !/*pa        441*/
!                                                                               !/*pa        442*/
      elseif(ni.eq.2) then                                                      !/*pa        443*/
         amn=0.0                                                                !/*pa        444*/
         amx=gda(lgda+ng3(ngd))                                                 !/*pa        445*/
         pmn=0.0                                                                !/*pa        446*/
         pmx=gdp(lgdp+ng2(ngd))                                                 !/*pa        447*/
!                                                                               !/*pa        448*/
         call hlimit(512000)                                                    !/*pa        449*/
         call hbook2(200,hf,n3,sngl(amn),sngl(amx),                             !/*pa        450*/
     &    n2,sngl(pmn),sngl(pmx),0.0)                                           !/*pa        451*/
         call hbook2(201,'error',n3,sngl(amn),sngl(amx),                        !/*pa        452*/
     &    n2,sngl(pmn),sngl(pmx),0.0)                                           !/*pa        453*/
!          <<<< write pdata and pa to output file here>>>                       !/*pa        454*/
!                                                                               !/*pa        455*/
         p=0.0                                                                  !/*pa        456*/
         a=0.0                                                                  !/*pa        457*/
         f=gdata(lgdat+i1)                                                      !/*pa        458*/
         g=gdata(lgdat+mxgc(ngd)+i1)                                            !/*pa        459*/
         call hf2(200,sngl(a),sngl(p),sngl(f))                                  !/*pa        460*/
         call hf2(201,sngl(a),sngl(p),sngl(g))                                  !/*pa        461*/
!                                                                               !/*pa        462*/
         a=0.0                                                                  !/*pa        463*/
         do 244 i2=2,n2                                                         !/*pa        464*/
         mx=(i2-1)*n1+i1                                                        !/*pa        465*/
         f=gdata(lgdat+mx)                                                      !/*pa        466*/
         g=gdata(lgdat+mxgc(ngd)+mx)                                            !/*pa        467*/
         p=gdp(lgdp+i2-1)                                                       !/*pa        468*/
         call hf2(200,sngl(a),sngl(p),sngl(f))                                  !/*pa        469*/
         call hf2(201,sngl(a),sngl(p),sngl(g))                                  !/*pa        470*/
  244    continue                                                               !/*pa        471*/
!                                                                               !/*pa        472*/
         p=0.0                                                                  !/*pa        473*/
         do 246 i3=2,n3                                                         !/*pa        474*/
         mx=(i3-1)*n1*n2+i1                                                     !/*pa        475*/
         f=gdata(lgdat+mx)                                                      !/*pa        476*/
         g=gdata(lgdat+mxgc(ngd)+mx)                                            !/*pa        477*/
         a=gda(lgda+i3-1)                                                       !/*pa        478*/
         call hf2(200,sngl(a),sngl(p),sngl(f))                                  !/*pa        479*/
         call hf2(201,sngl(a),sngl(p),sngl(g))                                  !/*pa        480*/
  246    continue                                                               !/*pa        481*/
!                                                                               !/*pa        482*/
         do 250 i2=2,n2                                                         !/*pa        483*/
         do 250 i3=2,n3                                                         !/*pa        484*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*pa        485*/
         f=gdata(lgdat+mx)                                                      !/*pa        486*/
         g=gdata(lgdat+mxgc(ngd)+mx)                                            !/*pa        487*/
         a=gda(lgda+i3-1)                                                       !/*pa        488*/
         p=gdp(lgdp+i2-1)                                                       !/*pa        489*/
         call hf2(200,sngl(a),sngl(p),sngl(f))                                  !/*pa        490*/
         call hf2(201,sngl(a),sngl(p),sngl(g))                                  !/*pa        491*/
  250    continue                                                               !/*pa        492*/
!                                                                               !/*pa        493*/
         write(6,'("Storing final histograms to ",a30)')hfp                     !/*pa        494*/
         open(unit=43,file=hfp,form='unformatted',status='new')                 !/*pa        495*/
         call hstore(0,43)                                                      !/*pa        496*/
         close(43)                                                              !/*pa        497*/
         return                                                                 !/*pa        498*/
!                                                                               !/*pa        499*/
      else                                                                      !/*pa        500*/
         amn=0.0                                                                !/*pa        501*/
         amx=gda(lgda+ng3(ngd))                                                 !/*pa        502*/
         rmn=gdr(lgdr+1)                                                        !/*pa        503*/
         rmx=gdr(lgdr+ng1(ngd))                                                 !/*pa        504*/
!                                                                               !/*pa        505*/
         call hlimit(512000)                                                    !/*pa        506*/
         call hbook2(300,hf,n3,sngl(amn),sngl(amx),                             !/*pa        507*/
     &    n1,sngl(rmn),sngl(rmx),0.0)                                           !/*pa        508*/
         call hbook2(301,'error',n3,sngl(amn),sngl(amx),                        !/*pa        509*/
     &    n1,sngl(rmn),sngl(rmx),0.0)                                           !/*pa        510*/
!          <<<< write pdata and tr to output file here>>>                       !/*pa        511*/
!                                                                               !/*pa        512*/
         do 260 i1=1,n1                                                         !/*pa        513*/
         a=0                                                                    !/*pa        514*/
         r=gdr(lgdr+i1)                                                         !/*pa        515*/
         mx=(i2-1)*n1+i1                                                        !/*pa        516*/
         f=gdata(lgdat+mx)                                                      !/*pa        517*/
         g=gdata(lgdat+mxgc(ngd)+mx)                                            !/*pa        518*/
         call hf2(300,sngl(a),sngl(r),sngl(f))                                  !/*pa        519*/
         call hf2(301,sngl(a),sngl(r),sngl(g))                                  !/*pa        520*/
!                                                                               !/*pa        521*/
         do 260 i3=2,n3                                                         !/*pa        522*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*pa        523*/
         f=gdata(lgdat+mx)                                                      !/*pa        524*/
         g=gdata(lgdat+mxgc(ngd)+mx)                                            !/*pa        525*/
         a=gda(lgda+i3-1)                                                       !/*pa        526*/
         r=gdr(lgdr+i1)                                                         !/*pa        527*/
         call hf2(300,sngl(a),sngl(r),sngl(f))                                  !/*pa        528*/
         call hf2(301,sngl(a),sngl(r),sngl(g))                                  !/*pa        529*/
  260    continue                                                               !/*pa        530*/
!                                                                               !/*pa        531*/
         write(6,'("Storing final histograms to ",a30)')hfp                     !/*pa        532*/
         open(unit=43,file=hfp,form='unformatted',status='new')                 !/*pa        533*/
         call hstore(0,43)                                                      !/*pa        534*/
         close(43)                                                              !/*pa        535*/
         return                                                                 !/*pa        536*/
!                                                                               !/*pa        537*/
      endif                                                                     !/*pa        538*/
!                                                                               !/*pa        539*/
  500 continue                                                                  !/*pa        540*/
#endif                                                                          /*!pa        541*/
      return                                                                    !/*pa        542*/
      end                                                                       !/*pa        543*/
