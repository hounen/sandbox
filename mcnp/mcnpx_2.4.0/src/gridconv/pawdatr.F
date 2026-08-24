!     $Id: pawdatr.F,v 1.6 2002/03/27 18:13:53 gwm Exp $                      !/*pa          1*/
      subroutine pawdatr                                                        !/*pa          2*/
#ifndef NO_PAW                                                                  /*!pa          3*/
!       This subroutine is designed to convert input data from a                !/*pa          4*/
!       mesh tally set to a data set compatible with the input                  !/*pa          5*/
!       requirements of the PAW graphics package.                               !/*pa          6*/
!                                                                               !/*pa          7*/
!       Written by E. C. Snow  2/9/98.   (revised: 11/15/2001)                  !/*pa          8*/
!                                                                               !/*pa          9*/
#include <gridconv/gcdouble.h>                                                  /*!pa         10*/
#include <gridconv/gccom5.h>                                                    /*!pa         11*/
!                                                                               !/*pa         12*/
      dimension gdr(1),gdz(1),gdt(1),gdata(1),pdr(1),pdz(1),                    !/*pa         13*/
     & pdt(1),pdata(1)                                                          !/*pa         14*/
      equivalence (das,gdr,gdz,gdt,gdata,pdr,pdz,pdt,pdata)                     !/*pa         15*/
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
         hfp(1:10)='datarz.paw'                                                 !/*pa         29*/
         write(6,'("Using default output file name: datarz.paw")')              !/*pa         30*/
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
      n2=ng2(ngd)-1                                                             !/*pa         59*/
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
      write(6,'("Plot data as fcn. of r, z, t(theta), or q to exit?")')         !/*pa         78*/
      write(6,'(" < No default! >")')                                           !/*pa         79*/
      write(6,'("&")')                                                          !/*pa         80*/
      read(5,'(a3)')hi                                                          !/*pa         81*/
      if(hi(1:1).eq.'q')go to 500                                               !/*pa         82*/
      if(hi(1:1).eq.'r')nv=1                                                    !/*pa         83*/
      if(hi(1:1).eq.'z')nv=2                                                    !/*pa         84*/
      if(hi(1:1).eq.'t')nv=3                                                    !/*pa         85*/
!                                                                               !/*pa         86*/
      if(nv.eq.1) then                                                          !/*pa         87*/
         if(irs.ne.1) then                                                      !/*pa         88*/
            write(6,'("R spacing must be constant for PAW")')                   !/*pa         89*/
            go to 500                                                           !/*pa         90*/
         endif                                                                  !/*pa         91*/
  110    write(6,'("What indicies do you what for z and theta?")')              !/*pa         92*/
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
  118    if(i2.gt.ng2(ngd)-1.or.i3.gt.ng3(ngd)) then                            !/*pa        124*/
            write(6,'("One of these indices is too large!")')                   !/*pa        125*/
            go to 110                                                           !/*pa        126*/
         endif                                                                  !/*pa        127*/
      elseif(nv.eq.2) then                                                      !/*pa        128*/
         if(izs.ne.1) then                                                      !/*pa        129*/
            write(6,'("Z spacing must be constant for PAW")')                   !/*pa        130*/
            go to 500                                                           !/*pa        131*/
         endif                                                                  !/*pa        132*/
  120    write(6,'("What indicies do you what for r and theta?")')              !/*pa        133*/
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
      elseif(nv.eq.3) then                                                      !/*pa        169*/
  130    write(6,'("What indicies do you what for r and z?")')                  !/*pa        170*/
         write(6,'(" <default = 1,1>")')                                        !/*pa        171*/
         write(6,'("&")')                                                       !/*pa        172*/
         read(5,'(a30)')hb                                                      !/*pa        173*/
         do 132 j=1,30                                                          !/*pa        174*/
         n=index('0123456789',hb(j:j))                                          !/*pa        175*/
         if(n.ne.0) go to 133                                                   !/*pa        176*/
  132    continue                                                               !/*pa        177*/
         i1=1                                                                   !/*pa        178*/
         i2=1                                                                   !/*pa        179*/
         go to 138                                                              !/*pa        180*/
!                                                                               !/*pa        181*/
  133    kk=1                                                                   !/*pa        182*/
         do 136 i=1,2                                                           !/*pa        183*/
         kx=0                                                                   !/*pa        184*/
         do 134 j=kk,30                                                         !/*pa        185*/
         n=index('0123456789',hb(j:j))                                          !/*pa        186*/
         if(n.ne.0) then                                                        !/*pa        187*/
            kx=kx*10+n-1                                                        !/*pa        188*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*pa        189*/
            kk=j+1                                                              !/*pa        190*/
            go to 135                                                           !/*pa        191*/
         endif                                                                  !/*pa        192*/
  134    continue                                                               !/*pa        193*/
!                                                                               !/*pa        194*/
  135    if(i.eq.1) then                                                        !/*pa        195*/
            i1=kx                                                               !/*pa        196*/
         else                                                                   !/*pa        197*/
            i2=kx                                                               !/*pa        198*/
         endif                                                                  !/*pa        199*/
  136    continue                                                               !/*pa        200*/
!                                                                               !/*pa        201*/
  138    if(i1.gt.ng1(ngd)-1.or.i2.gt.ng2(ngd)-1) then                          !/*pa        202*/
            write(6,'("One of these indices is too large!")')                   !/*pa        203*/
            go to 130                                                           !/*pa        204*/
         endif                                                                  !/*pa        205*/
      else                                                                      !/*pa        206*/
         go to 100                                                              !/*pa        207*/
      endif                                                                     !/*pa        208*/
!                                                                               !/*pa        209*/
!       nv is the number of the coordinate varibile that will                   !/*pa        210*/
!       looped over.   1,2,3 - r, z, theta.                                     !/*pa        211*/
!       The other two indices will be fixed.                                    !/*pa        212*/
!                                                                               !/*pa        213*/
      if(nv.eq.1) then                                                          !/*pa        214*/
         cmin=gdr(lgdr+1)                                                       !/*pa        215*/
         cmax=gdr(lgdr+ng1(ngd))                                                !/*pa        216*/
!                                                                               !/*pa        217*/
         do 140 i1=1,n1                                                         !/*pa        218*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*pa        219*/
         pdata(lpdat+i1)=gdata(lgdat+mx)                                        !/*pa        220*/
         pdata(lpdat+n1+i1)=gdata(lgdat+mxgc(ngd)+mx)                           !/*pa        221*/
  140    continue                                                               !/*pa        222*/
!                                                                               !/*pa        223*/
!       Write data the an unformatted file for input                            !/*pa        224*/
!       into the PAW utility for plotting.                                      !/*pa        225*/
!                                                                               !/*pa        226*/
         call hlimit(512000)                                                    !/*pa        227*/
         call hbook1(10,hf,n1,sngl(cmin),sngl(cmax),0.0)                        !/*pa        228*/
         call hbook1(11,'error',n1,sngl(cmin),sngl(cmax),0.0)                   !/*pa        229*/
c                                                                               !/*pa        230*/
         do 145 i=1,n1                                                          !/*pa        231*/
         r=pdr(lpdr+i)                                                          !/*pa        232*/
         f=pdata(lpdat+i)                                                       !/*pa        233*/
         g=pdata(lpdat+n1+i)                                                    !/*pa        234*/
         call hf1(10,sngl(r),sngl(f))                                           !/*pa        235*/
         call hf1(11,sngl(r),sngl(g))                                           !/*pa        236*/
  145    continue                                                               !/*pa        237*/
!                                                                               !/*pa        238*/
      elseif(nv.eq.2) then                                                      !/*pa        239*/
         cmin=gdz(lgdz+1)                                                       !/*pa        240*/
         cmax=gdz(lgdz+ng2(ngd))                                                !/*pa        241*/
!                                                                               !/*pa        242*/
         do 150 i2=1,n2                                                         !/*pa        243*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*pa        244*/
         pdata(lpdat+i2)=gdata(lgdat+mx)                                        !/*pa        245*/
         pdata(lpdat+n2+i2)=gdata(lgdat+mxgc(ngd)+mx)                           !/*pa        246*/
  150    continue                                                               !/*pa        247*/
!                                                                               !/*pa        248*/
!       Write data the an unformatted file for input                            !/*pa        249*/
!       into the PAW utility for plotting.                                      !/*pa        250*/
!                                                                               !/*pa        251*/
         call hlimit(512000)                                                    !/*pa        252*/
         call hbook1(20,hf,n2,sngl(cmin),sngl(cmax),0.0)                        !/*pa        253*/
         call hbook1(21,'error',n2,sngl(cmin),sngl(cmax),0.0)                   !/*pa        254*/
c                                                                               !/*pa        255*/
         do 155 i=1,n2                                                          !/*pa        256*/
         z=pdz(lpdz+i)                                                          !/*pa        257*/
         f=pdata(lpdat+i)                                                       !/*pa        258*/
         g=pdata(lpdat+n2+i)                                                    !/*pa        259*/
         call hf1(20,sngl(z),sngl(f))                                           !/*pa        260*/
         call hf1(21,sngl(z),sngl(g))                                           !/*pa        261*/
  155    continue                                                               !/*pa        262*/
!                                                                               !/*pa        263*/
      elseif(nv.eq.3) then                                                      !/*pa        264*/
         cmin=0.                                                                !/*pa        265*/
         cmax=360.                                                              !/*pa        266*/
!                                                                               !/*pa        267*/
         do 160 i3=1,n3                                                         !/*pa        268*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*pa        269*/
         pdata(lpdat+i3)=gdata(lgdat+mx)                                        !/*pa        270*/
         pdata(lpdat+n3+i3)=gdata(lgdat+mxgc(ngd)+mx)                           !/*pa        271*/
  160    continue                                                               !/*pa        272*/
!                                                                               !/*pa        273*/
!       Write data the an unformatted file for input                            !/*pa        274*/
!       into the PAW utility for plotting.                                      !/*pa        275*/
!                                                                               !/*pa        276*/
         call hlimit(512000)                                                    !/*pa        277*/
         call hbook1(30,hf,n3,sngl(cmin),sngl(cmax),0.0)                        !/*pa        278*/
         call hbook1(31,'error',n3,sngl(cmin),sngl(cmax),0.0)                   !/*pa        279*/
         t=0                                                                    !/*pa        280*/
         f=pdata(lpdat+1)                                                       !/*pa        281*/
         g=pdata(lpdat+n3+1)                                                    !/*pa        282*/
         call hf1(30,sngl(t),sngl(f))                                           !/*pa        283*/
         call hf1(31,sngl(t),sngl(g))                                           !/*pa        284*/
c                                                                               !/*pa        285*/
         do 190 i=2,n3                                                          !/*pa        286*/
         t=gdt(lgdt+i-1)                                                        !/*pa        287*/
         f=pdata(lpdat+i)                                                       !/*pa        288*/
         g=pdata(lpdat+n3+i)                                                    !/*pa        289*/
         call hf1(30,sngl(t),sngl(f))                                           !/*pa        290*/
         call hf1(31,sngl(t),sngl(g))                                           !/*pa        291*/
  190    continue                                                               !/*pa        292*/
      else                                                                      !/*pa        293*/
         go to 100                                                              !/*pa        294*/
      endif                                                                     !/*pa        295*/
!                                                                               !/*pa        296*/
      write(6,'("Storing final histograms to ",a30)')hfp                        !/*pa        297*/
      open(unit=43,file=hfp,form='unformatted',status='new')                    !/*pa        298*/
      call hstore(0,43)                                                         !/*pa        299*/
      close(43)                                                                 !/*pa        300*/
      return                                                                    !/*pa        301*/
!                                                                               !/*pa        302*/
!       Two dinemsional plot data.                                              !/*pa        303*/
!       Get dependent variables and index of other one.                         !/*pa        304*/
  200 ni=0                                                                      !/*pa        305*/
      write(6,'("Plot data as fcn. of rz, tz, tr or q to exit ?")')             !/*pa        306*/
      write(6,'(" <default = rz >")')                                           !/*pa        307*/
      write(6,'("&")')                                                          !/*pa        308*/
      read(5,'(a3)')hi                                                          !/*pa        309*/
      if(hi(1:1).eq.'q')go to 500                                               !/*pa        310*/
      if(hi(1:2).eq.'  ')hi(1:2)='rz'                                           !/*pa        311*/
      if(hi(1:2).eq.'rz') then                                                  !/*pa        312*/
         if(irs*izs.ne.1) then                                                  !/*pa        313*/
            write(6,'("R and Z spacing must be constant for PAW")')             !/*pa        314*/
            go to 500                                                           !/*pa        315*/
         endif                                                                  !/*pa        316*/
         ni=1                                                                   !/*pa        317*/
  210    write(6,'("What indicies do you what for theta?")')                    !/*pa        318*/
         write(6,'(" <default = 1>")')                                          !/*pa        319*/
         write(6,'("&")')                                                       !/*pa        320*/
         read(5,'(a30)')hb                                                      !/*pa        321*/
         kx=0                                                                   !/*pa        322*/
         do 215 j=1,30                                                          !/*pa        323*/
         n=index('0123456789',hb(j:j))                                          !/*pa        324*/
         if(n.ne.0) then                                                        !/*pa        325*/
            kx=kx*10+n-1                                                        !/*pa        326*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*pa        327*/
            i3=kx                                                               !/*pa        328*/
            go to 218                                                           !/*pa        329*/
         endif                                                                  !/*pa        330*/
  215    continue                                                               !/*pa        331*/
         i3=1                                                                   !/*pa        332*/
!                                                                               !/*pa        333*/
  218    if(i3.gt.ng3(ngd)) then                                                !/*pa        334*/
            write(6,'("This index is too large!")')                             !/*pa        335*/
            go to 210                                                           !/*pa        336*/
         endif                                                                  !/*pa        337*/
      elseif(hi(1:2).eq.'tz') then                                              !/*pa        338*/
         if(its*izs.ne.1) then                                                  !/*pa        339*/
            write(6,'("Theta and Z spacing must be constant for PAW")')         !/*pa        340*/
            go to 500                                                           !/*pa        341*/
         endif                                                                  !/*pa        342*/
         ni=2                                                                   !/*pa        343*/
  220    write(6,'("What indicies do you what for r?")')                        !/*pa        344*/
         write(6,'(" <default = 1>")')                                          !/*pa        345*/
         write(6,'("&")')                                                       !/*pa        346*/
         read(5,'(a30)')hb                                                      !/*pa        347*/
         kx=0                                                                   !/*pa        348*/
         do 225 j=1,30                                                          !/*pa        349*/
         n=index('0123456789',hb(j:j))                                          !/*pa        350*/
         if(n.ne.0) then                                                        !/*pa        351*/
            kx=kx*10+n-1                                                        !/*pa        352*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*pa        353*/
            i1=kx                                                               !/*pa        354*/
            go to 228                                                           !/*pa        355*/
         endif                                                                  !/*pa        356*/
  225    continue                                                               !/*pa        357*/
         i1=1                                                                   !/*pa        358*/
!                                                                               !/*pa        359*/
  228    if(i1.gt.ng1(ngd)-1) then                                              !/*pa        360*/
            write(6,'("This index is too large!")')                             !/*pa        361*/
            go to 220                                                           !/*pa        362*/
         endif                                                                  !/*pa        363*/
      elseif(hi(1:2).eq.'tr') then                                              !/*pa        364*/
         if(its*irs.ne.1) then                                                  !/*pa        365*/
            write(6,'("Theta and R spacing must be constant for PAW")')         !/*pa        366*/
            go to 500                                                           !/*pa        367*/
         endif                                                                  !/*pa        368*/
         ni=3                                                                   !/*pa        369*/
  230    write(6,'("What indicies do you what for z?")')                        !/*pa        370*/
         write(6,'(" <default = 1>")')                                          !/*pa        371*/
         write(6,'("&")')                                                       !/*pa        372*/
         read(5,'(a30)')hb                                                      !/*pa        373*/
         kx=0                                                                   !/*pa        374*/
         do 235 j=1,30                                                          !/*pa        375*/
         n=index('0123456789',hb(j:j))                                          !/*pa        376*/
         if(n.ne.0) then                                                        !/*pa        377*/
            kx=kx*10+n-1                                                        !/*pa        378*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*pa        379*/
            i2=kx                                                               !/*pa        380*/
            go to 238                                                           !/*pa        381*/
         endif                                                                  !/*pa        382*/
  235    continue                                                               !/*pa        383*/
         i2=1                                                                   !/*pa        384*/
!                                                                               !/*pa        385*/
  238    if(i2.gt.ng2(ngd)-1) then                                              !/*pa        386*/
            write(6,'("This index is too large!")')                             !/*pa        387*/
            go to 230                                                           !/*pa        388*/
         endif                                                                  !/*pa        389*/
      else                                                                      !/*pa        390*/
         go to 200                                                              !/*pa        391*/
      endif                                                                     !/*pa        392*/
!                                                                               !/*pa        393*/
!      ni is an indicator as to which two coordinate varibles will be           !/*pa        394*/
!      looped over.    1,2,3  --  rz,tz,tr                                      !/*pa        395*/
!      The remaining index will be fixed.                                       !/*pa        396*/
      if(ni.eq.1) then                                                          !/*pa        397*/
         rmin=gdr(lgdr+1)                                                       !/*pa        398*/
         rmax=gdr(lgdr+ng1(ngd))                                                !/*pa        399*/
         zmin=gdz(lgdz+1)                                                       !/*pa        400*/
         zmax=gdz(lgdz+ng2(ngd))                                                !/*pa        401*/
!                                                                               !/*pa        402*/
         call hlimit(512000)                                                    !/*pa        403*/
         call hbook2(100,hf,n1,sngl(rmin),sngl(rmax),                           !/*pa        404*/
     &    n2,sngl(zmin),sngl(zmax),0.0)                                         !/*pa        405*/
         call hbook2(101,'error',n1,sngl(rmin),sngl(rmax),                      !/*pa        406*/
     &    n2,sngl(zmin),sngl(zmax),0.0)                                         !/*pa        407*/
!          <<<< write pdata and rz to output file here>>>                       !/*pa        408*/
!                                                                               !/*pa        409*/
         do 240 i2=1,n2                                                         !/*pa        410*/
         do 240 i1=1,n1                                                         !/*pa        411*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*pa        412*/
         f=gdata(lgdat+mx)                                                      !/*pa        413*/
         g=gdata(lgdat+mxgc(ngd)+mx)                                            !/*pa        414*/
         r=pdr(lpdr+i1)                                                         !/*pa        415*/
         z=pdz(lpdz+i2)                                                         !/*pa        416*/
         call hf2(100,sngl(r),sngl(z),sngl(f))                                  !/*pa        417*/
         call hf2(101,sngl(r),sngl(z),sngl(g))                                  !/*pa        418*/
  240    continue                                                               !/*pa        419*/
!                                                                               !/*pa        420*/
         write(6,'("Storing final histograms to ",a30)')hfp                     !/*pa        421*/
         open(unit=43,file=hfp,form='unformatted',status='new')                 !/*pa        422*/
         call hstore(0,43)                                                      !/*pa        423*/
         close(43)                                                              !/*pa        424*/
         return                                                                 !/*pa        425*/
!                                                                               !/*pa        426*/
      elseif(ni.eq.2) then                                                      !/*pa        427*/
         tmin=0.0                                                               !/*pa        428*/
         tmax=gdt(lgdt+ng3(ngd))                                                !/*pa        429*/
         zmin=gdz(lgdz+1)                                                       !/*pa        430*/
         zmax=gdz(lgdz+ng2(ngd))                                                !/*pa        431*/
!                                                                               !/*pa        432*/
         call hlimit(512000)                                                    !/*pa        433*/
         call hbook2(200,hf,n3,sngl(tmin),sngl(tmax),                           !/*pa        434*/
     &    n2,sngl(zmin),sngl(zmax),0.0)                                         !/*pa        435*/
         call hbook2(201,'error',n3,sngl(tmin),sngl(tmax),                      !/*pa        436*/
     &    n2,sngl(zmin),sngl(zmax),0.0)                                         !/*pa        437*/
!          <<<< write pdata and tz to output file here>>>                       !/*pa        438*/
!                                                                               !/*pa        439*/
         do 250 i2=1,n2                                                         !/*pa        440*/
         t=0.0                                                                  !/*pa        441*/
         z=pdz(lpdz+i2)                                                         !/*pa        442*/
         mx=(i2-1)*n1+i1                                                        !/*pa        443*/
         f=gdata(lgdat+mx)                                                      !/*pa        444*/
         g=gdata(lgdat+mxgc(ngd)+mx)                                            !/*pa        445*/
         call hf2(200,sngl(t),sngl(z),sngl(f))                                  !/*pa        446*/
         call hf2(201,sngl(t),sngl(z),sngl(g))                                  !/*pa        447*/
c                                                                               !/*pa        448*/
         do 250 i3=2,n3                                                         !/*pa        449*/
         t=gdt(lgdt+i3-1)                                                       !/*pa        450*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*pa        451*/
         f=gdata(lgdat+mx)                                                      !/*pa        452*/
         g=gdata(lgdat+mxgc(ngd)+mx)                                            !/*pa        453*/
         call hf2(200,sngl(t),sngl(z),sngl(f))                                  !/*pa        454*/
         call hf2(201,sngl(t),sngl(z),sngl(g))                                  !/*pa        455*/
  250    continue                                                               !/*pa        456*/
!                                                                               !/*pa        457*/
         write(6,'("Storing final histograms to ",a30)')hfp                     !/*pa        458*/
         open(unit=43,file=hfp,form='unformatted',status='new')                 !/*pa        459*/
         call hstore(0,43)                                                      !/*pa        460*/
         close(43)                                                              !/*pa        461*/
         return                                                                 !/*pa        462*/
!                                                                               !/*pa        463*/
      else                                                                      !/*pa        464*/
         tmin=0.0                                                               !/*pa        465*/
         tmax=gdt(lgdt+ng3(ngd))                                                !/*pa        466*/
         rmin=gdr(lgdr+1)                                                       !/*pa        467*/
         rmax=gdr(lgdr+ng1(ngd))                                                !/*pa        468*/
!                                                                               !/*pa        469*/
         call hlimit(512000)                                                    !/*pa        470*/
         call hbook2(300,hf,n3,sngl(tmin),sngl(tmax),                           !/*pa        471*/
     &    n1,sngl(rmin),sngl(rmax),0.0)                                         !/*pa        472*/
         call hbook2(301,'error',n3,sngl(tmin),sngl(tmax),                      !/*pa        473*/
     &    n1,sngl(rmin),sngl(rmax),0.0)                                         !/*pa        474*/
!          <<<< write pdata and tr to output file here>>>                       !/*pa        475*/
!                                                                               !/*pa        476*/
         do 260 i1=1,n1                                                         !/*pa        477*/
         t=0                                                                    !/*pa        478*/
         r=pdr(lpdr+i1)                                                         !/*pa        479*/
         mx=(i2-1)*n1+i1                                                        !/*pa        480*/
         f=gdata(lgdat+mx)                                                      !/*pa        481*/
         g=gdata(lgdat+mxgc(ngd)+mx)                                            !/*pa        482*/
         call hf2(300,sngl(t),sngl(r),sngl(f))                                  !/*pa        483*/
         call hf2(301,sngl(t),sngl(r),sngl(g))                                  !/*pa        484*/
c                                                                               !/*pa        485*/
         do 260 i3=2,n3                                                         !/*pa        486*/
         t=gdt(lgdt+i3-1)                                                       !/*pa        487*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*pa        488*/
         f=gdata(lgdat+mx)                                                      !/*pa        489*/
         g=gdata(lgdat+mxgc(ngd)+mx)                                            !/*pa        490*/
         call hf2(300,sngl(t),sngl(r),sngl(f))                                  !/*pa        491*/
         call hf2(301,sngl(t),sngl(r),sngl(g))                                  !/*pa        492*/
  260    continue                                                               !/*pa        493*/
!                                                                               !/*pa        494*/
         write(6,'("Storing final histograms to ",a30)')hfp                     !/*pa        495*/
         open(unit=43,file=hfp,form='unformatted',status='new')                 !/*pa        496*/
         call hstore(0,43)                                                      !/*pa        497*/
         close(43)                                                              !/*pa        498*/
         return                                                                 !/*pa        499*/
!                                                                               !/*pa        500*/
      endif                                                                     !/*pa        501*/
!                                                                               !/*pa        502*/
  500 continue                                                                  !/*pa        503*/
#endif                                                                          /*!pa        504*/
      return                                                                    !/*pa        505*/
      end                                                                       !/*pa        506*/
