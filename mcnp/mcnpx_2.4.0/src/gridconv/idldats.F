!     $Id: idldats.F,v 1.6 2002/03/27 18:13:52 gwm Exp $                      !/*id          1*/
      subroutine idldats                                                        !/*id          2*/
!                                                                               !/*id          3*/
!       This subroutine is designed to convert input data from a                !/*id          4*/
!       mesh tally set to a data set compatible with the input                  !/*id          5*/
!       requirements of the IDL graphics package.                               !/*id          6*/
!                                                                               !/*id          7*/
!       Written by E. C. Snow  11/22/98.   (revised: 11/22/99)                  !/*id          8*/
!                                                                               !/*id          9*/
#include <gridconv/gcdouble.h>                                                  /*!id         10*/
#include <gridconv/gccom5.h>                                                    /*!id         11*/
!                                                                               !/*id         12*/
      dimension gdr(1),gdp(1),gda(1),gdata(1),pdr(1),pdp(1),                    !/*id         13*/
     & pda(1),pdata(1)                                                          !/*id         14*/
      equivalence (das,gdr,gdp,gda,gdata,pdr,pdp,pda,pdata)                     !/*id         15*/
!                                                                               !/*id         16*/
      character hi*3,hv(3)*1,hp*10,hf*10,hb*30                                  !/*id         17*/
      data hv/'R','P','A'/                                                      !/*id         18*/
!                                                                               !/*id         19*/
      write(6,'("What IDL filename do you want.")')                             !/*id         20*/
      write(6,'("%")')                                                          !/*id         21*/
      read(5,'(a30)')hfp                                                        !/*id         22*/
      j=leng(hfp)                                                               !/*id         23*/
      if(j.eq.0) then                                                           !/*id         24*/
         j=10                                                                   !/*id         25*/
         hfp(1:10)='datapa.idl'                                                 !/*id         26*/
         write(6,'("Using default output file name: datapa.idl")')              !/*id         27*/
      endif                                                                     !/*id         28*/
!                                                                               !/*id         29*/
   20 call unique(hfp,6)                                                        !/*id         30*/
      open(31,file=hfp,form='formatted',status='new')                           !/*id         31*/
!                                                                               !/*id         32*/
   30 write(6,'("Enter plot dimensionality, 1, 2, or 9 to exit")')              !/*id         33*/
      write(6,'("%")')                                                          !/*id         34*/
      read(5,'(a30)')hb                                                         !/*id         35*/
      kx=0                                                                      !/*id         36*/
      np=0                                                                      !/*id         37*/
      do 35 j=1,30                                                              !/*id         38*/
      n=index('0123456789',hb(j:j))                                             !/*id         39*/
      if(n.ne.0) then                                                           !/*id         40*/
         kx=kx*10+n-1                                                           !/*id         41*/
      elseif(n.eq.0.and.kx.gt.0) then                                           !/*id         42*/
         np=kx                                                                  !/*id         43*/
         go to 38                                                               !/*id         44*/
      elseif(np.eq.0.and.j.eq.30) then                                          !/*id         45*/
         write(6,'(" There is no default value! ")')                            !/*id         46*/
         go to 30                                                               !/*id         47*/
      endif                                                                     !/*id         48*/
   35 continue                                                                  !/*id         49*/
!                                                                               !/*id         50*/
   38 if(np.eq.9)go to 500                                                      !/*id         51*/
      if(np.ge.1.and.np.le.2)go to 40                                           !/*id         52*/
      go to 30                                                                  !/*id         53*/
!                                                                               !/*id         54*/
   40 n1=ng1(ngd)-1                                                             !/*id         55*/
      n2=ng2(ngd)                                                               !/*id         56*/
      n3=ng3(ngd)                                                               !/*id         57*/
      nt=mod(ngtyp(ngd),10)                                                     !/*id         58*/
      if(nt.eq.0) then                                                          !/*id         59*/
         hp=hip(igpt(ngd))                                                      !/*id         60*/
         hf=hdx(ngd,nxv)                                                        !/*id         61*/
      elseif(nt.eq.1) then                                                      !/*id         62*/
         hp=hdx(ngd,nxv)                                                        !/*id         63*/
         hf='source pts'                                                        !/*id         64*/
      elseif(nt.eq.2) then                                                      !/*id         65*/
         hp=hdd(ngv(ngd,nxv))                                                   !/*id         66*/
         hf='ergdeposit'                                                        !/*id         67*/
      elseif(nt.eq.3) then                                                      !/*id         68*/
         hp=hip(igpt(ngd))                                                      !/*id         69*/
         hf='PD tracks.'                                                        !/*id         70*/
      elseif(nt.eq.4) then                                                      !/*id         71*/
         hp=hip(igpt(ngd))                                                      !/*id         72*/
         hf='DX tracks.'                                                        !/*id         73*/
      endif                                                                     !/*id         74*/
      go to (100,200)np                                                         !/*id         75*/
!                                                                               !/*id         76*/
!       One dinemsional plot data.                                              !/*id         77*/
!       Get dependent variable and index of other two.                          !/*id         78*/
  100 nv=0                                                                      !/*id         79*/
      write(6,'("Plot data as fcn. of r, p, a, or q to exit?")')                !/*id         80*/
      write(6,'("%")')                                                          !/*id         81*/
      read(5,'(a3)')hi                                                          !/*id         82*/
      if(hi(1:1).eq.'q')go to 500                                               !/*id         83*/
      if(hi(1:1).eq.'r')nv=1                                                    !/*id         84*/
      if(hi(1:1).eq.'p')nv=2                                                    !/*id         85*/
      if(hi(1:1).eq.'a')nv=3                                                    !/*id         86*/
!                                                                               !/*id         87*/
      if(nv.eq.1) then                                                          !/*id         88*/
  110    write(6,'("What indicies do you what for p and a?")')                  !/*id         89*/
         write(6,'(" <default = 1,1>")')                                        !/*id         90*/
         write(6,'("%")')                                                       !/*id         91*/
         read(5,'(a30)')hb                                                      !/*id         92*/
         do 112 j=1,30                                                          !/*id         93*/
         n=index('0123456789',hb(j:j))                                          !/*id         94*/
         if(n.ne.0) go to 113                                                   !/*id         95*/
  112    continue                                                               !/*id         96*/
         i2=1                                                                   !/*id         97*/
         i3=1                                                                   !/*id         98*/
         go to 118                                                              !/*id         99*/
!                                                                               !/*id        100*/
  113    kk=1                                                                   !/*id        101*/
         do 116 i=1,2                                                           !/*id        102*/
         kx=0                                                                   !/*id        103*/
         do 114 j=kk,30                                                         !/*id        104*/
         n=index('0123456789',hb(j:j))                                          !/*id        105*/
         if(n.ne.0) then                                                        !/*id        106*/
            kx=kx*10+n-1                                                        !/*id        107*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*id        108*/
            kk=j+1                                                              !/*id        109*/
            go to 115                                                           !/*id        110*/
         endif                                                                  !/*id        111*/
  114    continue                                                               !/*id        112*/
!                                                                               !/*id        113*/
  115    if(i.eq.1) then                                                        !/*id        114*/
            i2=kx                                                               !/*id        115*/
         else                                                                   !/*id        116*/
            i3=kx                                                               !/*id        117*/
         endif                                                                  !/*id        118*/
  116    continue                                                               !/*id        119*/
!                                                                               !/*id        120*/
  118    if(i2.gt.ng2(ngd).or.i3.gt.ng3(ngd)) then                              !/*id        121*/
            write(6,'("One of these indices is too large!")')                   !/*id        122*/
            go to 110                                                           !/*id        123*/
         endif                                                                  !/*id        124*/
      elseif(nv.eq.2) then                                                      !/*id        125*/
  120    write(6,'("What indicies do you what for r and a?")')                  !/*id        126*/
         write(6,'(" <default = 1,1>")')                                        !/*id        127*/
         write(6,'("%")')                                                       !/*id        128*/
         read(5,'(a30)')hb                                                      !/*id        129*/
         do 122 j=1,30                                                          !/*id        130*/
         n=index('0123456789',hb(j:j))                                          !/*id        131*/
         if(n.ne.0) go to 123                                                   !/*id        132*/
  122    continue                                                               !/*id        133*/
         i1=1                                                                   !/*id        134*/
         i3=1                                                                   !/*id        135*/
         go to 128                                                              !/*id        136*/
!                                                                               !/*id        137*/
  123    kk=1                                                                   !/*id        138*/
         do 126 i=1,2                                                           !/*id        139*/
         kx=0                                                                   !/*id        140*/
         do 124 j=kk,30                                                         !/*id        141*/
         n=index('0123456789',hb(j:j))                                          !/*id        142*/
         if(n.ne.0) then                                                        !/*id        143*/
            kx=kx*10+n-1                                                        !/*id        144*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*id        145*/
            kk=j+1                                                              !/*id        146*/
            go to 125                                                           !/*id        147*/
         endif                                                                  !/*id        148*/
  124    continue                                                               !/*id        149*/
!                                                                               !/*id        150*/
  125    if(i.eq.1) then                                                        !/*id        151*/
            i1=kx                                                               !/*id        152*/
         else                                                                   !/*id        153*/
            i3=kx                                                               !/*id        154*/
         endif                                                                  !/*id        155*/
  126    continue                                                               !/*id        156*/
!                                                                               !/*id        157*/
  128    if(i1.gt.ng1(ngd)-1.or.i3.gt.ng3(ngd)) then                            !/*id        158*/
            write(6,'("One of these indices is too large!")')                   !/*id        159*/
            go to 120                                                           !/*id        160*/
         endif                                                                  !/*id        161*/
      elseif(nv.eq.3) then                                                      !/*id        162*/
  130    write(6,'("What indicies do you what for r and p?")')                  !/*id        163*/
         write(6,'(" <default = 1,1>")')                                        !/*id        164*/
         write(6,'("%")')                                                       !/*id        165*/
         read(5,'(a30)')hb                                                      !/*id        166*/
         do 132 j=1,30                                                          !/*id        167*/
         n=index('0123456789',hb(j:j))                                          !/*id        168*/
         if(n.ne.0) go to 133                                                   !/*id        169*/
  132    continue                                                               !/*id        170*/
         i1=1                                                                   !/*id        171*/
         i2=1                                                                   !/*id        172*/
         go to 138                                                              !/*id        173*/
!                                                                               !/*id        174*/
  133    kk=1                                                                   !/*id        175*/
         do 136 i=1,2                                                           !/*id        176*/
         kx=0                                                                   !/*id        177*/
         do 134 j=kk,30                                                         !/*id        178*/
         n=index('0123456789',hb(j:j))                                          !/*id        179*/
         if(n.ne.0) then                                                        !/*id        180*/
            kx=kx*10+n-1                                                        !/*id        181*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*id        182*/
            kk=j+1                                                              !/*id        183*/
            go to 135                                                           !/*id        184*/
         endif                                                                  !/*id        185*/
  134    continue                                                               !/*id        186*/
!                                                                               !/*id        187*/
  135    if(i.eq.1) then                                                        !/*id        188*/
            i1=kx                                                               !/*id        189*/
         else                                                                   !/*id        190*/
            i2=kx                                                               !/*id        191*/
         endif                                                                  !/*id        192*/
  136    continue                                                               !/*id        193*/
!                                                                               !/*id        194*/
  138    if(i1.gt.ng1(ngd)-1.or.i2.gt.ng2(ngd)) then                            !/*id        195*/
            write(6,'("One of these indices is too large!")')                   !/*id        196*/
            go to 130                                                           !/*id        197*/
         endif                                                                  !/*id        198*/
      else                                                                      !/*id        199*/
         go to 100                                                              !/*id        200*/
      endif                                                                     !/*id        201*/
!                                                                               !/*id        202*/
!       nv is the number of the coordinate varibile that will                   !/*id        203*/
!       looped over.   1,2,3 - r,p,a.                                           !/*id        204*/
!       The other two indices will be fixed.                                    !/*id        205*/
!                                                                               !/*id        206*/
      if(nv.eq.1) then                                                          !/*id        207*/
         do 140 i1=1,n1                                                         !/*id        208*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*id        209*/
         pdata(lpdat+i1)=gdata(lgdat+mx)                                        !/*id        210*/
         pdata(lpdat+n1+i1)=gdata(lgdat+mxgc(ngd)+mx)                           !/*id        211*/
  140    continue                                                               !/*id        212*/
!                                                                               !/*id        213*/
!       Write header and data the a formatted file for input                    !/*id        214*/
!       into the IDL utility for plotting.                                      !/*id        215*/
         write(31,'(a80)')aid                                                   !/*id        216*/
         write(31,'("This file contains one(1) dimensional ",'//                !/*id        217*/
     &   'a10," data.")')hp                                                     !/*id        218*/
         write(31,'("The energy range covered is: ",g12.6," to ",'//            !/*id        219*/
     &   'g12.6," MeV.")')ergl(ngd),ergh(ngd)                                   !/*id        220*/
         write(31,                                                              !/*id        221*/
     &   '("The number of histories run for this problem =",i10)')nps           !/*id        222*/
         write(31,'("The ",a1," index = ",i5,"  The ",a1,'//                    !/*id        223*/
     &   '" index = ",i5)')hv(2),i2,hv(3),i3                                    !/*id        224*/
         write(31,'("There are three(3) data arrays that follow",'//            !/*id        225*/
     &   '" this header.")')                                                    !/*id        226*/
         write(31,'("The 1st array contains the values of ",'//                 !/*id        227*/
     &   'a1,"(",i5,"). <format=1x,g12.6>")')hv(1),n1                           !/*id        228*/
         write(31,'("The 2nd array contains the values of ",'//                 !/*id        229*/
     &   'a10,"(",i5,"). <format=1x,g12.6>")')hf,n1                             !/*id        230*/
         write(31,'("The 3rd array contains ",'//                               !/*id        231*/
     &   'i5," error values. <format=1x,g12.6>")')n1                            !/*id        232*/
         write(31,'("***** End of header data. ***** ")')                       !/*id        233*/
         write(31,'(10000(1x,g12.6))')(pdr(lpdr+i),i=1,n1)                      !/*id        234*/
         write(31,'(10000(1x,g12.6))')(pdata(lpdat+i),i=1,n1)                   !/*id        235*/
         write(31,'(10000(1x,g12.6))')(pdata(lpdat+n1+i),i=1,n1)                !/*id        236*/
         close(31)                                                              !/*id        237*/
         go to 180                                                              !/*id        238*/
!                                                                               !/*id        239*/
      elseif(nv.eq.2) then                                                      !/*id        240*/
         do 150 i2=1,n2                                                         !/*id        241*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*id        242*/
         pdata(lpdat+i2)=gdata(lgdat+mx)                                        !/*id        243*/
         pdata(lpdat+n2+i2)=gdata(lgdat+mxgc(ngd)+mx)                           !/*id        244*/
  150    continue                                                               !/*id        245*/
!                                                                               !/*id        246*/
!       Write header and data the a formatted file for input                    !/*id        247*/
!       into the IDL utility for plotting.                                      !/*id        248*/
         write(31,'(a80)')aid                                                   !/*id        249*/
         write(31,'("This file contains one(1) dimensional ",'//                !/*id        250*/
     &   'a10," data.")')hp                                                     !/*id        251*/
         write(31,'("The energy range covered is: ",g12.6," to ",'//            !/*id        252*/
     &   'g12.6," MeV.")')ergl(ngd),ergh(ngd)                                   !/*id        253*/
         write(31,                                                              !/*id        254*/
     &   '("The number of histories run for this problem =",i10)')nps           !/*id        255*/
         write(31,'("The ",a1," index = ",i5,"  The ",a1,'//                    !/*id        256*/
     &   '" index = ",i5)')hv(1),i1,hv(3),i3                                    !/*id        257*/
         write(31,'("There are three(3) data arrays that follow",'//            !/*id        258*/
     &   '" this header.")')                                                    !/*id        259*/
         write(31,'("The 1st array contains the values of ",'//                 !/*id        260*/
     &   'a1,"(",i5,"). <format=1x,g12.6>")')hv(2),n2                           !/*id        261*/
         write(31,'("The 2nd array contains the values of ",'//                 !/*id        262*/
     &   'a10,"(",i5,"). <format=1x,g12.6>")')hf,n2                             !/*id        263*/
         write(31,'("The 3rd array contains ",'//                               !/*id        264*/
     &   'i5," error values. <format=1x,g12.6>")')n2                            !/*id        265*/
         write(31,'("***** End of header data. ***** ")')                       !/*id        266*/
         write(31,'(10000(1x,g12.6))')(pdp(lpdp+i),i=1,n2)                      !/*id        267*/
         write(31,'(10000(1x,g12.6))')(pdata(lpdat+i),i=1,n2)                   !/*id        268*/
         write(31,'(10000(1x,g12.6))')(pdata(lpdat+n2+i),i=1,n2)                !/*id        269*/
         close(31)                                                              !/*id        270*/
         go to 180                                                              !/*id        271*/
!                                                                               !/*id        272*/
      elseif(nv.eq.3) then                                                      !/*id        273*/
         do 160 i3=1,n3                                                         !/*id        274*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*id        275*/
         pdata(lpdat+i3)=gdata(lgdat+mx)                                        !/*id        276*/
         pdata(lpdat+n3+i3)=gdata(lgdat+mxgc(ngd)+mx)                           !/*id        277*/
  160    continue                                                               !/*id        278*/
!                                                                               !/*id        279*/
!       Write header and data the a formatted file for input                    !/*id        280*/
!       into the IDL utility for plotting.                                      !/*id        281*/
         write(31,'(a80)')aid                                                   !/*id        282*/
         write(31,'("This file contains one(1) dimensional ",'//                !/*id        283*/
     &   'a10," data.")')hp                                                     !/*id        284*/
         write(31,'("The energy range covered is: ",g12.6," to ",'//            !/*id        285*/
     &   'g12.6," MeV.")')ergl(ngd),ergh(ngd)                                   !/*id        286*/
         write(31,                                                              !/*id        287*/
     &   '("The number of histories run for this problem =",i10)')nps           !/*id        288*/
         write(31,'("The ",a1," index = ",i5,"  The ",a1,'//                    !/*id        289*/
     &   '" index = ",i5)')hv(1),i1,hv(2),i2                                    !/*id        290*/
         write(31,'("There are three(3) data arrays that follow",'//            !/*id        291*/
     &   '" this header.")')                                                    !/*id        292*/
         write(31,'("The 1st array contains the values of ",'//                 !/*id        293*/
     &   'a1,"(",i5,"). <format=1x,g12.6>")')hv(3),n3                           !/*id        294*/
         write(31,'("The 2nd array contains the values of ",'//                 !/*id        295*/
     &   'a10,"(",i5,"). <format=1x,g12.6>")')hf,n3                             !/*id        296*/
         write(31,'("The 3rd array contains ",'//                               !/*id        297*/
     &   'i5," error values. <format=1x,g12.6>")')n3                            !/*id        298*/
         write(31,'("***** End of header data. ***** ")')                       !/*id        299*/
         write(31,'(10000(1x,g12.6))')(pda(lpda+i),i=1,n3)                      !/*id        300*/
         write(31,'(10000(1x,g12.6))')(pdata(lpdat+i),i=1,n3)                   !/*id        301*/
         write(31,'(10000(1x,g12.6))')(pdata(lpdat+n3+i),i=1,n3)                !/*id        302*/
         close(31)                                                              !/*id        303*/
!                                                                               !/*id        304*/
      endif                                                                     !/*id        305*/
!                                                                               !/*id        306*/
  180 return                                                                    !/*id        307*/
!                                                                               !/*id        308*/
!       Two dinemsional plot data.                                              !/*id        309*/
!       Get dependent variable and index of other one.                          !/*id        310*/
  200 ni=0                                                                      !/*id        311*/
      write(6,'("Plot data as fcn. of rp, pa, ar or q to exit ?")')             !/*id        312*/
      write(6,'("Default = rp")')                                               !/*id        313*/
      write(6,'("%")')                                                          !/*id        314*/
      read(5,'(a3)')hi                                                          !/*id        315*/
      if(hi(1:1).eq.'q')go to 500                                               !/*id        316*/
      if(hi(1:2).eq.'  ')hi(1:2)='rp'                                           !/*id        317*/
      if(hi(1:2).eq.'rp') then                                                  !/*id        318*/
         ni=1                                                                   !/*id        319*/
  210    write(6,'("What indicies do you what for a?")')                        !/*id        320*/
         write(6,'(" <default = 1>")')                                          !/*id        321*/
         write(6,'("%")')                                                       !/*id        322*/
         read(5,'(a30)')hb                                                      !/*id        323*/
         kx=0                                                                   !/*id        324*/
         do 215 j=1,30                                                          !/*id        325*/
         n=index('0123456789',hb(j:j))                                          !/*id        326*/
         if(n.ne.0) then                                                        !/*id        327*/
            kx=kx*10+n-1                                                        !/*id        328*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*id        329*/
            i3=kx                                                               !/*id        330*/
            go to 218                                                           !/*id        331*/
         endif                                                                  !/*id        332*/
  215    continue                                                               !/*id        333*/
         i3=1                                                                   !/*id        334*/
!                                                                               !/*id        335*/
  218    if(i3.gt.ng3(ngd)) then                                                !/*id        336*/
            write(6,'("This index is too large!")')                             !/*id        337*/
            go to 210                                                           !/*id        338*/
         endif                                                                  !/*id        339*/
      elseif(hi(1:2).eq.'pa') then                                              !/*id        340*/
         ni=2                                                                   !/*id        341*/
  220    write(6,'("What indicies do you what for r?")')                        !/*id        342*/
         write(6,'(" <default = 1>")')                                          !/*id        343*/
         write(6,'("%")')                                                       !/*id        344*/
         read(5,'(a30)')hb                                                      !/*id        345*/
         kx=0                                                                   !/*id        346*/
         do 225 j=1,30                                                          !/*id        347*/
         n=index('0123456789',hb(j:j))                                          !/*id        348*/
         if(n.ne.0) then                                                        !/*id        349*/
            kx=kx*10+n-1                                                        !/*id        350*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*id        351*/
            i1=kx                                                               !/*id        352*/
            go to 228                                                           !/*id        353*/
         endif                                                                  !/*id        354*/
  225    continue                                                               !/*id        355*/
         i1=1                                                                   !/*id        356*/
!                                                                               !/*id        357*/
  228    if(i1.gt.ng1(ngd)-1) then                                              !/*id        358*/
            write(6,'("This index is too large!")')                             !/*id        359*/
            go to 220                                                           !/*id        360*/
         endif                                                                  !/*id        361*/
      elseif(hi(1:2).eq.'ar') then                                              !/*id        362*/
         ni=3                                                                   !/*id        363*/
  230    write(6,'("What indicies do you what for p?")')                        !/*id        364*/
         write(6,'(" <default = 1>")')                                          !/*id        365*/
         write(6,'("%")')                                                       !/*id        366*/
         read(5,'(a30)')hb                                                      !/*id        367*/
         kx=0                                                                   !/*id        368*/
         do 235 j=1,30                                                          !/*id        369*/
         n=index('0123456789',hb(j:j))                                          !/*id        370*/
         if(n.ne.0) then                                                        !/*id        371*/
            kx=kx*10+n-1                                                        !/*id        372*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*id        373*/
            i2=kx                                                               !/*id        374*/
            go to 238                                                           !/*id        375*/
         endif                                                                  !/*id        376*/
  235    continue                                                               !/*id        377*/
         i2=1                                                                   !/*id        378*/
!                                                                               !/*id        379*/
  238    if(i2.gt.ng2(ngd)) then                                                !/*id        380*/
            write(6,'("This index is too large!")')                             !/*id        381*/
            go to 230                                                           !/*id        382*/
         endif                                                                  !/*id        383*/
      else                                                                      !/*id        384*/
         go to 200                                                              !/*id        385*/
      endif                                                                     !/*id        386*/
!                                                                               !/*id        387*/
!      ni is an indicator as to which two coordinate varibles will be           !/*id        388*/
!      looped over.    1,2,3  --  rp,pa,ar                                      !/*id        389*/
!      The remaining index will be fixed.                                       !/*id        390*/
      if(ni.eq.1) then                                                          !/*id        391*/
         na=n1*n2                                                               !/*id        392*/
         do 240 i2=1,n2                                                         !/*id        393*/
         do 240 i1=1,n1                                                         !/*id        394*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*id        395*/
         mz=(i2-1)*n1+i1                                                        !/*id        396*/
         pdata(lpdat+mz)=gdata(lgdat+mx)                                        !/*id        397*/
         pdata(lpdat+na+mz)=gdata(lgdat+mxgc(ngd)+mx)                           !/*id        398*/
  240   continue                                                                !/*id        399*/
!            <<<< write pdata and rp to output file here>>>                     !/*id        400*/
         write(31,'(a80)')aid                                                   !/*id        401*/
         write(31,'("This file contains two(2) dimensional ",'//                !/*id        402*/
     &    'a10," data.")')hp                                                    !/*id        403*/
         write(31,'("The energy range covered is: ",g12.6," to ",'//            !/*id        404*/
     &    'g12.6," MeV.")')ergl(ngd),ergh(ngd)                                  !/*id        405*/
         write(31,                                                              !/*id        406*/
     &   '("The number of histories run for this problem =",i10)')nps           !/*id        407*/
         write(31,'("The ",a1," index was set to ",i5)')hv(3),i3                !/*id        408*/
         write(31,'("There are four(4) data arrays that follow",'//             !/*id        409*/
     &    '" this header.")')                                                   !/*id        410*/
         write(31,'("The 1st array contains the values of ",'//                 !/*id        411*/
     &    'a1,"(",i5,"). <format=1x,g12.6>")')hv(1),n1                          !/*id        412*/
         write(31,'("The 2nd array contains the values of ",'//                 !/*id        413*/
     &    'a1,"(",i5,"). <format=1x,g12.6>")')hv(2),n2                          !/*id        414*/
         write(31,'("The 3rd array contains the values of ",'//                 !/*id        415*/
     &    'a10,"(",i5,"x",i5,"). <format=1x,g12.6>")')hf,n1,n2                  !/*id        416*/
         write(31,'("The 4th array contains ",'//                               !/*id        417*/
     &    'i5,"x",i5," error values. <format=1x,g12.6>")')n1,n2                 !/*id        418*/
         write(31,'("***** End of header data. ***** ")')                       !/*id        419*/
         write(31,'(10000(1x,g12.6))')(pdr(lpdr+i),i=1,n1)                      !/*id        420*/
         write(31,'(10000(1x,g12.6))')(pdp(lpdp+i),i=1,n2)                      !/*id        421*/
         do 245 j=1,n2                                                          !/*id        422*/
  245    write(31,'(10000(1x,g12.6))')                                          !/*id        423*/
     &    (pdata(lpdat+(j-1)*n1+i),i=1,n1)                                      !/*id        424*/
         do 247 j=1,n2                                                          !/*id        425*/
  247    write(31,'(10000(1x,g12.6))')                                          !/*id        426*/
     &    (pdata(lpdat+na+(j-1)*n1+i),i=1,n1)                                   !/*id        427*/
         close(31)                                                              !/*id        428*/
         return                                                                 !/*id        429*/
!                                                                               !/*id        430*/
      elseif(ni.eq.2) then                                                      !/*id        431*/
         na=n2*n3                                                               !/*id        432*/
         do 250 i3=1,n3                                                         !/*id        433*/
         do 250 i2=1,n2                                                         !/*id        434*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*id        435*/
         mz=(i2-1)*n3+i3                                                        !/*id        436*/
         pdata(lpdat+mz)=gdata(lgdat+mx)                                        !/*id        437*/
         pdata(lpdat+na+mz)=gdata(lgdat+mxgc(ngd)+mx)                           !/*id        438*/
  250    continue                                                               !/*id        439*/
!            <<<< write pdata and pa to output file here>>>                     !/*id        440*/
         write(31,'(a80)')aid                                                   !/*id        441*/
         write(31,'("This file contains two(2) dimensional ",'//                !/*id        442*/
     &    'a10," data.")')hp                                                    !/*id        443*/
         write(31,'("The energy range covered is: ",g12.6," to ",'//            !/*id        444*/
     &    'g12.6," MeV.")')ergl(ngd),ergh(ngd)                                  !/*id        445*/
         write(31,                                                              !/*id        446*/
     &   '("The number of histories run for this problem =",i10)')nps           !/*id        447*/
         write(31,'("The ",a1," index was set to ",i5)')hv(1),i1                !/*id        448*/
         write(31,'("There are four(4) data arrays that follow",'//             !/*id        449*/
     &    '" this header.")')                                                   !/*id        450*/
         write(31,'("The 1st array contains the values of ",'//                 !/*id        451*/
     &    'a1,"(",i5,"). <format=1x,g12.6>")')hv(3),n3                          !/*id        452*/
         write(31,'("The 2nd array contains the values of ",'//                 !/*id        453*/
     &    'a1,"(",i5,"). <format=1x,g12.6>")')hv(2),n2                          !/*id        454*/
         write(31,'("The 3rd array contains the values of ",'//                 !/*id        455*/
     &    'a10,"(",i5,"x",i5,"). <format=1x,g12.6>")')hf,n3,n2                  !/*id        456*/
         write(31,'("The 4th array contains ",'//                               !/*id        457*/
     &    'i5,"x",i5," error values. <format=1x,g12.6>")')n3,n2                 !/*id        458*/
         write(31,'("***** End of header data. ***** ")')                       !/*id        459*/
         write(31,'(10000(1x,g12.6))')(pda(lpda+i),i=1,n3)                      !/*id        460*/
         write(31,'(10000(1x,g12.6))')(pdp(lpdp+i),i=1,n2)                      !/*id        461*/
         do 255 k=1,n2                                                          !/*id        462*/
  255    write(31,'(10000(1x,g12.6))')                                          !/*id        463*/
     &   (pdata(lpdat+(k-1)*n3+j),j=1,n3)                                       !/*id        464*/
         do 257 k=1,n2                                                          !/*id        465*/
  257    write(31,'(10000(1x,g12.6))')                                          !/*id        466*/
     &   (pdata(lpdat+na+(k-1)*n3+j),j=1,n3)                                    !/*id        467*/
         close(31)                                                              !/*id        468*/
         return                                                                 !/*id        469*/
!                                                                               !/*id        470*/
      elseif(ni.eq.3) then                                                      !/*id        471*/
         na=n3*n1                                                               !/*id        472*/
         do 260 i3=1,n3                                                         !/*id        473*/
         do 260 i1=1,n1                                                         !/*id        474*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*id        475*/
         mz=(i1-1)*n3+i3                                                        !/*id        476*/
         pdata(lpdat+mz)=gdata(lgdat+mx)                                        !/*id        477*/
         pdata(lpdat+na+mz)=gdata(lgdat+mxgc(ngd)+mx)                           !/*id        478*/
  260    continue                                                               !/*id        479*/
!            <<<< write pdata and ar to output file here>>>                     !/*id        480*/
         write(31,'(a80)')aid                                                   !/*id        481*/
         write(31,'("This file contains two(2) dimensional ",'//                !/*id        482*/
     &    'a10," data.")')hp                                                    !/*id        483*/
         write(31,'("The energy range covered is: ",g12.6," to ",'//            !/*id        484*/
     &    'g12.6," MeV.")')ergl(ngd),ergh(ngd)                                  !/*id        485*/
         write(31,                                                              !/*id        486*/
     &   '("The number of histories run for this problem =",i10)')nps           !/*id        487*/
         write(31,'("The ",a1," index was set to ",i5)')hv(2),i2                !/*id        488*/
         write(31,'("There are four(4) data arrays that follow",'//             !/*id        489*/
     &    '" this header.")')                                                   !/*id        490*/
         write(31,'("The 1st array contains the values of ",'//                 !/*id        491*/
     &    'a1,"(",i5,"). <format=1x,g12.6>")')hv(3),n3                          !/*id        492*/
         write(31,'("The 2nd array contains the values of ",'//                 !/*id        493*/
     &    'a1,"(",i5,"). <format=1x,g12.6>")')hv(1),n1                          !/*id        494*/
         write(31,'("The 3rd array contains the values of ",'//                 !/*id        495*/
     &    'a10,"(",i5,"x",i5,"). <format=1x,g12.6>")')hf,n3,n1                  !/*id        496*/
         write(31,'("The 4th array contains ",'//                               !/*id        497*/
     &    'i5,"x",i5," error values. <format=1x,g12.6>")')n3,n1                 !/*id        498*/
         write(31,'("***** End of header data. ***** ")')                       !/*id        499*/
         write(31,'(10000(1x,g12.6))')(pda(lpda+i),i=1,n3)                      !/*id        500*/
         write(31,'(10000(1x,g12.6))')(pdr(lpdr+i),i=1,n1)                      !/*id        501*/
         do 265 k=1,n1                                                          !/*id        502*/
  265    write(31,'(10000(1x,g12.6))')                                          !/*id        503*/
     &   (pdata(lpdat+(k-1)*n3+i),i=1,n3)                                       !/*id        504*/
         do 267 k=1,n1                                                          !/*id        505*/
  267    write(31,'(10000(1x,g12.6))')                                          !/*id        506*/
     &   (pdata(lpdat+na+(k-1)*n3+i),i=1,n3)                                    !/*id        507*/
         close(31)                                                              !/*id        508*/
         return                                                                 !/*id        509*/
      endif                                                                     !/*id        510*/
!                                                                               !/*id        511*/
  500 return                                                                    !/*id        512*/
!                                                                               !/*id        513*/
      end                                                                       !/*id        514*/
