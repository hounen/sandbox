!     $Id: idldat.F,v 1.5 2002/03/27 18:13:51 gwm Exp $                       !/*id          1*/
      subroutine idldat                                                         !/*id          2*/
!                                                                               !/*id          3*/
!       This subroutine is designed to convert input data from a                !/*id          4*/
!       mesh tally set to a data set compatible with the input                  !/*id          5*/
!       requirements of the IDL graphics package.                               !/*id          6*/
!                                                                               !/*id          7*/
!       Written by E. C. Snow  2/9/98.   (revised: 02/23/99)                    !/*id          8*/
!                                                                               !/*id          9*/
#include <gridconv/gcdouble.h>                                                  /*!id         10*/
#include <gridconv/gccom5.h>                                                    /*!id         11*/
!                                                                               !/*id         12*/
      dimension gdx(1),gdy(1),gdz(1),gdata(1),pdx(1),pdy(1),                    !/*id         13*/
     & pdz(1),pdata(1)                                                          !/*id         14*/
      equivalence (das,gdx,gdy,gdz,gdata,pdx,pdy,pdz,pdata)                     !/*id         15*/
!                                                                               !/*id         16*/
      character hi*3,hv(3)*1,hx*5,hp*10,hf*10,hb*30                             !/*id         17*/
      data hv/'X','Y','Z'/                                                      !/*id         18*/
      data hx/'X,Y,Z'/                                                          !/*id         19*/
!                                                                               !/*id         20*/
      write(6,'("What IDL filename do you want.")')                             !/*id         21*/
      write(6,'("%")')                                                          !/*id         22*/
      read(5,'(a30)')hfp                                                        !/*id         23*/
      j=leng(hfp)                                                               !/*id         24*/
      if(j.eq.0) then                                                           !/*id         25*/
         j=8                                                                    !/*id         26*/
         hfp(1:8)='data.idl'                                                    !/*id         27*/
         write(6,'("Using default output file name: data.idl")')                !/*id         28*/
      endif                                                                     !/*id         29*/
!                                                                               !/*id         30*/
   20 call unique(hfp,6)                                                        !/*id         31*/
      open(31,file=hfp,form='formatted',status='new')                           !/*id         32*/
!                                                                               !/*id         33*/
   30 write(6,'("Enter plot dimensionality, 1,2,3 or 9 to exit")')              !/*id         34*/
      write(6,'("%")')                                                          !/*id         35*/
      read(5,'(a30)')hb                                                         !/*id         36*/
      kx=0                                                                      !/*id         37*/
      np=0                                                                      !/*id         38*/
      do 35 j=1,30                                                              !/*id         39*/
      n=index('0123456789',hb(j:j))                                             !/*id         40*/
      if(n.ne.0) then                                                           !/*id         41*/
         kx=kx*10+n-1                                                           !/*id         42*/
      elseif(n.eq.0.and.kx.gt.0) then                                           !/*id         43*/
         np=kx                                                                  !/*id         44*/
         go to 38                                                               !/*id         45*/
      elseif(np.eq.0.and.j.eq.30) then                                          !/*id         46*/
         write(6,'(" There is no default value! ")')                            !/*id         47*/
         go to 30                                                               !/*id         48*/
      endif                                                                     !/*id         49*/
   35 continue                                                                  !/*id         50*/
!                                                                               !/*id         51*/
   38 if(np.eq.9)go to 500                                                      !/*id         52*/
      if(np.ge.1.and.np.le.3)go to 40                                           !/*id         53*/
      go to 30                                                                  !/*id         54*/
!                                                                               !/*id         55*/
   40 nx=ng1(ngd)-1                                                             !/*id         56*/
      ny=ng2(ngd)-1                                                             !/*id         57*/
      nz=ng3(ngd)-1                                                             !/*id         58*/
      nt=mod(ngtyp(ngd),10)                                                     !/*id         59*/
      if(nt.eq.0) then                                                          !/*id         60*/
         hp=hip(igpt(ngd))                                                      !/*id         61*/
         hf=hdx(ngd,nxv)                                                        !/*id         62*/
      elseif(nt.eq.1) then                                                      !/*id         63*/
         hp=hdx(ngd,nxv)                                                        !/*id         64*/
         hf='source pts'                                                        !/*id         65*/
      elseif(nt.eq.2) then                                                      !/*id         66*/
         hp=hdd(ngv(ngd,nxv))                                                   !/*id         67*/
         hf='ergdeposit'                                                        !/*id         68*/
      elseif(nt.eq.3) then                                                      !/*id         69*/
         hp=hip(igpt(ngd))                                                      !/*id         70*/
         hf='PD tracks.'                                                        !/*id         71*/
      elseif(nt.eq.4) then                                                      !/*id         72*/
         hp=hip(igpt(ngd))                                                      !/*id         73*/
         hf='DX tracks.'                                                        !/*id         74*/
      endif                                                                     !/*id         75*/
      go to (100,200,300)np                                                     !/*id         76*/
!                                                                               !/*id         77*/
!       One dinemsional plot data.                                              !/*id         78*/
!       Get dependent variable and index of other two.                          !/*id         79*/
  100 nv=0                                                                      !/*id         80*/
      write(6,'("Plot data as fcn. of x, y, z, or q to exit?")')                !/*id         81*/
      write(6,'("%")')                                                          !/*id         82*/
      read(5,'(a3)')hi                                                          !/*id         83*/
      if(hi(1:1).eq.'q')go to 500                                               !/*id         84*/
      if(hi(1:1).eq.'x')nv=1                                                    !/*id         85*/
      if(hi(1:1).eq.'y')nv=2                                                    !/*id         86*/
      if(hi(1:1).eq.'z')nv=3                                                    !/*id         87*/
!                                                                               !/*id         88*/
      if(nv.eq.1) then                                                          !/*id         89*/
  110    write(6,'("What indicies do you what for y and z?")')                  !/*id         90*/
         write(6,'(" <default = 1,1>")')                                        !/*id         91*/
         write(6,'("%")')                                                       !/*id         92*/
         read(5,'(a30)')hb                                                      !/*id         93*/
         do 112 j=1,30                                                          !/*id         94*/
         n=index('0123456789',hb(j:j))                                          !/*id         95*/
         if(n.ne.0) go to 113                                                   !/*id         96*/
  112    continue                                                               !/*id         97*/
         iy=1                                                                   !/*id         98*/
         iz=1                                                                   !/*id         99*/
         go to 118                                                              !/*id        100*/
!                                                                               !/*id        101*/
  113    kk=1                                                                   !/*id        102*/
         do 116 i=1,2                                                           !/*id        103*/
         kx=0                                                                   !/*id        104*/
         do 114 j=kk,30                                                         !/*id        105*/
         n=index('0123456789',hb(j:j))                                          !/*id        106*/
         if(n.ne.0) then                                                        !/*id        107*/
            kx=kx*10+n-1                                                        !/*id        108*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*id        109*/
            kk=j+1                                                              !/*id        110*/
            go to 115                                                           !/*id        111*/
         endif                                                                  !/*id        112*/
  114    continue                                                               !/*id        113*/
!                                                                               !/*id        114*/
  115    if(i.eq.1) then                                                        !/*id        115*/
            iy=kx                                                               !/*id        116*/
         else                                                                   !/*id        117*/
            iz=kx                                                               !/*id        118*/
         endif                                                                  !/*id        119*/
  116    continue                                                               !/*id        120*/
!                                                                               !/*id        121*/
  118    if(iy.gt.ng2(ngd)-1.or.iz.gt.ng3(ngd)-1) then                          !/*id        122*/
            write(6,'("One of these indices is too large!")')                   !/*id        123*/
            go to 110                                                           !/*id        124*/
         endif                                                                  !/*id        125*/
      elseif(nv.eq.2) then                                                      !/*id        126*/
  120    write(6,'("What indicies do you what for x and z?")')                  !/*id        127*/
         write(6,'(" <default = 1,1>")')                                        !/*id        128*/
         write(6,'("%")')                                                       !/*id        129*/
         read(5,'(a30)')hb                                                      !/*id        130*/
         do 122 j=1,30                                                          !/*id        131*/
         n=index('0123456789',hb(j:j))                                          !/*id        132*/
         if(n.ne.0) go to 123                                                   !/*id        133*/
  122    continue                                                               !/*id        134*/
         ix=1                                                                   !/*id        135*/
         iz=1                                                                   !/*id        136*/
         go to 128                                                              !/*id        137*/
!                                                                               !/*id        138*/
  123    kk=1                                                                   !/*id        139*/
         do 126 i=1,2                                                           !/*id        140*/
         kx=0                                                                   !/*id        141*/
         do 124 j=kk,30                                                         !/*id        142*/
         n=index('0123456789',hb(j:j))                                          !/*id        143*/
         if(n.ne.0) then                                                        !/*id        144*/
            kx=kx*10+n-1                                                        !/*id        145*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*id        146*/
            kk=j+1                                                              !/*id        147*/
            go to 125                                                           !/*id        148*/
         endif                                                                  !/*id        149*/
  124    continue                                                               !/*id        150*/
!                                                                               !/*id        151*/
  125    if(i.eq.1) then                                                        !/*id        152*/
            ix=kx                                                               !/*id        153*/
         else                                                                   !/*id        154*/
            iz=kx                                                               !/*id        155*/
         endif                                                                  !/*id        156*/
  126    continue                                                               !/*id        157*/
!                                                                               !/*id        158*/
  128    if(ix.gt.ng1(ngd)-1.or.iz.gt.ng3(ngd)-1) then                          !/*id        159*/
            write(6,'("One of these indices is too large!")')                   !/*id        160*/
            go to 120                                                           !/*id        161*/
         endif                                                                  !/*id        162*/
      else                                                                      !/*id        163*/
  130    write(6,'("What indicies do you what for x and y?")')                  !/*id        164*/
         write(6,'(" <default = 1,1>")')                                        !/*id        165*/
         write(6,'("%")')                                                       !/*id        166*/
         read(5,'(a30)')hb                                                      !/*id        167*/
         do 132 j=1,30                                                          !/*id        168*/
         n=index('0123456789',hb(j:j))                                          !/*id        169*/
         if(n.ne.0) go to 133                                                   !/*id        170*/
  132    continue                                                               !/*id        171*/
         ix=1                                                                   !/*id        172*/
         iy=1                                                                   !/*id        173*/
         go to 138                                                              !/*id        174*/
!                                                                               !/*id        175*/
  133    kk=1                                                                   !/*id        176*/
         do 136 i=1,2                                                           !/*id        177*/
         kx=0                                                                   !/*id        178*/
         do 134 j=kk,30                                                         !/*id        179*/
         n=index('0123456789',hb(j:j))                                          !/*id        180*/
         if(n.ne.0) then                                                        !/*id        181*/
            kx=kx*10+n-1                                                        !/*id        182*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*id        183*/
            kk=j+1                                                              !/*id        184*/
            go to 135                                                           !/*id        185*/
         endif                                                                  !/*id        186*/
  134    continue                                                               !/*id        187*/
!                                                                               !/*id        188*/
  135    if(i.eq.1) then                                                        !/*id        189*/
            ix=kx                                                               !/*id        190*/
         else                                                                   !/*id        191*/
            iy=kx                                                               !/*id        192*/
         endif                                                                  !/*id        193*/
  136    continue                                                               !/*id        194*/
!                                                                               !/*id        195*/
  138    if(ix.gt.ng1(ngd)-1.or.iy.gt.ng2(ngd)-1) then                          !/*id        196*/
            write(6,'("One of these indices is too large!")')                   !/*id        197*/
            go to 130                                                           !/*id        198*/
         endif                                                                  !/*id        199*/
      endif                                                                     !/*id        200*/
!                                                                               !/*id        201*/
!       nv is the number of the coordinate varibile that will                   !/*id        202*/
!       looped over.   1,2,3 - x,y,z.                                           !/*id        203*/
!       The other two indices will be fixed.                                    !/*id        204*/
!                                                                               !/*id        205*/
      if(nv.eq.1) then                                                          !/*id        206*/
         do 140 ix=1,nx                                                         !/*id        207*/
         mx=(iz-1)*nx*ny+(iy-1)*nx+ix                                           !/*id        208*/
         pdata(lpdat+ix)=gdata(lgdat+mx)                                        !/*id        209*/
         pdata(lpdat+nx+ix)=gdata(lgdat+mxgc(ngd)+mx)                           !/*id        210*/
  140    continue                                                               !/*id        211*/
!                                                                               !/*id        212*/
!       Write header and data the a formatted file for input                    !/*id        213*/
!       into the IDL utility for plotting.                                      !/*id        214*/
      write(31,'(a80)')aid                                                      !/*id        215*/
      write(31,                                                                 !/*id        216*/
     & '("This file contains one(1) dimensional ",a10," data.")')hp             !/*id        217*/
      write(31,                                                                 !/*id        218*/
     & '("The energy range covered is: ",g12.6," to ",g12.6," MeV.")')          !/*id        219*/
     &  ergl(ngd),ergh(ngd)                                                     !/*id        220*/
      write(31,'("The number of histories run for this problem =",i10)')        !/*id        221*/
     &  nps                                                                     !/*id        222*/
      write(31,'("The ",a1," index = ",i5,"  The ",a1," index = ",i5)')         !/*id        223*/
     &  hv(2),iy,hv(3),iz                                                       !/*id        224*/
      write(31,                                                                 !/*id        225*/
     & '("There are three(3) data arrays that follow this header.")')           !/*id        226*/
      write(31,                                                                 !/*id        227*/
     & '("The 1st array contains the values of ",a1,"(",i5,"). '//              !/*id        228*/
     & '<format=1x,g12.6>")')hv(1),nx                                           !/*id        229*/
      write(31,                                                                 !/*id        230*/
     & '("The 2nd array contains the values of ",a10,"(",i5,"). '//             !/*id        231*/
     & '<format=1x,g12.6>")')hf,nx                                              !/*id        232*/
      write(31,'("The 3rd array contains ",i5," error values. '//               !/*id        233*/
     & '<format=1x,g12.6>")')nx                                                 !/*id        234*/
      write(31,'("***** End of header data. ***** ")')                          !/*id        235*/
      write(31,'(10000(1x,g12.6))')(pdx(lpdx+i),i=1,nx)                         !/*id        236*/
      write(31,'(10000(1x,g12.6))')(pdata(lpdat+i),i=1,nx)                      !/*id        237*/
      write(31,'(10000(1x,g12.6))')(pdata(lpdat+nx+i),i=1,nx)                   !/*id        238*/
      close(31)                                                                 !/*id        239*/
      go to 180                                                                 !/*id        240*/
!                                                                               !/*id        241*/
      elseif(nv.eq.2) then                                                      !/*id        242*/
         do 150 iy=1,ny                                                         !/*id        243*/
         mx=(iz-1)*nx*ny+(iy-1)*nx+ix                                           !/*id        244*/
         pdata(lpdat+iy)=gdata(lgdat+mx)                                        !/*id        245*/
         pdata(lpdat+ny+iy)=gdata(lgdat+mxgc(ngd)+mx)                           !/*id        246*/
  150    continue                                                               !/*id        247*/
!                                                                               !/*id        248*/
!       Write header and data the a formatted file for input                    !/*id        249*/
!       into the IDL utility for plotting.                                      !/*id        250*/
      write(31,'(a80)')aid                                                      !/*id        251*/
      write(31,                                                                 !/*id        252*/
     & '("This file contains one(1) dimensional ",a10," data.")')hp             !/*id        253*/
      write(31,                                                                 !/*id        254*/
     & '("The energy range covered is: ",g12.6," to ",g12.6," MeV.")')          !/*id        255*/
     &  ergl(ngd),ergh(ngd)                                                     !/*id        256*/
      write(31,                                                                 !/*id        257*/
     & '("The number of histories run for this problem =",i10)')nps             !/*id        258*/
      write(31,                                                                 !/*id        259*/
     & '("The ",a1," index = ",i5,"  The ",a1," index = ",i5)')                 !/*id        260*/
     & hv(1),ix,hv(3),iz                                                        !/*id        261*/
      write(31,                                                                 !/*id        262*/
     & '("There are three(3) data arrays that follow this header.")')           !/*id        263*/
      write(31,                                                                 !/*id        264*/
     & '("The 1st array contains the values of ",a1,"(",i5,"). '//              !/*id        265*/
     & '<format=1x,g12.6>")')hv(2),ny                                           !/*id        266*/
      write(31,                                                                 !/*id        267*/
     & '("The 2nd array contains the values of ",a10,"(",i5,"). '//             !/*id        268*/
     & '<format=1x,g12.6>")')hf,ny                                              !/*id        269*/
      write(31,'("The 3rd array contains ",i5," error values. '//               !/*id        270*/
     & '<format=1x,g12.6>")')ny                                                 !/*id        271*/
      write(31,'("***** End of header data. ***** ")')                          !/*id        272*/
      write(31,'(10000(1x,g12.6))')(pdy(lpdy+i),i=1,ny)                         !/*id        273*/
      write(31,'(10000(1x,g12.6))')(pdata(lpdat+i),i=1,ny)                      !/*id        274*/
      write(31,'(10000(1x,g12.6))')(pdata(lpdat+ny+i),i=1,ny)                   !/*id        275*/
      close(31)                                                                 !/*id        276*/
      go to 180                                                                 !/*id        277*/
!                                                                               !/*id        278*/
      else                                                                      !/*id        279*/
         do 160 iz=1,nz                                                         !/*id        280*/
         mx=(iz-1)*nx*ny+(iy-1)*nx+ix                                           !/*id        281*/
         pdata(lpdat+iz)=gdata(lgdat+mx)                                        !/*id        282*/
         pdata(lpdat+nz+iz)=gdata(lgdat+mxgc(ngd)+mx)                           !/*id        283*/
  160    continue                                                               !/*id        284*/
!                                                                               !/*id        285*/
!       Write header and data the a formatted file for input                    !/*id        286*/
!       into the IDL utility for plotting.                                      !/*id        287*/
      write(31,'(a80)')aid                                                      !/*id        288*/
      write(31,                                                                 !/*id        289*/
     & '("This file contains one(1) dimensional ",a10," data.")')hp             !/*id        290*/
      write(31,                                                                 !/*id        291*/
     & '("The energy range covered is: ",g12.6," to ",g12.6," MeV.")')          !/*id        292*/
     &  ergl(ngd),ergh(ngd)                                                     !/*id        293*/
      write(31,                                                                 !/*id        294*/
     & '("The number of histories run for this problem =",i10)')nps             !/*id        295*/
      write(31,                                                                 !/*id        296*/
     & '("The ",a1," index = ",i5,"  The ",a1," index = ",i5)')                 !/*id        297*/
     &  hv(1),ix,hv(2),iy                                                       !/*id        298*/
      write(31,                                                                 !/*id        299*/
     & '("There are three(3) data arrays that follow this header.")')           !/*id        300*/
      write(31,                                                                 !/*id        301*/
     & '("The 1st array contains the values of ",a1,"(",i5,"). '//              !/*id        302*/
     & '<format=1x,g12.6>")')hv(3),nz                                           !/*id        303*/
      write(31,                                                                 !/*id        304*/
     & '("The 2nd array contains the values of ",a10,"(",i5,"). '//             !/*id        305*/
     & '<format=1x,g12.6>")')hf,nz                                              !/*id        306*/
      write(31,                                                                 !/*id        307*/
     & '("The 3rd array contains ",i5," error values. '//                       !/*id        308*/
     & '<format=1x,g12.6>")')nz                                                 !/*id        309*/
      write(31,'("***** End of header data. ***** ")')                          !/*id        310*/
      write(31,'(10000(1x,g12.6))')(pdz(lpdz+i),i=1,nz)                         !/*id        311*/
      write(31,'(10000(1x,g12.6))')(pdata(lpdat+i),i=1,nz)                      !/*id        312*/
      write(31,'(10000(1x,g12.6))')(pdata(lpdat+nz+i),i=1,nz)                   !/*id        313*/
      close(31)                                                                 !/*id        314*/
!                                                                               !/*id        315*/
      endif                                                                     !/*id        316*/
!                                                                               !/*id        317*/
  180 return                                                                    !/*id        318*/
!                                                                               !/*id        319*/
!       Two dinemsional plot data.                                              !/*id        320*/
!       Get dependent variable and index of other one.                          !/*id        321*/
  200 ni=0                                                                      !/*id        322*/
      write(6,'("Plot data as fcn. of xy, yz, xz or q to exit ?")')             !/*id        323*/
      write(6,'("%")')                                                          !/*id        324*/
      read(5,'(a3)')hi                                                          !/*id        325*/
      if(hi(1:1).eq.'q')go to 500                                               !/*id        326*/
      if(hi(1:2).eq.'xy') then                                                  !/*id        327*/
         ni=1                                                                   !/*id        328*/
  210    write(6,'("What indicies do you what for z?")')                        !/*id        329*/
         write(6,'(" <default = 1>")')                                          !/*id        330*/
         write(6,'("%")')                                                       !/*id        331*/
         read(5,'(a30)')hb                                                      !/*id        332*/
         kx=0                                                                   !/*id        333*/
         do 215 j=1,30                                                          !/*id        334*/
         n=index('0123456789',hb(j:j))                                          !/*id        335*/
         if(n.ne.0) then                                                        !/*id        336*/
            kx=kx*10+n-1                                                        !/*id        337*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*id        338*/
            iz=kx                                                               !/*id        339*/
            go to 218                                                           !/*id        340*/
         endif                                                                  !/*id        341*/
  215    continue                                                               !/*id        342*/
         iz=1                                                                   !/*id        343*/
!                                                                               !/*id        344*/
  218    if(iz.gt.ng3(ngd)-1) then                                              !/*id        345*/
            write(6,'("This index is too large!")')                             !/*id        346*/
            go to 210                                                           !/*id        347*/
         endif                                                                  !/*id        348*/
      elseif(hi(1:2).eq.'yz') then                                              !/*id        349*/
         ni=2                                                                   !/*id        350*/
  220    write(6,'("What indicies do you what for x?")')                        !/*id        351*/
         write(6,'(" <default = 1>")')                                          !/*id        352*/
         write(6,'("%")')                                                       !/*id        353*/
         read(5,'(a30)')hb                                                      !/*id        354*/
         kx=0                                                                   !/*id        355*/
         do 225 j=1,30                                                          !/*id        356*/
         n=index('0123456789',hb(j:j))                                          !/*id        357*/
         if(n.ne.0) then                                                        !/*id        358*/
            kx=kx*10+n-1                                                        !/*id        359*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*id        360*/
            ix=kx                                                               !/*id        361*/
            go to 228                                                           !/*id        362*/
         endif                                                                  !/*id        363*/
  225    continue                                                               !/*id        364*/
         ix=1                                                                   !/*id        365*/
!                                                                               !/*id        366*/
  228    if(ix.gt.ng1(ngd)-1) then                                              !/*id        367*/
            write(6,'("This index is too large!")')                             !/*id        368*/
            go to 220                                                           !/*id        369*/
         endif                                                                  !/*id        370*/
      else                                                                      !/*id        371*/
         ni=3                                                                   !/*id        372*/
  230    write(6,'("What indicies do you what for y?")')                        !/*id        373*/
         write(6,'(" <default = 1>")')                                          !/*id        374*/
         write(6,'("%")')                                                       !/*id        375*/
         read(5,'(a30)')hb                                                      !/*id        376*/
         kx=0                                                                   !/*id        377*/
         do 235 j=1,30                                                          !/*id        378*/
         n=index('0123456789',hb(j:j))                                          !/*id        379*/
         if(n.ne.0) then                                                        !/*id        380*/
            kx=kx*10+n-1                                                        !/*id        381*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*id        382*/
            iy=kx                                                               !/*id        383*/
            go to 238                                                           !/*id        384*/
         endif                                                                  !/*id        385*/
  235    continue                                                               !/*id        386*/
         iy=1                                                                   !/*id        387*/
!                                                                               !/*id        388*/
  238    if(iy.gt.ng2(ngd)-1) then                                              !/*id        389*/
            write(6,'("This index is too large!")')                             !/*id        390*/
            go to 230                                                           !/*id        391*/
         endif                                                                  !/*id        392*/
      endif                                                                     !/*id        393*/
!                                                                               !/*id        394*/
!      ni is an indicator as to which two coordinate varibles will be           !/*id        395*/
!      looped over.    1,2,3  --  xy,yz,xz                                      !/*id        396*/
!      The remaining index will be fixed.                                       !/*id        397*/
      if(ni.eq.1) then                                                          !/*id        398*/
         na=nx*ny                                                               !/*id        399*/
         do 240 iy=1,ny                                                         !/*id        400*/
         do 240 ix=1,nx                                                         !/*id        401*/
         mx=(iz-1)*nx*ny+(iy-1)*nx+ix                                           !/*id        402*/
         mz=nx*(iy-1)+ix                                                        !/*id        403*/
         pdata(lpdat+mz)=gdata(lgdat+mx)                                        !/*id        404*/
         pdata(lpdat+na+mz)=gdata(lgdat+mxgc(ngd)+mx)                           !/*id        405*/
  240    continue                                                               !/*id        406*/
!            <<<< write pdata and xy to output file here>>>                     !/*id        407*/
         write(31,'(a80)')aid                                                   !/*id        408*/
         write(31,                                                              !/*id        409*/
     & '("This file contains two(2) dimensional ",a10," data.")')hp             !/*id        410*/
         write(31,                                                              !/*id        411*/
     & '("The energy range covered is: ",g12.6," to ",g12.6," MeV.")')          !/*id        412*/
     & ergl(ngd),ergh(ngd)                                                      !/*id        413*/
         write(31,                                                              !/*id        414*/
     & '("The number of histories run for this problem =",i10)')nps             !/*id        415*/
         write(31,'("The ",a1," index was set to ",i5)')hv(3),iz                !/*id        416*/
         write(31,                                                              !/*id        417*/
     & '("There are four(4) data arrays that follow this header.")')            !/*id        418*/
         write(31,                                                              !/*id        419*/
     & '("The 1st array contains the values of ",a1,"(",i5,"). '//              !/*id        420*/
     & '<format=1x,g12.6>")')hv(1),nx                                           !/*id        421*/
         write(31,                                                              !/*id        422*/
     & '("The 2nd array contains the values of ",a1,"(",i5,"). '//              !/*id        423*/
     & '<format=1x,g12.6>")')hv(2),ny                                           !/*id        424*/
         write(31,                                                              !/*id        425*/
     & '("The 3rd array contains the values of ",a10,"(",i5,"x",'//             !/*id        426*/
     & 'i5,"). <format=1x,g12.6>")')hf,nx,ny                                    !/*id        427*/
         write(31,                                                              !/*id        428*/
     & '("The 4th array contains ",i5,"x",i5," error values. '//                !/*id        429*/
     & '<format=1x,g12.6>")')nx,ny                                              !/*id        430*/
         write(31,'("***** End of header data. ***** ")')                       !/*id        431*/
         write(31,'(10000(1x,g12.6))')(pdx(lpdx+i),i=1,nx)                      !/*id        432*/
         write(31,'(10000(1x,g12.6))')(pdy(lpdy+i),i=1,ny)                      !/*id        433*/
         do 245 j=1,ny                                                          !/*id        434*/
  245    write(31,'(10000(1x,g12.6))')                                          !/*id        435*/
     &   (pdata(lpdat+(j-1)*nx+i),i=1,nx)                                       !/*id        436*/
         do 247 j=1,ny                                                          !/*id        437*/
  247    write(31,'(10000(1x,g12.6))')                                          !/*id        438*/
     &   (pdata(lpdat+na+(j-1)*nx+i),i=1,nx)                                    !/*id        439*/
         close(31)                                                              !/*id        440*/
         return                                                                 !/*id        441*/
!                                                                               !/*id        442*/
      elseif(ni.eq.2) then                                                      !/*id        443*/
         na=ny*nz                                                               !/*id        444*/
         do 250 iz=1,nz                                                         !/*id        445*/
         do 250 iy=1,ny                                                         !/*id        446*/
         mx=(iz-1)*nx*ny+(iy-1)*nx+ix                                           !/*id        447*/
         mz=ny*(iz-1)+iy                                                        !/*id        448*/
         pdata(lpdat+mz)=gdata(lgdat+mx)                                        !/*id        449*/
         pdata(lpdat+na+mz)=gdata(lgdat+mxgc(ngd)+mx)                           !/*id        450*/
  250    continue                                                               !/*id        451*/
!            <<<< write pdata and yz to output file here>>>                     !/*id        452*/
         write(31,'(a80)')aid                                                   !/*id        453*/
         write(31,                                                              !/*id        454*/
     &   '("This file contains two(2) dimensional ",a10," data.")')             !/*id        455*/
     &    hp                                                                    !/*id        456*/
         write(31,                                                              !/*id        457*/
     &   '("The energy range covered is: ",g12.6," to ",g12.6," MeV.")')        !/*id        458*/
     &    ergl(ngd),ergh(ngd)                                                   !/*id        459*/
         write(31,                                                              !/*id        460*/
     &   '("The number of histories run for this problem =",i10)')nps           !/*id        461*/
         write(31,'("The ",a1," index was set to ",i5)')hv(1),ix                !/*id        462*/
         write(31,                                                              !/*id        463*/
     &   '("There are four(4) data arrays that follow this header.")')          !/*id        464*/
         write(31,                                                              !/*id        465*/
     &   '("The 1st array contains the values of ",a1,"(",i5,"). '//            !/*id        466*/
     &   '<format=1x,g12.6>")')hv(2),ny                                         !/*id        467*/
         write(31,                                                              !/*id        468*/
     &   '("The 2nd array contains the values of ",a1,"(",i5,"). '//            !/*id        469*/
     &   '<format=1x,g12.6>")')hv(3),nz                                         !/*id        470*/
         write(31,'("The 3rd array contains the values of ",a10,'//             !/*id        471*/
     &   '"(",i5,"x",i5,"). <format=1x,g12.6>")')hf,ny,nz                       !/*id        472*/
         write(31,                                                              !/*id        473*/
     &   '("The 4th array contains ",i5,"x",i5," error values. '//              !/*id        474*/
     &   '<format=1x,g12.6>")')ny,nz                                            !/*id        475*/
         write(31,'("***** End of header data. ***** ")')                       !/*id        476*/
         write(31,'(10000(1x,g12.6))')(pdy(lpdy+i),i=1,ny)                      !/*id        477*/
         write(31,'(10000(1x,g12.6))')(pdz(lpdz+i),i=1,nz)                      !/*id        478*/
         do 255 k=1,nz                                                          !/*id        479*/
  255    write(31,'(10000(1x,g12.6))')                                          !/*id        480*/
     &   (pdata(lpdat+(k-1)*ny+j),j=1,ny)                                       !/*id        481*/
         do 257 k=1,nz                                                          !/*id        482*/
  257    write(31,'(10000(1x,g12.6))')                                          !/*id        483*/
     &   (pdata(lpdat+na+(k-1)*ny+j),j=1,ny)                                    !/*id        484*/
         close(31)                                                              !/*id        485*/
         return                                                                 !/*id        486*/
!                                                                               !/*id        487*/
      elseif(ni.eq.3) then                                                      !/*id        488*/
         na=nx*nz                                                               !/*id        489*/
         do 260 iz=1,nz                                                         !/*id        490*/
         do 260 ix=1,nx                                                         !/*id        491*/
         mx=(iz-1)*nx*ny+(iy-1)*nx+ix                                           !/*id        492*/
         mz=nx*(iz-1)+ix                                                        !/*id        493*/
         pdata(lpdat+mz)=gdata(lgdat+mx)                                        !/*id        494*/
         pdata(lpdat+na+mz)=gdata(lgdat+mxgc(ngd)+mx)                           !/*id        495*/
  260    continue                                                               !/*id        496*/
!            <<<< write pdata and xz to output file here>>>                     !/*id        497*/
         write(31,'(a80)')aid                                                   !/*id        498*/
         write(31,                                                              !/*id        499*/
     &   '("This file contains two(2) dimensional ",a10," data.")')hp           !/*id        500*/
         write(31,                                                              !/*id        501*/
     &   '("The energy range covered is: ",g12.6," to ",g12.6," MeV.")')        !/*id        502*/
     &    ergl(ngd),ergh(ngd)                                                   !/*id        503*/
         write(31,                                                              !/*id        504*/
     &   '("The number of histories run for this problem =",i10)')nps           !/*id        505*/
         write(31,'("The ",a1," index was set to ",i5)')hv(2),iy                !/*id        506*/
         write(31,                                                              !/*id        507*/
     &   '("There are four(4) data arrays that follow this header.")')          !/*id        508*/
         write(31,                                                              !/*id        509*/
     &   '("The 1st array contains the values of ",a1,"(",i5,"). '//            !/*id        510*/
     &   '<format=1x,g12.6>")')hv(1),nx                                         !/*id        511*/
         write(31,                                                              !/*id        512*/
     &   '("The 2nd array contains the values of ",a1,"(",i5,"). '//            !/*id        513*/
     &   '<format=1x,g12.6>")')hv(3),nz                                         !/*id        514*/
         write(31,                                                              !/*id        515*/
     &   '("The 3rd array contains the values of ",a10,"(",i5,'//               !/*id        516*/
     &   '"x",i5,"). <format=1x,g12.6>")')hf,nx,nz                              !/*id        517*/
         write(31,                                                              !/*id        518*/
     &   '("The 4th array contains ",i5,"x",i5," error values. '//              !/*id        519*/
     &   '<format=1x,g12.6>")')nx,nz                                            !/*id        520*/
         write(31,'("***** End of header data. ***** ")')                       !/*id        521*/
         write(31,'(10000(1x,g12.6))')(pdx(lpdx+i),i=1,nx)                      !/*id        522*/
         write(31,'(10000(1x,g12.6))')(pdz(lpdz+i),i=1,nz)                      !/*id        523*/
         do 265 k=1,nz                                                          !/*id        524*/
  265    write(31,'(10000(1x,g12.6))')                                          !/*id        525*/
     &   (pdata(lpdat+(k-1)*nx+i),i=1,nx)                                       !/*id        526*/
         do 267 k=1,nz                                                          !/*id        527*/
  267    write(31,'(10000(1x,g12.6))')                                          !/*id        528*/
     &   (pdata(lpdat+na+(k-1)*nx+i),i=1,nx)                                    !/*id        529*/
         close(31)                                                              !/*id        530*/
         return                                                                 !/*id        531*/
!                                                                               !/*id        532*/
      endif                                                                     !/*id        533*/
      return                                                                    !/*id        534*/
!                                                                               !/*id        535*/
!       Three dinemsional plot data.                                            !/*id        536*/
!       Loops will be over all three coordinate variables.                      !/*id        537*/
!                                                                               !/*id        538*/
  300 na=nx*ny*nz                                                               !/*id        539*/
      do 310 iz=1,nz                                                            !/*id        540*/
      do 310 iy=1,ny                                                            !/*id        541*/
      do 310 ix=1,nx                                                            !/*id        542*/
      mx=(iz-1)*nx*ny+(iy-1)*nx+ix                                              !/*id        543*/
      pdata(lpdat+mx)=gdata(lgdat+mx)                                           !/*id        544*/
      pdata(lpdat+na+mx)=gdata(lgdat+mxgc(ngd)+mx)                              !/*id        545*/
  310 continue                                                                  !/*id        546*/
!         <<<<< write gdata and xyz to output file here >>>                     !/*id        547*/
      write(31,'(a80)')aid                                                      !/*id        548*/
      write(31,                                                                 !/*id        549*/
     & '("This file contains three(3) dimensional ",a10," data.")')hp           !/*id        550*/
      write(31,                                                                 !/*id        551*/
     & '("The energy range covered is: ",g12.6," to ",g12.6," MeV.")')          !/*id        552*/
     & ergl(ngd),ergh(ngd)                                                      !/*id        553*/
      write(31,                                                                 !/*id        554*/
     & '("The number of histories run for this problem =",i10)')nps             !/*id        555*/
      write(31,                                                                 !/*id        556*/
     & '("There are five(5) data arrays that follow this header.")')            !/*id        557*/
      write(31,                                                                 !/*id        558*/
     & '("The 1st array contains the values of ",a1,"(",i5,"). '//              !/*id        559*/
     & '<format=1x,g12.6>")')hv(1),nx                                           !/*id        560*/
      write(31,                                                                 !/*id        561*/
     & '("The 2nd array contains the values of ",a1,"(",i5,"). '//              !/*id        562*/
     & '<format=1x,g12.6>")')hv(2),ny                                           !/*id        563*/
      write(31,                                                                 !/*id        564*/
     & '("The 3rd array contains the values of ",a1,"(",i5,"). '//              !/*id        565*/
     & '<format=1x,g12.6>")')hv(3),nz                                           !/*id        566*/
      write(31,'("The 4th array contains the values of ",a10,"(",i5,'//         !/*id        567*/
     & '"x",i5,"x",i5,"). <format=1x,g12.6>")')hf,nx,ny,nz                      !/*id        568*/
      write(31,                                                                 !/*id        569*/
     & '("The 5th array contains ",i5,"x",i5,"x",i5," error values.'//          !/*id        570*/
     & ' <format=1x,g12.6>")')nx,ny,nz                                          !/*id        571*/
      write(31,'("***** End of header data. ***** ")')                          !/*id        572*/
      write(31,'(10000(1x,g12.6))')(pdx(lpdx+i),i=1,nx)                         !/*id        573*/
      write(31,'(10000(1x,g12.6))')(pdy(lpdy+i),i=1,ny)                         !/*id        574*/
      write(31,'(10000(1x,g12.6))')(pdz(lpdz+i),i=1,nz)                         !/*id        575*/
      do 320 k=1,nz                                                             !/*id        576*/
      do 320 j=1,ny                                                             !/*id        577*/
      mx=(k-1)*nx*ny+(j-1)*nx                                                   !/*id        578*/
  320 write(31,'(10000(1x,g12.6))')(pdata(lpdat+mx+i),i=1,nx)                   !/*id        579*/
      do 340 k=1,nz                                                             !/*id        580*/
      do 340 j=1,ny                                                             !/*id        581*/
      mx=(k-1)*nx*ny+(j-1)*nx                                                   !/*id        582*/
  340 write(31,'(10000(1x,g12.6))')(pdata(lpdat+na+mx+i),i=1,nx)                !/*id        583*/
      close(31)                                                                 !/*id        584*/
      return                                                                    !/*id        585*/
!                                                                               !/*id        586*/
  500 return                                                                    !/*id        587*/
!                                                                               !/*id        588*/
      end                                                                       !/*id        589*/
