!     $Id: pawdat.F,v 1.5 2002/03/27 18:13:53 gwm Exp $                       !/*pa          1*/
      subroutine pawdat                                                         !/*pa          2*/
#ifndef NO_PAW                                                                  /*!pa          3*/
!       This subroutine is designed to convert input data from a                !/*pa          4*/
!       mesh tally set to a data set compatible with the input                  !/*pa          5*/
!       requirements of the PAW graphics package.                               !/*pa          6*/
!                                                                               !/*pa          7*/
!       Written by E. C. Snow  2/9/98.   (revised: 11/14/2001)                  !/*pa          8*/
!                                                                               !/*pa          9*/
#include <gridconv/gcdouble.h>                                                  /*!pa         10*/
#include <gridconv/gccom5.h>                                                    /*!pa         11*/
!                                                                               !/*pa         12*/
      dimension gdx(1),gdy(1),gdz(1),gdata(1),pdx(1),pdy(1),                    !/*pa         13*/
     & pdz(1),pdata(1)                                                          !/*pa         14*/
      equivalence (das,gdx,gdy,gdz,gdata,pdx,pdy,pdz,pdata)                     !/*pa         15*/
!                                                                               !/*pa         16*/
      character hj*1,hi*3,hv(3)*1,hx*5,hf*10,hb*30                              !/*pa         17*/
      data hv/'X','Y','Z'/                                                      !/*pa         18*/
      data hx/'X,Y,Z'/                                                          !/*pa         19*/
!                                                                               !/*pa         20*/
      integer b(512000)                                                         !/*pa         21*/
      common /mibook/b                                                          !/*pa         22*/
!                                                                               !/*pa         23*/
      write(6,'("What PAW filename do you want.")')                             !/*pa         24*/
      write(6,'(" <default = data.paw>")')                                      !/*pa         25*/
      write(6,'("&")')                                                          !/*pa         26*/
      read(5,'(a30)')hfp                                                        !/*pa         27*/
      j=leng(hfp)                                                               !/*pa         28*/
      if(j.eq.0) then                                                           !/*pa         29*/
         j=8                                                                    !/*pa         30*/
         hfp(1:8)='data.paw'                                                    !/*pa         31*/
      endif                                                                     !/*pa         32*/
!                                                                               !/*pa         33*/
   20 call unique(hfp,6)                                                        !/*pa         34*/
!                                                                               !/*pa         35*/
   30 write(6,'("Enter plot dimensionality, 1,2, or 9 to exit")')               !/*pa         36*/
      write(6,'(" < No default!>")')                                            !/*pa         37*/
      write(6,'("&")')                                                          !/*pa         38*/
      read(5,'(a30)')hb                                                         !/*pa         39*/
      kx=0                                                                      !/*pa         40*/
      np=0                                                                      !/*pa         41*/
      do 35 j=1,30                                                              !/*pa         42*/
      n=index('0123456789',hb(j:j))                                             !/*pa         43*/
      if(n.ne.0) then                                                           !/*pa         44*/
         kx=kx*10+n-1                                                           !/*pa         45*/
      elseif(n.eq.0.and.kx.gt.0) then                                           !/*pa         46*/
         np=kx                                                                  !/*pa         47*/
         go to 38                                                               !/*pa         48*/
      elseif(np.eq.0.and.j.eq.30) then                                          !/*pa         49*/
         write(6,'(" There is no default value! ")')                            !/*pa         50*/
         go to 30                                                               !/*pa         51*/
      endif                                                                     !/*pa         52*/
   35 continue                                                                  !/*pa         53*/
!                                                                               !/*pa         54*/
   38 if(np.eq.9)go to 500                                                      !/*pa         55*/
      if(np.ge.1.and.np.le.2)go to 40                                           !/*pa         56*/
      if(np.eq.3) then                                                          !/*pa         57*/
         write(6,'("Three(3) dimensional plotting is not currently")')          !/*pa         58*/
         write(6,'("available with the PAW package that I have.")')             !/*pa         59*/
      endif                                                                     !/*pa         60*/
      go to 30                                                                  !/*pa         61*/
!                                                                               !/*pa         62*/
   40 nx=ng1(ngd)-1                                                             !/*pa         63*/
      ny=ng2(ngd)-1                                                             !/*pa         64*/
      nz=ng3(ngd)-1                                                             !/*pa         65*/
      nt=mod(ngtyp(ngd),10)                                                     !/*pa         66*/
      if(nt.eq.0) then                                                          !/*pa         67*/
         hf=hdx(ngd,nxv)                                                        !/*pa         68*/
      elseif(nt.eq.1) then                                                      !/*pa         69*/
         hf='source pts'                                                        !/*pa         70*/
      elseif(nt.eq.2) then                                                      !/*pa         71*/
         hf='ergdeposit'                                                        !/*pa         72*/
      elseif(nt.eq.3) then                                                      !/*pa         73*/
         hf='PD tracks.'                                                        !/*pa         74*/
      elseif(nt.eq.4) then                                                      !/*pa         75*/
         hf='DX tracks.'                                                        !/*pa         76*/
      endif                                                                     !/*pa         77*/
      go to (100,200)np                                                         !/*pa         78*/
!                                                                               !/*pa         79*/
!       One dinemsional plot data.                                              !/*pa         80*/
!       Get dependent variable and index of other two.                          !/*pa         81*/
  100 nv=0                                                                      !/*pa         82*/
      write(6,'("Plot data as fcn. of x, y, z, or q to exit?")')                !/*pa         83*/
      write(6,'(" < No default!>")')                                            !/*pa         84*/
      write(6,'("&")')                                                          !/*pa         85*/
      read(5,'(a3)')hi                                                          !/*pa         86*/
      if(hi(1:1).eq.'q')go to 500                                               !/*pa         87*/
      if(hi(1:1).eq.'x')nv=1                                                    !/*pa         88*/
      if(hi(1:1).eq.'y')nv=2                                                    !/*pa         89*/
      if(hi(1:1).eq.'z')nv=3                                                    !/*pa         90*/
!                                                                               !/*pa         91*/
      if(nv.eq.1) then                                                          !/*pa         92*/
         if(ixs.ne.1) then                                                      !/*pa         93*/
            write(6,'("X spacing must be constant for PAW")')                   !/*pa         94*/
            go to 500                                                           !/*pa         95*/
         endif                                                                  !/*pa         96*/
  110    write(6,'("What indicies do you what for y and z?")')                  !/*pa         97*/
         write(6,'(" <default = 1,1>")')                                        !/*pa         98*/
         write(6,'("&")')                                                       !/*pa         99*/
         read(5,'(a30)')hb                                                      !/*pa        100*/
         do 112 j=1,30                                                          !/*pa        101*/
         n=index('0123456789',hb(j:j))                                          !/*pa        102*/
         if(n.ne.0) go to 113                                                   !/*pa        103*/
  112    continue                                                               !/*pa        104*/
         iy=1                                                                   !/*pa        105*/
         iz=1                                                                   !/*pa        106*/
         go to 118                                                              !/*pa        107*/
!                                                                               !/*pa        108*/
  113    kk=1                                                                   !/*pa        109*/
         do 116 i=1,2                                                           !/*pa        110*/
         kx=0                                                                   !/*pa        111*/
         do 114 j=kk,30                                                         !/*pa        112*/
         n=index('0123456789',hb(j:j))                                          !/*pa        113*/
         if(n.ne.0) then                                                        !/*pa        114*/
            kx=kx*10+n-1                                                        !/*pa        115*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*pa        116*/
            kk=j+1                                                              !/*pa        117*/
            go to 115                                                           !/*pa        118*/
         endif                                                                  !/*pa        119*/
  114    continue                                                               !/*pa        120*/
!                                                                               !/*pa        121*/
  115    if(i.eq.1) then                                                        !/*pa        122*/
            iy=kx                                                               !/*pa        123*/
         else                                                                   !/*pa        124*/
            iz=kx                                                               !/*pa        125*/
         endif                                                                  !/*pa        126*/
  116    continue                                                               !/*pa        127*/
!                                                                               !/*pa        128*/
  118    if(iy.gt.ng2(ngd)-1.or.iz.gt.ng3(ngd)-1) then                          !/*pa        129*/
            write(6,'("One of these indices is too large!")')                   !/*pa        130*/
            go to 110                                                           !/*pa        131*/
         endif                                                                  !/*pa        132*/
      elseif(nv.eq.2) then                                                      !/*pa        133*/
         if(iys.ne.1) then                                                      !/*pa        134*/
            write(6,'("Y spacing must be constant for PAW")')                   !/*pa        135*/
            go to 500                                                           !/*pa        136*/
         endif                                                                  !/*pa        137*/
  120    write(6,'("What indicies do you what for x and z?")')                  !/*pa        138*/
         write(6,'(" <default = 1,1>")')                                        !/*pa        139*/
         write(6,'("&")')                                                       !/*pa        140*/
         read(5,'(a30)')hb                                                      !/*pa        141*/
         do 122 j=1,30                                                          !/*pa        142*/
         n=index('0123456789',hb(j:j))                                          !/*pa        143*/
         if(n.ne.0) go to 123                                                   !/*pa        144*/
  122    continue                                                               !/*pa        145*/
         ix=1                                                                   !/*pa        146*/
         iz=1                                                                   !/*pa        147*/
         go to 128                                                              !/*pa        148*/
!                                                                               !/*pa        149*/
  123    kk=1                                                                   !/*pa        150*/
         do 126 i=1,2                                                           !/*pa        151*/
         kx=0                                                                   !/*pa        152*/
         do 124 j=kk,30                                                         !/*pa        153*/
         n=index('0123456789',hb(j:j))                                          !/*pa        154*/
         if(n.ne.0) then                                                        !/*pa        155*/
            kx=kx*10+n-1                                                        !/*pa        156*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*pa        157*/
            kk=j+1                                                              !/*pa        158*/
            go to 125                                                           !/*pa        159*/
         endif                                                                  !/*pa        160*/
  124    continue                                                               !/*pa        161*/
!                                                                               !/*pa        162*/
  125    if(i.eq.1) then                                                        !/*pa        163*/
            ix=kx                                                               !/*pa        164*/
         else                                                                   !/*pa        165*/
            iz=kx                                                               !/*pa        166*/
         endif                                                                  !/*pa        167*/
  126    continue                                                               !/*pa        168*/
!                                                                               !/*pa        169*/
  128    if(ix.gt.ng1(ngd)-1.or.iz.gt.ng3(ngd)-1) then                          !/*pa        170*/
            write(6,'("One of these indices is too large!")')                   !/*pa        171*/
            go to 120                                                           !/*pa        172*/
         endif                                                                  !/*pa        173*/
      elseif(nv.eq.3) then                                                      !/*pa        174*/
         if(izs.ne.1) then                                                      !/*pa        175*/
            write(6,'("Z spacing must be constant for PAW")')                   !/*pa        176*/
            go to 500                                                           !/*pa        177*/
         endif                                                                  !/*pa        178*/
  130    write(6,'("What indicies do you what for x and y?")')                  !/*pa        179*/
         write(6,'(" <default = 1,1>")')                                        !/*pa        180*/
         write(6,'("&")')                                                       !/*pa        181*/
         read(5,'(a30)')hb                                                      !/*pa        182*/
         do 132 j=1,30                                                          !/*pa        183*/
         n=index('0123456789',hb(j:j))                                          !/*pa        184*/
         if(n.ne.0) go to 133                                                   !/*pa        185*/
  132    continue                                                               !/*pa        186*/
         ix=1                                                                   !/*pa        187*/
         iy=1                                                                   !/*pa        188*/
         go to 138                                                              !/*pa        189*/
!                                                                               !/*pa        190*/
  133    kk=1                                                                   !/*pa        191*/
         do 136 i=1,2                                                           !/*pa        192*/
         kx=0                                                                   !/*pa        193*/
         do 134 j=kk,30                                                         !/*pa        194*/
         n=index('0123456789',hb(j:j))                                          !/*pa        195*/
         if(n.ne.0) then                                                        !/*pa        196*/
            kx=kx*10+n-1                                                        !/*pa        197*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*pa        198*/
            kk=j+1                                                              !/*pa        199*/
            go to 135                                                           !/*pa        200*/
         endif                                                                  !/*pa        201*/
  134    continue                                                               !/*pa        202*/
!                                                                               !/*pa        203*/
  135    if(i.eq.1) then                                                        !/*pa        204*/
            ix=kx                                                               !/*pa        205*/
         else                                                                   !/*pa        206*/
            iy=kx                                                               !/*pa        207*/
         endif                                                                  !/*pa        208*/
  136    continue                                                               !/*pa        209*/
!                                                                               !/*pa        210*/
  138    if(ix.gt.ng1(ngd)-1.or.iy.gt.ng2(ngd)-1) then                          !/*pa        211*/
            write(6,'("One of these indices is too large!")')                   !/*pa        212*/
            go to 130                                                           !/*pa        213*/
         endif                                                                  !/*pa        214*/
      else                                                                      !/*pa        215*/
         go to 100                                                              !/*pa        216*/
      endif                                                                     !/*pa        217*/
!                                                                               !/*pa        218*/
!       nv is the number of the coordinate varibile that will                   !/*pa        219*/
!       looped over.   1,2,3 - x,y,z.                                           !/*pa        220*/
!       The other two indices will be fixed.                                    !/*pa        221*/
!                                                                               !/*pa        222*/
      if(nv.eq.1) then                                                          !/*pa        223*/
         cmin=gdx(lgdx+1)                                                       !/*pa        224*/
         cmax=gdx(lgdx+ng1(ngd))                                                !/*pa        225*/
!                                                                               !/*pa        226*/
         do 140 ix=1,nx                                                         !/*pa        227*/
         mx=(iz-1)*nx*ny+(iy-1)*nx+ix                                           !/*pa        228*/
         pdata(lpdat+ix)=gdata(lgdat+mx)                                        !/*pa        229*/
         pdata(lpdat+nx+ix)=gdata(lgdat+mxgc(ngd)+mx)                           !/*pa        230*/
  140    continue                                                               !/*pa        231*/
!                                                                               !/*pa        232*/
!       Write data the an unformatted file for input                            !/*pa        233*/
!       into the PAW utility for plotting.                                      !/*pa        234*/
!                                                                               !/*pa        235*/
         call hlimit(512000)                                                    !/*pa        236*/
         call hbook1(10,hf,nx,sngl(cmin),sngl(cmax),0.0)                        !/*pa        237*/
         call hbook1(11,'error',nx,sngl(cmin),sngl(cmax),0.0)                   !/*pa        238*/
         do 145 i=1,nx                                                          !/*pa        239*/
         x=pdx(lpdx+i)                                                          !/*pa        240*/
         f=pdata(lpdat+i)                                                       !/*pa        241*/
         g=pdata(lpdat+nx+i)                                                    !/*pa        242*/
         call hf1(10,sngl(x),sngl(f))                                           !/*pa        243*/
         call hf1(11,sngl(x),sngl(g))                                           !/*pa        244*/
  145    continue                                                               !/*pa        245*/
         go to 180                                                              !/*pa        246*/
!                                                                               !/*pa        247*/
      elseif(nv.eq.2) then                                                      !/*pa        248*/
         cmin=gdy(lgdy+1)                                                       !/*pa        249*/
         cmax=gdy(lgdy+ng2(ngd))                                                !/*pa        250*/
!                                                                               !/*pa        251*/
         do 150 iy=1,ny                                                         !/*pa        252*/
         mx=(iz-1)*nx*ny+(iy-1)*nx+ix                                           !/*pa        253*/
         pdata(lpdat+iy)=gdata(lgdat+mx)                                        !/*pa        254*/
         pdata(lpdat+ny+iy)=gdata(lgdat+mxgc(ngd)+mx)                           !/*pa        255*/
  150    continue                                                               !/*pa        256*/
!                                                                               !/*pa        257*/
!       Write data the an unformatted file for input                            !/*pa        258*/
!       into the PAW utility for plotting.                                      !/*pa        259*/
!                                                                               !/*pa        260*/
         call hlimit(512000)                                                    !/*pa        261*/
         call hbook1(20,hf,ny,sngl(cmin),sngl(cmax),0.0)                        !/*pa        262*/
         call hbook1(21,'error',ny,sngl(cmin),sngl(cmax),0.0)                   !/*pa        263*/
         do 155 i=1,ny                                                          !/*pa        264*/
         y=pdy(lpdy+i)                                                          !/*pa        265*/
         f=pdata(lpdat+i)                                                       !/*pa        266*/
         g=pdata(lpdat+ny+i)                                                    !/*pa        267*/
         call hf1(20,sngl(y),sngl(f))                                           !/*pa        268*/
         call hf1(21,sngl(y),sngl(g))                                           !/*pa        269*/
  155    continue                                                               !/*pa        270*/
         go to 180                                                              !/*pa        271*/
!                                                                               !/*pa        272*/
      elseif(nv.eq.3) then                                                      !/*pa        273*/
         cmin=gdz(lgdz+1)                                                       !/*pa        274*/
         cmax=gdz(lgdz+ng3(ngd))                                                !/*pa        275*/
!                                                                               !/*pa        276*/
         do 160 iz=1,nz                                                         !/*pa        277*/
         mx=(iz-1)*nx*ny+(iy-1)*nx+ix                                           !/*pa        278*/
         pdata(lpdat+iz)=gdata(lgdat+mx)                                        !/*pa        279*/
         pdata(lpdat+nz+iz)=gdata(lgdat+mxgc(ngd)+mx)                           !/*pa        280*/
  160    continue                                                               !/*pa        281*/
!                                                                               !/*pa        282*/
!       Write data the an unformatted file for input                            !/*pa        283*/
!       into the PAW utility for plotting.                                      !/*pa        284*/
!                                                                               !/*pa        285*/
         call hlimit(512000)                                                    !/*pa        286*/
         call hbook1(30,hf,nz,sngl(cmin),sngl(cmax),0.0)                        !/*pa        287*/
         call hbook1(31,'error',nz,sngl(cmin),sngl(cmax),0.0)                   !/*pa        288*/
         do 165 i=1,nz                                                          !/*pa        289*/
         z=pdz(lpdz+i)                                                          !/*pa        290*/
         f=pdata(lpdat+i)                                                       !/*pa        291*/
         g=pdata(lpdat+nz+i)                                                    !/*pa        292*/
         call hf1(30,sngl(z),sngl(f))                                           !/*pa        293*/
         call hf1(31,sngl(z),sngl(g))                                           !/*pa        294*/
  165    continue                                                               !/*pa        295*/
         go to 180                                                              !/*pa        296*/
      else                                                                      !/*pa        297*/
         go to 200                                                              !/*pa        298*/
      endif                                                                     !/*pa        299*/
!                                                                               !/*pa        300*/
  180 write(6,'("Storing final histograms to ",a30)')hfp                        !/*pa        301*/
      open(unit=43,file=hfp,form='unformatted',status='new')                    !/*pa        302*/
      call hstore(0,43)                                                         !/*pa        303*/
      close(43)                                                                 !/*pa        304*/
      return                                                                    !/*pa        305*/
!                                                                               !/*pa        306*/
!       Two dinemsional plot data.                                              !/*pa        307*/
!       Get dependent variable and index of other one.                          !/*pa        308*/
  200 ni=0                                                                      !/*pa        309*/
      write(6,'("Plot data as fcn. of xy,xz,yx,yz,zx,zy")')                     !/*pa        310*/
      write(6,'("or q to exit ?")')                                             !/*pa        311*/
      write(6,'(" < No default! >")')                                           !/*pa        312*/
      write(6,'("&")')                                                          !/*pa        313*/
      read(5,'(a3)')hi                                                          !/*pa        314*/
      if(hi(1:1).eq.'q')go to 500                                               !/*pa        315*/
      if(hi(1:2).eq.'xy'.or.hi(1:2).eq.'yx') then                               !/*pa        316*/
         if(ixs*iys.ne.1) then                                                  !/*pa        317*/
            write(6,'("X and Y spacing must be constant for PAW")')             !/*pa        318*/
            go to 500                                                           !/*pa        319*/
         endif                                                                  !/*pa        320*/
         ni=1                                                                   !/*pa        321*/
         if (hi(1:1).eq.'y')ni=4                                                !/*pa        322*/
  210    write(6,'("What indicies do you what for z?")')                        !/*pa        323*/
         write(6,'(" <default = 1>")')                                          !/*pa        324*/
         write(6,'("&")')                                                       !/*pa        325*/
         read(5,'(a30)')hb                                                      !/*pa        326*/
         kx=0                                                                   !/*pa        327*/
         do 215 j=1,30                                                          !/*pa        328*/
         n=index('0123456789',hb(j:j))                                          !/*pa        329*/
         if(n.ne.0) then                                                        !/*pa        330*/
            kx=kx*10+n-1                                                        !/*pa        331*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*pa        332*/
            iz=kx                                                               !/*pa        333*/
            go to 218                                                           !/*pa        334*/
         endif                                                                  !/*pa        335*/
  215    continue                                                               !/*pa        336*/
         iz=1                                                                   !/*pa        337*/
!                                                                               !/*pa        338*/
  218    if(iz.gt.ng3(ngd)-1) then                                              !/*pa        339*/
            write(6,'("This index is too large!")')                             !/*pa        340*/
            go to 210                                                           !/*pa        341*/
         endif                                                                  !/*pa        342*/
      elseif(hi(1:2).eq.'yz'.or.hi(1:2).eq.'zy') then                           !/*pa        343*/
         if(iys*izs.ne.1) then                                                  !/*pa        344*/
            write(6,'("Y and Z spacing must be constant for PAW")')             !/*pa        345*/
            go to 500                                                           !/*pa        346*/
         endif                                                                  !/*pa        347*/
         ni=2                                                                   !/*pa        348*/
         if (hi(1:1).eq.'z')ni=5                                                !/*pa        349*/
  220    write(6,'("What indicies do you what for x?")')                        !/*pa        350*/
         write(6,'(" <default = 1>")')                                          !/*pa        351*/
         write(6,'("&")')                                                       !/*pa        352*/
         read(5,'(a30)')hb                                                      !/*pa        353*/
         kx=0                                                                   !/*pa        354*/
         do 225 j=1,30                                                          !/*pa        355*/
         n=index('0123456789',hb(j:j))                                          !/*pa        356*/
         if(n.ne.0) then                                                        !/*pa        357*/
            kx=kx*10+n-1                                                        !/*pa        358*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*pa        359*/
            ix=kx                                                               !/*pa        360*/
            go to 228                                                           !/*pa        361*/
         endif                                                                  !/*pa        362*/
  225    continue                                                               !/*pa        363*/
         ix=1                                                                   !/*pa        364*/
!                                                                               !/*pa        365*/
  228    if(ix.gt.ng1(ngd)-1) then                                              !/*pa        366*/
            write(6,'("This index is too large!")')                             !/*pa        367*/
            go to 220                                                           !/*pa        368*/
         endif                                                                  !/*pa        369*/
      elseif(hi(1:2).eq.'xz'.or.hi(1:2).eq.'zx') then                           !/*pa        370*/
         if(ixs*izs.ne.1) then                                                  !/*pa        371*/
            write(6,'("X and Z spacing must be constant for PAW")')             !/*pa        372*/
            go to 500                                                           !/*pa        373*/
         endif                                                                  !/*pa        374*/
         ni=3                                                                   !/*pa        375*/
         if (hi(1:1).eq.'z')ni=6                                                !/*pa        376*/
  230    write(6,'("What indicies do you what for y?")')                        !/*pa        377*/
         write(6,'(" <default = 1>")')                                          !/*pa        378*/
         write(6,'("&")')                                                       !/*pa        379*/
         read(5,'(a30)')hb                                                      !/*pa        380*/
         kx=0                                                                   !/*pa        381*/
         do 235 j=1,30                                                          !/*pa        382*/
         n=index('0123456789',hb(j:j))                                          !/*pa        383*/
         if(n.ne.0) then                                                        !/*pa        384*/
            kx=kx*10+n-1                                                        !/*pa        385*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*pa        386*/
            iy=kx                                                               !/*pa        387*/
            go to 238                                                           !/*pa        388*/
         endif                                                                  !/*pa        389*/
  235    continue                                                               !/*pa        390*/
         iy=1                                                                   !/*pa        391*/
!                                                                               !/*pa        392*/
  238    if(iy.gt.ng2(ngd)-1) then                                              !/*pa        393*/
            write(6,'("This index is too large!")')                             !/*pa        394*/
            go to 230                                                           !/*pa        395*/
         endif                                                                  !/*pa        396*/
      else                                                                      !/*pa        397*/
         go to 200                                                              !/*pa        398*/
      endif                                                                     !/*pa        399*/
!                                                                               !/*pa        400*/
!      ni is an indicator as to which two coordinate varibles will be           !/*pa        401*/
!      looped over.    1,2,3,4,5,6  --  xy,yz,xz,yx,zy,zx                       !/*pa        402*/
!      The remaining index will be fixed.                                       !/*pa        403*/
      if(ni.eq.1) then                                                          !/*pa        404*/
         xmin=gdx(lgdx+1)                                                       !/*pa        405*/
         xmax=gdx(lgdx+ng1(ngd))                                                !/*pa        406*/
         ymin=gdy(lgdy+1)                                                       !/*pa        407*/
         ymax=gdy(lgdy+ng2(ngd))                                                !/*pa        408*/
!                                                                               !/*pa        409*/
!                                                                               !/*pa        410*/
         call hlimit(512000)                                                    !/*pa        411*/
         call hbook2(100,hf,nx,sngl(xmin),sngl(xmax),                           !/*pa        412*/
     &    ny,sngl(ymin),sngl(ymax),0.0)                                         !/*pa        413*/
         call hbook2(101,'error',nx,sngl(xmin),sngl(xmax),                      !/*pa        414*/
     &    ny,sngl(ymin),sngl(ymax),0.0)                                         !/*pa        415*/
!          <<<< write pdata and xy to output file here>>>                       !/*pa        416*/
!                                                                               !/*pa        417*/
         do 240 iy=1,ny                                                         !/*pa        418*/
         do 240 ix=1,nx                                                         !/*pa        419*/
         mx=(iz-1)*nx*ny+(iy-1)*nx+ix                                           !/*pa        420*/
         f=gdata(lgdat+mx)                                                      !/*pa        421*/
         g=gdata(lgdat+mxgc(ngd)+mx)                                            !/*pa        422*/
         x=pdx(lpdx+ix)                                                         !/*pa        423*/
         y=pdy(lpdy+iy)                                                         !/*pa        424*/
         call hf2(100,sngl(x),sngl(y),sngl(f))                                  !/*pa        425*/
         call hf2(101,sngl(x),sngl(y),sngl(g))                                  !/*pa        426*/
  240    continue                                                               !/*pa        427*/
!                                                                               !/*pa        428*/
         write(6,'("Storing final histograms to ",a30)')hfp                     !/*pa        429*/
         open(unit=43,file=hfp,form='unformatted',status='new')                 !/*pa        430*/
         call hstore(0,43)                                                      !/*pa        431*/
         close(43)                                                              !/*pa        432*/
         return                                                                 !/*pa        433*/
!                                                                               !/*pa        434*/
      elseif(ni.eq.2) then                                                      !/*pa        435*/
         ymin=gdy(lgdy+1)                                                       !/*pa        436*/
         ymax=gdy(lgdy+ng2(ngd))                                                !/*pa        437*/
         zmin=gdz(lgdz+1)                                                       !/*pa        438*/
         zmax=gdz(lgdz+ng3(ngd))                                                !/*pa        439*/
!                                                                               !/*pa        440*/
         call hlimit(512000)                                                    !/*pa        441*/
         call hbook2(200,hf,ny,sngl(ymin),sngl(ymax),                           !/*pa        442*/
     &   nz,sngl(zmin),sngl(zmax),0.0)                                          !/*pa        443*/
         call hbook2(201,'error',ny,sngl(ymin),sngl(ymax),                      !/*pa        444*/
     &   nz,sngl(zmin),sngl(zmax),0.0)                                          !/*pa        445*/
!            <<<< write pdata and yz to output file here>>>                     !/*pa        446*/
         do 250 iz=1,nz                                                         !/*pa        447*/
         do 250 iy=1,ny                                                         !/*pa        448*/
         mx=(iz-1)*nx*ny+(iy-1)*nx+ix                                           !/*pa        449*/
         f=gdata(lgdat+mx)                                                      !/*pa        450*/
         g=gdata(lgdat+mxgc(ngd)+mx)                                            !/*pa        451*/
         y=pdy(lpdy+iy)                                                         !/*pa        452*/
         z=pdx(lpdz+iz)                                                         !/*pa        453*/
         call hf2(200,sngl(y),sngl(z),sngl(f))                                  !/*pa        454*/
         call hf2(201,sngl(y),sngl(z),sngl(g))                                  !/*pa        455*/
  250    continue                                                               !/*pa        456*/
!                                                                               !/*pa        457*/
         write(6,'("Storing final histograms to ",a30)')hfp                     !/*pa        458*/
         open(unit=43,file=hfp,form='unformatted',status='new')                 !/*pa        459*/
         call hstore(0,43)                                                      !/*pa        460*/
         close(43)                                                              !/*pa        461*/
         return                                                                 !/*pa        462*/
!                                                                               !/*pa        463*/
      elseif(ni.eq.3) then                                                      !/*pa        464*/
         xmin=gdx(lgdx+1)                                                       !/*pa        465*/
         xmax=gdx(lgdx+ng1(ngd))                                                !/*pa        466*/
         zmin=gdz(lgdz+1)                                                       !/*pa        467*/
         zmax=gdz(lgdz+ng3(ngd))                                                !/*pa        468*/
!                                                                               !/*pa        469*/
         call hlimit(512000)                                                    !/*pa        470*/
         call hbook2(300,hf,nx,sngl(xmin),sngl(xmax),                           !/*pa        471*/
     &    nz,sngl(zmin),sngl(zmax),0.0)                                         !/*pa        472*/
         call hbook2(301,'error',nx,sngl(xmin),sngl(xmax),                      !/*pa        473*/
     &    nz,sngl(zmin),sngl(zmax),0.0)                                         !/*pa        474*/
!            <<<< write pdata and xz to output file here>>>                     !/*pa        475*/
!                                                                               !/*pa        476*/
         do 260 iz=1,nz                                                         !/*pa        477*/
         do 260 ix=1,nx                                                         !/*pa        478*/
         mx=(iz-1)*nx*ny+(iy-1)*nx+ix                                           !/*pa        479*/
         f=gdata(lgdat+mx)                                                      !/*pa        480*/
         g=gdata(lgdat+mxgc(ngd)+mx)                                            !/*pa        481*/
         x=pdx(lpdx+ix)                                                         !/*pa        482*/
         z=pdz(lpdz+iz)                                                         !/*pa        483*/
         call hf2(300,sngl(x),sngl(z),sngl(f))                                  !/*pa        484*/
         call hf2(301,sngl(x),sngl(z),sngl(g))                                  !/*pa        485*/
  260    continue                                                               !/*pa        486*/
!                                                                               !/*pa        487*/
         write(6,'("Storing final histograms to ",a30)')hfp                     !/*pa        488*/
         open(unit=43,file=hfp,form='unformatted',status='new')                 !/*pa        489*/
         call hstore(0,43)                                                      !/*pa        490*/
         close(43)                                                              !/*pa        491*/
         return                                                                 !/*pa        492*/
!                                                                               !/*pa        493*/
!                                                                               !/*pa        494*/
      elseif(ni.eq.4) then                                                      !/*pa        495*/
         ymin=gdy(lgdy+1)                                                       !/*pa        496*/
         ymax=gdy(lgdy+ng2(ngd))                                                !/*pa        497*/
         xmin=gdx(lgdx+1)                                                       !/*pa        498*/
         xmax=gdx(lgdx+ng1(ngd))                                                !/*pa        499*/
!                                                                               !/*pa        500*/
         call hlimit(512000)                                                    !/*pa        501*/
         call hbook2(100,hf,ny,sngl(ymin),sngl(ymax),                           !/*pa        502*/
     &   nx,sngl(xmin),sngl(xmax),0.0)                                          !/*pa        503*/
         call hbook2(101,'error',ny,sngl(ymin),sngl(ymax),                      !/*pa        504*/
     &   nx,sngl(xmin),sngl(xmax),0.0)                                          !/*pa        505*/
!            <<<< write pdata and yx to output file here>>>                     !/*pa        506*/
         do 270 ix=1,nx                                                         !/*pa        507*/
         do 270 iy=1,ny                                                         !/*pa        508*/
         mx=(iz-1)*nx*ny+(iy-1)*nx+ix                                           !/*pa        509*/
         f=gdata(lgdat+mx)                                                      !/*pa        510*/
         g=gdata(lgdat+mxgc(ngd)+mx)                                            !/*pa        511*/
         y=pdy(lpdy+iy)                                                         !/*pa        512*/
         x=pdx(lpdx+ix)                                                         !/*pa        513*/
         call hf2(100,sngl(y),sngl(x),sngl(f))                                  !/*pa        514*/
         call hf2(101,sngl(y),sngl(x),sngl(g))                                  !/*pa        515*/
  270    continue                                                               !/*pa        516*/
!                                                                               !/*pa        517*/
         write(6,'("Storing final histograms to ",a30)')hfp                     !/*pa        518*/
         open(unit=43,file=hfp,form='unformatted',status='new')                 !/*pa        519*/
         call hstore(0,43)                                                      !/*pa        520*/
         close(43)                                                              !/*pa        521*/
         return                                                                 !/*pa        522*/
!                                                                               !/*pa        523*/
!                                                                               !/*pa        524*/
      elseif(ni.eq.5) then                                                      !/*pa        525*/
         zmin=gdz(lgdz+1)                                                       !/*pa        526*/
         zmax=gdz(lgdz+ng3(ngd))                                                !/*pa        527*/
         ymin=gdy(lgdy+1)                                                       !/*pa        528*/
         ymax=gdy(lgdy+ng2(ngd))                                                !/*pa        529*/
!                                                                               !/*pa        530*/
         call hlimit(512000)                                                    !/*pa        531*/
         call hbook2(200,hf,nz,sngl(zmin),sngl(zmax),                           !/*pa        532*/
     &   ny,sngl(ymin),sngl(ymax),0.0)                                          !/*pa        533*/
         call hbook2(201,'error',nz,sngl(zmin),sngl(zmax),                      !/*pa        534*/
     &   ny,sngl(ymin),sngl(ymax),0.0)                                          !/*pa        535*/
!            <<<< write pdata and zy to output file here>>>                     !/*pa        536*/
         do 280 iy=1,ny                                                         !/*pa        537*/
         do 280 iz=1,nz                                                         !/*pa        538*/
         mx=(iz-1)*nx*ny+(iy-1)*nx+ix                                           !/*pa        539*/
         f=gdata(lgdat+mx)                                                      !/*pa        540*/
         g=gdata(lgdat+mxgc(ngd)+mx)                                            !/*pa        541*/
         z=pdz(lpdz+iz)                                                         !/*pa        542*/
         y=pdy(lpdy+iy)                                                         !/*pa        543*/
         call hf2(200,sngl(z),sngl(y),sngl(f))                                  !/*pa        544*/
         call hf2(201,sngl(z),sngl(y),sngl(g))                                  !/*pa        545*/
  280    continue                                                               !/*pa        546*/
!                                                                               !/*pa        547*/
         write(6,'("Storing final histograms to ",a30)')hfp                     !/*pa        548*/
         open(unit=43,file=hfp,form='unformatted',status='new')                 !/*pa        549*/
         call hstore(0,43)                                                      !/*pa        550*/
         close(43)                                                              !/*pa        551*/
         return                                                                 !/*pa        552*/
!                                                                               !/*pa        553*/
!                                                                               !/*pa        554*/
      elseif(ni.eq.6) then                                                      !/*pa        555*/
         zmin=gdz(lgdz+1)                                                       !/*pa        556*/
         zmax=gdz(lgdz+ng3(ngd))                                                !/*pa        557*/
         xmin=gdx(lgdx+1)                                                       !/*pa        558*/
         xmax=gdx(lgdx+ng1(ngd))                                                !/*pa        559*/
!                                                                               !/*pa        560*/
         call hlimit(512000)                                                    !/*pa        561*/
         call hbook2(300,hf,nz,sngl(zmin),sngl(zmax),                           !/*pa        562*/
     &   nx,sngl(xmin),sngl(xmax),0.0)                                          !/*pa        563*/
         call hbook2(301,'error',nz,sngl(zmin),sngl(zmax),                      !/*pa        564*/
     &   nx,sngl(xmin),sngl(xmax),0.0)                                          !/*pa        565*/
!            <<<< write pdata and zx to output file here>>>                     !/*pa        566*/
         do 290 ix=1,nx                                                         !/*pa        567*/
         do 290 iz=1,nz                                                         !/*pa        568*/
         mx=(iz-1)*nx*ny+(iy-1)*nx+ix                                           !/*pa        569*/
         f=gdata(lgdat+mx)                                                      !/*pa        570*/
         g=gdata(lgdat+mxgc(ngd)+mx)                                            !/*pa        571*/
         z=pdz(lpdz+iz)                                                         !/*pa        572*/
         x=pdx(lpdx+ix)                                                         !/*pa        573*/
         call hf2(300,sngl(z),sngl(x),sngl(f))                                  !/*pa        574*/
         call hf2(301,sngl(z),sngl(x),sngl(g))                                  !/*pa        575*/
  290    continue                                                               !/*pa        576*/
!                                                                               !/*pa        577*/
         write(6,'("Storing final histograms to ",a30)')hfp                     !/*pa        578*/
         open(unit=43,file=hfp,form='unformatted',status='new')                 !/*pa        579*/
         call hstore(0,43)                                                      !/*pa        580*/
         close(43)                                                              !/*pa        581*/
         return                                                                 !/*pa        582*/
      endif                                                                     !/*pa        583*/
!                                                                               !/*pa        584*/
  500 continue                                                                  !/*pa        585*/
#endif                                                                          /*!pa        586*/
      return                                                                    !/*pa        587*/
      end                                                                       !/*pa        588*/
