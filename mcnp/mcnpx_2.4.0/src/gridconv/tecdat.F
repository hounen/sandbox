!     $Id: tecdat.F,v 1.7 2002/03/27 18:13:55 gwm Exp $                       !/*te          1*/
      subroutine tecdat                                                         !/*te          2*/
!                                                                               !/*te          3*/
!       This subroutine is designed to convert input data from a                !/*te          4*/
!       mesh tally set to a data set compatible with the input                  !/*te          5*/
!       requirements of the Tecplot graphics package.                           !/*te          6*/
!                                                                               !/*te          7*/
!       Written by E. C. Snow  2/9/98.   (revised: 12/07/2000)                  !/*te          8*/
!                                                                               !/*te          9*/
#include <gridconv/gcdouble.h>                                                  /*!te         10*/
#include <gridconv/gccom5.h>                                                    /*!te         11*/
!                                                                               !/*te         12*/
      dimension gdx(1),gdy(1),gdz(1),gdata(1),pdx(1),pdy(1),                    !/*te         13*/
     & pdz(1),pdata(1)                                                          !/*te         14*/
      equivalence (das,gdx,gdy,gdz,gdata,pdx,pdy,pdz,pdata)                     !/*te         15*/
!                                                                               !/*te         16*/
      character hi*3,hv(3)*1,hx*5,cm*80,hf*10,hb*30                             !/*te         17*/
      data hv/'X','Y','Z'/                                                      !/*te         18*/
      data hx/'X,Y,Z'/                                                          !/*te         19*/
!                                                                               !/*te         20*/
      write(6,'("What Tecplot filename do you want.")')                         !/*te         21*/
      write(6,'("$")')                                                          !/*te         22*/
      read(5,'(a30)')hfp                                                        !/*te         23*/
      j=leng(hfp)                                                               !/*te         24*/
      if(j.eq.0)then                                                            !/*te         25*/
         j=8                                                                    !/*te         26*/
         hfp(1:8)='data.tec'                                                    !/*te         27*/
         write(6,'("Using default output file name: data.tec")')                !/*te         28*/
      endif                                                                     !/*te         29*/
!                                                                               !/*te         30*/
   20 call unique(hfp,6)                                                        !/*te         31*/
      open(31,file=hfp,form='formatted',status='new')                           !/*te         32*/
!                                                                               !/*te         33*/
!       Ask user what comments and title are to be written to this file.        !/*te         34*/
!                                                                               !/*te         35*/
      write(31,'(1h#,a80)')aid                                                  !/*te         36*/
      write(31,'(1h#,"The number of histories = ",i10)')nps                     !/*te         37*/
      write(6,'("Do you want comment lines on this file. (y/n)")')              !/*te         38*/
      write(6,'("  <default = n or CR = 1st line of INP file.> ")')             !/*te         39*/
      write(6,'("$")')                                                          !/*te         40*/
      read(5,'(a3)')hi                                                          !/*te         41*/
      if(hi(1:1).eq.'y'.or.hi(1:3).eq.'yes') then                               !/*te         42*/
   30    write(6,'("Enter up to 80 characters:")')                              !/*te         43*/
         write(6,'("$")')                                                       !/*te         44*/
         read(5,'(a80)')cm                                                      !/*te         45*/
         write(31,'(1h#,a80)')cm                                                !/*te         46*/
         write(6,'("Do you want to add another line: (y/n)")')                  !/*te         47*/
         write(6,'("  <default = n or CR > ")')                                 !/*te         48*/
         write(6,'("$")')                                                       !/*te         49*/
         read(5,'(a3)')hi                                                       !/*te         50*/
         if(hi(1:1).eq.'y'.or.hi(1:3).eq.'yes')go to 30                         !/*te         51*/
      endif                                                                     !/*te         52*/
!                                                                               !/*te         53*/
      write(6,'("Enter the Title of your plot, up to 50 chars.:")')             !/*te         54*/
      write(6,'("  <default = n or CR = 1-50 char. of title on INP>")')         !/*te         55*/
      write(6,'("$")')                                                          !/*te         56*/
      read(5,'(a80)')cm                                                         !/*te         57*/
      do 40 i=1,50                                                              !/*te         58*/
   40 if(cm(i:i).ne.' ')go to 50                                                !/*te         59*/
      write(31,'(8h Title=",a50,1h")')aid                                       !/*te         60*/
      go to 60                                                                  !/*te         61*/
!                                                                               !/*te         62*/
   50 write(31,'(8h Title=",a50,1h")')cm                                        !/*te         63*/
!                                                                               !/*te         64*/
   60 continue                                                                  !/*te         65*/
!                                                                               !/*te         66*/
   70 write(6,'("Enter plot dimensionality, 1,2,3 or 9 to exit")')              !/*te         67*/
      write(6,'("$")')                                                          !/*te         68*/
      read(5,'(a30)')hb                                                         !/*te         69*/
      kx=0                                                                      !/*te         70*/
      np=0                                                                      !/*te         71*/
      do 75 j=1,30                                                              !/*te         72*/
      n=index('0123456789',hb(j:j))                                             !/*te         73*/
      if(n.ne.0) then                                                           !/*te         74*/
         kx=kx*10+n-1                                                           !/*te         75*/
      elseif(n.eq.0.and.kx.gt.0) then                                           !/*te         76*/
         np=kx                                                                  !/*te         77*/
         go to 78                                                               !/*te         78*/
      elseif(np.eq.0.and.j.eq.30) then                                          !/*te         79*/
         write(6,'(" There is no default value! ")')                            !/*te         80*/
         go to 70                                                               !/*te         81*/
      endif                                                                     !/*te         82*/
   75 continue                                                                  !/*te         83*/
!                                                                               !/*te         84*/
   78 if(np.eq.9)go to 500                                                      !/*te         85*/
      if(np.ge.1.and.np.le.3)go to 80                                           !/*te         86*/
      go to 70                                                                  !/*te         87*/
!                                                                               !/*te         88*/
   80 nx=ng1(ngd)-1                                                             !/*te         89*/
      ny=ng2(ngd)-1                                                             !/*te         90*/
      nz=ng3(ngd)-1                                                             !/*te         91*/
      nt=mod(ngtyp(ngd),10)                                                     !/*te         92*/
      if(nt.eq.0) then                                                          !/*te         93*/
         hf=hdx(ngd,nxv)                                                        !/*te         94*/
      elseif(nt.eq.1) then                                                      !/*te         95*/
         hf='source pts'                                                        !/*te         96*/
      elseif(nt.eq.2) then                                                      !/*te         97*/
         hf='ergdeposit'                                                        !/*te         98*/
      elseif(nt.eq.3) then                                                      !/*te         99*/
         hf='PD tracks.'                                                        !/*te        100*/
      elseif(nt.eq.4) then                                                      !/*te        101*/
         hf='DX tracks.'                                                        !/*te        102*/
      endif                                                                     !/*te        103*/
      go to (100,200,300)np                                                     !/*te        104*/
!                                                                               !/*te        105*/
!       One dinemsional plot data.                                              !/*te        106*/
!       Get dependent variable and index of other two.                          !/*te        107*/
  100 nv=0                                                                      !/*te        108*/
      write(6,'("Plot data as fcn. of x, y, z, or q to exit?")')                !/*te        109*/
      write(6,'("$")')                                                          !/*te        110*/
      read(5,'(a3)')hi                                                          !/*te        111*/
      if(hi(1:1).eq.'q')go to 500                                               !/*te        112*/
      if(hi(1:1).eq.'x')nv=1                                                    !/*te        113*/
      if(hi(1:1).eq.'y')nv=2                                                    !/*te        114*/
      if(hi(1:1).eq.'z')nv=3                                                    !/*te        115*/
!                                                                               !/*te        116*/
      if(nv.eq.1) then                                                          !/*te        117*/
  110    write(6,'("What indicies do you what for y and z?")')                  !/*te        118*/
         write(6,'(" <default = 1,1>")')                                        !/*te        119*/
         write(6,'("$")')                                                       !/*te        120*/
         read(5,'(a30)')hb                                                      !/*te        121*/
         do 112 j=1,30                                                          !/*te        122*/
         n=index('0123456789',hb(j:j))                                          !/*te        123*/
         if(n.ne.0) go to 113                                                   !/*te        124*/
  112    continue                                                               !/*te        125*/
         iy=1                                                                   !/*te        126*/
         iz=1                                                                   !/*te        127*/
         go to 118                                                              !/*te        128*/
!                                                                               !/*te        129*/
  113    kk=1                                                                   !/*te        130*/
         do 116 i=1,2                                                           !/*te        131*/
         kx=0                                                                   !/*te        132*/
         do 114 j=kk,30                                                         !/*te        133*/
         n=index('0123456789',hb(j:j))                                          !/*te        134*/
         if(n.ne.0) then                                                        !/*te        135*/
            kx=kx*10+n-1                                                        !/*te        136*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*te        137*/
            kk=j+1                                                              !/*te        138*/
            go to 115                                                           !/*te        139*/
         endif                                                                  !/*te        140*/
  114    continue                                                               !/*te        141*/
!                                                                               !/*te        142*/
  115    if(i.eq.1) then                                                        !/*te        143*/
            iy=kx                                                               !/*te        144*/
         else                                                                   !/*te        145*/
            iz=kx                                                               !/*te        146*/
         endif                                                                  !/*te        147*/
  116    continue                                                               !/*te        148*/
!                                                                               !/*te        149*/
  118    if(iy.gt.ng2(ngd)-1.or.iz.gt.ng3(ngd)-1) then                          !/*te        150*/
            write(6,'("One of these indices is too large!")')                   !/*te        151*/
            go to 110                                                           !/*te        152*/
         endif                                                                  !/*te        153*/
      elseif(nv.eq.2) then                                                      !/*te        154*/
  120    write(6,'("What indicies do you what for x and z?")')                  !/*te        155*/
         write(6,'(" <default = 1,1>")')                                        !/*te        156*/
         write(6,'("$")')                                                       !/*te        157*/
         read(5,'(a30)')hb                                                      !/*te        158*/
         do 122 j=1,30                                                          !/*te        159*/
         n=index('0123456789',hb(j:j))                                          !/*te        160*/
         if(n.ne.0) go to 123                                                   !/*te        161*/
  122    continue                                                               !/*te        162*/
         ix=1                                                                   !/*te        163*/
         iz=1                                                                   !/*te        164*/
         go to 128                                                              !/*te        165*/
!                                                                               !/*te        166*/
  123    kk=1                                                                   !/*te        167*/
         do 126 i=1,2                                                           !/*te        168*/
         kx=0                                                                   !/*te        169*/
         do 124 j=kk,30                                                         !/*te        170*/
         n=index('0123456789',hb(j:j))                                          !/*te        171*/
         if(n.ne.0) then                                                        !/*te        172*/
            kx=kx*10+n-1                                                        !/*te        173*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*te        174*/
            kk=j+1                                                              !/*te        175*/
            go to 125                                                           !/*te        176*/
         endif                                                                  !/*te        177*/
  124    continue                                                               !/*te        178*/
!                                                                               !/*te        179*/
  125    if(i.eq.1) then                                                        !/*te        180*/
            ix=kx                                                               !/*te        181*/
         else                                                                   !/*te        182*/
            iz=kx                                                               !/*te        183*/
         endif                                                                  !/*te        184*/
  126    continue                                                               !/*te        185*/
!                                                                               !/*te        186*/
  128    if(ix.gt.ng1(ngd)-1.or.iz.gt.ng3(ngd)-1) then                          !/*te        187*/
            write(6,'("One of these indices is too large!")')                   !/*te        188*/
            go to 120                                                           !/*te        189*/
         endif                                                                  !/*te        190*/
      elseif(nv.eq.3) then                                                      !/*te        191*/
  130    write(6,'("What indicies do you what for x and y?")')                  !/*te        192*/
         write(6,'(" <default = 1,1>")')                                        !/*te        193*/
         write(6,'("$")')                                                       !/*te        194*/
         read(5,'(a30)')hb                                                      !/*te        195*/
         do 132 j=1,30                                                          !/*te        196*/
         n=index('0123456789',hb(j:j))                                          !/*te        197*/
         if(n.ne.0) go to 133                                                   !/*te        198*/
  132    continue                                                               !/*te        199*/
         ix=1                                                                   !/*te        200*/
         iy=1                                                                   !/*te        201*/
         go to 138                                                              !/*te        202*/
!                                                                               !/*te        203*/
  133    kk=1                                                                   !/*te        204*/
         do 136 i=1,2                                                           !/*te        205*/
         kx=0                                                                   !/*te        206*/
         do 134 j=kk,30                                                         !/*te        207*/
         n=index('0123456789',hb(j:j))                                          !/*te        208*/
         if(n.ne.0) then                                                        !/*te        209*/
            kx=kx*10+n-1                                                        !/*te        210*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*te        211*/
            kk=j+1                                                              !/*te        212*/
            go to 135                                                           !/*te        213*/
         endif                                                                  !/*te        214*/
  134    continue                                                               !/*te        215*/
!                                                                               !/*te        216*/
  135    if(i.eq.1) then                                                        !/*te        217*/
            ix=kx                                                               !/*te        218*/
         else                                                                   !/*te        219*/
            iy=kx                                                               !/*te        220*/
         endif                                                                  !/*te        221*/
  136    continue                                                               !/*te        222*/
!                                                                               !/*te        223*/
  138    if(ix.gt.ng1(ngd)-1.or.iy.gt.ng2(ngd)-1) then                          !/*te        224*/
            write(6,'("One of these indices is too large!")')                   !/*te        225*/
            go to 130                                                           !/*te        226*/
         endif                                                                  !/*te        227*/
      else                                                                      !/*te        228*/
         go to 100                                                              !/*te        229*/
      endif                                                                     !/*te        230*/
!                                                                               !/*te        231*/
!       nv is the number of the coordinate variable that will                   !/*te        232*/
!       looped over.   1,2,3 - x,y,z.                                           !/*te        233*/
!       The other two indices will be fixed.                                    !/*te        234*/
!                                                                               !/*te        235*/
      if(nv.eq.1) then                                                          !/*te        236*/
         do 140 ix=1,nx                                                         !/*te        237*/
         mx=(iz-1)*nx*ny+(iy-1)*nx+ix                                           !/*te        238*/
         pdata(lpdat+ix)=gdata(lgdat+mx)                                        !/*te        239*/
         pdata(lpdat+nx+ix)=gdata(lgdat+mxgc(ngd)+mx)                           !/*te        240*/
  140    continue                                                               !/*te        241*/
         write(31,'(1h#," The ",a1," index set to ",i5,'//                      !/*te        242*/
     &    '" and the ",a1," index set to ",i5)')hv(2),iy,hv(3),iz               !/*te        243*/
!                                                                               !/*te        244*/
!       Write header and data the a formatted file for input                    !/*te        245*/
!       into the Tecplot utility for plotting.                                  !/*te        246*/
!                                                                               !/*te        247*/
         write(31,'(12h Variables=",a1,3h" ",a10,9h" "error")')                 !/*te        248*/
     &    hv(nv),hf                                                             !/*te        249*/
         write(31,'(" zone i=",i1," f=point")')nx                               !/*te        250*/
         do 145 i=1,nx                                                          !/*te        251*/
         write(31,'(3(1x,g13.6))')pdx(lpdx+i),pdata(lpdat+i),                   !/*te        252*/
     &    pdata(lpdat+nx+i)                                                     !/*te        253*/
  145    continue                                                               !/*te        254*/
         close(31)                                                              !/*te        255*/
         return                                                                 !/*te        256*/
!                                                                               !/*te        257*/
!                                                                               !/*te        258*/
      elseif(nv.eq.2) then                                                      !/*te        259*/
         do 150 iy=1,ny                                                         !/*te        260*/
         mx=(iz-1)*nx*ny+(iy-1)*nx+ix                                           !/*te        261*/
         pdata(lpdat+iy)=gdata(lgdat+mx)                                        !/*te        262*/
         pdata(lpdat+ny+iy)=gdata(lgdat+mxgc(ngd)+mx)                           !/*te        263*/
  150    continue                                                               !/*te        264*/
         write(31,'(1h#," The ",a1," index set to ",i5,'//                      !/*te        265*/
     &    '" and the ",a1," index set to ",i5)')hv(1),ix,hv(3),iz               !/*te        266*/
!                                                                               !/*te        267*/
!       Write header and data the a formatted file for input                    !/*te        268*/
!       into the Tecplot utility for plotting.                                  !/*te        269*/
!                                                                               !/*te        270*/
         write(31,'(12h Variables=",a1,3h" ",a10,9h" "error")')                 !/*te        271*/
     &    hv(nv),hf                                                             !/*te        272*/
         write(31,'(" zone i=",i1," f=point")')ny                               !/*te        273*/
         do 155 i=1,ny                                                          !/*te        274*/
         write(31,'(3(1x,g13.6))')pdy(lpdy+i),pdata(lpdat+i),                   !/*te        275*/
     &    pdata(lpdat+ny+i)                                                     !/*te        276*/
  155    continue                                                               !/*te        277*/
         close(31)                                                              !/*te        278*/
         return                                                                 !/*te        279*/
!                                                                               !/*te        280*/
!                                                                               !/*te        281*/
      else                                                                      !/*te        282*/
         do 160 iz=1,nz                                                         !/*te        283*/
         mx=(iz-1)*nx*ny+(iy-1)*nx+ix                                           !/*te        284*/
         pdata(lpdat+iz)=gdata(lgdat+mx)                                        !/*te        285*/
         pdata(lpdat+nz+iz)=gdata(lgdat+mxgc(ngd)+mx)                           !/*te        286*/
  160    continue                                                               !/*te        287*/
         write(31,'(1h#," The ",a1," index set to ",i5,'//                      !/*te        288*/
     &    '" and the ",a1," index set to ",i5)')hv(1),ix,hv(2),iy               !/*te        289*/
!                                                                               !/*te        290*/
!       Write header and data the a formatted file for input                    !/*te        291*/
!       into the Tecplot utility for plotting.                                  !/*te        292*/
!                                                                               !/*te        293*/
         write(31,'(12h Variables=",a1,3h" ",a10,9h" "error")')                 !/*te        294*/
     &    hv(nv),hf                                                             !/*te        295*/
         write(31,'(" zone i=",i1," f=point")')nz                               !/*te        296*/
         do 190 i=1,nz                                                          !/*te        297*/
         write(31,'(3(1x,g13.6))')pdz(lpdz+i),pdata(lpdat+i),                   !/*te        298*/
     &    pdata(lpdat+nz+i)                                                     !/*te        299*/
  190    continue                                                               !/*te        300*/
         close(31)                                                              !/*te        301*/
         return                                                                 !/*te        302*/
!                                                                               !/*te        303*/
      endif                                                                     !/*te        304*/
!                                                                               !/*te        305*/
!       Two dinemsional plot data.                                              !/*te        306*/
!       Get dependent variable and index of other one.                          !/*te        307*/
  200 ni=0                                                                      !/*te        308*/
      write(6,'("Plot data as fcn. of xy, yz, xz or q to exit ?")')             !/*te        309*/
      write(6,'("$")')                                                          !/*te        310*/
      read(5,'(a3)')hi                                                          !/*te        311*/
      if(hi(1:1).eq.'q')go to 500                                               !/*te        312*/
      if(hi(1:2).eq.'xy') then                                                  !/*te        313*/
         ni=1                                                                   !/*te        314*/
  210    write(6,'("What indicies do you what for z?")')                        !/*te        315*/
         write(6,'(" <default = 1>")')                                          !/*te        316*/
         write(6,'("$")')                                                       !/*te        317*/
         read(5,'(a30)')hb                                                      !/*te        318*/
         kx=0                                                                   !/*te        319*/
         do 215 j=1,30                                                          !/*te        320*/
         n=index('0123456789',hb(j:j))                                          !/*te        321*/
         if(n.ne.0) then                                                        !/*te        322*/
            kx=kx*10+n-1                                                        !/*te        323*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*te        324*/
            iz=kx                                                               !/*te        325*/
            go to 218                                                           !/*te        326*/
         endif                                                                  !/*te        327*/
  215    continue                                                               !/*te        328*/
         iz=1                                                                   !/*te        329*/
!                                                                               !/*te        330*/
  218    if(iz.gt.ng3(ngd)-1) then                                              !/*te        331*/
            write(6,'("This index is too large!")')                             !/*te        332*/
            go to 210                                                           !/*te        333*/
         endif                                                                  !/*te        334*/
      elseif(hi(1:2).eq.'yz') then                                              !/*te        335*/
         ni=2                                                                   !/*te        336*/
  220    write(6,'("What indicies do you what for x?")')                        !/*te        337*/
         write(6,'(" <default = 1>")')                                          !/*te        338*/
         write(6,'("$")')                                                       !/*te        339*/
         read(5,'(a30)')hb                                                      !/*te        340*/
         kx=0                                                                   !/*te        341*/
         do 225 j=1,30                                                          !/*te        342*/
         n=index('0123456789',hb(j:j))                                          !/*te        343*/
         if(n.ne.0) then                                                        !/*te        344*/
            kx=kx*10+n-1                                                        !/*te        345*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*te        346*/
            ix=kx                                                               !/*te        347*/
            go to 228                                                           !/*te        348*/
         endif                                                                  !/*te        349*/
  225    continue                                                               !/*te        350*/
         ix=1                                                                   !/*te        351*/
!                                                                               !/*te        352*/
  228    if(ix.gt.ng1(ngd)-1) then                                              !/*te        353*/
            write(6,'("This index is too large!")')                             !/*te        354*/
            go to 220                                                           !/*te        355*/
         endif                                                                  !/*te        356*/
      elseif(hi(1:2).eq.'xz') then                                              !/*te        357*/
         ni=3                                                                   !/*te        358*/
  230    write(6,'("What indicies do you what for y?")')                        !/*te        359*/
         write(6,'(" <default = 1>")')                                          !/*te        360*/
         write(6,'("$")')                                                       !/*te        361*/
         read(5,'(a30)')hb                                                      !/*te        362*/
         kx=0                                                                   !/*te        363*/
         do 235 j=1,30                                                          !/*te        364*/
         n=index('0123456789',hb(j:j))                                          !/*te        365*/
         if(n.ne.0) then                                                        !/*te        366*/
            kx=kx*10+n-1                                                        !/*te        367*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*te        368*/
            iy=kx                                                               !/*te        369*/
            go to 238                                                           !/*te        370*/
         endif                                                                  !/*te        371*/
  235    continue                                                               !/*te        372*/
         iy=1                                                                   !/*te        373*/
!                                                                               !/*te        374*/
  238    if(iy.gt.ng2(ngd)-1) then                                              !/*te        375*/
            write(6,'("This index is too large!")')                             !/*te        376*/
            go to 230                                                           !/*te        377*/
         endif                                                                  !/*te        378*/
      else                                                                      !/*te        379*/
         go to 200                                                              !/*te        380*/
      endif                                                                     !/*te        381*/
!                                                                               !/*te        382*/
!      ni is an indicator as to which two coordinate varibles will be           !/*te        383*/
!      looped over.    1,2,3  --  xy,yz,xz                                      !/*te        384*/
!      The remaining index will be fixed.                                       !/*te        385*/
      if(ni.eq.1) then                                                          !/*te        386*/
         write(31,                                                              !/*te        387*/
     &   '(14h Variables = ",a1,4h", ",a1,4h", ",a10,10h", "error")')           !/*te        388*/
     &    hv(1),hv(2),hf                                                        !/*te        389*/
         write(31,'(" zone i=",i3,1h,," j=",i3,1h,," f=point")')nx,ny           !/*te        390*/
         do 240 iy=1,ny                                                         !/*te        391*/
         do 240 ix=1,nx                                                         !/*te        392*/
         mx=(iz-1)*nx*ny+(iy-1)*nx+ix                                           !/*te        393*/
         write(31,'(4(1x,g13.6))')pdx(lpdx+ix),pdy(lpdy+iy),                    !/*te        394*/
     &    gdata(lgdat+mx),gdata(lgdat+mxgc(ngd)+mx)                             !/*te        395*/
  240    continue                                                               !/*te        396*/
         close(31)                                                              !/*te        397*/
         return                                                                 !/*te        398*/
!                                                                               !/*te        399*/
      elseif(ni.eq.2) then                                                      !/*te        400*/
         write(31,                                                              !/*te        401*/
     &   '(14h Variables = ",a1,4h", ",a1,4h", ",a10,10h", "error")')           !/*te        402*/
     &    hv(2),hv(3),hf                                                        !/*te        403*/
         write(31,'(" zone i=",i3,1h,," j=",i3,1h,," f=point")')ny,nz           !/*te        404*/
         do 250 iz=1,nz                                                         !/*te        405*/
         do 250 iy=1,ny                                                         !/*te        406*/
         mx=(iz-1)*nx*ny+(iy-1)*nx+ix                                           !/*te        407*/
         write(31,'(4(1x,g13.6))')pdy(lpdy+iy),pdz(lpdz+iz),                    !/*te        408*/
     &    gdata(lgdat+mx),gdata(lgdat+mxgc(ngd)+mx)                             !/*te        409*/
  250    continue                                                               !/*te        410*/
         close(31)                                                              !/*te        411*/
         return                                                                 !/*te        412*/
!                                                                               !/*te        413*/
      else                                                                      !/*te        414*/
         write(31,                                                              !/*te        415*/
     &   '(14h Variables = ",a1,4h", ",a1,4h", ",a10,10h", "error")')           !/*te        416*/
     &    hv(1),hv(3),hf                                                        !/*te        417*/
         write(31,'(" zone i=",i3,1h,," j=",i3,1h,," f=point")')nx,nz           !/*te        418*/
         do 260 iz=1,nz                                                         !/*te        419*/
         do 260 ix=1,nx                                                         !/*te        420*/
         mx=(iz-1)*nx*ny+(iy-1)*nx+ix                                           !/*te        421*/
         write(31,'(4(1x,g13.6))')pdx(lpdx+ix),pdz(lpdz+iz),                    !/*te        422*/
     &    gdata(lgdat+mx),gdata(lgdat+mxgc(ngd)+mx)                             !/*te        423*/
  260    continue                                                               !/*te        424*/
         close(31)                                                              !/*te        425*/
         return                                                                 !/*te        426*/
!                                                                               !/*te        427*/
      endif                                                                     !/*te        428*/
!                                                                               !/*te        429*/
!       Three dinemsional plot data.                                            !/*te        430*/
!       Loops will be over all three coordinate variables.                      !/*te        431*/
!                                                                               !/*te        432*/
  300 write(31,'(14h Variables = ",a1,4h", ",a1,4h", ",a1,4h", ",'//            !/*te        433*/
     & 'a10,10h", "error")')hv(1),hv(2),hv(3),hf                                !/*te        434*/
      write(31,                                                                 !/*te        435*/
     & '(" zone i=",i3,1h,," j=",i3,1h,," k=",i3,1h,," f=point")')              !/*te        436*/
     &  nx,ny,nz                                                                !/*te        437*/
      do 310 iz=1,nz                                                            !/*te        438*/
      do 310 iy=1,ny                                                            !/*te        439*/
      do 310 ix=1,nx                                                            !/*te        440*/
      mx=(iz-1)*nx*ny+(iy-1)*nx+ix                                              !/*te        441*/
      write(31,'(5(1x,g13.6))')pdx(lpdx+ix),pdy(lpdy+iy),                       !/*te        442*/
     & pdz(lpdz+iz),gdata(lgdat+mx),gdata(lgdat+mxgc(ngd)+mx)                   !/*te        443*/
  310 continue                                                                  !/*te        444*/
      close(31)                                                                 !/*te        445*/
      return                                                                    !/*te        446*/
!                                                                               !/*te        447*/
  500 return                                                                    !/*te        448*/
!                                                                               !/*te        449*/
      end                                                                       !/*te        450*/
