!     $Id: tecdatr.F,v 1.8 2002/03/27 18:13:55 gwm Exp $                      !/*te          1*/
      subroutine tecdatr                                                        !/*te          2*/
!                                                                               !/*te          3*/
!       This subroutine is designed to convert input data from a                !/*te          4*/
!       mesh tally set to a data set compatible with the input                  !/*te          5*/
!       requirements of the Tecplot graphics package.                           !/*te          6*/
!                                                                               !/*te          7*/
!       Written by E. C. Snow  2/9/98.   (revised: 11/09/2001)                  !/*te          8*/
!                                                                               !/*te          9*/
#include <gridconv/gcdouble.h>                                                  /*!te         10*/
#include <gridconv/gccom5.h>                                                    /*!te         11*/
!                                                                               !/*te         12*/
      dimension gdr(1),gdz(1),gdt(1),gdata(1),pdr(1),pdz(1),                    !/*te         13*/
     & pdt(1),pdata(1)                                                          !/*te         14*/
      equivalence (das,gdr,gdz,gdt,gdata,pdr,pdz,pdt,pdata)                     !/*te         15*/
!                                                                               !/*te         16*/
      character hi*3,hv(3)*1,cm*80,hf*10,hb*30                                  !/*te         17*/
      data hv/'R','Z','T'/                                                      !/*te         18*/
!                                                                               !/*te         19*/
      write(6,'("What Tecplot filename do you want.")')                         !/*te         20*/
      write(6,'("$")')                                                          !/*te         21*/
      read(5,'(a30)')hfp                                                        !/*te         22*/
      j=leng(hfp)                                                               !/*te         23*/
      if(j.eq.0)then                                                            !/*te         24*/
         j=10                                                                   !/*te         25*/
         hfp(1:10)='datarz.tek'                                                 !/*te         26*/
         write(6,'("Using default output file name: datarz.tek")')              !/*te         27*/
      endif                                                                     !/*te         28*/
!                                                                               !/*te         29*/
   20 call unique(hfp,6)                                                        !/*te         30*/
      open(31,file=hfp,form='formatted',status='new')                           !/*te         31*/
!                                                                               !/*te         32*/
!       Ask user what comments and title are to be written to this file.        !/*te         33*/
!                                                                               !/*te         34*/
      write(31,'(1h#,a80)')aid                                                  !/*te         35*/
      write(31,'(1h#,"The number of histories = ",i10)')nps                     !/*te         36*/
      write(6,'("Do you want comment lines on this file. (y/n)")')              !/*te         37*/
      write(6,'("  <default = n or CR = 1st line of INP file.> ")')             !/*te         38*/
      write(6,'("$")')                                                          !/*te         39*/
      read(5,'(a3)')hi                                                          !/*te         40*/
      if(hi(1:1).eq.'y'.or.hi(1:3).eq.'yes') then                               !/*te         41*/
   30    write(6,'("Enter up to 80 characters:")')                              !/*te         42*/
         write(6,'("$")')                                                       !/*te         43*/
         read(5,'(a80)')cm                                                      !/*te         44*/
         write(31,'(1h#,a80)')cm                                                !/*te         45*/
         write(6,'("Do you want to add another line: (y/n)")')                  !/*te         46*/
         write(6,'("  <default = n or CR > ")')                                 !/*te         47*/
         write(6,'("$")')                                                       !/*te         48*/
         read(5,'(a3)')hi                                                       !/*te         49*/
         if(hi(1:1).eq.'y'.or.hi(1:3).eq.'yes')go to 30                         !/*te         50*/
      endif                                                                     !/*te         51*/
!                                                                               !/*te         52*/
      write(6,'("Enter the Title of your plot, up to 50 chars.:")')             !/*te         53*/
      write(6,'("  <default = n or CR = 1-50 char. of title on INP>")')         !/*te         54*/
      write(6,'("$")')                                                          !/*te         55*/
      read(5,'(a80)')cm                                                         !/*te         56*/
      do 40 i=1,50                                                              !/*te         57*/
   40 if(cm(i:i).ne.' ')go to 50                                                !/*te         58*/
      write(31,'(8h Title=",a50,1h")')aid                                       !/*te         59*/
      go to 60                                                                  !/*te         60*/
!                                                                               !/*te         61*/
   50 write(31,'(8h Title=",a50,1h")')cm                                        !/*te         62*/
!                                                                               !/*te         63*/
   60 continue                                                                  !/*te         64*/
   70 write(6,'("Enter plot dimensionality, 1, 2, or 9 to exit")')              !/*te         65*/
      write(6,'("$")')                                                          !/*te         66*/
      read(5,'(a30)')hb                                                         !/*te         67*/
      kx=0                                                                      !/*te         68*/
      np=0                                                                      !/*te         69*/
      do 75 j=1,30                                                              !/*te         70*/
      n=index('0123456789',hb(j:j))                                             !/*te         71*/
      if(n.ne.0) then                                                           !/*te         72*/
         kx=kx*10+n-1                                                           !/*te         73*/
      elseif(n.eq.0.and.kx.gt.0) then                                           !/*te         74*/
         np=kx                                                                  !/*te         75*/
         go to 78                                                               !/*te         76*/
      elseif(np.eq.0.and.j.eq.30) then                                          !/*te         77*/
         write(6,'(" There is no default value! ")')                            !/*te         78*/
         go to 70                                                               !/*te         79*/
      endif                                                                     !/*te         80*/
   75 continue                                                                  !/*te         81*/
!                                                                               !/*te         82*/
   78 if(np.eq.9)go to 500                                                      !/*te         83*/
      if(np.ge.1.and.np.le.2)go to 80                                           !/*te         84*/
      go to 70                                                                  !/*te         85*/
!                                                                               !/*te         86*/
   80 n1=ng1(ngd)-1                                                             !/*te         87*/
      n2=ng2(ngd)-1                                                             !/*te         88*/
      n3=ng3(ngd)                                                               !/*te         89*/
      nt=mod(ngtyp(ngd),10)                                                     !/*te         90*/
      if(nt.eq.0) then                                                          !/*te         91*/
         hf=hdx(ngd,nxv)                                                        !/*te         92*/
      elseif(nt.eq.1) then                                                      !/*te         93*/
         hf='source pts'                                                        !/*te         94*/
      elseif(nt.eq.2) then                                                      !/*te         95*/
         hf='ergdeposit'                                                        !/*te         96*/
      elseif(nt.eq.3) then                                                      !/*te         97*/
         hf='PD tracks.'                                                        !/*te         98*/
      elseif(nt.eq.4) then                                                      !/*te         99*/
         hf='DX tracks.'                                                        !/*te        100*/
      endif                                                                     !/*te        101*/
      go to (100,200)np                                                         !/*te        102*/
!                                                                               !/*te        103*/
!       One dinemsional plot data.                                              !/*te        104*/
!       Get dependent variable and index of other two.                          !/*te        105*/
  100 nv=0                                                                      !/*te        106*/
      write(6,'("Plot data as fcn. of r, z, t(theta), or q to exit?")')         !/*te        107*/
      write(6,'("$")')                                                          !/*te        108*/
      read(5,'(a3)')hi                                                          !/*te        109*/
      if(hi(1:1).eq.'q')go to 500                                               !/*te        110*/
      if(hi(1:1).eq.'r')nv=1                                                    !/*te        111*/
      if(hi(1:1).eq.'z')nv=2                                                    !/*te        112*/
      if(hi(1:1).eq.'t')nv=3                                                    !/*te        113*/
!                                                                               !/*te        114*/
      if(nv.eq.1) then                                                          !/*te        115*/
  110    write(6,'("What indicies do you what for z and theta?")')              !/*te        116*/
         write(6,'(" <default = 1,1>")')                                        !/*te        117*/
         write(6,'("$")')                                                       !/*te        118*/
         read(5,'(a30)')hb                                                      !/*te        119*/
         do 112 j=1,30                                                          !/*te        120*/
         n=index('0123456789',hb(j:j))                                          !/*te        121*/
         if(n.ne.0) go to 113                                                   !/*te        122*/
  112    continue                                                               !/*te        123*/
         i2=1                                                                   !/*te        124*/
         i3=1                                                                   !/*te        125*/
         go to 118                                                              !/*te        126*/
!                                                                               !/*te        127*/
  113    kk=1                                                                   !/*te        128*/
         do 116 i=1,2                                                           !/*te        129*/
         kx=0                                                                   !/*te        130*/
         do 114 j=kk,30                                                         !/*te        131*/
         n=index('0123456789',hb(j:j))                                          !/*te        132*/
         if(n.ne.0) then                                                        !/*te        133*/
            kx=kx*10+n-1                                                        !/*te        134*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*te        135*/
            kk=j+1                                                              !/*te        136*/
            go to 115                                                           !/*te        137*/
         endif                                                                  !/*te        138*/
  114    continue                                                               !/*te        139*/
!                                                                               !/*te        140*/
  115    if(i.eq.1) then                                                        !/*te        141*/
            i2=kx                                                               !/*te        142*/
         else                                                                   !/*te        143*/
            i3=kx                                                               !/*te        144*/
         endif                                                                  !/*te        145*/
  116    continue                                                               !/*te        146*/
!                                                                               !/*te        147*/
  118    if(i2.gt.ng2(ngd)-1.or.i3.gt.ng3(ngd)) then                            !/*te        148*/
            write(6,'("One of these indices is too large!")')                   !/*te        149*/
            go to 110                                                           !/*te        150*/
         endif                                                                  !/*te        151*/
      elseif(nv.eq.2) then                                                      !/*te        152*/
  120    write(6,'("What indicies do you what for r and theta?")')              !/*te        153*/
         write(6,'(" <default = 1,1>")')                                        !/*te        154*/
         write(6,'("$")')                                                       !/*te        155*/
         read(5,'(a30)')hb                                                      !/*te        156*/
         do 122 j=1,30                                                          !/*te        157*/
         n=index('0123456789',hb(j:j))                                          !/*te        158*/
         if(n.ne.0) go to 123                                                   !/*te        159*/
  122    continue                                                               !/*te        160*/
         i1=1                                                                   !/*te        161*/
         i3=1                                                                   !/*te        162*/
         go to 128                                                              !/*te        163*/
!                                                                               !/*te        164*/
  123    kk=1                                                                   !/*te        165*/
         do 126 i=1,2                                                           !/*te        166*/
         kx=0                                                                   !/*te        167*/
         do 124 j=kk,30                                                         !/*te        168*/
         n=index('0123456789',hb(j:j))                                          !/*te        169*/
         if(n.ne.0) then                                                        !/*te        170*/
            kx=kx*10+n-1                                                        !/*te        171*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*te        172*/
            kk=j+1                                                              !/*te        173*/
            go to 125                                                           !/*te        174*/
         endif                                                                  !/*te        175*/
  124    continue                                                               !/*te        176*/
!                                                                               !/*te        177*/
  125    if(i.eq.1) then                                                        !/*te        178*/
            i1=kx                                                               !/*te        179*/
         else                                                                   !/*te        180*/
            i3=kx                                                               !/*te        181*/
         endif                                                                  !/*te        182*/
  126    continue                                                               !/*te        183*/
!                                                                               !/*te        184*/
  128    if(i1.gt.ng1(ngd)-1.or.i3.gt.ng3(ngd)) then                            !/*te        185*/
            write(6,'("One of these indices is too large!")')                   !/*te        186*/
            go to 120                                                           !/*te        187*/
         endif                                                                  !/*te        188*/
      elseif(nv.eq.3) then                                                      !/*te        189*/
  130    write(6,'("What indicies do you what for r and z?")')                  !/*te        190*/
         write(6,'(" <default = 1,1>")')                                        !/*te        191*/
         write(6,'("$")')                                                       !/*te        192*/
         read(5,'(a30)')hb                                                      !/*te        193*/
         do 132 j=1,30                                                          !/*te        194*/
         n=index('0123456789',hb(j:j))                                          !/*te        195*/
         if(n.ne.0) go to 133                                                   !/*te        196*/
  132    continue                                                               !/*te        197*/
         i1=1                                                                   !/*te        198*/
         i2=1                                                                   !/*te        199*/
         go to 138                                                              !/*te        200*/
!                                                                               !/*te        201*/
  133    kk=1                                                                   !/*te        202*/
         do 136 i=1,2                                                           !/*te        203*/
         kx=0                                                                   !/*te        204*/
         do 134 j=kk,30                                                         !/*te        205*/
         n=index('0123456789',hb(j:j))                                          !/*te        206*/
         if(n.ne.0) then                                                        !/*te        207*/
            kx=kx*10+n-1                                                        !/*te        208*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*te        209*/
            kk=j+1                                                              !/*te        210*/
            go to 135                                                           !/*te        211*/
         endif                                                                  !/*te        212*/
  134    continue                                                               !/*te        213*/
!                                                                               !/*te        214*/
  135    if(i.eq.1) then                                                        !/*te        215*/
            i1=kx                                                               !/*te        216*/
         else                                                                   !/*te        217*/
            i2=kx                                                               !/*te        218*/
         endif                                                                  !/*te        219*/
  136    continue                                                               !/*te        220*/
!                                                                               !/*te        221*/
  138    if(i1.gt.ng1(ngd)-1.or.i2.gt.ng2(ngd)-1) then                          !/*te        222*/
            write(6,'("One of these indices is too large!")')                   !/*te        223*/
            go to 130                                                           !/*te        224*/
         endif                                                                  !/*te        225*/
      else                                                                      !/*te        226*/
         go to 100                                                              !/*te        227*/
      endif                                                                     !/*te        228*/
!                                                                               !/*te        229*/
!       nv is the number of the coordinate variable that will                   !/*te        230*/
!       looped over.   1,2,3 - r,z,theta.                                       !/*te        231*/
!       The other two indices will be fixed.                                    !/*te        232*/
!                                                                               !/*te        233*/
      if(nv.eq.1) then                                                          !/*te        234*/
         do 140 i1=1,n1                                                         !/*te        235*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*te        236*/
         pdata(lpdat+i1)=gdata(lgdat+mx)                                        !/*te        237*/
         pdata(lpdat+n1+i1)=gdata(lgdat+mxgc(ngd)+mx)                           !/*te        238*/
  140    continue                                                               !/*te        239*/
!                                                                               !/*te        240*/
         write(31,'(1h#," The ",a1," index set to ",i5,'//                      !/*te        241*/
     &    '" and the ",a1," index set to ",i5)')hv(2),i2,hv(3),i3               !/*te        242*/
!                                                                               !/*te        243*/
!       Write header and data the a formatted file for input                    !/*te        244*/
!       into the Tecplot utility for plotting.                                  !/*te        245*/
!                                                                               !/*te        246*/
         write(31,'(12h Variables=",a1,3h" ",a10,9h" "error")')                 !/*te        247*/
     &    hv(nv),hf                                                             !/*te        248*/
         write(31,'(" zone i=",i1," f=point")')n1                               !/*te        249*/
         do 145 i=1,n1                                                          !/*te        250*/
         write(31,'(3(1x,g13.6))')pdr(lpdr+i),pdata(lpdat+i),                   !/*te        251*/
     &   pdata(lpdat+n1+i)                                                      !/*te        252*/
  145    continue                                                               !/*te        253*/
         close(31)                                                              !/*te        254*/
         return                                                                 !/*te        255*/
!                                                                               !/*te        256*/
      elseif(nv.eq.2) then                                                      !/*te        257*/
         do 150 i2=1,n2                                                         !/*te        258*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*te        259*/
         pdata(lpdat+i2)=gdata(lgdat+mx)                                        !/*te        260*/
         pdata(lpdat+n2+i2)=gdata(lgdat+mxgc(ngd)+mx)                           !/*te        261*/
  150    continue                                                               !/*te        262*/
!                                                                               !/*te        263*/
         write(31,'(1h#," The ",a1," index set to ",i5,'//                      !/*te        264*/
     &    '" and the ",a1," index set to ",i5)')hv(1),i1,hv(3),i3               !/*te        265*/
!                                                                               !/*te        266*/
!       Write header and data the a formatted file for input                    !/*te        267*/
!       into the Tecplot utility for plotting.                                  !/*te        268*/
!                                                                               !/*te        269*/
         write(31,'(12h Variables=",a1,3h" ",a10,9h" "error")')                 !/*te        270*/
     &    hv(nv),hf                                                             !/*te        271*/
         write(31,'(" zone i=",i1," f=point")')n2                               !/*te        272*/
         do 155 i=1,n2                                                          !/*te        273*/
         write(31,'(3(1x,g13.6))')pdz(lpdz+i),pdata(lpdat+i),                   !/*te        274*/
     &    pdata(lpdat+n2+i)                                                     !/*te        275*/
  155    continue                                                               !/*te        276*/
         close(31)                                                              !/*te        277*/
         return                                                                 !/*te        278*/
!                                                                               !/*te        279*/
      else                                                                      !/*te        280*/
         do 160 i3=1,n3                                                         !/*te        281*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*te        282*/
         pdata(lpdat+i3)=gdata(lgdat+mx)                                        !/*te        283*/
         pdata(lpdat+n3+i3)=gdata(lgdat+mxgc(ngd)+mx)                           !/*te        284*/
  160    continue                                                               !/*te        285*/
!                                                                               !/*te        286*/
         write(31,'(1h#," The ",a1," index set to ",i5,'//                      !/*te        287*/
     &    '" and the ",a1," index set to ",i5)')hv(1),i1,hv(2),i2               !/*te        288*/
!                                                                               !/*te        289*/
!       Write header and data the a formatted file for input                    !/*te        290*/
!       into the Tecplot utility for plotting.                                  !/*te        291*/
!                                                                               !/*te        292*/
         write(31,'(12h Variables=",a1,3h" ",a10,9h" "error")')                 !/*te        293*/
     &    hv(nv),hf                                                             !/*te        294*/
         write(31,'(" zone i=",i1," f=point")')n3                               !/*te        295*/
         do 190 i=1,n3                                                          !/*te        296*/
         write(31,'(3(1x,g13.6))')pdt(lpdt+i),pdata(lpdat+i),                   !/*te        297*/
     &    pdata(lpdat+n3+i)                                                     !/*te        298*/
  190    continue                                                               !/*te        299*/
         close(31)                                                              !/*te        300*/
         return                                                                 !/*te        301*/
!                                                                               !/*te        302*/
      endif                                                                     !/*te        303*/
!                                                                               !/*te        304*/
!       Two dinemsional plot data.                                              !/*te        305*/
!       Get dependent variables and index of other one.                         !/*te        306*/
  200 ni=0                                                                      !/*te        307*/
      write(6,'("Plot data as fcn. of rz, tz, tr or q to exit ?")')             !/*te        308*/
      write(6,'("Default = rz")')                                               !/*te        309*/
      write(6,'("$")')                                                          !/*te        310*/
      read(5,'(a3)')hi                                                          !/*te        311*/
      if(hi(1:1).eq.'q')go to 500                                               !/*te        312*/
      if(hi(1:2).eq.'  ')hi(1:2)='rz'                                           !/*te        313*/
      if(hi(1:2).eq.'rz') then                                                  !/*te        314*/
         ni=1                                                                   !/*te        315*/
  210    write(6,'("What indicies do you what for theta?")')                    !/*te        316*/
         write(6,'(" <default = 1>")')                                          !/*te        317*/
         write(6,'("$")')                                                       !/*te        318*/
         read(5,'(a30)')hb                                                      !/*te        319*/
         kx=0                                                                   !/*te        320*/
         do 215 j=1,30                                                          !/*te        321*/
         n=index('0123456789',hb(j:j))                                          !/*te        322*/
         if(n.ne.0) then                                                        !/*te        323*/
            kx=kx*10+n-1                                                        !/*te        324*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*te        325*/
            i3=kx                                                               !/*te        326*/
            go to 218                                                           !/*te        327*/
         endif                                                                  !/*te        328*/
  215    continue                                                               !/*te        329*/
         i3=1                                                                   !/*te        330*/
!                                                                               !/*te        331*/
  218    if(i3.gt.ng3(ngd)) then                                                !/*te        332*/
            write(6,'("This index is too large!")')                             !/*te        333*/
            go to 210                                                           !/*te        334*/
         endif                                                                  !/*te        335*/
      elseif(hi(1:2).eq.'tz') then                                              !/*te        336*/
         ni=2                                                                   !/*te        337*/
  220    write(6,'("What indicies do you what for r?")')                        !/*te        338*/
         write(6,'(" <default = 1>")')                                          !/*te        339*/
         write(6,'("$")')                                                       !/*te        340*/
         read(5,'(a30)')hb                                                      !/*te        341*/
         kx=0                                                                   !/*te        342*/
         do 225 j=1,30                                                          !/*te        343*/
         n=index('0123456789',hb(j:j))                                          !/*te        344*/
         if(n.ne.0) then                                                        !/*te        345*/
            kx=kx*10+n-1                                                        !/*te        346*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*te        347*/
            i1=kx                                                               !/*te        348*/
            go to 228                                                           !/*te        349*/
         endif                                                                  !/*te        350*/
  225    continue                                                               !/*te        351*/
         i1=1                                                                   !/*te        352*/
!                                                                               !/*te        353*/
  228    if(i1.gt.ng1(ngd)-1) then                                              !/*te        354*/
            write(6,'("This index is too large!")')                             !/*te        355*/
            go to 220                                                           !/*te        356*/
         endif                                                                  !/*te        357*/
      elseif(hi(1:2).eq.'tr') then                                              !/*te        358*/
         ni=3                                                                   !/*te        359*/
  230    write(6,'("What indicies do you what for z?")')                        !/*te        360*/
         write(6,'(" <default = 1>")')                                          !/*te        361*/
         write(6,'("$")')                                                       !/*te        362*/
         read(5,'(a30)')hb                                                      !/*te        363*/
         kx=0                                                                   !/*te        364*/
         do 235 j=1,30                                                          !/*te        365*/
         n=index('0123456789',hb(j:j))                                          !/*te        366*/
         if(n.ne.0) then                                                        !/*te        367*/
            kx=kx*10+n-1                                                        !/*te        368*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*te        369*/
            i2=kx                                                               !/*te        370*/
            go to 238                                                           !/*te        371*/
         endif                                                                  !/*te        372*/
  235    continue                                                               !/*te        373*/
         i2=1                                                                   !/*te        374*/
!                                                                               !/*te        375*/
  238    if(i2.gt.ng2(ngd)-1) then                                              !/*te        376*/
            write(6,'("This index is too large!")')                             !/*te        377*/
            go to 230                                                           !/*te        378*/
         endif                                                                  !/*te        379*/
      else                                                                      !/*te        380*/
         go to 200                                                              !/*te        381*/
      endif                                                                     !/*te        382*/
!                                                                               !/*te        383*/
!      ni is an indicator as to which two coordinate varibles will be           !/*te        384*/
!      looped over.    1,2,3  --  rz,tz,tr                                      !/*te        385*/
!      The remaining index will be fixed.                                       !/*te        386*/
      if(ni.eq.1) then                                                          !/*te        387*/
         write(31,'(14h Variables = ",a1,4h", ",a1,4h", ",'//                   !/*te        388*/
     &    'a10,10h", "error")')hv(1),hv(2),hf                                   !/*te        389*/
         write(31,'(" zone i=",i3,1h,," j=",i3,1h,," f=point")')n1,n2           !/*te        390*/
         do 240 i2=1,n2                                                         !/*te        391*/
         do 240 i1=1,n1                                                         !/*te        392*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*te        393*/
         write(31,'(4(1x,g13.6))')pdr(lpdr+i1),pdz(lpdz+i2),                    !/*te        394*/
     &   gdata(lgdat+mx),gdata(lgdat+mxgc(ngd)+mx)                              !/*te        395*/
  240    continue                                                               !/*te        396*/
         close(31)                                                              !/*te        397*/
         return                                                                 !/*te        398*/
!                                                                               !/*te        399*/
      elseif(ni.eq.2) then                                                      !/*te        400*/
         write(31,'(14h Variables = ",a1,4h", ",a1,4h", ",'//                   !/*te        401*/
     &    'a10,10h", "error")')hv(3),hv(2),hf                                   !/*te        402*/
         write(31,'(" zone i=",i3,1h,," j=",i3,1h,," f=point")')n3,n2           !/*te        403*/
         do 250 i2=1,n2                                                         !/*te        404*/
         do 250 i3=1,n3                                                         !/*te        405*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*te        406*/
         write(31,'(4(1x,g13.6))')pdt(lpdt+i3),pdz(lpdz+i2),                    !/*te        407*/
     &    gdata(lgdat+mx),gdata(lgdat+mxgc(ngd)+mx)                             !/*te        408*/
  250    continue                                                               !/*te        409*/
         close(31)                                                              !/*te        410*/
         return                                                                 !/*te        411*/
!                                                                               !/*te        412*/
      else                                                                      !/*te        413*/
         write(31,'(14h Variables = ",a1,4h", ",a1,4h", ",'//                   !/*te        414*/
     &    'a10,10h", "error")')hv(3),hv(1),hf                                   !/*te        415*/
         write(31,'(" zone i=",i3,1h,," j=",i3,1h,," f=point")')n3,n1           !/*te        416*/
         do 260 i1=1,n1                                                         !/*te        417*/
         do 260 i3=1,n3                                                         !/*te        418*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*te        419*/
         write(31,'(4(1x,g13.6))')pdt(lpdt+i3),pdr(lpdr+i1),                    !/*te        420*/
     &    gdata(lgdat+mx),gdata(lgdat+mxgc(ngd)+mx)                             !/*te        421*/
  260    continue                                                               !/*te        422*/
         close(31)                                                              !/*te        423*/
         return                                                                 !/*te        424*/
!                                                                               !/*te        425*/
      endif                                                                     !/*te        426*/
!                                                                               !/*te        427*/
  500 return                                                                    !/*te        428*/
!                                                                               !/*te        429*/
      end                                                                       !/*te        430*/
