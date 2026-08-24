!     $Id: tecdats.F,v 1.8 2002/03/27 18:13:55 gwm Exp $                      !/*te          1*/
      subroutine tecdats                                                        !/*te          2*/
!                                                                               !/*te          3*/
!       This subroutine is designed to convert input data from a                !/*te          4*/
!       mesh tally set to a data set compatible with the input                  !/*te          5*/
!       requirements of the Tecplot graphics package.                           !/*te          6*/
!                                                                               !/*te          7*/
!       Written by E. C. Snow  11/22/98.   (revised: 11/14/2001)                !/*te          8*/
!                                                                               !/*te          9*/
#include <gridconv/gcdouble.h>                                                  /*!te         10*/
#include <gridconv/gccom5.h>                                                    /*!te         11*/
!                                                                               !/*te         12*/
      dimension gdr(1),gdp(1),gda(1),gdata(1),pdr(1),pdp(1),                    !/*te         13*/
     & pda(1),pdata(1)                                                          !/*te         14*/
      equivalence (das,gdr,gdp,gda,gdata,pdr,pdp,pda,pdata)                     !/*te         15*/
!                                                                               !/*te         16*/
      character hi*3,hv(3)*1,cm*80,hf*10,hb*30                                  !/*te         17*/
      data hv/'R','P','A'/                                                      !/*te         18*/
!                                                                               !/*te         19*/
      write(6,'("What Tecplot filename do you want.")')                         !/*te         20*/
      write(6,'("$")')                                                          !/*te         21*/
      read(5,'(a30)')hfp                                                        !/*te         22*/
      j=leng(hfp)                                                               !/*te         23*/
      if(j.eq.0)then                                                            !/*te         24*/
         j=10                                                                   !/*te         25*/
         hfp(1:10)='datars.tek'                                                 !/*te         26*/
         write(6,'("Using default output file name: datars.tek")')              !/*te         27*/
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
      n2=ng2(ngd)                                                               !/*te         88*/
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
      write(6,'("Plot data as fcn. of r, p, a, or q to exit?")')                !/*te        107*/
      write(6,'("$")')                                                          !/*te        108*/
      read(5,'(a3)')hi                                                          !/*te        109*/
      if(hi(1:1).eq.'q')go to 500                                               !/*te        110*/
      if(hi(1:1).eq.'r')nv=1                                                    !/*te        111*/
      if(hi(1:1).eq.'p')nv=2                                                    !/*te        112*/
      if(hi(1:1).eq.'a')nv=3                                                    !/*te        113*/
!                                                                               !/*te        114*/
      if(nv.eq.1) then                                                          !/*te        115*/
  110    write(6,'("What indicies do you what for p and a?")')                  !/*te        116*/
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
  118    if(i2.gt.ng2(ngd).or.i3.gt.ng3(ngd)) then                              !/*te        148*/
            write(6,'("One of these indices is too large!")')                   !/*te        149*/
            go to 110                                                           !/*te        150*/
         endif                                                                  !/*te        151*/
      elseif(nv.eq.2) then                                                      !/*te        152*/
  120    write(6,'("What indicies do you what for r and a?")')                  !/*te        153*/
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
  130    write(6,'("What indicies do you what for r and p?")')                  !/*te        190*/
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
  138    if(i1.gt.ng1(ngd)-1.or.i2.gt.ng2(ngd)) then                            !/*te        222*/
            write(6,'("One of these indices is too large!")')                   !/*te        223*/
            go to 130                                                           !/*te        224*/
         endif                                                                  !/*te        225*/
      else                                                                      !/*te        226*/
         go to 100                                                              !/*te        227*/
      endif                                                                     !/*te        228*/
!                                                                               !/*te        229*/
!       nv is the number of the coordinate variable that will                   !/*te        230*/
!       looped over.   1,2,3 - r,p,a.                                           !/*te        231*/
!       The other two indices will be fixed.                                    !/*te        232*/
!                                                                               !/*te        233*/
      if(nv.eq.1) then                                                          !/*te        234*/
         do 140 i1=1,n1                                                         !/*te        235*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*te        236*/
         pdata(lpdat+i1)=gdata(lgdat+mx)                                        !/*te        237*/
         pdata(lpdat+n1+i1)=gdata(lgdat+mxgc(ngd)+mx)                           !/*te        238*/
  140    continue                                                               !/*te        239*/
         write(31,'(1h#," The ",a1," index set to ",i5,'//                      !/*te        240*/
     &    '" and the ",a1," index set to ",i5)')hv(2),i2,hv(3),i3               !/*te        241*/
!                                                                               !/*te        242*/
!       Write header and data the a formatted file for input                    !/*te        243*/
!       into the Tecplot utility for plotting.                                  !/*te        244*/
!                                                                               !/*te        245*/
         write(31,'(12h Variables=",a1,3h" ",a10,9h" "error")')                 !/*te        246*/
     &    hv(nv),hf                                                             !/*te        247*/
         write(31,'(" zone i=",i1," f=point")')n1                               !/*te        248*/
         do 145 i=1,n1                                                          !/*te        249*/
         write(31,'(3(1x,g13.6))')pdr(lpdr+i),pdata(lpdat+i),                   !/*te        250*/
     &    pdata(lpdat+n1+i)                                                     !/*te        251*/
  145    continue                                                               !/*te        252*/
         close(31)                                                              !/*te        253*/
         return                                                                 !/*te        254*/
!                                                                               !/*te        255*/
      elseif(nv.eq.2) then                                                      !/*te        256*/
         do 150 i2=1,n2                                                         !/*te        257*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*te        258*/
         pdata(lpdat+i2)=gdata(lgdat+mx)                                        !/*te        259*/
         pdata(lpdat+n2+i2)=gdata(lgdat+mxgc(ngd)+mx)                           !/*te        260*/
  150    continue                                                               !/*te        261*/
         write(31,'(1h#," The ",a1," index set to ",i5,'//                      !/*te        262*/
     &    '" and the ",a1," index set to ",i5)')hv(1),i1,hv(3),i3               !/*te        263*/
!                                                                               !/*te        264*/
!       Write header and data the a formatted file for input                    !/*te        265*/
!       into the Tecplot utility for plotting.                                  !/*te        266*/
!                                                                               !/*te        267*/
         write(31,'(12h Variables=",a1,3h" ",a10,9h" "error")')                 !/*te        268*/
     &    hv(nv),hf                                                             !/*te        269*/
         write(31,'(" zone i=",i1," f=point")')n2                               !/*te        270*/
         do 155 i=1,n2                                                          !/*te        271*/
         write(31,'(3(1x,g13.6))')pdp(lpdp+i),pdata(lpdat+i),                   !/*te        272*/
     &    pdata(lpdat+n2+i)                                                     !/*te        273*/
  155    continue                                                               !/*te        274*/
         close(31)                                                              !/*te        275*/
         return                                                                 !/*te        276*/
!                                                                               !/*te        277*/
      else                                                                      !/*te        278*/
         do 160 i3=1,n3                                                         !/*te        279*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*te        280*/
         pdata(lpdat+i3)=gdata(lgdat+mx)                                        !/*te        281*/
         pdata(lpdat+n3+i3)=gdata(lgdat+mxgc(ngd)+mx)                           !/*te        282*/
  160    continue                                                               !/*te        283*/
         write(31,'(1h#," The ",a1," index set to ",i5,'//                      !/*te        284*/
     &    '" and the ",a1," index set to ",i5)')hv(1),i1,hv(2),i2               !/*te        285*/
!                                                                               !/*te        286*/
!       Write header and data the a formatted file for input                    !/*te        287*/
!       into the Tecplot utility for plotting.                                  !/*te        288*/
!                                                                               !/*te        289*/
         write(31,'(12h Variables=",a1,3h" ",a10,9h" "error")')                 !/*te        290*/
     &    hv(nv),hf                                                             !/*te        291*/
         write(31,'(" zone i=",i1," f=point")')n3                               !/*te        292*/
         do 190 i=1,n3                                                          !/*te        293*/
         write(31,'(3(1x,g13.6))')pda(lpda+i),pdata(lpdat+i),                   !/*te        294*/
     &    pdata(lpdat+n3+i)                                                     !/*te        295*/
  190    continue                                                               !/*te        296*/
         close(31)                                                              !/*te        297*/
         return                                                                 !/*te        298*/
!                                                                               !/*te        299*/
      endif                                                                     !/*te        300*/
!                                                                               !/*te        301*/
!       Two dinemsional plot data.                                              !/*te        302*/
!       Get dependent variables and index of other one.                         !/*te        303*/
  200 ni=0                                                                      !/*te        304*/
      write(6,'("Plot data as fcn. of rp, pa, ar or q to exit ?")')             !/*te        305*/
      write(6,'("Default = rp")')                                               !/*te        306*/
      write(6,'("$")')                                                          !/*te        307*/
      read(5,'(a3)')hi                                                          !/*te        308*/
      if(hi(1:1).eq.'q')go to 500                                               !/*te        309*/
      if(hi(1:2).eq.'  ')hi(1:2)='rp'                                           !/*te        310*/
      if(hi(1:2).eq.'rp') then                                                  !/*te        311*/
         ni=1                                                                   !/*te        312*/
  210    write(6,'("What indicies do you what for a?")')                        !/*te        313*/
         write(6,'(" <default = 1>")')                                          !/*te        314*/
         write(6,'("$")')                                                       !/*te        315*/
         read(5,'(a30)')hb                                                      !/*te        316*/
         kx=0                                                                   !/*te        317*/
         do 215 j=1,30                                                          !/*te        318*/
         n=index('0123456789',hb(j:j))                                          !/*te        319*/
         if(n.ne.0) then                                                        !/*te        320*/
            kx=kx*10+n-1                                                        !/*te        321*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*te        322*/
            i3=kx                                                               !/*te        323*/
            go to 218                                                           !/*te        324*/
         endif                                                                  !/*te        325*/
  215    continue                                                               !/*te        326*/
         i3=1                                                                   !/*te        327*/
!                                                                               !/*te        328*/
  218    if(i3.gt.ng3(ngd)) then                                                !/*te        329*/
            write(6,'("This index is too large!")')                             !/*te        330*/
            go to 210                                                           !/*te        331*/
         endif                                                                  !/*te        332*/
      elseif(hi(1:2).eq.'pa') then                                              !/*te        333*/
         ni=2                                                                   !/*te        334*/
  220    write(6,'("What indicies do you what for r?")')                        !/*te        335*/
         write(6,'(" <default = 1>")')                                          !/*te        336*/
         write(6,'("$")')                                                       !/*te        337*/
         read(5,'(a30)')hb                                                      !/*te        338*/
         kx=0                                                                   !/*te        339*/
         do 225 j=1,30                                                          !/*te        340*/
         n=index('0123456789',hb(j:j))                                          !/*te        341*/
         if(n.ne.0) then                                                        !/*te        342*/
            kx=kx*10+n-1                                                        !/*te        343*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*te        344*/
            i1=kx                                                               !/*te        345*/
            go to 228                                                           !/*te        346*/
         endif                                                                  !/*te        347*/
  225    continue                                                               !/*te        348*/
         i1=1                                                                   !/*te        349*/
!                                                                               !/*te        350*/
  228    if(i1.gt.ng1(ngd)-1) then                                              !/*te        351*/
            write(6,'("This index is too large!")')                             !/*te        352*/
            go to 220                                                           !/*te        353*/
         endif                                                                  !/*te        354*/
      elseif(hi(1:2).eq.'ar') then                                              !/*te        355*/
         ni=3                                                                   !/*te        356*/
  230    write(6,'("What indicies do you what for p?")')                        !/*te        357*/
         write(6,'(" <default = 1>")')                                          !/*te        358*/
         write(6,'("$")')                                                       !/*te        359*/
         read(5,'(a30)')hb                                                      !/*te        360*/
         kx=0                                                                   !/*te        361*/
         do 235 j=1,30                                                          !/*te        362*/
         n=index('0123456789',hb(j:j))                                          !/*te        363*/
         if(n.ne.0) then                                                        !/*te        364*/
            kx=kx*10+n-1                                                        !/*te        365*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*te        366*/
            i2=kx                                                               !/*te        367*/
            go to 238                                                           !/*te        368*/
         endif                                                                  !/*te        369*/
  235    continue                                                               !/*te        370*/
         i2=1                                                                   !/*te        371*/
!                                                                               !/*te        372*/
  238    if(i2.gt.ng2(ngd)) then                                                !/*te        373*/
            write(6,'("This index is too large!")')                             !/*te        374*/
            go to 230                                                           !/*te        375*/
         endif                                                                  !/*te        376*/
      else                                                                      !/*te        377*/
         go to 200                                                              !/*te        378*/
      endif                                                                     !/*te        379*/
!                                                                               !/*te        380*/
!      ni is an indicator as to which two coordinate varibles will be           !/*te        381*/
!      looped over.    1,2,3  --  rp,pa,ar                                      !/*te        382*/
!      The remaining index will be fixed.                                       !/*te        383*/
      if(ni.eq.1) then                                                          !/*te        384*/
         write(31,'(14h Variables = ",a1,4h", ",a1,4h", ",'//                   !/*te        385*/
     &    'a10,10h", "error")')hv(1),hv(2),hf                                   !/*te        386*/
         write(31,'(" zone i=",i3,1h,," j=",i3,1h,," f=point")')n1,n2           !/*te        387*/
         do 240 i2=1,n2                                                         !/*te        388*/
         do 240 i1=1,n1                                                         !/*te        389*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*te        390*/
         write(31,'(4(1x,g13.6))')pdr(lpdr+i1),pdp(lpdp+i2),                    !/*te        391*/
     &   gdata(lgdat+mx),gdata(lgdat+mxgc(ngd)+mx)                              !/*te        392*/
  240    continue                                                               !/*te        393*/
         close(31)                                                              !/*te        394*/
         return                                                                 !/*te        395*/
!                                                                               !/*te        396*/
      elseif(ni.eq.2) then                                                      !/*te        397*/
         write(31,'(14h Variables = ",a1,4h", ",a1,4h", ",'//                   !/*te        398*/
     &    'a10,10h", "error")')hv(3),hv(2),hf                                   !/*te        399*/
         write(31,'(" zone i=",i3,1h,," j=",i3,1h,," f=point")')n3,n2           !/*te        400*/
         do 250 i2=1,n2                                                         !/*te        401*/
         do 250 i3=1,n3                                                         !/*te        402*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*te        403*/
         write(31,'(4(1x,g13.6))')pda(lpda+i3),pdp(lpdp+i2),                    !/*te        404*/
     &    gdata(lgdat+mx),gdata(lgdat+mxgc(ngd)+mx)                             !/*te        405*/
  250    continue                                                               !/*te        406*/
         close(31)                                                              !/*te        407*/
         return                                                                 !/*te        408*/
!                                                                               !/*te        409*/
      else                                                                      !/*te        410*/
         write(31,'(14h Variables = ",a1,4h", ",a1,4h", ",'//                   !/*te        411*/
     &    'a10,10h", "error")')hv(3),hv(1),hf                                   !/*te        412*/
         write(31,'(" zone i=",i3,1h,," j=",i3,1h,," f=point")')n3,n1           !/*te        413*/
         do 260 i1=1,n1                                                         !/*te        414*/
         do 260 i3=1,n3                                                         !/*te        415*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*te        416*/
         write(31,'(4(1x,g13.6))')pda(lpda+i3),pdr(lpdr+i1),                    !/*te        417*/
     &    gdata(lgdat+mx),gdata(lgdat+mxgc(ngd)+mx)                             !/*te        418*/
  260    continue                                                               !/*te        419*/
         close(31)                                                              !/*te        420*/
         return                                                                 !/*te        421*/
!                                                                               !/*te        422*/
      endif                                                                     !/*te        423*/
!                                                                               !/*te        424*/
  500 return                                                                    !/*te        425*/
!                                                                               !/*te        426*/
      end                                                                       !/*te        427*/
