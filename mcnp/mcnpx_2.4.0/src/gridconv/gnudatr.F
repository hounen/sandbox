!     $Id: gnudatr.F,v 1.7 2002/03/27 18:13:51 gwm Exp $                      !/*gn          1*/
      subroutine gnudatr                                                        !/*gn          2*/
!                                                                               !/*gn          3*/
!       This subroutine is designed to convert input data from a                !/*gn          4*/
!       mesh tally set to a data set compatible with the input                  !/*gn          5*/
!       requirements of the Gnuplot graphics package.                           !/*gn          6*/
!                                                                               !/*gn          7*/
!       Written by E. C. Snow  12/11/2000.   (revised: 11/09/2001)              !/*gn          8*/
!                                                                               !/*gn          9*/
#include <gridconv/gcdouble.h>                                                  /*!gn         10*/
#include <gridconv/gccom5.h>                                                    /*!gn         11*/
!                                                                               !/*gn         12*/
      dimension gdr(1),gdz(1),gdt(1),gdata(1),pdr(1),pdz(1),                    !/*gn         13*/
     & pdt(1),pdata(1)                                                          !/*gn         14*/
      equivalence (das,gdr,gdz,gdt,gdata,pdr,pdz,pdt,pdata)                     !/*gn         15*/
      dimension cl(20)                                                          !/*gn         16*/
!                                                                               !/*gn         17*/
      character ha*1,hi*3,hf*30,hb*30,hj(2)*5,hv(3)*1,hl(3)*2                   !/*gn         18*/
      data hv/'R','Z','T'/,hj/'tally','error'/,hl/'RZ','ZT','TR'/               !/*gn         19*/
!                                                                               !/*gn         20*/
      write(6,'("What Gnuplot filenames do you want.")')                        !/*gn         21*/
      write(6,'("They will be called <name>#a.")')                              !/*gn         22*/
      write(6,'("The default name is gnudata.")')                               !/*gn         23*/
      write(6,'("$")')                                                          !/*gn         24*/
      read(5,'(a30)')hf                                                         !/*gn         25*/
      jb=leng(hf)                                                               !/*gn         26*/
      if(jb.eq.0)then                                                           !/*gn         27*/
         jb=7                                                                   !/*gn         28*/
         hf(1:7)='gnudata'                                                      !/*gn         29*/
         write(6,'("Using default output file name: gnudata")')                 !/*gn         30*/
      endif                                                                     !/*gn         31*/
!                                                                               !/*gn         32*/
!       Write information about this mdata file.                                !/*gn         33*/
!                                                                               !/*gn         34*/
      write(6,'("The INPUT title for this problem was:")')                      !/*gn         35*/
      write(6,'(1h ,a80)')aid                                                   !/*gn         36*/
      write(6,'(1h ,"The number of histories = ",i10)')nps                      !/*gn         37*/
      write(6,'("********")')                                                   !/*gn         38*/
      write(6,'(" ")')                                                          !/*gn         39*/
!                                                                               !/*gn         40*/
   10 write(6,'("Enter plot dimensionality, 1,2 or 9 to exit")')                !/*gn         41*/
      write(6,'("$")')                                                          !/*gn         42*/
      read(5,'(a30)')hb                                                         !/*gn         43*/
      kx=0                                                                      !/*gn         44*/
      np=0                                                                      !/*gn         45*/
      do 20 j=1,30                                                              !/*gn         46*/
      n=index('0123456789',hb(j:j))                                             !/*gn         47*/
      if(n.ne.0) then                                                           !/*gn         48*/
         kx=kx*10+n-1                                                           !/*gn         49*/
      elseif(n.eq.0.and.kx.gt.0) then                                           !/*gn         50*/
         np=kx                                                                  !/*gn         51*/
         go to 30                                                               !/*gn         52*/
      elseif(np.eq.0.and.j.eq.30) then                                          !/*gn         53*/
         write(6,'(" There is no default value! ")')                            !/*gn         54*/
         go to 10                                                               !/*gn         55*/
      endif                                                                     !/*gn         56*/
   20 continue                                                                  !/*gn         57*/
!                                                                               !/*gn         58*/
   30 if(np.eq.9)go to 999                                                      !/*gn         59*/
      if(np.ge.1.and.np.le.2)go to 40                                           !/*gn         60*/
      go to 10                                                                  !/*gn         61*/
!                                                                               !/*gn         62*/
   40 n1=ng1(ngd)-1                                                             !/*gn         63*/
      n2=ng2(ngd)-1                                                             !/*gn         64*/
      n3=ng3(ngd)                                                               !/*gn         65*/
!                                                                               !/*gn         66*/
      write(6,'("Do you wish to plot the tally or error?")')                    !/*gn         67*/
      write(6,'("  <default = tally>")')                                        !/*gn         68*/
      read(5,'(a30)')hb                                                         !/*gn         69*/
      md=0                                                                      !/*gn         70*/
      if(hb(1:5).eq.'error')md=1                                                !/*gn         71*/
!                                                                               !/*gn         72*/
!                                                                               !/*gn         73*/
      go to (100,500)np                                                         !/*gn         74*/
!                                                                               !/*gn         75*/
!       One dinemsional plot data.                                              !/*gn         76*/
!       Get dependent variable and index of other two.                          !/*gn         77*/
  100 nv=0                                                                      !/*gn         78*/
      write(6,'("Plot data as fcn. of r, z, t, or q to exit?")')                !/*gn         79*/
      write(6,'("r = radius, z = z-coord., and t = azimuthial.")')              !/*gn         80*/
      write(6,'("$")')                                                          !/*gn         81*/
      read(5,'(a3)')hi                                                          !/*gn         82*/
      if(hi(1:1).eq.'q')go to 999                                               !/*gn         83*/
      if(hi(1:1).eq.'r')nv=1                                                    !/*gn         84*/
      if(hi(1:1).eq.'z')nv=2                                                    !/*gn         85*/
      if(hi(1:1).eq.'t')nv=3                                                    !/*gn         86*/
!                                                                               !/*gn         87*/
      if(nv.eq.1) then                                                          !/*gn         88*/
  110    write(6,'("What indicies do you what for z and t?")')                  !/*gn         89*/
         write(6,'(" <default = 1,1>")')                                        !/*gn         90*/
         write(6,'("$")')                                                       !/*gn         91*/
         read(5,'(a30)')hb                                                      !/*gn         92*/
         do 120 j=1,30                                                          !/*gn         93*/
         n=index('0123456789',hb(j:j))                                          !/*gn         94*/
         if(n.ne.0) go to 130                                                   !/*gn         95*/
  120    continue                                                               !/*gn         96*/
         i2=1                                                                   !/*gn         97*/
         i3=1                                                                   !/*gn         98*/
         go to 180                                                              !/*gn         99*/
!                                                                               !/*gn        100*/
  130    kk=1                                                                   !/*gn        101*/
         do 160 i=1,2                                                           !/*gn        102*/
         kx=0                                                                   !/*gn        103*/
         do 140 j=kk,30                                                         !/*gn        104*/
         n=index('0123456789',hb(j:j))                                          !/*gn        105*/
         if(n.ne.0) then                                                        !/*gn        106*/
            kx=kx*10+n-1                                                        !/*gn        107*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*gn        108*/
            kk=j+1                                                              !/*gn        109*/
            go to 150                                                           !/*gn        110*/
         endif                                                                  !/*gn        111*/
  140    continue                                                               !/*gn        112*/
!                                                                               !/*gn        113*/
  150    if(i.eq.1) then                                                        !/*gn        114*/
            i2=kx                                                               !/*gn        115*/
         else                                                                   !/*gn        116*/
            i3=kx                                                               !/*gn        117*/
         endif                                                                  !/*gn        118*/
  160    continue                                                               !/*gn        119*/
!                                                                               !/*gn        120*/
  180    if(i2.gt.n2.or.i3.gt.n3) then                                          !/*gn        121*/
            write(6,'("One of these indices is too large!")')                   !/*gn        122*/
            go to 110                                                           !/*gn        123*/
         endif                                                                  !/*gn        124*/
      elseif(nv.eq.2) then                                                      !/*gn        125*/
  200    write(6,'("What indicies do you what for r and t?")')                  !/*gn        126*/
         write(6,'(" <default = 1,1>")')                                        !/*gn        127*/
         write(6,'("$")')                                                       !/*gn        128*/
         read(5,'(a30)')hb                                                      !/*gn        129*/
         do 220 j=1,30                                                          !/*gn        130*/
         n=index('0123456789',hb(j:j))                                          !/*gn        131*/
         if(n.ne.0) go to 230                                                   !/*gn        132*/
  220    continue                                                               !/*gn        133*/
         i1=1                                                                   !/*gn        134*/
         i3=1                                                                   !/*gn        135*/
         go to 280                                                              !/*gn        136*/
!                                                                               !/*gn        137*/
  230    kk=1                                                                   !/*gn        138*/
         do 260 i=1,2                                                           !/*gn        139*/
         kx=0                                                                   !/*gn        140*/
         do 240 j=kk,30                                                         !/*gn        141*/
         n=index('0123456789',hb(j:j))                                          !/*gn        142*/
         if(n.ne.0) then                                                        !/*gn        143*/
            kx=kx*10+n-1                                                        !/*gn        144*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*gn        145*/
            kk=j+1                                                              !/*gn        146*/
            go to 250                                                           !/*gn        147*/
         endif                                                                  !/*gn        148*/
  240    continue                                                               !/*gn        149*/
!                                                                               !/*gn        150*/
  250    if(i.eq.1) then                                                        !/*gn        151*/
            i1=kx                                                               !/*gn        152*/
         else                                                                   !/*gn        153*/
            i3=kx                                                               !/*gn        154*/
         endif                                                                  !/*gn        155*/
  260    continue                                                               !/*gn        156*/
!                                                                               !/*gn        157*/
  280    if(i1.gt.n1.or.i3.gt.n3) then                                          !/*gn        158*/
            write(6,'("One of these indices is too large!")')                   !/*gn        159*/
            go to 200                                                           !/*gn        160*/
         endif                                                                  !/*gn        161*/
!                                                                               !/*gn        162*/
      elseif(nv.eq.3) then                                                      !/*gn        163*/
  300    write(6,'("What indicies do you what for r and z?")')                  !/*gn        164*/
         write(6,'(" <default = 1,1>")')                                        !/*gn        165*/
         write(6,'("$")')                                                       !/*gn        166*/
         read(5,'(a30)')hb                                                      !/*gn        167*/
         do 320 j=1,30                                                          !/*gn        168*/
         n=index('0123456789',hb(j:j))                                          !/*gn        169*/
         if(n.ne.0) go to 330                                                   !/*gn        170*/
  320    continue                                                               !/*gn        171*/
         i1=1                                                                   !/*gn        172*/
         i2=1                                                                   !/*gn        173*/
         go to 380                                                              !/*gn        174*/
!                                                                               !/*gn        175*/
  330    kk=1                                                                   !/*gn        176*/
         do 360 i=1,2                                                           !/*gn        177*/
         kx=0                                                                   !/*gn        178*/
         do 340 j=kk,30                                                         !/*gn        179*/
         n=index('0123456789',hb(j:j))                                          !/*gn        180*/
         if(n.ne.0) then                                                        !/*gn        181*/
            kx=kx*10+n-1                                                        !/*gn        182*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*gn        183*/
            kk=j+1                                                              !/*gn        184*/
            go to 350                                                           !/*gn        185*/
         endif                                                                  !/*gn        186*/
  340    continue                                                               !/*gn        187*/
!                                                                               !/*gn        188*/
  350    if(i.eq.1) then                                                        !/*gn        189*/
            i1=kx                                                               !/*gn        190*/
         else                                                                   !/*gn        191*/
            i2=kx                                                               !/*gn        192*/
         endif                                                                  !/*gn        193*/
  360    continue                                                               !/*gn        194*/
!                                                                               !/*gn        195*/
  380    if(i1.gt.n1.or.i2.gt.n2) then                                          !/*gn        196*/
            write(6,'("One of these indices is too large!")')                   !/*gn        197*/
            go to 300                                                           !/*gn        198*/
         endif                                                                  !/*gn        199*/
      else                                                                      !/*gn        200*/
         go to 100                                                              !/*gn        201*/
      endif                                                                     !/*gn        202*/
!                                                                               !/*gn        203*/
!       nv is the number of the coordinate variable that will                   !/*gn        204*/
!       looped over.   1,2,3 - r,z,t.                                           !/*gn        205*/
!       The other two indices will be fixed.                                    !/*gn        206*/
!                                                                               !/*gn        207*/
      if(nv.eq.1) then                                                          !/*gn        208*/
         do 400 i1=1,n1                                                         !/*gn        209*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*gn        210*/
         if(md.eq.0) then                                                       !/*gn        211*/
            pdata(lpdat+i1)=gdata(lgdat+mx)                                     !/*gn        212*/
         else                                                                   !/*gn        213*/
            pdata(lpdat+i1)=gdata(lgdat+mxgc(ngd)+mx)                           !/*gn        214*/
         endif                                                                  !/*gn        215*/
  400    continue                                                               !/*gn        216*/
!                                                                               !/*gn        217*/
!       Write header and data the a formatted file for input                    !/*gn        218*/
!       into the Gnuplot utility for plotting.                                  !/*gn        219*/
!                                                                               !/*gn        220*/
!       Write information about this mdata file.                                !/*gn        221*/
!                                                                               !/*gn        222*/
         hb=hf(1:jb)//'1Da'                                                     !/*gn        223*/
         call unique(hb,6)                                                      !/*gn        224*/
         open(31,file=hb,form='formatted',status='new')                         !/*gn        225*/
         write(31,'(1h#)')                                                      !/*gn        226*/
         write(31,'(1h# ,a80)')aid                                              !/*gn        227*/
         write(31,'(1h# ,"The number of histories = ",i10)')nps                 !/*gn        228*/
         write(31,'(1h# ,"These are ",a10,'//                                   !/*gn        229*/
     &    '" data for the cylindrical mesh tally # ",i5)')                      !/*gn        230*/
     &    hip(igpt(ngd)),nugd(ngd)                                              !/*gn        231*/
         write(31,'(1h# ,"  ***************** ")')                              !/*gn        232*/
         write(31,'(1h# ,"The following is ",a5," data vs. ",a1)')              !/*gn        233*/
     &    hj(md+1),hv(nv)                                                       !/*gn        234*/
         write(31,'(1h#)')                                                      !/*gn        235*/
!                                                                               !/*gn        236*/
         do 405 i=1,n1                                                          !/*gn        237*/
         write(31,'(2(1x,g13.6))')pdr(lpdr+i),pdata(lpdat+i)                    !/*gn        238*/
  405    continue                                                               !/*gn        239*/
         close(31)                                                              !/*gn        240*/
         return                                                                 !/*gn        241*/
!                                                                               !/*gn        242*/
      elseif(nv.eq.2) then                                                      !/*gn        243*/
         do 410 i2=1,n2                                                         !/*gn        244*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*gn        245*/
         if(md.eq.0) then                                                       !/*gn        246*/
            pdata(lpdat+i2)=gdata(lgdat+mx)                                     !/*gn        247*/
         else                                                                   !/*gn        248*/
            pdata(lpdat+i2)=gdata(lgdat+mxgc(ngd)+mx)                           !/*gn        249*/
         endif                                                                  !/*gn        250*/
  410    continue                                                               !/*gn        251*/
!                                                                               !/*gn        252*/
!       Write header and data the a formatted file for input                    !/*gn        253*/
!       into the Gnuplot utility for plotting.                                  !/*gn        254*/
!                                                                               !/*gn        255*/
!       Write information about this mdata file.                                !/*gn        256*/
!                                                                               !/*gn        257*/
         hb=hf(1:jb)//'1Da'                                                     !/*gn        258*/
         call unique(hb,6)                                                      !/*gn        259*/
         open(31,file=hb,form='formatted',status='new')                         !/*gn        260*/
         write(31,'(1h#)')                                                      !/*gn        261*/
         write(31,'(1h# ,a80)')aid                                              !/*gn        262*/
         write(31,'(1h# ,"The number of histories = ",i10)')nps                 !/*gn        263*/
         write(31,'(1h# ,"These are ",a10,'//                                   !/*gn        264*/
     &    '" data for the cylindrical mesh tally # ",i5)')                      !/*gn        265*/
     &    hip(igpt(ngd)),nugd(ngd)                                              !/*gn        266*/
         write(31,'(1h# ,"  ***************** ")')                              !/*gn        267*/
         write(31,'(1h# ,"The following is ",a5," data vs. ",a1)')              !/*gn        268*/
     &    hj(md+1),hv(nv)                                                       !/*gn        269*/
         write(31,'(1h#)')                                                      !/*gn        270*/
!                                                                               !/*gn        271*/
         do 415 i=1,n2                                                          !/*gn        272*/
         write(31,'(2(1x,g13.6))')pdz(lpdz+i),pdata(lpdat+i)                    !/*gn        273*/
  415    continue                                                               !/*gn        274*/
         close(31)                                                              !/*gn        275*/
         return                                                                 !/*gn        276*/
!                                                                               !/*gn        277*/
      else                                                                      !/*gn        278*/
         do 420 i3=1,n3                                                         !/*gn        279*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*gn        280*/
         if(md.eq.0) then                                                       !/*gn        281*/
            pdata(lpdat+i3)=gdata(lgdat+mx)                                     !/*gn        282*/
         else                                                                   !/*gn        283*/
            pdata(lpdat+i3)=gdata(lgdat+mxgc(ngd)+mx)                           !/*gn        284*/
         endif                                                                  !/*gn        285*/
  420    continue                                                               !/*gn        286*/
!                                                                               !/*gn        287*/
!       Write header and data the a formatted file for input                    !/*gn        288*/
!       into the Gnuplot utility for plotting.                                  !/*gn        289*/
!                                                                               !/*gn        290*/
!       Write information about this mdata file.                                !/*gn        291*/
!                                                                               !/*gn        292*/
         hb=hf(1:jb)//'1Da'                                                     !/*gn        293*/
         call unique(hb,6)                                                      !/*gn        294*/
         open(31,file=hb,form='formatted',status='new')                         !/*gn        295*/
         write(31,'(1h#)')                                                      !/*gn        296*/
         write(31,'(1h# ,a80)')aid                                              !/*gn        297*/
         write(31,'(1h# ,"The number of histories = ",i10)')nps                 !/*gn        298*/
         write(31,'(1h# ,"These are ",a10,'//                                   !/*gn        299*/
     &    '" data for the cylindrical mesh tally # ",i5)')                      !/*gn        300*/
     &    hip(igpt(ngd)),nugd(ngd)                                              !/*gn        301*/
         write(31,'(1h# ,"  ***************** ")')                              !/*gn        302*/
         write(31,'(1h# ,"The following is ",a5," data vs. ",a1)')              !/*gn        303*/
     &    hj(md+1),hv(nv)                                                       !/*gn        304*/
         write(31,'(1h#)')                                                      !/*gn        305*/
!                                                                               !/*gn        306*/
         do 440 i=1,n3                                                          !/*gn        307*/
         write(31,'(2(1x,g13.6))')pdt(lpdt+i),pdata(lpdat+i)                    !/*gn        308*/
  440    continue                                                               !/*gn        309*/
         close(31)                                                              !/*gn        310*/
         return                                                                 !/*gn        311*/
!                                                                               !/*gn        312*/
      endif                                                                     !/*gn        313*/
!                                                                               !/*gn        314*/
!       Two dinemsional plot data.                                              !/*gn        315*/
!       Get dependent variable and index of other one.                          !/*gn        316*/
!                                                                               !/*gn        317*/
  500 ni=0                                                                      !/*gn        318*/
      write(6,'("Plot data as fcn. of rz, zt, tr or q to exit ?")')             !/*gn        319*/
      write(6,'("r = radius, z = z-coord., and t = azimuthial.")')              !/*gn        320*/
      write(6,'("$")')                                                          !/*gn        321*/
      read(5,'(a3)')hi                                                          !/*gn        322*/
      if(hi(1:1).eq.'q')go to 999                                               !/*gn        323*/
      if(hi(1:2).eq.'rz'.or.hi(1:2).eq.'zr') then                               !/*gn        324*/
         ni=1                                                                   !/*gn        325*/
  510    write(6,'("What indicies do you what for t?")')                        !/*gn        326*/
         write(6,'(" <default = 1>")')                                          !/*gn        327*/
         write(6,'("$")')                                                       !/*gn        328*/
         read(5,'(a30)')hb                                                      !/*gn        329*/
         kx=0                                                                   !/*gn        330*/
         do 520 j=1,30                                                          !/*gn        331*/
         n=index('0123456789',hb(j:j))                                          !/*gn        332*/
         if(n.ne.0) then                                                        !/*gn        333*/
            kx=kx*10+n-1                                                        !/*gn        334*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*gn        335*/
            i3=kx                                                               !/*gn        336*/
            go to 530                                                           !/*gn        337*/
         endif                                                                  !/*gn        338*/
  520    continue                                                               !/*gn        339*/
         i3=1                                                                   !/*gn        340*/
!                                                                               !/*gn        341*/
  530    if(i3.gt.n3) then                                                      !/*gn        342*/
            write(6,'("This index is too large!")')                             !/*gn        343*/
            go to 510                                                           !/*gn        344*/
         endif                                                                  !/*gn        345*/
      elseif(hi(1:2).eq.'zt'.or.hi(1:2).eq.'tz') then                           !/*gn        346*/
         ni=2                                                                   !/*gn        347*/
  540    write(6,'("What indicies do you what for r?")')                        !/*gn        348*/
         write(6,'(" <default = 1>")')                                          !/*gn        349*/
         write(6,'("$")')                                                       !/*gn        350*/
         read(5,'(a30)')hb                                                      !/*gn        351*/
         kx=0                                                                   !/*gn        352*/
         do 550 j=1,30                                                          !/*gn        353*/
         n=index('0123456789',hb(j:j))                                          !/*gn        354*/
         if(n.ne.0) then                                                        !/*gn        355*/
            kx=kx*10+n-1                                                        !/*gn        356*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*gn        357*/
            i1=kx                                                               !/*gn        358*/
            go to 560                                                           !/*gn        359*/
         endif                                                                  !/*gn        360*/
  550    continue                                                               !/*gn        361*/
         i1=1                                                                   !/*gn        362*/
!                                                                               !/*gn        363*/
  560    if(i1.gt.n1) then                                                      !/*gn        364*/
            write(6,'("This index is too large!")')                             !/*gn        365*/
            go to 540                                                           !/*gn        366*/
         endif                                                                  !/*gn        367*/
      elseif(hi(1:2).eq.'tr'.or.hi(1:2).eq.'rt') then                           !/*gn        368*/
         ni=3                                                                   !/*gn        369*/
  570    write(6,'("What indicies do you what for z?")')                        !/*gn        370*/
         write(6,'(" <default = 1>")')                                          !/*gn        371*/
         write(6,'("$")')                                                       !/*gn        372*/
         read(5,'(a30)')hb                                                      !/*gn        373*/
         kx=0                                                                   !/*gn        374*/
         do 580 j=1,30                                                          !/*gn        375*/
         n=index('0123456789',hb(j:j))                                          !/*gn        376*/
         if(n.ne.0) then                                                        !/*gn        377*/
            kx=kx*10+n-1                                                        !/*gn        378*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*gn        379*/
            i2=kx                                                               !/*gn        380*/
            go to 600                                                           !/*gn        381*/
         endif                                                                  !/*gn        382*/
  580    continue                                                               !/*gn        383*/
         i2=1                                                                   !/*gn        384*/
!                                                                               !/*gn        385*/
  600    if(i2.gt.n2) then                                                      !/*gn        386*/
            write(6,'("This index is too large!")')                             !/*gn        387*/
            go to 570                                                           !/*gn        388*/
         endif                                                                  !/*gn        389*/
      else                                                                      !/*gn        390*/
         go to 500                                                              !/*gn        391*/
      endif                                                                     !/*gn        392*/
!                                                                               !/*gn        393*/
!      ni is an indicator as to which two coordinate varibles will be           !/*gn        394*/
!      looped over.    1,2,3  --  rz,zt,tr                                      !/*gn        395*/
!      The remaining index will be fixed.                                       !/*gn        396*/
!                                                                               !/*gn        397*/
!      Find min. and max. values and ask for number of colors                   !/*gn        398*/
!      to be used in a 2D contour plot.                                         !/*gn        399*/
!                                                                               !/*gn        400*/
      pn=1.e30                                                                  !/*gn        401*/
      px=0.                                                                     !/*gn        402*/
      if(ni.eq.1) then                                                          !/*gn        403*/
         do 610 i2=1,n2                                                         !/*gn        404*/
         do 610 i1=1,n1                                                         !/*gn        405*/
         mx=mxgc(ngd)*md+(i3-1)*n1*n2+(i2-1)*n1+i1                              !/*gn        406*/
         mz=(i2-1)*n1+i1                                                        !/*gn        407*/
         pdata(lpdat+mz)=gdata(lgdat+mx)                                        !/*gn        408*/
         if(pdata(lpdat+mz).lt.pn)pn=pdata(lpdat+mz)                            !/*gn        409*/
         if(pdata(lpdat+mz).gt.px)px=pdata(lpdat+mz)                            !/*gn        410*/
  610    continue                                                               !/*gn        411*/
         na=n1                                                                  !/*gn        412*/
         nb=n2                                                                  !/*gn        413*/
!                                                                               !/*gn        414*/
      elseif(ni.eq.2) then                                                      !/*gn        415*/
         do 620 i3=1,n3                                                         !/*gn        416*/
         do 620 i2=1,n2                                                         !/*gn        417*/
         mx=mxgc(ngd)*md+(i3-1)*n1*n2+(i2-1)*n1+i1                              !/*gn        418*/
         mz=(i3-1)*n2+i2                                                        !/*gn        419*/
         pdata(lpdat+mz)=gdata(lgdat+mx)                                        !/*gn        420*/
         if(pdata(lpdat+mz).lt.pn)pn=pdata(lpdat+mz)                            !/*gn        421*/
         if(pdata(lpdat+mz).gt.px)px=pdata(lpdat+mz)                            !/*gn        422*/
  620    continue                                                               !/*gn        423*/
         na=n2                                                                  !/*gn        424*/
         nb=n3                                                                  !/*gn        425*/
!                                                                               !/*gn        426*/
      else                                                                      !/*gn        427*/
         do 630 i1=1,n1                                                         !/*gn        428*/
         do 630 i3=1,n3                                                         !/*gn        429*/
         mx=mxgc(ngd)*md+(i3-1)*n1*n2+(i2-1)*n1+i1                              !/*gn        430*/
         mz=(i1-1)*n3+i3                                                        !/*gn        431*/
         pdata(lpdat+mz)=gdata(lgdat+mx)                                        !/*gn        432*/
         if(pdata(lpdat+mz).lt.pn)pn=pdata(lpdat+mz)                            !/*gn        433*/
         if(pdata(lpdat+mz).gt.px)px=pdata(lpdat+mz)                            !/*gn        434*/
  630    continue                                                               !/*gn        435*/
         na=n3                                                                  !/*gn        436*/
         nb=n1                                                                  !/*gn        437*/
!                                                                               !/*gn        438*/
      endif                                                                     !/*gn        439*/
!                                                                               !/*gn        440*/
      write(6,'("How many contours do you want to plot?")')                     !/*gn        441*/
      write(6,'("  <no default!  Max=20> ")')                                   !/*gn        442*/
      read(5,'(i2)') nc                                                         !/*gn        443*/
      write(6,'("Data min and max are ",2(1pe13.6))')pn,px                      !/*gn        444*/
      write(6,'("Do you wish to reset min and max values? (y/n)")')             !/*gn        445*/
      write(6,'("  <default = n> ")')                                           !/*gn        446*/
      read(5,'(1a)')ha                                                          !/*gn        447*/
      if(ha.eq.'n'.or.ha.eq.' ') go to 640                                      !/*gn        448*/
      write(6,'("Enter min and max in 2(1pe13.6) format.")')                    !/*gn        449*/
      write(6,'("  <No default at this point!>")')                              !/*gn        450*/
      read(5,'(2(1pe14.6))')pn,px                                               !/*gn        451*/
!                                                                               !/*gn        452*/
!      Set up contour bin limits.                                               !/*gn        453*/
!                                                                               !/*gn        454*/
  640 dx=(px-pn)/nc                                                             !/*gn        455*/
      cl(1)=pn                                                                  !/*gn        456*/
      do 650 i=2,nc+1                                                           !/*gn        457*/
      cl(i)=cl(i-1)+dx                                                          !/*gn        458*/
  650 continue                                                                  !/*gn        459*/
!                                                                               !/*gn        460*/
      write(6,'("The color bins are for data limits = ")')                      !/*gn        461*/
      write(6,'(5(1pe13.5))')(cl(i),i=1,nc+1)                                   !/*gn        462*/
!                                                                               !/*gn        463*/
      do 670 l=2,nc+1                                                           !/*gn        464*/
      if(l-1.lt.10) then                                                        !/*gn        465*/
         hb=hf(1:jb)//'0'                                                       !/*gn        466*/
         write(hb(jb+2:jb+2),'(i1)')l-1                                         !/*gn        467*/
         hb(jb+3:jb+3)='a'                                                      !/*gn        468*/
      else                                                                      !/*gn        469*/
         hb=hf(1:jb)                                                            !/*gn        470*/
         write(hb(jb+1:nb+2),'(i2)')l-1                                         !/*gn        471*/
         hb(jb+3:jb+3)='a'                                                      !/*gn        472*/
      endif                                                                     !/*gn        473*/
      call unique(hb,6)                                                         !/*gn        474*/
      open(31,file=hb,form='formatted',status='new')                            !/*gn        475*/
!                                                                               !/*gn        476*/
!      Write some header information here.                                      !/*gn        477*/
!                                                                               !/*gn        478*/
      write(31,'(1h#)')                                                         !/*gn        479*/
      write(31,'(1h# ,a80)')aid                                                 !/*gn        480*/
      write(31,'(1h# ,"The number of histories = ",i10)')nps                    !/*gn        481*/
      write(31,'(1h# ,"These are ",a10,'//                                      !/*gn        482*/
     & '" data for the cylindrical mesh tally # ",i5)')                         !/*gn        483*/
     & hip(igpt(ngd)),nugd(ngd)                                                 !/*gn        484*/
      write(31,'(1h# ,"  ***************** ")')                                 !/*gn        485*/
      write(31,'(1h# ,"The following are ",a2," values for ",a5)')              !/*gn        486*/
     & hl(ni),hj(md+1)                                                          !/*gn        487*/
      write(31,'(1h# ,"values between ",1pe13.6," and ",1pe13.6)')              !/*gn        488*/
     & cl(l-1),cl(l)                                                            !/*gn        489*/
      write(31,'(1h#)')                                                         !/*gn        490*/
!                                                                               !/*gn        491*/
      do 660 j=1,nb                                                             !/*gn        492*/
      do 660 i=1,na                                                             !/*gn        493*/
      mz=(j-1)*na+i                                                             !/*gn        494*/
      if(pdata(lpdat+mz).gt.cl(l-1).and.pdata(lpdat+mz).le.cl(l)) then          !/*gn        495*/
         if(ni.eq.1) then                                                       !/*gn        496*/
            write(31,'(2(1pe14.6))')pdr(lpdr+i),pdz(lpdz+j)                     !/*gn        497*/
         elseif(ni.eq.2) then                                                   !/*gn        498*/
            write(31,'(2(1pe14.6))')pdz(lpdz+i),pdt(lpdt+j)                     !/*gn        499*/
         else                                                                   !/*gn        500*/
            write(31,'(2(1pe14.6))')pdt(lpdt+i),pdr(lpdr+j)                     !/*gn        501*/
         endif                                                                  !/*gn        502*/
      endif                                                                     !/*gn        503*/
  660 continue                                                                  !/*gn        504*/
      close(31)                                                                 !/*gn        505*/
  670 continue                                                                  !/*gn        506*/
!                                                                               !/*gn        507*/
  999 return                                                                    !/*gn        508*/
!                                                                               !/*gn        509*/
      end                                                                       !/*gn        510*/
