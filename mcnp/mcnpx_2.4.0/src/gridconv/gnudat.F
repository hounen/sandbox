!     $Id: gnudat.F,v 1.6 2002/03/27 18:13:51 gwm Exp $                       !/*gn          1*/
      subroutine gnudat                                                         !/*gn          2*/
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
      dimension gdx(1),gdy(1),gdz(1),gdata(1),pdx(1),pdy(1),                    !/*gn         13*/
     & pdz(1),pdata(1)                                                          !/*gn         14*/
      equivalence (das,gdx,gdy,gdz,gdata,pdx,pdy,pdz,pdata)                     !/*gn         15*/
      dimension cl(20)                                                          !/*gn         16*/
!                                                                               !/*gn         17*/
      character ha*1,hi*3,hf*30,hb*30,hj(2)*5,hv(3)*1,hl(3)*2,dt*5              !/*gn         18*/
      data hv/'X','Y','Z'/,hj/'tally','error'/,hl/'XY','YZ','ZX'/               !/*gn         19*/
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
      n3=ng3(ngd)-1                                                             !/*gn         65*/
!                                                                               !/*gn         66*/
      write(6,'("Do you wish to plot the tally or error?")')                    !/*gn         67*/
      write(6,'("  <default = tally>")')                                        !/*gn         68*/
      read(5,'(a5)')dt                                                          !/*gn         69*/
      md=0                                                                      !/*gn         70*/
      if(dt.eq.'error')md=1                                                     !/*gn         71*/
!                                                                               !/*gn         72*/
      go to (100,500)np                                                         !/*gn         73*/
!                                                                               !/*gn         74*/
!       One dinemsional plot data.                                              !/*gn         75*/
!       Get dependent variable and index of other two.                          !/*gn         76*/
  100 nv=0                                                                      !/*gn         77*/
      write(6,'("Plot data as fcn. of x, y, z, or q to exit?")')                !/*gn         78*/
      write(6,'("$")')                                                          !/*gn         79*/
      read(5,'(a3)')hi                                                          !/*gn         80*/
      if(hi(1:1).eq.'q')go to 999                                               !/*gn         81*/
      if(hi(1:1).eq.'x')nv=1                                                    !/*gn         82*/
      if(hi(1:1).eq.'y')nv=2                                                    !/*gn         83*/
      if(hi(1:1).eq.'z')nv=3                                                    !/*gn         84*/
!                                                                               !/*gn         85*/
      if(nv.eq.1) then                                                          !/*gn         86*/
  110    write(6,'("What indicies do you what for y and z?")')                  !/*gn         87*/
         write(6,'(" <default = 1,1>")')                                        !/*gn         88*/
         write(6,'("$")')                                                       !/*gn         89*/
         read(5,'(a30)')hb                                                      !/*gn         90*/
         do 120 j=1,30                                                          !/*gn         91*/
         n=index('0123456789',hb(j:j))                                          !/*gn         92*/
         if(n.ne.0) go to 130                                                   !/*gn         93*/
  120    continue                                                               !/*gn         94*/
         i2=1                                                                   !/*gn         95*/
         i3=1                                                                   !/*gn         96*/
         go to 180                                                              !/*gn         97*/
!                                                                               !/*gn         98*/
  130    kk=1                                                                   !/*gn         99*/
         do 160 i=1,2                                                           !/*gn        100*/
         kx=0                                                                   !/*gn        101*/
         do 140 j=kk,30                                                         !/*gn        102*/
         n=index('0123456789',hb(j:j))                                          !/*gn        103*/
         if(n.ne.0) then                                                        !/*gn        104*/
            kx=kx*10+n-1                                                        !/*gn        105*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*gn        106*/
            kk=j+1                                                              !/*gn        107*/
            go to 150                                                           !/*gn        108*/
         endif                                                                  !/*gn        109*/
  140    continue                                                               !/*gn        110*/
!                                                                               !/*gn        111*/
  150    if(i.eq.1) then                                                        !/*gn        112*/
            i2=kx                                                               !/*gn        113*/
         else                                                                   !/*gn        114*/
            i3=kx                                                               !/*gn        115*/
         endif                                                                  !/*gn        116*/
  160    continue                                                               !/*gn        117*/
!                                                                               !/*gn        118*/
  180    if(i2.gt.n2.or.i3.gt.n3) then                                          !/*gn        119*/
            write(6,'("One of these indices is too large!")')                   !/*gn        120*/
            go to 110                                                           !/*gn        121*/
         endif                                                                  !/*gn        122*/
      elseif(nv.eq.2) then                                                      !/*gn        123*/
  200    write(6,'("What indicies do you what for x and z?")')                  !/*gn        124*/
         write(6,'(" <default = 1,1>")')                                        !/*gn        125*/
         write(6,'("$")')                                                       !/*gn        126*/
         read(5,'(a30)')hb                                                      !/*gn        127*/
         do 220 j=1,30                                                          !/*gn        128*/
         n=index('0123456789',hb(j:j))                                          !/*gn        129*/
         if(n.ne.0) go to 230                                                   !/*gn        130*/
  220    continue                                                               !/*gn        131*/
         i1=1                                                                   !/*gn        132*/
         i3=1                                                                   !/*gn        133*/
         go to 280                                                              !/*gn        134*/
!                                                                               !/*gn        135*/
  230    kk=1                                                                   !/*gn        136*/
         do 260 i=1,2                                                           !/*gn        137*/
         kx=0                                                                   !/*gn        138*/
         do 240 j=kk,30                                                         !/*gn        139*/
         n=index('0123456789',hb(j:j))                                          !/*gn        140*/
         if(n.ne.0) then                                                        !/*gn        141*/
            kx=kx*10+n-1                                                        !/*gn        142*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*gn        143*/
            kk=j+1                                                              !/*gn        144*/
            go to 250                                                           !/*gn        145*/
         endif                                                                  !/*gn        146*/
  240    continue                                                               !/*gn        147*/
!                                                                               !/*gn        148*/
  250    if(i.eq.1) then                                                        !/*gn        149*/
            i1=kx                                                               !/*gn        150*/
         else                                                                   !/*gn        151*/
            i3=kx                                                               !/*gn        152*/
         endif                                                                  !/*gn        153*/
  260    continue                                                               !/*gn        154*/
!                                                                               !/*gn        155*/
  280    if(i1.gt.n1.or.i3.gt.n3) then                                          !/*gn        156*/
            write(6,'("One of these indices is too large!")')                   !/*gn        157*/
            go to 200                                                           !/*gn        158*/
         endif                                                                  !/*gn        159*/
      elseif(nv.eq.3) then                                                      !/*gn        160*/
  300    write(6,'("What indicies do you what for x and y?")')                  !/*gn        161*/
         write(6,'(" <default = 1,1>")')                                        !/*gn        162*/
         write(6,'("$")')                                                       !/*gn        163*/
         read(5,'(a30)')hb                                                      !/*gn        164*/
         do 320 j=1,30                                                          !/*gn        165*/
         n=index('0123456789',hb(j:j))                                          !/*gn        166*/
         if(n.ne.0) go to 330                                                   !/*gn        167*/
  320    continue                                                               !/*gn        168*/
         i1=1                                                                   !/*gn        169*/
         i2=1                                                                   !/*gn        170*/
         go to 380                                                              !/*gn        171*/
!                                                                               !/*gn        172*/
  330    kk=1                                                                   !/*gn        173*/
         do 360 i=1,2                                                           !/*gn        174*/
         kx=0                                                                   !/*gn        175*/
         do 340 j=kk,30                                                         !/*gn        176*/
         n=index('0123456789',hb(j:j))                                          !/*gn        177*/
         if(n.ne.0) then                                                        !/*gn        178*/
            kx=kx*10+n-1                                                        !/*gn        179*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*gn        180*/
            kk=j+1                                                              !/*gn        181*/
            go to 350                                                           !/*gn        182*/
         endif                                                                  !/*gn        183*/
  340    continue                                                               !/*gn        184*/
!                                                                               !/*gn        185*/
  350    if(i.eq.1) then                                                        !/*gn        186*/
            i1=kx                                                               !/*gn        187*/
         else                                                                   !/*gn        188*/
            i2=kx                                                               !/*gn        189*/
         endif                                                                  !/*gn        190*/
  360    continue                                                               !/*gn        191*/
!                                                                               !/*gn        192*/
  380    if(i1.gt.n1.or.i2.gt.n2) then                                          !/*gn        193*/
            write(6,'("One of these indices is too large!")')                   !/*gn        194*/
            go to 300                                                           !/*gn        195*/
         endif                                                                  !/*gn        196*/
      else                                                                      !/*gn        197*/
         go to 100                                                              !/*gn        198*/
      endif                                                                     !/*gn        199*/
!                                                                               !/*gn        200*/
!       nv is the number of the coordinate variable that will                   !/*gn        201*/
!       looped over.   1,2,3 - x,y,z.                                           !/*gn        202*/
!       The other two indices will be fixed.                                    !/*gn        203*/
!                                                                               !/*gn        204*/
      if(nv.eq.1) then                                                          !/*gn        205*/
         do 400 i1=1,n1                                                         !/*gn        206*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*gn        207*/
         if(md.eq.0) then                                                       !/*gn        208*/
            pdata(lpdat+i1)=gdata(lgdat+mx)                                     !/*gn        209*/
         else                                                                   !/*gn        210*/
            pdata(lpdat+i1)=gdata(lgdat+mxgc(ngd)+mx)                           !/*gn        211*/
         endif                                                                  !/*gn        212*/
  400    continue                                                               !/*gn        213*/
!                                                                               !/*gn        214*/
!       Write header and data the a formatted file for input                    !/*gn        215*/
!       into the Gnuplot utility for plotting.                                  !/*gn        216*/
!                                                                               !/*gn        217*/
!       Write information about this mdata file.                                !/*gn        218*/
!                                                                               !/*gn        219*/
         hb=hf(1:jb)//'1Da'                                                     !/*gn        220*/
         call unique(hb,6)                                                      !/*gn        221*/
         open(31,file=hb,form='formatted',status='new')                         !/*gn        222*/
         write(31,'(1h#)')                                                      !/*gn        223*/
         write(31,'(1h# ,a80)')aid                                              !/*gn        224*/
         write(31,'(1h# ,"The number of histories = ",i10)')nps                 !/*gn        225*/
         write(31,'(1h# ,"These are ",a10,'//                                   !/*gn        226*/
     &    '" data for the rectangular mesh tally # ",i5)')                      !/*gn        227*/
     &    hip(igpt(ngd)),nugd(ngd)                                              !/*gn        228*/
         write(31,'(1h# ,"  ***************** ")')                              !/*gn        229*/
         write(31,'(1h# ,"The following is ",a5," data vs. ",a1)')              !/*gn        230*/
     &    hj(md+1),hv(nv)                                                       !/*gn        231*/
         write(31,'(1h#)')                                                      !/*gn        232*/
!                                                                               !/*gn        233*/
         do 405 i=1,n1                                                          !/*gn        234*/
         write(31,'(2(1x,g13.6))')pdx(lpdx+i),pdata(lpdat+i)                    !/*gn        235*/
  405    continue                                                               !/*gn        236*/
         close(31)                                                              !/*gn        237*/
         return                                                                 !/*gn        238*/
!                                                                               !/*gn        239*/
!                                                                               !/*gn        240*/
      elseif(nv.eq.2) then                                                      !/*gn        241*/
         do 410 i2=1,n2                                                         !/*gn        242*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*gn        243*/
         if(md.eq.0) then                                                       !/*gn        244*/
            pdata(lpdat+i2)=gdata(lgdat+mx)                                     !/*gn        245*/
         else                                                                   !/*gn        246*/
            pdata(lpdat+i2)=gdata(lgdat+mxgc(ngd)+mx)                           !/*gn        247*/
         endif                                                                  !/*gn        248*/
  410    continue                                                               !/*gn        249*/
!                                                                               !/*gn        250*/
!       Write header and data the a formatted file for input                    !/*gn        251*/
!       into the Gnuplot utility for plotting.                                  !/*gn        252*/
!                                                                               !/*gn        253*/
!       Write information about this mdata file.                                !/*gn        254*/
!                                                                               !/*gn        255*/
         hb=hf(1:jb)//'1Da'                                                     !/*gn        256*/
         call unique(hb,6)                                                      !/*gn        257*/
         open(31,file=hb,form='formatted',status='new')                         !/*gn        258*/
         write(31,'(1h#)')                                                      !/*gn        259*/
         write(31,'(1h# ,a80)')aid                                              !/*gn        260*/
         write(31,'(1h# ,"The number of histories = ",i10)')nps                 !/*gn        261*/
         write(31,'(1h# ,"These are ",a10,'//                                   !/*gn        262*/
     &    '" data for the rectangular mesh tally # ",i5)')                      !/*gn        263*/
     &    hip(igpt(ngd)),nugd(ngd)                                              !/*gn        264*/
         write(31,'(1h# ,"  ***************** ")')                              !/*gn        265*/
         write(31,'(1h# ,"The following is ",a5," data vs. ",a1)')              !/*gn        266*/
     &    hj(md+1),hv(nv)                                                       !/*gn        267*/
         write(31,'(1h#)')                                                      !/*gn        268*/
!                                                                               !/*gn        269*/
         do 415 i=1,n2                                                          !/*gn        270*/
         write(31,'(2(1x,g13.6))')pdy(lpdy+i),pdata(lpdat+i)                    !/*gn        271*/
  415    continue                                                               !/*gn        272*/
         close(31)                                                              !/*gn        273*/
         return                                                                 !/*gn        274*/
!                                                                               !/*gn        275*/
!                                                                               !/*gn        276*/
      else                                                                      !/*gn        277*/
         do 420 i3=1,n3                                                         !/*gn        278*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*gn        279*/
         if(md.eq.0) then                                                       !/*gn        280*/
            pdata(lpdat+i3)=gdata(lgdat+mx)                                     !/*gn        281*/
         else                                                                   !/*gn        282*/
            pdata(lpdat+i3)=gdata(lgdat+mxgc(ngd)+mx)                           !/*gn        283*/
         endif                                                                  !/*gn        284*/
  420    continue                                                               !/*gn        285*/
!                                                                               !/*gn        286*/
!       Write header and data the a formatted file for input                    !/*gn        287*/
!       into the Gnuplot utility for plotting.                                  !/*gn        288*/
!                                                                               !/*gn        289*/
!       Write information about this mdata file.                                !/*gn        290*/
!                                                                               !/*gn        291*/
         hb=hf(1:jb)//'1Da'                                                     !/*gn        292*/
         call unique(hb,6)                                                      !/*gn        293*/
         open(31,file=hb,form='formatted',status='new')                         !/*gn        294*/
         write(31,'(1h#)')                                                      !/*gn        295*/
         write(31,'(1h# ,a80)')aid                                              !/*gn        296*/
         write(31,'(1h# ,"The number of histories = ",i10)')nps                 !/*gn        297*/
         write(31,'(1h# ,"These are ",a10,'//                                   !/*gn        298*/
     &    '" data for the rectangular mesh tally # ",i5)')                      !/*gn        299*/
     &    hip(igpt(ngd)),nugd(ngd)                                              !/*gn        300*/
         write(31,'(1h# ,"  ***************** ")')                              !/*gn        301*/
         write(31,'(1h# ,"The following is ",a5," data vs. ",a1)')              !/*gn        302*/
     &    hj(md+1),hv(nv)                                                       !/*gn        303*/
         write(31,'(1h#)')                                                      !/*gn        304*/
!                                                                               !/*gn        305*/
         do 440 i=1,n3                                                          !/*gn        306*/
         write(31,'(2(1x,g13.6))')pdz(lpdz+i),pdata(lpdat+i)                    !/*gn        307*/
  440    continue                                                               !/*gn        308*/
         close(31)                                                              !/*gn        309*/
         return                                                                 !/*gn        310*/
!                                                                               !/*gn        311*/
      endif                                                                     !/*gn        312*/
!                                                                               !/*gn        313*/
!       Two dinemsional plot data.                                              !/*gn        314*/
!       Get dependent variable and index of other one.                          !/*gn        315*/
!                                                                               !/*gn        316*/
  500 ni=0                                                                      !/*gn        317*/
      write(6,'("Plot data as fcn. of xy, yz, xz or q to exit ?")')             !/*gn        318*/
      write(6,'("$")')                                                          !/*gn        319*/
      read(5,'(a3)')hi                                                          !/*gn        320*/
      if(hi(1:1).eq.'q')go to 999                                               !/*gn        321*/
      if(hi(1:2).eq.'xy'.or.hi(1:2).eq.'yx') then                               !/*gn        322*/
         ni=1                                                                   !/*gn        323*/
  510    write(6,'("What indicies do you what for z?")')                        !/*gn        324*/
         write(6,'(" <default = 1>")')                                          !/*gn        325*/
         write(6,'("$")')                                                       !/*gn        326*/
         read(5,'(a30)')hb                                                      !/*gn        327*/
         kx=0                                                                   !/*gn        328*/
         do 520 j=1,30                                                          !/*gn        329*/
         n=index('0123456789',hb(j:j))                                          !/*gn        330*/
         if(n.ne.0) then                                                        !/*gn        331*/
            kx=kx*10+n-1                                                        !/*gn        332*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*gn        333*/
            i3=kx                                                               !/*gn        334*/
            go to 530                                                           !/*gn        335*/
         endif                                                                  !/*gn        336*/
  520    continue                                                               !/*gn        337*/
         i3=1                                                                   !/*gn        338*/
!                                                                               !/*gn        339*/
  530    if(i3.gt.n3) then                                                      !/*gn        340*/
            write(6,'("This index is too large!")')                             !/*gn        341*/
            go to 510                                                           !/*gn        342*/
         endif                                                                  !/*gn        343*/
      elseif(hi(1:2).eq.'yz'.or.hi(1:2).eq.'zy') then                           !/*gn        344*/
         ni=2                                                                   !/*gn        345*/
  540    write(6,'("What indicies do you what for x?")')                        !/*gn        346*/
         write(6,'(" <default = 1>")')                                          !/*gn        347*/
         write(6,'("$")')                                                       !/*gn        348*/
         read(5,'(a30)')hb                                                      !/*gn        349*/
         kx=0                                                                   !/*gn        350*/
         do 550 j=1,30                                                          !/*gn        351*/
         n=index('0123456789',hb(j:j))                                          !/*gn        352*/
         if(n.ne.0) then                                                        !/*gn        353*/
            kx=kx*10+n-1                                                        !/*gn        354*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*gn        355*/
            i1=kx                                                               !/*gn        356*/
            go to 560                                                           !/*gn        357*/
         endif                                                                  !/*gn        358*/
  550    continue                                                               !/*gn        359*/
         i1=1                                                                   !/*gn        360*/
!                                                                               !/*gn        361*/
  560    if(i1.gt.n1) then                                                      !/*gn        362*/
            write(6,'("This index is too large!")')                             !/*gn        363*/
            go to 540                                                           !/*gn        364*/
         endif                                                                  !/*gn        365*/
      elseif(hi(1:2).eq.'xz'.or.hi(1:2).eq.'zx') then                           !/*gn        366*/
         ni=3                                                                   !/*gn        367*/
  570    write(6,'("What indicies do you what for y?")')                        !/*gn        368*/
         write(6,'(" <default = 1>")')                                          !/*gn        369*/
         write(6,'("$")')                                                       !/*gn        370*/
         read(5,'(a30)')hb                                                      !/*gn        371*/
         kx=0                                                                   !/*gn        372*/
         do 580 j=1,30                                                          !/*gn        373*/
         n=index('0123456789',hb(j:j))                                          !/*gn        374*/
         if(n.ne.0) then                                                        !/*gn        375*/
            kx=kx*10+n-1                                                        !/*gn        376*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*gn        377*/
            i2=kx                                                               !/*gn        378*/
            go to 600                                                           !/*gn        379*/
         endif                                                                  !/*gn        380*/
  580    continue                                                               !/*gn        381*/
         i2=1                                                                   !/*gn        382*/
!                                                                               !/*gn        383*/
  600    if(i2.gt.n2) then                                                      !/*gn        384*/
            write(6,'("This index is too large!")')                             !/*gn        385*/
            go to 570                                                           !/*gn        386*/
         endif                                                                  !/*gn        387*/
      else                                                                      !/*gn        388*/
         go to 500                                                              !/*gn        389*/
      endif                                                                     !/*gn        390*/
!                                                                               !/*gn        391*/
!      ni is an indicator as to which two coordinate varibles will be           !/*gn        392*/
!      looped over.    1,2,3  --  xy,yz,xz                                      !/*gn        393*/
!      The remaining index will be fixed.                                       !/*gn        394*/
!                                                                               !/*gn        395*/
!      Find min. and max. values and ask for number of colors                   !/*gn        396*/
!      to be used in a 2D contour plot.                                         !/*gn        397*/
!                                                                               !/*gn        398*/
      pn=1.e30                                                                  !/*gn        399*/
      px=0.                                                                     !/*gn        400*/
      if(ni.eq.1) then                                                          !/*gn        401*/
         do 610 i2=1,n2                                                         !/*gn        402*/
         do 610 i1=1,n1                                                         !/*gn        403*/
         mx=mxgc(ngd)*md+(i3-1)*n1*n2+(i2-1)*n1+i1                              !/*gn        404*/
         mz=(i2-1)*n1+i1                                                        !/*gn        405*/
         pdata(lpdat+mz)=gdata(lgdat+mx)                                        !/*gn        406*/
         if(pdata(lpdat+mz).lt.pn)pn=pdata(lpdat+mz)                            !/*gn        407*/
         if(pdata(lpdat+mz).gt.px)px=pdata(lpdat+mz)                            !/*gn        408*/
  610    continue                                                               !/*gn        409*/
         na=n1                                                                  !/*gn        410*/
         nb=n2                                                                  !/*gn        411*/
!                                                                               !/*gn        412*/
      elseif(ni.eq.2) then                                                      !/*gn        413*/
         do 620 i3=1,n3                                                         !/*gn        414*/
         do 620 i2=1,n2                                                         !/*gn        415*/
         mx=mxgc(ngd)*md+(i3-1)*n1*n2+(i2-1)*n1+i1                              !/*gn        416*/
         mz=(i3-1)*n2+i2                                                        !/*gn        417*/
         pdata(lpdat+mz)=gdata(lgdat+mx)                                        !/*gn        418*/
         if(pdata(lpdat+mz).lt.pn)pn=pdata(lpdat+mz)                            !/*gn        419*/
         if(pdata(lpdat+mz).gt.px)px=pdata(lpdat+mz)                            !/*gn        420*/
  620    continue                                                               !/*gn        421*/
         na=n2                                                                  !/*gn        422*/
         nb=n3                                                                  !/*gn        423*/
!                                                                               !/*gn        424*/
      else                                                                      !/*gn        425*/
         do 630 i1=1,n1                                                         !/*gn        426*/
         do 630 i3=1,n3                                                         !/*gn        427*/
         mx=mxgc(ngd)*md+(i3-1)*n1*n2+(i2-1)*n1+i1                              !/*gn        428*/
         mz=(i1-1)*n3+i3                                                        !/*gn        429*/
         pdata(lpdat+mz)=gdata(lgdat+mx)                                        !/*gn        430*/
         if(pdata(lpdat+mz).lt.pn)pn=pdata(lpdat+mz)                            !/*gn        431*/
         if(pdata(lpdat+mz).gt.px)px=pdata(lpdat+mz)                            !/*gn        432*/
  630    continue                                                               !/*gn        433*/
         na=n3                                                                  !/*gn        434*/
         nb=n1                                                                  !/*gn        435*/
!                                                                               !/*gn        436*/
      endif                                                                     !/*gn        437*/
!                                                                               !/*gn        438*/
      write(6,'("How many contours do you want to plot?")')                     !/*gn        439*/
      write(6,'("  <no default!  Max=20> ")')                                   !/*gn        440*/
      read(5,'(i2)') nc                                                         !/*gn        441*/
      write(6,'("Data min and max are ",2(1pe13.6))')pn,px                      !/*gn        442*/
      write(6,'("Do you wish to reset min and max values? (y/n)")')             !/*gn        443*/
      write(6,'("  <default = n> ")')                                           !/*gn        444*/
      read(5,'(1a)')ha                                                          !/*gn        445*/
      if(ha.eq.'n'.or.ha.eq.' ') go to 640                                      !/*gn        446*/
      write(6,'("Enter min and max in 2(1pe13.6) format.")')                    !/*gn        447*/
      write(6,'("  <No default at this point!>")')                              !/*gn        448*/
      read(5,'(2(1pe14.6))')pn,px                                               !/*gn        449*/
!                                                                               !/*gn        450*/
!      Set up contour bin limits.                                               !/*gn        451*/
!                                                                               !/*gn        452*/
  640 dx=(px-pn)/nc                                                             !/*gn        453*/
      cl(1)=pn                                                                  !/*gn        454*/
      do 650 i=2,nc+1                                                           !/*gn        455*/
      cl(i)=cl(i-1)+dx                                                          !/*gn        456*/
  650 continue                                                                  !/*gn        457*/
!                                                                               !/*gn        458*/
      write(6,'("The color bins are for data limits = ")')                      !/*gn        459*/
      write(6,'(5(1pe13.5))')(cl(i),i=1,nc+1)                                   !/*gn        460*/
!                                                                               !/*gn        461*/
      do 670 l=2,nc+1                                                           !/*gn        462*/
      if(l-1.lt.10) then                                                        !/*gn        463*/
         hb=hf(1:jb)//'0'                                                       !/*gn        464*/
         write(hb(jb+2:jb+2),'(i1)')l-1                                         !/*gn        465*/
         hb(jb+3:jb+3)='a'                                                      !/*gn        466*/
      else                                                                      !/*gn        467*/
         hb=hf(1:jb)                                                            !/*gn        468*/
         write(hb(jb+1:nb+2),'(i2)')l-1                                         !/*gn        469*/
         hb(jb+3:jb+3)='a'                                                      !/*gn        470*/
      endif                                                                     !/*gn        471*/
      call unique(hb,6)                                                         !/*gn        472*/
      open(31,file=hb,form='formatted',status='new')                            !/*gn        473*/
!                                                                               !/*gn        474*/
!      Write some header information here.                                      !/*gn        475*/
!                                                                               !/*gn        476*/
      write(31,'(1h#)')                                                         !/*gn        477*/
      write(31,'(1h# ,a80)')aid                                                 !/*gn        478*/
      write(31,'(1h# ,"The number of histories = ",i10)')nps                    !/*gn        479*/
      write(31,'(1h# ,"These are ",a10,'//                                      !/*gn        480*/
     & '" data for the rectangular mesh"," tally # ",i5)')                      !/*gn        481*/
     & hip(igpt(ngd)),nugd(ngd)                                                 !/*gn        482*/
      write(31,'(1h# ,"  ***************** ")')                                 !/*gn        483*/
      write(31,'(1h# ,"The following are ",a2," values for ",a5)')              !/*gn        484*/
     & hl(ni),hj(md+1)                                                          !/*gn        485*/
      write(31,'(1h# ,"values between ",1pe13.6," and ",1pe13.6)')              !/*gn        486*/
     & cl(l-1),cl(l)                                                            !/*gn        487*/
      write(31,'(1h#)')                                                         !/*gn        488*/
!                                                                               !/*gn        489*/
      do 660 j=1,nb                                                             !/*gn        490*/
      do 660 i=1,na                                                             !/*gn        491*/
      mz=(j-1)*na+i                                                             !/*gn        492*/
      if(pdata(lpdat+mz).gt.cl(l-1).and.pdata(lpdat+mz).le.cl(l)) then          !/*gn        493*/
         if(ni.eq.1) then                                                       !/*gn        494*/
            write(31,'(2(1pe14.6))')pdx(lpdx+i),pdy(lpdy+j)                     !/*gn        495*/
         elseif(ni.eq.2) then                                                   !/*gn        496*/
            write(31,'(2(1pe14.6))')pdy(lpdy+i),pdz(lpdz+j)                     !/*gn        497*/
         else                                                                   !/*gn        498*/
            write(31,'(2(1pe14.6))')pdz(lpdz+i),pdx(lpdx+j)                     !/*gn        499*/
         endif                                                                  !/*gn        500*/
      endif                                                                     !/*gn        501*/
  660 continue                                                                  !/*gn        502*/
      close(31)                                                                 !/*gn        503*/
  670 continue                                                                  !/*gn        504*/
!                                                                               !/*gn        505*/
  999 return                                                                    !/*gn        506*/
!                                                                               !/*gn        507*/
      end                                                                       !/*gn        508*/
