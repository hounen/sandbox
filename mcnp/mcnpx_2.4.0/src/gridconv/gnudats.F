!     $Id: gnudats.F,v 1.6 2002/03/27 18:13:51 gwm Exp $                      !/*gn          1*/
      subroutine gnudats                                                        !/*gn          2*/
!                                                                               !/*gn          3*/
!       This subroutine is designed to convert input data from a                !/*gn          4*/
!       mesh tally set to a data set compatible with the input                  !/*gn          5*/
!       requirements of the Gnuplot graphics package.                           !/*gn          6*/
!                                                                               !/*gn          7*/
!       Written by E. C. Snow  12/11/2000.   (revised: 11/14/2001)              !/*gn          8*/
!                                                                               !/*gn          9*/
#include <gridconv/gcdouble.h>                                                  /*!gn         10*/
#include <gridconv/gccom5.h>                                                    /*!gn         11*/
!                                                                               !/*gn         12*/
      dimension gdr(1),gdp(1),gda(1),gdata(1),pdr(1),pdp(1),                    !/*gn         13*/
     & pda(1),pdata(1)                                                          !/*gn         14*/
      equivalence (das,gdr,gdp,gda,gdata,pdr,pdp,pda,pdata)                     !/*gn         15*/
      dimension cl(20)                                                          !/*gn         16*/
!                                                                               !/*gn         17*/
      character ha*1,hi*3,hf*30,hb*30,hj(2)*5,hv(3)*1,hl(3)*2                   !/*gn         18*/
      data hv/'R','P','A'/,hj/'tally','error'/,hl/'RP','PA','AR'/               !/*gn         19*/
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
      n2=ng2(ngd)                                                               !/*gn         64*/
      n3=ng3(ngd)                                                               !/*gn         65*/
!                                                                               !/*gn         66*/
      write(6,'("Do you wish to plot the tally or error?")')                    !/*gn         67*/
      write(6,'("  <default = tally>")')                                        !/*gn         68*/
      read(5,'(a30)')hb                                                         !/*gn         69*/
      md=0                                                                      !/*gn         70*/
      if(hb(1:5).eq.'error')md=1                                                !/*gn         71*/
!                                                                               !/*gn         72*/
      go to (100,500)np                                                         !/*gn         73*/
!                                                                               !/*gn         74*/
!       One dinemsional plot data.                                              !/*gn         75*/
!       Get dependent variable and index of other two.                          !/*gn         76*/
  100 nv=0                                                                      !/*gn         77*/
      write(6,'("Plot data as fcn. of r, p, a, or q to exit?")')                !/*gn         78*/
      write(6,'("r = radius, p = polar, and a = azimuthial.")')                 !/*gn         79*/
      write(6,'("$")')                                                          !/*gn         80*/
      read(5,'(a3)')hi                                                          !/*gn         81*/
      if(hi(1:1).eq.'q')go to 999                                               !/*gn         82*/
      if(hi(1:1).eq.'r')nv=1                                                    !/*gn         83*/
      if(hi(1:1).eq.'p')nv=2                                                    !/*gn         84*/
      if(hi(1:1).eq.'a')nv=3                                                    !/*gn         85*/
!                                                                               !/*gn         86*/
      if(nv.eq.1) then                                                          !/*gn         87*/
  110    write(6,'("What indicies do you what for p and a?")')                  !/*gn         88*/
         write(6,'(" <default = 1,1>")')                                        !/*gn         89*/
         write(6,'("$")')                                                       !/*gn         90*/
         read(5,'(a30)')hb                                                      !/*gn         91*/
         do 120 j=1,30                                                          !/*gn         92*/
         n=index('0123456789',hb(j:j))                                          !/*gn         93*/
         if(n.ne.0) go to 130                                                   !/*gn         94*/
  120    continue                                                               !/*gn         95*/
         i2=1                                                                   !/*gn         96*/
         i3=1                                                                   !/*gn         97*/
         go to 180                                                              !/*gn         98*/
!                                                                               !/*gn         99*/
  130    kk=1                                                                   !/*gn        100*/
         do 160 i=1,2                                                           !/*gn        101*/
         kx=0                                                                   !/*gn        102*/
         do 140 j=kk,30                                                         !/*gn        103*/
         n=index('0123456789',hb(j:j))                                          !/*gn        104*/
         if(n.ne.0) then                                                        !/*gn        105*/
            kx=kx*10+n-1                                                        !/*gn        106*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*gn        107*/
            kk=j+1                                                              !/*gn        108*/
            go to 150                                                           !/*gn        109*/
         endif                                                                  !/*gn        110*/
  140    continue                                                               !/*gn        111*/
!                                                                               !/*gn        112*/
  150    if(i.eq.1) then                                                        !/*gn        113*/
            i2=kx                                                               !/*gn        114*/
         else                                                                   !/*gn        115*/
            i3=kx                                                               !/*gn        116*/
         endif                                                                  !/*gn        117*/
  160    continue                                                               !/*gn        118*/
!                                                                               !/*gn        119*/
  180    if(i2.gt.n2.or.i3.gt.n3) then                                          !/*gn        120*/
            write(6,'("One of these indices is too large!")')                   !/*gn        121*/
            go to 110                                                           !/*gn        122*/
         endif                                                                  !/*gn        123*/
      elseif(nv.eq.2) then                                                      !/*gn        124*/
  200    write(6,'("What indicies do you what for r and a?")')                  !/*gn        125*/
         write(6,'(" <default = 1,1>")')                                        !/*gn        126*/
         write(6,'("$")')                                                       !/*gn        127*/
         read(5,'(a30)')hb                                                      !/*gn        128*/
         do 220 j=1,30                                                          !/*gn        129*/
         n=index('0123456789',hb(j:j))                                          !/*gn        130*/
         if(n.ne.0) go to 230                                                   !/*gn        131*/
  220    continue                                                               !/*gn        132*/
         i1=1                                                                   !/*gn        133*/
         i3=1                                                                   !/*gn        134*/
         go to 280                                                              !/*gn        135*/
!                                                                               !/*gn        136*/
  230    kk=1                                                                   !/*gn        137*/
         do 260 i=1,2                                                           !/*gn        138*/
         kx=0                                                                   !/*gn        139*/
         do 240 j=kk,30                                                         !/*gn        140*/
         n=index('0123456789',hb(j:j))                                          !/*gn        141*/
         if(n.ne.0) then                                                        !/*gn        142*/
            kx=kx*10+n-1                                                        !/*gn        143*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*gn        144*/
            kk=j+1                                                              !/*gn        145*/
            go to 250                                                           !/*gn        146*/
         endif                                                                  !/*gn        147*/
  240    continue                                                               !/*gn        148*/
!                                                                               !/*gn        149*/
  250    if(i.eq.1) then                                                        !/*gn        150*/
            i1=kx                                                               !/*gn        151*/
         else                                                                   !/*gn        152*/
            i3=kx                                                               !/*gn        153*/
         endif                                                                  !/*gn        154*/
  260    continue                                                               !/*gn        155*/
!                                                                               !/*gn        156*/
  280    if(i1.gt.n1.or.i3.gt.n3) then                                          !/*gn        157*/
            write(6,'("One of these indices is too large!")')                   !/*gn        158*/
            go to 200                                                           !/*gn        159*/
         endif                                                                  !/*gn        160*/
      elseif(nv.eq.3) then                                                      !/*gn        161*/
  300    write(6,'("What indicies do you what for r and p?")')                  !/*gn        162*/
         write(6,'(" <default = 1,1>")')                                        !/*gn        163*/
         write(6,'("$")')                                                       !/*gn        164*/
         read(5,'(a30)')hb                                                      !/*gn        165*/
         do 320 j=1,30                                                          !/*gn        166*/
         n=index('0123456789',hb(j:j))                                          !/*gn        167*/
         if(n.ne.0) go to 330                                                   !/*gn        168*/
  320    continue                                                               !/*gn        169*/
         i1=1                                                                   !/*gn        170*/
         i2=1                                                                   !/*gn        171*/
         go to 380                                                              !/*gn        172*/
!                                                                               !/*gn        173*/
  330    kk=1                                                                   !/*gn        174*/
         do 360 i=1,2                                                           !/*gn        175*/
         kx=0                                                                   !/*gn        176*/
         do 340 j=kk,30                                                         !/*gn        177*/
         n=index('0123456789',hb(j:j))                                          !/*gn        178*/
         if(n.ne.0) then                                                        !/*gn        179*/
            kx=kx*10+n-1                                                        !/*gn        180*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*gn        181*/
            kk=j+1                                                              !/*gn        182*/
            go to 350                                                           !/*gn        183*/
         endif                                                                  !/*gn        184*/
  340    continue                                                               !/*gn        185*/
!                                                                               !/*gn        186*/
  350    if(i.eq.1) then                                                        !/*gn        187*/
            i1=kx                                                               !/*gn        188*/
         else                                                                   !/*gn        189*/
            i2=kx                                                               !/*gn        190*/
         endif                                                                  !/*gn        191*/
  360    continue                                                               !/*gn        192*/
!                                                                               !/*gn        193*/
  380    if(i1.gt.n1.or.i2.gt.n2) then                                          !/*gn        194*/
            write(6,'("One of these indices is too large!")')                   !/*gn        195*/
            go to 300                                                           !/*gn        196*/
         endif                                                                  !/*gn        197*/
      else                                                                      !/*gn        198*/
         go to 100                                                              !/*gn        199*/
      endif                                                                     !/*gn        200*/
!                                                                               !/*gn        201*/
!       nv is the number of the coordinate variable that will                   !/*gn        202*/
!       looped over.   1,2,3 - r,p,a.                                           !/*gn        203*/
!       The other two indices will be fixed.                                    !/*gn        204*/
!                                                                               !/*gn        205*/
      if(nv.eq.1) then                                                          !/*gn        206*/
         do 400 i1=1,n1                                                         !/*gn        207*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*gn        208*/
         if(md.eq.0) then                                                       !/*gn        209*/
            pdata(lpdat+i1)=gdata(lgdat+mx)                                     !/*gn        210*/
         else                                                                   !/*gn        211*/
            pdata(lpdat+i1)=gdata(lgdat+mxgc(ngd)+mx)                           !/*gn        212*/
         endif                                                                  !/*gn        213*/
  400    continue                                                               !/*gn        214*/
!                                                                               !/*gn        215*/
!       Write header and data the a formatted file for input                    !/*gn        216*/
!       into the Gnuplot utility for plotting.                                  !/*gn        217*/
!                                                                               !/*gn        218*/
!       Write information about this mdata file.                                !/*gn        219*/
!                                                                               !/*gn        220*/
         hb=hf(1:jb)//'1Da'                                                     !/*gn        221*/
         call unique(hb,6)                                                      !/*gn        222*/
         open(31,file=hb,form='formatted',status='new')                         !/*gn        223*/
         write(31,'(1h#)')                                                      !/*gn        224*/
         write(31,'(1h# ,a80)')aid                                              !/*gn        225*/
         write(31,'(1h# ,"The number of histories = ",i10)')nps                 !/*gn        226*/
         write(31,'(1h# ,"These are ",a10," data for the spherical",            !/*gn        227*/
     &   '//'" mesh tally # ",i5)')hip(igpt(ngd)),nugd(ngd)                     !/*gn        228*/
         write(31,'(1h# ,"  ***************** ")')                              !/*gn        229*/
         write(31,'(1h# ,"The following is ",a5," data vs. ",a1)')              !/*gn        230*/
     &    hj(md+1),hv(nv)                                                       !/*gn        231*/
         write(31,'(1h#)')                                                      !/*gn        232*/
!                                                                               !/*gn        233*/
         do 405 i=1,n1                                                          !/*gn        234*/
         write(31,'(2(1x,g13.6))')pdr(lpdr+i),pdata(lpdat+i)                    !/*gn        235*/
  405    continue                                                               !/*gn        236*/
         close(31)                                                              !/*gn        237*/
         return                                                                 !/*gn        238*/
!                                                                               !/*gn        239*/
      elseif(nv.eq.2) then                                                      !/*gn        240*/
         do 410 i2=1,n2                                                         !/*gn        241*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*gn        242*/
         if(md.eq.0) then                                                       !/*gn        243*/
            pdata(lpdat+i2)=gdata(lgdat+mx)                                     !/*gn        244*/
         else                                                                   !/*gn        245*/
            pdata(lpdat+i2)=gdata(lgdat+mxgc(ngd)+mx)                           !/*gn        246*/
         endif                                                                  !/*gn        247*/
  410    continue                                                               !/*gn        248*/
!                                                                               !/*gn        249*/
!       Write header and data the a formatted file for input                    !/*gn        250*/
!       into the Gnuplot utility for plotting.                                  !/*gn        251*/
!                                                                               !/*gn        252*/
!       Write information about this mdata file.                                !/*gn        253*/
!                                                                               !/*gn        254*/
         hb=hf(1:jb)//'1Da'                                                     !/*gn        255*/
         call unique(hb,6)                                                      !/*gn        256*/
         open(31,file=hb,form='formatted',status='new')                         !/*gn        257*/
         write(31,'(1h#)')                                                      !/*gn        258*/
         write(31,'(1h# ,a80)')aid                                              !/*gn        259*/
         write(31,'(1h# ,"The number of histories = ",i10)')nps                 !/*gn        260*/
         write(31,'(1h# ,"These are ",a10," data for the spherical",            !/*gn        261*/
     &   '//'" mesh tally # ",i5)')hip(igpt(ngd)),nugd(ngd)                     !/*gn        262*/
         write(31,'(1h# ,"  ***************** ")')                              !/*gn        263*/
         write(31,'(1h# ,"The following is ",a5," data vs. ",a1)')              !/*gn        264*/
     &    hj(md+1),hv(nv)                                                       !/*gn        265*/
         write(31,'(1h#)')                                                      !/*gn        266*/
!                                                                               !/*gn        267*/
         do 415 i=1,n2                                                          !/*gn        268*/
         write(31,'(2(1x,g13.6))')pdp(lpdp+i),pdata(lpdat+i)                    !/*gn        269*/
  415    continue                                                               !/*gn        270*/
         close(31)                                                              !/*gn        271*/
         return                                                                 !/*gn        272*/
!                                                                               !/*gn        273*/
      else                                                                      !/*gn        274*/
         do 420 i3=1,n3                                                         !/*gn        275*/
         mx=(i3-1)*n1*n2+(i2-1)*n1+i1                                           !/*gn        276*/
         if(md.eq.0) then                                                       !/*gn        277*/
            pdata(lpdat+i3)=gdata(lgdat+mx)                                     !/*gn        278*/
         else                                                                   !/*gn        279*/
            pdata(lpdat+i3)=gdata(lgdat+mxgc(ngd)+mx)                           !/*gn        280*/
         endif                                                                  !/*gn        281*/
  420    continue                                                               !/*gn        282*/
!                                                                               !/*gn        283*/
!       Write header and data the a formatted file for input                    !/*gn        284*/
!       into the Gnuplot utility for plotting.                                  !/*gn        285*/
!                                                                               !/*gn        286*/
!       Write information about this mdata file.                                !/*gn        287*/
!                                                                               !/*gn        288*/
         hb=hf(1:jb)//'1Da'                                                     !/*gn        289*/
         call unique(hb,6)                                                      !/*gn        290*/
         open(31,file=hb,form='formatted',status='new')                         !/*gn        291*/
         write(31,'(1h#)')                                                      !/*gn        292*/
         write(31,'(1h# ,a80)')aid                                              !/*gn        293*/
         write(31,'(1h# ,"The number of histories = ",i10)')nps                 !/*gn        294*/
         write(31,'(1h# ,"These are ",a10," data for the spherical",            !/*gn        295*/
     &   '//'" mesh tally # ",i5)')hip(igpt(ngd)),nugd(ngd)                     !/*gn        296*/
         write(31,'(1h# ,"  ***************** ")')                              !/*gn        297*/
         write(31,'(1h# ,"The following is ",a5," data vs. ",a1)')              !/*gn        298*/
     &    hj(md+1),hv(nv)                                                       !/*gn        299*/
         write(31,'(1h#)')                                                      !/*gn        300*/
!                                                                               !/*gn        301*/
         do 440 i=1,n3                                                          !/*gn        302*/
         write(31,'(2(1x,g13.6))')pda(lpda+i),pdata(lpdat+i)                    !/*gn        303*/
  440    continue                                                               !/*gn        304*/
         close(31)                                                              !/*gn        305*/
         return                                                                 !/*gn        306*/
!                                                                               !/*gn        307*/
      endif                                                                     !/*gn        308*/
!                                                                               !/*gn        309*/
!       Two dinemsional plot data.                                              !/*gn        310*/
!       Get dependent variable and index of other one.                          !/*gn        311*/
!                                                                               !/*gn        312*/
  500 ni=0                                                                      !/*gn        313*/
      write(6,'("Plot data as fcn. of rp, pa, ar or q to exit ?")')             !/*gn        314*/
      write(6,'("r = radius, p = polar, and a = azimuthial.")')                 !/*gn        315*/
      write(6,'("$")')                                                          !/*gn        316*/
      read(5,'(a3)')hi                                                          !/*gn        317*/
      if(hi(1:1).eq.'q')go to 999                                               !/*gn        318*/
      if(hi(1:2).eq.'rp'.or.hi(1:2).eq.'pr') then                               !/*gn        319*/
         ni=1                                                                   !/*gn        320*/
  510    write(6,'("What indicies do you what for a?")')                        !/*gn        321*/
         write(6,'(" <default = 1>")')                                          !/*gn        322*/
         write(6,'("$")')                                                       !/*gn        323*/
         read(5,'(a30)')hb                                                      !/*gn        324*/
         kx=0                                                                   !/*gn        325*/
         do 520 j=1,30                                                          !/*gn        326*/
         n=index('0123456789',hb(j:j))                                          !/*gn        327*/
         if(n.ne.0) then                                                        !/*gn        328*/
            kx=kx*10+n-1                                                        !/*gn        329*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*gn        330*/
            i3=kx                                                               !/*gn        331*/
            go to 530                                                           !/*gn        332*/
         endif                                                                  !/*gn        333*/
  520    continue                                                               !/*gn        334*/
         i3=1                                                                   !/*gn        335*/
!                                                                               !/*gn        336*/
  530    if(i3.gt.n3) then                                                      !/*gn        337*/
            write(6,'("This index is too large!")')                             !/*gn        338*/
            go to 510                                                           !/*gn        339*/
         endif                                                                  !/*gn        340*/
      elseif(hi(1:2).eq.'pa'.or.hi(1:2).eq.'ap') then                           !/*gn        341*/
         ni=2                                                                   !/*gn        342*/
  540    write(6,'("What indicies do you what for r?")')                        !/*gn        343*/
         write(6,'(" <default = 1>")')                                          !/*gn        344*/
         write(6,'("$")')                                                       !/*gn        345*/
         read(5,'(a30)')hb                                                      !/*gn        346*/
         kx=0                                                                   !/*gn        347*/
         do 550 j=1,30                                                          !/*gn        348*/
         n=index('0123456789',hb(j:j))                                          !/*gn        349*/
         if(n.ne.0) then                                                        !/*gn        350*/
            kx=kx*10+n-1                                                        !/*gn        351*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*gn        352*/
            i1=kx                                                               !/*gn        353*/
            go to 560                                                           !/*gn        354*/
         endif                                                                  !/*gn        355*/
  550    continue                                                               !/*gn        356*/
         i1=1                                                                   !/*gn        357*/
!                                                                               !/*gn        358*/
  560    if(i1.gt.n1) then                                                      !/*gn        359*/
            write(6,'("This index is too large!")')                             !/*gn        360*/
            go to 540                                                           !/*gn        361*/
         endif                                                                  !/*gn        362*/
      elseif(hi(1:2).eq.'ar'.or.hi(1:2).eq.'ra') then                           !/*gn        363*/
         ni=3                                                                   !/*gn        364*/
  570    write(6,'("What indicies do you what for p?")')                        !/*gn        365*/
         write(6,'(" <default = 1>")')                                          !/*gn        366*/
         write(6,'("$")')                                                       !/*gn        367*/
         read(5,'(a30)')hb                                                      !/*gn        368*/
         kx=0                                                                   !/*gn        369*/
         do 580 j=1,30                                                          !/*gn        370*/
         n=index('0123456789',hb(j:j))                                          !/*gn        371*/
         if(n.ne.0) then                                                        !/*gn        372*/
            kx=kx*10+n-1                                                        !/*gn        373*/
         elseif(n.eq.0.and.kx.gt.0) then                                        !/*gn        374*/
            i2=kx                                                               !/*gn        375*/
            go to 600                                                           !/*gn        376*/
         endif                                                                  !/*gn        377*/
  580    continue                                                               !/*gn        378*/
         i2=1                                                                   !/*gn        379*/
!                                                                               !/*gn        380*/
  600    if(i2.gt.n2) then                                                      !/*gn        381*/
            write(6,'("This index is too large!")')                             !/*gn        382*/
            go to 570                                                           !/*gn        383*/
         endif                                                                  !/*gn        384*/
      else                                                                      !/*gn        385*/
         go to 500                                                              !/*gn        386*/
      endif                                                                     !/*gn        387*/
!                                                                               !/*gn        388*/
!      ni is an indicator as to which two coordinate varibles will be           !/*gn        389*/
!      looped over.    1,2,3  --  rp,pa,ar                                      !/*gn        390*/
!      The remaining index will be fixed.                                       !/*gn        391*/
!                                                                               !/*gn        392*/
!      Find min. and max. values and ask for number of colors                   !/*gn        393*/
!      to be used in a 2D contour plot.                                         !/*gn        394*/
!                                                                               !/*gn        395*/
      pn=1.e30                                                                  !/*gn        396*/
      px=0.                                                                     !/*gn        397*/
      if(ni.eq.1) then                                                          !/*gn        398*/
         do 610 i2=1,n2                                                         !/*gn        399*/
         do 610 i1=1,n1                                                         !/*gn        400*/
         mx=mxgc(ngd)*md+(i3-1)*n1*n2+(i2-1)*n1+i1                              !/*gn        401*/
         mz=(i2-1)*n1+i1                                                        !/*gn        402*/
         pdata(lpdat+mz)=gdata(lgdat+mx)                                        !/*gn        403*/
         if(pdata(lpdat+mz).lt.pn)pn=pdata(lpdat+mz)                            !/*gn        404*/
         if(pdata(lpdat+mz).gt.px)px=pdata(lpdat+mz)                            !/*gn        405*/
  610    continue                                                               !/*gn        406*/
         na=n1                                                                  !/*gn        407*/
         nb=n2                                                                  !/*gn        408*/
!                                                                               !/*gn        409*/
      elseif(ni.eq.2) then                                                      !/*gn        410*/
         do 620 i3=1,n3                                                         !/*gn        411*/
         do 620 i2=1,n2                                                         !/*gn        412*/
         mx=mxgc(ngd)*md+(i3-1)*n1*n2+(i2-1)*n1+i1                              !/*gn        413*/
         mz=(i3-1)*n2+i2                                                        !/*gn        414*/
         pdata(lpdat+mz)=gdata(lgdat+mx)                                        !/*gn        415*/
         if(pdata(lpdat+mz).lt.pn)pn=pdata(lpdat+mz)                            !/*gn        416*/
         if(pdata(lpdat+mz).gt.px)px=pdata(lpdat+mz)                            !/*gn        417*/
  620    continue                                                               !/*gn        418*/
         na=n2                                                                  !/*gn        419*/
         nb=n3                                                                  !/*gn        420*/
!                                                                               !/*gn        421*/
      else                                                                      !/*gn        422*/
         do 630 i1=1,n1                                                         !/*gn        423*/
         do 630 i3=1,n3                                                         !/*gn        424*/
         mx=mxgc(ngd)*md+(i3-1)*n1*n2+(i2-1)*n1+i1                              !/*gn        425*/
         mz=(i1-1)*n3+i3                                                        !/*gn        426*/
         pdata(lpdat+mz)=gdata(lgdat+mx)                                        !/*gn        427*/
         if(pdata(lpdat+mz).lt.pn)pn=pdata(lpdat+mz)                            !/*gn        428*/
         if(pdata(lpdat+mz).gt.px)px=pdata(lpdat+mz)                            !/*gn        429*/
  630    continue                                                               !/*gn        430*/
         na=n3                                                                  !/*gn        431*/
         nb=n1                                                                  !/*gn        432*/
!                                                                               !/*gn        433*/
      endif                                                                     !/*gn        434*/
!                                                                               !/*gn        435*/
      write(6,'("How many contours do you want to plot?")')                     !/*gn        436*/
      write(6,'("  <no default!  Max=20> ")')                                   !/*gn        437*/
      read(5,'(i2)') nc                                                         !/*gn        438*/
      write(6,'("Data min and max are ",2(1pe13.6))')pn,px                      !/*gn        439*/
      write(6,'("Do you wish to reset min and max values? (y/n)")')             !/*gn        440*/
      write(6,'("  <default = n> ")')                                           !/*gn        441*/
      read(5,'(1a)')ha                                                          !/*gn        442*/
      if(ha.eq.'n'.or.ha.eq.' ') go to 640                                      !/*gn        443*/
      write(6,'("Enter min and max in 2(1pe13.6) format.")')                    !/*gn        444*/
      write(6,'("  <No default at this point!>")')                              !/*gn        445*/
      read(5,'(2(1pe14.6))')pn,px                                               !/*gn        446*/
!                                                                               !/*gn        447*/
!      Set up contour bin limits.                                               !/*gn        448*/
!                                                                               !/*gn        449*/
  640 dx=(px-pn)/nc                                                             !/*gn        450*/
      cl(1)=pn                                                                  !/*gn        451*/
      do 650 i=2,nc+1                                                           !/*gn        452*/
      cl(i)=cl(i-1)+dx                                                          !/*gn        453*/
  650 continue                                                                  !/*gn        454*/
!                                                                               !/*gn        455*/
      write(6,'("The color bins are for data limits = ")')                      !/*gn        456*/
      write(6,'(5(1pe13.5))')(cl(i),i=1,nc+1)                                   !/*gn        457*/
!                                                                               !/*gn        458*/
      do 670 l=2,nc+1                                                           !/*gn        459*/
      if(l-1.lt.10) then                                                        !/*gn        460*/
         hb=hf(1:jb)//'0'                                                       !/*gn        461*/
         write(hb(jb+2:jb+2),'(i1)')l-1                                         !/*gn        462*/
         hb(jb+3:jb+3)='a'                                                      !/*gn        463*/
      else                                                                      !/*gn        464*/
         hb=hf(1:jb)                                                            !/*gn        465*/
         write(hb(jb+1:nb+2),'(i2)')l-1                                         !/*gn        466*/
         hb(jb+3:jb+3)='a'                                                      !/*gn        467*/
      endif                                                                     !/*gn        468*/
      call unique(hb,6)                                                         !/*gn        469*/
      open(31,file=hb,form='formatted',status='new')                            !/*gn        470*/
!                                                                               !/*gn        471*/
!      Write some header information here.                                      !/*gn        472*/
!                                                                               !/*gn        473*/
      write(31,'(1h#)')                                                         !/*gn        474*/
      write(31,'(1h# ,a80)')aid                                                 !/*gn        475*/
      write(31,'(1h# ,"The number of histories = ",i10)')nps                    !/*gn        476*/
      write(31,'(1h# ,"These are ",a10," data for the spherical",'//            !/*gn        477*/
     & '" mesh tally # ",i5)')hip(igpt(ngd)),nugd(ngd)                          !/*gn        478*/
      write(31,'(1h# ,"  ***************** ")')                                 !/*gn        479*/
      write(31,'(1h# ,"The following are ",a2," values for ",a5)')              !/*gn        480*/
     & hl(ni),hj(md+1)                                                          !/*gn        481*/
      write(31,'(1h# ,"values between ",1pe13.6," and ",1pe13.6)')              !/*gn        482*/
     & cl(l-1),cl(l)                                                            !/*gn        483*/
      write(31,'(1h#)')                                                         !/*gn        484*/
!                                                                               !/*gn        485*/
      do 660 j=1,nb                                                             !/*gn        486*/
      do 660 i=1,na                                                             !/*gn        487*/
      mz=(j-1)*na+i                                                             !/*gn        488*/
      if(pdata(lpdat+mz).gt.cl(l-1).and.pdata(lpdat+mz).le.cl(l)) then          !/*gn        489*/
         if(ni.eq.1) then                                                       !/*gn        490*/
            write(31,'(2(1pe14.6))')pdr(lpdr+i),pdp(lpdp+j)                     !/*gn        491*/
         elseif(ni.eq.2) then                                                   !/*gn        492*/
            write(31,'(2(1pe14.6))')pdp(lpdp+i),pda(lpda+j)                     !/*gn        493*/
         else                                                                   !/*gn        494*/
            write(31,'(2(1pe14.6))')pda(lpda+i),pdr(lpdr+j)                     !/*gn        495*/
         endif                                                                  !/*gn        496*/
      endif                                                                     !/*gn        497*/
  660 continue                                                                  !/*gn        498*/
      close(31)                                                                 !/*gn        499*/
  670 continue                                                                  !/*gn        500*/
!                                                                               !/*gn        501*/
  999 return                                                                    !/*gn        502*/
!                                                                               !/*gn        503*/
      end                                                                       !/*gn        504*/
