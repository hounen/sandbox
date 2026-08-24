!     $Id: rdgd.F,v 1.9 2002/03/27 18:13:54 gwm Exp $                         !/*rd          1*/
      subroutine rdgd                                                           !/*rd          2*/
!                                                                               !/*rd          3*/
!       This program is designed to convert rectangular mesh                    !/*rd          4*/
!       tally data output from MCNPX to input files for use                     !/*rd          5*/
!       with one of several graphics packages.                                  !/*rd          6*/
!                                                                               !/*rd          7*/
!       Written by E. C. Snow  11/20/97   (revised: 11/16/2001)                 !/*rd          8*/
!                                                                               !/*rd          9*/
!       The current package selections are:                                     !/*rd         10*/
!          ng=1   IDL <default>                                                 !/*rd         11*/
!          ng=2   PAW (provided that NO_PAW is not set)                         !/*rd         12*/
!          ng=3   Tecplot                                                       !/*rd         13*/
!          ng=4   Gnuplot                                                       !/*rd         14*/
!                                                                               !/*rd         15*/
#include <gridconv/gcdouble.h>                                                  /*!rd         16*/
#include <gridconv/gccom5.h>                                                    /*!rd         17*/
!                                                                               !/*rd         18*/
      dimension gdx(1),gdy(1),gdz(1),gdata(1),pdx(1),pdy(1),                    !/*rd         19*/
     & pdz(1),pdata(1)                                                          !/*rd         20*/
      equivalence (das,gdx,gdy,gdz,gdata,pdx,pdy,pdz,pdata)                     !/*rd         21*/
!                                                                               !/*rd         22*/
      character hf*30,ha*30,ht(3)*11                                            !/*rd         23*/
      data ht/'(X,Y,Z)    ','(R,Z,Theta)','(R,P,A)'/                            !/*rd         24*/
!                                                                               !/*rd         25*/
!       See if this is a asci or binary file.                                   !/*rd         26*/
      mfltyp=1                                                                  !/*rd         27*/
      write(6,'("Is this a binary data file? y/n ")')                           !/*rd         28*/
      write(6,'(" <default=yes>: ")')                                           !/*rd         29*/
      write(6,'("#")')                                                          !/*rd         30*/
      read(5,'(a30)')hf                                                         !/*rd         31*/
      if(hf(1:1).eq.'n'.or.hf(1:2).eq.'no') then                                !/*rd         32*/
         mfltyp=0                                                               !/*rd         33*/
      endif                                                                     !/*rd         34*/
!                                                                               !/*rd         35*/
!       If this is a binary file, see if the user wishes to                     !/*rd         36*/
!       generate an asci file for porting to another system.                    !/*rd         37*/
      if(mfltyp.eq.0)go to 100                                                  !/*rd         38*/
      write(6,'("Do you wish to create a text file? y/n ")')                    !/*rd         39*/
      write(6,'("This file is portable to any system.")')                       !/*rd         40*/
      write(6,'(" <default=no>: ")')                                            !/*rd         41*/
      write(6,'("#")')                                                          !/*rd         42*/
      read(5,'(a30)')hf                                                         !/*rd         43*/
      if(hf.eq.' ')hf='n'                                                       !/*rd         44*/
      if(hf(1:1).eq.'n'.or.hf(1:2).eq.'no')go to 100                            !/*rd         45*/
      write(6,'("What do you want to name your file? ")')                       !/*rd         46*/
      write(6,'(" <default=mdata>: ")')                                         !/*rd         47*/
      write(6,'("#")')                                                          !/*rd         48*/
      read(5,'(a30)')ha                                                         !/*rd         49*/
      if(ha.eq.' ')ha='mdata'                                                   !/*rd         50*/
      call unique(ha,6)                                                         !/*rd         51*/
      open(30,file=hfd,form='unformatted',status='old')                         !/*rd         52*/
      open(31,file=ha,form='formatted',status='new')                            !/*rd         53*/
!                                                                               !/*rd         54*/
      read(30)aid                                                               !/*rd         55*/
      read(30)nmesh,kodvar,vervar,loddatvar,nps                                 !/*rd         56*/
      read(30)n                                                                 !/*rd         57*/
      do 10 i=1,nmesh                                                           !/*rd         58*/
      read(30)ngtyp(i),igpt(i),ng1(i),ng2(i),ng3(i),mxgv(i),mxgc(i),            !/*rd         59*/
     & nugd(i),mxgt(i),(ngv(i,j),j=1,mxv)                                       !/*rd         60*/
      read(30)ergl(i),ergh(i)                                                   !/*rd         61*/
      read(30)m1                                                                !/*rd         62*/
      read(30)m2                                                                !/*rd         63*/
   10 continue                                                                  !/*rd         64*/
!                                                                               !/*rd         65*/
      write(31,'(a80)')aid                                                      !/*rd         66*/
      write(31,'(i2,2x,a8,2x,a5,2x,a28,i10)')                                   !/*rd         67*/
     & nmesh,kodvar,vervar,loddatvar,nps                                        !/*rd         68*/
      write(31,'(1x,i5)')n                                                      !/*rd         69*/
      do 20 i=1,nmesh                                                           !/*rd         70*/
      if(mod(ngtyp(i),10).ge.3)ngv(i,1)=1                                       !/*rd         71*/
      write(31,'(1000(1x,i5))')ngtyp(i),igpt(i),ng1(i),ng2(i),ng3(i),           !/*rd         72*/
     & mxgv(i),mxgc(i),nugd(i),mxgt(i),(ngv(i,j),j=1,mxv)                       !/*rd         73*/
      write(31,'(2(1x,g12.6))')ergl(i),ergh(i)                                  !/*rd         74*/
      write(31,'(1x,i5)')m1                                                     !/*rd         75*/
      write(31,'(1x,i5)')m2                                                     !/*rd         76*/
   20 continue                                                                  !/*rd         77*/
!                                                                               !/*rd         78*/
      lgdx=1                                                                    !/*rd         79*/
      do 60 ig=1,nmesh                                                          !/*rd         80*/
      nz=mod(ngtyp(ig),10)                                                      !/*rd         81*/
      nt=ngtyp(ig)/10                                                           !/*rd         82*/
      lgdy=lgdx+ng1(ig)                                                         !/*rd         83*/
      lgdz=lgdy+ng2(ig)                                                         !/*rd         84*/
      lgdat=lgdz+ng3(ig)                                                        !/*rd         85*/
      read(30)(gdx(lgdx+i),i=1,ng1(ig))                                         !/*rd         86*/
      read(30)(gdy(lgdy+i),i=1,ng2(ig))                                         !/*rd         87*/
      read(30)(gdz(lgdz+i),i=1,ng3(ig))                                         !/*rd         88*/
      write(31,'(1000(1x,g12.6))')(gdx(lgdx+i),i=1,ng1(ig))                     !/*rd         89*/
      write(31,'(1000(1x,g12.6))')(gdy(lgdy+i),i=1,ng2(ig))                     !/*rd         90*/
      write(31,'(1000(1x,g12.6))')(gdz(lgdz+i),i=1,ng3(ig))                     !/*rd         91*/
!                                                                               !/*rd         92*/
      n2=ng2(ig)-1                                                              !/*rd         93*/
      n3=ng3(ig)-1                                                              !/*rd         94*/
      if(nt.eq.3)n2=ng2(ig)                                                     !/*rd         95*/
      if(nt.ne.1)n3=ng3(ig)                                                     !/*rd         96*/
!                                                                               !/*rd         97*/
      do 50 iv=1,mxgv(ig)                                                       !/*rd         98*/
      do 30 k=1,n3                                                              !/*rd         99*/
      do 30 j=1,n2                                                              !/*rd        100*/
      mz=(k-1)*(ng1(ig)-1)*n2+(j-1)*(ng1(ig)-1)                                 !/*rd        101*/
      read(30)(gdata(lgdat+mz+i),i=1,ng1(ig)-1)                                 !/*rd        102*/
   30 continue                                                                  !/*rd        103*/
!                                                                               !/*rd        104*/
      do 40 k=1,n3                                                              !/*rd        105*/
      do 40 j=1,n2                                                              !/*rd        106*/
      mz=(k-1)*(ng1(ig)-1)*n2+(j-1)*(ng1(ig)-1)                                 !/*rd        107*/
      read(30)(gdata(lgdat+mz+mxgc(ig)+i),i=1,ng1(ig)-1)                        !/*rd        108*/
   40 continue                                                                  !/*rd        109*/
!                                                                               !/*rd        110*/
      fpi=1./nps                                                                !/*rd        111*/
      do 33 k=1,n3                                                              !/*rd        112*/
      do 33 j=1,n2                                                              !/*rd        113*/
      mz=(k-1)*(ng1(ig)-1)*n2+(j-1)*(ng1(ig)-1)                                 !/*rd        114*/
      do 33 i=1,ng1(ig)-1                                                       !/*rd        115*/
      t1=gdata(lgdat+mz+i)                                                      !/*rd        116*/
      t2=gdata(lgdat+mxgc(ig)+mz+i)                                             !/*rd        117*/
      if(t1.gt.1.e-35.and.t2.ne.0.) then                                        !/*rd        118*/
         t3=t2/t1**2-fpi                                                        !/*rd        119*/
         if(t3.gt.1.)t3=1.0                                                     !/*rd        120*/
         if(t3.lt.0.)t3=0.0                                                     !/*rd        121*/
         gdata(lgdat+mz+i)=t1*fpi                                               !/*rd        122*/
         gdata(lgdat+mxgc(ig)+mz+i)=sqrt(t3)                                    !/*rd        123*/
      else                                                                      !/*rd        124*/
         gdata(lgdat+mz+i)=0.                                                   !/*rd        125*/
         gdata(lgdat+mxgc(ig)+mz+i)=0.                                          !/*rd        126*/
      endif                                                                     !/*rd        127*/
   33 continue                                                                  !/*rd        128*/
!                                                                               !/*rd        129*/
      do 35 k=1,n3                                                              !/*rd        130*/
      do 35 j=1,n2                                                              !/*rd        131*/
      mz=(k-1)*(ng1(ig)-1)*n2+(j-1)*(ng1(ig)-1)                                 !/*rd        132*/
      write(31,'(1000(1x,1pe12.6))')(gdata(lgdat+mz+i),i=1,ng1(ig)-1)           !/*rd        133*/
   35 continue                                                                  !/*rd        134*/
!                                                                               !/*rd        135*/
      do 45 k=1,n3                                                              !/*rd        136*/
      do 45 j=1,n2                                                              !/*rd        137*/
      mz=(k-1)*(ng1(ig)-1)*n2+(j-1)*(ng1(ig)-1)                                 !/*rd        138*/
      write(31,'(1000(5x,f8.6))')(gdata(lgdat+mz+mxgc(ig)+i),                   !/*rd        139*/
     & i=1,ng1(ig)-1)                                                           !/*rd        140*/
   45 continue                                                                  !/*rd        141*/
   50 continue                                                                  !/*rd        142*/
!                                                                               !/*rd        143*/
   60 continue                                                                  !/*rd        144*/
!                                                                               !/*rd        145*/
!       See if the user wants to continue with creating an                      !/*rd        146*/
!       input for a graphics package.                                           !/*rd        147*/
!                                                                               !/*rd        148*/
      write(6,'("Do you wish to continue? y/n ")')                              !/*rd        149*/
      write(6,'(" <default=no>: ")')                                            !/*rd        150*/
      write(6,'("#")')                                                          !/*rd        151*/
      read(5,'(a30)')hf                                                         !/*rd        152*/
      if(hf.eq.' ')hf='n'                                                       !/*rd        153*/
      if(hf(1:1).eq.'y'.or.hf(1:3).eq.'yes') then                               !/*rd        154*/
         rewind(30)                                                             !/*rd        155*/
         close(31)                                                              !/*rd        156*/
         go to 105                                                              !/*rd        157*/
      else                                                                      !/*rd        158*/
         go to 600                                                              !/*rd        159*/
      endif                                                                     !/*rd        160*/
!                                                                               !/*rd        161*/
!       Open data input file and read header information.                       !/*rd        162*/
!                                                                               !/*rd        163*/
  100 if(mfltyp.eq.0) then                                                      !/*rd        164*/
         open(30,file=hfd,form='formatted',status='old')                        !/*rd        165*/
      else                                                                      !/*rd        166*/
         open(30,file=hfd,form='unformatted',status='old')                      !/*rd        167*/
      endif                                                                     !/*rd        168*/
!                                                                               !/*rd        169*/
  105 nd=mxg*mxv/10                                                             !/*rd        170*/
      if(mfltyp.eq.0) then                                                      !/*rd        171*/
         read(30,'(a80)')aid                                                    !/*rd        172*/
         read(30,'(i2,2x,a8,2x,a5,2x,a28,i10)')                                 !/*rd        173*/
     &    nmesh,kodvar,vervar,loddatvar,nps                                     !/*rd        174*/
         read(30,'(1x,i5)')n                                                    !/*rd        175*/
         do 110 i=1,nmesh                                                       !/*rd        176*/
         read(30,'(1000(1x,i5))')ngtyp(i),igpt(i),ng1(i),ng2(i),ng3(i),         !/*rd        177*/
     &    mxgv(i),mxgc(i),nugd(i),mxgt(i),(ngv(i,j),j=1,mxv)                    !/*rd        178*/
         read(30,'(2(1x,g12.6))')ergl(i),ergh(i)                                !/*rd        179*/
         read(30,'(1x,i5)')m1                                                   !/*rd        180*/
         read(30,'(1x,i5)')m2                                                   !/*rd        181*/
  110    continue                                                               !/*rd        182*/
      else                                                                      !/*rd        183*/
         read(30)aid                                                            !/*rd        184*/
         read(30)nmesh,kodvar,vervar,loddatvar,nps                              !/*rd        185*/
         read(30)n                                                              !/*rd        186*/
         do 115 i=1,nmesh                                                       !/*rd        187*/
         read(30)ngtyp(i),igpt(i),ng1(i),ng2(i),ng3(i),mxgv(i),mxgc(i),         !/*rd        188*/
     &    nugd(i),mxgt(i),(ngv(i,j),j=1,mxv)                                    !/*rd        189*/
         read(30)ergl(i),ergh(i)                                                !/*rd        190*/
         read(30)m1                                                             !/*rd        191*/
         read(30)m2                                                             !/*rd        192*/
  115    continue                                                               !/*rd        193*/
      endif                                                                     !/*rd        194*/
!                                                                               !/*rd        195*/
!       See if there is going to be enough storage space.                       !/*rd        196*/
      ms=0                                                                      !/*rd        197*/
      do 120 i=1,nmesh                                                          !/*rd        198*/
      ms=max(ms,(mxgc(i)+ng1(i)+ng2(i)+ng3(i))*2)                               !/*rd        199*/
  120 continue                                                                  !/*rd        200*/
      if(ms.gt.mdas) then                                                       !/*rd        201*/
         write(6,'("Data sets on this file are too large for space.")')         !/*rd        202*/
         write(6,'("This code will have to be recompiled with a ")')            !/*rd        203*/
         write(6,'("vaule of MDAS at least = ",i10)')ms                         !/*rd        204*/
         go to 600                                                              !/*rd        205*/
      endif                                                                     !/*rd        206*/
!                                                                               !/*rd        207*/
!        Print header information about data file.                              !/*rd        208*/
      write(6,125)nmesh,kodvar,vervar,loddatvar,aid                             !/*rd        209*/
  125 format(1h ,'There are ',i1,' sets of mesh data on this file.',//          !/*rd        210*/
     & 5x,'Written by ',a8,' version ',a8,' loaded ',a28,//                     !/*rd        211*/
     & 5x,'The comment line on the INP file read:',/                            !/*rd        212*/
     & 10x,a80,/// )                                                            !/*rd        213*/
!                                                                               !/*rd        214*/
      write(6,130)                                                              !/*rd        215*/
  130 format(1h ,'The discription of what information is on each',              !/*rd        216*/
     & ' data set follows.')                                                    !/*rd        217*/
      do 500 i=1,nmesh                                                          !/*rd        218*/
      jk=0                                                                      !/*rd        219*/
      jx=mod(ngtyp(i),10)                                                       !/*rd        220*/
      jt=(ngtyp(i)-jx)/10                                                       !/*rd        221*/
      if(jx.gt.2) go to 170                                                     !/*rd        222*/
      do 140 j=1,mxv                                                            !/*rd        223*/
      if(ngv(i,j).eq.0)go to 140                                                !/*rd        224*/
      jk=jk+1                                                                   !/*rd        225*/
      if(jx.eq.0) then                                                          !/*rd        226*/
         hdx(i,jk)=hdv(ngv(i,j))                                                !/*rd        227*/
      elseif(jx.eq.1) then                                                      !/*rd        228*/
         hdx(i,jk)=hip(ngv(i,j))                                                !/*rd        229*/
      elseif(jx.eq.2) then                                                      !/*rd        230*/
         hdx(i,jk)=hdd(ngv(i,j))                                                !/*rd        231*/
      endif                                                                     !/*rd        232*/
      nmv(i,jk)=jk                                                              !/*rd        233*/
  140 continue                                                                  !/*rd        234*/
      njt(i)=jk                                                                 !/*rd        235*/
!                                                                               !/*rd        236*/
!       Write information about each data set. (nmesh)                          !/*rd        237*/
      write(6,150)i,jt,ht(jt),ergl(i),ergh(i)                                   !/*rd        238*/
  150 format(///5x,'Data set no. ',i1,' for mesh type ',i1,2x,a11,/             !/*rd        239*/
     & 10x,'including energies from ',g12.6,' to ',g12.6,' Mev.',/              !/*rd        240*/
     & 10x,'and contains the following information:'/ )                         !/*rd        241*/
      n2=ng2(i)-1                                                               !/*rd        242*/
      n3=ng3(i)-1                                                               !/*rd        243*/
      if(jt.eq.3)n2=ng2(i)                                                      !/*rd        244*/
      if(jt.ne.1)n3=ng3(i)                                                      !/*rd        245*/
      if(jx.eq.0) then                                                          !/*rd        246*/
         write(6,160)nugd(i),hip(igpt(i)),ng1(i)-1,n2,n3,                       !/*rd        247*/
     &    (hdx(i,j),nmv(i,j),j=1,jk)                                            !/*rd        248*/
      else                                                                      !/*rd        249*/
         write(6,165)nugd(i),ng1(i)-1,n2,n3,                                    !/*rd        250*/
     &    (hdx(i,j),nmv(i,j),j=1,jk)                                            !/*rd        251*/
      endif                                                                     !/*rd        252*/
  160 format(1h ,/10x,'The user assigned number = ',i6,/                        !/*rd        253*/
     & 10x,'The particle type is a ',a10,/                                      !/*rd        254*/
     & 10x,'The number of cora,corb,corc values = ',3i5,/                       !/*rd        255*/
     & 10x,'The data was recorded for:',                                        !/*rd        256*/
     & :/(15x,a10,'   Data index = ',i1))                                       !/*rd        257*/
  165 format(1h ,/10x,'The user assigned number = ',i6,/                        !/*rd        258*/
     & 10x,'This a source point mesh ',/                                        !/*rd        259*/
     & 10x,'The number of cora,corb,corc values = ',3i5,/                       !/*rd        260*/
     & 10x,'The data was recorded for:',                                        !/*rd        261*/
     & :/(15x,a10,'   Data index = ',i1))                                       !/*rd        262*/
      if(m2.ne.0)write(6,168)m2                                                 !/*rd        263*/
  168 format(1h ,/10x,'This mesh is multiplied by function ',i2)                !/*rd        264*/
      go to 500                                                                 !/*rd        265*/
!                                                                               !/*rd        266*/
  170 nmv(i,1)=1                                                                !/*rd        267*/
      jk=1                                                                      !/*rd        268*/
      do 180 j=2,mxv                                                            !/*rd        269*/
      if(ngv(i,j).eq.0)go to 180                                                !/*rd        270*/
      jk=jk+1                                                                   !/*rd        271*/
      nmv(i,jk)=jk                                                              !/*rd        272*/
  180 continue                                                                  !/*rd        273*/
      njt(i)=jk                                                                 !/*rd        274*/
!                                                                               !/*rd        275*/
      go to(200,300)jx-2                                                        !/*rd        276*/
!                                                                               !/*rd        277*/
!       Point detector contribution tracks.                                     !/*rd        278*/
!                                                                               !/*rd        279*/
!                                                                               !/*rd        280*/
!       Write information about each data set. (nmesh)                          !/*rd        281*/
  200 write(6,210)i,jt,ht(jt),ergl(i),ergh(i)                                   !/*rd        282*/
  210 format(///5x,'Data set no. ',i1,' for mesh type ',i1,2x,a11,/             !/*rd        283*/
     & 10x,'including energies from ',g12.6,' to ',g12.6,' Mev.',/              !/*rd        284*/
     & 10x,'and contains the following information:'/ )                         !/*rd        285*/
      n2=ng2(i)-1                                                               !/*rd        286*/
      n3=ng3(i)-1                                                               !/*rd        287*/
      if(jt.eq.3)n2=ng2(i)                                                      !/*rd        288*/
      if(jt.ne.1)n3=ng3(i)                                                      !/*rd        289*/
      write(6,220)nugd(i),ng1(i)-1,n2,n3                                        !/*rd        290*/
  220 format(1h ,/10x,'The user assigned number = ',i6,/                        !/*rd        291*/
     & 10x,'This a point detector track mesh ',/                                !/*rd        292*/
     & 10x,'The number of cora,corb,corc values = ',3i5,/                       !/*rd        293*/
     & 10x,'The data was recorded for all point detectors.')                    !/*rd        294*/
      if(m2.ne.0)write(6,230)m2                                                 !/*rd        295*/
  230 format(1h ,/10x,'This mesh is multiplied by function ',i2)                !/*rd        296*/
      go to 500                                                                 !/*rd        297*/
!                                                                               !/*rd        298*/
!       Dxtran sphere contribution tracks.                                      !/*rd        299*/
!                                                                               !/*rd        300*/
!                                                                               !/*rd        301*/
!       Write information about each data set. (nmesh)                          !/*rd        302*/
  300 write(6,310)i,jt,ht(jt),ergl(i),ergh(i)                                   !/*rd        303*/
  310 format(///5x,'Data set no. ',i1,' for mesh type ',i1,2x,a11,/             !/*rd        304*/
     & 10x,'including energies from ',g12.6,' to ',g12.6,' Mev.',/              !/*rd        305*/
     & 10x,'and contains the following information:'/ )                         !/*rd        306*/
      n2=ng2(i)-1                                                               !/*rd        307*/
      n3=ng3(i)-1                                                               !/*rd        308*/
      if(jt.eq.3)n2=ng2(i)                                                      !/*rd        309*/
      if(jt.ne.1)n3=ng3(i)                                                      !/*rd        310*/
      write(6,320)nugd(i),ng1(i)-1,n2,n3                                        !/*rd        311*/
  320 format(1h ,/10x,'The user assigned number = ',i6,/                        !/*rd        312*/
     & 10x,'This a point detector track mesh ',/                                !/*rd        313*/
     & 10x,'The number of cora,corb,corc values = ',3i5,/                       !/*rd        314*/
     & 10x,'The data was recorded for all dxtran spheres.')                     !/*rd        315*/
      if(m2.ne.0)write(6,330)m2                                                 !/*rd        316*/
  330 format(1h ,/10x,'This mesh is multiplied by function ',i2)                !/*rd        317*/
!                                                                               !/*rd        318*/
!       Space for additional types of mesh tallies.(#300 & #400 ...)            !/*rd        319*/
!                                                                               !/*rd        320*/
  500 continue                                                                  !/*rd        321*/
!                                                                               !/*rd        322*/
!       Read in the set of data requested by user.                              !/*rd        323*/
      ngd=0                                                                     !/*rd        324*/
      nxv=0                                                                     !/*rd        325*/
      write(6,'(" ")')                                                          !/*rd        326*/
      write(6,'(" ")')                                                          !/*rd        327*/
      write(6,'(" ")')                                                          !/*rd        328*/
      write(6,'("Enter the data set no. n, and data index i")')                 !/*rd        329*/
      write(6,'("  Default =1 1 or n 1 ")')                                     !/*rd        330*/
      write(6,'("#")')                                                          !/*rd        331*/
      read(5,'(a30)')hf                                                         !/*rd        332*/
      do 502 j=1,30                                                             !/*rd        333*/
      n=index('0123456789',hf(j:j))                                             !/*rd        334*/
      if(n.ne.0) go to 503                                                      !/*rd        335*/
  502 continue                                                                  !/*rd        336*/
      ngd=1                                                                     !/*rd        337*/
      nxv=1                                                                     !/*rd        338*/
      go to 508                                                                 !/*rd        339*/
!                                                                               !/*rd        340*/
  503 kk=1                                                                      !/*rd        341*/
      do 506 i=1,2                                                              !/*rd        342*/
      kx=0                                                                      !/*rd        343*/
      do 504 j=kk,30                                                            !/*rd        344*/
      n=index('0123456789',hf(j:j))                                             !/*rd        345*/
      if(n.ne.0) then                                                           !/*rd        346*/
         kx=kx*10+n-1                                                           !/*rd        347*/
      elseif(n.eq.0.and.kx.gt.0) then                                           !/*rd        348*/
         kk=j+1                                                                 !/*rd        349*/
         go to 505                                                              !/*rd        350*/
      elseif(i.eq.2.and.kx.eq.0.and.j.eq.30) then                               !/*rd        351*/
         nxv=1                                                                  !/*rd        352*/
         go to 508                                                              !/*rd        353*/
      endif                                                                     !/*rd        354*/
  504 continue                                                                  !/*rd        355*/
!                                                                               !/*rd        356*/
  505 if(i.eq.1) then                                                           !/*rd        357*/
         ngd=kx                                                                 !/*rd        358*/
      else                                                                      !/*rd        359*/
         nxv=kx                                                                 !/*rd        360*/
      endif                                                                     !/*rd        361*/
  506 continue                                                                  !/*rd        362*/
!                                                                               !/*rd        363*/
  508 if(ngd.le.nmesh)go to 510                                                 !/*rd        364*/
      write(6,'("Data set no. must be between 1 and ",i5)')nmesh                !/*rd        365*/
      Write(6,'("Please reenter:")')                                            !/*rd        366*/
      write(6,'("#")')                                                          !/*rd        367*/
      read(5,'(a30)')hf                                                         !/*rd        368*/
      kx=0                                                                      !/*rd        369*/
      do 509 j=1,30                                                             !/*rd        370*/
      n=index('0123456789',hf(j:j))                                             !/*rd        371*/
      if(n.ne.0) then                                                           !/*rd        372*/
         kx=10*kx+n-1                                                           !/*rd        373*/
      elseif(n.eq.0.and.kx.gt.0) then                                           !/*rd        374*/
         ngd=kx                                                                 !/*rd        375*/
         go to 508                                                              !/*rd        376*/
      endif                                                                     !/*rd        377*/
  509 continue                                                                  !/*rd        378*/
      ngd=1                                                                     !/*rd        379*/
!                                                                               !/*rd        380*/
  510 do 520 i=1,mxv                                                            !/*rd        381*/
      if(nxv.eq.i.and.ngv(ngd,i).ne.0)go to 530                                 !/*rd        382*/
  520 continue                                                                  !/*rd        383*/
      write(6,'("This data index does not exist in this mesh set.")')           !/*rd        384*/
      write(6,'("Please select another or press return to exit.")')             !/*rd        385*/
      write(6,'("#")')                                                          !/*rd        386*/
      read(5,'(a30)')hf                                                         !/*rd        387*/
      kx=0                                                                      !/*rd        388*/
      do 525 j=1,30                                                             !/*rd        389*/
      n=index('0123456789',hf(j:j))                                             !/*rd        390*/
      if(n.ne.0) then                                                           !/*rd        391*/
         kx=10*kx+n-1                                                           !/*rd        392*/
      elseif(n.eq.0.and.kx.gt.0) then                                           !/*rd        393*/
         nxv=kx                                                                 !/*rd        394*/
         if(nxv.le.mxv.and.nxv.ne.0)go to 510                                   !/*rd        395*/
      endif                                                                     !/*rd        396*/
  525 continue                                                                  !/*rd        397*/
      write(6,'("Index no. error!")')                                           !/*rd        398*/
      go to 600                                                                 !/*rd        399*/
!                                                                               !/*rd        400*/
!       Read into memory the data of choice.                                    !/*rd        401*/
!       First, setup memory offsets for this data.                              !/*rd        402*/
!                                                                               !/*rd        403*/
  530 jx=mod(ngtyp(ngd),10)                                                     !/*rd        404*/
      jt=(ngtyp(ngd)-jx)/10                                                     !/*rd        405*/
      if(jt.eq.1)call rdgdx                                                     !/*rd        406*/
      if(jt.eq.2)call rdgdr                                                     !/*rd        407*/
      if(jt.eq.3)call rdgds                                                     !/*rd        408*/
!                                                                               !/*rd        409*/
!                                                                               !/*rd        410*/
!       Find out what graphics package is to be used                            !/*rd        411*/
!       and set the default to IDL. (ng=1)                                      !/*rd        412*/
!                                                                               !/*rd        413*/
  540 write(6,'("Enter name of graphics package to be used.")')                 !/*rd        414*/
  550 write(6,'("Your choices are:")')                                          !/*rd        415*/
      write(6,'("  IDL <default>")')                                            !/*rd        416*/
#ifndef NO_PAW                                                                  /*!rd        417*/
      write(6,'("  PAW")')                                                      !/*rd        418*/
#endif                                                                          /*!rd        419*/
      write(6,'("  tecplot")')                                                  !/*rd        420*/
      write(6,'("  gnuplot")')                                                  !/*rd        421*/
      write(6,'("#")')                                                          !/*rd        422*/
      read(5,'(a30)')hfp                                                        !/*rd        423*/
      if(hfp(1:3).eq.'idl'.or.hfp(1:3).eq.'IDL'                                 !/*rd        424*/
     & .or.hfp(1:3).eq.'Idl'.or.hfp.eq.' ') then                                !/*rd        425*/
        ng=1                                                                    !/*rd        426*/
        go to 560                                                               !/*rd        427*/
#ifndef NO_PAW                                                                  /*!rd        428*/
      elseif(hfp(1:3).eq.'paw'.or.hfp(1:3).eq.'PAW'                             !/*rd        429*/
     & .or.hfp(1:3).eq.'Paw') then                                              !/*rd        430*/
        ng=2                                                                    !/*rd        431*/
        go to 560                                                               !/*rd        432*/
#endif                                                                          /*!rd        433*/
      elseif(hfp(1:7).eq.'tecplot'.or.hfp(1:7).eq.'TECPLOT'.or.                 !/*rd        434*/
     & hfp(1:7).eq.'TecPlot'.or.hfp(1:7).eq.'Tecplot') then                     !/*rd        435*/
        ng=3                                                                    !/*rd        436*/
        go to 560                                                               !/*rd        437*/
      elseif(hfp(1:7).eq.'gnuplot'.or.hfp(1:7).eq.'GNUPLOT'.or.                 !/*rd        438*/
     & hfp(1:7).eq.'GnuPlot'.or.hfp(1:7).eq.'Gnuplot') then                     !/*rd        439*/
        ng=4                                                                    !/*rd        440*/
        go to 560                                                               !/*rd        441*/
      else                                                                      !/*rd        442*/
         write(6,'("Graphics package ",a10," is unacceptable.")')               !/*rd        443*/
     &    hfp(1:10)                                                             !/*rd        444*/
         write(6,'("Please try again.")')                                       !/*rd        445*/
         go to 550                                                              !/*rd        446*/
      endif                                                                     !/*rd        447*/
!                                                                               !/*rd        448*/
  560 if(jt.eq.1) then                                                          !/*rd        449*/
         if(ng.eq.1)call idldat                                                 !/*rd        450*/
#ifndef NO_PAW                                                                  /*!rd        451*/
         if(ng.eq.2)call pawdat                                                 !/*rd        452*/
#endif                                                                          /*!rd        453*/
         if(ng.eq.3)call tecdat                                                 !/*rd        454*/
         if(ng.eq.4)call gnudat                                                 !/*rd        455*/
      elseif(jt.eq.2) then                                                      !/*rd        456*/
         if(ng.eq.1)call idldatr                                                !/*rd        457*/
#ifndef NO_PAW                                                                  /*!rd        458*/
         if(ng.eq.2)call pawdatr                                                !/*rd        459*/
#endif                                                                          /*!rd        460*/
         if(ng.eq.3)call tecdatr                                                !/*rd        461*/
         if(ng.eq.4)call gnudatr                                                !/*rd        462*/
      else                                                                      !/*rd        463*/
         if(ng.eq.1)call idldats                                                !/*rd        464*/
#ifndef NO_PAW                                                                  /*!rd        465*/
         if(ng.eq.2)call pawdats                                                !/*rd        466*/
#endif                                                                          /*!rd        467*/
         if(ng.eq.3)call tecdats                                                !/*rd        468*/
         if(ng.eq.4)call gnudats                                                !/*rd        469*/
      endif                                                                     !/*rd        470*/
!                                                                               !/*rd        471*/
      write(6,'("Do you wish to create another plot file. y/n ?")')             !/*rd        472*/
      write(6,'("Type yes(y) or no(n) or hit return to exit.")')                !/*rd        473*/
      write(6,'("#")')                                                          !/*rd        474*/
      read(5,'(a3)')hf                                                          !/*rd        475*/
      if(hf(1:1).eq.'y'.or.hf.eq.'yes') then                                    !/*rd        476*/
         write(6,'("Do you wish to use the same data set. y/n ?")')             !/*rd        477*/
         write(6,'("Type yes(y) or no(n). <default yes>")')                     !/*rd        478*/
         write(6,'("#")')                                                       !/*rd        479*/
         read(5,'(a3)')hf                                                       !/*rd        480*/
         if(hf(1:1).eq.'n'.or.hf.eq.'no ') then                                 !/*rd        481*/
            rewind 30                                                           !/*rd        482*/
            go to 105                                                           !/*rd        483*/
         endif                                                                  !/*rd        484*/
         go to 540                                                              !/*rd        485*/
      endif                                                                     !/*rd        486*/
!                                                                               !/*rd        487*/
  600 close(30)                                                                 !/*rd        488*/
      close(31)                                                                 !/*rd        489*/
      return                                                                    !/*rd        490*/
!                                                                               !/*rd        491*/
      end                                                                       !/*rd        492*/
