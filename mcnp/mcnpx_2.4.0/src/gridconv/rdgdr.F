!     $Id: rdgdr.F,v 1.8 2002/03/27 18:13:54 gwm Exp $                        !/*rd          1*/
      subroutine rdgdr                                                          !/*rd          2*/
!                                                                               !/*rd          3*/
!       This program is designed to convert r, z, theta mesh                    !/*rd          4*/
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
      dimension gdr(1),gdz(1),gdt(1),gdata(1),pdr(1),pdz(1),                    !/*rd         19*/
     & pdt(1),pdata(1)                                                          !/*rd         20*/
      equivalence (das,gdr,gdz,gdt,gdata,pdr,pdz,pdt,pdata)                     !/*rd         21*/
!                                                                               !/*rd         22*/
!       Read into memory the data of choice.                                    !/*rd         23*/
!       First, setup memory offsets for this data.                              !/*rd         24*/
!                                                                               !/*rd         25*/
      lgdr=1                                                                    !/*rd         26*/
      lgdz=lgdr+ng1(ngd)                                                        !/*rd         27*/
      lgdt=lgdz+ng2(ngd)                                                        !/*rd         28*/
      lgdat=lgdt+ng3(ngd)                                                       !/*rd         29*/
      lpdr=lgdat+2*ng1(ngd)*ng2(ngd)*ng3(ngd)                                   !/*rd         30*/
      lpdz=lpdr+ng1(ngd)-1                                                      !/*rd         31*/
      lpdt=lpdz+ng2(ngd)-1                                                      !/*rd         32*/
      lpdat=lpdt+ng3(ngd)                                                       !/*rd         33*/
!                                                                               !/*rd         34*/
      if(ngd.eq.1)go to 150                                                     !/*rd         35*/
      do 140 ii=1,ngd-1                                                         !/*rd         36*/
      if(mfltyp.eq.0) then                                                      !/*rd         37*/
         read(30,'(g13.6)')xdum                                                 !/*rd         38*/
         read(30,'(g13.6)')xdum                                                 !/*rd         39*/
         read(30,'(g13.6)')xdum                                                 !/*rd         40*/
      else                                                                      !/*rd         41*/
         read(30)xdum                                                           !/*rd         42*/
         read(30)xdum                                                           !/*rd         43*/
         read(30)xdum                                                           !/*rd         44*/
      endif                                                                     !/*rd         45*/
      n3=ng3(ii)                                                                !/*rd         46*/
      jt=ngtyp(ii)/10                                                           !/*rd         47*/
      if(jt.eq.1)n3=n3-1                                                        !/*rd         48*/
      do 140 jj=1,njt(ii)                                                       !/*rd         49*/
      do 140 kk=1,2                                                             !/*rd         50*/
      do 143 k=1,n3                                                             !/*rd         51*/
      do 143 j=1,ng2(ii)-1                                                      !/*rd         52*/
      if(mfltyp.eq.0) then                                                      !/*rd         53*/
         read(30,'(g13.6)')xdum                                                 !/*rd         54*/
      else                                                                      !/*rd         55*/
         read(30)xdum                                                           !/*rd         56*/
      endif                                                                     !/*rd         57*/
  143 continue                                                                  !/*rd         58*/
  140 continue                                                                  !/*rd         59*/
!                                                                               !/*rd         60*/
  150 if(mfltyp.eq.0) then                                                      !/*rd         61*/
         read(30,'(1000(1x,g12.6))')(gdr(lgdr+i),i=1,ng1(ngd))                  !/*rd         62*/
         read(30,'(1000(1x,g12.6))')(gdz(lgdz+i),i=1,ng2(ngd))                  !/*rd         63*/
         read(30,'(1000(1x,g12.6))')(gdt(lgdt+i),i=1,ng3(ngd))                  !/*rd         64*/
      else                                                                      !/*rd         65*/
         read(30)(gdr(lgdr+i),i=1,ng1(ngd))                                     !/*rd         66*/
         read(30)(gdz(lgdz+i),i=1,ng2(ngd))                                     !/*rd         67*/
         read(30)(gdt(lgdt+i),i=1,ng3(ngd))                                     !/*rd         68*/
      endif                                                                     !/*rd         69*/
      do 160 jj=1,mxv                                                           !/*rd         70*/
      if(nxv.eq.jj.and.ngv(ngd,jj).ne.0)go to 170                               !/*rd         71*/
      do 160 kk=1,2                                                             !/*rd         72*/
      do 155 k=1,ng3(ngd)                                                       !/*rd         73*/
      do 155 j=1,ng2(ngd)-1                                                     !/*rd         74*/
      if(mfltyp.eq.0) then                                                      !/*rd         75*/
         read(30,'(g13.6)')xdum                                                 !/*rd         76*/
      else                                                                      !/*rd         77*/
         read(30)xdum                                                           !/*rd         78*/
      endif                                                                     !/*rd         79*/
  155 continue                                                                  !/*rd         80*/
  160 continue                                                                  !/*rd         81*/
!                                                                               !/*rd         82*/
  170 do 180 k=1,ng3(ngd)                                                       !/*rd         83*/
      do 180 j=1,ng2(ngd)-1                                                     !/*rd         84*/
      mz=(k-1)*(ng1(ngd)-1)*(ng2(ngd)-1)+(j-1)*(ng1(ngd)-1)                     !/*rd         85*/
      if(mfltyp.eq.0) then                                                      !/*rd         86*/
         read(30,'(1000(1x,g12.6))')                                            !/*rd         87*/
     &    (gdata(lgdat+mz+i),i=1,ng1(ngd)-1)                                    !/*rd         88*/
      else                                                                      !/*rd         89*/
         read(30)(gdata(lgdat+mz+i),i=1,ng1(ngd)-1)                             !/*rd         90*/
      endif                                                                     !/*rd         91*/
  180 continue                                                                  !/*rd         92*/
!                                                                               !/*rd         93*/
      do 190 k=1,ng3(ngd)                                                       !/*rd         94*/
      do 190 j=1,ng2(ngd)-1                                                     !/*rd         95*/
      mz=(k-1)*(ng1(ngd)-1)*(ng2(ngd)-1)+(j-1)*(ng1(ngd)-1)                     !/*rd         96*/
      if(mfltyp.eq.0) then                                                      !/*rd         97*/
         read(30,'(1000(1x,g12.6))')                                            !/*rd         98*/
     &    (gdata(lgdat+mxgc(ngd)+mz+i),i=1,ng1(ngd)-1)                          !/*rd         99*/
      else                                                                      !/*rd        100*/
         read(30)(gdata(lgdat+mxgc(ngd)+mz+i),i=1,ng1(ngd)-1)                   !/*rd        101*/
      endif                                                                     !/*rd        102*/
  190 continue                                                                  !/*rd        103*/
!                                                                               !/*rd        104*/
!       Normalize and calculate error if this is raw data.                      !/*rd        105*/
      if (mfltyp.ne.0) then                                                     !/*rd        106*/
         fpi=1./nps                                                             !/*rd        107*/
         do 192 k=1,ng3(ngd)                                                    !/*rd        108*/
            do 192 j=1,ng2(ngd)-1                                               !/*rd        109*/
               mz=(k-1)*(ng1(ngd)-1)*(ng2(ngd)-1)+(j-1)*(ng1(ngd)-1)            !/*rd        110*/
               do 192 i=1,ng1(ngd)-1                                            !/*rd        111*/
                  t1=gdata(lgdat+mz+i)                                          !/*rd        112*/
                  t2=gdata(lgdat+mxgc(ngd)+mz+i)                                !/*rd        113*/
                  if(t1.gt.1.e-35.and.t2.ne.0.) then                            !/*rd        114*/
                     t3=t2/t1**2-fpi                                            !/*rd        115*/
                     if(t3.gt.1.0)t3=1.0                                        !/*rd        116*/
                     if(t3.lt.0.0)t3=0.0                                        !/*rd        117*/
                     gdata(lgdat+mz+i)=t1*fpi                                   !/*rd        118*/
                     gdata(lgdat+mxgc(ngd)+mz+i)=sqrt(t3)                       !/*rd        119*/
                  else                                                          !/*rd        120*/
                     gdata(lgdat+mz+i)=0.                                       !/*rd        121*/
                     gdata(lgdat+mxgc(ngd)+mz+i)=0.                             !/*rd        122*/
                  endif                                                         !/*rd        123*/
  192    continue                                                               !/*rd        124*/
      endif                                                                     !/*rd        125*/
!                                                                               !/*rd        126*/
!       Set up to write data to graphics input file.                            !/*rd        127*/
!       First determine if the mesh is regular or irregular.                    !/*rd        128*/
      dx1=abs(gdr(lgdr+2)-gdr(lgdr+1))                                          !/*rd        129*/
      dx4=0.01*dx1                                                              !/*rd        130*/
      do 210 i=3,ng1(ngd)                                                       !/*rd        131*/
      dx2=abs(gdr(lgdr+i)-gdr(lgdr+i-1))                                        !/*rd        132*/
      dx3=abs(dx1-dx2)                                                          !/*rd        133*/
      if(dx3.gt.dx4)go to 220                                                   !/*rd        134*/
  210 continue                                                                  !/*rd        135*/
      irs=1                                                                     !/*rd        136*/
      go to 230                                                                 !/*rd        137*/
!                                                                               !/*rd        138*/
  220 irs=2                                                                     !/*rd        139*/
!                                                                               !/*rd        140*/
  230 do 240 i=2,ng1(ngd)                                                       !/*rd        141*/
  240 pdr(lpdr+i-1)=(gdr(lgdr+i-1)+gdr(lgdr+i))/2.0                             !/*rd        142*/
!                                                                               !/*rd        143*/
      dy1=abs(gdz(lgdz+2)-gdz(lgdz+1))                                          !/*rd        144*/
      dy4=0.01*dy1                                                              !/*rd        145*/
      do 250 i=3,ng2(ngd)                                                       !/*rd        146*/
      dy2=abs(gdz(lgdz+i)-gdz(lgdz+i-1))                                        !/*rd        147*/
      dy3=abs(dy1-dy2)                                                          !/*rd        148*/
      if(dy3.gt.dy4)go to 260                                                   !/*rd        149*/
  250 continue                                                                  !/*rd        150*/
      izs=1                                                                     !/*rd        151*/
      go to 270                                                                 !/*rd        152*/
!                                                                               !/*rd        153*/
  260 izs=2                                                                     !/*rd        154*/
!                                                                               !/*rd        155*/
  270 do 280 i=2,ng2(ngd)                                                       !/*rd        156*/
  280 pdz(lpdz+i-1)=(gdz(lgdz+i-1)+gdz(lgdz+i))/2.0                             !/*rd        157*/
!                                                                               !/*rd        158*/
      dz1=gdt(lgdt+1)                                                           !/*rd        159*/
      dz4=0.01*dz1                                                              !/*rd        160*/
      do 290 i=2,ng3(ngd)                                                       !/*rd        161*/
      dz2=abs(gdt(lgdt+i)-gdt(lgdt+i-1))                                        !/*rd        162*/
      dz3=abs(dz1-dz2)                                                          !/*rd        163*/
      if(dz3.gt.dz4)go to 300                                                   !/*rd        164*/
  290 continue                                                                  !/*rd        165*/
      its=1                                                                     !/*rd        166*/
      go to 310                                                                 !/*rd        167*/
!                                                                               !/*rd        168*/
  300 its=2                                                                     !/*rd        169*/
!                                                                               !/*rd        170*/
  310 pdt(lpdt+1)=gdt(lgdt+1)/2.0                                               !/*rd        171*/
      do 320 i=2,ng3(ngd)                                                       !/*rd        172*/
  320 pdt(lpdt+i)=(gdt(lgdt+i)+gdt(lgdt+i-1))/2.0                               !/*rd        173*/
!                                                                               !/*rd        174*/
      write(6,'("Spacing in the three dimensions are./ ")')                     !/*rd        175*/
      write(6,'("  R is ",a9)')hdb(irs)                                         !/*rd        176*/
      write(6,'("The range is from ",1pe15.6," to ",1pe15.6)')                  !/*rd        177*/
     & gdr(lgdr+1),gdr(lgdr+ng1(ngd))                                           !/*rd        178*/
      if(irs.eq.1) then                                                         !/*rd        179*/
         write(6,'("  The Delta-R is ",1pe15.6)')dx1                            !/*rd        180*/
      endif                                                                     !/*rd        181*/
      write(6,'(/ )')                                                           !/*rd        182*/
!                                                                               !/*rd        183*/
      write(6,'("  Z is ",a9)')hdb(izs)                                         !/*rd        184*/
      write(6,'("The range is from ",1pe15.6," to ",1pe15.6)')                  !/*rd        185*/
     & gdz(lgdz+1),gdz(lgdz+ng2(ngd))                                           !/*rd        186*/
      if(izs.eq.1) then                                                         !/*rd        187*/
         write(6,'("  The Delta-Z is ",1pe15.6)')dy1                            !/*rd        188*/
      endif                                                                     !/*rd        189*/
      write(6,'(/ )')                                                           !/*rd        190*/
!                                                                               !/*rd        191*/
      write(6,'("  Theta is ",a9)')hdb(its)                                     !/*rd        192*/
      t1=0.0                                                                    !/*rd        193*/
      write(6,'("The range is from ",1pe15.6," to ",1pe15.6)')                  !/*rd        194*/
     & t1,gdt(lgdt+ng3(ngd))                                                    !/*rd        195*/
      if(its.eq.1) then                                                         !/*rd        196*/
         write(6,'("  The Delta-Theta is ",1pe15.6)')dz1                        !/*rd        197*/
      endif                                                                     !/*rd        198*/
      write(6,'(/ )')                                                           !/*rd        199*/
!                                                                               !/*rd        200*/
      return                                                                    !/*rd        201*/
!                                                                               !/*rd        202*/
      end                                                                       !/*rd        203*/
