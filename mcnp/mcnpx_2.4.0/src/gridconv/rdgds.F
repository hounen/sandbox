!     $Id: rdgds.F,v 1.8 2002/03/27 18:13:54 gwm Exp $                        !/*rd          1*/
      subroutine rdgds                                                          !/*rd          2*/
!                                                                               !/*rd          3*/
!       This program is designed to convert polar coord. mesh                   !/*rd          4*/
!       tally data output from MCNPX to input files for use                     !/*rd          5*/
!       with one of several graphics packages.                                  !/*rd          6*/
!       Coordinates are "r" for radius, "p" for polar angle, and                !/*rd          7*/
!       "a" for azimuthal angle.                                                !/*rd          8*/
!                                                                               !/*rd          9*/
!       Written by E. C. Snow  11/22/99   (revised: 11/16/2001)                 !/*rd         10*/
!                                                                               !/*rd         11*/
!       The current package selections are:                                     !/*rd         12*/
!          ng=1   IDL <default>                                                 !/*rd         13*/
!          ng=2   PAW (provided that NO_PAW is not set)                         !/*rd         14*/
!          ng=3   Tecplot                                                       !/*rd         15*/
!          ng=4   Gnuplot                                                       !/*rd         16*/
!                                                                               !/*rd         17*/
#include <gridconv/gcdouble.h>                                                  /*!rd         18*/
#include <gridconv/gccom5.h>                                                    /*!rd         19*/
!                                                                               !/*rd         20*/
      dimension gdr(1),gdp(1),gda(1),gdata(1),pdr(1),pdp(1),                    !/*rd         21*/
     & pda(1),pdata(1)                                                          !/*rd         22*/
      equivalence (das,gdr,gdp,gda,gdata,pdr,pdp,pda,pdata)                     !/*rd         23*/
!                                                                               !/*rd         24*/
!       Read into memory the data of choice.                                    !/*rd         25*/
!       First, setup memory offsets for this data.                              !/*rd         26*/
!                                                                               !/*rd         27*/
      lgdr=1                                                                    !/*rd         28*/
      lgdp=lgdr+ng1(ngd)                                                        !/*rd         29*/
      lgda=lgdp+ng2(ngd)                                                        !/*rd         30*/
      lgdat=lgda+ng3(ngd)                                                       !/*rd         31*/
      lpdr=lgdat+2*ng1(ngd)*ng2(ngd)*ng3(ngd)                                   !/*rd         32*/
      lpdp=lpdr+ng1(ngd)-1                                                      !/*rd         33*/
      lpda=lpdp+ng2(ngd)                                                        !/*rd         34*/
      lpdat=lpda+ng3(ngd)                                                       !/*rd         35*/
!                                                                               !/*rd         36*/
      if(ngd.eq.1)go to 150                                                     !/*rd         37*/
      do 140 ii=1,ngd-1                                                         !/*rd         38*/
      if(mfltyp.eq.0) then                                                      !/*rd         39*/
         read(30,'(g13.6)')xdum                                                 !/*rd         40*/
         read(30,'(g13.6)')xdum                                                 !/*rd         41*/
         read(30,'(g13.6)')xdum                                                 !/*rd         42*/
      else                                                                      !/*rd         43*/
         read(30)xdum                                                           !/*rd         44*/
         read(30)xdum                                                           !/*rd         45*/
         read(30)xdum                                                           !/*rd         46*/
      endif                                                                     !/*rd         47*/
      jt=ngtyp(ii)/10                                                           !/*rd         48*/
      n3=ng3(ii)-1                                                              !/*rd         49*/
      n2=ng2(ii)-1                                                              !/*rd         50*/
      if(jt.eq.3)n2=ng2(ii)                                                     !/*rd         51*/
      if(jt.ne.1)n3=ng3(ii)                                                     !/*rd         52*/
      do 140 jj=1,njt(ii)                                                       !/*rd         53*/
      do 140 kk=1,2                                                             !/*rd         54*/
      do 143 k=1,n3                                                             !/*rd         55*/
      do 143 j=1,n2                                                             !/*rd         56*/
      if(mfltyp.eq.0) then                                                      !/*rd         57*/
         read(30,'(g13.6)')xdum                                                 !/*rd         58*/
      else                                                                      !/*rd         59*/
         read(30)xdum                                                           !/*rd         60*/
      endif                                                                     !/*rd         61*/
  143 continue                                                                  !/*rd         62*/
  140 continue                                                                  !/*rd         63*/
!                                                                               !/*rd         64*/
  150 if(mfltyp.eq.0) then                                                      !/*rd         65*/
         read(30,'(1000(1x,g12.6))')(gdr(lgdr+i),i=1,ng1(ngd))                  !/*rd         66*/
         read(30,'(1000(1x,g12.6))')(gdp(lgdp+i),i=1,ng2(ngd))                  !/*rd         67*/
         read(30,'(1000(1x,g12.6))')(gda(lgda+i),i=1,ng3(ngd))                  !/*rd         68*/
      else                                                                      !/*rd         69*/
         read(30)(gdr(lgdr+i),i=1,ng1(ngd))                                     !/*rd         70*/
         read(30)(gdp(lgdp+i),i=1,ng2(ngd))                                     !/*rd         71*/
         read(30)(gda(lgda+i),i=1,ng3(ngd))                                     !/*rd         72*/
      endif                                                                     !/*rd         73*/
      do 160 jj=1,mxv                                                           !/*rd         74*/
      if(nxv.eq.jj.and.ngv(ngd,jj).ne.0)go to 170                               !/*rd         75*/
      do 160 kk=1,2                                                             !/*rd         76*/
      do 155 k=1,ng3(ngd)                                                       !/*rd         77*/
      do 155 j=1,ng2(ngd)                                                       !/*rd         78*/
      if(mfltyp.eq.0) then                                                      !/*rd         79*/
         read(30,'(g13.6)')xdum                                                 !/*rd         80*/
      else                                                                      !/*rd         81*/
         read(30)xdum                                                           !/*rd         82*/
      endif                                                                     !/*rd         83*/
  155 continue                                                                  !/*rd         84*/
  160 continue                                                                  !/*rd         85*/
!                                                                               !/*rd         86*/
  170 do 180 k=1,ng3(ngd)                                                       !/*rd         87*/
      do 180 j=1,ng2(ngd)                                                       !/*rd         88*/
      mz=(k-1)*(ng1(ngd)-1)*ng2(ngd)+(j-1)*(ng1(ngd)-1)                         !/*rd         89*/
      if(mfltyp.eq.0) then                                                      !/*rd         90*/
         read(30,'(1000(1x,g12.6))')                                            !/*rd         91*/
     &    (gdata(lgdat+mz+i),i=1,ng1(ngd)-1)                                    !/*rd         92*/
      else                                                                      !/*rd         93*/
         read(30)(gdata(lgdat+mz+i),i=1,ng1(ngd)-1)                             !/*rd         94*/
      endif                                                                     !/*rd         95*/
  180 continue                                                                  !/*rd         96*/
!                                                                               !/*rd         97*/
      do 190 k=1,ng3(ngd)                                                       !/*rd         98*/
      do 190 j=1,ng2(ngd)                                                       !/*rd         99*/
      mz=(k-1)*(ng1(ngd)-1)*ng2(ngd)+(j-1)*(ng1(ngd)-1)                         !/*rd        100*/
      if(mfltyp.eq.0) then                                                      !/*rd        101*/
         read(30,'(1000(1x,g12.6))')                                            !/*rd        102*/
     &    (gdata(lgdat+mxgc(ngd)+mz+i),i=1,ng1(ngd)-1)                          !/*rd        103*/
      else                                                                      !/*rd        104*/
         read(30)(gdata(lgdat+mxgc(ngd)+mz+i),i=1,ng1(ngd)-1)                   !/*rd        105*/
      endif                                                                     !/*rd        106*/
  190 continue                                                                  !/*rd        107*/
!                                                                               !/*rd        108*/
!       Normalize and calculate error if this is raw data.                      !/*rd        109*/
      if (mfltyp.ne.0) then                                                     !/*rd        110*/
         fpi=1./nps                                                             !/*rd        111*/
         do 192 k=1,ng3(ngd)                                                    !/*rd        112*/
            do 192 j=1,ng2(ngd)                                                 !/*rd        113*/
               mz=(k-1)*(ng1(ngd)-1)*ng2(ngd)+(j-1)*(ng1(ngd)-1)                !/*rd        114*/
               do 192 i=1,ng1(ngd)-1                                            !/*rd        115*/
                  t1=gdata(lgdat+mz+i)                                          !/*rd        116*/
                  t2=gdata(lgdat+mxgc(ngd)+mz+i)                                !/*rd        117*/
                  if(t1.gt.1.e-35.and.t2.ne.0.) then                            !/*rd        118*/
                     t3=t2/t1**2-fpi                                            !/*rd        119*/
                     if(t3.gt.1.0)t3=1.0                                        !/*rd        120*/
                     if(t3.lt.0.0)t3=0.0                                        !/*rd        121*/
                     gdata(lgdat+mz+i)=t1*fpi                                   !/*rd        122*/
                     gdata(lgdat+mxgc(ngd)+mz+i)=sqrt(t3)                       !/*rd        123*/
                  else                                                          !/*rd        124*/
                     gdata(lgdat+mz+i)=0.                                       !/*rd        125*/
                     gdata(lgdat+mxgc(ngd)+mz+i)=0.                             !/*rd        126*/
                  endif                                                         !/*rd        127*/
  192    continue                                                               !/*rd        128*/
      endif                                                                     !/*rd        129*/
!                                                                               !/*rd        130*/
!       Set up to write data to graphics input file.                            !/*rd        131*/
!       First determine if the mesh is regular or irregular.                    !/*rd        132*/
      dx1=abs(gdr(lgdr+2)-gdr(lgdr+1))                                          !/*rd        133*/
      dx4=0.01*dx1                                                              !/*rd        134*/
      do 210 i=3,ng1(ngd)                                                       !/*rd        135*/
      dx2=abs(gdr(lgdr+i)-gdr(lgdr+i-1))                                        !/*rd        136*/
      dx3=abs(dx1-dx2)                                                          !/*rd        137*/
      if(dx3.gt.dx4)go to 220                                                   !/*rd        138*/
  210 continue                                                                  !/*rd        139*/
      irs=1                                                                     !/*rd        140*/
      go to 230                                                                 !/*rd        141*/
!                                                                               !/*rd        142*/
  220 irs=2                                                                     !/*rd        143*/
!                                                                               !/*rd        144*/
  230 do 240 i=2,ng1(ngd)                                                       !/*rd        145*/
  240 pdr(lpdr+i-1)=(gdr(lgdr+i-1)+gdr(lgdr+i))/2.0                             !/*rd        146*/
!                                                                               !/*rd        147*/
      dy1=gdp(lgdp+1)                                                           !/*rd        148*/
      dy4=0.01*dy1                                                              !/*rd        149*/
      do 250 i=2,ng2(ngd)                                                       !/*rd        150*/
      dy2=abs(gdp(lgdp+i)-gdp(lgdp+i-1))                                        !/*rd        151*/
      dy3=abs(dy1-dy2)                                                          !/*rd        152*/
      if(dy3.gt.dy4)go to 260                                                   !/*rd        153*/
  250 continue                                                                  !/*rd        154*/
      izs=1                                                                     !/*rd        155*/
      go to 270                                                                 !/*rd        156*/
!                                                                               !/*rd        157*/
  260 izs=2                                                                     !/*rd        158*/
!                                                                               !/*rd        159*/
  270 pdp(lpdp+1)=gdp(lgdp+1)/2.0                                               !/*rd        160*/
      do 280 i=2,ng2(ngd)                                                       !/*rd        161*/
  280 pdp(lpdp+i)=(gdp(lgdp+i-1)+gdp(lgdp+i))/2.0                               !/*rd        162*/
!                                                                               !/*rd        163*/
      dz1=gda(lgda+1)                                                           !/*rd        164*/
      dz4=0.01*dz1                                                              !/*rd        165*/
      do 290 i=2,ng3(ngd)                                                       !/*rd        166*/
      dz2=abs(gda(lgda+i)-gda(lgda+i-1))                                        !/*rd        167*/
      dz3=abs(dz1-dz2)                                                          !/*rd        168*/
      if(dz3.gt.dz4)go to 300                                                   !/*rd        169*/
  290 continue                                                                  !/*rd        170*/
      its=1                                                                     !/*rd        171*/
      go to 310                                                                 !/*rd        172*/
!                                                                               !/*rd        173*/
  300 its=2                                                                     !/*rd        174*/
!                                                                               !/*rd        175*/
  310 pda(lpda+1)=gda(lgda+1)/2.0                                               !/*rd        176*/
      do 320 i=2,ng3(ngd)                                                       !/*rd        177*/
  320 pda(lpda+i)=(gda(lgda+i-1)+gda(lgda+i))/2.0                               !/*rd        178*/
!                                                                               !/*rd        179*/
      write(6,'("Spacing in the three dimensions are./ ")')                     !/*rd        180*/
      write(6,'("  R is ",a9)')hdb(irs)                                         !/*rd        181*/
      write(6,'("The range is from ",1pe15.6," to ",1pe15.6)')                  !/*rd        182*/
     & gdr(lgdr+1),gdr(lgdr+ng1(ngd))                                           !/*rd        183*/
      if(irs.eq.1) then                                                         !/*rd        184*/
         write(6,'("  The Delta-R is ",1pe15.6)')dx1                            !/*rd        185*/
      endif                                                                     !/*rd        186*/
      write(6,'(/ )')                                                           !/*rd        187*/
!                                                                               !/*rd        188*/
      write(6,'("  Polar Angle is ",a9)')hdb(izs)                               !/*rd        189*/
      t1=0.0                                                                    !/*rd        190*/
      write(6,'("The range is from ",1pe15.6," to ",1pe15.6)')                  !/*rd        191*/
     & t1,gdp(lgdp+ng2(ngd))                                                    !/*rd        192*/
      if(izs.eq.1) then                                                         !/*rd        193*/
         write(6,'("  The Delta-P is ",1pe15.6)')dy1                            !/*rd        194*/
      endif                                                                     !/*rd        195*/
      write(6,'(/ )')                                                           !/*rd        196*/
!                                                                               !/*rd        197*/
      write(6,'("  Azimuthal Angle is ",a9)')hdb(its)                           !/*rd        198*/
      write(6,'("The range is from ",1pe15.6," to ",1pe15.6)')                  !/*rd        199*/
     & t1,gda(lgda+ng3(ngd))                                                    !/*rd        200*/
      if(its.eq.1) then                                                         !/*rd        201*/
         write(6,'("  The Delta-A is ",1pe15.6)')dz1                            !/*rd        202*/
      endif                                                                     !/*rd        203*/
      write(6,'(/ )')                                                           !/*rd        204*/
!                                                                               !/*rd        205*/
      return                                                                    !/*rd        206*/
!                                                                               !/*rd        207*/
      end                                                                       !/*rd        208*/
