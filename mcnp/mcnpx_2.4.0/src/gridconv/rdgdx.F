!     $Id: rdgdx.F,v 1.8 2002/03/27 18:13:54 gwm Exp $                        !/*rd          1*/
      subroutine rdgdx                                                          !/*rd          2*/
!                                                                               !/*rd          3*/
!       This program is designed to convert rectangular mesh                    !/*rd          4*/
!       tally data output from MCNPX to input files for use                     !/*rd          5*/
!       with one of several graphics packages.                                  !/*rd          6*/
!                                                                               !/*rd          7*/
!       Written by E. C. Snow  11/20/97   (revised: 09/17/2001)                 !/*rd          8*/
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
!       Read into memory the data of choice.                                    !/*rd         23*/
!       First, setup memory offsets for this data.                              !/*rd         24*/
!                                                                               !/*rd         25*/
      lgdx=1                                                                    !/*rd         26*/
      lgdy=lgdx+ng1(ngd)                                                        !/*rd         27*/
      lgdz=lgdy+ng2(ngd)                                                        !/*rd         28*/
      lgdat=lgdz+ng3(ngd)                                                       !/*rd         29*/
      lpdx=lgdat+2*ng1(ngd)*ng2(ngd)*ng3(ngd)                                   !/*rd         30*/
      lpdy=lpdx+ng1(ngd)-1                                                      !/*rd         31*/
      lpdz=lpdy+ng2(ngd)-1                                                      !/*rd         32*/
      lpdat=lpdz+ng3(ngd)-1                                                     !/*rd         33*/
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
         read(30,'(1000(1x,g12.6))')(gdx(lgdx+i),i=1,ng1(ngd))                  !/*rd         62*/
         read(30,'(1000(1x,g12.6))')(gdy(lgdy+i),i=1,ng2(ngd))                  !/*rd         63*/
         read(30,'(1000(1x,g12.6))')(gdz(lgdz+i),i=1,ng3(ngd))                  !/*rd         64*/
      else                                                                      !/*rd         65*/
         read(30)(gdx(lgdx+i),i=1,ng1(ngd))                                     !/*rd         66*/
         read(30)(gdy(lgdy+i),i=1,ng2(ngd))                                     !/*rd         67*/
         read(30)(gdz(lgdz+i),i=1,ng3(ngd))                                     !/*rd         68*/
      endif                                                                     !/*rd         69*/
      do 160 jj=1,mxv                                                           !/*rd         70*/
      if(nxv.eq.jj.and.ngv(ngd,jj).ne.0)go to 170                               !/*rd         71*/
      do 160 kk=1,2                                                             !/*rd         72*/
      do 155 k=1,ng3(ngd)-1                                                     !/*rd         73*/
      do 155 j=1,ng2(ngd)-1                                                     !/*rd         74*/
      if(mfltyp.eq.0) then                                                      !/*rd         75*/
         read(30,'(g13.6)')xdum                                                 !/*rd         76*/
      else                                                                      !/*rd         77*/
         read(30)xdum                                                           !/*rd         78*/
      endif                                                                     !/*rd         79*/
  155 continue                                                                  !/*rd         80*/
  160 continue                                                                  !/*rd         81*/
!                                                                               !/*rd         82*/
  170 do 180 k=1,ng3(ngd)-1                                                     !/*rd         83*/
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
      do 190 k=1,ng3(ngd)-1                                                     !/*rd         94*/
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
         do 192 k=1,ng3(ngd)-1                                                  !/*rd        108*/
            do 192 j=1,ng2(ngd)-1                                               !/*rd        109*/
               mz=(k-1)*(ng1(ngd)-1)*(ng2(ngd)-1)+(j-1)*(ng1(ngd)-1)            !/*rd        110*/
               do 192 i=1,ng1(ngd)-1                                            !/*rd        111*/
                  t1=gdata(lgdat+mz+i)                                          !/*rd        112*/
                  t2=gdata(lgdat+mxgc(ngd)+mz+i)                                !/*rd        113*/
                  if(t1.gt.1.e-35.and.t2.ne.0.) then                            !/*rd        114*/
                     t3=t2/t1**2-fpi                                            !/*rd        115*/
                     if(t3.gt.1.)t3=1.0                                         !/*rd        116*/
                     if(t3.lt.0.)t3=0.0                                         !/*rd        117*/
                     gdata(lgdat+mz+i)=t1*fpi                                   !/*rd        118*/
                     gdata(lgdat+mxgc(ngd)+mz+i)=sqrt(t3)                       !/*rd        119*/
                  else                                                          !/*rd        120*/
                     gdata(lgdat+mz+i)=0.                                       !/*rd        121*/
                     gdata(lgdat+mxgc(ngd)+mz+i)=0.                             !/*rd        122*/
                  endif                                                         !/*rd        123*/
  192    continue                                                               !/*rd        124*/
      endif                                                                     !/*rd        125*/
!                                                                               !/*rd        126*/
!       Set up to write data to IDL type input file.                            !/*rd        127*/
!       First determine if the mesh is regular or irregular.                    !/*rd        128*/
      dx1=abs(gdx(lgdx+2)-gdx(lgdx+1))                                          !/*rd        129*/
      do 210 i=3,ng1(ngd)                                                       !/*rd        130*/
      dx2=abs(gdx(lgdx+i)-gdx(lgdx+i-1))                                        !/*rd        131*/
      dx3=abs(dx1-dx2)                                                          !/*rd        132*/
      if(dx3.gt.0.0001*dx1)go to 220                                            !/*rd        133*/
  210 continue                                                                  !/*rd        134*/
      ixs=1                                                                     !/*rd        135*/
      go to 230                                                                 !/*rd        136*/
!                                                                               !/*rd        137*/
  220 ixs=2                                                                     !/*rd        138*/
!                                                                               !/*rd        139*/
  230 do 240 i=2,ng1(ngd)                                                       !/*rd        140*/
  240 pdx(lpdx+i-1)=(gdx(lgdx+i-1)+gdx(lgdx+i))/2.0                             !/*rd        141*/
!                                                                               !/*rd        142*/
      dy1=abs(gdy(lgdy+2)-gdx(lgdy+1))                                          !/*rd        143*/
      do 250 i=3,ng2(ngd)                                                       !/*rd        144*/
      dy2=abs(gdy(lgdy+i)-gdx(lgdy+i-1))                                        !/*rd        145*/
      dy3=abs(dy1-dy2)                                                          !/*rd        146*/
      if(dy3.gt.0.0001*dy1)go to 260                                            !/*rd        147*/
  250 continue                                                                  !/*rd        148*/
      iys=1                                                                     !/*rd        149*/
      go to 270                                                                 !/*rd        150*/
!                                                                               !/*rd        151*/
  260 iys=2                                                                     !/*rd        152*/
!                                                                               !/*rd        153*/
  270 do 280 i=2,ng2(ngd)                                                       !/*rd        154*/
  280 pdy(lpdy+i-1)=(gdy(lgdy+i-1)+gdy(lgdy+i))/2.0                             !/*rd        155*/
!                                                                               !/*rd        156*/
      dz1=abs(gdz(lgdz+2)-gdz(lgdz+1))                                          !/*rd        157*/
      do 290 i=3,ng3(ngd)                                                       !/*rd        158*/
      dz2=abs(gdz(lgdz+i)-gdz(lgdz+i-1))                                        !/*rd        159*/
      dz3=abs(dz1-dz2)                                                          !/*rd        160*/
      if(dz3.gt.0.0001*dz1)go to 300                                            !/*rd        161*/
  290 continue                                                                  !/*rd        162*/
      izs=1                                                                     !/*rd        163*/
      go to 310                                                                 !/*rd        164*/
!                                                                               !/*rd        165*/
  300 izs=2                                                                     !/*rd        166*/
!                                                                               !/*rd        167*/
  310 do 320 i=2,ng3(ngd)                                                       !/*rd        168*/
  320 pdz(lpdz+i-1)=(gdz(lgdz+i-1)+gdz(lgdz+i))/2.0                             !/*rd        169*/
!                                                                               !/*rd        170*/
      write(6,'("Spacing in the three dimensions are.",/ )')                    !/*rd        171*/
      write(6,'("  X is ",a9)')hdb(ixs)                                         !/*rd        172*/
      write(6,'("The range is from ",1pe15.6," to ",1pe15.6)')                  !/*rd        173*/
     & gdx(lgdx+1),gdx(lgdx+ng1(ngd))                                           !/*rd        174*/
      if(ixs.eq.1) then                                                         !/*rd        175*/
         write(6,'("  The Delta-X is ",1pe15.6)')dx1                            !/*rd        176*/
      endif                                                                     !/*rd        177*/
      write(6,'(/ )')                                                           !/*rd        178*/
      write(6,'("  Y is ",a9)')hdb(iys)                                         !/*rd        179*/
      write(6,'("The range is from ",1pe15.6," to ",1pe15.6)')                  !/*rd        180*/
     & gdy(lgdy+1),gdy(lgdy+ng2(ngd))                                           !/*rd        181*/
      if(iys.eq.1) then                                                         !/*rd        182*/
         write(6,'("  The Delta-Y is ",1pe15.6)')dy1                            !/*rd        183*/
      endif                                                                     !/*rd        184*/
      write(6,'(/ )')                                                           !/*rd        185*/
      write(6,'("  Z is ",a9)')hdb(izs)                                         !/*rd        186*/
      write(6,'("The range is from ",1pe15.6," to ",1pe15.6)')                  !/*rd        187*/
     & gdz(lgdz+1),gdz(lgdz+ng3(ngd))                                           !/*rd        188*/
      if(izs.eq.1) then                                                         !/*rd        189*/
         write(6,'("  The Delta-Z is ",1pe15.6)')dz1                            !/*rd        190*/
      endif                                                                     !/*rd        191*/
      write(6,'(/ )')                                                           !/*rd        192*/
!                                                                               !/*rd        193*/
      return                                                                    !/*rd        194*/
!                                                                               !/*rd        195*/
      end                                                                       !/*rd        196*/
