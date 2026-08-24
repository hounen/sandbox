!     $Id: gdtal.F,v 1.20 2002/07/31 22:40:05 jxh Exp $                         !/*gd          1*/
      subroutine gdtal(mm,da,zd,jz)                                             !/*gd          2*/
#ifdef MESHTAL                                                                  /*!gd          3*/
!                                                                               !/*gd          4*/
!       This subroutine is designed to score a volume tally like                !/*gd          5*/
!       quantity in a three dimensional rectangular mesh for later              !/*gd          6*/
!       post processing with any number of graphics packages.                   !/*gd          7*/
!       Written by Ed Snow 11/20/97.  <revised 11/16/2001>                      !/*gd          8*/
!                                                                               !/*gd          9*/
!       mm=0   Open file mdata and write header and fixed data,                 !/*gd         10*/
!              and do some final setup.                                         !/*gd         11*/
!       mm=1   Score source or banked particles relative to mesh.               !/*gd         12*/
!       mm=2   Track particle a distance 'da' and score tally info.             !/*gd         13*/
!       mm=3   Gather tally data for this history like in 'talshf'.             !/*gd         14*/
!       mm=4   Calculate error and write tally and error to mdata               !/*gd         15*/
!              and close file.                                                  !/*gd         16*/
!       mm=5   Point detector contribution.                                     !/*gd         17*/
!       mm=6   Dxtran contribution.                                             !/*gd         18*/
!       mm=7   Continue run setup.                                              !/*gd         19*/
!       mm=8   Score energy locally.                                            !/*gd         20*/
!       mm=9   not currently used.                                              !/*gd         21*/
!       mm=10  Define lemsh offsets.                                            !/*gd         22*/
!       mm=11  Define all other meshtal offsets.                                !/*gd         23*/
!       mm=12  Not used.                                                        !/*gdxk        1*/
!       mm=13  Define ktgd(maxg) for each tasks.                                !/*gdxk        2*/
!       mm=14  Zero out gdata(ktgd...) array.                                   !/*gdxk        3*/
!       mm=15  Gather gdata(maxg) for all tasks.                                !/*gdxk        4*/
!       mm=16  Master, send initialized data.                                   !/*gdxk        5*/
!       mm=17  Subtask, receive initialized data.                               !/*gdxk        6*/
!       mm=18  Subtask, send gdata(ktgd...) array.                              !/*gdxk        7*/
!       mm=19  Master, receive gdata(ktgd...) array.                            !/*gdxk        8*/
!                                                                               !/*gd         24*/
      use global_data                                                           !/*gdxc        1*/
      use message_passing                                                       !/*gdxk        9*/
!                                                                               !/*gdxc        2*/
      implicit real(kindr) (a-h,o-z)                                            !/*gdxc        3*/
      implicit integer(kindi) (i-n)                                             !/*gdxc        4*/
!                                                                               !/*gdxc        5*/
!                                                                               !/*gd         27*/
      common /meshtal/ ergl(maxg),ergh(maxg),rdos(5,maxg),                      !/*gd         28*/
     & rmult(maxg),mdos(3,5,maxg),mmult(3,maxg),nemsh(maxg),                    !/*gd         29*/
     & mshtr(maxg),mstyp(maxg),mshpt(maxg),ng1(maxg),ng2(maxg),                 !/*gd         30*/
     & ng3(maxg),mxgt(maxg),ngv(maxg,maxgv),mxgc(maxg),mxgv(maxg)               !/*gd         31*/
!                                                                               !/*gd         32*/
      dimension zd(10),jz(5)                                                    !/*gd         33*/
!                                                                               !/*gd         34*/
      character ax*80,kd*8,vr*5,ld*28                                           !/*gd         35*/
!                                                                               !/*gdxk       10*/
      parameter(nf=8*maxg)                                                      !/*gdxk       11*/
      parameter(lf=(28+maxgv)*maxg)                                             !/*gdxk       12*/
      dimension gmeshtal(nf), lmeshtal(lf)                                      !/*gdxk       13*/
      equivalence (ergl,gmeshtal), (mdos,lmeshtal)                              !/*gdxk       14*/
!                                                                               !/*gd         36*/
!       Transfer according to 'mm+1'.                                           !/*gd         37*/
      go to (10,200,200,300,400,200,200,100,200,2000,                           !/*gdxk       15*/
     & 800,900,2000,1020,1040,1060                                              !/*gdxk       16*/
#ifdef MULTP                                                                    /*!gdxk       17*/
     & ,1080,1100,1120,1140                                                     !/*gdxk       18*/
#endif                                                                          /*!gdxk       19*/
     & ) mm+1                                                                   !/*gdxk       20*/
!                                                                               !/*gdxk       21*/
!       Open MDATA and write header and some fixed data.          (mm=0)        !/*gdxk       22*/
   10 call unique(mdata,jtty)                                                   !/*gdxk       23*/
!     TEMPORARY expedient.  MCT is being used to indicate that                  !/*gd         45*/
!                           the problem is a test problem only.                 !/*gd         46*/
      if (mct.lt.0) then                                                        !/*gd         47*/
         open(ims,file=mdata,form='formatted',status='new')                     !/*gd         48*/
      else                                                                      !/*gd         49*/
         open(ims,file=mdata,form='unformatted',status='new')                   !/*gd         50*/
      endif                                                                     !/*gd         51*/
!                                                                               !/*gd         66*/
!       Be sure that mesh tally numbers are unique.                             !/*gd         67*/
      do 50 i=1,nmesh                                                           !/*gd         68*/
      im=nugd(i)                                                                !/*gd         69*/
      do 30 j=1,ntal                                                            !/*gd         70*/
      jm=jptal(ljpt+1,j)                                                        !/*gd         71*/
      if(im.eq.jm) then                                                         !/*gd         72*/
         call erprnt(1,1,1,im,0,0,0,0,                                          !/*gd         73*/
     &    '16h Mesh tally no. ,i5,15h is not unique!')                          !/*gd         74*/
      endif                                                                     !/*gd         75*/
   30 continue                                                                  !/*gd         76*/
      do 40 k=i+1,nmesh                                                         !/*gd         77*/
      km=nugd(k)                                                                !/*gd         78*/
      if(i.eq.nmesh)km=0                                                        !/*gd         79*/
      if(im.eq.km) then                                                         !/*gd         80*/
         call erprnt(1,1,1,im,0,0,0,0,                                          !/*gd         81*/
     &    '16h Mesh tally no. ,i5,15h is not unique!')                          !/*gd         82*/
      endif                                                                     !/*gd         83*/
   40 continue                                                                  !/*gd         84*/
   50 continue                                                                  !/*gd         85*/
!                                                                               !/*gd         86*/
!       Set up proper tr numbers for each mesh.                                 !/*gd         87*/
      do 70 ig=1,nmesh                                                          !/*gd         88*/
      if(mshtr(ig).eq.0) go to 70                                               !/*gd         89*/
      do 60 kt=1,mxtr                                                           !/*gd         90*/
      n=int(abs(trf(ltrf+1,kt)))                                                !/*gd         91*/
      if(mshtr(ig).eq.n) then                                                   !/*gd         92*/
         mshtr(ig)=kt                                                           !/*gd         93*/
         go to 70                                                               !/*gd         94*/
      endif                                                                     !/*gd         95*/
   60 continue                                                                  !/*gd         96*/
      call erprnt(1,1,1,mshtr(ig),0,0,0,0,                                      !/*gd         97*/
     & '35h TR card is missing for mesh tally ,i5')                             !/*gd         98*/
   70 continue                                                                  !/*gd         99*/
!                                                                               !/*gd        100*/
      return                                                                    !/*gd        101*/
!                                                                               !/*gd        102*/
!       Set up data for a continue run by reading old information               !/*gd        103*/
!       into proper storage locations.                                          !/*gd        104*/
  100 if(inqire(mdata).eq.0)call expire(0,'gdtal',                              !/*gd        105*/
     & 'file '//mdata//' dose not exist.')                                      !/*gd        106*/
      open(ims,file=mdata,form='unformatted',status='old')                      !/*gd        107*/
      ax=aid(1:52)//loddat                                                      !/*gd        108*/
      read(ims)ax                                                               !/*gd        109*/
      read(ims)ns,kd,vr,ld,np                                                   !/*gd        110*/
      read(ims)(nemsh(i),i=1,maxg)                                              !/*gd        111*/
      do 120 i=1,ns                                                             !/*gd        112*/
      read(ims)mstyp(i),mshpt(i),ng1(i),ng2(i),ng3(i),                          !/*gd        113*/
     & mxgv(i),mxgc(i),nugd(i),mxgt(i),(ngv(i,j),j=1,maxgv)                     !/*gd        114*/
      read(ims)ergl(i),ergh(i),mshtr(i)                                         !/*gd        115*/
      read(ims)((mdos(k,l,i),l=1,5),k=1,3),(rdos(j,i),j=1,5)                    !/*gd        116*/
      read(ims)(mmult(k,i),k=1,3),rmult(i)                                      !/*gd        117*/
  120 continue                                                                  !/*gd        118*/
!                                                                               !/*gd        119*/
!       Be sure this 'gdata' file is compatible with the current                !/*gdxc        6*/
!       dump on the runtpe.                                                     !/*gd        121*/
      if(vr.ne.ver)call erprnt(1,2,0,0,0,0,0,0,                                 !/*gd        122*/
     & '33h DUMP1 written with version '//vr)                                   !/*gd        123*/
      if(np.ne.nps) then                                                        !/*gd        124*/
         call erprnt(1,1,2,np,nps,0,0,0,                                        !/*gd        125*/
     &    '14h Runtpe NPS = ,i9,13h Dump1 NPS = ,i9')                           !/*gd        126*/
         return                                                                 !/*gd        127*/
      endif                                                                     !/*gd        128*/
      if(ns.ne.nmesh) then                                                      !/*gd        129*/
         call erprnt(1,1,2,nmesh,ns,0,0,0,                                      !/*gd        130*/
     &    '16h Runtpe nmesh = ,i9,15h Dump1 nmesh = ,i9')                       !/*gd        131*/
         return                                                                 !/*gd        132*/
      endif                                                                     !/*gd        133*/
!                                                                               !/*gd        134*/
      do 150 ig=1,nmesh                                                         !/*gd        135*/
      read(ims)(gd1(lgd1(ig)+i),i=1,ng1(ig))                                    !/*gd        136*/
      read(ims)(gd2(lgd2(ig)+i),i=1,ng2(ig))                                    !/*gd        137*/
      read(ims)(gd3(lgd3(ig)+i),i=1,ng3(ig))                                    !/*gd        138*/
!                                                                               !/*gd        139*/
      l1=ltgd(ig)                                                               !/*gd        140*/
      l2=l1+mxgt(ig)                                                            !/*gd        141*/
      l3=l2+mxgt(ig)                                                            !/*gd        142*/
      nz=mod(mstyp(ig),10)                                                      !/*gd        143*/
      nt=(mstyp(ig)-nz)/10                                                      !/*gd        144*/
      do 150 iv=1,mxgv(ig)                                                      !/*gd        145*/
      mx=(iv-1)*mxgc(ig)                                                        !/*gd        146*/
      n2=ng2(ig)-1                                                              !/*gd        147*/
      if(nt.eq.3)n2=ng2(ig)                                                     !/*gd        148*/
      n3=ng3(ig)-1                                                              !/*gd        149*/
      if(nt.eq.2.or.nt.eq.3)n3=ng3(ig)                                          !/*gd        150*/
      do 130 k=1,n3                                                             !/*gd        151*/
      do 130 j=1,n2                                                             !/*gd        152*/
      mz=(k-1)*(ng1(ig)-1)*n2+(j-1)*(ng1(ig)-1)                                 !/*gd        153*/
      read(ims)(gdata(l2+mx+mz+i),i=1,ng1(ig)-1)                                !/*gd        154*/
  130 continue                                                                  !/*gd        155*/
!                                                                               !/*gd        156*/
      do 140 k=1,n3                                                             !/*gd        157*/
      do 140 j=1,n2                                                             !/*gd        158*/
      mz=(k-1)*(ng1(ig)-1)*n2+(j-1)*(ng1(ig)-1)                                 !/*gd        159*/
      read(ims)(gdata(l3+mx+mz+i),i=1,ng1(ig)-1)                                !/*gd        160*/
  140 continue                                                                  !/*gd        161*/
!                                                                               !/*gd        162*/
  150 continue                                                                  !/*gd        163*/
!                                                                               !/*gd        164*/
!                                                                               !/*gd        165*/
      return                                                                    !/*gd        166*/
!                                                                               !/*gd        167*/
!       loop over all meshs and determine the location of starting point        !/*gd        168*/
!       for source or banked particle in each mesh for the proper               !/*gd        169*/
!       particle type.                                    (mm=1,2,5,6,8)        !/*gdxk       24*/
!                                                                               !/*gd        171*/
  200 continue                                                                  !/*gd        172*/
!     Return if kcode problem is not settled.                                   !/*gd        173*/
      if(kc8.gt.0)return                                                        !/*gd        174*/
!                                                                               !/*gd        175*/
      do 210 ig=1,nmesh                                                         !/*gd        176*/
      nt=(mstyp(ig)-mod(mstyp(ig),10))/10                                       !/*gd        177*/
      if(nt.eq.1)call gd1tal(mm,da,ig,zd,jz)                                    !/*gd        178*/
      if(nt.eq.2)call gd2tal(mm,da,ig,zd,jz)                                    !/*gd        179*/
      if(nt.eq.3)call gd3tal(mm,da,ig,zd,jz)                                    !/*gd        180*/
  210 continue                                                                  !/*gd        181*/
      return                                                                    !/*gd        182*/
!                                                                               !/*gd        183*/
!       Gather data for this history and store allong with data**2              !/*gd        184*/
!       for use with error estimate.                              (mm=3)        !/*gdxk       25*/
!                                                                               !/*gd        186*/
  300 continue                                                                  !/*gd        187*/
!     Return if kcode problem is not settled.                                   !/*gd        188*/
      if(kc8.gt.0)return                                                        !/*gd        189*/
!                                                                               !/*gd        190*/
      do 310 ig=1,nmesh                                                         !/*gd        191*/
      l1=ktgd(ig)                                                               !/*gdxk       26*/
      l2=l1+mxgt(ig)                                                            !/*gd        193*/
      l3=l2+mxgt(ig)                                                            !/*gd        194*/
      do 310 j=1,mxgv(ig)                                                       !/*gd        195*/
      mx=(j-1)*mxgc(ig)                                                         !/*gd        196*/
      do 310 k=1,mxgc(ig)                                                       !/*gd        197*/
      gdata(l2+mx+k)=gdata(l2+mx+k)+gdata(l1+mx+k)                              !/*gd        198*/
      gdata(l3+mx+k)=gdata(l3+mx+k)+gdata(l1+mx+k)**2                           !/*gd        199*/
      gdata(l1+mx+k)=0.                                                         !/*gd        200*/
  310 continue                                                                  !/*gd        201*/
      return                                                                    !/*gd        202*/
!                                                                               !/*gd        203*/
!       Write header information and fixed data to MDATA                        !/*gd        204*/
!       Write tally and error to MDATA and close file.            (mm=4)        !/*gdxk       27*/
!                                                                               !/*gd        206*/
  400 rewind ims                                                                !/*gd        207*/
!                                                                               !/*gd        216*/
      if (mct.lt.0) then                                                        !/*gd        217*/
         ax=aid(1:60)                                                           !/*gd        218*/
         write(ims,'(a60)')ax                                                   !/*gd        219*/
         write(ims,'(i2,47x,i10)')nmesh,nps                                     !/*gdxl        1*/
         write(ims,'(1x,i5)')nemsh(1)                                           !/*gd        221*/
         do 410 i=1,nmesh                                                       !/*gd        222*/
         if(mod(mstyp(i),10).ge.3)ngv(i,1)=1                                    !/*gd        223*/
         write(ims,'(1000(1x,i5))')mstyp(i),mshpt(i),ng1(i),ng2(i),             !/*gd        224*/
     &    ng3(i),mxgv(i),mxgc(i),nugd(i),mxgt(i),(ngv(i,j),j=1,maxgv)           !/*gd        225*/
         write(ims,'(2(1x,g12.6))')ergl(i),ergh(i)                              !/*gd        226*/
         write(ims,'(1x,i5)')mdos(1,1,1)                                        !/*gd        227*/
         write(ims,'(1x,i5)')mmult(1,1)                                         !/*gd        228*/
  410    continue                                                               !/*gd        229*/
      else                                                                      !/*gd        230*/
         ax=aid(1:60)//' '//probid                                              !/*gd        231*/
         write(ims)ax                                                           !/*gd        232*/
         write(ims)nmesh,kod,ver,loddat,nps                                     !/*gdxl        2*/
         write(ims)(nemsh(i),i=1,maxg)                                          !/*gd        234*/
         do 420 i=1,nmesh                                                       !/*gd        235*/
         if(mod(mstyp(i),10).ge.3)ngv(i,1)=1                                    !/*gd        236*/
         write(ims)mstyp(i),mshpt(i),ng1(i),ng2(i),ng3(i),                      !/*gd        237*/
     &    mxgv(i),mxgc(i),nugd(i),mxgt(i),(ngv(i,j),j=1,maxgv)                  !/*gd        238*/
         write(ims)ergl(i),ergh(i),mshtr(i)                                     !/*gd        239*/
         write(ims)((mdos(k,l,i),l=1,5),k=1,3),(rdos(j,i),j=1,5)                !/*gd        240*/
         write(ims)(mmult(k,i),k=1,3),rmult(i)                                  !/*gd        241*/
  420    continue                                                               !/*gd        242*/
      endif                                                                     !/*gd        243*/
!                                                                               !/*gd        244*/
      do 450 ig=1,nmesh                                                         !/*gd        245*/
      if (mct.lt.0) then                                                        !/*gd        246*/
         write(ims,'(1000(1x,g12.6))')(gd1(lgd1(ig)+i),                         !/*gd        247*/
     &    i=1,ng1(ig))                                                          !/*gd        248*/
         write(ims,'(1000(1x,g12.6))')(gd2(lgd2(ig)+i),                         !/*gd        249*/
     &    i=1,ng2(ig))                                                          !/*gd        250*/
         write(ims,'(1000(1x,g12.6))')(gd3(lgd3(ig)+i),                         !/*gd        251*/
     &    i=1,ng3(ig))                                                          !/*gd        252*/
      else                                                                      !/*gd        253*/
         write(ims)(gd1(lgd1(ig)+i),i=1,ng1(ig))                                !/*gd        254*/
         write(ims)(gd2(lgd2(ig)+i),i=1,ng2(ig))                                !/*gd        255*/
         write(ims)(gd3(lgd3(ig)+i),i=1,ng3(ig))                                !/*gd        256*/
      endif                                                                     !/*gd        257*/
!                                                                               !/*gd        258*/
      l1=ltgd(ig)                                                               !/*gd        259*/
      l2=l1+mxgt(ig)                                                            !/*gd        260*/
      l3=l2+mxgt(ig)                                                            !/*gd        261*/
      nz=mod(mstyp(ig),10)                                                      !/*gd        262*/
      nt=(mstyp(ig)-nz)/10                                                      !/*gd        263*/
      do 450 iv=1,mxgv(ig)                                                      !/*gd        264*/
      mx=(iv-1)*mxgc(ig)                                                        !/*gd        265*/
      if (mct.lt.0) then                                                        !/*gd        266*/
         do 425 kc=1,mxgc(ig)                                                   !/*gd        268*/
         t1=gdata(l2+mx+kc)                                                     !/*gd        269*/
         t2=gdata(l3+mx+kc)                                                     !/*gd        270*/
         if(t1.gt.1.e-35.and.t2.ne.0.) then                                     !/*gd        271*/
            gdata(l1+mx+kc)=sqrt(max(zero,min(t2/t1**2-fpi,one)))               !/*gdxl        3*/
            gdata(l2+mx+kc)=t1*fpi                                              !/*gdxl        4*/
         else                                                                   !/*gd        274*/
            gdata(l1+mx+kc)=0.                                                  !/*gd        275*/
            gdata(l2+mx+kc)=0.                                                  !/*gd        276*/
         endif                                                                  !/*gd        277*/
  425    continue                                                               !/*gd        278*/
      endif                                                                     !/*gd        279*/
!                                                                               !/*gd        280*/
      n2=ng2(ig)-1                                                              !/*gd        281*/
      if(nt.eq.3)n2=ng2(ig)                                                     !/*gd        282*/
      n3=ng3(ig)-1                                                              !/*gd        283*/
      if(nt.eq.2.or.nt.eq.3)n3=ng3(ig)                                          !/*gd        284*/
      do 430 k=1,n3                                                             !/*gd        285*/
      do 430 j=1,n2                                                             !/*gd        286*/
      mz=(k-1)*(ng1(ig)-1)*n2+(j-1)*(ng1(ig)-1)                                 !/*gd        287*/
      if (mct.lt.0) then                                                        !/*gd        288*/
         write(ims,'(1000(1x,1pe12.6))')(gdata(l2+mx+mz+i),                     !/*gd        289*/
     &    i=1,ng1(ig)-1)                                                        !/*gd        290*/
      else                                                                      !/*gd        291*/
         write(ims)(gdata(l2+mx+mz+i),i=1,ng1(ig)-1)                            !/*gd        292*/
      endif                                                                     !/*gd        293*/
  430 continue                                                                  !/*gd        294*/
!                                                                               !/*gd        295*/
      do 440 k=1,n3                                                             !/*gd        296*/
      do 440 j=1,n2                                                             !/*gd        297*/
      mz=(k-1)*(ng1(ig)-1)*n2+(j-1)*(ng1(ig)-1)                                 !/*gd        298*/
      if (mct.lt.0) then                                                        !/*gd        299*/
         write(ims,'(1000(5x,f8.6))')(gdata(l1+mx+mz+i),                        !/*gd        300*/
     &    i=1,ng1(ig)-1)                                                        !/*gd        301*/
      else                                                                      !/*gd        302*/
         write(ims)(gdata(l3+mx+mz+i),i=1,ng1(ig)-1)                            !/*gd        303*/
      endif                                                                     !/*gd        304*/
  440 continue                                                                  !/*gd        305*/
!                                                                               !/*gd        306*/
  450 continue                                                                  !/*gd        307*/
      return                                                                    !/*gd        308*/
!                                                                               !/*gd        309*/
!        Set up lemsh(i) offsets.                                (mm=10)        !/*gdxk       28*/
  800 lemsh(1)=0                                                                !/*gda         1*/
      do 810 i=2,maxg                                                           !/*gd        312*/
  810 lemsh(i)=lemsh(i-1)+2*nemsh(i-1)                                          !/*gd        313*/
      nemsh1=lemsh(maxg)+2*nemsh(maxg)                                          !/*gda         2*/
      return                                                                    !/*gd        315*/
!                                                                               !/*gd        316*/
!        Set up the rest of the mesh tally offsets.              (mm=11)        !/*gdxk       29*/
  900 do 910 i=1,nmesh                                                          !/*gd        318*/
      if(mxgv(i).gt.0) then                                                     !/*gd        319*/
         n=mstyp(i)/10                                                          !/*gd        320*/
         n2=ng2(i)-1                                                            !/*gd        321*/
         if(n.eq.3)n2=ng2(i)                                                    !/*gd        322*/
         n3=ng3(i)-1                                                            !/*gd        323*/
         if(n.eq.2.or.n.eq.3)n3=ng3(i)                                          !/*gd        324*/
         mxgc(i)=(ng1(i)-1)*n2*n3                                               !/*gd        325*/
         if(mxgc(i).le.0)call erprnt(1,1,1,nugd(i),0,0,0,0,                     !/*gd        326*/
     &   '6h Mesh ,i3,26h has improper coord. data.')                           !/*gd        327*/
      endif                                                                     !/*gd        328*/
      mxgt(i)=mxgv(i)*mxgc(i)                                                   !/*gdxk       30*/
  910 continue                                                                  !/*gd        329*/
!                                                                               !/*gd        330*/
      ltgd(1)=0                                                                 !/*gda         3*/
      mt=1                                                                      !/*gdxk       31*/
#if defined(MULTT) || defined(MULTP)                                            /*!gdxk       32*/
      mt=mtasks+1                                                               !/*gdxk       33*/
#endif                                                                          /*!gdxk       34*/
      do 920 i=2,nmesh                                                          !/*gd        332*/
  920 ltgd(i)=ltgd(i-1)+mxgt(i-1)*3*mt                                          !/*gdxk       35*/
      ngdt=ltgd(nmesh)+mxgt(nmesh)*3*mt                                         !/*gdxk       36*/
      lgd1(1)=0                                                                 !/*gda         5*/
      do 930 i=2,nmesh                                                          !/*gd        335*/
  930 lgd1(i)=lgd1(i-1)+ng1(i-1)                                                !/*gd        336*/
      ngd1=lgd1(nmesh)+ng1(nmesh)                                               !/*gda         6*/
      lgd2(1)=0                                                                 !/*gda         7*/
      do 940 i=2,nmesh                                                          !/*gd        338*/
  940 lgd2(i)=lgd2(i-1)+ng2(i-1)                                                !/*gd        339*/
      ngd2=lgd2(nmesh)+ng2(nmesh)                                               !/*gda         8*/
      lgd3(1)=0                                                                 !/*gda         9*/
      do 950 i=2,nmesh                                                          !/*gd        341*/
  950 lgd3(i)=lgd3(i-1)+ng3(i-1)                                                !/*gd        342*/
      ngd3=lgd3(nmesh)+ng3(nmesh)                                               !/*gda        10*/
      return                                                                    !/*gdxk       37*/
!                                                                               !/*gdxk       38*/
!     Setup ktgd offsets for mtask multitask run.                (mm=13)        !/*gdxk       39*/
 1020 do 1030 i=1,nmesh                                                         !/*gdxk       40*/
      ktgd(i)=ltgd(i)+(ktask+1)*mxgt(i)*3                                       !/*gdxk       41*/
 1030 continue                                                                  !/*gdxk       42*/
      return                                                                    !/*gdxk       43*/
!                                                                               !/*gdxk       44*/
!        Zero out all of the gdata array.                        (mm=14)        !/*gdxk       45*/
 1040 do 1050 ig=1,nmesh                                                        !/*gdxk       46*/
      l1=ktgd(ig)                                                               !/*gdxk       47*/
      mx=3*mxgt(ig)                                                             !/*gdxk       48*/
      do 1050 k=1,mx                                                            !/*gdxk       49*/
 1050 gdata(l1+k)=0.                                                            !/*gdxk       50*/
      return                                                                    !/*gdxk       51*/
!                                                                               !/*gdxk       52*/
!        Gather task gdata scores to main gdata array.           (mm=15)        !/*gdxk       53*/
!        Only 2nd and 3rd block are needed.                                     !/*gdxk       54*/
 1060 do 1070 ig=1,nmesh                                                        !/*gdxk       55*/
      l1=ltgd(ig)+mxgt(ig)                                                      !/*gdxk       56*/
      l2=ktgd(ig)+mxgt(ig)                                                      !/*gdxk       57*/
      mx=mxgt(ig)*2                                                             !/*gdxk       58*/
      do 1070 j=1,mx                                                            !/*gdxk       59*/
      gdata(l1+j)=gdata(l1+j)+gdata(l2+j)                                       !/*gdxk       60*/
      gdata(l2+j)=0.                                                            !/*gdxk       61*/
 1070 continue                                                                  !/*gdxk       62*/
      return                                                                    !/*gdxk       63*/
#ifdef MULTP                                                                    /*!gdxk       64*/
!                                                                               !/*gdxk       65*/
!       master send mesh tally initialization data.              (mm=16)        !/*gdxk       66*/
 1080 call msndi                                                                !/*gdxk       67*/
      call mputd(gmeshtal,nf,i)                                                 !/*gdxk       68*/
      call mputi(lmeshtal,lf,i)                                                 !/*gdxk       69*/
      do 1090 ig=1,nmesh                                                        !/*gdxk       70*/
         call mputd(gd1(lgd1(ig)+1),ng1(ig),i)                                  !/*gdxk       71*/
         call mputd(gd2(lgd2(ig)+1),ng2(ig),i)                                  !/*gdxk       72*/
         call mputd(gd3(lgd3(ig)+1),ng3(ig),i)                                  !/*gdxk       73*/
 1090 continue                                                                  !/*gdxk       74*/
      call msend(-1,4,i)                                                        !/*gdxk       75*/
      return                                                                    !/*gdxk       76*/
!                                                                               !/*gdxk       77*/
!       subtask receive mesh tally initialization data.          (mm=17)        !/*gdxk       78*/
 1100 call mrecv(-1,4,i)                                                        !/*gdxk       79*/
      call mgetd(gmeshtal,nf,i)                                                 !/*gdxk       80*/
      call mgeti(lmeshtal,lf,i)                                                 !/*gdxk       81*/
      do 1110 ig=1,nmesh                                                        !/*gdxk       82*/
         call mgetd(gd1(lgd1(ig)+1),ng1(ig),i)                                  !/*gdxk       83*/
         call mgetd(gd2(lgd2(ig)+1),ng2(ig),i)                                  !/*gdxk       84*/
         call mgetd(gd3(lgd3(ig)+1),ng3(ig),i)                                  !/*gdxk       85*/
 1110 continue                                                                  !/*gdxk       86*/
      return                                                                    !/*gdxk       87*/
!                                                                               !/*gdxk       88*/
!       subtask send mesh tally gdata array.                     (mm=18)        !/*gdxk       89*/
 1120 call msndi                                                                !/*gdxk       90*/
      do 1130 ig=1,nmesh                                                        !/*gdxk       91*/
        l2=ktgd(ig)+mxgt(ig)                                                    !/*gdxk       92*/
        l3=l2+mxgt(ig)                                                          !/*gdxk       93*/
        call mputd(gdata(l2+1),mxgt(ig),i)                                      !/*gdxk       94*/
        call mputd(gdata(l3+1),mxgt(ig),i)                                      !/*gdxk       95*/
 1130 continue                                                                  !/*gdxk       96*/
      call msend(0,4,i)                                                         !/*gdxk       97*/
      return                                                                    !/*gdxk       98*/
!                                                                               !/*gdxk       99*/
!       master receive mesh tally gdata array.                   (mm=19)        !/*gdxk      100*/
 1140 call mrecv(jz(1),4,i)                                                     !/*gdxk      101*/
      do 1150 ig=1,nmesh                                                        !/*gdxk      102*/
        l2=ktgd(ig)+mxgt(ig)                                                    !/*gdxk      103*/
        l3=l2+mxgt(ig)                                                          !/*gdxk      104*/
        call mgetd(gdata(l2+1),mxgt(ig),i)                                      !/*gdxk      105*/
        call mgetd(gdata(l3+1),mxgt(ig),i)                                      !/*gdxk      106*/
 1150 continue                                                                  !/*gdxk      107*/
#endif                                                                          /*!gdxk      108*/
#endif                                                                          /*!gdxk      109*/
 2000 return                                                                    !/*gdxk      110*/
      end                                                                       !/*gd        347*/
