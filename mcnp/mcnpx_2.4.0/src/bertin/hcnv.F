!     $Id: hcnv.F,v 1.5 2002/03/27 18:13:50 gwm Exp $                         !/*hc          1*/
      program hcnv                                                              !/*hc          2*/
! **********************************************************************        !/*hc          3*/
! *** itape controls the reading and writing of the tape containing             !/*hc          4*/
!        the intranuclear cascade and evaporation data. ****************        !/*hc          5*/
!     itape  equal zero - read binary tape and write bcd tape                   !/*hc          6*/
!     itape  not equal zero - read bcd tape and write binary tape.              !/*hc          7*/
! **********************************************************************        !/*hc          8*/
#include <htape3x/com32b.h>                                                     /*!hc          9*/
      external bdhc                                                             !/*hc         10*/
#include <htape3x/lpt.h>                                                        /*!hc         11*/
#include <htape3x/contrl.h>                                                     /*!hc         12*/
#include <htape3x/charhc.h>                                                     /*!hc         13*/
#include <htape3x/wapcom.h>                                                     /*!hc         14*/
#include <htape3x/xsmax.h>                                                      /*!hc         15*/
      common /z/ t1(600), t2(29849)                                             !/*hc         16*/
      dimension p0(1001), p1(1001), p2(1001), ia(6), iz(6), rho(6),             !/*hc         17*/
     & omega(6), exmass(6), cam2(130), cam3(200), cam4(130), cam5(200),         !/*hc         18*/
     & t(4,7), rmass(300), alph(300), bet(300), waps0(250,20)                   !/*hc         19*/
      dimension ainfo(20), epkin(80,6), sigke(80,6), delexc(80,6,3)             !/*hc         20*/
      parameter (ln0=81, lt0=21)                                                !/*hc         21*/
      parameter (ln1=61, lt1=61)                                                !/*hc         22*/
      dimension psi0(ln0), trans0(lt0,ln0), x0(lt0), f0(ln0)                    !/*hc         23*/
      dimension psi1(ln1), trans1(lt1,ln1), x1(lt1), f1(ln1)                    !/*hc         24*/
      dimension ej(200), jj(200), tiso(200)                                     !/*hc         25*/
      character *8 new, old                                                     !/*hc         26*/
      data new /'new'/, old /'old'/                                             !/*hc         27*/
!        open the file or files for the user terminal.                          !/*hc         28*/
      itty=5                                                                    !/*hc         29*/
      jtty=6                                                                    !/*hc         30*/
      ltesting = .false.                                                        !/*hc         31*/
!                                                                               !/*hc         32*/
      call exemes (isub(1),msub(1),ndef)                                        !/*hc         33*/
      io=8                                                                      !/*hc         34*/
      if (inqire(hcnvout).ne.0.and.hcnvout.eq.'hcnvout')                        !/*hc         35*/
     &    hcnvout='hcnvouta'                                                    !/*hc         36*/
      call uopen (jtty,io,hcnvout,'f')                                          !/*hc         37*/
      nbitp=9                                                                   !/*hc         38*/
      nbcdtp=7                                                                  !/*hc         39*/
      write (jtty,90)                                                           !/*hc         40*/
      read (itty,*) itape                                                       !/*hc         41*/
      if (itape.ne.0) go to 40                                                  !/*hc         42*/
! *** read intranculear cascade data in binary *************************        !/*hc         43*/
      if (inqire(bcdtp).ne.0.and.bcdtp.eq.'bcdtp') bcdtp='bcdtpa'               !/*hc         44*/
      call uopen (jtty,nbcdtp,bcdtp,'f')                                        !/*hc         45*/
      open (nbitp,file=bintp,status=old,access='sequential',                    !/*hc         46*/
     & form='unformatted')                                                      !/*hc         47*/
      do 10 i=1,4                                                               !/*hc         48*/
      read (nbitp) (t1(k),k=1,600)                                              !/*hc         49*/
      write (io,*) ' ***   ','t1'                                               !/*hc         50*/
      write (io,100) (t1(k),k=1,600)                                            !/*hc         51*/
   10 write (nbcdtp,100) (t1(k),k=1,600)                                        !/*hc         52*/
      read (nbitp) (t2(k),k=1,29849)                                            !/*hc         53*/
!*****read evaporation data                                                     !/*hc         54*/
      read (nbitp) (p0(j),p1(j),p2(j),j=1,1001)                                 !/*hc         55*/
      read (nbitp) (ia(j),j=1,6),(iz(j),j=1,6)                                  !/*hc         56*/
      read (nbitp) (rho(j),j=1,6),(omega(j),j=1,6)                              !/*hc         57*/
      read (nbitp) (exmass(j),j=1,6)                                            !/*hc         58*/
      read (nbitp) (cam2(j),j=1,130)                                            !/*hc         59*/
      read (nbitp) (cam3(j),j=1,200)                                            !/*hc         60*/
      read (nbitp) (cam4(j),j=1,130)                                            !/*hc         61*/
      read (nbitp) (cam5(j),j=1,200)                                            !/*hc         62*/
      read (nbitp) ((t(i,j),j=1,7),i=1,3)                                       !/*hc         63*/
      read (nbitp) (rmass(j),j=1,297)                                           !/*hc         64*/
      read (nbitp) (alph(j),j=1,297)                                            !/*hc         65*/
      read (nbitp) (bet(j),j=1,297)                                             !/*hc         66*/
      read (nbitp) ((waps0(i,j),i=1,250),j=1,20)                                !/*hc         67*/
!*****read fission data                                                         !/*hc         68*/
      read (nbitp) ainfo                                                        !/*hc         69*/
      read (nbitp) epkin,sigke                                                  !/*hc         70*/
      read (nbitp) delexc                                                       !/*hc         71*/
!*****read coulomb barrier data                                                 !/*hc         72*/
      read (nbitp) delp0,delx0                                                  !/*hc         73*/
      read (nbitp) psi0                                                         !/*hc         74*/
      read (nbitp) x0                                                           !/*hc         75*/
      read (nbitp) f0                                                           !/*hc         76*/
      read (nbitp) trans0                                                       !/*hc         77*/
      read (nbitp) delp1,delx1                                                  !/*hc         78*/
      read (nbitp) psi1                                                         !/*hc         79*/
      read (nbitp) x1                                                           !/*hc         80*/
      read (nbitp) f1                                                           !/*hc         81*/
      read (nbitp) trans1                                                       !/*hc         82*/
!***********  write listing of data                                             !/*hc         83*/
      write (io,*) ' ***   ','t2         '                                      !/*hc         84*/
      write (io,100) (t2(k),k=1,29849)                                          !/*hc         85*/
      write (io,*) ' ***   ','p0,p1,p2   '                                      !/*hc         86*/
      write (io,110) (p0(j),p1(j),p2(j),j=1,1001)                               !/*hc         87*/
      write (io,*) ' ***   ','ia,iz      '                                      !/*hc         88*/
      write (io,120) (ia(j),j=1,6),(iz(j),j=1,6)                                !/*hc         89*/
      write (io,*) ' ***   ','rho,omega  '                                      !/*hc         90*/
      write (io,110) (rho(j),j=1,6),(omega(j),j=1,6)                            !/*hc         91*/
      write (io,*) ' ***   ','exmass     '                                      !/*hc         92*/
      write (io,110) (exmass(j),j=1,6)                                          !/*hc         93*/
      write (io,*) ' ***   ','cam2       '                                      !/*hc         94*/
      write (io,110) (cam2(j),j=1,130)                                          !/*hc         95*/
      write (io,*) ' ***   ','cam3       '                                      !/*hc         96*/
      write (io,110) (cam3(j),j=1,200)                                          !/*hc         97*/
      write (io,*) ' ***   ','cam4       '                                      !/*hc         98*/
      write (io,110) (cam4(j),j=1,130)                                          !/*hc         99*/
      write (io,*) ' ***   ','cam5       '                                      !/*hc        100*/
      write (io,110) (cam5(j),j=1,200)                                          !/*hc        101*/
      write (io,*) ' ***   ','t          '                                      !/*hc        102*/
      write (io,110) ((t(i,j),j=1,7),i=1,3)                                     !/*hc        103*/
      write (io,*) ' ***   ','rmass      '                                      !/*hc        104*/
      write (io,110) (rmass(j),j=1,297)                                         !/*hc        105*/
      write (io,*) ' ***   ','alph       '                                      !/*hc        106*/
      write (io,110) (alph(j),j=1,297)                                          !/*hc        107*/
      write (io,*) ' ***   ','bet        '                                      !/*hc        108*/
      write (io,110) (bet(j),j=1,297)                                           !/*hc        109*/
      write (io,*) ' ***   ','waps (original)       '                           !/*hc        110*/
      write (io,110) ((waps0(i,j),i=1,250),j=1,20)                              !/*hc        111*/
      write (io,*) ' ***   ','ainfo      '                                      !/*hc        112*/
      write (io,110) ainfo                                                      !/*hc        113*/
      write (io,*) ' ***   ','epkin,sigke'                                      !/*hc        114*/
      write (io,110) epkin,sigke                                                !/*hc        115*/
      write (io,*) ' ***   ','delexc     '                                      !/*hc        116*/
      write (io,110) delexc                                                     !/*hc        117*/
      write (io,*) ' ***   ','delp0,delx0'                                      !/*hc        118*/
      write (io,150) delp0,delx0                                                !/*hc        119*/
      write (io,*) ' ***   ','psi0       '                                      !/*hc        120*/
      write (io,150) psi0                                                       !/*hc        121*/
      write (io,*) ' ***   ','x0         '                                      !/*hc        122*/
      write (io,150) x0                                                         !/*hc        123*/
      write (io,*) ' ***   ','f0         '                                      !/*hc        124*/
      write (io,150) f0                                                         !/*hc        125*/
      write (io,*) ' ***   ','trans0     '                                      !/*hc        126*/
      write (io,150) trans0                                                     !/*hc        127*/
      write (io,*) ' ***   ','delp1,delx1'                                      !/*hc        128*/
      write (io,150) delp1,delx1                                                !/*hc        129*/
      write (io,*) ' ***   ','psi1       '                                      !/*hc        130*/
      write (io,150) psi1                                                       !/*hc        131*/
      write (io,*) ' ***   ','x1         '                                      !/*hc        132*/
      write (io,150) x1                                                         !/*hc        133*/
      write (io,*) ' ***   ','f1         '                                      !/*hc        134*/
      write (io,150) f1                                                         !/*hc        135*/
      write (io,*) ' ***   ','trans1     '                                      !/*hc        136*/
      write (io,150) trans1                                                     !/*hc        137*/
!*****write BCD                                                                 !/*hc        138*/
      write (nbcdtp,100) (t2(k),k=1,29849)                                      !/*hc        139*/
      write (nbcdtp,110) (p0(j),p1(j),p2(j),j=1,1001)                           !/*hc        140*/
      write (nbcdtp,120) (ia(j),j=1,6),(iz(j),j=1,6)                            !/*hc        141*/
      write (nbcdtp,110) (rho(j),j=1,6),(omega(j),j=1,6)                        !/*hc        142*/
      write (nbcdtp,110) (exmass(j),j=1,6)                                      !/*hc        143*/
      write (nbcdtp,110) (cam2(j),j=1,130)                                      !/*hc        144*/
      write (nbcdtp,110) (cam3(j),j=1,200)                                      !/*hc        145*/
      write (nbcdtp,110) (cam4(j),j=1,130)                                      !/*hc        146*/
      write (nbcdtp,110) (cam5(j),j=1,200)                                      !/*hc        147*/
      write (nbcdtp,110) ((t(i,j),j=1,7),i=1,3)                                 !/*hc        148*/
      write (nbcdtp,110) (rmass(j),j=1,297)                                     !/*hc        149*/
      write (nbcdtp,110) (alph(j),j=1,297)                                      !/*hc        150*/
      write (nbcdtp,110) (bet(j),j=1,297)                                       !/*hc        151*/
      write (nbcdtp,110) ((waps0(i,j),i=1,250),j=1,20)                          !/*hc        152*/
      write (nbcdtp,110) ainfo                                                  !/*hc        153*/
      write (nbcdtp,110) epkin,sigke                                            !/*hc        154*/
      write (nbcdtp,110) delexc                                                 !/*hc        155*/
      write (nbcdtp,150) delp0,delx0                                            !/*hc        156*/
      write (nbcdtp,150) psi0                                                   !/*hc        157*/
      write (nbcdtp,150) x0                                                     !/*hc        158*/
      write (nbcdtp,150) f0                                                     !/*hc        159*/
      write (nbcdtp,150) trans0                                                 !/*hc        160*/
      write (nbcdtp,150) delp1,delx1                                            !/*hc        161*/
      write (nbcdtp,150) psi1                                                   !/*hc        162*/
      write (nbcdtp,150) x1                                                     !/*hc        163*/
      write (nbcdtp,150) f1                                                     !/*hc        164*/
      write (nbcdtp,150) trans1                                                 !/*hc        165*/
!*****read and write Fermi breakup data                                         !/*hc        166*/
      write (io,*) ' ***   ','breakup data'                                     !/*hc        167*/
      read (nbitp) nr,nr1,nr2                                                   !/*hc        168*/
      write (nbcdtp,160) nr,nr1,nr2                                             !/*hc        169*/
      write (io,160) nr,nr1,nr2                                                 !/*hc        170*/
      do 20 i1=1,nr                                                             !/*hc        171*/
      read (nbitp) zin,ain,xmin,nsin                                            !/*hc        172*/
      write (nbcdtp,170) zin,ain,xmin,nsin                                      !/*hc        173*/
      write (io,170) zin,ain,xmin,nsin                                          !/*hc        174*/
      read (nbitp) (ej(i),jj(i),tiso(i),i=1,nsin)                               !/*hc        175*/
      write (nbcdtp,180) (ej(i),jj(i),tiso(i),i=1,nsin)                         !/*hc        176*/
      write (io,180) (ej(i),jj(i),tiso(i),i=1,nsin)                             !/*hc        177*/
   20 continue                                                                  !/*hc        178*/
      nrx=nr1+nr2                                                               !/*hc        179*/
      do 30 i=1,nrx                                                             !/*hc        180*/
      read (nbitp) zin,ain,xmin                                                 !/*hc        181*/
      write (nbcdtp,170) zin,ain,xmin                                           !/*hc        182*/
      write (io,170) zin,ain,xmin                                               !/*hc        183*/
   30 continue                                                                  !/*hc        184*/
!*****read and write new mass data                                              !/*hc        185*/
      read (nbitp) waps                                                         !/*hc        186*/
      read (nbitp) elbnde                                                       !/*hc        187*/
      read (nbitp) deln                                                         !/*hc        188*/
      read (nbitp) third                                                        !/*hc        189*/
      read (nbitp) iz0                                                          !/*hc        190*/
      read (nbitp) maxz                                                         !/*hc        191*/
      read (nbitp) maxn                                                         !/*hc        192*/
      read (nbitp) izmin                                                        !/*hc        193*/
      read (nbitp) izmax                                                        !/*hc        194*/
      read (nbitp) cwaps                                                        !/*hc        195*/
!*****                                                                          !/*hc        196*/
      write (io,*) ' ***   ','new mass data'                                    !/*hc        197*/
      write (io,*) ' ***   ','waps'                                             !/*hc        198*/
      write (io,200) waps                                                       !/*hc        199*/
      write (io,*) ' ***   ','elbnde'                                           !/*hc        200*/
      write (io,200) elbnde                                                     !/*hc        201*/
      write (io,*) ' ***   ','deln'                                             !/*hc        202*/
      write (io,200) deln                                                       !/*hc        203*/
      write (io,*) ' ***   ','third'                                            !/*hc        204*/
      write (io,200) third                                                      !/*hc        205*/
      write (io,*) ' ***   ','iz0'                                              !/*hc        206*/
      write (io,210) iz0                                                        !/*hc        207*/
      write (io,*) ' ***   ','maxz'                                             !/*hc        208*/
      write (io,210) maxz                                                       !/*hc        209*/
      write (io,*) ' ***   ','maxn'                                             !/*hc        210*/
      write (io,210) maxn                                                       !/*hc        211*/
      write (io,*) ' ***   ','izmin'                                            !/*hc        212*/
      write (io,210) izmin                                                      !/*hc        213*/
      write (io,*) ' ***   ','izmax'                                            !/*hc        214*/
      write (io,210) izmax                                                      !/*hc        215*/
      write (io,*) ' ***   ','cwaps'                                            !/*hc        216*/
      write (io,220) cwaps                                                      !/*hc        217*/
!*****                                                                          !/*hc        218*/
      write (nbcdtp,200) waps                                                   !/*hc        219*/
      write (nbcdtp,200) elbnde                                                 !/*hc        220*/
      write (nbcdtp,200) deln                                                   !/*hc        221*/
      write (nbcdtp,200) third                                                  !/*hc        222*/
      write (nbcdtp,210) iz0                                                    !/*hc        223*/
      write (nbcdtp,210) maxz                                                   !/*hc        224*/
      write (nbcdtp,210) maxn                                                   !/*hc        225*/
      write (nbcdtp,210) izmin                                                  !/*hc        226*/
      write (nbcdtp,210) izmax                                                  !/*hc        227*/
      write (nbcdtp,220) cwaps                                                  !/*hc        228*/
!*****read and write cross section maximum data                                 !/*hc        229*/
      read (nbitp) xmaxa                                                        !/*hc        230*/
      read (nbitp) xmaxs                                                        !/*hc        231*/
      read (nbitp) xmaxt                                                        !/*hc        232*/
      write (io,*) ' ***   ','cross section maximum data'                       !/*hc        233*/
      write (io,*) ' ***   ','xmaxa'                                            !/*hc        234*/
      write (io,230) xmaxa                                                      !/*hc        235*/
      write (io,*) ' ***   ','xmaxs'                                            !/*hc        236*/
      write (io,230) xmaxs                                                      !/*hc        237*/
      write (io,*) ' ***   ','xmaxt'                                            !/*hc        238*/
      write (io,230) xmaxt                                                      !/*hc        239*/
      write (nbcdtp,230) xmaxa                                                  !/*hc        240*/
      write (nbcdtp,230) xmaxs                                                  !/*hc        241*/
      write (nbcdtp,230) xmaxt                                                  !/*hc        242*/
!*****                                                                          !/*hc        243*/
      write (jtty,130)                                                          !/*hc        244*/
      go to 80                                                                  !/*hc        245*/
! *** read intranculear cascade data in bcd ****************************        !/*hc        246*/
   40 continue                                                                  !/*hc        247*/
      open (nbcdtp,file=bcdtp,status=old)                                       !/*hc        248*/
      if (inqire(bintp).ne.0.and.bintp.eq.'bintp') bintp='bintpa'               !/*hc        249*/
!     call uopen (jtty,nbitp,bintp,'u')                                         !/*hc        250*/
      if (inqire(bintp).ne.0) call unique (bintp,jtty)                          !/*hc        251*/
      open (nbitp,file=bintp,status=new,access='sequential',form=               !/*hc        252*/
     & 'unformatted')                                                           !/*hc        253*/
      do 50 i=1,4                                                               !/*hc        254*/
      read (nbcdtp,100) (t1(k),k=1,600)                                         !/*hc        255*/
      write (io,*) ' ***   ','t1'                                               !/*hc        256*/
      write (io,100) (t1(k),k=1,600)                                            !/*hc        257*/
   50 write (nbitp) (t1(k),k=1,600)                                             !/*hc        258*/
      read (nbcdtp,100) (t2(k),k=1,29849)                                       !/*hc        259*/
      read (nbcdtp,110) (p0(j),p1(j),p2(j),j=1,1001)                            !/*hc        260*/
      read (nbcdtp,120) (ia(j),j=1,6),(iz(j),j=1,6)                             !/*hc        261*/
      read (nbcdtp,110) (rho(j),j=1,6),(omega(j),j=1,6)                         !/*hc        262*/
      read (nbcdtp,110) (exmass(j),j=1,6)                                       !/*hc        263*/
      read (nbcdtp,110) (cam2(j),j=1,130)                                       !/*hc        264*/
      read (nbcdtp,110) (cam3(j),j=1,200)                                       !/*hc        265*/
      read (nbcdtp,110) (cam4(j),j=1,130)                                       !/*hc        266*/
      read (nbcdtp,110) (cam5(j),j=1,200)                                       !/*hc        267*/
      read (nbcdtp,110) ((t(i,j),j=1,7),i=1,3)                                  !/*hc        268*/
      read (nbcdtp,110) (rmass(j),j=1,297)                                      !/*hc        269*/
      read (nbcdtp,110) (alph(j),j=1,297)                                       !/*hc        270*/
      read (nbcdtp,110) (bet(j),j=1,297)                                        !/*hc        271*/
      read (nbcdtp,110) ((waps0(i,j),i=1,250),j=1,20)                           !/*hc        272*/
      read (nbcdtp,110) ainfo                                                   !/*hc        273*/
      read (nbcdtp,110) epkin,sigke                                             !/*hc        274*/
      read (nbcdtp,110) delexc                                                  !/*hc        275*/
      read (nbcdtp,150) delp0,delx0                                             !/*hc        276*/
      read (nbcdtp,150) psi0                                                    !/*hc        277*/
      read (nbcdtp,150) x0                                                      !/*hc        278*/
      read (nbcdtp,150) f0                                                      !/*hc        279*/
      read (nbcdtp,150) trans0                                                  !/*hc        280*/
      read (nbcdtp,150) delp1,delx1                                             !/*hc        281*/
      read (nbcdtp,150) psi1                                                    !/*hc        282*/
      read (nbcdtp,150) x1                                                      !/*hc        283*/
      read (nbcdtp,150) f1                                                      !/*hc        284*/
      read (nbcdtp,150) trans1                                                  !/*hc        285*/
!***********  write listing of data                                             !/*hc        286*/
      write (io,*) ' ***   ','t2         '                                      !/*hc        287*/
      write (io,100) (t2(k),k=1,29849)                                          !/*hc        288*/
      write (io,*) ' ***   ','p0,p1,p2   '                                      !/*hc        289*/
      write (io,110) (p0(j),p1(j),p2(j),j=1,1001)                               !/*hc        290*/
      write (io,*) ' ***   ','ia,iz      '                                      !/*hc        291*/
      write (io,120) (ia(j),j=1,6),(iz(j),j=1,6)                                !/*hc        292*/
      write (io,*) ' ***   ','rho,omega  '                                      !/*hc        293*/
      write (io,110) (rho(j),j=1,6),(omega(j),j=1,6)                            !/*hc        294*/
      write (io,*) ' ***   ','exmass     '                                      !/*hc        295*/
      write (io,110) (exmass(j),j=1,6)                                          !/*hc        296*/
      write (io,*) ' ***   ','cam2       '                                      !/*hc        297*/
      write (io,110) (cam2(j),j=1,130)                                          !/*hc        298*/
      write (io,*) ' ***   ','cam3       '                                      !/*hc        299*/
      write (io,110) (cam3(j),j=1,200)                                          !/*hc        300*/
      write (io,*) ' ***   ','cam4       '                                      !/*hc        301*/
      write (io,110) (cam4(j),j=1,130)                                          !/*hc        302*/
      write (io,*) ' ***   ','cam5       '                                      !/*hc        303*/
      write (io,110) (cam5(j),j=1,200)                                          !/*hc        304*/
      write (io,*) ' ***   ','t          '                                      !/*hc        305*/
      write (io,110) ((t(i,j),j=1,7),i=1,3)                                     !/*hc        306*/
      write (io,*) ' ***   ','rmass      '                                      !/*hc        307*/
      write (io,110) (rmass(j),j=1,297)                                         !/*hc        308*/
      write (io,*) ' ***   ','alph       '                                      !/*hc        309*/
      write (io,110) (alph(j),j=1,297)                                          !/*hc        310*/
      write (io,*) ' ***   ','bet        '                                      !/*hc        311*/
      write (io,110) (bet(j),j=1,297)                                           !/*hc        312*/
      write (io,*) ' ***   ','waps (original)       '                           !/*hc        313*/
      write (io,110) ((waps0(i,j),i=1,250),j=1,20)                              !/*hc        314*/
      write (io,*) ' ***   ','ainfo      '                                      !/*hc        315*/
      write (io,110) ainfo                                                      !/*hc        316*/
      write (io,*) ' ***   ','epkin,sigke'                                      !/*hc        317*/
      write (io,110) epkin,sigke                                                !/*hc        318*/
      write (io,*) ' ***   ','delexc     '                                      !/*hc        319*/
      write (io,110) delexc                                                     !/*hc        320*/
      write (io,*) ' ***   ','delp0,delx0'                                      !/*hc        321*/
      write (io,150) delp0,delx0                                                !/*hc        322*/
      write (io,*) ' ***   ','psi0       '                                      !/*hc        323*/
      write (io,150) psi0                                                       !/*hc        324*/
      write (io,*) ' ***   ','x0         '                                      !/*hc        325*/
      write (io,150) x0                                                         !/*hc        326*/
      write (io,*) ' ***   ','f0         '                                      !/*hc        327*/
      write (io,150) f0                                                         !/*hc        328*/
      write (io,*) ' ***   ','trans0     '                                      !/*hc        329*/
      write (io,150) trans0                                                     !/*hc        330*/
      write (io,*) ' ***   ','delp1,delx1'                                      !/*hc        331*/
      write (io,150) delp1,delx1                                                !/*hc        332*/
      write (io,*) ' ***   ','psi1       '                                      !/*hc        333*/
      write (io,150) psi1                                                       !/*hc        334*/
      write (io,*) ' ***   ','x1         '                                      !/*hc        335*/
      write (io,150) x1                                                         !/*hc        336*/
      write (io,*) ' ***   ','f1         '                                      !/*hc        337*/
      write (io,150) f1                                                         !/*hc        338*/
      write (io,*) ' ***   ','trans1     '                                      !/*hc        339*/
      write (io,150) trans1                                                     !/*hc        340*/
!*****write binary                                                              !/*hc        341*/
      write (nbitp) (t2(k),k=1,29849)                                           !/*hc        342*/
      write (nbitp) (p0(j),p1(j),p2(j),j=1,1001)                                !/*hc        343*/
      write (nbitp) (ia(j),j=1,6),(iz(j),j=1,6)                                 !/*hc        344*/
      write (nbitp) (rho(j),j=1,6),(omega(j),j=1,6)                             !/*hc        345*/
      write (nbitp) (exmass(j),j=1,6)                                           !/*hc        346*/
      write (nbitp) (cam2(j),j=1,130)                                           !/*hc        347*/
      write (nbitp) (cam3(j),j=1,200)                                           !/*hc        348*/
      write (nbitp) (cam4(j),j=1,130)                                           !/*hc        349*/
      write (nbitp) (cam5(j),j=1,200)                                           !/*hc        350*/
      write (nbitp) ((t(i,j),j=1,7),i=1,3)                                      !/*hc        351*/
      write (nbitp) (rmass(j),j=1,297)                                          !/*hc        352*/
      write (nbitp) (alph(j),j=1,297)                                           !/*hc        353*/
      write (nbitp) (bet(j),j=1,297)                                            !/*hc        354*/
      write (nbitp) ((waps0(i,j),i=1,250),j=1,20)                               !/*hc        355*/
      write (nbitp) ainfo                                                       !/*hc        356*/
      write (nbitp) epkin,sigke                                                 !/*hc        357*/
      write (nbitp) delexc                                                      !/*hc        358*/
      write (nbitp) delp0,delx0                                                 !/*hc        359*/
      write (nbitp) psi0                                                        !/*hc        360*/
      write (nbitp) x0                                                          !/*hc        361*/
      write (nbitp) f0                                                          !/*hc        362*/
      write (nbitp) trans0                                                      !/*hc        363*/
      write (nbitp) delp1,delx1                                                 !/*hc        364*/
      write (nbitp) psi1                                                        !/*hc        365*/
      write (nbitp) x1                                                          !/*hc        366*/
      write (nbitp) f1                                                          !/*hc        367*/
      write (nbitp) trans1                                                      !/*hc        368*/
!*****read and write Fermi breakup data                                         !/*hc        369*/
      write (io,*) ' ***   ','breakup data'                                     !/*hc        370*/
      read (nbcdtp,160) nr,nr1,nr2                                              !/*hc        371*/
      write (nbitp) nr,nr1,nr2                                                  !/*hc        372*/
      write (io,160) nr,nr1,nr2                                                 !/*hc        373*/
      do 60 i1=1,nr                                                             !/*hc        374*/
      read (nbcdtp,170) zin,ain,xmin,nsin                                       !/*hc        375*/
      write (nbitp) zin,ain,xmin,nsin                                           !/*hc        376*/
      write (io,170) zin,ain,xmin,nsin                                          !/*hc        377*/
      read (nbcdtp,180) (ej(i),jj(i),tiso(i),i=1,nsin)                          !/*hc        378*/
      write (nbitp) (ej(i),jj(i),tiso(i),i=1,nsin)                              !/*hc        379*/
      write (io,180) (ej(i),jj(i),tiso(i),i=1,nsin)                             !/*hc        380*/
   60 continue                                                                  !/*hc        381*/
      nrx=nr1+nr2                                                               !/*hc        382*/
      do 70 i=1,nrx                                                             !/*hc        383*/
      read (nbcdtp,170) zin,ain,xmin                                            !/*hc        384*/
      write (nbitp) zin,ain,xmin                                                !/*hc        385*/
      write (io,170) zin,ain,xmin                                               !/*hc        386*/
   70 continue                                                                  !/*hc        387*/
!*****read and write new mass data                                              !/*hc        388*/
      read (nbcdtp,200) waps                                                    !/*hc        389*/
      read (nbcdtp,200) elbnde                                                  !/*hc        390*/
      read (nbcdtp,200) deln                                                    !/*hc        391*/
      read (nbcdtp,200) third                                                   !/*hc        392*/
      read (nbcdtp,210) iz0                                                     !/*hc        393*/
      read (nbcdtp,210) maxz                                                    !/*hc        394*/
      read (nbcdtp,210) maxn                                                    !/*hc        395*/
      read (nbcdtp,210) izmin                                                   !/*hc        396*/
      read (nbcdtp,210) izmax                                                   !/*hc        397*/
      read (nbcdtp,220) cwaps                                                   !/*hc        398*/
!*****                                                                          !/*hc        399*/
      write (io,*) ' ***   ','new mass data'                                    !/*hc        400*/
      write (io,*) ' ***   ','waps'                                             !/*hc        401*/
      write (io,200) waps                                                       !/*hc        402*/
      write (io,*) ' ***   ','elbnde'                                           !/*hc        403*/
      write (io,200) elbnde                                                     !/*hc        404*/
      write (io,*) ' ***   ','deln'                                             !/*hc        405*/
      write (io,200) deln                                                       !/*hc        406*/
      write (io,*) ' ***   ','third'                                            !/*hc        407*/
      write (io,200) third                                                      !/*hc        408*/
      write (io,*) ' ***   ','iz0'                                              !/*hc        409*/
      write (io,210) iz0                                                        !/*hc        410*/
      write (io,*) ' ***   ','maxz'                                             !/*hc        411*/
      write (io,210) maxz                                                       !/*hc        412*/
      write (io,*) ' ***   ','maxn'                                             !/*hc        413*/
      write (io,210) maxn                                                       !/*hc        414*/
      write (io,*) ' ***   ','izmin'                                            !/*hc        415*/
      write (io,210) izmin                                                      !/*hc        416*/
      write (io,*) ' ***   ','izmax'                                            !/*hc        417*/
      write (io,210) izmax                                                      !/*hc        418*/
      write (io,*) ' ***   ','cwaps'                                            !/*hc        419*/
      write (io,220) cwaps                                                      !/*hc        420*/
!*****                                                                          !/*hc        421*/
      write (nbitp) waps                                                        !/*hc        422*/
      write (nbitp) elbnde                                                      !/*hc        423*/
      write (nbitp) deln                                                        !/*hc        424*/
      write (nbitp) third                                                       !/*hc        425*/
      write (nbitp) iz0                                                         !/*hc        426*/
      write (nbitp) maxz                                                        !/*hc        427*/
      write (nbitp) maxn                                                        !/*hc        428*/
      write (nbitp) izmin                                                       !/*hc        429*/
      write (nbitp) izmax                                                       !/*hc        430*/
      write (nbitp) cwaps                                                       !/*hc        431*/
!*****read and write cross section maximum data                                 !/*hc        432*/
      read (nbcdtp,230) xmaxa                                                   !/*hc        433*/
      read (nbcdtp,230) xmaxs                                                   !/*hc        434*/
      read (nbcdtp,230) xmaxt                                                   !/*hc        435*/
      write (io,*) ' ***   ','cross section maximum data'                       !/*hc        436*/
      write (io,*) ' ***   ','xmaxa'                                            !/*hc        437*/
      write (io,230) xmaxa                                                      !/*hc        438*/
      write (io,*) ' ***   ','xmaxs'                                            !/*hc        439*/
      write (io,230) xmaxs                                                      !/*hc        440*/
      write (io,*) ' ***   ','xmaxt'                                            !/*hc        441*/
      write (io,230) xmaxt                                                      !/*hc        442*/
      write (nbitp) xmaxa                                                       !/*hc        443*/
      write (nbitp) xmaxs                                                       !/*hc        444*/
      write (nbitp) xmaxt                                                       !/*hc        445*/
!*****                                                                          !/*hc        446*/
      write (jtty,140)                                                          !/*hc        447*/
   80 continue                                                                  !/*hc        448*/
      stop                                                                      !/*hc        449*/
!                                                                               !/*hc        450*/
   90 format (' enter 1 for bcd to binary , 0 for binary to bcd')               !/*hc        451*/
  100 format (5d16.8)                                                           !/*hc        452*/
  110 format (5d16.8)                                                           !/*hc        453*/
  120 format (6i10)                                                             !/*hc        454*/
  130 format (' a new bcd tape has been written')                               !/*hc        455*/
  140 format (' a new binary tape has been written')                            !/*hc        456*/
  150 format (1p6d13.5)                                                         !/*hc        457*/
  160 format (3i5)                                                              !/*hc        458*/
  170 format (2f10.0,f10.5,i10)                                                 !/*hc        459*/
  180 format (4(f11.5,i4,f5.1))                                                 !/*hc        460*/
  200 format (6f12.7)                                                           !/*hc        461*/
  210 format (18i4)                                                             !/*hc        462*/
  220 format (72a1)                                                             !/*hc        463*/
  230 format (13f7.3)                                                           !/*hc        464*/
      end                                                                       !/*hc        465*/
