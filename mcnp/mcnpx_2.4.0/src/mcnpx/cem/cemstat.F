      subroutine cemstat(iuo)                                                   !/*ce          1*/
#ifdef CEM                                                                      /*!ce          2*/
! ======================================================================        !/*ce          3*/
!                                                                               !/*ce          4*/
!     Generates CEM statistics at all physics stages.                           !/*ce          5*/
!     Is based on the TYPEOUT-routine of the standalone CEM97 code              !/*ce          6*/
!                                                                               !/*ce          7*/
!     CEM95 written by S. G. Mashnik                                            !/*ce          8*/
!                                                                               !/*ce          9*/
!     Edited by F.X.Gallmeier, June, 1998                                       !/*ce         10*/
!                                                                               !/*ce         11*/
! ======================================================================        !/*ce         12*/
#include <cem/cdouble.h>                                                        /*!ce         13*/
      character*3  par(9)                                                       !/*ce         14*/
      character*10 par2(9)                                                      !/*ce         15*/
      character*3  par3(5)                                                      !/*ce         16*/
      character*10 ch1(72)                                                      !/*ce         17*/
      character*10 ch2(72)                                                      !/*ce         18*/
      character*10 ch3(74)                                                      !/*ce         19*/
      character*10 ch(218)                                                      !/*ce         20*/
      character*5 part(23)                                                      !/*ce         21*/
      equivalence (ch1(1), ch(1)), (ch2(1), ch(73)), (ch3(1), ch(145))          !/*ce         22*/
      dimension emeanc(14), emean(9)                                            !/*ce         23*/
      dimension pmulc(14), pmul(9)                                              !/*ce         24*/
      dimension pcs(9), dpcs(9), pcsc(14), dpcsc(14)                            !/*ce         25*/
      dimension dpmulc(14), dpmul(9)                                            !/*ce         26*/
      dimension y(9), dy(9)                                                     !/*ce         27*/
      dimension yc(14), dyc(14)                                                 !/*ce         28*/
      dimension epcasav(5), epcasaq(5)                                          !/*ce         29*/
      common /ajsbar/ ainit, zinit, eb(20,50,90), egs(20,50,90)                 !/*ce         30*/
      common /begin/  cm0, t0gev, me0, mb0, ms0, l0                             !/*ce         31*/
      common /d2sdto/ tet1(10), tet2(10), te1(140), te2(140), dtt(140),         !/*ce         32*/
     &                se(140), dtdo(140,10), d2spec(9,10,140),                  !/*ce         33*/
     &                d2spe(14,10,140), ntt, ntet, nt2, nt3                     !/*ce         34*/
      common /degrad/ degrad                                                    !/*ce         35*/
      common /dele/   sfu, wf, fusion, sigfw                                    !/*ce         36*/
      common /deltt/  dteta                                                     !/*ce         37*/
      common /eqidat/ eestt, eestsq, aeqtot, aeqsq, zeqtot, zeqsq,              !/*ce         38*/
     &                aemin, aemax, eletot, elesq, elemin, elemax,              !/*ce         39*/
     &                eestrmn, eestrmx, zemin, zemax, neq                       !/*ce         40*/
      common /fisdat/ estart, estarsq, estrmn, estrmx,                          !/*ce         41*/
     &                elstart, elstarsq, elmmin, elmmax,                        !/*ce         42*/
     &                astart, astarsq, ammin, ammax,                            !/*ce         43*/
     &                zstart, zstarsq, zmmin, zmmax,                            !/*ce         44*/
     &                bftot, bfsq, bfmin, bfmax                                 !/*ce         45*/
      common /fiss/   fision, sigfis, nfis                                      !/*ce         46*/
      common /menu/   mspec, mpyld, mchy, misy, mdubl, mang                     !/*ce         47*/
      common /menu1/  ipar1, ipar2, ifam, ib, ish, idelta, ibe, idel            !/*ce         48*/
      common /o/      anucl, znucl, eps, vpi, rsm(10), rbig(10),                !/*ce         49*/
     &                rhop(10), rhon(10), af(10), tfp(10), tfn(10), no          !/*ce         50*/
      common /outsid/ icount,ifil,nout(1000),izout(1000),inout(1000)            !/*ce         51*/
      common /pii/     pi                                                       !/*ce         52*/
      common /predat/ epstt, epstsq, apqtot, apqsq, zpqtot, zpqsq,              !/*ce         53*/
     &                apmin, apmax, elptot, elpsq, elpmin, elpmax,              !/*ce         54*/
     &                epstrmn, epstrmx, zpmin, zpmax, npreq                     !/*ce         55*/
      common /resdat/ erkt, erksq, artot, arsq, zrtot, zrsq,                    !/*ce         56*/
     &                armin, armax, elrtot, elrsq, elrmin, elrmax,              !/*ce         57*/
     &                erkmn, erkmx, zrmin, zrmax, nres                          !/*ce         58*/
      common /resmac/ spe(14,140), an(14,20)                                    !/*ce         59*/
      common /result/ spec(9,140), ang(9,20), chan(218), dadz(50,18)            !/*ce         60*/
      common /retdat/ arttot, artsq, artmin, artmax, zrttot, zrtsq,             !/*ce         61*/
     &                zrtmin, zrtmax, elrttot, elrtsq, elrtmin,                 !/*ce         62*/
     &                elrtmax, nret                                             !/*ce         63*/
      common /stopr/  istp                                                      !/*ce         64*/
      common /vul/    ncas, intel, flimcc, sigom, sigomm                        !/*ce         65*/
      common /phstat/ ephts, ephtsq, ephmax, ephmin,                            !/*ce         66*/
     &                aphts, aphtsq, aphmax, aphmin,                            !/*ce         67*/
     &                zphts, zphtsq, zphmax, zphmin, nphts                      !/*ce         68*/
      common /casdat/ epcas(5), epcasq(5), epcmax(5), epcmin(5),                !/*ce         69*/
     &                atcas, atcasq, atcmax, atcmin,                            !/*ce         70*/
     &                ztcas, ztcasq, ztcmax, ztcmin, nccas(5)                   !/*ce         71*/
      data zro, one, two /0.d0, 1.d0, 2.d0/                                     !/*ce         72*/
      data par /'n  ', 'p  ', 'd  ', 't  ', 'He3', 'He4', 'pi-', 'pi0',         !/*ce         73*/
     &          'pi+'/                                                          !/*ce         74*/
      data par2 /' neutrons ', ' protons  ', 'deuterons ', ' tritons  ',        !/*ce         75*/
     &           ' Helium-3 ', '  alphas  ', 'neg. pions', 'neut pions',        !/*ce         76*/
     &           'pos. pions'/                                                  !/*ce         77*/
      data par3 /'n  ', 'p  ', 'pi-', 'pi0','pi+'/                              !/*ce         78*/
      data  part /'T n  ', 'C n  ', 'P n  ', 'E n  ', 'T p  ', 'C p  ',         !/*ce         79*/
     &            'P p  ', 'E p  ', 'T d  ', 'P d  ', 'E d  ', 'T t  ',         !/*ce         80*/
     &            'P t  ', 'E t  ', 'T He3', 'P He3', 'E He3', 'T He4',         !/*ce         81*/
     &            'P He4', 'E He4', 'pi-  ', 'pi0  ', 'pi+  '/                  !/*ce         82*/
      data ch1 /                                                                !/*ce         83*/
     &         '(N,n)     ', '(N,2n)    ', '(N,3n)    ', '(N,4n)    ',          !/*ce         84*/
     &         '(N,5n)    ', '(N,6n)    ', '(N,7n)    ', '(N,8n)    ',          !/*ce         85*/
     &         '(N,9n)    ', '(N,10n)   ', '(N,11n)   ', '(N,12n)   ',          !/*ce         86*/
     &         '(N,13n)   ', '(N,14n)   ', '(N,15n)   ', '(N,16n)   ',          !/*ce         87*/
     &         '(N,17n)   ', '(N,18n)   ', '(N,19n)   ', '(N,20n)   ',          !/*ce         88*/
     &         '(N,p)     ', '(N,2p)    ', '(N,3p)    ', '(N,4p)    ',          !/*ce         89*/
     &         '(N,5p)    ', '(N,n+p)   ', '(N,2n+p)  ', '(N,3n+p)  ',          !/*ce         90*/
     &         '(N,4n+p)  ', '(N,5n+p)  ', '(N,6n+p)  ', '(N,7n+p)  ',          !/*ce         91*/
     &         '(N,8n+p)  ', '(N,9n+p)  ', '(N,10n+p) ', '(N,11n+p) ',          !/*ce         92*/
     &         '(N,12n+p) ', '(N,13n+p) ', '(N,14n+p) ', '(N,15n+p) ',          !/*ce         93*/
     &         '(N,16n+p) ', '(N,17n+p) ', '(N,18n+p) ', '(N,19n+p) ',          !/*ce         94*/
     &         '(N,n+2p)  ', '(N,2n+2p) ', '(N,3n+2p) ', '(N,4n+2p) ',          !/*ce         95*/
     &         '(N,5n+2p) ', '(N,6n+2p) ', '(N,7n+2p) ', '(N,8n+2p) ',          !/*ce         96*/
     &         '(N,9n+2p) ', '(N,10n+2p)', '(N,11n+2p)', '(N,12n+2p)',          !/*ce         97*/
     &         '(N,13n+2p)', '(N,14n+2p)', '(N,15n+2p)', '(N,16n+2p)',          !/*ce         98*/
     &         '(N,17n+2p)', '(N,18n+2p)', '(N,n+3p)  ', '(N,2n+3p) ',          !/*ce         99*/
     &         '(N,3n+3p) ', '(N,4n+3p) ', '(N,5n+3p) ', '(N,6n+3p) ',          !/*ce        100*/
     &         '(N,7n+3p) ', '(N,8n+3p) ', '(N,9n+3p) ', '(N,10n+3p)'/          !/*ce        101*/
      data ch2 /                                                                !/*ce        102*/
     &         '(N,11n+3p)', '(N,12n+3p)', '(N,13n+3p)', '(N,14n+3p)',          !/*ce        103*/
     &         '(N,15n+3p)', '(N,16n+3p)', '(N,17n+3p)', '(N,n+4p)  ',          !/*ce        104*/
     &         '(N,2n+4p) ', '(N,3n+4p) ', '(N,4n+4p) ', '(N,5n+4p) ',          !/*ce        105*/
     &         '(N,6n+4p) ', '(N,7n+4p) ', '(N,8n+4p) ', '(N,9n+4p) ',          !/*ce        106*/
     &         '(N,10n+4p)', '(N,11n+4p)', '(N,12n+4p)', '(N,13n+4p)',          !/*ce        107*/
     &         '(N,14n+4p)', '(N,15n+4p)', '(N,16n+4p)', '(N,n+5p)  ',          !/*ce        108*/
     &         '(N,2n+5p) ', '(N,3n+5p) ', '(N,4n+5p) ', '(N,5n+5p) ',          !/*ce        109*/
     &         '(N,6n+5p) ', '(N,7n+5p) ', '(N,8n+5p) ', '(N,9n+5p) ',          !/*ce        110*/
     &         '(N,10n+5p)', '(N,11n+5p)', '(N,12n+5p)', '(N,13n+5p)',          !/*ce        111*/
     &         '(N,14n+5p)', '(N,15n+5p)', '(N,d)     ', '(N,n+d)   ',          !/*ce        112*/
     &         '(N,2n+d)  ', '(N,3n+d)  ', '(N,4n+d)  ', '(N,5n+d)  ',          !/*ce        113*/
     &         '(N,6n+d)  ', '(N,7n+d)  ', '(N,8n+d)  ', '(N,9n+d)  ',          !/*ce        114*/
     &         '(N,10n+d) ', '(N,11n+d) ', '(N,12n+d) ', '(N,13n+d) ',          !/*ce        115*/
     &         '(N,14n+d) ', '(N,15n+d) ', '(N,16n+d) ', '(N,17n+d) ',          !/*ce        116*/
     &         '(N,18n+d) ', '(N,19n+d) ', '(N,p+d)   ', '(N,n+p+d) ',          !/*ce        117*/
     &         '(N,2n+p+d)', '(N,3n+p+d)', '(N,4n+p+d)', '(N,5n+p+d)',          !/*ce        118*/
     &         '(N,6n+p+d)', '(N,7n+p+d)', '(N,8n+p+d)', '(N,9n+p+d)',          !/*ce        119*/
     &         '(N,10n+pd)', '(N,11n+pd)', '(N,12n+pd)', '(N,13n+pd)'/          !/*ce        120*/
      data ch3 /                                                                !/*ce        121*/
     &         '(N,14n+pd)', '(N,2p+d)  ', '(N,n+2p+d)', '(N,2n+2pd)',          !/*ce        122*/
     &         '(N,3n+2pd)', '(N,4n+2pd)', '(N,5n+2pd)', '(N,6n+2pd)',          !/*ce        123*/
     &         '(N,7n+2pd)', '(N,8n+2pd)', '(N,9n+2pd)', '(N,10n2pd)',          !/*ce        124*/
     &         '(N,11n2pd)', '(N,12n2pd)', '(N,He4)   ', '(N,2He4)  ',          !/*ce        125*/
     &         '(N,3He4)  ', '(N,n+He4) ', '(N,2n+He4)', '(N,3n+He4)',          !/*ce        126*/
     &         '(N,4n+He4)', '(N,5n+He4)', '(N,6n+He4)', '(N,7n+He4)',          !/*ce        127*/
     &         '(N,8n+He4)', '(N,9n+He4)', '(N,10nHe4)', '(N,11nHe4)',          !/*ce        128*/
     &         '(N,12nHe4)', '(N,13nHe4)', '(N,14nHe4)', '(N,n+2He4)',          !/*ce        129*/
     &         '(N,2n2He4)', '(N,p+He4) ', '(N,np+He4)', '(N,2npHe4)',          !/*ce        130*/
     &         '(N,3npHe4)', '(N,4npHe4)', '(N,5npHe4)', '(N,6npHe4)',          !/*ce        131*/
     &         '(N,7npHe4)', '(N,8npHe4)', '(N,9npHe4)', '(N,10npHe)',          !/*ce        132*/
     &         '(N,11npHe)', '(N,12npHe)', '(N,13npHe)', '(N,14npHe)',          !/*ce        133*/
     &         '(N,Xn)    ', '(N,Xp)    ', '(N,Xn+Yp) ', '(N,Xn+d)  ',          !/*ce        134*/
     &         '(N,Xn+2d) ', '(N,Xn+p+d)', '(N,Xn+2pd)', '(N,Xn+p2d)',          !/*ce        135*/
     &         '(N,Xn2p2d)', '(N,Xn+t)  ', '(N,Xn+2t) ', '(N,Xn+p+t)',          !/*ce        136*/
     &         '(N,Xn+2pt)', '(N,Xn+d+t)', '(N,Xn+pdt)', '(N,Xn+He4)',          !/*ce        137*/
     &         '(N,Xn2He4)', '(N,XnpHe4)', '(N,Xn2pHe)', '(N,Xn3pHe)',          !/*ce        138*/
     &         '(N,XndHe4)', '(N,t+X)   ', '(N,He3+X) ', '(N,pi- +X)',          !/*ce        139*/
     &         '(N,pi0 +X)', '(N,pi+ +X)'/                                      !/*ce        140*/
! ======================================================================        !/*ce        141*/
      radeg = one/degrad                                                        !/*ce        142*/
      twpi = two*pi                                                             !/*ce        143*/
      fl =flimcc                                                                !/*ce        144*/
!       (reset later)                                                           !/*ce        145*/
      fn = ncas                                                                 !/*ce        146*/
      fneq = neq                                                                !/*ce        147*/
      fnpreq = npreq                                                            !/*ce        148*/
      fnres = nres                                                              !/*ce        149*/
      fnret = nret                                                              !/*ce        150*/
      fnfis = nfis                                                              !/*ce        151*/
      sumpar = zro                                                              !/*ce        152*/
!                                                                               !/*ce        153*/
!  cascade stats                                                                !/*ce        154*/
      nccass=0                                                                  !/*ce        155*/
      do 10 i=1,5                                                               !/*ce        156*/
         nccass= nccass + nccas(i)                                              !/*ce        157*/
         if ( nccas(i).gt.0) then                                               !/*ce        158*/
            fnccas=nccas(i)                                                     !/*ce        159*/
            epcasav(i) = epcas(i)/fnccas                                        !/*ce        160*/
            epcasaq(i) = sqrt(epcasq(i)/fnccas - epcasav(i)**2)                 !/*ce        161*/
         else                                                                   !/*ce        162*/
            fnccas = dp0                                                        !/*ce        163*/
            epcasav(i) = dp0                                                    !/*ce        164*/
            epcasaq(i) = dp0                                                    !/*ce        165*/
         endif                                                                  !/*ce        166*/
   10 continue                                                                  !/*ce        167*/
      fn=nccass                                                                 !/*ce        168*/
      fe = fl - fn                                                              !/*ce        169*/
      sigomm=sigomm/nccass                                                      !/*ce        170*/
      sigin = sigomm*fn/fl                                                      !/*ce        171*/
      sigel = sigomm*fe/fl                                                      !/*ce        172*/
      siginn = sigomm/fl                                                        !/*ce        173*/
      if(nccass.gt.0) then                                                      !/*ce        174*/
         atcasav = atcas/fn                                                     !/*ce        175*/
         atcasaq = sqrt(atcasq/fn - atcasav*atcasav)                            !/*ce        176*/
         ztcasav = ztcas/fn                                                     !/*ce        177*/
         ztcasaq = sqrt(ztcasq/fn - ztcasav*ztcasav)                            !/*ce        178*/
      endif                                                                     !/*ce        179*/
      if (sfu.ne.zro) then                                                      !/*ce        180*/
         sfu1 = sfu/fn                                                          !/*ce        181*/
         sigfw = sfu1*sigin                                                     !/*ce        182*/
         if (nfis.ne.0) then                                                    !/*ce        183*/
            fision = fnfis/fn                                                   !/*ce        184*/
            dfis = sqrt(fnfis)/fn                                               !/*ce        185*/
            sigfis = fision*sigin                                               !/*ce        186*/
            dsigf = dfis*sigin                                                  !/*ce        187*/
            zstarav = zstart/fnfis                                              !/*ce        188*/
            zstaraq = sqrt(zstarsq/fnfis -zstarav**2)                           !/*ce        189*/
            astarav = astart/fnfis                                              !/*ce        190*/
            astaraq = sqrt(astarsq/fnfis -astarav**2)                           !/*ce        191*/
            estarav = estart/fnfis                                              !/*ce        192*/
            estaraq = sqrt(estarsq/fnfis - estarav**2)                          !/*ce        193*/
            elstarav = elstart/fnfis                                            !/*ce        194*/
            elstaraq = sqrt(elstarsq/fnfis - elstarav**2)                       !/*ce        195*/
            bfav = bftot/fnfis                                                  !/*ce        196*/
            bfaq = sqrt(bfsq/fnfis - bfav**2)                                   !/*ce        197*/
         endif                                                                  !/*ce        198*/
      endif                                                                     !/*ce        199*/
      if (npreq.gt.0) then                                                      !/*ce        200*/
         epav = epstt/fnpreq                                                    !/*ce        201*/
         zpqav = zpqtot/fnpreq                                                  !/*ce        202*/
         apqav = apqtot/fnpreq                                                  !/*ce        203*/
         elpav = elptot/fnpreq                                                  !/*ce        204*/
         sigep = sqrt(abs(epstsq/fnpreq - epav**2))                             !/*ce        205*/
         sigzpq = sqrt(abs(zpqsq/fnpreq - zpqav**2))                            !/*ce        206*/
         sigapq = sqrt(abs(apqsq/fnpreq - apqav**2))                            !/*ce        207*/
         siglp = sqrt(abs(elpsq/fnpreq - elpav**2))                             !/*ce        208*/
      endif                                                                     !/*ce        209*/
      if (neq.gt.0) then                                                        !/*ce        210*/
         eeav = eestt/fneq                                                      !/*ce        211*/
         zeqav = zeqtot/fneq                                                    !/*ce        212*/
         aeqav = aeqtot/fneq                                                    !/*ce        213*/
         eleav = eletot/fneq                                                    !/*ce        214*/
         sigee = sqrt(abs(eestsq/fneq - eeav**2))                               !/*ce        215*/
         sigzeq = sqrt(abs(zeqsq/fneq - zeqav**2))                              !/*ce        216*/
         sigaeq = sqrt(abs(aeqsq/fneq - aeqav**2))                              !/*ce        217*/
         sigle = sqrt(abs(elesq/fneq - eleav**2))                               !/*ce        218*/
      endif                                                                     !/*ce        219*/
      if (nres.gt.0) then                                                       !/*ce        220*/
         erkav = erkt/fnres                                                     !/*ce        221*/
         zrav = zrtot/fnres                                                     !/*ce        222*/
         arav = artot/fnres                                                     !/*ce        223*/
         elrav = elrtot/fnres                                                   !/*ce        224*/
         sigerk = sqrt(abs(erksq/fnres - erkav**2))                             !/*ce        225*/
         sigzr = sqrt(abs(zrsq/fnres - zrav**2))                                !/*ce        226*/
         sigar = sqrt(abs(arsq/fnres - arav**2))                                !/*ce        227*/
         siglr = sqrt(abs(elrsq/fnres - elrav**2))                              !/*ce        228*/
      endif                                                                     !/*ce        229*/
      if (nret.gt.0) then                                                       !/*ce        230*/
         zrtav = zrttot/fnret                                                   !/*ce        231*/
         artav = arttot/fnret                                                   !/*ce        232*/
         elrtav = elrttot/fnret                                                 !/*ce        233*/
         sigzrt = sqrt(abs(zrtsq/fnret - zrtav**2))                             !/*ce        234*/
         sigart = sqrt(abs(artsq/fnret - artav**2))                             !/*ce        235*/
         siglrt = sqrt(abs(elrtsq/fnret - elrtav**2))                           !/*ce        236*/
      endif                                                                     !/*ce        237*/
      if (nphts.gt.0) then                                                      !/*ce        238*/
         fnphts = nphts                                                         !/*ce        239*/
         ephtsav = ephts/fnphts                                                 !/*ce        240*/
         ephtsaq = sqrt(ephtsq/fnphts - ephtsav*ephtsav)                        !/*ce        241*/
         aphtsav = aphts/fnphts                                                 !/*ce        242*/
         aphtsaq = sqrt(aphtsq/fnphts - aphtsav*aphtsav)                        !/*ce        243*/
         zphtsav = zphts/fnphts                                                 !/*ce        244*/
         zphtsaq = sqrt(zphtsq/fnphts - zphtsav*zphtsav)                        !/*ce        245*/
      endif                                                                     !/*ce        246*/
      write (iuo, 999)                                                          !/*ce        247*/
      write (iuo, 1000) fn, fl-fn, sigin, sigel                                 !/*ce        248*/
      write (iuo, 1210) (par3(i), nccas(i), epcasav(i),epcasaq(i),              !/*ce        249*/
     &                   epcmin(i), epcmax(i), i=1,5),                          !/*ce        250*/
     &                   ztcasav, ztcasaq, ztcmin, ztcmax,                      !/*ce        251*/
     &                   atcasav, atcasaq, atcmin, atcmax                       !/*ce        252*/
      write (iuo, 1240) npreq, epav, sigep, epstrmn, epstrmx,                   !/*ce        253*/
     &                 zpqav, sigzpq, zpmin, zpmax, apqav, sigapq,              !/*ce        254*/
     &                 apmin, apmax, elpav, siglp, elpmin, elpmax               !/*ce        255*/
      write (iuo, 1245) nret, zrtav, sigzrt, zrtmin, zrtmax, artav,             !/*ce        256*/
     &                 sigart, artmin, artmax, elrtav, siglrt,                  !/*ce        257*/
     &                 elrtmin, elrtmax                                         !/*ce        258*/
      write (iuo, 1290) neq, eeav, sigee, eestrmn, eestrmx,                     !/*ce        259*/
     &                 zeqav, sigzeq, zemin, zemax, aeqav, sigaeq,              !/*ce        260*/
     &                 aemin, aemax, eleav, sigle, elemin, elemax               !/*ce        261*/
      write (iuo, 1260) nres, erkav, sigerk, erkmn, erkmx,                      !/*ce        262*/
     &                 zrav, sigzr, zrmin, zrmax, arav, sigar,                  !/*ce        263*/
     &                 armin, armax, elrav, siglr, elrmin, elrmax               !/*ce        264*/
      if (sfu.gt.zro) then                                                      !/*ce        265*/
         if (nfis.gt.0) then                                                    !/*ce        266*/
            write (iuo, 1250) nfis,                                             !/*ce        267*/
     &                   estarav, estaraq, estrmn, estrmx,                      !/*ce        268*/
     &                   zstarav, zstaraq, zmmin, zmmax,                        !/*ce        269*/
     &                   astarav, astaraq, ammin, ammax,                        !/*ce        270*/
     &                   elstarav, elstaraq, elmmin, elmmax,                    !/*ce        271*/
     &                   bfav, bfaq, bfmin, bfmax                               !/*ce        272*/
            write (iuo, 1100) fision, dfis, sigfis, dsigf                       !/*ce        273*/
         endif                                                                  !/*ce        274*/
         write (iuo, 1200) sfu1, sigfw                                          !/*ce        275*/
      endif                                                                     !/*ce        276*/
      if (nphts.gt.0) then                                                      !/*ce        277*/
         write (iuo, 1220) nphts,                                               !/*ce        278*/
     &                  ephtsav, ephtsaq,  ephmin, ephmax,                      !/*ce        279*/
     &                  zphtsav, zphtsaq, zphmin, zphmax,                       !/*ce        280*/
     &                  aphtsav, aphtsaq, aphmin, aphmax                        !/*ce        281*/
      endif                                                                     !/*ce        282*/
      if (mpyld.ge.0 .or. mspec.ge.0 .or. misy.ge.0) then                       !/*ce        283*/
         do 20 i = 1,14                                                         !/*ce        284*/
            if (spe(i,nt3).gt.zro) then                                         !/*ce        285*/
               emeanc(i) = spe(i,nt2)/spe(i,nt3)                                !/*ce        286*/
               pmulc(i) = spe(i,nt3)/fl                                         !/*ce        287*/
               dpmulc(i) = sqrt(abs(spe(i,nt3)))/fl                             !/*ce        288*/
            else                                                                !/*ce        289*/
               emeanc(i) = zro                                                  !/*ce        290*/
               pmulc(i) = zro                                                   !/*ce        291*/
               dpmulc(i) = zro                                                  !/*ce        292*/
            endif                                                               !/*ce        293*/
 20      continue                                                               !/*ce        294*/
         do 30 j = 1,9                                                          !/*ce        295*/
            if (spec(j,nt3).gt.zro) then                                        !/*ce        296*/
               sumpar = sumpar + spec(j,nt3)                                    !/*ce        297*/
               emean(j) = spec(j,nt2)/spec(j,nt3)                               !/*ce        298*/
               pmul(j) = spec(j,nt3)/fl                                         !/*ce        299*/
               dpmul(j) = sqrt(abs(spec(j,nt3)))/fl                             !/*ce        300*/
               dy(j) = dpmul(j)*sigomm                                          !/*ce        301*/
               y(j) = pmul(j)*sigomm                                            !/*ce        302*/
            else                                                                !/*ce        303*/
               emean(j) = zro                                                   !/*ce        304*/
               pmul(j) = zro                                                    !/*ce        305*/
               dpmul(j) = zro                                                   !/*ce        306*/
               y(j) = zro                                                       !/*ce        307*/
               dy(j) = zro                                                      !/*ce        308*/
            endif                                                               !/*ce        309*/
 30      continue                                                               !/*ce        310*/
         fac = sigin/(sumpar + fnfis)                                           !/*ce        311*/
         do 40 j = 1,9                                                          !/*ce        312*/
            if (spec(j,nt3).gt.zro) then                                        !/*ce        313*/
               pcs(j) = spec(j,nt3)*fac                                         !/*ce        314*/
               dpcs(j) = sqrt(abs(spec(j,nt3)))*fac                             !/*ce        315*/
            endif                                                               !/*ce        316*/
 40      continue                                                               !/*ce        317*/
         do 50 i = 1,14                                                         !/*ce        318*/
            if (spe(i,nt3).gt.zro) then                                         !/*ce        319*/
               dyc(i) = dpmulc(i)*sigomm                                        !/*ce        320*/
               yc(i) = pmulc(i)*sigomm                                          !/*ce        321*/
               pcsc(i) = spe(i,nt3)*fac                                         !/*ce        322*/
               dpcsc(i) = sqrt(abs(spe(i,nt3)))*fac                             !/*ce        323*/
            else                                                                !/*ce        324*/
               yc(i) = zro                                                      !/*ce        325*/
               dyc(i) = zro                                                     !/*ce        326*/
            endif                                                               !/*ce        327*/
 50      continue                                                               !/*ce        328*/
        write (iuo, 1300)                                                       !/*ce        329*/
        write (iuo, 1400)                                                       !/*ce        330*/
        write (iuo, 1500)                                                       !/*ce        331*/
        write (iuo, 1600) part(1), pmul(1), dpmul(1), y(1), dy(1),              !/*ce        332*/
     &                   emean(1), pcs(1), dpcs(1)                              !/*ce        333*/
        write (iuo, 1600) part(2), pmulc(1), dpmulc(1), yc(1), dyc(1),          !/*ce        334*/
     &                   emeanc(1), pcsc(1), dpcsc(1)                           !/*ce        335*/
        write (iuo, 1600) part(3), pmulc(2), dpmulc(2), yc(2), dyc(2),          !/*ce        336*/
     &                   emeanc(2), pcsc(2), dpcsc(2)                           !/*ce        337*/
        write (iuo, 1600) part(4), pmulc(3), dpmulc(3), yc(3), dyc(3),          !/*ce        338*/
     &                   emeanc(3), pcsc(3), dpcsc(3)                           !/*ce        339*/
        write (iuo, 1500)                                                       !/*ce        340*/
        write (iuo, 1600) part(5), pmul(2), dpmul(2), y(2), dy(2),              !/*ce        341*/
     &                   emean(2), pcs(2), dpcs(2)                              !/*ce        342*/
        write (iuo, 1600) part(6), pmulc(4), dpmulc(4), yc(4), dyc(4),          !/*ce        343*/
     &                   emeanc(4), pcsc(4), dpcsc(4)                           !/*ce        344*/
        write (iuo, 1600) part(7), pmulc(5), dpmulc(5), yc(5), dyc(5),          !/*ce        345*/
     &                   emeanc(5), pcsc(5), dpcsc(5)                           !/*ce        346*/
        write (iuo, 1600) part(8), pmulc(6), dpmulc(6), yc(6), dyc(6),          !/*ce        347*/
     &                   emeanc(6), pcsc(6), dpcsc(6)                           !/*ce        348*/
        write (iuo, 1500)                                                       !/*ce        349*/
        write (iuo, 1600) part(9), pmul(3), dpmul(3), y(3), dy(3),              !/*ce        350*/
     &                   emean(3), pcs(3), dpcs(3)                              !/*ce        351*/
        write (iuo, 1600) part(10), pmulc(7), dpmulc(7), yc(7), dyc(7),         !/*ce        352*/
     &                   emeanc(7), pcsc(7), dpcsc(7)                           !/*ce        353*/
        write (iuo, 1600) part(11), pmulc(8), dpmulc(8), yc(8), dyc(8),         !/*ce        354*/
     &                   emeanc(8), pcsc(8), dpcsc(8)                           !/*ce        355*/
        write (iuo, 1500)                                                       !/*ce        356*/
        write (iuo, 1600) part(12), pmul(4), dpmul(4), y(4), dy(4),             !/*ce        357*/
     &                   emean(4), pcs(4), dpcs(4)                              !/*ce        358*/
        write (iuo, 1600) part(13), pmulc(9), dpmulc(9), yc(9), dyc(9),         !/*ce        359*/
     &                   emeanc(9), pcsc(9), dpcsc(9)                           !/*ce        360*/
        write (iuo, 1600) part(14), pmulc(10), dpmulc(10), yc(10),              !/*ce        361*/
     &                   dyc(10), emeanc(10), pcsc(10), dpcsc(10)               !/*ce        362*/
        write (iuo, 1500)                                                       !/*ce        363*/
        write (iuo, 1600) part(15), pmul(5), dpmul(5), y(5), dy(5),             !/*ce        364*/
     &                   emean(5), pcs(5), dpcs(5)                              !/*ce        365*/
        write (iuo, 1600) part(16), pmulc(11), dpmulc(11), yc(11),              !/*ce        366*/
     &                   dyc(11), emeanc(11), pcsc(11), dpcsc(11)               !/*ce        367*/
        write (iuo, 1600) part(17), pmulc(12), dpmulc(12), yc(12),              !/*ce        368*/
     &                   dyc(12), emeanc(12), pcsc(12), dpcsc(12)               !/*ce        369*/
        write (iuo, 1500)                                                       !/*ce        370*/
        write (iuo, 1600) part(18), pmul(6), dpmul(6), y(6), dy(6),             !/*ce        371*/
     &                   emean(6), pcs(6), dpcs(6)                              !/*ce        372*/
        write (iuo, 1600) part(19), pmulc(13), dpmulc(13), yc(13),              !/*ce        373*/
     &                   dyc(13), emeanc(13), pcsc(13), dpcsc(13)               !/*ce        374*/
        write (iuo, 1600) part(20), pmulc(14), dpmulc(14), yc(14),              !/*ce        375*/
     &                   dyc(14), emeanc(14), pcsc(14), dpcsc(14)               !/*ce        376*/
        write (iuo, 1500)                                                       !/*ce        377*/
        write (iuo, 1600) part(21), pmul(7), dpmul(7), y(7), dy(7),             !/*ce        378*/
     &                   emean(7), pcs(7), dpcs(7)                              !/*ce        379*/
        write (iuo, 1600) part(22), pmul(8), dpmul(8), y(8), dy(8),             !/*ce        380*/
     &                   emean(8), pcs(8), dpcs(8)                              !/*ce        381*/
        write (iuo, 1600) part(23), pmul(9), dpmul(9), y(9), dy(9),             !/*ce        382*/
     &                   emean(9), pcs(9), dpcs(9)                              !/*ce        383*/
        write (iuo, 1500)                                                       !/*ce        384*/
      endif                                                                     !/*ce        385*/
! **********************************************************************        !/*ce        386*/
!                                                                               !/*ce        387*/
!   Print out cross sections for various particle-emission channels:            !/*ce        388*/
      if (mchy.ge.0) then                                                       !/*ce        389*/
         fact = siginn                                                          !/*ce        390*/
         write (iuo, 1700)                                                      !/*ce        391*/
         m = 1                                                                  !/*ce        392*/
 530     if (m.gt.192) go to 560                                                !/*ce        393*/
         if (fact*chan(m).gt.one) then                                          !/*ce        394*/
            ef = chan(m)*fact                                                   !/*ce        395*/
            def = sqrt(abs(chan(m)))*fact                                       !/*ce        396*/
            if (m.lt.192) then                                                  !/*ce        397*/
               do 540 in = m+1,192                                              !/*ce        398*/
                  if (fact*chan(in).gt.one) go to 550                           !/*ce        399*/
 540           continue                                                         !/*ce        400*/
            endif                                                               !/*ce        401*/
            write (iuo, 1850) ch(m), ef, def                                    !/*ce        402*/
            go to 560                                                           !/*ce        403*/
 550        continue                                                            !/*ce        404*/
            n = in                                                              !/*ce        405*/
            efp = chan(n)*fact                                                  !/*ce        406*/
            defp = sqrt(abs(chan(n)))*fact                                      !/*ce        407*/
            write (iuo, 1800) ch(m), ef, def, ch(n), efp, defp                  !/*ce        408*/
            m = n + 1                                                           !/*ce        409*/
            go to 530                                                           !/*ce        410*/
         else                                                                   !/*ce        411*/
            m = m + 1                                                           !/*ce        412*/
            go to 530                                                           !/*ce        413*/
         endif                                                                  !/*ce        414*/
 560     continue                                                               !/*ce        415*/
         write (iuo, 1750)                                                      !/*ce        416*/
         do 570 ii = 1, 13                                                      !/*ce        417*/
            ij = 192 + 2*ii-1                                                   !/*ce        418*/
            ik = 192 + 2*ii                                                     !/*ce        419*/
            ef = chan(ij)*fact                                                  !/*ce        420*/
            def = sqrt(abs(chan(ij)))*fact                                      !/*ce        421*/
            efp = chan(ik)*fact                                                 !/*ce        422*/
            defp = sqrt(abs(chan(ik)))*fact                                     !/*ce        423*/
            if (ef.gt.dp1 .or. efp.gt.dp1) write (iuo, 1800)                    !/*ce        424*/
     &           ch(ij), ef, def, ch(ik), efp, defp                             !/*ce        425*/
 570     continue                                                               !/*ce        426*/
      endif                                                                     !/*ce        427*/
!                                                                               !/*ce        428*/
!   Print fission fragment statistics                                           !/*ce        429*/
      if(idel.eq.1) call fisstat(iuo)                                           !/*ce        430*/
! ======================================================================        !/*ce        431*/
  999 format(37h1CEM statistics at all physics stages,67x,                      !/*ce        432*/
     & 14hprint table 46)                                                       !/*ce        433*/
 1000 format ( /1x,'Number of inelastic interactions = ',f10.0                  !/*ce        434*/
     &         //1x,'Number of elastic interactions = ',f10.0,                  !/*ce        435*/
     &         //1x,'Reaction cross section = ',f7.2,' mb,',                    !/*ce        436*/
     &           1x,'Elastic cross section = ',f7.2,' mb.')                     !/*ce        437*/
 1010 format (/15x,f6.1,' MeV (Z = ',i2,', A = ',i1,') + (Z = ',f3.0,           !/*ce        438*/
     &      ', A = ',f4.0,')')                                                  !/*ce        439*/
 1020 format (/5x,a24)                                                          !/*ce        440*/
 1100 format (/15x,'Direct Monte Carlo Simulation Method:'/                     !/*ce        441*/
     &         1x,'Fissility = ',f7.5,' +/- ',f7.5,','/1x,'Fission ',           !/*ce        442*/
     &        'cross section = ',1pe11.5,' +/- ',e9.3,' mb.')                   !/*ce        443*/
 1200 format (/15x,'Statistical Weight Functions Method:'/                      !/*ce        444*/
     &      1x,'Fissility = ',f7.5,','/1x,'Fission cross section = ',           !/*ce        445*/
     &      1pe11.5,' mb.')                                                     !/*ce        446*/
 1210 format (/2x,'CEM-CASCADE-STAGE-ENTRY:',                                   !/*ce        447*/
     &       /2x,'Number of projectiles and their energy statistics:',          !/*ce        448*/
     &        5(/2x, a3,': events =', i7, ' E*av = ',f5.1,' +/-',               !/*ce        449*/
     &       f5.1,' MeV; E*min =',f6.1,'; E*max =',f6.1),                       !/*ce        450*/
     &       /2x,'Target charge and mass statistics:',                          !/*ce        451*/
     &       /3x,'Zav = ',f5.1,' +/- ',f4.1,';   Zmin = ',f4.0,                 !/*ce        452*/
     &       ';    Zmax = ',f4.0                                                !/*ce        453*/
     &       /3x,'Aav = ',f5.1,' +/- ',f4.1,';',3x,                             !/*ce        454*/
     &       'Amin = ',f4.0,';    Amax = ',f4.0 )                               !/*ce        455*/
 1220 format (/2x,'CEM-PHT-DEEXCITATION-ENTRY:',                                !/*ce        456*/
     &       '    number of events = ', i7,                                     !/*ce        457*/
     &       /2x,'exitation energy, charge and mass and statistics of',         !/*ce        458*/
     &       /2x,'excited nuclei:',                                             !/*ce        459*/
     &       /2x,'E*av = ',f5.1,' +/-',f5.1,                                    !/*ce        460*/
     &       ' MeV;  E*min =',f6.1,';  E*max =',f6.1,                           !/*ce        461*/
     &       /2x,' Zav = ',f5.1,' +/- ',f4.1,';        Zmin = ',f4.0,           !/*ce        462*/
     &       ';    Zmax = ',f4.0                                                !/*ce        463*/
     &       /2x,' Aav = ',f5.1,' +/- ',f4.1,';',7x,                            !/*ce        464*/
     &       'Amin = ',f4.0,';    Amax = ',f4.0 )                               !/*ce        465*/
 1240 format (/2x,'CEM-PREEQUILIBRIUM-STAGE-ENTRY:',                            !/*ce        466*/
     &        '    number of events= ',i7,                                      !/*ce        467*/
     &        /2x, 'The mean excitation energy, charge, mass, and ',            !/*ce        468*/
     &       'angular momentum'/2x,'after the ',                                !/*ce        469*/
     &       'cascade and before preequilibrium decay are:'/                    !/*ce        470*/
     &       2x,'E*av = ',f5.1,' +/-',f5.1,                                     !/*ce        471*/
     &       ' MeV;  E*min =',f5.1,';   E*max = ',f5.1,                         !/*ce        472*/
     &       /2x,' Zav = ',f5.1,' +/- ',f4.1,';       Zmin = ',f4.0,            !/*ce        473*/
     &       ';    Zmax = ',f4.0/2x,' Aav = ',f5.1,' +/- ',f4.1,';',7x,         !/*ce        474*/
     &       'Amin = ',f4.0,';    Amax = ',f4.0                                 !/*ce        475*/
     &       /2x,' Lav =  ',f4.1,' +/- ',f4.1,' h-bar; Lmin = ',f4.0,           !/*ce        476*/
     &       ';    Lmax = ',f4.0)                                               !/*ce        477*/
 1242 format (/2x,'The code entered PRECOF with Z < 3 or A < 5 ',               !/*ce        478*/
     &        i7,' times.')                                                     !/*ce        479*/
 1245 format (/2x,'The mean charge, mass, and angular momentum '/2x,            !/*ce        480*/
     &       'of the ',i7,' residual nuclei '/2x,'with less than ',             !/*ce        481*/
     &       '3 MeV of excitation energy after the cascade are:'                !/*ce        482*/
     &       /2x,' Zav = ',f5.1,' +/-',f5.1,';       Zmin = ',f4.0,             !/*ce        483*/
     &       ';   Zmax = ',f4.0/2x,' Aav = ',f5.1,' +/-',f5.1,';',7x,           !/*ce        484*/
     &       'Amin = ',f4.0,';   Amax = ',f4.0                                  !/*ce        485*/
     &       /2x,' Lav =  ',f4.1,' +/-',f5.1,' h-bar; Lmin = ',f4.0,            !/*ce        486*/
     &       ';   Lmax = ',f4.0)                                                !/*ce        487*/
 1250 format (/2x,'CEM-FISSION-STAGE-ENTRY:',                                   !/*ce        488*/
     &       '    number of events = ',i7,                                      !/*ce        489*/
     &       /2x,'The mean excitation energy, charge, mass, ',                  !/*ce        490*/
     &       'angular momentum, and '/2x,'fission barrier height are:',         !/*ce        491*/
     &       /2x,'E*av = ',f5.1,                                                !/*ce        492*/
     &       ' +/-',f5.1,' MeV;  E*min = ',f4.1,'; E*max = ',f5.1,              !/*ce        493*/
     &       /2x,' Zav = ',f5.1,' +/-',f4.1,';        Zmin = ',f4.0,            !/*ce        494*/
     &       ';  Zmax = ',f4.0/2x,' Aav = ',f5.1,' +/-',f4.1,';',8x,            !/*ce        495*/
     &       'Amin = ',f4.0,';  Amax = ',f4.0                                   !/*ce        496*/
     &       /2x,' Lav =  ',f4.1,' +/-',f4.1,' h-bar;  Lmin = ',f4.0,           !/*ce        497*/
     &       ';  Lmax = ',f4.0                                                  !/*ce        498*/
     &       /2x,'Bfav =  ',f4.1,' +/-',f4.1,' MeV;   Bfmin = ',f4.1,           !/*ce        499*/
     &       '; Bfmax = ',f4.1)                                                 !/*ce        500*/
 1260 format (/2x,'The mean kinetic energy, charge, mass, and ',                !/*ce        501*/
     &       'angular momentum '/2x,'of ',                                      !/*ce        502*/
     &       'the ',i7,' residual nuclei are:'/2x,'Ekav = ',f5.1,               !/*ce        503*/
     &       ' +/-',f5.1,' MeV;  Ekmin = ',f4.1,';  Ekmax = ',f5.1,             !/*ce        504*/
     &       /2x,' Zav = ',f5.1,' +/-',f5.1,';       Zmin = ',f4.0,             !/*ce        505*/
     &       ';   Zmax = ',f4.0/2x,' Aav = ',f5.1,' +/-',f5.1,';',7x,           !/*ce        506*/
     &       'Amin = ',f4.0,';   Amax = ',f4.0                                  !/*ce        507*/
     &       /2x,' Lav =  ',f6.1,' +/-',f6.1,' h-bar; Lmin = ',f4.0,            !/*ce        508*/
     &       ';   Lmax = ',f4.0)                                                !/*ce        509*/
 1290 format (/2x,'CEM-EQUILIBRIUM-STATISITCAL-DECAY-ENTRY: ',                  !/*ce        510*/
     &        '   number of events = ',i7,                                      !/*ce        511*/
     &       /2x,'The mean excitation energy, charge, mass, and ',              !/*ce        512*/
     &       'angular momentum'/2x,'after ',                                    !/*ce        513*/
     &       'preequilibrium decay and before the start of statistical '        !/*ce        514*/
     &       ,'decay are:'/2x,'E*av = ',f5.1,' +/- ',f4.1,' MeV; ',             !/*ce        515*/
     &       ' E*min = ',f5.1,'; E*max =',f6.1,                                 !/*ce        516*/
     &       /2x,' Zav = ',f5.1,' +/- ',f4.1,';       Zmin = ',f4.0,            !/*ce        517*/
     &       ';   Zmax = ',f4.0/2x,' Aav = ',f5.1,' +/- ',f4.1,';',7x,          !/*ce        518*/
     &       'Amin = ',f4.0,';   Amax = ',f4.0                                  !/*ce        519*/
     &       /2x,' Lav =  ',f4.1,' +/- ',f4.1,' h-bar; Lmin = ',f4.0,           !/*ce        520*/
     &       ';   Lmax = ',f4.0)                                                !/*ce        521*/
 1300 format (/1x,'Mean multiplicities, yields, mean energies and ',            !/*ce        522*/
     &       'production cross section'/1x,'of ejected particles. ',            !/*ce        523*/
     &       '(notation: T - all production mechanisms,'/1x,                    !/*ce        524*/
     &       'C - cascade, P - pre-equilibrium, E - equilibrium.) ',            !/*ce        525*/
     &       'Production cross '/1x,'sections are normalized so that ',         !/*ce        526*/
     &       'the sum of production cross sections of '                         !/*ce        527*/
     &       /1x,'all types of ejected particles with the fission ',            !/*ce        528*/
     &       'cross section is equal '/1x,'to the reaction cross ',             !/*ce        529*/
     &       'section.')                                                        !/*ce        530*/
 1400 format (/1x,'Part.',4x,'Multiplicities',8x,'Yields [mb]',                 !/*ce        531*/
     &         4x,'<T> [MeV]',1x,'Prod. cr. sec. [mb]')                         !/*ce        532*/
 1500 format (2x,'****************************************************',        !/*ce        533*/
     &      '*********************')                                            !/*ce        534*/
 1600 format (1x,a,f8.4,' +/- ',f6.4,1x,f10.3,' +/-',f7.3,1x,f6.2,              !/*ce        535*/
     &        1x,f9.3,' +/-',f7.3)                                              !/*ce        536*/
 1700 format (/1x, 'Yields of different channels (with > 1 mb):'/)              !/*ce        537*/
 1750 format (/1x, 'Yields of several summed channels (with > 1 mb):'/)         !/*ce        538*/
 1800 format (1x,a,': ',f7.2,' +/- ',f5.2,' mb,',4x,a,': ',f7.2,                !/*ce        539*/
     &        ' +/- ',f5.2,' mb,')                                              !/*ce        540*/
 1850 format (1x,a,': ',f7.2,' +/- ',f5.2,' mb.')                               !/*ce        541*/
! ======================================================================        !/*ce        542*/
#endif                                                                          /*!ce        543*/
      return                                                                    !/*ce        544*/
      end                                                                       !/*ce        545*/
