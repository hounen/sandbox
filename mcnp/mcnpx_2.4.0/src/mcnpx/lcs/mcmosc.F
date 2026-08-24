!     $Id: mcmosc.F,v 1.4 2002/03/27 18:15:11 gwm Exp $                       !/*mc          1*/
      subroutine mcmosc (atar,itinc,eoprs,eincs,noslo,itslo,eslo,alpslo         !/*mc          2*/
     & ,betslo,gamslo,efas,wtfas,alpfas,betfas,gamfas,rmfas,nofas,exfas         !/*mc          3*/
     & ,refas,why)                                                              !/*mc          4*/
! MCMOSC       calls...  CII$1  SQRT%  RTOR%  ATAN%  COSS%  %SQRT%              !/*mc          5*/
! MCMOSC                 called by...   ESKALE  SKALE                           !/*mc          6*/
#include <lcs/double.h>                                                         /*!mc          7*/
!     calculation of momentum spectra                                           !/*mc          8*/
!     for scaled-up incident particle energies                                  !/*mc          9*/
      dimension itslo(1), eslo(1), alpslo(1), betslo(1), gamslo(1), efas        !/*mc         10*/
     & (1), wtfas(1), alpfas(1), betfas(1), gamfas(1)                           !/*mc         11*/
      dimension ecmke(5), plicit(5)                                             !/*mc         12*/
      dimension weight(65), eprcm(65), array(500), brray(500)                   !/*mc         13*/
#include <lcs/mcm.h>                                                            /*!mc         14*/
      data done /dp1/                                                           !/*mc         15*/
      data bpern /7.d0/                                                         !/*mc         16*/
      pin=itinc+1                                                               !/*mc         17*/
      zkz=zkz+dp1                                                               !/*mc         18*/
      ipin=pin                                                                  !/*mc         19*/
      nofas=0                                                                   !/*mc         20*/
      why=dp0                                                                   !/*mc         21*/
      intype=itinc+1                                                            !/*mc         22*/
      if (intype-2) 10,20,30                                                    !/*mc         23*/
   10 fug(1)=dp1                                                                !/*mc         24*/
      fug(2)=dp1                                                                !/*mc         25*/
      go to 40                                                                  !/*mc         26*/
   20 fug(1)=dp1                                                                !/*mc         27*/
      fug(2)=dp1                                                                !/*mc         28*/
      go to 40                                                                  !/*mc         29*/
   30 fug(1)=dp1                                                                !/*mc         30*/
      fug(2)=dp1                                                                !/*mc         31*/
   40 continue                                                                  !/*mc         32*/
      amasno=atar                                                               !/*mc         33*/
      tarmas=(resmas(1)-7.0000001d0)*amasno                                     !/*mc         34*/
      tarmat=tarmat+tarmas                                                      !/*mc         35*/
      amasin=resmas(ipin)                                                       !/*mc         36*/
      eopr=eoprs                                                                !/*mc         37*/
      einc=eincs                                                                !/*mc         38*/
      cheinc=dp1                                                                !/*mc         39*/
      if (ipin.gt.2) cheinc=dp0                                                 !/*mc         40*/
      einct(ipin)=einct(ipin)+einc                                              !/*mc         41*/
      anumbe(ipin)=anumbe(ipin)+dp1                                             !/*mc         42*/
      einct(ipin+5)=einct(ipin+5)+eopr                                          !/*mc         43*/
      anumbe(ipin+5)=anumbe(ipin+5)+dp1                                         !/*mc         44*/
      do 50 i=1,5                                                               !/*mc         45*/
      ecmke(i)=dp0                                                              !/*mc         46*/
!     temporary storage for unscaled k.e. in c.m. used in weight calcula        !/*mc         47*/
   50 plicit(i)=dp0                                                             !/*mc         48*/
!     temporary storage for unscaled multiplicities used in weight calcu        !/*mc         49*/
      nwds=noslo*8+1                                                            !/*mc         50*/
      do 60 iii=2,nwds,8                                                        !/*mc         51*/
      m8=(iii+6)/8                                                              !/*mc         52*/
      array(iii)=itslo(m8)+1                                                    !/*mc         53*/
      array(iii+1)=eslo(m8)                                                     !/*mc         54*/
      array(iii+2)=alpslo(m8)                                                   !/*mc         55*/
      array(iii+3)=betslo(m8)                                                   !/*mc         56*/
   60 array(iii+4)=gamslo(m8)                                                   !/*mc         57*/
!     total energy of incident particle  lab                                    !/*mc         58*/
      etot=einc+amasin                                                          !/*mc         59*/
!     total energy of scaled incident particle  lab                             !/*mc         60*/
      etotpr=eopr+amasin                                                        !/*mc         61*/
!     momentum of incident particle  lab                                        !/*mc         62*/
      amoinc=sqrt(einc*(einc+dp2*amasin))                                       !/*mc         63*/
!     momentum of scaled incident particle  lab                                 !/*mc         64*/
      amoipr=sqrt(etotpr*etotpr-amasin*amasin)                                  !/*mc         65*/
!     beta for incident particle and target                                     !/*mc         66*/
      beta=amoinc/(etot+tarmas)                                                 !/*mc         67*/
!     beta for scaled incident particle and target                              !/*mc         68*/
      betapr=amoipr/(etotpr+tarmas)                                             !/*mc         69*/
!     gamma for incident particle                                               !/*mc         70*/
      gamma=dp1/sqrt(dp1-beta*beta)                                             !/*mc         71*/
!     gamma for scaled incident particle                                        !/*mc         72*/
      gampr=dp1/sqrt(dp1-betapr*betapr)                                         !/*mc         73*/
!     total energy of incident particle in c.m.                                 !/*mc         74*/
      ecm=gamma*(etot-beta*amoinc)                                              !/*mc         75*/
      recal1=ecm                                                                !/*mc         76*/
!     total energy of scaled incident particle in c.m.                          !/*mc         77*/
      ecmpr=gampr*(etotpr-betapr*amoipr)                                        !/*mc         78*/
!     total cutoff energy in lab system for the five particles                  !/*mc         79*/
!     kinetic energy of incident particle in c.m. system                        !/*mc         80*/
      ecmoke=ecm-resmas(ipin)                                                   !/*mc         81*/
      recal2=ecmoke                                                             !/*mc         82*/
!     momentum of incident particle in c.m. system                              !/*mc         83*/
      pcmo=sqrt(ecm*ecm-resmas(ipin)*resmas(ipin))                              !/*mc         84*/
      recal3=pcmo                                                               !/*mc         85*/
!     counter for number of collisions                                          !/*mc         86*/
      ecm=recal1                                                                !/*mc         87*/
      ecmoke=recal2                                                             !/*mc         88*/
      pcmo=recal3                                                               !/*mc         89*/
      do 70 iii=2,nwds                                                          !/*mc         90*/
   70 brray(iii)=array(iii)                                                     !/*mc         91*/
      excit=einc+resmas(ipin)*(dp1-cheinc)                                      !/*mc         92*/
      px=dp0                                                                    !/*mc         93*/
      py=dp0                                                                    !/*mc         94*/
      pz=amoinc                                                                 !/*mc         95*/
      pxs=dp0                                                                   !/*mc         96*/
      pys=dp0                                                                   !/*mc         97*/
      pzs=amoipr                                                                !/*mc         98*/
!     setup of initial lab momentum                                             !/*mc         99*/
      do 90 iii=2,nwds,8                                                        !/*mc        100*/
      ktype=array(iii)                                                          !/*mc        101*/
!     defines type of escaping particle                                         !/*mc        102*/
      m8=(iii+6)/8                                                              !/*mc        103*/
      array(iii+1)=array(iii+1)+resmas(ktype)                                   !/*mc        104*/
!     total energy of escaping particle                                         !/*mc        105*/
      avere(ktype)=array(iii+1)+avere(ktype)-cutl(ktype)                        !/*mc        106*/
!     energy conservation check of unscaled system in lab                       !/*mc        107*/
      yield(ktype)=yield(ktype)+dp1                                             !/*mc        108*/
!     summing up particles  for multiplicity                                    !/*mc        109*/
      plicit(ktype)=plicit(ktype)+dp1                                           !/*mc        110*/
!     multiplicity of escaping particles for this collision                     !/*mc        111*/
      p=(array(iii+1)*array(iii+1)-resmas(ktype)*resmas(ktype))**dph            !/*mc        112*/
!     momentum in lab of escaping unscaled particle                             !/*mc        113*/
      px=px+p*array(iii+2)                                                      !/*mc        114*/
      py=py+p*array(iii+3)                                                      !/*mc        115*/
      pz=pz-p*array(iii+4)                                                      !/*mc        116*/
!     three momentum components in lab of escaping unscaled particle            !/*mc        117*/
      check=dp0                                                                 !/*mc        118*/
      if (ktype.lt.3) check=dp1                                                 !/*mc        119*/
      excit=excit-array(iii+1)+resmas(ktype)*check                              !/*mc        120*/
!     preliminary calculation to obtain excitation energy                       !/*mc        121*/
      weight(m8)=gamma*(array(iii+1)-array(iii+4)*p*beta)                       !/*mc        122*/
      if (weight(m8).gt.ecm) count0=count0+dp1                                  !/*mc        123*/
!     will be used in weight calculation.                                       !/*mc        124*/
!     now contains t.e. in cm of unscaled escaping particle                     !/*mc        125*/
      if (weight(m8).gt.ecm) ecmoke=weight(m8)-resmas(ipin)                     !/*mc        126*/
      if (weight(m8).gt.ecm) pcmo=sqrt(weight(m8)**2-resmas(ipin)**2)           !/*mc        127*/
      if (weight(m8).gt.ecm) ecm=weight(m8)                                     !/*mc        128*/
      ecmke(ktype)=ecmke(ktype)+weight(m8)-resmas(ktype)                        !/*mc        129*/
!     cm ke of unscaled particles....used in calculating b                      !/*mc        130*/
      pcm=(weight(m8)*weight(m8)-resmas(ktype)*resmas(ktype))**dph              !/*mc        131*/
!     momentum in cm of unscaled escaping particle                              !/*mc        132*/
      eprcm(m8)=((weight(m8)-cutcm(ktype))/(ecm-cutcm(ipin)))**(dp1/fug         !/*mc        133*/
     & (ktype))*(ecmpr-cutcm(ipin))+cutcm(ktype)                                !/*mc        134*/
!     scaled up total energy of escaping particle in cm                         !/*mc        135*/
      ptemp=sqrt(eprcm(m8)*eprcm(m8)-resmas(ktype)*resmas(ktype))               !/*mc        136*/
!     momentum of scaled up particle in cm                                      !/*mc        137*/
      w=dp1-(p/ptemp)**2+((p/ptemp)**2)*array(iii+4)*array(iii+4)               !/*mc        138*/
      if (w.le.dp0) check8=check8+dp1                                           !/*mc        139*/
      if (w.le.dp0) w=dp0                                                       !/*mc        140*/
      if (w.le.dp0) go to 80                                                    !/*mc        141*/
      w=sqrt(w)                                                                 !/*mc        142*/
!     polar angle in cm...comes from conservation of transverse momentum        !/*mc        143*/
      wcm=gamma*(p*array(iii+4)-beta*array(iii+1))/pcm                          !/*mc        144*/
!     the sign of w is checked next                                             !/*mc        145*/
!     unscaled cm polar angle                                                   !/*mc        146*/
      if (wcm.lt.dp0) w=-w                                                      !/*mc        147*/
   80 continue                                                                  !/*mc        148*/
      array(iii+1)=gampr*(eprcm(m8)+betapr*ptemp*w)                             !/*mc        149*/
!     contains total scaled energy in lab                                       !/*mc        150*/
      ppr=(array(iii+1)*array(iii+1)-resmas(ktype)*resmas(ktype))**dph          !/*mc        151*/
!     momentum of scaled particle in lab                                        !/*mc        152*/
      array(iii+4)=gampr*(ptemp*w+betapr*eprcm(m8))/ppr                         !/*mc        153*/
!     corrected polar angle in lab to conserve transverse momentum              !/*mc        154*/
      array(iii+1)=array(iii+1)-resmas(ktype)                                   !/*mc        155*/
!     k.e. in lab of scaled particle                                            !/*mc        156*/
      pstor1=sqrt(done-array(iii+4)*array(iii+4))*cos(atan(array(iii+3)         !/*mc        157*/
     & /array(iii+2)))                                                          !/*mc        158*/
      pstor1=abs(pstor1)                                                        !/*mc        159*/
      pstor2=sqrt(done-array(iii+4)*array(iii+4))*sin(atan(array(iii+3)         !/*mc        160*/
     & /array(iii+2)))                                                          !/*mc        161*/
      pstor2=abs(pstor2)                                                        !/*mc        162*/
      if (array(iii+2).lt.dp0) pstor1=-pstor1                                   !/*mc        163*/
      if (array(iii+3).lt.dp0) pstor2=-pstor2                                   !/*mc        164*/
!     setting up modified px,py to preserve the azimuthal angle                 !/*mc        165*/
      array(iii+2)=pstor1                                                       !/*mc        166*/
      array(iii+3)=pstor2                                                       !/*mc        167*/
!     restoring corrected x and y cosines                                       !/*mc        168*/
   90 continue                                                                  !/*mc        169*/
      b=ecmoke+amasin*(dp1-cheinc)                                              !/*mc        170*/
      do 100 iiv=1,5                                                            !/*mc        171*/
  100 b=b-ecmke(iiv)+plicit(iiv)*(cutcm(iiv)-resmas(iiv))                       !/*mc        172*/
      btot=btot+b                                                               !/*mc        173*/
      blank=dp1                                                                 !/*mc        174*/
      blank1=dp1/(ecm-cheinc*amasin-b)                                          !/*mc        175*/
      do 110 j=2,nwds,8                                                         !/*mc        176*/
      ktype=array(j)                                                            !/*mc        177*/
      m9=(j+6)/8                                                                !/*mc        178*/
      if (ktype.le.2) weight(m9)=(weight(m9)-cutcm(ktype))/(eprcm(m9)           !/*mc        179*/
     & -cutcm(ktype))                                                           !/*mc        180*/
      if (ktype.gt.2) weight(m9)=weight(m9)/eprcm(m9)                           !/*mc        181*/
  110 continue                                                                  !/*mc        182*/
      amorn=tarmas+cheinc*amasin-plicit(1)*resmas(1)-plicit(2)*resmas(2)        !/*mc        183*/
     & +(plicit(1)+plicit(2))*bpern-cheinc*bpern                                !/*mc        184*/
!     mass of residual nucleus                                                  !/*mc        185*/
      if (amorn.le.dp0) why=dp1                                                 !/*mc        186*/
!     why=1  unscaled residual mass.le.0                                        !/*mc        187*/
      if (amorn.le.dp0) go to 120                                               !/*mc        188*/
      amornt=amornt+amorn                                                       !/*mc        189*/
      ern=sqrt(px*px+py*py+pz*pz+amorn*amorn)-amorn                             !/*mc        190*/
!     energy of recoiling nucleus (unscaled)                                    !/*mc        191*/
      ernt=ernt+ern                                                             !/*mc        192*/
!     summing total recoil energies for all collisions unscaled                 !/*mc        193*/
      estar=excit-ern-(plicit(1)+plicit(2)-dp1*cheinc)*bpern                    !/*mc        194*/
      estart=estart+estar                                                       !/*mc        195*/
      if (estar.lt.dp0) check7=check7+dp1                                       !/*mc        196*/
      if (estar.lt.dp0) go to 170                                               !/*mc        197*/
      go to 190                                                                 !/*mc        198*/
  120 nmlteo=nmlteo+1                                                           !/*mc        199*/
      zkz=zkz-dp1                                                               !/*mc        200*/
  130 btot=btot-b                                                               !/*mc        201*/
      do 140 iii=2,nwds,8                                                       !/*mc        202*/
      ktype=array(iii)                                                          !/*mc        203*/
      yield(ktype)=yield(ktype)-dp1                                             !/*mc        204*/
  140 avere(ktype)=avere(ktype)-brray(iii+1)+(cutl(ktype)-resmas(ktype))        !/*mc        205*/
      go to 330                                                                 !/*mc        206*/
  150 do 160 iii=2,nwds,8                                                       !/*mc        207*/
      ktype=array(iii)                                                          !/*mc        208*/
      m8=(iii+6)/8                                                              !/*mc        209*/
      yield(ktype)=yield(ktype)-dp1                                             !/*mc        210*/
      avere(ktype)=avere(ktype)-brray(iii+1)+(cutl(ktype)-resmas(ktype))        !/*mc        211*/
      yield(ktype+5)=yield(ktype+5)-weight(m8)                                  !/*mc        212*/
  160 avere(ktype+5)=avere(ktype+5)-array(iii+1)*weight(m8)+(cutl(ktype)        !/*mc        213*/
     & -resmas(ktype))*weight(m8)                                               !/*mc        214*/
      btot=btot-b                                                               !/*mc        215*/
      zkz=zkz-dp1                                                               !/*mc        216*/
      ernt=ernt-ern                                                             !/*mc        217*/
      estart=estart-estar                                                       !/*mc        218*/
      amornt=amornt-amorn                                                       !/*mc        219*/
      go to 330                                                                 !/*mc        220*/
  170 ernt=ernt-ern                                                             !/*mc        221*/
      ern=ern+estar                                                             !/*mc        222*/
      estart=estart-estar                                                       !/*mc        223*/
      ernt=ernt+ern                                                             !/*mc        224*/
      estar=dp0                                                                 !/*mc        225*/
      if (ern.lt.dp0) count5=count5+dp1                                         !/*mc        226*/
      if (ern.lt.dp0) why=2                                                     !/*mc        227*/
!     why=2  energy of unscaled recoiling nucleus.lt.0 when estar.lt.0.         !/*mc        228*/
!                 ern=ern+estar.......ern.lt.0                                  !/*mc        229*/
      if (ern.ge.dp0) go to 190                                                 !/*mc        230*/
      zkz=zkz-dp1                                                               !/*mc        231*/
      amornt=amornt-amorn                                                       !/*mc        232*/
      btot=btot-b                                                               !/*mc        233*/
      ernt=ernt-ern                                                             !/*mc        234*/
      do 180 iii=2,nwds,8                                                       !/*mc        235*/
      ktype=array(iii)                                                          !/*mc        236*/
      yield(ktype)=yield(ktype)-dp1                                             !/*mc        237*/
  180 avere(ktype)=avere(ktype)-brray(iii+1)+(cutl(ktype)-resmas(ktype))        !/*mc        238*/
      go to 330                                                                 !/*mc        239*/
  190 continue                                                                  !/*mc        240*/
!*****calculations dealing with scaled up nuclear recoil and bpr********        !/*mc        241*/
      a1=dp0                                                                    !/*mc        242*/
      a2=dp0                                                                    !/*mc        243*/
      a3=dp0                                                                    !/*mc        244*/
      a4=dp0                                                                    !/*mc        245*/
      a5=dp0                                                                    !/*mc        246*/
      do 200 iii=2,nwds,8                                                       !/*mc        247*/
      m8=(iii+6)/8                                                              !/*mc        248*/
      ktype=array(iii)                                                          !/*mc        249*/
      p=((array(iii+1)+resmas(ktype))**2-resmas(ktype)**2)**dph                 !/*mc        250*/
      a1=p*array(iii+2)*weight(m8)+a1                                           !/*mc        251*/
      a2=p*array(iii+3)*weight(m8)+a2                                           !/*mc        252*/
      a3=p*array(iii+4)*weight(m8)+a3                                           !/*mc        253*/
      a4=(array(iii+1)+resmas(ktype))*weight(m8)+a4                             !/*mc        254*/
      if (ktype.gt.dp2) go to 200                                               !/*mc        255*/
      a5=(bpern-resmas(ktype))*weight(m8)+a5                                    !/*mc        256*/
  200 continue                                                                  !/*mc        257*/
      a11=a1*a1+a2*a2+a3*a3+a5*a5-a4*a4                                         !/*mc        258*/
      b11=2.d0*((tarmas+(amasin-bpern)*cheinc)*a5-amoipr*a3+a4*(etotpr          !/*mc        259*/
     & +tarmas-estar))                                                          !/*mc        260*/
      c11=amoipr*amoipr+(tarmas+(amasin-bpern)*cheinc)**2-(etotpr+tarmas        !/*mc        261*/
     & -estar)**2                                                               !/*mc        262*/
      if (a11.eq.dp0) count1=count1+dp1                                         !/*mc        263*/
      if (a11.eq.dp0) go to 210                                                 !/*mc        264*/
      ansplu=(-b11+sqrt(b11**2-dp4*a11*c11))/(dp2*a11)                          !/*mc        265*/
      ansmin=(-b11-sqrt(b11**2-dp4*a11*c11))/(dp2*a11)                          !/*mc        266*/
      go to 220                                                                 !/*mc        267*/
  210 answer=-c11/b11                                                           !/*mc        268*/
      if (answer.le.dp0) count2=count2+dp1                                      !/*mc        269*/
      if (answer.le.dp0) why=3                                                  !/*mc        270*/
!     solution to energy balance equation is .le. 0.                            !/*mc        271*/
      if (answer.le.dp0) go to 250                                              !/*mc        272*/
      ansmin=answer                                                             !/*mc        273*/
      ansplu=answer                                                             !/*mc        274*/
!$$$$$put in check                                                              !/*mc        275*/
  220 score=dp0                                                                 !/*mc        276*/
      check=dp0                                                                 !/*mc        277*/
      do 240 i=1,2                                                              !/*mc        278*/
      a1=dp0                                                                    !/*mc        279*/
      a2=dp0                                                                    !/*mc        280*/
      a3=dp0                                                                    !/*mc        281*/
      a4=dp0                                                                    !/*mc        282*/
      a5=dp0                                                                    !/*mc        283*/
      answer=ansplu                                                             !/*mc        284*/
      if (i.eq.2) answer=ansmin                                                 !/*mc        285*/
      do 230 iii=2,nwds,8                                                       !/*mc        286*/
      m8=(iii+6)/8                                                              !/*mc        287*/
      ktype=array(iii)                                                          !/*mc        288*/
      p=((array(iii+1)+resmas(ktype))**2-resmas(ktype)**2)**dph                 !/*mc        289*/
      a6=answer*weight(m8)                                                      !/*mc        290*/
      a1=a1+p*array(iii+2)*a6                                                   !/*mc        291*/
      a2=a2+p*array(iii+3)*a6                                                   !/*mc        292*/
      a3=a3+p*array(iii+4)*a6                                                   !/*mc        293*/
      if (ktype.lt.3) a4=a4+(bpern-resmas(ktype))*a6                            !/*mc        294*/
  230 a5=a5+(array(iii+1)+resmas(ktype))*weight(m8)                             !/*mc        295*/
      a7=a1*a1+a2*a2+(amoipr-a3)**2+(tarmas+(amasin-bpern)*cheinc+a4)**2        !/*mc        296*/
      a7=sqrt(a7)                                                               !/*mc        297*/
      d1=(etotpr+tarmas-estar-a7)/a5                                            !/*mc        298*/
      sto=abs((answer-d1)/answer)                                               !/*mc        299*/
      if (i.eq.1) a8=a7                                                         !/*mc        300*/
      if (i.eq.1) stoo=sto                                                      !/*mc        301*/
      if (sto.le.1.d-05) count3=count3+dp1                                      !/*mc        302*/
      if (sto.le.1.d-05) check=i                                                !/*mc        303*/
      if (sto.le.1.d-05) score=score+dp1                                        !/*mc        304*/
  240 continue                                                                  !/*mc        305*/
      if (score.eq.dp0) why=5                                                   !/*mc        306*/
!     why=5  energy equation not satisfied                                      !/*mc        307*/
      if (score.ne.dp0) go to 260                                               !/*mc        308*/
  250 zkz=zkz-dp1                                                               !/*mc        309*/
      nttme=nttme+1                                                             !/*mc        310*/
      ernt=ernt-ern                                                             !/*mc        311*/
      estart=estart-estar                                                       !/*mc        312*/
      amornt=amornt-amorn                                                       !/*mc        313*/
      go to 130                                                                 !/*mc        314*/
  260 continue                                                                  !/*mc        315*/
      if (score.eq.dp2) go to 270                                               !/*mc        316*/
      if (check.eq.dp1) answer=ansplu                                           !/*mc        317*/
      if (answer.le.dp0) count2=count2+dp1                                      !/*mc        318*/
      if (answer.le.dp0) why=3                                                  !/*mc        319*/
      if (answer.lt.dp0) go to 250                                              !/*mc        320*/
      go to 290                                                                 !/*mc        321*/
  270 if (ansplu.gt.0.and.ansmin.gt.dp0) go to 280                              !/*mc        322*/
      if (ansplu.gt.dp0) answer=ansplu                                          !/*mc        323*/
      if (ansmin.gt.dp0) answer=ansmin                                          !/*mc        324*/
      go to 290                                                                 !/*mc        325*/
  280 if (a8.lt.a7) answer=ansplu                                               !/*mc        326*/
      if (answer.le.dp0) count2=count2+dp1                                      !/*mc        327*/
      if (answer.le.dp0) why=3                                                  !/*mc        328*/
      if (answer.le.dp0) go to 250                                              !/*mc        329*/
  290 d1=dp0                                                                    !/*mc        330*/
      do 310 iii=2,nwds,8                                                       !/*mc        331*/
      ktype=array(iii)                                                          !/*mc        332*/
      m8=(iii+6)/8                                                              !/*mc        333*/
      weight(m8)=answer*weight(m8)                                              !/*mc        334*/
      if (ktype.gt.2) go to 300                                                 !/*mc        335*/
      d1=d1+weight(m8)*resmas(ktype)-bpern*weight(m8)                           !/*mc        336*/
  300 p=((array(iii+1)+resmas(ktype))**2-resmas(ktype)**2)**dph                 !/*mc        337*/
      avere(ktype+5)=avere(ktype+5)+array(iii+1)*weight(m8)-(cutl(ktype)        !/*mc        338*/
     & -resmas(ktype))*weight(m8)                                               !/*mc        339*/
!     calculation for scaled-up energy                                          !/*mc        340*/
      yield(ktype+5)=yield(ktype+5)+weight(m8)                                  !/*mc        341*/
!     calculation for multiplicity of scaled-up particles                       !/*mc        342*/
      pxs=pxs+weight(m8)*p*array(iii+2)                                         !/*mc        343*/
      pys=pys+weight(m8)*p*array(iii+3)                                         !/*mc        344*/
  310 pzs=pzs-weight(m8)*p*array(iii+4)                                         !/*mc        345*/
      barb=tarmas+(amasin-bpern)*cheinc-d1                                      !/*mc        346*/
      if (barb.le.dp0) count4=count4+dp1                                        !/*mc        347*/
      if (barb.le.dp0) why=dp4                                                  !/*mc        348*/
!     why=4  scaled up residual mass le.0                                       !/*mc        349*/
      if (barb.le.dp0) go to 150                                                !/*mc        350*/
      ernpr=(pxs*pxs+pys*pys+pzs*pzs+(tarmas+(amasin-bpern)*cheinc-d1)**        !/*mc        351*/
     & 2)**dph-tarmas-(amasin-bpern)*cheinc+d1                                  !/*mc        352*/
      ernprt=ernpr+ernprt                                                       !/*mc        353*/
      btotpr=btotpr+ecmpr-answer/blank1-resmas(ipin)*cheinc                     !/*mc        354*/
      amrnst=amrnst+(tarmas+(amasin-bpern)*cheinc-d1)                           !/*mc        355*/
      rmfas=barb                                                                !/*mc        356*/
      refas=ernpr                                                               !/*mc        357*/
      exfas=estar                                                               !/*mc        358*/
      nofas=noslo                                                               !/*mc        359*/
!*****end calculations dealing with scaled up nuclear recoil and bpr****        !/*mc        360*/
      do 320 iii=2,nwds,8                                                       !/*mc        361*/
      m8=(iii+6)/8                                                              !/*mc        362*/
      efas(m8)=array(iii+1)                                                     !/*mc        363*/
      alpfas(m8)=array(iii+2)                                                   !/*mc        364*/
      betfas(m8)=array(iii+3)                                                   !/*mc        365*/
      gamfas(m8)=array(iii+4)                                                   !/*mc        366*/
  320 wtfas(m8)=weight(m8)                                                      !/*mc        367*/
      return                                                                    !/*mc        368*/
!     defines incident particle                                                 !/*mc        369*/
  330 einct(ipin)=einct(ipin)-einc                                              !/*mc        370*/
      einct(ipin+5)=einct(ipin+5)-eopr                                          !/*mc        371*/
      anumbe(ipin)=anumbe(ipin)-dp1                                             !/*mc        372*/
      anumbe(ipin+5)=anumbe(ipin+5)-dp1                                         !/*mc        373*/
      tarmat=tarmat-tarmas                                                      !/*mc        374*/
      return                                                                    !/*mc        375*/
      end                                                                       !/*mc        376*/
