!     $Id: pht.F,v 1.4 2002/03/27 18:15:15 gwm Exp $                          !/*ph          1*/
      subroutine pht (at,zt,e,ecv,eph,elv,tph,nph,maxph,icc0)                   !/*ph          2*/
#include <lcs/double.h>                                                         /*!ph          3*/
      external phbd                                                             !/*ph          4*/
      dimension eph(maxph), elv(maxph), tph(maxph)                              !/*ph          5*/
#include <lcs/phtcom.h>                                                         /*!ph          6*/
#include <lcs/phtdat.h>                                                         /*!ph          7*/
#include <lcs/inout.h>                                                          /*!ph          8*/
      dimension ps(500)                                                         !/*ph          9*/
      parameter (llev=150)                                                      !/*ph         10*/
      parameter (llev1=llev+1)                                                  !/*ph         11*/
      dimension ip(llev1), ij(llev1)                                            !/*ph         12*/
      dimension pslog(500)                                                      !/*ph         13*/
!=======================================================================        !/*ph         14*/
!     icc0, icalc, etc                                                          !/*ph         15*/
! icalc=0, ground state + continuum above pairing gap; E1 transitions           !/*ph         16*/
! icalc=1, all levels + continuum above top level; E1 transitions               !/*ph         17*/
! icalc=2, only transitions between levels in range where all spin/             !/*ph         18*/
!     parity is known use Weisskopf probabilitits                               !/*ph         19*/
!                                                                               !/*ph         20*/
! icalc=3, spin assignment where unknown,                                       !/*ph         21*/
! ncont=2, continuum-to-continuum and continuum-to-level transitions            !/*ph         22*/
! ncont=1, continuum-to-level transitions only                                  !/*ph         23*/
! ncont=0, level-to-level transitions only                                      !/*ph         24*/
!=======================================================================        !/*ph         25*/
!DBG                                                                            !/*ph         26*/
      e00=e                                                                     !/*ph         27*/
      nocas=igetd(1)                                                            !/*ph         28*/
!DBG                                                                            !/*ph         29*/
      icalc=icc0                                                                !/*ph         30*/
      ecv=dp0                                                                   !/*ph         31*/
      iiz=zt                                                                    !/*ph         32*/
      iia=at                                                                    !/*ph         33*/
      iiza=1000*iiz+iia                                                         !/*ph         34*/
      jn=iia-iiz                                                                !/*ph         35*/
      tp=dp0                                                                    !/*ph         36*/
      tcon=1.0d-11                                                              !/*ph         37*/
      it0=0                                                                     !/*ph         38*/
      itop=0                                                                    !/*ph         39*/
      nlev=0                                                                    !/*ph         40*/
      lvls=0                                                                    !/*ph         41*/
      e11=dp0                                                                   !/*ph         42*/
      ncont=2                                                                   !/*ph         43*/
      k1=iref(iiz)                                                              !/*ph         44*/
      eminn=dp0                                                                 !/*ph         45*/
      k2=iref(iiz+1)-1                                                          !/*ph         46*/
      if (k2.ge.k1) then                                                        !/*ph         47*/
        do 10 k=k1,k2                                                           !/*ph         48*/
        if (iiza.eq.izao(k)) go to 20                                           !/*ph         49*/
   10   continue                                                                !/*ph         50*/
      endif                                                                     !/*ph         51*/
! iiza NOT in library                                                           !/*ph         52*/
      if (e.le.0.) go to 250                                                    !/*ph         53*/
      icalc=0                                                                   !/*ph         54*/
      ik=0                                                                      !/*ph         55*/
      go to 50                                                                  !/*ph         56*/
! iiza in library                                                               !/*ph         57*/
   20 continue                                                                  !/*ph         58*/
      ik=k                                                                      !/*ph         59*/
      nup=ipt(ik+1)-1                                                           !/*ph         60*/
      nlo=ipt(ik)                                                               !/*ph         61*/
      if (e.le.elo(nlo)) then                                                   !/*ph         62*/
        write (itty,*) ' warning (PHT-1) at,zt,e,elo(nlo)'                      !/*ph         63*/
        write (itty,*) at,zt,e,elo(nlo)                                         !/*ph         64*/
        write (io,*) ' warning (PHT-1) at,zt,e,elo(nlo)'                        !/*ph         65*/
        write (io,*) at,zt,e,elo(nlo)                                           !/*ph         66*/
        go to 250                                                               !/*ph         67*/
      endif                                                                     !/*ph         68*/
      nlev=nup-nlo+1                                                            !/*ph         69*/
      itop=nup                                                                  !/*ph         70*/
      if (icalc.le.0) then                                                      !/*ph         71*/
        if (nlev.gt.1) eminn=elo(nlo+1)                                         !/*ph         72*/
        ik=0                                                                    !/*ph         73*/
        go to 50                                                                !/*ph         74*/
      endif                                                                     !/*ph         75*/
      eminn=elo(nup)                                                            !/*ph         76*/
      do 30 i=nlo,nup                                                           !/*ph         77*/
      itop=i                                                                    !/*ph         78*/
      if (e.le.elo(i)) go to 40                                                 !/*ph         79*/
   30 continue                                                                  !/*ph         80*/
      itop=nup                                                                  !/*ph         81*/
      go to 50                                                                  !/*ph         82*/
   40 continue                                                                  !/*ph         83*/
      itop=itop-1                                                               !/*ph         84*/
!rep                                                                            !/*ph         85*/
!     p1=(e-elo(it0-1))/(elo(it0)-elo(it0-1))                                   !/*ph         86*/
!     if (rang().gt.p1) it0=it0-1                                               !/*ph         87*/
!     e=elo(it0)                                                                !/*ph         88*/
!     if (itop.eq.nup) go to 50                                                 !/*ph         89*/
!     if (icalc.lt.3) then                                                      !/*ph         90*/
!       ncont=1                                                                 !/*ph         91*/
!       go to 60                                                                !/*ph         92*/
!     endif                                                                     !/*ph         93*/
!rep                                                                            !/*ph         94*/
   50 continue                                                                  !/*ph         95*/
      is=1                                                                      !/*ph         96*/
      if ((iiz.ge.54.and.iiz.le.78).or.(iiz.ge.86.and.iiz.le.122).or.(jn        !/*ph         97*/
     & .ge.86.and.jn.le.122).or.(jn.ge.130.and.jn.le.182)) is=2                 !/*ph         98*/
      call plev (e,iiz,iia,0)                                                   !/*ph         99*/
      eminn=max(de,eminn)                                                       !/*ph        100*/
      if (e.le.eminn) ncont=1                                                   !/*ph        101*/
      if (icalc.lt.2) go to 70                                                  !/*ph        102*/
      if (icalc.gt.2.and.ncont.ne.0) then                                       !/*ph        103*/
        ipar0=1                                                                 !/*ph        104*/
        if (rang().gt.dph) ipar0=-1                                             !/*ph        105*/
        isp0=mspin(iia,e)                                                       !/*ph        106*/
      endif                                                                     !/*ph        107*/
      do 60 i=nlo,itop                                                          !/*ph        108*/
      j=i-nlo+1                                                                 !/*ph        109*/
      ip(j)=sign(1,isp(i))                                                      !/*ph        110*/
      ij(j)=abs(isp(i))                                                         !/*ph        111*/
      if (ij(j).eq.0) ip(j)=0                                                   !/*ph        112*/
      if (icalc.lt.3) then                                                      !/*ph        113*/
        if (j.gt.1.and.ip(j-1).eq.0) then                                       !/*ph        114*/
          ip(j)=0                                                               !/*ph        115*/
          ij(j)=0                                                               !/*ph        116*/
        endif                                                                   !/*ph        117*/
      else                                                                      !/*ph        118*/
        if (ip(j).eq.0) then                                                    !/*ph        119*/
          ip(j)=1                                                               !/*ph        120*/
          if (rang().gt.dph) ip(j)=-ip(j)                                       !/*ph        121*/
          epp=elo(i)                                                            !/*ph        122*/
          ij(j)=mspin(iia,epp)                                                  !/*ph        123*/
        endif                                                                   !/*ph        124*/
      endif                                                                     !/*ph        125*/
   60 continue                                                                  !/*ph        126*/
!      re-entry point for more cascade                                          !/*ph        127*/
   70 continue                                                                  !/*ph        128*/
      nph=nph+1                                                                 !/*ph        129*/
      if (nph.ge.maxph) then                                                    !/*ph        130*/
        write (itty,*) ' warning (PHT-2) nph > maxph'                           !/*ph        131*/
        write (io,*) ' warning (PHT-2) nph > maxph'                             !/*ph        132*/
        go to 240                                                               !/*ph        133*/
      endif                                                                     !/*ph        134*/
      if (ik.le.0) then                                                         !/*ph        135*/
        if (e.gt.dp0) then                                                      !/*ph        136*/
          pslog(1)=dp3*log10(e)                                                 !/*ph        137*/
        else                                                                    !/*ph        138*/
          pslog(1)=-1.0d+30                                                     !/*ph        139*/
        endif                                                                   !/*ph        140*/
        lvls=1                                                                  !/*ph        141*/
        go to 150                                                               !/*ph        142*/
      endif                                                                     !/*ph        143*/
      if (it0.ne.0.and.it0.eq.nlo) then                                         !/*ph        144*/
        go to 230                                                               !/*ph        145*/
      endif                                                                     !/*ph        146*/
      if (ncont.gt.0) then                                                      !/*ph        147*/
        if (icalc.ge.3) go to 110                                               !/*ph        148*/
        go to 130                                                               !/*ph        149*/
      endif                                                                     !/*ph        150*/
! ncont=0 only                                                                  !/*ph        151*/
   80 continue                                                                  !/*ph        152*/
      tcon=tlev(it0)*1.0e+09                                                    !/*ph        153*/
      klo=idk(it0)                                                              !/*ph        154*/
      khi=idk(it0+1)-1                                                          !/*ph        155*/
      ndk=khi-klo+1                                                             !/*ph        156*/
      if (ndk.le.0) go to 110                                                   !/*ph        157*/
      test=rang()                                                               !/*ph        158*/
      do 90 kd=klo,khi                                                          !/*ph        159*/
      test=test-mod(pmode(kd),dp1)                                              !/*ph        160*/
      if (test.le.dp0) go to 100                                                !/*ph        161*/
   90 continue                                                                  !/*ph        162*/
      kd=khi                                                                    !/*ph        163*/
  100 continue                                                                  !/*ph        164*/
      mode=int(pmode(kd))                                                       !/*ph        165*/
      if (mode.eq.7) then                                                       !/*ph        166*/
        if (icalc.le.3) go to 110                                               !/*ph        167*/
        it0=nlo+lfin(kd)                                                        !/*ph        168*/
        e11=elo(it0)                                                            !/*ph        169*/
        if (it0.eq.nlo) go to 230                                               !/*ph        170*/
        itop=it0-1                                                              !/*ph        171*/
        go to 220                                                               !/*ph        172*/
      endif                                                                     !/*ph        173*/
      if (mode.eq.17) then                                                      !/*ph        174*/
        it0=nlo+lfin(kd)                                                        !/*ph        175*/
        e11=elo(it0)                                                            !/*ph        176*/
        ecv=ecv+(e-e11)                                                         !/*ph        177*/
        e=e11                                                                   !/*ph        178*/
        if (it0.eq.nlo) then                                                    !/*ph        179*/
          go to 240                                                             !/*ph        180*/
        endif                                                                   !/*ph        181*/
        itop=it0-1                                                              !/*ph        182*/
        go to 80                                                                !/*ph        183*/
      endif                                                                     !/*ph        184*/
      if (mode.eq.19) go to 110                                                 !/*ph        185*/
      if (mode.le.6.and.tlev(it0).le.1.0d-12) go to 110                         !/*ph        186*/
      go to 240                                                                 !/*ph        187*/
  110 continue                                                                  !/*ph        188*/
      if (icalc.eq.1) go to 130                                                 !/*ph        189*/
      if (ncont.gt.0) then                                                      !/*ph        190*/
        if (icalc.le.2) go to 130                                               !/*ph        191*/
      else                                                                      !/*ph        192*/
        jt0=it0-nlo+1                                                           !/*ph        193*/
        if (ip(jt0).eq.0) go to 130                                             !/*ph        194*/
        ipar0=ip(jt0)                                                           !/*ph        195*/
        isp0=ij(jt0)                                                            !/*ph        196*/
      endif                                                                     !/*ph        197*/
      psum=dp0                                                                  !/*ph        198*/
      lvls=0                                                                    !/*ph        199*/
      do 120 j=nlo,itop                                                         !/*ph        200*/
      i=j-nlo+1                                                                 !/*ph        201*/
      isp1=ij(i)                                                                !/*ph        202*/
      ipar1=ip(i)                                                               !/*ph        203*/
      j0=abs(isp1-isp0)/2                                                       !/*ph        204*/
      if (j0.eq.0) j0=1                                                         !/*ph        205*/
      iparx=ipar0*ipar1                                                         !/*ph        206*/
      if (mod(j0,2).ne.0) iparx=-iparx                                          !/*ph        207*/
      lvls=lvls+1                                                               !/*ph        208*/
      egam=e-elo(j)                                                             !/*ph        209*/
      temp=-1.0d+30                                                             !/*ph        210*/
      if (egam.gt.dp0) temp=trns(egam,j0,iparx,iia)                             !/*ph        211*/
      if ((isp1+isp0).eq.2) temp=temp-dp10                                      !/*ph        212*/
      pslog(lvls)=temp                                                          !/*ph        213*/
  120 continue                                                                  !/*ph        214*/
      if (icalc.ge.3) go to 150                                                 !/*ph        215*/
      go to 160                                                                 !/*ph        216*/
  130 continue                                                                  !/*ph        217*/
      psum=dp0                                                                  !/*ph        218*/
      lvls=0                                                                    !/*ph        219*/
      do 140 j=nlo,itop                                                         !/*ph        220*/
      lvls=lvls+1                                                               !/*ph        221*/
      temp=-1.0d+30                                                             !/*ph        222*/
      if (e.gt.elo(j)) temp=dp3*log10(e-elo(j))                                 !/*ph        223*/
      pslog(lvls)=temp                                                          !/*ph        224*/
  140 continue                                                                  !/*ph        225*/
  150 continue                                                                  !/*ph        226*/
!=======================================================================        !/*ph        227*/
! Add continuum-to-continuum probability                                        !/*ph        228*/
!                                                                               !/*ph        229*/
      if(ncont.eq.2.and.(e-eminn).lt.0.001d0) ncont=1                           !/*ph        230*/
      if (ncont.eq.2) then                                                      !/*ph        231*/
        temp=pgs(e,eminn)                                                       !/*ph        232*/
        if (icalc.ge.3) temp=temp+log10((dph+10.0*(c2/cra(iia))**2)*fmax        !/*ph        233*/
     &  (e))                                                                    !/*ph        234*/
        if (temp.le.-1.0d+20) then                                              !/*ph        235*/
          ncont=1                                                               !/*ph        236*/
        else                                                                    !/*ph        237*/
          lvls=lvls+1                                                           !/*ph        238*/
          pslog(lvls)=temp                                                      !/*ph        239*/
        endif                                                                   !/*ph        240*/
      endif                                                                     !/*ph        241*/
  160 continue                                                                  !/*ph        242*/
      if (lvls.eq.1) go to 230                                                  !/*ph        243*/
      pslm=pslog(1)                                                             !/*ph        244*/
      do 170 j=2,lvls                                                           !/*ph        245*/
  170 pslm=max(pslm,pslog(j))                                                   !/*ph        246*/
      psum=dp0                                                                  !/*ph        247*/
      do 180 j=1,lvls                                                           !/*ph        248*/
      temp=pslog(j)-pslm                                                        !/*ph        249*/
      temp1=dp0                                                                 !/*ph        250*/
      if (temp.gt.-dp10) temp1=dp10**temp                                       !/*ph        251*/
      psum=psum+temp1                                                           !/*ph        252*/
      ps(j)=psum                                                                !/*ph        253*/
  180 continue                                                                  !/*ph        254*/
      irep=0                                                                    !/*ph        255*/
      if (lvls.eq.1) go to 230                                                  !/*ph        256*/
      ratio=ps(lvls-1)/ps(lvls)                                                 !/*ph        257*/
      if (ratio.le.1.0d-05) irep=1                                              !/*ph        258*/
  190 continue                                                                  !/*ph        259*/
      ptest=ps(lvls)*rang()                                                     !/*ph        260*/
      do 200 ll=1,lvls                                                          !/*ph        261*/
      if (ptest.le.ps(ll)) go to 210                                            !/*ph        262*/
  200 continue                                                                  !/*ph        263*/
      ll=lvls                                                                   !/*ph        264*/
  210 continue                                                                  !/*ph        265*/
      if (ll.eq.1) then                                                         !/*ph        266*/
        it0=nlo                                                                 !/*ph        267*/
        go to 230                                                               !/*ph        268*/
      endif                                                                     !/*ph        269*/
      if (ncont.eq.2.and.ll.eq.lvls) then                                       !/*ph        270*/
        e11=skeme(e,eminn)                                                      !/*ph        271*/
        if (e11.lt.eminn) then                                                  !/*ph        272*/
          call errorf ('pht.1')                                                 !/*ph        273*/
        endif                                                                   !/*ph        274*/
        if (icalc.lt.3) go to 220                                               !/*ph        275*/
        call rejgam (e)                                                         !/*ph        276*/
        if (iccc.ne.0) go to 220                                                !/*ph        277*/
        go to 190                                                               !/*ph        278*/
      endif                                                                     !/*ph        279*/
      it0=nlo+ll-1                                                              !/*ph        280*/
      e11=elo(it0)                                                              !/*ph        281*/
      itop=it0-1                                                                !/*ph        282*/
      ncont=0                                                                   !/*ph        283*/
  220 continue                                                                  !/*ph        284*/
      if (e11.eq.dp0) then                                                      !/*ph        285*/
        ll=it0-nlo+1                                                            !/*ph        286*/
        write(itty,*)                                                           !/*ph        287*/
     &   ' warning (PHT-3) iiza,icalc,nlev,ll,e,e11,elo(it0)'                   !/*ph        288*/
        write(itty,*) iiza,icalc,nlev,ll,e,e11,elo(it0)                         !/*ph        289*/
        write(io,*) ' warning (PHT-3) iiza,icalc,nlev,ll,e,e11,elo(it0)'        !/*ph        290*/
        write(io,*) iiza,icalc,nlev,ll,e,e11,elo(it0)                           !/*ph        291*/
        go to 230                                                               !/*ph        292*/
      endif                                                                     !/*ph        293*/
      tp=tp-tcon*log(rang())                                                    !/*ph        294*/
      tph(nph)=tp                                                               !/*ph        295*/
      eph(nph)=e-e11                                                            !/*ph        296*/
      elv(nph)=e11                                                              !/*ph        297*/
      e=e11                                                                     !/*ph        298*/
      go to 70                                                                  !/*ph        299*/
  230 continue                                                                  !/*ph        300*/
! Ground state                                                                  !/*ph        301*/
      e11=dp0                                                                   !/*ph        302*/
      tp=tp-tcon*log(rang())                                                    !/*ph        303*/
      tph(nph)=tp                                                               !/*ph        304*/
      eph(nph)=e-e11                                                            !/*ph        305*/
      if (eph(nph).eq.dp0) then                                                 !/*ph        306*/
         write (itty,*) ' warning (PHT-4) iiza,icalc,nlev,e,e11'                !/*ph        307*/
         write (itty,*) iiza,icalc,nlev,e,e11                                   !/*ph        308*/
         write (io,*) ' warning (PHT-4) iiza,icalc,nlev,e,e11'                  !/*ph        309*/
         write (io,*) iiza,icalc,nlev,e,e11                                     !/*ph        310*/
      endif                                                                     !/*ph        311*/
      elv(nph)=e11                                                              !/*ph        312*/
      e=e11                                                                     !/*ph        313*/
      go to 250                                                                 !/*ph        314*/
  240 continue                                                                  !/*ph        315*/
      nph=nph-1                                                                 !/*ph        316*/
  250 continue                                                                  !/*ph        317*/
      return                                                                    !/*ph        318*/
!                                                                               !/*ph        319*/
      end                                                                       !/*ph        320*/
