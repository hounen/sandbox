!     $Id: interact.F,v 1.11 2002/03/27 18:15:08 gwm Exp $                    !/*in          1*/
      subroutine interact(nbeta,ii,ec,emn,icell,med,namex,tc,u,v,w,wt,          !/*in          2*/
     & xc,yc,zc,nel,mel,a9,z9,iazdex,fmf,rho,pcap,hsigg,xsiso,esxlp,            !/*in          3*/
#ifdef HISTP                                                                    /*!in          4*/
     & m_npstc,                                                                 !/*in          5*/
#endif                                                                          /*!in          6*/
#ifdef MESHTAL                                                                  /*!in          7*/
     & f_rc,f_nt,                                                               !/*in          8*/
#endif                                                                          /*!in          9*/
     & lter)                                                                    !/*in         10*/
!     <<< MESHTAL:  E.C.Snow 02/18/99. Revised 04/26/2001 >>>                   !/*in         11*/
!=======================================================================        !/*in         12*/
! NBETA:  interaction flag                                                      !/*in         13*/
! EC:     collision energy                                                      !/*in         14*/
! EMN:    minimum energy for the current particle                               !/*in         15*/
! ICELL:  cell number                                                           !/*in         16*/
! MED:    material number                                                       !/*in         17*/
! NAME:   (not used in MCNP)                                                    !/*in         18*/
! SFAC:   (not used in MCNP)                                                    !/*in         19*/
! TC:     collision time                                                        !/*in         20*/
! U,V,W:  direction cosines                                                     !/*in         21*/
! WT:     weight                                                                !/*in         22*/
! XC,YC,ZC: collision coordinates (not used in MCNP)                            !/*in         23*/
!=======================================================================        !/*in         24*/
#include <lcs/double.h>                                                         /*!in         25*/
#include <lcs/lptS.h>                                                           /*!in         26*/
#include <lcs/rucom.h>                                                          /*!in         27*/
#include <lcs/banka.h>                                                          /*!in         28*/
#include <lcs/bankb.h>                                                          /*!in         29*/
#include <lcs/elstic.h>                                                         /*!in         30*/
#include <lcs/concom.h>                                                         /*!in         31*/
#include <lcs/inout.h>                                                          /*!in         32*/
#include <lcs/charla.h>                                                         /*!in         33*/
#include <lcs/komon.h>                                                          /*!in         34*/
#include <globals/interact_outcomes.h>                                          /*!in         35*/
      logical lmc                                                               !/*in         36*/
      logical lrevrs                                                            !/*in         37*/
      dimension nel(1), mel(1), a9(1), z9(1), iazdex(1), fmf(1), pcap(1)        !/*in         38*/
     & , rho(1)                                                                 !/*in         39*/
      dimension hsigg(lpt,1), xsiso(2,1), esxlp(iemaxp,1)                       !/*in         40*/
      dimension itt(lpt)                                                        !/*in         41*/
      dimension bar(lpt)                                                        !/*in         42*/
      data bar /2*dp1,5*dp0,dp2,2*dp3,dp4,5*dp0,-1.d0,-1.d0,4*dp0/              !/*in         43*/
      data itt /5*1,0,0,3,4,5,6,0,4*1,2,2,4*0/                                  !/*in         44*/
#ifdef MESHTAL                                                                  /*!in         45*/
      dimension ika(22)                                                         !/*in         46*/
      data ika /1,0,2,0,2,3,3,6,7,8,9,0,4,0,0,4,5,0,13,13,0,0/                  !/*in         47*/
      data elmass /0.511008d0/                                                  !/*in         48*/
#endif                                                                          /*!in         49*/
!=======================================================================        !/*in         50*/
!----- decay, slowing down, or interaction                                      !/*in         51*/
! ind = 1 error exit                                                            !/*in         52*/
! ind = 2 escape system                                                         !/*in         53*/
! ind = 3 internal surface crossing                                             !/*in         54*/
! ind = 4 substep, slowing down, or collision                                   !/*in         55*/
! nbeta = 1 collision                                                           !/*in         56*/
! nbeta = 2 slowing down                                                        !/*in         57*/
! nbeta = 3 source or bank particle below cutoff                                !/*in         58*/
! lter = 1 real nonelastic collision (terminal)                                 !/*in         59*/
! lter = 2 real elastic collision (nonterminal)                                 !/*in         60*/
! lter = 3 true pseudocollision (nonterminal)                                   !/*in         61*/
! lter = 4 full decay pseudocollision (terminal)                                !/*in         62*/
! lter = 5 partial decay pseudocollision (nonterminal)                          !/*in         63*/
! lter = 6 simple stopped particle (terminal)                                   !/*in         64*/
      ityp=ii                                                                   !/*in         65*/
      if (noact.eq.2.and.lmxs(ityp).ne.0) then                                  !/*in         66*/
         call expirx(1,'INTERACT','improper use of noact=2')                    !/*in         67*/
      endif                                                                     !/*in         68*/
      oldwt=wt                                                                  !/*in         69*/
#ifdef LCS                                                                      /*!in         70*/
      nocas=igetd(1)                                                            !/*in         71*/
#endif                                                                          /*!in         72*/
#ifdef HISTP                                                                    /*!in         73*/
      nocas=m_npstc                                                             !/*in         74*/
#endif                                                                          /*!in         75*/
#ifndef LCS                                                                     /*!in         76*/
      name=1                                                                    !/*in         77*/
      wt7=wt                                                                    !/*in         78*/
      tip7=ityp-1                                                               !/*in         79*/
      name7=name                                                                !/*in         80*/
      ibold7=icell                                                              !/*in         81*/
      iblz7=icell                                                               !/*in         82*/
      x7=xc                                                                     !/*in         83*/
      xc7=xc                                                                    !/*in         84*/
      y7=yc                                                                     !/*in         85*/
      yc7=yc                                                                    !/*in         86*/
      z7=zc                                                                     !/*in         87*/
      zc7=zc                                                                    !/*in         88*/
      e7=ec                                                                     !/*in         89*/
      ec7=ec                                                                    !/*in         90*/
      tc7=tc                                                                    !/*in         91*/
      u7=u                                                                      !/*in         92*/
      v7=v                                                                      !/*in         93*/
      w7=w                                                                      !/*in         94*/
      mtl=med                                                                   !/*in         95*/
      nmed7=med                                                                 !/*in         96*/
#endif                                                                          /*!in         97*/
      call set_i                                                                !/*in         98*/
      lter = 0                                                                  !/*in         99*/
      ielast=0                                                                  !/*in        100*/
      nopart=-1                                                                 !/*in        101*/
#ifdef MESHTAL                                                                  /*!in        102*/
      f_rc=dp0                                                                  !/*in        103*/
      f_nt=dp0                                                                  !/*in        104*/
#endif                                                                          /*!in        105*/
      if (n1col.eq.-1) go to 50                                                 !/*in        106*/
      go to (20,120,10), nbeta                                                  !/*in        107*/
!=======================================================================        !/*in        108*/
!----- particle from source or bank below cutoff                                !/*in        109*/
   10 continue                                                                  !/*in        110*/
      if (instab(ityp).ne.0) go to 120                                          !/*in        111*/
      if (ityp.eq.17) go to 20                                                  !/*in        112*/
      lter = id_stopped_particle_terminal                                       !/*in        113*/
      go to 200                                                                 !/*in        114*/
!=======================================================================        !/*in        115*/
!----- interaction or decay in material                                         !/*in        116*/
   20 continue                                                                  !/*in        117*/
      wt=oldwt                                                                  !/*in        118*/
      if (instab(ityp).eq.0) go to 50                                           !/*in        119*/
      if (ityp.eq.5.and.ec.le.emn.and.npidk.lt.0) go to 40                      !/*in        120*/
      if (med.eq.0) go to 30                                                    !/*in        121*/
      temp=totm*rho(icell)+sigdk                                                !/*in        122*/
      r1=rang()                                                                 !/*in        123*/
      if (r1*temp.lt.sigdk) go to 30                                            !/*in        124*/
      go to 50                                                                  !/*in        125*/
   30 continue                                                                  !/*in        126*/
      if (charge(ityp).eq.dp0.or.med.eq.0.or.ec.lt.edkmin) then                 !/*in        127*/
        sigdk0=sigdk                                                            !/*in        128*/
        wt=dp0                                                                  !/*in        129*/
      else                                                                      !/*in        130*/
        sigdk0=dkcons(ityp)/sqrt((ec/pmass(ityp)+dp1)**2-dp1)                   !/*in        131*/
        r2=rang()                                                               !/*in        132*/
        if (r2*sigdk.le.sigdk0) wt=dp0                                          !/*in        133*/
      endif                                                                     !/*in        134*/
      if (wt.lt.oldwt) then                                                     !/*in        135*/
        call mfpd2 (ec,icell,tc,u,v,w,xc,yc,zc,wt)                              !/*in        136*/
        if (wt.eq.dp0) then                                                     !/*in        137*/
          lter = id_fulldecay_pseudo_terminal                                   !/*in        138*/
        else                                                                    !/*in        139*/
          lter = id_partdecay_pseudo_nonterminal                                !/*in        140*/
        endif                                                                   !/*in        141*/
      else                                                                      !/*in        142*/
        lter = id_true_pseudo_nonterminal                                       !/*in        143*/
      endif                                                                     !/*in        144*/
      go to 140                                                                 !/*in        145*/
!=======================================================================        !/*in        146*/
!----- interaction chosen                                                       !/*in        147*/
   40 continue                                                                  !/*in        148*/
!----- pi- capture                                                              !/*in        149*/
      call set_pcap (ec,med,nel(1),mel(1),a9(1),pcap(1))                        !/*in        150*/
      nopart=-1                                                                 !/*in        151*/
      lmc=.false.                                                               !/*in        152*/
      if (lelem.gt.0) then                                                      !/*in        153*/
         mel1=mel(med)-1+iexp                                                   !/*in        154*/
         a=a9(mel1)                                                             !/*in        155*/
         z=z9(mel1)                                                             !/*in        156*/
         fmf_=fmf(mel1)                                                         !/*in        157*/
#ifndef OLDM                                                                    /*!in        158*/
         lmc=.true.                                                             !/*in        159*/
#endif                                                                          /*!in        160*/
         go to 90                                                               !/*in        161*/
      else                                                                      !/*in        162*/
         oldwt=wt                                                               !/*in        163*/
         go to 130                                                              !/*in        164*/
      endif                                                                     !/*in        165*/
!=======================================================================        !/*in        166*/
!----- select target                                                            !/*in        167*/
   50 continue                                                                  !/*in        168*/
      if (ityp.eq.17.and.ec.le.emn) then                                        !/*in        169*/
#ifndef OLDM                                                                    /*!in        170*/
         lmc=.true.                                                             !/*in        171*/
#else                                                                           /*!in        172*/
         lmc=.false.                                                            !/*in        173*/
#endif                                                                          /*!in        174*/
      elseif (ityp.ge.8.and.ityp.le.11) then                                    !/*in        175*/
         lmc=.false.                                                            !/*in        176*/
      elseif (lmxs(ityp).eq.0) then                                             !/*in        177*/
         lmc=.true.                                                             !/*in        178*/
      else                                                                      !/*in        179*/
         lmc=.false.                                                            !/*in        180*/
      endif                                                                     !/*in        181*/
      geomxs=totm                                                               !/*in        182*/
      lelem=0                                                                   !/*in        183*/
      nel1=nel(med)                                                             !/*in        184*/
      r=totm*rang()                                                             !/*in        185*/
      do 60 i=1,nel1                                                            !/*in        186*/
      r=r-xsiso(1,i)                                                            !/*in        187*/
      if (r.le.dp0) then                                                        !/*in        188*/
        iexp=i                                                                  !/*in        189*/
        go to 70                                                                !/*in        190*/
      endif                                                                     !/*in        191*/
   60 continue                                                                  !/*in        192*/
      iexp=nel1                                                                 !/*in        193*/
   70 continue                                                                  !/*in        194*/
      mel1=mel(med)-1+iexp                                                      !/*in        195*/
      a=a9(mel1)                                                                !/*in        196*/
      z=z9(mel1)                                                                !/*in        197*/
      fmf_=fmf(mel1)                                                            !/*in        198*/
      lelem=iexp                                                                !/*in        199*/
      if (a.eq.dp1) lelem=-1                                                    !/*in        200*/
      if (lmc) then                                                             !/*in        201*/
        if (ityp.eq.17.and.ec.le.emn) go to 90                                  !/*in        202*/
        if (charge(ityp).ne.dp0) then                                           !/*in        203*/
          call xs_real (ityp,ec,a,z,fmf_,xs_abs,xs_els,xs_tot)                  !/*in        204*/
          if (a.eq.dp1.and.noact.ne.2) xs_els=dp0                               !/*in        205*/
          if (noact.eq.0) then                                                  !/*in        206*/
            xs_tot=xs_els                                                       !/*in        207*/
            xs_abs=dp0                                                          !/*in        208*/
          endif                                                                 !/*in        209*/
          test=xsiso(1,iexp)*rang()                                             !/*in        210*/
          if (test.le.xs_tot) then                                              !/*in        211*/
            if (noact.eq.2) then                                                !/*in        212*/
              fac=xs_els/xs_tot                                                 !/*in        213*/
              wt=fac*wt                                                         !/*in        214*/
              if (a.eq.dp1) go to 90                                            !/*in        215*/
              go to 80                                                          !/*in        216*/
            endif                                                               !/*in        217*/
            if (a.eq.dp1) go to 90                                              !/*in        218*/
            if (test.le.xs_els) go to 80                                        !/*in        219*/
            go to 90                                                            !/*in        220*/
          else                                                                  !/*in        221*/
            nopart=-1                                                           !/*in        222*/
            lter = id_true_pseudo_nonterminal                                   !/*in        223*/
            go to 100                                                           !/*in        224*/
          endif                                                                 !/*in        225*/
        else                                                                    !/*in        226*/
          if (noact.eq.0) then                                                  !/*in        227*/
            xs_tot=xs_els                                                       !/*in        228*/
            xs_abs=dp0                                                          !/*in        229*/
          endif                                                                 !/*in        230*/
          if (noact.eq.2) then                                                  !/*in        231*/
            fac=xsiso(2,iexp)/xsiso(1,iexp)                                     !/*in        232*/
            wt=fac*wt                                                           !/*in        233*/
            if (a.eq.dp1) go to 90                                              !/*in        234*/
            go to 80                                                            !/*in        235*/
          endif                                                                 !/*in        236*/
          if (a.eq.dp1) go to 90                                                !/*in        237*/
          test=xsiso(1,iexp)*rang()                                             !/*in        238*/
          if (test.lt.xsiso(2,iexp)) go to 80                                   !/*in        239*/
          go to 90                                                              !/*in        240*/
        endif                                                                   !/*in        241*/
      else                                                                      !/*in        242*/
        if (noact.eq.2) call expirx(1,'INTERACT','not.lmc for noact=2')         !/*in        243*/
        if (xsiso(2,iexp).le.dp0) go to 90                                      !/*in        244*/
!----- test for elastic                                                         !/*in        245*/
        if (xsiso(1,iexp)*rang().ge.xsiso(2,iexp)) go to 90                     !/*in        246*/
        ielast=1                                                                !/*in        247*/
        if (ityp.ne.1) go to 80                                                 !/*in        248*/
        if (ec.ge.esminp) then                                                  !/*in        249*/
          ie=ieidxp(ec)                                                         !/*in        250*/
          temp=fsgels(ec,mel1,esxlp(1,1),esp,iemaxp,ie)                         !/*in        251*/
          if (xsiso(2,iexp)*rang().le.temp) go to 80                            !/*in        252*/
        endif                                                                   !/*in        253*/
!----- elastic pseudocollision                                                  !/*in        254*/
        nopart=-1                                                               !/*in        255*/
        lter = id_true_pseudo_nonterminal                                       !/*in        256*/
        go to 100                                                               !/*in        257*/
      endif                                                                     !/*in        258*/
!=======================================================================        !/*in        259*/
!----- elastic scattering of nucleons                                           !/*in        260*/
   80 continue                                                                  !/*in        261*/
      call elsc2 (ec,a,z)                                                       !/*in        262*/
      nbelo = 0                                                                 !/*in        263*/
      nabov = 0                                                                 !/*in        264*/
      lter = id_elastic_collisn_nonterminal                                     !/*in        265*/
      go to 110                                                                 !/*in        266*/
!=======================================================================        !/*in        267*/
!----- nonelastic interaction                                                   !/*in        268*/
   90 continue                                                                  !/*in        269*/
      if (itt(ityp).eq.0) call expirx(1,'INTERACT','itt(ityp).eq.0')            !/*in        270*/
      idt=iazdex(mel1)                                                          !/*in        271*/
!     ************************************************************              !/*in        272*/
      lrevrs=.false.                                                            !/*in        273*/
#ifndef OLDM                                                                    /*!in        274*/
      if (ityp.ge.8.and.ityp.le.11.and.(a.lt.bar(ityp).or.(a.eq.bar(ityp        !/*in        275*/
     & ).and.z.lt.charge(ityp)))) then                                          !/*in        276*/
        lrevrs=.true.                                                           !/*in        277*/
        ityp0=ityp                                                              !/*in        278*/
        if (a.eq.dp1) then                                                      !/*in        279*/
          ityp=1                                                                !/*in        280*/
        elseif (a.eq.dp2) then                                                  !/*in        281*/
          ityp=8                                                                !/*in        282*/
        elseif (a.eq.dp3) then                                                  !/*in        283*/
          if (z.eq.dp1) ityp=9                                                  !/*in        284*/
          if (z.eq.dp2) ityp=10                                                 !/*in        285*/
        elseif (a.eq.dp4) then                                                  !/*in        286*/
          ityp=11                                                               !/*in        287*/
        else                                                                    !/*in        288*/
          call expirx(1,'INTERACT','target/projectile switch error')            !/*in        289*/
        endif                                                                   !/*in        290*/
        a=bar(ityp0)                                                            !/*in        291*/
        z=charge(ityp0)                                                         !/*in        292*/
        ec0=ec                                                                  !/*in        293*/
        ec=ec0*pmass(ityp)/pmass(ityp0)                                         !/*in        294*/
        emn0=emn                                                                !/*in        295*/
        emn=emin(ityp)                                                          !/*in        296*/
        idt=ityp0-6                                                             !/*in        297*/
        lelem0=lelem                                                            !/*in        298*/
        lelem=9999                                                              !/*in        299*/
      endif                                                                     !/*in        300*/
#endif                                                                          /*!in        301*/
!     ************************************************************              !/*in        302*/
   95 continue                                                                  !/*in        303*/
      mnuc=0                                                                    !/*in        304*/
      call cascad (ec,name,med,emn,                                             !/*in        305*/
#ifdef CEM                                                                      /*!in        306*/
     & iccem,                                                                   !/*in        307*/
#endif                                                                          /*!in        308*/
     & a,z,idt,fmf_,hsigg(1,1),xsiso(1,1),lmc,lmxs(ityp))                       !/*in        309*/
!     ************************************************************              !/*in        310*/
      if (noact.eq.2.and.nopart.lt.0) go to 95                                  !/*in        311*/
      if (lrevrs) then                                                          !/*in        312*/
        lrevrs=.false.                                                          !/*in        313*/
        ec=ec0                                                                  !/*in        314*/
        emn=emn0                                                                !/*in        315*/
        ityp=ityp0                                                              !/*in        316*/
        lelem=lelem0                                                            !/*in        317*/
        if (nopart.ge.0) then                                                   !/*in        318*/
          s=ec/pmass(ityp)                                                      !/*in        319*/
          gam0=1+s                                                              !/*in        320*/
          bet0=sqrt(s*(s+dp2))/gam0                                             !/*in        321*/
          call revers (bet0,gam0)                                               !/*in        322*/
        endif                                                                   !/*in        323*/
      endif                                                                     !/*in        324*/
!     ************************************************************              !/*in        325*/
      if (nopart.lt.0) then                                                     !/*in        326*/
        if (ityp.eq.17.and.ec.le.emn) then                                      !/*in        327*/
           write(io,*) ' **** ityp,iexp',ityp,iexp                              !/*in        328*/
           write(itty,*) ' **** ityp,iexp',ityp,iexp                            !/*in        329*/
        endif                                                                   !/*in        330*/
        lter = id_true_pseudo_nonterminal                                       !/*in        331*/
        go to 100                                                               !/*in        332*/
      endif                                                                     !/*in        333*/
      if (lelem.eq.-1.and.nopart.eq.2.and.kind(2).eq.0.and.                     !/*in        334*/
     &    apr.eq.dp0.and.nbeta.eq.1) then                                       !/*in        335*/
        ielast=1                                                                !/*in        336*/
        call swapp                                                              !/*in        337*/
        lter = id_elastic_collisn_nonterminal                                   !/*in        338*/
      else                                                                      !/*in        339*/
        if (noact.eq.2) go to 95                                                !/*in        340*/
        lter = id_nonelastic_collisn_terminal                                   !/*in        341*/
      endif                                                                     !/*in        342*/
      go to 110                                                                 !/*in        343*/
  100 continue                                                                  !/*in        344*/
      if (ec.gt.emn.or.n1col.lt.0) then                                         !/*in        345*/
         lter = id_true_pseudo_nonterminal                                      !/*in        346*/
         go to 140                                                              !/*in        347*/
      else                                                                      !/*in        348*/
! ---    call info                                                              !/*in        349*/
         ncol=3                                                                 !/*in        350*/
         go to 130                                                              !/*in        351*/
      endif                                                                     !/*in        352*/
  110 continue                                                                  !/*in        353*/
!----- nuclear interaction; ncol = 2 or 6                                       !/*in        354*/
!     ==================================================================        !/*in        355*/
#ifdef LCS                                                                      /*!in        356*/
      isprd=-1                                                                  !/*in        357*/
      call sav1                                                                 !/*in        358*/
#endif                                                                          /*!in        359*/
      call collide (name,icell,u,v,w,tc,wt,xc,yc,zc,                            !/*in        360*/
#ifdef CEM                                                                      /*!in        361*/
     & iccem,                                                                   !/*in        362*/
#endif                                                                          /*!in        363*/
     & lter)                                                                    !/*in        364*/
#ifdef MESHTAL                                                                  /*!in        365*/
      if (mnuc.gt.0) then                                                       !/*in        366*/
         do 115 i=1,mnuc                                                        !/*in        367*/
  115       f_rc=f_rc+wt*(erecr(i)+ecvr(i))                                     !/*in        368*/
      endif                                                                     !/*in        369*/
      if (nbelo.gt.0) then                                                      !/*in        370*/
         do 118 j=1,nbelo                                                       !/*in        371*/
            wx=wtb(j)                                                           !/*in        372*/
            itp = mod(nint(tipb(j)),100) + 1                                    !/*in        373*/
            ik=ika(itp)                                                         !/*in        374*/
            if(ik.gt.0)f_nt=f_nt+wx*eb(j)                                       !/*in        375*/
!                                                                               !/*in        376*/
!           QUESTION:  Return to these two lines.  This may be the              !/*in        377*/
!                      only opportunity to deposit the energy of                !/*in        378*/
!                      below-cutoff photons and positrons.  On the              !/*in        379*/
!                      other hand, the annihilation gammas may not              !/*in        380*/
!                      be below cutoff.                                         !/*in        381*/
!                                                                               !/*in        384*/
  118    continue                                                               !/*in        385*/
      endif                                                                     !/*in        386*/
#endif                                                                          /*!in        387*/
!                                                                               !/*in        388*/
#ifdef HISTP                                                                    /*!in        389*/
      if (nhstp.ne.0)call wrt_histp                                             !/*in        390*/
#endif                                                                          /*!in        391*/
#ifdef LCS                                                                      /*!in        392*/
      call analyz                                                               !/*in        393*/
#endif                                                                          /*!in        394*/
!     ==================================================================        !/*in        395*/
!----- bank trackable secondaries                                               !/*in        396*/
      if (lter.eq.id_elastic_collisn_nonterminal) then                          !/*in        397*/
         if (n1col.eq.0.or.n1col.ge.1.and.name.eq.1) then                       !/*in        398*/
            call update (lter)                                                  !/*in        399*/
            isprd=1                                                             !/*in        400*/
            go to 200                                                           !/*in        401*/
         endif                                                                  !/*in        402*/
      else                                                                      !/*in        403*/
         if (n1col.eq.0.or.n1col.eq.1.and.name.eq.1) then                       !/*in        404*/
            call updat1 (lter)                                                  !/*in        405*/
            go to 190                                                           !/*in        406*/
         endif                                                                  !/*in        407*/
      endif                                                                     !/*in        408*/
      if (wt.le.dp0) go to 200                                                  !/*in        409*/
      if (n1col.eq.-1.or.wt.le.dp0) go to 190                                   !/*in        410*/
      go to 200                                                                 !/*in        411*/
!=======================================================================        !/*in        412*/
!----- charged particle slowed down                                             !/*in        413*/
!----- force interaction for pi- or p-bar                                       !/*in        414*/
  120 continue                                                                  !/*in        415*/
      if (ityp.eq.5.and.npidk.le.0) then                                        !/*in        416*/
        go to 40                                                                !/*in        417*/
      elseif (ityp.eq.17) then                                                  !/*in        418*/
        go to 50                                                                !/*in        419*/
      endif                                                                     !/*in        420*/
  130 continue                                                                  !/*in        421*/
      ncol=3                                                                    !/*in        422*/
      lter = id_stopped_particle_terminal                                       !/*in        423*/
      if (instab(ityp).eq.0) go to 170                                          !/*in        424*/
      lter = id_fulldecay_pseudo_terminal                                       !/*in        425*/
      wt=dp0                                                                    !/*in        426*/
      call mfpd2 (ec,icell,tc,u,v,w,xc,yc,zc,wt)                                !/*in        427*/
      go to 170                                                                 !/*in        428*/
!=======================================================================        !/*in        429*/
!----- process pseudo-collisions : ncol = 5                                     !/*in        430*/
!----- NO decay comes here                                                      !/*in        431*/
  140 continue                                                                  !/*in        432*/
      ncol=5                                                                    !/*in        433*/
!=======================================================================        !/*in        434*/
#ifdef LCS                                                                      /*!in        435*/
      wtsum(ityp,5)=wtsum(ityp,5)+oldwt                                         !/*in        436*/
      if (krec(5)) 150,170,160                                                  !/*in        437*/
#endif                                                                          /*!in        438*/
  150 continue                                                                  !/*in        439*/
      if (oldwt.ne.wt) go to 170                                                !/*in        440*/
!----- skip record for ncol=5                                                   !/*in        441*/
  160 continue                                                                  !/*in        442*/
#ifdef LCS                                                                      /*!in        443*/
      call sav0                                                                 !/*in        444*/
#endif                                                                          /*!in        445*/
      go to 180                                                                 !/*in        446*/
!=======================================================================        !/*in        447*/
!----- write record for ncol=3 or ncol=5                                        !/*in        448*/
  170 continue                                                                  !/*in        449*/
#ifdef MESHTAL                                                                  /*!in        450*/
!     Account for local energy deposition.                                      !/*in        451*/
      ity=tip7+1                                                                !/*in        452*/
      if (ncol.ne.3.or.instab(ity).ne.0) then                                   !/*in        453*/
         if (nbelo.gt.0) then                                                   !/*in        454*/
            do 175 j=1,nbelo                                                    !/*in        455*/
               wx=wtb(j)                                                        !/*in        456*/
               itp = mod(nint(tipb(j)),100) + 1                                 !/*in        457*/
               ik=ika(itp)                                                      !/*in        458*/
               if(ik.gt.0)f_nt=f_nt+wx*eb(j)                                    !/*in        459*/
!                                                                               !/*in        460*/
!              QUESTION:  Return to these two lines.  This may be the           !/*in        461*/
!                         only opportunity to deposit the energy of             !/*in        462*/
!                         below-cutoff photons and positrons.  On the           !/*in        463*/
!                         other hand, the annihilation gammas may not           !/*in        464*/
!                         be below cutoff.                                      !/*in        465*/
!                                                                               !/*in        468*/
  175       continue                                                            !/*in        469*/
         endif                                                                  !/*in        470*/
      endif                                                                     !/*in        471*/
#endif                                                                          /*!in        472*/
!                                                                               !/*in        473*/
#ifdef HISTP                                                                    /*!in        474*/
      if (nhstp.ne.0)call wrt_histp                                             !/*in        475*/
#endif                                                                          /*!in        476*/
#ifdef LCS                                                                      /*!in        477*/
      call sav1                                                                 !/*in        478*/
      call analyz                                                               !/*in        479*/
#endif                                                                          /*!in        480*/
!----- pseudocollision in CASCAD comes here                                     !/*in        481*/
  180 continue                                                                  !/*in        482*/
      if (n1col.ge.0) call updat1 (lter)                                        !/*in        483*/
      if (ncol.eq.3.or.n1col.eq.-1) go to 190                                   !/*in        484*/
      if (wt.le.dp0) go to 200                                                  !/*in        485*/
      if (charge(ityp).eq.dp0) go to 200                                        !/*in        486*/
      if (ec.le.emn) go to 120                                                  !/*in        487*/
      go to 200                                                                 !/*in        488*/
!=======================================================================        !/*in        489*/
  190 continue                                                                  !/*in        490*/
!----- dead particle                                                            !/*in        491*/
      wt=dp0                                                                    !/*in        492*/
  200 continue                                                                  !/*in        493*/
#ifdef LCS                                                                      /*!in        494*/
!     ==================================================================        !/*in        495*/
      if (ncol.eq.5) then                                                       !/*in        496*/
        if (lter.eq.id_true_pseudo_nonterminal) then                            !/*in        497*/
          if (ielast.eq.1) then                                                 !/*in        498*/
            ielast=0                                                            !/*in        499*/
            call pdevnt (2)                                                     !/*in        500*/
          else                                                                  !/*in        501*/
            call pdevnt (3)                                                     !/*in        502*/
          endif                                                                 !/*in        503*/
        elseif (lter.eq.id_fulldecay_pseudo_terminal .or.                       !/*in        504*/
     &          lter.eq.id_partdecay_pseudo_nonterminal) then                   !/*in        505*/
          if (med.eq.0) then                                                    !/*in        506*/
            peiv(ityp)=peiv(ityp)+dp1                                           !/*in        507*/
          else                                                                  !/*in        508*/
            call pdevnt (1)                                                     !/*in        509*/
          endif                                                                 !/*in        510*/
        endif                                                                   !/*in        511*/
      endif                                                                     !/*in        512*/
!     ==================================================================        !/*in        513*/
#endif                                                                          /*!in        514*/
      if (n1col.eq.-1) lter = id_nonelastic_collisn_terminal                    !/*in        515*/
      return                                                                    !/*in        516*/
      end                                                                       !/*in        517*/
