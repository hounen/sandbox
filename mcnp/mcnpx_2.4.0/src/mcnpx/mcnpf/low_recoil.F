c#ident src/mcnpx/mcnpf/low_recoil.F from deck lirk                           /*lirk        1*/
      subroutine low_recoil(v0)                                                 !/*lirk        2*/
!        light ion recoil from neutrons, protons, and possible other            !/*lirk        3*/
!        projectiles in the low-energy (table) range.                           !/*lirk        4*/
!        generate and bank the recoil ion from incident particles               !/*lirk        5*/
!        undergoing elastic scatter with nuclides za < 2004.                    !/*lirk        6*/
!        done if rcoilf (7th phys:n entry) nonzero.                             !/*lirk        7*/
!        erg=incident neutron energy                                            !/*lirk        8*/
!           =eg0 for nonthermal, ssr**2*ttn/awn(lawn+iex) free gas.             !/*lirk        9*/
!        v0=incident projectile direction (unchanged).                          !/*lirk       10*/
!                                                                               !/*lirk       11*/
      use global_data                                                           !/*lirk       12*/
!                                                                               !/*lirk       13*/
      implicit real(kindr) (a-h,o-z)                                            !/*lirk       14*/
      implicit integer(kindi) (i-n)                                             !/*lirk       15*/
!                                                                               !/*lirk       16*/
      dimension v0(3),v1(3),v2(3),v3(3)                                         !/*lirk       17*/
!                                                                               !/*lirk       18*/
!        set ip=9,31,32,33,34 for zaid=1001,1002,1003,2003,2004.                !/*lirk       19*/
      iz=nxs(lnxs+2,iex)/1000                                                   !/*lirk       20*/
      ia=nxs(lnxs+2,iex)-1000*iz                                                !/*lirk       21*/
      ip=28+iz+ia                                                               !/*lirk       22*/
      if(nxs(lnxs+2,iex).eq.1001)ip=9                                           !/*lirk       23*/
      if(kpt(ip).eq.0.or.fim(lfim+ip,icl).eq.0.)return                          !/*lirk       24*/
      if(ssr.eq.zero.and.erg.lt.elc(ip))return                                  !/*lirk       25*/
!                                                                               !/*lirk       26*/
!        get ei=ionization energy.                                              !/*lirk       27*/
!        (Handbook of Chemistry & Physics, ionization potentials.)              !/*lirk       28*/
      ei=13.595e-6                                                              !/*lirk       29*/
      if(iz.eq.2)ei=54.403e-6                                                   !/*lirk       30*/
!                                                                               !/*lirk       31*/
!        For the following 2-body kinematics, see R. D. Evans,                  !/*lirk       32*/
!        "The Atomic Nucleus," McGraw-Hill (Nov 20, 1970)                       !/*lirk       33*/
!        Appendix B, center-of-mass coordinates, and the nonrelativistic        !/*lirk       34*/
!        elastic collision in classical mechanics. (pg 835)                     !/*lirk       35*/
!        Equations are center of mass (c.m.), target-at-rest (t.a.r.).          !/*lirk       36*/
!        Evans, pg 835, eqn 28: v2**2=2*(v0/(1+a2))**2*(1-c0)                   !/*lirk       37*/
!        where:  a2 = atomic mass ratio = awn(lawn+iex)                         !/*lirk       38*/
!                c0 = center of mass cosine = colout(3,1) from acecol           !/*lirk       39*/
!                v0 = incident particle velocity (energy e0)                    !/*lirk       40*/
!                v1 = exiting particle velocity (energy e1)                     !/*lirk       41*/
!                v2 = recoil (exiting) target velocity (energy e2)              !/*lirk       42*/
!                     note v2**2/v0**e = e2/e0                                  !/*lirk       43*/
!                c2 = recoil (exiting) ion cosine.                              !/*lirk       44*/
!                c1 = exiting particle cosine.                                  !/*lirk       45*/
      a2=awn(lawn+iex)*gpt(1)/gpt(ipt)                                          !/*lirk       46*/
      c0=colout(3,1)                                                            !/*lirk       47*/
      v1(1)=uuu                                                                 !/*lirk       48*/
      v1(2)=vvv                                                                 !/*lirk       49*/
      v1(3)=www                                                                 !/*lirk       50*/
      e0=erg                                                                    !/*lirk       51*/
      e2=e0*two*(one-c0)/(one+a2)**2-ei                                         !/*lirk       52*/
      if(e2.le.zero)return                                                      !/*lirk       53*/
      if(ssr.eq.zero.and.e2.lt.elc(ip))return                                   !/*lirk       54*/
!        t.a.r. cosine of scatter, evans, pg 835, eqn 28a, 29                   !/*lirk       55*/
!        c1=colout(2,1)=(1+a2*c0)/sqrt(1+a2**2+2*a2*c0)                         !/*lirk       56*/
      c1=colout(2,1)                                                            !/*lirk       57*/
      c2=sqrt(half*(one-c0))                                                    !/*lirk       58*/
!        add vectors to get v0,v1,v2 all in same plane.                         !/*lirk       59*/
      s1=one-c1**2                                                              !/*lirk       60*/
      if(s1.lt.p_1m6)return                                                     !/*lirk       61*/
      h=sqrt((one-c2**2)/s1)                                                    !/*lirk       62*/
      g=c2+h*c1                                                                 !/*lirk       63*/
      v2(1)=g*v0(1)-h*v1(1)                                                     !/*lirk       64*/
      v2(2)=g*v0(2)-h*v1(2)                                                     !/*lirk       65*/
      v2(3)=g*v0(3)-h*v1(3)                                                     !/*lirk       66*/
!        t should always be one, but renormalize to prevent roundoff.           !/*lirk       67*/
      t=v2(1)**2+v2(2)**2+v2(3)**2                                              !/*lirk       68*/
      if(abs(one-t).gt.p_1m6)call expirx(1,'low_recoil','bad angle.')           !/*lirk       69*/
      s=one/sqrt(t)                                                             !/*lirk       70*/
      v2(1)=s*v2(1)                                                             !/*lirk       71*/
      v2(2)=s*v2(2)                                                             !/*lirk       72*/
      v2(3)=s*v2(3)                                                             !/*lirk       73*/
!                                                                               !/*lirk       74*/
!        convert from t.a.r. coordinates to lab frame if needed.                !/*lirk       75*/
      if(ssr.ne.zero)then                                                       !/*lirk       76*/
         v=sqrt(e2*awn(lawn+iex)/ttn)                                           !/*lirk       77*/
         v3(1)=v*v2(1)+vtr(1)                                                   !/*lirk       78*/
         v3(2)=v*v2(2)+vtr(2)                                                   !/*lirk       79*/
         v3(3)=v*v2(3)+vtr(3)                                                   !/*lirk       80*/
         t=v3(1)**2+v3(2)**2+v3(3)**2                                           !/*lirk       81*/
         e2=e2*t/v**2                                                           !/*lirk       82*/
         if(e2.lt.elc(ip))return                                                !/*lirk       83*/
         s=one/sqrt(t)                                                          !/*lirk       84*/
         v2(1)=s*v3(1)                                                          !/*lirk       85*/
         v2(2)=s*v3(2)                                                          !/*lirk       86*/
         v2(3)=s*v3(3)                                                          !/*lirk       87*/
      endif                                                                     !/*lirk       88*/
!                                                                               !/*lirk       89*/
!        roulette recoil nucleus if required.                                   !/*lirk       90*/
      ix=min(ipt,2)                                                             !/*lirk       91*/
      if(rcoilf(ix).lt..9)then                                                  !/*lirk       92*/
         if(rang().gt.rcoilf(ix))return                                         !/*lirk       93*/
         wgt=wgt/rcoilf(ix)                                                     !/*lirk       94*/
      endif                                                                     !/*lirk       95*/
!                                                                               !/*lirk       96*/
!        save continuing particle parameters.                                   !/*lirk       97*/
      call savpar(0,1)                                                          !/*lirk       98*/
      jb=30                                                                     !/*lirk       99*/
      if(ipt.gt.1)jb=31                                                         !/*lirk      100*/
!                                                                               !/*lirk      101*/
!        bank the recoil ion.                                                   !/*lirk      102*/
      npa=1                                                                     !/*lirk      103*/
      ipt=ip                                                                    !/*lirk      104*/
      ncp=0                                                                     !/*lirk      105*/
      jan = not_self_anti(ip)                                                   !/*lirk      106*/
      idx=0                                                                     !/*lirk      107*/
      erg=e2                                                                    !/*lirk      108*/
      vel=slite*sqrt(erg*(erg+2.*gpt(ip)))/(erg+gpt(ip))                        !/*lirk      109*/
      uuu=v2(1)                                                                 !/*lirk      110*/
      vvv=v2(2)                                                                 !/*lirk      111*/
      www=v2(3)                                                                 !/*lirk      112*/
      if(lev.ne.0)call uplev(uuu)                                               !/*lirk      113*/
      call bankit(jb)                                                           !/*lirk      114*/
      call gain_pax (igain_gen_chg_elastic_recoil, ipt, 1, wgt, wgt*erg)        !/*lirk      115*/
      call contrib_pwb (ipt,ipwb_gen_chg_nuc_interaction,icl,wgt)               !/*lirk      116*/
!                                                                               !/*lirk      117*/
!        restore continuing particle parameters.                                !/*lirk      118*/
      call getpar(0,1)                                                          !/*lirk      119*/
      return                                                                    !/*lirk      120*/
      end                                                                       !/*lirk      121*/
