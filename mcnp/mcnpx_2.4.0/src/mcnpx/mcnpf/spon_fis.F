c#ident src/mcnpx/mcnpf/spon_fis.F from deck spfk                             /*spfk        1*/
      subroutine spon_fis                                                       !/*spfk        2*/
!        sample spontaneous fission nuclides, multiplicity, energies,           !/*spfk        3*/
!        and directions.                                                        !/*spfk        4*/
!                                                                               !/*spfk        5*/
      use global_data                                                           !/*spfk        6*/
!                                                                               !/*spfk        7*/
      implicit real(kindr) (a-h,o-z)                                            !/*spfk        8*/
      implicit integer(kindi) (i-n)                                             !/*spfk        9*/
!                                                                               !/*spfk       10*/
!        fm = multiplicity probabilities normalized to fn.                      !/*spfk       11*/
!        fy = spontaneous fission yields (n/s-g).                               !/*spfk       12*/
!        id = zaids                                                             !/*spfk       13*/
!        sa,sb = watt spectrum parameters (use 240pu for 238pu).                !/*spfk       14*/
      dimension fm(9,6),fy(6),sa(6),sb(6),id(6)                                 !/*spfk       15*/
      data (fm(i,1),i=1,9)/                                                     !/*spfk       16*/
     & .054d0,.259d0,.639d0,.864d0,.972d0,   one,   one,   one,   one/          !/*spfk       17*/
      data (fm(i,2),i=1,9)/                                                     !/*spfk       18*/
     & .066d0,.298d0,.627d0,.878d0,.980d0,.998d0,   one,   one,   one/          !/*spfk       19*/
      data (fm(i,3),i=1,9)/                                                     !/*spfk       20*/
     & .068d0,.298d0,.632d0,.879d0,.978d0,.996d0,.999d0,.999d0,.999d0/          !/*spfk       21*/
      data (fm(i,4),i=1,9)/                                                     !/*spfk       22*/
     & .021d0,.168d0,.495d0,.822d0,.960d0,.997d0,1.d0,1.001d0,1.001d0/          !/*spfk       23*/
      data (fm(i,5),i=1,9)/                                                     !/*spfk       24*/
     & .015d0,.131d0,.431d0,.764d0,.948d0,.991d0,   one,   one,   one/          !/*spfk       25*/
      data (fm(i,6),i=1,9)/                                                     !/*spfk       26*/
     & .002d0,.028d0,.155d0,.428d0,.732d0,.917d0,.983d0,.998d0,   one/          !/*spfk       27*/
      data fy/2.59d3,1.02d3,1.72d3,2.10d7,1.08d7,2.34d12/                       !/*spfk       28*/
      data id/94238,94240,94242,96242,96244,98252/                              !/*spfk       29*/
      data sa/0.8234d0, .799d0, .833668d0, .891d0, .906d0,1.025d0/              !/*spfk       30*/
      data sb/4.6986d0,4.903d0,4.431658d0,4.046d0,3.848d0,2.926d0/              !/*spfk       31*/
!                                                                               !/*spfk       32*/
      ns=0                                                                      !/*spfk       33*/
!                                                                               !/*spfk       34*/
!        select spontaneous fission zaid.                                       !/*spfk       35*/
      mk=mat(lmat+icl)                                                          !/*spfk       36*/
      if(mk.eq.0)call expirx(1,'colidn','spontaneous fission in void.')         !/*spfk       37*/
      m1=0                                                                      !/*spfk       38*/
      m2=0                                                                      !/*spfk       39*/
      ty=zero                                                                   !/*spfk       40*/
      do 30 m=jmd(ljmd+mk),jmd(ljmd+mk+1)-1                                     !/*spfk       41*/
      iz=nxs(lnxs+2,lme(llme+1,m))                                              !/*spfk       42*/
      if(iz.lt.94000)go to 30                                                   !/*spfk       43*/
      do 10 ii=1,6                                                              !/*spfk       44*/
   10 if(id(ii).eq.iz)go to 20                                                  !/*spfk       45*/
      go to 30                                                                  !/*spfk       46*/
   20 ty=ty+fy(ii)*fme(lfme+m)                                                  !/*spfk       47*/
      if(m1.eq.0)m1=m                                                           !/*spfk       48*/
      m2=m                                                                      !/*spfk       49*/
   30 continue                                                                  !/*spfk       50*/
      if(m2.eq.0)                                                               !/*spfk       51*/
     & call expirx(0,'spon_fis','spontaneous fission impossible.')              !/*spfk       52*/
!                                                                               !/*spfk       53*/
!        more than one spontaneous fission zaid in source cell.                 !/*spfk       54*/
      if(m1.eq.m2)go to 70                                                      !/*spfk       55*/
      t1=rang()*ty                                                              !/*spfk       56*/
      do 60 m=jmd(ljmd+mk),jmd(ljmd+mk+1)-1                                     !/*spfk       57*/
      iz=nxs(lnxs+2,lme(llme+1,m))                                              !/*spfk       58*/
      if(iz.lt.94000)go to 60                                                   !/*spfk       59*/
      do 40 ii=1,6                                                              !/*spfk       60*/
   40 if(id(ii).eq.iz)go to 50                                                  !/*spfk       61*/
      go to 60                                                                  !/*spfk       62*/
   50 t1=t1-fy(ii)*fme(lfme+m)                                                  !/*spfk       63*/
      m1=m                                                                      !/*spfk       64*/
      if(t1.lt.zero)go to 70                                                    !/*spfk       65*/
   60 continue                                                                  !/*spfk       66*/
!                                                                               !/*spfk       67*/
!        sample multiplicity, ns, for spontaneous fission nuclide iz.           !/*spfk       68*/
   70 t2=rang()*fm(9,ii)                                                        !/*spfk       69*/
      do 80 ns=0,7                                                              !/*spfk       70*/
   80 if(fm(ns+1,ii).ge.t2)go to 90                                             !/*spfk       71*/
!                                                                               !/*spfk       72*/
!        increment summary tables.                                              !/*spfk       73*/
   90 mpan=ipan(lipa+icl)+m1-jmd(ljmd+mk)                                       !/*spfk       74*/
      smultc(1)=smultc(1)+wgt*(ns-1)                                            !/*spfk       75*/
      if(ns.eq.0)then                                                           !/*spfk       80*/
         nter=loss_neutron_loss_to_fission                                      !/*spfk       81*/
         pan(kpan+1,4,mpan)=pan(kpan+1,4,mpan)-wgt                              !/*spfk       82*/
         return                                                                 !/*spfk       83*/
      endif                                                                     !/*spfk       84*/
      call lose_pax (loss_neutron_loss_to_fission,1,1,wgt,wgt*erg)              !/*spfk       86*/
      call contrib_pwb (1,ipwb_neutron_loss_to_fission,icl,-wgt)                !/*spfk       87*/
      cmult=ns                                                                  !/*spfk       88*/
      call contrib_pwb (1,ipwb_neutron_fission,icl,ns*wgt)                      !/*spfk       89*/
      pan(kpan+1,4,mpan)=pan(kpan+1,4,mpan)+wgt*(ns-1)                          !/*spfk       90*/
!                                                                               !/*spfk       91*/
!        loop over ns spontaneous fission neutrons.                             !/*spfk       92*/
      npa=1                                                                     !/*spfk       93*/
      do 110 np=1,ns                                                            !/*spfk       94*/
!                                                                               !/*spfk       95*/
!        sample isotropic source direction.                                     !/*spfk       96*/
      call isos(uuu,lev)                                                        !/*spfk       97*/
!                                                                               !/*spfk       98*/
!        sample from watt spontaneous fission spectrum.                         !/*spfk       99*/
      q9=sa(ii)                                                                 !/*spfk      100*/
      q8=sb(ii)                                                                 !/*spfk      101*/
      t2=q9*q8                                                                  !/*spfk      102*/
      t1=one+.125*t2                                                            !/*spfk      103*/
      q5=t1+sqrt(t1**2-one)-one                                                 !/*spfk      104*/
      q6=q9*(q5+one)                                                            !/*spfk      105*/
  100 t1=-log(rang())                                                           !/*spfk      106*/
      if((-log(rang())-q5*(1.+t1))**2.gt.q8*q6*t1)go to 100                     !/*spfk      107*/
      erg=q6*t1                                                                 !/*spfk      108*/
      vel=slite*sqrt(erg*(erg+2.*gpt(1)))/(erg+gpt(1))                          !/*spfk      109*/
      call gain_pax (igain_neutron_prompt_fission,1,1,wgt,wgt*erg)              !/*spfk      110*/
!                                                                               !/*spfk      111*/
!        bank all but the continuing neutron.                                   !/*spfk      112*/
      if(np.eq.ns)return                                                        !/*spfk      113*/
      if(kf8.lt.0.and.locph(llph+icl).ne.0)call talph(icl,erg)                  !/*spfk      114*/
  110 call bankit(0)                                                            !/*spfk      115*/
      return                                                                    !/*spfk      116*/
      end                                                                       !/*spfk      117*/
