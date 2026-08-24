!#ident escat.F from deck eu4c                                                  !/*eu4c        1*/
      subroutine escat(aq,dk,fd,ae,nq,ik)                                       !/*eu4c        2*/
!        electron scatter and deflection correction.                            !/*eu4c        3*/
!        return new cosine, aq, and direction, uuu, for particle.               !/*eu4c        4*/
!        return new uold for bremsstrahlung photons for tallyd, dxtran.         !/*eu4c        5*/
!                                                                               !/*eu4c        6*/
!        ik = 1 gives new direction for electron (called from electr).          !/*eu4c        7*/
!        ik = 2/3 gives new direction for photon(brems)/electron(knock)         !/*eu4c        8*/
!           with correction for electron deflection along substep.              !/*eu4c        9*/
!        dk = pathlength < pmf; most often ik = 1, dk = pmf = substep.          !/*eu4c       10*/
!        fd = fraction of total pathlength traversed if dbcn(17).ne.0.          !/*eu4c       11*/
!        ae = direction cosine of electron initially if dbcn(17)>0.             !/*eu4c       12*/
!        nq = current electron energy index.                                    !/*eu4c       13*/
!                                                                               !/*eu4c       14*/
!        uold = direction at start of step and                                  !/*eu4c       15*/
!        ue = direction at deflection/emission event.                           !/*eu4c       16*/
      use global_data                                                           !/*eux         1*/
!                                                                               !/*eux         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*eux         3*/
      implicit integer(kindi) (i-n)                                             !/*eux         4*/
!                                                                               !/*eux         5*/
      dimension ue(3)                                                           !/*eu4c       18*/
      ue(1)=uold(1)                                                             !/*eu4c       19*/
      ue(2)=uold(2)                                                             !/*eu4c       20*/
      ue(3)=uold(3)                                                             !/*eu4c       21*/
      if(ik.ne.1.and.dbcn(17).ne.0.)go to 40                                    !/*eu4c       22*/
!                                                                               !/*eu4c       23*/
!        dbcn(17)=0: proper delta scattering. xtm:95-236,319                    !/*eu4c       24*/
      if(dk.ge.pmf)go to 10                                                     !/*eu4c       25*/
      ep=1.d-10                                                                 !/*eu4c       30*/
      if(rang().gt.max(ep,1.-egg(legg+maxi,nq+nee(3)*(mkc-1)))**                !/*eux         6*/
     & (dk/pmf))go to 10                                                        !/*eu4c       32*/
      if(ik.eq.1)return                                                         !/*eu4c       34*/
      go to 60                                                                  !/*eu4c       35*/
   10 rn=rang()                                                                 !/*eu4c       36*/
      do 20 m=2,maxi                                                            !/*eu4c       37*/
   20 if(egg(legg+m,nq+nee(3)*(mkc-1)).gt.rn)go to 30                           !/*eux         7*/
      if(dk.lt.pmf)go to 10                                                     !/*eu4c       39*/
      return                                                                    !/*eu4c       40*/
   30 a=calph(m)+(calph(m-1)-calph(m))                                          !/*eux         8*/
     & *(egg(legg+m,nq+nee(3)*(mkc-1))-rn)                                      !/*eux         9*/
     & /(egg(legg+m,nq+nee(3)*(mkc-1))-egg(legg+m-1,nq+nee(3)*(mkc-1)))         !/*eux        10*/
      if(dk.lt.pmf)a=1.-(1.-a)*dk/pmf                                           !/*eu4c       43*/
      if(ik.eq.1)aq=a                                                           !/*eu4c       44*/
      if(ik.gt.1)call rotas(a,uold,ue,lev,irt)                                  !/*eu4c       45*/
      go to 60                                                                  !/*eu4c       46*/
!                                                                               !/*eu4c       47*/
!        dbcn(17)>0: schlumberger (sdr) scattering method. xtm:95-267           !/*eu4c       48*/
   40 if(dbcn(17).lt.0.)go to 50                                                !/*eu4c       49*/
      tpp(1)=fd*(1.-ae)                                                         !/*eu4c       50*/
      tpp(2)=sqrt((2.*fd-fd*tpp(1))/(1.+ae))                                    !/*eu4c       51*/
      tpp(3)=1.-tpp(1)-ae*tpp(2)                                                !/*eu4c       52*/
      ue(1)=tpp(2)*uuu+tpp(3)*uold(1)                                           !/*eu4c       53*/
      ue(2)=tpp(2)*vvv+tpp(3)*uold(2)                                           !/*eu4c       54*/
      ue(3)=tpp(2)*www+tpp(3)*uold(3)                                           !/*eu4c       55*/
      go to 60                                                                  !/*eu4c       56*/
!                                                                               !/*eu4c       57*/
!        dbcn(17)<0: its3.0 methodology.                                        !/*eu4c       58*/
   50 if(fd.le..5)go to 60                                                      !/*eu4c       59*/
      ue(1)=uuu                                                                 !/*eu4c       60*/
      ue(2)=vvv                                                                 !/*eu4c       61*/
      ue(3)=www                                                                 !/*eu4c       62*/
!                                                                               !/*eu4c       63*/
!        get new direction, uuu, for cosine aq.                                 !/*eu4c       64*/
   60 aq=min(max(aq,-one),one)                                                  !/*eu4c       65*/
      call rotas(aq,ue,uuu,lev,irt)                                             !/*eu4c       66*/
!                                                                               !/*eu4c       67*/
!        change uold for bremsstrahlung photons for tallyd, dxtran.             !/*eu4c       68*/
      if(ik.ne.2)return                                                         !/*eu4c       69*/
      uold(1)=ue(1)                                                             !/*eu4c       70*/
      uold(2)=ue(2)                                                             !/*eu4c       71*/
      uold(3)=ue(3)                                                             !/*eu4c       72*/
      return                                                                    !/*eu4c       73*/
      end                                                                       !/*eu4c       74*/
