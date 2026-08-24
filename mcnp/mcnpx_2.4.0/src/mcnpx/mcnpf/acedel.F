!#ident acedel.F from deck zd4c                                                 !/*zd4c        1*/
      subroutine acedel(jq)                                                     !/*zd4c        2*/
!        sample prompt and delayed neutrons.                                    !/*zd4c        3*/
!        jq=0/2=normal/kcode sampling of fission energy only.                   !/*zd4c        4*/
!        colout(1,i)=energy; colout(2,i)=cosine scattering angle;               !/*zd4c        5*/
!        colout(3,i)=delay time of delayed neutrons.                            !/*zd4c        6*/
!        if delayed neutron biasing (4th phys:n entry) then prompt              !/*zd4c        7*/
!           neutrons are followed by delayed neutrons and the biased            !/*zd4c        8*/
!           weight is -colout(3,1).                                             !/*zd4c        9*/
      use global_data                                                           !/*zdx         1*/
!                                                                               !/*zdx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*zdx         3*/
      implicit integer(kindi) (i-n)                                             !/*zdx         4*/
!                                                                               !/*zdx         5*/
      q=xss(jxs(ljxs+4,iex)+ixre-1)                                             !/*zd4c       11*/
      ia=jxs(ljxs+9,iex)                                                        !/*zd4c       12*/
      ka=nint(xss(jxs(ljxs+8,iex)+ixre))                                        !/*zd4c       13*/
!                                                                               !/*zd4c       17*/
!        get delayed nu.                                                        !/*zd4c       18*/
      dn=acenu(jxs(ljxs+24,iex))                                                !/*zd4c       19*/
!                                                                               !/*zd4c       20*/
!        jq=2: call from colidk; get fission energy only.                       !/*zdxc        2*/
      if(jq.eq.2)then                                                           !/*zdxc        3*/
         tn=rtc(krtc+10,iex)                                                    !/*zdxc        4*/
         cmult=aint(tn+rang())                                                  !/*zdxc        5*/
         nt=1                                                                   !/*zdxc        6*/
         np=100                                                                 !/*zdxc        7*/
         go to 20                                                               !/*zdxc        8*/
      endif                                                                     !/*zdxc        9*/
!                                                                               !/*zdxc       10*/
!        get total nu.                                                          !/*zdxc       11*/
      tn=acenu(jxs(ljxs+2,iex))                                                 !/*zdxc       12*/
!                                                                               !/*zdxc       16*/
!        biased sampling of delayed neutrons.                                   !/*zdxc       17*/
      if(dnb.le.zero)go to 10                                                   !/*zdxc       18*/
      pn=tn-dn                                                                  !/*zdxc       19*/
      np=pn+rang()                                                              !/*zdxc       20*/
      db=min(mcol-np-p_1m6,dnb)                                                 !/*zdxc       24*/
      nt=np+db+rang()                                                           !/*zd4c       25*/
      cmult=nt                                                                  !/*zd4c       26*/
      go to 20                                                                  !/*zd4c       27*/
!                                                                               !/*zd4c       28*/
!        analog sampling of delayed neutrons.                                   !/*zd4c       29*/
   10 nt=tn+rang()                                                              !/*zd4c       30*/
      cmult=nt                                                                  !/*zd4c       31*/
      np=100                                                                    !/*zd4c       33*/
!                                                                               !/*zd4c       34*/
!        sample prompt and delayed neutrons.                                    !/*zd4c       35*/
   20 do 70 i=1,nt                                                              !/*zd4c       36*/
      if(i.gt.np)go to 40                                                       !/*zd4c       37*/
      if(np.ne.100)go to 30                                                     !/*zd4c       38*/
      if(rang()*tn.le.dn)go to 40                                               !/*zd4c       39*/
!                                                                               !/*zd4c       40*/
!        get prompt neutrons.                                                   !/*zd4c       41*/
   30 id=jxs(ljxs+11,iex)                                                       !/*zd4c       42*/
      kd=nint(xss(jxs(ljxs+10,iex)+ixre-1))                                     !/*zd4c       43*/
      call acecas(i,1,q,ia,ka,id,kd)                                            !/*zd4c       44*/
      colout(3,i)=0.                                                            !/*zd4c       45*/
      go to 70                                                                  !/*zd4c       46*/
!                                                                               !/*zd4c       47*/
!        sample delayed neutrons.                                               !/*zd4c       48*/
   40 ns=jxs(ljxs+25,iex)                                                       !/*zd4c       49*/
      rn=rang()                                                                 !/*zd4c       50*/
      j=0                                                                       !/*zd4c       51*/
      pg=0.                                                                     !/*zd4c       52*/
      do 50 ix=1,nxs(lnxs+8,iex)                                                !/*zd4c       53*/
      pg=pg+acefcn(ns+1,erg,j)                                                  !/*zd4c       54*/
      if(rn.le.pg)go to 60                                                      !/*zd4c       55*/
   50 ns=ns+1+j                                                                 !/*zd4c       56*/
      ix=nxs(lnxs+8,iex)                                                        !/*zd4c       57*/
      ns=ns-1-j                                                                 !/*zd4c       58*/
   60 id=jxs(ljxs+27,iex)                                                       !/*zd4c       59*/
      kd=nint(xss(jxs(ljxs+26,iex)+ix-1))                                       !/*zd4c       60*/
      call acecas(i,1,q,0,0,id,kd)                                              !/*zd4c       61*/
      colout(3,i)=-log(rang())/xss(ns)                                          !/*zd4c       62*/
   70 continue                                                                  !/*zd4c       63*/
      if(jq.eq.2)return                                                         !/*zdxk        5*/
!                                                                               !/*zd4c       64*/
!        store biased delayed neutron weight in 1st prompt neutron time.        !/*zd4c       65*/
      if(np.ne.100)colout(3,1)=-wgt*dn/db                                       !/*zd4c       66*/
!                                                                               !/*zdxk        7*/
!        rare case of a single delayed neutron emitted.                         !/*zdxk        8*/
      if(colout(3,1).le.zero.or.nt.ne.1)return                                  !/*zdxk        9*/
      tme=tme+colout(3,1)                                                       !/*zdxk       10*/
      if(tme.ge.tco(1))nter = loss_all_pls_time_cutoff                          !/*zdxk       11*/
      return                                                                    !/*zd4c       67*/
      end                                                                       !/*zd4c       68*/
