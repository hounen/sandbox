!#ident knock.F from deck kn                                                    !/*kn          1*/
      subroutine knock(d,f,no,ae)                                               !/*kn4b        1*/
!        generate knock-on electrons along path segment d.                      !/*kn          3*/
!        f = energy interpolation factor.                                       !/*kn          4*/
!        no = energy group at the beginning of the substep.                     !/*knx         1*/
!        ae = deflection cosine of electron over path segment d.                !/*kn4b        2*/
      use global_data                                                           !/*knx         2*/
!                                                                               !/*knx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*knx         4*/
      implicit integer(kindi) (i-n)                                             !/*knx         5*/
!                                                                               !/*knx         6*/
#ifdef MESHTAL                                                                  /*!knx         7*/
      dimension zd(10),jz(5)                                                    !/*knx         8*/
      qx=0.                                                                     !/*knx         9*/
#endif                                                                          /*!knx        10*/
!                                                                               !/*kn          6*/
      ni=rang()+d*rnok*rho(lrho+icl)*                                           !/*kn          7*/
     & ((1.-f)*pkn(lpkn+ngp+nee(3)*(mkc-1))+                                    !/*knx        11*/
     & f*pkn(lpkn+ngp+1+nee(3)*(mkc-1)))                                        !/*knx        12*/
      if(ni.eq.0)return                                                         !/*kn          9*/
      call savpar(0,3)                                                          !/*kn4c2       1*/
      nb=npb                                                                    !/*kn4c2       2*/
      if(dbcn(20).ne.0.)wgt=wgt/rnok                                            !/*knxa        1*/
      do 110 n=1,ni                                                             !/*kn4c2       3*/
      if(dbcn(20).eq.0.)wgt=wgt/rnok                                            !/*knxa        2*/
      rn=rang()                                                                 !/*kn         20*/
      p=(1.-f)*pru(lpru+ngp)+f*pru(lpru+ngp+1)                                  !/*kn         21*/
      es=ecf(3)/eee(leee+3,ngp)                                                 !/*knx        13*/
      s=eee(leee+3,ngp)/gpt(3)                                                  !/*knx        14*/
      if(rn.gt.p)go to 70                                                       !/*kn         24*/
      ag=(s/(s+1.))**2                                                          !/*kn         25*/
      bs=1.-ag                                                                  !/*kn         26*/
      e1=1./es-2.                                                               !/*kn         27*/
      if(rn.gt.(e1+bs*log(2.*es))/(p+ag*(.5-es)))go to 50                       !/*kn         28*/
      b=1./(1.-es*bs)                                                           !/*kn         29*/
   40 as=1./(2.+rang()*e1)                                                      !/*kn         30*/
      if(rang().gt.(1.-as*bs)*b)go to 40                                        !/*kn         31*/
      go to 80                                                                  !/*kn         32*/
   50 e2=2.-1./(1.-es)                                                          !/*kn         33*/
      b=1./(1.-.5*bs)                                                           !/*kn         34*/
   60 as=1./(2.-rang()*e2)                                                      !/*kn         35*/
      if(rang().gt.(1.-as*bs)*b)go to 60                                        !/*kn         36*/
      as=1.-as                                                                  !/*kn         37*/
      go to 80                                                                  !/*kn         38*/
   70 as=.5-rang()*(.5-es)                                                      !/*kn         39*/
   80 erg=as*eee(leee+3,ngp)                                                    !/*knx        15*/
      if(erg.lt.elc(3))go to 110                                                !/*kn         41*/
      fd=rang()                                                                 !/*kn         42*/
      dk=fd*d                                                                   !/*kn         43*/
      call uplpos(xxx,uold,lev,dk,vel,0)                                        !/*kn4b        3*/
      aq=sqrt(as*(s+2.)/(as*s+2.))                                              !/*kn4b        5*/
      call escat(aq,dk,fd,ae,no,3)                                              !/*kn4c        1*/
   95 call gain_pax (igain_electron_knock_on, 3, 1, wgt, wgt*erg)               !/*knx        16*/
      call contrib_pwb (3,ipwb_electron_knock_on,icl,wgt)                       !/*knx        17*/
      jsu=0                                                                     !/*kn         60*/
      if(jgp.le.-2)jgp=jgp+2                                                    !/*cor4-1    159*/
      vel=slite*sqrt(erg*(erg+2.*gpt(3)))/(erg+gpt(3))                          !/*kn         63*/
      call bankit(17)                                                           !/*kn4a        1*/
#ifdef MESHTAL                                                                  /*!knx        18*/
      qx=qx-erg                                                                 !/*knx        19*/
#endif                                                                          /*!knx        20*/
  110 call getpar(nb,3)                                                         !/*kn4c2       4*/
      npb=nb-1                                                                  !/*kn4c2       5*/
#ifdef MESHTAL                                                                  /*!knx        21*/
      if(qx.eq.0.) return                                                       !/*knx        22*/
      ej=erg                                                                    !/*knx        23*/
      erg=eg0                                                                   !/*knx        24*/
      if (mtlflg.ne.0.or.medflg.ne.0) then                                      !/*knx        25*/
         do 98 i=1,6                                                            !/*knx        26*/
            if(lev.eq.0) then                                                   !/*knx        27*/
               zd(i)=gpblcm(i)                                                  !/*knx        28*/
            else                                                                !/*knx        29*/
               zd(i)=udt(i,0)                                                   !/*knx        30*/
            endif                                                               !/*knx        31*/
   98       continue                                                            !/*knx        32*/
         zd(7)=wgt                                                              !/*knx        33*/
         zd(8)=erg                                                              !/*knx        34*/
         zd(9)=qx                                                               !/*knx        35*/
         jz(1)=mkc                                                              !/*knx        36*/
         jz(2)=mspf                                                             !/*knx        37*/
         jz(5)=2                                                                !/*knx        38*/
         call gdtal(8,zero,zd,jz)                                               !/*knx        39*/
      endif                                                                     !/*knx        40*/
      l=locct(llct+1,icl)                                                       !/*knx        41*/
      if(l.ne.0)call tally(qx,1,l,d)                                            !/*knx        42*/
      l=locct(llct+ipt,icl)                                                     !/*knx        43*/
      if(l.ne.0)call tally(qx,2,l,d)                                            !/*knx        44*/
      if(kdb.ne.0)return                                                        !/*knx        45*/
      do 99 i=0,lev-1                                                           !/*knx        46*/
         l=locct(llct+1,int(udt(7,i)))                                          !/*knx        47*/
         if(l.ne.0)call tally(qx,1,l,d)                                         !/*knx        48*/
         l=locct(llct+ipt,int(udt(7,i)))                                        !/*knx        49*/
         if(l.ne.0)call tally(qx,2,l,d)                                         !/*knx        50*/
   99    continue                                                               !/*knx        51*/
      erg=ej                                                                    !/*knx        52*/
#endif                                                                          /*!knx        53*/
      return                                                                    !/*kn         72*/
      end                                                                       !/*kn         73*/
