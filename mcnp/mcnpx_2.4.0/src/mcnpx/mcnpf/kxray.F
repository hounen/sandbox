!#ident kxray.F from deck kx                                                    !/*kx          1*/
      subroutine kxray(d,f)                                                     !/*kx          2*/
!        generate x-rays and/or auger electrons along path segment d.           !/*kx          3*/
!        f = energy interpolation factor.                                       !/*kx          4*/
      use global_data                                                           !/*kxx         1*/
!                                                                               !/*kxx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*kxx         3*/
      implicit integer(kindi) (i-n)                                             !/*kxx         4*/
!                                                                               !/*kxx         5*/
#ifdef MESHTAL                                                                  /*!kxx         6*/
      dimension zd(10),jz(5)                                                    !/*kxx         7*/
      qx=0.                                                                     !/*kxx         8*/
#endif                                                                          /*!kxx         9*/
!                                                                               !/*kx          6*/
      ni=rang()+d*rho(lrho+icl)*xnm(lxnm+mkc)*                                  !/*kx          7*/
     & ((1.-f)*pxr(lpxr+ngp+nee(3)*(mkc-1))+                                    !/*kxx        10*/
     & f*pxr(lpxr+ngp+1+nee(3)*(mkc-1)))                                        !/*kxx        11*/
      if(ni.eq.0)return                                                         !/*kx          9*/
      call savpar(0,3)                                                          !/*kx4c2       1*/
      nb=npb                                                                    !/*kx4c2       2*/
      do 80 n=1,ni                                                              !/*kx4c2       3*/
      dk=rang()*d                                                               !/*kx         18*/
      call uplpos(xxx,uold,lev,dk,vel,0)                                        !/*kx4b        1*/
      wgt=wgt/xnm(lxnm+mkc)                                                     !/*kx         23*/
      jsu=0                                                                     !/*cor4-2    206*/
!                                                                               !/*kx4c        1*/
!        er=energy edge energy for kxray.                                       !/*kx4c        2*/
!        er=xss(jxs(ljxs+4,ix)+nxs(lnxs+4,ix)-1)*1.00001                        !/*kx4c        3*/
!        1.0001 ensures round-off does not give spurious results.               !/*kx4c        4*/
      er=edg(ledg+mkc)*1.0001                                                   !/*kx4c        5*/
      ix=iex                                                                    !/*kx4c        6*/
      mk=mkc                                                                    !/*kx4c        7*/
      ik=ipt                                                                    !/*kx4c        8*/
!                                                                               !/*kx4c        9*/
!        integrated tiger series (its) method before its3.0.                    !/*kx4c       10*/
      if(nxs(lnxs+16,lme(llme+3,jmd(ljmd+mkc))).ne.3)go to 70                   !/*kx4c       11*/
!                                                                               !/*kx4c       12*/
!        assume the electron has enough energy to excite the atom               !/*kx4c       13*/
!        above the threshold of the highest k shell energy of the               !/*kx4c       14*/
!        highest z atom of the material.                                        !/*kx4c       15*/
      if(iphot.ne.0.or.fiml(2).eq.0..or.eek(leek+mkc).le.elc(2))go to 80        !/*kx4c       16*/
      jm=jmd(ljmd+mkc)                                                          !/*kx4c       17*/
      do 60 j=jmd(ljmd+mkc),jmd(ljmd+mkc+1)-1                                   !/*kx4c       18*/
   60 if(iza(liza+j).gt.iza(liza+jm))jm=j                                       !/*kx4c       19*/
      ix=lme(llme+2,jm)                                                         !/*kx4c       20*/
      iex=ix                                                                    !/*kx4c       21*/
      if(er.gt.eg0)er=eg0                                                       !/*kx4c       22*/
!                                                                               !/*kx4c       23*/
!        generate x-rays or auger electrons.                                    !/*kx4c       24*/
#ifdef MESHTAL                                                                  /*!kxx        12*/
   70 call flaug(er,ix,mk,ik,qx)                                                !/*kxx        13*/
#else                                                                           /*!kxx        14*/
   70 call flaug(er,ix,mk,ik)                                                   !/*kxx        15*/
#endif                                                                          /*!kxx        16*/
!                                                                               !/*kx4c       26*/
!        restore particle attributes.                                           !/*kx4c       27*/
   80 call getpar(nb,3)                                                         !/*kx4c2       4*/
      npb=nb-1                                                                  !/*kx4c2       5*/
#ifdef MESHTAL                                                                  /*!kxx        17*/
      if(qx.eq.0.) return                                                       !/*kxx        18*/
      ej=erg                                                                    !/*kxx        19*/
      erg=eg0                                                                   !/*kxx        20*/
      if (mtlflg.ne.0.or.medflg.ne.0) then                                      !/*kxx        21*/
         do 90 i=1,6                                                            !/*kxx        22*/
         if (lev.eq.0) then                                                     !/*kxx        23*/
            zd(i)=gpblcm(i)                                                     !/*kxx        24*/
         else                                                                   !/*kxx        25*/
            zd(i)=udt(i,0)                                                      !/*kxx        26*/
         endif                                                                  !/*kxx        27*/
   90    continue                                                               !/*kxx        28*/
         zd(7)=wgt                                                              !/*kxx        29*/
         zd(8)=erg                                                              !/*kxx        30*/
         zd(9)=qx                                                               !/*kxx        31*/
         jz(1)=mkc                                                              !/*kxx        32*/
         jz(5)=2                                                                !/*kxx        33*/
         call gdtal(8,zero,zd,jz)                                               !/*kxx        34*/
      endif                                                                     !/*kxx        35*/
      l=locct(llct+1,icl)                                                       !/*kxx        36*/
      if(l.ne.0)call tally(qx,1,l,d)                                            !/*kxx        37*/
      l=locct(llct+ipt,icl)                                                     !/*kxx        38*/
      if(l.ne.0)call tally(qx,2,l,d)                                            !/*kxx        39*/
      if(kdb.ne.0)return                                                        !/*kxx        40*/
      do 100 i=0,lev-1                                                          !/*kxx        41*/
      l=locct(llct+1,int(udt(7,i)))                                             !/*kxx        42*/
      if(l.ne.0)call tally(qx,1,l,d)                                            !/*kxx        43*/
      l=locct(llct+ipt,int(udt(7,i)))                                           !/*kxx        44*/
      if(l.ne.0)call tally(qx,2,l,d)                                            !/*kxx        45*/
  100 continue                                                                  !/*kxx        46*/
      erg=ej                                                                    !/*kxx        47*/
#endif                                                                          /*!kxx        48*/
      return                                                                    !/*kx         59*/
      end                                                                       !/*kx         60*/
