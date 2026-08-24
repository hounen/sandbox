!#ident sig_max.F from deck xsm                                               /*xsm         1*/
      subroutine sig_max(mk,emin,emax)                                          !/*xsm         2*/
!  this routine returns the maximum total cross section (totm)                  !/*xsm         3*/
!  for material mk over the energy range from emin to emax.                     !/*xsm         4*/
      use global_data                                                           !/*xsm         5*/
!                                                                               !/*xsm         6*/
      implicit real(kindr) (a-h,o-z)                                            !/*xsm         7*/
      implicit integer(kindi) (i-n)                                             !/*xsm         8*/
!                                                                               !/*xsm         9*/
!                                                                               !/*xsm        10*/
!  k(1)=index in cross-section table for e(1)                                   !/*xsm        11*/
!  r(1)=table interpolation fraction for e(1)                                   !/*xsm        12*/
!  k(2)=index in cross-section table for e(2)                                   !/*xsm        13*/
!  r(2)=table interpolation fraction for e(2)                                   !/*xsm        14*/
!  t(1)=total cross section at e(1)                                             !/*xsm        15*/
!  t(2)=total cross section at e(2)                                             !/*xsm        16*/
!                                                                               !/*xsm        17*/
!  local variables                                                              !/*xsm        18*/
      real e(2), r(2), t(2)                                                     !/*xsm        19*/
      integer k(2)                                                              !/*xsm        20*/
!                                                                               !/*xsm        21*/
      e(1)=emin                                                                 !/*xsm        22*/
      e(2)=emax                                                                 !/*xsm        23*/
      totm=0.                                                                   !/*xsm        24*/
!                                                                               !/*xsm        25*/
!     loop over number of nuclides in this material                             !/*xsm        26*/
      do m=jmd(ljmd+mk),jmd(ljmd+mk+1)-1                                        !/*xsm        27*/
         iex=lme(llme+ipt,m)                                                    !/*xsm        28*/
         n=nxs(lnxs+3,iex)                                                      !/*xsm        29*/
!                                                                               !/*xsm        30*/
!        loop over e(1) and e(2)                                                !/*xsm        31*/
         do ie=1,2                                                              !/*xsm        32*/
            ic=jxs(ljxs+1,iex)                                                  !/*xsm        33*/
            ib=ic+n-1                                                           !/*xsm        34*/
            if(e(ie).lt.rtc(krtc+6,iex))ib=ic+ktc(kktc+1,iex)                   !/*xsm        35*/
            if(e(ie).gt.rtc(krtc+6,iex))ic=ic+ktc(kktc+1,iex)-1                 !/*xsm        36*/
   60       if(ib-ic.eq.1)go to 80                                              !/*xsm        37*/
            ih=(ic+ib)/2                                                        !/*xsm        38*/
            if(e(ie).lt.xss(ih))go to 70                                        !/*xsm        39*/
            ic=ih                                                               !/*xsm        40*/
            go to 60                                                            !/*xsm        41*/
   70       ib=ih                                                               !/*xsm        42*/
            go to 60                                                            !/*xsm        43*/
!                                                                               !/*xsm        44*/
!           get the bin index, the interpolation fraction, and                  !/*xsm        45*/
!           the total cross section.                                            !/*xsm        46*/
   80       k(ie)=ic                                                            !/*xsm        47*/
            ex=(e(ie)-xss(ic))/(xss(ib)-xss(ic))                                !/*xsm        48*/
            r(ie)=max(zero,min(one,ex))                                         !/*xsm        49*/
            t(ie)=xss(ic+n)+r(ie)*(xss(ib+n)-xss(ic+n))                         !/*xsm        50*/
         enddo                                                                  !/*xsm        51*/
!                                                                               !/*xsm        52*/
!        find max total xs (xm) for this nuclide over requested                 !/*xsm        53*/
!        energy range                                                           !/*xsm        54*/
!                                                                               !/*xsm        55*/
         xm=max(t(1),t(2))                                                      !/*xsm        56*/
         ic=k(1)                                                                !/*xsm        57*/
         if(r(1).gt.0.) ic=ic+1                                                 !/*xsm        58*/
         ib=k(2)                                                                !/*xsm        59*/
         if(r(2).eq.0.) ib=ib-1                                                 !/*xsm        60*/
         if(ib.ge.ic) then                                                      !/*xsm        61*/
            do i=ic,ib                                                          !/*xsm        62*/
               if(xss(i+n).gt.xm) xm=xss(i+n)                                   !/*xsm        63*/
            enddo                                                               !/*xsm        64*/
         endif                                                                  !/*xsm        65*/
         totm=totm+xm*fme(lfme+m)                                               !/*xsm        66*/
      enddo                                                                     !/*xsm        67*/
      return                                                                    !/*xsm        68*/
      end                                                                       !/*xsm        69*/
