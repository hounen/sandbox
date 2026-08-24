!#ident sig_tot.F from deck xst                                               /*xst         1*/
      subroutine sig_tot(mk)                                                    !/*xst         2*/
!  this routine loads various cross sections in the rtc array                   !/*xst         3*/
!  at energy erg for material mk.  it also loads totm and siga.                 !/*xst         4*/
      use global_data                                                           !/*xst         5*/
!                                                                               !/*xst         6*/
      implicit real(kindr) (a-h,o-z)                                            !/*xst         7*/
      implicit integer(kindi) (i-n)                                             !/*xst         8*/
!                                                                               !/*xst         9*/
!                                                                               !/*xst        10*/
!  ktc(1,iex)=index in cross-section table (also ktc(2,iex))                    !/*xst        11*/
!  rtc(1,iex)=table interpolation fraction (also rtc(2,iex))                    !/*xst        12*/
!  rtc(3,iex)=absorption cross section                                          !/*xst        13*/
!  rtc(4,iex)=total cross section [also rtc(5,iex)]                             !/*xst        14*/
!  rtc(6,iex)=energy                                                            !/*xst        15*/
!                                                                               !/*xst        16*/
      totm=0.                                                                   !/*xst        17*/
      siga=0.                                                                   !/*xst        18*/
!                                                                               !/*xst        19*/
!  loop over number of nuclides in this material                                !/*xst        20*/
!                                                                               !/*xst        21*/
      do m=jmd(ljmd+mk),jmd(ljmd+mk+1)-1                                        !/*xst        22*/
         iex=lme(llme+ipt,m)                                                    !/*xst        23*/
         if(rtc(krtc+6,iex).ne.erg) then                                        !/*xst        24*/
            n=nxs(lnxs+3,iex)                                                   !/*xst        25*/
            ic=jxs(ljxs+1,iex)                                                  !/*xst        26*/
            ib=ic+n-1                                                           !/*xst        27*/
            if(erg.lt.rtc(krtc+6,iex))ib=ic+ktc(kktc+1,iex)                     !/*xst        28*/
            if(erg.gt.rtc(krtc+6,iex))ic=ic+ktc(kktc+1,iex)-1                   !/*xst        29*/
   60       if(ib-ic.eq.1)go to 80                                              !/*xst        30*/
            ih=(ic+ib)/2                                                        !/*xst        31*/
            if(erg.lt.xss(ih))go to 70                                          !/*xst        32*/
            ic=ih                                                               !/*xst        33*/
            go to 60                                                            !/*xst        34*/
   70       ib=ih                                                               !/*xst        35*/
            go to 60                                                            !/*xst        36*/
!                                                                               !/*xst        37*/
!           get the bin index, the interpolation fraction,                      !/*xst        38*/
!           and the absorption and total cross sections.                        !/*xst        39*/
   80       ktc(kktc+1,iex)=ic-jxs(ljxs+1,iex)+1                                !/*xst        40*/
            ex=(erg-xss(ic))/(xss(ib)-xss(ic))                                  !/*xst        41*/
            rtc(krtc+1,iex)=max(zero,min(one,ex))                               !/*xst        42*/
            ktc(kktc+2,iex)=ktc(kktc+1,iex)                                     !/*xst        43*/
            rtc(krtc+2,iex)=rtc(krtc+1,iex)                                     !/*xst        44*/
            rtc(krtc+3,iex)=xss(ic+2*n)+rtc(krtc+1,iex)*                        !/*xst        45*/
     &         (xss(ic+2*n+1)-xss(ic+2*n))                                      !/*xst        46*/
            rtc(krtc+4,iex)=xss(ic+n)+rtc(krtc+1,iex)*                          !/*xst        47*/
     &         (xss(ib+n)-xss(ic+n))                                            !/*xst        48*/
            rtc(krtc+5,iex)=rtc(krtc+4,iex)                                     !/*xst        49*/
            rtc(krtc+6,iex)=erg                                                 !/*xst        50*/
         endif                                                                  !/*xst        51*/
         totm=totm+rtc(krtc+5,iex)*fme(lfme+m)                                  !/*xst        52*/
         siga=siga+rtc(krtc+3,iex)*fme(lfme+m)                                  !/*xst        53*/
      enddo                                                                     !/*xst        54*/
      return                                                                    !/*xst        55*/
      end                                                                       !/*xst        56*/
