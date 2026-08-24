!#ident pnctot.F from deck nt4c2                                                !/*nt4c2       1*/
      subroutine pnctot(mk)                                                     !/*nt4c2       2*/
!        calculate the photonuclear cross section in material mk.               !/*nt4c2       3*/
!                                                                               !/*nt4c2       4*/
!        local variables: ii - isotope index, it - table index,                 !/*nt4c2       5*/
!          ib - upper energy index, ic - lower energy index,                    !/*nt4c2       6*/
!                                                                               !/*nt4c2       7*/
!        global variables:                                                      !/*nt4c2       8*/
!          jmd - material isotope indices                                       !/*nt4c2       9*/
!          lmn - photonuclear isotope table indices                             !/*nt4c2      10*/
!          erg - current photon energy                                          !/*nt4c2      11*/
!          pnt(mk) - the lowest photonuclear threshold in material mk           !/*nt4c2      12*/
!          ktc(1,table) - table index above current energy                      !/*nt4c2      13*/
!          rtc(1,table) - current linear table interpolation factor             !/*nt4c2      14*/
!          rtc(2,table) - current total cross section                           !/*nt4c2      15*/
!          rtc(6,table) - current energy being interpolated                     !/*nt4c2      16*/
      use global_data                                                           !/*ntx         1*/
!                                                                               !/*ntx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*ntx         3*/
      implicit integer(kindi) (i-n)                                             !/*ntx         4*/
!                                                                               !/*ntx         5*/
!                                                                               !/*nt4c2      18*/
      totpn=0.                                                                  !/*nt4c2      19*/
!                                                                               !/*nt4c2      20*/
!        return if below photonuclear threshold for material.                   !/*nt4c2      21*/
      if(erg.le.pnt(lpnt+mk))return                                             !/*nt4c2      22*/
!                                                                               !/*nt4c2      23*/
!        accumulate the cross sections by nuclide in material.                  !/*nt4c2      24*/
      do 60 ii=jmd(ljmd+mk),jmd(ljmd+mk+1)-1                                    !/*nt4c2      25*/
      it=lmn(llmn+ii)                                                           !/*nt4c2      26*/
      if(it.eq.0)go to 60                                                       !/*nt4c2      27*/
      if(erg.eq.rtc(krtc+6,it))go to 50                                         !/*nt4c2      28*/
      rtc(krtc+6,it)=erg                                                        !/*nt4c2      29*/
!                                                                               !/*nt4c2      30*/
!        find index of current energy.                                          !/*nt4c2      31*/
      ic=jxs(ljxs+1,it)                                                         !/*nt4c2      32*/
      ib=ic+nxs(lnxs+3,it)-1                                                    !/*nt4c2      33*/
!                                                                               !/*nt4c2      34*/
!        if below photonuclear threshold, use zero values.                      !/*nt4c2      35*/
      if(erg.ge.xss(ic))go to 10                                                !/*nt4c2      36*/
      ktc(kktc+1,it)=0                                                          !/*nt4c2      37*/
      rtc(krtc+1,it)=0.                                                         !/*nt4c2      38*/
      rtc(krtc+2,it)=0.                                                         !/*nt4c2      39*/
      go to 50                                                                  !/*nt4c2      40*/
!                                                                               !/*nt4c2      41*/
!        if above last energy value, use boundary values.                       !/*nt4c2      42*/
   10 if(erg.lt.xss(ib))go to 20                                                !/*nt4c2      43*/
      ktc(kktc+1,it)=ib-jxs(ljxs+1,it)                                          !/*ntxl        1*/
      if(dbcn(24).ne.zero)ktc(kktc+1,it)=ic-jxs(ljxs+1,it)                      !/*ntxl        2*/
      rtc(krtc+1,it)=0.                                                         !/*nt4c2      45*/
      rtc(krtc+2,it)=xss(jxs(ljxs+2,it)+ktc(kktc+1,it))                         !/*nt4c2      46*/
      go to 50                                                                  !/*nt4c2      47*/
!                                                                               !/*nt4c2      48*/
!        else handle normal value within table.                                 !/*nt4c2      49*/
   20 if(ib-ic.eq.1)go to 40                                                    !/*nt4c2      50*/
      ih=(ic+ib)/2                                                              !/*nt4c2      51*/
      if(erg.lt.xss(ih))go to 30                                                !/*nt4c2      52*/
      ic=ih                                                                     !/*nt4c2      53*/
      go to 20                                                                  !/*nt4c2      54*/
   30 ib=ih                                                                     !/*nt4c2      55*/
      go to 20                                                                  !/*nt4c2      56*/
   40 ktc(kktc+1,it)=ib-jxs(ljxs+1,it)                                          !/*nt4c2      57*/
      rtc(krtc+1,it)=(erg-xss(ic))/(xss(ib)-xss(ic))                            !/*nt4c2      58*/
      k=ktc(kktc+1,it)+jxs(ljxs+2,it)                                           !/*nt4c2      59*/
      rtc(krtc+2,it)=(xss(k)-xss(k-1))*rtc(krtc+1,it)+xss(k-1)                  !/*nt4c2      60*/
!                                                                               !/*nt4c2      61*/
!        accumulate the photonuclear microscopic xs for the material.           !/*nt4c2      62*/
   50 totpn=totpn+rtc(krtc+2,it)*fme(lfme+ii)                                   !/*nt4c2      63*/
   60 continue                                                                  !/*nt4c2      64*/
!                                                                               !/*nt4c2      65*/
!        add totpn to totm for distance to collision calculations.              !/*nt4c2      66*/
      totm=totm+totpn                                                           !/*nt4c2      67*/
      return                                                                    !/*nt4c2      68*/
      end                                                                       !/*nt4c2      69*/
