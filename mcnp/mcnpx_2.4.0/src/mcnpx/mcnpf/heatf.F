!#ident heatf.F from deck xhf                                                 /*xhf         1*/
      function heatf(mk,lx)                                                     !/*xhf         2*/
!       generate heating number for material mk.                                !/*xhf         3*/
!       lx=tally location for pertc perturbation correction.                    !/*xhf         4*/
      use global_data                                                           !/*xhf         5*/
!                                                                               !/*xhf         6*/
      implicit real(kindr) (a-h,o-z)                                            !/*xhf         7*/
      implicit integer(kindi) (i-n)                                             !/*xhf         8*/
!                                                                               !/*xhf         9*/
      h=0.                                                                      !/*xhf        10*/
      h2=0.                                                                     !/*xhf        11*/
      do 10 m=jmd(ljmd+mk),jmd(ljmd+mk+1)-1                                     !/*xhf        12*/
!                                                                               !/*xhf        13*/
!        multigroup heating numbers.                                            !/*xhf        14*/
      if(mcal.ne.0) then                                                        !/*xhf        15*/
         iex=lme(llme+ipt,m)                                                    !/*xhf        16*/
         h2=fme(lfme+m)*getxs(401)                                              !/*xhf        17*/
!                                                                               !/*xhf        18*/
!        neutron and proton heating numbers.                                    !/*xhf        19*/
      else if (ipt.eq.1.or.ipt.eq.9) then                                       !/*xhf        20*/
         i=lme(llme+ipt,m)                                                      !/*xhf        21*/
         l=jxs(ljxs+1,i)+4*nxs(lnxs+3,i)+ktc(kktc+1,i)                          !/*xhf        22*/
         tx=rtc(krtc+4,i)*rtc(krtc+13,i)                                        !/*xhf        23*/
         if(rtc(krtc+11,i).lt.0..or.ipt.eq.9)tx=rtc(krtc+5,i)*                  !/*xhf        24*/
     &   (xss(l-1)+rtc(krtc+1,i)*(xss(l)-xss(l-1)))                             !/*xhf        25*/
         h2=fme(lfme+m)*max(zero,tx)                                            !/*xhf        26*/
!                                                                               !/*xhf        27*/
!        photon heating numbers.                                                !/*xhf        28*/
      else if(ipt.eq.2) then                                                    !/*xhf        29*/
         i=lme(llme+2,m)                                                        !/*xhf        30*/
         h2=fme(lfme+m)*rtc(krtc+4,i)*rtc(krtc+5,i)                             !/*xhf        31*/
      endif                                                                     !/*xhf        32*/
!                                                                               !/*xhf        33*/
!        perturbation correction for f6 tallies with sd 1.                      !/*xhf        34*/
      if(npert.ne.0.and.lx.gt.0)call pertc(2,mk,m,0,lx,h2)                      !/*xhf        35*/
!                                                                               !/*xhf        36*/
   10 h=h+h2                                                                    !/*xhf        37*/
      heatf=h                                                                   !/*xhf        38*/
      return                                                                    !/*xhf        39*/
      end                                                                       !/*xhf        40*/
