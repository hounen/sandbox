!#ident photot.F from deck pt                                                   !/*pt          1*/
      subroutine photot(mk)                                                     !/*pt          2*/
!        calculate photon cross sections in cell icl.                           !/*pt          3*/
      use global_data                                                           !/*ptx         1*/
!                                                                               !/*ptx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*ptx         3*/
      implicit integer(kindi) (i-n)                                             !/*ptx         4*/
!                                                                               !/*ptx         5*/
!                                                                               !/*pt          5*/
      totm=0.                                                                   !/*pt          6*/
      siga=0.                                                                   !/*pt          7*/
      if(mcal.ne.0)go to 80                                                     !/*pt          8*/
      el=log(erg)                                                               !/*pt          9*/
!                                                                               !/*pt4c2       1*/
!        calculate photonuclear cross sections.                                 !/*pt4c2       2*/
      if(ispn.ne.0)call pnctot(mk)                                              !/*pt4c2       3*/
!                                                                               !/*pt         10*/
!        calculate cross sections for each nuclide in the cell.                 !/*pt         11*/
      do 70 m=jmd(ljmd+mk),jmd(ljmd+mk+1)-1                                     !/*pt         12*/
      ie=lme(llme+2,m)                                                          !/*pt         13*/
      if(erg.eq.rtc(krtc+6,ie))go to 60                                         !/*pt         14*/
      nz=nxs(lnxs+3,ie)                                                         !/*pt         15*/
!                                                                               !/*pt         16*/
!        binary search for the location of log(energy) in the table.            !/*pt         17*/
      ic=jxs(ljxs+1,ie)                                                         !/*pt         18*/
      ib=ic+nz-1                                                                !/*pt         19*/
   10 if(ib-ic.eq.1)go to 30                                                    !/*pt         20*/
      ih=(ic+ib)/2                                                              !/*pt         21*/
      if(el.lt.xss(ih))go to 20                                                 !/*pt         22*/
      ic=ih                                                                     !/*pt         23*/
      go to 10                                                                  !/*pt         24*/
   20 ib=ih                                                                     !/*pt         25*/
      go to 10                                                                  !/*pt         26*/
!                                                                               !/*pt         27*/
!        generate incoherent, coherent, photo-electric, and pair                !/*pt         28*/
!        production cross sections and store in rtc.                            !/*pt         29*/
   30 t1=(el-xss(ib))/(xss(ib-1)-xss(ib))                                       !/*pt         30*/
      k=ib-jxs(ljxs+1,ie)+jxs(ljxs+5,ie)                                        !/*pt         31*/
      rtc(krtc+5,ie)=xss(k)*(xss(k-1)/xss(k))**t1                               !/*pt         32*/
      c=0.                                                                      !/*pt         33*/
      t2=0.                                                                     !/*pt         34*/
      do 50 j=1,4                                                               !/*pt         35*/
      ib=ib+nz                                                                  !/*pt         36*/
      t3=0.                                                                     !/*pt         37*/
      if(xss(ib)*xss(ib-1).eq.0.)go to 50                                       !/*pt         38*/
      t3=exp(xss(ib)+t1*(xss(ib-1)-xss(ib)))                                    !/*pt         39*/
!                                                                               !/*pt         40*/
!        if mcg treatment, set coherent cross section to zero and               !/*pt         41*/
!        adjust heating with ratio of total cross sections.                     !/*pt         42*/
      if(j.ne.2)go to 40                                                        !/*pt         43*/
      if(erg.lt.emcf(2).and.nocoh.eq.0)go to 40                                 !/*pt         44*/
      c=t3                                                                      !/*pt         45*/
      t3=0.                                                                     !/*pt         46*/
   40 t2=t2+t3                                                                  !/*pt         47*/
   50 rtc(krtc+j,ie)=t2                                                         !/*pt         48*/
      if(erg.ge.emcf(2))rtc(krtc+5,ie)=rtc(krtc+5,ie)*                          !/*pt         49*/
     & (rtc(krtc+4,ie)+c)/rtc(krtc+4,ie)                                        !/*pt         50*/
      rtc(krtc+6,ie)=erg                                                        !/*pt         51*/
!                                                                               !/*pt         52*/
!        accumulate the total cross section, totm.                              !/*pt         53*/
   60 totm=totm+rtc(krtc+4,ie)*fme(lfme+m)                                      !/*pt         54*/
   70 siga=siga+(rtc(krtc+3,ie)-rtc(krtc+2,ie))*fme(lfme+m)                     !/*pt         55*/
      return                                                                    !/*pt         56*/
!                                                                               !/*pt         57*/
!        compute multigroup cross sections.                                     !/*pt         58*/
   80 do 90 m=jmd(ljmd+mk),jmd(ljmd+mk+1)-1                                     !/*pt         59*/
      ie=lme(llme+2,m)                                                          !/*pt         60*/
      rtc(krtc+4,ie)=xss(jxs(ljxs+2,ie)-1+jgp)                                  !/*pt         61*/
      totm=totm+rtc(krtc+4,ie)*fme(lfme+m)                                      !/*pt         62*/
   90 siga=siga+xss(jxs(ljxs+6,ie)-1+jgp)*fme(lfme+m)                           !/*pt         63*/
      return                                                                    !/*pt         64*/
      end                                                                       !/*pt         65*/
