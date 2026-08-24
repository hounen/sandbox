!#ident sabcol.F from deck sb                                                   !/*sb          1*/
      subroutine sabcol                                                         !/*sb          2*/
!        calculate exit energy and direction for an s(a,b) collision.           !/*sb          3*/
      use global_data                                                           !/*sbx         1*/
!                                                                               !/*sbx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*sbx         3*/
      implicit integer(kindi) (i-n)                                             !/*sbx         4*/
!                                                                               !/*sbx         5*/
!                                                                               !/*sb          5*/
!        select elastic or inelastic scattering.                                !/*sb          6*/
      ipsc=9                                                                    !/*sb          7*/
      ntyn=0                                                                    !/*sb          8*/
      ixcos=0                                                                   !/*sb          9*/
      if(jxs(ljxs+4,iet).eq.0)go to 10                                          !/*sb         10*/
      if(rang()*rtc(krtc+7,iet).gt.rtc(krtc+8,iet))go to 60                     !/*sb         11*/
!                                                                               !/*sb         12*/
!        get the sampling parameters and energy for the inelastic case.         !/*sb         13*/
   10 if(nxs(lnxs+7,iet).ne.0)go to 20                                          !/*sb         14*/
      kr=rang()*nxs(lnxs+4,iet)                                                 !/*sb         15*/
      go to 50                                                                  !/*sb         16*/
   20 r=rang()*(nxs(lnxs+4,iet)-3)                                              !/*sb         17*/
      if(r.lt.1.)go to 30                                                       !/*sb         18*/
      kr=r+1.                                                                   !/*sb         19*/
      go to 50                                                                  !/*sb         20*/
   30 if(r.lt..5)go to 40                                                       !/*sb         21*/
      kr=nxs(lnxs+4,iet)-2                                                      !/*sb         22*/
      if(r.lt..6)kr=kr+1                                                        !/*sb         23*/
      go to 50                                                                  !/*sb         24*/
   40 kr=1                                                                      !/*sb         25*/
      if(r.lt..1)kr=0                                                           !/*sb         26*/
   50 ll=nxs(lnxs+3,iet)                                                        !/*sb         27*/
      ni=nxs(lnxs+4,iet)*(ll+2)                                                 !/*sb         28*/
      kx=(ktc(kktc+1,iet)-1)*ni+jxs(ljxs+3,iet)+kr*(ll+2)                       !/*sb         29*/
      erg=xss(kx)+rtc(krtc+1,iet)*(xss(kx+ni)-xss(kx))                          !/*sb         30*/
      if(erg.lt.elc(1))return                                                   !/*sb         31*/
      if(erg.lt.1.e-11)erg=1.e-11                                               !/*sb         32*/
      kx=kx+1                                                                   !/*sb         33*/
      ri=rtc(krtc+1,iet)                                                        !/*sb         34*/
      ntyn=1                                                                    !/*sb         35*/
      mtp=4                                                                     !/*cz4c2       4*/
      go to(140,140,70,80)nxs(lnxs+2,iet)+1                                     !/*sb         36*/
!                                                                               !/*sb         37*/
!        set up the sampling parameters for the elastic case.                   !/*sb         38*/
   60 kx=jxs(ljxs+6,iet)+(ktc(kktc+2,iet)-1)*(abs(nxs(lnxs+6,iet))+1)           !/*sb         39*/
      ri=rtc(krtc+4,iet)                                                        !/*sb         40*/
      ni=nxs(lnxs+6,iet)+1                                                      !/*sb         41*/
      ll=nxs(lnxs+6,iet)                                                        !/*sb         42*/
      ntyn=2                                                                    !/*sb         43*/
      go to(140,140,70,80,90)nxs(lnxs+5,iet)+1                                  !/*sb         44*/
!                                                                               !/*sb         45*/
! >>>>>  equally-probable angle bins.                                           !/*sb         46*/
   70 if(rang().le.ri)kx=kx+ni                                                  !/*sb         47*/
      f=ll*rang()+1.                                                            !/*sb         48*/
      j=kx+f                                                                    !/*sb         49*/
      cs=xss(j)+(xss(j-1)-xss(j))*(f-aint(f))                                   !/*sb         50*/
      ixcos=-kx                                                                 !/*sb         51*/
      go to 130                                                                 !/*sb         52*/
!                                                                               !/*sb         53*/
! >>>>>  equally-probable discrete angles.                                      !/*sb         54*/
   80 j=kx+(ll+1)*rang()                                                        !/*sb         55*/
      cs=xss(j)+ri*(xss(j+ni)-xss(j))                                           !/*sb         56*/
      ixcos=kx                                                                  !/*sb         57*/
      go to 130                                                                 !/*sb         58*/
!                                                                               !/*sb         59*/
! >>>>>  exact treatment of coherent elastic scattering                         !/*sb         60*/
   90 ic=jxs(ljxs+5,iet)-1                                                      !/*sb         61*/
      ib=jxs(ljxs+5,iet)+ktc(kktc+2,iet)-1                                      !/*sb         62*/
      if(ib.eq.ic+1)go to 120                                                   !/*sb         63*/
      pr=xss(ib)*rang()                                                         !/*sb         64*/
  100 if(ib-ic.eq.1)go to 120                                                   !/*sb         65*/
      ih=(ic+ib)/2                                                              !/*sb         66*/
      if(pr.lt.xss(ih))go to 110                                                !/*sb         67*/
      ic=ih                                                                     !/*sb         68*/
      go to 100                                                                 !/*sb         69*/
  110 ib=ih                                                                     !/*sb         70*/
      go to 100                                                                 !/*sb         71*/
  120 cs=1.-2.*xss(ib-nint(xss(jxs(ljxs+4,iet))))/erg                           !/*sb4c        1*/
!                                                                               !/*sb         73*/
!        sample the azimuthal angle.                                            !/*sb         74*/
  130 call rotas(cs,uold,uuu,lev,irt)                                           !/*sb4b        1*/
      return                                                                    !/*sb         76*/
  140 call expirx(1,'sabcol','inappropriate distribution required.')            !/*sb         77*/
      return                                                                    !/*sb         78*/
      end                                                                       !/*sb         79*/
