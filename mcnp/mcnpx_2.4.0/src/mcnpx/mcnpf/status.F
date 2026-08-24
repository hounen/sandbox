!#ident status.F from deck tu                                                   !/*tu          1*/
      subroutine status(ju)                                                     !/*tu          3*/
#ifdef MCPLOT                                                                   /*!tux         1*/
!        print the status of the mcplot settings on unit ju.                    !/*tu          4*/
      use global_data                                                           !/*tux         2*/
      use mcplot_data                                                           !/*tux         3*/
!                                                                               !/*tux         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*tux         5*/
      implicit integer(kindi) (i-n)                                             !/*tux         6*/
!                                                                               !/*tux         7*/
      character hr*69,hp*2,hc*7,hi*25                                           !/*tu4a        1*/
      data hp/' %'/,hc/'ss cdcc'/,hi/'mefvspntl1234567812345678'/               !/*tu4a        2*/
!                                                                               !/*tu          9*/
      if(ktfile.eq.1)write(ju,'(9x,10hruntpe is ,a8)')runtpe                    !/*tu         10*/
      if(ktfile.eq.2)write(ju,'(9x,15hrmctal file is ,a8)')mctal                !/*tu         11*/
      if(nonorm.ne.0)write(ju,'(9x,6hnonorm)')                                  !/*tu         12*/
      if(ititle(5).ne.0)write(ju,'(9x,9htitle = ",a40,1h")')titles(5)           !/*tu         13*/
      if(ititle(6).ne.0)write(ju,'(9x,9htitle = ",a40,1h")')titles(6)           !/*tu         14*/
      if(ititle(7).ne.0)write(ju,'(9x,10hsubtitle=",a40,1h")')titles(7)         !/*tu         15*/
      if(ititle(1).ne.0)write(ju,'(9x,10hxtitle = ",a40,1h")')titles(1)         !/*tu         16*/
      if(ititle(2).ne.0)write(ju,'(9x,10hytitle = ",a40,1h")')titles(2)         !/*tu         17*/
      if(ititle(3).ne.0)write(ju,'(9x,10hztitle = ",a40,1h")')titles(3)         !/*tu         18*/
      if(itfc.gt.0.and.itfc.le.25)                                              !/*tu4a        3*/
     & write(ju,'(9x,4htfc ,a1)')hi(itfc:itfc)                                  !/*tu4a        4*/
      if(itfc.gt.30)write(ju,'(9x,6hkcode ,i2)')itfc-30                         !/*tu4a        5*/
      if(lax.eq.1)write(ju,'(9x,31hlinear x axis and linear y axis)')           !/*tu         21*/
      if(lax.eq.2)                                                              !/*tu         22*/
     & write(ju,'(9x,36hlinear x axis and logarithmic y axis)')                 !/*tu         23*/
      if(lax.eq.3)                                                              !/*tu         24*/
     & write(ju,'(9x,36hlogarithmic x axis and linear y axis)')                 !/*tu         25*/
      if(lax.eq.4)                                                              !/*tu         26*/
     & write(ju,'(9x,41hlogarithmic x axis and logarithmic y axis)')            !/*tu         27*/
      if(kurv.eq.1)write(ju,'(9x,9hhistogram)')                                 !/*tu         28*/
      if(kurv.eq.2)write(ju,'(9x,7hplinear)')                                   !/*tu         29*/
      if(kurv.eq.3.and.tensn.eq.0.)write(ju,'(9x,17hspline (stineman))')        !/*tu         30*/
      if(kurv.eq.3.and.tensn.ne.0.)                                             !/*tu         31*/
     & write(ju,'(9x,27hspline (rational, tension =,f11.3,1h))')tensn           !/*tu         32*/
      if(kurv.eq.4)write(ju,'(9x,3hbar)')                                       !/*tu         33*/
      if(noerbr.ne.0)write(ju,'(9x,8hnoerrbar)')                                !/*tu         34*/
      write(ju,'(9x,18hcurve thickness = ,f6.3)')cthick                         !/*tu         35*/
      j=0                                                                       !/*tu4b        1*/
      if(lpert.gt.0)j=iptb(lipb+1,lpert)                                        !/*tu4b        2*/
      if(mtal.ne.0)write(ju,'(9x,6htally ,i5,15h, perturbation ,i5)')           !/*tu4b        3*/
     & jptal(ljpt+1,mtal),j                                                     !/*tu4b        4*/
      if(nfree.eq.2.and.j3d.eq.0)                                               !/*tu         37*/
     & write(ju,'(9x,8hcontour:,3g16.5,3x,a1)')contur,hp(ipct+1:ipct+1)         !/*tu         38*/
      if(nfree.eq.2.and.j3d.ne.0)                                               !/*tu         39*/
     & write(ju,'(9x,2h3d,10x,5hview:,2f12.3)')pht                              !/*tu         40*/
      if(jlim(1).ne.0)write(ju,'(9x,7hx axis:,2g20.10)')xyzmn(1),               !/*tu         41*/
     & xyzmx(1)                                                                 !/*tu         42*/
      if(nfree.eq.2.and.jlim(2).ne.0)                                           !/*tu         43*/
     & write(ju,'(9x,7hy axis:,2g20.10)')xyzmn(2),xyzmx(2)                      !/*tu         44*/
      if(mtal.eq.0)go to 20                                                     !/*tu4b        5*/
      write(ju,'(/9x,16hbin information:/)')                                    !/*tu         46*/
      j=jptal(ljpt+2,mtal)                                                      !/*tu         47*/
      do 10 k=1,8                                                               !/*tu         48*/
      hr='         '//ibin(k:k)//'          *'                                  !/*tu         49*/
      if(k.eq.1)hr(15:15)=hc(j:j)                                               !/*tu         50*/
      if(ifree(1).eq.k.or.ifree(nfree).eq.k)go to 10                            !/*tu         51*/
      write(hr(17:21),'(i5)')kbin(k,1)                                          !/*tu         52*/
      if(k.gt.2.and.kbin(k,1).eq.iptal(lipt+k,4,mtal))hr(23:23)='t'             !/*tu         53*/
   10 write(ju,'(a69)')hr                                                       !/*tu         54*/
!                                                                               !/*tu4b        6*/
!        status of cross section plot.                                          !/*tu4b        7*/
   20 if(kxsplt.eq.0)return                                                     !/*tu4b        8*/
      if(kxspma.ne.0)write(ju,'(9x,10hmaterial: ,a10)')xspttl                   !/*tu4b        9*/
      if(kxspma.eq.0)write(ju,'(9x,6hzaid: ,a10)')xspttl                        !/*tu4b       10*/
      do 30 j=jlbl(1,kxsptp),jlbl(2,kxsptp)                                     !/*tu4b       11*/
   30 if(kxspmt.eq.klbl(j))go to 40                                             !/*tu4b       12*/
      j=jlbl(2,kxsptp)                                                          !/*tu4b       13*/
   40 write(ju,'(9x,4hmt =,i3,5x,a40)')klbl(j),hlbl(j)                          !/*tu4b       14*/
#endif                                                                          /*!tux         8*/
      return                                                                    !/*tu         55*/
      end                                                                       !/*tu         56*/
