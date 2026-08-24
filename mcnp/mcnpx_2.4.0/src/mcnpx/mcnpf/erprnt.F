!#ident erprnt.F from deck er                                                   !/*er          1*/
      subroutine erprnt(im,jf,nk,k1,k2,k3,k4,js,hf)                             !/*er          2*/
!        print  messages about errors in the problem specifications.            !/*er          3*/
!        im=1  print message only.    im=2  klin also, to controller.           !/*er          4*/
!        jf=1  fatal error.           jf=2  warning only.                       !/*er          5*/
!        nk = how many of the parameters k1 thru k4 are to be printed.          !/*er          6*/
!        js > 0 set ics = -1;  js < 0 print to jtty only.                       !/*er4a        1*/
!        hf = partial printing format.                                          !/*er          8*/
!                                                                               !/*er          9*/
!        tty messages on some systems are limited to 70 characters.             !/*er         10*/
!        so fatal errors should have only 55 characters, warnings 59.           !/*er         11*/
!                                                                               !/*er         12*/
      use global_data                                                           !/*erx         1*/
!                                                                               !/*erx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*erx         3*/
      implicit integer(kindi) (i-n)                                             !/*erx         4*/
!                                                                               !/*erx         5*/
      dimension kk(4)                                                           !/*er         14*/
      character hf*(*),hp*100,hs(2)*20                                          !/*er         15*/
      data hs/'(15h fatal error.  ,','(11h warning.  ,'/                        !/*er         16*/
!                                                                               !/*er         17*/
!        move the parameters to be printed into the printing block.             !/*er         18*/
      kk(1)=k1                                                                  !/*er         19*/
      kk(2)=k2                                                                  !/*er         20*/
      kk(3)=k3                                                                  !/*er         21*/
      kk(4)=k4                                                                  !/*er         22*/
!                                                                               !/*er         23*/
!        assemble the format and print the error message.                       !/*er         24*/
      hp=hs(jf)//hf//')'                                                        !/*er         25*/
      if(js.ge.0.and.im.eq.1)write(iuo,'(1h )')                                 !/*er4a        2*/
      if(iln1.lt.0.and.im.eq.2)write(iuo,'(1x,a69)')klin(1:69)                  !/*er4a        3*/
      if(js.ge.0.and.nk.eq.0)write(iuo,hp)                                      !/*er4a        4*/
      if(js.ge.0.and.nk.gt.0)write(iuo,hp)(kk(i),i=1,nk)                        !/*er4a        5*/
!                                                                               !/*er         29*/
!        also print, if required, to the controller.                            !/*er         30*/
!        in production mode messages are not sent to the controller.            !/*er         31*/
      if((kprod.ne.0.or.nwer+nfer.ge.20).and.(nfer.ne.0.or.jf.ne.1))            !/*er         32*/
     & go to 10                                                                 !/*er         33*/
      if(iovr.eq.1.and.im.ne.2.and.iln1.ne.0)write(jtty,'(1h )')                !/*er         34*/
      if(im.ne.2)iln1=0                                                         !/*er         35*/
      if(iln.ne.iln1.and.im.eq.2)write(jtty,'(1x,a69)')klin(1:69)               !/*er         36*/
      if(im.eq.2)iln1=iln                                                       !/*er         37*/
      if(nk.eq.0)write(jtty,hp)                                                 !/*er         38*/
      if(nk.gt.0)write(jtty,hp)(kk(i),i=1,nk)                                   !/*er         39*/
!                                                                               !/*er         40*/
!        limit the number of error messages to the controller to 20.            !/*er         41*/
   10 if(kprod.eq.0.and.nfer+nwer.eq.20)write(jtty,20)outp                      !/*er         42*/
   20 format(35h additional error messages on file ,a8)                         !/*er         43*/
!                                                                               !/*er         44*/
!        increment count of fatal errors or warnings.                           !/*er         45*/
      if(jf.eq.1)go to 30                                                       !/*er         46*/
      if(npp.ge.0)nwer=nwer+1                                                   !/*er4a        6*/
      go to 50                                                                  !/*er         48*/
!                                                                               !/*er         49*/
!        fatal-error breakpoint label.                                          !/*er         50*/
   30 nfer=nfer+1                                                               !/*er         51*/
      if(mct.lt.0)go to 50                                                      !/*er4b        1*/
      do 40 i=1,mink                                                            !/*er         52*/
   40 ink(i)=1                                                                  !/*er         53*/
      mnk=1                                                                     !/*er         54*/
   50 if(js.gt.0)ics=-1                                                         !/*er4a        7*/
      return                                                                    !/*er         56*/
      end                                                                       !/*er         57*/
