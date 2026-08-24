!#ident cgsdci.F from deck gc                                                   !/*gc          1*/
      subroutine cgsdci(m,nw)                                                   !/*gc          3*/
#if defined(CGS) || defined(DISSCGS)                                            /*!gcx         1*/
!        control workstation nw via cgs device control.                         !/*gc          4*/
!        m=0 for open, 1 for activate, 2 for deactivate.                        !/*gc          5*/
!        nw=1 for terminal, 2 for metafile.                                     !/*gc          6*/
      use global_data                                                           !/*gcx         2*/
!                                                                               !/*gcx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*gcx         4*/
      implicit integer(kindi) (i-n)                                             !/*gcx         5*/
!                                                                               !/*gcx         6*/
      save buff1,buff2,ivsc,jaa                                                 !/*gc          8*/
      real buff1(2048),buff2(2048)                                              !/*gc          9*/
      dimension nn(20),ivsc(300,2),jaa(2)                                       !/*gc         10*/
      character hk(3)*5                                                         !/*gc         11*/
      data hk/'4010','4014','4014e'/,jaa/0,0/                                   !/*gc         12*/
!                                                                               !/*gc         13*/
!        open terminal workstation.                                             !/*gc         14*/
      if(m.ne.0)go to 20                                                        !/*gc         15*/
      jaa(nw)=1                                                                 !/*gc         16*/
      if(nw.eq.2)go to 10                                                       !/*gc         17*/
#ifdef SUNCGS                                                                   /*!cor4-2    223*/
      call gsubeg(ivsc(1,1),buff1,2048)                                         !/*cor4-2    224*/
      call gsucur(ivsc(1,1),3hoff)                                              !/*cor4-2    225*/
      call chrhol(hk(min(3,iterm)),nn,0)                                        !/*cor4-2    226*/
      call gsubat(ivsc(1,1),2hon)                                               !/*cor4-2    227*/
      call gsulim(ivsc(1,1),400-20,1151,200-10,899)                             !/*cor4-2    228*/
      call gsuend(ivsc(1,1))                                                    !/*cor4-2    229*/
#else                                                                           /*!gcx         7*/
      call gtebeg(ivsc(1,1),buff1,2048)                                         !/*gc         18*/
      call gtecur(ivsc(1,1),3hoff)                                              !/*gc         19*/
      call chrhol(hk(min(3,iterm)),nn,0)                                        !/*gc         20*/
      call gtemod(ivsc(1,1),nn)                                                 !/*gc         21*/
      call gtebps(ivsc(1,1),lspeed)                                             !/*gc         22*/
      call gteend(ivsc(1,1))                                                    !/*gc         23*/
#endif                                                                          /*!cpp       493*/
      return                                                                    !/*gc         24*/
!                                                                               !/*gc         25*/
!        open metafile workstation.                                             !/*gc         26*/
   10 call gplbeg(ivsc(1,2),buff2,2048)                                         !/*gc         27*/
      call chrhol(aid,nn,0)                                                     !/*gc         28*/
      call gtitle(ivsc(1,2),nn,60)                                              !/*gc         29*/
      call chrhol(plotm,nn,0)                                                   !/*gc         30*/
      call gfname(ivsc(1,2),nn)                                                 !/*gc         31*/
      call gplend(ivsc(1,2))                                                    !/*gc         32*/
      return                                                                    !/*gc         33*/
!                                                                               !/*gc         34*/
!        activate or deactivate workstation.                                    !/*gc         35*/
   20 if(m.eq.1.and.jaa(nw).eq.0)call gon(ivsc(1,nw))                           !/*gc         36*/
      if(m.eq.2)call goff(ivsc(1,nw))                                           !/*gc         37*/
      jaa(nw)=2-m                                                               !/*gc         38*/
#endif                                                                          /*!gcx         8*/
      return                                                                    !/*gc         39*/
      end                                                                       !/*gc         40*/
