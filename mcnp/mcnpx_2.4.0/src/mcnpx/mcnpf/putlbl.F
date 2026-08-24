!#ident putlbl.F from deck pu                                                   !/*pu          1*/
      subroutine putlbl(lp)                                                     !/*pu          3*/
#ifdef PLOT                                                                     /*!pux         1*/
!        put the surface number label on an arc.                                !/*pu          4*/
!        lp = index in zst of the description of the arc.                       !/*pu          5*/
      use global_data                                                           !/*pux         2*/
      use plot_data                                                             !/*pux         3*/
!                                                                               !/*pux         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*pux         5*/
      implicit integer(kindi) (i-n)                                             !/*pux         6*/
!                                                                               !/*pux         7*/
      dimension fr(8)                                                           !/*pu          8*/
      character hl*10                                                           !/*pu          9*/
      data fr/.4,.5,.625,.25,.75,.125,.875,.42/                                 !/*pu         10*/
!                                                                               !/*pu         11*/
!        convert the surface number and count the characters.                   !/*pu         12*/
!        write out facet of macrobody surface as required.                      !/*pu4c        1*/
      js=zst(lp)                                                                !/*pu         13*/
      if(js.eq.0)return                                                         !/*pu         14*/
      if(abs(js).le.1000000)go to 4                                             !/*pu4c        2*/
      jt=abs(js)                                                                !/*pu4c        3*/
      jq=(jt-1000000)/10                                                        !/*pu4c        4*/
      iq=jt-1000000-jq*10                                                       !/*pu4c        5*/
      js=sign(jq,js)                                                            !/*pu4c        6*/
      write(hl,'(i8,1h.,i1)')js,iq                                              !/*pu4c        7*/
      go to 6                                                                   !/*pu4c        8*/
    4 write(hl,'(i10)')js                                                       !/*pu4c        9*/
    6 call ljusti(hl)                                                           !/*pu4c       10*/
      if(js.lt.0)hl(1:1)='*'                                                    !/*pu         17*/
      n=leng(hl)                                                                !/*pu         18*/
!                                                                               !/*pu         19*/
!        calculate the width and height of the label.                           !/*pu         20*/
      b=n*sch*extent(1)*sclabl(1)                                               !/*pu4a        1*/
      h=sch*extent(2)*sclabl(1)                                                 !/*pu4a        2*/
!                                                                               !/*pu         23*/
!        pick a location that does not overlap any previous label.              !/*pu         24*/
      do 20 i=1,8                                                               !/*pu         25*/
      m=fr(i)*(zst(lp+1)-1.)                                                    !/*pu         26*/
      f=fr(i)*(zst(lp+1)-1.)-m                                                  !/*pu         27*/
      s=zst(lp+3+m*2)*(1.-f)+zst(lp+5+m*2)*f-.5*b                               !/*pu         28*/
      t=zst(lp+4+m*2)*(1.-f)+zst(lp+6+m*2)*f-.5*h                               !/*pu         29*/
      do 10 j=1,nlb                                                             !/*pu         30*/
   10 if(abs(s-plb(lplb+3*j-2)).lt..5*(b+plb(lplb+3*j)).and.                    !/*pu         31*/
     & abs(t-plb(lplb+3*j-1)).lt.h)go to 20                                     !/*pu         32*/
      go to 30                                                                  !/*pu         33*/
   20 continue                                                                  !/*pu         34*/
!                                                                               !/*pu         35*/
!        write the label.                                                       !/*pu         36*/
   30 call gtx(real(s),real(t),hl(1:n))                                         !/*pu         37*/
!                                                                               !/*pu         38*/
!        record the location and width of the current label.                    !/*pu         39*/
      if(3*(nlb+1).ge.3*4*mxj+100)return                                        !/*puxk        1*/
      nlb=nlb+1                                                                 !/*pu         41*/
      plb(lplb+3*nlb-2)=s                                                       !/*pu         42*/
      plb(lplb+3*nlb-1)=t                                                       !/*pu         43*/
      plb(lplb+3*nlb)=b                                                         !/*pu         44*/
#endif                                                                          /*!pux         9*/
      return                                                                    !/*pu         45*/
      end                                                                       !/*pu         46*/
