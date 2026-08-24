!#ident bankit.F from deck ba                                                   !/*ba          1*/
      subroutine bankit(m)                                                      !/*ba          2*/
!        store, m<100, or retrieve, m=100, a banked particle.                   !/*ba4a        1*/
      use global_data                                                           !/*bax         1*/
!                                                                               !/*bax         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*bax         3*/
      implicit integer(kindi) (i-n)                                             !/*bax         4*/
!                                                                               !/*bax         5*/
      parameter (nx=nbmx/2)                                                     !/*ba          5*/
!                                                                               !/*ba          6*/
      lb=nptb(lnpt+npert+1)                                                     !/*ba4b        1*/
      lu=lpblcm+2*abs(iunr)                                                     !/*ba4c        1*/
      if(m.eq.100)go to 130                                                     !/*ba4a        2*/
!                                                                               !/*ba         10*/
!********************  m<100 -- bank a particle.  **********************        !/*bax         6*/
!                                                                      *        !/*ba         12*/
      if(npa.ne.0)go to 10                                                      !/*ba         13*/
      call expirx(1,'bankit',                                                   !/*ba         14*/
     & 'a no-count particle tried to enter the bank.')                          !/*ba         15*/
      return                                                                    !/*ba         16*/
   10 if(kpt(ipt).eq.0) then                                                    !/*bax         7*/
         racctc(ipt,1)=racctc(ipt,1)+wgt                                        !/*bax         8*/
         racctc(ipt,2)=racctc(ipt,2)+wgt*erg                                    !/*bax         9*/
         return                                                                 !/*bax        10*/
      endif                                                                     !/*bax        11*/
      if(nee(ipt).gt.0) then                                                    !/*bax        12*/
         if(erg.gt.eee(leee+ipt,1)) then                                        !/*bax        13*/
            eacctc(ipt,1)=eacctc(ipt,1)+wgt                                     !/*bax        14*/
            eacctc(ipt,2)=eacctc(ipt,2)+wgt*erg                                 !/*bax        15*/
            eacctc(ipt,3)=max(erg,eacctc(ipt,3))                                !/*bax        16*/
            return                                                              !/*bax        17*/
         endif                                                                  !/*bax        18*/
      endif                                                                     !/*bax        19*/
!                                                                               !/*ba         22*/
!        make room for the new particle if necessary.                           !/*ba         23*/
      if(jbnk.lt.nbmx)go to 80                                                  !/*bax        20*/
!                                                                               !/*ba         25*/
!        create the bank backup file if necessary.                              !/*ba         26*/
#ifdef MULTT                                                                    /*!baxk        1*/
      call thread_lockon(jlock)                                                 !/*baxk        2*/
#endif                                                                          /*!baxk        3*/
      if(lbb(llbb+ktask+1).ne.0)go to 40                                        !/*ba         28*/
      lbb(llbb+ktask+1)=1                                                       !/*ba         29*/
      write(iuo,30)npstc                                                        !/*ba         30*/
      if(ntasks.le.1.and.ltasks.le.1)write(jtty,30)npstc                        !/*ba4c        2*/
   30 format(50h bank is full.  bank backup file is being created.,             !/*ba         32*/
     & 7h  nps =,i10)                                                           !/*ba         33*/
      open(iub+ktask,form='unformatted',status='scratch')                       !/*ba         34*/
!                                                                               !/*ba         35*/
!        increment the bank overflow count, nbov, if not yet done.              !/*ba         36*/
   40 if(lsb.ne.0)go to 50                                                      !/*ba         37*/
      lsb=1                                                                     !/*ba         38*/
      nbov=nbov+1                                                               !/*ba         39*/
!                                                                               !/*ba         40*/
!        write the first half (one block) of bank to the backup file.           !/*ba         41*/
   50 write (iub+ktask) ibnk(kbnk:kbnk+lu*nx)                                   !/*bax        22*/
      write (iub+ktask) gbnk(kgbn+1:kgbn+ibnk(kbnk))                            !/*bax        23*/
#ifdef MULTT                                                                    /*!baxk        4*/
      call thread_lockoff(jlock)                                                !/*baxk        5*/
#endif                                                                          /*!baxk        6*/
!                                                                               !/*ba         45*/
!        shift the second half of the bank to the first half.                   !/*ba         46*/
      do 60 i=1,lu*(nbmx-nx)                                                    !/*ba4c        4*/
   60 ibnk(kbnk+i)=ibnk(kbnk+lu*nx+i)                                           !/*ba4c        5*/
      do 70 i=1,mbb-ibnk(kbnk)                                                  !/*ba         49*/
   70 gbnk(kgbn+i)=gbnk(kgbn+ibnk(kbnk)+i)                                      !/*ba         50*/
      jbnk=jbnk-nx                                                              !/*ba         51*/
      mbb=mbb-ibnk(kbnk)                                                        !/*ba         52*/
      ibnk(kbnk)=mbb                                                            !/*ba4a        5*/
!                                                                               !/*ba         53*/
!        bank the particle.  increment particle counts.                         !/*ba         54*/
!        pack iex with m and ntyn for use in eventp, ptrak.                     !/*ba4c2       1*/
   80 nbnk=nbnk+1                                                               !/*ba         55*/
      nbhwtc=max(nbhwtc,nbnk)                                                   !/*ba         56*/
      node=node+1                                                               !/*ba         57*/
      ie=iex                                                                    !/*ba4a        6*/
      iex=sign(m*1000000+min(abs(iex),999)*1000+min(abs(ntyn),999),ntyn)        !/*ba4a        7*/
      nbttc(ipt)=nbttc(ipt)+1                                                   !/*ba         58*/
      rnk=rnr+rnrtc                                                             !/*ba         59*/
      do 90 i=1,lpblcm                                                          !/*ba         60*/
   90 ibnk(kbnk+jbnk*lu+i)=jpblcm(i)                                            !/*ba4c        6*/
      iex=ie                                                                    !/*ba4c2       2*/
      do 100 i=1,npblcm                                                         !/*ba         62*/
  100 gbnk(kgbn+mbb+i)=gpblcm(i)                                                !/*ba         63*/
      mbb=mbb+npblcm                                                            !/*ba4b        2*/
      do 115 i=1,lb                                                             !/*ba4b        6*/
      do 115 j=1,3                                                              !/*ba4b        7*/
  115 gbnk(kgbn+mbb+(i-1)*3+j)=ptb(kptb+j,i)                                    !/*ba4b        8*/
      mbb=mbb+3*lb                                                              !/*ba4b        9*/
      if(lev.eq.0)go to 121                                                     !/*ba4c2       3*/
      do 120 j=0,lev                                                            !/*ba4c2       4*/
      do 120 i=1,10                                                             !/*ba4c2       5*/
  120 gbnk(kgbn+mbb+i+10*j)=udt(i,j)                                            !/*ba4c2       6*/
      mbb=mbb+10*lev                                                            !/*ba4b       12*/
!                                                                               !/*ba4c        7*/
!        write out rtcr array for unresolved treatment                          !/*ba4c        8*/
  121 if(iunr.eq.0)go to 128                                                    !/*ba4c2       7*/
      if(ipt.ne.1)go to 126                                                     !/*ba4c       10*/
      if(erg.le.xunrl.or.erg.ge.xunru)go to 126                                 !/*ba4c       11*/
      ju=0                                                                      !/*ba4c2       8*/
      do 124 i=1,mxe                                                            !/*ba4c       12*/
      if(nty(lnty+i).ne.1)go to 124                                             !/*ba4c       13*/
      lk=jxs(ljxs+23,i)                                                         !/*ba4c       15*/
      if(lk.eq.0)go to 124                                                      !/*ba4c       16*/
      ju=ju+1                                                                   !/*ba4c2       9*/
      gbnk(kgbn+mbb+(ju-1)*15+6)=0.                                             !/*ba4c2      10*/
      if(rtc(krtc+6,i).ne.erg)go to 124                                         !/*ba4c2      11*/
      if(erg.le.xss(lk+6))go to 124                                             !/*ba4c       17*/
      if(erg.ge.xss(lk+5+nint(xss(lk))))go to 124                               !/*ba4c       18*/
      do 122 j=1,15                                                             !/*ba4c       19*/
  122 gbnk(kgbn+mbb+(ju-1)*15+j)=rtc(krtc+j,i)                                  !/*ba4c2      12*/
      ibnk(kbnk+jbnk*lu+lpblcm+(ju-1)*2+1)=ktc(kktc+1,i)                        !/*ba4c2      13*/
      ibnk(kbnk+jbnk*lu+lpblcm+(ju-1)*2+2)=ktc(kktc+2,i)                        !/*ba4c2      14*/
  124 continue                                                                  !/*ba4c       23*/
  126 mbb=mbb+15*abs(iunr)                                                      !/*ba4c       24*/
!                                                                               !/*ba4c       25*/
  128 jbnk=jbnk+1                                                               !/*ba4c       26*/
      if(jbnk.eq.nx)ibnk(kbnk)=mbb                                              !/*ba         70*/
      return                                                                    !/*ba         71*/
!                                                                               !/*ba         72*/
!************  m=100 -- retrieve a particle from the bank.  ************        !/*ba4a        9*/
!                                                                      *        !/*ba         74*/
!        fetch a block from the backup file if necessary.                       !/*ba         75*/
  130 if(jbnk.ne.0)go to 140                                                    !/*ba         76*/
#ifdef MULTT                                                                    /*!baxk        7*/
      call thread_lockon(jlock)                                                 !/*baxk        8*/
#endif                                                                          /*!baxk        9*/
      backspace iub+ktask                                                       !/*ba         78*/
      backspace iub+ktask                                                       !/*ba         79*/
      read (iub+ktask) ibnk(kbnk:kbnk+lu*nx)                                    !/*bax        26*/
      read (iub+ktask) gbnk(kgbn+1:kgbn+ibnk(kbnk))                             !/*bax        27*/
      jbnk=nx                                                                   !/*ba         82*/
      backspace iub+ktask                                                       !/*ba         83*/
      backspace iub+ktask                                                       !/*ba         84*/
      mbb=ibnk(kbnk)                                                            !/*ba         85*/
#ifdef MULTT                                                                    /*!baxk       10*/
      call thread_lockoff(jlock)                                                !/*baxk       11*/
#endif                                                                          /*!baxk       12*/
!                                                                               !/*ba         87*/
!        retrieve the particle.                                                 !/*ba         88*/
!        unpack tpp(1), iex, and ntyn from iex for eventp, ptrak.               !/*ba4c2      15*/
  140 do 150 i=1,lpblcm                                                         !/*ba         89*/
  150 jpblcm(i)=ibnk(kbnk+(jbnk-1)*lu+i)                                        !/*ba4c       28*/
      i=abs(iex)                                                                !/*ba4c2      16*/
      l=i/1000000                                                               !/*ba4c2      17*/
      j=(i-l*1000000)/1000                                                      !/*ba4c2      18*/
      ntyn=sign(i-l*1000000-j*1000,iex)                                         !/*ba4c2      19*/
      iex=j                                                                     !/*ba4c2      20*/
      tpp(1)=l                                                                  !/*ba4c2      21*/
      mb=mbb-15*abs(iunr)-10*lev                                                !/*ba4c       29*/
      if(lev.eq.0)go to 161                                                     !/*ba4c2      22*/
      do 160 j=0,lev                                                            !/*ba4c2      23*/
      do 160 i=1,10                                                             !/*ba4c2      24*/
  160 udt(i,j)=gbnk(kgbn+mb+i+10*j)                                             !/*ba4c2      25*/
  161 mb=mb-3*lb                                                                !/*ba4c2      26*/
      do 170 i=1,lb                                                             !/*ba4b       17*/
      do 170 j=1,3                                                              !/*ba4b       18*/
  170 ptb(kptb+j,i)=gbnk(kgbn+mb+(i-1)*3+j)                                     !/*ba4b       19*/
      mb=mb-npblcm                                                              !/*ba4b       23*/
      do 185 i=1,npblcm                                                         !/*ba4b       24*/
  185 gpblcm(i)=gbnk(kgbn+mb+i)                                                 !/*ba4b       25*/
!                                                                               !/*ba4c       30*/
!        retrieve rtcr block for unresolved treatment.                          !/*ba4c       31*/
      if(iunr.eq.0)go to 188                                                    !/*ba4c       32*/
      if(ipt.ne.1)go to 188                                                     !/*ba4c       33*/
      if(erg.le.xunrl.or.erg.ge.xunru)go to 188                                 !/*ba4c       34*/
      mq=mbb-15*abs(iunr)                                                       !/*ba4c       35*/
      ju=0                                                                      !/*ba4c2      27*/
      do 187 i=1,mxe                                                            !/*ba4c       36*/
      if(nty(lnty+i).ne.1)go to 187                                             !/*ba4c       37*/
      lk=jxs(ljxs+23,i)                                                         !/*ba4c       38*/
      if(lk.eq.0)go to 187                                                      !/*ba4c       39*/
      ju=ju+1                                                                   !/*ba4c2      28*/
      if(gbnk(kgbn+mq+(ju-1)*15+6).ne.erg)go to 187                             !/*ba4c2      29*/
      do 186 j=1,15                                                             !/*ba4c       43*/
  186 rtc(krtc+j,i)=gbnk(kgbn+mq+(ju-1)*15+j)                                   !/*ba4c2      30*/
      ktc(kktc+1,i)=ibnk(kbnk+(jbnk-1)*lu+lpblcm+(ju-1)*2+1)                    !/*ba4c2      31*/
      ktc(kktc+2,i)=ibnk(kbnk+(jbnk-1)*lu+lpblcm+(ju-1)*2+2)                    !/*ba4c2      32*/
  187 continue                                                                  !/*ba4c       47*/
!                                                                               !/*ba4c       48*/
  188 if(npa.gt.1)go to 190                                                     !/*ba4c       49*/
      nbnk=nbnk-1                                                               !/*ba         98*/
      jbnk=jbnk-1                                                               !/*ba         99*/
      mbb=mb                                                                    !/*ba4b       26*/
      return                                                                    !/*ba        101*/
  190 ibnk(kbnk+(jbnk-1)*lu+1)=npa-1                                            !/*ba4c       50*/
      return                                                                    !/*ba        103*/
      end                                                                       !/*ba        104*/
