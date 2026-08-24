!#ident xact.F from deck xa                                                     !/*xa          1*/
      subroutine xact                                                           !/*xa          2*/
!        set up the cross-section tables.                                       !/*xa          3*/
      use global_data                                                           !/*xax         1*/
!                                                                               !/*xax         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*xax         3*/
      implicit integer(kindi) (i-n)                                             !/*xax         4*/
!                                                                               !/*xax         5*/
      character hm*5,ht*10,hz*10                                                !/*xa4b        1*/
!                                                                               !/*xax         6*/
!        offset lxs by 1 at start so that kxspen(1)=jxs((1,1)-1 > 0             !/*xax         7*/
!        to make cross section plot work.                                       !/*xax         8*/
      lxs=1                                                                     !/*xax         9*/
!                                                                               !/*xa          6*/
!        create runtpe.  get the cross-section tables.                          !/*xa          7*/
      hovr='xact'                                                               !/*xa          8*/
      if(ink(98).ne.0)call pconst                                               !/*xa4a        1*/
      nt=0                                                                      !/*xa          9*/
      do 1 i=1,mxe                                                              !/*xax        10*/
    1 if(nty(lnty+i).eq.10)nt=nt+1                                              !/*xax        11*/
      mxe=mxe-nt                                                                !/*xa         12*/
      call tpefil(1)                                                            !/*xa         13*/
      if(mxe.ne.0)call getxst(0)                                                !/*xa         14*/
      if(nt.ne.0)call getxst(nt)                                                !/*xa         15*/
!                                                                               !/*xax        12*/
!        set up particle energy grids.                                          !/*xax        13*/
      do 5 mp=1,mipt                                                            !/*xax        14*/
      if(nee(mp).eq.0)go to 5                                                   !/*xax        15*/
      if(charge(mp).eq.zero) then                                               !/*xax        16*/
         eee(leee+mp,1)=emx(mp)                                                 !/*xax        17*/
         eee(leee+mp,nee(mp))=0.99*ecf(mp)                                      !/*xax        18*/
      else                                                                      !/*xax        19*/
         eee(leee+mp,1)=emx(mp)                                                 !/*xax        20*/
         do 3 n=2,nee(mp)                                                       !/*xax        21*/
    3    eee(leee+mp,n)=efac(mp)*eee(leee+mp,n-1)                               !/*xax        22*/
      endif                                                                     !/*xax        23*/
    5 continue                                                                  !/*xax        24*/
!                                                                               !/*xax        25*/
      if(nt.ne.0)call xsgen(nt)                                                 !/*xa         16*/
      call thetag                                                               !/*xax        26*/
      do 10 mp=4,mipt                                                           !/*xax        27*/
   10 if(kpt(mp).ne.0.and.charge(mp).ne.zero)call cp_rng(mp)                    !/*xax        28*/
!                                                                               !/*xa4b        2*/
!        print assignment of s(a,b) data to nuclides.                           !/*xa4b        3*/
      if(indt.eq.0.or.ink(102).eq.0)go to 18                                    !/*xa4b        4*/
      write(iuo,12)                                                             !/*xa4b        5*/
   12 format(39h1assignment of s(a,b) data to nuclides.,65x,                    !/*xa4b        6*/
     & 15hprint table 102//7x,3hmat,8x,7hnuclide,9x,6hs(a,b))                   !/*xa4b        7*/
      do 16 i=1,nmat                                                            !/*xa4b        8*/
      write(hm,'(i5)')nmt(lnmt+i)                                               !/*xa4b        9*/
      do 14 m=jmd(ljmd+i),jmd(ljmd+i+1)-1                                       !/*xa4b       10*/
      if(lmt(llmt+m).eq.0)go to 14                                              !/*xa4b       11*/
      call zaid(2,hz,ixl(lixl+1,lme(llme+1,m)))                                 !/*xa4b       12*/
      call zaid(2,ht,ixl(lixl+1,lmt(llmt+m)))                                   !/*xa4b       13*/
      write(iuo,'(5x,a5,5x,a10,5x,a10)')hm,hz,ht                                !/*xa4b       14*/
      hm=' '                                                                    !/*xa4b       15*/
   14 continue                                                                  !/*xa4b       16*/
   16 continue                                                                  !/*xa4b       17*/
!                                                                               !/*xa         17*/
!        print table of use of dynamically allocated storage.                   !/*xa         18*/
   18 if(mxe.le.16)write(iuo,20)                                                !/*xa4b       18*/
   20 format(/////47h decimal words of dynamically allocated storage)           !/*xa4a        2*/
      if(mxe.gt.16)write(iuo,30)                                                !/*xa         21*/
   30 format(47h1decimal words of dynamically allocated storage)                !/*xa4a        3*/
      if(mct.ge.0)write(iuo,40)0,(nmxf*mxf+ktls+(2*nspt+5)*ntal)                !/*xax        29*/
     & *(mtasks+1),(mbnk+(nbmx*(lpblcm+2*abs(iunr))+1))*mtasks,lxs,0,0          !/*xax        30*/
   40 format(/5x,7hgeneral,i19/5x,7htallies,i19/5x,4hbank,i22/5x,               !/*xa4b       25*/
     & 14hcross sections,i12//5x,5htotal,i21,5x,1h=,i12,6h bytes)               !/*xa4b       26*/
!                                                                               !/*xa         42*/
!        read all the cross-section tables back in, and write the fixed         !/*xa         43*/
!        data and the first dump.                                               !/*xa         44*/
      if(nfer.ne.0.and.lfatl.eq.0)return                                        !/*xa         45*/
      call tpefil(8)                                                            !/*xa         46*/
      if(mcal.ne.0)call mgimps                                                  !/*xa         47*/
      call tpefil(3)                                                            !/*xa         48*/
      if(mxe.eq.0)return                                                        !/*xax        31*/
!                                                                               !/*xa4c        1*/
!        check if addresses are too large for integer arithmetic.               !/*xa4c        2*/
      xss(lxss+lxs+1)=jxs(ljxs+1,mxe)+nxs(lnxs+1,mxe)                           !/*xa4c        3*/
      i=xss(lxss+lxs+1)                                                         !/*xa4c        4*/
      if(jxs(ljxs+1,mxe)+nxs(lnxs+1,mxe).ne.i)call erprnt(1,1,0,0,0,0,          !/*xa4c        5*/
     & 0,0,'45hproblem too large for single precision xsecs.')                  !/*xa4c        6*/
!                                                                               !/*xa         49*/
!        write the initial kcode source to the srctp file.                      !/*xa         50*/
      if(nsr.eq.71.and.nsa.ne.0.and.ksdef.eq.0)call ksrctp(4)                   !/*xa         51*/
      return                                                                    !/*xa         52*/
      end                                                                       !/*xa         53*/
