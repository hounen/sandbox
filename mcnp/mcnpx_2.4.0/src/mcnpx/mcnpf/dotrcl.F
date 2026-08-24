!#ident dotrcl.F from deck dt                                                   !/*dt          1*/
      subroutine dotrcl(ic)                                                     !/*dt          2*/
!        create new surfaces for trcl cell ic.                                  !/*dt          3*/
      use global_data                                                           !/*dtx         1*/
      use imcn_data                                                             !/*dtx         2*/
!                                                                               !/*dtx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*dtx         4*/
      implicit integer(kindi) (i-n)                                             !/*dtx         5*/
!                                                                               !/*dtx         6*/
      dimension nc(23)                                                          !/*dt          6*/
      data nc/4,4*1,4,3*2,3*3,3*1,3*5,3*3,2*10/                                 !/*dt          7*/
!                                                                               !/*dt          8*/
!        loop over all surfaces of cell ic.                                     !/*dt4c        1*/
      do 60 js=abs(lca(llca+ic)),abs(lca(llca+ic+1))-1                          !/*dt4c        2*/
!                                                                               !/*dt4c        3*/
!        generate a new surface for each unique surface of the cell.            !/*dt4c        4*/
      if(lja(llja+js).gt.1000000.or.lja(llja+js-1).eq.1000004)go to 60          !/*dt4c        5*/
      do 10 k=abs(lca(llca+ic)),js-1                                            !/*dt4c        6*/
   10 if(abs(lja(llja+k)).eq.abs(lja(llja+js)).and.lja(llja+k-1).ne.            !/*dt4c        7*/
     & 1000004)go to 60                                                         !/*dt4c        8*/
      mxj=mxj+1                                                                 !/*dt4c        9*/
!                                                                               !/*dt4c       10*/
!        generate a unique name for the new surface.                            !/*dt4c       11*/
      ns=abs(lja(llja+js))+1000*ncl(lncl+ic)                                    !/*dt4c       12*/
      if(namchg(2,ns).ne.0)call erprnt(1,1,2,ns,ncl(lncl+ic),0,0,0,             !/*dt4c       13*/
     & '22hgenerated surface name,i7,8h in cell,i4,14h is not unique')          !/*dt4c       14*/
      nsfm(lnsf+mxj)=ns                                                         !/*dt4c       15*/
      jj=namchg(2,abs(lja(llja+js)))                                            !/*dt4c       16*/
      if(jj.eq.0)call erprnt(1,1,2,lja(llja+js),ncl(lncl+ic),0,0,0,             !/*dt4c       17*/
     & '7hsurface,i7,8h of cell,i5,16h is not defined.')                        !/*dt4c       18*/
      if(jj.eq.0)return                                                         !/*dt4c       19*/
      nf=1                                                                      !/*dt4c       20*/
!                                                                               !/*dt4c       21*/
!        if macrobody, add the master macrobody surface.                        !/*dt4c       22*/
      if(idna(lida+js).eq.0)go to 20                                            !/*dt4c       23*/
      jtr(ljtr+mxj)=jtr(ljtr+jj)                                                !/*dt4c       24*/
      ara(lara+mxj)=ara(lara+jj)                                                !/*dt4c       25*/
      kst(lkst+mxj)=kst(lkst+jj)                                                !/*dt4c       26*/
      ksu(lksu+mxj)=ksu(lksu+jj)                                                !/*dt4c       27*/
      ksm(lksm+mxj)=ksm(lksm+jj)                                                !/*dt4c       28*/
      lsc(llsc+mxj+1)=lsc(llsc+mxj)                                             !/*dt4c       29*/
      nf=6                                                                      !/*dt4c       30*/
      ji=-ksm(lksm+jj)                                                          !/*dt4c       31*/
      if(ji.eq.33.or.ji.eq.34.or.ji.eq.36)nf=3                                  !/*dt4c       32*/
      if(ji.eq.39)nf=8                                                          !/*dt4c       33*/
      if(ji.gt.4000)nf=ji/1000                                                  !/*dt4c2       1*/
      if(ji.eq.37)nf=5                                                          !/*dt4c       34*/
      if(ji.eq.1030.or.ji.eq.1031)nf=4                                          !/*dt4c       35*/
      jj=jj+1                                                                   !/*dt4c       36*/
      mxj=mxj+1                                                                 !/*dt4c       37*/
      nsfm(lnsf+mxj)=0                                                          !/*dt4c       38*/
!                                                                               !/*dt4c       39*/
!        change the surface name.                                               !/*dt4c       40*/
   20 do 30 kz=js,abs(lca(llca+ic+1))-1                                         !/*dt4c       41*/
      k=js+abs(lca(llca+ic+1))-1-kz                                             !/*dt4c       42*/
   30 if(abs(lja(llja+k)).eq.abs(lja(llja+js)).and.                             !/*dt4c       43*/
     & lja(llja+k-1).ne.1000004)lja(llja+k)=sign(ns,lja(llja+k))                !/*dt4c       44*/
!                                                                               !/*dt4c       45*/
!        copy the attributes of the old surface.                                !/*dt4c       46*/
!        if macrobody, do for each facet.                                       !/*dt4c       47*/
      do 50 jf=1,nf                                                             !/*dt4c       48*/
      ara(lara+mxj)=ara(lara+jj)                                                !/*dt4c       49*/
      kst(lkst+mxj)=kst(lkst+jj)                                                !/*dt4c       50*/
      ksu(lksu+mxj)=ksu(lksu+jj)                                                !/*dt4c       51*/
      ksm(lksm+mxj)=ksm(lksm+jj)                                                !/*dt4c       52*/
      if(nf.ne.1)ksm(lksm+mxj)=mxj-jf                                           !/*dt4c       53*/
      lsc(llsc+mxj+1)=lsc(llsc+mxj)+nc(kst(lkst+mxj))                           !/*dt4c       54*/
      do 40 i=1,nc(kst(lkst+mxj))                                               !/*dt4c       55*/
   40 scf(lsc(llsc+mxj)+i)=scf(lsc(llsc+jj)+i)                                  !/*dt4c       56*/
!                                                                               !/*dt4c       57*/
!        transform the surface coefficients.                                    !/*dt4c       58*/
      jtr(ljtr+mxj)=ktr(lktr+ic)                                                !/*dt4c       59*/
      call trfsrf(mxj)                                                          !/*dt4c       60*/
      if(jf.eq.nf)go to 50                                                      !/*dt4c       61*/
      jj=jj+1                                                                   !/*dt4c       62*/
      mxj=mxj+1                                                                 !/*dt4c       63*/
      nsfm(lnsf+mxj)=0                                                          !/*dt4c       64*/
   50 continue                                                                  !/*dt4c       65*/
   60 continue                                                                  !/*dt4c       66*/
      return                                                                    !/*dt         42*/
      end                                                                       !/*dt         43*/
