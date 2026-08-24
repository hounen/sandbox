!     $Id: xtp11.F,v 1.5 2002/03/27 18:16:27 gwm Exp $                        !/*xt          1*/
      subroutine xtp11 (iflag,nc)                                               !/*xt          2*/
#include <htape3x/com32b.h>                                                     /*!xt          3*/
#include <htape3x/label.h>                                                      /*!xt          4*/
#include <htape3x/contrl.h>                                                     /*!xt          5*/
#include <htape3x/charht.h>                                                     /*!xt          6*/
#include <htape3x/com11.h>                                                      /*!xt          7*/
      character*8 nn1, nn2                                                      !/*xt          8*/
      dimension nc(8)                                                           !/*xt          9*/
      dimension ssb(11)                                                         !/*xt         10*/
      dimension wtin(2)                                                         !/*xt         11*/
      character aids*80, idtms*19, kods*8, lods*8, probs*19, vers*5             !/*xt         12*/
      if (lf11b) go to 40                                                       !/*xt         13*/
!------ start of run                                                            !/*xt         14*/
      lf11b=.true.                                                              !/*xt         15*/
      ncol=-1                                                                   !/*xt         16*/
      read (nhst,end=30) kods,vers,lods,idtms,probs,aids,knods                  !/*xt         17*/
      if (knods.eq.0) call errorf ('xtp11.1 ')                                  !/*xt         18*/
      read (nhst,end=30) np1,nrss,nrcd,njsw,niss                                !/*xt         19*/
      nrrs=0                                                                    !/*xt         20*/
      wtin(1)=dp0                                                               !/*xt         21*/
      wtin(2)=dp0                                                               !/*xt         22*/
      mipts=2                                                                   !/*xt         23*/
      if (np1.lt.0) read (nhst,end=30) niwr,mipts                               !/*xt         24*/
      np1=abs(np1)                                                              !/*xt         25*/
      if (nrcd.ne.11) call errorf ('xtp11.2 ')                                  !/*xt         26*/
      do 10 i=1,njsw+niwr+1                                                     !/*xt         27*/
      read (nhst,end=30) j                                                      !/*xt         28*/
   10 continue                                                                  !/*xt         29*/
      maxcas=abs(np1)                                                           !/*xt         30*/
      maxbch=1                                                                  !/*xt         31*/
      nn1(1:8)=idtms(3:10)                                                      !/*xt         32*/
      nn2(1:8)=idtms(12:19)                                                     !/*xt         33*/
      if (icase.le.1) then                                                      !/*xt         34*/
        if (.not.ltesting) then                                                 !/*xt         35*/
          write (io,70) histx,nn1,nn2,maxbch,maxcas                             !/*xt         36*/
        else                                                                    !/*xt         37*/
          write (io,110) maxbch,maxcas                                          !/*xt         38*/
        endif                                                                   !/*xt         39*/
        write (jtty,70) histx,nn1,nn2                                           !/*xt         40*/
      endif                                                                     !/*xt         41*/
      if (iflag.ne.-1) go to 40                                                 !/*xt         42*/
      go to 20                                                                  !/*xt         43*/
   30 continue                                                                  !/*xt         44*/
      call errorf ('xtp11.3 ')                                                  !/*xt         45*/
!***********************************************************************        !/*xt         46*/
   40 continue                                                                  !/*xt         47*/
      if (log11) then                                                           !/*xt         48*/
        if (nrrs.ge.nrss) then                                                  !/*xt         49*/
!         write (jtty,*) ' nrrs.ge.nrss ',nrrs,nrss                             !/*xt         50*/
          go to 60                                                              !/*xt         51*/
        endif                                                                   !/*xt         52*/
        ibad11=0                                                                !/*xt         53*/
        read (nhst,end=50,iostat=ibad11,err=50) ssb                             !/*xt         54*/
        if (ibad11.ne.0) go to 50                                               !/*xt         55*/
        nrrs=nrrs+1                                                             !/*xt         56*/
        nx=abs(ssb(2))                                                          !/*xt         57*/
        i=nx/1000000                                                            !/*xt         58*/
        lelem=nx-1000000*i                                                      !/*xt         59*/
        ipt=i                                                                   !/*xt         60*/
        jc=0                                                                    !/*xt         61*/
        if(i.ge.100) then                                                       !/*xt         62*/
          jgp=-i/200                                                            !/*xt         63*/
          jc=i/100+2*jgp                                                        !/*xt         64*/
          ipt=i-100*jc+200*jgp                                                  !/*xt         65*/
        endif                                                                   !/*xt         66*/
        if (ltran(ipt).le.0) go to 40                                           !/*xt         67*/
        if (ipt.le.2) wtin(ipt)=wtin(ipt)+ssb(3)                                !/*xt         68*/
      else                                                                      !/*xt         69*/
        log11=.true.                                                            !/*xt         70*/
      endif                                                                     !/*xt         71*/
      ncol=7                                                                    !/*xt         72*/
      nocs11=abs(ssb(1))                                                        !/*xt         73*/
      if (ibad10.ne.0.and.nocs11.gt.nocas) go to 60                             !/*xt         74*/
        nx=abs(ssb(2))                                                          !/*xt         75*/
        i=nx/1000000                                                            !/*xt         76*/
        lelem=nx-1000000*i                                                      !/*xt         77*/
        ipt=i                                                                   !/*xt         78*/
        jc=0                                                                    !/*xt         79*/
        if(i.ge.100) then                                                       !/*xt         80*/
          jgp=-i/200                                                            !/*xt         81*/
          jc=i/100+2*jgp                                                        !/*xt         82*/
          ipt=i-100*jc+200*jgp                                                  !/*xt         83*/
        endif                                                                   !/*xt         84*/
      if (nc(ncol).eq.0) go to 40                                               !/*xt         85*/
      nocas=nocs11                                                              !/*xt         86*/
      mcp=1                                                                     !/*xt         87*/
      if (ssb(1).lt.0.) mcp=0                                                   !/*xt         88*/
      nx=abs(ssb(2))                                                            !/*xt         89*/
      i=nx/1000000                                                              !/*xt         90*/
      lelem=nx-1000000*i                                                        !/*xt         91*/
      ipt=i                                                                     !/*xt         92*/
      jc=0                                                                      !/*xt         93*/
      if(i.ge.100) then                                                         !/*xt         94*/
        jgp=-i/200                                                              !/*xt         95*/
        jc=i/100+2*jgp                                                          !/*xt         96*/
        ipt=i-100*jc+200*jgp                                                    !/*xt         97*/
      endif                                                                     !/*xt         98*/
      tip=ltran(ipt)-1                                                          !/*xt         99*/
      bold=0                                                                    !/*xt        100*/
      wt=ssb(3)                                                                 !/*xt        101*/
      ec=ssb(4)                                                                 !/*xt        102*/
      tc=10.d0*ssb(5)                                                           !/*xt        103*/
      xc=ssb(6)                                                                 !/*xt        104*/
      yc=ssb(7)                                                                 !/*xt        105*/
      zc=ssb(8)                                                                 !/*xt        106*/
      u=ssb(9)                                                                  !/*xt        107*/
      v=ssb(10)                                                                 !/*xt        108*/
      w=sign(sqrt(max(dp0,dp1-u**2-v**2)),ssb(2))                               !/*xt        109*/
      fmu=ssb(11)                                                               !/*xt        110*/
      if (iflag.le.0) go to 20                                                  !/*xt        111*/
      ityp=tip+1.001d0                                                          !/*xt        112*/
      if (ncp(ityp).eq.0) go to 40                                              !/*xt        113*/
      go to 20                                                                  !/*xt        114*/
!***********************************************************************        !/*xt        115*/
   50 continue                                                                  !/*xt        116*/
      maxcas=nocs11                                                             !/*xt        117*/
      maxbch=1                                                                  !/*xt        118*/
      nocas=nocs11                                                              !/*xt        119*/
      name=0                                                                    !/*xt        120*/
      write (io,80) nocs11,ibad11                                               !/*xt        121*/
      write (jtty,80) nocs11,ibad11                                             !/*xt        122*/
      write (io,90) maxcas                                                      !/*xt        123*/
      write (jtty,90) maxcas                                                    !/*xt        124*/
   60 continue                                                                  !/*xt        125*/
      wtin(1)=wtin(1)/abs(np1)                                                  !/*xt        126*/
      wtin(2)=wtin(2)/abs(np1)                                                  !/*xt        127*/
      write (io,100) nrrs,histx,wtin                                            !/*xt        128*/
      write (jtty,100) nrrs,histx,wtin                                          !/*xt        129*/
      ncol=-4                                                                   !/*xt        130*/
!     write (jtty,*) ' set A ncol',ncol                                         !/*xt        131*/
   20 continue                                                                  !/*xt        132*/
      if (ncol.gt.0) then                                                       !/*xt        133*/
        ncol11=nocs11                                                           !/*xt        134*/
      else                                                                      !/*xt        135*/
        ncol11=ncol                                                             !/*xt        136*/
      endif                                                                     !/*xt        137*/
      return                                                                    !/*xt        138*/
!                                                                               !/*xt        139*/
   70 format ('1',30x,' reading ',a8,' as history tape written ',a8,2x          !/*xt        140*/
     & ,a8,' by mcnp'/40x,'maxbch =',i3,'     maxcas =',i7)                     !/*xt        141*/
   80 format (' unexpected EOF - ncas =',i8,' in TP11 - error code =',i5        !/*xt        142*/
     & )                                                                        !/*xt        143*/
   90 format (' norm reset to MAXCAS =',i8)                                     !/*xt        144*/
  100 format (/10x,i9,' tracks read from file ',a8/10x,                         !/*xt        145*/
     & ' mean neutron weight =',1pe13.5/10x,' mean photon weight =',            !/*xt        146*/
     & 1pe13.5)                                                                 !/*xt        147*/
  110 format ('1'/40x,'maxbch =',i3,'     maxcas =',i7)                         !/*xt        148*/
      end                                                                       !/*xt        149*/
