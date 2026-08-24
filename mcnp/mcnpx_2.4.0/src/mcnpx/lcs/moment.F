!     $Id: moment.F,v 1.5 2002/03/27 18:15:11 gwm Exp $                       !/*mo          1*/
      subroutine moment (izr,iar,uu,vv,ww,ex,erec,ierr,exi,vp,erc,npt           !/*mo          2*/
     & ,jjp)                                                                    !/*mo          3*/
#include <lcs/double.h>                                                         /*!mo          4*/
!.......................................................................        !/*mo          5*/
! These modifications imply the use of real masses in MOMENT.                   !/*mo          6*/
! The arguements to MOMENT are Z and A of the residual nucleus, the             !/*mo          7*/
! direction cosines U, V, and W of its recoil momentum, the excitation          !/*mo          8*/
! energy EX, and the recoil energy EREC.                                        !/*mo          9*/
!                                                                               !/*mo         10*/
! The flag IERR is 0 for a normal execution, +1 if the mass of the              !/*mo         11*/
! residual is not in the data tables, and is -1 if the residual is              !/*mo         12*/
! particle  stable.                                                             !/*mo         13*/
!.......................................................................        !/*mo         14*/
!                                                                               !/*mo         15*/
!    REFERENCES.......  FBRK                                                    !/*mo         16*/
!    EXTERNALS........  RANF      BETAD     SQRT      SIN       COS             !/*mo         17*/
!                       ACOS      SPECTR    LEVBRK    EXITA                     !/*mo         18*/
!                                                                               !/*mo         19*/
#include <lcs/parms.h>                                                          /*!mo         20*/
#include <lcs/fbrcom.h>                                                         /*!mo         21*/
      dimension exi(20), vp(3,20), erc(20), jjp(20)                             !/*mo         22*/
      dimension ilev(lchan), ill1(lchan), iip1(lchan), vx1(lchan), vy1          !/*mo         23*/
     & (lchan), vz1(lchan), kip(lchan)                                          !/*mo         24*/
      ierr=0                                                                    !/*mo         25*/
      aaaa=amass(izr,iar,iok)                                                   !/*mo         26*/
      if (iok.le.0) then                                                        !/*mo         27*/
        ierr=1                                                                  !/*mo         28*/
        return                                                                  !/*mo         29*/
      endif                                                                     !/*mo         30*/
      ipt0=iok                                                                  !/*mo         31*/
      call cbreak (ipt0,ex,n,kip,ilev,t,ki)                                     !/*moa         1*/
      if (n.gt.1) go to 20                                                      !/*mo         33*/
      ierr=-1                                                                   !/*mo         34*/
      return                                                                    !/*mo         35*/
   20 continue                                                                  !/*mo         36*/
      vs=sqrt(dp2*erec/aaaa)                                                    !/*mo         37*/
!.......................................................................        !/*mo         38*/
! this modification is intended to conserve energy in a primary breakup         !/*mo         39*/
!.......................................................................        !/*mo         40*/
      amt=dp0                                                                   !/*mo         41*/
      do 30 i=1,n                                                               !/*mo         42*/
   30 amt=amt+axm(kip(i))                                                       !/*mo         43*/
      vs=vs*sqrt(aaaa/amt)                                                      !/*mo         44*/
!.......................................................................        !/*mo         45*/
      vx=vs*uu                                                                  !/*mo         46*/
      vy=vs*vv                                                                  !/*mo         47*/
      vz=vs*ww                                                                  !/*mo         48*/
      ntot=n                                                                    !/*mo         49*/
      nextra=0                                                                  !/*mo         50*/
      nt=1                                                                      !/*mo         51*/
      npt=0                                                                     !/*mo         52*/
      izr1=izr                                                                  !/*mo         53*/
      iar1=iar                                                                  !/*mo         54*/
   40 continue                                                                  !/*mo         55*/
      arr=axm(kip(1))+axm(kip(2))                                               !/*mo         56*/
      if (n.gt.2) arr=arr+axm(kip(3))                                           !/*mo         57*/
      m1=n-1                                                                    !/*mo         58*/
      do 50 n2=1,m1                                                             !/*mo         59*/
      ipt=kip(n2)                                                               !/*mo         60*/
      iap=ja(ipt)                                                               !/*mo         61*/
      izp=jz(ipt)                                                               !/*mo         62*/
      iar=iar-iap                                                               !/*mo         63*/
      izr=izr-izp                                                               !/*mo         64*/
      ap=axm(ipt)                                                               !/*mo         65*/
      arr=arr-ap                                                                !/*mo         66*/
!                                                                               !/*mo         67*/
!       iar,izr are the mass and charge of resid.nuc.                           !/*mo         68*/
!       after one of the particles emitted in breakup                           !/*mo         69*/
!                                                                               !/*mo         70*/
      rn2=rang()                                                                !/*mo         71*/
      call azirn (sinp,cosp)                                                    !/*mo         72*/
      costh=dp1-dp2*rn2                                                         !/*mo         73*/
      sox=dp2*ap*t/(arr*(arr+ap))                                               !/*mo         74*/
      np=n+1-n2                                                                 !/*mo         75*/
      xsq=dp1                                                                   !/*mo         76*/
      if (np.gt.2) xsq=betad(np)                                                !/*mo         77*/
      sox=sox*xsq                                                               !/*mo         78*/
      t=t*(dp1-xsq)                                                             !/*mo         79*/
      vt=sqrt(sox)                                                              !/*mo         80*/
!                                                                               !/*mo         81*/
!       vt is the recoil vel. of resid.nuc.                                     !/*mo         82*/
!       t is the total k.e. of res.nuc.in c.m. system                           !/*mo         83*/
!                                                                               !/*mo         84*/
      sinsq=dp1-costh*costh                                                     !/*mo         85*/
      sinth=sqrt(sinsq)                                                         !/*mo         86*/
      vzse=vt*costh                                                             !/*mo         87*/
      vyse=vt*sinth*sinp                                                        !/*mo         88*/
      vxse=vt*sinth*cosp                                                        !/*mo         89*/
!                                                                               !/*mo         90*/
!     vzse,vyse,vxse are recoil vel. components of resid.nuc.                   !/*mo         91*/
!                                                                               !/*mo         92*/
      vz=vz+vzse                                                                !/*mo         93*/
      vy=vy+vyse                                                                !/*mo         94*/
      vx=vx+vxse                                                                !/*mo         95*/
!                                                                               !/*mo         96*/
!     vz,vy,vx are vel.comp. of resid.nuc.                                      !/*mo         97*/
!                                                                               !/*mo         98*/
      v1=arr/ap+dp1                                                             !/*mo         99*/
      vzp=vz-vzse*v1                                                            !/*mo        100*/
      vyp=vy-vyse*v1                                                            !/*mo        101*/
      vxp=vx-vxse*v1                                                            !/*mo        102*/
!                                                                               !/*mo        103*/
!     vzp,vyp,vxp are the vel.comp. of emitted particle in breakup              !/*mo        104*/
!                                                                               !/*mo        105*/
      llev=ilev(n2)                                                             !/*mo        106*/
      if (nklev(jns(ipt)+llev).ne.0) then                                       !/*mo        107*/
        nextra=nextra+1                                                         !/*mo        108*/
        nt=nt+1                                                                 !/*mo        109*/
        iip1(nextra)=ipt                                                        !/*mo        110*/
        ill1(nextra)=llev                                                       !/*mo        111*/
        vx1(nextra)=vxp                                                         !/*mo        112*/
        vy1(nextra)=vyp                                                         !/*mo        113*/
        vz1(nextra)=vzp                                                         !/*mo        114*/
      else                                                                      !/*mo        115*/
        npt=npt+1                                                               !/*mo        116*/
        jjp(npt)=ipt                                                            !/*mo        117*/
        exi(npt)=e(jns(ipt)+llev)                                               !/*mo        118*/
        vp(1,npt)=vxp                                                           !/*mo        119*/
        vp(2,npt)=vyp                                                           !/*mo        120*/
        vp(3,npt)=vzp                                                           !/*mo        121*/
      endif                                                                     !/*mo        122*/
   50 continue                                                                  !/*mo        123*/
      ipt=kip(n)                                                                !/*mo        124*/
      if (n.gt.1) then                                                          !/*mo        125*/
        llev=ilev(n)                                                            !/*mo        126*/
        if (nklev(jns(ipt)+llev).ne.0) then                                     !/*mo        127*/
          nextra=nextra+1                                                       !/*mo        128*/
          nt=nt+1                                                               !/*mo        129*/
          iip1(nextra)=ipt                                                      !/*mo        130*/
          ill1(nextra)=llev                                                     !/*mo        131*/
          vx1(nextra)=vx                                                        !/*mo        132*/
          vy1(nextra)=vy                                                        !/*mo        133*/
          vz1(nextra)=vz                                                        !/*mo        134*/
          go to 60                                                              !/*mo        135*/
        endif                                                                   !/*mo        136*/
        npt=npt+1                                                               !/*mo        137*/
        jjp(npt)=ipt                                                            !/*mo        138*/
        exi(npt)=e(jns(ipt)+llev)                                               !/*mo        139*/
        vp(1,npt)=vx                                                            !/*mo        140*/
        vp(2,npt)=vy                                                            !/*mo        141*/
        vp(3,npt)=vz                                                            !/*mo        142*/
      endif                                                                     !/*mo        143*/
   60 continue                                                                  !/*mo        144*/
      call levbrk (ipt,llev,n,kip,ilev,t)                                       !/*mo        145*/
      if (nextra.ne.0) then                                                     !/*mo        146*/
        nk=nextra                                                               !/*mo        147*/
        nextra=nextra-1                                                         !/*mo        148*/
        ipt=iip1(nk)                                                            !/*mo        149*/
        llev=ill1(nk)                                                           !/*mo        150*/
        vx=vx1(nk)                                                              !/*mo        151*/
        vy=vy1(nk)                                                              !/*mo        152*/
        vz=vz1(nk)                                                              !/*mo        153*/
        iar=ja(ipt)                                                             !/*mo        154*/
        izr=jz(ipt)                                                             !/*mo        155*/
        call levbrk (ipt,llev,n,kip,ilev,t)                                     !/*mo        156*/
        if (n.lt.2) call expirx(1,'MOMENT','n.lt.2')                            !/*mo        157*/
!.......................................................................        !/*mo        158*/
! this modification conserves energy in a secondary breakup                     !/*mo        159*/
!.......................................................................        !/*mo        160*/
        a0=axm(ipt)                                                             !/*mo        161*/
        amt=axm(kip(1))+axm(kip(2))                                             !/*mo        162*/
        if (n.gt.2) amt=amt+axm(kip(3))                                         !/*mo        163*/
        fact=sqrt(a0/amt)                                                       !/*mo        164*/
        vx=vx*fact                                                              !/*mo        165*/
        vy=vy*fact                                                              !/*mo        166*/
        vz=vz*fact                                                              !/*mo        167*/
!.......................................................................        !/*mo        168*/
        ntot=ntot-1+n                                                           !/*mo        169*/
        go to 40                                                                !/*mo        170*/
      endif                                                                     !/*mo        171*/
      if (ntot.gt.1.and.npt.ne.ntot) call expirx(1,'MOMENT',                    !/*mo        172*/
     & 'ntot.gt.1.and.npt.ne.ntot')                                             !/*mo        173*/
      izr=izr1                                                                  !/*mo        174*/
      iar=iar1                                                                  !/*mo        175*/
      do 80 i=1,npt                                                             !/*mo        176*/
      vsq=vp(1,i)**2+vp(2,i)**2+vp(3,i)**2                                      !/*mo        177*/
      erc(i)=dph*axm(jjp(i))*vsq                                                !/*mo        178*/
      vsq=sqrt(vsq)                                                             !/*mo        179*/
      do 70 j=1,3                                                               !/*mo        180*/
   70 vp(j,i)=vp(j,i)/vsq                                                       !/*mo        181*/
   80 continue                                                                  !/*mo        182*/
      return                                                                    !/*mo        183*/
!                                                                               !/*mo        184*/
      end                                                                       !/*mo        185*/
