!#ident rhoden.F from deck rh                                                   !/*rh          1*/
      subroutine rhoden                                                         !/*rh          2*/
!        convert mass densities to atom densities.  print the materials.        !/*rh          3*/
      use global_data                                                           !/*rhx         1*/
      use imcn_data                                                             !/*rhx         2*/
!                                                                               !/*rhx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*rhx         4*/
      implicit integer(kindi) (i-n)                                             !/*rhx         5*/
!                                                                               !/*rhx         6*/
      character ht*82,hm*6                                                      !/*cor4-1     95*/
!                                                                               !/*rh          7*/
      if(dbcn(26).ne.0..and.mxe.eq.0)ink(40)=0                                  !/*rhx         7*/
      if(ink(40).ne.0)write(iuo,10)                                             !/*rh          8*/
   10 format(21h1material composition,83x,14hprint table 40)                    !/*rh          9*/
      nl=13                                                                     !/*rh         10*/
!                                                                               !/*rh         11*/
!        do all of the materials.                                               !/*rh         12*/
      nu=0                                                                      !/*rh         13*/
      nd=0                                                                      !/*rh         14*/
      nr=0                                                                      !/*rh         15*/
      do 100 im=1,nmat                                                          !/*rh         16*/
!                                                                               !/*rh         17*/
!        convert mass fractions to atom fractions.                              !/*rh         18*/
      su=0.                                                                     !/*rh         19*/
      sf=0.                                                                     !/*rh         20*/
      sw=0.                                                                     !/*rh         21*/
      do 20 m=jmd(ljmd+im),jmd(ljmd+im+1)-1                                     !/*rh         22*/
      a=awc(lawc+m)                                                             !/*rh         23*/
      write(hm,'(i6)')iza(liza+m)                                               !/*rh         24*/
      if (a.eq.zero) then                                                       !/*rhxa        1*/
         if (lcaopt(8).eq.-1.and.fme(lfme+m).gt.zero) then                      !/*rhxa        2*/
            a=mod(iza(liza+m),1000)                                             !/*rhxa        3*/
            awc(lawc+m)=a                                                       !/*rhxa        4*/
         else                                                                   !/*rhxa        5*/
            call expire(0,'rhoden',                                             !/*rhxa        6*/
     &      'atomic weight of '//hm//' is zero.')                               !/*rhxa        7*/
         endif                                                                  !/*rhxa        8*/
      endif                                                                     !/*rhxa        9*/
      su=su+fme(lfme+m)                                                         !/*rh         27*/
      if(fme(lfme+m).lt.0.)fme(lfme+m)=-fme(lfme+m)/a                           !/*rh         28*/
      sf=sf+fme(lfme+m)                                                         !/*rh         29*/
   20 sw=sw+fme(lfme+m)*a                                                       !/*rh         30*/
!                                                                               !/*rh         31*/
!        normalize the atom fractions.                                          !/*rh         32*/
      do 30 m=jmd(ljmd+im),jmd(ljmd+im+1)-1                                     !/*rh         33*/
   30 fme(lfme+m)=fme(lfme+m)/sf                                                !/*rh         34*/
!                                                                               !/*rh         35*/
!        convert mass densities of the cells to atom densities.                 !/*rh         36*/
      de=0.                                                                     !/*rh         37*/
      do 40 i=1,mxa                                                             !/*rh         38*/
      if(mat(lmat+i).ne.im)go to 40                                             !/*rh         39*/
      if(rho(lrho+i).lt.0.)rho(lrho+i)=-rho(lrho+i)*(avgdn*sf/sw)               !/*rh         40*/
      den(lden+i)=rho(lrho+i)/(avgdn*sf/sw)                                     !/*rh         41*/
      if(den(lden+i).gt.40.)nr=nr+1                                             !/*rh         42*/
      if(de.eq.0.)de=rho(lrho+i)                                                !/*rh         43*/
      if(de.ne.rho(lrho+i))de=-1.                                               !/*rh         44*/
   40 continue                                                                  !/*rh         45*/
!                                                                               !/*rh         46*/
!        convert rho*x entries on fm cards to atom densities.                   !/*rh         47*/
      do 70 it=1,ntal                                                           !/*rh         48*/
      if(iptal(lipt+5,1,it).eq.0)go to 70                                       !/*rh         49*/
      do 60 j=1,int(tds(iptal(lipt+5,1,it)))                                    !/*rh         50*/
      l=abs(tds(iptal(lipt+5,1,it)+j))                                          !/*rh         51*/
      if(l.le.1)go to 60                                                        !/*rh         52*/
      if(tds(l-1).ge.0.)go to 60                                                !/*rh         53*/
      do 50 i=1,int(abs(tds(l))),2                                              !/*rh         54*/
   50 if(tds(l+i+1).lt.0..and.tds(l+i).eq.im)                                   !/*rh         55*/
     & tds(l+i+1)=-tds(l+i+1)*avgdn*sf/sw                                       !/*rh         56*/
   60 continue                                                                  !/*rh         57*/
   70 continue                                                                  !/*rh         58*/
!                                                                               !/*rh         59*/
!        print warnings about unusual fractions and densities.                  !/*rh         60*/
      fc=abs(1.-abs(su))                                                        !/*rh4a        1*/
      dc=abs(1.-de/sf)                                                          !/*rh4a        2*/
      if(su.lt.0.)dc=abs(1.-de/sf/avgdn)                                        !/*rh4a        3*/
      if(min(fc,dc).lt..00001)go to 90                                          !/*rh4a        4*/
      nu=nu+1                                                                   !/*rh         62*/
      if(ink(40).ne.0)write(iuo,80)nmt(lnmt+im),abs(su)                         !/*rh         63*/
   80 format(/37h the sum of the fractions of material,i5,4h was,               !/*rh         64*/
     & 1pe13.6)                                                                 !/*rh         65*/
   90 if(de.lt.0.)nd=nd+1                                                       !/*rh         66*/
  100 if(de.lt.0..and.ink(40).ne.0)write(iuo,110)nmt(lnmt+im)                   !/*rh4a        5*/
  110 format(/34h not all cells containing material,i5,                         !/*rh         68*/
     & 23h have the same density.)                                              !/*rh         69*/
!                                                                               !/*rh         70*/
!        print table of material compositions (atom fractions).                 !/*rh         71*/
      if(ink(40).eq.0)go to 230                                                 !/*rh         72*/
      write(iuo,120)                                                            !/*rh         73*/
  120 format(/9h material/                                                      !/*rh         74*/
     & 8h  number,5x,32hcomponent nuclide, atom fraction/)                      !/*rh         75*/
      do 170 im=1,nmat                                                          !/*rh         76*/
      if(dbcn(26).ne.0.)then                                                    !/*rhx         8*/
      write(iuo,129)nmt(lnmt+im),(iza(liza+m),fme(lfme+m),                      !/*rhx         9*/
     & m=jmd(ljmd+im),jmd(ljmd+im+1)-1)                                         !/*rhx        10*/
  129 format(i6,5x,4(i11,1h,,1pe12.5)/(i22,1h,,e12.5,i11,1h,,e12.5,             !/*rhx        11*/
     & i11,1h,,e12.5,i11,1h,,e12.5))                                            !/*rhx        12*/
      else                                                                      !/*rhx        13*/
      write(iuo,130)nmt(lnmt+im),(iza(liza+m),fme(lfme+m),                      !/*rh         77*/
     & m=jmd(ljmd+im),jmd(ljmd+im+1)-1)                                         !/*rh         78*/
      endif                                                                     !/*rhx        14*/
  130 format(i6,5x,4(i11,1h,,1pe18.11)/(i22,1h,,e18.11,i11,1h,,e18.11,          !/*rhx        15*/
     & i11,1h,,e18.11,i11,1h,,e18.11))                                          !/*rhx        16*/
      ht=' '                                                                    !/*rh         81*/
      l=1                                                                       !/*rh         82*/
      do 150 m=1,indt                                                           !/*rh         83*/
      if(jmt(ljmt+m).ne.nmt(lnmt+im))go to 150                                  !/*rh         84*/
      call zaid(2,ht(l:l+9),kmt(lkmt+1,m))                                      !/*cor4-1     96*/
      ht(l+9:l+9)='t'                                                           !/*cor4-1     97*/
      l=l+12                                                                    !/*rh         89*/
  150 continue                                                                  !/*rh         90*/
      if(l.ne.1)write(iuo,160)ht                                                !/*rh         91*/
  160 format(37h associated thermal s(a,b) data sets:,2x,a82)                   !/*rh         92*/
  170 nl=nl+((npq(lnpq+im)+3)/4)*2+min(1,l-1)                                   !/*rh         93*/
!                                                                               !/*rh         94*/
!        print table of material compositions (mass fractions).                 !/*rh         95*/
      if(2*(nu+nd)+nl.lt.58)write(iuo,'(/////9h material)')                     !/*rh         96*/
      if(2*(nu+nd)+nl.ge.58)write(iuo,180)                                      !/*rh         97*/
  180 format(9h1material,95x,14hprint table 40)                                 !/*rh         98*/
      write(iuo,190)                                                            !/*rh         99*/
  190 format(8h  number,5x,32hcomponent nuclide, mass fraction/)                !/*rh        100*/
      do 210 im=1,nmat                                                          !/*rh        101*/
      sw=0.                                                                     !/*rh        102*/
      do 200 m=jmd(ljmd+im),jmd(ljmd+im+1)-1                                    !/*rh        103*/
  200 sw=sw+fme(lfme+m)*awc(lawc+m)                                             !/*rh        104*/
      if(dbcn(26).ne.0.)then                                                    !/*rhx        17*/
      write(iuo,129)nmt(lnmt+im),(iza(liza+m),                                  !/*rhx        18*/
     & fme(lfme+m)*awc(lawc+m)/sw,m=jmd(ljmd+im),jmd(ljmd+im+1)-1)              !/*rhx        19*/
      else                                                                      !/*rhx        20*/
      write(iuo,130)nmt(lnmt+im),(iza(liza+m),                                  !/*rhx        21*/
     & fme(lfme+m)*awc(lawc+m)/sw,m=jmd(ljmd+im),jmd(ljmd+im+1)-1)              !/*rhx        22*/
      endif                                                                     !/*rhx        23*/
  210 continue                                                                  !/*rhx        24*/
!                                                                               !/*rh        109*/
!        print warning messages.                                                !/*rh        110*/
  230 if(nu.ne.0)call erprnt(1,2,1,nu,0,0,0,0,                                  !/*rh        111*/
     & 'i3,54h materials had unnormalized fractions. print table 40.')          !/*rh4b        4*/
      if(nd.ne.0)call erprnt(1,2,1,nd,0,0,0,0,                                  !/*rh        113*/
     & 'i3,50h of the materials appear at more than one density.')              !/*rh        114*/
      if(nr.ne.0)call erprnt(1,2,1,nr,0,0,0,0,                                  !/*rh4a        6*/
     & 'i3,44h cell densities are greater than 40 gram/cc.')                    !/*rh4a        7*/
      return                                                                    !/*rh        117*/
      end                                                                       !/*rh        118*/
