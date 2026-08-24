!#ident cp_rng.F from deck xcr                                                /*xcr         1*/
      subroutine cp_rng(ip)                                                     !/*xcr         2*/
!        calculate charged particle mean energy loss and step length.           !/*xcr         3*/
!        ip = particle designator number for charged partilce type.             !/*xcr         4*/
!        az = average atomic number   aa = average atomic weight                !/*xcr         5*/
!        TEMPORARY LIMITATION.  number of substeps per energy step              !/*xcr         6*/
!                               controlled by electron nsb(lnsb+mkc).           !/*xcr         7*/
!        see related comments in subroutine ronge.                              !/*xcr         8*/
      use global_data                                                           !/*xcr         9*/
!                                                                               !/*xcr        10*/
      implicit real(kindr) (a-h,o-z)                                            !/*xcr        11*/
      implicit integer(kindi) (i-n)                                             !/*xcr        12*/
!                                                                               !/*xcr        13*/
      parameter (em=1.0d-06, ao=0.99d0)                                         !/*xcr        14*/
      parameter (km=5, mz=103)                                                  !/*xcr        15*/
      dimension el(km),g(km),ns(mz)                                             !/*xcr        16*/
      character hr*12                                                           !/*xcr        17*/
      data ns/5*2,4*3,3*4,9*5,7*6,11*7,10*8,5*9,10*10,5*11,9*12,6*13,           !/*xcr        18*/
     &  7*14,12*15/                                                             !/*xcr        19*/
!                                                                               !/*xcr        20*/
      dk=km-1                                                                   !/*xcr        21*/
!                                                                               !/*xcr        22*/
!     material loop.                                                            !/*xcr        23*/
      do 70 mk=1,nmat                                                           !/*xcr        24*/
         do 10 ic=1,mxa                                                         !/*xcr        25*/
   10       if(mat(lmat+ic).eq.mk)go to 20                                      !/*xcr        26*/
         go to 70                                                               !/*xcr        27*/
   20    az=0.                                                                  !/*xcr        28*/
         aa=0.                                                                  !/*xcr        29*/
         zq=0.                                                                  !/*xcr        30*/
         kk=0                                                                   !/*xcr        31*/
         kz=iza(liza+jmd(ljmd+mk))/1000                                         !/*xcr        32*/
         do 30 j=jmd(ljmd+mk),jmd(ljmd+mk+1)-1                                  !/*xcr        33*/
            iz=iza(liza+j)/1000                                                 !/*xcr        34*/
            if(iz.ne.kz)kk=2                                                    !/*xcr        35*/
            az=az+fme(lfme+j)*iz                                                !/*xcr        36*/
            aa=aa+awc(lawc+j)*fme(lfme+j)                                       !/*xcr        37*/
   30       zq=zq+awc(lawc+j)*fme(lfme+j)*iz**2                                 !/*xcr        38*/
         avz(lavz+mk)=az                                                        !/*xcr        39*/
         ava(lava+mk)=aa                                                        !/*xcr        40*/
         af=avgdn/aa                                                            !/*xcr        41*/
         jz=sqrt(zq/aa)+ao                                                      !/*xcr        42*/
         if (jz.lt.1 .or. jz.gt.mz) call expire(0,'cp_rng',                     !/*xcr        43*/
     &      'atomic number out of range.')                                      !/*xcr        44*/
         nd=ns(jz)                                                              !/*xcr        45*/
!           TEMPORARY expedient: allow estep to control all substeps.           !/*xcr        46*/
         if(nsb(lnsb+mk).le.0)nsb(lnsb+mk)=nd                                   !/*xcr        47*/
         if(nsb(lnsb+mk).lt.nd)call erprnt(1,2,1,mk,0,0,0,0,                    !/*xcr        48*/
     &     '45hfewer substeps than the default for material ,i5')               !/*xcr        49*/
!                                                                               !/*xcr        50*/
!        calculate the mean-ionization-potential term.                          !/*xcr        51*/
!        TEMPORARY LIMITATION.  the density of the first cell with              !/*xcr        52*/
!                               the current material is used.                   !/*xcr        53*/
         jj=1                                                                   !/*xcr        54*/
         if(jemi(ljem+mk).eq.0)jj=2                                             !/*xcr        55*/
         jj=jj+kk                                                               !/*xcr        56*/
         bi=0.                                                                  !/*xcr        57*/
         do 40 j=jmd(ljmd+mk),jmd(ljmd+mk+1)-1                                  !/*xcr        58*/
            iz=iza(liza+j)/1000                                                 !/*xcr        59*/
            p=em*f_ionp(iz,2,jj)                                                !/*xcr        60*/
   40       bi=bi+log(p)*iz*fme(lfme+j)                                         !/*xcr        61*/
         bi=bi/az                                                               !/*xcr        62*/
         sr=exp(bi)/em                                                          !/*xcr        63*/
!                                                                               !/*xcr        64*/
!        energy loop.                                                           !/*xcr        65*/
         nc=in_erg(ip)*log(emx(ip)/ecut_min(ip))/log(two)+one                   !/*xcr        66*/
         do 60 nz=1,nc                                                          !/*xcr        67*/
            n=nc+1-nz                                                           !/*xcr        68*/
            if(n.le.nee(ip)) then                                               !/*xcr        69*/
               e1=eee(leee+ip,n)                                                !/*xcr        70*/
            else                                                                !/*xcr        71*/
               e1=eee(leee+ip,nee(ip))*efac(ip)**(n-nee(ip))                    !/*xcr        72*/
            endif                                                               !/*xcr        73*/
            de=e1*(one-efac(ip))                                                !/*xcr        74*/
            bs=e1*(e1+2.*gpt(ip))/(e1+gpt(ip))**2                               !/*xcr        75*/
!                                                                               !/*xcr        76*/
!           integration loop.                                                   !/*xcr        77*/
            do 50 k=1,km                                                        !/*xcr        78*/
               e=e1-(k-1)*de/dk                                                 !/*xcr        79*/
               el(k)=dedx(e,gpt(ip),abs(charge(ip)),az,rho(lrho+ic),bi,         !/*xcr        80*/
     &           fme(lfme+jmd(ljmd+mk)),awc(lawc+jmd(ljmd+mk)),                 !/*xcr        81*/
     &           iza(liza+jmd(ljmd+mk)),jmd(ljmd+mk+1)-jmd(ljmd+mk),jj,         !/*xcr        82*/
     &           dp)/rho(lrho+ic)                                               !/*xcr        83*/
               if(k.eq.1)dd=dp                                                  !/*xcr        84*/
   50          g(k)=one/el(k)                                                   !/*xcr        85*/
!                                                                               !/*xcr        86*/
            if(n.eq.nc)rg=.5*(e1-de)*g(km)                                      !/*xcr        87*/
            if(n.eq.nee(ip)-1)rng(lrng+ip,nee_max*mk)=rg                        !/*xcr        88*/
!                                                                               !/*xcr        89*/
!           average the range increment over the energy interval.               !/*xcr        90*/
            dr=simint(de,g,km)                                                  !/*xcr        91*/
            rg=rg+dr                                                            !/*xcr        92*/
!                                                                               !/*xcr        93*/
            if(n.lt.nee(ip)) then                                               !/*xcr        94*/
               rng(lrng+ip,n+nee_max*(mk-1))=rg                                 !/*xcr        95*/
               drs(ldrs+ip,n+nee_max*(mk-1))=dr/nsb(lnsb+mk)                    !/*xcr        96*/
               qav(lqav+ip,n+nee_max*(mk-1))=de/dr                              !/*xcr        97*/
!                                                                               !/*xcr        98*/
               if(jovr(1)+jovr(3)+jovr(5).eq.3.and.ip.eq.3) then                !/*xcr        99*/
!                 load print table 85 for xs plotting.                          !/*xcr       100*/
                  nk=n+nee(ip)*(mk-1)                                           !/*xcr       101*/
                  xse85(lx85+1,nk)=el(1)*af                                     !/*xcr       102*/
                  xse85(lx85+2,nk)=0.                                           !/*xcr       103*/
                  xse85(lx85+3,nk)=el(1)*af                                     !/*xcr       104*/
                  xse85(lx85+4,nk)=rg/af                                        !/*xcr       105*/
                  xse85(lx85+5,nk)=0.                                           !/*xcr       106*/
                  xse85(lx85+6,nk)=bs                                           !/*xcr       107*/
                  xse85(lx85+7,nk)=dd                                           !/*xcr       108*/
                  xse85(lx85+8,nk)=0.                                           !/*xcr       109*/
                  xse85(lx85+9,nk)=dr/af                                        !/*xcr       110*/
                  xse85(lx85+10,nk)=0.                                          !/*xcr       111*/
               endif                                                            !/*xcr       112*/
            endif                                                               !/*xcr       113*/
!                                                                               !/*xcr       114*/
            if(ink(85).ne.0) then                                               !/*xcr       115*/
!              print range table.                                               !/*xcr       116*/
               if(jemi(ljem+mk).eq.0) then                                      !/*xcr       117*/
                  hr=' (condensed)'                                             !/*xcr       118*/
               else                                                             !/*xcr       119*/
                  hr=' (gas)      '                                             !/*xcr       120*/
               endif                                                            !/*xcr       121*/
               if(nz.eq.1)write(iuo,80)hnp(ip),nmt(lnmt+mk),hr,                 !/*xcr       122*/
     &           nsb(lnsb+mk),nd,sr                                             !/*xcr       123*/
               write(iuo,90)n,e1,el(1)*af,0.,el(1)*af,rg/af,0.,                 !/*xcr       124*/
     &           bs,dd,0.,dr/af,0.                                              !/*xcr       125*/
            endif                                                               !/*xcr       126*/
   60       continue                                                            !/*xcr       127*/
   70    continue                                                               !/*xcr       128*/
      return                                                                    !/*xcr       129*/
!                                                                               !/*xcr       130*/
   80 format(1h1,a11,                                                           !/*xcr       131*/
     &  25h range table for material,i5,a12,62x,14hprint table 85//             !/*xcr       132*/
     &  36h particle substeps per energy step =,i4,1h,,2x,9hdefault =,          !/*xcr       133*/
     &  i4,1h.,2x,24hmean ionization energy =,1pe12.5,4h ev.//                  !/*xcr       134*/
     &  9x,6henergy,11x,14hstopping power,11x,5hrange,3x,9hradiation,           !/*xcr       135*/
     &  3x,7hbeta**2,3x,7hdensity,3x,7hrad/col,3x,6hdrange,4x,6hdyield/         !/*xcr       136*/
     &  4x,1hn,14x,19hcollision radiation,3x,5htotal,15x,5hyield,16x,           !/*xcr       137*/
     &  4hcorr/10x,3hmev,6x,29hmev cm2/g mev cm2/g mev cm2/g,3x,                !/*xcr       138*/
     &  5hg/cm2,24x,9hmev cm2/g,13x,5hg/cm2/1h )                                !/*xcr       139*/
   90 format(i5,1pe12.4,2(1x,5e10.3))                                           !/*xcr       140*/
      end                                                                       !/*xcr       141*/
