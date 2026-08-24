!#ident f_ionp.F from deck xfi                                                /*xfi         1*/
      function f_ionp (iz,im,jj)                                                !/*xfi         2*/
!        Computes mean ionization potentials in ev.                             !/*xfi         3*/
!        See SPAR Manual, p. 5.                                                 !/*xfi         4*/
!                                                                               !/*xfi         5*/
!        im = 1 Original SPAR model. Similar, but not identical,                !/*xfi         6*/
!               to ITS1 and MCNP4A.                                             !/*xfi         7*/
!        im = 2 From Berger and Selzer, as in ITS3 and MCNP4B.                  !/*xfi         8*/
!               Recommended.                                                    !/*xfi         9*/
!                                                                               !/*xfi        10*/
!        jj=1 gaseous element                                                   !/*xfi        11*/
!        jj=2 condensed element                                                 !/*xfi        12*/
!        jj=3 gaseous mixture                                                   !/*xfi        13*/
!        jj=4 condensed mixture                                                 !/*xfi        14*/
!                                                                               !/*xfi        15*/
#ifdef CHEAP                                                                    /*!xfi        16*/
      implicit double precision (a-h,o-z)                                       !/*xfi        17*/
#endif                                                                          /*!xfi        18*/
      parameter (p0=0.d0, p2=2.0d0, p3=3.0d0)                                   !/*xfi        19*/
      parameter (a1=9.76d0, a2=58.8d0, a3=-0.19d0)                              !/*xfi        20*/
      parameter (b1=10.3d0, b2=1.0d0, b3=0.793d0, b4=p2/p3)                     !/*xfi        21*/
      dimension pv(13), pm(10:100), ph(9,4), ff(4)                              !/*xfi        22*/
!                                                                               !/*xfi        23*/
!        mean ionization potentials.                                            !/*xfi        24*/
      data pv /18.7d0,42.d0,39.d0,60.d0,68.d0,78.d0,99.5d0,98.5d0,              !/*xfi        25*/
     & 117.d0,140.d0,150.d0,157.d0,163.d0/                                      !/*xfi        26*/
      data (pm(i),i=10,100) /1.37d+02,1.49d+02,1.56d+02,1.66d+02,               !/*xfi        27*/
     & 1.73d+02,1.73d+02,1.80d+02,1.5929d+02,1.88d+02,1.90d+02,1.91d+02,        !/*xfi        28*/
     & 2.16d+02,2.33d+02,2.45d+02,2.57d+02,2.72d+02,2.86d+02,2.97d+02,          !/*xfi        29*/
     & 3.11d+02,3.22d+02,3.30d+02,3.34d+02,3.50d+02,3.47d+02,3.48d+02,          !/*xfi        30*/
     & 3.57d+02,3.52d+02,3.63d+02,3.66d+02,3.79d+02,3.93d+02,4.17d+02,          !/*xfi        31*/
     & 4.24d+02,4.28d+02,4.41d+02,4.49d+02,4.70d+02,4.70d+02,4.69d+02,          !/*xfi        32*/
     & 4.88d+02,4.88d+02,4.87d+02,4.85d+02,4.91d+02,4.82d+02,4.88d+02,          !/*xfi        33*/
     & 4.91d+02,5.01d+02,5.23d+02,5.35d+02,5.46d+02,5.60d+02,5.74d+02,          !/*xfi        34*/
     & 5.80d+02,5.91d+02,6.14d+02,6.28d+02,6.50d+02,6.58d+02,6.74d+02,          !/*xfi        35*/
     & 6.84d+02,6.94d+02,7.05d+02,7.18d+02,7.27d+02,7.36d+02,7.46d+02,          !/*xfi        36*/
     & 7.57d+02,7.90d+02,7.90d+02,8.00d+02,8.10d+02,8.23d+02,8.23d+02,          !/*xfi        37*/
     & 8.30d+02,8.25d+02,7.94d+02,8.27d+02,8.26d+02,8.41d+02,8.47d+02,          !/*xfi        38*/
     & 8.78d+02,8.90d+02,9.02d+02,9.21d+02,9.34d+02,9.39d+02,9.52d+02,          !/*xfi        39*/
     & 9.66d+02,9.80d+02,9.94d+02/                                              !/*xfi        40*/
      data ph /1.92d+01,4.18d+01,3.40d+01,3.86d+01,4.90d+01,7.00d+01,           !/*xfi        41*/
     & 8.20d+01,9.50d+01,1.15d+02,2.18d+01,4.18d+01,4.00d+01,6.37d+01,          !/*xfi        42*/
     & 7.60d+01,7.80d+01,9.09d+01,1.05d+02,1.12d+02,1.92d+01,4.18d+01,          !/*xfi        43*/
     & 3.40d+01,3.86d+01,4.90d+01,7.00d+01,8.20d+01,9.70d+01,1.15d+02,          !/*xfi        44*/
     & 1.92d+01,4.18d+01,4.52d+01,7.20d+01,8.59d+01,8.10d+01,8.20d+01,          !/*xfi        45*/
     & 1.06d+02,1.12d+02/                                                       !/*xfi        46*/
!                                                                               !/*xfi        47*/
      ff(1)=1.                                                                  !/*xfi        48*/
      ff(2)=1.                                                                  !/*xfi        49*/
      ff(3)=1.13                                                                !/*xfi        50*/
      ff(4)=1.13                                                                !/*xfi        51*/
!                                                                               !/*xfi        52*/
      z=iz                                                                      !/*xfi        53*/
      if (im.eq.1) then                                                         !/*xfi        54*/
         if (iz.gt.13) then                                                     !/*xfi        55*/
            p=a1*z+a2*z**a3                                                     !/*xfi        56*/
         else                                                                   !/*xfi        57*/
            p=pv(iz)                                                            !/*xfi        58*/
         endif                                                                  !/*xfi        59*/
      elseif (im.eq.2) then                                                     !/*xfi        60*/
         if (iz.le.9) then                                                      !/*xfi        61*/
            p=ph(iz,jj)                                                         !/*xfi        62*/
         elseif (iz.le.100) then                                                !/*xfi        63*/
            p=ff(jj)*pm(iz)                                                     !/*xfi        64*/
         else                                                                   !/*xfi        65*/
            p=b1*z*(b2-b3/z**b4)                                                !/*xfi        66*/
         endif                                                                  !/*xfi        67*/
      else                                                                      !/*xfi        68*/
         p=p0                                                                   !/*xfi        69*/
         call erprnt(1,1,1,im,0,0,0,0,                                          !/*xfi        70*/
     &    '44hillegal ionization energy model designator: ,i10')                !/*xfi        71*/
      endif                                                                     !/*xfi        72*/
      f_ionp=p                                                                  !/*xfi        73*/
      return                                                                    !/*xfi        74*/
      end                                                                       !/*xfi        75*/
