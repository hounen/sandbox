!#ident covar.F from deck cw4b                                                  !/*cw4b        1*/
      subroutine covar(os,o2,za,ea,zb,eb,zc,ec,cz,mc)                           !/*cw4b        2*/
!        get simple and covariance weighted averages and std. devs.             !/*cw4b        3*/
!        given:                                                                 !/*cw4b        4*/
!           mc = number of cycles over which to average.                        !/*cw4b        5*/
!           os(3) = cumulative sum of zz(i).                                    !/*cw4b        6*/
!           o2(3,3) = cumulative sum of zz(i) cross products.                   !/*cw4b        7*/
!        calculate: (col/abs), (abs/tl), (tl/col), (col/abs/tl)                 !/*cw4b        8*/
!           za(3),ea(3) = simple (col,abs,tl) averages and std. devs.           !/*cw4b        9*/
!           zb(4),eb(4) = simple combined averages and std. devs.               !/*cw4b       10*/
!           zc(4),ec(4) = covariance-weighted averages and std. devs.           !/*cw4b       11*/
!           cz(3) = covariances.                                                !/*cw4b       12*/
!                                                                               !/*cw4b       13*/
!        the combined estimators and errors are based on the article by         !/*cw4b       14*/
!        max halperin, "almost linearly-optimum combination of unbiased         !/*cw4b       15*/
!        estimates," amer. stat. assn. j. 56, p. 36 - 43 (1961).                !/*cw4b       16*/
!                                                                               !/*cw4b       17*/
      use global_parameters                                                     !/*cwx         1*/
      implicit real(kindr) (a-h,o-z)                                            !/*cwx         2*/
      implicit integer(kindi) (i-n)                                             !/*cwx         3*/
      dimension cv(3,3),cz(3),ea(3),eb(4),ec(4),os(3),o2(3,3),                  !/*cw4b       20*/
     & za(3),zb(4),zc(4)                                                        !/*cw4b       21*/
!                                                                               !/*cw4b       22*/
      do 10 i=1,4                                                               !/*cw4b       23*/
      zb(i)=0.                                                                  !/*cw4b       24*/
      zc(i)=0.                                                                  !/*cw4b       25*/
      eb(i)=0.                                                                  !/*cw4b       26*/
   10 ec(i)=0.                                                                  !/*cw4b       27*/
!                                                                               !/*cw4b       28*/
!        calculate simple average, std. dev., covariance: za, ea and cv.        !/*cw4b       29*/
      co=0.                                                                     !/*cw4b       30*/
      f0=mc                                                                     !/*cw4b       31*/
      f1=sqrt(f0/max(1,mc-1))                                                   !/*cw4b       32*/
      do 30 i=1,3                                                               !/*cw4b       33*/
      za(i)=os(i)/mc                                                            !/*cw4b       34*/
      cz(i)=0.                                                                  !/*cw4b       35*/
      do 20 j=1,3                                                               !/*cw4b       36*/
      cv(i,j)=o2(i,j)-os(i)*os(j)/mc                                            !/*cw4b       37*/
   20 co=co+cv(i,j)                                                             !/*cw4b       38*/
   30 ea(i)=f1*sqrt(abs(cv(i,i)))/mc                                            !/*cw4b       39*/
!                                                                               !/*cw4b       40*/
!        calculate simple combined averages, zb, and covariance weighted        !/*cw4b       41*/
!        averages, zc, and std. devs., eb and ec, and covariances, cz,          !/*cw4b       42*/
!        by pairs:  (col/abs), (abs,tl), (tl,col).                              !/*cw4b       43*/
      if(mc.le.2)return                                                         !/*cw4b       44*/
      f2=sqrt(f0/(f0-2.))                                                       !/*cw4b       45*/
      do 40 i=1,3                                                               !/*cw4b       46*/
      j=mod(i,3)+1                                                              !/*cw4b       47*/
      zb(i)=.5*(za(i)+za(j))                                                    !/*cw4b       48*/
      eb(i)=.5*f1*sqrt(abs(cv(i,i)+cv(j,j)+cv(i,j)+cv(i,j)))/mc                 !/*cw4b       49*/
      zc(i)=za(i)                                                               !/*cw4b       50*/
      ec(i)=ea(i)                                                               !/*cw4b       51*/
      t5=cv(i,i)+cv(j,j)-cv(i,j)-cv(i,j)                                        !/*cw4b       52*/
      if(t5.le.1.e-10*(abs(cv(i,i))+abs(cv(i,j))+abs(cv(j,j))))go to 40         !/*cw4b       53*/
      zc(i)=za(i)-(za(i)-za(j))*(cv(i,i)-cv(i,j))/t5                            !/*cw4b       54*/
      ec(i)=f2*sqrt(abs((cv(i,i)*cv(j,j)-cv(i,j)**2)*                           !/*cw4b       55*/
     & (t5+f0*(za(i)-za(j))**2)/t5**2))/mc                                      !/*cw4b       56*/
   40 if(cv(i,i)*cv(j,j).gt.0.)cz(i)=cv(i,j)/sqrt(abs(cv(i,i)*cv(j,j)))         !/*cw4b       57*/
      zb(4)=(za(1)+za(2)+za(3))/3                                               !/*cw4b       58*/
      eb(4)=f1*sqrt(abs(co))/(3*mc)                                             !/*cw4b       59*/
!                                                                               !/*cw4b       60*/
!        calculate the combined average (col/abs/tl), zc(4).                    !/*cw4b       61*/
      if(mc.eq.3)return                                                         !/*cw4b       62*/
      a=0.                                                                      !/*cw4b       63*/
      s1=0.                                                                     !/*cw4b       64*/
      s2=0.                                                                     !/*cw4b       65*/
      s3=0.                                                                     !/*cw4b       66*/
      k=3                                                                       !/*cw4b       67*/
      if(abs(cz(1)-1.).lt.1.e-5)go to 60                                        !/*cw4b       68*/
      do 50 i=1,3                                                               !/*cw4b       69*/
      j=mod(i,3)+1                                                              !/*cw4b       70*/
      k=mod(j,3)+1                                                              !/*cw4b       71*/
      al=cv(j,j)*cv(k,k)-cv(j,j)*cv(i,k)-cv(k,k)*cv(i,j)+                       !/*cw4b       72*/
     & cv(j,k)*(cv(i,j)+cv(i,k)-cv(j,k))                                        !/*cw4b       73*/
      a=a+al                                                                    !/*cw4b       74*/
      zc(4)=zc(4)+al*za(i)                                                      !/*cw4b       75*/
      s1=s1+al*cv(i,1)                                                          !/*cw4b       76*/
      s2=s2+(cv(j,j)+cv(k,k)-cv(j,k)-cv(j,k))*za(i)**2                          !/*cw4b       77*/
   50 s3=s3+(cv(k,k)+cv(i,j)-cv(j,k)-cv(i,k))*za(i)*za(j)                       !/*cw4b       78*/
      if(a.eq.0..or.zc(4).eq.0.)go to 60                                        !/*cw4b       79*/
      zc(4)=zc(4)/a                                                             !/*cw4b       80*/
      ec(4)=sqrt(abs(s1*(1.+f0*(s2-s3-s3)/a)/(a*f0*(f0-3.))))                   !/*cw4b       81*/
      return                                                                    !/*cw4b       82*/
   60 zc(4)=zc(k)                                                               !/*cw4b       83*/
      ec(4)=ec(k)                                                               !/*cw4b       84*/
      return                                                                    !/*cw4b       85*/
      end                                                                       !/*cw4b       86*/
