!#ident qnc7.F from deck math                                                   !/*math        1*/
      function qnc7(fn,y1,y2,fe,me,kn)                                          !/*math        2*/
!        this subroutine is from the los alamos mathematics library.            !/*math        3*/
!        it has been modified to comply with fortran 77 and godfrey             !/*math4c      1*/
!        programming standards.  see writeup d119 for details.                  !/*math        5*/
!                                                                               !/*math        6*/
!        integrate function fn from y1 to y2 within relative error fe.          !/*math        7*/
!        me = nominal maximum number of evaluations of fn.                      !/*math        8*/
!        the actual number of evaluations of fn is returned in kn.              !/*math        9*/
!                                                                               !/*math       10*/
      use global_parameters                                                     !/*mthx        1*/
      implicit real(kindr) (a-h,o-z)                                            !/*mthx        2*/
      implicit integer(kindi) (i-n)                                             !/*mthx        3*/
      dimension gn(7,20),f(13),lg(20),dt(20),xt(20),ag(20),et(20),              !/*math       13*/
     & es(20),sm(20)                                                            !/*math       14*/
      parameter (wf=840d0,w1=41./wf,w2=216./wf,w3=27./wf,w4=272./wf)            !/*math       15*/
      external fn                                                               !/*cor4-1    229*/
!                                                                               !/*math       16*/
      bk=0.                                                                     !/*math       17*/
      kd=1                                                                      !/*math       18*/
      mv=me                                                                     !/*math       19*/
      ld=me                                                                     !/*math       20*/
      lm=20                                                                     !/*math       21*/
      a=y1                                                                      !/*math       22*/
      ep=fe                                                                     !/*math       23*/
      da=y2-a                                                                   !/*math       24*/
      ar=1.                                                                     !/*math       25*/
      ee=1.                                                                     !/*math       26*/
      l=0                                                                       !/*math       27*/
      h2=da/12.                                                                 !/*math       28*/
      do 10 i=1,11,2                                                            !/*math       29*/
   10 f(i)=fn(a+h2*(i-1))                                                       !/*math       30*/
      f(13)=fn(y2)                                                              !/*math       31*/
      kn=7                                                                      !/*math       32*/
   20 dx=.5*da                                                                  !/*math       33*/
      h2=da/12.                                                                 !/*math       34*/
      xm=a+dx                                                                   !/*math       35*/
      do 30 i=2,12,2                                                            !/*math       36*/
   30 f(i)=fn(a+h2*(i-1))                                                       !/*math       37*/
      kn=kn+6                                                                   !/*math       38*/
      if(kn.le.mv)go to 40                                                      !/*math       39*/
      ld=.5*ld                                                                  !/*math       40*/
      mv=mv+ld                                                                  !/*math       41*/
      lm=10.*(y2-a)/(y2-y1)+1.                                                  !/*math       42*/
      kd=-1                                                                     !/*math       43*/
   40 el=(w1*(f(1)+f(7))+w2*(f(2)+f(6))+w3*(f(3)+f(5))+w4*f(4))*dx              !/*math       44*/
      er=(w1*(f(7)+f(13))+w2*(f(8)+f(12))+w3*(f(9)+f(11))+w4*f(10))*dx          !/*math       45*/
      ar=(ar-abs(ee))+(abs(el)+abs(er))                                         !/*math       46*/
      su=el+er                                                                  !/*math       47*/
      if(abs(ee-su)-ep*ar-.8*bk.gt.0.)go to 50                                  !/*math       48*/
      bk=bk+ep*ar-abs(ee-su)                                                    !/*math       49*/
      if(ee.ne.1.)go to 80                                                      !/*math       50*/
   50 if(l.ge.lm)go to 80                                                       !/*math       51*/
      l=l+1                                                                     !/*math       52*/
      lg(l)=2                                                                   !/*math       53*/
      do 60 i=1,7                                                               !/*math       54*/
   60 gn(i,l)=f(i+6)                                                            !/*math       55*/
      dt(l)=dx                                                                  !/*math       56*/
      xt(l)=xm                                                                  !/*math       57*/
      ag(l)=ar                                                                  !/*math       58*/
      et(l)=ep/1.4                                                              !/*math       59*/
      es(l)=er                                                                  !/*math       60*/
      da=dx                                                                     !/*math       61*/
      do 70 ib=2,7                                                              !/*math       62*/
      i=9-ib                                                                    !/*math       63*/
   70 f(2*i-1)=f(i)                                                             !/*math       64*/
      ee=el                                                                     !/*math       65*/
      ep=et(l)                                                                  !/*math       66*/
      go to 20                                                                  !/*math       67*/
   80 do 90 i=1,l                                                               !/*math       68*/
      if(lg(l+1-i).ge.2)go to 100                                               !/*math       69*/
   90 su=sm(l+1-i)+su                                                           !/*math       70*/
      go to 120                                                                 !/*math       71*/
  100 l=l+1-i                                                                   !/*math       72*/
      sm(l)=su                                                                  !/*math       73*/
      lg(l)=1                                                                   !/*math       74*/
      a=xt(l)                                                                   !/*math       75*/
      da=dt(l)                                                                  !/*math       76*/
      do 110 i=1,7                                                              !/*math       77*/
  110 f(2*i-1)=gn(i,l)                                                          !/*math       78*/
      ar=ag(l)                                                                  !/*math       79*/
      ee=es(l)                                                                  !/*math       80*/
      ep=et(l)                                                                  !/*math       81*/
      go to 20                                                                  !/*math       82*/
  120 qnc7=su                                                                   !/*math       83*/
      kn=sign(kn,kd)                                                            !/*math       84*/
      return                                                                    !/*math       85*/
      end                                                                       !/*math       86*/
      subroutine smevev(nn,t,r,g,a,el,ke,m)                                     !/*math       87*/
!        this subroutine is from the los alamos mathematics library.            !/*math       88*/
!        it has been modified to comply with fortran 77 and godfrey             !/*math4c      2*/
!        programming standards.  see writeup f224 for details.                  !/*math       90*/
!                                                                               !/*math       91*/
!        find the eigenvalues r and eigenvectors g of real symmetric            !/*math       92*/
!        matrix t.                                                              !/*math       93*/
!                                                                               !/*math       94*/
      use global_parameters                                                     !/*mthx        4*/
      implicit real(kindr) (a-h,o-z)                                            !/*mthx        5*/
      implicit integer(kindi) (i-n)                                             !/*mthx        6*/
      parameter (ts=2.**(-47))                                                  !/*math       97*/
      dimension a(nn,nn),r(m),t(m),g(m,m)                                       !/*math       98*/
!                                                                               !/*math       99*/
      call smhhtr(nn,t,a,ke,m)                                                  !/*math      100*/
      if(ke.ne.0)return                                                         !/*math      101*/
      call prodhh(nn,t,a,ke,g,m)                                                !/*math      102*/
      if(ke.ne.0)return                                                         !/*math      103*/
      if(nn.gt.1)go to 10                                                       !/*math      104*/
      r(1)=a(1,1)                                                               !/*math      105*/
      return                                                                    !/*math      106*/
   10 es=max(el,ts)                                                             !/*math      107*/
      a(nn,2)=0.                                                                !/*mthx        7*/
      l1=1                                                                      !/*math      109*/
      ll=1                                                                      !/*math      110*/
   20 ll=ll+1                                                                   !/*math      111*/
      n=ll                                                                      !/*math      112*/
      if(ll.eq.nn)go to 30                                                      !/*math      113*/
      i=ll                                                                      !/*math      114*/
      if(abs(a(i-1,2)).gt.(abs(a(i,1))+abs(a(i-1,1)))*es)go to 130              !/*math      115*/
   30 nl=l1                                                                     !/*math      116*/
      np=nl+1                                                                   !/*math      117*/
   40 n1=n-1                                                                    !/*math      118*/
      if(n.lt.nl)go to 130                                                      !/*math      119*/
      if(n.eq.nl)go to 120                                                      !/*math      120*/
      mi=0                                                                      !/*math      121*/
      ep=es*(abs(a(n1,1))+abs(a(n,1)))                                          !/*math      122*/
      gp=es*(abs(a(nl,1))+abs(a(np,1)))                                         !/*math      123*/
      if(ep.eq.0.)ep=es                                                         !/*math      124*/
      if(gp.eq.0.)gp=es                                                         !/*math      125*/
   50 c=.5*(a(n,1)-a(n1,1))                                                     !/*math      126*/
      b=(a(n,1)+a(n1,1))*.5                                                     !/*math      127*/
      s=abs(c)                                                                  !/*math      128*/
      d=abs(a(n1,2))                                                            !/*math      129*/
      if(d.le.ep)go to 90                                                       !/*math      130*/
      d=max(d,s)*sqrt(1.+(min(d,s)/max(d,s))**2)                                !/*math      131*/
      sh=b+sign(d,c)                                                            !/*math      132*/
      a1=a(nl,1)-sh                                                             !/*math      133*/
      a2=a(np,1)-sh                                                             !/*math      134*/
      b1=a(nl,2)                                                                !/*math      135*/
      s=abs(b1)                                                                 !/*math      136*/
      d=abs(a1)                                                                 !/*math      137*/
      if(s.lt.gp)go to 110                                                      !/*math      138*/
      d=max(d,s)*sqrt(1.+(min(d,s)/max(d,s))**2)                                !/*math      139*/
      s=b1/d                                                                    !/*math      140*/
      c=a1/d                                                                    !/*math      141*/
      a1=a1+sh                                                                  !/*math      142*/
      a2=a2+sh                                                                  !/*math      143*/
      t2=c*a2-s*b1                                                              !/*math      144*/
      a(nl,1)=c*(c*a1+s*b1)+s*(c*b1+s*a2)                                       !/*math      145*/
      t1=c*b1-s*a1                                                              !/*math      146*/
      a(nl,2)=s*t2+c*t1                                                         !/*math      147*/
      a(np,1)=c*t2-s*t1                                                         !/*math      148*/
      e=a(np,2)*s                                                               !/*math      149*/
      a(np,2)=c*a(np,2)                                                         !/*math      150*/
      do 60 l=1,nn                                                              !/*math      151*/
      t1=c*g(l,nl)+s*g(l,np)                                                    !/*math      152*/
      g(l,np)=c*g(l,np)-s*g(l,nl)                                               !/*math      153*/
   60 g(l,nl)=t1                                                                !/*math      154*/
      do 80 i=np,n1                                                             !/*math      155*/
      b1=a(i-1,2)                                                               !/*math      156*/
      s=abs(b1)                                                                 !/*math      157*/
      c=abs(e)                                                                  !/*math      158*/
      if(c.lt.ts*s)go to 100                                                    !/*math      159*/
      d=max(c,s)*sqrt(1.+(min(c,s)/max(c,s))**2)                                !/*math      160*/
      s=e/d                                                                     !/*math      161*/
      c=b1/d                                                                    !/*math      162*/
      a2=a(i,1)                                                                 !/*math      163*/
      a3=a(i+1,1)                                                               !/*math      164*/
      a(i-1,2)=c*b1+s*e                                                         !/*math      165*/
      a(i,1)=c*(c*a2+s*a(i,2))+s*(c*a(i,2)+s*a3)                                !/*math      166*/
      t1=c*a(i,2)-s*a2                                                          !/*math      167*/
      t2=c*a3-s*a(i,2)                                                          !/*math      168*/
      a(i,2)=c*t1+s*t2                                                          !/*math      169*/
      a(i+1,1)=c*t2-s*t1                                                        !/*math      170*/
      do 70 l=1,nn                                                              !/*math      171*/
      t1=c*g(l,i)+s*g(l,i+1)                                                    !/*math      172*/
      g(l,i+1)=c*g(l,i+1)-s*g(l,i)                                              !/*math      173*/
   70 g(l,i)=t1                                                                 !/*math      174*/
      if(i.eq.n1)go to 80                                                       !/*math      175*/
      e=a(i+1,2)*s                                                              !/*math      176*/
      a(i+1,2)=c*a(i+1,2)                                                       !/*math      177*/
   80 continue                                                                  !/*math      178*/
      mi=mi+1                                                                   !/*math      179*/
      if(mi.gt.40)go to 140                                                     !/*math      180*/
      if(abs(a(n1,2)).gt.ep)go to 50                                            !/*math      181*/
   90 r(n)=a(n,1)                                                               !/*math      182*/
      n=n1                                                                      !/*math      183*/
      go to 40                                                                  !/*math      184*/
  100 ll=l1                                                                     !/*math      185*/
      go to 20                                                                  !/*math      186*/
  110 r(nl)=a(nl,1)                                                             !/*math      187*/
      nl=nl+1                                                                   !/*math      188*/
      np=np+1                                                                   !/*math      189*/
      go to 40                                                                  !/*math      190*/
  120 r(n)=a(n,1)                                                               !/*math      191*/
      l1=ll                                                                     !/*math      192*/
  130 if(ll.lt.nn)go to 20                                                      !/*math      193*/
      return                                                                    !/*math      194*/
  140 ke=1                                                                      !/*math      195*/
      return                                                                    !/*math      196*/
      end                                                                       !/*math      197*/
      subroutine smhhtr (in,a,t,ke,ig)                                          !/*math      198*/
!        auxiliary subroutine for smevev.                                       !/*math      199*/
!        this subroutine is from the los alamos mathematics library.            !/*math      200*/
!        it has been modified to comply with fortran 77 and godfrey             !/*math4c      3*/
!        programming standards.  see writeup f223 for details.                  !/*math      202*/
!                                                                               !/*math      203*/
      use global_parameters                                                     !/*mthx        8*/
      implicit real(kindr) (a-h,o-z)                                            !/*mthx        9*/
      implicit integer(kindi) (i-n)                                             !/*mthx       10*/
      dimension a(*),t(2*in)                                                    !/*math      206*/
!                                                                               !/*math      207*/
      n=in                                                                      !/*math      208*/
      l=1                                                                       !/*math      209*/
      do 10 j=1,n                                                               !/*math      210*/
      do 10 i=j,n                                                               !/*math      211*/
      a(l)=a(i+ig*(j-1))                                                        !/*math      212*/
   10 l=l+1                                                                     !/*math      213*/
      if(n.gt.2)go to 20                                                        !/*math      214*/
      t(1)=a(1)                                                                 !/*math      215*/
      ke=0                                                                      !/*math      216*/
      if(n.eq.1)return                                                          !/*math      217*/
      t(2)=a(3)                                                                 !/*math      218*/
      t(3)=a(2)                                                                 !/*math      219*/
      return                                                                    !/*math      220*/
   20 ir=2                                                                      !/*math      221*/
      nf=n-1                                                                    !/*math      222*/
      do 160 nr=3,n                                                             !/*math      223*/
      sq=0.                                                                     !/*math      224*/
      do 30 i=1,nf-1                                                            !/*math      225*/
   30 sq=sq+a(ir+i)**2                                                          !/*math      226*/
      t(nr-2)=a(ir-1)                                                           !/*math      227*/
      t(nr+n-2)=a(ir)                                                           !/*math      228*/
      s=a(ir)**2                                                                !/*math      229*/
      j=ir+1                                                                    !/*math      230*/
!                                                                               !/*math      231*/
!       skip if this col already tridiagonal (relatively)                       !/*math      232*/
      if(sq+s.eq.s)go to 40                                                     !/*math      233*/
      sq=sq+s                                                                   !/*math      234*/
      j=ir                                                                      !/*math      235*/
!                                                                               !/*math      236*/
!       skip if off diagonal elements insignificant.                            !/*math      237*/
      if(sq+a(ir-1)**2.gt.a(ir-1)**2)go to 60                                   !/*math      238*/
   40 do 50 i=j,ir+nf-1                                                         !/*math      239*/
   50 a(i)=0.                                                                   !/*math      240*/
      go to 150                                                                 !/*math      241*/
   60 s=sign(sqrt(sq),a(ir))                                                    !/*math      242*/
      tk=1./(sq+s*a(ir))                                                        !/*math      243*/
      a(ir)=a(ir)+s                                                             !/*math      244*/
      t(nr+n-2)=-s                                                              !/*math      245*/
      nm=nr-1                                                                   !/*math      246*/
      jr=ir+nf                                                                  !/*math      247*/
!                                                                               !/*math      248*/
!       calc p(r)                                                               !/*math      249*/
      t(nm)=0.                                                                  !/*math      250*/
      do 70 i=1,nf                                                              !/*math      251*/
   70 t(nm)=t(nm)+a(ir+i-1)*a(jr+i-1)                                           !/*math      252*/
      t(nm)=t(nm)*tk                                                            !/*math      253*/
      do 80 i=nr,n                                                              !/*math      254*/
   80 t(i)=0.                                                                   !/*math      255*/
      kr=jr+nf                                                                  !/*math      256*/
      l=jr                                                                      !/*math      257*/
      do 110 i=nr,n                                                             !/*math      258*/
      l=l+1                                                                     !/*math      259*/
      do 90 j=i,n                                                               !/*math      260*/
      t(j)=t(j)+a(l)*a(ir+i-nr)                                                 !/*math      261*/
   90 l=l+1                                                                     !/*math      262*/
      do 100 j=1,nf-1-i+nr                                                      !/*math      263*/
  100 t(i)=t(i)+a(kr+j-1)*a(ir+i-nr+j)                                          !/*math      264*/
      t(i)=t(i)*tk                                                              !/*math      265*/
  110 kr=kr+nf-1-i+nr                                                           !/*math      266*/
!                                                                               !/*math      267*/
!       calc q(r)                                                               !/*math      268*/
      up=0.                                                                     !/*math      269*/
      do 120 i=1,nf                                                             !/*math      270*/
  120 up=up+a(ir+i-1)*t(nm+i-1)                                                 !/*math      271*/
      up=.5*up*tk                                                               !/*math      272*/
      do 130 i=nm,n                                                             !/*math      273*/
  130 t(i)=t(i)-up*a(i+ir-nm)                                                   !/*math      274*/
!                                                                               !/*math      275*/
!       calc a(r)                                                               !/*math      276*/
      k=ir+nf                                                                   !/*math      277*/
      do 140 i=nm,n                                                             !/*math      278*/
      do 140 j=i,n                                                              !/*math      279*/
      a(k)=a(k)-t(i)*a(ir-nm+j)-t(j)*a(ir+i-nm)                                 !/*math      280*/
  140 k=k+1                                                                     !/*math      281*/
  150 ir=ir+nf+1                                                                !/*math      282*/
  160 nf=nf-1                                                                   !/*math      283*/
      t(n-1)=a(ir-1)                                                            !/*math      284*/
      t(n)=a(ir+1)                                                              !/*math      285*/
      t(n+n-1)=a(ir)                                                            !/*math      286*/
      ke=0                                                                      !/*math      287*/
      return                                                                    !/*math      288*/
      end                                                                       !/*math      289*/
      subroutine prodhh (n,a,t,ke,g,iv)                                         !/*math      290*/
!        auxiliary subroutine for smevev.                                       !/*math      291*/
!        this subroutine is from the los alamos mathematics library.            !/*math      292*/
!        it has been modified to comply with fortran 77 and godfrey             !/*math4c      4*/
!        programming standards.  see writeup f225 for details.                  !/*math      294*/
!                                                                               !/*math      295*/
      use global_parameters                                                     !/*mthx       11*/
      implicit real(kindr) (a-h,o-z)                                            !/*mthx       12*/
      implicit integer(kindi) (i-n)                                             !/*mthx       13*/
      dimension a(*),t(2*n),g(iv,iv)                                            !/*math      298*/
!                                                                               !/*math      299*/
      ke=0                                                                      !/*math      300*/
      g(n,n)=1.                                                                 !/*math      301*/
      if(n.eq.1)return                                                          !/*math      302*/
      ir=(n*(n+1))/2-4                                                          !/*math      303*/
      nf=2                                                                      !/*math      304*/
      mv=n-1                                                                    !/*math      305*/
      g(n-1,n-1)=1.                                                             !/*math      306*/
      g(n,n-1)=0.                                                               !/*math      307*/
      g(n-1,n)=0.                                                               !/*math      308*/
      do 80 mr=3,n                                                              !/*math      309*/
      do 10 i=1,nf-1                                                            !/*math      310*/
   10 if(a(ir+i).ne.0.)go to 20                                                 !/*math      311*/
      go to 60                                                                  !/*math      312*/
   20 tp=-1./(a(ir)*t(n+mv-1))                                                  !/*math      313*/
      do 40 i=mv+1,n                                                            !/*math      314*/
      g(1,i)=0.                                                                 !/*math      315*/
      do 30 j=1,nf                                                              !/*math      316*/
   30 g(1,i)=g(1,i)+a(ir+j-1)*g(mv+j-1,i)                                       !/*math      317*/
   40 g(1,i)=g(1,i)*tp                                                          !/*math      318*/
      g(1,mv)=a(ir)*tp                                                          !/*math      319*/
      do 50 i=mv,n                                                              !/*math      320*/
      do 50 j=mv,n                                                              !/*math      321*/
   50 g(i,j)=g(i,j)-a(ir+i-mv)*g(1,j)                                           !/*math      322*/
   60 mv=mv-1                                                                   !/*math      323*/
      do 70 i=mv,n                                                              !/*math      324*/
      g(mv,i)=0.                                                                !/*math      325*/
   70 g(i,mv)=0.                                                                !/*math      326*/
      g(mv,mv)=1.                                                               !/*math      327*/
      nf=nf+1                                                                   !/*math      328*/
   80 ir=ir-nf-1                                                                !/*math      329*/
      return                                                                    !/*math      330*/
      end                                                                       !/*math      331*/
