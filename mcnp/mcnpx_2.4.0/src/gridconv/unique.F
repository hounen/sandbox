!     $Id: unique.F,v 1.4 2002/03/27 18:13:56 gwm Exp $                       !/*un          1*/
      subroutine unique(hf,jt)                                                  !/*un          2*/
!        find a unique name for file hf.                                        !/*un          3*/
!        jt = tty output unit number.                                           !/*un          4*/
      character hf*(*),ht*40,ha*1                                               !/*un          5*/
!                                                                               !/*un          6*/
      n=0                                                                       !/*un          7*/
      l=leng(hf)                                                                !/*un          8*/
      ht=hf(1:l)//' '                                                           !/*un          9*/
      j=index(ht(1:l+1),' ')-1                                                  !/*un         10*/
      ht=hf(1:l)//'.'                                                           !/*un         11*/
      k=index(ht(1:l+1),'.')-1                                                  !/*un         12*/
      j=min(j,k)                                                                !/*un         13*/
      m=26                                                                      !/*un         14*/
      ha='a'                                                                    !/*un         15*/
      if(index('0123456789',hf(j:j)).eq.0)go to 10                              !/*un         16*/
      m=10                                                                      !/*un         17*/
      ha='0'                                                                    !/*un         18*/
   10 if(inqire(hf(1:l)).eq.0)return                                            !/*un         19*/
      n=n+1                                                                     !/*un         20*/
      ht=hf                                                                     !/*un         21*/
      if(n.gt.m) then                                                           !/*un         22*/
         write(6,'("cannot create ",a40)')ht(1:l)                               !/*un         23*/
         stop                                                                   !/*un         24*/
      endif                                                                     !/*un         25*/
      hf(j:j)=char(mod(ichar(ht(j:j))-ichar(ha)+1,m)+ichar(ha))                 !/*un         26*/
      if(jt.ne.0)write(jt,20)ht(1:l),hf(1:l)                                    !/*un         27*/
   20 format(1x,a,18h already exists.  ,a,20h is created instead.)              !/*un         28*/
      go to 10                                                                  !/*un         29*/
      end                                                                       !/*un         30*/
