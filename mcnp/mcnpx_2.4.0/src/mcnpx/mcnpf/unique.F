!#ident unique.F from deck uq                                                   !/*uq          1*/
      subroutine unique(hf,jt)                                                  !/*uq          2*/
!        find a unique name for file hf.                                        !/*uq          3*/
!        jt = tty output unit number.                                           !/*uq          4*/
      character hf*(*),ht*40,ha*1                                               !/*uq4b        1*/
!                                                                               !/*uq          7*/
      n=0                                                                       !/*uq          8*/
      l=leng(hf)                                                                !/*uq4b        2*/
      ht=hf(1:l)//' '                                                           !/*uq4b        3*/
      j=index(ht(1:l+1),' ')-1                                                  !/*uq4b        4*/
      ht=hf(1:l)//'.'                                                           !/*uq4b        5*/
      k=index(ht(1:l+1),'.')-1                                                  !/*uq4b        6*/
      j=min(j,k)                                                                !/*uq4b        7*/
      m=26                                                                      !/*uq         10*/
      ha='a'                                                                    !/*uq         11*/
      if(index('0123456789',hf(j:j)).eq.0)go to 10                              !/*uq         12*/
      m=10                                                                      !/*uq         13*/
      ha='0'                                                                    !/*uq         14*/
   10 if(inqire(hf(1:l)).eq.0)return                                            !/*uq4b        8*/
      n=n+1                                                                     !/*uq         16*/
      ht=hf                                                                     !/*uq         18*/
      if(n.gt.m)call expire(0,'unique','cannot create '//ht(1:l))               !/*uq4b        9*/
      hf(j:j)=char(mod(ichar(ht(j:j))-ichar(ha)+1,m)+ichar(ha))                 !/*uq         19*/
      if(jt.ne.0)write(jt,20)ht(1:l),hf(1:l)                                    !/*uq4b       10*/
   20 format(1x,a,18h already exists.  ,a,20h is created instead.)              !/*uq4b       11*/
      go to 10                                                                  !/*uq         22*/
      end                                                                       !/*uq         24*/
