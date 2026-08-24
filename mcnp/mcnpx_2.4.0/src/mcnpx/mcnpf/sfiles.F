!#ident sfiles.F from deck zs                                                   !/*zs          1*/
      subroutine sfiles                                                         !/*zs          2*/
!        initiate the surface-source files.                                     !/*zs          3*/
      use global_data                                                           !/*zsx         1*/
      use imcn_data                                                             !/*zsx         2*/
!                                                                               !/*zsx         3*/
      implicit real(kindr) (a-h,o-z)                                            !/*zsx         4*/
      implicit integer(kindi) (i-n)                                             !/*zsx         5*/
!                                                                               !/*zsx         6*/
      dimension rs(6),rg(11)                                                    !/*zs4b        1*/
      character ha*80,hi*19,hk*8,hl*28,hp*19,hv*5,wx*8                          !/*zsxc        1*/
      data wx/'wxxa'/                                                           !/*zs          8*/
!                                                                               !/*zs          9*/
      if(konrun.ne.0)go to 20                                                   !/*zs         10*/
!                                                                               !/*zs         11*/
!        open surface-source files for an initial run.                          !/*zs         12*/
!        create the scratch file and write the first 2 records.                 !/*zs         13*/
      if(issw.eq.0)go to 10                                                     !/*zs         14*/
      call unique(wx,jtty)                                                      !/*zs         15*/
      open(iusc,file=wx,status='new',form='unformatted')                        !/*zs         16*/
      write(iusc)kod,ver,loddat,idtm,probid,aid,0                               !/*zs         17*/
      write(iusc)zero,zero                                                      !/*zs         18*/
!                                                                               !/*zs         19*/
!        open and read enough of rssa header for setdas information.            !/*zs         20*/
      if(nsr.ne.6)return                                                        !/*zs         21*/
   10 if(inqire(rssa).ne.0)go to 15                                             !/*cor4-2     93*/
      call erprnt(2,1,0,0,0,0,0,0,'26hfile '//rssa//' unavailable.')            !/*cor4-2     94*/
      return                                                                    !/*cor4-2     95*/
   15 open(iusr,file=rssa,status='old',form='unformatted')                      !/*cor4-2     96*/
      rewind iusr                                                               !/*zs         25*/
      read(iusr,end=170)kods,vers,lods,idtms,probs,aids,knods                   !/*zs         26*/
      if(knods.eq.0)call expire(0,'sfiles','rssa from wxxa, not wssa')          !/*cor4-2     97*/
      read(iusr,end=170)np1,nrss,nrcd,njsw,niss                                 !/*zs         27*/
      mipts=2                                                                   !/*zs         28*/
      if(np1.lt.0)read(iusr,end=170)niwr,mipts,kjaq                             !/*zs4c2       1*/
      np1=abs(np1)                                                              !/*zs         30*/
      if(nrcd.ne.6.and.nrcd.ne.10)nrcd=nrcd-1                                   !/*zs         31*/
      if(nrcd.ne.6.and.nrcd.ne.10)call expire(0,'sfiles',                       !/*zs         32*/
     & 'the record size is incorrect on '//rssa//' file.')                      !/*zs         33*/
      if(njsr(1).eq.0.and.nilr(1).eq.0)njsr(1)=njsw                             !/*zs4c        1*/
      if(njsr(1).eq.0.and.nilr(1).eq.0)nilr(1)=niwr                             !/*zs4c        2*/
      if(njsx(1).eq.0)njsx(1)=njsr(1)                                           !/*zs4c        3*/
      return                                                                    !/*zs         37*/
!                                                                               !/*zs         38*/
!        open surface-source files for a continue run.                          !/*zs         39*/
!        open wssa.  create the scratch file and write first 2 records.         !/*zs         40*/
   20 if(issw.eq.0)go to 130                                                    !/*zs         41*/
      if(inqire(wssa).eq.0)call expire(0,'sfiles',                              !/*zs         42*/
     & 'file '//wssa//' can not be opened.')                                    !/*zs         43*/
      open(iusw,file=wssa,status='old',form='unformatted')                      !/*zs         44*/
      rewind iusw                                                               !/*zs         45*/
      call unique(wx,jtty)                                                      !/*zs         46*/
      open(iusc,file=wx,status='new',form='unformatted')                        !/*zs         47*/
      write(iusc)kod,ver,loddat,idtm,probid,aid,0                               !/*zs         48*/
      write(iusc)zero,zero                                                      !/*zs         49*/
!                                                                               !/*zs         50*/
!        check that wssa is the correct file.                                   !/*zs         51*/
      read(iusw,end=160)hk,hv,hl,hi,hp,ha,kn                                    !/*zs         52*/
      if(hk//hv.ne.kod//ver)call expire(0,'sfiles','file '                      !/*zsxc        2*/
     & //wssa//' was written by '//hk//' version '//hv)                         !/*cor4-2    100*/
      if(hl.ne.loddat.and.mct.ge.0)write(iuo,30)wssa,hk,hv,hl                   !/*zs4a        1*/
   30 format(/6h file ,a8,16h was written by ,a8,9h version ,a5,                !/*zs         56*/
     & 11h loaded on ,a28)                                                      !/*zsx         8*/
      if(hp.ne.probid)call expire(0,'sfiles','files '//runtpe//                 !/*zs         58*/
     & ' and '//wssa//' were written by different problems.')                   !/*zs         59*/
      if(kn.ge.knod)go to 60                                                    !/*zs         60*/
      if(kn.gt.0)go to 40                                                       !/*zs         61*/
      read(iusw)id                                                              !/*zs4a        2*/
      go to 80                                                                  !/*zs         63*/
   40 write(iuo,50)knod,wssa,kn,kn,wssa                                         !/*zs         64*/
      write(jtty,50)knod,wssa,kn,kn,wssa                                        !/*zs         65*/
   50 format(46h this problem is to be continued from dump no.,i5/              !/*zs4a        3*/
     & 15h however, file ,a8,24h was written at dump no.,i5/                    !/*zs4a        4*/
     & 40h you will have to continue from dump no.,i5/                          !/*zs4a        5*/
     & 13h or your new ,a8,24h file will be incorrect.)                         !/*zs         69*/
      call expire(0,'sfiles','the dump numbers on '//                           !/*zs         70*/
     & runtpe//' and '//wssa//' files are inconsistent.')                       !/*zs         71*/
!                                                                               !/*zs         72*/
!        skip the rest of the header information.                               !/*zs         73*/
   60 do 70 j=1,njss+nilw+3                                                     !/*zs4a        6*/
   70 read(iusw,end=160)id                                                      !/*zs4a        7*/
!                                                                               !/*zs         76*/
!        copy records from wssa to the scratch file.                            !/*zs         77*/
   80 if(nsph.eq.1)go to 100                                                    !/*zs4b        2*/
      do 90 i=1,nrsw                                                            !/*zs         79*/
      read(iusw,end=160)rg                                                      !/*zs         80*/
   90 write(iusc)rg                                                             !/*zs         81*/
      go to 120                                                                 !/*zs         82*/
  100 do 110 i=1,nrsw                                                           !/*zs         83*/
      read(iusw,end=160)rs                                                      !/*zs         84*/
  110 write(iusc)rs                                                             !/*zs         85*/
  120 close(iusw,status='delete')                                               !/*cor4-2    101*/
!                                                                               !/*zs         88*/
!        open, check, and position rssa.                                        !/*zs         89*/
      if(nsr.ne.6)return                                                        !/*zs         90*/
  130 if(inqire(rssa).eq.0)call expire(0,'sfiles',                              !/*zs         91*/
     & 'file '//rssa//' can not be opened.')                                    !/*zs         92*/
      open(iusr,file=rssa,status='old',form='unformatted')                      !/*zs         93*/
      rewind iusr                                                               !/*zs         94*/
      read(iusr,end=170)hk,hv,hl,hi,hp,ha,kn                                    !/*zs         95*/
      if(hp.ne.probs.or.kn.ne.knods)call expire(0,'sfiles','file '//            !/*zs         96*/
     & rssa//' is not the same file that was used in the initial run.')         !/*zs         97*/
      read(iusr,end=170)i,j,n,njsw                                              !/*zs         98*/
      if(i.lt.0)read(iusr,end=170)niwr                                          !/*zs         99*/
      do 140 i=1,njsw+niwr+nrrs+1                                               !/*zs        100*/
  140 read(iusr,end=170)id                                                      !/*zs4a        8*/
      read(iusr,end=150)(ssb(i),i=1,nrcd)                                       !/*zs        102*/
  150 return                                                                    !/*zs        103*/
!                                                                               !/*zs        104*/
!        quit if unexpected eof is encountered.                                 !/*zs        105*/
  160 call expire(0,'sfiles','unexpected eof on '//wssa//' file.')              !/*zs        106*/
  170 call expire(0,'sfiles','unexpected eof on '//rssa//' file.')              !/*zs        107*/
      end                                                                       !/*zs        108*/
