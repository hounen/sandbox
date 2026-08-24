c#ident src/mcnpx/mcnpf/GLOBAL4_mb.F from deck mmp                            /*mmp         1*/
      module message_passing                                                    !/*mmp         2*/
!        module for message passing, including process creation,                !/*mmp         3*/
!        machine configuration and status, sending and receiving                !/*mmp         4*/
!        messages, and process termination.                                     !/*mmp         5*/
      use global_parameters, only : kindi,kindr,mcpu                            !/*mmp         6*/
      implicit none                                                             !/*mmp         7*/
      save                                                                      !/*mmp         8*/
      public                                                                    !/*mmp         9*/
#ifdef MULTP                                                                    /*!mmp        10*/
      integer(4), private :: idbuf                                              !/*mmp        11*/
      integer(4), private :: idtid                                              !/*mmp        12*/
      integer(4), private :: iflag                                              !/*mmp        13*/
      integer(4), private :: ihtid                                              !/*mmp        14*/
      integer(4), private :: imtid                                              !/*mmp        15*/
      integer(4), private :: info                                               !/*mmp        16*/
      integer(4), private :: iptid                                              !/*mmp        17*/
      integer(4), private :: ispeed                                             !/*mmp        18*/
      integer(4), private :: itid(mcpu)                                         !/*mmp        19*/
      integer(4), private :: narch                                              !/*mmp        20*/
      integer(4), private :: nhost                                              !/*mmp        21*/
      integer(4), private :: nitem                                              !/*mmp        22*/
      integer(4), private :: nmsg                                               !/*mmp        23*/
      integer(4), private :: nsub                                               !/*mmp        24*/
      integer(4), private :: ntask                                              !/*mmp        25*/
!                                                                               !/*mmp        26*/
      interface mputi                                                           !/*mmp        27*/
        module procedure mputi0                                                 !/*mmp        28*/
        module procedure mputi1                                                 !/*mmp        29*/
        module procedure mputi2                                                 !/*mmp        30*/
        module procedure mputi3                                                 !/*mmp        31*/
      end interface                                                             !/*mmp        32*/
      interface mputr                                                           !/*mmp        33*/
        module procedure mputr0                                                 !/*mmp        34*/
        module procedure mputr1                                                 !/*mmp        35*/
        module procedure mputr2                                                 !/*mmp        36*/
        module procedure mputr3                                                 !/*mmp        37*/
      end interface                                                             !/*mmp        38*/
      interface mputd                                                           !/*mmp        39*/
        module procedure mputd0                                                 !/*mmp        40*/
        module procedure mputd1                                                 !/*mmp        41*/
        module procedure mputd2                                                 !/*mmp        42*/
        module procedure mputd3                                                 !/*mmp        43*/
      end interface                                                             !/*mmp        44*/
      interface mgeti                                                           !/*mmp        45*/
        module procedure mgeti0                                                 !/*mmp        46*/
        module procedure mgeti1                                                 !/*mmp        47*/
        module procedure mgeti2                                                 !/*mmp        48*/
        module procedure mgeti3                                                 !/*mmp        49*/
      end interface                                                             !/*mmp        50*/
      interface mgetr                                                           !/*mmp        51*/
        module procedure mgetr0                                                 !/*mmp        52*/
        module procedure mgetr1                                                 !/*mmp        53*/
        module procedure mgetr2                                                 !/*mmp        54*/
        module procedure mgetr3                                                 !/*mmp        55*/
      end interface                                                             !/*mmp        56*/
      interface mgetd                                                           !/*mmp        57*/
        module procedure mgetd0                                                 !/*mmp        58*/
        module procedure mgetd1                                                 !/*mmp        59*/
        module procedure mgetd2                                                 !/*mmp        60*/
        module procedure mgetd3                                                 !/*mmp        61*/
      end interface                                                             !/*mmp        62*/
      contains                                                                  !/*mmp        63*/
!                                                                               !/*mmp        64*/
      subroutine menrl(mn)                                                      !/*mmp        65*/
!        perform function 'enroll task'.                                        !/*mmp        66*/
      integer(kindi), intent(out) :: mn                                         !/*mmp        67*/
      integer(kindi) :: i                                                       !/*mmp        68*/
#ifdef PVM                                                                      /*!mmp        69*/
!                                                                               !/*mmp        70*/
!        return if pvm is not initialized.                                      !/*mmp        71*/
      mn=-1                                                                     !/*mmp        72*/
      call pvmfmytid(imtid)                                                     !/*mmp        73*/
      if(imtid.lt.0)return                                                      !/*mmp        74*/
!                                                                               !/*mmp        75*/
!        return if this is the master task.                                     !/*mmp        76*/
      mn=0                                                                      !/*mmp        77*/
      itid(1)=imtid                                                             !/*mmp        78*/
      call pvmfparent(iptid)                                                    !/*mmp        79*/
      if(iptid.lt.0)return                                                      !/*mmp        80*/
!                                                                               !/*mmp        81*/
!        subtasks receive tid information and determine mynum.                  !/*mmp        82*/
      call pvmfrecv(-1_4,99999_4,idbuf)                                         !/*mmp        83*/
      call pvmfunpack(3_4,nsub,1_4,1_4,info)                                    !/*mmp        84*/
      call pvmfunpack(3_4,itid,nsub+1_4,1_4,info)                               !/*mmp        85*/
      if(nsub.le.1)go to 20                                                     !/*mmp        86*/
      do 10 i=1,nsub                                                            !/*mmp        87*/
      if(imtid.ne.itid(i+1))go to 10                                            !/*mmp        88*/
      mn=i                                                                      !/*mmp        89*/
      return                                                                    !/*mmp        90*/
   10 continue                                                                  !/*mmp        91*/
!                                                                               !/*mmp        92*/
!        abort if only one task or if my TID not found in list.                 !/*mmp        93*/
   20 call pvmfexit(info)                                                       !/*mmp        94*/
#endif                                                                          /*!mmp        95*/
      stop                                                                      !/*mmp        96*/
      end subroutine menrl                                                      !/*mmp        97*/
      subroutine mnump(np,in)                                                   !/*mmp        98*/
!        perform function 'get number of processors'.                           !/*mmp        99*/
      integer(kindi), intent(out) :: np                                         !/*mmp       100*/
      integer(kindi), intent(out) :: in                                         !/*mmp       101*/
#ifdef PVM                                                                      /*!mmp       102*/
      character hh*80,ha*8                                                      !/*mmp       103*/
      call pvmfconfig(nhost,narch,ihtid,hh,ha,ispeed,info)                      !/*mmp       104*/
      np=nhost                                                                  !/*mmp       105*/
      in=min(0,info)                                                            !/*mmp       106*/
#endif                                                                          /*!mmp       107*/
      return                                                                    !/*mmp       108*/
      end subroutine mnump                                                      !/*mmp       109*/
      subroutine minit(n,nt)                                                    !/*mmp       110*/
!        perform function 'initiate processes'.                                 !/*mmp       111*/
      integer(kindi), intent(in) :: n                                           !/*mmp       112*/
      integer(kindi), intent(inout) :: nt                                       !/*mmp       113*/
#ifdef PVM                                                                      /*!mmp       114*/
      character hh*80,ha*8                                                      !/*mmp       115*/
      integer(kindi) :: i                                                       !/*mmp       116*/
      integer(kindi) :: j                                                       !/*mmp       117*/
!                                                                               !/*mmp       118*/
!        spawn a subtask on each available host.                                !/*mmp       119*/
      j=nt                                                                      !/*mmp       120*/
      nt=0                                                                      !/*mmp       121*/
      call pvmftasks(itid(1),ntask,imtid,iptid,idtid,iflag,hh,info)             !/*mmp       122*/
      if(info.lt.0)return                                                       !/*mmp       123*/
!                                                                               !/*mmp       124*/
!        loop through the hosts - skip the master host if j=1.                  !/*mmp       125*/
      do 10 i=1,n                                                               !/*mmp       126*/
      call pvmfconfig(nhost,narch,ihtid,hh,ha,ispeed,info)                      !/*mmp       127*/
      if(idtid.eq.ihtid.and.j.eq.1)go to 10                                     !/*mmp       128*/
      call pvmfspawn('mcnpx.pvm',1_4,hh,1_4,itid(nt+2),info)                    !/*mmp       129*/
      if(info.le.0)go to 10                                                     !/*mmp       130*/
      nt=nt+1                                                                   !/*mmp       131*/
      if(nt.eq.n)go to 20                                                       !/*mmp       132*/
   10 continue                                                                  !/*mmp       133*/
   20 if(nt.eq.0)return                                                         !/*mmp       134*/
      nsub=nt                                                                   !/*mmp       135*/
!                                                                               !/*mmp       136*/
!        send subtasks tid information.                                         !/*mmp       137*/
      call pvmfinitsend(0_4,idbuf)                                              !/*mmp       138*/
      call pvmfpack(3_4,nsub,1_4,1_4,info)                                      !/*mmp       139*/
      call pvmfpack(3_4,itid,nsub+1_4,1_4,info)                                 !/*mmp       140*/
      call pvmfmcast(nsub,itid(2),99999_4,info)                                 !/*mmp       141*/
#endif                                                                          /*!mmp       142*/
      return                                                                    !/*mmp       143*/
      end subroutine minit                                                      !/*mmp       144*/
      subroutine mstat(mn,in)                                                   !/*mmp       145*/
!        perform function 'check process status'.                               !/*mmp       146*/
      integer(kindi), intent(in) :: mn                                          !/*mmp       147*/
      integer(kindi), intent(out) :: in                                         !/*mmp       148*/
#ifdef PVM                                                                      /*!mmp       149*/
      integer(kindi) :: i                                                       !/*mmp       150*/
      call pvmfpstat(itid(mn+1),info)                                           !/*mmp       151*/
      in=info                                                                   !/*mmp       152*/
      if(info.eq.0)return                                                       !/*mmp       153*/
      do 10 i=mn+1,nsub                                                         !/*mmp       154*/
   10 itid(i)=itid(i+1)                                                         !/*mmp       155*/
      nsub=nsub-1                                                               !/*mmp       156*/
#endif                                                                          /*!mmp       157*/
      return                                                                    !/*mmp       158*/
      end subroutine mstat                                                      !/*mmp       159*/
      subroutine msndi                                                          !/*mmp       160*/
!        perform function 'initialize send buffer'.                             !/*mmp       161*/
#ifdef PVM                                                                      /*!mmp       162*/
      call pvmfinitsend(0_4,idbuf)                                              !/*mmp       163*/
#endif                                                                          /*!mmp       164*/
      return                                                                    !/*mmp       165*/
      end subroutine msndi                                                      !/*mmp       166*/
      subroutine mputi0(iv,ml,in)                                               !/*mmp       167*/
!        perform function 'write integer to message buffer'.                    !/*mmp       168*/
      integer(kindi), intent(in) :: iv                                          !/*mmp       169*/
      integer(kindi), intent(in) :: ml                                          !/*mmp       170*/
      integer(kindi), intent(out) :: in                                         !/*mmp       171*/
!                                                                               !/*mmp       172*/
#ifdef PVM                                                                      /*!mmp       173*/
      if(ml.le.0)return                                                         !/*mmp       174*/
      nitem=kind(iv)*ml                                                         !/*mmp       175*/
      call pvmfpack(1_4,iv,nitem,1_4,info)                                      !/*mmp       176*/
!                                                                               !/*mmp       177*/
      in=info                                                                   !/*mmp       178*/
#endif                                                                          /*!mmp       179*/
      return                                                                    !/*mmp       180*/
      end subroutine mputi0                                                     !/*mmp       181*/
      subroutine mputi1(iv,ml,in)                                               !/*mmp       182*/
!        perform function 'write integer rank 1 to message buffer'.             !/*mmp       183*/
      integer(kindi), intent(in) :: iv(:)                                       !/*mmp       184*/
      integer(kindi), intent(in) :: ml                                          !/*mmp       185*/
      integer(kindi), intent(out) :: in                                         !/*mmp       186*/
!                                                                               !/*mmp       187*/
#ifdef PVM                                                                      /*!mmp       188*/
      if(ml.le.0)return                                                         !/*mmp       189*/
      nitem=kind(iv)*ml                                                         !/*mmp       190*/
      call pvmfpack(1_4,iv,nitem,1_4,info)                                      !/*mmp       191*/
!                                                                               !/*mmp       192*/
      in=info                                                                   !/*mmp       193*/
#endif                                                                          /*!mmp       194*/
      return                                                                    !/*mmp       195*/
      end subroutine mputi1                                                     !/*mmp       196*/
      subroutine mputi2(iv,ml,in)                                               !/*mmp       197*/
!        perform function 'write integer rank 2 to message buffer'.             !/*mmp       198*/
      integer(kindi), intent(in) :: iv(:,:)                                     !/*mmp       199*/
      integer(kindi), intent(in) :: ml                                          !/*mmp       200*/
      integer(kindi), intent(out) :: in                                         !/*mmp       201*/
!                                                                               !/*mmp       202*/
#ifdef PVM                                                                      /*!mmp       203*/
      if(ml.le.0)return                                                         !/*mmp       204*/
      nitem=kind(iv)*ml                                                         !/*mmp       205*/
      call pvmfpack(1_4,iv,nitem,1_4,info)                                      !/*mmp       206*/
!                                                                               !/*mmp       207*/
      in=info                                                                   !/*mmp       208*/
#endif                                                                          /*!mmp       209*/
      return                                                                    !/*mmp       210*/
      end subroutine mputi2                                                     !/*mmp       211*/
      subroutine mputi3(iv,ml,in)                                               !/*mmp       212*/
!        perform function 'write integer rank 3 to message buffer'.             !/*mmp       213*/
      integer(kindi), intent(in) :: iv(:,:,:)                                   !/*mmp       214*/
      integer(kindi), intent(in) :: ml                                          !/*mmp       215*/
      integer(kindi), intent(out) :: in                                         !/*mmp       216*/
!                                                                               !/*mmp       217*/
#ifdef PVM                                                                      /*!mmp       218*/
      if(ml.le.0)return                                                         !/*mmp       219*/
      nitem=kind(iv)*ml                                                         !/*mmp       220*/
      call pvmfpack(1_4,iv,nitem,1_4,info)                                      !/*mmp       221*/
!                                                                               !/*mmp       222*/
      in=info                                                                   !/*mmp       223*/
#endif                                                                          /*!mmp       224*/
      return                                                                    !/*mmp       225*/
      end subroutine mputi3                                                     !/*mmp       226*/
      subroutine mputc(hv,ml,in)                                                !/*mmp       227*/
!        perform function 'write character to message buffer'.                  !/*mmp       228*/
      integer(kindi), intent(in) :: ml                                          !/*mmp       229*/
      character(*), intent(in) :: hv                                            !/*mmp       230*/
      integer(kindi), intent(out) :: in                                         !/*mmp       231*/
!                                                                               !/*mmp       232*/
#ifdef PVM                                                                      /*!mmp       233*/
      if(ml.le.0)return                                                         !/*mmp       234*/
!        send as byte1 data.                                                    !/*mmp       235*/
      nitem=ml                                                                  !/*mmp       236*/
      call pvmfpack(1_4,hv,nitem,1_4,info)                                      !/*mmp       237*/
      in=info                                                                   !/*mmp       238*/
#endif                                                                          /*!mmp       239*/
      return                                                                    !/*mmp       240*/
      end subroutine mputc                                                      !/*mmp       241*/
      subroutine mputr0(rv,ml,in)                                               !/*mmp       242*/
!        perform function 'write real*4 to message buffer'.                     !/*mmp       243*/
      real(4), intent(in) :: rv                                                 !/*mmp       244*/
      integer(kindi), intent(in) :: ml                                          !/*mmp       245*/
      integer(kindi), intent(out) :: in                                         !/*mmp       246*/
!                                                                               !/*mmp       247*/
#ifdef PVM                                                                      /*!mmp       248*/
      if(ml.le.0)return                                                         !/*mmp       249*/
      nitem=ml                                                                  !/*mmp       250*/
      call pvmfpack(4_4,rv,nitem,1_4,info)                                      !/*mmp       251*/
      in=info                                                                   !/*mmp       252*/
#endif                                                                          /*!mmp       253*/
      return                                                                    !/*mmp       254*/
      end subroutine mputr0                                                     !/*mmp       255*/
      subroutine mputr1(rv,ml,in)                                               !/*mmp       256*/
!        perform function 'write real*4 rank 1 to message buffer'.              !/*mmp       257*/
      real(4), intent(in) :: rv(:)                                              !/*mmp       258*/
      integer(kindi), intent(in) :: ml                                          !/*mmp       259*/
      integer(kindi), intent(out) :: in                                         !/*mmp       260*/
!                                                                               !/*mmp       261*/
#ifdef PVM                                                                      /*!mmp       262*/
      if(ml.le.0)return                                                         !/*mmp       263*/
      nitem=ml                                                                  !/*mmp       264*/
      call pvmfpack(4_4,rv,nitem,1_4,info)                                      !/*mmp       265*/
      in=info                                                                   !/*mmp       266*/
#endif                                                                          /*!mmp       267*/
      return                                                                    !/*mmp       268*/
      end subroutine mputr1                                                     !/*mmp       269*/
      subroutine mputr2(rv,ml,in)                                               !/*mmp       270*/
!        perform function 'write real*4 rank 2 to message buffer'.              !/*mmp       271*/
      real(4), intent(in) :: rv(:,:)                                            !/*mmp       272*/
      integer(kindi), intent(in) :: ml                                          !/*mmp       273*/
      integer(kindi), intent(out) :: in                                         !/*mmp       274*/
!                                                                               !/*mmp       275*/
#ifdef PVM                                                                      /*!mmp       276*/
      if(ml.le.0)return                                                         !/*mmp       277*/
      nitem=ml                                                                  !/*mmp       278*/
      call pvmfpack(4_4,rv,nitem,1_4,info)                                      !/*mmp       279*/
      in=info                                                                   !/*mmp       280*/
#endif                                                                          /*!mmp       281*/
      return                                                                    !/*mmp       282*/
      end subroutine mputr2                                                     !/*mmp       283*/
      subroutine mputr3(rv,ml,in)                                               !/*mmp       284*/
!        perform function 'write real*4 rank 3 to message buffer'.              !/*mmp       285*/
      real(4), intent(in) :: rv(:,:,:)                                          !/*mmp       286*/
      integer(kindi), intent(in) :: ml                                          !/*mmp       287*/
      integer(kindi), intent(out) :: in                                         !/*mmp       288*/
!                                                                               !/*mmp       289*/
#ifdef PVM                                                                      /*!mmp       290*/
      if(ml.le.0)return                                                         !/*mmp       291*/
      nitem=ml                                                                  !/*mmp       292*/
      call pvmfpack(4_4,rv,nitem,1_4,info)                                      !/*mmp       293*/
      in=info                                                                   !/*mmp       294*/
#endif                                                                          /*!mmp       295*/
      return                                                                    !/*mmp       296*/
      end subroutine mputr3                                                     !/*mmp       297*/
      subroutine mputd0(dv,ml,in)                                               !/*mmp       298*/
!        perform function 'write real*8 to message buffer'.                     !/*mmp       299*/
      real(kindr), intent(in) :: dv                                             !/*mmp       300*/
      integer(kindi), intent(in) :: ml                                          !/*mmp       301*/
      integer(kindi), intent(out) :: in                                         !/*mmp       302*/
!                                                                               !/*mmp       303*/
#ifdef PVM                                                                      /*!mmp       304*/
      if(ml.le.0)return                                                         !/*mmp       305*/
      nitem=ml                                                                  !/*mmp       306*/
      call pvmfpack(6_4,dv,nitem,1_4,info)                                      !/*mmp       307*/
      in=info                                                                   !/*mmp       308*/
#endif                                                                          /*!mmp       309*/
      return                                                                    !/*mmp       310*/
      end subroutine mputd0                                                     !/*mmp       311*/
      subroutine mputd1(dv,ml,in)                                               !/*mmp       312*/
!        perform function 'write real*8 rank 1 to message buffer'.              !/*mmp       313*/
      real(kindr), intent(in) :: dv(:)                                          !/*mmp       314*/
      integer(kindi), intent(in) :: ml                                          !/*mmp       315*/
      integer(kindi), intent(out) :: in                                         !/*mmp       316*/
!                                                                               !/*mmp       317*/
#ifdef PVM                                                                      /*!mmp       318*/
      if(ml.le.0)return                                                         !/*mmp       319*/
      nitem=ml                                                                  !/*mmp       320*/
      call pvmfpack(6_4,dv,nitem,1_4,info)                                      !/*mmp       321*/
      in=info                                                                   !/*mmp       322*/
#endif                                                                          /*!mmp       323*/
      return                                                                    !/*mmp       324*/
      end subroutine mputd1                                                     !/*mmp       325*/
      subroutine mputd2(dv,ml,in)                                               !/*mmp       326*/
!        perform function 'write real*8 rank 2 to message buffer'.              !/*mmp       327*/
      real(kindr), intent(in) :: dv(:,:)                                        !/*mmp       328*/
      integer(kindi), intent(in) :: ml                                          !/*mmp       329*/
      integer(kindi), intent(out) :: in                                         !/*mmp       330*/
!                                                                               !/*mmp       331*/
#ifdef PVM                                                                      /*!mmp       332*/
      if(ml.le.0)return                                                         !/*mmp       333*/
      nitem=ml                                                                  !/*mmp       334*/
      call pvmfpack(6_4,dv,nitem,1_4,info)                                      !/*mmp       335*/
      in=info                                                                   !/*mmp       336*/
#endif                                                                          /*!mmp       337*/
      return                                                                    !/*mmp       338*/
      end subroutine mputd2                                                     !/*mmp       339*/
      subroutine mputd3(dv,ml,in)                                               !/*mmp       340*/
!        perform function 'write real*8 rank 3 to message buffer'.              !/*mmp       341*/
      real(kindr), intent(in) :: dv(:,:,:)                                      !/*mmp       342*/
      integer(kindi), intent(in) :: ml                                          !/*mmp       343*/
      integer(kindi), intent(out) :: in                                         !/*mmp       344*/
!                                                                               !/*mmp       345*/
#ifdef PVM                                                                      /*!mmp       346*/
      if(ml.le.0)return                                                         !/*mmp       347*/
      nitem=ml                                                                  !/*mmp       348*/
      call pvmfpack(6_4,dv,nitem,1_4,info)                                      !/*mmp       349*/
      in=info                                                                   !/*mmp       350*/
#endif                                                                          /*!mmp       351*/
      return                                                                    !/*mmp       352*/
      end subroutine mputd3                                                     !/*mmp       353*/
      subroutine msend(mn,mt,in)                                                !/*mmp       354*/
!        perform function 'send message'.                                       !/*mmp       355*/
      integer(kindi), intent(in) :: mn                                          !/*mmp       356*/
      integer(kindi), intent(in) :: mt                                          !/*mmp       357*/
      integer(kindi), intent(out) :: in                                         !/*mmp       358*/
!                                                                               !/*mmp       359*/
#ifdef PVM                                                                      /*!mmp       360*/
      integer(kindi) :: i                                                       !/*mmp       361*/
      integer(kindi) :: n                                                       !/*mmp       362*/
      nmsg=mt                                                                   !/*mmp       363*/
      if(mn.lt.0)call pvmfmcast(nsub,itid(2),nmsg,info)                         !/*mmp       364*/
      if(mn.ge.0)call pvmfsend(itid(mn+1),nmsg,info)                            !/*mmp       365*/
      in=info                                                                   !/*mmp       366*/
!                                                                               !/*mmp       367*/
!        wait for acknowledgement from tasks                                    !/*mmp       368*/
      if(mt.ne.4)return                                                         !/*mmp       369*/
      n=1                                                                       !/*mmp       370*/
      if(mn.lt.0)n=nsub                                                         !/*mmp       371*/
      do 10 i=1,n                                                               !/*mmp       372*/
   10 call pvmfrecv(-1_4,99999_4,info)                                          !/*mmp       373*/
#endif                                                                          /*!mmp       374*/
      return                                                                    !/*mmp       375*/
      end subroutine msend                                                      !/*mmp       376*/
      subroutine mprob(mn,mt,in)                                                !/*mmp       377*/
!        perform function 'check message arrival'.                              !/*mmp       378*/
      integer(kindi), intent(in) :: mn                                          !/*mmp       379*/
      integer(kindi), intent(in) :: mt                                          !/*mmp       380*/
      integer(kindi), intent(out) :: in                                         !/*mmp       381*/
!                                                                               !/*mmp       382*/
#ifdef PVM                                                                      /*!mmp       383*/
      idtid=mn                                                                  !/*mmp       384*/
      nmsg=mt                                                                   !/*mmp       385*/
      call pvmfprobe(idtid,nmsg,idbuf)                                          !/*mmp       386*/
      in=idbuf                                                                  !/*mmp       387*/
#endif                                                                          /*!mmp       388*/
      return                                                                    !/*mmp       389*/
      end subroutine mprob                                                      !/*mmp       390*/
      subroutine mrecb(ib,in)                                                   !/*mmp       391*/
!        perform function 'set receive message buffer'.                         !/*mmp       392*/
      integer(kindi), intent(in) :: ib                                          !/*mmp       393*/
      integer(kindi), intent(out) :: in                                         !/*mmp       394*/
!                                                                               !/*mmp       395*/
#ifdef PVM                                                                      /*!mmp       396*/
      idbuf=ib                                                                  !/*mmp       397*/
      call pvmfsetrbuf(idbuf,info)                                              !/*mmp       398*/
      in=info                                                                   !/*mmp       399*/
#endif                                                                          /*!mmp       400*/
      return                                                                    !/*mmp       401*/
      end subroutine mrecb                                                      !/*mmp       402*/
      subroutine mfbuf(ib,in)                                                   !/*mmp       403*/
!        perform function 'free message buffer'.                                !/*mmp       404*/
      integer(kindi), intent(in) :: ib                                          !/*mmp       405*/
      integer(kindi), intent(out) :: in                                         !/*mmp       406*/
!                                                                               !/*mmp       407*/
#ifdef PVM                                                                      /*!mmp       408*/
      idbuf=ib                                                                  !/*mmp       409*/
      call pvmffreebuf(idbuf,info)                                              !/*mmp       410*/
      in=info                                                                   !/*mmp       411*/
#endif                                                                          /*!mmp       412*/
      return                                                                    !/*mmp       413*/
      end subroutine mfbuf                                                      !/*mmp       414*/
      subroutine mrecv(nh,mt,in)                                                !/*mmp       415*/
!        perform function 'receive message'.                                    !/*mmp       416*/
      integer(kindi), intent(in) :: nh                                          !/*mmp       417*/
      integer(kindi), intent(in) :: mt                                          !/*mmp       418*/
      integer(kindi), intent(out) :: in                                         !/*mmp       419*/
!                                                                               !/*mmp       420*/
#ifdef PVM                                                                      /*!mmp       421*/
      nmsg=mt                                                                   !/*mmp       422*/
      idtid=nh                                                                  !/*mmp       423*/
      if(nh.gt.0)idtid=itid(nh+1)                                               !/*mmp       424*/
      call pvmfrecv(idtid,nmsg,idbuf)                                           !/*mmp       425*/
      in=idbuf                                                                  !/*mmp       426*/
!                                                                               !/*mmp       427*/
!        send acknowledgement                                                   !/*mmp       428*/
      if(mt.ne.4)return                                                         !/*mmp       429*/
      call pvmfbufinfo(idbuf,nitem,nmsg,idtid,info)                             !/*mmp       430*/
      call pvmfinitsend(0_4,info)                                               !/*mmp       431*/
      call pvmfsend(idtid,99999_4,info)                                         !/*mmp       432*/
#endif                                                                          /*!mmp       433*/
      return                                                                    !/*mmp       434*/
      end subroutine mrecv                                                      !/*mmp       435*/
      subroutine mrcvi(nt)                                                      !/*mmp       436*/
!        perform function 'receive message information'.                        !/*mmp       437*/
      integer(kindi), intent(out) :: nt                                         !/*mmp       438*/
!                                                                               !/*mmp       439*/
#ifdef PVM                                                                      /*!mmp       440*/
      call pvmfbufinfo(idbuf,nitem,nmsg,idtid,info)                             !/*mmp       441*/
      do 10 nt=1,nsub                                                           !/*mmp       442*/
   10 if(idtid.eq.itid(nt+1))return                                             !/*mmp       443*/
#endif                                                                          /*!mmp       444*/
      return                                                                    !/*mmp       445*/
      end subroutine mrcvi                                                      !/*mmp       446*/
      subroutine mgeti0(iv,ml,in)                                               !/*mmp       447*/
!        perform function 'read integer from message buffer'.                   !/*mmp       448*/
      integer(kindi), intent(out) :: iv                                         !/*mmp       449*/
      integer(kindi), intent(in) :: ml                                          !/*mmp       450*/
      integer(kindi), intent(out) :: in                                         !/*mmp       451*/
!                                                                               !/*mmp       452*/
#ifdef PVM                                                                      /*!mmp       453*/
      if(ml.le.0)return                                                         !/*mmp       454*/
      nitem=kind(iv)*ml                                                         !/*mmp       455*/
      call pvmfunpack(1_4,iv,nitem,1_4,info)                                    !/*mmp       456*/
      in=info                                                                   !/*mmp       457*/
#endif                                                                          /*!mmp       458*/
      return                                                                    !/*mmp       459*/
      end subroutine mgeti0                                                     !/*mmp       460*/
      subroutine mgeti1(iv,ml,in)                                               !/*mmp       461*/
!        perform function 'read integer rank 1 from message buffer'.            !/*mmp       462*/
      integer(kindi), intent(out) :: iv(:)                                      !/*mmp       463*/
      integer(kindi), intent(in) :: ml                                          !/*mmp       464*/
      integer(kindi), intent(out) :: in                                         !/*mmp       465*/
!                                                                               !/*mmp       466*/
#ifdef PVM                                                                      /*!mmp       467*/
      if(ml.le.0)return                                                         !/*mmp       468*/
      nitem=kind(iv)*ml                                                         !/*mmp       469*/
      call pvmfunpack(1_4,iv,nitem,1_4,info)                                    !/*mmp       470*/
      in=info                                                                   !/*mmp       471*/
#endif                                                                          /*!mmp       472*/
      return                                                                    !/*mmp       473*/
      end subroutine mgeti1                                                     !/*mmp       474*/
      subroutine mgeti2(iv,ml,in)                                               !/*mmp       475*/
!        perform function 'read integer rank 2 from message buffer'.            !/*mmp       476*/
      integer(kindi), intent(out) :: iv(:,:)                                    !/*mmp       477*/
      integer(kindi), intent(in) :: ml                                          !/*mmp       478*/
      integer(kindi), intent(out) :: in                                         !/*mmp       479*/
!                                                                               !/*mmp       480*/
#ifdef PVM                                                                      /*!mmp       481*/
      if(ml.le.0)return                                                         !/*mmp       482*/
      nitem=kind(iv)*ml                                                         !/*mmp       483*/
      call pvmfunpack(1_4,iv,nitem,1_4,info)                                    !/*mmp       484*/
      in=info                                                                   !/*mmp       485*/
#endif                                                                          /*!mmp       486*/
      return                                                                    !/*mmp       487*/
      end subroutine mgeti2                                                     !/*mmp       488*/
      subroutine mgeti3(iv,ml,in)                                               !/*mmp       489*/
!        perform function 'read integer rank 3 from message buffer'.            !/*mmp       490*/
      integer(kindi), intent(out) :: iv(:,:,:)                                  !/*mmp       491*/
      integer(kindi), intent(in) :: ml                                          !/*mmp       492*/
      integer(kindi), intent(out) :: in                                         !/*mmp       493*/
!                                                                               !/*mmp       494*/
#ifdef PVM                                                                      /*!mmp       495*/
      if(ml.le.0)return                                                         !/*mmp       496*/
      nitem=kind(iv)*ml                                                         !/*mmp       497*/
      call pvmfunpack(1_4,iv,nitem,1_4,info)                                    !/*mmp       498*/
      in=info                                                                   !/*mmp       499*/
#endif                                                                          /*!mmp       500*/
      return                                                                    !/*mmp       501*/
      end subroutine mgeti3                                                     !/*mmp       502*/
      subroutine mgetc(hv,ml,in)                                                !/*mmp       503*/
!        perform function 'read character from message buffer'.                 !/*mmp       504*/
      character(*), intent(out) :: hv                                           !/*mmp       505*/
      integer(kindi), intent(in) :: ml                                          !/*mmp       506*/
      integer(kindi), intent(out) :: in                                         !/*mmp       507*/
!                                                                               !/*mmp       508*/
#ifdef PVM                                                                      /*!mmp       509*/
      if(ml.le.0)return                                                         !/*mmp       510*/
!        receive as byte1 data.                                                 !/*mmp       511*/
      nitem=ml                                                                  !/*mmp       512*/
      call pvmfunpack(1_4,hv,nitem,1_4,info)                                    !/*mmp       513*/
      in=info                                                                   !/*mmp       514*/
#endif                                                                          /*!mmp       515*/
      return                                                                    !/*mmp       516*/
      end subroutine mgetc                                                      !/*mmp       517*/
      subroutine mgetr0(rv,ml,in)                                               !/*mmp       518*/
!        perform function 'read real*4 from message buffer'.                    !/*mmp       519*/
      real(4), intent(out) :: rv                                                !/*mmp       520*/
      integer(kindi), intent(in) :: ml                                          !/*mmp       521*/
      integer(kindi), intent(out) :: in                                         !/*mmp       522*/
!                                                                               !/*mmp       523*/
#ifdef PVM                                                                      /*!mmp       524*/
      if(ml.le.0)return                                                         !/*mmp       525*/
      nitem=ml                                                                  !/*mmp       526*/
      call pvmfunpack(4_4,rv,nitem,1_4,info)                                    !/*mmp       527*/
      in=info                                                                   !/*mmp       528*/
#endif                                                                          /*!mmp       529*/
      return                                                                    !/*mmp       530*/
      end subroutine mgetr0                                                     !/*mmp       531*/
      subroutine mgetr1(rv,ml,in)                                               !/*mmp       532*/
!        perform function 'read real*4 rank 1 from message buffer'.             !/*mmp       533*/
      real(4), intent(out) :: rv(:)                                             !/*mmp       534*/
      integer(kindi), intent(in) :: ml                                          !/*mmp       535*/
      integer(kindi), intent(out) :: in                                         !/*mmp       536*/
!                                                                               !/*mmp       537*/
#ifdef PVM                                                                      /*!mmp       538*/
      if(ml.le.0)return                                                         !/*mmp       539*/
      nitem=ml                                                                  !/*mmp       540*/
      call pvmfunpack(4_4,rv,nitem,1_4,info)                                    !/*mmp       541*/
      in=info                                                                   !/*mmp       542*/
#endif                                                                          /*!mmp       543*/
      return                                                                    !/*mmp       544*/
      end subroutine mgetr1                                                     !/*mmp       545*/
      subroutine mgetr2(rv,ml,in)                                               !/*mmp       546*/
!        perform function 'read real*4 rank 2 from message buffer'.             !/*mmp       547*/
      real(4), intent(out) :: rv(:,:)                                           !/*mmp       548*/
      integer(kindi), intent(in) :: ml                                          !/*mmp       549*/
      integer(kindi), intent(out) :: in                                         !/*mmp       550*/
!                                                                               !/*mmp       551*/
#ifdef PVM                                                                      /*!mmp       552*/
      if(ml.le.0)return                                                         !/*mmp       553*/
      nitem=ml                                                                  !/*mmp       554*/
      call pvmfunpack(4_4,rv,nitem,1_4,info)                                    !/*mmp       555*/
      in=info                                                                   !/*mmp       556*/
#endif                                                                          /*!mmp       557*/
      return                                                                    !/*mmp       558*/
      end subroutine mgetr2                                                     !/*mmp       559*/
      subroutine mgetr3(rv,ml,in)                                               !/*mmp       560*/
!        perform function 'read real*4 rank 3 from message buffer'.             !/*mmp       561*/
      real(4), intent(out) :: rv(:,:,:)                                         !/*mmp       562*/
      integer(kindi), intent(in) :: ml                                          !/*mmp       563*/
      integer(kindi), intent(out) :: in                                         !/*mmp       564*/
!                                                                               !/*mmp       565*/
#ifdef PVM                                                                      /*!mmp       566*/
      if(ml.le.0)return                                                         !/*mmp       567*/
      nitem=ml                                                                  !/*mmp       568*/
      call pvmfunpack(4_4,rv,nitem,1_4,info)                                    !/*mmp       569*/
      in=info                                                                   !/*mmp       570*/
#endif                                                                          /*!mmp       571*/
      return                                                                    !/*mmp       572*/
      end subroutine mgetr3                                                     !/*mmp       573*/
      subroutine mgetd0(dv,ml,in)                                               !/*mmp       574*/
!        perform function 'read real*8 from message buffer'.                    !/*mmp       575*/
      real(kindr), intent(out) :: dv                                            !/*mmp       576*/
      integer(kindi), intent(in) :: ml                                          !/*mmp       577*/
      integer(kindi), intent(out) :: in                                         !/*mmp       578*/
!                                                                               !/*mmp       579*/
#ifdef PVM                                                                      /*!mmp       580*/
      if(ml.le.0)return                                                         !/*mmp       581*/
      nitem=ml                                                                  !/*mmp       582*/
      call pvmfunpack(6_4,dv,nitem,1_4,info)                                    !/*mmp       583*/
      in=info                                                                   !/*mmp       584*/
#endif                                                                          /*!mmp       585*/
      return                                                                    !/*mmp       586*/
      end subroutine mgetd0                                                     !/*mmp       587*/
      subroutine mgetd1(dv,ml,in)                                               !/*mmp       588*/
!        perform function 'read real*8 rank 1 from message buffer'.             !/*mmp       589*/
      real(kindr), intent(out) :: dv(:)                                         !/*mmp       590*/
      integer(kindi), intent(in) :: ml                                          !/*mmp       591*/
      integer(kindi), intent(out) :: in                                         !/*mmp       592*/
!                                                                               !/*mmp       593*/
#ifdef PVM                                                                      /*!mmp       594*/
      if(ml.le.0)return                                                         !/*mmp       595*/
      nitem=ml                                                                  !/*mmp       596*/
      call pvmfunpack(6_4,dv,nitem,1_4,info)                                    !/*mmp       597*/
      in=info                                                                   !/*mmp       598*/
#endif                                                                          /*!mmp       599*/
      return                                                                    !/*mmp       600*/
      end subroutine mgetd1                                                     !/*mmp       601*/
      subroutine mgetd2(dv,ml,in)                                               !/*mmp       602*/
!        perform function 'read real*8 rank 2 from message buffer'.             !/*mmp       603*/
      real(kindr), intent(out) :: dv(:,:)                                       !/*mmp       604*/
      integer(kindi), intent(in) :: ml                                          !/*mmp       605*/
      integer(kindi), intent(out) :: in                                         !/*mmp       606*/
!                                                                               !/*mmp       607*/
#ifdef PVM                                                                      /*!mmp       608*/
      if(ml.le.0)return                                                         !/*mmp       609*/
      nitem=ml                                                                  !/*mmp       610*/
      call pvmfunpack(6_4,dv,nitem,1_4,info)                                    !/*mmp       611*/
      in=info                                                                   !/*mmp       612*/
#endif                                                                          /*!mmp       613*/
      return                                                                    !/*mmp       614*/
      end subroutine mgetd2                                                     !/*mmp       615*/
      subroutine mgetd3(dv,ml,in)                                               !/*mmp       616*/
!        perform function 'read real*8 rank 3 from message buffer'.             !/*mmp       617*/
      real(kindr), intent(out) :: dv(:,:,:)                                     !/*mmp       618*/
      integer(kindi), intent(in) :: ml                                          !/*mmp       619*/
      integer(kindi), intent(out) :: in                                         !/*mmp       620*/
!                                                                               !/*mmp       621*/
#ifdef PVM                                                                      /*!mmp       622*/
      if(ml.le.0)return                                                         !/*mmp       623*/
      nitem=ml                                                                  !/*mmp       624*/
      call pvmfunpack(6_4,dv,nitem,1_4,info)                                    !/*mmp       625*/
      in=info                                                                   !/*mmp       626*/
#endif                                                                          /*!mmp       627*/
      return                                                                    !/*mmp       628*/
      end subroutine mgetd3                                                     !/*mmp       629*/
      subroutine mkill                                                          !/*mmp       630*/
!        perform function 'kill subtasks'.                                      !/*mmp       631*/
!                                                                               !/*mmp       632*/
#ifdef PVM                                                                      /*!mmp       633*/
      integer(kindi) :: i                                                       !/*mmp       634*/
      do 10 i=1,nsub                                                            !/*mmp       635*/
   10 call pvmfkill(itid(i+1),info)                                             !/*mmp       636*/
#endif                                                                          /*!mmp       637*/
      return                                                                    !/*mmp       638*/
      end subroutine mkill                                                      !/*mmp       639*/
      subroutine mterm                                                          !/*mmp       640*/
!        perform function 'exit communication software'.                        !/*mmp       641*/
!                                                                               !/*mmp       642*/
#ifdef PVM                                                                      /*!mmp       643*/
      call pvmfexit(info)                                                       !/*mmp       644*/
#endif                                                                          /*!mmp       645*/
      return                                                                    !/*mmp       646*/
      end subroutine mterm                                                      !/*mmp       647*/
#endif                                                                          /*!mmp       648*/
!                                                                               !/*mmp       649*/
      end module message_passing                                                !/*mmp       650*/
