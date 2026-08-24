c#ident src/mcnpx/mcnpf/GLOBAL6_ht.F from deck mht                            /*mht         1*/
      module histp                                                              !/*mht         2*/
!        module for message passing routines needed for histp.                  !/*mht         3*/
      save                                                                      !/*mht         4*/
      public                                                                    !/*mht         5*/
#if defined(MULTP) && defined(HISTP)                                            /*!mht         6*/
      contains                                                                  !/*mht         7*/
!                                                                               !/*mht         8*/
!                                                                               !/*mht         9*/
      subroutine ins_histp_d(mynum)                                             !/*mht        10*/
!        initializes the histp file for daughter processes.                     !/*mht        11*/
!                                                                               !/*mht        12*/
#include <lcs/double.h>                                                         /*!mht        13*/
#include <lcs/lptS.h>                                                           /*!mht        14*/
#include <lcs/concom.h>                                                         /*!mht        15*/
#include <lcs/charla.h>                                                         /*!mht        16*/
#include <lcs/inout.h>                                                          /*!mht        17*/
!                                                                               !/*mht        18*/
      nhstp=55                                                                  !/*mht        19*/
#ifdef MHISTP                                                                   /*!mht        20*/
      write(histp,'(i3.3,a5)')mynum,'histp'                                     !/*mht        21*/
      if (inqire(histp).ne.0) then                                              !/*mht        22*/
        call unique(histp,io)                                                   !/*mht        23*/
      endif                                                                     !/*mht        24*/
      open(nhstp,file=histp,status='new',form='unformatted')                    !/*mht        25*/
#else                                                                           /*!mht        26*/
      open(nhstp,status='scratch',form='unformatted')                           !/*mht        27*/
#endif                                                                          /*!mht        28*/
      rewind nhstp                                                              !/*mht        29*/
      return                                                                    !/*mht        30*/
      end subroutine ins_histp_d                                                !/*mht        31*/
!                                                                               !/*mht        32*/
!                                                                               !/*mht        33*/
      subroutine del_histp_d                                                    !/*mht        34*/
!        deletes the histp file before terminating daughter processes.          !/*mht        35*/
!                                                                               !/*mht        36*/
#include <lcs/double.h>                                                         /*!mht        37*/
#include <lcs/lptS.h>                                                           /*!mht        38*/
#include <lcs/concom.h>                                                         /*!mht        39*/
!                                                                               !/*mht        40*/
#ifdef MHISTP                                                                   /*!mht        41*/
      if (nhstp.gt.0) close (nhstp,status='delete')                             !/*mht        42*/
#endif                                                                          /*!mht        43*/
      return                                                                    !/*mht        44*/
      end subroutine del_histp_d                                                !/*mht        45*/
!                                                                               !/*mht        46*/
!                                                                               !/*mht        47*/
      subroutine pack_histp(icnt)                                               !/*mht        48*/
!        pack messages with histp data.                                         !/*mht        49*/
      use message_passing                                                       !/*mht        50*/
#include <lcs/double.h>                                                         /*!mht        51*/
#include <lcs/lptS.h>                                                           /*!mht        52*/
#include <lcs/bankb.h>                                                          /*!mht        53*/
#include <lcs/concom.h>                                                         /*!mht        54*/
#include <lcs/rucom.h>                                                          /*!mht        55*/
#include <lcs/charla.h>                                                         /*!mht        56*/
#ifdef MHISTP                                                                   /*!mht        57*/
      character hx*8, hy*8                                                      !/*mht        58*/
#endif                                                                          /*!mht        59*/
!                                                                               !/*mht        60*/
      real q1,q2,q3                                                             !/*mht        61*/
      dimension iq1(29),iq3(l02), q1(29),q2(8),q3(7,l02)                        !/*mht        62*/
      equivalence (iq1,q1)                                                      !/*mht        63*/
      if (nhstp.eq.0) return                                                    !/*mht        64*/
!                                                                               !/*mht        65*/
!        send and reset counters to be updated.                                 !/*mht        66*/
      call msndi                                                                !/*mht        67*/
      call mputd(exlow,5,i)                                                     !/*mht        68*/
      exlow(5)=dp0                                                              !/*mht        69*/
      call mputi(nrnorm,4,i)                                                    !/*mht        70*/
      do 10 j=1,4                                                               !/*mht        71*/
         nrnorm(j)=0                                                            !/*mht        72*/
 10   continue                                                                  !/*mht        73*/
      call mputi(negex,1,i)                                                     !/*mht        74*/
      negex=0                                                                   !/*mht        75*/
      call mputi(negaz,1,i)                                                     !/*mht        76*/
      negaz=0                                                                   !/*mht        77*/
      call mputi(lowaz,1,i)                                                     !/*mht        78*/
      lowaz=0                                                                   !/*mht        79*/
      call mputi(ifisct,1,i)                                                    !/*mht        80*/
      ifisct=0                                                                  !/*mht        81*/
      call msend(0,4,i)                                                         !/*mht        82*/
!                                                                               !/*mht        83*/
!        initialize transfer.                                                   !/*mht        84*/
#ifdef MHISTP                                                                   /*!mht        85*/
      close (nhstp)                                                             !/*mht        86*/
      open(nhstp,file=histp,status='old',form='unformatted')                    !/*mht        87*/
      hx=histp                                                                  !/*mht        88*/
#endif                                                                          /*!mht        89*/
      rewind nhstp                                                              !/*mht        90*/
      call msndi                                                                !/*mht        91*/
!                                                                               !/*mht        92*/
!        read the histp file information.                                       !/*mht        93*/
      mmcol=-2                                                                  !/*mht        94*/
 100  read(nhstp,END=999,ERR=997) iq1                                           !/*mht        95*/
#ifdef MHISTP                                                                   /*!mht        96*/
      if (iq1(1).eq.mmcol) then                                                 !/*mht        97*/
         backspace (nhstp)                                                      !/*mht        98*/
         hy=hx                                                                  !/*mht        99*/
         read(nhstp) i1,hx                                                      !/*mht       100*/
!                                                                               !/*mht       101*/
!        delete all but the first histp file.                                   !/*mht       102*/
         if (hy.eq.histp) then                                                  !/*mht       103*/
            close (nhstp)                                                       !/*mht       104*/
         else                                                                   !/*mht       105*/
            close (nhstp,status='delete')                                       !/*mht       106*/
         endif                                                                  !/*mht       107*/
         open(nhstp,file=hx,status='old',form='unformatted')                    !/*mht       108*/
         go to 100                                                              !/*mht       109*/
      endif                                                                     !/*mht       110*/
#endif                                                                          /*!mht       111*/
!                                                                               !/*mht       112*/
!        end of run: not to appear in daughter runs.                            !/*mht       113*/
!      if(iqq(1).eq.-4) read(nhstp,END=999,ERR=997) wtsum,nosum                 !/*mht       114*/
!                                                                               !/*mht       115*/
!        regular information.                                                   !/*mht       116*/
      call mputi(iq1(1),7,i)                                                    !/*mht       117*/
      call mputr(q1(8),15,i)                                                    !/*mht       118*/
      call mputi(iq1(23),5,i)                                                   !/*mht       119*/
      call mputr(q1(28),2,i)                                                    !/*mht       120*/
!                                                                               !/*mht       121*/
!        residual nuclei records.                                               !/*mht       122*/
      if (iq1(27).gt.0) then                                                    !/*mht       123*/
         do 20 j=1,iq1(27)                                                      !/*mht       124*/
            read(nhstp,END=998,ERR=997) q2                                      !/*mht       125*/
            call mputr(q2,8,i)                                                  !/*mht       126*/
 20      continue                                                               !/*mht       127*/
      endif                                                                     !/*mht       128*/
!                                                                               !/*mht       129*/
!        secondary particle records.                                            !/*mht       130*/
      if (iq1(25).gt.0) then                                                    !/*mht       131*/
         read(nhstp,END=998,ERR=997) (iq3(j),(q3(k,j),k=1,7),                   !/*mht       132*/
     &                                j=1,iq1(25))                              !/*mht       133*/
         call mputi(iq3,iq1(25),i)                                              !/*mht       134*/
         call mputr(q3,7*iq1(25),i)                                             !/*mht       135*/
      endif                                                                     !/*mht       136*/
      if (iq1(26).gt.0) then                                                    !/*mht       137*/
         read(nhstp,END=998,ERR=997) ((q3(k,j),k=1,7),j=1,iq1(26))              !/*mht       138*/
         call mputr(q3,7*iq1(26),i)                                             !/*mht       139*/
      endif                                                                     !/*mht       140*/
      icnt=icnt+1                                                               !/*mht       141*/
      goto 100                                                                  !/*mht       142*/
!                                                                               !/*mht       143*/
!        error in record length.                                                !/*mht       144*/
 997  call expirx(0,'msgtsk','record length error on histp file.')              !/*mht       145*/
      return                                                                    !/*mht       146*/
!                                                                               !/*mht       147*/
!        unexpected end of file.                                                !/*mht       148*/
 998  call expirx(0,'msgtsk','unexpected eof on histp file.')                   !/*mht       149*/
      return                                                                    !/*mht       150*/
!                                                                               !/*mht       151*/
!        regular end of file.                                                   !/*mht       152*/
 999  info=icnt                                                                 !/*mht       153*/
      iq1(1)=-99999                                                             !/*mht       154*/
      iq1(2)=icnt                                                               !/*mht       155*/
      call mputi(iq1(1),7,i)                                                    !/*mht       156*/
      call msend(0,4,i)                                                         !/*mht       157*/
#ifdef MHISTP                                                                   /*!mht       158*/
      if (hx.eq.histp) then                                                     !/*mht       159*/
         ntapes=0                                                               !/*mht       160*/
      else                                                                      !/*mht       161*/
         close (nhstp,status='delete')                                          !/*mht       162*/
         open(nhstp,file=histp,status='old',form='unformatted')                 !/*mht       163*/
      endif                                                                     !/*mht       164*/
#endif                                                                          /*!mht       165*/
      rewind nhstp                                                              !/*mht       166*/
      return                                                                    !/*mht       167*/
      end subroutine pack_histp                                                 !/*mht       168*/
!                                                                               !/*mht       169*/
!                                                                               !/*mht       170*/
      subroutine unpack_histp(nh,icnt,isent)                                    !/*mht       171*/
!        unpack messages with histp data.                                       !/*mht       172*/
      use message_passing                                                       !/*mht       173*/
!                                                                               !/*mht       174*/
#include <lcs/double.h>                                                         /*!mht       175*/
#include <lcs/lptS.h>                                                           /*!mht       176*/
#include <lcs/bankb.h>                                                          /*!mht       177*/
#include <lcs/concom.h>                                                         /*!mht       178*/
#include <lcs/rucom.h>                                                          /*!mht       179*/
#include <lcs/charla.h>                                                         /*!mht       180*/
#include <lcs/inout.h>                                                          /*!mht       181*/
!                                                                               !/*mht       182*/
      real q1,q2,q3,qq                                                          !/*mht       183*/
      dimension iq1(29),iq3(l02), q1(29),q2(8),q3(7,l02), dq(5), qq(28)         !/*mht       184*/
      equivalence (iq1,q1)                                                      !/*mht       185*/
      data llq /29/, margin /4096/                                              !/*mht       186*/
      data qq /28*0.0/                                                          !/*mht       187*/
!                                                                               !/*mht       188*/
      isent=0                                                                   !/*mht       189*/
      icnt=0                                                                    !/*mht       190*/
      if (nhstp.eq.0) return                                                    !/*mht       191*/
!                                                                               !/*mht       192*/
!        receive and update counters.                                           !/*mht       193*/
      call mrecv(nh,4,i)                                                        !/*mht       194*/
      call mgetd(dq(1),5,i)                                                     !/*mht       195*/
      exlow(1)=min(dq(1),exlow(1))                                              !/*mht       196*/
      exlow(2)=min(dq(2),exlow(2))                                              !/*mht       197*/
      exlow(3)=min(dq(3),exlow(3))                                              !/*mht       198*/
      exlow(4)=max(dq(4),exlow(4))                                              !/*mht       199*/
      exlow(5)=exlow(5)+dq(5)                                                   !/*mht       200*/
      call mgeti(iq1(1),4,i)                                                    !/*mht       201*/
      do 10 j=1,4                                                               !/*mht       202*/
         nrnorm(j)=nrnorm(j)+iq1(j)                                             !/*mht       203*/
 10   continue                                                                  !/*mht       204*/
      call mgeti(iq1(1),1,i)                                                    !/*mht       205*/
      negex=negex+iq1(1)                                                        !/*mht       206*/
      call mgeti(iq1(1),1,i)                                                    !/*mht       207*/
      negaz=negaz+iq1(1)                                                        !/*mht       208*/
      call mgeti(iq1(1),1,i)                                                    !/*mht       209*/
      lowaz=lowaz+iq1(1)                                                        !/*mht       210*/
      call mgeti(iq1(1),1,i)                                                    !/*mht       211*/
      ifisct=ifisct+iq1(1)                                                      !/*mht       212*/
!                                                                               !/*mht       213*/
!        loop back here until ncol=-99999 received.                             !/*mht       214*/
      call mrecv(nh,4,i)                                                        !/*mht       215*/
 100  continue                                                                  !/*mht       216*/
!                                                                               !/*mht       217*/
!        regular information.                                                   !/*mht       218*/
#ifdef MHISTP                                                                   /*!mht       219*/
      if (nwrds.ge.(lhist-margin)) then                                         !/*mht       220*/
         if (histp.eq.'histp') histp='histpa'                                   !/*mht       221*/
         call unique (histp,itty)                                               !/*mht       222*/
         mmcol=-2                                                               !/*mht       223*/
#ifdef CHEAP                                                                    /*!mht       224*/
         write (nhstp) mmcol,histp,(qq(i),i=1,26)                               !/*mht       225*/
#else                                                                           /*!mht       226*/
         write (nhstp) mmcol,histp,(qq(i),i=1,27)                               !/*mht       227*/
#endif                                                                          /*!mht       228*/
         nwrds=1+nwrds+llq                                                      !/*mht       229*/
         close (nhstp)                                                          !/*mht       230*/
         open(nhstp,file=histp,status='new',access='sequential',                !/*mht       231*/
     &        form='unformatted')                                               !/*mht       232*/
         write (itty,15) histp                                                  !/*mht       233*/
         write (io,15) histp                                                    !/*mht       234*/
 15      format (/5x,'history tape is file',1x,a10/)                            !/*mht       235*/
         ntapes=ntapes+1                                                        !/*mht       236*/
         if (ntapes.ge.mxhist) call expire (0,'wrt_hstp',                       !/*mht       237*/
     &   'no. of histp files exceeds mxhist.')                                  !/*mht       238*/
         nwrds=0                                                                !/*mht       239*/
      endif                                                                     !/*mht       240*/
#endif                                                                          /*!mht       241*/
!                                                                               !/*mht       242*/
      call mgeti(iq1(1),7,i)                                                    !/*mht       243*/
      if(iq1(1).eq.-99999) go to 200                                            !/*mht       244*/
      call mgetr(q1(8),15,i)                                                    !/*mht       245*/
      call mgeti(iq1(23),5,i)                                                   !/*mht       246*/
      call mgetr(q1(28),2,i)                                                    !/*mht       247*/
      ip=q1(8)+1.01d0                                                           !/*mht       248*/
      wtsum(ip,iq1(1))=wtsum(ip,iq1(1))+q1(15)                                  !/*mht       249*/
      nosum(ip,iq1(1))=nosum(ip,iq1(1))+1                                       !/*mht       250*/
      ngo=iq1(1)+5                                                              !/*mht       251*/
      ncolc(ngo)=ncolc(ngo)+1                                                   !/*mht       252*/
      write(nhstp) iq1                                                          !/*mht       253*/
      nwrds=1+nwrds+llq                                                         !/*mht       254*/
!                                                                               !/*mht       255*/
!        residual nuclei records.                                               !/*mht       256*/
      if (iq1(27).gt.0) then                                                    !/*mht       257*/
         do 20 j=1,iq1(27)                                                      !/*mht       258*/
            call mgetr(q2,8,i)                                                  !/*mht       259*/
            write(nhstp) q2                                                     !/*mht       260*/
 20      continue                                                               !/*mht       261*/
        nwrds=1+nwrds+8*iq1(27)                                                 !/*mht       262*/
      endif                                                                     !/*mht       263*/
!                                                                               !/*mht       264*/
!        secondary particle records.                                            !/*mht       265*/
      if (iq1(25).gt.0) then                                                    !/*mht       266*/
         call mgeti(iq3,iq1(25),i)                                              !/*mht       267*/
         call mgetr(q3,7*iq1(25),i)                                             !/*mht       268*/
         write(nhstp) (iq3(j),(q3(k,j),k=1,7),j=1,iq1(25))                      !/*mht       269*/
         nwrds=1+nwrds+8*iq1(25)                                                !/*mht       270*/
      endif                                                                     !/*mht       271*/
      if (iq1(26).gt.0) then                                                    !/*mht       272*/
         call mgetr(q3,7*iq1(26),i)                                             !/*mht       273*/
         write(nhstp) ((q3(k,j),k=1,7),j=1,iq1(26))                             !/*mht       274*/
         nwrds=1+nwrds+7*iq1(26)                                                !/*mht       275*/
      endif                                                                     !/*mht       276*/
      icnt=icnt+1                                                               !/*mht       277*/
      go to 100                                                                 !/*mht       278*/
!                                                                               !/*mht       279*/
!        end of message reached.                                                !/*mht       280*/
 200  isent=iq1(2)                                                              !/*mht       281*/
      return                                                                    !/*mht       282*/
      end subroutine unpack_histp                                               !/*mht       283*/
!                                                                               !/*mht       284*/
#endif                                                                          /*!mht       285*/
!                                                                               !/*mht       286*/
      end module histp                                                          !/*mht       287*/
