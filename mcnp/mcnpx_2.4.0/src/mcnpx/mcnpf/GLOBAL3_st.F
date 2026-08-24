!#ident GLOBAL3_st.F from deck mgs                                              !/*mgs         1*/
      module global_static                                                      !/*mgs         2*/
!                                                                               !/*mgs         3*/
      use global_parameters                                                     !/*mgs         4*/
      implicit real(kindr) (a-h,o-z)                                            !/*mgs         5*/
      implicit integer(kindi) (i-n)                                             !/*mgs         6*/
      save                                                                      !/*mgs         7*/
      public                                                                    !/*mgs         8*/
!                                                                               !/*mgs         9*/
!        whole-block declarations.                                              !/*mgs        10*/
!        fixcom lengths and equivalences                                        !/*mgs        11*/
      parameter (nfixcm = 17*(1)+1*(2)+1*(9)+2*(26)+1*(30)+1*(51)               !/*mgsk        1*/
     & +1*(maxi)                                                                !/*mgsk        2*/
     & +7*(mipt)+1*(m_lcb)+1*(m_leb)                                            !/*mgsl        1*/
     & +1*(mbng)+2*(mtop)+1*(nsp)+1*(3*maxv)+1*(mipt*10)+1*(mipt*3)             !/*mgs        14*/
     & +1*(mipt*7)+1*(2*mxdt)+1*(mipt*5*mxdx)+1*(mipt*2*mxdx)                   !/*mgsl        2*/
     & )                                                                        !/*mgs        19*/
      parameter (lfixcm = 388*(1)+3*(3)+1*(2*6)+1*(2*mxdt)                      !/*mgsk        3*/
     & +4*(maxv)+1*(mink)+12*(mipt)+3*(mipt+1)+1*(mxdt)                         !/*mgs        21*/
     & +3*(mxss)+1*(nmkey)+1*(m_lca)+1*(m_lea)                                  !/*mgs        22*/
#ifdef HISTP                                                                    /*!mgs        26*/
     & +2                                                                       !/*mgs        27*/
#endif                                                                          /*!mgs        28*/
#ifdef CEM                                                                      /*!mgs        29*/
     & +2                                                                       !/*mgs        30*/
#endif                                                                          /*!mgs        31*/
#ifdef MESHTAL                                                                  /*!mgs        32*/
     & +12*(1)+4*(maxg)+1*(maxg*maxgv)                                          !/*mgs        33*/
#endif                                                                          /*!mgs        34*/
#ifdef SPABI                                                                    /*!mgs        35*/
     & +4*(1)+3*(mipt)+1*(mipt*mipt)                                            !/*mgs        36*/
#endif                                                                          /*!mgs        37*/
     & )                                                                        !/*mgs        38*/
      dimension gfixcm(nfixcm),jfixcm(lfixcm)                                   !/*mgs        39*/
      equivalence (bbrem,gfixcm),(ibad,jfixcm)                                  !/*mgs        40*/
!                                                                               !/*mgs        41*/
!        varcom lengths and equivalences                                        !/*mgs        42*/
      parameter (nvarsw = 1*(1)+1*(2)+1*(3)+1*(7)+1*(4*2)                       !/*mgs        43*/
     & +1*(6*items_pax_total*mipt)+1*(mipt)+1*(mipt*2)+2*(mipt*3)               !/*mgs        44*/
     & )                                                                        !/*mgs        48*/
      parameter (nvarcm = nvarsw + 15*(1)+2*(3)+1*(4)+1*(10)                    !/*mgs        49*/
     & +1*(2*3)+2*(3*3)+2*(mipt)+1*(mdum)                                       !/*mgs        50*/
     & )                                                                        !/*mgs        54*/
      parameter (lvarsw = 1*(1)+1*(3)+1*(8*mxdx*mipt)+1*(mipt)                  !/*mgs        55*/
     & )                                                                        !/*mgs        56*/
      parameter (lvarcm = lvarsw + 45*(1)+2*(2)+1*(3)+1*(10)+1*(20)             !/*mgs        57*/
     & +1*(2*99)+1*(mcpu)+1*(mdum)                                              !/*mgs        58*/
#ifdef RADIOG                                                                   /*!mgs        62*/
     & +4*(1)+1*(100)                                                           !/*mgs        63*/
#endif                                                                          /*!mgs        64*/
     & )                                                                        !/*mgs        65*/
      dimension gvarcm(nvarcm),jvarcm(lvarcm)                                   !/*mgs        66*/
      equivalence (coll,gvarcm),(nbhwm,jvarcm)                                  !/*mgs        67*/
!                                                                               !/*mgs        68*/
!        ephcom lengths and equivalences                                        !/*mgs        69*/
      parameter (nephcm = 12*(1)+1*(4)+1*(11)+1*(mcpu)                          !/*mgs        70*/
     & )                                                                        !/*mgs        71*/
      parameter (lephcm = 51*(1)+2*(2)+1*(3)+1*(ncolor+7)                       !/*mgs        72*/
     & +1*(novr)+1*(nptr)                                                       !/*mgs        73*/
#ifdef MESHTAL                                                                  /*!mgs        74*/
     & +1*(maxg)                                                                !/*mgs        75*/
#endif                                                                          /*!mgs        76*/
     & )                                                                        !/*mgs        77*/
      dimension gephcm(nephcm),jephcm(lephcm)                                   !/*mgs        78*/
      equivalence (cp1,gephcm),(ichan,jephcm)                                   !/*mgs        79*/
      logical lockl                                                             !/*mgs        80*/
!                                                                               !/*mgs        81*/
!*******************  statically allocated common  *********************        !/*mgs        82*/
!                                                                      *        !/*mgs        83*/
!        fixed common -- constant after the problem is initiated.               !/*mgs        84*/
      common /fixcom/ bbrem(mtop),bnum,calph(maxi),coincd,                      !/*mgs        85*/
     & cut_h,cut_n,dbcn(30),ddg(2,mxdt),ddx(mipt,2,mxdx),dnb,                   !/*mgs        89*/
     & dxw(mipt,3),dxx(mipt,5,mxdx),ecf(mipt),efac(mipt),                       !/*mgsl        3*/
     & emcf(mipt),emx(mipt),enum,espl(mipt,10),fnw,hsb(nsp),rcoilf(2),          !/*mgsk        4*/
     & optlcb(m_lcb),optleb(m_leb),rim,rkt(mtop),rka(mbng),rnok,                !/*mgs        92*/
     & rnfb,rnfs,rngb,rngs,rnmult,srv(3,maxv),tco(mipt),thgf(0:50),             !/*mgs        93*/
     & wc1(mipt),wc2(mipt),wwg(9),wwp(mipt,7),wwm(26),wwma(26),                 !/*mgs        94*/
     & xunrl,xunru                                                              !/*mgs        95*/
      common /fixcom/ zfixcm                                                    !/*mgs        96*/
!                                                                               !/*mgs        97*/
!        fixcom integers.                                                       !/*mgs        98*/
      common /fixcom/                                                           !/*mgs        99*/
     & ibad,icw,idefv(maxv),ides,idrc(mxdt),ifft,igm,ikz,img,imt,               !/*mgs       100*/
#ifdef HISTP                                                                    /*!mgs       101*/
     & ihistp,                                                                  !/*mgs       102*/
#endif                                                                          /*!mgs       103*/
     & imesh(nmkey),indt,ink(mink),ioid,iphot,iplt,ipty(mipt),isb,              !/*mgs       107*/
     & ism(3),in_erg(mipt),isanti,lcaopt(m_lca),leaopt(m_lea),                  !/*mgs       108*/
     & ispn,issw,istern,istrg(mipt),its30,iunr,ivdd(maxv),ivdis(maxv),          !/*mgs       109*/
     & isfiss,ivord(maxv),iwwg,jgm(mipt),jtlx,junf,kf8,kfl,kfq,kjaq,            !/*mgsk        5*/
     & knods,knrm,kpt(mipt),ktls,kufil(2,6),lfcdg,lfcdj,locdt(2,mxdt),          !/*mgs       111*/
     & lvcdg,lvcdj,lxs,mai,mbnk,mcal,mct,mgegbt(mipt),mgm(mipt+1),              !/*mgs       112*/
     & mgww(mipt+1),mix,mjss,mlaj,mlja,mrkp,mrl,msd,msrk,mtasks,                !/*mgs       113*/
     & mww(mipt+1),mxa,mxafs,mxe,mxf,mxj,mxt,mxtr,mxxs,ndet(mipt),              !/*mgs       114*/
     & ndnd,ndtt,ndx(mipt),nee(mipt),ngww(mipt),nhb,nilr(mxss),nilw,            !/*mgs       115*/
     & nips,nipt,nee_max,                                                       !/*mgs       116*/
     & niss,njsr(mxss),njss,njsx(mxss),nkxs,nlat,nlev,nlja,nmat,                !/*mgs       117*/
     & nmxf,nnpos,nocoh,nord,np1,npikmt,npn,ipert,mnnm,mxfp,nmaz,               !/*mgs       118*/
     & nmip,nmzu,npert,nrcd,nrss,nsph,nsr,nsrc,nsrck,nstp,nstrid,               !/*mgs       119*/
     & ntal,ntop,numb,nvec,nwang,nwgeoa,nwgeom,nwgm,nwgma,nwng,                 !/*mgs       120*/
     & nww(mipt),nwwm,nwwma,nxnx                                                !/*mgs       121*/
#ifdef MESHTAL                                                                  /*!mgs       122*/
      common /fixcom/ nmesh,mshfm,mtlflg,mspflg,medflg,mpdflg,mdxflg            !/*mgs       123*/
#endif                                                                          /*!mgs       124*/
#ifdef SPABI                                                                    /*!mgs       125*/
      common /fixcom/ list_secs_primaries(mipt,mipt),                           !/*mgs       126*/
     & num_primaries_active(mipt),num_energy_bands(mipt),                       !/*mgs       127*/
     & interior_offset(mipt),len_dynamic_spb_data,is_spabi                      !/*mgsa        1*/
#endif                                                                          /*!mgs       129*/
!                                                                               !/*mgs       130*/
!        save all variables used for dynamic allocation.                        !/*mgs       131*/
!        some of these can be eliminated if unchanged in second pass.           !/*mgs       132*/
      common /fixcom/ nee3,nmat1,istrg3,mix1,mxe1,iplt1,mxa1,igm1,              !/*mgs       133*/
     & npert1,mnnm1,nxnx1,msd1,mbbm1,mgww1,mcal1,npn1,kpt1(mipt),               !/*mgs       134*/
     & ipert1,nsc1,mxxs1,ltd1,mxt1,mxtr1,nlat1,nvec1,nwgm1,nwwm1,               !/*mgs       135*/
     & nwgma1,nwma1(3),mlja1,mxj1,ntal1,mww1,njsx1,lit1,njsr1,nilr1,            !/*mgs       136*/
     & njsw1,niwr1,mssc1,mjss1,nilw1,junf1,mkcp1,mlaf1,mxafs1,                  !/*mgs       137*/
     & nmzu1,mipts1,mlaj1,ndnd1,icw1,msrk1,nwwma1,nmaz1,njss1,                  !/*mgs       138*/
     & npikmt1,iptra1,iptra2,kfl1,indt1,ndup1(3),lrt1,naw1,nmfm1,               !/*mgs       139*/
     & msso1,ktl1,ides1,mrkp1,kjaq1,                                            !/*mgs       140*/
#ifdef SPABI                                                                    /*!mgs       141*/
     & nspabi,                                                                  !/*mgs       142*/
#endif                                                                          /*!mgs       143*/
#ifdef MESHTAL                                                                  /*!mgs       144*/
     & nemsh1,ngdt,ngd1,ngd2,ngd3,                                              !/*mgs       145*/
#endif                                                                          /*!mgs       146*/
     & mxmel1                                                                   !/*mgs       147*/
!        offsets for virtual arrays in dynamically allocated storage.           !/*mgs       148*/
      common /fixcom/ lara,lasm,lasp,lawc,lawn,lcmg,lden,ldpt,                  !/*mgs       149*/
     & ldrs,ldxp,leaa,lear,leba,lebd,lebl,lebt,lech,ledg,leee,                  !/*mgs       150*/
     & leek,legg,lelp,lesa,lewg,lfim,lflc,lfme,lfmg,lfor,lfrc,lfst,             !/*mgsl        4*/
     & lftt,lgmg,lgvl,lgwt,lpbr,lpbt,lpkn,lpmg,lpnt,lpru,lpxr,lqav,             !/*mgs       152*/
     & lqax,lqcn,lrho,lrpt,lscf,lsmg,lspf,lsqq,lsso,ltbt,ltds,ltrf,             !/*mgsl        5*/
     & ltth,lvcl,lvec,lvol,lwga,lwgm,lwwe,lwwf,lwwk,lxnm,lx85,                  !/*mgs       154*/
     & lida,lide,lids,lidt,lksm,                                                !/*mgs       155*/
     & lipa,lipb,lipt,liss,litd,lixl,lixs,liza,lizn,ljaq,ljar,ljaw,ljmd,        !/*mgs       156*/
     & ljpt,ljsc,ljsq,ljss,ljtf,ljun,ljvc,ljxs,lkcp,lkmm,lksc,lksd,lkst,        !/*mgs       157*/
     & lksu,lktp,lkxs,llaf,llat,llca,llfc,llft,llja,llme,llmn,llmt,             !/*mgs       158*/
     & llct,llph,llst,llsc,lmat,lmbd,lmbi,lmfl,lmzp,lmzu,lncl,                  !/*mgs       159*/
     & lngm,lnht,lnmt,lnpt,lnpq,lnsb,lnsf,lnsr,lnty,lnxs,                       !/*mgs       160*/
     & lrkp,ltfc,ljfq,lnpw,lnsl,lntb,                                           !/*mgs       161*/
     & lscr,lptr,lpts,lrng,lixc,ljco,ljem,ljmt,lkmt,                            !/*mgs       162*/
     & lkxd,llbb,                                                               !/*mgs       163*/
     & laa9,larg,lava,lavz,lemaxt,lexn,lexp,lesp,lfmf,lhsg,lpcp,                !/*mgs       167*/
     & lsgg,lsgx,lxso,lzz9,liax,lpptb,lmel,lnel,                                !/*mgs       168*/
#ifdef HISTP                                                                    /*!mgs       169*/
     & lnhw,                                                                    !/*mgs       170*/
#endif                                                                          /*!mgs       171*/
#ifdef CEM                                                                      /*!mgs       172*/
     & lipnf,lepnm,                                                             !/*mgs       173*/
#endif                                                                          /*!mgs       174*/
#ifdef MESHTAL                                                                  /*!mgs       175*/
     & lgd1(maxg),lgd2(maxg),lgd3(maxg),lemsh(maxg*maxgv),                      !/*mgsk        6*/
#endif                                                                          /*!mgs       177*/
#ifdef SPABI                                                                    /*!mgs       178*/
     & main_spb_offset,                                                         !/*mgs       179*/
#endif                                                                          /*!mgs       180*/
     & llgtsk,lljtsk,lmb,lxss,lexs                                              !/*mgs       181*/
!                                                                               !/*mgs       182*/
!        offset l<var>s with parallel k<var>s in common /itskpt/.               !/*mgs       183*/
      common /fixcom/ lbnk,lddm,lddn,ldec,ldxc,ldxd,lfeb,lflx,                  !/*mgs       184*/
     & lfso,lgbn,lpac,lpan,lpcc,lpwb,lshs,lstt,lsum,                            !/*mgsl        6*/
     & lsww,ltmp,lwfa,lwns,                                                     !/*mgs       186*/
     & lise,llaj,llcj,llse,lmaz,lndp,lndr,lnhs,                                 !/*mgs       187*/
     & ldrc,lfdd,lpik,lptb,lrtc,ltgp,                                           !/*mgs       188*/
     & lifg,lifl,lpc2,ljfl,ljft,ljpb,lktc,lnmc,                                 !/*mgs       189*/
     & ltal,                                                                    !/*mgsk        7*/
#ifdef MESHTAL                                                                  /*!mgsk        8*/
     & ltgd(maxg),                                                              !/*mgsk        9*/
#endif                                                                          /*!mgsk       10*/
     & mfixcm                                                                   !/*mgsk       11*/
!                                                                               !/*mgs       192*/
!        variable common -- variable but required for a continue run.           !/*mgs       193*/
!        arrays that are backed up when a track is lost.                        !/*mgs       194*/
!                                                                               !/*mgs       195*/
!        real variables swept from tskcom - order important, see tskcom.        !/*mgs       196*/
      common /varcom/ coll(mipt),eacc(mipt,3),                                  !/*mgs       197*/
     & pax(6,items_pax_total,mipt),racc(mipt,2),rlt(4,2),rnr,smul(7),           !/*mgs       201*/
     & sumk(3),tmav(mipt,3),wgts(2)                                             !/*mgs       202*/
!                                                                               !/*mgs       203*/
!        non-swept real variables.                                              !/*mgs       204*/
      common /varcom/ bcw(2,3),cpk,cts,dmp,                                     !/*mgs       205*/
     & osum(3),osum2(3,3),pptme(4),prn,rani,ranj,rdum(mdum),rijk,               !/*mgs       209*/
     & rkk,rssp,rsum(3),rsum2(3,3),snit,swtm,twac,twss,                         !/*mgs       210*/
     & wcs1(mipt),wcs2(mipt),wssi(10),wt0                                       !/*mgs       211*/
      common /varcom/ zvarcm                                                    !/*mgs       212*/
!                                                                               !/*mgs       213*/
!        int variables swept from tskcom - order important, see tskcom.         !/*mgs       214*/
      common /varcom/ nbhwm,nbt(mipt),nrnh(3),nziy(8,mxdx,mipt)                 !/*mgs       215*/
!                                                                               !/*mgs       216*/
!        non-swept integer variables.                                           !/*mgs       217*/
      common /varcom/ idum(mdum),inif,ist,ist0,ixak,ixak0,jrad,                 !/*mgs       218*/
     & kc8,kcz,kcsf,kct,kcy,knod,ksdef,lost(2),                                 !/*mgs       222*/
     & nbal(mcpu),nbov,ndmp,nerr,nesm,netb(2),nfer,                             !/*mgs       223*/
     & npc(20),npd,npnm,npp,nppm,nps,npsout,npsr,npum,nqss,nqsw,nrrs,           !/*mgs       224*/
     & nrsw,nsa,nsa0,nskk,nsom,nss,nss0,nssi(10),ntc,ntc1,ntss,nwer,            !/*mgs       225*/
     & npxm,                                                                    !/*mgs       226*/
#ifdef RADIOG                                                                   /*!mgs       227*/
     & notal,notrn,ntprt(100),monod,npsmg,                                      !/*mgs       228*/
#endif                                                                          /*!mgs       229*/
     & nwsb,nwse,nwsg(3),nwst,nwws(2,99)                                        !/*mgs       230*/
      common /varcom/ mvarcm                                                    !/*mgs       231*/
!                                                                               !/*mgs       232*/
!        ephemeral common -- not needed after the current run.                  !/*mgs       233*/
!        ephcom reals.                                                          !/*mgs       234*/
      common /ephcom/ cp1,cp2(mcpu),cp3,ctme,fpi,freq,                          !/*mgs       235*/
     & ssb(11),tdc,tlc,trm,wnvp(4),xhom,xnum,yhom                               !/*mgs       236*/
      common /ephcom/ zephcm                                                    !/*mgs       237*/
!        ephcom integers.                                                       !/*mgs       238*/
      common /ephcom/ ichan,ics,idmp,ifile,iln,iln1,inform,iovr,inpd,           !/*mgs       239*/
     & iptr,iptra(nptr),irup,itask,iterm,itfxs,itotnu,iuou,jchar,jfcn,          !/*mgs       240*/
     & jgf,jgxa(2),jgxo(2),jovr(novr),jtasks,jtfc,jvp,kbp,                      !/*mgs       241*/
     & kcolor(ncolor+7),kdbnps,kmplot,komout,konrun,kprod,krflg,krtm,           !/*mgs       242*/
     & ksr,ktfile,lchnk,lfatl,lfll,locki,lspeed,ltasks,mazf(3),mcolor,          !/*mgs       243*/
#ifdef MESHTAL                                                                  /*!mgs       244*/
     & nugd(maxg),                                                              !/*mgs       245*/
#endif                                                                          /*!mgs       246*/
     & mdc,mmkdb,mnk,mpc,mrm,nde,nkrp,nomore,nrc,nst,ntasks                     !/*mgs       247*/
      common /ephcom/ mephcm,lockl,mynum                                        !/*mgs       248*/
!                                                                               !/*mgs       249*/
!        pbl common reals -- particle description.                              !/*mgs       250*/
#ifdef MULTT                                                                    /*!mgsk       12*/
!$OMP THREADPRIVATE (/pblcom/,/tskcom/,/itskpt/)                                !/*mgsk       13*/
#endif                                                                          /*!mgsk       14*/
      common /pblcom/                                                           !/*mgsk       15*/
     & xxx,yyy,zzz,uuu,vvv,www,erg,wgt,tme,vel,dls,dxl,dtc,elc(mipt),           !/*mgs       256*/
     & fiml(mipt),fismg,wtfasv,rnk,spare(mspare),totmp,alfap(2),gamef,          !/*mgs       257*/
     & zpblcm,                                                                  !/*mgs       258*/
     & xxx9(mpb),yyy9(mpb),zzz9(mpb),uuu9(mpb),vvv9(mpb),www9(mpb),             !/*mgs       259*/
     & erg9(mpb),wgt9(mpb),tme9(mpb),vel9(mpb),dls9(mpb),dxl9(mpb),             !/*mgs       260*/
     & dtc9(mpb),elc9(mpb,mipt),fiml9(mpb,mipt),fismg9(mpb),wtfas9(mpb),        !/*mgs       261*/
     & rnk9(mpb),spare9(mpb,mspare),totmp9(mpb),alfap9(mpb,2),                  !/*mgs       262*/
     & gamef9(mpb),                                                             !/*mgs       263*/
     & zpb9cm(mpb)                                                              !/*mgs       264*/
!                                                                               !/*mgs       265*/
!        pbl common integers -- particle description.                           !/*mgs       266*/
      common /pblcom/                                                           !/*mgsk       16*/
     & npa,icl,jsu,ipt,iex,node,idx,ncp,jgp,lev,iii,jjj,kkk,iap,iexp,           !/*mgs       272*/
     & mtp,nmco,jan,                                                            !/*mgs       273*/
#ifdef MESHTAL                                                                  /*!mgs       274*/
     & mspf,                                                                    !/*mgs       275*/
#endif                                                                          /*!mgs       276*/
     & mpblcm,                                                                  !/*mgs       277*/
     & npa9(mpb),icl9(mpb),jsu9(mpb),ipt9(mpb),iex9(mpb),node9(mpb),            !/*mgs       278*/
     & idx9(mpb),ncp9(mpb),jgp9(mpb),lev9(mpb),iii9(mpb),jjj9(mpb),             !/*mgs       279*/
     & kkk9(mpb),iap9(mpb),iexp9(mpb),mtp9(mpb),nmco9(mpb),jan9(mpb),           !/*mgs       280*/
#ifdef MESHTAL                                                                  /*!mgs       281*/
     & mspf9(mpb),                                                              !/*mgs       282*/
#endif                                                                          /*!mgs       283*/
     & mpb9cm(mpb)                                                              !/*mgs       284*/
!        pblcom lengths and equivalences                                        !/*mgs       285*/
      parameter (npblcm = 18*(1)+1*(2)+2*(mipt)+1*(mspare)                      !/*mgs       286*/
     & )                                                                        !/*mgs       287*/
      parameter (lpblcm = 18*(1)                                                !/*mgs       288*/
#ifdef MESHTAL                                                                  /*!mgs       289*/
     & +1                                                                       !/*mgs       290*/
#endif                                                                          /*!mgs       291*/
     & )                                                                        !/*mgs       292*/
      dimension gpblcm(npblcm+1),jpblcm(lpblcm+1),                              !/*mgs       293*/
     & gpb9cm(mpb,npblcm+1),jpb9cm(mpb,lpblcm+1)                                !/*mgs       294*/
      equivalence (xxx,gpblcm),(npa,jpblcm),(xxx9,gpb9cm),(npa9,jpb9cm)         !/*mgs       295*/
      dimension ijk(3)                                                          !/*mgs       296*/
      equivalence (iii,ijk)                                                     !/*mgs       297*/
!                                                                               !/*mgs       298*/
!        task common -- parallel variables private to tasks.                    !/*mgs       299*/
!        swept tskcom reals - order is important, see varcom.                   !/*mgs       300*/
      common /tskcom/                                                           !/*mgsk       17*/
     & colltc(mipt),eacctc(mipt,3),                                             !/*mgs       306*/
     & paxtc(6,items_pax_total,mipt),racctc(mipt,2),rlttc(4,2),                 !/*mgs       310*/
     & rnrtc,smultc(7),sumktc(3),tmavtc(mipt,3),wgtstc(2)                       !/*mgs       311*/
!                                                                               !/*mgs       312*/
!        non-swept tskcom reals.                                                !/*mgs       313*/
      common /tskcom/                                                           !/*mgsk       18*/
     & amfp,ang(3),cbwf,cmult,colout(3,mcol),cpv,ddet,deb,dti(mlgc),            !/*mgsc       25*/
     & eg0,ergace,pfp,ple,pmf,psc,ptbtc,qpl,ranb,ranitc,ranjtc,rans,            !/*mgs       320*/
     & rijktc,rnb(5),rnrtc0,rr0,sff(3,maxv),siga,ssr,stp,totgp1,                !/*mgs       321*/
     & totm,totpn,tpd(7),tpp(64),ttn,udt(10,0:mxlv),                            !/*mgs       322*/
     & udtsav(3,10*mxlv+10),uold(3),vtr(3),wcs1tc(mipt),                        !/*mgs       323*/
     & wcs2tc(mipt),ycn,                                                        !/*mgs       324*/
#ifdef RADIOG                                                                   /*!mgs       325*/
     & deltas,deltat,                                                           !/*mgs       326*/
#endif                                                                          /*!mgs       327*/
     & ztskcm                                                                   !/*mgs       328*/
!                                                                               !/*mgs       329*/
!        swept tskcom integers - order is important, see varcom.                !/*mgs       330*/
      common /tskcom/                                                           !/*mgsk       19*/
     & nbhwtc,nbttc(mipt),nrnhtc(3),nziytc(8,mxdx,mipt)                         !/*mgs       336*/
!                                                                               !/*mgs       337*/
!        non-swept tskcom integers.                                             !/*mgs       338*/
      common /tskcom/                                                           !/*mgsk       20*/
     & iax,ibc,ibe,ibs,ibt,ibu,ic0,iclp(5,0:mxlv),id0,idet,iet,imd,iper,        !/*mgs       344*/
     & ipsc,irt,isic(maxv),ital,iti(mlgc),iw0,ixcos,ixre,jap,jbd,jbnk,          !/*mgs       345*/
     & jev,jlock,jtls,kdb,kqss,ktask,levp,lgc(mlgc+1),lsb,mbb,mkc,mpan,         !/*mgs       346*/
     & nbnk,nch(mipt),ngp,nlaj,nlse,nlt,nmc,npb,npsrtc,npstc,nter,ntii,         !/*mgs       347*/
     & ntx,ntyn,ntyr,                                                           !/*mgs       348*/
     & mtskcm                                                                   !/*mgs       349*/
!        tskcom,itskpt lengths and equivalences.                                !/*mgs       350*/
      parameter (ntskcm = nvarsw + 30*(1)+3*(3)+1*(5)+1*(7)+1*(64)              !/*mgs       351*/
     & +1*(3*mcol)+2*(mipt)+1*(mlgc)+1*(3*maxv)+4*(10*mxlv+10)                  !/*mgsc       26*/
#ifdef RADIOG                                                                   /*!mgs       353*/
     & +2                                                                       !/*mgs       354*/
#endif                                                                          /*!mgs       355*/
     & )                                                                        !/*mgs       356*/
      parameter (ltskcm = lvarsw + 47*(1)+1*(5*(1+mxlv))+1*(maxv)               !/*mgs       357*/
     & +1*(mipt)+1*(mlgc)+1*(mlgc+1)                                            !/*mgs       358*/
     & )                                                                        !/*mgs       359*/
      dimension gtskcm(ntskcm),jtskcm(ltskcm)                                   !/*mgs       360*/
      equivalence (colltc,gtskcm),(nbhwtc,jtskcm)                               !/*mgs       361*/
!                                                                               !/*mgs       362*/
!        itskpt common -- parallel offset variables.                            !/*mgs       363*/
      common /itskpt/                                                           !/*mgsk       21*/
     & kbnk,kddm,kddn,kdec,kdxc,kdxd,kfeb,kflx,                                 !/*mgs       369*/
     & kfso,kgbn,kpac,kpan,kpcc,kpwb,kshs,kstt,ksum,                            !/*mgsl        7*/
     & ksww,ktmp,kwfa,kwns,                                                     !/*mgs       371*/
     & kise,klaj,klcj,klse,kmaz,kndp,kndr,knhs,                                 !/*mgs       372*/
     & kdrc,kfdd,kpik,kptb,krtc,ktgp,                                           !/*mgs       373*/
     & kifg,kifl,kpc2,kjfl,kjft,kjpb,kktc,knmc,                                 !/*mgs       374*/
     & ktal,                                                                    !/*mgs       375*/
#ifdef MESHTAL                                                                  /*!mgsk       22*/
     & ktgd(maxg),                                                              !/*mgsk       23*/
#endif                                                                          /*!mgsk       24*/
     & mtskpt                                                                   !/*mgs       376*/
!        itskpt lengths and equivalences.                                       !/*mgs       377*/
      parameter (ltskpt = 45*(1)                                                !/*mgsl        8*/
#ifdef MESHTAL                                                                  /*!mgsk       25*/
     & +1*(maxg)                                                                !/*mgsk       26*/
#endif                                                                          /*!mgsk       27*/
     & )                                                                        !/*mgs       379*/
      dimension ktskpt(ltskpt),lpntcm(ltskpt)                                   !/*mgs       380*/
      equivalence (kbnk,ktskpt),(lbnk,lpntcm)                                   !/*mgs       381*/
!                                                                               !/*mgs       382*/
      end module global_static                                                  !/*mgs       383*/
