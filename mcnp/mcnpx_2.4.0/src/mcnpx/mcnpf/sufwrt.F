!#ident sufwrt.F from deck sw                                                   !/*sw          1*/
      subroutine sufwrt(ib,cs)                                                  !/*sw          2*/
!        write surface-crossing data to the surface-source scratch file.        !/*sw          3*/
!        cs = cosine of angle between track and normal to surface jsu.          !/*sw          4*/
!        ib = index in jss.                                                     !/*sw          5*/
      use global_data                                                           !/*swx         1*/
!                                                                               !/*swx         2*/
      implicit real(kindr) (a-h,o-z)                                            !/*swx         3*/
      implicit integer(kindi) (i-n)                                             !/*swx         4*/
!                                                                               !/*swx         5*/
!                                                                               !/*sw          7*/
      if(krflg.eq.2)return                                                      !/*sw          8*/
      if(kc8.gt.0)return                                                        !/*sw4c        1*/
#ifdef MULTT                                                                    /*!swxk        1*/
      call thread_lockon(jlock)                                                 !/*swxk        2*/
#endif                                                                          /*!swxk        3*/
      a=npstc                                                                   !/*sw         10*/
      if(nsr.eq.6)a=npsrtc                                                      !/*sw         11*/
      if(nsr.eq.71)a=a-nskk                                                     !/*sw         12*/
      if(ncp.eq.0)a=-a                                                          !/*sw         13*/
      if(ib.le.njss)b=ipt*1000000+nsf(lnsf+jsu)-200000000*min(0,jgp)            !/*sw         14*/
      if(ib.gt.njss)b=(ipt+100)*1000000+ncl(lncl+icl)                           !/*sw         15*/
      if(img.ne.0)b=jgp*1000000+nsf(lnsf+jsu)                                   !/*sw         16*/
!        include surface facet*0.1 on b.                                        !/*sw4c2       1*/
      if(ib.le.njss.or.img.ne.0)b=sign(b+kfq*0.1,b)                             !/*sw4c2       2*/
      if(nsph.ne.1.and.lev.eq.0)                                                !/*sw4b        1*/
     & write(iusc)a,sign(b,www),wgt,erg,tme,(gpblcm(i),i=1,5),cs                !/*sw4b        2*/
      if(nsph.ne.1.and.lev.ne.0)                                                !/*sw4b        3*/
     & write(iusc)a,sign(b,udt(6,0)),wgt,erg,tme,(udt(i,0),i=1,5),cs            !/*sw4b        4*/
      if(nsph.eq.1)write(iusc)a,b,wgt,erg,tme,cs                                !/*sw4b        5*/
!                                                                               !/*sw         22*/
!        update the surface-source summary array.                               !/*sw         23*/
      ns=npsw(lnpw+ib)                                                          !/*sw         24*/
      nsl(lnsl+1,ib)=nsl(lnsl+1,ib)+1                                           !/*sw         25*/
      if(npstc.ne.ns)nsl(lnsl+2,ib)=nsl(lnsl+2,ib)+1                            !/*sw         26*/
      i=lnsl+4*(ipt-1)                                                          !/*sw         27*/
      nsl(i+3,ib)=nsl(i+3,ib)+1                                                 !/*sw         28*/
      if(npstc.ne.ns)nsl(i+4,ib)=nsl(i+4,ib)+1                                  !/*sw         29*/
      if(ncp.eq.0)nsl(i+5,ib)=nsl(i+5,ib)+1                                     !/*sw         30*/
      if(npstc.ne.ns.and.ncp.eq.0)nsl(i+6,ib)=nsl(i+6,ib)+1                     !/*sw         31*/
      if(kqss.eq.0)nqsw=nqsw+1                                                  !/*sw         32*/
      kqss=1                                                                    !/*sw         33*/
      npsw(lnpw+ib)=npstc                                                       !/*sw         34*/
      nrsw=nrsw+1                                                               !/*sw         35*/
#ifdef MULTT                                                                    /*!swxk        4*/
      call thread_lockoff(jlock)                                                !/*swxk        5*/
#endif                                                                          /*!swxk        6*/
      return                                                                    !/*sw         37*/
      end                                                                       !/*sw         38*/
