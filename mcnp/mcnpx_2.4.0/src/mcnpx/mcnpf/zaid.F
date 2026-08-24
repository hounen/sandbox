!#ident zaid.F from deck za                                                     !/*za          1*/
      subroutine zaid(m,h,ix)                                                   !/*za          2*/
!        m=1 encode the zaid h into ix.                                         !/*za          3*/
!        m=2 decode into h the zaid that is coded in ix.                        !/*za          4*/
      dimension ix(3)                                                           !/*za          5*/
      character h*10                                                            !/*za          6*/
!                                                                               !/*za          7*/
      if(m.eq.2)go to 10                                                        !/*za          8*/
      ix(1)=ichar(h(1:1))+256*(ichar(h(2:2))+256*ichar(h(3:3)))                 !/*za          9*/
      ix(2)=ichar(h(4:4))+256*(ichar(h(5:5))+256*ichar(h(6:6)))                 !/*za         10*/
      ix(3)=ichar(h(8:8))+256*(ichar(h(9:9))+256*ichar(h(10:10)))               !/*za         11*/
      return                                                                    !/*za         12*/
   10 do 20 i=1,3                                                               !/*za         13*/
   20 h(i**2-i/3:)=char(mod(ix(i),256))//char(mod(ix(i)/256,256))//             !/*za         14*/
     & char(ix(i)/65536)                                                        !/*za         15*/
      h(7:7)='.'                                                                !/*za         16*/
      return                                                                    !/*za         17*/
      end                                                                       !/*za         18*/
