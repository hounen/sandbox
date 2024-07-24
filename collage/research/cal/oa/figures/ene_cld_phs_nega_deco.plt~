#!/usr/bin/gnuplot -persist
set term postscript eps enh color solid 8 
set output "ene_cld_phs_nega_deco.eps"
set xrange[0:20]
set multiplot

###phs#################################################################

set yrange[-100:240]
set key 15,140
unset xtics
set ytics 0,60,480
set ylabel "{/Symbol d} [deg.]"
set label "K^{/Symbol p}=0_{1}^{-} J^{/Symbol p}=1^{-} E_{r}=0.49MeV {/Symbol G}=1.68*10^{-4}MeV" at 10,220 center
set label "{/Symbol d}" at 10,150 center font "Times,15"
set arrow from 0,0 to 20,0 lt 0 nohead
set arrow from 0,180 to 20,180 lt 0 nohead
set origin 0.0,0.7
set size 0.2,0.3
set tmargin 0
set bmargin 0
set rmargin 0
set lmargin 0
p "../phs/data/analysis/phs_k1_j1.d" u 1:2 w l t "total" lt 3 lw 3,\
  "../phs/data/analysis/phs_k1_j1.d" u 1:3 w l t "resonance" lt 1 lw 3,\
  "../phs/data/analysis/phs_k1_j1.d" u 1:4 w l t "continuum" lt 2 lw 3

#set ytics (0,45,90,135,180)
#set noy2tics
unset ytics
unset ylabel
unset label
set label "K^{/Symbol p}=0_{1}^{-} J^{/Symbol p}=3^{-} E_{r}=2.31MeV {/Symbol G}=0.01MeV" at 10,220 center
#set label "{/Symbol d}" at 4.5,150 center font "Times,15"
set origin 0.2,0.7
set size 0.2,0.3
set tmargin 0
set bmargin 0
set rmargin 0
set lmargin 0
p "../phs/data/analysis/phs_k1_j3.d" u 1:2 w l t "" lt 3 lw 3,\
  "../phs/data/analysis/phs_k1_j3.d" u 1:3 w l t "" lt 1 lw 3,\
  "../phs/data/analysis/phs_k1_j3.d" u 1:4 w l t "" lt 2 lw 3

unset ytics
unset ylabel
unset label
set label "K^{/Symbol p}=0_{1}^{-} J^{/Symbol p}=5^{-} E_{r}=5.64MeV {/Symbol G}=0.3MeV" at 10,220 center
#set label "{/Symbol d}" at 4.5,150 center font "Times,15"
set origin 0.4,0.7
set size 0.2,0.3
set tmargin 0
set bmargin 0
set rmargin 0
set lmargin 0
p "../phs/data/analysis/phs_k1_j5.d" u 1:2 w l t "" lt 3 lw 3,\
  "../phs/data/analysis/phs_k1_j5.d" u 1:3 w l t "" lt 1 lw 3,\
  "../phs/data/analysis/phs_k1_j5.d" u 1:4 w l t "" lt 2 lw 3

unset ytics
unset ylabel
unset label
set label "K^{/Symbol p}=0_{1}^{-} J^{/Symbol p}=7^{-} E_{r}=10.88MeV {/Symbol G}=1.04MeV" at 10,220 center
#set label "{/Symbol d}" at 4.5,150 center font "Times,15"
set origin 0.6,0.7
set size 0.2,0.3
set tmargin 0
set bmargin 0
set rmargin 0
set lmargin 0
p "../phs/data/analysis/phs_k1_j7.d" u 1:2 w l t "" lt 3 lw 3,\
  "../phs/data/analysis/phs_k1_j7.d" u 1:3 w l t "" lt 1 lw 3,\
  "../phs/data/analysis/phs_k1_j7.d" u 1:4 w l t "" lt 2 lw 3

unset ytics
unset ylabel
unset label
set label "K^{/Symbol p}=0_{1}^{-} J^{/Symbol p}=9^{-} E_{r}=18.74MeV {/Symbol G}=1.72MeV" at 10,220 center
set label "K^{/Symbol p}=0_{*}^{-} J^{/Symbol p}=9^{-} E_{r}=23.70MeV {/Symbol G}=18.94MeV" at 10,200 center
#set label "{/Symbol d}" at 4.5,150 center font "Times,15"
set origin 0.8,0.7
set size 0.2,0.3
set tmargin 0
set bmargin 0
set rmargin 0
set lmargin 0
p "../phs/data/analysis/phs_k1_j9.d" u 1:2 w l t "" lt 3 lw 3,\
  "../phs/data/analysis/phs_k1_j9.d" u 1:3 w l t "" lt 1 lw 3,\
  "../phs/data/analysis/phs_k1_j9.d" u 1:4 w l t "" lt 2 lw 3

###cld#################################################################

set yrange[-0.2:0.5]
set key 15,0.4 
set ytics -0.1,0.1,0.4
set ylabel "{/Symbol D} [MeV^{-1}]"
set label "{/Symbol D}" at 10,0.2 center font "Times,15"
unset arrow
set origin 0.0,0.4
set size 0.2,0.3
set tmargin 0
set bmargin 0
set rmargin 0
set lmargin 0
p "../cld/data/cld_k1_j1.d" u 1:2 t "total" w l lt 3  lw 3,\
  "../cld/data/cld_k1_j1.d" u 1:3 t "resonance" w l lt 1  lw 3,\
  "../cld/data/cld_k1_j1.d" u 1:4 t "continuum" w l lt 2  lw 3

unset ylabel
unset ytics
unset label
set origin 0.2,0.4
set size 0.2,0.3
set tmargin 0
set bmargin 0
set rmargin 0
set lmargin 0
p "../cld/data/cld_k1_j3.d" u 1:2 t "" w l lt 3  lw 3,\
  "../cld/data/cld_k1_j3.d" u 1:3 t "" w l lt 1  lw 3,\
  "../cld/data/cld_k1_j3.d" u 1:4 t "" w l lt 2  lw 3

set origin 0.4,0.4
set size 0.2,0.3
set tmargin 0
set bmargin 0
set rmargin 0
set lmargin 0
p "../cld/data/cld_k1_j5.d" u 1:2 t "" w l lt 3  lw 3,\
  "../cld/data/cld_k1_j5.d" u 1:3 t "" w l lt 1  lw 3,\
  "../cld/data/cld_k1_j5.d" u 1:4 t "" w l lt 2  lw 3

set origin 0.6,0.4
set size 0.2,0.3
set tmargin 0
set bmargin 0
set rmargin 0
set lmargin 0
p "../cld/data/cld_k1_j7.d" u 1:2 t "" w l lt 3  lw 3,\
  "../cld/data/cld_k1_j7.d" u 1:3 t "" w l lt 1  lw 3,\
  "../cld/data/cld_k1_j7.d" u 1:4 t "" w l lt 2  lw 3

set origin 0.8,0.4
set size 0.2,0.3
set tmargin 0
set bmargin 0
set rmargin 0
set lmargin 0
p "../cld/data/cld_k1_j9.d" u 1:2 t "" w l lt 3  lw 3,\
  "../cld/data/cld_k1_j9.d" u 1:3 t "" w l lt 1  lw 3,\
  "../cld/data/cld_k1_j9.d" u 1:4 t "" w l lt 2  lw 3

###ene#################################################################

set yrange[-5:0]
set xtics 0,5,15
set ytics -5,1,0
set xlabel "Re-energy[MeV]"                   
set ylabel "Im-energy[MeV]"                   
set label "E" at 10,-2 center font "Times,15"
set origin 0.0,0.1
set size 0.2,0.3
set tmargin 0
set bmargin 0
set rmargin 0
set lmargin 0
p "../ene/data/analysis/ene_k1_j1.d" u 2:3 t "" lt 3  pt 7,\
  "../ene/data/analysis/pole_k1_j1.txt" t "" w p lt 1 pt 7

unset ytics
unset y2tics
unset label
unset ylabel
set origin 0.2,0.1
set size 0.2,0.3
set tmargin 0
set bmargin 0
set rmargin 0
set lmargin 0
p "../ene/data/analysis/ene_k1_j3.d" u 2:3 t "" lt 3  pt 7,\
  "../ene/data/analysis/pole_k1_j3.txt" t "" w p lt 1 pt 7

set origin 0.4,0.1
set size 0.2,0.3
set tmargin 0
set bmargin 0
set rmargin 0
set lmargin 0
p "../ene/data/analysis/ene_k1_j5.d" u 2:3 t "" lt 3  pt 7,\
  "../ene/data/analysis/pole_k1_j5.txt" t "" w p lt 1 pt 7

set origin 0.6,0.1
set size 0.2,0.3
set tmargin 0
set bmargin 0
set rmargin 0
set lmargin 0
p "../ene/data/analysis/ene_k1_j7.d" u 2:3 t "" lt 3  pt 7,\
  "../ene/data/analysis/pole_k1_j7.txt" t "" w p lt 1 pt 7

set label "K^{/Symbol p}=0_{1}^{-}" at 16,-1 center
set origin 0.8,0.1
set size 0.2,0.3
set tmargin 0
set bmargin 0
set rmargin 0
set lmargin 0
p "../ene/data/analysis/ene_k1^4_j9.d" u 2:3 t "" lt 3  pt 7,\
  "../ene/data/analysis/pole_1_j9.txt" t "" w p lt 1 pt 7, \
  "../ene/data/analysis/pole_2_j9.txt" t "" w p lt 1 pt 7
