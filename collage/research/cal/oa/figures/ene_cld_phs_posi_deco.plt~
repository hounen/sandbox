#!/usr/bin/gnuplot -persist
set term postscript enh color solid 8
set output "ene_cld_phs_posi_deco.ps"
set xrange[0:20]
set multiplot

###phs#################################################################

set yrange[-30:380]
unset xtics
set ytics 0,60,480
set key 15,300 
set ylabel "{/Symbol d} [deg.]"
set label "K^{/Symbol p}=0_{4}^{+} J^{/Symbol p}=0^{+} E_{r}=3.23MeV {/Symbol G}=1.79MeV" at 10,360 center
set label "{/Symbol d}" at 10,200 center font "Times,15"
set arrow from 0,0 to 20,0 lt 0 nohead
set arrow from 0,180 to 20,180 lt 0 nohead
set origin 0.0,0.7
set size 0.2,0.3
set tmargin 0
set bmargin 0
set rmargin 0
set lmargin 0
p "../phs/data/analysis/phs_k4_j0.d" u 1:($2+180) w l t "total" lt 3 lw 3,\
  "../phs/data/analysis/phs_k4_j0.d" u 1:($3+180) w l t "resonance" lt 1 lw 3,\
  "../phs/data/analysis/phs_k4_j0.d" u 1:($4+180) w l t "continuum" lt 2 lw 3

#set ytics (0,45,90,135,180)
#set noy2tics
unset ytics
unset ylabel
unset label
set label "K^{/Symbol p}=0_{4}^{+} J^{/Symbol p}=2^{+} E_{r}=4.21MeV {/Symbol G}=2.73MeV" at 10,360 center
#set label "{/Symbol d}" at 4.5,150 center font "Times,15"
set origin 0.2,0.7
set size 0.2,0.3
set tmargin 0
set bmargin 0
set rmargin 0
set lmargin 0
p "../phs/data/analysis/phs_k4_j2.d" u 1:($2+180) w l t "" lt 3 lw 3,\
  "../phs/data/analysis/phs_k4_j2.d" u 1:($3+180) w l t "" lt 1 lw 3,\
  "../phs/data/analysis/phs_k4_j2.d" u 1:($4+180) w l t "" lt 2 lw 3

#set ytics (0,45,90,135,180)
#set noy2tics
unset label
set label "K^{/Symbol p}=0_{4}^{+} J^{/Symbol p}=4^{+} E_{r}=6.63MeV {/Symbol G}=4.48MeV" at 10,360 center
#set label "{/Symbol d}" at 4.5,150 center font "Times,15"
set origin 0.4,0.7
set size 0.2,0.3
set tmargin 0
set bmargin 0
set rmargin 0
set lmargin 0
p "../phs/data/analysis/phs_k4_j4.d" u 1:($2+180) w l t "" lt 3 lw 3,\
  "../phs/data/analysis/phs_k4_j4.d" u 1:($3+180) w l t "" lt 1 lw 3,\
  "../phs/data/analysis/phs_k4_j4.d" u 1:($4+180) w l t "" lt 2 lw 3

#set ytics (0,45,90,135,180)
#set noy2tics
unset label
set label "K^{/Symbol p}=0_{1}^{+} J^{/Symbol p}=6^{+} E_{r}=3.72MeV {/Symbol G}=0.002MeV" at 10,360 center
set label "K^{/Symbol p}=0_{4}^{+} J^{/Symbol p}=6^{+} E_{r}=10.9MeV {/Symbol G}=6.62MeV" at 10,330 center
#set label "{/Symbol d}" at 4.5,150 center font "Times,15"
set origin 0.6,0.7
set size 0.2,0.3
set tmargin 0
set bmargin 0
set rmargin 0
set lmargin 0
p "../phs/data/analysis/phs_k1^4_j6.d" u 1:2 w l t "" lt 3 lw 3,\
  "../phs/data/analysis/phs_k1^4_j6.d" u 1:3 w l t "" lt 1 lw 3,\
  "../phs/data/analysis/phs_k1^4_j6.d" u 1:4 w l t "" lt 2 lw 3

#set ytics (0,45,90,135,180)
#set noy2tics
unset label
set label "K^{/Symbol p}=0_{1}^{+} J^{/Symbol p}=8^{+} E_{r}=9.87MeV {/Symbol G}=0.02MeV" at 10,360 center
set label "K^{/Symbol p}=0_{4}^{+} J^{/Symbol p}=8^{+} E_{r}=17.35MeV {/Symbol G}=8.10MeV" at 10,330 center
#set label "{/Symbol d}" at 4.5,150 center font "Times,15"
set origin 0.8,0.7
set size 0.2,0.3
set tmargin 0
set bmargin 0
set rmargin 0
set lmargin 0
p "../phs/data/analysis/phs_k1^4_j8.d" u 1:2 w l  t "" lt 3 lw 3,\
  "../phs/data/analysis/phs_k1^4_j8.d" u 1:3 w l  t "" lt 1 lw 3,\
  "../phs/data/analysis/phs_k1^4_j8.d" u 1:4 w l  t "" lt 2 lw 3

#set ytics (0,45,90,135,180)
#set noy2tics
#set label "s_{1/2}" at 2.5,160 center
#set label "{/Symbol d}" at 4.5,150 center font "Times,15"
#set origin 0.84,0.7
#set size 0.16,0.3
#set tmargin 0
#set bmargin 0
#set rmargin 0
#set lmargin 0
#p "../phs/data/analysis/phs_k1^4_j10.d" u 1:2 w l t "" lt 3 lw 3

###cld#################################################################

set yrange[-0.2:0.5]
#set ytics (0,45,90,135,180)
#set noy2tics
set ytics -0.1,0.1,0.4
set key 15,0.4
set ylabel "{/Symbol D} [MeV^{-1}]"
set label "{/Symbol D}" at 10,0.2 center font "Times,15"
#set label "s_{1/2}" at 2.5,160 center
#set label "{/Symbol d}" at 4.5,150 center font "Times,15"
unset arrow
set origin 0.0,0.4
set size 0.2,0.3
set tmargin 0
set bmargin 0
set rmargin 0
set lmargin 0
p "../cld/data/cld_k4_j0.d" u 1:2 t "total" w l lt 3  lw 3,\
  "../cld/data/cld_k4_j0.d" u 1:3 t "resonance" w l lt 1  lw 3,\
  "../cld/data/cld_k4_j0.d" u 1:4 t "continuum" w l lt 2  lw 3

#set ytics (0,45,90,135,180)
#set noy2tics
unset ylabel
unset ytics
unset label
#set label "s_{1/2}" at 2.5,160 center
#set label "{/Symbol d}" at 4.5,150 center font "Times,15"
set origin 0.2,0.4
set size 0.2,0.3
set tmargin 0
set bmargin 0
set rmargin 0
set lmargin 0
p "../cld/data/cld_k4_j2.d" u 1:2 t "" w l lt 3  lw 3,\
  "../cld/data/cld_k4_j2.d" u 1:3 t "" w l lt 1  lw 3,\
  "../cld/data/cld_k4_j2.d" u 1:4 t "" w l lt 2  lw 3

#set ytics (0,45,90,135,180)
#set noy2tics
#set label "s_{1/2}" at 2.5,160 center
#set label "{/Symbol d}" at 4.5,150 center font "Times,15"
set origin 0.4,0.4
set size 0.2,0.3
set tmargin 0
set bmargin 0
set rmargin 0
set lmargin 0
p "../cld/data/cld_k4_j4.d" u 1:2 t "" w l lt 3  lw 3,\
  "../cld/data/cld_k4_j4.d" u 1:3 t "" w l lt 1  lw 3,\
  "../cld/data/cld_k4_j4.d" u 1:4 t "" w l lt 2  lw 3

#set ytics (0,45,90,135,180)
#set noy2tics
#set label "s_{1/2}" at 2.5,160 center
#set label "{/Symbol d}" at 4.5,150 center font "Times,15"
set origin 0.6,0.4
set size 0.2,0.3
set tmargin 0
set bmargin 0
set rmargin 0
set lmargin 0
p "../cld/data/cld_k1^4_j6.d" u 1:2 t "" w l lt 3  lw 3,\
  "../cld/data/cld_k1^4_j6.d" u 1:3 t "" w l lt 1  lw 3,\
  "../cld/data/cld_k1^4_j6.d" u 1:4 t "" w l lt 2  lw 3

#set ytics (0,45,90,135,180)
#set noy2tics
#set label "s_{1/2}" at 2.5,160 center
#set label "{/Symbol d}" at 4.5,150 center font "Times,15"
set origin 0.8,0.4
set size 0.2,0.3
set tmargin 0
set bmargin 0
set rmargin 0
set lmargin 0
p "../cld/data/cld_k1^4_j8.d" u 1:2 t "" w l lt 3  lw 3,\
  "../cld/data/cld_k1^4_j8.d" u 1:3 t "" w l lt 1  lw 3,\
  "../cld/data/cld_k1^4_j8.d" u 1:4 t "" w l lt 2  lw 3

###ene#################################################################
set yrange[-5:0]
#set ytics (0,45,90,135,180)
set xtics 0,5,15
set ytics -5,1,0
set xlabel "Re-energy[MeV]"                   
set ylabel "Im-energy[MeV]"                   
set label "E" at 10,-2 center font "Times,15"
#set label "s_{1/2}" at 2.5,160 center
#set label "{/Symbol d}" at 4.5,150 center font "Times,15"
set origin 0.0,0.1
set size 0.2,0.3
set tmargin 0
set bmargin 0
set rmargin 0
set lmargin 0
p "../ene/data/analysis/ene_k4_j0.d" u 2:3 t "" lt 3  pt 7,\
  "../ene/data/analysis/pole_k4_j0.txt" t "" w p lt 1 pt 7

unset ytics
unset y2tics
unset label
unset ylabel
#set label "s_{1/2}" at 2.5,160 center
#set label "{/Symbol d}" at 4.5,150 center font "Times,15"
set origin 0.2,0.1
set size 0.2,0.3
set tmargin 0
set bmargin 0
set rmargin 0
set lmargin 0
p "../ene/data/analysis/ene_k4_j2.d" u 2:3 t "" lt 3  pt 7,\
  "../ene/data/analysis/pole_k4_j2.txt" t "" w p lt 1 pt 7

#set ytics (0,45,90,135,180)
#set noy2tics
#set label "s_{1/2}" at 2.5,160 center
#set label "{/Symbol d}" at 4.5,150 center font "Times,15"
set origin 0.4,0.1
set size 0.2,0.3
set tmargin 0
set bmargin 0
set rmargin 0
set lmargin 0
p "../ene/data/analysis/ene_k4_j4.d" u 2:3 t "" lt 3  pt 7,\
  "../ene/data/analysis/pole_k4_j4.txt" t "" w p lt 1 pt 7

#set ytics (0,45,90,135,180)
#set noy2tics
#set label "s_{1/2}" at 2.5,160 center
#set label "{/Symbol d}" at 4.5,150 center font "Times,15"
set label "k_{/Symbol p}=0_{1}^{+}" at 5,-0.3 center font "Times,10"
set label "k_{/Symbol p}=0_{4}^{+}" at 10,-3 center font "Times,10"
set origin 0.6,0.1
set size 0.2,0.3
set tmargin 0
set bmargin 0
set rmargin 0
set lmargin 0
p "../ene/data/analysis/ene_k1^4_j6.d" u 2:3 t "" lt 3  pt 7,\
  "../ene/data/analysis/pole_k1_j6.txt" t "" w p lt 1 pt 7,\
  "../ene/data/analysis/pole_k4_j6.txt" t "" w p lt 1 pt 7

#set ytics (0,45,90,135,180)
#set noy2tics
#set label "s_{1/2}" at 2.5,160 center
unset label
set label "k_{/Symbol p}=0_{1}^{+}" at 10,-0.3 center font "Times,10"
set label "k_{/Symbol p}=0_{4}^{+}" at 15,-4 center font "Times,10"
set origin 0.8,0.1
set size 0.2,0.3
set tmargin 0
set bmargin 0
set rmargin 0
set lmargin 0
p "../ene/data/analysis/ene_k1^4_j8.d" u 2:3 t "" lt 3 pt 7 ,\
  "../ene/data/analysis/pole_k1_j8.txt" t "" w p lt 1 pt 7,\
  "../ene/data/analysis/pole_k4_j8.txt" t "" w p lt 1 pt 7

#set ytics (0,45,90,135,180)
#set noy2tics
#set label "s_{1/2}" at 2.5,160 center
#set label "{/Symbol d}" at 4.5,150 center font "Times,15"
#set origin 0.84,0.1
#set size 0.16,0.3
#set tmargin 0
#set bmargin 0
#set rmargin 0
#set lmargin 0
#p "../ene/data/analysis/ene_k1^4_j10.d" u 2:3 t "" lt 3 pt 7,\
#  "../ene/data/analysis/pole_k1_j10.txt" t "" w p lt 1 pt 7,\
#  "../ene/data/analysis/pole_k4_j10.txt" t "" w p lt 1 pt 7
