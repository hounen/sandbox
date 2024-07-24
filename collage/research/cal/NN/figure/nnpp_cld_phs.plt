#!/usr/bin/gnuplot -persist
set term postscript enh color 12 solid
set output "nnpp_cld_phs.ps"
set xrange[-10:190]
set multiplot
#######################################################################
# nn
#######################################################################
# phs
#######################################################################
set yrange [-20:70]
unset key
unset xtics
set ytics -20,20,70
set ylabel "{/Symbol d} [deg.]"
set label "{/Symbol d}" at 50,40 center font "Times,20"
set label "NN" at 100,80 center font "Times,20"
#set label "^4He-n p_{3/2} E_{r}=0.78MeV {/Symbol G}=0.55MeV" at 5,165 center font "Times,15"
set origin 0.1,0.5
set size 0.4,0.4
set tmargin 0
set bmargin 0
set rmargin 0
set lmargin 0
p "../phs/data/nn/analysis/nnphs02.d" u 1:2 t "nn" w l lt 3 lw 5.0,\
  0 t "" lt 0 lw 3
#######################################################################
# cld
#######################################################################
set yrange [-0.5:1]
set xtics 0,50,200
set ytics -0.5,0.5,0.5
set xlabel "Energy[MeV]"
set ylabel "{/Symbol D} [MeV^{-1}]"
set label "{/Symbol D}" at 50,0.5 center font "Times,20"
#set label "^4He-n p_{3/2} E_{r}=0.78MeV {/Symbol G}=0.55MeV" at 5,165 center font "Times,15"
set origin 0.1,0.1
set size 0.4,0.4
set tmargin 0
set bmargin 0
set rmargin 0
set lmargin 0
p "../cld/data/nn/nncld0.d" u 1:2 t "nn" w l lt 3 lw 5.0
#######################################################################
# pp
#######################################################################
# phs
#######################################################################
set yrange [-20:70]
unset xtics
unset ytics
unset xlabel
unset ylabel 
unset label
set label "PP" at 100,80 center font "Times,20"
#set label "^4He-n p_{3/2} E_{r}=0.78MeV {/Symbol G}=0.55MeV" at 5,165 center font "Times,15"
set origin 0.5,0.5
set size 0.4,0.4
set tmargin 0
set bmargin 0
set rmargin 0
set lmargin 0
p "../phs/data/pp/analysis/ppphs02.d" u 1:2 t "pp" w l lt 3 lw 5.0,\
  0 t "" lt 0 lw 3
#######################################################################
# cld
#######################################################################
set yrange [-0.5:1]
set xtics 0,50,200
set xlabel "Energy[MeV]"
#set label "^4He-n p_{3/2} E_{r}=0.78MeV {/Symbol G}=0.55MeV" at 5,165 center font "Times,15"
set origin 0.5,0.1
set size 0.4,0.4
set tmargin 0
set bmargin 0
set rmargin 0
set lmargin 0
p "../cld/data/pp/ppcld0.d" u 1:2 t "pp" w l lt 3 lw 5.0
unset multiplot
