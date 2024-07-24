#!/usr/bin/gnuplot -persist
PS = 0
PS = 1
if (PS == 1) set term postscript eps enhanced color 30 solid
if (PS == 1) set output "ene_k4_j4.eps"
set xrange [0:40]                              
set x2range [0:40]                              
set yrange [-3:0]
set y2range [-3:0]
set xlabel "Re-energy[MeV]"                   
set ylabel "Im-energy[MeV]"                   
set label "K^{/Symbol p}=0_{4}^{+} (E_{r},{/Symbol G})=(6.63,4.48)" at 8.00,-2.24 left
set x2tics (" " 20)
p "data/analysis/ene_k4_j4.d" u 2:3 t "J^{/Symbol p}=4^{+}" w p lt 3 pt 7 ps 3,\
  "data/analysis/pole_k4_j4.txt" t "" w p lt 1 pt 7 ps 3
if (PS == 0) pause -1

