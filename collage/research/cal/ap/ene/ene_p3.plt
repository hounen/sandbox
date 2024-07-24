#!/usr/bin/gnuplot -persist
set term postscript eps color enhanced 30 solid
set output "ene_p3.eps"
set xrange [0:5]
set yrange [-5:0]
set y2range [-5:0]
unset key
set xlabel "Re-energy[MeV]"
set label "(E_{r},{/Symbol G})=(1.67,1.33)" at 2.00,-1.00 center
set ylabel "Im-energy[MeV]"
p "data/ene_p3.d" u 2:3 t "p_{3/2}" w p lt 3 pt 7 ps 3,\
  "data/pole_p3.txt" t "" w p lt 1 pt 7 ps 3

