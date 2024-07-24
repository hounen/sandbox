#!/usr/bin/gnuplot -persist
set terminal postscript eps enhanced color 30 solid
set xrange [0:40]
set yrange [0:360]
set xlabel "Energy [MeV]"
set ylabel "{/Symbol d} [deg.]"
set output "phs_nega.eps"
p "phs_1.d" u 1:2 t "j=1" w l lw 6.0,\
  "phs_3.d" u 1:2 t "j=3" w l lw 6.0,\
  "phs_5.d" u 1:2 t "j=5" w l lw 6.0,\
  "phs_7.d" u 1:2 t "j=7" w l lw 6.0,\
  "phs_9.d" u 1:2 t "j=9" w l lw 6.0

