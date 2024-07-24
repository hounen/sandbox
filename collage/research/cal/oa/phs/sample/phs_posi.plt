#!/usr/bin/gnuplot -persist
set terminal postscript eps enhanced color 30 solid
set xrange [0:40]
set yrange [0:360]
set xlabel "Energy [MeV]"
set ylabel "{/Symbol d} [deg.]"
set output "phs_posi.eps"
p "phs_0.d" u 1:($2+180) t "j=0" w l lw 6.0,\
  "phs_2.d" u 1:($2+180) t "j=2" w l lw 6.0,\
  "phs_4.d" u 1:($2+180) t "j=4" w l lw 6.0,\
  "phs_6.d" u 1:2 t "j=6" w l lw 6.0,\
  "phs_8.d" u 1:2 t "j=8" w l lw 6.0,\
  "phs_10.d" u 1:($2+180) t "j=10" w l lw 6.0,\
  90 t "" w l 0

