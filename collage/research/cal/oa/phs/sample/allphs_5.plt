#!/usr/bin/gnuplot -persis
set terminal postscript eps enhanced color 30 solid
set xrange [0:40]
set yrange [-180:360]
set xlabel "Energy [MeV]"
set ylabel "{/Symbol d} [deg.]"
set output "allphs_5.eps"
p "phs_5.d" u 1:2 t "j=5" w l lw 6.0,\
  "phs_5.d" u 1:3 t "reso" w l lw 6.0,\
  "phs_5.d" u 1:4 t "conti" w l lw 6.0,\
  90 t "" w l 0

