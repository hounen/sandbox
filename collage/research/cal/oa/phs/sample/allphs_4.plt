#!/usr/bin/gnuplot -persis
set terminal postscript eps enhanced color 30 solid
set xrange [0:40]
set yrange [0:360]
set xlabel "Energy [MeV]"
set ylabel "{/Symbol d} [deg.]"
set output "allphs_4.eps"
p "phs_4.d" u 1:($2+180) t "j=4" w l lw 6.0,\
  "phs_4.d" u 1:($3+180) t "reso" w l lw 6.0,\
  "phs_4.d" u 1:($4+180) t "conti" w l lw 6.0,\
  90 t "" w l 0

