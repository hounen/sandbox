#!/usr/bin/gnuplot -persist
set xrange[0:10]
set yrange[0:180]
set xlabel "Energy [MeV]"
set ylabel "{/Symbol d} [deg.]"
set terminal postscript eps enhanced color 30 solid
set output "phs_s1_he5_li5.eps"
p "../helium5/phs/data/analysis/phs_ana_s1.d" \
  u 1:($2+180) t "he5 s_{1/2}" w l lt -1 lw 2.0,\
  "../lithium5/phs/data/analysis/phs_ana_s1.d" \
  u 1:($2+180) t "li5 s_{1/2}" w l lt 0 lw 5.0,\
  90 t "" w l 0

