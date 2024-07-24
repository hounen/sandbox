#!/usr/bin/gnuplot -persist
set xrange[0:1]
set yrange[150:180]
set xtics 0,0.1,1
set ytics 150,10,180
set xlabel "Energy [MeV]"
set ylabel "{/Symbol d} [deg.]"
set terminal postscript eps enhanced color 30 solid
set output "li5_phs_com_s1.eps"
p "data/analysis/li5_phs_ana_s1_100MeV.d" u 1:($2+180) t "li5_s_{1/2}" w l lw 6.0,\
  "../../helium5/phs/phs_s1_ana.d" u 1:($2+180) t "he5_s_{1/2}" w l lw 6.0,\
  90 t "" w l 0

