#!/usr/bin/gnuplot -persist
set terminal postscript eps enhanced color 30 solid
set xrange [0:40]
set yrange [0:360]
set xlabel "Energy [MeV]"
set ylabel "{/Symbol d} [deg.]"
#set x2tics ("E_{r}" 3.14)
set ytics 0,60,360
#set arrow 1 from 3.14,0 rto 3.14,360 nohead lt -1 lw 3 
set output "phs_k4_j0.eps"
p "data/analysis/phs_k4_j0.d" u 1:($2+180) t "K^{/Symbol p}=0_{4}^{+} J^{/Symbol p}=0^{+}" w l lw 6.0


