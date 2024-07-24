#!/usr/bin/gnuplot -persist
set terminal postscript eps enhanced color 30 solid
set xrange [0:10]
set yrange [-1:360]
set xlabel "Energy [MeV]"
set ylabel "{/Symbol d} [deg.]"
#set x2tics ("E_{r}" 0.49)
set ytics 0,60,360
#set arrow 1 from 0.49,0 rto 0.49,360 nohead lt -1 lw 3 
set output "phs_k1_j1.eps"
p "data/analysis/phs_k1_j1.d" u 1:2 t "K^{/Symbol p}=0_{1}^{+} J^{/Symbol p}=1^{-}" w l lw 6.0

