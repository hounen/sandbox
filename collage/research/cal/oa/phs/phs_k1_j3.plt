#!/usr/bin/gnuplot -persist
set terminal postscript eps enhanced color 30 solid
set xrange [0:10]
set yrange [0:200]
set xlabel "Energy [MeV]"
set ylabel "{/Symbol d} [deg.]"
#set x2tics ("E_{r}" 2.31)
set ytics 0,60,360
#set arrow 1 from 2.31,0 rto 2.31,200 nohead lt -1 lw 3 
set tmargin 2
set bmargin 3
set rmargin 2
set lmargin 10
set output "phs_k1_j3.eps"
p "data/analysis/phs_k1_j3.d" u 1:2 t "K^{/Symbol p}=0_{1}^{-} J^{/Symbol p}=3^{-}" w l lw 6.0

