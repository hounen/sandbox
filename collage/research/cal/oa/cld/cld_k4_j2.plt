#!/usr/bin/gnuplot -persist
set terminal postscript eps enhanced color 30 solid
set xrange [0:40]
set yrange [-0.1:0.2]
set xlabel "Energy [MeV]"
set ylabel "{/Symbol D} [MeV^{-1}]"
#set x2tics ("E_{r}" 4.21)
#set arrow 1 from 4.21,-1 rto 4.21,1 nohead lt -1 lw 3 
set output "cld_k4_j2.eps"
p "data/cld_k4_j2.d" u 1:2 t "K^{/Symbol p}=0_{4}^{+} J^{/Symbol p}=2^{+}" w l lw 6.0,\
  90 t "" w l 0

