#!/usr/bin/gnuplot -persist
set terminal postscript eps enhanced color 30 solid
set xrange [0:40]
set yrange [-0.1:0.5]
set xlabel "Energy [MeV]"
set ylabel "{/Symbol D} [MeV^{-1}]"
#set x2tics ("E_{r}" 6.63)
#set arrow 1 from 6.63,-1 rto 6.63,1 nohead lt -1 lw 3 
set output "cld_k1_j7.eps"
p "data/cld_k1_j7.d" u 1:2 t "K^{/Symbol p}=0_{1}^{-} J^{/Symbol p}=7^{-}" w l lw 6.0,\
  90 t "" w l 0

