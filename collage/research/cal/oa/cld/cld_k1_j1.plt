#!/usr/bin/gnuplot -persist
set terminal postscript eps enhanced color 30 solid
set xrange [0:10]
set yrange [-1:10]
set xlabel "Energy [MeV]"
set ylabel "{/Symbol D} [MeV^{-1}]"
#set x2tics ("E_{r}" 0.49)
#set arrow 1 from 0.49,-1 rto 0.49,1 nohead lt -1 lw 3 
set output "cld_k1_j1.eps"
p "data/cld_k1_j1.d" u 1:2 t "K^{/Symbol p}=0_{1}^{-} J^{/Symbol p}=1^{-}" w l lw 6.0,\
  90 t "" w l 0

