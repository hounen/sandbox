#!/usr/bin/gnuplot -persist
set terminal postscript eps enhanced color 30 solid
set xrange [0:10]
set yrange [-1:40]
set ylabel "{/Symbol D} [MeV^{-1}]"
set xlabel "Energy [MeV]"
#set x2tics ("E_{r}" 2.31)
#set arrow 1 from 2.31,-1 rto 2.31,40 nohead lt -1 lw 3 
unset key
set tmargin 2
set bmargin 3
set rmargin 2
set lmargin 10
set output "cld_k1_j3.eps"
p "data/cld_k1_j3.d" u 1:2 t "K^{/Symbol p}=0_{1}^{-} J^{/Symbol p}=3^{-}" w l lw 6.0,\
  90 t "" w l 0

