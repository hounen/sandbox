#!/usr/bin/gnuplot -persist
set terminal postscript eps enhanced color 30 solid
set xrange [0:40]
set yrange [-0.05:0.2]
set xlabel "Energy [MeV]"
set ylabel "{/Symbol D} [MeV^{-1}]"
#set x2tics ("E_{r}" 0.49)
#set arrow 1 from 0.49,-1 rto 0.49,1 nohead lt -1 lw 3 
set output "cld_j10_n60_b0.11_g1.17_th17^25.eps"
p "data/cld_j10_n60_b0.11_g1.17_th17.d" u 1:2 t \
  "J^{/Symbol p}=10^{+} {/Symbol q}=17 [deg.]" w l lw 6.0,\
  "data/cld_j10_n60_b0.11_g1.17_th25.d" u 1:2 t \
  "J^{/Symbol p}=10^{+} {/Symbol q}=25 [deg.]" w l lw 6.0

