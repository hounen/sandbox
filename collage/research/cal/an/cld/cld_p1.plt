#!/usr/bin/gnuplot -persist
set xrange [0:10]
set yrange [-0.2:0.2]
set xlabel "Energy[MeV]"
set ylabel "{/Symbol D}(E)[MeV^{-1}]"
set terminal postscript eps enhanced color 30 solid
set x2tics ("p_{1/2}" 2.077087)
set arrow 2 from 2.077087,-0.2 rto 2.077087,0.2 nohead lt 1 lw 3
set output "cld_p1.eps"        
p  "cld_p1.d" t "p_{1/2}" w l lw 6.0 
