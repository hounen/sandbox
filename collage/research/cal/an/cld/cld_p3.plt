#!/usr/bin/gnuplot -persist
set xrange [0:10]
set yrange [-0.5:1.5]
set xlabel "Energy[MeV]"
set ylabel "{/Symbol D}(E)[MeV^{-1}]"
set terminal postscript eps enhanced color 30 solid
set x2tics ("p_{3/2}" 0.7846474)
#set arrow 1 from 0.7846474,-0.5 rto 0.7846474,1.5 nohead lt 1 lw 3
set output "cld_p3.eps"        
p  "cld_p3.d" t "p_{3/2}" w l lw 6.0 

