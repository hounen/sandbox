#!/usr/bin/gnuplot -persist
set xrange [0:10]
set yrange [-0.5:0.5]
set xlabel "Energy[MeV]"
set ylabel "{/Symbol D}(E)[MeV^{-1}]"
set terminal postscript eps enhanced color 30 solid
set output "cld_s1.eps"        
p "cld_s1.d" t "s_{1/2}" w l lw 6.0
