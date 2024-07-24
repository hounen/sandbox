#!/usr/bin/gnuplot -persist
set xrange [0:10]
set yrange [-0.5:1.5]
set xlabel "Energy[MeV]"
set ylabel "{/Symbol D}(E)[MeV^{-1}]"
set terminal postscript eps enhanced color 30 solid
set x2tics ("p_{1/2}" 2.077087, "p_{3/2}" 0.7846474)
set arrow 1 from 0.7846474,-0.5 rto 0.7846474,1.5 nohead lt 3 lw 3
set arrow 2 from 2.077087,-0.5 rto 2.077087,1.5 nohead lt 2 lw 3
set output "cld_sp.eps"        
p "cld_s1.d" t "s_{1/2}" w l lw 6.0 ,\
  "cld_p1.d" t "p_{1/2}" w l lw 6.0 ,\
  "cld_p3.d" t "p_{3/2}" w l lw 6.0 
