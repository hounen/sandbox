#!/usr/bin/gnuplot -persist
set xrange[0:100] 
set yrange[0:100]
#set xtics 0,2,10
#set ytics -180,30,180
#set xlabel "Energy [MeV]"
#set ylabel "{/Symbol d} [deg.]"
#set x2tics ("E_{r}=3.08" 3.08)
#set grid x2tics lt -1 lw 3
set terminal postscript eps enhanced color 30 solid
set output "test.eps"
p  "test.d" w p pt 6 lt 3 ,\
   50*sin(x)+50 ,\
   0.005*x**2 ,\
   0.005*x**3-50
