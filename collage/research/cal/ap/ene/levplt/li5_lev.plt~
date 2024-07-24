#!/usr/bin/gnuplot -persist
set term postscript eps enhanced color "Times-Bold" 28
set output 'li5_lev.eps'
set border 10
unset xtics
set ytics 0,1,4
set ylabel 'Energy[MeV]'
set xrange [0:5]
set yrange [-0.5:4]
set label "Exp." at 1.5,-0.5 center 
set label "calc." at 3.5,-0.5 center
set label "{/Symbol G}=1.23" at 1.5,1.82 center
set label "{/Symbol G}=6.60" at 1.5,3.32 center
set label "{/Symbol G}=1.18" at 3.5,1.74 center
set label "{/Symbol G}=6.81" at 3.5,3.14 center
set label "3/2^-" at 0.5,1.82 left
set label "1/2^-" at 0.5,3.32 left 
set label "3/2^-" at 4.5,1.74 right 
set label "1/2^-" at 4.5,3.14 right
set label "{/Symbol a}-p threshold" at 0.8,0.2 center
#set zeroaxis
p 'li5_lev_exp.txt' u 1:($2+1.69) t "" w l lw 3 lt -1,\
  'li5_lev_ana.txt' t "" w l lw 3 lt -1,\
  'li5_lev_dif.txt' notitle  w l lw 3 lt 0,\
  0 t "" lt 0
