#! /usr/bin/gnuplot --persist

set term postscript color eps 
set output "ene_cld_phs_p1.eps"

#set key right bottom
set size 0.8, 1.5
set multiplot
set size 0.8, 0.5
set origin 0.0,1.0
set lmargin 7
set rmargin 3
set bmargin 0
set ytics (5,6,7,8,9)
set format x ""

# set label "C"  at 3.3, 7    #font "Times-Bold, 30"
# set label "O"  at 5.3, 8.5  #font "Times-Bold, 30"
# set label "Si" at 9.1, 7.5  #font "Times-Bold, 30"
# set label "Ca" at 11.6, 8.0 #font "Times-Bold, 30"
# set label "Ni" at 14.8, 8.0 #font "Times-Bold, 30"
# set label "Zr" at 19.4, 8   #font "Times-Bold, 30"
# set label "Sn" at 23.4, 8   #font "Times-Bold, 30"
# set label "Pb" at 33.3, 7.2 #font "Times-Bold, 30"

set yrange [4:9]
set xrange [0:40]
# set title "N" -14,-2.2 font "Times-Bold, 28"
set ylabel "E/A(MeV)" 0.1, 0.0
set xlabel ""

#p "../bepnrho0_150sig690-rev" u ($1**(2.0/3.0)):2 w l   title "Chiral SU(3)" lt 1 lw 4,\
#  "../dat/bepnexp"            u ($1**(2.0/3.0)):($2/$1) title "exp."  lt 3 ps 1 pt 7

set size 0.8, 0.49
set origin 0.0,0.51
set tmargin 0
set key 33.5, 1.5 

set nolabel
set label "s" at 36,27
set label "p" at 36,23.5
set label "d" at 36,18.5
set label "f" at 36,13.5
set ytics (0,10,20,30)

set title "{/Symbol L}" -14,-2.1 font "Times-Bold, 28"

set ylabel "S_{/Symbol L}(MeV)" 0.8,0.0 #font "Times-Bold, 32"
set yrange [-5.0:30.0]

 #p "../Single-L_SCL-SU3/KAcore/serho0_150sig690-gsigL3_0"  u (1/$1):2 title 'Chiral SU(3)' w l lw 4 lt 1,\
 #  "../Single-L_SCL-SU3/KAcore/serho0_150sig690-gsigL3_0p" u (1/$1):(($2*2.0 + $3)/3.0) title "" w l lw 4 lt 1,\
 #  "../Single-L_SCL-SU3/KAcore/serho0_150sig690-gsigL3_0d" u (1/$1):(($2*3.0 + $3*2.0)/5.0) title "" w l lw 4 lt 1,\
 #  "../Single-L_SCL-SU3/KAcore/serho0_150sig690-gsigL3_0f" u (1/$1):(($2*4.0 + $3*3.0)/7.0) title "" w l lw 4 lt 1,\
 #  "../dat/LamSE-rev.dat" u (1.0/$1):2:3 title "exp." w yerrorbar lt 3 pt 7 ps 2

set size 0.8, 0.51
set origin 0.0,0.0
set key right bottom
set tmargin 0
set bmargin 3.5
unset format 
unset title

set title "{/Symbol S}" -14, -2.2 font "Times-Bold, 28"

set nolabel
set label "4->3"  at 1, 200
set label "5->4"  at 11, 100
set label "10->9" at 30.5, 650

set xlabel "A^{2/3}" 0.0, 0.2
set ylabel "Shift (eV)" 1.9,0.0 

set logscale y
set ytics (10, 50, 100, 500, 1000)
set yrange [10:1000]

#p "../S-Finite/AtomicShift/Shifts-SR.dat"     u ($5**(2.0/3.0)):2   title "Chiral SU(3)(SR)"   w lp        lt 1 lw 2 ps 2 pt 7, \
#  "../S-Finite/AtomicShift/Shifts-WR.dat"     u ($5**(2.0/3.0)):2   title "Chiral SU(3)(WR)"   w lp        lt 2 lw 2 ps 2 pt 9, \
#  "../S-Finite/AtomicShift/Shifts.dat"        u ($6**(2.0/3.0)):4:5 title "Exp." w yerrorbar lt 3 lw 3 ps 0 pt 11

set nomultiplot

set term postscript eps enh color
set output "ene_cld_phs_p1.eps"

set xrange [0:5]
set xtics 0,0.5,5

set format x""
set multiplot
p "phs_p1.d" t "p_{1/2}" w l lw 6.0 
p "cld_p1.d" t "p_{1/2}" w l lw 6.0 
p "ene_p1.d" u 2:3 t "p_{1/2}" w p lt 3 pt 7 ps 3
