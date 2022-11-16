set title 'Repeated SMM (32x32) computation'                       # plot title
set xlabel 'iterations'                              # x-axis label
set ylabel 'time(seconds)'                          # y-axis label
set key top left
set grid

set terminal pngcairo dashed size 800,600 enhanced
set output 'output.png'

set label "recompilation overhead: 409ms" at 150000,0.5 right
set arrow from 0,0.4 to 200000,0.4 nohead lc rgb 'black' lt 3

set label "pay off time: 644ms" at 150000,0.755555 left
set arrow from 0,0.64 to 200000,0.64 nohead lc rgb 'black' lt 3

set label "34.8 iterations/ms" at 140000,4.3 right rotate by 35
set label "108 iterations/ms" at 140000,2.2 right rotate by 15

plot 'measured_times.txt' using 1:2 with linespoints title "baseline", \
     'measured_times.txt' using 1:3 with linespoints title "doping recompilation"
