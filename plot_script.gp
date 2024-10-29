set terminal pngcairo enhanced font 'Verdana,10'
set output 'output.png'

set xlabel 'Year'
set ylabel 'Temperature (left axis)'
set y2label 'Precipitation (right axis)'

set xrange [1951:2021]
set yrange [min(data(:,1)):max(data(:,1))]
set y2range [min(data(:,2)):max(data(:,2))]

set grid
set style data lines
set key outside

plot 'data.txt' index 0 with lines linestyle 1 title 'Temperature', \
     'data.txt' index 1 with lines linestyle 2 axis x1y2 title 'Precipitation'