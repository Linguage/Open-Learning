# 启用LaTeX选项
set macros
TEX = 'C:\texlive\2023\bin\windows'
set terminal pdfcairo enhanced color font 'Times New Roman,12'
set output 'output.pdf'
set encoding utf8
set format '$%g$'

# 设置标题和坐标轴标签
set title "$\\sin(x)$ Function Plot" font "Times New Roman,18"
set xlabel "$x$ (radians)" font "Times New Roman,14"
set ylabel "$\\sin(x)$" font "Times New Roman,14"

# 设置图例中的LaTeX格式文本
set key font "Times New Roman,14"
set key at -6,0.8
set key samplen 2
set key spacing 1.5
set key box

# 设置图形的范围
set xrange [-2*pi:2*pi]
set yrange [-1:1]

# 绘制正弦函数曲线
plot sin(x) with lines lw 2 title "$\\sin(x)$"
