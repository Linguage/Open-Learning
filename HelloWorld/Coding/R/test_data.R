# 安装并加载 latex2exp 包
# install.packages("latex2exp")
library(latex2exp)

# # 绘制一个包含 LaTeX 公式的散点图
# x <- 1:10
# y <- x^2
# 
# plot(x, y, main = TeX("$y = x^2$"))
# 绘制 LaTeX 公式
plot(TeX("A $\\LaTeX$ formula: $\\frac{2hc^2}{\\lambda^5} \\cdot
      \\frac{1}{e^{\\frac{hc}{\\lambda k_B T}} - 1}$"), cex=2)
