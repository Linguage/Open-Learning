#  对一个时间序列进行小波变换，并绘制小波系数图。
# 程序的框架为：
# 1. 定义时间序列
# 2. 定义信号
# 3. 进行小波变换
# 4. 绘制小波系数图
# 其中，小波变换的函数为pywt.wavedec，绘制小波系数图的函数为matplotlib.pyplot.subplot。

import numpy as np
import pywt
import matplotlib.pyplot as plt

# 定义时间序列  
t = np.arange(0, 1, 0.01)
# 定义信号  
signal = np.sin(2*np.pi*5*t) + np.sin(2*np.pi*10*t)
# 进行小波变换  
coefficients = pywt.wavedec(signal, 'db1', level=3)
# 绘制小波系数图  
titles = ['A3', 'D3', 'D2', 'D1']
for i, coeff in enumerate(coefficients):
    # 生成对应长度的时间序列
    t_coeff = np.linspace(0, 1, len(coeff))
    plt.subplot(len(coefficients), 1, i+1)
    plt.plot(t_coeff, coeff, 'r')
    plt.title(titles[i])
    plt.grid(True)
plt.show()


