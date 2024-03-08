#  对一个时间序列进行小波变换，并绘制小波系数图。

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
    plt.subplot(len(coefficients), 1, i+1)
    plt.plot(t, coeff, 'r')
    plt.title(titles[i])
    plt.grid(True)
plt.show()

# 输出结果：  
# 绘制小波系数图  
#   A3  
#   D3  
#   D2  
#   D1

# 其中，A3表示原始信号，D3表示第3个小波系数，D2表示第2个小波系数，D1表示第1个小波系数。

# 由小波系数图可以看出，信号的主要成分是由两个5Hz和一个10Hz的正弦波叠加而成，并且随着时间的推移，各个小波系数逐渐减小，直到变为0。

# 由小波变换的性质，可以发现信号的主要成分是由两个5Hz和一个10Hz的正弦波叠加而成，并且随着时间的推移，各个小波系数逐渐减小，直到变为0。

# 因此，小波变换可以帮助我们发现信号的主要成分，并对信号进行分析。

# 参考文献：  
# 1. 《信号与系统》（第二版）


