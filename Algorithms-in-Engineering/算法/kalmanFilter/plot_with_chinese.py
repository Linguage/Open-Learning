# filepath: /Users/henri/Documents/WIndSurf/Hello/kalmanFilter/kf_plot.ipynb
# -*- coding: utf-8 -*-
import matplotlib.pyplot as plt
import numpy as np
from matplotlib import rcParams
import matplotlib as mpl

# 更全面的中文字体设置
plt.rcParams['font.sans-serif'] = 'Times New Roman'
#['Arial Unicode MS', 'PingFang SC', 'STHeiti', 'SimHei', 'Microsoft YaHei', 'WenQuanYi Micro Hei']
plt.rcParams['axes.unicode_minus'] = False

# 读取Julia保存的数据（当前目录）
# ...existing code...

# 读取Julia保存的数据（当前目录）
measurements = np.loadtxt('kalmanFilter/kf_measurements.txt')
positions = np.loadtxt('kalmanFilter/kf_positions.txt')
estimates = np.loadtxt('kalmanFilter/kf_estimates.txt')
n = len(measurements)

plt.figure(figsize=(8, 5))
plt.scatter(range(1, n+1), measurements, label='measurements', color='gray', s=20)
plt.plot(range(1, n+1), positions, label='true positions', color='blue', linewidth=2)
plt.plot(range(1, n+1), estimates, label='Kalman filter estimates', color='red', linewidth=2)
plt.title('One-dimensional Kalman Filter Example')
plt.xlabel('Time Step')
plt.ylabel('Position')
plt.legend()
plt.grid(True)
plt.tight_layout()
plt.savefig('kalmanFilter/plot_with_english.png')  # 保存图片到当前目录
plt.show()
