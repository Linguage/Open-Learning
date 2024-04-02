# 本函数的功能为：利用离散傅立叶变换（DFT）和逆离散傅立叶变换（IDFT）进行信号的频域分析和重构。
# 输入：时域信号signal，采样频率fs。
# 输出：频域信号spectrum，复原信号reconstructed_signal，误差error。
# 算法：
# 1. 定义离散傅立叶变换（DFT）函数。
# 2. 定义逆离散傅立叶变换（IDFT）函数。
# 3. 从文件导入时域信号。
# 4. 将时域信号转换为频域信号（DFT）。
# 5. 将频域信号复原为时域信号（IDFT）。
# 6. 计算复原信号与原始信号之间的误差。
# 7. 绘制原始信号图像、频域信号图像、复原信号图像、误差图。

# 导入必要的库
import numpy as np
import matplotlib.pyplot as plt

# 定义离散傅立叶变换（DFT）函数
def DFT(signal):
    N = len(signal)
    n = np.arange(N)
    k = n.reshape((N, 1))
    e = np.exp(-2j * np.pi * k * n / N)
    return np.dot(e, signal)

# 定义逆离散傅立叶变换（IDFT）函数
def IDFT(spectrum):
    N = len(spectrum)
    n = np.arange(N)
    k = n.reshape((N, 1))
    e = np.exp(2j * np.pi * k * n / N)
    return np.dot(e, spectrum) / N

# 从文件导入时域信号
signal = np.loadtxt('1.txt')

# 将时域信号转换为频域信号（DFT）
spectrum = DFT(signal)

# 将频域信号复原为时域信号（IDFT）
reconstructed_signal = IDFT(spectrum).real

# 计算复原信号与原始信号之间的误差
error = signal - reconstructed_signal

# 绘制原始信号图像
plt.figure(figsize=(10, 7))
plt.subplot(411)
plt.plot(signal, label='Original Signal')
plt.xlabel('Time')
plt.ylabel('Amplitude')
plt.title('Original Signal')
plt.legend()

# 绘制频域信号图像
plt.subplot(412)
plt.plot(np.abs(spectrum), label='Frequency Domain Signal')
plt.xlabel('Frequency')
plt.ylabel('Amplitude')
plt.title('Frequency Domain Signal')
plt.legend()

# 绘制复原信号图像
plt.subplot(413)
plt.plot(reconstructed_signal, label='Reconstructed Signal')
plt.xlabel('Time')
plt.ylabel('Amplitude')
plt.title('Reconstructed Signal')
plt.legend()

# 绘制误差图
plt.subplot(414)
plt.plot(error, label='Error', color='red')
plt.xlabel('Time')
plt.ylabel('Amplitude')
plt.title('Error')
plt.xticks([])
plt.legend()

plt.tight_layout()
plt.show()
